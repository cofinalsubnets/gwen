#include "gwen.h"
#include <stdbool.h>
#include <stdarg.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

// the vm assumes tail calls are optimized by the C compiler when possible.
// if this isn't the case then defining this macro to 0 will use a trampoline
// to avoid tail calls which however has large overhead (tests run ~2x longer)
#ifndef GwenCanUseTco
// try and use TCO by default
#define GwenCanUseTco 1
#endif

#if GwenCanUseTco
#define Vm(n, ...) gwen_status n(gwen_core f, gwen_cell Ip, gwen_heap Hp, gwen_stack Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define YieldStatus GwenStatusOk
#define GwenContinue() Ip->ap(f, Ip, Hp, Sp)
#define Have(n) if (Sp - Hp < n) return gc(f, Ip, Hp, Sp, n)
#define Have1() if (Sp == Hp) return gc(f, Ip, Hp, Sp, 1)
#else
#define Vm(n, ...) gwen_status n(gwen_core f, ##__VA_ARGS__)
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#define YieldStatus GwenStatusEof
#define GwenContinue() Ok
#define Have(n) if (Sp - Hp < n) return gc(f, n)
#define Have1() if (Sp == Hp) return gc(f, 1)
#endif

// thanks !!
typedef gwen_word word, *gwen_stack, *gwen_heap;
typedef union gwen_cell *gwen_cell, *gwen_thread, *cell, *thread;
typedef Vm(gwen_vm);
union gwen_cell {
  gwen_vm *ap;
  gwen_word x;
  gwen_cell m; };

//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.
static struct tag { union gwen_cell *null, *head, end[]; } *ttag(thread k) {
  while (k->ap) k++;
  return (struct tag*) k; }

static gwen_cell mo_ini(gwen_cell _, gwen_size len) {
  struct tag *t = (struct tag*) (_ + len);
  return t->null = NULL, t->head = _; }

typedef bool
  gwen_equal_t(gwen_core, gwen_word, gwen_word);
typedef gwen_word
  gwen_copy_t(gwen_core, gwen_word, gwen_word*, gwen_word*),
  gwen_hash_t(gwen_core, gwen_word);
typedef void
  gwen_evac_t(gwen_core, gwen_word, gwen_word*, gwen_word*),
  gwen_print_t(gwen_core, gwen_file, gwen_word);
// basic data type method table
typedef struct gwen_type {
  gwen_copy_t *copy;
  gwen_evac_t *evac;
  gwen_equal_t *equal;
  gwen_print_t *emit;
  gwen_hash_t *hash;
} *typ;

static gwen_hash_t hash_two, hash_string, hash_symbol, hash_table;
static gwen_copy_t cp_two, copy_string, copy_symbol, copy_table;
static gwen_evac_t wk_two, walk_string, walk_symbol, walk_table;
static gwen_equal_t eq_two, string_equal, not_equal;
static gwen_print_t print_string, print_two, print_table, print_symbol;
static struct gwen_type
  pair_type = { .hash = hash_two, .copy = cp_two, .evac = wk_two, .emit = print_two, .equal = eq_two, },
  string_type = { .hash = hash_string, .copy = copy_string, .evac = walk_string, .emit = print_string, .equal = string_equal, },
  symbol_type = { .hash = hash_symbol, .copy = copy_symbol, .evac = walk_symbol, .equal = not_equal, .emit = print_symbol, },
  table_type = { .hash = hash_table, .copy = copy_table, .evac = walk_table, .equal = not_equal, .emit = print_table, };

// allocated data types other than threads
#define GwenDataHeader() gwen_vm *ap; struct gwen_type *typ
static gwen_vm data;

typedef struct gwen_pair {
  GwenDataHeader();
  gwen_word a, b;
} *gwen_pair, *pair;

static gwen_pair ini_pair(gwen_pair w, gwen_word a, gwen_word b) {
  return w->ap = data, w->typ = &pair_type, w->a = a, w->b = b, w; }

typedef struct gwen_string {
  GwenDataHeader();
  gwen_size len;
  char text[];
} *gwen_string, *string;

static gwen_string ini_str(gwen_string s, gwen_size len) {
  return s->ap = data, s->typ = &string_type, s->len = len, s; }

typedef struct gwen_symbol {
  GwenDataHeader();
  gwen_string nom;
  gwen_word code;
  struct gwen_symbol *l, *r;
} *gwen_symbol, *symbol;

static symbol ini_sym(gwen_symbol y, string nom, uintptr_t code) {
  return y->ap = data, y->typ = &symbol_type, y->nom = nom, y->code = code, y->l = y->r = 0, y; }
static symbol ini_anon(gwen_symbol y, gwen_word code) {
  return y->ap = data, y->typ = &symbol_type, y->nom = 0, y->code = code, y; }

typedef struct gwen_table {
  GwenDataHeader();
  gwen_size len, cap;
  struct gwen_table_entry {
    gwen_word key, val;
    struct gwen_table_entry *next;
  } **tab;
} *gwen_table, *table;

static gwen_table ini_table(gwen_table t, gwen_size len, gwen_size cap, struct gwen_table_entry **tab) {
  return t->ap = data, t->typ = &table_type, t->len = len, t->cap = cap, t->tab = tab, t; }

// runtime core data structure -- 1 core = 1 thread of execution
struct gwen_core {
  // vm registers
  gwen_cell ip;
  gwen_heap hp; // heap pointer
  gwen_stack sp; // stack pointer
  // environment
  gwen_table dict, macro; // global environment and macros
  gwen_symbol symbols; // internal symbols
                  //
  bool (*please)(gwen_core, gwen_size);
  // memory management
  gwen_word len, // size of each pool
            *pool, // on pool
            *loop; // off pool
  struct gwen_mm { // gc save list
    gwen_word *addr; // stack address of value
    struct gwen_mm *next; // prior list
  } *safe;
  union { // gc state
    uintptr_t t0; // end time of last gc
    gwen_heap cp; }; };

_Static_assert(-1 >> 1 == -1, "support sign extended shift");
_Static_assert(sizeof(gwen_word) == sizeof(union gwen_cell), "cell is 1 word wide");

static gwen_pair
  ini_pair(gwen_pair, gwen_word, gwen_word),
  pairof(gwen_core, gwen_word, gwen_word);
static gwen_table
  new_table(gwen_core),
  table_set(gwen_core, gwen_table, gwen_word, gwen_word);
static gwen_symbol
  literal_symbol(gwen_core, const char*),
  intern(gwen_core, gwen_string);
static void
  *bump(gwen_core, gwen_size),
  *cells(gwen_core, gwen_size),
  copy_from(gwen_core, gwen_word*, gwen_size),
  transmit(gwen_core, gwen_file, gwen_word);
static Vm(gc, gwen_size s);
static bool
  gwen_please(gwen_core, gwen_size),
  eql(gwen_core, gwen_word, gwen_word);
static gwen_word
  table_get(gwen_core, gwen_table, gwen_word, gwen_word),
  pushs(gwen_core, gwen_size, ...),
  hash(gwen_core, gwen_word),
  cp(gwen_core, gwen_word, gwen_word*, gwen_word*); // for recursive use by evac functions
static gwen_vm display, bnot, rng, data,
   gensym, ev0,
   Xp, Np, stringp, defmacro,
   ssub, sget, slen, scat,
   prc,
   cons, car, cdr,
   lt, le, eq, gt, ge,
   tset, tget, tdel, tnew, tkeys, tlen,
   seek, peek, poke, trim, thda,
   add, sub, mul, quot, rem,
   curry;

#define Oom GwenStatusOom
#define Ok GwenStatusOk
#define Eof GwenStatusEof
static gwen_status gwen_run(gwen_core f) {
  gwen_status s;
#if GwenCanUseTco
  s = f->ip->ap(f, f->ip, f->hp, f->sp);
#else
  for (s = Ok; s == Ok; s = f->ip->ap(f));
  s = s == Eof ? Ok : s;
#endif
  if (s != Ok) f->ip = 0, f->sp = f->pool + f->len;
  return s; }

#define Width(_) b2w(sizeof(_))
#define avail(f) (f->sp-f->hp)
#define getnum(_) ((gwen_word)(_)>>1)
#define putnum(_) (((gwen_word)(_)<<1)|1)
#define nil putnum(0)
#define MM(f,r) ((f->safe=&((struct gwen_mm){(gwen_word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define A(o) ((gwen_pair)(o))->a
#define B(o) ((gwen_pair)(o))->b
#define nilp(_) ((_)==nil)
#define nump(_) ((gwen_word)(_)&1)
#define homp(_) (!nump(_))
#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define datp(_) (ptr(_)->ap==data)
#define mix ((uintptr_t)2708237354241864315)
#define bind(n, x) if (!(n = (x))) return 0
#define ptr(o) ((cell)(o))
#define dtyp(x) ((typ)ptr(x)[1].m)
static bool strp(gwen_word _) { return homp(_) && dtyp(_) == &string_type; }
static bool twop(gwen_word _) { return homp(_) && dtyp(_) == &pair_type; }
static bool tblp(gwen_word _) { return homp(_) && dtyp(_) == &table_type; }
static bool symp(gwen_word _) { return homp(_) && dtyp(_) == &symbol_type; }

// align bytes up to the nearest word
static size_t b2w(size_t b) {
  size_t q = b / sizeof(gwen_word), r = b % sizeof(gwen_word);
  return q + (r ? 1 : 0); }

#define pop1(f) (*((f)->sp++))
gwen_word gwen_pop1(gwen_core f) { return pop1(f); }


#define S1(i) ((union gwen_cell[]){{i}})
#define S2(i) ((union gwen_cell[]){{curry},{.x=putnum(2)},{i}})
#define S3(i) ((union gwen_cell[]){{curry},{.x=putnum(3)},{i}})
#define ini(_)\
  _("nump", S1(Np)) _("+", S2(add)) _("-", S2(sub)) _("*", S2(mul))\
  _("/", S2(quot)) _("%", S2(rem)) _("<", S2(lt)) _("<=", S2(le))\
  _("=", S2(eq)) _(">=", S2(ge)) _(">", S2(gt))\
  _("twop", S1(Xp)) _("X", S2(cons)) _("A", S1(car)) _("B", S1(cdr))\
  _("strp", S1(stringp)) _("sget", S2(sget)) _("ssub", S3(ssub)) _("slen", S1(slen)) _("scat", S2(scat))\
  _(".", S1(display)) _("putc", S1(prc))\
  _("rand", S1(rng)) _("~", S1(bnot))\
  _("thd", S1(thda)) _("peek", S1(peek)) _("poke", S2(poke)) _("trim", S1(trim)) _("seek", S2(seek))\
  _("tnew", S1(tnew)) _("tkeys", S1(tkeys)) _("tlen", S1(tlen)) _("tset", S3(tset)) _("tget", S3(tget)) _("tdel", S3(tdel))\
  _("gensym", S1(gensym)) _("ev", S1(ev0)) _("::", S2(defmacro))

static NoInline bool gwen_define(gwen_core f, const char *k, gwen_word v) {
  if (!pushs(f, 1, v)) return false;
  gwen_symbol y = literal_symbol(f, k);
  v = pop1(f);
  return y && table_set(f, f->dict, (gwen_word) y, v); }

#define Dict(a, b) {a, b},
static struct { const char *n; gwen_cell v; } ini_dict[] = { ini(Dict) };
gwen_core gwen_open(void) {
  gwen_core f = malloc(sizeof(struct gwen_core));
  if (!f) return f;
  memset(f, 0, sizeof(struct gwen_core));
  const gwen_size len0 = 1;
  gwen_word *pool = malloc(2 * len0 * sizeof(gwen_word));
  if (!pool) return gwen_close(f);
  f->t0 = clock();
  f->sp = f->loop = (f->hp = f->pool = pool) + (f->len = len0);
#define Definition(a, b) gwen_define(f, a, (gwen_word) b) &&
  if (!(f->dict = new_table(f)) ||
      !(f->macro = new_table(f)) ||
      !gwen_define(f, "global-namespace", (gwen_word) f->dict))
    return gwen_close(f);
  for (long i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++)
    if (!gwen_define(f, ini_dict[i].n, (gwen_word) ini_dict[i].v))
      return gwen_close(f);
  return f; }

static NoInline gwen_word pushsr(gwen_core f, gwen_size m, gwen_size n, va_list xs) {
  if (!n) return gwen_please(f, m) ? m : n;
  gwen_word x = va_arg(xs, gwen_word), y;
  avec(f, x, y = pushsr(f, m, n - 1, xs));
  return y ? *--f->sp = x : y; }

static gwen_word pushs(gwen_core f, gwen_size m, ...) {
  va_list xs; va_start(xs, m);
  gwen_word n, r = 0;
  if (avail(f) < m) r = pushsr(f, m, m, xs);
  else for (n = 0, f->sp -= m; n < m; f->sp[n++] = r = va_arg(xs, gwen_word));
  va_end(xs);
  return r; }

////
/// " the parser "
//
//
// get the next significant character from the stream
static int read_char(gwen_core f, gwen_file i) {
  for (int c;;) switch (c = getc(i)) {
    default: return c;
    case '#': case ';': while (!feof(i) && (c = getc(i)) != '\n' && c != '\r');
    case ' ': case '\t': case '\n': case '\r': case '\f': continue; } }

static gwen_word read_str_lit(gwen_core, gwen_file), read_atom(gwen_core, gwen_file);
static gwen_status reads(gwen_core, gwen_file);

static gwen_status enquote(gwen_core f) {
  pair w = pairof(f, f->sp[0], nil);
  if (!w) return Oom;
  f->sp[0] = (gwen_word) w;
  symbol y = literal_symbol(f, "`");
  if (!y) return Oom;
  w = pairof(f, (gwen_word) y, f->sp[0]);
  if (!w) return Oom;
  f->sp[0] = (gwen_word) w;
  return Ok; }

gwen_status gwen_read1f(gwen_core f, gwen_file i) {
  int c = read_char(f, i);
  if (feof(i)) return Eof;
  gwen_word x; switch (c) {
    case '\'': return (c = gwen_read1f(f, i)) == Ok ? enquote(f) : c;
    case '(': return reads(f, i);
    case ')': x = nil; break;
    case '"': x = read_str_lit(f, i); break;
    default: ungetc(c, i), x = read_atom(f, i); }
  return x && pushs(f, 1, x) ? Ok : Oom; }

static gwen_status reads(gwen_core f, gwen_file i) {
  gwen_word c = read_char(f, i);
  if (c == EOF || c == ')') return pushs(f, 1, nil) ? Ok : Oom;
  ungetc(c, i);
  if ((c = gwen_read1f(f, i)) != Ok) return c;
  if ((c = reads(f, i)) != Ok) return c;
  if (!(c = (gwen_word) pairof(f, f->sp[1], f->sp[0]))) return Oom;
  *++f->sp = c;
  return Ok; }

// create and grow buffers for reading
static gwen_string bnew(gwen_core f) {
  string s = cells(f, Width(struct gwen_string) + 1);
  return s ? ini_str(s, sizeof(gwen_word)) : s; }
static gwen_string bgrow(gwen_core f, gwen_string s) {
  gwen_string t; gwen_size len = s->len;
  avec(f, s, t = cells(f, Width(struct gwen_string) + 2 * b2w(len)));
  if (t) memcpy(ini_str(t, 2 * len)->text, s->text, len);
  return t; }

static gwen_word read_str_lit(gwen_core f, gwen_file i) {
  gwen_string b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(gwen_word); b; b = bgrow(f, b), lim *= 2)
    while (n < lim) switch (c = fgetc(i)) {
      case '\\': c = fgetc(i); if (c == EOF)
      case '"': case EOF: goto out;
      default: b->text[n++] = c; } out:
  if (!b) return 0;
  b->len = n;
  return (gwen_word) b; }

static gwen_word read_atom(gwen_core f, gwen_file i) {
  gwen_string b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(gwen_word); b; b = bgrow(f, b), lim *= 2)
    while (n < lim) switch (c = getc(i)) {
      case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
      case '(': case ')': case '"': case '\'': case EOF: ungetc(c, i); goto out;
      default: b->text[n++] = c; } out:
  if (!b) return 0;
  b->len = n, b->text[n] = 0; // zero terminate for strtol ; n < lim so this is safe
  char *e; long j = strtol(b->text, &e, 0);
  return *e == 0 ? putnum(j) : (gwen_word) intern(f, b); }

// end of parser

#define op(n, x) (Ip = (thread) Sp[n], Sp[n] = (x), Sp += n, GwenContinue())
static Vm(prc)     { gwen_word w = *Sp; putchar(getnum(w));     return op(1, w); }
static Vm(display) { gwen_word w = *Sp; transmit(f, stdout, w); return op(1, w); }

static void transmit(gwen_core f, gwen_file out, gwen_word x) {
  if (nump(x)) fprintf(out, "%ld", (long) getnum(x));
  else if (ptr(x)->ap == data) dtyp(x)->emit(f, out, x);
  else fprintf(out, "#%lx", (long) x); }

static NoInline Vm(gc, gwen_size n) {
  Pack(f);
  bool ok = gwen_please(f, n);
  Unpack(f);
  return ok ? GwenContinue() : Oom; }

static void *bump(gwen_core f, size_t n) { void *x = f->hp; return f->hp += n, x; }
static void *cells(gwen_core f, size_t n) { return
  n <= avail(f) || gwen_please(f, n) ? bump(f, n) : 0; }

// garbage collector
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. governs the heap size
// as a side effect by trying to keep
//   v = (t2 - t0) / (t2 - t1)
// between
#define v_lo 8
#define too_little (len1 < req || v < v_lo)
// and
#define v_hi (v_lo << 6)
#define too_big (len1 >> 1 > req && v > v_hi)
// where
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
static NoInline bool gwen_please(gwen_core f, gwen_size req) {
  gwen_word *b0p0 = f->pool, *b0p1 = f->loop;
  f->pool = b0p1, f->loop = b0p0;
  size_t t0 = f->t0, t1 = clock(),
         len0 = f->len;
  // do initial copy to alternate pool
  copy_from(f, b0p0, len0);
  size_t t2 = f->t0 = clock(), // set last gc timestamp
         v = t2 == t1 ? v_hi : (t2 - t0) / (t2 - t1), // speed factor
         len1 = len0; // target size
  // calculate the minimum memory required to be able to return true:
  //    req + used = req + (total - free)
  req += len0 - avail(f);
  // if v is out of bounds calculate new len to compensate assuming v = k*len
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return true; // no change reqired, common case
  // we are going to try and resize
  // allocate a pool with the new target size
  gwen_word *b1p0 = malloc(len1 * 2 * sizeof(gwen_word));
  // if it failed we can still still return true if request was satisfied by original pool
  if (!b1p0) return req <= len0;
  // we got a new pool so copy again
  // reset core variables on new pool
  f->loop = (f->pool = b1p0) + (f->len = len1);
  copy_from(f, b0p1, len0); // do second copy
  free(b0p0 < b0p1 ? b0p0 : b0p1); // free old pool
  f->t0 = clock(); // set last gc timestamp
  return true; } // size successfully adjusted


#define CP(d) ((d) = (void*) cp(f, (gwen_word) (d), p0, t0))
static NoInline void copy_from(gwen_core f, gwen_word *p0, gwen_size len0) {
  gwen_word len1 = f->len, // target pool length
            *p1 = f->pool, // target pool
            *t0 = p0 + len0, // source pool top
            *t1 = p1 + len1, // target pool top
            *sp0 = f->sp, // source pool stack
            sn = t0 - sp0, // stack height
            *sp1 = t1 - sn; // target pool stack
  // reset stack, heap, symbols
  f->sp = sp1, f->hp = f->cp = p1, f->symbols = 0;
  // copy stack
  while (sn--) *sp1++ = cp(f, *sp0++, p0, t0);
  // copy other internal state
  CP(f->ip), CP(f->dict), CP(f->macro);
  // copy protected stack values
  for (struct gwen_mm *r = f->safe; r; r = r->next) *r->addr = cp(f, *r->addr, p0, t0);
  // copy all reachable values using cheney's method
  for (gwen_cell k; (k = (gwen_cell) f->cp) < (gwen_cell) f->hp;)
    if (datp(k)) dtyp(k)->evac(f, (gwen_word) k, p0, t0); // is data
    else { // is thread
      while (k->x) k->x = cp(f, k->x, p0, t0), k++;
      f->cp = (gwen_word*) k + 2; } }

#define bounded(a, b, c) ((gwen_word)(a)<=(gwen_word)(b)&&(gwen_word)(b)<(gwen_word)(c))
static NoInline gwen_word cp(gwen_core v, gwen_word x, gwen_word *p0, gwen_word *t0) {
  // if it's a number or out of managed memory then return it
  if (nump(x) || !bounded(p0, x, t0)) return x;
  gwen_cell src = (gwen_cell) x;
  x = src->x;
  // if the cell holds a pointer to the new space then return the pointer
  if (homp(x) && bounded(v->pool, x, v->pool + v->len)) return x;
  // if it's data then call the given copy function
  if (datp(src)) return dtyp(src)->copy(v, (gwen_word) src, p0, t0);
  // it's a thread, find the end
  struct tag *t = ttag(src);
  thread ini = t->head, d = bump(v, t->end - ini), dst = d;
  for (cell s = ini; (d->x = s->x); s++->x = (gwen_word) d++);
  d[1].ap = (gwen_vm*) dst;
  return (gwen_word) (src - ini + dst); }

static Vm(add) { return op(2, putnum(getnum(Sp[0])+getnum(Sp[1]))); }
static Vm(sub) { return op(2, putnum(getnum(Sp[0])-getnum(Sp[1]))); }
static Vm(mul) { return op(2, putnum(getnum(Sp[0])*getnum(Sp[1]))); }
static Vm(quot) { return op(2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])/getnum(Sp[1]))); }
static Vm(rem) { return op(2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])%getnum(Sp[1]))); }
static Vm(eq) { return op(2, eql(f, Sp[0], Sp[1]) ? putnum(-1) : nil); }
static Vm(lt) { return op(2, Sp[0] < Sp[1] ? putnum(-1) : nil); }
static Vm(le) { return op(2, Sp[0] <= Sp[1] ? putnum(-1) : nil); }
static Vm(gt) { return op(2, Sp[0] > Sp[1] ? putnum(-1) : nil); }
static Vm(ge) { return op(2, Sp[0] >= Sp[1] ? putnum(-1) : nil);}
static Vm(bnot) { return op(1, ~Sp[0] | 1); }
static Vm(rng) { return op(1, putnum(random())); }
static Vm(Xp) { return op(1, twop(Sp[0]) ? putnum(-1) : nil); }
static Vm(Np) { return op(1, nump(Sp[0]) ? putnum(-1) : nil); }
static Vm(stringp) { return op(1, strp(Sp[0]) ? putnum(-1) : nil); }
static bool eql(gwen_core f, gwen_word a, gwen_word b) {
  if (a == b) return true;
  if (nump(a | b) ||
      ptr(a)->ap != data ||
      ptr(b)->ap != data ||
      dtyp(a) != dtyp(b)) return false;
  return dtyp(a)->equal(f, a, b); }

static bool not_equal(gwen_core f, gwen_word a, gwen_word b) { return false; }

static Vm(trim) {
  gwen_thread k = (gwen_thread) Sp[0];
  ttag(k)->head = k;
  return op(1, (gwen_word) k); }

static Vm(seek) {
  gwen_thread k = (gwen_thread) Sp[1];
  return op(2, (gwen_word) (k + getnum(Sp[0]))); }

static Vm(peek) {
  gwen_thread k = (gwen_thread) Sp[0];
  return op(1, k[0].x); }

static Vm(poke) {
  thread k = (thread) Sp[1];
  k->x = Sp[0];
  return op(2, (gwen_word) k); }

static Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct tag));
  gwen_thread k = mo_ini(memset(Hp, -1, n * sizeof(gwen_word)), n);
  Hp += n + Width(struct tag);
  return op(1, (gwen_word) k); }

static gwen_word cp_two(gwen_core v, gwen_word x, gwen_word *p0, gwen_word *t0) {
  gwen_pair src = (pair) x,
            dst = ini_pair(bump(v, Width(struct gwen_pair)), src->a, src->b);
  return (gwen_word) (src->ap = (gwen_vm*) dst); }

static void wk_two(gwen_core f, gwen_word x, gwen_word *p0, gwen_word *t0) {
  f->cp += Width(struct gwen_pair);
  A(x) = cp(f, A(x), p0, t0);
  B(x) = cp(f, B(x), p0, t0); }

static void print_two(gwen_core f, gwen_file o, gwen_word x) {
  for (putc('(', o);; putc(' ', o)) {
    transmit(f, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

// FIXME could overflow the stack -- use off pool for this
static bool eq_two(gwen_core f, gwen_word x, gwen_word y) {
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static gwen_word hash_two(gwen_core v, gwen_word x) {
  gwen_word hc = hash(v, A(x)) * hash(v, B(x));
  return hc ^ mix; }

static gwen_pair pairof(gwen_core f, gwen_word a, gwen_word b) {
  if (avail(f) < Width(struct gwen_pair)) {
    bool ok;
    avec(f, a, avec(f, b, ok = gwen_please(f, Width(struct gwen_pair))));
    if (!ok) return 0; }
  gwen_pair w = (pair) f->hp;
  f->hp += Width(struct gwen_pair);
  return ini_pair(w, a, b); }

static Vm(car) { return op(1, twop(Sp[0]) ? A(Sp[0]) : Sp[0]); }
static Vm(cdr) { return op(1, twop(Sp[0]) ? B(Sp[0]) : nil); }
static Vm(cons) {
  Have(Width(struct gwen_pair));
  gwen_pair w = ini_pair((gwen_pair) Hp, Sp[0], Sp[1]);
  Hp += Width(struct gwen_pair);
  return op(2, (gwen_word) w); }


static gwen_word copy_string(gwen_core v, gwen_word x, gwen_word *p0, gwen_word *t0) {
  gwen_string src = (string) x;
  size_t len = sizeof(struct gwen_string) + src->len;
  return (gwen_word) (src->ap = memcpy(bump(v, b2w(len)), src, len)); }

static void walk_string(gwen_core v, gwen_word x, gwen_word *p0, gwen_word *t0) {
  v->cp += Width(struct gwen_string) + b2w(((string) x)->len); }

static void print_string(gwen_core v, gwen_file o, gwen_word _) {
  gwen_string s = (string) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

static gwen_word hash_string(gwen_core v, gwen_word _) {
  string s = (string) _;
  uintptr_t h = 1;
  size_t words = s->len / sizeof(gwen_word),
         bytes = s->len % sizeof(gwen_word);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const intptr_t *ws = (intptr_t*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static bool string_equal(gwen_core f, gwen_word x, gwen_word y) {
  string a = (string) x, b = (string) y;
  return a->len == b->len && 0 == strncmp(a->text, b->text, a->len); }

static Vm(slen) {
  gwen_word x = Sp[0];
  return op(1, strp(x) ? putnum(((string)x)->len) : nil); }

#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))
static Vm(ssub) {
  gwen_cell r = (gwen_cell) Sp[3];
  if (!strp(Sp[0])) Sp[3] = nil;
  else {
    string s = (string) Sp[0];
    size_t i = nump(Sp[1]) ? getnum(Sp[1]) : 0,
           j = nump(Sp[2]) ? getnum(Sp[2]) : 0;
    i = max(i, 0), j = min(j, s->len);
    Have(Width(struct gwen_string) + b2w(j - i));
    gwen_string t = ini_str((string) Hp, j - i);
    memcpy(t->text, s->text + i, j);
    Sp[3] = (gwen_word) t; }
  Ip = r, Sp += 3;
  return GwenContinue(); }

static Vm(sget) {
  thread r = (thread) Sp[2];
  if (!strp(Sp[0])) Sp[2] = nil;
  else {
    string s = (string) Sp[0];
    size_t i = min(s->len - 1, getnum(Sp[1]));
    i = max(i, 0);
    Sp[2] = putnum(s->text[i]); }
  Ip = r, Sp += 2;
  return GwenContinue(); }

static Vm(scat) {
  gwen_word a = Sp[0], b = Sp[1];
  if (!strp(a)) return op(2, b);
  if (!strp(b)) return op(2, a);
  gwen_string x = (gwen_string) a, y = (gwen_string) b;
  size_t len = x->len + y->len,
         req = Width(struct gwen_string) + b2w(len);
  Have(req);
  string z = ini_str((gwen_string) Hp, len);
  Hp += req;
  memcpy(z->text, x->text, x->len);
  memcpy(z->text + x->len, y->text, y->len);
  return op(2, (gwen_word) z); }

static symbol intern_r(gwen_core, string, symbol*);

static gwen_word hash_symbol(gwen_core v, gwen_word _) {
  return ((gwen_symbol) _)->code; }
static gwen_word copy_symbol(gwen_core f, gwen_word x, gwen_word *p0, gwen_word *t0) {
  symbol src = (symbol) x,
         dst = src->nom ?
           intern_r(f, (string) cp(f, (gwen_word) src->nom, p0, t0), &f->symbols) :
           ini_anon(bump(f, Width(struct gwen_symbol) - 2), src->code);
  return (gwen_word) (src->ap = (gwen_vm*) dst); }
static void walk_symbol(gwen_core f, gwen_word x, gwen_word *p0, gwen_word *t0) {
  f->cp += Width(struct gwen_symbol) - (((symbol)x)->nom ? 0 : 2); }

  /*
static bool atomp(string s) {
  const char cc[] = " \n\t;#()\"'";
  for (size_t i = 0; i < s->len; i++)
    for (const char *c = cc; *c; c++)
      if (s->text[i] == *c) return false;
  return true; }
  */

static void print_symbol(gwen_core f, gwen_file o, gwen_word x) {
  gwen_string s = ((symbol) x)->nom;
  if (s) for (int i = 0; i < s->len; putc(s->text[i++], o));
  else fprintf(o, "#sym@%lx", (long) x); }


static gwen_symbol intern_r(gwen_core v, gwen_string b, gwen_symbol *y) {
  gwen_symbol z = *y;
  if (!z) return *y =
    ini_sym(bump(v, Width(struct gwen_symbol)), b, hash(v, putnum(hash(v, (gwen_word) b))));
  gwen_string a = z->nom;
  int i = a->len < b->len ? -1 :
          a->len > b->len ? 1 :
          strncmp(a->text, b->text, a->len);
  return i == 0 ? z : intern_r(v, b, i < 0 ? &z->l : &z->r); }

static gwen_symbol intern(gwen_core f, gwen_string b) {
  if (avail(f) < Width(struct gwen_symbol)) {
    bool ok;
    avec(f, b, ok = gwen_please(f, Width(struct gwen_symbol)));
    if (!ok) return 0; }
  return intern_r(f, b, &f->symbols); }


static gwen_symbol literal_symbol(gwen_core f, const char *nom) {
  size_t len = strlen(nom);
  gwen_string o = cells(f, Width(struct gwen_string) + b2w(len));
  if (!o) return 0;
  memcpy(o->text, nom, len);
  return intern(f, ini_str(o, len)); }

static Vm(gensym) {
  const int req = Width(struct gwen_symbol) - 2;
  Have(req);
  gwen_symbol y = (gwen_symbol) Hp;
  Hp += req;
  return op(1, (gwen_word) ini_anon(y, random())); }


// FIXME poor hashing method :(
static gwen_word hash_table(gwen_core f, gwen_word h) { return mix; }

static gwen_word copy_table(gwen_core f, gwen_word x, gwen_word *p0, gwen_word *t0) {
  gwen_table src = (table) x;
  gwen_word i = src->cap;
  gwen_table dst = bump(f, Width(struct gwen_table) + i);
  src->ap = (gwen_vm*) ini_table(dst, src->len, src->cap, (void*) (dst + 1));

  //FIXME do these allocations in a block with the rest
  for (struct gwen_table_entry *s, *e, *d; i--; dst->tab[i] = e)
    for (s = src->tab[i], e = NULL; s;
      d = bump(f, Width(struct gwen_table_entry)),
      d->key = s->key, d->val = s->val,
      d->next = e, e = d,
      s = s->next);
  return (gwen_word) dst; }

static void walk_table(gwen_core f, gwen_word x, gwen_word *p0, gwen_word *t0) {
  table t = (table) x;
  f->cp += Width(struct gwen_table) + t->cap + t->len * Width(struct gwen_table_entry);
  for (gwen_word i = 0, lim = t->cap; i < lim; i++)
    for (struct gwen_table_entry *e = t->tab[i]; e;
      e->key = cp(f, e->key, p0, t0),
      e->val = cp(f, e->val, p0, t0),
      e = e->next); }

static void print_table(gwen_core f, gwen_file o, gwen_word x) {
  gwen_table t = (gwen_table) x;
  fprintf(o, "#table:%ld/%ld@%lx", (long) t->len, (long) t->cap, (long) x); }

// this is a totally ad hoc, unproven hashing method.
//
// its performance on hash tables and anonymous functions
// is very bad (they all go to the same bucket!)
//
// strings, symbols, and numbers do better. for pairs it
// depends on what they contain.
//
// copying GC complicates the use of memory addresses for
// hashing mutable data, which is the obvious way to fix
// the bad cases. we would either need to assign each datum
// a unique identifier when it's created & hash using that,
// or use the address but rehash as part of garbage collection.

static gwen_word hash(gwen_core f, gwen_word x) {
  if (nump(x)) {
    const int half_word_bits = sizeof(gwen_word) * 4;
    x *= mix;
    x = (x << half_word_bits) | (x >> half_word_bits);
    return x; }

  if (datp(x)) return dtyp(x)->hash(f, x);
  if (!bounded(f->pool, x, f->pool+f->len)) return mix ^ (mix * x);
  // it's a function, hash by length
  struct tag *t = ttag((gwen_cell) x);
  gwen_word len = (cell) t - t->head;
  return mix ^ (mix * len); }

static gwen_table new_table(gwen_core f) {
  gwen_table t = cells(f, Width(struct gwen_table) + 1);
  if (!t) return t;
  struct gwen_table_entry **tab = (void*) (t + 1);
  tab[0] = 0;
  return ini_table(t, 0, 1, tab); }


static NoInline table table_insert(gwen_core f, gwen_table t, gwen_word k, gwen_word v, gwen_word i) {
  struct gwen_table_entry *e;
  avec(f, t, avec(f, k, avec(f, v, e = cells(f, Width(struct gwen_table_entry)))));
  if (!e) return 0;
  e->key = k, e->val = v, e->next = t->tab[i];
  t->tab[i] = e;
  gwen_word cap0 = t->cap, load = ++t->len / cap0;
  if (load <= 1) return t;
  // grow the table
  struct gwen_table_entry **tab0, **tab1;
  gwen_word cap1 = 2 * cap0;
  avec(f, t, tab1 = cells(f, cap1));
  tab0 = t->tab;
  if (!tab1) return 0;
  memset(tab1, 0, cap1 * sizeof(gwen_word));
  for (gwen_word i; cap0--;)
    for (struct gwen_table_entry *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = (cap1-1) & hash(f, e->key),
      e->next = tab1[i],
      tab1[i] = e);

  t->cap = cap1, t->tab = tab1;
  return t; }

static Inline gwen_word index_of_key(gwen_core f, gwen_table t, gwen_word k) {
  // relies on table capacity being a power of 2
  return (t->cap - 1) & hash(f, k); }

static NoInline table table_set(gwen_core f, gwen_table t, gwen_word k, gwen_word v) {
  gwen_word index = index_of_key(f, t, k);
  struct gwen_table_entry *entry = t->tab[index];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  if (entry) return entry->val = v, t;
  return table_insert(f, t, k, v, index); }

static struct gwen_table_entry *table_delete_r(gwen_core f, gwen_table t, gwen_word k, gwen_word *v, struct gwen_table_entry *e) {
  if (!e) return e;
  if (eql(f, e->key, k)) return t->len--, *v = e->val, e->next;
  return e->next = table_delete_r(f, t, k, v, e->next), e; }

static void table_shrink(gwen_core f, gwen_table t) {
  gwen_word cap = t->cap;
  struct gwen_table_entry *coll = 0, *x, *y; // collect all entries in one list
  for (gwen_word i = 0; i < cap; i++)
    for (x = t->tab[i], t->tab[i] = 0; x;)
      y = x, x = x->next, y->next = coll, coll = y;
  t->cap = cap >>= 2;
  for (gwen_word i; coll;)
    i = (cap - 1) & hash(f, coll->key),
    x = coll->next,
    coll->next = t->tab[i],
    t->tab[i] = coll,
    coll = x; }

static NoInline gwen_word table_delete(gwen_core f, gwen_table t, gwen_word k, gwen_word v) {
  gwen_word idx = index_of_key(f, t, k);
  t->tab[idx] = table_delete_r(f, t, k, &v, t->tab[idx]);
  if (t->len / t->cap > 1) table_shrink(f, t);
  return v; }

static Vm(tnew) {
  Have(Width(struct gwen_table) + 1);
  gwen_table t = (void*) Hp;
  struct gwen_table_entry **tab = (void*) (t + 1);
  Hp += Width(struct gwen_table) + 1;
  tab[0] = 0;
  return op(1, (gwen_word) ini_table(t, 0, 1, tab)); }

static gwen_word table_get(gwen_core f, gwen_table t, gwen_word k, gwen_word zero) {
  struct gwen_table_entry *entry = t->tab[index_of_key(f, t, k)];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  return entry ? entry->val : zero; }

static Vm(tget) {
  return op(3, !tblp(Sp[1]) ? Sp[2] :
    table_get(f, (table) Sp[1], Sp[2], Sp[0])); }

static Vm(tset) {
  gwen_word x = Sp[0];
  if (!tblp(x)) return op(3, nil);
  Pack(f);
  table t = table_set(f, (table) x, Sp[1], Sp[2]);
  Unpack(f);
  return !t ? Oom : op(3, Sp[2]); }

static Vm(tdel) {
  gwen_word x = Sp[1];
  return op(3, !tblp(x) ? nil :
    table_delete(f, (table) x, Sp[2], Sp[0])); }

static Vm(tlen) {
  gwen_word x = Sp[0];
  if (!tblp(x)) return op(1, nil);
  gwen_table t = (gwen_table) x;
  return op(1, putnum(t->len)); }

static Vm(tkeys) {
  if (!tblp(Sp[0])) return op(1, nil);
  gwen_table t = (table) Sp[0];
  gwen_word len = t->len, list = nil;
  Have(len * Width(struct gwen_pair));
  gwen_pair pairs = (gwen_pair) Hp;
  Hp += len * Width(struct gwen_pair);
  for (int i = t->cap; i;)
    for (struct gwen_table_entry *e = t->tab[--i]; e; e = e->next)
      pairs->ap = data, pairs->typ = &pair_type,
      pairs->a = e->key, pairs->b = list,
      list = (gwen_word) pairs, pairs++;
  return op(1, list); }


static thread mo_n(gwen_core, size_t);
// index of item in list
static long lidx(gwen_core f, gwen_word l, gwen_word x) {
  for (long i = 0; twop(l); l = B(l), i++) if (eql(f, A(l), x)) return i;
  return -1; }
// list length
static size_t llen(gwen_word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }
static gwen_word
  lassoc(gwen_core, gwen_word, gwen_word),
  lconcat(gwen_core, gwen_word, gwen_word),
  rlconcat(gwen_core, gwen_word, gwen_word);

// at all times vm is running a function. thread compiler tracks
// function state using this type
typedef struct scope {
  // these parameters represent stack state at a point in compile process
  gwen_word args, // list // function positional arguments (never empty)
       imps, // list // closure variables
       pals; // list // current state of stack
  // these are values of variables known at compile time
  gwen_word lams; // dict // known function definitions
  // these are two stacks of jump target addresses for conditional expressions
  gwen_word alts, // list // alternate branch address stack
       ends; // list // exit branch address stack
  // this is the enclosing function scope if any
  struct scope *par;
} *scope;

static gwen_vm ref, cond, jump, K, yield, ret, ap, apn, tap, tapn;
// thread compiler operates in two phases
//
// 1. analyze phase: analyze expression; assemble constructor on stack; compute code size bound
#define C0(n, ...) size_t n(gwen_core f, scope *c, size_t m, gwen_word x, ##__VA_ARGS__)
typedef C0(c0);
// and
// - generate phase: allocate thread; call constructor; trim thread
#define C1(n, ...) thread n(gwen_core f, scope *c, thread k, ##__VA_ARGS__)
typedef C1(c1);


// scope constructor
static scope enscope(gwen_core f, scope par, gwen_word args, gwen_word imps) {
  if (!pushs(f, 3, args, imps, par)) return 0;
  scope c = (scope) mo_n(f, 7);
  if (c)
    c->args = pop1(f), c->imps = pop1(f),
    c->par = (scope) pop1(f),
    c->pals = c->alts = c->ends = nil,
    c->lams = nil;
  return c; }

static c0 analyze_if, analyze_let, analyze_arguments, analyze_list;
static c1 c1apn, c1var, c2var;
// basic functions
static C1(yieldk) { return k; }
static C1(pull) { return ((c1*) (*f->sp++))(f, c, k); }
static thread construct(gwen_core f, scope *c, size_t m) {
  thread k = mo_n(f, m);
  if (!k) return k;
  memset(k, -1, m * sizeof(gwen_word));
  return pull(f, c, k + m); }
static C1(c1i) { return k[-1].x = *f->sp++, pull(f, c, k - 1); }
static C1(c1ix) { return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull(f, c, k - 2); }
// generic instruction c0 handlers
static size_t em1(gwen_core f, scope *c, size_t m, gwen_vm *i) {
  return pushs(f, 2, c1i, i) ? m + 1 : 0; }
static size_t em2(gwen_core f, scope *c, size_t m, gwen_vm *i, gwen_word x) {
  return pushs(f, 3, c1ix, i, x) ? m + 2 : 0; }
// analyzer

static NoInline size_t analyze_symbol(gwen_core, scope*, size_t, gwen_word, scope);
static C0(analyze) {
  if (homp(x) && ptr(x)->ap == data) {
    typ y = dtyp(x);
    if (y == &pair_type) return analyze_list(f, c, m, x);
    if (y == &symbol_type) return analyze_symbol(f, c, m, x, *c); }
  return em2(f, c, m, K, x); }

static gwen_vm lazy_bind, drop, define, top_bind;
static C0(analyze_variable_reference) {
  return nilp((gwen_word) (*c)->par) ? em2(f, c, m, top_bind, x) : // XXX undefined case
         pushs(f, 3, c1var, x, (*c)->pals) ? m + 2 :
         0; }

static long index_of(gwen_core f, scope c, gwen_word var) {
  size_t i = 0;
  // is it a closure variable?
  for (gwen_word l = c->imps; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  for (gwen_word l = c->args; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  return -1; }


static C1(c2var) {
  gwen_word var = *f->sp++, pals = *f->sp++;
  size_t i = lidx(f, pals, var);
  return
    k[-2].ap = ref,
    k[-1].x = putnum(i),
    pull(f, c, k - 2); }

// emit stack reference instruction
static C1(c1var) {
  gwen_word var = *f->sp++, // variable name
       ins = llen(*f->sp++), // stack inset
       idx = index_of(f, *c, var);
  return
    k[-2].ap = ref,
    k[-1].x = putnum(idx + ins),
    pull(f, c, k - 2); }

static C0(analyze_symbol, scope d) {
  gwen_word y;
  if (nilp((gwen_word) d)) {
    y = table_get(f, f->dict, x, 0);
    if (y) return em2(f, c, m, K, y);
    x = (gwen_word) pairof(f, x, (*c)->imps),
    x = x ? A((*c)->imps = x) : x;
    return x ? analyze_variable_reference(f, c, m, x) : 0; }

  // look in vals
  if ((y = lassoc(f, d->lams, x))) {
    // lazy bind
    bind(y, (gwen_word) pairof(f, y, (gwen_word) d));
    bind(m, em2(f, c, m, lazy_bind, y));
    y = B(B(A(f->sp[2]))); // get the closure args to pass in
    return analyze_arguments(f, c, m, y); } // XXX

  // look in pals
  if ((y = lidx(f, d->pals, x)) >= 0)
    return pushs(f, 3, c2var, x, d->pals) ? m + 2 : 0;

  // look in imps args
  y = index_of(f, d, x);
  if (y >= 0) {
    if (*c != d)
      x = (gwen_word) pairof(f, x, (*c)->imps),
      x = x ? A((*c)->imps = x) : x;
    return x ? analyze_variable_reference(f, c, m, x) : 0; }
  // recur on outer scope
  return analyze_symbol(f, c, m, x, d->par); }

// emits call instruction and modifies to tail call
// if next operation is return
static C1(c1ap) {
  if (k->ap == ret) k->ap = tap; // tail call
  else (--k)->ap = ap; // regular call
  return pull(f, c, k); } // ok

static C1(c1apn) {
  gwen_word n = *f->sp++;
  if (k->ap == ret) k->x = n, (--k)->ap = tapn;
  else (--k)->x = n, (--k)->ap = apn;
  return pull(f, c, k); }

// evaluate function call arguments and apply
static size_t analyze_arguments(gwen_core f, scope *c, size_t m, gwen_word x) {
  MM(f, &x); // handle oom here ..
  if (!((*c)->pals = (gwen_word) pairof(f, nil, (*c)->pals))) m = 0;
  else {
    for (; m && twop(x); x = B(x))
      m = analyze(f, c, m + 1, A(x)),
      m = m && pushs(f, 1, c1ap) ? m : 0;
    (*c)->pals = B((*c)->pals); }
  UM(f);
  return m; }

// lambda decons pushes last list item to stack returns init of list
static gwen_word linit(gwen_core f, gwen_word x) {
  if (!twop(x)) return pushs(f, 1, nil) ? nil : 0;
  if (!twop(B(x))) return pushs(f, 1, A(x)) ? nil : 0;
  word y = A(x);
  return avec(f, y, x = linit(f, B(x))),
         x ? (word) pairof(f, y, x) : x; }

static word analyze_lambda(gwen_core f, scope *c, word imps, word exp) {
  // storing exp in scope->args for the moment is expedient
  scope d = enscope(f, *c, exp, imps);
  if (!d) return 0;
  MM(f, &d);
  // get the real args
  word args = linit(f, d->args);
  if (!(d->args = args)) goto fail;
  exp = f->sp[0], f->sp[0] = (word) yieldk;
  size_t m = analyze(f, &d, 4, exp);
  if (!m) goto fail;
  size_t arity = llen(d->args) + llen(d->imps);
  thread k = pushs(f, 3, c1ix, ret, putnum(arity)) ? construct(f, &d, m) : 0;
  if (!k) goto fail;
  if (arity > 1) (--k)->x = putnum(arity), (--k)->ap = curry;
  ttag(k)->head = k;
  UM(f);
  return (word) pairof(f, (word) k, d->imps);
fail:
  UM(f);
  return 0; }


static Vm(defmacro) {
  Pack(f);
  if (!table_set(f, f->macro, Sp[0], Sp[1])) return Oom;
  Unpack(f);
  return op(2, Sp[1]); }

static Vm(data) {
  gwen_word r = (word) Ip;
  return op(1, r); }

static Vm(K) { Have1();
  *--Sp = Ip[1].x;
  Ip += 2;
  return GwenContinue(); }

static Vm(jump) {
  Ip = Ip[1].m;
  return GwenContinue(); }

static Vm(cond) {
  Ip = nilp(*Sp) ? Ip[1].m : Ip + 2,
  Sp++;
  return GwenContinue(); }

static Vm(ref) { Have1();
  Sp[-1] = Sp[getnum(Ip[1].x)];
  Sp--;
  Ip += 2;
  return GwenContinue(); }

static Vm(ret) {
  word n = getnum(Ip[1].x) + 1;
  return op(n, *Sp); }

static Vm(yield) { Pack(f); return YieldStatus; }

static Vm(ap) {
  if (nump(Sp[1])) return Ip++, Sp++, GwenContinue();
  thread k = (thread) Sp[1];
  Sp[1] = (gwen_word) (Ip + 1);
  Ip = k;
  return GwenContinue(); }

static Vm(apn) {
  size_t n = getnum(Ip[1].x);
  thread ra = Ip + 2; // return address
  Ip = ((thread) Sp[n]) + 2; // only used by let form so will not be num
  Sp[n] = (word) ra; // store return address
  return GwenContinue(); }

static Vm(tap) {
  word x = Sp[0], j = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) return op(1, j);
  Ip = (thread) j;
  *Sp = x;
  return GwenContinue(); }

static Vm(tapn) {
  size_t n = getnum(Ip[1].x),
         r = getnum(Ip[2].x);
  Ip = ((thread) Sp[n]) + 2;
  gwen_stack osp = Sp;
  Sp += r + 1;
  while (n--) Sp[n] = osp[n];
  return GwenContinue(); }

static Vm(Kj) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return GwenContinue(); }

static Vm(curry) {
  thread k;
  size_t n = getnum(Ip[1].x),
         S = 3 + Width(struct tag);
  if (n == 2) {
    Have(S);
    k = (thread) Hp;
    k[0].ap = Kj, k[1].x = *Sp++, k[2].m = Ip + 2;
    k[3].x = 0,   k[4].m = k; }
  else {
    S += 2;
    Have(S);
    k = (thread) Hp;
    k[0].ap = curry, k[1].x = putnum(n - 1);
    k[2].ap = Kj,  k[3].x = *Sp++, k[4].m = Ip + 2;
    k[5].x = 0,    k[6].m = k; }
  Hp += S;
  Ip = (thread) *Sp;
  *Sp = (word) k;
  return GwenContinue(); }

// conditionals
// to emit targeted jumps etc
static c1
  generate_cond_push_branch,
  generate_cond_pop_branch,
  generate_cond_push_exit,
  generate_cond_pop_exit,
  generate_cond_peek_exit;

// conditional expression analyzer
static C0(analyze_if) {
  if (!pushs(f, 2, x, generate_cond_pop_exit)) return 0;
  struct gwen_pair p = { data, &pair_type, nil, nil };
  for (x = pop1(f), MM(f, &x); m; x = B(B(x))) {
    if (!twop(x)) x = (word) &p;
    m = analyze(f, c, m + 2, A(x));
    if (!twop(B(x))) { // at end, default branch
      m = pushs(f, 1, generate_cond_peek_exit) ? m : 0;
      break; }
    m = pushs(f, 1, generate_cond_pop_branch) ? m : 0;
    m = m ? analyze(f, c, m + 2, A(B(x))) : m;
    m = pushs(f, 2, generate_cond_push_branch, generate_cond_peek_exit) ? m : 0; }
  return UM(f), m && pushs(f, 1, generate_cond_push_exit) ? m : 0; }

// first emitter called for cond expression
// pushes cond expression exit address onto scope stack ends
static C1(generate_cond_push_exit) {
  pair w = pairof(f, (word) k, (*c)->ends);
  return !w ? 0 : pull(f, c, (thread) A((*c)->ends = (word) w)); }

// last emitter called for cond expression
// pops cond expression exit address off scope stack ends
static C1(generate_cond_pop_exit) {
  return (*c)->ends = B((*c)->ends), pull(f, c, k); }

static C1(generate_cond_push_branch) {
  pair w = pairof(f, (word) k, (*c)->alts);
  if (!w) return (thread) w;
  (*c)->alts = (word) w;
  k = (thread) w->a;
  return pull(f, c, k); }

static C1(generate_cond_peek_exit) {
  k -= 2;
  thread addr = (cell) A((*c)->ends);
  // if the destination is a return or tail call,
  // then copy it forward instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k[0].ap = addr[0].ap, k[1].x = addr[1].x;
  else k[0].ap = jump, k[1].x = (word) addr;
  return pull(f, c, k); }

// last emitter called for a branch
// pops next branch address off scope stack alts
static C1(generate_cond_pop_branch) {
  return k[-2].ap = cond,
         k[-1].x = A((*c)->alts),
         (*c)->alts = B((*c)->alts),
         pull(f, c, k - 2); }

static bool lambp(gwen_core f, word x) {
  if (!twop(x) || !symp(x = A(x))) return false;
  string s = ((symbol) x)->nom;
  return s && s->len == 1 && s->text[0] == '\\'; }

// DEFINE
// let expressions

static word ldels(gwen_core f, word lam, word l) {
  if (!twop(l)) return nil;
  word m = ldels(f, lam, B(l));
  if (!lassoc(f, lam, A(l))) B(l) = m, m = l;
  return m; }

static word desugr(gwen_core f, word *d, word *e, word a) {
  if (!twop(a)) return (word) pairof(f, *e, nil);
  word b; avec(f, a, b = desugr(f, d, e, B(a)));
  return !b ? b : (word) pairof(f, A(a), b); }

static gwen_status desug(gwen_core f, word *d, word *e) {
  if (!twop(*d)) return Ok;
  word x, l = (word) literal_symbol(f, "\\");
  if (!l || !pushs(f, 1, l)) return Oom;
  do if (!(x = (word) desugr(f, d, e, B(*d))) ||
         !(x = (word) pairof(f, f->sp[0], x)))
    return Oom;
  else *d = A(*d), *e = x;
  while (twop(*d));
  return f->sp++, Ok; }

// this function is loooong
static size_t analyze_let_l(gwen_core f, scope *b, scope *c, size_t m, word exp) {
  if (!twop(exp)) return nil;
  if (!twop(B(exp))) return A(exp);
  // lots of variables :(
  word nom = nil, def = nil, lam = nil,
       v = nil, d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &exp), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v);

  // collect vars and defs into two lists
  for (; twop(exp) && twop(B(exp)); exp = B(B(exp))) {
    d = A(exp), e = A(B(exp)), desug(f, &d, &e);
    if (!(nom = (word) pairof(f, d, nom)) ||
        !(def = (word) pairof(f, e, def)))
      goto fail;
    else if (lambp(f, A(def))) {
      // if it's a lambda compile it and record in lam list
      word x = analyze_lambda(f, c, nil, B(A(def)));
      x = x ? (word) pairof(f, A(nom), x) : x;
      x = x ? (word) pairof(f, x, lam) : x;
      if (x) lam = x;
      else goto fail; } }

  // if there's no body then use the last definition
  bool even = !twop(exp);
  if (even) {
    word x = (word) pairof(f, A(nom), nil);
    if (!x) goto fail;
    exp = x; }

  // find closures
  // for each function f with closure C(f)
  // for each function g with closure C(g)
  // if f in C(g) then C(g) include C(f)
  long j;
  do for (j = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
    for (e = lam; twop(e); e = B(e)) // for each bound function variable
      if (A(A(d)) != A(A(e)) && // skip yourself
          lidx(f, B(B(A(e))), A(A(d))) >= 0) // if you need this function
        for (word v = B(A(d)); twop(v); v = B(v)) { // then you need its variables
          word vars = B(B(A(e))), var = A(v);
          if (lidx(f, vars, var) < 0 && !(vars = (word) pairof(f, var, vars))) goto fail; // oom
          else if (vars != B(B(A(e)))) B(B(A(e))) = vars, j++; } // if list is updated then record the change
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); e = B(e)) B(B(A(e))) = ldels(f, lam, B(B(A(e))));

  (*c)->lams = lam, e = nil;
  // construct lambda with reversed argument list
  exp = lconcat(f, nom, exp);
  symbol l = literal_symbol(f, "\\"); // XXX change to symbol
  exp = exp && l ? (word) pairof(f, (word) l, exp) : 0;
  if (!exp) goto fail;
  // exp is now the required lambda expression, analyze it
  m = analyze(f, b, m, exp);
  if (!m) goto fail;
  if (!((*b)->pals = (word) pairof(f, nil, (*b)->pals))) goto fail;
  // now evaluate definitions in order tracking var names on pals list
  // first reverse the nom and def lists
  nom = rlconcat(f, nom, nil), def = rlconcat(f, def, nil);
  size_t nn = 0;
  // store lambdas on scope for lazy binding and construct new lambda application expression
  // - reverse noms onto exp
  // - reverse expressions onto e = nil and recompile lambdas
  for (; twop(nom); nom = B(nom), def = B(def), nn++) {
    // if lambda then recompile with the explicit closure
    // and put in arg list and lam list (latter is used for lazy binding)
    if (lambp(f, A(def))) {
      d = lassoc(f, lam, A(nom));
      word _;
      if (!(_ = analyze_lambda(f, c, B(B(d)), B(A(def))))) goto fail;
      else A(def) = B(d) = _; }
    // if toplevel then bind
    if (even && nilp((*b)->args)) {
      thread t = cells(f, 2 * Width(struct gwen_pair) + 2 + Width(struct tag));
      if (!t) goto fail;
      gwen_pair w = (gwen_pair) t,
                x = w + 1;
      t += 2 * Width(struct gwen_pair);
      t[0].ap = define, t[1].x = A(nom), t[2].x = 0, t[3].m = t;
      ini_pair(w, A(def), nil); // dict add
      ini_pair(x, (word) t, (word) w);
      A(def) = (word) x; }
    if (!(m = analyze(f, b, m, A(def))) ||
        !((*b)->pals = (word) pairof(f, A(nom), (*b)->pals)))
      goto fail; }
  if (nn > 1) m = pushs(f, 2, c1apn, putnum(nn)) ? m + 2 : 0;
  else m = pushs(f, 1, c1ap) ? m + 1 : 0;
  if (m) for (nn++; nn--; (*b)->pals = B((*b)->pals));
done: return UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), m;
fail: m = 0; goto done; }

static size_t analyze_let(gwen_core f, scope *c, size_t m, word x) {
  scope d = *c;
  avec(f, x, d = enscope(f, d, d->args, d->imps));
  avec(f, d, m = analyze_let_l(f, c, &d, m, x));
  return m; }

static size_t analyze_sequence(gwen_core f, scope *c, size_t m, word x) {
  if (!twop(x)) return em2(f, c, m, K, nil);
  for (MM(f, &x); m && twop(B(x)); x = B(x))
    m = analyze(f, c, m, A(x)),
    m = m ? em1(f, c, m, drop) : m;
  return UM(f), m ? analyze(f, c, m, A(x)) : m; }

static C0(analyze_macro, word b) {
  if (!pushs(f, 2, x, b)) return 0;
  x = (word) literal_symbol(f, "`"); // XXX change to symbol
  if (!x || !pushs(f, 1, x)) return 0;
  pair mxp = (pair) cells(f, 4 * Width(struct gwen_pair));
  if (!mxp) return 0;
  x = (word) ini_pair(mxp, f->sp[1], (word) ini_pair(mxp+1, (word) ini_pair(mxp+2, f->sp[0], (word) ini_pair(mxp+3, f->sp[2], nil)), nil));
  f->sp += 2, *f->sp = x;
  return gwen_eval(f) != Ok ? 0 : analyze(f, c, m, pop1(f)); }

static C0(analyze_list) {
  word a = A(x), b = B(x);
  if (!twop(b)) return analyze(f, c, m, a); // singleton list has value of first element
  if (symp(a)) {
    word macro = table_get(f, f->macro, a, 0);
    if (macro) return analyze_macro(f, c, m, macro, b);
    string n = ((symbol) a)->nom;
    if (n && n->len == 1)
      switch (n->text[0]) { // special form?
        case '`': return em2(f, c, m, K, twop(b) ? A(b) : nil); // quote
        case ',': return analyze_sequence(f, c, m, b); // sequence
        case ':': return analyze_let(f, c, m, b);
        case '?': return analyze_if(f, c, m, b);
        case '\\': return (x = analyze_lambda(f, c, nil, b)) ? analyze(f, c, m, x) : x; } }
  avec(f, b, m = analyze(f, c, m, a));
  return m ? analyze_arguments(f, c, m, b) : m; }

static Vm(drop) {
  return Ip++, Sp++, GwenContinue(); }

static Vm(define) {
  Pack(f);
  if (!table_set(f, f->dict, Ip[1].x, Sp[0])) return Oom;
  Unpack(f);
  return op(1, Sp[0]); }

static Vm(ev0) {
  Pack(f);
  gwen_status s = gwen_eval(f);
  Unpack(f);
  return s != Ok ? s : op(1, *Sp); }

static Vm(top_bind) {
  gwen_word var = Ip[1].x;
  var = table_get(f, f->dict, var, var);
  Ip[0].ap = K;
  Ip[1].x = var;
  return GwenContinue(); }

static Vm(lazy_bind) {
  gwen_word ref = Ip[1].x, var = A(A(ref));
  scope env = (scope) B(ref);
  var = A(B(lassoc(f, env->lams, var)));
  Ip[0].ap = K;
  Ip[1].x = var;
  return GwenContinue(); }

static word lassoc(gwen_core f, word l, word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, A(A(l)))) return A(l);
  return 0; }

// list concat
static word lconcat(gwen_core f, word l, word n) {
  if (!twop(l)) return n;
  avec(f, l, n = lconcat(f, B(l), n));
  return n ? (word) pairof(f, A(l), n) : n; }

// reverse list concat
static word rlconcat(gwen_core f, word l, word n) {
  for (word m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
  return n; }

// allocate a thread
static gwen_thread mo_n(gwen_core f, gwen_size n) {
  gwen_thread k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

// compile and execute expression
NoInline gwen_status gwen_eval(gwen_core f) {
  scope c = enscope(f, (scope) nil, nil, nil);
  if (!c) return Oom;
  gwen_word x = f->sp[0];
  f->sp[0] = (gwen_word) yieldk;
  thread k = 0;
  size_t m = 1;
  avec(f, c,
    m = analyze(f, &c, m, x),
    m = m ? em1(f, &c, m, yield) : m,
    k = m ? construct(f, &c, m) : k);
  k = k ? (gwen_cell) pushs(f, 1, k) : k;
  if (!k) return Oom;
  f->sp[0] = (gwen_word) f->ip;
  f->ip = k;
  gwen_status s = gwen_run(f);
  if (s == Ok)
    x = f->sp[0],
    f->ip = (thread) *++f->sp,
    f->sp[0] = x;
  return s; }

gwen_core gwen_close(gwen_core f) {
  if (f) free(f->pool < f->loop ? f->pool : f->loop), free(f);
  return NULL; }

void gwen_write1f(gwen_core f, gwen_file out) {
  transmit(f, out, f->sp[0]); }
