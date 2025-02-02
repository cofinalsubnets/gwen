#include "p.h"
#include <stdarg.h> // this is a freestanding header

// these are not freestanding headers
#include <time.h>
#include <string.h>
#include <stdlib.h>

typedef intptr_t PWord, *PStack, *PHeap;

// theres a big benefit in speed from tail call optimization but not all platforms support it
#ifndef TCO
#define TCO 1 // on by default
#endif

#if TCO
#define Vm(n, ...) PStatus n(PCore *f, PCell* Ip, PHeap Hp, PStack Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define YieldStatus PStatusOk
#define Continue() Ip->ap(f, Ip, Hp, Sp)
#define Have(n) if (Sp - Hp < n) return gc(f, Ip, Hp, Sp, n)
#define Have1() if (Sp == Hp) return gc(f, Ip, Hp, Sp, 1)
#else
#define Vm(n, ...) PStatus n(PCore *f, ##__VA_ARGS__)
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#define YieldStatus PStatusEof
#define Continue() Ok
#define Have(n) if (Sp - Hp < n) return gc(f, n)
#define Have1() if (Sp == Hp) return gc(f, 1)
#endif


// thanks !!
typedef PWord *PStack, *PHeap;
typedef union PCell PCell;
typedef Vm(PVm);
union PCell {
  PVm *ap;
  PWord x;
  PCell *m; };

typedef bool
  p_equal_t(PCore*, PWord, PWord);
typedef PWord
  p_copy_t(PCore*, PWord, PWord*, PWord*),
  p_hash_t(PCore*, PWord);
typedef void
  p_evac_t(PCore*, PWord, PWord*, PWord*, PHeap*),
  p_print_t(PCore*, p_file, PWord);

// basic data type method table
typedef struct PType {
  p_copy_t *copy;
  p_evac_t *evac;
  p_equal_t *equal;
  p_print_t *emit;
  p_hash_t *hash;
} PType;

#define PDataHeader PVm *ap; PType *typ
typedef struct PPair {
  PDataHeader;
  PWord a, b;
} PPair;


typedef struct PString {
  PDataHeader;
  uintptr_t len;
  char text[];
} PString;
typedef struct PSymbol {
  PDataHeader;
  PString *nom;
  PWord code;
  struct PSymbol *l, *r;
} PSymbol;

typedef struct PTableEntry {
  PWord key, val;
  struct PTableEntry *next;
} PTableEntry;
typedef struct PTable {
  PDataHeader;
  uintptr_t len, cap;
  PTableEntry **tab;
} PTable;
typedef struct PMm {
  PWord *addr;
  struct PMm *next;
} PMm;

// runtime core data structure -- 1 core = 1 thread of execution
struct PCore {
  // vm registers
  PCell *ip;
  PHeap  hp; // heap pointer
  PStack sp; // stack pointer
  // environment
  PTable *dict,
            *macro; // global environment and macros
  PSymbol *symbols; // internal symbols
  // memory management
  PWord len,
           *pool,
           *loop; // memory pool size and pointers
  uintptr_t t0; // end time of last gc
  PMm *safe; };

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
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
static struct tag { PCell *null, *head, end[]; } *ttag(PCell *k) {
  while (k->x) k++;
  return (struct tag*) k; }

static Inline PCell* mo_ini(PCell* _, uintptr_t len) {
  struct tag *t = (struct tag*) (_ + len);
  return t->null = NULL, t->head = _; }


static p_hash_t hash_two, hash_string, hash_symbol, hash_table;
static p_copy_t cp_two, copy_string, copy_symbol, copy_table;
static p_evac_t wk_two, walk_string, walk_symbol, walk_table;
static p_equal_t eq_two, string_equal, not_equal;
static p_print_t print_string, print_two, print_table, print_symbol;


// allocated data types other than threads
static PVm data;

_Static_assert(-1 >> 1 == -1, "support sign extended shift");
_Static_assert(sizeof(PWord) == sizeof(PCell), "cell is 1 word wide");

static PPair
  *pairof(PCore*, PWord, PWord);
static PTable
  *new_table(PCore*),
  *table_set(PCore*, PTable*, PWord, PWord);
static PSymbol
  *literal_symbol(PCore*, const char*),
  *intern(PCore*, PString*);
static void
  *bump(PCore*, uintptr_t),
  *cells(PCore*, uintptr_t),
  copy_from(PCore*, PWord*, uintptr_t),
  transmit(PCore*, PFile*, PWord);
static Vm(gc, uintptr_t s);
static bool
  p_please(PCore*, uintptr_t),
  eql(PCore*, PWord, PWord);
static PWord
  table_get(PCore*, PTable*, PWord, PWord),
  pushs(PCore*, uintptr_t, ...),
  hash(PCore*, PWord),
  cp(PCore*, PWord, PWord*, PWord*); // for recursive use by evac functions
static PVm display, bnot, rng, data,
   gensym, ev0, pairp, fixnump, symbolp, stringp, defmacro,
   ssub, sget, slen, scat, prc, error, cons, car, cdr,
   lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
   seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
   curry;

#define Oom PStatusOom
#define Ok PStatusOk
#define Eof PStatusEof

#define Width(_) b2w(sizeof(_))
#define avail(f) (f->sp-f->hp)
#define getnum(_) ((PWord)(_)>>1)
#define putnum(_) (((PWord)(_)<<1)|1)
#define nil putnum(0)
#define MM(f,r) ((f->safe=&((PMm){(PWord*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define A(o) ((PPair*)(o))->a
#define B(o) ((PPair*)(o))->b
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define BBA(o) B(B(A(o)))
#define nilp(_) ((_)==nil)
#define nump(_) ((PWord)(_)&1)
#define homp(_) (!nump(_))
#define datp(_) (ptr(_)->ap==data)
#define mix ((uintptr_t)2708237354241864315)
#define bind(n, x) if (!(n = (x))) return 0
#define ptr(o) ((PCell*)(o))
#define dtyp(x) ((PType*)ptr(x)[1].m)
#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))

static PType
  pair_type = { .hash = hash_two, .copy = cp_two, .evac = wk_two, .emit = print_two, .equal = eq_two, },
  string_type = { .hash = hash_string, .copy = copy_string, .evac = walk_string, .emit = print_string, .equal = string_equal, },
  symbol_type = { .hash = hash_symbol, .copy = copy_symbol, .evac = walk_symbol, .equal = not_equal, .emit = print_symbol, },
  table_type = { .hash = hash_table, .copy = copy_table, .evac = walk_table, .equal = not_equal, .emit = print_table, };

static Inline bool strp(PWord _) { return homp(_) && dtyp(_) == &string_type; }
static Inline bool twop(PWord _) { return homp(_) && dtyp(_) == &pair_type; }
static Inline bool tblp(PWord _) { return homp(_) && dtyp(_) == &table_type; }
static Inline bool symp(PWord _) { return homp(_) && dtyp(_) == &symbol_type; }

static Inline PPair *ini_pair(PPair *w, PWord a, PWord b) {
  return w->ap = data, w->typ = &pair_type, w->a = a, w->b = b, w; }

static Inline PString* ini_str(PString* s, uintptr_t len) {
  return s->ap = data, s->typ = &string_type, s->len = len, s; }

static Inline PSymbol* ini_sym(PSymbol* y, PString*nom, uintptr_t code) {
  return y->ap = data, y->typ = &symbol_type, y->nom = nom, y->code = code, y->l = y->r = 0, y; }
static Inline PSymbol* ini_anon(PSymbol* y, PWord code) {
  return y->ap = data, y->typ = &symbol_type, y->nom = 0, y->code = code, y; }

static Inline PTable *ini_table(PTable *t, uintptr_t len, uintptr_t cap, PTableEntry **tab) {
  return t->ap = data, t->typ = &table_type, t->len = len, t->cap = cap, t->tab = tab, t; }


// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(PWord), r = b % sizeof(PWord);
  return q + (r ? 1 : 0); }

static Inline PWord pop1(PCore *f) { return *f->sp++; }
static Inline size_t stack_height(PCore *f) { return f->pool + f->len - f->sp; }
size_t p_drop(PCore *f, size_t n) {
  size_t h = stack_height(f);
  n = min(n, h);
  f->sp += n;
  return n; }


#define S1(i) ((PCell[]){{i}})
#define S2(i) ((PCell[]){{curry},{.x=putnum(2)},{i}})
#define S3(i) ((PCell[]){{curry},{.x=putnum(3)},{i}})
#define ini(_)\
  _("+", S2(add)) _("-", S2(sub)) _("*", S2(mul))\
  _("/", S2(quot)) _("%", S2(rem)) _("<", S2(lt)) _("<=", S2(le))\
  _("=", S2(eq)) _(">=", S2(ge)) _(">", S2(gt))\
  _("twop", S1(pairp)) _("X", S2(cons)) _("A", S1(car)) _("B", S1(cdr))\
  _("strp", S1(stringp)) _("symp", S1(symbolp)) _("nump", S1(fixnump))\
  _("sget", S2(sget)) _("ssub", S3(ssub)) _("slen", S1(slen)) _("scat", S2(scat))\
  _("error", S1(error))\
  _(".", S1(display)) _("putc", S1(prc))\
  _("rand", S1(rng)) _("~", S1(bnot))\
  _("thd", S1(thda)) _("peek", S1(peek)) _("poke", S2(poke)) _("trim", S1(trim)) _("seek", S2(seek))\
  _("tnew", S1(tnew)) _("tkeys", S1(tkeys)) _("tlen", S1(tlen)) _("tset", S3(tset)) _("tget", S3(tget)) _("tdel", S3(tdel))\
  _("gensym", S1(gensym))\
  _("ev", S1(ev0)) _("::", S2(defmacro))

static NoInline bool p_define(PCore *f, const char *k, PWord v) {
  if (!pushs(f, 1, v)) return false;
  PSymbol* y = literal_symbol(f, k);
  v = pop1(f);
  return y && table_set(f, f->dict, (PWord) y, v); }


#define Dict(a, b) {a, b},
static struct { const char *n; PCell* v; } ini_dict[] = { ini(Dict) };
PCore *p_open(void) {
  PCore *f = malloc(sizeof(PCore));
  if (!f) return f;
  memset(f, 0, sizeof(PCore));
  const uintptr_t len0 = 1;
  PWord *pool = malloc(2 * len0 * sizeof(PWord));
  if (!pool) return p_close(f), NULL;
  f->t0 = clock();
  f->sp = f->loop = (f->hp = f->pool = pool) + (f->len = len0);
#define Definition(a, b) p_define(f, a, (PWord) b) &&
  if (!(f->dict = new_table(f)) ||
      !(f->macro = new_table(f)) ||
      !p_define(f, "global-namespace", (PWord) f->dict))
    return p_close(f), NULL;
  for (long i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++)
    if (!p_define(f, ini_dict[i].n, (PWord) ini_dict[i].v))
      return p_close(f), NULL;
  return f; }

static NoInline PWord pushsr(PCore *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (!n) return p_please(f, m) ? m : n;
  PWord x = va_arg(xs, PWord), y;
  avec(f, x, y = pushsr(f, m, n - 1, xs));
  return y ? *--f->sp = x : y; }

static PWord pushs(PCore *f, uintptr_t m, ...) {
  va_list xs; va_start(xs, m);
  PWord n, r = 0;
  if (avail(f) < m) r = pushsr(f, m, m, xs);
  else for (n = 0, f->sp -= m; n < m; f->sp[n++] = r = va_arg(xs, PWord));
  va_end(xs);
  return r; }

////
/// " the parser "
//
//
// get the next significant character from the stream
static int read_char(PCore *f, PFile *i) {
  for (int c;;) switch (c = getc(i)) {
    default: return c;
    case '#': case ';': while (!feof(i) && (c = getc(i)) != '\n' && c != '\r');
    case ' ': case '\t': case '\n': case '\r': case '\f': continue; } }

static PStatus reads(PCore*, PFile*), read_str_lit(PCore*, PFile*),
  read_atom(PCore*, PFile*);

static PStatus enquote(PCore *f) {
  PPair *w = pairof(f, f->sp[0], nil);
  if (!w) return Oom;
  f->sp[0] = (PWord) w;
  PSymbol* y = literal_symbol(f, "`");
  if (!y) return Oom;
  w = pairof(f, (PWord) y, f->sp[0]);
  if (!w) return Oom;
  f->sp[0] = (PWord) w;
  return Ok; }


PStatus p_read1f(PCore *f, PFile* i) {
  int c = read_char(f, i);
  switch (c) {
    case EOF: return Eof;
    case '\'': return (c = p_read1f(f, i)) == Ok ? enquote(f) : c;
    case '(': return reads(f, i);
    case ')': return pushs(f, 1, nil) ? Ok : Oom;
    case '"': return read_str_lit(f, i);
    default: return ungetc(c, i), read_atom(f, i); } }

static PStatus reads(PCore *f, PFile* i) {
  PWord c = read_char(f, i);
  if (c == EOF || c == ')') return pushs(f, 1, nil) ? Ok : Oom;
  ungetc(c, i);
  if ((c = p_read1f(f, i)) != Ok) return c;
  if ((c = reads(f, i)) != Ok) return c;
  if (!(c = (PWord) pairof(f, f->sp[1], f->sp[0]))) return Oom;
  *++f->sp = c;
  return Ok; }

// create and grow buffers for reading
static PString* bnew(PCore *f) {
  PString*s = cells(f, Width(PString) + 1);
  return s ? ini_str(s, sizeof(PWord)) : s; }

static PString* bgrow(PCore *f, PString* s) {
  PString* t; uintptr_t len = s->len;
  avec(f, s, t = cells(f, Width(PString) + 2 * b2w(len)));
  if (t) memcpy(ini_str(t, 2 * len)->text, s->text, len);
  return t; }

static PStatus read_str_lit(PCore *f, PFile* i) {
  PString* b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(PWord); b; b = bgrow(f, b), lim *= 2)
    while (n < lim) switch (c = fgetc(i)) {
      case '\\': c = fgetc(i); if (c == EOF)
      case '"': case EOF: goto out;
      default: b->text[n++] = c; } out:
  if (!b) return Oom;
  b->len = n;
  return pushs(f, 1, b) ? Ok : Oom; }

static PStatus read_atom(PCore *f, PFile* i) {
  PString* b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(PWord); b; b = bgrow(f, b), lim *= 2)
    while (n < lim) switch (c = getc(i)) {
      case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
      case '(': case ')': case '"': case '\'': case EOF: ungetc(c, i); goto out;
      default: b->text[n++] = c; } out:
  if (!b) return Oom;
  b->len = n, b->text[n] = 0; // zero terminate for strtol ; n < lim so this is safe
  char *e; long j = strtol(b->text, &e, 0);
  PWord x = *e == 0 ? putnum(j) : (PWord) intern(f, b);
  return !x || !pushs(f, 1, x) ? Oom : Ok; }

// end of parser

#define op(n, x) (Ip = (PCell*) Sp[n], Sp[n] = (x), Sp += n, Continue())
static Vm(prc)     { PWord w = *Sp; putc(getnum(w), stdout);     return op(1, w); }
static Vm(display) { PWord w = *Sp; transmit(f, stdout, w); return op(1, w); }
static Vm(error) { exit(1); }

static void transmit(PCore *f, PFile* out, PWord x) {
  if (nump(x)) fprintf(out, "%ld", (long) getnum(x));
  else if (ptr(x)->ap == data) dtyp(x)->emit(f, out, x);
  else fprintf(out, "#%lx", (long) x); }

static NoInline Vm(gc, uintptr_t n) {
  Pack(f);
  bool ok = p_please(f, n);
  Unpack(f);
  return ok ? Continue() : Oom; }

static void *bump(PCore *f, size_t n) { void *x = f->hp; return f->hp += n, x; }
static void *cells(PCore *f, size_t n) { return
  n <= avail(f) || p_please(f, n) ? bump(f, n) : 0; }

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
static NoInline bool p_please(PCore *f, uintptr_t req) {
  PWord *b0p0 = f->pool, *b0p1 = f->loop;
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
  PWord *b1p0 = malloc(len1 * 2 * sizeof(PWord));
  // if it failed we can still still return true if request was satisfied by original pool
  if (!b1p0) return req <= len0;
  // we got a new pool so copy again
  // reset core variables on new pool
  f->loop = (f->pool = b1p0) + (f->len = len1);
  copy_from(f, b0p1, len0); // do second copy
  free(b0p0 < b0p1 ? b0p0 : b0p1); // free old pool
  f->t0 = clock(); // set last gc timestamp
  return true; } // size successfully adjusted


#define CP(d) ((d) = (void*) cp(f, (PWord) (d), p0, t0))
// this function expects pool loop and len to have been set already on the state
static NoInline void copy_from(PCore *f, PWord *p0, uintptr_t len0) {
  PWord len1 = f->len, // target pool length
           *p1 = f->pool, // target pool
           *t0 = p0 + len0, // source pool top
           *t1 = p1 + len1, // target pool top
           *sp0 = f->sp, // source pool stack
           sn = t0 - sp0, // stack height
           *sp1 = t1 - sn; // target pool stack
  // reset stack, heap, symbols
  f->sp = sp1, f->hp = p1, f->symbols = 0;
  // copy stack and variables
  while (sn--) *sp1++ = cp(f, *sp0++, p0, t0);
  CP(f->ip), CP(f->dict), CP(f->macro);
  // copy protected values
  for (PMm *r = f->safe; r; r = r->next) *r->addr = cp(f, *r->addr, p0, t0);
  // copy all reachable values using cheney's method
  PHeap cptr = p1;
  for (PCell* k; (k = (PCell*) cptr) < (PCell*) f->hp;)
    if (datp(k)) dtyp(k)->evac(f, (PWord) k, p0, t0, &cptr); // is data
    else { // is thread
      while (k->x) k->x = cp(f, k->x, p0, t0), k++;
      cptr = (PWord*) k + 2; } }

#define bounded(a, b, c) ((PWord)(a)<=(PWord)(b)&&(PWord)(b)<(PWord)(c))
static NoInline PWord cp(PCore *v, PWord x, PWord *p0, PWord *t0) {
  // if it's a number or out of managed memory then return it
  if (nump(x) || !bounded(p0, x, t0)) return x;
  PCell* src = (PCell*) x;
  x = src->x;
  // if the cell holds a pointer to the new space then return the pointer
  if (homp(x) && bounded(v->pool, x, v->pool + v->len)) return x;
  // if it's data then call the given copy function
  if (datp(src)) return dtyp(src)->copy(v, (PWord) src, p0, t0);
  // it's a thread, find the end
  struct tag *t = ttag(src);
  PCell *ini = t->head, *d = bump(v, t->end - ini), *dst = d;
  for (PCell *s = ini; (d->x = s->x); s++->x = (PWord) d++);
  d[1].ap = (PVm*) dst;
  return (PWord) (src - ini + dst); }

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
static Vm(rng) { return op(1, putnum(rand())); }
static Vm(pairp) { return op(1, twop(Sp[0]) ? putnum(-1) : nil); }
static Vm(fixnump) { return op(1, nump(Sp[0]) ? putnum(-1) : nil); }
static Vm(stringp) { return op(1, strp(Sp[0]) ? putnum(-1) : nil); }
static Vm(symbolp) { return op(1, symp(Sp[0]) ? putnum(-1) : nil); }
static bool eql(PCore *f, PWord a, PWord b) {
  if (a == b) return true;
  if (nump(a | b) ||
      ptr(a)->ap != data ||
      ptr(b)->ap != data ||
      dtyp(a) != dtyp(b)) return false;
  return dtyp(a)->equal(f, a, b); }

static bool not_equal(PCore *f, PWord a, PWord b) { return false; }

static void trim_thread(PCell *k) { ttag(k)->head = k; }

static Vm(trim) {
  PCell *k = (PCell*) Sp[0];
  trim_thread(k);
  return op(1, (PWord) k); }

static Vm(seek) {
  PCell *k = (PCell*) Sp[1];
  return op(2, (PWord) (k + getnum(Sp[0]))); }

static Vm(peek) {
  PCell *k = (PCell*) Sp[0];
  return op(1, k[0].x); }

static Vm(poke) {
  PCell *k = (PCell*) Sp[1];
  k->x = Sp[0];
  return op(2, (PWord) k); }

static Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct tag));
  PCell *k = mo_ini((PCell*) Hp, n);
  memset(k, -1, n * sizeof(PWord));
  Hp += n + Width(struct tag);
  return op(1, (PWord) k); }

static PWord cp_two(PCore *v, PWord x, PWord *p0, PWord *t0) {
  PPair *src = (PPair*) x,
           *dst = ini_pair(bump(v, Width(PPair)), src->a, src->b);
  return (PWord) (src->ap = (PVm*) dst); }

static void wk_two(PCore *f, PWord x, PWord *p0, PWord *t0, PHeap *cptr) {
  *cptr += Width(PPair);
  A(x) = cp(f, A(x), p0, t0);
  B(x) = cp(f, B(x), p0, t0); }

static void print_two(PCore *f, PFile *o, PWord x) {
  for (putc('(', o);; putc(' ', o)) {
    transmit(f, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

// FIXME could overflow the stack -- use off pool for this
static bool eq_two(PCore *f, PWord x, PWord y) {
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static PWord hash_two(PCore *f, PWord x) {
  PWord hc = hash(f, A(x)) * hash(f, B(x));
  return hc ^ mix; }

static PPair *pairof(PCore *f, PWord a, PWord b) {
  if (avail(f) < Width(PPair)) {
    bool ok;
    avec(f, a, avec(f, b, ok = p_please(f, Width(PPair))));
    if (!ok) return 0; }
  PPair *w = (PPair*) f->hp;
  f->hp += Width(PPair);
  return ini_pair(w, a, b); }

static Vm(car) { return op(1, twop(Sp[0]) ? A(Sp[0]) : Sp[0]); }
static Vm(cdr) { return op(1, twop(Sp[0]) ? B(Sp[0]) : nil); }
static Vm(cons) {
  Have(Width(PPair));
  PPair *w = ini_pair((PPair*) Hp, Sp[0], Sp[1]);
  Hp += Width(PPair);
  return op(2, (PWord) w); }

static PWord copy_string(PCore* v, PWord x, PWord *p0, PWord *t0) {
  PString* src = (PString*) x;
  size_t len = sizeof(PString) + src->len;
  return (PWord) (src->ap = memcpy(bump(v, b2w(len)), src, len)); }

static void walk_string(PCore* v, PWord x, PWord *p0, PWord *t0, PHeap *cp) {
  *cp += Width(PString) + b2w(((PString*) x)->len); }

static void print_string(PCore* v, PFile *o, PWord _) {
  PString* s = (PString*) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

static PWord hash_string(PCore* v, PWord _) {
  PString *s = (PString*) _;
  uintptr_t h = 1;
  size_t words = s->len / sizeof(PWord),
         bytes = s->len % sizeof(PWord);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const intptr_t *ws = (intptr_t*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static bool string_equal(PCore *f, PWord x, PWord y) {
  PString*a = (PString*) x, *b = (PString*) y;
  return a->len == b->len && 0 == strncmp(a->text, b->text, a->len); }

static Vm(slen) {
  PWord x = Sp[0];
  return op(1, strp(x) ? putnum(((PString*)x)->len) : nil); }

static Vm(ssub) {
  PCell* r = (PCell*) Sp[3];
  if (!strp(Sp[0])) Sp[3] = nil;
  else {
    PString *s = (PString*) Sp[0];
    intptr_t i = nump(Sp[1]) ? getnum(Sp[1]) : 0,
             j = nump(Sp[2]) ? getnum(Sp[2]) : 0;
    i = max(i, 0), i = min(i, s->len);
    j = max(j, i), j = min(j, s->len);
    if (i == j) Sp[3] = nil;
    else {
      size_t req = Width(PString) + b2w(j - i);
      Have(req);
      PString* t = ini_str((PString*) Hp, j - i);
      Hp += req;
      memcpy(t->text, s->text + i, j - i);
      Sp[3] = (PWord) t; } }
  Ip = r, Sp += 3;
  return Continue(); }

static Vm(sget) {
  PCell *r = (PCell*) Sp[2];
  if (!strp(Sp[0])) Sp[2] = nil;
  else {
    PString*s = (PString*) Sp[0];
    size_t i = min(s->len - 1, getnum(Sp[1]));
    i = max(i, 0);
    Sp[2] = putnum(s->text[i]); }
  Ip = r, Sp += 2;
  return Continue(); }

static Vm(scat) {
  PWord a = Sp[0], b = Sp[1];
  if (!strp(a)) return op(2, b);
  if (!strp(b)) return op(2, a);
  PString *x = (PString*) a, *y = (PString*) b;
  size_t len = x->len + y->len,
         req = Width(PString) + b2w(len);
  Have(req);
  PString*z = ini_str((PString*) Hp, len);
  Hp += req;
  memcpy(z->text, x->text, x->len);
  memcpy(z->text + x->len, y->text, y->len);
  return op(2, (PWord) z); }

static PSymbol* intern_r(PCore*, PString*, PSymbol**);

static PWord hash_symbol(PCore *v, PWord _) {
  return ((PSymbol*) _)->code; }
static PWord copy_symbol(PCore *f, PWord x, PWord *p0, PWord *t0) {
  PSymbol *src = (PSymbol*) x,
             *dst = src->nom ?
               intern_r(f, (PString*) cp(f, (PWord) src->nom, p0, t0), &f->symbols) :
               ini_anon(bump(f, Width(PSymbol) - 2), src->code);
  return (PWord) (src->ap = (PVm*) dst); }
static void walk_symbol(PCore *f, PWord x, PWord *p0, PWord *t0, PHeap *cp) {
  *cp += Width(PSymbol) - (((PSymbol*)x)->nom ? 0 : 2); }

static void print_symbol(PCore *f, PFile *o, PWord x) {
  PString* s = ((PSymbol*) x)->nom;
  if (s) for (int i = 0; i < s->len; putc(s->text[i++], o));
  else fprintf(o, "#sym@%lx", (long) x); }


static PSymbol* intern_r(PCore *v, PString* b, PSymbol* *y) {
  PSymbol* z = *y;
  if (!z) return *y =
    ini_sym(bump(v, Width(PSymbol)), b, hash(v, putnum(hash(v, (PWord) b))));
  PString* a = z->nom;
  int i = a->len < b->len ? -1 :
          a->len > b->len ? 1 :
          strncmp(a->text, b->text, a->len);
  return i == 0 ? z : intern_r(v, b, i < 0 ? &z->l : &z->r); }

static PSymbol* intern(PCore *f, PString* b) {
  if (avail(f) < Width(PSymbol)) {
    bool ok;
    avec(f, b, ok = p_please(f, Width(PSymbol)));
    if (!ok) return 0; }
  return intern_r(f, b, &f->symbols); }


static PSymbol* literal_symbol(PCore *f, const char *nom) {
  size_t len = strlen(nom);
  PString* o = cells(f, Width(PString) + b2w(len));
  if (!o) return 0;
  memcpy(o->text, nom, len);
  return intern(f, ini_str(o, len)); }

static Vm(gensym) {
  const int req = Width(PSymbol) - 2;
  Have(req);
  PSymbol* y = (PSymbol*) Hp;
  Hp += req;
  return op(1, (PWord) ini_anon(y, rand())); }


// FIXME poor hashing method :(
static PWord hash_table(PCore *f, PWord h) { return mix; }

static PWord copy_table(PCore *f, PWord x, PWord *p0, PWord *t0) {
  PTable *src = (PTable*) x;
  PWord i = src->cap;
  PTable *dst = bump(f, Width(PTable) + i);
  src->ap = (PVm*) ini_table(dst, src->len, src->cap, (void*) (dst + 1));

  //FIXME do these allocations in a block with the rest
  for (PTableEntry *s, *e, *d; i--; dst->tab[i] = e)
    for (s = src->tab[i], e = NULL; s;
      d = bump(f, Width(PTableEntry)),
      d->key = s->key, d->val = s->val,
      d->next = e, e = d,
      s = s->next);
  return (PWord) dst; }

static void walk_table(PCore *f, PWord x, PWord *p0, PWord *t0, PHeap *cptr) {
  PTable *t = (PTable*) x;
  *cptr += Width(PTable) + t->cap + t->len * Width(PTableEntry);
  for (PWord i = 0, lim = t->cap; i < lim; i++)
    for (PTableEntry *e = t->tab[i]; e;
      e->key = cp(f, e->key, p0, t0),
      e->val = cp(f, e->val, p0, t0),
      e = e->next); }

static void print_table(PCore *f, PFile *o, PWord x) {
  PTable *t = (PTable*) x;
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

static PWord hash(PCore *f, PWord x) {
  if (nump(x)) {
    const int half_word_bits = sizeof(PWord) * 4;
    x *= mix;
    x = (x << half_word_bits) | (x >> half_word_bits);
    return x; }

  if (datp(x)) return dtyp(x)->hash(f, x);
  if (!bounded(f->pool, x, f->pool+f->len)) return mix ^ (mix * x);
  // it's a function, hash by length
  struct tag *t = ttag((PCell*) x);
  PWord len = (PCell*) t - t->head;
  return mix ^ (mix * len); }

static PTable *new_table(PCore *f) {
  PTable *t = cells(f, Width(PTable) + 1);
  if (!t) return t;
  PTableEntry **tab = (void*) (t + 1);
  tab[0] = 0;
  return ini_table(t, 0, 1, tab); }


static NoInline PTable *table_insert(PCore *f, PTable *t, PWord k, PWord v, PWord i) {
  PTableEntry *e;
  avec(f, t, avec(f, k, avec(f, v, e = cells(f, Width(PTableEntry)))));
  if (!e) return 0;
  e->key = k, e->val = v, e->next = t->tab[i];
  t->tab[i] = e;
  PWord cap0 = t->cap, load = ++t->len / cap0;
  if (load <= 1) return t;
  // grow the table
  PTableEntry **tab0, **tab1;
  PWord cap1 = 2 * cap0;
  avec(f, t, tab1 = cells(f, cap1));
  tab0 = t->tab;
  if (!tab1) return 0;
  memset(tab1, 0, cap1 * sizeof(PWord));
  for (PWord i; cap0--;)
    for (PTableEntry *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = (cap1-1) & hash(f, e->key),
      e->next = tab1[i],
      tab1[i] = e);

  t->cap = cap1, t->tab = tab1;
  return t; }

static Inline PWord index_of_key(PCore *f, PTable *t, PWord k) {
  // relies on table capacity being a power of 2
  return (t->cap - 1) & hash(f, k); }

static NoInline PTable *table_set(PCore *f, PTable *t, PWord k, PWord v) {
  PWord index = index_of_key(f, t, k);
  PTableEntry *entry = t->tab[index];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  if (entry) return entry->val = v, t;
  return table_insert(f, t, k, v, index); }

static PTableEntry *table_delete_r(PCore *f, PTable *t, PWord k, PWord *v, PTableEntry *e) {
  if (!e) return e;
  if (eql(f, e->key, k)) return t->len--, *v = e->val, e->next;
  return e->next = table_delete_r(f, t, k, v, e->next), e; }

static void table_shrink(PCore *f, PTable *t) {
  PWord cap = t->cap;
  PTableEntry *coll = 0, *x, *y; // collect all entries in one list
  for (PWord i = 0; i < cap; i++)
    for (x = t->tab[i], t->tab[i] = 0; x;)
      y = x, x = x->next, y->next = coll, coll = y;
  t->cap = cap >>= 2;
  for (PWord i; coll;)
    i = (cap - 1) & hash(f, coll->key),
    x = coll->next,
    coll->next = t->tab[i],
    t->tab[i] = coll,
    coll = x; }

static NoInline PWord table_delete(PCore *f, PTable *t, PWord k, PWord v) {
  PWord idx = index_of_key(f, t, k);
  t->tab[idx] = table_delete_r(f, t, k, &v, t->tab[idx]);
  if (t->len / t->cap > 1) table_shrink(f, t);
  return v; }

static Vm(tnew) {
  Have(Width(PTable) + 1);
  PTable *t = (PTable*) Hp;
  PTableEntry **tab = (PTableEntry**) (t + 1);
  Hp += Width(PTable) + 1;
  tab[0] = 0;
  return op(1, (PWord) ini_table(t, 0, 1, tab)); }

static PWord table_get(PCore *f, PTable *t, PWord k, PWord zero) {
  PTableEntry *entry = t->tab[index_of_key(f, t, k)];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  return entry ? entry->val : zero; }

static Vm(tget) {
  return op(3, !tblp(Sp[1]) ? Sp[2] :
    table_get(f, (PTable*) Sp[1], Sp[2], Sp[0])); }

static Vm(tset) {
  PWord x = Sp[0];
  if (!tblp(x)) return op(3, nil);
  Pack(f);
  PTable *t = table_set(f, (PTable*) x, Sp[1], Sp[2]);
  Unpack(f);
  return !t ? Oom : op(3, Sp[2]); }

static Vm(tdel) {
  PWord x = Sp[1];
  return op(3, !tblp(x) ? nil :
    table_delete(f, (PTable*) x, Sp[2], Sp[0])); }

static Vm(tlen) {
  PWord x = Sp[0];
  if (!tblp(x)) return op(1, nil);
  PTable *t = (PTable*) x;
  return op(1, putnum(t->len)); }

static Vm(tkeys) {
  if (!tblp(Sp[0])) return op(1, nil);
  PTable *t = (PTable*) Sp[0];
  PWord len = t->len, list = nil;
  Have(len * Width(PPair));
  PPair *pairs = (PPair*) Hp;
  Hp += len * Width(PPair);
  for (int i = t->cap; i;)
    for (PTableEntry *e = t->tab[--i]; e; e = e->next)
      pairs->ap = data, pairs->typ = &pair_type,
      pairs->a = e->key, pairs->b = list,
      list = (PWord) pairs, pairs++;
  return op(1, list); }


static PCell *mo_n(PCore*, size_t);
// index of item in list
static long lidx(PCore *f, PWord l, PWord x) {
  for (long i = 0; twop(l); l = B(l), i++) if (eql(f, A(l), x)) return i;
  return -1; }
// list length
static size_t llen(PWord l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }
// at all times vm is running a function. thread compiler tracks
// function state using this type
typedef struct scope {
  // these parameters represent stack state at a point in compile process
  PWord args, // list // function positional arguments (never empty)
       imps, // list // closure variables
       pals; // list // current state of stack
  // these are values of variables known at compile time
  PWord lams; // dict // known function definitions
  // these are two stacks of jump target addresses for conditional expressions
  PWord alts, // list // alternate branch address stack
       ends; // list // exit branch address stack
  // this is the enclosing function scope if any
  struct scope *par;
} *scope;

static PVm pushr, cond, jump, pushk, yield, ret, ap, apn, tap, tapn;

// thread compiler operates in two phases
//
// 1. analyze phase: analyze expression; assemble constructor on stack; compute code size bound
#define C0(n, ...) size_t n(PCore *f, scope *c, size_t m, PWord x, ##__VA_ARGS__)
typedef C0(c0);
// and
// - generate phase: allocate thread; call constructor; trim thread
#define C1(n, ...) PCell *n(PCore *f, scope *c, PCell *k, ##__VA_ARGS__)
typedef C1(c1);


// scope constructor
static scope enscope(PCore *f, scope par, PWord args, PWord imps) {
  if (!pushs(f, 3, args, imps, par)) return 0;
  scope c = (scope) mo_n(f, 7);
  if (c)
    c->args = pop1(f), c->imps = pop1(f),
    c->par = (scope) pop1(f),
    c->pals = c->alts = c->ends = nil,
    c->lams = nil;
  return c; }

static PWord
  lassoc(PCore*, PWord, PWord),
  lconcat(PCore*, PWord, PWord),
  rlconcat(PCore*, PWord, PWord);

static c0 analyze_if, analyze_let, analyze_arguments, analyze_list;
static c1 c1apn, c1var, c2var;
// basic functions
static C1(yieldk) { return k; }
static C1(pull) { return ((c1*) (*f->sp++))(f, c, k); }
static PCell *construct(PCore *f, scope *c, size_t m) {
  PCell *k = mo_n(f, m);
  if (!k) return k;
  memset(k, -1, m * sizeof(PWord));
  return pull(f, c, k + m); }
static C1(c1i) { return k[-1].x = *f->sp++, pull(f, c, k - 1); }
static C1(c1ix) { return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull(f, c, k - 2); }
// generic instruction c0 handlers
static size_t em1(PCore *f, scope *c, size_t m, PVm *i) {
  return pushs(f, 2, c1i, i) ? m + 1 : 0; }
static size_t em2(PCore *f, scope *c, size_t m, PVm *i, PWord x) {
  return pushs(f, 3, c1ix, i, x) ? m + 2 : 0; }
// analyzer

static NoInline size_t analyze_symbol(PCore*, scope*, size_t, PWord, scope);
static C0(analyze) {
  if (homp(x) && ptr(x)->ap == data) {
    PType *y = dtyp(x);
    if (y == &pair_type) return analyze_list(f, c, m, x);
    if (y == &symbol_type) return analyze_symbol(f, c, m, x, *c); }
  return em2(f, c, m, pushk, x); }

static PVm lazy_bind, drop, define, g_bind;
static C0(analyze_variable_reference) {
  if (nilp((PWord) (*c)->par)) {
    x = (PWord) pairof(f, x, (PWord) f->dict);
    return !x ? x : em2(f, c, m, g_bind, x); }
  return pushs(f, 3, c1var, x, (*c)->pals) ? m + 2 : 0; }

static long index_of(PCore *f, scope c, PWord var) {
  size_t i = 0;
  // is it a closure variable?
  for (PWord l = c->imps; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  for (PWord l = c->args; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  return -1; }


static C1(c2var) {
  PWord var = *f->sp++, pals = *f->sp++;
  size_t i = lidx(f, pals, var);
  return
    k[-2].ap = pushr,
    k[-1].x = putnum(i),
    pull(f, c, k - 2); }

// emit stack reference instruction
static C1(c1var) {
  PWord var = *f->sp++, // variable name
       ins = llen(*f->sp++), // stack inset
       idx = index_of(f, *c, var);
  return
    k[-2].ap = pushr,
    k[-1].x = putnum(idx + ins),
    pull(f, c, k - 2); }

#define P(x) (transmit(f, stdout, x), puts(""))
static C0(analyze_symbol, scope d) {
  PWord y;
  if (nilp((PWord) d)) {
    y = table_get(f, f->dict, x, 0);
    if (y) return em2(f, c, m, pushk, y);
    x = (PWord) pairof(f, x, (*c)->imps),
    x = x ? A((*c)->imps = x) : x;
    return x ? analyze_variable_reference(f, c, m, x) : 0; }

  // look in vals
  if ((y = lassoc(f, d->lams, x))) {
    // lazy bind
    bind(y, (PWord) pairof(f, y, (PWord) d));
    bind(m, em2(f, c, m, lazy_bind, y));
    x = f->sp[2];
    y = BBA(x); // get the closure args to pass in
    A(x) = AA(x);
    return analyze_arguments(f, c, m, y); } // XXX

  // look in pals
  if ((y = lidx(f, d->pals, x)) >= 0)
    return pushs(f, 3, c2var, x, d->pals) ? m + 2 : 0;

  // look in imps args
  y = index_of(f, d, x);
  if (y >= 0) {
    if (*c != d)
      x = (PWord) pairof(f, x, (*c)->imps),
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
  PWord n = *f->sp++;
  if (k->ap == ret) k->x = n, (--k)->ap = tapn;
  else (--k)->x = n, (--k)->ap = apn;
  return pull(f, c, k); }

// evaluate function call arguments and apply
static size_t analyze_arguments(PCore *f, scope *c, size_t m, PWord x) {
  MM(f, &x); // handle oom here ..
  if (!((*c)->pals = (PWord) pairof(f, nil, (*c)->pals))) m = 0;
  else {
    for (; m && twop(x); x = B(x))
      m = analyze(f, c, m + 1, A(x)),
      m = m && pushs(f, 1, c1ap) ? m : 0;
    (*c)->pals = B((*c)->pals); }
  UM(f);
  return m; }

// lambda decons pushes last list item to stack returns init of list
static PWord linit(PCore *f, PWord x) {
  if (!twop(x)) return pushs(f, 1, nil) ? nil : 0;
  if (!twop(B(x))) return pushs(f, 1, A(x)) ? nil : 0;
  PWord y = A(x);
  avec(f, y, x = linit(f, B(x)));
  return x ? (PWord) pairof(f, y, x) : x; }

static PWord analyze_lambda(PCore *f, scope *c, PWord imps, PWord exp) {
  // storing exp in scope->args for the moment is expedient
  scope d = enscope(f, *c, exp, imps);
  if (!d) return 0;
  MM(f, &d);
  // get the real args
  PWord args = linit(f, d->args);
  if (!(d->args = args)) goto fail;
  exp = f->sp[0], f->sp[0] = (PWord) yieldk;
  size_t m = analyze(f, &d, 4, exp);
  if (!m) goto fail;
  size_t arity = llen(d->args) + llen(d->imps);
  PCell *k = pushs(f, 3, c1ix, ret, putnum(arity)) ? construct(f, &d, m) : 0;
  if (!k) goto fail;
  if (arity > 1) (--k)->x = putnum(arity), (--k)->ap = curry;
  trim_thread(k);
  UM(f);
  return (PWord) pairof(f, (PWord) k, d->imps);
fail:
  UM(f);
  return 0; }

static Vm(defmacro) {
  Pack(f);
  if (!table_set(f, f->macro, Sp[0], Sp[1])) return Oom;
  Unpack(f);
  return op(2, Sp[1]); }

static Vm(data) {
  PWord this = (PWord) Ip;
  return op(1, this); }

static Vm(pushk) {
  Have1();
  *--Sp = Ip[1].x;
  Ip += 2;
  return Continue(); }

static Vm(jump) {
  Ip = Ip[1].m;
  return Continue(); }

static Vm(cond) {
  Ip = nilp(*Sp) ? Ip[1].m : Ip + 2,
  Sp++;
  return Continue(); }

static Vm(pushr) {
  Have1();
  Sp[-1] = Sp[getnum(Ip[1].x)];
  Sp--;
  Ip += 2;
  return Continue(); }

static Vm(ret) {
  PWord n = getnum(Ip[1].x) + 1;
  return op(n, *Sp); }

static Vm(yield) { Pack(f); return YieldStatus; }

static Vm(ap) {
  if (nump(Sp[1])) return Ip++, Sp++, Continue();
  PCell *k = (PCell*) Sp[1];
  Sp[1] = (PWord) (Ip + 1);
  Ip = k;
  return Continue(); }

static Vm(apn) {
  size_t n = getnum(Ip[1].x);
  PCell *ra = Ip + 2; // return address
  Ip = ((PCell*) Sp[n]) + 2; // only used by let form so will not be num
  Sp[n] = (PWord) ra; // store return address
  return Continue(); }

static Vm(tap) {
  PWord x = Sp[0], j = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) return op(1, j);
  Ip = (PCell*) j;
  *Sp = x;
  return Continue(); }

static Vm(tapn) {
  size_t n = getnum(Ip[1].x),
         r = getnum(Ip[2].x);
  Ip = ((PCell*) Sp[n]) + 2;
  PStack osp = Sp;
  Sp += r + 1;
  while (n--) Sp[n] = osp[n];
  return Continue(); }

static Vm(pushkj) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return Continue(); }

static Vm(curry) {
  PCell *k;
  size_t n = getnum(Ip[1].x),
         S = 3 + Width(struct tag);
  if (n == 2) {
    Have(S);
    k = (PCell*) Hp;
    k[0].ap = pushkj, k[1].x = *Sp++, k[2].m = Ip + 2;
    k[3].x = 0,   k[4].m = k; }
  else {
    S += 2;
    Have(S);
    k = (PCell*) Hp;
    k[0].ap = curry, k[1].x = putnum(n - 1);
    k[2].ap = pushkj,  k[3].x = *Sp++, k[4].m = Ip + 2;
    k[5].x = 0,    k[6].m = k; }
  Hp += S;
  Ip = (PCell*) *Sp;
  *Sp = (PWord) k;
  return Continue(); }

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
  PPair p = { data, &pair_type, nil, nil };
  for (x = pop1(f), MM(f, &x); m; x = BB(x)) {
    if (!twop(x)) x = (PWord) &p;
    m = analyze(f, c, m + 2, A(x));
    if (!twop(B(x))) { // at end, default branch
      m = pushs(f, 1, generate_cond_peek_exit) ? m : 0;
      break; }
    m = pushs(f, 1, generate_cond_pop_branch) ? m : 0;
    m = m ? analyze(f, c, m + 2, AB(x)) : m;
    m = pushs(f, 2, generate_cond_push_branch, generate_cond_peek_exit) ? m : 0; }
  return UM(f), m && pushs(f, 1, generate_cond_push_exit) ? m : 0; }

// first emitter called for cond expression
// pushes cond expression exit address onto scope stack ends
static C1(generate_cond_push_exit) {
  PPair *w = pairof(f, (PWord) k, (*c)->ends);
  return !w ? 0 : pull(f, c, (PCell*) A((*c)->ends = (PWord) w)); }

// last emitter called for cond expression
// pops cond expression exit address off scope stack ends
static C1(generate_cond_pop_exit) {
  return (*c)->ends = B((*c)->ends), pull(f, c, k); }

static C1(generate_cond_push_branch) {
  PPair *w = pairof(f, (PWord) k, (*c)->alts);
  if (!w) return (PCell*) w;
  (*c)->alts = (PWord) w;
  k = (PCell*) w->a;
  return pull(f, c, k); }

static C1(generate_cond_peek_exit) {
  k -= 2;
  PCell *addr = (PCell*) A((*c)->ends);
  // if the destination is a return or tail call,
  // then copy it forward instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k[0].ap = addr[0].ap, k[1].x = addr[1].x;
  else k[0].ap = jump, k[1].x = (PWord) addr;
  return pull(f, c, k); }

// last emitter called for a branch
// pops next branch address off scope stack alts
static C1(generate_cond_pop_branch) {
  return k[-2].ap = cond,
         k[-1].x = A((*c)->alts),
         (*c)->alts = B((*c)->alts),
         pull(f, c, k - 2); }

static bool lambp(PCore *f, PWord x) {
  if (!twop(x) || !symp(x = A(x))) return false;
  PString* s = ((PSymbol*) x)->nom;
  return s && s->len == 1 && s->text[0] == '\\'; }

// DEFINE
// let expressions

static PWord ldels(PCore *f, PWord lam, PWord l) {
  if (!twop(l)) return nil;
  PWord m = ldels(f, lam, B(l));
  if (!lassoc(f, lam, A(l))) B(l) = m, m = l;
  return m; }

static PWord desugr(PCore *f, PWord *d, PWord *e, PWord a) {
  if (!twop(a)) return (PWord) pairof(f, *e, nil);
  PWord b; avec(f, a, b = desugr(f, d, e, B(a)));
  return !b ? b : (PWord) pairof(f, A(a), b); }

static PStatus desug(PCore *f, PWord *d, PWord *e) {
  if (!twop(*d)) return Ok;
  PWord x, l = (PWord) literal_symbol(f, "\\");
  if (!l || !pushs(f, 1, l)) return Oom;
  do if (!(x = (PWord) desugr(f, d, e, B(*d))) ||
         !(x = (PWord) pairof(f, f->sp[0], x)))
    return Oom;
  else *d = A(*d), *e = x;
  while (twop(*d));
  return f->sp++, Ok; }

static size_t analyze_let(PCore *f, scope *b, size_t m, PWord exp) {
  if (!twop(exp)) return analyze(f, b, m, nil);
  if (!twop(B(exp))) return analyze(f, b, m, A(exp));
  scope q = *b, *c = &q;
  avec(f, exp, q = enscope(f, q, q->args, q->imps));
  if (!q) return 0;
  // lots of variables :(
  PWord nom = nil, def = nil, lam = nil,
       v = nil, d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &exp), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);
// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.

  // collect vars and defs into two lists
  for (; twop(exp) && twop(B(exp)); exp = BB(exp)) {
    d = A(exp), e = AB(exp), desug(f, &d, &e);
    if (!(nom = (PWord) pairof(f, d, nom)) ||
        !(def = (PWord) pairof(f, e, def)))
      goto fail;
    if (lambp(f, e)) {
      // if it's a lambda compile it and record in lam list
      PWord x = analyze_lambda(f, c, nil, B(e));
      x = x ? (PWord) pairof(f, d, x) : x;
      lam = x ? (PWord) pairof(f, x, lam) : x;
      if (!lam) goto fail; } }

  // if there's no body then evaluate the name of the last definition
  bool even = !twop(exp); // we check this again later to make global bindings at top level
  if (even && !(exp = (PWord) pairof(f, A(nom), nil))) goto fail;

  // find closures
  // for each function f with closure C(f)
  // for each function g with closure C(g)
  // if f in C(g) then C(g) include C(f)
  long j;
  do for (j = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
    for (e = lam; twop(e); e = B(e)) // for each bound function variable
      if (d != e && // skip yourself
          lidx(f, BBA(e), AA(d)) >= 0) // if you need this function
        for (v = BA(d); twop(v); v = B(v)) { // then you need its variables
          PWord vars = BBA(e), var = A(v);
          if (lidx(f, vars, var) < 0) { // only add if it's not already there
            if (!(vars = (PWord) pairof(f, var, vars))) goto fail; // oom
            j++, BBA(e) = vars; } }
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e)) ;

  (*c)->lams = lam;
  // construct lambda with reversed argument list
  exp = lconcat(f, nom, exp);
  PSymbol* l = exp ? literal_symbol(f, "\\") : 0;
  exp = l ? (PWord) pairof(f, (PWord) l, exp) : 0;
  m = exp ? analyze(f, b, m, exp) : 0; // exp is now the required lambda, analyze it
  if (!m || !((*b)->pals = (PWord) pairof(f, nil, (*b)->pals))) goto fail;

  // reverse the nom and def lists
  nom = rlconcat(f, nom, nil), def = rlconcat(f, def, nil);
  // evaluate definitions in order tracking var names on pals list
  // store lambdas on scope for lazy binding and construct new application
  // - reverse noms onto exp
  // - reverse onto e = nil and recompile lambdas
  size_t nn = 0;
  for (; twop(nom); nom = B(nom), def = B(def), nn++) {
    // if lambda then recompile with the explicit closure
    // and put in arg list and lam list (latter is used for lazy binding)
    if (lambp(f, A(def))) {
      d = lassoc(f, lam, A(nom));
      PWord _;
      if (!(_ = analyze_lambda(f, c, BB(d), BA(def)))) goto fail;
      else A(def) = B(d) = _; }
    // if toplevel then bind
    if (even && nilp((*b)->args)) {
      PCell *t = cells(f, 2 * Width(PPair) + 2 + Width(struct tag));
      if (!t) goto fail;
      PPair *w = (PPair*) t,
               *x = w + 1;
      t += 2 * Width(PPair);
      t[0].ap = define, t[1].x = A(nom), t[2].x = 0, t[3].m = t;
      ini_pair(w, A(def), nil); // dict add
      ini_pair(x, (PWord) t, (PWord) w);
      A(def) = (PWord) x; }
    if (!(m = analyze(f, b, m, A(def))) ||
        !((*b)->pals = (PWord) pairof(f, A(nom), (*b)->pals)))
      goto fail; }

  m = nn <= 1 ? pushs(f, 1, c1ap) ? m + 1 : 0 :
                pushs(f, 2, c1apn, putnum(nn)) ? m + 2 : 0;
  for (nn++; nn--; (*b)->pals = B((*b)->pals));
done: return UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), m;
fail: m = 0; goto done; }

static size_t analyze_sequence(PCore *f, scope *c, size_t m, PWord x) {
  if (!twop(x)) return em2(f, c, m, pushk, nil);
  for (MM(f, &x); m && twop(B(x)); x = B(x))
    m = analyze(f, c, m, A(x)),
    m = m ? em1(f, c, m, drop) : m;
  return UM(f), m ? analyze(f, c, m, A(x)) : m; }

static C0(analyze_macro, PWord b) {
  if (!pushs(f, 2, x, b)) return 0;
  x = (PWord) literal_symbol(f, "`");
  if (!x || !pushs(f, 1, x)) return 0;
  PPair *mxp = (PPair*) cells(f, 4 * Width(PPair));
  if (!mxp) return 0;
  x = (PWord) ini_pair(mxp, f->sp[1], (PWord) ini_pair(mxp+1, (PWord) ini_pair(mxp+2, f->sp[0], (PWord) ini_pair(mxp+3, f->sp[2], nil)), nil));
  f->sp += 2, *f->sp = x;
  return p_eval(f) != Ok ? 0 : analyze(f, c, m, pop1(f)); }

static C0(analyze_list) {
  PWord a = A(x), b = B(x);
  if (!twop(b)) return analyze(f, c, m, a); // singleton list has value of first element
  if (symp(a)) {
    PWord macro = table_get(f, f->macro, a, 0);
    if (macro) return analyze_macro(f, c, m, macro, b);
    PString* n = ((PSymbol*) a)->nom;
    if (n && n->len == 1)
      switch (n->text[0]) { // special form?
        case '`': return em2(f, c, m, pushk, twop(b) ? A(b) : nil); // quote
        case ',': return analyze_sequence(f, c, m, b); // sequence
        case ':': return analyze_let(f, c, m, b);
        case '?': return analyze_if(f, c, m, b);
        case '\\': return (x = analyze_lambda(f, c, nil, b)) ? analyze(f, c, m, x) : x; } }
  avec(f, b, m = analyze(f, c, m, a));
  return m ? analyze_arguments(f, c, m, b) : m; }

static Vm(drop) {
  return Ip++, Sp++, Continue(); }

static Vm(define) {
  Pack(f);
  if (!table_set(f, f->dict, Ip[1].x, Sp[0])) return Oom;
  Unpack(f);
  return op(1, Sp[0]); }

static Vm(ev0) {
  Pack(f);
  PStatus s = p_eval(f);
  Unpack(f);
  return s == Ok ? op(1, *Sp) : s; }

static Vm(g_bind) {
  PWord x = Ip[1].x, var = A(x);
  PTable *t = (PTable*) B(x);
  x = table_get(f, t, var, var);
  Ip[0].ap = pushk;
  Ip[1].x = x;
  return Continue(); }

static Vm(lazy_bind) {
  PWord ref = Ip[1].x, var = A(ref);
  scope env = (scope) B(ref);
  var = AB(lassoc(f, env->lams, var));
  Ip[0].ap = pushk;
  Ip[1].x = var;
  return Continue(); }

static PWord lassoc(PCore *f, PWord l, PWord k) {
  for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
  return 0; }

// list concat
static PWord lconcat(PCore *f, PWord l, PWord n) {
  if (!twop(l)) return n;
  avec(f, l, n = lconcat(f, B(l), n));
  return n ? (PWord) pairof(f, A(l), n) : n; }

// reverse list concat
static PWord rlconcat(PCore *f, PWord l, PWord n) {
  for (PWord m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
  return n; }

// allocate a thread
static PCell *mo_n(PCore *f, uintptr_t n) {
  PCell *k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

// compile and execute expression
NoInline PStatus p_eval(PCore *f) {
  scope c = enscope(f, (scope) nil, nil, nil);
  if (!c) return Oom;
  PWord x = f->sp[0];
  f->sp[0] = (PWord) yieldk;
  PCell *k = 0;
  size_t m = 1;
  avec(f, c,
    m = analyze(f, &c, m, x),
    m = m ? em1(f, &c, m, yield) : m,
    k = m ? construct(f, &c, m) : k);
  k = k ? (PCell*) pushs(f, 1, k) : k;
  if (!k) return Oom;
  f->sp[0] = (PWord) f->ip;
  f->ip = k;
  PStatus s;
#if TCO
  s = f->ip->ap(f, f->ip, f->hp, f->sp);
#else
  for (s = Ok; s == Ok; s = f->ip->ap(f));
  s = s == Eof ? Ok : s;
#endif
  if (s != Ok) f->ip = 0, f->sp = f->pool + f->len;
  else x = f->sp[0], f->ip = (PCell*) *++f->sp, f->sp[0] = x;
  return s; }

void p_close(PCore *f) {
  if (f) free(f->pool < f->loop ? f->pool : f->loop), free(f); }

void p_write1f(PCore *f, PFile *out) {
  transmit(f, out, f->sp[0]); }
