#include "p.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>
#include <unistd.h>

// thanks !!
typedef int PStatus, Status;
typedef struct p_core p_core, PCore, core, Core, proc;
typedef intptr_t p_word, word, Word;
typedef struct type type;
typedef union cell cell, Cell, thread;

// non freestanding headers
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// theres a big benefit in speed from tail call optimization but not all platforms support it

#ifdef TCO
#define YieldStatus PStatusOk
#define Vm(n, ...) Status n(Core *f, thread* Ip, word* Hp, word* Sp, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, Ip, Hp, Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define Continue() Ap(Ip->ap, f)
#else
#define YieldStatus PStatusEof
#define Vm(n, ...) Status n(Core *f, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, ##__VA_ARGS__)
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#define Continue() Ok
#endif

#define Jump(v) Ap(v, f)
#define Have(n) if (Sp - Hp < n) return Ap(gc, f, n)
#define Have1() if (Sp == Hp) return Ap(gc, f, 1)

typedef Vm(vm);
typedef Vm(MVm, int);
union cell { vm *ap; word x; Cell *m; type *typ; };

// primitive type method tables
struct type {
  Word (*copy)(Core*, Word, Word*, Word*); // for gc
  void (*evac)(Core*, Word, Word*, Word*); // for gc
  bool (*equal)(Core*, Word, Word);        // check equality with another object of same type
  void (*emit)(Core*, FILE*, Word);        // print it // replace this with stringify...
  Word (*hash)(Core*, Word); };            // hash it

#define DataHeader vm *ap; type *typ
typedef struct Pair {
  DataHeader;
  word a, b;
} Pair, pair;

typedef struct string {
  DataHeader;
  uintptr_t len;
  char text[];
} string;

typedef struct symbol symbol;
typedef struct table table, Table;

// runtime core data structure -- 1 core = 1 thread of execution
struct p_core {
  cell *ip; // instruction pointer
  word *hp, *sp;
  symbol *symbols; // interned symbol tree
  table *dict,
        *macro;
  symbol *quote,
         *begin,
         *let,
         *cond,
         *lambda;
  uintptr_t len; // memory pool size
  word *pool, *loop; // on and off pool
  struct Mm { // linked list of root cells
    p_word *addr;
    struct Mm *next; } *safe;
  union { uintptr_t t0;  // end time of last gc
          word *cp; }; }; // gc copy pointer

struct tag { Cell *null, *head, end[]; } *ttag(Cell*);

static cell *mo_n(core*, size_t);

static pair
  *ini_pair(pair*, word, word),
  *pairof(core*, word, word);

static table
  *mktbl(core*),
  *table_set(core*, table*, word, word);

static symbol *intern(core*, string*);

static string
  *strof(core*, const char*),
  *ini_str(string*, uintptr_t);

static void
  *bump(core*, uintptr_t),
  *cells(core*, uintptr_t),
  transmit(core*, FILE*, word);

static bool symp(word), strp(word), twop(word),
  not_equal(core*, word, word),
  p_please(core*, uintptr_t),
  eql(core*, word, word);

static word
  table_get(core*, table*, word, word),
  pushs(core*, uintptr_t, ...),
  hash(core*, word),
  cp(core*, word, word*, word*);

static Vm(gc, uintptr_t);
static vm display, bnot, rng, data,
   symnom, ret, ap, apn, tap, tapn,
   jump, cond, ref, imm, yield,
   gensym, ev0, pairp, fixnump, symbolp, stringp,
   ssub, sget, slen, scat, prc, cons, car, cdr,
   lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
   seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
   read0, readf, p_isatty,
   curry;

static type
  string_type,
  pair_type,
  string_type,
  symbol_type,
  table_type;

typedef struct In {
  int (*getc)(struct In*),
      (*ungetc)(struct In*, int),
      (*eof)(struct In*); } In;
typedef struct FileIn {
  struct In in;
  FILE *file; } FileIn;
typedef struct TextIn {
  struct In in;
  const char *text;
  int i; } TextIn;

#define Oom PStatusOom
#define Ok PStatusOk
#define Eof PStatusEof

#define Width(_) b2w(sizeof(_))
#define avail(f) (f->sp-f->hp)
#define getnum(_) ((Word)(_)>>1)
#define putnum(_) (((Word)(_)<<1)|1)
#define nil putnum(0)
#define MM(f,r) ((f->safe=&((struct Mm){(Word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define A(o) ((Pair*)(o))->a
#define B(o) ((Pair*)(o))->b
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define BBA(o) B(B(A(o)))
#define Z(_) ((Word)(_))
#define W(_) ((word)(_))
#define R(_) ((Cell*)(_))
#define nilp(_) (W(_)==nil)
#define nump(_) (W(_)&1)
#define homp(_) (!nump(_))
#define datp(_) (R(_)->ap==data)
#define typof(_) R(_)[1].typ
#define dtyp typof
#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))
#define mix ((uintptr_t)2708237354241864315)
#define consw(...) W(pairof(__VA_ARGS__))

#define pop1(f) (*(f)->sp++)

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))


// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(Word), r = b % sizeof(Word);
  return q + (r ? 1 : 0); }

#define within(a, b, c) (W(a)<=W(b)&&W(b)<W(c))
#define op(n, x) (Ip = (Cell*) Sp[n], Sp[n] = (x), Sp += n, Continue())
#define owns(f, x) within(f->pool, x, f->pool + f->len)

_Static_assert(-1 >> 1 == -1, "support sign extended shift");
_Static_assert(sizeof(Cell*) == sizeof(Cell), "cell is 1 word wide");

#define S1(i) ((Cell[]){{i}})
#define S2(i) ((Cell[]){{curry},{.x=putnum(2)},{i}})
#define S3(i) ((Cell[]){{curry},{.x=putnum(3)},{i}})
#define bifs(_) \
  _("+", S2(add)) _("-", S2(sub)) _("*", S2(mul)) _("/", S2(quot)) _("%", S2(rem)) \
  _("<", S2(lt))  _("<=", S2(le)) _("=", S2(eq))  _(">=", S2(ge))  _(">", S2(gt)) \
  _("rand", S1(rng)) \
  _("X", S2(cons)) _("A", S1(car)) _("B", S1(cdr)) \
  _("sget", S2(sget)) _("ssub", S3(ssub)) _("slen", S1(slen)) _("scat", S2(scat)) \
  _(".", S1(display)) _("putc", S1(prc)) \
  _("~", S1(bnot)) \
  _("thd", S1(thda)) _("peek", S1(peek)) _("poke", S2(poke))\
  _("trim", S1(trim)) _("seek", S2(seek)) \
  _("tnew", S1(tnew)) _("tkeys", S1(tkeys)) _("tlen", S1(tlen))\
  _("tset", S3(tset)) _("tget", S3(tget)) _("tdel", S3(tdel))\
  _("twop", S1(pairp)) _("strp", S1(stringp))\
  _("symp", S1(symbolp)) _("nump", S1(fixnump))\
  _("sym", S1(gensym)) _("nom", S1(symnom))\
  _("ev", S1(ev0))\
  _("isatty", S1(p_isatty))\
  _("read", S1(read0)) _("readf", S1(readf))

#define dict_entry(n, d) {n, d},
static struct { const char *n; Cell *v; } ini_dict[] = { bifs(dict_entry) };

static symbol *symof(core *f, const char *nom) {
  string *o = strof(f, nom);
  return o ? intern(f, o) : 0; }

static NoInline bool p_define(core *f, const char *k, word v) {
  symbol *y;
  avec(f, v, y = symof(f, k));
  return y && table_set(f, f->dict, (Word) y, v); }

static Vm(gc, uintptr_t n) {
  Pack(f);
  int s = p_please(f, n) ? Ok : Oom;
  Unpack(f);
  return s != Ok ? s : Ap(f->ip->ap, f); }

static void *bump(Core *f, size_t n) { void *x = f->hp; return f->hp += n, x; }
static void *cells(Core *f, size_t n) { return
  n <= avail(f) || p_please(f, n) ? bump(f, n) : 0; }

static NoInline void copy_from(core*, word*, uintptr_t);
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
static NoInline bool p_please(core *f, uintptr_t req) {
  word *src = f->pool, *dest = f->loop;
  f->pool = dest, f->loop = src;    // swap
  size_t t0 = f->t0, t1 = clock() , // get last gc end time
         len0 = f->len;             // get original length
  copy_from(f, src, len0);          // copy to new pool
  size_t t2 = f->t0 = clock(),      // get and set last gc end time
         v = t2 == t1 ?             // t1 and t2 can be the same
           v_hi :                   // in that case choose high
           (t2 - t0) / (t2 - t1),   // otherwise take ratio of total run time to gc time
         len1 = len0;               // initial destination size is same as target size
  // calculate the minimum memory required to be able to return true:
  //    req + used = req + (total - free)
  req += len0 - avail(f);
  // if v is out of bounds then calculate len1
  // by inverse proportionally adjusting len and v until v is in bounds
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return true; // no change reqired, hopefully the most common case
                    //
  // at this point we got a new target length and are gonna try and resize
  word *dest2 = malloc(len1 * 2 * sizeof(word)); // allocate pool with the new target size
  return !dest2 ? req <= len0 :                  // if this fails gc still succeeds if the original pool is not too small
   (f->pool = dest2,                             // reset core variables on new pool
    f->len = len1,
    f->loop = dest2 + len1,
    copy_from(f, dest, len0),                    // do second copy
    free(src < dest ? src : dest),               // free original pool
    f->t0 = clock(),                             // set last gc timestamp
    true); }                                     // size successfully adjusted


// this function expects pool loop and len to have been set already on the state
static NoInline void copy_from(core *f, word *p0, uintptr_t len0) {
#define CP(x) cp(f, W(x), p0, t0)
  word len1 = f->len, // target pool length
       *p1 = f->pool, // target pool
       *t0 = p0 + len0, // source pool top
       *t1 = p1 + len1, // target pool top
       *sp0 = f->sp, // source pool stack
       sn = t0 - sp0, // stack height
       *sp1 = t1 - sn; // target pool stack
  // reset stack, heap, symbols
  f->sp = sp1;
  f->hp = f->cp = p1;
  f->symbols = 0;
  // copy variables
  f->ip     = (cell*)   CP(f->ip);
  f->dict   = (table*)  CP(f->dict);
  f->macro  = (table*)  CP(f->macro);
  f->quote  = (symbol*) CP(f->quote);
  f->begin  = (symbol*) CP(f->begin);
  f->let    = (symbol*) CP(f->let);
  f->cond   = (symbol*) CP(f->cond);
  f->lambda = (symbol*) CP(f->lambda);
  // copy stack
  while (sn--) *sp1++ = CP(*sp0++);
  // copy protected values
  for (struct Mm *r = f->safe; r; r = r->next)
    *r->addr = CP(*r->addr);
  // copy all reachable values using cheney's method
  for (cell *k; (k = R(f->cp)) < R(f->hp);)
    if (datp(k)) typof(k)->evac(f, Z(k), p0, t0); // is data
    else { // is thread
      while (k->x) k->x = CP(k->x), k++;
      f->cp = (word*) k + 2; } }

static NoInline word cp(proc *v, word x, word *p0, word *t0) {
  // if it's a number or out of managed memory then return it
  if (nump(x) || !within(p0, x, t0)) return x;
  cell *src = (cell*) x;
  x = src->x;
  // if the cell holds a pointer to the new space then return the pointer
  if (homp(x) && owns(v, x)) return x;
  // if it's data then call the given copy function
  if (datp(src)) return typof(src)->copy(v, (Word) src, p0, t0);
  // it's a thread, find the end
  struct tag *t = ttag(src);
  cell *ini = t->head, *d = bump(v, t->end - ini), *dst = d;
  for (cell *s = ini; (d->x = s->x); s++->x = W(d++));
  d[1].ap = (vm*) dst;
  return W(src - ini + dst); }

static Vm(data) {
  word _ = W(Ip);
  return op(1, _); }

static Vm(pushk_jump) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return Continue(); }

// branch instructions

static Vm(jump) {
  return Ip = Ip[1].m,
         Continue(); }

static Vm(cond) {
  return Ip = nilp(*Sp++) ? Ip[1].m : Ip + 2,
         Continue(); }

// load instructions
//
// push an immediate value
static Vm(imm) {
  Have1();
  *--Sp = Ip[1].x;
  Ip += 2;
  return Continue(); }

// push a value from the stack
static Vm(ref) {
  Have1();
  Sp[-1] = Sp[getnum(Ip[1].x)];
  Sp--;
  Ip += 2;
  return Continue(); }

// call and return
// apply function to one argument
static Vm(ap) {
  if (nump(Sp[1])) Ip++, Sp++;
  else {
    cell *k = R(Sp[1]);
    Sp[1] = Z(Ip + 1);
    Ip = k; }
  return Continue(); }

// tail call
static Vm(tap) {
  word x = Sp[0], j = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) return op(1, j);
  Ip = R(j);
  *Sp = x;
  return Continue(); }

// apply to multiple arguments
static Vm(apn) {
  size_t n = getnum(Ip[1].x);
  cell *ra = Ip + 2; // return address
  Ip = R(Sp[n]) + 2; // this instruction is only emitted when the callee is known to be a function
  Sp[n] = Z(ra); // store return address
  return Continue(); }

// tail call
static Vm(tapn) {
  size_t n = getnum(Ip[1].x),
         r = getnum(Ip[2].x);
  Ip = R(Sp[n]) + 2;
  word *o = Sp;
  for (Sp += r + 1; n--; Sp[n] = o[n]);
  return Continue(); }

// return
static Vm(ret) {
  word n = getnum(Ip[1].x) + 1;
  return op(n, *Sp); }

// exit vm and return to C
static Vm(yield) { return Pack(f), YieldStatus; }

// currying
static Vm(curry) {
  Cell *k = R(Hp), *j = k;
  size_t S = 3 + Width(struct tag),
         n = getnum(Ip[1].x);

  if (n == 2) { Have(S); }
  else {
    S += 2, j += 2;
    Have(S);
    k[0].ap = curry, k[1].x = putnum(n - 1); }

  j[0].ap = pushk_jump, j[1].x = *Sp++, j[2].m = Ip + 2;
  j[3].x = 0, j[4].m = k;

  return
    Hp += S,
    Ip = R(*Sp),
    *Sp = Z(k),
    Continue(); }

// at all times vm is running a function. thread compiler tracks
// function state using this type
typedef struct Env {
  // these parameters represent stack state at a point in compile process
  Word args,  // list // function positional arguments (never empty)
       imps,  // list // closure variables
       stack; // list // current values on stack
  // these are values of variables known at compile time
  Word lams; // alist // known function definitions
  // these are two stacks of jump target addresses for conditional expressions
  Word alts, // list // alternate branch address stack
       ends; // list // exit branch address stack
  // this is the enclosing function PEnv* if any
  struct Env *par;
} PEnv, Env;

static long lidx(PCore*, Word, Word);

// thread compiler functions are defined in two phases:
#define Ana(n, ...) size_t n(Core *f, Env **c, size_t m, Word x, ##__VA_ARGS__)
// ana phase
// - takes an expression and returns an upper bound for the length of a thread that
//   pushes the input expression's value onto the stack.
// - side effect: top of stack is now a function pointer that when called with a
//   properly initialized thread object, starts next compiler phase.
#define Cata(n, ...) Cell *n(Core *f, Env **c, Cell *k, ##__VA_ARGS__)
// cata phase
// - takes a thread pointer, emits code immediately prior to the indexed instruction,
//   pops a precomputed (by phase 0) continuation off the stack and calls it with the
//   decremented pointer.

typedef Ana(ana);
typedef Cata(cata);

static ana analyze, ana_if, ana_let, ana_list;
static Env *envup(Core*, Env*, Word, Word);
static size_t ana_i1(Core*, Env**, size_t, vm*);
static Cell *construct(Core*, Env**, size_t);
static Cata(yieldk) { return k; }
static Word
  ana_lam(Core*, Env**, Word, Word),
  lassoc(Core*, Word, Word),
  lconcat(Core*, Word, Word),
  rlconcat(Core*, Word, Word);


static cata
  cataap, cataapn, cata_var,
  generate_cond_push_branch,
  generate_cond_pop_branch,
  generate_cond_push_exit,
  generate_cond_pop_exit,
  generate_cond_peek_exit;

// compile and execute expression
static NoInline int p_eval(Core *f) {
  Env *c = envup(f, (PEnv*) nil, nil, nil);
  if (!c) return Oom;
  Word x = f->sp[0];
  f->sp[0] = (Word) yieldk;
  Cell *k = 0;
  size_t m = 1;
  avec(f, c,
    m = analyze(f, &c, m, x),
    m = m ? ana_i1(f, &c, m, yield) : m,
    k = m ? construct(f, &c, m) : k);
  k = k ? (Cell*) pushs(f, 1, k) : k;
  if (!k) return Oom;
  f->sp[0] = (Word) f->ip;
  f->ip = k;
  int s;
#ifdef TCO
  s = f->ip->ap(f, f->ip, f->hp, f->sp);
#else
  do s = f->ip->ap(f); while (s == Ok);
  s = s == Eof ? Ok : s;
#endif
  if (s != Ok) f->ip = 0, f->sp = f->pool + f->len;
  else x = f->sp[0], f->ip = (Cell*) *++f->sp, f->sp[0] = x;
  return s; }


Vm(ev0) {
  Pack(f);
  int s = p_eval(f);
  Unpack(f);
  return s == Ok ? op(1, *Sp) : s; }

static cell *mo_ini(thread *_, size_t len) {
  struct tag *t = (struct tag*) (_ + len);
  return t->null = NULL, t->head = _; }

// allocate a thread
static cell *mo_n(Core *f, size_t n) {
  Cell *k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

// PEnv* constructor
static Env *envup(Core *f, Env* par, Word args, Word imps) {
  if (!pushs(f, 3, args, imps, par)) return 0;
  Env *c = (Env*) mo_n(f, Width(Env));
  args = pop1(f), imps = pop1(f), par = (Env*) pop1(f);
  if (c)
    c->args = args, c->imps = imps, c->par = par,
    c->stack = c->alts = c->ends = c->lams = nil;
  return c; }

// basic functions
static Inline Cata(pull) { return ((cata*) (*f->sp++))(f, c, k); }
static Cata(cata_i) { return k[-1].x = *f->sp++, pull(f, c, k - 1); }
static Cata(cata_ix) { return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull(f, c, k - 2); }
static Cell *construct(Core *f, Env **c, size_t m) {
  Cell *k = mo_n(f, m);
  if (!k) return k;
  memset(k, -1, m * sizeof(Word));
  return pull(f, c, k + m); }

// generic instruction ana handlers
static size_t ana_i1(Core *f, Env **c, size_t m, vm *i) {
  return pushs(f, 2, cata_i, i) ? m + 1 : 0; }
static size_t ana_i2(Core *f, Env **c, size_t m, vm *i, Word x) {
  return pushs(f, 3, cata_ix, i, x) ? m + 2 : 0; }

// conditional expression analyzer
static NoInline Ana(ana_if) {
  if (!pushs(f, 2, x, generate_cond_pop_exit)) return 0;
  Pair p = { 0, 0, nil, nil };
  x = pop1(f);
  MM(f, &x);
  for (; m; x = BB(x)) {
    if (!twop(x)) x = (Word) &p;
    m = analyze(f, c, m + 2, A(x));
    if (!twop(B(x))) { // this means we have just analyzed the default case
      m = pushs(f, 1, generate_cond_peek_exit) ? m : 0; // now branch to exit
      break; }
    // otherwise we analyzed a conditional test
    // pop the last branch address off the stack to be jumped to in case the test failed
    m = pushs(f, 1, generate_cond_pop_branch) ? m : 0;
    // otherwise here is the consequent
    m = m ? analyze(f, c, m + 2, AB(x)) : m;
    // after consequent jump to exit, then push new branch address at present code position (these are emitted backwards)
    m = m && pushs(f, 2, generate_cond_push_branch,
                         generate_cond_peek_exit) ? m : 0; }
  UM(f);
  m = m && pushs(f, 1, generate_cond_push_exit) ? m : 0; // push the exit address for the whole conditional
  return m; }

// first emitter called for cond expression
// pushes cond expression exit address onto PEnv* stack ends
static Cata(generate_cond_push_exit) {
  pair *w = pairof(f, W(k), (*c)->ends);
  return !w ? 0 : pull(f, c, (Cell*) A((*c)->ends = (Word) w)); }

// last emitter called for cond expression
// pops cond expression exit address off PEnv* stack ends
static Cata(generate_cond_pop_exit) {
  return (*c)->ends = B((*c)->ends), pull(f, c, k); }

static Cata(generate_cond_push_branch) {
  pair *w = pairof(f, W(k), (*c)->alts);
  if (!w) return (Cell*) w;
  (*c)->alts = (word) w;
  k = (Cell*) w->a;
  return pull(f, c, k); }

static Cata(generate_cond_peek_exit) {
  k -= 2;
  Cell *addr = (Cell*) A((*c)->ends);
  // if the destination is a return or tail call,
  // then copy it forward instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k[0].ap = addr[0].ap, k[1].x = addr[1].x;
  else k[0].ap = jump, k[1].x = (Word) addr;
  return pull(f, c, k); }

// last emitter called for a branch
// pops next branch address off PEnv* stack alts
static Cata(generate_cond_pop_branch) {
  return k[-2].ap = cond,
         k[-1].x = A((*c)->alts),
         (*c)->alts = B((*c)->alts),
         pull(f, c, k - 2); }

static bool lambp(PCore *f, Word x) {
  return twop(x) && A(x) == (Word) f->lambda; }

// DEFINE
// let expressions
static Word ldels(Core *f, Word lam, Word l) {
  if (!twop(l)) return nil;
  Word m = ldels(f, lam, B(l));
  if (!lassoc(f, lam, A(l))) B(l) = m, m = l;
  return m; }

static word desugr(core *f, word *d, word *e, word a) {
  if (!twop(a)) return consw(f, *e, nil);
  Word b; avec(f, a, b = desugr(f, d, e, B(a)));
  return !b ? b : consw(f, A(a), b); }

static int desug(core *f, word *d, word *e) {
  if (!twop(*d)) return Ok;
  Word x;
  if (!pushs(f, 1, f->lambda)) return Oom;
  do if (!(x = desugr(f, d, e, B(*d))) ||
         !(x = consw(f, f->sp[0], x)))
    return Oom;
  else *d = A(*d), *e = x;
  while (twop(*d));
  return f->sp++, Ok; }

// list length
static size_t llen(Word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

static Vm(defglobal) {
  Pack(f);
  if (!table_set(f, f->dict, Ip[1].x, Sp[0])) return Oom;
  Unpack(f);
  return op(1, Sp[0]); }

// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.
static size_t ana_let(Core *f, Env* *b, size_t m, Word exp) {
  if (!twop(exp)) return analyze(f, b, m, nil);
  if (!twop(B(exp))) return analyze(f, b, m, A(exp));
  Env *q = *b, **c = &q;
  avec(f, exp, q = envup(f, q, q->args, q->imps));
  if (!q) return 0;
  // lots of variables :(
#define fail() (f->safe = mm, 0)
  struct Mm *mm = f->safe;
  Word nom = nil, def = nil, lam = nil, v = nil, d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &exp), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

  // collect vars and defs into two lists
  for (; twop(exp) && twop(B(exp)); exp = BB(exp)) {
    d = A(exp), e = AB(exp), desug(f, &d, &e);
    if (!(nom = consw(f, d, nom)) ||
        !(def = consw(f, e, def)))
      return fail();
    if (lambp(f, e)) {
      // if it's a lambda compile it and record in lam list
      Word x = ana_lam(f, c, nil, B(e));
      x = x ? consw(f, d, x) : x;
      lam = x ? consw(f, x, lam) : x;
      if (!lam) return fail(); } }

  // if there's no body then evaluate the name of the last definition
  bool even = !twop(exp); // we check this again later to make global bindings at top level
  if (even && !(exp = consw(f, A(nom), nil))) return fail();

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
          Word vars = BBA(e), var = A(v);
          if (lidx(f, vars, var) < 0) { // only add if it's not already there
            if (!(vars = consw(f, var, vars))) return fail(); // oom
            j++, BBA(e) = vars; } }
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e));

  (*c)->lams = lam;
  // construct lambda with reversed argument list
  exp = lconcat(f, nom, exp);
  symbol *l = exp ? f->lambda : 0;
  exp = l ? consw(f, Z(l), exp) : 0;
  m = exp ? analyze(f, b, m, exp) : 0; // exp is now the required lambda, analyze it
  if (!m || !((*b)->stack = consw(f, nil, (*b)->stack))) return fail();

  // reverse the nom and def lists
  nom = rlconcat(f, nom, nil), def = rlconcat(f, def, nil);
  // evaluate definitions in order tracking var names on stack list
  // store lambdas on PEnv* for lazy binding and construct new application
  // - reverse noms onto exp
  // - reverse onto e = nil and recompile lambdas
  size_t nn = 0;
  for (; twop(nom); nom = B(nom), def = B(def), nn++) {
    // if lambda then recompile with the explicit closure
    // and put in arg list and lam list (latter is used for lazy binding)
    if (lambp(f, A(def))) {
      d = lassoc(f, lam, A(nom));
      Word _;
      if (!(_ = ana_lam(f, c, BB(d), BA(def)))) return fail();
      else A(def) = B(d) = _; }
    // if toplevel then bind
    if (even && nilp((*b)->args)) {
      Cell *t = cells(f, 2 * Width(pair) + 2 + Width(struct tag));
      if (!t) return fail();
      pair *w = (pair*) t,
           *x = w + 1;
      t += 2 * Width(pair);
      t[0].ap = defglobal, t[1].x = A(nom), t[2].x = 0, t[3].m = t;
      ini_pair(w, A(def), nil); // dict add
      ini_pair(x, Z(t), Z(w));
      A(def) = Z(x); }
    if (!(m = analyze(f, b, m, A(def))) ||
        !((*b)->stack = consw(f, A(nom), (*b)->stack)))
      return fail(); }

  m = nn <= 1 ? pushs(f, 1, cataap) ? m + 1 : 0 :
                pushs(f, 2, cataapn, putnum(nn)) ? m + 2 : 0;
  for (nn++; nn--; (*b)->stack = B((*b)->stack));
  return f->safe = mm, m; }

static Vm(drop1) { return Ip++, Sp++, Continue(); }

static size_t ana_imm(Core *f, Env **c, size_t m, Word x) {
  return ana_i2(f, c, m, imm, x); }

static size_t ana_seq(Core *f, Env* *c, size_t m, Word x) {
  if (!twop(x)) return ana_imm(f, c, m, nil);
  MM(f, &x);
  while (m && twop(B(x)))
    m = analyze(f, c, m, A(x)),
    m = m ? ana_i1(f, c, m, drop1) : m,
    x = B(x);
  UM(f);
  return m ? analyze(f, c, m, A(x)) : m; }

static Ana(ana_mac, Word b) {
  if (!pushs(f, 3, f->quote, x, b)) return 0;
  Pair *mxp = (Pair*) cells(f, 4 * Width(Pair));
  if (!mxp) return 0;
  x = Z(ini_pair(mxp, f->sp[1], Z(ini_pair(mxp+1, Z(ini_pair(mxp+2, f->sp[0],  Z(ini_pair(mxp+3, f->sp[2], nil)))), nil))));
  *(f->sp += 2) = x;
  return p_eval(f) != Ok ? 0 : analyze(f, c, m, pop1(f)); }

// evaluate function call arguments and apply
static size_t ana_args(Core *f, Env* *c, size_t m, Word x) {
  MM(f, &x); // handle oom here ..
  m = ((*c)->stack = consw(f, nil, (*c)->stack)) ? m : 0;
  while (m && twop(x))
    m = analyze(f, c, m + 1, A(x)),
    x = B(x),
    m = m && pushs(f, 1, cataap) ? m : 0;
  return
    (*c)->stack = B((*c)->stack),
    UM(f),
    m; }

static size_t ana_args_r2l(Core *f, Env* *c, size_t m, Word x) {
  if (!twop(x)) return m;
  Word a = A(x);
  MM(f, &a);
  m = ana_args_r2l(f, c, m, B(x));
  UM(f);
  if (!m) return m;
  m = analyze(f, c, m, a);
  if (!((*c)->stack = consw(f, nil, (*c)->stack))) return 0;
  return m; }

static size_t ana_args_r(core *f, PEnv**c, size_t m, word x) {
  avec(f, x, m = ((*c)->stack = consw(f, nil, (*c)->stack)) ? m : 0);
  return m ? ana_args_r2l(f, c, m, x) : m; }


static Inline bool can_apn(core *f, size_t argc) {
  Word s0 = f->sp[0], s1 = f->sp[1], s2 = f->sp[2];
  return
    s0 == Z(cata_ix) &&
    s1 == Z(imm) &&
    homp(s2) &&
    R(s2)[0].ap == curry &&
    getnum(R(s2)[1].x) == argc; }

static size_t ana_ap(Core *f, Env* *c, size_t m, Word fn, Word args) {
  size_t argc = llen(args);
  avec(f, args, m = analyze(f, c, m, fn));
  if (!m || !argc) return m;
  if (argc == 1 || !can_apn(f, argc)) return ana_args(f, c, m, args);
  if (!(m = ana_args_r(f, c, m + 2, args)) ||
      !pushs(f, 2, cataapn, putnum(argc)))
    return 0;
  do (*c)->stack = B((*c)->stack); while (argc--);
  return m; }

static Ana(ana_list) {
  Word a = A(x), b = B(x);
  if (!twop(b)) return analyze(f, c, m, a); // singleton list has value of first element
  if (a == (Word) f->quote) return ana_imm(f, c, m, twop(b) ? A(b) : nil);
  if (a == (Word) f->begin) return ana_seq(f, c, m, b);
  if (a == (Word) f->let) return ana_let(f, c, m, b);
  if (a == (Word) f->cond) return ana_if(f, c, m, b);
  if (a == (Word) f->lambda) return
    (x = ana_lam(f, c, nil, b)) ? analyze(f, c, m, x) : x;
  Word macro = table_get(f, f->macro, a, 0);
  return macro ? ana_mac(f, c, m, macro, b) :
                 ana_ap(f, c, m, a, b); }

static Vm(free_variable) {
  Word x = Ip[1].x;
  x = table_get(f, f->dict, x, nil); // error here if you want on undefined variable
  Ip[0].ap = imm;
  Ip[1].x = x;
  return Continue(); }

static Vm(lazy_bind) {
  Word ref = Ip[1].x, var = A(ref);
  Env *env = (Env*) B(ref);
  var = AB(lassoc(f, env->lams, var));
  Ip[0].ap = imm;
  Ip[1].x = var;
  return Continue(); }

static Word lassoc(Core *f, Word l, Word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
  return 0; }

// list concat
static Word lconcat(Core *f, Word l, Word n) {
  if (!twop(l)) return n;
  avec(f, l, n = lconcat(f, B(l), n));
  return n ? consw(f, A(l), n) : n; }

// reverse list concat
static Word rlconcat(Core *f, Word l, Word n) {
  for (Word m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
  return n; }

static Ana(ana_sym_r, Env *d);
static Ana(analyze) {
  if (twop(x)) return ana_list(f, c, m, x);
  if (symp(x)) return ana_sym_r(f, c, m, x, *c);
  return ana_imm(f, c, m, x); }

// index of item in list
static long lidx(Core *f, Word l, Word x) {
  for (long i = 0; twop(l); l = B(l), i++) if (eql(f, x, A(l))) return i;
  return -1; }

static long stack_index_of_symbol(Core *f, Env *c, Word var) {
  long l, i = 0;
  for (l = c->imps; twop(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  for (l = c->args; twop(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  return -1; }

static Ana(ana_sym_free) {
  Word y = table_get(f, f->dict, x, 0);
  if (y) return ana_imm(f, c, m, y);
  x = consw(f, x, (*c)->imps), // XXX why is this needed???
  x = x ? A((*c)->imps = x) : x;
  return x ? ana_i2(f, c, m, free_variable, x) : x;  }

static Ana(ana_sym_local_fn, Env *d) {
  x = consw(f, x, Z(d));
  m = x ? ana_i2(f, c, m, lazy_bind, x) : 0;
  if (!m) return m;
  x = f->sp[2]; // get the (symbol arg1 arg2 ...)
  Word y = BBA(x); // get the args
  A(x) = AA(x); // set car of pair to just the symbol -- (symbol . PEnv*)
  return ana_args(f, c, m, y); }

static Cata(c2var) {
  Word var = *f->sp++, stack = *f->sp++;
  long i = lidx(f, stack, var);
  return k[-2].ap = ref,
         k[-1].x = putnum(i),
         pull(f, c, k - 2); }

// emit stack reference instruction
static Cata(cata_var) {
  Word
    var = *f->sp++, // variable name
    ins = llen(*f->sp++), // stack inset
    idx = stack_index_of_symbol(f, *c, var);
  return
    k[-2].ap = ref,
    k[-1].x = putnum(idx + ins),
    pull(f, c, k - 2); }

static Ana(ana_sym_local_def, Word stack) {
  return pushs(f, 3, c2var, x, stack) ? m + 2 : 0; }

static Ana(ana_sym_stack_ref, Env *d) {
  if (*c != d) // if we have found the variable in an enclosing scope then import it
    x = consw(f, x, (*c)->imps),
    x = x ? A((*c)->imps = x) : x;
  return pushs(f, 3, cata_var, x, (*c)->stack) ? m + 2 : 0; }

static Ana(ana_sym_r, Env *d) {
  // free symbol?
  if (nilp(d)) return ana_sym_free(f, c, m, x);
  // defined as a function by a local let binding?
  Word y = lassoc(f, d->lams, x);
  if (y) return ana_sym_local_fn(f, c, m, y, d);
  // bound on the stack by a local let binding?
  if (lidx(f, d->stack, x) >= 0)
    return ana_sym_local_def(f, c, m, x, d->stack);
  // bound on the stack as a closure or positional argument?
  if (stack_index_of_symbol(f, d, x) >= 0)
    return ana_sym_stack_ref(f, c, m, x, d);
  // otherwise recur on the enclosing env
  return ana_sym_r(f, c, m, x, d->par); }

// emits call instruction and modifies to tail call
// if next operation is return
static Cata(cataap) {
  if (k->ap == ret) k->ap = tap; // tail call
  else (--k)->ap = ap; // regular call
  return pull(f, c, k); } // ok

static Cata(cataapn) {
  Word n = *f->sp++;
  if (k->ap == ret) k->x = n, (--k)->ap = tapn;
  else (--k)->x = n, (--k)->ap = apn;
  return pull(f, c, k); }

// lambda decons pushes last list item to stack returns init of list
static Word linit(Core *f, Word x) {
  if (!twop(x)) return pushs(f, 1, nil) ? nil : 0;
  if (!twop(B(x))) return pushs(f, 1, A(x)) ? nil : 0;
  Word y = A(x);
  avec(f, y, x = linit(f, B(x)));
  return x ? consw(f, y, x) : x; }

static Word ana_lam(Core *f, Env **c, Word imps, Word exp) {
  // expediently storing exp in args for the moment
  Env *d = envup(f, *c, exp, imps);
  if (!d) return 0;
  MM(f, &d);
  // get the real args
  Word args = linit(f, d->args);
  if (!(d->args = args)) goto fail;
  exp = f->sp[0], f->sp[0] = Z(yieldk);
  size_t m = analyze(f, &d, 4, exp);
  if (!m) goto fail;
  size_t arity = llen(d->args) + llen(d->imps);
  Cell *k = pushs(f, 3, cata_ix, ret, putnum(arity)) ? construct(f, &d, m) : 0;
  if (!k) goto fail;
  if (arity > 1) (--k)->x = putnum(arity), (--k)->ap = curry;
  UM(f);
  return consw(f, Z(ttag(k)->head = k), d->imps);
fail:
  UM(f);
  return 0; }

#define p_getc(i) getc(i)
#define p_ungetc(i, c) ungetc(c, i)
#define p_eof(i) feof(i)
#define fi(i) ((FileIn*)(i))

static int p_in_getc(In *i) { return i->getc(i); }
static int p_in_ungetc(In *i, int c) { return i->ungetc(i, c); }
static int p_in_eof(In *i) { return i->eof(i); }

static int p_file_getc(In *i) { return getc(fi(i)->file); }
static int p_file_ungetc(In *i, int c) { return ungetc(c, fi(i)->file); }
static int p_file_eof(In *i) { return feof(fi(i)->file); }

#define ti(i) ((TextIn*)(i))
static int p_text_getc(In *i) {
  TextIn *t = ti(i);
  char c = t->text[t->i];
  if (c) t->i++;
  return c; }

static int p_text_ungetc(In *i, int _) {
  TextIn *t = ti(i);
  int idx = t->i;
  idx = idx ? idx - 1 : idx;
  t->i = idx;
  return t->text[idx]; }

static int p_text_eof(In *i) {
  return !ti(i)->text[ti(i)->i]; }

////
/// " the parser "
//
//
// get the next significant character from the stream
static int read_char(Core *f, In *i) {
  for (int c;;) switch (c = p_in_getc(i)) {
    default: return c;
    case '#': case ';': while (!p_in_eof(i) && (c = p_in_getc(i)) != '\n' && c != '\r');
    case ' ': case '\t': case '\n': case '\r': case '\f': continue; } }

static int
  reads(Core*, In*),
  read_string(Core*, In*, char),
  read_atom(Core*, In*);

static Status enquote(Core *f) {
  Pair *w = pairof(f, f->sp[0], nil);
  if (!w) return Oom;
  f->sp[0] = (word) w;
  w = pairof(f, W(f->quote), f->sp[0]);
  if (!w) return Oom;
  f->sp[0] = W(w);
  return Ok; }

static int p_read1(core *f, In* i) {
  int c = read_char(f, i);
  switch (c) {
    case EOF:  return Eof;
    case '\'': return (c = p_read1(f, i)) == Ok ? enquote(f) : c;
    case '(':  return reads(f, i);
    case ')':  return pushs(f, 1, nil) ? Ok : Oom;
    case '"':  return read_string(f, i, '"');
    default:   return p_in_ungetc(i, c),
                      read_atom(f, i); } }

static int reads(Core *f, In* i) {
  word c = read_char(f, i);
  if (c == EOF || c == ')') return pushs(f, 1, nil) ? Ok : Oom;
  p_in_ungetc(i, c);
  int s = p_read1(f, i);
  s = s == Ok ? reads(f, i) : s;
  return s != Ok ? s :
    !(c = consw(f, f->sp[1], f->sp[0])) ? Oom :
    (*++f->sp = c, Ok); }

// create and grow buffers for reading
static string *bnew(Core *f) {
  string *s = cells(f, Width(string) + 1);
  return s ? ini_str(s, sizeof(word)) : s; }

static string *bgrow(Core *f, string *s) {
  string *t; uintptr_t len = s->len;
  avec(f, s, t = cells(f, Width(string) + 2 * b2w(len)));
  if (t) memcpy(ini_str(t, 2 * len)->text, s->text, len);
  return t; }

static int read_string(Core *f, In* i, char delim) {
  string *b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(Word); b; b = bgrow(f, b), lim *= 2)
    while (n < lim)
      if ((c = p_in_getc(i)) == EOF ||
           c == delim ||
           (c == '\\' && (c = p_in_getc(i)) == EOF))
        goto out;
      else b->text[n++] = c;
out:
  if (!b) return Oom;
  b->len = n;
  return pushs(f, 1, b) ? Ok : Oom; }

static int read_atom(core *f, In *i) {
  string *b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(word); b; b = bgrow(f, b), lim *= 2)
    while (n < lim) switch (c = p_in_getc(i)) {
      case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
      case '(': case ')': case '"': case '\'': case EOF: p_in_ungetc(i, c); goto out;
      default: b->text[n++] = c; } out:
  if (!b) return Oom;
  b->len = n, b->text[n] = 0; // zero terminate for strtol ; n < lim so this is safe
  char *e; long j = strtol(b->text, &e, 0);
  Word x = *e == 0 ? putnum(j) : Z(intern(f, b));
  return !x || !pushs(f, 1, x) ? Oom : Ok; }

static NoInline int p_read1f(core *f, FILE* i) {
  FileIn fi = {{p_file_getc, p_file_ungetc, p_file_eof}, i};
  return p_read1(f, (In*) &fi); }

Vm(read0) {
  Pack(f);
  int s = p_read1f(f, stdin);
  if (s == Eof) return Unpack(f), op(1, nil);
  if (s != Ok) return s;
  pair *p = pairof(f, f->sp[0], f->sp[1]);
  if (!p) return Oom;
  return Unpack(f),
         op(2, W(p)); }

Vm(readf) {
  if (!strp(Sp[0])) return op(1, nil);
  string *s = (string*)Sp[0];
  if (s->len > 255) return op(1, nil);
  char n[265]; // :)
  memcpy(n, s->text, s->len);
  n[s->len] = 0;
  FILE *i = fopen(n, "r");
  if (!i) return op(1, nil);
  Pack(f);
  FileIn fi = {{p_file_getc, p_file_ungetc, p_file_eof}, i};
  int t = reads(f, (In*)&fi);
  fclose(i);
  return t == Ok ? (Unpack(f), op(2, *Sp)) : t; }

Vm(p_isatty) { return op(1, isatty(getnum(*Sp)) ? putnum(-1) : nil); }
Vm(prc)     { word w = *Sp; putc(getnum(w), stdout);     return op(1, w); }
Vm(display) { word w = *Sp; transmit(f, stdout, w); return op(1, w); }

static void transmit(core *f, FILE* out, word x) {
  if (nump(x)) fprintf(out, "%ld", (long) getnum(x));
  else if (datp(x)) dtyp(x)->emit(f, out, x);
  else fprintf(out, "#%lx", (long) x); }

static NoInline word pushsr(core *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (!n) return p_please(f, m) ? m : n;
  word x = va_arg(xs, word), y;
  avec(f, x, y = pushsr(f, m, n - 1, xs));
  return y ? *--f->sp = x : y; }

static NoInline word vpushs(core *f, uintptr_t m, va_list xs) {
  word n, r = 0;
  if (avail(f) < m) r = pushsr(f, m, m, xs);
  else for (n = 0, f->sp -= m; n < m; f->sp[n++] = r = va_arg(xs, Word));
  return r; }

word pushs(core *f, uintptr_t m, ...) {
  va_list xs;
  va_start(xs, m);
  word r = vpushs(f, m, xs);
  va_end(xs);
  return r; }

bool twop(word _) { return homp(_) && dtyp(_) == &pair_type; }

pair *pairof(core *f, word a, word b) {
  if (avail(f) < Width(pair)) {
    bool ok;
    avec(f, a, avec(f, b, ok = p_please(f, Width(pair))));
    if (!ok) return 0; }
  pair *w = (pair*) f->hp;
  f->hp += Width(pair);
  return ini_pair(w, a, b); }

Vm(car) { return op(1, twop(Sp[0]) ? A(Sp[0]) : Sp[0]); }
Vm(cdr) { return op(1, twop(Sp[0]) ? B(Sp[0]) : nil); }
Vm(cons) {
  Have(Width(pair));
  pair *w = ini_pair((pair*) Hp, Sp[0], Sp[1]);
  Hp += Width(pair);
  return op(2, Z(w)); }

static Word cp_two(Core *v, Word x, Word *p0, Word *t0) {
  Pair *src = (Pair*) x,
       *dst = ini_pair(bump(v, Width(Pair)), src->a, src->b);
  return Z(src->ap = (vm*) dst); }

static void wk_two(Core *f, Word x, Word *p0, Word *t0) {
  f->cp += Width(Pair);
  A(x) = cp(f, A(x), p0, t0);
  B(x) = cp(f, B(x), p0, t0); }

static void print_two(Core *f, FILE *o, Word x) {
  for (putc('(', o);; putc(' ', o)) {
    transmit(f, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

// FIXME could overflow the stack -- use off pool for this
static bool eq_two(Core *f, Word x, Word y) {
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static Word hash_two(PCore *f, Word x) {
  Word hc = hash(f, A(x)) * hash(f, B(x));
  return hc ^ mix; }

static Vm(pairp) { return op(1, twop(Sp[0]) ? putnum(-1) : nil); }
static pair *ini_pair(pair *w, word a, word b) {
  return w->ap = data,
         w->typ = &pair_type,
         w->a = a,
         w->b = b,
         w; }

static word list_r(Core *f, size_t n, va_list xs) {
  if (n == 0) return nil;
  word x = va_arg(xs, Word), r;
  avec(f, x, r = list_r(f, n - 1, xs));
  return r ? consw(f, x, r) : r; }

static word list(Core *f, size_t n, ...) {
  va_list xs;
  va_start(xs, n);
  word r = list_r(f, n, xs);
  va_end(xs);
  return  r; }

static bool strp(Word _) { return homp(_) && dtyp(_) == &string_type; }

static Vm(slen) {
  Word x = Sp[0];
  return op(1, strp(x) ? putnum(((string*)x)->len) : nil); }

static Vm(ssub) {
  Cell* r = (Cell*) Sp[3];
  if (!strp(Sp[0])) Sp[3] = nil;
  else {
    string *s = (string*) Sp[0];
    intptr_t i = nump(Sp[1]) ? getnum(Sp[1]) : 0,
             j = nump(Sp[2]) ? getnum(Sp[2]) : 0;
    i = max(i, 0), i = min(i, s->len);
    j = max(j, i), j = min(j, s->len);
    if (i == j) Sp[3] = nil;
    else {
      size_t req = Width(string) + b2w(j - i);
      Have(req);
      string* t = ini_str((string*) Hp, j - i);
      Hp += req;
      memcpy(t->text, s->text + i, j - i);
      Sp[3] = (Word) t; } }
  Ip = r, Sp += 3;
  return Continue(); }

static Vm(sget) {
  Cell *r = (Cell*) Sp[2];
  if (!strp(Sp[0])) Sp[2] = nil;
  else {
    string*s = (string*) Sp[0];
    size_t i = min(s->len - 1, getnum(Sp[1]));
    i = max(i, 0);
    Sp[2] = putnum(s->text[i]); }
  Ip = r, Sp += 2;
  return Continue(); }

static Vm(scat) {
  word a = Sp[0], b = Sp[1];
  if (!strp(a)) return op(2, b);
  if (!strp(b)) return op(2, a);
  string *x = (string*) a, *y = (string*) b;
  size_t len = x->len + y->len,
         req = Width(string) + b2w(len);
  Have(req);
  string*z = ini_str((string*) Hp, len);
  Hp += req;
  memcpy(z->text, x->text, x->len);
  memcpy(z->text + x->len, y->text, y->len);
  return op(2, (Word) z); }

static Vm(stringp) { return op(1, strp(Sp[0]) ? putnum(-1) : nil); }
static string* ini_str(string *s, uintptr_t len) {
  return s->ap = data,
         s->typ = &string_type,
         s->len = len,
         s; }

static Word copy_string(Core* v, Word x, Word *p0, Word *t0) {
  string* src = (string*) x;
  size_t len = sizeof(string) + src->len;
  return (Word) (src->ap = memcpy(bump(v, b2w(len)), src, len)); }

static void walk_string(Core* f, Word x, Word *p0, Word *t0) {
  f->cp += Width(string) + b2w(((string*) x)->len); }

static void print_string(Core* v, FILE *o, Word _) {
  string* s = (string*) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

static word hash_string(core *v, word _) {
  string *s = (string*) _;
  uintptr_t h = 1;
  size_t words = s->len / sizeof(word),
         bytes = s->len % sizeof(word);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const intptr_t *ws = (intptr_t*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static bool string_equal(Core *f, Word x, Word y) {
  string *a = (string*) x, *b = (string*) y;
  return a->len == b->len && 0 == strncmp(a->text, b->text, a->len); }

static string *strof(Core *f, const char *text) {
  size_t len = strlen(text);
  string *o = cells(f, Width(string) + b2w(len));
  if (o) memcpy(ini_str(o, len)->text, text, len);
  return o; }

typedef struct table {
  DataHeader;
  uintptr_t len, cap;
  struct entry {
    word key, val;
    struct entry *next;
  } **tab;
} Table, table;

static Inline bool tblp(word _) { return homp(_) && dtyp(_) == &table_type; }

static table *ini_table(table *t, uintptr_t len, uintptr_t cap, struct entry**tab) {
  return t->ap = data,
         t->typ = &table_type,
         t->len = len,
         t->cap = cap,
         t->tab = tab,
         t; }

// FIXME very poor hashing method :(
static word hash_table(core *f, word h) { return mix; }

static word copy_table(core *f, word x, word *p0, word *t0) {
  table *src = (table*) x;
  word i = src->cap;
  table *dst = bump(f, Width(table) + i);
  src->ap = (vm*) ini_table(dst, src->len, src->cap, (void*) (dst + 1));

  //FIXME do these allocations in a block with the rest
  for (struct entry *s, *e, *d; i--; dst->tab[i] = e)
    for (s = src->tab[i], e = NULL; s;
      d = bump(f, Width(struct entry)),
      d->key = s->key, d->val = s->val,
      d->next = e, e = d,
      s = s->next);
  return (Word) dst; }

static void walk_table(core *f, word x, word *p0, word *t0) {
  table *t = (table*) x;
  f->cp += Width(table) + t->cap + t->len * Width(struct entry);
  for (word i = 0, lim = t->cap; i < lim; i++)
    for (struct entry*e = t->tab[i]; e;
      e->key = cp(f, e->key, p0, t0),
      e->val = cp(f, e->val, p0, t0),
      e = e->next); }

static void print_table(Core *f, FILE *o, Word x) {
  table *t = (table*) x;
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

word hash(core *f, word x) {
  if (nump(x)) {
    const int shift = sizeof(word) * 4;
    return x *= mix, (x << shift) | (x >> shift); }

  if (datp(x)) return dtyp(x)->hash(f, x);
  if (!owns(f, x)) return mix ^ (mix * x);

  // it's a function, hash by length
  struct tag *t = ttag((Cell*) x);
  word len = (Cell*) t - t->head;
  return mix ^ (mix * len); }

table *mktbl(core *f) {
  table *t = cells(f, Width(table) + 1);
  if (!t) return t;
  struct entry**tab = (void*) (t + 1);
  tab[0] = 0;
  return ini_table(t, 0, 1, tab); }


static NoInline table *table_insert(core *f, table *t, word k, word v, word i) {
  struct entry *e;
  avec(f, t, avec(f, k, avec(f, v, e = cells(f, Width(struct entry)))));
  if (!e) return 0;
  e->key = k, e->val = v, e->next = t->tab[i];
  t->tab[i] = e;
  word cap0 = t->cap, load = ++t->len / cap0;
  if (load <= 1) return t;
  // grow the table
  struct entry **tab0, **tab1;
  word cap1 = 2 * cap0;
  avec(f, t, tab1 = cells(f, cap1));
  tab0 = t->tab;
  if (!tab1) return 0;
  memset(tab1, 0, cap1 * sizeof(Word));
  for (word i; cap0--;)
    for (struct entry *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = (cap1-1) & hash(f, e->key),
      e->next = tab1[i],
      tab1[i] = e);

  t->cap = cap1, t->tab = tab1;
  return t; }

// relies on table capacity being a power of 2
static Inline word index_of_key(core *f, table *t, word k) {
  return (t->cap - 1) & hash(f, k); }

NoInline table *table_set(core *f, table *t, word k, word v) {
  word index = index_of_key(f, t, k);
  struct entry *entry = t->tab[index];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  if (entry) return entry->val = v, t;
  return table_insert(f, t, k, v, index); }

static struct entry *table_delete_r(Core *f, Table *t, Word k, Word *v, struct entry *e) {
  if (!e) return e;
  if (eql(f, e->key, k)) return t->len--, *v = e->val, e->next;
  return e->next = table_delete_r(f, t, k, v, e->next), e; }

static void table_shrink(core *f, table *t) {
  word cap = t->cap;
  struct entry *coll = 0, *x, *y; // collect all entries in one list
  for (word i = 0; i < cap; i++)
    for (x = t->tab[i], t->tab[i] = 0; x;)
      y = x, x = x->next, y->next = coll, coll = y;
  t->cap = cap >>= 2;
  for (word i; coll;)
    i = (cap - 1) & hash(f, coll->key),
    x = coll->next,
    coll->next = t->tab[i],
    t->tab[i] = coll,
    coll = x; }

static NoInline word table_delete(core *f, table *t, word k, word v) {
  word idx = index_of_key(f, t, k);
  t->tab[idx] = table_delete_r(f, t, k, &v, t->tab[idx]);
  if (t->len / t->cap > 1) table_shrink(f, t);
  return v; }

Vm(tnew) {
  Have(Width(table) + 1);
  table *t = (table*) Hp;
  struct entry **tab = (struct entry**) (t + 1);
  Hp += Width(table) + 1;
  tab[0] = 0;
  return op(1, (word) ini_table(t, 0, 1, tab)); }

static word table_get(core *f, table *t, word k, word zero) {
  size_t i = index_of_key(f, t, k);
  struct entry *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;
  return e ? e->val : zero; }

static Vm(tget) {
  return op(3, !tblp(Sp[1]) ? Sp[2] :
    table_get(f, (Table*) Sp[1], Sp[2], Sp[0])); }

static Vm(tset) {
  word x = Sp[0];
  if (!tblp(x)) return op(3, nil);
  Pack(f);
  table *t = table_set(f, (table*) x, Sp[1], Sp[2]);
  Unpack(f);
  return !t ? Oom : op(3, Sp[2]); }

static Vm(tdel) {
  Word x = Sp[1];
  return op(3, !tblp(x) ? nil :
    table_delete(f, (Table*) x, Sp[2], Sp[0])); }

Vm(tlen) {
  Word x = Sp[0];
  if (!tblp(x)) return op(1, nil);
  Table *t = (Table*) x;
  return op(1, putnum(t->len)); }

Vm(tkeys) {
  if (!tblp(Sp[0])) return op(1, nil);
  Table *t = (Table*) Sp[0];
  Word len = t->len, list = nil;
  Have(len * Width(Pair));
  Pair *pairs = (Pair*) Hp;
  Hp += len * Width(Pair);
  for (int i = t->cap; i;)
    for (struct entry *e = t->tab[--i]; e; e = e->next)
      ini_pair(pairs, e->key, list),
      list = (Word) pairs, pairs++;
  return op(1, list); }

Vm(trim) {
  cell *k = (cell*) Sp[0];
  return op(1, W(ttag(k)->head = k)); }

Vm(seek) {
  cell *k = (cell*) Sp[1];
  return op(2, (Word) (k + getnum(Sp[0]))); }

Vm(peek) {
  cell *k = (cell*) Sp[0];
  return op(1, k[0].x); }

Vm(poke) {
  cell *k = (cell*) Sp[1];
  k->x = Sp[0];
  return op(2, (word) k); }

Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct tag));
  cell *k = mo_ini(R(Hp), n);
  memset(k, -1, n * sizeof(Word));
  Hp += n + Width(struct tag);
  return op(1, (word) k); }

struct tag *ttag(cell *k) {
  while (k->x) k++;
  return (struct tag*) k; }

struct symbol {
  DataHeader;
  string *nom;
  Word code;
  symbol *l, *r; };

static symbol *intern_r(core*, string*, symbol**);
Vm(symbolp) { return op(1, symp(Sp[0]) ? putnum(-1) : nil); }
bool symp(Word _) { return homp(_) && dtyp(_) == &symbol_type; }

static symbol *ini_sym(symbol *y, string *nom, uintptr_t code) {
  return y->ap = data,
         y->typ = &symbol_type,
         y->nom = nom,
         y->code = code,
         y->l = y->r = 0,
         y; }

static Inline symbol *ini_anon(symbol *y, Word code) {
  return y->ap = data,
         y->typ = &symbol_type,
         y->nom = 0,
         y->code = code,
         y; }

static Word hash_symbol(Core *v, Word _) { return ((symbol*) _)->code; }
static Word copy_symbol(Core *f, Word x, Word *p0, Word *t0) {
  symbol *src = (symbol*) x,
         *dst = src->nom ?
           intern_r(f, (string*) cp(f, (Word) src->nom, p0, t0), &f->symbols) :
           ini_anon(bump(f, Width(symbol) - 2), src->code);
  return (word) (src->ap = (vm*) dst); }

static void walk_symbol(core *f, word x, word *p0, word *t0) {
  f->cp += Width(symbol) - (((symbol*)x)->nom ? 0 : 2); }

static void print_symbol(core *f, FILE *o, word x) {
  string* s = ((symbol*) x)->nom;
  if (s) for (int i = 0; i < s->len; putc(s->text[i++], o));
  else fprintf(o, "#sym@%lx", (long) x); }

static symbol *intern_r(core *v, string *b, symbol **y) {
  symbol *z = *y;
  if (!z) return *y =
    ini_sym(bump(v, Width(symbol)), b, hash(v, putnum(hash(v, (Word) b))));
  string *a = z->nom;
  int i = a->len < b->len ? -1 :
          a->len > b->len ? 1 :
          strncmp(a->text, b->text, a->len);
  return i == 0 ? z : intern_r(v, b, i < 0 ? &z->l : &z->r); }

symbol *intern(Core *f, string* b) {
  if (avail(f) < Width(symbol)) {
    bool ok;
    avec(f, b, ok = p_please(f, Width(symbol)));
    if (!ok) return 0; }
  return intern_r(f, b, &f->symbols); }

static Vm(symm) {
  Have(Width(symbol));
  Pack(f);
  symbol *y = intern_r(f, (string*) f->sp[0], &f->symbols);
  Unpack(f);
  return op(1, Z(y)); }

Vm(gensym) {
  if (strp(Sp[0])) return Jump(symm);
  const int req = Width(symbol) - 2;
  Have(req);
  symbol *y = (symbol*) Hp;
  Hp += req;
  return op(1, (word) ini_anon(y, rand())); }

Vm(symnom) {
  word y = Sp[0];
  y = symp(y) && ((symbol*)y)->nom ? Z(((symbol*)y)->nom) : nil;
  return op(1, y); }

static bool not_equal(Core *f, Word a, Word b) { return false; }
static type
  pair_type = {
    .hash = hash_two,
    .copy = cp_two,
    .evac = wk_two,
    .emit = print_two,
    .equal = eq_two, },
  string_type = {
    .hash = hash_string,
    .copy = copy_string,
    .evac = walk_string,
    .emit = print_string,
    .equal = string_equal, },
  symbol_type = {
    .hash = hash_symbol,
    .copy = copy_symbol,
    .evac = walk_symbol,
    .equal = not_equal,
    .emit = print_symbol, },
  table_type = {
    .hash = hash_table,
    .copy = copy_table,
    .evac = walk_table,
    .equal = not_equal,
    .emit = print_table, };

Vm(add)  { return op(2, (Sp[0]|1) + (Sp[1]&~1)); }
Vm(sub)  { return op(2, (Sp[0]|1) - (Sp[1]&~1)); }
Vm(mul)  { return op(2, putnum(getnum(Sp[0])*getnum(Sp[1]))); }
Vm(quot) { return op(2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])/getnum(Sp[1]))); }
Vm(rem)  { return op(2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])%getnum(Sp[1]))); }
Vm(eq) { return op(2, eql(f, Sp[0], Sp[1]) ? putnum(-1) : nil); }
Vm(lt) { return op(2, Sp[0] < Sp[1] ? putnum(-1) : nil); }
Vm(le) { return op(2, Sp[0] <= Sp[1] ? putnum(-1) : nil); }
Vm(gt) { return op(2, Sp[0] > Sp[1] ? putnum(-1) : nil); }
Vm(ge) { return op(2, Sp[0] >= Sp[1] ? putnum(-1) : nil);}
Vm(bnot) { return op(1, ~Sp[0] | 1); }
Vm(band) { return op(2, (Sp[0] & Sp[1]) | 1); }
Vm(bor)  { return op(2, (Sp[0] | Sp[1]) | 1); }
Vm(bxor) { return op(2, (Sp[0] ^ Sp[1]) | 1); }
Vm(rng)  { return op(1, putnum(rand())); }
Vm(fixnump) { return op(1, nump(Sp[0]) ? putnum(-1) : nil); }

// default equality method for things that are only equal to themselves
static bool eql(Core *f, Word a, Word b) {
  // everything equals itself
  if (a == b) return true;
  // if a and b have same type
  if (nump(a | b) ||
      R(a)->ap != data ||
      R(b)->ap != data ||
      dtyp(a) != dtyp(b)) return false;
  // in that case call the type's equality method to check
  return typof(a)->equal(f, a, b); }

static word mkargv(p_core *f, const char **av) {
  if (!*av) return nil;
  string *s = strof(f, *av);
  if (!s) return 0;
  word r; avec(f, s, r = mkargv(f, av + 1));
  return !r ? r : consw(f, (word) s, r); }

static int p_fin(Core *f, int s) {
  return free(f->pool < f->loop ? f->pool : f->loop), s; }

static int p_ini(core *f) {
  memset(f, 0, sizeof(core));
  const uintptr_t len0 = 1;
  word *pool = malloc(2 * len0 * sizeof(word));
  if (!pool) return Oom;
  f->t0 = clock();
  f->sp = f->loop = (f->hp = f->pool = pool) + (f->len = len0);
  if (!(f->dict = mktbl(f)) ||
      !(f->macro = mktbl(f)) ||
      !(f->let = symof(f, ":")) ||
      !(f->cond = symof(f, "?")) ||
      !(f->quote = symof(f, "`")) ||
      !(f->begin = symof(f, ",")) ||
      !(f->lambda = symof(f, "\\")) ||
      !p_define(f, "global-namespace", Z(f->dict)) ||
      !p_define(f, "macros", W(f->macro)))
    return p_fin(f, Oom);
  for (long i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++)
    if (!p_define(f, ini_dict[i].n, (word) ini_dict[i].v))
      return p_fin(f, Oom);
  return Ok; }

int p_main(const char *p, const char **av) {
  TextIn ti = {{p_text_getc, p_text_ungetc, p_text_eof}, p, 0};
  In *i = (In*) &ti;
  core _f, *f = &_f;
  word v, s;
  return Ok != (s = p_ini(f)) ? s : p_fin(f,
    Ok != (s = p_read1(f, i)) ||
    Ok != (s = p_eval(f)) ?  s :
    !(v = mkargv(f, av)) ||
    !(v = consw(f, v, nil)) ||
    !(v = consw(f, W(f->quote), v)) ||
    !(v = consw(f, v, nil)) ||
    !(v = consw(f, pop1(f), v)) ||
    !(v = pushs(f, 1, v)) ?
      Oom :
      p_eval(f)); }
