#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>
#include <unistd.h>
// non freestanding headers
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef VERSION
#define VERSION ""
#endif

// theres a big benefit in speed from tail call optimization but not all platforms support it
#ifdef NTCO
#define YieldStatus PStatusEof
#define Vm(n, ...) int n(core *f, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, ##__VA_ARGS__)
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#define Continue() Ok
#else
#define YieldStatus PStatusOk
#define Vm(n, ...) int n(core *f, thread* Ip, word* Hp, word* Sp, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, Ip, Hp, Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define Continue() Ap(Ip->ap, f)
#endif

#define PStatusOk 0
#define PStatusEof -1
#define PStatusOom 1
#define PStatusVar 2
#define PStatusDom 3

// thanks !!
typedef struct p_core p_core, core;
typedef intptr_t word;
typedef struct type type;
typedef union cell cell, thread;

typedef Vm(vm);
typedef Vm(MVm, int);
union cell { vm *ap; word x; cell *m; type *typ; };
typedef struct symbol symbol;
typedef struct table table;

// runtime core data structure -- 1 core = 1 thread of execution
struct p_core {
  cell *ip; // instruction pointer
  word *hp, *sp;
  symbol *symbols; // interned symbol tree
  table *dict, *macro;
  symbol *eval, *quote, *begin, *let, *cond, *lambda;
  uintptr_t len; // memory pool size
  word *pool, *loop; // on and off pool
  struct Mm { // linked list of root cells
    word *addr;
    struct Mm *next; } *safe;
  union { uintptr_t t0;  // end time of last gc
          word *cp; }; // gc copy pointer
  struct dtor {
    word x;
    void (*d)(word);
    struct dtor *next; } *dtors; };

typedef struct string string;
// primitive type method tables
struct type {
  word (*cp)(core*, word, word*, word*); // for gc
  void (*wk)(core*, word, word*, word*); // for gc
  bool (*eq)(core*, word, word);        // check equality with another object of same type
  void (*em)(core*, FILE*, word);        // print it // replace this with stringify...
  word (*xx)(core*, word);               // hash it
  string *(*show)(core*, word); };

typedef struct In {
  int (*getc)(struct In*), (*ungetc)(struct In*, int), (*eof)(struct In*); } In;

typedef struct FileIn { struct In in; FILE *file; } FileIn;
typedef struct TextIn { struct In in; const char *text; int i; } TextIn;

#define DataHeader vm *ap; type *typ
typedef struct Pair { DataHeader; word a, b; } Pair, pair;
struct string { DataHeader; uintptr_t len; char text[]; };

static struct tag { cell *null, *head, end[]; } *ttag(cell*);

static bool gw_please(core*, uintptr_t),
            eql(core*, word, word);

static word cp(core*, word, word*, word*);

static vm bnot, rng, data, nullp, sysclock,
          symnom, ret, ret0, ap, apn, tap, tapn,
          jump, cond, ref, imm, dot,
          gensym, ev0, pairp, fixnump, symbolp, stringp,
          ssub, sget, slen, scat, prc, cons, car, cdr,
          lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
          seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
          read0, readf, p_isatty, curry;

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
// align bytes up to the nearest word
static NoInline long gw_clock(void);

_Static_assert(-1 >> 1 == -1, "support sign extended shift");
_Static_assert(sizeof(cell*) == sizeof(cell), "cell is 1 word wide");

#define Eof PStatusEof
#define Oom PStatusOom
#define Ok PStatusOk
static NoInline Vm(gc, uintptr_t n) {
  Pack(f);
  int s = gw_please(f, n) ? Ok : Oom;
  Unpack(f);
  return s != Ok ? s : Ap(f->ip->ap, f); }

static void *bump(core *f, size_t n) { void *x = f->hp; return f->hp += n, x; }
#define avail(f) (f->sp-f->hp)
static void *cells(core *f, size_t n) { return
  n <= avail(f) || gw_please(f, n) ? bump(f, n) : 0; }

#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))

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
static NoInline bool gw_please(core *f, uintptr_t req0) {
  word *src = f->pool, *dest = f->loop;
  f->pool = dest, f->loop = src;    // swap
  size_t t0 = f->t0, t1 = gw_clock() , // get last gc end time
         len0 = f->len;             // get original length
                                    //
  copy_from(f, src, len0);          // copy to new pool
  size_t t2 = f->t0 = gw_clock(),      // get and set last gc end time
         total = len0,
         avail = avail(f),
         used = total - avail,
         req = req0 + used,
         v = t2 == t1 ?             // t1 and t2 can be the same
           v_hi :                   // in that case choose high
           (t2 - t0) / (t2 - t1),   // otherwise take ratio of total run time to gc time
         len1 = len0;               // initial destination size same as the target size
  // if v is out of bounds then calculate len1
  // by inverse proportionally adjusting len and v until v is in bounds
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return true; // no change reqired, hopefully the most common case
                    //
  // at this point we got a new target length and are gonna try and resize
  word *dest2 = malloc(len1 * 2 * sizeof(word)); // allocate pool with the new target size
  if (!dest2) return req <= total; // if this fails still return true if the original pool is not too small

  // we got the new pool so copy again and return true
  f->pool = dest2;          // reset core variables on new pool
  f->len = len1;
  f->loop = dest2 + len1;
  copy_from(f, dest, len0); // do second copy
  free(min(src, dest));     // free original pool
  f->t0 = gw_clock();          // set last gc timestamp
  return true; }            // size successfully adjusted


static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(word), r = b % sizeof(word);
  return q + (r ? 1 : 0); }
#define W(_) ((word)(_))
#define R(_) ((cell*)(_))
#define Width(_) b2w(sizeof(_))
#define CP(x) cp(f, W(x), p0, t0)
#define within(a, b, c) (W(a)<=W(b)&&W(b)<W(c))
#define owns(f, x) within(f->pool, x, f->pool + f->len)
#define datp(_) (R(_)->ap==data)
#define typof(_) R(_)[1].typ
// this function expects pool loop and len to have been set already on the state
static NoInline void copy_from(core *f, word *p0, uintptr_t len0) {
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
    if (datp(k)) typof(k)->wk(f, W(k), p0, t0); // is data
    else { while (k->x) k->x = CP(k->x), k++;     // is thread
           f->cp = (word*) k + 2; }
  // run destructors ...
  // this has never been tested or used
  struct dtor *nd = NULL;
  for (struct dtor *n, *d = f->dtors; d; d = d->next)
    if (!owns(f, R(d->x)->x)) d->d(d->x);
    else n = bump(f, Width(struct dtor)),
         n->d = d->d,
         n->x = R(d->x)->x,
         n->next = nd,
         nd = n;
  f->dtors = nd; }

#define nump(_) (W(_)&1)
#define homp(_) (!nump(_))
static NoInline word cp(core *v, word x, word *p0, word *t0) {
  // if it's a number or outside managed memory then return it
  if (nump(x) || !within(p0, x, t0)) return x;
  cell *src = (cell*) x;
  x = src->x;
  // if the cell holds a pointer to the new space then return the pointer
  if (homp(x) && owns(v, x)) return x;
  // if it's data then call the given copy function
  if (datp(src)) return typof(src)->cp(v, (word) src, p0, t0);
  // it's a thread, find the end to find the head
  struct tag *t = ttag(src);
  cell *ini = t->head, *d = bump(v, t->end - ini), *dst = d;
  // copy source contents to dest and write dest addresses to source
  for (cell *s = ini; (d->x = s->x); s++->x = W(d++));
  ((struct tag*) d)->head = dst;
  return W(dst + (src - ini)); }

static Vm(data) {
  word x = W(Ip);
  Ip = R(Sp[1]);
  Sp[1] = x;
  Sp += 1;
  return Continue(); }

#define Have1() if (Sp == Hp) return Ap(gc, f, 1)
static Vm(uncurry) { Have1(); return *--Sp = Ip[1].x, Ip = Ip[2].m, Continue(); }

// branch instructions

static Vm(jump) { return Ip = Ip[1].m, Continue(); }

#define putnum(_) (((word)(_)<<1)|1)
#define nil putnum(0)
#define nilp(_) (W(_)==nil)
static Vm(cond) { return Ip = nilp(*Sp++) ? Ip[1].m : Ip + 2, Continue(); }

// load instructions
//
// push an immediate value
static Vm(imm) { Have1(); return Sp -= 1, Sp[0] = Ip[1].x, Ip += 2, Continue(); }

#define getnum(_) ((word)(_)>>1)
// push a value from the stack
static Vm(ref) { Have1(); return
  Sp[-1] = Sp[getnum(Ip[1].x)], Sp -= 1, Ip += 2, Continue(); }

// call and return
// apply function to one argument
static Vm(ap) {
  if (nump(Sp[1])) return Ip++, Sp++, Continue();
  cell *k = R(Sp[1]); return Sp[1] = W(Ip + 1), Ip = k, Continue(); }

// tail call
static Vm(tap) {
  word x = Sp[0], j = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) return Ip = R(Sp[1]), Sp[1] = j, Sp += 1, Continue();
  return Ip = R(j), Sp[0] = x, Continue(); }

// apply to multiple arguments
static Vm(apn) {
  size_t n = getnum(Ip[1].x);
  cell *ra = Ip + 2; // return address
  // this instruction is only emitted when the callee is known to be a function
  // so putting a value off the stack into Ip is safe. the +2 is cause we leave
  // the currying instruction in there... should be skipped in compiler instead FIXME
  Ip = R(Sp[n]) + 2;
  Sp[n] = W(ra); // store return address
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
  return Ip = R(Sp[n]), Sp[n] = Sp[0], Sp += n, Continue(); }
static Vm(ret0) { return Ip = R(Sp[1]), Sp[1] = Sp[0], Sp += 1, Continue(); }

#define Have(n) if (Sp - Hp < n) return Ap(gc, f, n)
// currying
static Vm(curry) {
  cell *k = R(Hp), *j = k;
  size_t S = 3 + Width(struct tag),
         n = getnum(Ip[1].x);
  if (n == 2) { Have(S); }
  else { S += 2; Have(S); j += 2, k[0].ap = curry, k[1].x = putnum(n - 1); }
  j[0].ap = uncurry, j[1].x = *Sp++, j[2].m = Ip + 2;
  j[3].x = 0, j[4].m = k;
  return Hp += S, Ip = R(*Sp), Sp[0] = W(k), Continue(); }

// at all times vm is running a function. thread compiler tracks
// function state using this type
typedef struct env {
  // these parameters represent stack state at a point in compile process
  word args,  // list // function positional arguments (never empty)
       imps,  // list // closure variables
       stack; // list // current values on stack
  // these are values of variables known at compile time
  word lams; // alist // known function definitions
  // these are two stacks of jump target addresses for conditional expressions
  word alts, // list // alternate branch address stack
       ends; // list // exit branch address stack
  // this is the enclosing function env* if any
  struct env *par;
} env;

static long lidx(core*, word, word);

// thread compiler functions are defined in two phases:
#define Ana(n, ...) size_t n(core *f, env **c, size_t m, word x, ##__VA_ARGS__)
// ana phase
// - takes an expression and returns an upper bound for the length of a thread that
//   pushes the input expression's value onto the stack.
// - side effect: top of stack is now a function pointer that when called with a
//   properly initialized thread object, starts next compiler phase.
#define Cata(n, ...) cell *n(core *f, env **c, cell *k, ##__VA_ARGS__)
// cata phase
// - takes a thread pointer, emits code immediately prior to the indexed instruction,
//   pops a precomputed (by phase 0) continuation off the stack and calls it with the
//   decremented pointer.

typedef Ana(ana); typedef Cata(cata);
static ana analyze, ana_if, ana_let;
static Inline Cata(pull) { return ((cata*) (*f->sp++))(f, c, k); }
static Cata(cata_i) { return k[-1].x = *f->sp++, pull(f, c, k - 1); }
static Cata(cata_ix) { return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull(f, c, k - 2); }

static cell *mo_n(core *f, size_t n) { // allocate a thread
  cell *k = cells(f, n + Width(struct tag));
  if (!k) return k;
  struct tag *t = (struct tag*) (k + n);
  return t->null = NULL, t->head = k; }

static cell *pull_m(core *f, env **c, size_t m) {
  cell *k = mo_n(f, m);
  return !k ? k : (memset(k, -1, m * sizeof(word)), pull(f, c, k + m)); }

static word pushs(core*, uintptr_t, ...);
// generic instruction ana handlers
static size_t ana_i1(core *f, env **c, size_t m, vm *i) {
  return pushs(f, 2, cata_i, i) ? m + 1 : 0; }
static size_t ana_i2(core *f, env **c, size_t m, vm *i, word x) {
  return pushs(f, 3, cata_ix, i, x) ? m + 2 : 0; }

static Cata(yieldk) { return k; }

#define pop1(f) (*(f)->sp++)
static env *enscope(core *f, env* par, word args, word imps) {
  if (!pushs(f, 3, args, imps, par)) return 0;
  env *c = (env*) mo_n(f, Width(env));
  args = pop1(f), imps = pop1(f), par = (env*) pop1(f);
  if (c) c->args = args, c->imps = imps, c->par = par,
         c->stack = c->alts = c->ends = c->lams = nil;
  return c; }

#define MM(f,r) ((f->safe=&((struct Mm){(word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
static NoInline int p_ana(core *f, vm *y) {
  env *c = enscope(f, (env*) nil, nil, nil);
  if (!c) return Oom;
  word x = f->sp[0];
  f->sp[0] = (word) yieldk; // function that returns thread from code generation
  MM(f, &c);
  size_t m = analyze(f, &c, 1, x);
  m = m ? ana_i2(f, &c, m, y, W(f->ip)) : m;
  cell *k = m ? pull_m(f, &c, m) : 0;
  UM(f);
  return k ? (f->ip = k, Ok) : Oom; }
// compile and execute expression
static NoInline int p_eval(core *f, vm *y) {
  int s = p_ana(f, y);
  if (s != Ok) return s;
#ifdef NTCO
  do s = f->ip->ap(f); while (s == Ok);
  return s == Eof ? Ok : s; }
#else
  return f->ip->ap(f, f->ip, f->hp, f->sp); }
#endif

static NoInline Vm(ev0) { return Ip++, Pack(f), p_eval(f, jump); }

#define A(o) ((pair*)(o))->a
#define B(o) ((pair*)(o))->b
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define BBA(o) B(B(A(o)))

static cata ana_if_push_branch, ana_if_pop_branch, ana_if_push_exit, ana_if_pop_exit, ana_if_peek_exit;

static bool twop(word);
// conditional expression analyzer
static NoInline Ana(ana_if) {
  if (!pushs(f, 2, x, ana_if_pop_exit)) return 0;
  Pair p = { 0, 0, nil, nil }; // this is weird :/
  x = pop1(f);
  MM(f, &x);
  for (; m; x = BB(x)) {
    if (!twop(x)) x = (word) &p;
    m = analyze(f, c, m + 3, A(x));
    if (!twop(B(x))) { // this means we have just analyzed the default case
      m = pushs(f, 1, ana_if_peek_exit) ? m : 0; // now branch to exit
      break; }
    // otherwise we analyzed a conditional test
    // pop the last branch address off the stack to be jumped to in case the test failed
    m = pushs(f, 1, ana_if_pop_branch) ? m : 0;
    // otherwise here is the consequent
    m = m ? analyze(f, c, m + 3, AB(x)) : m;
    // after consequent jump to exit, then push new branch address at present code position (these are emitted backwards)
    m = m && pushs(f, 2, ana_if_push_branch,
                         ana_if_peek_exit) ? m : 0; }
  UM(f);
  m = m && pushs(f, 1, ana_if_push_exit) ? m : 0; // push the exit address for the whole conditional
  return m; }

static pair *pairof(core*, word, word);
// first emitter called for cond expression
// pushes cond expression exit address onto env stack ends
static Cata(ana_if_push_exit) {
  pair *w = pairof(f, W(k), (*c)->ends);
  return !w ? 0 : pull(f, c, R(A((*c)->ends = (word) w))); }

// last emitter called for cond expression
// pops cond expression exit address off env stack ends
static Cata(ana_if_pop_exit) { return (*c)->ends = B((*c)->ends), pull(f, c, k); }

static Cata(ana_if_push_branch) {
  pair *w = pairof(f, W(k), (*c)->alts);
  return !w ? 0 : ((*c)->alts = W(w), k = R(w->a), pull(f, c, k)); }

static Cata(ana_if_peek_exit) {
  k -= 2;
  cell *addr = (cell*) A((*c)->ends);
  // if the destination is a return or tail call,
  // then copy it forward instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k[0].ap = addr[0].ap, k[1].x = addr[1].x; // XXX optimization to remove ...
  else if (addr->ap == tapn)
    k--, k[0].ap = tapn, k[1].x = addr[1].x, k[2].x = addr[2].x;
  else k[0].ap = jump, k[1].x = (word) addr;
  return pull(f, c, k); }

// last emitter called for a branch
// pops next branch address off env stack alts
static Cata(ana_if_pop_branch) {
  return k[-2].ap = cond,
         k[-1].x = A((*c)->alts),
         (*c)->alts = B((*c)->alts),
         pull(f, c, k - 2); }

static bool lambp(core *f, word x) { return twop(x) && A(x) == (word) f->lambda; }

static word lassoc(core *f, word l, word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
  return 0; }

// DEFINE
// let expressions
static word ldels(core *f, word lam, word l) {
  if (!twop(l)) return nil;
  word m = ldels(f, lam, B(l));
  if (!lassoc(f, lam, A(l))) B(l) = m, m = l;
  return m; }

#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define wpairof(...) W(pairof(__VA_ARGS__))
static word desugr(core *f, word *d, word *e, word a) {
  if (!twop(a)) return wpairof(f, *e, nil);
  word b; avec(f, a, b = desugr(f, d, e, B(a)));
  return !b ? b : wpairof(f, A(a), b); }

static int desug(core *f, word *d, word *e) {
  if (!twop(*d)) return Ok;
  word x;
  if (!pushs(f, 1, f->lambda)) return Oom;
  do if (!(x = desugr(f, d, e, B(*d))) || !(x = wpairof(f, f->sp[0], x))) return Oom;
  else *d = A(*d), *e = x;
  while (twop(*d));
  return f->sp++, Ok; }

// list length
static size_t llen(word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

static table *table_set(core*, table*, word, word);
static Vm(defglob) { return Pack(f),
  !table_set(f, f->dict, Ip[1].x, Sp[0]) ?  Oom :
   (Unpack(f), Ip += 2, Continue()); }

// emits call instruction and modifies to tail call
// if next operation is return
static Cata(cataap) { return k[0].ap == ret ?
  (k[0].ap = tap, pull(f, c, k)) :
  (k[-1].ap = ap, pull(f, c, k - 1)); }
static Cata(cataapn) {
  word n = *f->sp++;
  return k[0].ap == ret ?
   (k[0].x = n,  k[-1].ap = tapn, pull(f, c, k - 1)) :
   (k[-1].x = n, k[-2].ap = apn,  pull(f, c, k - 2)); }

// list concat
static word lconcat(core *f, word l, word n) { return !twop(l) ? n :
  (avec(f, l, n = lconcat(f, B(l), n)),
   n ? wpairof(f, A(l), n) : n); }

// reverse list concat
static word rlconcat(core *f, word l, word n) {
  for (word m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
  return n; }

static word ana_lam(core*, env**, word, word);
// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.
static size_t ana_let(core *f, env* *b, size_t m, word exp) {
  if (!twop(exp)) return analyze(f, b, m, nil);
  if (!twop(B(exp))) return analyze(f, b, m, A(exp));
  env *q = *b, **c = &q;
  avec(f, exp, q = enscope(f, q, q->args, q->imps));
  if (!q) return 0;
  // lots of variables :(
#define fail() (f->safe = mm, 0)
  struct Mm *mm = f->safe;
  word nom = nil, def = nil, lam = nil, v = nil, d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &exp), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

  // collect vars and defs into two lists
  for (; twop(exp) && twop(B(exp)); exp = BB(exp)) {
    d = A(exp), e = AB(exp), desug(f, &d, &e);
    if (!(nom = wpairof(f, d, nom)) || !(def = wpairof(f, e, def))) return fail();
    // if it's a lambda compile it and record in lam list
    if (lambp(f, e)) { word x = ana_lam(f, c, nil, B(e));
                       x = x ? wpairof(f, d, x) : x;
                       lam = x ? wpairof(f, x, lam) : x;
                       if (!lam) return fail(); } }

  // if there's no body then evaluate the name of the last definition
  bool even = !twop(exp); // we check this again later to make global bindings at top level
  if (even && !(exp = wpairof(f, A(nom), nil))) return fail();

  // find closures
  // for each function f with closure C(f)
  // for each function g with closure C(g)
  // if f in C(g) then C(g) include C(f)
  long j;
  do for (j = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
    for (e = lam; twop(e); e = B(e)) // for each bound function variable
      if (d != e && // skip yourself
          lidx(f, BBA(e), AA(d)) >= 0) // if you need this function
        for (v = BBA(d); twop(v); v = B(v)) { // then you need its variables
          word vars = BBA(e), var = A(v);
          if (lidx(f, vars, var) < 0) { // only add if it's not already there
            if (!(vars = wpairof(f, var, vars))) return fail(); // oom
            else j++, BBA(e) = vars; } }
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e));

  (*c)->lams = lam;
  // pull_m lambda with reversed argument list
  exp = lconcat(f, nom, exp);
  symbol *l = exp ? f->lambda : 0;
  exp = l ? wpairof(f, W(l), exp) : 0;
  m = exp ? analyze(f, b, m, exp) : 0; // exp is now the required lambda, analyze it
  if (!m || !((*b)->stack = wpairof(f, nil, (*b)->stack))) return fail();

  // reverse the nom and def lists
  nom = rlconcat(f, nom, nil), def = rlconcat(f, def, nil);
  // evaluate definitions in order tracking var names on stack list
  // store lambdas on env for lazy binding and pull_m new application
  // - reverse noms onto exp
  // - reverse onto e = nil and recompile lambdas
  size_t nn = 0;
  for (; twop(nom); nom = B(nom), def = B(def), nn++) {
    // if lambda then recompile with the explicit closure
    // and put in arg list and lam list (latter is used for lazy binding)
    if (lambp(f, A(def))) {
      d = lassoc(f, lam, A(nom));
      word _;
      if (!(_ = ana_lam(f, c, BB(d), BA(def)))) return fail();
      else A(def) = B(d) = _; }
    if (!(m = analyze(f, b, m, A(def))) ||
        !((*b)->stack = wpairof(f, A(nom), (*b)->stack)))
      return fail();
    // if toplevel then bind
    if (even && nilp((*b)->args))
      if (!(m = ana_i2(f, c, m, defglob, A(nom))))
        return fail(); }

  m = nn <= 1 ? pushs(f, 1, cataap) ? m + 1 : 0 :
                pushs(f, 2, cataapn, putnum(nn)) ? m + 2 : 0;
  for (nn++; nn--; (*b)->stack = B((*b)->stack));
  return f->safe = mm, m; }

static Vm(drop1) { return Ip++, Sp++, Continue(); }

static Inline size_t ana_imm(core *f, env **c, size_t m, word x) {
  return ana_i2(f, c, m, imm, x); }

static size_t ana_seq(core *f, env* *c, size_t m, word x) {
  if (!twop(x)) return ana_imm(f, c, m, nil);
  MM(f, &x);
  while (m && twop(B(x))) m = analyze(f, c, m, A(x)),
                          m = m ? ana_i1(f, c, m, drop1) : m,
                          x = B(x);
  UM(f);
  return m ? analyze(f, c, m, A(x)) : m; }

// exit vm and return to C
static Vm(yieldi) { return Ip = Ip[1].m, Pack(f), YieldStatus; }
static pair *ini_pair(pair*, word, word);
static Ana(ana_mac, word b) {
  if (!pushs(f, 3, f->quote, x, b)) return 0;
  Pair *mxp = (Pair*) cells(f, 4 * Width(Pair));
  if (!mxp) return 0;
  x = W(ini_pair(mxp, f->sp[1], W(ini_pair(mxp+1, W(ini_pair(mxp+2, f->sp[0],  W(ini_pair(mxp+3, f->sp[2], nil)))), nil))));
  *(f->sp += 2) = x;
  return p_eval(f, yieldi) != Ok ? 0 : analyze(f, c, m, pop1(f)); }

// evaluate function call arguments and apply
static size_t ana_ap_l2r(core *f, env **c, size_t m, word x) {
  MM(f, &x); // handle oom here ..
  // push one anonymous argument on the stack representing the function
  m = ((*c)->stack = wpairof(f, nil, (*c)->stack)) ? m : 0;
  while (m && twop(x)) m = analyze(f, c, m + 1, A(x)), // eval each argument
                       x = B(x),
                       m = m && pushs(f, 1, cataap) ? m : 0; // and apply the function
  return (*c)->stack = B((*c)->stack), // pop anonymous stack argument
         UM(f),
         m; }

// evaluate a function expression by applying the function to arguments
static size_t ana_ap(core *f, env* *c, size_t m, word fn, word args) {
  size_t argc = llen(args);
  avec(f, args, m = analyze(f, c, m, fn));
  return !argc || !m ? m : ana_ap_l2r(f, c, m, args); }

static word table_get(core*, table*, word, word);
static Ana(ana_sym_r, env *d);
static Ana(ana_lambda, word);
static bool symp(word);
static NoInline Ana(analyze) {
  if (symp(x)) return ana_sym_r(f, c, m, x, *c);
  if (!twop(x)) return ana_imm(f, c, m, x);
  word a = A(x), b = B(x);
  if (a == W(f->quote)) return ana_imm(f, c, m, twop(b) ? A(b) : nil);
  if (a == W(f->begin)) return ana_seq(f, c, m, b);
  if (a == W(f->let)) return ana_let(f, c, m, b);
  if (a == W(f->cond)) return ana_if(f, c, m, b);
  if (a == W(f->lambda)) return !twop(b) ? ana_imm(f, c, m, nil) :
                                !twop(B(b)) ? analyze(f, c, m, A(b)) :
                                ana_lambda(f, c, m, x, b);
  if (!twop(b)) return analyze(f, c, m, a); // singleton list has value of first element
  word macro = table_get(f, f->macro, a, 0);
  return macro ? ana_mac(f, c, m, macro, b) : ana_ap(f, c, m, a, b); }

static Vm(free_variable) {
  word x = Ip[1].x;
  x = table_get(f, f->dict, x, x); // error here if you want on undefined variable
  return Ip[0].ap = imm, Ip[1].x = x, Continue(); }

// index of item in list
static long lidx(core *f, word l, word x) {
  for (long i = 0; !nilp(l); l = B(l), i++) if (eql(f, x, A(l))) return i;
  return -1; }

static long stack_index_of_symbol(core *f, env *c, word var) {
  long l, i = 0;
  for (l = c->imps; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  for (l = c->args; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  return -1; }

static Cata(c2var) {
  word var = *f->sp++, stack = *f->sp++;
  long i = lidx(f, stack, var);
  return k[-2].ap = ref, k[-1].x = putnum(i), pull(f, c, k - 2); }

// emit stack reference instruction
static Cata(cata_var) {
  word var = *f->sp++, // variable name
       ins = llen(*f->sp++), // stack inset
       idx = stack_index_of_symbol(f, *c, var);
  return k[-2].ap = ref, k[-1].x = putnum(idx + ins), pull(f, c, k - 2); }

static Vm(lazy_bind) {
  word ref = Ip[1].x,
       lfd = ref;
  ref = AB(lfd);
  if (!ref) return PStatusVar;
  return Ip[0].ap = imm, Ip[1].x = ref, Continue(); }

static Ana(ana_sym_r, env *d) {
  // free symbol?
  if (nilp(d)) {
    word y = table_get(f, f->dict, x, 0);
    return y ? ana_imm(f, c, m, y) :
     (x = wpairof(f, x, (*c)->imps), // XXX why is this needed???
      x = x ? A((*c)->imps = x) : x,
      x ? ana_i2(f, c, m, free_variable, x) : x);  }
  // defined as a function by a local let binding?
  word y = lassoc(f, d->lams, x);
  if (y) {
    m = ana_i2(f, c, m, lazy_bind, y);
    if (!m) return m;
    y = f->sp[2];
    word z = BB(y); // get the args
    return ana_ap_l2r(f, c, m, z); }
  // bound on the stack by a local let binding?
  if (lidx(f, d->stack, x) >= 0)
    return pushs(f, 3, c2var, x, d->stack) ? m + 2 : 0;
  // bound on the stack as a closure or positional argument?
  if (stack_index_of_symbol(f, d, x) >= 0) {
    if (*c != d) // if we have found the variable in an enclosing scope then import it
      x = wpairof(f, x, (*c)->imps),
      x = x ? A((*c)->imps = x) : x;
    return pushs(f, 3, cata_var, x, (*c)->stack) ? m + 2 : 0; }
  // otherwise recur on the enclosing env
  return ana_sym_r(f, c, m, x, d->par); }


// lambda decons pushes last list item to stack returns init of list
static word linit(core *f, word x) {
  if (!twop(x)) return pushs(f, 1, nil) ? nil : 0;
  if (!twop(B(x))) return pushs(f, 1, A(x)) ? nil : 0;
  word y = A(x); return avec(f, y, x = linit(f, B(x))),
                        x ? wpairof(f, y, x) : x; }

static cell *trim_thread(cell*);

static Ana(ana_lambda, word b) {
  return (x = ana_lam(f, c, nil, b)) ? analyze(f, c, m, x) : x; }
static word ana_lam(core *f, env **c, word imps, word exp) {
  // expediently storing exp in args for the moment
  env *d = enscope(f, *c, exp, imps);
  if (!d) return 0;
  MM(f, &d);
  // get the real args
  word args = linit(f, d->args);
  if (!(d->args = args)) return UM(f), 0;
  exp = f->sp[0], f->sp[0] = W(yieldk);
  size_t m = analyze(f, &d, 2, exp);
  if (!m) return UM(f), 0;
  size_t arity = llen(d->args) + llen(d->imps);
  cell *k = (m = ana_i2(f, c, m, ret, putnum(arity))) ? pull_m(f, &d, m) : 0;
  if (!k) return UM(f), 0;
  if (arity > 1) (--k)->x = putnum(arity), (--k)->ap = curry;
  return UM(f), wpairof(f, W(trim_thread(k)), d->imps); }

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
  return idx = idx ? idx - 1 : idx, t->i = idx, t->text[idx]; }

static int p_text_eof(In *i) { return !ti(i)->text[ti(i)->i]; }

////
/// " the parser "
//
//
// get the next significant character from the stream
static int read_char(core *f, In *i) {
  for (int c;;) switch (c = p_in_getc(i)) {
    default: return c;
    case '#': case ';': while (!p_in_eof(i) && (c = p_in_getc(i)) != '\n' && c != '\r');
    case ' ': case '\t': case '\n': case '\r': case '\f': continue; } }

static int reads(core*, In*), read_string(core*, In*, char), read_atom(core*, In*);

static int p_read1(core*, In*);

static int rquote(core *f, In *i) {
  int s = p_read1(f, i);
  if (s != Ok) return s;
  pair *w = pairof(f, f->sp[0], nil);
  w = !w ? w : pairof(f, W(f->quote), f->sp[0] = W(w));
  return !w ? Oom : (f->sp[0] = W(w), Ok); }

static int p_read1(core *f, In* i) {
  int c = read_char(f, i);
  switch (c) {
    case EOF:  return Eof;
    case '\'': return rquote(f, i);
    case '(':  return reads(f, i);
    case ')':  return pushs(f, 1, nil) ? Ok : Oom;
    case '"':  return read_string(f, i, '"');
    default:   return p_in_ungetc(i, c), read_atom(f, i); } }

static int reads(core *f, In* i) {
  word c = read_char(f, i);
  if (c == EOF || c == ')') return pushs(f, 1, nil) ? Ok : Oom;
  p_in_ungetc(i, c);
  int s = p_read1(f, i);
  s = s == Ok ? reads(f, i) : s;
  return s != Ok ? s :
    !(c = wpairof(f, f->sp[1], f->sp[0])) ? Oom :
    (*++f->sp = c, Ok); }

static string *ini_str(string*, uintptr_t);
// create and grow buffers for reading
static string *bnew(core *f) {
  string *s = cells(f, Width(string) + 1);
  return s ? ini_str(s, sizeof(word)) : s; }

static string *bgrow(core *f, string *s) {
  string *t; uintptr_t len = s->len;
  avec(f, s, t = cells(f, Width(string) + 2 * b2w(len)));
  if (t) memcpy(ini_str(t, 2 * len)->text, s->text, len);
  return t; }

static int read_string(core *f, In* i, char delim) {
  string *b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(word); b; b = bgrow(f, b), lim *= 2)
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

static symbol *intern(core*, string*);
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
  word x = *e == 0 ? putnum(j) : W(intern(f, b));
  return !x || !pushs(f, 1, x) ? Oom : Ok; }

#define FileInput(f) ((In*)&(FileIn){{p_file_getc, p_file_ungetc, p_file_eof}, f})
static NoInline int p_read1f(core *f, FILE* i) {
  In *fi = FileInput(i);
  return p_read1(f, fi); }

static Vm(read0) {
  Pack(f);
  int s = p_read1f(f, stdin);
  if (s == Eof) return // no error but end of file
    Unpack(f), Sp[0] = nil, Ip++, Continue();
  if (s != Ok) return s; // or was there an error?
  // no error and got a value on stack
  // make a list of it
  pair *p = pairof(f, f->sp[0], nil);
  if (!p) return Oom; // ...
  return Unpack(f), Sp += 1, Ip += 1, Sp[0] = W(p), Continue(); }

static NoInline int p_readsp(core *f, string *s) {
  char n[265]; // :)
  memcpy(n, s->text, s->len);
  n[s->len] = 0;
  FILE *i = fopen(n, "r");
  if (!i) return pushs(f, 1, nil) ? Ok : Oom;
  In *fi = FileInput(i);
  int t = reads(f, fi);
  fclose(i);
  return t; }

static bool strp(word);
static Vm(readf) {
  if (!strp(Sp[0])) return Sp[0] = nil,
                           Ip += 1,
                           Continue();
  string *s = (string*)Sp[0];
  if (s->len > 255) return Sp[0] = nil,
                           Ip += 1,
                           Continue();
  Pack(f);
  int t = p_readsp(f, s);
  return t != Ok ? t : (Unpack(f), Sp[1] = Sp[0], Sp++, Ip++, Continue()); }

static NoInline long gw_clock(void) {
  struct timespec ts;
  int s = clock_gettime(CLOCK_REALTIME, &ts);
  return s ? -1 : ts.tv_sec  * 1000 + ts.tv_nsec / 1000000; }

static Vm(sysclock) { return Sp[0] = putnum(gw_clock()), Ip += 1, Continue(); }

static Vm(p_isatty) { return
  Sp[0] = isatty(getnum(Sp[0])) ? putnum(-1) : nil, Ip += 1, Continue(); }

static Vm(prc) { word w = *Sp; return putc(getnum(w), stdout), Ip++, Continue(); }

static void transmit(core *f, FILE* out, word x) {
  if (nump(x)) fprintf(out, "%ld", (long) getnum(x));
  else if (datp(x)) typof(x)->em(f, out, x);
  else fprintf(out, "#%lx", (long) x); }
static Vm(dot) { return transmit(f, stdout, Sp[0]), Ip++, Continue(); }

static NoInline word pushsr(core *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (!n) return gw_please(f, m) ? m : n;
  word x = va_arg(xs, word), y;
  return avec(f, x, y = pushsr(f, m, n - 1, xs)), y ? *--f->sp = x : y; }

static word pushs(core *f, uintptr_t m, ...) {
  va_list xs;
  va_start(xs, m);
  word n, r = 0;
  if (avail(f) < m) r = pushsr(f, m, m, xs);
  else for (n = 0, f->sp -= m; n < m; f->sp[n++] = r = va_arg(xs, word));
  return va_end(xs), r; }

static type str_type, two_type, str_type, sym_type, tbl_type;
static bool twop(word _) { return homp(_) && typof(_) == &two_type; }

static pair *pairof(core *f, word a, word b) {
  if (avail(f) < Width(pair)) {
    bool ok;
    avec(f, a, avec(f, b, ok = gw_please(f, Width(pair))));
    if (!ok) return 0; }
  pair *w = (pair*) f->hp;
  return f->hp += Width(pair), ini_pair(w, a, b); }

static Vm(car) { return Sp[0] = twop(Sp[0]) ? A(Sp[0]) : Sp[0], Ip += 1, Continue(); }
static Vm(cdr) { return Sp[0] = twop(Sp[0]) ? B(Sp[0]) : nil, Ip += 1, Continue(); }
static Vm(cons) {
  Have(Width(pair));
  pair *w = ini_pair((pair*) Hp, Sp[0], Sp[1]);
  return Hp += Width(pair), Sp[1] = W(w), Sp += 1, Ip += 1, Continue(); }

static Vm(pairp) { return Sp[0] = twop(Sp[0]) ? putnum(-1) : nil, Ip += 1, Continue(); }

static pair *ini_pair(pair *w, word a, word b) {
  return w->ap = data, w->typ = &two_type, w->a = a, w->b = b, w; }

static bool strp(word _) { return homp(_) && typof(_) == &str_type; }

static Vm(slen) { return
  Sp[0] = strp(Sp[0]) ? putnum(((string*)Sp[0])->len) : nil,
  Ip += 1,
  Continue(); }

static Vm(ssub) {
  cell* r = (cell*) Sp[3];
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
      Sp[3] = (word) t; } }
  return Ip = r, Sp += 3, Continue(); }

static Vm(sget) {
  if (!strp(Sp[0])) Sp[1] = nil;
  else {
    string *s = (string*) Sp[0];
    size_t i = min(s->len - 1, getnum(Sp[1]));
    i = max(i, 0);
    Sp[1] = putnum(s->text[i]); }
  return Ip++, Sp++, Continue(); }

static Vm(scat) {
  word a = Sp[0];
  if (!strp(a)) return Sp += 1, Ip += 1, Continue();
  word b = Sp[1];
  if (!strp(b)) return Sp[1] = a, Sp += 1, Ip += 1, Continue();
  string *x = (string*) a, *y = (string*) b;
  size_t len = x->len + y->len,
         req = Width(string) + b2w(len);
  Have(req);
  string *z = ini_str((string*) Hp, len);
  return Hp += req,
         memcpy(z->text, x->text, x->len),
         memcpy(z->text + x->len, y->text, y->len),
         Sp[1] = W(z),
         Ip += 1,
         Continue(); }

static Vm(stringp) { return Sp[0] = strp(Sp[0]) ? putnum(-1) : nil, Ip += 1, Continue(); }

static string* ini_str(string *s, uintptr_t len) {
  return s->ap = data, s->typ = &str_type, s->len = len, s; }

static word xx_str(core *v, word _);
static bool eq_str(core *f, word x, word y);
static void em_str(core* v, FILE *o, word _);
static void wk_str(core* f, word x, word *p0, word *t0);
static word cp_str(core* v, word x, word *p0, word *t0);

static string *strof(core *f, const char *text) {
  size_t len = strlen(text);
  string *o = cells(f, Width(string) + b2w(len));
  if (o) memcpy(ini_str(o, len)->text, text, len);
  return o; }

typedef struct table {
  DataHeader;
  uintptr_t len, cap;
  struct entry { word key, val; struct entry *next; } **tab;
} table;

static bool tblp(word _) { return homp(_) && typof(_) == &tbl_type; }

static table *ini_table(table *t, uintptr_t len, uintptr_t cap, struct entry**tab) {
  return t->ap = data, t->typ = &tbl_type, t->len = len, t->cap = cap, t->tab = tab, t; }

// FIXME very poor hashing method :(
static void em_tbl(core *f, FILE *o, word x);
static void wk_tbl(core *f, word x, word *p0, word *t0);
static word cp_tbl(core *f, word x, word *p0, word *t0);
static word xx_tbl(core *f, word h);

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

#define mix ((uintptr_t)2708237354241864315)
static word hash(core *f, word x) {
  if (nump(x)) {
    const int shift = sizeof(word) * 4;
    return x *= mix, (x << shift) | (x >> shift); }

  if (datp(x)) return typof(x)->xx(f, x);
  if (!owns(f, x)) return mix ^ (mix * x);

  // it's a function, hash by length
  struct tag *t = ttag((cell*) x);
  word len = (cell*) t - t->head;
  return mix ^ (mix * len); }

static table *mktbl(core *f) {
  table *t = cells(f, Width(table) + 1);
  struct entry **tab = (struct entry**) (t + 1);
  return !t ? 0 : ini_table(t, 0, 1, (tab[0] = 0, tab)); }

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
  memset(tab1, 0, cap1 * sizeof(word));
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

static NoInline table *table_set(core *f, table *t, word k, word v) {
  word index = index_of_key(f, t, k);
  struct entry *entry = t->tab[index];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  return entry ? (entry->val = v, t) : table_insert(f, t, k, v, index); }

static struct entry *table_delete_r(core *f, table *t, word k, word *v, struct entry *e) {
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

static Vm(tnew) {
  Have(Width(table) + 1);
  table *t = (table*) Hp;
  struct entry **tab = (struct entry**) (t + 1);
  return Hp += Width(table) + 1,
         tab[0] = 0,
         Sp[0] = (word) ini_table(t, 0, 1, tab),
         Ip++,
         Continue(); }

static word table_get(core *f, table *t, word k, word zero) {
  size_t i = index_of_key(f, t, k);
  struct entry *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;
  return e ? e->val : zero; }

static Vm(tget) { return
  Sp[2] = !tblp(Sp[1]) ? Sp[0] : table_get(f, (table*) Sp[1], Sp[2], Sp[0]),
  Sp += 2,
  Ip += 1,
  Continue(); }

static Vm(tset) {
  if (tblp(Sp[0])) {
    Pack(f);
    if (!table_set(f, (table*) Sp[0], Sp[1], Sp[2])) return Oom;
    Unpack(f); }
  return Sp += 2, Ip += 1, Continue(); }

static Vm(tdel) { return
  Sp[2] = !tblp(Sp[1]) ? nil : table_delete(f, (table*) Sp[1], Sp[2], Sp[0]),
  Sp += 2,
  Ip += 1,
  Continue(); }

static Vm(tlen) { return
  Sp[0] = tblp(Sp[0]) ? putnum(((table*)Sp[0])->len) : nil,
  Ip += 1,
  Continue(); }

static Vm(tkeys) {
  word list = nil;
  if (tblp(Sp[0])) {
    table *t = (table*) Sp[0];
    word len = t->len;
    Have(len * Width(Pair));
    pair *pairs = (Pair*) Hp;
    Hp += len * Width(Pair);
    for (int i = t->cap; i;)
      for (struct entry *e = t->tab[--i]; e; e = e->next)
        ini_pair(pairs, e->key, list),
        list = (word) pairs, pairs++; }
  return Sp[0] = list, Ip++, Continue(); }

static cell *trim_thread(cell *k) { return ttag(k)->head = k; }
static Vm(trim) { return Sp[0] = W(trim_thread((cell*) Sp[0])), Ip += 1, Continue(); }
static Vm(seek) { return Sp[1] = W(((cell*) Sp[1]) + getnum(Sp[0])), Sp += 1, Ip += 1, Continue(); }
static Vm(peek) { return Sp[0] = R(Sp[0])->x, Ip += 1, Continue(); }
static Vm(poke) { return R(Sp[1])->x = Sp[0], Sp += 1, Ip += 1, Continue(); }
static Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct tag));
  cell *k = R(Hp);
  struct tag *t = (struct tag*) (k + n);
  return Hp += n + Width(struct tag),
         t->null = NULL,
         t->head = k,
         memset(k, -1, n * sizeof(word)),
         Sp[0] = (word) k,
         Ip += 1,
         Continue(); }

static struct tag *ttag(cell *k) {
  while (k->x) k++;
  return (struct tag*) k; }

struct symbol { DataHeader; string *nom; word code; symbol *l, *r; };

Vm(symbolp) { return Sp[0] = symp(Sp[0]) ? putnum(-1) : nil, Ip++, Continue(); }
Vm(nullp) { return Sp[0] = nilp(Sp[0]) ? putnum(-1) : nil, Ip++, Continue(); }
static bool symp(word _) { return homp(_) && typof(_) == &sym_type; }

static symbol *ini_sym(symbol *y, string *nom, uintptr_t code) {
  return y->ap = data, y->typ = &sym_type, y->nom = nom, y->code = code, y->l = y->r = 0, y; }
static symbol *ini_anon(symbol *y, word code) {
  return y->ap = data, y->typ = &sym_type, y->nom = 0, y->code = code, y; }

static symbol *intern_seek(core *v, string *b, symbol **y) {
  symbol *z = *y;
  if (!z) return *y =
    ini_sym(bump(v, Width(symbol)), b, hash(v, putnum(hash(v, (word) b))));
  string *a = z->nom;
  int i = a->len < b->len ? -1 :
          a->len > b->len ? 1 :
          strncmp(a->text, b->text, a->len);
  return i == 0 ? z : intern_seek(v, b, i < 0 ? &z->l : &z->r); }

static symbol *intern(core *f, string* b) {
  if (avail(f) < Width(symbol)) {
    bool ok;
    avec(f, b, ok = gw_please(f, Width(symbol)));
    if (!ok) return 0; }
  return intern_seek(f, b, &f->symbols); }

static Vm(nomsym) {
  Have(Width(symbol));
  symbol *y; return
    Pack(f),
    y = intern_seek(f, (string*) f->sp[0], &f->symbols),
    Unpack(f),
    Sp[0] = W(y),
    Ip++,
    Continue(); }

static Vm(gensym) {
  if (strp(Sp[0])) return Ap(nomsym, f);
  const int req = Width(symbol) - 2;
  Have(req);
  symbol *y = (symbol*) Hp;
  return Hp += req, Sp[0] = W(ini_anon(y, rand())), Ip++, Continue(); }

static Vm(symnom) {
  word y = Sp[0];
  return y = symp(y) && ((symbol*)y)->nom ? W(((symbol*)y)->nom) : nil,
         Sp[0] = y,
         Ip++,
         Continue(); }

// FIXME io ???
#define io(n, x) word _ = (x); *(Sp += n-1) = _; Ip++; return Continue()
Vm(add)  { io(2, (Sp[0]|1) + (Sp[1]&~1)); }
Vm(sub)  { io(2, (Sp[0]|1) - (Sp[1]&~1)); }
Vm(mul)  { io(2, putnum(getnum(Sp[0])*getnum(Sp[1]))); }
Vm(quot) { io(2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])/getnum(Sp[1]))); }
Vm(rem)  { io(2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])%getnum(Sp[1]))); }
Vm(eq) { io(2, eql(f, Sp[0], Sp[1]) ? putnum(-1) : nil); }
Vm(lt) { io(2, Sp[0] < Sp[1] ? putnum(-1) : nil); }
Vm(le) { io(2, Sp[0] <= Sp[1] ? putnum(-1) : nil); }
Vm(gt) { io(2, Sp[0] > Sp[1] ? putnum(-1) : nil); }
Vm(ge) { io(2, Sp[0] >= Sp[1] ? putnum(-1) : nil);}
Vm(bnot) { io(1, ~Sp[0] | 1); }
Vm(band) { io(2, (Sp[0] & Sp[1]) | 1); }
Vm(bor)  { io(2, (Sp[0] | Sp[1]) | 1); }
Vm(bxor) { io(2, (Sp[0] ^ Sp[1]) | 1); }
Vm(rng)  { io(1, putnum(rand())); }
Vm(fixnump) { io(1, nump(Sp[0]) ? putnum(-1) : nil); }

// default equality method for things that are only equal to themselves
static bool eql(core *f, word a, word b) {
  // everything equals itself
  if (a == b) return true;
  // if a and b have same type
  if (     nump(a | b) ||
      R(a)->ap != data ||
      R(b)->ap != data ||
      typof(a) != typof(b)) return false;
  // in that case call the type's equality method to check
  return typof(a)->eq(f, a, b); }

static word
  cp_two(core *v, word x, word *p0, word *t0),
  cp_sym(core *f, word x, word *p0, word *t0),
  xx_two(core *f, word x),
  xx_sym(core *v, word _);
static void wk_two(core *f, word x, word *p0, word *t0),
  em_two(core *f, FILE *o, word x),
  wk_sym(core *f, word x, word *p0, word *t0),
  em_sym(core *f, FILE *o, word x);

static bool eq_two(core *f, word x, word y);
static bool eq_not(core *f, word a, word b) { return false; }
static type
  two_type = { .xx = xx_two, .cp = cp_two, .wk = wk_two, .em = em_two, .eq = eq_two, },
  str_type = { .xx = xx_str, .cp = cp_str, .wk = wk_str, .em = em_str, .eq = eq_str, },
  sym_type = { .xx = xx_sym, .cp = cp_sym, .wk = wk_sym, .eq = eq_not, .em = em_sym, },
  tbl_type = { .xx = xx_tbl, .cp = cp_tbl, .wk = wk_tbl, .eq = eq_not, .em = em_tbl, };

static word cp_two(core *v, word x, word *p0, word *t0) {
  pair *src = (pair*) x,
       *dst = ini_pair(bump(v, Width(pair)), src->a, src->b);
  return W(src->ap = (vm*) dst); }

static void wk_two(core *f, word x, word *p0, word *t0) {
  f->cp += Width(Pair);
  A(x) = cp(f, A(x), p0, t0);
  B(x) = cp(f, B(x), p0, t0); }

static void em_two(core *f, FILE *o, word x) {
  if (A(x) == W(f->quote) && twop(B(x)))
    putc('\'', o),
    transmit(f, o, AB(x));
  else for (putc('(', o);; putc(' ', o)) {
    transmit(f, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

// FIXME could overflow the stack -- use off pool for this
static bool eq_two(core *f, word x, word y) {
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static word xx_two(core *f, word x) {
  word hc = hash(f, A(x)) * hash(f, B(x));
  return hc ^ mix; }

static word xx_sym(core *v, word _) { return ((symbol*) _)->code; }
static word cp_sym(core *f, word x, word *p0, word *t0) {
  symbol *src = (symbol*) x,
         *dst = src->nom ?
           intern_seek(f, (string*) cp(f, (word) src->nom, p0, t0), &f->symbols) :
           ini_anon(bump(f, Width(symbol) - 2), src->code);
  return (word) (src->ap = (vm*) dst); }

static void wk_sym(core *f, word x, word *p0, word *t0) {
  f->cp += Width(symbol) - (((symbol*)x)->nom ? 0 : 2); }

static void em_sym(core *f, FILE *o, word x) {
  string* s = ((symbol*) x)->nom;
  if (s) for (int i = 0; i < s->len; putc(s->text[i++], o));
  else fprintf(o, "#sym@%lx", (long) x); }

static word cp_str(core* v, word x, word *p0, word *t0) {
  string* src = (string*) x;
  size_t len = sizeof(string) + src->len;
  return (word) (src->ap = memcpy(bump(v, b2w(len)), src, len)); }

static void wk_str(core* f, word x, word *p0, word *t0) {
  f->cp += Width(string) + b2w(((string*) x)->len); }

static void em_str(core* v, FILE *o, word _) {
  string* s = (string*) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

static word xx_str(core *v, word _) {
  string *s = (string*) _;
  uintptr_t h = 1;
  size_t words = s->len / sizeof(word),
         bytes = s->len % sizeof(word);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const intptr_t *ws = (intptr_t*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static bool eq_str(core *f, word x, word y) {
  string *a = (string*) x, *b = (string*) y;
  return a->len == b->len && 0 == strncmp(a->text, b->text, a->len); }

static word xx_tbl(core *f, word h) { return mix; }

static word cp_tbl(core *f, word x, word *p0, word *t0) {
  table *src = (table*) x;
  size_t len = src->len, cap = src->cap;
  table *dst = bump(f, Width(table) + cap + Width(struct entry) * len);
  struct entry **tab = (struct entry**) (dst + 1),
               *dd = (struct entry*) (tab + cap);
  src->ap = (vm*) ini_table(dst, len, cap, tab);
  while (cap--) {
    struct entry *s = src->tab[cap], *last = NULL;
    while (s) {
      struct entry *d = dd++;
      d->key = s->key;
      d->val = s->val;
      d->next = last;
      last = d;
      s = s->next; }
    tab[cap] = last; }
  return (word) dst; }

static void wk_tbl(core *f, word x, word *p0, word *t0) {
  table *t = (table*) x;
  f->cp += Width(table) + t->cap + t->len * Width(struct entry);
  for (word i = 0, lim = t->cap; i < lim; i++)
    for (struct entry*e = t->tab[i]; e;
      e->key = cp(f, e->key, p0, t0),
      e->val = cp(f, e->val, p0, t0),
      e = e->next); }

static void em_tbl(core *f, FILE *o, word x) {
  table *t = (table*) x;
  fprintf(o, "#table:%ld/%ld@%lx", (long) t->len, (long) t->cap, (long) x); }

static symbol *symof(core *f, const char *nom) {
  string *o = strof(f, nom);
  return o ? intern(f, o) : 0; }

#define d_entry(bn, n, _) && gw_ini_def(f, n, W(bn))
#define i_entry(i)        && gw_ini_def(f, "i_"#i, W(i))
static NoInline bool gw_ini_def(core *f, const char *k, word v) {
  symbol *y; return pushs(f, 1, v) && (y = symof(f, k)) &&
                      table_set(f, f->dict, (word) y, pop1(f)); }

#define insts(_) \
  _(dot) _(free_variable)\
  _(data) _(ret) _(ap) _(tap) _(apn) _(tapn) \
  _(jump) _(cond) _(ref) _(imm) _(yield) _(drop1) \
  _(curry) _(defglob) _(lazy_bind) _(ret0)

#define S1(i) {{i}, {ret0}}
#define S2(i) {{curry},{.x=putnum(2)},{i}, {ret0}}
#define S3(i) {{curry},{.x=putnum(3)},{i}, {ret0}}
#define BIFS(_) \
  _(bif_clock, "clock", S1(sysclock))\
  _(bif_add, "+", S2(add)) _(bif_sub, "-", S2(sub)) \
  _(bif_mul, "*", S2(mul)) _(bif_quot, "/", S2(quot)) _(bif_rem, "%", S2(rem)) \
  _(bif_lt, "<", S2(lt))  _(bif_le, "<=", S2(le)) _(bif_eq, "=", S2(eq))\
  _(bif_ge, ">=", S2(ge))  _(bif_gt, ">", S2(gt)) \
  _(bif_rand, "rand", S1(rng)) \
  _(bif_cons, "X", S2(cons)) _(bif_car, "A", S1(car)) _(bif_cdr, "B", S1(cdr)) \
  _(bif_sget, "sget", S2(sget)) _(bif_ssub, "ssub", S3(ssub)) \
  _(bif_slen, "slen", S1(slen)) _(bif_scat, "scat", S2(scat)) \
  _(bif_dot, ".", S1(dot)) _(bif_putc, "putc", S1(prc)) \
  _(bif_bnot, "~", S1(bnot)) \
  _(bif_thd, "thd", S1(thda)) _(bif_peek, "peek", S1(peek)) _(bif_poke, "poke", S2(poke))\
  _(bif_trim, "trim", S1(trim)) _(bif_seek, "seek", S2(seek)) \
  _(bif_tnew, "tnew", S1(tnew)) _(bif_tkeys, "tkeys", S1(tkeys)) \
  _(bif_tlen, "tlen", S1(tlen))\
  _(bif_tset, "tset", S3(tset)) _(bif_tget, "tget", S3(tget)) _(bif_tdel, "tdel", S3(tdel))\
  _(bif_twop, "twop", S1(pairp)) _(bif_strp, "strp", S1(stringp))\
  _(bif_symp, "symp", S1(symbolp)) _(bif_nump, "nump", S1(fixnump))\
  _(bif_nilp, "nilp", S1(nullp))\
  _(bif_sym, "sym", S1(gensym)) _(bif_nom, "nom", S1(symnom))\
  _(bif_ev, "ev", S1(ev0))\
  _(bif_isatty, "isatty", S1(p_isatty))\
  _(bif_read, "read", S1(read0)) _(bif_readf, "readf", S1(readf))
#define bif_entry(n, _, d) static const cell n[] = d;
BIFS(bif_entry);

static int gw_fin(core *f, int s) { return
  free(min(f->pool, f->loop)), f->pool = f->loop = NULL, s; }
static Vm(yield) { return Pack(f), YieldStatus; }
static cell bif_yield[] = { {yield} };
static int gw_ini(core *f) {
  memset(f, 0, sizeof(core));
  const uintptr_t len0 = 1;
  word *pool = malloc(2 * len0 * sizeof(word));
  if (!pool) return Oom;
  f->t0 = gw_clock();
  f->sp = f->loop = (f->hp = f->pool = pool) + (f->len = len0);
  f->ip = bif_yield;
  string *v;
  return
    (f->dict = mktbl(f)) &&
    (f->macro = mktbl(f)) &&
    (f->eval = symof(f, "ev")) &&
    (f->let = symof(f, ":")) &&
    (f->cond = symof(f, "?")) &&
    (f->quote = symof(f, "`")) &&
    (f->begin = symof(f, ",")) &&
    (f->lambda = symof(f, "\\")) &&
    gw_ini_def(f, "globals", W(f->dict)) &&
    gw_ini_def(f, "macros", W(f->macro)) &&
    (v = strof(f, VERSION)) &&
    gw_ini_def(f, "version", W(v)) insts(i_entry) BIFS(d_entry) ?
      Ok : gw_fin(f, Oom); }

static word mkargv(p_core *f, const char **av) {
  if (!*av) return nil;
  string *s = strof(f, *av);
  if (!s) return 0;
  word r; return avec(f, s, r = mkargv(f, av + 1)),
                 !r ? r : wpairof(f, (word) s, r); }

static Inline int mkxpn(core *f, const char **av) {
  word v = mkargv(f, av);                  // get argv
  v = !v ? v : wpairof(f, v, nil);         // put it in a list
  v = !v ? v : wpairof(f, W(f->quote), v); // quote it
  v = !v ? v : wpairof(f, v, nil);         // put that in a list
  v = !v ? v : wpairof(f, f->sp[0], v);    // apply the function
  return !v ? Oom : (f->sp[0] = v, Ok); }

int gw_main(const char *p, const char **av) {
  In *i = ((In*)&(TextIn){{p_text_getc, p_text_ungetc, p_text_eof}, p, 0});
  core *f = &((core){});
  int s = gw_ini(f);
  s = s != Ok ? s : p_read1(f, i);
  s = s != Ok ? s : mkxpn(f, av);
  s = s != Ok ? s : p_eval(f, yieldi);
  return gw_fin(f, s); }
