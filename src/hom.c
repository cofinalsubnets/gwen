#include "i.h"
//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|?|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.

// allocate a thread
verb mo_n(state f, size n) {
  verb k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

static NoInline verb thdr(state f, size n, va_list xs) {
  ob x = va_arg(xs, ob);
  if (!x) return mo_n(f, n);
  verb k; avec(f, x, k = thdr(f, n + 1, xs));
  if (k) k[n].x = x;
  return k; }

NoInline verb thd(state f, ...) {
  verb k; va_list xs; return
    va_start(xs, f),
    k = thdr(f, 0, xs),
    va_end(xs),
    k; }

status yield(state f, verb ip, word *hp, word *sp) {
  return Pack(), Ok; }

status ref(state f, verb ip, word *hp, word *sp) {
  Have1();
  word x = sp[getnum(ip[1].x)];
  *--sp = x;
  ip += 2;
  return ip->ap(f, ip, hp, sp); }

status K(state f, verb ip, word *hp, word *sp) {
  Have1();
  return
    *--sp = ip[1].x,
    ip += 2,
    ip->ap(f, ip, hp, sp); }

status jump(state f, verb ip, word *hp, word *sp) {
  return ip = (mo) ip[1].x, ip->ap(f, ip, hp, sp); }
status Kj(state f, verb ip, word *hp, word *sp) {
  Have1();
  *--sp = ip[1].x;
  ip = (mo) ip[2].x;
  return ip->ap(f, ip, hp, sp); }

status branch(state f, verb ip, word *hp, word *sp) {
  ip = nilp(*sp++) ? ip[1].m : ip + 2;
  return ip->ap(f, ip, hp, sp); }

status retn(state f, verb ip, word *hp, word *sp) {
  ob r = *sp++;
  sp += getnum(ip[1].x);
  ip = (verb) *sp;
  *sp = r;
  return ip->ap(f, ip, hp, sp); }

status apply(state f, verb ip, word *hp, word *sp) {
  if (nump(sp[1])) ip++;
  else {
    mo j = (mo) sp[1];
    sp[1] = (ob) (ip + 1);
    ip = j; }
  return ip->ap(f, ip, hp, sp); }

status curry(state f, verb ip, word *hp, word *sp) {
  intptr_t n = getnum(ip[1].x);
  if (n == 1) return
    ip = ip[2].m, ip->ap(f, ip, hp, sp);
  const size S = 3 + Width(struct tag);
  Have(2 * S);
  verb c0 = (mo) hp, c1;
  hp += S;
  c0[0].ap = Kj;
  c0[1].x = *sp++;
  c0[2].x = ip[2].x;
  c1 = c0 + S,
  hp += S,
  c1[0].ap = curry,
  c1[1].x = putnum(n - 1),
  c1[2].x = (ob) c0,
  c0 = c1;
  ip = (mo) *sp;
  *sp = (ob) c0;
  return ip->ap(f, ip, hp, sp); }

struct cctx {
  word s1, s2, eb, ib, sb, sn;
  struct cctx *par; };

#define One 2
static struct cctx *scope(state f, struct cctx **par) {
  struct cctx *sc = (void*) mo_n(f, Width(struct cctx));
  if (sc)
    sc->s1 = sc->s2 = sc->eb = sc->ib = sc->sb = sc->sn = nil,
    sc->par = par ? *par : (void*) nil;
  return sc; }

static size llen(ob l) {
  size n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

static verb yield_thread(state f, struct cctx **c, verb k) {
  return k; }

static verb pull_thread(state f, struct cctx **c, verb k) { return
  ((mo (*)(state, struct cctx**, mo)) (*f->sp++))(f, c, k); }
static verb e1(state f, struct cctx **c, verb k) {
  return (--k)->x = *f->sp++,
         pull_thread(f, c, k); }
static verb e2(state f, struct cctx **c, verb k) {
  return k[-2].x = *f->sp++,
         k[-1].x = *f->sp++,
         pull_thread(f, c, k - 2); }

static word lidx(state f, ob l, ob x) {
  for (word i = 0; twop(l); l = B(l), i++)
    if (eql(f, A(l), x)) return i;
  return -1; }

static verb var(state f, struct cctx **c, verb k) {
  word sym = *f->sp++,
       off = getnum(*f->sp++),
       ldx = lidx(f, (*c)->sb, sym),
       idx = ldx + off;
  return
    k[-2].ap = ref,
    k[-1].x = putnum(idx),
    pull_thread(f, c, k - 2); }

static size ana(state, struct cctx**, size, word);
static verb cata(O f, struct cctx **c, size m) {
  assert((*c)->sn == nil);
  verb k = mo_n(f, m);
  if (!k) return k;
  return pull_thread(f, c, k + m); }

static size yld(state f, size m) {
  return pushs(f, e1, yield, End) ? m + 1 : 0; }

verb compile_expression(state f, word x) {
  verb k = 0;
  struct cctx *c = pushs(f, x, yield_thread, End) ? scope(f, NULL) : NULL;
  if (c) avec(f, c,
    x = ana(f, &c, 0, pop1(f)),
    x = x ? yld(f, x) : x,
    c->sn -= One,
    k = x ? cata(f, &c, x) : k);
  return k; }

bool kstrq(str s0, const char *s1) { return
  strlen(s1) == s0->len && strncmp(s0->text, s1, s0->len) == 0; }

static size value(state f, struct cctx**c, size m, word x) {
  return pushs(f, e2, K, x, End) ? m + 2 : 0; }

static size ana_str(state f, struct cctx **c, size m, word x) {
  if (lidx(f, (*c)->sb, x) < 0) {
    x = (word) pair(f, x, (*c)->sb);
    if (!x) return x;
    (*c)->sb = x;
    x = (word) pair(f, A(x), (*c)->ib);
    if (!x) return x;
    (*c)->ib = x;
    x = A(x); }
  return pushs(f, var, x, (*c)->sn, End) ? m + 2 : 0; }

static size ana_two(state, struct cctx**, size, word);

static size ana(state f, struct cctx **c, size m, word x) {
  m = twop(x) ? ana_two(f, c, m, x) :
      strp(x) ? ana_str(f, c, m, x) :
                value(f, c, m, x);
  (*c)->sn += One; // caller should do this instead
  return m; }

static verb
  ana_cond_push_continuation(state, struct cctx **, verb),
  ana_cond_push_alternative(state, struct cctx**, verb),
  ana_cond_pop_alternative(state, struct cctx**, verb),
  ana_cond_pop_continuation(state, struct cctx**, verb);

static verb ana_cond_pop_alternative(state f, struct cctx**c, verb k) {
  (--k)->x = A((*c)->s1);
  (--k)->ap = branch;
  (*c)->s1 = B((*c)->s1);
  return pull_thread(f, c, k); }

static size ana_cond(state f, struct cctx **c, uintptr_t m, word x) {
  if (!pushs(f, x, ana_cond_pop_continuation, End)) return 0;
  x = pop1(f);
  MM(f, &x);
  for (; m; x = B(B(x))) {
    if (!twop(x)) {
      x = (ob) pair(f, x, nil);
      if (!x) { m = 0; break; } }
    if (!twop(B(x))) {
      m = ana(f, c, m, A(x)), (*c)->sn -= One;
      break; }
    m = ana(f, c, m + 2, A(x)), (*c)->sn -= One;
    m = m && pushs(f, ana_cond_pop_alternative, End) ? m : 0;
    m = m ? ana(f, c, m + 2, A(B(x))): 0, (*c)->sn -= One;
    m = m && pushs(f, ana_cond_push_alternative, End) ? m + 2 : 0; }
  UM(f);
  return m && pushs(f, ana_cond_push_continuation, End) ? m : 0; }

// reverse decons: pushes last list item to stack, returns init of list.
static word snoced(state f, word x) {
  if (!twop(x)) return push1(f, nil) ? nil : 0;
  if (!twop(B(x))) return push1(f, A(x)) ? nil : 0;
  ob y = A(x);
  avec(f, y, x = snoced(f, B(x)));
  return x ? (word) pair(f, y, x) : x; }

static size ana_lambda(state f, struct cctx **c, size m, word x) {
  x = snoced(f, x);
  if (!x || !push1(f, x)) return 0;
  struct cctx *d = scope(f, c);
  if (!d) return 0;
  d->eb = d->sb = pop1(f);
  size n;
  verb k;
  avec(f, d,
    n = pushs(f, pop1(f), yield_thread, End) ? ana(f, &d, 2, pop1(f)) : 0,
    d->sn -= One,
    k = n && pushs(f, e2, retn, putnum(llen(d->sb)), End) ? cata(f, &d, n) : 0,
    n = llen(d->sb),
    k = k && n > 1 ? thd(f, curry, putnum(n), k, End) : k,
    x = k && twop(d->ib) ? (word) pair(f, (word) k, d->ib) : (word) k);
  if (!x) return x;
  x = x ? ana(f, c, m, x) : x;
  (*c)->sn -= One;
  return x; }

static size ap(state f, struct cctx **c, size m) {
  if (!pushs(f, e1, apply, End)) return 0;
  (*c)->sn -= One;
  return m + 1; }

static size ana_two(state f, struct cctx **c, size m, word x) {
  if (strp(A(x))) {
    str s = (str) A(x);
    if (kstrq(s, Quote)) return value(f, c, m, twop(B(x)) ? A(B(x)) : B(x));
    if (kstrq(s, Cond)) return ana_cond(f, c, m, B(x));
    if (kstrq(s, Lambda)) return ana_lambda(f, c, m, B(x)); }
  for (MM(f, &x), m = ana(f, c, m, A(x)), x = B(x); m && twop(x); x = B(x))
    m = ana(f, c, m, A(x)), m = ap(f, c, m);
  (*c)->sn -= One;
  return UM(f), m; }

static verb ana_cond_push_continuation(state f, struct cctx **c, verb k) {
  two w = pair(f, (ob) k, (*c)->s2);
  return !w ? (verb) w : pull_thread(f, c, (verb) A((*c)->s2 = (ob) w)); }

static verb ana_cond_push_alternative(state f, struct cctx**c, verb k) {
  two w = pair(f, (ob) k, (*c)->s1);
  if (!w) return (verb) w;
  k = (verb) A((*c)->s1 = (ob) w);
  verb kk = (verb) A((*c)->s2);
  if (kk->ap == retn)
    (--k)->x = kk[1].x, (--k)->ap = retn;
  else (--k)->m = kk, (--k)->ap = jump;
  return pull_thread(f, c, k); }

static verb ana_cond_pop_continuation(state f, struct cctx **c, verb k) {
  return (*c)->s2 = B((*c)->s2), pull_thread(f, c, k); }
