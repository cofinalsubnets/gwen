#include <stdarg.h>
#include "em.h"

////
/// host embedding
//
// this phase does no optimization
//
// pull back the pushforward
#define Pull(...) pushs(v, __VA_ARGS__, (ob) 0)
// push forward the pullback
#define Push(m) ((c1*)getnum(*v->sp++))(v, e, m)


// " compilation environments "
#define arg(x)  ((ob*)(x))[0] // argument variables : a list
#define loc(x)  ((ob*)(x))[1] // local variables : a list
#define clo(x)  ((ob*)(x))[2] // closure variables : a list
#define par(x)  ((ob*)(x))[3] // surrounding scope : tuple or nil
#define name(x) ((ob*)(x))[4] // function name : a symbol or nil
#define asig(x) ((ob*)(x))[5] // arity signature : an integer
#define s1(x)   ((ob*)(x))[6]
#define s2(x)   ((ob*)(x))[7]
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.

enum where { Here, Loc, Arg, Clo, Wait };
#define Co(nom,...) yo nom(em v, ob* e, uintptr_t m, ##__VA_ARGS__)
static bool scan(em, ob*, ob);
typedef Co(c1);
typedef Co(c2, ob);
static c1 xx_yo_, let_yo_bind, em_i, em_i_d, mk_yo;
static c2 xx_yo, var_yo, two_yo, im_yo;
static ob yo_yo_clo(em, ob*, ob, ob),
          yo_yo_lam(em, ob*, ob, ob);

#define Put(x) putnum((ob)(x))

static bool pushss(em v, uintptr_t i, va_list xs) {
  bool _;
  ob x = va_arg(xs, ob);
  return !x ?  Avail >= i || please(v, i) :
    (with(x, _ = pushss(v, i+1, xs)),
     _ && (*--v->sp = x)); }

static bool pushs(em v, ...) {
  va_list xs;
  va_start(xs, v);
  bool _ = pushss(v, 0, xs);
  va_end(xs);
  return _; }

static Inline yo ee1(ll *i, yo k) { return (--k)->ll = i, k; }
static Inline yo ee2(ll *i, ob x, yo k) { return ee1(i, ee1((ll*) x, k)); }

// helper functions for lists
static intptr_t lidx(ob l, ob x) {
  for (intptr_t i = 0; twop(l); l = B(l), i++)
    if (x == A(l)) return i;
  return -1; }

static ob linitp(em v, ob x, ob* d) {
  ob y;
  return !twop(B(x)) ? (*d = x, nil) :
    (with(x, y = linitp(v, B(x), d)),
     !y ? 0 : pair(v, A(x), y)); }

static ob snoc(em v, ob l, ob x) {
  return !twop(l) ? pair(v, x, l) :
    (with(l, x = snoc(v, B(l), x)),
     !x ? 0 : pair(v, A(l), x)); }

static yo ini_yo(em v, uintptr_t n) {
  yo a = cells(v, n + 2);
  return !a ? 0 :
    (a[n].ll = NULL,
     a[n+1].ll = (ll*) a,
     setptr((ob*) a, nil, n),
     a + n); }

static yo tuplr(em v, uintptr_t i, va_list xs) {
  ob x = va_arg(xs, ob);
  yo k;
  return !x ? ini_yo(v, i) :
    (with(x, k = tuplr(v, i+1, xs)),
     k ? ee1((ll*) x, k) : 0); }

static ob tupl(em v, ...) {
  yo t;
  va_list xs;
  va_start(xs, v);
  t = tuplr(v, 0, xs);
  va_end(xs);
  return (ob) t; }

static yo imx(em v, ob *e, intptr_t m, ll *i, ob x) {
  return Pull(Put(i), x) ? em_i_d(v, e, m) : 0; }

#define Bind(v, x) if(!((v)=(x)))goto fail
static NoInline ob rw_let_fn(em v, ob x) {
  ob y;
  for (mm(&x); twop(A(x));) {
    Bind(y, snoc(v, BA(x), AB(x)));
    Bind(y, pair(v, v->glob[Lamb], y));
    Bind(y, pair(v, y, BB(x)));
    Bind(x, pair(v, AA(x), y)); }
  return um, x; fail:
  return um, 0; }

static int scan_def(em v, ob *e, ob x) {
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(B(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  mm(&x);
  int r = scan_def(v, e, BB(x));
  if (r == 1) {
    Bind(x, rw_let_fn(v, x));
    ob y;
    Bind(y, pair(v, A(x), loc(*e)));
    loc(*e) = y;
    Bind(y, scan(v, e, AB(x))); }
  return um, r; fail:
  return um, -1; }

static bool scan(em v, ob* e, ob x) {
  if (!twop(x) ||
      A(x) == v->glob[Lamb] ||
      A(x) == v->glob[Quote])
    return true;
  if (A(x) == v->glob[Def])
    return scan_def(v, e, B(x)) != -1;
  for (mm(&x); twop(x); x = B(x))
    if (!scan(v, e, A(x))) return um, false;
  return um, true; }

static ob asign(em v, ob a, intptr_t i, ob*m) {
  ob x;
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == v->glob[Splat]) {
    *m = -i-1;
    return pair(v, A(a), nil); }
  with(a, x = asign(v, B(a), i+1, m));
  return !x ? 0 : pair(v, A(a), x); }

static Inline ob new_scope(em v, ob*e, ob a, ob n) {
  intptr_t s = 0;
  with(n, a = asign(v, a, 0, &s));
  return !a ? 0 : tupl(v, a, nil, nil, e ? *e : nil, n, putnum(s), nil, nil, (ob) 0); }

static Inline ob comp_body(em v, ob*e, ob x) {
  bind(x, Pull(Put(xx_yo_), x,
               Put(em_i), Put(ret),
               Put(mk_yo)));
  scan(v, e, v->sp[1]);
  bind(x, (ob) Push(4)); // 4 = 2 + 2
  intptr_t i = llen(loc(*e));
  if (i) x = (ob) ee2(locals, putnum(i), (yo) x);
  i = getnum(asig(*e));
  if (i > 0) x = (ob) ee2(arity, putnum(i), (yo) x);
  else if (i < 0) x = (ob) ee2(vararg, putnum(-i-1), (yo) x);
  button(gethom(x))[1].ll = (ll*) x;
  return twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static Inline ob yo_yo_lam(em v, ob* e, ob n, ob l) {
  ob y = nil;
  l = B(l);
  mm(&n); mm(&y); mm(&l);
  Bind(l, twop(l) ? l : pair(v, l, nil));
  Bind(l, linitp(v, l, &y));
  Bind(n, pair(v, n, e ? name(*e) : nil));
  Bind(n, new_scope(v, e, l, n));
  Bind(l, comp_body(v, &n, A(y)));
  return um, um, um, l; fail:
  return um, um, um, 0; }

static Inline ob yo_yo_clo(em v, ob*e, ob arg, ob seq) {
  intptr_t i = llen(arg);
  mm(&arg), mm(&seq);
  bool _;
  Bind(_, Pull(
    Put(em_i_d), Put(take), putnum(i),
    Put(mk_yo)));
  while (twop(arg)) {
    Bind(_, Pull(
      Put(xx_yo_), A(arg),
      Put(em_i), Put(push)));
    arg = B(arg); }

  Bind(arg, (ob) Push(0));
  return um, um, pair(v, seq, arg); fail:
  return um, um, 0; }

static Co(pf_pf, ob x) {
 ll* j = imm;
 ob k, nom = *v->sp == Put(let_yo_bind) ? v->sp[1] : nil;
 with(nom, with(x, k = (ob) Push(m+2)));
 bind(k, k);
 mm(&k);
 if (twop(x = yo_yo_lam(v, e, nom, x)))
   j = e && twop(loc(*e)) ? encll : encln,
   x = yo_yo_clo(v, e, A(x), B(x));
 um;
 return !x ? 0 : ee2(j, x, (yo) k); }

static Co(im_yo, ob x) {
  return !(x = Pull(Put(imm), x)) ? 0 : em_i_d(v, e, m); }

static Co(let_yo_bind) {
  ob y = *v->sp++;
  return e ? imx(v, e, m, loc_, putnum(lidx(loc(*e), y))) :
             imx(v, e, m, tbind, y); }

static bool let_yo_r(em v, ob*e, ob x) {
  bool _;
  return !twop(x) ||
    ((x = rw_let_fn(v, x)) &&
     (with(x, _ = let_yo_r(v, e, BB(x))), _) &&
     Pull(Put(xx_yo_), AB(x), Put(let_yo_bind), A(x))); }

// syntactic sugar for define
static Inline ob def_sug(em v, ob x) {
  ob y = nil;
  with(y, x = linitp(v, x, &y));
  return x &&
    (x = pair(v, x, y)) &&
    (x = pair(v, v->glob[Seq], x)) &&
    (x = pair(v, x, nil)) &&
    (x = pair(v, v->glob[Lamb], x)) ?
      pair(v, x, nil) :
      0; }

static Co(let_yo, ob x) {
  return
    !twop(B(x)) ? im_yo(v, e, m, nil) :
    llen(B(x)) % 2 ?
      (x = def_sug(v, x)) ? xx_yo(v, e, m, x) : 0 :
    (x = let_yo_r(v, e, B(x))) ? Push(m) : 0; }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
static Co(if_pf_pre) {
  ob x; return
    (x = (ob) Push(m)) &&
    (x = pair(v, x, s2(*e))) ?
      (s2(*e) = x, (yo) A(x)) :
      0; }

// before generating a branch emit a jump to
// the top of stack 2
static Co(if_pf_pre_con) {
  yo k, x = Push(m + 2);
  return !x ? 0 :
    (k = (yo) A(s2(*e)),
     k->ll == ret ?
       ee1(ret, x) :
       ee2(jump, (ob) k, x)); }

// after generating a branch store its address
// in stack 1
static Co(if_pf_post_con) {
  ob x; return
    (x = (ob) Push(m)) &&
    (x = pair(v, x, s1(*e))) ?
      (s1(*e) = x, (yo) A(x)) :
      0; }

// before generating an antecedent emit a branch to
// the top of stack 1
static Co(if_pf_pre_ant) {
  yo x = Push(m+2);
  return !x ? 0 :
    (x = ee2(branch, A(s1(*e)), x),
     s1(*e) = B(s1(*e)),
     x); }

static bool if_pf_loop(em v, ob*e, ob x) {
  bool _;
  if (!twop(x)) bind(x, pair(v, nil, nil));
  if (!twop(B(x)))
    return Pull(Put(xx_yo_), A(x), Put(if_pf_pre_con));
  with(x,
    _ = Pull(
      Put(if_pf_post_con),
      Put(xx_yo_), AB(x),
      Put(if_pf_pre_con)));
  bind(_, _);
  with(x, _ = if_pf_loop(v, e, BB(x)));
  bind(_, _);
  return Pull(
    Put(xx_yo_), A(x),
    Put(if_pf_pre_ant)); }

static Co(if_pf, ob x) {
  bool _;
  yo k;
  with(x, _ = Pull(Put(if_pf_pre)));
  return _ && if_pf_loop(v, e, B(x)) && (k = Push(m)) ?
    (s2(*e) =  B(s2(*e)), k) :
    0; }

static Co(em_call) {
  ob a = *v->sp++;
  yo k = Push(m + 2);
  return !k ? 0 : ee2(k->ll == ret ? rec : call, a, k); }

static ob lookup_mod(em v, ob x) {
  return tbl_get(v, v->glob[Topl], x); }

static ob lookup_lex(em v, ob e, ob y) {
  if (nilp(e)) {
    ob q = lookup_mod(v, y);
    return q ? pair(v, putnum(Here), q) : pair(v, putnum(Wait), v->glob[Topl]); }
  return
    lidx(loc(e), y) > -1 ? pair(v, putnum(Loc), e) :
    lidx(arg(e), y) > -1 ? pair(v, putnum(Arg), e) :
    lidx(clo(e), y) > -1 ? pair(v, putnum(Clo), e) :
    lookup_lex(v, par(e), y); }

static Co(var_yo, ob x) {
  ob y, q;
  with(x, q = lookup_lex(v, e ? *e:nil, x));
  bind(q, q);
  y = A(q);
  switch ((enum where) getnum(y)) {
    case Here: return im_yo(v, e, m, B(q));
    case Wait: return
      (x = pair(v, B(q), x)) &&
      (with(x, y = (ob) Push(m+2)), y) &&
      (with(y, x = pair(v, putnum(sizeof(ob)), x)), x) ?
        ee2(lbind, x, (yo) y) :
        0;
    default:
      if (B(q) == *e) switch (getnum(y)) {
        case Loc: return imx(v, e, m, loc,
                           putnum(lidx(loc(*e), x)));
        case Arg: return imx(v, e, m, arg,
                           putnum(lidx(arg(*e), x)));
        default:  return imx(v, e, m, clo,
                           putnum(lidx(clo(*e), x))); }
      else return
        y = llen(clo(*e)),
        with(x, q = snoc(v, clo(*e), x)),
        !q ? 0 : (clo(*e) = q,
                  imx(v, e, m, clo, putnum(y))); } }

static Co(xx_yo_) { return xx_yo(v, e, m, *v->sp++); }
static Co(xx_yo, ob x) { return
  (symp(x) ? var_yo : twop(x) ? two_yo : im_yo)(v, e, m, x); }

static Co(ap_pf, ob fun, ob args) {
  mm(&args);
  Bind(fun, Pull(
    Put(xx_yo_), fun,
    Put(em_i), Put(idH),
    Put(em_call), putnum(llen(args))));
  while (twop(args)) {
    Bind(fun, Pull(
      Put(xx_yo_), A(args),
      Put(em_i), Put(push)));
    args = B(args); }

  return um, Push(m); fail:
  return um, NULL; }

static bool seq_yo_loop(em v, ob*e, ob x) {
  bool _;
  return !twop(x) ? 1 :
    (with(x, _ = seq_yo_loop(v, e, B(x))), _) &&
    Pull(Put(xx_yo_), A(x)); }

static Co(two_yo, ob x) {
  ob z = A(x); return 
    z == v->glob[Cond] ? if_pf(v, e, m, x) :
    z == v->glob[Def]  ? let_yo(v, e, m, x) :
    z == v->glob[Lamb] ? pf_pf(v, e, m, x) :

    z == v->glob[Seq]  ?
      (twop(x = B(x)) || (x = pair(v, x, nil))) &&
      (x = seq_yo_loop(v, e, x)) ?
        Push(m) :
        0 :

    z == v->glob[Quote] ?
      (x = twop(x = B(x)) ? A(x) : x,
       im_yo(v, e, m, x)) :

    ap_pf(v, e, m, A(x), B(x)); }

static Co(em_i) {
  ll* i = (ll*) getnum(*v->sp++);
  yo k = Push(m+1);
  return !k ? 0 : ee1(i, k); }

static Co(em_i_d) {
  ll* i = (ll*) getnum(*v->sp++);
  yo k;
  ob x = *v->sp++;
  with(x, k = Push(m+2));
  return !k ? 0 : ee2(i, x, k); }

static Co(mk_yo) {
  yo k = ini_yo(v, m+1);
  return !k ? 0 : ee1((ll*)(e ? name(*e) : nil), k); }

static ob apply(em, ob, ob) NoInline;
ob eval(em v, ob x) {
  ob args = pair(v, x, nil);
  return !args ? 0 :
  (x = homp(v->glob[Eval]) ?
     v->glob[Eval] : tbl_get(v, v->glob[Topl], v->glob[Eval]),
   apply(v, x, args)); }

// return to C
static Ll(yield) { Pack(); return xp; }

static NoInline ob apply(em v, ob f, ob x) {
  yo h; return
    !Pull(f, x) || !(h = cells(v, 5)) ? 0 :
      (h[0].ll = call,
       h[1].ll = (ll*) putnum(2),
       h[2].ll = yield,
       h[3].ll = NULL,
       h[4].ll = (ll*) h,
       x = tbl_get(v, v->glob[Topl], v->glob[Apply]),
       call(v, h, (ob*) v->fp, v->sp, v->hp, x)); }

// instructions used by the compiler
Ll(hom_u) {
  Arity(1);
  ob x = *Argv;
  TypeCheck(x, Num);
  intptr_t len = getnum(x) + 2;
  Have(len);
  yo k = (yo) hp;
  hp += len;
  setptr((ob*) k, nil, len);
  k[len-1].ll = (ll*) k;
  k[len-2].ll = NULL;
  return ApC(ret, (ob) (k+len-2)); }

Ll(hfin_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  yo k; return
    k = (yo) *Argv,
    button(k)[1].ll = (ll*) k,
    ApC(ret, (ob) k); }

Ll(emx) {
  yo k; return
    k = (yo) *sp++ - 1,
    k->ll = (ll*) xp,
    ApN(1, (ob) k); }

Ll(emi) {
  yo k; return
    k = (yo) *sp++ - 1,
    k->ll = (ll*) getnum(xp),
    ApN(1, (ob) k); }

Ll(emx_u) {
 Arity(2);
 CheckType(Argv[1], Hom);
 yo k = (yo) Argv[1];
 (--k)->ll = (ll*) Argv[0];
 return ApC(ret, (ob) k); }

Ll(emi_u) {
 Arity(2);
 TypeCheck(Argv[0], Num);
 ob h = Argv[1];
 TypeCheck(h, Hom);
 h -= sizeof(void*);
 gethom(h)->ll = (ll*) getnum(Argv[0]);
 return ApC(ret, h); }

Ll(hgeti_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, Put(gethom(*Argv)->ll)); }

Ll(hgetx_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, (ob) gethom(*Argv)->ll); }

Ll(hseek_u) {
  Arity(2);
  TypeCheck(Argv[0], Hom);
  TypeCheck(Argv[1], Num);
  return ApC(ret, puthom(gethom(Argv[0]) + getnum(Argv[1]))); }

ob analyze(em v, ob x) {
  with(x, Pull(Put(em_i), Put(ret), Put(mk_yo)));
  return (ob) xx_yo(v, NULL, 0, x); }

Ll(ev_u) {
  Arity(1);
  return
    homp(v->glob[Eval]) ?
      ApY((yo) v->glob[Eval], xp) :
    (Pack(),
     (v->ip = (yo) analyze(v, *Argv)) ?
       (Unpack(), ApY(ip, xp)) :
       0); }

Ll(bootstrap) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return
    v->glob[Eval] = xp = *Argv, // FIXME neither intern nor tbl_set will allocate if ev is already interned / defined
    tbl_set(v, v->glob[Topl], interns(v, "ev"), xp),
    ApC(ret, xp); }

Ll(hnom_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, homnom(v, *Argv)); }

ob sequence(em v, ob a, ob b) {
  yo h;
  with(a, with(b, h = cells(v, 8)));
  return !h ? 0 :
    (h[0].ll = imm,
     h[1].ll = (ll*) a,
     h[2].ll = call,
     h[3].ll = (ll*) N0,
     h[4].ll = jump,
     h[5].ll = (ll*) b,
     h[6].ll = NULL,
     h[7].ll = (ll*) h,
     (ob) h); }
