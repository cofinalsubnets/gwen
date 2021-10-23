#include "lips.h"
#include "terp.h"
#include "hom.h"
#include "table.h"

////
/// bootstrap thread compiler
//
// functions construct their continuations by pushing function
// pointers onto the main stack with Push
#define Push(...) pushs(v,__VA_ARGS__,(obj)0)
// and then calling them with Ccc
#define Ccc ((c1*)Gn(*Sp++))
// there's a natural correspondence between the push/Ccc pattern
// in this file and normal continuation passing style in lisp
// (cf. the stage 2 compiler).

// in addition to the main stack, the compiler uses Xp and Ip
// as stacks for storing code entry points when generating
// conditionals, which is admittedly kind of sus.
//
// this compiler emits runtime type checks for safety but does
// (almost) no optimizations or static typing since all it has
// to do is bootstrap the main compiler.

// " compilation environments "
// the current lexical environment is passed to compiler
// functions as a pointer to an object, either a tuple with a
// structure specified below, or nil for toplevel. it's a
// pointer to an object, instead of just an object, so it can
// be gc-protected once instead of separately by every function.
// in the other compiler it's just a regular object.
#define arg(x)  V(x)->xs[0] // argument variables : a list
#define loc(x)  V(x)->xs[1] // local variables : a list
#define clo(x)  V(x)->xs[2] // closure variables : a list
#define par(x)  V(x)->xs[3] // surrounding scope : tuple or nil
#define name(x) V(x)->xs[4] // function name : a symbol or nil
#define asig(x) V(x)->xs[5] // arity signature : an integer
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.

static u0 scan(lips, mem, obj);
static obj hfin(lips, obj), hini(lips, u64), c_la_clo(lips, mem, obj, obj), ltu(lips, mem, obj, obj);
typedef obj c1(lips, mem, u64), c2(lips, mem, u64, obj);
static c1 pre_eval_, pre_bind, inst, insx, pre_alloc;
static c2 pre_eval, pre_var, pre_two, pre_imm, pre_apply;

enum { Here, Loc, Arg, Clo, Wait };
#define c1(nom,...) static obj nom(lips v, mem e, u64 m, ##__VA_ARGS__)
#define c2(nom,...) static obj nom(lips v, mem e, u64 m, obj x, ##__VA_ARGS__)

// helper functions for lists
static i64 lidx(obj l, obj x) {
 for (i64 i = 0; twop(l); l = Y(l), i++)
  if (x == X(l)) return i;
 return -1; }

static obj linitp(lips v, obj x, mem d) {
 obj y; return !twop(Y(x)) ? (*d = x, nil) :
  (with(x, y = linitp(v, Y(x), d)), pair(v, X(x), y)); }

static obj snoc(lips v, obj l, obj x) {
 return !twop(l) ? pair(v, x, l) :
  (with(l, x = snoc(v, Y(l), x)), pair(v, X(l), x)); }

static u0 pushss(lips v, i64 i, va_list xs) {
 obj x = va_arg(xs, obj);
 x ? (with(x, pushss(v, i, xs)), *--Sp = x) : reqsp(v, i); }

static u0 pushs(lips v, ...) {
 i64 i = 0;
 va_list xs; va_start(xs, v);
 while (va_arg(xs, obj)) i++;
 va_end(xs), va_start(xs, v);
 if (Avail < i) pushss(v, i, xs);
 else for (mem sp = Sp -= i; i--; *sp++ = va_arg(xs, obj));
 va_end(xs); }

// emit code backwards like cons
static obj em1(terp *i, obj k) {
 return k -= W, G(k) = i, k; }
static obj em2(terp *i, obj j, obj k) {
 return em1(i, em1((terp*)j, k)); }

static obj imx(lips v, mem e, i64 m, terp *i, obj x) {
 Push(_N(i), x);
 return insx(v, e, m); }

NoInline obj apply(lips v, obj f, obj x) {
 Push(f, x);
 hom h = cells(v, 5);
 h[0] = call;
 h[1] = (terp*) Pn(2);
 h[2] = yield;
 h[3] = NULL;
 h[4] = (terp*) h;
 return call(v, Ph(h), Fp, Sp, Hp, tblget(v, Top, App)); }

static NoInline obj rwlade(lips v, obj x) {
 mm(&x);
 for (obj y; twop(X(x));
  y = snoc(v, Y(X(x)), X(Y(x))),
  y = pair(v, v->glob[Lamb], y),
  y = pair(v, y, Y(Y(x))),
  x = pair(v, X(X(x)), y));
 return um, x; }

static int scan_def(lips v, mem e, obj x) {
 if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
 if (!twop(Y(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
 mm(&x);
 int r = scan_def(v, e, Y(Y(x)));
 if (r) {
  x = rwlade(v, x);
  obj y = pair(v, X(x), loc(*e));
  loc(*e) = y;
  scan(v, e, X(Y(x))); }
 return um, r; }

static u0 scan(lips v, mem e, obj x) {
 if (!twop(x) || X(x) == La || X(x) == Qt) return;
 if (X(x) == De) return (u0) scan_def(v, e, Y(x));
 for (mm(&x); twop(x); x = Y(x)) scan(v, e, X(x));
 um; }

static obj asign(lips v, obj a, i64 i, mem m) {
 obj x;
 if (!twop(a)) return *m = i, a;
 if (twop(Y(a)) && X(Y(a)) == Va)
  return *m = -(i+1), pair(v, X(a), nil);
 with(a, x = asign(v, Y(a), i+1, m));
 return pair(v, X(a), x); }

static vec tuplr(lips v, i64 i, va_list xs) {
 vec t; obj x;
 return (x = va_arg(xs, obj)) ?
  (with(x, t = tuplr(v, i+1, xs)), t->xs[i] = x, t) :
  ((t = cells(v, Size(tup) + i))->len = i, t); }

static obj tupl(lips v, ...) {
 vec t; va_list xs;
 va_start(xs, v);
 t = tuplr(v, 0, xs);
 va_end(xs);
 return putvec(t); }

static Inline obj scope(lips v, mem e, obj a, obj n) {
 i64 s = 0;
 return with(n, a = asign(v, a, 0, &s)),
        tupl(v, a, nil, nil, e ? *e : nil, n, Pn(s), (obj)0); }

static Inline obj compose(lips v, mem e, obj x) {
 Push(_N(pre_eval_), x, _N(inst), _N(ret), _N(pre_alloc));
 scan(v, e, Sp[1]);
 x = Ccc(v, e, 4); // 4 = 2 + 2
 i64 i = llen(loc(*e));
 if (i) x = em2(locals,  Pn(i), x);
 i = N(asig(*e));
 if (i > 0) x = em2(arity, Pn(i), x);
 else if (i < 0) x = em2(vararg, Pn(-i-1), x);
 x = hfin(v, x);
 return twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expression, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static obj ltu(lips v, mem e, obj n, obj l) {
 obj y;
 l = Y(l);
 with(n,
  l = twop(l) ? l : pair(v, l, nil),
  with(y, l = linitp(v, l, &y),
          with(l, n = pair(v, n, e ? name(*e) : nil)),
          n = scope(v, e, l, n)),
  l = compose(v, &n, X(y)));
 return l; }

c2(c_la) {
 terp* j = imm;
 obj k, nom = *Sp == Pn(pre_bind) ? Sp[1] : nil;
 with(nom, with(x, k = Ccc(v, e, m+2)));
 with(k,
  x = homp(x = ltu(v, e, nom, x)) ? x :
  (j = e && twop(loc(*e)) ? encll : encln,
   c_la_clo(v, e, X(x), Y(x))));
 return em2(j, x, k); }

c2(pre_imm) { return Push(Pn(imm), x), insx(v, e, m); }

static obj c_la_clo(lips v, mem e, obj arg, obj seq) {
 i64 i = llen(arg);
 mm(&arg), mm(&seq);
 for (Push(Pn(insx), Pn(take), Pn(i), Pn(pre_alloc));
      twop(arg);
      Push(Pn(pre_eval_), X(arg), Pn(inst), Pn(push)), arg = Y(arg));
 return arg = Ccc(v, e, 0), um, um, pair(v, seq, arg); }

c1(pre_bind) { obj y = *Sp++; return
 e ? imx(v, e, m, loc_, Pn(lidx(loc(*e), y))) :
     imx(v, e, m, tbind, y); }

static u0 pre_let_r(lips v, mem e, obj x) {
 if (twop(x))
  x = rwlade(v, x),
  with(x, pre_let_r(v, e, Y(Y(x)))),
  Push(Pn(pre_eval_), X(Y(x)), Pn(pre_bind), X(x)); }

// syntactic sugar for define
static obj def_sug(lips v, obj x) {
 obj y = nil; return
  with(y, x = linitp(v, x, &y)),
  x = pair(v, x, y),   x = pair(v, Se, x),
  x = pair(v, x, nil), x = pair(v, La, x),
  pair(v, x, nil); }

c2(pre_let) { return
 !twop(Y(x))    ? pre_imm(v, e, m, nil) :
 llen(Y(x)) % 2 ? pre_eval(v, e, m, def_sug(v, x)) :
                  (pre_let_r(v, e, Y(x)), Ccc(v, e, m)); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 v->xp
#define S2 v->ip

// before generating anything, store the
// exit address in stack 2
c1(c_co_pre) {
 obj x = Ccc(v, e, m);
 return X(S2 = pair(v, x, S2)); }

// before generating a branch emit a jump to
// the top of stack 2
c1(c_co_pre_con) {
 obj x = Ccc(v, e, m+2), k = X(S2);
 return G(k) == ret ? em1(ret, x) : em2(jump, k, x); }

// after generating a branch store its address
// in stack 1
c1(c_co_post_con) {
 obj x = Ccc(v, e, m);
 return X(S1 = pair(v, x, S1)); }

// before generating an antecedent emit a branch to
// the top of stack 1
c1(c_co_pre_ant) {
 obj x = Ccc(v, e, m+2);
 return x = em2(branch, X(S1), x), S1 = Y(S1), x; }

static u0 c_co_r(lips v, mem e, obj x) {
 if (!twop(x)) x = pair(v, nil, nil);
 if (!twop(Y(x))) return Push(Pn(pre_eval_), X(x), Pn(c_co_pre_con));
 with(x,
  Push(_N(c_co_post_con), N_(pre_eval_), X(Y(x)), _N(c_co_pre_con)),
  c_co_r(v, e, Y(Y(x))));
 Push(Pn(pre_eval_), X(x), Pn(c_co_pre_ant)); }

c2(c_co) { return
 with(x, Push(Pn(c_co_pre))),
 c_co_r(v, e, Y(x)),
 x = Ccc(v, e, m),
 S2 = Y(S2),
 x; }

static u0 c_se_r(lips v, mem e, obj x) {
 if (twop(x))
  with(x, c_se_r(v, e, Y(x))),
  Push(Pn(pre_eval_), X(x)); }

c2(c_se) {
 if (!twop(x = Y(x))) x = pair(v, nil, nil);
 return c_se_r(v, e, x), Ccc(v, e, m); }

c1(c_call) {
 obj a = *Sp++, k = Ccc(v, e, m + 2);
 return em2(G(k) == ret ? rec : call, a, k); }

#define L(n,x) pair(v, Pn(n), x)
static obj look(lips v, obj e, obj y) {
 obj q; return
  nilp(e) ?
   ((q = tblget(v, Top, y)) ?  L(Here, q) : L(Wait, Top)) :
  ((q = lidx(loc(e), y)) != -1) ? L(Loc, e) :
  ((q = lidx(arg(e), y)) != -1) ? L(Arg, e) :
  ((q = lidx(clo(e), y)) != -1) ? L(Clo, e) :
  look(v, par(e), y); }
#undef L

c2(pre_var) {
 obj y, q;
 with(x, y = X(q = look(v, e ? *e:nil, x)));
 switch (Gn(y)) {
  case Here: return pre_imm(v, e, m, Y(q));
  case Wait:
   x = pair(v, Y(q), x);
   with(x, y = Ccc(v, e, m+2));
   with(y, x = pair(v, Pn(8), x));
   return em2(lbind, x, y);
  default:
   if (Y(q) == *e) switch (Gn(y)) {
    case Loc: return imx(v, e, m, loc, Pn(lidx(loc(*e), x)));
    case Arg: return imx(v, e, m, arg, Pn(lidx(arg(*e), x)));
    default:  return imx(v, e, m, clo, Pn(lidx(clo(*e), x))); }
   return y = llen(clo(*e)),
          with(x, q = snoc(v, clo(*e), x)),
          clo(*e) = q,
          imx(v, e, m, clo, Pn(y)); } }

c1(pre_eval_) { return pre_eval(v, e, m, *Sp++); }
c2(pre_eval) { return (symp(x) ? pre_var : twop(x) ? pre_two : pre_imm)(v, e, m, x); }

c2(pre_quote) { return pre_imm(v, e, m, twop(x = Y(x)) ? X(x) : x); }
c2(pre_two) { obj z = X(x); return
 (z == Qt ? pre_quote : z == If ? c_co : z == De ? pre_let :
  z == La ? c_la : z == Se ? c_se : pre_apply)(v, e, m, x); }

c2(pre_apply) {
 obj mac = tblget(v, Mac, X(x));
 if (mac) {
  obj s1 = S1, s2 = S2;
  with(s1, with(s2, x = apply(v, mac, Y(x))));
  S1 = s1, S2 = s2;
  return pre_eval(v, e, m, x); }
 for (mm(&x),
      Push(Pn(pre_eval_), X(x), Pn(inst), Pn(idH), Pn(c_call), Pn(llen(Y(x))));
      twop(x = Y(x));
      Push(Pn(pre_eval_), X(x), Pn(inst), Pn(push)));
 return um, Ccc(v, e, m); }

c1(inst) {
 terp* i = (terp*) Gn(*Sp++);
 return em1(i, Ccc(v, e, m+1)); }

c1(insx) {
 terp* i = (terp*) Gn(*Sp++);
 obj x = *Sp++, k;
 return with(x, k = Ccc(v, e, m+2)), em2(i, x, k); }

c1(pre_alloc) {
 obj k = hini(v, m+1);
 return em1((terp*)(e ? name(*e) : Eva), k); }

static NoInline obj hini(lips v, u64 n) {
 hom a = cells(v, n + 2);
 return G(a+n) = NULL,
        GF(a+n) = (terp*) a,
        set64((mem) a, nil, n),
        Ph(a+n); }

static obj hfin(lips v, obj a) {
 return (obj) (GF(button(Gh(a))) = (terp*) a); }

NoInline u0 defprim(lips v, const char *a, terp *b) {
 obj z = pair(v, interns(v, a), nil);
 if (!Avail) with(z, reqsp(v, 1));
 *--Sp = z;
 obj x = hini(v, 2);
 x = em2(b, z = *Sp++, x);
 tblset(v, Top, X(z), x); }

obj compile(lips v, obj x) { return
 Push(Pn(pre_eval_), x, Pn(inst), Pn(yield), Pn(pre_alloc)),
 Ccc(v, NULL, 0); }