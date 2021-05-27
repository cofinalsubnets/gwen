#include "lips.h"
#include "terp.h"
// subs for some libc functions: memset, memcpy, strlen
u0 fill(_*_d, O i, N l) { // fill with a word
 for (M d = _d; l--; *d++=i); }
u0 wcpy(_*_d, Ko _*_s, N l) { // copy words
 mem d = _d; Ko O *s = _s;
 while (l--) *d++=*s++; }
void bcpy(void*_d, const void*_s, u64 l) { // copy bytes
 char *d = _d; const char *s = _s;
 while (l--) *d++=*s++; }

////
/// bootstrap thread compiler
//
// functions construct their continuations by pushing function
// pointers onto the main stack with Pu()
#define Pu(...) pushs(v,__VA_ARGS__,non)
#define Push(...) Pu(__VA_ARGS__)
// and then calling them with ccc
#define Cc ((c1*)Gn(*Sp++))
#define ccc Cc
// there's a natural correspondence between the Pu/Cc pattern
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
#define toplp(x) !e
#define arg(x)  AR(x)[0] // argument variables : a list
#define loc(x)  AR(x)[1] // local variables : a list
#define clo(x)  AR(x)[2] // closure variables : a list
#define par(x)  AR(x)[3] // surrounding scope : tuple or nil
#define name(x) AR(x)[4] // function name : a symbol or nil
#define asig(x) AR(x)[5] // arity signature : an integer
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.
//

typedef obj
 c1(lips, mem, i64),
 c2(lips, mem, i64, obj),
 c3(lips, mem, i64, obj, obj);
static u0
 c_de_r(lips, mem, obj),
 scan(lips, mem, obj),
 pushs(lips, ...);
static c1 c_ev, c_d_bind, inst, insx, c_ini;
static c2 c_eval, c_sy, c_2, c_imm, ltu, c_ap, c_la_clo;
static c3 late;
static obj
 look(lips, obj, obj),
 hfin(lips, obj),
 hini(lips, i64);
#define interns(v,c) intern(v,string(v,c))

En { Here, Loc, Arg, Clo, Wait };
#define c1(nom,...) static O nom(V v,M e,Z m,##__VA_ARGS__)
#define c2(nom,...) static O nom(V v,M e,Z m,O x,##__VA_ARGS__)

// emit code backwards like cons
static Inline obj em1(terp *i, obj k) {
  return k -= W, G(k) = i, k; }
static Inline obj em2(terp *i, obj j, obj k) {
  return em1(i, em1((T)j, k)); }

static obj imx(lips v, mem e, i64 m, terp *i, obj x) {
 return Pu(Pn(i), x), insx(v, e, m); }

static obj apply(lips v, obj f, obj x) {
  Pu(f, x);
  hom h = cells(v, 5);
  h[0] = call;
  h[1] = (T) Pn(2);
  h[2] = yield;
  h[3] = NULL;
  h[4] = (T) h;
  return call(v, Ph(h), Fp, Sp, Hp, tblget(v, Dict, App)); }

obj compile(lips v, obj x) {
 Pu(Pn(c_ev), x, Pn(inst), Pn(yield), Pn(c_ini));
 return ccc(v, NULL, 0); }

obj eval(lips v, obj x) {
 x = pair(v, x, nil);
 return apply(v, tblget(v, Dict, Eva), x); }

static obj rwlade(lips v, obj x) {
 obj y;
 mm(&x);
 while (twop(X(x)))
  y = snoc(v, YX(x), XY(x)),
  y = pair(v, La, y),
  y = pair(v, y, YY(x)),
  x = pair(v, XX(x), y) ;
 return um, x; }

static int scan_def(lips v, mem e, obj x) {
 if (!twop(x)) R 1; // this is an even case so export all the definitions to the local scope
 if (!twop(Y(x))) R 0; // this is an odd case so ignore these, they'll be imported after the rewrite
 mm(&x);
 int r = scan_def(v, e, YY(x));
 if (r) {
  x = rwlade(v, x);
  obj y = pair(v, X(x), loc(*e));
  loc(*e) = y;
  scan(v, e, XY(x)); }
 R um, r; }

St _ scan(V v, M e, O x) {
 if (!twop(x) || X(x) == La || X(x) == Qt) R;
 if (X(x) == De) R (void) scan_def(v, e, Y(x));
 Fo (mm(&x); twop(x); x = Y(x)) scan(v, e, X(x));
 um; }

St O asign(V v, O a, Z i, M m) { O x;
 if (!twop(a)) R *m = i, a;
 if (twop(Y(a)) && XY(a) == Va)
  R *m = -(i+1), pair(v, X(a), nil);
 Mm(a, x = asign(v, Y(a), i+1, m));
 R pair(v, X(a), x); }

St Ve tuplr(V v, Z i, va_list xs) { Ve t; O x;
 R (x = va_arg(xs, O)) ?
  (Mm(x, t = tuplr(v, i+1, xs)), t->xs[i] = x, t) :
  ((t = cells(v, Size(tup) + i))->len = i, t); }

St O tupl(V v, ...) { Ve t; va_list xs;
 R va_start(xs, v),
  t = tuplr(v, 0, xs),
  va_end(xs),
  puttup(t); }

St O scope(V v, M e, O a, O n) { Z s = 0;
  R Mm(n, a = asign(v, a, 0, &s)),
    tupl(v, a, nil, nil, e ? *e : nil, n, Pn(s), non); }

St O compose(V v, M e, O x) {
  Pu(Pn(c_ev), x, Pn(inst), Pn(ret), Pn(c_ini));
  scan(v, e, Sp[1]);
  x = ccc(v, e, 4); // 4 = 2 + 2
  Z i = llen(loc(*e));
  if (i) x = em2(locals,  Pn(i), x);
  i = Gn(asig(*e));
     if (i > 0) x = em2(arity, Pn(i), x);
  El if (i < 0) x = em2(vararg, Pn(-i-1), x);
  x = hfin(v, x);
  R twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expression, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
St O ltu(V v, M e, O n, O l) { O y;
  l = Y(l);
  Mm(n,
    l = twop(l) ? l : pair(v, l, nil),
    Mm(y, l = linitp(v, l, &y),
          Mm(l, n = pair(v, n, toplp(e) ? nil : e ? name(*e):nil)),
          n = scope(v, e, l, n)),
    l = compose(v, &n, X(y)));
  R l; }


c2(c_la) {
  T j = imm;
  O k, nom = *Sp == Pn(c_d_bind) ? Sp[1] : nil;
  Mm(nom, Mm(x, k = ccc(v, e, m+2)));
  Mm(k,
    x = homp(x = ltu(v, e, nom, x)) ? x :
    (j = toplp(e) || !twop(loc(*e)) ? encln : encll,
     c_la_clo(v, e, X(x), Y(x))));
  R em2(j, x, k); }

c2(c_imm) { R Pu(Pn(imm), x), insx(v, e, m); }

St O c_la_clo(V v, M e, O arg, O seq) {
  Z i = llen(arg);
  mm(&arg), mm(&seq);
  Fo (Pu(Pn(insx), Pn(take), Pn(i), Pn(c_ini));
      twop(arg);
      Pu(Pn(c_ev), X(arg), Pn(inst), Pn(push)), arg = Y(arg));
  R arg = ccc(v, e, 0), um, um,
    pair(v, seq, arg); }

c1(c_d_bind) {
 O y = *Sp++;
 R toplp(e) ? imx(v, e, m, tbind, y) :
              imx(v, e, m, loc_, Pn(idx(loc(*e), y))); }

static u0 c_de_r(V v, M e, O x) {
 if (!twop(x)) R;
 x = rwlade(v, x);
 Mm(x, c_de_r(v, e, YY(x))),
 Pu(Pn(c_ev), XY(x), Pn(c_d_bind), X(x)); }

// syntactic sugar for define
static obj def_sug(lips v, obj x) {
 obj y = nil;
 Mm(y, x = linitp(v, x, &y));
 x = pair(v, x, y),   x = pair(v, Se, x);
 x = pair(v, x, nil), x = pair(v, La, x);
 return pair(v, x, nil); }

c2(c_de) { return
 !twop(Y(x))    ? c_imm(v, e, m, nil) :
 llen(Y(x)) % 2 ? c_eval(v, e, m, def_sug(v, x)) :
                  (c_de_r(v, e, Y(x)), ccc(v, e, m)); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 Xp
#define S2 Ip

// before generating anything, store the
// exit address in stack 2
c1(c_co_pre) {
 obj x = ccc(v, e, m);
 return X(S2 = pair(v, x, S2)); }

// before generating a branch emit a jump to
// the top of stack 2
c1(c_co_pre_con) {
 obj x = ccc(v, e, m+2), k = X(S2);
 return G(k) == ret ? em1(ret, x) : em2(jump, k, x); }

// after generating a branch store its address
// in stack 1
c1(c_co_post_con) {
 obj x = ccc(v, e, m);
 return X(S1 = pair(v, x, S1)); }

// before generating an antecedent emit a branch to
// the top of stack 1
c1(c_co_pre_ant) {
 obj x = ccc(v, e, m+2);
 return x = em2(branch, X(S1), x), S1 = Y(S1), x; }

static u0 c_co_r(lips v, mem e, obj x) {
 if (!twop(x)) x = pair(v, nil, nil);
 if (!twop(Y(x))) Pu(Pn(c_ev), X(x), Pn(c_co_pre_con));
 else Mm(x, Pu(Pn(c_co_post_con), Pn(c_ev), XY(x),
               Pn(c_co_pre_con)),
            c_co_r(v, e, YY(x))),
      Pu(Pn(c_ev), X(x), Pn(c_co_pre_ant)); }

c2(c_co) { return
 Mm(x, Pu(Pn(c_co_pre))),
 c_co_r(v, e, Y(x)),
 x = ccc(v, e, m),
 S2 = Y(S2),
 x; }

static u0 c_se_r(lips v, mem e, obj x) {
 if (twop(x)) Mm(x, c_se_r(v, e, Y(x))),
              Pu(Pn(c_ev), X(x)); }
c2(c_se) {
 if (!twop(x = Y(x))) x = pair(v, nil, nil);
 return c_se_r(v, e, x), ccc(v, e, m); }

c1(c_call) {
 obj a = *Sp++, k = ccc(v, e, m + 2);
 return em2(G(k) == ret ? rec : call, a, k); }

#define L(n,x) pair(v, Pn(n), x)
static obj look(lips v, obj e, obj y) { obj q; return
 nilp(e) ?
  ((q = tblget(v, Dict, y)) ?  L(Here, q) : L(Wait, Dict)) :
 ((q = idx(loc(e), y)) != -1) ?
  L(Loc, e) :
 ((q = idx(arg(e), y)) != -1) ?
  L(Arg, e) :
 ((q = idx(clo(e), y)) != -1) ?
  L(Clo, e) :
 look(v, par(e), y); }
#undef L

c2(late, obj d) { obj k; return
 x = pair(v, d, x),
 Mm(x, k = ccc(v, e, m+2)),
 Mm(k, x = pair(v, Pn(8), x)),
 em2(lbind, x, k); }

c2(c_sy) { O y, q;
 Mm(x, y = X(q = look(v, e ? *e:nil, x)));
 Sw (Gn(y)) {
  case Here: return c_imm(v, e, m, Y(q));
  case Wait: return late(v, e, m, x, Y(q));
  default:
   if (Y(q) == *e) switch (Gn(y)) {
    case Loc: return imx(v, e, m, loc, Pn(idx(loc(*e), x)));
    case Arg: return imx(v, e, m, arg, Pn(idx(arg(*e), x)));
    case Clo: return imx(v, e, m, clo, Pn(idx(clo(*e), x))); }
   y = llen(clo(*e));
   Mm(x, q = snoc(v, clo(*e), x)), clo(*e) = q;
   return imx(v, e, m, clo, Pn(y)); } }

c1(c_ev) { return c_eval(v, e, m, *Sp++); }
c2(c_eval) {
 return (symp(x)?c_sy:twop(x)?c_2:c_imm)(v,e,m,x); }

c2(c_qt) { return c_imm(v, e, m, twop(x = Y(x)) ? X(x) : x); }
c2(c_2) { obj z = X(x); return 
 (z==Qt?c_qt:
  z==If?c_co:
  z==De?c_de:
  z==La?c_la:
  z==Se?c_se:c_ap)(v,e,m,x); }

#define Rec(...) {\
  O _s1 = S1, _s2 = S2;\
  Mm(_s1, Mm(_s2,__VA_ARGS__));\
  S1 = _s1, S2 = _s2; }

c2(c_ap) {
 obj y = tblget(v, Mac, X(x));
 if (y) {
  Rec(x = apply(v, y, Y(x)));
  return c_eval(v, e, m, x); }
 for (mm(&x),
      Pu(Pn(c_ev), X(x), Pn(inst), Pn(idH),
         Pn(c_call), Pn(llen(Y(x))));
      twop(x = Y(x));
      Pu(Pn(c_ev), X(x), Pn(inst), Pn(push)));
 return um, ccc(v, e, m); }

c1(inst) {
 terp* i = (terp*) Gn(*Sp++);
 return em1(i, ccc(v, e, m+1)); }

c1(insx) {
 terp* i = (terp*) Gn(*Sp++);
 obj x = *Sp++, k;
 Mm(x, k = ccc(v, e, m+2));
 return em2(i, x, k); }

c1(c_ini) {
 obj k = hini(v, m+1);
 return em1((T)(e ? name(*e):Eva), k); }

static u0 pushss(lips v, i64 i, va_list xs) {
 obj x; (x = va_arg(xs, obj)) ?
  (Mm(x, pushss(v, i, xs)), *--Sp = x) :
  reqsp(v, i); }

static u0 pushs(V v, ...) {
 i64 i = 0;
 va_list xs; va_start(xs, v);
 while (va_arg(xs, O)) i++;
 va_end(xs), va_start(xs, v);
 if (Avail < i) pushss(v, i, xs);
 else for (mem sp = Sp -= i; i--; *sp++ = va_arg(xs, obj));
 va_end(xs); }

obj hini(V v, Z n) {
 hom a = cells(v, n + 2);
 return
  G(a+n) = NULL,
  GF(a+n) = (T) a,
  fill((M) a, nil, n),
  Ph(a+n); }

static obj hfin(lips v, obj a) {
 return (obj) (GF(button(Gh(a))) = (terp*) a); }

obj homnom(lips v, obj x) {
 terp *k = G(x);
 if (k == clos || k == pc0 || k == pc1)
  return homnom(v, (O) G(FF(x)));
 mem h = (M) Gh(x);
 while (*h) h++;
 x = h[-1];
 return (mem) x >= Pool && (M) x < Pool+Len ? x :
        x == Ob yield                       ? Eva :
                                              nil; }

static u0 rpr(lips v, mem d, const char *n, terp *u) {
 obj x, y = pair(v, interns(v, n), nil);
 Mm(y, x = hini(v, 2));
 x = em2(u, y, x);
 tblset(v, *d, X(y), x); }

static u0 rin(lips v, mem d, const char *n, terp *u) {
 obj y = interns(v, n);
 tblset(v, *d, y, Pn(u)); }

#define RPR(a,b) rpr(v,&d,a,b)
#define RIN(x) rin(v,&d,"i-"#x,x)
static Inline obj code_dictionary(V v) {
 obj d = table(v);;
 Mm(d, prims(RPR), insts(RIN));
 return d; }
#undef RPR
#undef RIN

static Inline u0 init_globals_array(lips v) {
 vec t = cells(v, sizeof (struct tup)/W + NGlobs);
 fill(t->xs, nil, t->len = NGlobs);
 obj z, y = Glob = puttup(t);
 Mm(y,
  z = code_dictionary(v), Top = z,
  z = table(v), Mac = z,
#define bsym(i,s)(z=interns(v,s),AR(y)[i]=z)
  bsym(Eval, "ev"), bsym(Apply, "ap"),
  bsym(Def, ":"),   bsym(Cond, "?"), bsym(Lamb, "\\"),
  bsym(Quote, "`"), bsym(Seq, ","),  bsym(Splat, ".")); }
#undef bsym

#define NOM "lips"
#define USR_PATH ".local/lib/"NOM"/"
#define SYS_PATH "/usr/lib/"NOM"/"
static int seekp(Ko Ch* p) {
 int b, c;
 b = open(getenv("HOME"), O_RDONLY);
 c = openat(b, USR_PATH, O_RDONLY), close(b);
 b = openat(c, p, O_RDONLY), close(c);
 if (-1 < b) return b;
 b = open(SYS_PATH, O_RDONLY);
 c = openat(b, p, O_RDONLY), close(b);
 return c; }


lips bootstrap(lips v) {
 if (v == NULL) R v;
 const char *path = "prelude.lips";
 int pre = seekp(path);
 if (pre == -1) errp(v, "can't find %s", path);
 else {
  FILE *f = fdopen(pre, "r");
  if (setjmp(v->restart)) return
   errp(v, "error in %s", path),
   fclose(f), finalize(v);
  script(v, f), fclose(f); }
 return v; }

lips initialize(int argc, const char **argv) {
 lips v = malloc(sizeof(struct lips));
 if (!v || setjmp(v->restart))
  return errp(v, "oom"), finalize(v);
 v->t0 = clock(),
 v->ip = v->xp = v->syms = v->glob = nil,
 v->fp = v->hp = v->sp = (M) w2b(1),
 v->count = 0, v->mem_len = 1, v->mem_pool = NULL,
 v->mem_root = NULL;
 init_globals_array(v);
 obj y = interns(v, "ns");
 tblset(v, Top, y, Top);
 y = interns(v, "macros");
 tblset(v, Top, y, Mac);
 y = interns(v, "argv");
 obj a = nil;
 mm(&y); mm(&a);
 for (obj z; argc--;)
  z = string(v, argv[argc]),
  a = pair(v, z, a);
 um, um, tblset(v, Top, y, a);
 srand(clock());
 return v; }

lips finalize(lips v) {
 if (v) free(v->mem_pool), free(v);
 return NULL; }

#undef arg
#undef loc
#undef clo

