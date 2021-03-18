#include "lips.h"

typedef obj c1(vm, mem, num),
            c2(vm, mem, num, obj),
            c3(vm, mem, num, obj, obj);
#define StackHeight (Pool+Len-Sp)
////
/// bootstrap thread compiler
//
// functions construct their continuations by pushing function
// pointers onto the main stack with Push()
#define Push(...) pushs(v,__VA_ARGS__,non)
// and then calling them with ccc
#define ccc ((c1*)Gn(*Sp++))
// there's a natural correspondence between the Push(...),
// ccc(...) pattern in this file and normal continuation
// passing style in lisp (cf. the stage 2 compiler).

// in addition to the main stack, the compiler uses Xp and Ip
// as stacks for storing code entry points when generating
// conditionals, which is admittedly kind of sus.
//
// this compiler emits runtime type checks for safety but no
// optimizations or static typing. it only has to compile the
// lips compiler once, and that version only has to compile
// itself once, so it seems better to keep the C code minimal.

// " compilation environments "
// the current lexical environment is passed to compiler
// functions as a pointer to an object, either a tuple with a
// structure specified below, or nil for toplevel. it's a
// pointer to an object, instead of just an object, so it can
// be gc-protected once instead of separately by every function.
// in the other compiler it's just a regular object.
#define toplp(x) nilp(*x)
#define arg(x)  AR(x)[0] // argument variables : a list
#define loc(x)  AR(x)[1] // local variables : a list
#define clo(x)  AR(x)[2] // closure variables : a list
#define par(x)  AR(x)[3] // surrounding scope : tuple or nil
#define name(x) AR(x)[4] // function name : a symbol or nil
#define asig(x) AR(x)[5] // arity signature : an integer
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.
static void c_de_r(vm, mem, obj),
            scan(vm, mem, obj), pushs(vm, ...);
static c1  c_ev, c_d_bind, inst, insx, c_ini;
static c2 c_eval, c_sy, c_2, c_imm, ltu, c_ap, c_la_clo;
static c3 late;
static obj tupl(vm, ...),
           def_sug(vm, obj), snoc(vm, obj, obj),
           look(vm, obj, obj),
           linitp(vm, obj, mem), hom_fin(vm, obj),
           imx(vm, mem, num, terp*, obj),
           hom_ini(vm, num);
static num idx(obj, obj);

#define N(x) putnum(x)
#define Gn(x) getnum(x)
enum location { Here, Loc, Arg, Clo, Wait };
#define c1(nom,...) static obj nom(vm v,mem e,num m,##__VA_ARGS__)
#define c2(nom,...) static obj nom(vm v,mem e,num m,obj x,##__VA_ARGS__)

// this is such a genius idea, i stole it from luajit
#define insts(_)\
  _(arity),  _(tcnum),  _(tchom),   _(tctwo),  _(lbind),\
  _(immv),   _(argn),   _(clon),    _(locn),   _(take),\
  _(prel),   _(setl),   _(pc0),     _(pc1),    _(clos),\
  _(encll),  _(encln),  _(yield),   _(ret),    _(jump),\
  _(branch), _(barnch), _(call),    _(rec),  _(loop),\
  _(tbind),  _(push),   _(add),     _(sub),    _(mul),\
  _(dqv),    _(mod),    _(neg),     _(lt),     _(lteq),\
  _(eq),     _(gteq),   _(gt),      _(twopp),  _(numpp),\
  _(nilpp),  _(strpp),  _(tblpp),   _(sympp),  _(hompp),\
  _(car),    _(cdr),    _(setcar),  _(setcdr), _(cons),\
  _(add_u),  _(sub_u),  _(mul_u),   _(div_u),  _(mod_u),\
  _(lt_u),   _(lteq_u), _(eq_u),    _(gteq_u), _(gt_u),\
  _(twop_u), _(nump_u), _(homp_u),  _(tblp_u), _(strp_u),\
  _(nilp_u), _(car_u),  _(cdr_u),   _(cons_u),\
  _(strmk),  _(strg),   _(strl),_(strs),_(strc),_(hom_fin_u),\
  _(setcar_u), _(setcdr_u),\
  _(symp_u), _(emse), _(hom_u), _(pc_u),\
  _(or_u), _(and_u), _(zzz),\
  _(tbll), _(tblmk),_(tblg),_(tblc),_(tbls),_(tbld),_(tblks),\
  _(hom_seek_u),_(hom_geti_u),_(emi),\
  _(fail),_(ccc_u),_(cont),_(vararg),_(tuck),\
  _(rd_u),\
  _(drop),_(hom_getx_u),_(emx),_(em_u),_(ev_u),_(ap_u)
#define ninl(x) x NoInline
static terp insts(ninl);
#undef ninl
#define Rec(...) {\
  obj _s1 = S1, _s2 = S2;\
  with(_s1, with(_s2,__VA_ARGS__));\
  S1 = _s1, S2 = _s2; }

// emit code backwards like cons
static Inline obj em1(terp *i, obj k) {
  return k -= Word, G(k) = i, k; }
static Inline obj em2(terp *i, obj j, obj k) {
  return em1(i, em1((terp*)j, k)); }

static obj compile(vm v, obj x) {
  static obj top = nil;
  Push(N(c_ev), x, N(inst), N(yield), N(c_ini));
  return ccc(v, &top, 0); }

static obj apply(vm v, obj f, obj x) {
  Push(f, x);
  hom h = cells(v, 5);
  h[0].g = call;
  h[1].g = (terp*) N(2);
  h[2].g = yield;
  h[3].g = NULL;
  h[4].g = (terp*) h;
  return call(v, h, Fp, Sp, Hp, tbl_get(v, Dict, App)); }

/// evaluate an expression
obj eval(vm v, obj x) {
  x = pair(v, x, nil);
  return apply(v, tbl_get(v, Dict, Eva), x); }

static void scan_def_add(vm v, mem e, obj y, obj x) {
  with(x, y = pair(v, y, loc(*e)), loc(*e) = y);
  scan(v, e, x); }

static int scan_def(vm v, mem e, obj x) {
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(Y(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  obj r; with(x, r = scan_def(v, e, YY(x)));
  if (r) scan_def_add(v, e, X(x), XY(x));
  return r; }

static void scan(vm v, mem e, obj x) {
  if (!twop(x) || X(x) == La || X(x) == Qt) return;
  if (X(x) == De) return (void) scan_def(v, e, Y(x));
  for (mm(&x); twop(x); x = Y(x)) scan(v, e, X(x));
  um; }

static obj asign(vm v, obj a, num i, mem m) {
  if (!twop(a)) return *m = i, a;
  if (twop(Y(a)) && XY(a) == Va)
    return *m = -(i+1), pair(v, X(a), nil);
  obj x;
  with(a, x = asign(v, Y(a), i+1, m));
  return pair(v, X(a), x); }

static obj scope(vm v, mem e, obj a, obj n) {
  num s = 0;
  with(n, a = asign(v, a, 0, &s));
  return tupl(v, a, nil, nil, *e, n, N(s), non); }

static obj compose(vm v, mem e, obj x) {
  Push(N(c_ev), x, N(inst), N(ret), N(c_ini));
  scan(v, e, Sp[1]);
  obj i; x = ccc(v, e, 4); // 4 = 2 + 2
  if ((i = llen(loc(*e)))) x = em2(prel,  N(i), x);
  i = Gn(asig(*e));
  if (i > 0) x = em2(arity, N(i), x);
  else if (i < 0) x = em2(vararg, N(-i-1), x);
  x = hom_fin(v, x);
  return twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expression, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static obj ltu(vm v, mem e, obj n, obj l) {
  obj y;
  l = Y(l);
  with(n,
    l = twop(l) ? l : pair(v, l, nil),
    with(y, l = linitp(v, l, &y),
            with(l, n = pair(v, n, toplp(e) ? nil : name(*e))),
            n = scope(v, e, l, n)),
    l = compose(v, &n, X(y)));
  return l; }

c1(c_ev) { return c_eval(v, e, m, *Sp++); }
c2(c_eval) { return (symp(x) ? c_sy : twop(x) ? c_2 : c_imm)(v, e, m, x); }

c2(c_la) {
  terp *j = immv;
  obj k, nom = *Sp == N(c_d_bind) ? Sp[1] : nil;
  with(nom, with(x, k = ccc(v, e, m+2)));
  with(k,
    x = homp(x = ltu(v, e, nom, x)) ? x :
    (j = toplp(e) || !twop(loc(*e)) ? encln : encll,
     c_la_clo(v, e, X(x), Y(x))));
  return em2(j, x, k); }

c2(c_imm) { return Push(N(immv), x), insx(v, e, m); }

static obj c_la_clo(vm v, mem e, obj arg, obj seq) {
  num i = llen(arg);
  mm(&arg), mm(&seq);
  for (Push(N(insx), N(take), N(i), N(c_ini));
       twop(arg);
       Push(N(c_ev), X(arg), N(inst), N(push)), arg = Y(arg));
  return arg = ccc(v, e, 0), um, um, pair(v, seq, arg); }

c1(c_d_bind) {
  obj y = *Sp++;
  return toplp(e) ? imx(v, e, m, tbind, y) :
                    imx(v, e, m, setl, N(idx(loc(*e), y))); }

static void c_de_r(vm v, mem e, obj x) {
  if (twop(x))
    with(x, c_de_r(v, e, YY(x))),
    Push(N(c_ev), XY(x), N(c_d_bind), X(x)); }

c2(c_de) {
  return !twop(Y(x)) ? c_imm(v, e, m, nil) :
         llen(Y(x)) % 2 ?  c_eval(v, e, m, def_sug(v, x)) :
         (c_de_r(v, e, Y(x)), ccc(v, e, m)); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 Xp
#define S2 Ip
//
// before generating anything, store the
// exit address in stack 2
c1(c_co_pre) {
  obj x = ccc(v, e, m);
  x = pair(v, x, S2);
  return X(S2 = x); }

// before generating a branch emit a jump to
// the top of stack 2
c1(c_co_pre_con) {
  obj x = ccc(v, e, m+2), k = X(S2);
  terp *i = G(k);
  return
    i == ret ? em1(i, x) :
    em2(jump, i == jump ? (obj) GF(k) : k, x); }

// after generating a branch store its address
// in stack 1
c1(c_co_post_con) {
  obj x = ccc(v, e, m);
  x = pair(v, x, S1);
  return X(S1 = x); }

// before generating an antecedent emit a branch to
// the top of stack 1
c1(c_co_pre_ant) {
  obj x = ccc(v, e, m+2);
  return x = em2(branch, X(S1), x), S1 = Y(S1), x; }

static void c_co_r(vm v, mem e, obj x) {
  if (!twop(x)) x = pair(v, nil, nil);
  if (!twop(Y(x)))
    Push(N(c_ev), X(x), N(c_co_pre_con));
  else
    with(x,
      Push(N(c_co_post_con), N(c_ev), XY(x), N(c_co_pre_con)),
      c_co_r(v, e, YY(x))),
    Push(N(c_ev), X(x), N(c_co_pre_ant)); }

c2(c_co) {
  return with(x, Push(N(c_co_pre))),
         c_co_r(v, e, Y(x)),
         x = ccc(v, e, m), S2 = Y(S2), x; }

static void c_se_r(vm v, mem e, obj x) {
  if (twop(x)) with(x, c_se_r(v, e, Y(x))),
               Push(N(c_ev), X(x)); }
c2(c_se) {
  if (!twop(x = Y(x))) x = pair(v, nil, nil);
  return c_se_r(v, e, x), ccc(v, e, m); }

c1(c_call) {
  obj a = *Sp++, k = ccc(v, e, m+2);
  return G(k) != ret ? em2(call, a, k) :
         Gn(a) == llen(arg(*e)) ? em1(loop, k) :
         em2(rec, a, k); }

#define L(n,x) pair(v, N(n), x)
static obj look(vm v, obj e, obj y) {
  obj q;
  if (nilp(e)) return (q = tbl_get(v, Dict, y)) ?
    L(Here, q) : L(Wait, Dict);
  if ((q = idx(loc(e), y)) != -1) return L(Loc, e);
  if ((q = idx(arg(e), y)) != -1) return L(Arg, e);
  if ((q = idx(clo(e), y)) != -1) return L(Clo, e);
  return look(v, par(e), y); }
#undef L

static obj imx(vm v, mem e, num m, terp *i, obj x) {
  return Push(N(i), x), insx(v, e, m); }

c2(late, obj d) {
  obj k;
  x = pair(v, d, x);
  with(x, k = ccc(v, e, m+2));
  with(k, x = pair(v, N(8), x));
  return em2(lbind, x, k); }

c2(c_sy) {
  obj y, q;
  with(x, y = X(q = look(v, *e, x)));
  switch (Gn(y)) {
    case Here: return c_imm(v, e, m, Y(q));
    case Wait: return late(v, e, m, x, Y(q));
    default:
      if (Y(q) == *e) switch (Gn(y)) {
        case Loc : return imx(v, e, m, locn, N(idx(loc(*e), x)));
        case Arg : return imx(v, e, m, argn, N(idx(arg(*e), x)));
        case Clo : return imx(v, e, m, clon, N(idx(clo(*e), x))); }
      y = llen(clo(*e));
      with(x, q = snoc(v, clo(*e), x)), clo(*e) = q;
      return imx(v, e, m, clon, N(y)); } }

c2(c_qt) { return c_imm(v, e, m, twop(x = Y(x)) ? X(x) : x); }

c2(c_2) {
  obj z = X(x);
  return (z == Qt ? c_qt :
          z == If ? c_co :
          z == De ? c_de :
          z == La ? c_la :
          z == Se ? c_se :
                    c_ap)(v, e, m, x); }

c2(c_ap) {
  obj y = tbl_get(v, Mac, X(x));
  if (y) {
    Rec(x = apply(v, y, Y(x)));
    return c_eval(v, e, m, x); }
  for (mm(&x),
       Push(N(c_ev), X(x), N(inst), N(tchom),
            N(c_call), N(llen(Y(x))));
       twop(x = Y(x));
       Push(N(c_ev), X(x), N(inst), N(push)));
  return um, ccc(v, e, m); }

c1(inst) {
  terp *i = (terp*) Gn(*Sp++);
  return em1(i, ccc(v, e, m+1)); }

c1(insx) {
  terp *i = (terp*) Gn(*Sp++);
  obj x = *Sp++, k;
  with(x, k = ccc(v, e, m+2));
  return em2(i, x, k); }

c1(c_ini) {
  obj k = hom_ini(v, m+1);
  if (!toplp(e)) k = em1((terp*)name(*e), k);
  return k; }

static obj snoc(vm v, obj l, obj x) {
  if (!twop(l)) return pair(v, x, l);
  with(l, x = snoc(v, Y(l), x));
  return pair(v, X(l), x); }

static obj linitp(vm v, obj x, mem d) {
  if (!twop(Y(x))) return *d = x, nil;
  obj y; with(x, y = linitp(v, Y(x), d));
  return pair(v, X(x), y); }

// syntactic sugar for define
static obj def_sug(vm v, obj x) {
  obj y = nil;
  with(y, x = linitp(v, x, &y));
  x = pair(v, x, y),   x = pair(v, Se, x);
  x = pair(v, x, nil), x = pair(v, La, x);
  return pair(v, x, nil); }

// list functions
static num idx(obj l, obj x) {
  for (num i = 0; twop(l); l = Y(l), i++)
    if (x == X(l)) return i;
  return -1; }

num llen(obj l) {
  for (num i = 0;; l = Y(l), i++) if (!twop(l)) return i; }

static tup tuplr(vm v, num i, va_list xs) {
  tup t; obj x = va_arg(xs, obj);
  return x ?
    (with(x, t = tuplr(v, i+1, xs)), t->xs[i] = x, t) :
    ((t = cells(v, Size(tup) + i))->len = i, t); }

static obj tupl(vm v, ...) {
  tup t; va_list xs;
  return va_start(xs, v), t = tuplr(v, 0, xs), va_end(xs), puttup(t); }

static void pushss(vm v, num i, va_list xs) {
  obj x = va_arg(xs, obj);
  if (x) with(x, pushss(v, i, xs)), *--Sp = x;
  else reqsp(v, i); }

static void pushs(vm v, ...) {
  num i = 0;
  va_list xs; va_start(xs, v);
  while (va_arg(xs, obj)) i++;
  va_end(xs); va_start(xs, v);
  if (Avail < i) pushss(v, i, xs);
  else for (mem sp = Sp -= i; i--; *sp++ = va_arg(xs, obj));
  va_end(xs); }

obj hom_ini(vm v, num n) {
  hom a = cells(v, n + 2);
  return G(a+n) = NULL,
         GF(a+n) = (terp*) a,
         memset(a, -1, w2b(n)),
         puthom(a+n); }

static obj hom_fin(vm v, obj a) {
  for (hom b = gethom(a);;) if (!b++->g)
    return (obj) (b->g = (terp*) a); }

obj homnom(vm v, obj x) {
  terp *k = G(x);
  if (k == clos || k == pc0 || k == pc1)
    return homnom(v, (obj) G(FF(x)));
  mem h = (mem) gethom(x);
  while (*h) h++;
  x = h[-1];
  return (mem)x >= Pool && (mem)x < Pool+Len ? x :
    x == (obj)yield ? Eva :
    nil; }

static void rpr(vm v, mem d, const char *n, terp *u) {
  obj x, y = pair(v, interns(v, n), nil);
  with(y, x = hom_ini(v, 2));
  x = em2(u, y, x);
  tbl_set(v, *d, X(y), x); }
static void rin(vm v, mem d, const char *n, terp *u) {
  obj y = interns(v, n);
  tbl_set(v, *d, y, putnum(u)); }

#define prims(_)\
  _("read", rd_u),\
  _(".", em_u),\
  _("*:", car_u),     _(":*", cdr_u),\
  _("*!", setcar_u), _("!*", setcdr_u),\
  _("::", cons_u),   _("=", eq_u),\
  _("<", lt_u),        _("<=", lteq_u),\
  _(">", gt_u),        _(">=", gteq_u),\
  _("+", add_u),      _("-", sub_u),\
  _("*", mul_u),      _("/", div_u),\
  _("%", mod_u),      _("ap", ap_u),\
  _("ccc", ccc_u),     _("ev", ev_u),\
  _("||", or_u),       _("&&", and_u),\
  _("fail", fail), _("tbl", tblmk),\
  _("tget", tblg),  _("tset", tbls),\
  _("thas", tblc),  _("tdel", tbld),\
  _("tkeys", tblks),_("tlen", tbll),\
  _("slen", strl),  _("sget", strg),\
  _("scat", strc), _("ssub", strs),\
  _("str", strmk),     _(".c", pc_u),\
  _("hom", hom_u),     _("hom-seek", hom_seek_u),\
  _("hom-fin", hom_fin_u),\
  _("emx", emx), _("hom-get-x", hom_getx_u),\
  _("emi", emi), _("hom-get-i", hom_geti_u),\
  _("zzz", zzz),       _("nump", nump_u),\
  _("symp", symp_u), _("twop", twop_u),\
  _("tblp", tblp_u), _("strp", strp_u),\
  _("nilp", nilp_u), _("homp", homp_u)

#define RPR(a,b) rpr(v,&d,a,b)
#define RIN(x) rin(v,&d,"i-"#x,x)
static Inline obj code_dictionary(vm v) {
  obj d = table(v);
  with(d, prims(RPR), insts(RIN));
  return d; }
#undef RPR
#undef RIN

static Inline void init_globals_array(vm v) {
  tup t = cells(v, Size(tup) + NGlobs);
  t->len = NGlobs, memset(t->xs, -1, w2b(NGlobs));
  obj z, y = Glob = puttup(t);
  with(y,
    z = code_dictionary(v), Top = z,
    z = table(v), Mac = z,
#define bsym(i,s)(z=interns(v,s),AR(y)[i]=z)
    bsym(Eval, "ev"), bsym(Apply, "ap"),
    bsym(Def, ":"), bsym(Cond, "?"), bsym(Lamb, "\\"),
    bsym(Quote, "`"), bsym(Seq, ","), bsym(Splat, ".")); }
#undef bsym


#define USR_PATH ".local/lib/"NOM"/"
#define SYS_PATH "/usr/lib/"NOM"/"
static int seekp(const char *p) {
  int b, c;
  b = open(getenv("HOME"), O_RDONLY);
  c = openat(b,USR_PATH, O_RDONLY), close(b);
  b = openat(c, p, O_RDONLY), close(c);
  if (-1 < b) return b;
  b = open(SYS_PATH, O_RDONLY);
  c = openat(b, p, O_RDONLY), close(b);
  return c; }


static Inline void sinitv(vm v) {
  v->t0 = clock(),
  v->ip = v->xp = v->syms = v->glob = nil,
  v->fp = v->hp = v->sp = (mem)w2b(1),
  v->count = 0, v->mem_len = 1, v->mem_pool = NULL,
  v->mem_root = NULL;
  init_globals_array(v);
  obj y = interns(v, "ns");
  tbl_set(v, Top, y, Top);
  y = interns(v, "macros");
  tbl_set(v, Top, y, Mac); }

vm bootstrap(vm v) {
  if (v == NULL) return v;
  // now we can bootstrap ...
  const char *path = "prelude.lips";
  int pre = seekp(path);
  if (pre == -1) errp(v, 0, "[init] can't find %s", path);
  else {
    FILE *f = fdopen(pre, "r");
    if (setjmp(v->restart)) return
      errp(v, 0, "[init] error in %s", path),
      fclose(f), finalize(v);
    scr(v, f), fclose(f); }
  return v; }

vm initialize() {
  vm v = malloc(sizeof(struct rt));
  if (!v || setjmp(v->restart)) return
    errp(v, 0, "[init] oom"), finalize(v);
  sinitv(v);
  return v; }

vm finalize(vm v) {
  if (v) free(v->mem_pool), free(v);
  return NULL; }

// "the interpreter"
// it's a stack machine with one free register (xp)
// that's implemented on top of the C compiler's calling
// convention. this allows us to keep all the most important
// vm state variables in CPU registers at all times while the
// interpreter is running, without any platform-specific code.
// basically the interpreter amounts to a set of functions
// with a shared type that observe certain restrictions
// concerning memory access, function calls, etc. the C
// compiler has to tail call optimize these functions,
// otherwise the call stack will grow every time a
// nonoptimized instruction is executed.

// the System V AMD64 ABI calling convention passes the
// first six pointer or integer function arguments in CPU
// registers; further arguments are passed on the stack
// which affects the compiler's ability to optimize tail
// calls. interpreter functions have six arguments for
// that reason. the six arguments are:
// - v  : vm instance pointer
// - ip : instruction pointer
// - fp : frame pointer
// - sp : stack pointer
// - hp : heap pointer
// - xp : return value
// variables ip-xp all have corresponding slots in the vm
// structure that store their state when the interpreter
// is inactive. these slots are also used by the interpreter
// in certain cases to pass arguments in excess of 6 without
// spilling them to the stack.
//
// here's some macros for interpreter functions
#define vm_op(n,...) static NoInline obj n(vm v,hom ip,mem fp,mem sp,mem hp,obj xp,##__VA_ARGS__)
#define Pack() (Ip=puthom(ip),Sp=sp,Hp=hp,Fp=fp,Xp=xp)
#define Unpack() (fp=Fp,hp=Hp,sp=Sp,ip=gethom(Ip),xp=Xp)
#define Jump(f,...) return (f)(v,ip,fp,sp,hp,xp,##__VA_ARGS__)
#define Have(n) if (avail < n) Jump((Xp=n,gc))
#define CallC(...)(Pack(),(__VA_ARGS__),Unpack())
#define Cont(n, x) return ip+=n,xp=x,G(ip)(v,ip,fp,sp,hp,xp)
#define Ap(f,x) return G(f)(v,f,fp,sp,hp,x)
#define Go(f,x) return f(v,ip,fp,sp,hp,x)
#define Next(n) Ap(ip+n,xp)

// these are for accessing the current stack frame
typedef struct fr { obj clos, retp, subd, argc, argv[]; } *fr;
#define ff(x)((fr)(x))
#define Locs fp[-1]
#define Clos ff(fp)->clos
#define Retp ff(fp)->retp
#define Subd ff(fp)->subd
#define Argc ff(fp)->argc
#define Argv ff(fp)->argv

static Inline void perrarg(vm v, mem fp) {
  if (fp == Pool + Len) return;
  num argc = getnum(Argc), i = 0;
  if (argc == 0) return;
  else for (fputs(" at ", stderr);;fputc(' ', stderr)) {
    obj x = Argv[i++];
    emit(v, x, stderr);
    if (i == argc) break; } }

// this is for runtime errors from the interpreter, it prints
// a backtrace and everything.
vm_op(interpret_error) {
  fputs("# ", stderr), emit(v, puthom(ip), stderr),
  // an error is expressed as the failure of the current function
  // to be defined for its arguments.
  fputs(" does not exist", stderr), perrarg(v, fp);
  fputc('\n', stderr);
  if (fp < Pool + Len)
    do ip = gethom(Retp), fp += Size(fr) + getnum(Argc) + getnum(Subd),
       fputs("#  in ", stderr), emsep(v, puthom(ip), stderr, '\n');
    while (fp < Pool + Len);
  return Hp = hp, restart(v); }

obj restart(vm v) {
  Fp = Sp = Pool + Len;
  Xp = Ip = nil;
  v->mem_root = NULL;
  //reqsp(v, 0);
  longjmp(v->restart, 1); }

// vm instructions for different errors. the compiler will
// never emit these.
#define TypeCheck(x,t) if(kind(x)!=t)Jump(interpret_error)
#define Arity(n) if(n>Argc)Jump(interpret_error)
#define ArityCheck(n) Arity(putnum(n))

// this is the garbage collector interface used
// by the interpreter. interpreter functions that
// allocate memory check the available space and
// trigger garbage collection when more space is
// needed than is available.
//
// the amount of space required is passed in a
// register slot on the vm structure in order not
// to spill function arguments on AMD64, sorry.
vm_op(gc) { num n = Xp; CallC(reqsp(v, n)); Next(0); }
#define avail (sp-hp)


// load instructions
#define fast_ref(b) (*(num*)((num)(b)+(num)GF(ip)-Num))
// this pointer arithmetic works because fixnums are
// premultiplied by sizeof(obj)
//
// load immediate value
vm_op(immv) { xp = (obj) GF(ip); Next(2); }
// simple variable reference
vm_op(argn) { xp = fast_ref(Argv);     Next(2); }
vm_op(locn) { xp = fast_ref(AR(Locs)); Next(2); }
vm_op(clon) { xp = fast_ref(AR(Clos)); Next(2); }
// special functions for eg. the first 4
// arguments in each location, and special values
// like 0 and nil, and functions that combine
// a reference with a stack push should all
// markedly improve general performance.

// environment operations
//
// push a value onto the stack
vm_op(push)  { Have(1); *--sp = xp; Next(1); }

// create a local variable environment
vm_op(prel) {
  num n = getnum(GF(ip));
  Have(n + 2);
  tup t = (tup) hp;
  hp += n + 1;
  t->len = n;
  while (n--) t->xs[n] = nil;
  *--sp = puttup(t);
  Next(2); }

// set a local variable
vm_op(setl) { fast_ref(AR(Locs)) = xp; Next(2); }

// resolve a lazy binding
vm_op(lbind) {
  obj w = (obj) GF(ip),
      d = XY(w), y = X(w);
  w = tbl_get(v, d, xp = YY(w));
  if (!w) Jump(interpret_error);
  xp = w;
  if (getnum(y) != 8) TypeCheck(xp, getnum(y));
  G(ip) = immv;
  GF(ip) = (terp*) xp;
  Next(2); }

// set a global variable
vm_op(tbind) {
  CallC(tbl_set(v, Dict, (obj) GF(ip), xp));
  Next(2); }

// control flow functions
// return to C
vm_op(yield) { return Pack(), xp; }

// return from a function
vm_op(ret) {
  ip = gethom(Retp);
  sp = (mem) ((num) Argv + Argc - Num);
  fp = (mem) ((num)   sp + Subd - Num);
  Next(0); }

// jumps
// unconditional
vm_op(jump) { Ap(gethom(GF(ip)), xp); }
// conditional
vm_op(branch) {
  ip = gethom(xp == nil ? FF(ip) : gethom(GF(ip)));
  Next(0); }
// opposite conditional
vm_op(barnch) {
  ip = gethom(xp != nil ? FF(ip) : gethom(GF(ip)));
  Next(0); }
// unconditional with closure
vm_op(clos) {
  Clos = (obj) GF(ip);
  ip = gethom(G(FF(ip)));
  Next(0); }


// regular function call
vm_op(call) {
  Have(Size(fr));
  obj adic = (obj) GF(ip);
  num off = fp - (mem) ((num) sp + adic - Num);
  fp = sp -= Size(fr);
  Retp = puthom(ip+2);
  Subd = putnum(off);
  Clos = nil;
  Argc = adic;
  Ap(gethom(xp), nil); }

// tail call: special case where the caller and callee
// have the same arity, so we can keep the frame
vm_op(loop) {
  num adic = getnum(Argc);
  for (mem p = Argv; adic--; *p++ = *sp++);
  sp = fp;
  Ap(gethom(xp), nil); }

// general tail call
vm_op(rec) {
  num adic = getnum(GF(ip));

  obj off = Subd, rp = Retp; // save return info
  mem src = sp + adic;
  // overwrite current frame with new frame
  sp = Argv + getnum(Argc);
  // important to copy in reverse order since they
  // may overlap
  for (num i = adic; i--; *--sp = *--src);
  fp = sp -= Size(fr);
  Retp = rp;
  Argc = putnum(adic);
  Subd = off;
  Clos = nil;
  Ap(gethom(xp), nil); }

// type/arity checking
vm_op(tcnum) { TypeCheck(xp, Num); Next(1); }
vm_op(tctwo) { TypeCheck(xp, Two); Next(1); }
vm_op(tchom) { TypeCheck(xp, Hom); Next(1); }
vm_op(arity) { Arity((obj)GF(ip)); Next(2); }

// continuations
//
// this is a simple but expensive way of doing continuations.
// it would be more memory efficient to do a copy-on-write
// kind of thing where the stack is only copied if the function
// returns. a spaghetti stack would be another option but it
// would be slower. faster continuations at the cost of slower
// function calls seems like a bad deal given the relative
// frequency of the two.
vm_op(ccc_u) {
  ArityCheck(1);
  obj x = Argv[0];
  TypeCheck(x, Hom);
  // we need space for:
  // the entire stack
  // the frame offset
  // the length (to put it all in a tuple)
  // the continuation thread (4 words)
  num ht = Pool + Len - sp;
  Have(ht + 6);
  tup t = (tup) hp;
  hp += ht + 2;
  t->len = ht + 1;
  t->xs[0] = putnum(fp - sp);
  memcpy(t->xs+1, sp, w2b(ht));
  hom c = (hom) hp;
  hp += 4;
  c[0].g = cont;
  c[1].g = (terp*) puttup(t);
  c[2].g = NULL;
  c[3].g = (terp*) c;
  Argv[0] = puthom(c);
  Ap(gethom(x), nil); }

// call a continuation
vm_op(cont) {
  tup t = gettup(GF(ip));
  Have(t->len - 1);
  xp = getnum(Argc) == 0 ? nil : *Argv;
  num off = getnum(t->xs[0]);
  sp = Pool + Len - (t->len - 1);
  fp = sp + off;
  memcpy(sp, t->xs+1, w2b(t->len-1));
  Jump(ret); }

vm_op(rd_u) {
  obj x; CallC(x = parse(v, stdin), x = x ? pair(v, x, nil) : nil);
  Go(ret, x); }

// eval
vm_op(ev_u) {
  ArityCheck(1);
  obj x; hom h;
  CallC(
   h = gethom(compile(v, *Argv)),
   x = G(h)(v, h, Fp, Sp, Hp, nil));
  Go(ret, x); }

// apply
vm_op(ap_u) {
  ArityCheck(2);
  obj x = Argv[0];
  TypeCheck(x, Hom);
  obj y = Argv[1];
  num adic = llen(y);
  Have(adic);
  obj off = Subd, rp = Retp;
  sp = Argv + getnum(Argc) - adic;
  for (num j = 0; j < adic; sp[j++] = X(y), y = Y(y));
  fp = sp -= Size(fr);
  Retp = rp;
  Argc = putnum(adic);
  Subd = off;
  Clos = nil;
  Ap(gethom(x), nil); }


// instructions used by the compiler
vm_op(hom_u) {
  ArityCheck(1);
  TypeCheck(*Argv, Num);
  num len = getnum(*Argv) + 2;
  Have(len);
  hom h = (hom) hp;
  hp += len;
  memset(h, -1, w2b(len));
  h[len-1].g = (terp*) h;
  h[len-2].g = NULL;
  Go(ret, puthom(h+len-2)); }
vm_op(hom_fin_u) {
  ArityCheck(1);
  TypeCheck(*Argv, Hom);
  obj x; CallC(x = hom_fin(v, *Argv));
  Go(ret, x); }
vm_op(emx) {
  ArityCheck(2);
  TypeCheck(Argv[1], Hom);
  hom h = gethom(Argv[1]) - 1;
  G(h) = (terp*) Argv[0];
  Go(ret, puthom(h)); }
vm_op(emi) {
  ArityCheck(2);
  TypeCheck(Argv[0], Num);
  TypeCheck(Argv[1], Hom);
  hom h = gethom(Argv[1]) - 1;
  G(h) = (terp*) getnum(Argv[0]);
  Go(ret, puthom(h)); }
vm_op(hom_geti_u) {
  ArityCheck(1);
  TypeCheck(Argv[0], Hom);
  Go(ret, putnum(G(Argv[0]))); }
vm_op(hom_getx_u) {
  ArityCheck(1);
  TypeCheck(Argv[0], Hom);
  Go(ret, (obj)G(Argv[0])); }
vm_op(hom_seek_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Hom);
  TypeCheck(Argv[1], Num);
  Go(ret, puthom(gethom(Argv[0])+getnum(Argv[1]))); }

// hash tables
vm_op(tblg) {
  ArityCheck(2);
  TypeCheck(Argv[0], Tbl);
  xp = tbl_get(v, Argv[0], Argv[1]);
  Go(ret, xp ? xp : nil); }
vm_op(tblc) {
  ArityCheck(2);
  TypeCheck(Argv[0], Tbl);
  xp = tbl_get(v, Argv[0], Argv[1]);
  Go(ret, xp ? putnum(0) : nil); }

static obj tblss(vm v, num i, num l) {
  mem fp = Fp;
  return i > l-2 ? Argv[i-1] :
    (tbl_set(v, Xp, Argv[i], Argv[i+1]),
     tblss(v, i+2, l)); }

vm_op(tbls) {
  obj x = nil;
  ArityCheck(1);
  xp = *Argv;
  TypeCheck(xp, Tbl);
  CallC(x = tblss(v, 1, getnum(Argc)));
  Go(ret, x); }
vm_op(tbld) {
  ArityCheck(2);
  TypeCheck(Argv[0], Tbl);
  obj x = nil;
  CallC(x = tbl_del(v, Argv[0], Argv[1]));
  Go(ret, x); }
vm_op(tblks) {
  ArityCheck(1);
  TypeCheck(Argv[0], Tbl);
  obj x;
  CallC(x = tbl_keys(v, Argv[0]));
  Go(ret, x); }
vm_op(tbll) {
  ArityCheck(1);
  TypeCheck(Argv[0], Tbl);
  Go(ret, putnum(gettbl(*Argv)->len)); }
vm_op(tblmk) {
  CallC(Xp = table(v),
    tblss(v, 0, getnum(Argc)));
  Go(ret, Xp); }

// string instructions
vm_op(strl) {
  ArityCheck(1);
  TypeCheck(*Argv, Oct);
  Go(ret, putnum(getoct(*Argv)->len-1)); }
vm_op(strg) {
  ArityCheck(2);
  TypeCheck(Argv[0], Oct);
  TypeCheck(Argv[1], Num);
  Go(ret, getnum(Argv[1]) < getoct(Argv[0])->len-1 ?
    putnum(getoct(Argv[0])->text[getnum(Argv[1])]) :
    nil); }

vm_op(strc) {
  num l = getnum(Argc), sum = 0, i = 0;
  while (i < l) {
    obj x = Argv[i++];
    TypeCheck(x, Oct);
    sum += getoct(x)->len - 1; }
  num words = b2w(sum+1) + 1;
  Have(words); oct d = (oct) hp; hp += words;
  d->len = sum + 1;
  d->text[sum] = 0;
  while (i) {
    oct x = getoct(Argv[--i]);
    sum -= x->len - 1;
    memcpy(d->text+sum, x->text, x->len - 1); }
  Go(ret, putoct(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
vm_op(strs) {
  ArityCheck(3);
  TypeCheck(Argv[0], Oct);
  TypeCheck(Argv[1], Num);
  TypeCheck(Argv[2], Num);
  oct src = getoct(Argv[0]);
  num lb = getnum(Argv[1]), ub = getnum(Argv[2]);
  lb = max(lb, 0), ub = max(min(ub, src->len-1), lb);
  num words = 1 + b2w(ub - lb + 1);
  Have(words);
  oct dst = (oct) hp; hp += words;
  dst->len = ub - lb + 1;
  dst->text[ub - lb] = 0;
  memcpy(dst->text, src->text + lb, ub - lb);
  Go(ret, putoct(dst)); }

vm_op(strmk) {
  num i, l = getnum(Argc)+1, size = 1 + b2w(l);
  Have(size);
  oct s = (oct) hp;
  hp += size;
  for (i = 0; i < l-1; i++) {
    obj x = Argv[i];
    TypeCheck(x, Num);
    if (x == putnum(0)) break;
    s->text[i] = getnum(x); }
  s->text[i] = 0;
  s->len = i+1;
  Go(ret, putoct(s)); }

vm_op(vararg) {
  num reqd = getnum(GF(ip)),
      vdicity = getnum(Argc) - reqd;
  ArityCheck(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdicity) {
    Have(1);
    sp = --fp;
    for (num i = 0; i < Size(fr) + reqd; i++)
      fp[i] = fp[i+1];
    Argc += Word;
    Argv[reqd] = nil; }
  // in this case we just keep the existing slots.
  // the path is knowable at compile time in many cases
  // so maybe vararg should be two or more different
  // functions.
  else {
    Have(2 * vdicity);
    two t = (two) hp;
    hp += 2 * vdicity;
    for (num i = vdicity; i--;)
      t[i].x = Argv[reqd + i],
      t[i].y = puttwo(t+i+1);
    t[vdicity-1].y = nil;
    Argv[reqd] = puttwo(t); }
  Next(2); }

// the next few functions create and store
// lexical environments.
vm_op(encl) {
  num n = Xp;
  obj x = (obj) GF(ip);
  mem block = hp;
  hp += n;
  obj arg = nil; // optional argument array
  if (n > 11) {
    n -= 12;
    tup t = (tup) block;
    block += 1 + n;
    t->len = n;
    while (n--) t->xs[n] = Argv[n];
    arg = puttup(t); }

  tup t = (tup) block; // compiler thread closure array (1 length 5 elements)
  hom at = (hom) (block+6); // compiler thread (1 instruction 2 data 2 tag)

  t->len = 5; // initialize alpha closure
  t->xs[0] = arg;
  t->xs[1] = xp; // Locs or nil
  t->xs[2] = Clos;
  t->xs[3] = Y(x);
  t->xs[4] = puthom(at);

  at[0].g = pc0;
  at[1].g = (terp*) puttup(t);
  at[2].g = (terp*) X(x);
  at[3].g = 0;
  at[4].g = (terp*) at;

  Ap(ip+2, puthom(at)); }

vm_op(prencl) {
  num n = getnum(Argc);
  n += n ? 12 : 11;
  Have(n);
  Xp = n;
  Jump(encl); }

vm_op(encll) { Go(prencl, Locs); }
vm_op(encln) { Go(prencl, nil); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
vm_op(pc0) {
  obj ec = (obj) GF(ip),
      arg = AR(ec)[0],
      loc = AR(ec)[1];
  num adic = nilp(arg) ? 0 : AL(arg);
  Have(Size(fr) + adic + 1);
  num off = (mem) fp - sp;
  G(ip) = pc1;
  sp -= adic;
  for (num z = adic; z--; sp[z] = AR(arg)[z]);
  ec = (obj) GF(ip);
  fp = sp -= Size(fr);
  Retp = puthom(ip);
  Subd = putnum(off);
  Argc = putnum(adic);
  Clos = AR(ec)[2];
  if (!nilp(loc)) *--sp = loc;
  ip = gethom(AR(ec)[3]);
  Next(0); }

// finalize function instance closure
vm_op(pc1) {
  G(ip) = clos;
  GF(ip) = (terp*) xp;
  Next(0); }

// this is used to create closures.
vm_op(take) {
  num n = getnum((obj)GF(ip));
  Have(n + 1);
  tup t = (tup) hp;
  hp += n + 1;
  t->len = n;
  memcpy(t->xs, sp, w2b(n));
  sp += n;
  Go(ret, puttup(t)); }

// print to console
vm_op(em_u) {
  num l = getnum(Argc), i;
  if (l) {
    for (i = 0; i < l - 1; i++)
      emsep(v, Argv[i], stdout, ' ');
    emit(v, xp = Argv[i], stdout); }
  fputc('\n', stdout);
  Jump(ret); }

vm_op(pc_u) {
  ArityCheck(1);
  xp = *Argv;
  TypeCheck(xp, Num);
  fputc(getnum(xp), stdout);
  Jump(ret); }

vm_op(emse) {
  emsep(v, xp, stdout, getnum(GF(ip)));
  Next(2); }

// pairs
vm_op(cons) {
  Have(1); hp[0] = *sp++, hp[1] = xp;
  xp = puttwo(hp); hp += 2; Next(1); }
vm_op(car) { Ap(ip+1, X(xp)); }
vm_op(cdr) { Ap(ip+1, Y(xp)); }
vm_op(setcar) { obj x = *sp++; X(xp) = x; xp = x; Next(1); }
vm_op(setcdr) { obj x = *sp++; Y(xp) = x; xp = x; Next(1); }

vm_op(cons_u) {
  ArityCheck(2);
  Have(2); hp[0] = Argv[0], hp[1] = Argv[1];
  xp = puttwo(hp), hp += 2; Jump(ret); }
vm_op(car_u) {
  ArityCheck(1); TypeCheck(*Argv, Two);
  Go(ret, X(*Argv)); }
vm_op(cdr_u) {
  ArityCheck(1); TypeCheck(*Argv, Two);
  Go(ret, Y(*Argv)); }
vm_op(setcar_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Two);
  Go(ret, X(Argv[0]) = Argv[1]); }
vm_op(setcdr_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Two);
  Go(ret, Y(Argv[0]) = Argv[1]); }

// arithmetic
#define ok putnum(0)
vm_op(neg) { Ap(ip+1, putnum(-getnum(xp))); }
vm_op(add) {
  xp = xp + *sp++ - Num;
  Next(1); }
vm_op(sub) {
  xp = *sp++ - xp + Num;
  Next(1); }
vm_op(mul) {
  xp = putnum(getnum(xp) * getnum(*sp++));
  Next(1); }
vm_op(dqv) {
  if (xp == putnum(0)) Jump(interpret_error);
  xp = putnum(getnum(*sp++) / getnum(xp));
  Next(1); }
vm_op(mod) {
  if (xp == putnum(0)) Jump(interpret_error);
  xp = putnum(getnum(*sp++) % getnum(xp));
  Next(1); }

#define mm_u(_c,_v,_z,op){\
  obj x,m=_z,*xs=_v,*l=xs+_c;\
  if (_c) for(;xs<l;m=m op getnum(x)){\
    x = *xs++; TypeCheck(x, Num);}\
  Go(ret, putnum(m));}
#define mm_u0(_c,_v,_z,op){\
  obj x,m=_z,*xs=_v,*l=xs+_c;\
  if (_c) for(;xs<l;m=m op getnum(x)){\
    x = *xs++; TypeCheck(x, Num);\
    if (x == putnum(0)) Jump(interpret_error);}\
  Go(ret, putnum(m));}

vm_op(add_u) {
  mm_u(getnum(Argc), Argv, 0, +); }
vm_op(mul_u) {
  mm_u(getnum(Argc), Argv, 1, *); }
vm_op(sub_u) {
  num i = getnum(Argc);
  if (i == 0) Go(ret, putnum(0));
  TypeCheck(*Argv, Num);
  if (i == 1) Go(ret, putnum(-getnum(*Argv)));
  mm_u(i-1,Argv+1,getnum(Argv[0]),-); }

vm_op(div_u) {
  num i = getnum(Argc);
  if (i == 0) Go(ret, putnum(1));
  TypeCheck(*Argv, Num);
  mm_u0(i-1,Argv+1,getnum(*Argv),/); }
vm_op(mod_u) {
  num i = getnum(Argc);
  if (i == 0) Go(ret, putnum(1));
  TypeCheck(*Argv, Num);
  mm_u0(i-1,Argv+1,getnum(*Argv),%); }

// type predicates
vm_op(numpp) { xp = nump(xp) ? ok : nil; Next(1); }
vm_op(hompp) { xp = homp(xp) ? ok : nil; Next(1); }
vm_op(twopp) { xp = twop(xp) ? ok : nil; Next(1); }
vm_op(sympp) { xp = symp(xp) ? ok : nil; Next(1); }
vm_op(strpp) { xp = octp(xp) ? ok : nil; Next(1); }
vm_op(tblpp) { xp = tblp(xp) ? ok : nil; Next(1); }
vm_op(nilpp) { xp = nilp(xp) ? ok : nil; Next(1); }

// comparison
int eql(obj, obj);
static int twoeq(obj a, obj b) {
  return eql(X(a), X(b)) && eql(Y(a), Y(b)); }

static int tupeq(obj a, obj b) {
  tup t = gettup(a), u = gettup(b);
  if (t->len != u->len) return 0;
  for (num i = 0; i < t->len; i++)
    if (!eql(t->xs[i], u->xs[i])) return 0;
  return 1; }
static int streq(obj a, obj b) {
  oct o = getoct(a), m = getoct(b);
  if (o->len != m->len) return 0;
  for (num i = 0; i < o->len; i++)
    if (o->text[i] != m->text[i]) return 0;
  return 1; }
int eql(obj a, obj b) {
  if (a == b) return 1;
  if (kind(a) != kind(b)) return 0;
  switch (kind(a)) {
    case Tup: return tupeq(a, b);
    case Two: return twoeq(a, b);
    case Oct: return streq(a, b);
    default: return 0; } }

vm_op(lt)    { xp = *sp++ <  xp ? ok : nil; Next(1); }
vm_op(lteq)  { xp = *sp++ <= xp ? ok : nil; Next(1); }
vm_op(gteq)  { xp = *sp++ >= xp ? ok : nil; Next(1); }
vm_op(gt)    { xp = *sp++ >  xp ? ok : nil; Next(1); }
// there should be a separate instruction for simple equality.
vm_op(eq)   {
  obj y = *sp++;
  xp = eql(xp, y) ? ok : nil;
  Next(1); }

#define ord_w(r){\
  obj n=getnum(Argc),*xs=Argv,m,*l;\
  switch(n){\
    case 0: no: Go(ret, nil);\
    case 1: break;\
    default: for(l=xs+n-1,m=*xs;xs<l;m=*++xs)\
               if(!(m r xs[1])) goto no;}\
  Go(ret, ok);}

#define ord_wv(r){\
  obj n=getnum(Argc),*xs=Argv,m,*l;\
  switch(n){\
    case 0: no: Go(ret, nil);\
    case 1: break;\
    default: for(l=xs+n-1,m=*xs;xs<l;m=*++xs)\
               if(!(r(m,xs[1]))) goto no;}\
  Go(ret, ok);}

vm_op(and_u) {
  num i, l = getnum(Argc);
  switch (l) {
    case 0: Go(ret, nil);
    default:
      for (i = 0; i < l-1; i++) if (nilp(Argv[i])) break;
      Go(ret, Argv[i]); } }

vm_op(or_u) {
  num i, l = getnum(Argc);
  switch (l) {
    case 0: Go(ret, nil);
    default:
      for (i = 0; i < l-1; i++) if (!nilp(Argv[i])) break;
      Go(ret, Argv[i]); } }

#define ord_v(r) Go(ret, ord_u(getnum(Argc), Argv, r))

vm_op(lt_u)   { ord_w(<); }
vm_op(lteq_u) { ord_w(<=); }
vm_op(eq_u)   { ord_wv(eql); }
vm_op(gteq_u) { ord_w(>=); }
vm_op(gt_u)   { ord_w(>); }

#define typpp(t) {\
  for (obj *xs = Argv, *l=xs+getnum(Argc);xs<l;)\
    if (kind(*xs++)!=t) Go(ret, nil);\
  Go(ret, ok); }
vm_op(nump_u) { typpp(Num); }
vm_op(homp_u) { typpp(Hom); }
vm_op(strp_u) { typpp(Oct); }
vm_op(tblp_u) { typpp(Tbl); }
vm_op(twop_u) { typpp(Two); }
vm_op(symp_u) { typpp(Sym); }
vm_op(nilp_u) { typpp(Nil); }

// stack manipulation
vm_op(tuck) { Have(1); sp--, sp[0] = sp[1], sp[1] = xp; Next(1); }
vm_op(drop) { sp++; Next(1); }

// errors
vm_op(fail) { Jump(interpret_error); }
vm_op(zzz) { exit(EXIT_SUCCESS); }
