#include "i.h"
int
 memcmp(void const*, void const*, size_t);
void
 *malloc(size_t),
 free(void*),
 *memcpy(void*restrict, void const*restrict, size_t),
 *memset(void*, int, size_t);

// function state using this type
typedef struct env {
  struct env *par;
  // these parameters represent stack state at a point in compile process
  intptr_t
    args,  // list  // function positional arguments
    imps,  // list  // closure variables
    stack, // list  // current values on stack
    lams,  // alist // known function definitions
    alts, ends; } env;

#define Ana(n, ...) struct g *n(struct g *f, struct env **c, intptr_t x, ##__VA_ARGS__)
#define Cata(n, ...) struct g *n(struct g *f, struct env **c, size_t m, ##__VA_ARGS__)
typedef Ana(ana);
typedef Cata(cata);
static ana analyze, g_c0_if, g_c0_let, g_c0_apargs;
static cata pull, g_c1_i, g_c1_ix, g_c1var, g_c1ap, g_c1_yield, g_c1_ret;

// generic instruction ana handlers
static struct g *g_c0_ix(struct g *f, g_vm_t *i, intptr_t x) {
 return g_push(f, 3, g_c1_ix, i, x); }

static g_noinline struct g *enscope(struct g *f, struct env *par, intptr_t args, intptr_t imps) {
 f = g_push(f, 3, args, imps, par);
 uintptr_t n = Width(env);
 f = g_have(f, n + Width(struct g_tag) + 1);
 if (g_ok(f)) {
  union u *k = bump(f, n + Width(struct g_tag));
  *--f->sp = (intptr_t) k;
  struct g_tag *t = (struct g_tag*) (k + n);
  t->null = NULL;
  t->head = k;
  struct env *c = (struct env*) k;
  c->stack = c->alts = c->ends = c->lams = g_nil;
  c->args = f->sp[1],
  c->imps = f->sp[2],
  c->par = (struct env*) f->sp[3];
  f->sp[3] = (intptr_t) c,
  f->sp += 3; }
 return f; }
static struct g *gx2(struct g *f, intptr_t a, intptr_t b) {
 return gxl(g_push(f, 2, a, b)); }

static intptr_t memq(struct g *f, g_num l, g_num k) {
 for (; twop(l); l = B(l)) if (eql(f, k, A(l))) return l;
 return 0; }

static intptr_t assq(struct g *f, g_num l, g_num k) {
 for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
 return 0; }


// don't inline this so callers can tail call optimize
g_noinline struct g *g_c0(struct g *f, g_vm_t *y) {
 f = enscope(f, (struct env*) g_nil, g_nil, g_nil);
 if (!g_ok(f)) return f;
 struct env *c = (struct env*) ptr(g_pop1(f));
 g_num x = f->sp[0];
 return
  f->sp[0] = (g_num) g_c1_yield,
  avec(f, c,
   avec(f, x,
    f = g_c0_ix(f, y, word(f->ip))),
    f = analyze(f, &c, x),
    f = pull(f, &c, 0)),
  f; }

#define Kp (f->ip)
static Cata(g_c1_yield) {
 f = g_have(f, m + Width(struct g_tag));
 if (g_ok(f)) {
  union u *k = bump(f, m + Width(struct g_tag));
  struct g_tag *t = (struct g_tag*) (k + m);
  t->null = NULL;
  t->head = k;
  memset(k, -1, m * sizeof(intptr_t));
  Kp = k + m; }
 return f; }

static Cata(g_c1_if_pop_exit) {
  f = pull(f, c, m);
  if (g_ok(f)) (*c)->ends = B((*c)->ends); // pops cond expression exit address off env stack ends
  return f; }

static Cata(g_c1_if_pop_branch) {
 f = pull(f, c, m + 2);
 if (g_ok(f))
  Kp -= 2,
  Kp[0].ap = g_vm_cond,
  Kp[1].x = A((*c)->alts);
 (*c)->alts = B((*c)->alts);
 return f; }

static Cata(g_c1_if_push_branch) {
 f = pull(f, c, m);
 f = gx2(f, (intptr_t) Kp, (*c)->alts);
 if (g_ok(f)) (*c)->alts = g_pop1(f);
 return f; }

static Cata(g_c1_if_push_exit) {
 f = pull(f, c, m);
 f = gx2(f, (intptr_t) Kp, (*c)->ends);
 if (g_ok(f)) (*c)->ends = g_pop1(f);
 return f; }

static Cata(g_c1_if_jump_out) {
 f = pull(f, c, m + 3);
 if (g_ok(f)) {
  union u *a = cell(A((*c)->ends));
  if (a->ap == g_vm_ret || a->ap == g_vm_tap)
   Kp = memcpy(Kp - 2, a, 2 * sizeof(g_num));
  else if (a->ap == g_vm_tapn)
   Kp = memcpy(Kp - 3, a, 3 * sizeof(g_num));
  else
   Kp -= 2,
   Kp[0].ap = g_vm_jump,
   Kp[1].x = (g_num) a; }
 return f; }

static Cata(g_c1ap) {
 f = pull(f, c, m + 1);
 if (g_ok(f)) {
  if (Kp[0].ap == g_vm_ret) Kp[0].ap = g_vm_tap;
  else Kp -= 1, Kp[0].ap = g_vm_ap; }
 return f; }

static Cata(g_c1apn) {
 g_num arity = g_pop1(f);
 f = pull(f, c, m + 2);
 if (g_ok(f)) {
  if (Kp[0].ap == g_vm_ret) Kp -= 1, Kp[0].ap = g_vm_tapn, Kp[1].x = arity;
  else Kp -= 2, Kp[0].ap = g_vm_apn, Kp[1].x = arity; }
 return f; }

static Cata(g_c1_ix_, g_vm_t *i, g_num x) {
 avec(f, x, f = pull(f, c, m + 2));
 if (g_ok(f))
  Kp -= 2,
  Kp[0].ap = i,
  Kp[1].x = x;
 return f; }

static Cata(g_c1var_2) {
 g_num var = g_pop1(f),
       stack = g_pop1(f),
       i = 0;
 while (twop(stack))
  if (eql(f, A(stack), var)) break;
  else stack = B(stack), i++;
 return g_c1_ix_(f, c, m, g_vm_pushr, gputnum(i)); }

static struct g *append(struct g *f) {
 uintptr_t i = 0;
 for (intptr_t l; g_ok(f) && twop(f->sp[0]); i++)
  l = B(f->sp[0]),
  f->sp[0] = A(f->sp[0]),
  f = g_push(f, 1, l);
 if (!g_ok(f)) return f;
 if (i == 0) return f->sp++, f;
 if (g_ok(f)) f->sp[0] = f->sp[i + 1];
 while (i--) f = gxr(f);
 if (g_ok(f)) f->sp[1] = f->sp[0], f->sp++;
 return f; }

static size_t llen(intptr_t l) {
 size_t n = 0;
 while (twop(l)) n++, l = B(l);
 return n; }

// emit stack g_vm_pushrerence instruction
static Cata(g_c1var) {
 g_num l, v = g_pop1(f), // variable name
          i = llen(g_pop1(f)); // stack inset
 for (l = (*c)->imps; !nilp(l); l = B(l), i++) if (eql(f, v, A(l))) goto out;
 for (l = (*c)->args; !nilp(l); l = B(l), i++) if (eql(f, v, A(l))) goto out;
out:
 return g_c1_ix_(f, c, m, g_vm_pushr, gputnum(i)); }

static g_inline Cata(pull) {
 return ((cata*) g_pop1(f))(f, c, m); }

static Cata(g_c1_i) {
 g_vm_t *i = (g_vm_t*) g_pop1(f);
 f = pull(f, c, m + 1);
 if (g_ok(f)) Kp -= 1, Kp[0].ap = i;
 return f; }

static Cata(g_c1_ix) {
 g_vm_t *i = (g_vm_t*) g_pop1(f);
 g_num x = g_pop1(f);
 return g_c1_ix_(f, c, m, i, x); }

static Cata(g_c1_curry) {
 struct env *e = (struct env*) g_pop1(f);
 uintptr_t ar = llen(e->args) + llen(e->imps);
 return ar == 1 ?  pull(f, c, m) :
  g_c1_ix_(f, c, m, g_vm_curry, gputnum(ar)) ; }

static Cata(g_c1_ret) {
 struct env *e = (struct env*) g_pop1(f);
 uintptr_t ar = llen(e->args) + llen(e->imps);
 return g_c1_ix_(f, c, m, g_vm_ret, gputnum(ar)); }

static struct g
 *g_c0_lambda(struct g*, struct env**, intptr_t, intptr_t),
 *g_c0_seq(struct g*, struct env**, intptr_t, intptr_t);

static struct g *g_eval(struct g *f) {
 f = g_c0(f, g_vm_yieldk);
#if g_tco
 return g_ok(f) ? f->ip->ap(f, f->ip, f->hp, f->sp) : f; }
#else
 while (g_ok(f)) f = f->ip->ap(f);
 return g_code_of(f) == g_status_eof ? g_core_of(f) : f; }
#endif

static g_noinline Ana(analyze) {
 if (!g_ok(f)) return f;
 // is it a variable?
 if (symp(x)) for (struct env *d = *c;; d = d->par) {
  if (nilp(d)) { // free variable?
   intptr_t y = g_tget(f, 0, dict_of(f), x);
   if (y) return g_c0_ix(f, g_vm_pushk, y);
   f = gx2(f, x, (*c)->imps);
   if (g_ok(f)) (*c)->imps = g_pop1(f),
                f = g_c0_ix(f, g_vm_freev, (*c)->imps);
   return f; }
  // defined as a function by a local let form?
  intptr_t y;
  if ((y = assq(f, d->lams, x))) return
   avec(f, y, f = g_c0_apargs(f, c, BB(y))),
   g_c0_ix(f, g_vm_lazyb, y);
  // non function definition from local let form?
  if (memq(f, d->stack, x)) return
   g_push(f, 3, g_c1var_2, x, d->stack);
  // closure or positional argument?
  if (memq(f, d->imps, x) || memq(f, d->args, x)) {
   if (*c != d) { // if we have found the variable in an enclosing scope then import it
    f = gx2(f, x, (*c)->imps);
    if (g_ok(f)) x = A((*c)->imps = g_pop1(f)); }
   return g_push(f, 3, g_c1var, x, (*c)->stack); } }
 // if it's not a variable and it's not a list (pair) then it evals to itself
 if (!twop(x)) return g_c0_ix(f, g_vm_pushk, x);
 intptr_t a = A(x), b = B(x);
 // singleton list?
 if (!twop(b)) return analyze(f, c, a); // value of first element
 // special form?
 if (symp(a) && nom(a)->nom && len(nom(a)->nom) == 1) switch (*txt(nom(a)->nom)) {
  case '`': return g_c0_ix(f, g_vm_pushk, !twop(b) ? b : A(b));
  case ':': return !twop(B(b)) ? analyze(f, c, A(b)) :
                                 g_c0_let(f, c, b);
  case ',': return g_c0_seq(f, c, A(b), B(b));
  case '\\': return f = g_c0_lambda(f, c, g_nil, b),
                    analyze(f, c, g_ok(f) ? g_pop1(f) : 0);
  case '?': return !twop(B(b)) ? analyze(f, c, A(b)) :
                                 g_c0_if(f, c, b); }
 // macro?
 intptr_t mac = g_tget(f, 0, f->macro, a);
 if (mac) return
  f = g_push(f, 5, b, g_nil ,f->quote, g_nil, mac),
  f = g_eval(gxr(gxl(gxr(gxl(f))))),
  analyze(f, c, g_ok(f) ? g_pop1(f) : 0);
 // application.
 avec(f, a, f = g_c0_apargs(f, c, b));
 return analyze(f, c, a); }

static struct g *g_c0_lambda(struct g *f, struct env **c, intptr_t imps, intptr_t exp) {
 f = enscope(f, *c, exp, imps);
 if (!g_ok(f)) return f;
 struct env *d = (struct env*) g_pop1(f);
 MM(f, &d);
 intptr_t x = d->args;

 // push exp args onto stack
 if (!twop(x)) f = g_push(f, 2, g_nil, g_nil);
 else { // there's at least one argument
  MM(f, &x);
  int n = 0;
  while (twop(B(x))) f = g_push(f, 1, A(x)), n++, x = B(x);
  f = g_push(f, 1, g_nil);
  while (n--) f = gxr(f);
  UM(f);
  f = g_push(f, 1, A(x)); }

 union u*k, *ip;
 if (g_ok(f))
  exp = g_pop1(f),
  d->args = f->sp[0],
  f->sp[0] = word(g_c1_yield),
  avec(f, exp, f = g_push(f, 2, g_c1_ret, d)),
  f = analyze(f, &d, exp),
  f = g_push(f, 2, g_c1_curry, d),
  ip = f->ip,
  avec(f, ip, f = pull(f, &d, 0));

 if (g_ok(f))
  k = f->ip,
  ttag(k)->head = k,
  f->ip = ip,
  f = gx2(f, word(k), d->imps);

 return UM(f), f; }

static Ana(g_c0_if_r) { return
 avec(f, x, f =
  !twop(x) || !twop(B(x)) ?
    g_push(f, 1, g_c1_if_jump_out) :
    (f = g_c0_if_r(f, c, BB(x)),
     f = g_push(f, 2, g_c1_if_jump_out, g_c1_if_push_branch),
     f = analyze(f, c, AB(x)),
     g_push(f, 1, g_c1_if_pop_branch))),
 analyze(f, c, twop(x) ? A(x) : g_nil); }

static Ana(g_c0_if) { return
 avec(f, x, f = g_push(f, 1, g_c1_if_push_exit)),
 g_push(g_c0_if_r(f, c, x), 1, g_c1_if_pop_exit); }

static struct g *g_c0_seq(struct g*f, struct env**c, intptr_t a, intptr_t b) {
 if (g_ok(f) && twop(b))
  avec(f, a,
   f = g_c0_seq(f, c, A(b), B(b)),
   f = g_push(f, 2, g_c1_i, g_vm_drop1));
 return analyze(f, c, a); }

static struct g *g_c0_apargsr(struct g *f, struct env**c, intptr_t x) {
 return !twop(x) ? f : (avec(f, x, f = g_c0_apargsr(f, c, B(x)),
                                   f = g_push(f, 1, g_c1ap)),
                        analyze(f, c, A(x))); }

// evaluate function call arguments and apply
static struct g *g_c0_apargs(struct g *f, struct env **c, intptr_t x) {
 avec(f, x, f = gx2(f, g_nil, (*c)->stack));
 if (g_ok(f))
  (*c)->stack = g_pop1(f),
  f = g_c0_apargsr(f, c, x),
  (*c)->stack = B((*c)->stack);
 return f; }

static g_num ldels(struct g *f, g_num lam, g_num l) {
 if (!twop(l)) return g_nil;
 intptr_t m = ldels(f, lam, B(l));
 if (!assq(f, lam, A(l))) B(l) = m, m = l;
 return m; }

static bool lambp(struct g *f, g_num x) {
 if (!twop(x) || !symp(A(x))) return false;
 struct g_vec *n = sym(A(x))->nom;
 return n && len(n) == 1 && *txt(n) == '\\'; }

static g_num reverse(struct g *f, g_num l) {
 g_num n = g_nil;
 for (g_num m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
 return n; }

// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.
static struct g *g_c0_let(struct g *f, struct env **b, g_num exp) {
 struct g_root *mm = f->root;
#define forget() ((f)->root=(mm),f)
 MM(f, &exp);
 f = enscope(f, *b, (*b)->args, (*b)->imps);
 if (!g_ok(f)) return forget();
 struct env
  *q = (struct env*) g_pop1(f),
  **c = &q;
 // lots of variables :(
 g_num nom = g_nil, def = g_nil, lam = g_nil,
       v = g_nil, d = g_nil, e = g_nil;
 MM(f, &nom), MM(f, &def), MM(f, &lam);
 MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

 // collect vars and defs into two lists
 while (twop(exp) && twop(B(exp))) {
  for (d = A(exp), e = AB(exp); twop(d); e = g_pop1(f), d = A(d)) {
   f = gx2(f, e, g_nil);
   f = append(gx2(f, word(f->lambda), B(d)));
   if (!g_ok(f)) return forget(); }
  f = gx2(f, d, nom);
  f = gx2(f, e, def);
  if (!g_ok(f)) return forget();
  def = g_pop1(f), nom = g_pop1(f);
  // if it's a lambda compile it and record in lam list
  if (lambp(f, e)) {
   f = g_push(f, 2, d, lam);
   f = gxl(gxr(g_c0_lambda(f, c, g_nil, B(e))));
   if (!g_ok(f)) return forget();
   lam = g_pop1(f); }
  exp = BB(exp); }

 // if there's no body then evaluate the name of the last definition
 bool even = !twop(exp); // we check this again later to make global bindings at top level
 if (even) {
  f = gx2(f, A(nom), g_nil);
  if (!g_ok(f)) return forget();
  exp = g_pop1(f); }

 // find closures
 // for each function f with closure C(f)
 // for each function g with closure C(g)
 // if f in C(g) then C(g) include C(f)
 long j;
 do for (j = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
  for (e = lam; twop(e); e = B(e)) // for each bound function variable
#define BBA(o) B(BA(o))
   if (d != e && memq(f, BBA(e), AA(d))) // if you need this function
    for (v = BBA(d); twop(v); v = B(v)) { // then you need its variables
     intptr_t vars = BBA(e), var = A(v);
     if (!memq(f, vars, var)) { // only add if it's not already there
      f = gx2(f, var, vars);
      if (!g_ok(f)) return forget();
      j++, BBA(e) = g_pop1(f); } }
 while (j);

 // now delete defined functions from the closure variable lists
 // they will be bound lazily when the function runs
 for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e));

 (*c)->lams = lam;
 f = append(gxl(g_push(f, 3, f->lambda, nom, exp)));

 if (!g_ok(f)) return forget();
 exp = g_pop1(f);

 size_t ll = llen(nom);
 f = ll > 1 ? g_push(f, 2, g_c1apn, gputnum(ll)) :
              g_push(f, 1, g_c1ap);
 f = gx2(f, g_nil, (*b)->stack); // push function stack rep
 (*b)->stack = g_pop1(f);
 nom = reverse(f, nom); // put in literal order
 for (v = nom; g_ok(f) && twop(B(v)); v = B(v)) { // push initial variable stack reps
  f = gx2(f, A(v), (*b)->stack);
  if (g_ok(f)) (*b)->stack = g_pop1(f); }


 nom = reverse(f, nom); // back to reverse order
 for (; twop(nom); nom = B(nom), def = B(def)) {
  if (lambp(f, A(def))) {
   d = assq(f, lam, A(nom));
   f = g_c0_lambda(f, c, BB(d), BA(def));
   if (!g_ok(f)) return forget();
   A(def) = B(d) = g_pop1(f); }
  if (even && nilp((*b)->args)) {
   f = g_c0_ix(f, g_vm_defglob, A(nom));
   if (!g_ok(f)) return forget(); }
  f = analyze(f, b, A(def));
  (*b)->stack = B((*b)->stack); }

 f = analyze(f, b, exp);
 return forget(); }


g_vm(g_vm_defglob) {
 Have(3);
 Sp -= 3;
 struct g_tab *t = dict_of(f);
 g_num k = Ip[1].x,
       v = Sp[3];
 Sp[0] = k;
 Sp[1] = v;
 Sp[2] = (g_num) t;
 Pack(f);
 f = g_tput(f);
 return !g_ok(f) ? f :
  (Unpack(f),
   Sp += 1,
   Ip += 2,
   Continue()); }

g_vm(g_vm_drop1) { return Ip++, Sp++, Continue(); }

g_vm(g_vm_freev) {
 g_num
  y = Ip[1].x,
  v = g_tget(f, y, f->dict, y); // see if it's defined now...
 return Ip[0].ap = g_vm_pushk,
        Ip[1].x = v,
        Continue(); }

g_vm(g_vm_lazyb) {
 intptr_t v = AB(Ip[1].x);
 return Ip[0].ap = g_vm_pushk,
        Ip[1].x = v,
        Continue(); }

struct ti { struct g_in in; char const *t; uintptr_t i; } ;
static int p_text_eof(struct g*f, struct g_in *j) {
 struct ti *i = (struct ti*) j;
 return !i->t[i->i]; }

static int p_text_getc(struct g*f, struct g_in *j) {
 struct ti *i = (struct ti*) j;
 char c = i->t[i->i];
 if (c) i->i++;
 return c ? c : EOF; }

static int p_text_ungetc(struct g*f, int _, struct g_in *j) {
 struct ti *i = (struct ti*) j;
 uintptr_t idx = i->i;
 return i->t[i->i = idx ? idx - 1 : idx]; }

g_noinline struct g *g_evals_(struct g*f, char const*s) {
 static char const *t = "((:(e a b)(? b(e(ev'ev(A b))(B b))a)e)0)";
 struct ti i = {{p_text_getc, p_text_ungetc, p_text_eof}, t, 0};
 f = g_eval(g_reads(f, (void*) &i));
 f = g_push(f, 3, g_nil, g_ok(f) ? f->quote : NULL, g_nil);
 i.t = s, i.i = 0;
 f = g_eval(gxr(gxl(gxr(gxl(g_reads(f, (void*) &i))))));
 if (g_ok(f)) f->sp++;
 return f; }
