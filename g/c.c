#include "i.h"
int
 memcmp(void const*, void const*, size_t);
void
 *malloc(size_t),
 free(void*),
 *memcpy(void*restrict, void const*restrict, size_t),
 *memset(void*, int, size_t);

// function state using this type
struct env {
  struct env *par;
  // these parameters represent stack state at a point in compile process
  intptr_t
    args,  // list  // function positional arguments
    imps,  // list  // closure variables
    stack, // list  // current values on stack
    lams,  // alist // known function definitions
    len,
    alts, ends; };

#define Ana(n, ...) struct g *n(struct g *f, struct env **c, intptr_t x, ##__VA_ARGS__)
#define Cata(n, ...) struct g *n(struct g *f, struct env **c, ##__VA_ARGS__)
typedef Ana(ana);
typedef Cata(cata);
static ana analyze, g_c0_if, g_c0_let, g_c0_apargs;
static cata pull, g_c1_i, g_c1_ix, g_c1_var, g_c1_ap, g_c1_yield, g_c1_ret, g_c1_alloc;

#define incl(e, n) ((e)->len += ((n)<<1))
// generic instruction ana handlers
static struct g *g_c0_ix(struct g *f, struct env **c, g_vm_t *i, g_num x) {
 incl(*c, 2);
 return g_push(f, 3, g_c1_ix, i, x); }

static struct g *g_c0_i(struct g *f, struct env **c, g_vm_t *i) {
 incl(*c, 1);
 return g_push(f, 2, g_c1_i, i); }


static g_noinline struct g *enscope(struct g *f, struct env *par, g_num args, g_num imps) {
 f = g_push(f, 3, args, imps, par);
 uintptr_t n = Width(struct env);
 f = g_have(f, n + Width(struct g_tag));
 if (g_ok(f)) {
  union u *k = bump(f, n + Width(struct g_tag));
  struct g_tag *t = (struct g_tag*) (k + n);
  t->null = NULL;
  t->head = k;
  struct env *c = (struct env*) k;
  c->stack = c->alts = c->ends = c->lams = c->len = g_nil;
  c->args = f->sp[0],
  c->imps = f->sp[1],
  c->par = (struct env*) f->sp[2];
  f->sp[2] = (g_num) c,
  f->sp += 2; }
 return f; }

static struct g *gx2(struct g *f, g_num a, g_num b) {
 return gxl(g_push(f, 2, a, b)); }

static g_num memq(struct g *f, g_num l, g_num k) {
 for (; twop(l); l = B(l)) if (eql(f, k, A(l))) return l;
 return 0; }

static g_num assq(struct g *f, g_num l, g_num k) {
 for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
 return 0; }

static struct g *append(struct g *f) {
 uintptr_t i = 0;
 for (g_num l; g_ok(f) && twop(f->sp[0]); i++)
  l = B(f->sp[0]),
  f->sp[0] = A(f->sp[0]),
  f = g_push(f, 1, l);
 if (!g_ok(f)) return f;
 if (i == 0) return f->sp++, f;
 if (g_ok(f)) f->sp[0] = f->sp[i + 1];
 while (i--) f = gxr(f);
 if (g_ok(f)) f->sp[1] = f->sp[0], f->sp++;
 return f; }

static size_t llen(g_num l) {
 size_t n = 0;
 while (twop(l)) n++, l = B(l);
 return n; }

static struct g
 *g_c0_lambda(struct g*, struct env**, g_num, g_num),
 *g_c0_seq(struct g*, struct env**, g_num, g_num);

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
    f = g_c0_ix(f, &c, y, word(f->ip))),
   f = analyze(f, &c, x),
   f = g_c1_alloc(f, &c),
   f = pull(f, &c)),
  f; }

#define Kp (f->ip)
static Cata(g_c1_alloc) {
 uintptr_t l = g_getnum((*c)->len);
 f = g_have(f, l + Width(struct g_tag));
 if (g_ok(f)) {
  union u *k = bump(f, l + Width(struct g_tag));
  struct g_tag *t = (struct g_tag*) (k + l);
  t->null = NULL;
  t->head = k;
  memset(k, -1, l * sizeof(g_num));
  Kp = k + l; }
 return f; }

static Cata(g_c1_yield) {
 return f; }

static Cata(g_c1_if_pop_exit) {
  f = pull(f, c);
  if (g_ok(f)) (*c)->ends = B((*c)->ends); // pops cond expression exit address off env stack ends
  return f; }

static Cata(g_c1_ap) {
 f = pull(f, c);
 if (g_ok(f)) {
  if (Kp[0].ap == g_vm_ret) Kp[0].ap = g_vm_tap;
  else Kp -= 1, Kp[0].ap = g_vm_ap; }
 return f; }

static Cata(g_c1_apn) {
 g_num arity = g_pop1(f);
 f = pull(f, c);
 if (g_ok(f)) {
  if (Kp[0].ap == g_vm_ret) Kp -= 1, Kp[0].ap = g_vm_tapn, Kp[1].x = arity;
  else Kp -= 2, Kp[0].ap = g_vm_apn, Kp[1].x = arity; }
 return f; }

static Cata(g_c1_ix_, g_vm_t *i, g_num x) {
 avec(f, x, f = pull(f, c));
 if (g_ok(f))
  Kp -= 2,
  Kp[0].ap = i,
  Kp[1].x = x;
 return f; }

static Cata(g_c1_var_2) {
 g_num var = g_pop1(f),
       stack = g_pop1(f),
       i = 0;
 while (twop(stack))
  if (eql(f, A(stack), var)) break;
  else stack = B(stack), i++;
 return g_c1_ix_(f, c, g_vm_pushr, gputnum(i)); }

// emit stack g_vm_pushrerence instruction
static Cata(g_c1_var) {
 g_num l, v = g_pop1(f), // variable name
          i = llen(g_pop1(f)); // stack inset
 for (l = (*c)->imps; !nilp(l); l = B(l), i++)
  if (eql(f, v, A(l))) goto out;
 for (l = (*c)->args; !nilp(l); l = B(l), i++)
  if (eql(f, v, A(l))) goto out;
out:
 return g_c1_ix_(f, c, g_vm_pushr, gputnum(i)); }

static g_inline Cata(pull) {
 return ((cata*) g_pop1(f))(f, c); }

static Cata(g_c1_i) {
 g_vm_t *i = (g_vm_t*) g_pop1(f);
 f = pull(f, c);
 if (g_ok(f)) Kp -= 1, Kp[0].ap = i;
 return f; }

static Cata(g_c1_ix) {
 g_vm_t *i = (g_vm_t*) g_pop1(f);
 g_num x = g_pop1(f);
 return g_c1_ix_(f, c, i, x); }

static Cata(g_c1_curry) {
 struct env *e = (struct env*) g_pop1(f);
 uintptr_t ar = llen(e->args) + llen(e->imps);
 return ar == 1 ?  pull(f, c) :
  g_c1_ix_(f, c, g_vm_curry, gputnum(ar)) ; }

static Cata(g_c1_ret) {
 struct env *e = (struct env*) g_pop1(f);
 uintptr_t ar = llen(e->args) + llen(e->imps);
 return g_c1_ix_(f, c, g_vm_ret, gputnum(ar)); }

static Cata(g_c1_if_push_branch) {
 f = pull(f, c);
 f = gx2(f, (g_num) Kp, (*c)->alts);
 if (g_ok(f)) (*c)->alts = g_pop1(f);
 return f; }

static Cata(g_c1_if_push_exit) {
 f = pull(f, c);
 f = gx2(f, (g_num) Kp, (*c)->ends);
 if (g_ok(f)) (*c)->ends = g_pop1(f);
 return f; }


static Cata(g_c1_if_pop_branch) {
 f = pull(f, c);
 if (g_ok(f))
  Kp -= 2,
  Kp[0].ap = g_vm_cond,
  Kp[1].x = A((*c)->alts);
 (*c)->alts = B((*c)->alts);
 return f; }

static Cata(g_c1_if_jump_out) {
 f = pull(f, c);
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

static struct g *g_eval(struct g *f) {
 f = g_c0(f, g_vm_yieldk);
#if g_tco
 return g_ok(f) ? f->ip->ap(f, f->ip, f->hp, f->sp) : f; }
#else
 while (g_ok(f)) f = f->ip->ap(f);
 return g_code_of(f) == g_status_eof ? g_core_of(f) : f; }
#endif

static Ana(g_c0_sym) {
 for (struct env *d = *c;; d = d->par) {
  if (nilp(d)) { // free variable?
   g_num y = g_tget(f, 0, dict_of(f), x);
   if (y) return g_c0_ix(f, c, g_vm_pushk, y);
   f = gx2(f, x, (*c)->imps);
   if (g_ok(f)) (*c)->imps = g_pop1(f),
                f = g_c0_ix(f, c, g_vm_freev, (*c)->imps);
   return f; }
  // defined as a function by a local let form?
  g_num y;
  if ((y = assq(f, d->lams, x))) return
   avec(f, y, f = g_c0_apargs(f, c, BB(y))),
   g_c0_ix(f, c, g_vm_lazyb, y);
  // non function definition from local let form?
  if (memq(f, d->stack, x)) return
   incl(*c,2),
   g_push(f, 3, g_c1_var_2, x, d->stack);
  // closure or positional argument?
  if (memq(f, d->imps, x) || memq(f, d->args, x)) {
   if (*c != d) { // if we have found the variable in an enclosing scope then import it
    f = gx2(f, x, (*c)->imps);
    if (g_ok(f)) x = A((*c)->imps = g_pop1(f)); }
   incl(*c, 2);
   return g_push(f, 3, g_c1_var, x, (*c)->stack); } } }

static Ana(g_c0_apm, g_num b) {
 f = g_push(f, 5, b, g_nil ,f->quote, g_nil, x);
 f = g_eval(gxr(gxl(gxr(gxl(f)))));
 return analyze(f, c, g_ok(f) ? g_pop1(f) : 0); }

static Ana(g_c0_ap, g_num b) {
 avec(f, x, f = g_c0_apargs(f, c, b)); // XXX swap next line
 return analyze(f, c, x); }

static g_noinline Ana(analyze) {
 if (!g_ok(f)) return f; // error...
 if (symp(x)) return g_c0_sym(f, c, x); // variable
 if (!twop(x)) return g_c0_ix(f, c, g_vm_pushk, x); // self quoting expression
 g_num a = A(x), b = B(x);
 // singleton list?
 if (!twop(b)) return analyze(f, c, a); // value of first element
 if (symp(a) && nom(a)->nom && len(nom(a)->nom) == 1) switch (*txt(nom(a)->nom)) {
  case '`': return g_c0_ix(f, c, g_vm_pushk, !twop(b) ? b : A(b));
  case ':': return g_c0_let(f, c, b);
  case ',': return g_c0_seq(f, c, A(b), B(b));
  case '?': return g_c0_if(f, c, b);
  case '\\': return f = g_c0_lambda(f, c, g_nil, b),
                    analyze(f, c, g_ok(f) ? g_pop1(f) : 0); }
 x = g_tget(f, 0, f->macro, a);
 return x ? g_c0_apm(f, c, x, b) :
            g_c0_ap(f, c, a, b); }

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
  incl(d, 4),
  avec(f, exp, f = g_push(f, 2, g_c1_ret, d)),
  f = analyze(f, &d, exp),
  f = g_push(f, 2, g_c1_curry, d),
  ip = f->ip,
  avec(f, ip,
   f = g_c1_alloc(f, &d),
   f = pull(f, &d));

 if (g_ok(f))
  k = f->ip,
  ttag(k)->head = k, // trim
  f->ip = ip,
  f = gx2(f, word(k), d->imps);

 return UM(f), f; }

static Ana(g_c0_if_r) {
 incl(*c, 5);
 avec(f, x, f =
  !twop(x) || !twop(B(x)) ?
    g_push(f, 1, g_c1_if_jump_out) :
    (f = g_c0_if_r(f, c, BB(x)),
     f = g_push(f, 2, g_c1_if_jump_out, g_c1_if_push_branch),
     f = analyze(f, c, AB(x)),
     g_push(f, 1, g_c1_if_pop_branch)));
 return analyze(f, c, twop(x) ? A(x) : g_nil); }

static Ana(g_c0_if) {
 if (!twop(B(x))) return analyze(f, c, A(x));
 avec(f, x, f = g_push(f, 1, g_c1_if_push_exit));
 f = g_c0_if_r(f, c, x);
 return g_push(f, 1, g_c1_if_pop_exit); }

static struct g *g_c0_seq(struct g*f, struct env**c, intptr_t a, intptr_t b) {
 if (g_ok(f) && twop(b))
  avec(f, a,
   f = g_c0_seq(f, c, A(b), B(b)),
   f = g_c0_i(f, c, g_vm_drop1));
 return analyze(f, c, a); }

static struct g *g_c0_apargsr(struct g *f, struct env**c, intptr_t x) {
 return !twop(x) ? f : (avec(f, x, incl(*c, 1),
                                   f = g_c0_apargsr(f, c, B(x)),
                                   f = g_push(f, 1, g_c1_ap)),
                        analyze(f, c, A(x))); }

static g_num reverse(struct g *f, g_num l) {
 g_num n = g_nil;
 for (g_num m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
 return n; }

// evaluate function call arguments and apply
static struct g *g_c0_apargs(struct g *f, struct env **c, intptr_t x) {
 f = gxl(g_push(f, 3, g_nil, (*c)->stack, x));
 if (g_ok(f)) {
  (*c)->stack = g_pop1(f);
  x = g_pop1(f);
  f = g_c0_apargsr(f, c, x);
  (*c)->stack = B((*c)->stack); }
 return f; }

static g_num ldels(struct g *f, g_num lam, g_num l) {
 if (!twop(l)) return g_nil;
 g_num m = ldels(f, lam, B(l));
 if (!assq(f, lam, A(l))) B(l) = m, m = l;
 return m; }

static bool lambp(struct g *f, g_num x) {
 if (!twop(x) || !symp(A(x))) return false;
 struct g_vec *n = sym(A(x))->nom;
 return n && len(n) == 1 && *txt(n) == '\\'; }

// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.
static struct g *g_c0_let(struct g *f, struct env **b, g_num exp) {
 if (!twop(B(exp))) return analyze(f, b, A(exp));
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

 // all the code emissions are below here (??)
 incl(*b, 2);
 intptr_t ll = llen(nom);
 f = ll > 1 ? g_push(f, 2, g_c1_apn, gputnum(ll)) :
              g_push(f, 1, g_c1_ap);
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
   f = g_c0_ix(f, b, g_vm_defglob, A(nom));
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

g_vm(g_vm_freev) { return
 Ip[0].ap = g_vm_pushk,
 Ip[1].x = g_tget(f, Ip[1].x, f->dict, Ip[1].x),
 Continue(); }

g_vm(g_vm_lazyb) { return
 Ip[0].ap = g_vm_pushk,
 Ip[1].x = AB(Ip[1].x),
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
