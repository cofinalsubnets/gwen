#include "i.h"
static struct g *g_c0(struct g *f, g_vm_t *y);

// function state using this type
struct env {
  struct env *par; // enclosing scope
  word args, imps, // positional and closure variables
   stack, // computed arguments and let bindings on stack
   lams, // lambdas defined in a local let form
   len,  // thread length accumulator
   branches, // stack for conditional alternate branch addresses
   exits; }; // stach for conditional exit addresses

#define Ana(n, ...) struct g *n(struct g *f, struct env **c, intptr_t x, ##__VA_ARGS__)
#define Cata(n, ...) struct g *n(struct g *f, struct env **c, ##__VA_ARGS__)
typedef Ana(ana);
typedef Cata(cata);
static ana analyze, g_c0_let, g_c0_apply;
static cata g_c1_i, g_c1_ix, g_c1_var, g_c1_yield, g_c1_ret, g_c1;
static g_inline Cata(pull) { return ((cata*) pop1(f))(f, c); }

#define incl(e, n) ((e)->len += ((n)<<1))
// generic instruction ana handlers
static g_inline struct g *g_c0_ix(struct g *f, struct env **c, g_vm_t *i, word x) { 
 return incl(*c, 2), g_push(f, 3, g_c1_ix, i, x); }

static g_inline struct g *g_c0_i(struct g *f, struct env **c, g_vm_t *i) {
 return incl(*c, 1), g_push(f, 2, g_c1_i, i); }

static g_noinline struct g *enscope(struct g *f, struct env *par, word args, word imps) {
 f = g_push(f, 3, args, imps, par);
 uintptr_t n = Width(struct env);
 f = g_have(f, n + Width(struct g_tag));
 if (g_ok(f)) {
  union u *k = bump(f, n + Width(struct g_tag));
  struct g_tag *t = (struct g_tag*) (k + n);
  t->null = NULL;
  t->head = k;
  struct env *c = (struct env*) k;
  c->stack = c->branches = c->exits = c->lams = c->len = nil;
  c->args = f->sp[0],
  c->imps = f->sp[1],
  c->par = (struct env*) f->sp[2];
  f->sp[2] = (word) c,
  f->sp += 2; }
 return f; }

static word memq(struct g *f, word l, word k) {
 for (; twop(l); l = B(l)) if (eql(f, k, A(l))) return l;
 return 0; }

static word assq(struct g *f, word l, word k) {
 for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
 return 0; }

static struct g *append(struct g *f) {
 uintptr_t i = 0;
 for (word l; g_ok(f) && twop(f->sp[0]); i++)
  l = B(f->sp[0]),
  f->sp[0] = A(f->sp[0]),
  f = g_push(f, 1, l);
 if (!g_ok(f)) return f;
 if (i == 0) return f->sp++, f;
 if (g_ok(f)) f->sp[0] = f->sp[i + 1];
 while (i--) f = gxr(f);
 if (g_ok(f)) f->sp[1] = f->sp[0], f->sp++;
 return f; }

static size_t llen(word l) {
 size_t n = 0;
 while (twop(l)) n++, l = B(l);
 return n; }

static struct g *g_c0_lambda(struct g*, struct env**, word, word);

// don't inline this so callers can tail call optimize
static g_noinline struct g *g_c0(struct g *f, g_vm_t *y) {
 f = enscope(f, (struct env*) nil, nil, nil);
 if (!g_ok(f)) return f;
 struct env *c = (void*) ptr(pop1(f));
 word x = f->sp[0];
 f->sp[0] = (word) g_c1_yield;
 MM(f, &c); MM(f, &x);
 f = analyze(f, &c, x);
 f = g_c0_ix(f, &c, y, word(f->ip));
 return UM(f), f = g_c1(f, &c), UM(f), f; }


#define Kp (f->ip)
static Cata(g_c1) {
 uintptr_t l = getnum((*c)->len);
 f = g_have(f, l + Width(struct g_tag));
 if (g_ok(f)) {
  union u *k = bump(f, l + Width(struct g_tag));
  struct g_tag *t = (void*) (k + l);
  t->null = NULL;
  t->head = memset(k, -1, l * sizeof(word));
  Kp = (void*) t;
  f = pull(f, c); }
 if (g_ok(f)) clip(f->ip);
 return f; }


static Cata(g_c1_yield) { return f; }

static Cata(g_c1_cond_pop_exit) { return
 (*c)->exits = B((*c)->exits), // pops cond expression exit address off env stack exits
 pull(f, c); }

static Cata(g_c1_apn) {
 word arity = pop1(f);
 if (arity == g_putnum(1)) {
  if (Kp[0].ap == g_vm_ret) Kp[0].ap = g_vm_tap;
  else Kp -= 1, Kp[0].ap = g_vm_ap; }
 else {
  if (Kp[0].ap == g_vm_ret) Kp -= 1, Kp[0].ap = g_vm_tapn, Kp[1].x = arity;
  else Kp -= 2, Kp[0].ap = g_vm_apn, Kp[1].x = arity; }
 return pull(f, c); }

static Cata(g_c1_var_, word i) { return
 Kp -= 2,
 Kp[0].ap = g_vm_arg,
 Kp[1].x = putnum(i),
 pull(f, c); }

static Cata(g_c1_var_2) {
 num var = pop1(f),
     stack = pop1(f),
     i = 0;
 while (twop(stack))
  if (eql(f, A(stack), var)) break;
  else stack = B(stack), i++;
 return g_c1_var_(f, c, i); }

static Cata(g_c1_var) {
 word l, v = pop1(f),
         i = llen(pop1(f)); // stack inset
 for (l = (*c)->imps; !nilp(l); l = B(l), i++)
  if (eql(f, v, A(l))) return g_c1_var_(f, c, i);
 for (l = (*c)->args; !nilp(l); l = B(l), i++)
  if (eql(f, v, A(l))) return g_c1_var_(f, c, i);
 return g_c1_var_(f, c, i); }

static Cata(g_c1_i) {
 g_vm_t *i = (void*) pop1(f);
 Kp -= 1;
 Kp[0].ap = i;
 return pull(f, c); }

static Cata(g_c1_ix) {
 g_vm_t *i = (void*) pop1(f);
 word x = pop1(f);
 Kp -= 2;
 Kp[0].ap = i;
 Kp[1].x = x;
 return pull(f, c); }

static Cata(g_c1_ar, g_vm_t *i, word ar) { return
 Kp -= 2,
 Kp[0].ap = i,
 Kp[1].x = putnum(ar),
 pull(f, c); }

static Cata(g_c1_curry) {
 struct env *e = (void*) pop1(f);
 uintptr_t ar = llen(e->args) + llen(e->imps);
 return ar == 1 ? pull(f, c) : g_c1_ar(f, c, g_vm_curry, ar); }

#define C1(n, ...) static Cata(n) { return __VA_ARGS__, pull(f, c); }
static Cata(g_c1_ret) {
 struct env *e = (struct env*) pop1(f);
 uintptr_t ar = llen(e->args) + llen(e->imps);
 return g_c1_ar(f, c, g_vm_ret, ar); }

C1(g_c1_cond_push_branch, f = gxl(g_push(f, 2, Kp, (*c)->branches)), (*c)->branches = g_ok(f) ? pop1(f) : nil)
C1(g_c1_cond_push_exit, f = gxl(g_push(f, 2, Kp, (*c)->exits)), (*c)->exits = g_ok(f) ? pop1(f) : nil)
C1(g_c1_cond_pop_branch, Kp -= 2, Kp[0].ap = g_vm_cond, Kp[1].x = A((*c)->branches), (*c)->branches = B((*c)->branches))

static Cata(g_c1_cond_exit) {
 union u *a = cell(A((*c)->exits));
 if (a->ap == g_vm_ret || a->ap == g_vm_tap)
  Kp = memcpy(Kp - 2, a, 2 * sizeof(*Kp));
 else if (a->ap == g_vm_tapn)
  Kp = memcpy(Kp - 3, a, 3 * sizeof(*Kp));
 else
  Kp -= 2, Kp[0].ap = g_vm_jump, Kp[1].x = (word) a;
 return pull(f, c); }

static g_vm(g_vm_yieldk) { return
 Ip = Ip[1].m,
 Pack(f),
 encode(f, g_status_yield); }

static struct g *gev(struct g *f) {
 f = g_c0(f, g_vm_yieldk);
#if g_tco
 if (g_ok(f)) f = f->ip->ap(f, f->ip, f->hp, f->sp);
#else
 while (g_ok(f)) f = f->ip->ap(f);
 if (g_code_of(f) == g_status_eof) f = g_core_of(f);
#endif
 return f; }

g_vm(g_vm_lazyb) { return
 Ip[0].ap = g_vm_quote,
 Ip[1].x = AB(Ip[1].x),
 Continue(); }

static Ana(g_c0_cond_r);

static Ana(g_c0_var) {
 word y;
 for (struct env *d = *c;; d = d->par) {
  if (nilp(d)) return (y = g_tget(f, 0, dict_of(f), x)) ?
   g_c0_ix(f, c, g_vm_quote, y) :
   (f = gxl(g_push(f, 2, x, (*c)->imps)),
    f = g_c0_ix(f, c, g_vm_freev, (*c)->imps = g_ok(f) ? pop1(f) : nil),
    f);
  // lambda definition of local let form?
  if ((y = assq(f, d->lams, x))) return
    f = g_c0_ix(f, c, g_vm_lazyb, y),
    g_c0_apply(f, c, BB(f->sp[2]));
  // other definition of local let form?
  if (memq(f, d->stack, x)) return
   incl(*c, 2),
   g_push(f, 3, g_c1_var_2, x, d->stack);
  // closure or lambda argument?
  if (memq(f, d->imps, x) || memq(f, d->args, x)) {
   incl(*c, 2);
   if (d != *c) // if we have found the variable in an enclosing scope then import it
    f = gxl(g_push(f, 2, x, (*c)->imps)),
    x = g_ok(f) ? A((*c)->imps = pop1(f)) : nil;
   return g_push(f, 3, g_c1_var, x, (*c)->stack); } } }

static g_noinline Ana(analyze) {
 if (!g_ok(f)) return f; // error...
 if (symp(x)) return g_c0_var(f, c, x); // variable
 if (!twop(x)) return g_c0_ix(f, c, g_vm_quote, x); // self quoting expression
 // it is a pair
 word a = A(x), b = B(x);
 // singleton list has value of element
 if (!twop(b)) return analyze(f, c, a);
 // if it is a special form then do that
 if (symp(a) && nom(a)->nom && len(nom(a)->nom) == 1)
  switch (*txt(nom(a)->nom)) {
   case '`':
    return g_c0_ix(f, c, g_vm_quote, !twop(b) ? nil : A(b));
   case '\\': return
    f = g_c0_lambda(f, c, nil, b),
    analyze(f, c, g_ok(f) ? pop1(f) : 0);
   case ':': return g_c0_let(f, c, b);
   case '?':
    if (!twop(B(b))) return analyze(f, c, A(b));
    f = g_push(f, 2, b, g_c1_cond_pop_exit);
    f = g_c0_cond_r(f, c, g_ok(f) ? pop1(f) : nil);
    return g_push(f, 1, g_c1_cond_push_exit); }

 // if it is a macro then apply the macro and analyze the result
 if ((x = g_tget(f, 0, f->macro, a))) return
  f = gev(gxr(gxl(gxr(gxl(g_push(f, 5, b, nil ,f->quote, nil, x)))))),
  analyze(f, c, g_ok(f) ? pop1(f) : 0);
 return // apply function to arguments
  avec(f, b, f = analyze(f, c, a)),
  g_c0_apply(f, c, b); }


static struct g *g_c0_lambda(struct g *f, struct env **c, intptr_t imps, intptr_t exp) {
 union u *k, *ip;
 struct env *d = NULL;
 MM(f, &d); MM(f, &exp);
 f = enscope(f, *c, exp, imps);

 if (g_ok(f)) {
  d = (struct env*) pop1(f);
  exp = d->args;
  int n = 0; // push exp args onto stack
  for (; twop(B(exp)); exp = B(exp), n++)
   f = g_push(f, 1, A(exp));
  for (f = g_push(f, 1, nil); n--; f = gxr(f));
  exp = A(exp); }

 if (g_ok(f))
  d->args = f->sp[0],
  f->sp[0] = (word) g_c1_yield,
  incl(d, 4),
  f = g_push(f, 2, g_c1_curry, d),
  f = analyze(f, &d, exp),
  f = g_push(f, 2, g_c1_ret, d),
  ip = f->ip,
  avec(f, ip, f = g_c1(f, &d));

 if (g_ok(f))
  k = f->ip,
  f->ip = ip,
  f = gxl(g_push(f, 2, k, d->imps));

 return UM(f), UM(f), f; }

static Ana(g_c0_cond_exit) { return
 incl(*c, 3),
 g_push(analyze(f, c, x), 1, g_c1_cond_exit); }

static Ana(g_c0_cond_r) { return
 !twop(x) ? g_c0_cond_exit(f, c, nil) :
 !twop(B(x)) ? g_c0_cond_exit(f, c, A(x)) :
 (avec(f, x,
   incl(*c, 2),
   f = analyze(f, c, A(x)),
   f = g_push(f, 1, g_c1_cond_pop_branch),
   f = g_c0_cond_exit(f, c, AB(x)),
   f = g_push(f, 1, g_c1_cond_push_branch),
   f = g_c0_cond_r(f, c, BB(x))), f); }


static struct g *g_c0_apr2l(struct g *f, struct env **c, word x);
// TODO move optimizations to self
static struct g *g_c0_apply(struct g *f, struct env **c, intptr_t x) {
 bool is_immediate_function =
  f->sp[0] == (word) g_c1_ix &&
  f->sp[1] == (word) g_vm_quote &&
  even(f->sp[2]);
 intptr_t
   call_arity = llen(x),
   value_arity =
    is_immediate_function && cell(f->sp[2])->ap == g_vm_curry ?
     getnum(cell(f->sp[2])[1].x) :
     1;
 bool is_unary_bif =
  call_arity == 1 &&
  is_immediate_function &&
  cell(f->sp[2])[1].ap == g_vm_ret0,
  is_n_ary_ap = value_arity == call_arity && call_arity > 1,
  is_n_ary_bif = is_n_ary_ap && cell(f->sp[2])[3].ap == g_vm_ret0;
  // inline a unary bif
 if (is_unary_bif) {
  g_vm_t *i = cell(f->sp[2])->ap;
  f->sp += 3;
  f = g_c0_i(analyze(f, c, A(x)), c, i);
  return f; }

 if (is_n_ary_bif) {
  g_vm_t *i = cell(f->sp[2])[2].ap;
  f->sp += 3;
  f = g_c0_i(g_c0_apr2l(f, c, x), c, i);
  if (g_ok(f)) while (call_arity--) (*c)->stack = B((*c)->stack);
  return f; }

 f = gxl(g_push(f, 3, nil, (*c)->stack, x));
 if (g_ok(f)) {
  (*c)->stack = pop1(f), x = pop1(f);
  MM(f, &x);
  // one right-to-left n-ary application
  if (is_n_ary_ap)
   for (f = g_c0_apr2l(f, c, x),
        incl(*c, 2),
        f = g_push(f, 2, g_c1_apn, putnum(call_arity));
        call_arity--;
        (*c)->stack = g_ok(f) ? B((*c)->stack) : nil);
  // n left-to-right unary application
  else while (twop(x))
   f = analyze(f, c, A(x)),
   incl(*c, 2),
   f = g_push(f, 2, g_c1_apn, putnum(1)),
   x = B(x);
  UM(f);
  (*c)->stack = B((*c)->stack); }

 return f; }


static struct g *g_c0_apr2l(struct g *f, struct env **c, word x) {
 if (twop(x)) {
  word y = A(x);
  avec(f, y, f = g_c0_apr2l(f, c, B(x)));
  f = analyze(f, c, y);
  f = gxl(g_push(f, 2, nil, (*c)->stack));
  if (g_ok(f)) (*c)->stack = pop1(f); }
 return f; }

static bool lambp(struct g *f, word x) {
 if (!twop(x) || !symp(A(x)) || !twop(B(x))) return false;
 struct g_vec *n = sym(A(x))->nom;
 return n && len(n) == 1 && *txt(n) == '\\'; }

static word reverse(word l) {
 word n = nil;
 for (word m; twop(l); m = l, l = B(l), B(m) = n, n = m);
 return n; }

static word ldels(struct g *f, word lam, word l);
// this is the longest C function :(
// it handles the let special form in a way to support sequential and recursive binding.
static struct g *g_c0_let(struct g *f, struct env **b, word exp) {
 if (!twop(B(exp))) return analyze(f, b, A(exp));
 struct g_root *mm = f->root;
#define forget() (g_core_of(f)->root=(mm),f)
 MM(f, &exp);
 f = enscope(f, *b, (*b)->args, (*b)->imps);
 if (!g_ok(f)) return forget();
 struct env *q = (struct env*) pop1(f), **c = &q;
 // lots of variables :(
 word nom = nil, def = nil, lam = nil,
      v = nil, d = nil, e = nil;
 MM(f, &nom), MM(f, &def), MM(f, &lam);
 MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

 // collect vars and defs into two lists
 while (twop(exp) && twop(B(exp))) {
  for (d = A(exp), e = AB(exp); twop(d); e = pop1(f), d = A(d)) {
   f = gxl(g_push(f, 2, e, nil));
   f = append(gxl(g_push(f, 2, f->lambda, B(d))));
   if (!g_ok(f)) return forget(); }
  f = gxl(g_push(f, 2, d, nom));
  f = gxl(g_push(f, 2, e, def));
  if (!g_ok(f)) return forget();
  def = pop1(f), nom = pop1(f);
  // if it's a lambda compile it and record in lam list
  if (lambp(f, e)) {
   f = g_push(f, 2, d, lam);
   f = gxl(gxr(g_c0_lambda(f, c, nil, B(e))));
   if (!g_ok(f)) return forget();
   lam = pop1(f); }
  exp = BB(exp); }

 intptr_t ll = llen(nom);
 bool odd = twop(exp),
      even = !odd,
      top_def = even && nilp((*b)->args); // we check this again later to make global bindings at top level
 if (even) { // if there's no body then evaluate the name of the last definition
  f = gxl(g_push(f, 2, A(nom), nil));
  if (!g_ok(f)) return forget();
  exp = pop1(f); }

 // find closures
 // for each function f with closure C(f)
 // for each function g with closure C(g)
 // if f in C(g) then C(g) include C(f)
 word j, vars, var;
 do for (j = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
  for (e = lam; twop(e); e = B(e)) if (d != e) // for each other bound function variable
   if (memq(f, BB(A(e)), AA(d))) // if you need this function
    for (v = BB(A(d)); twop(v); v = B(v)) // then you need its variables
     if (!memq(f, vars = BB(A(e)), var = A(v))) // only add if it's not already there
      j++,
      f = gxl(g_push(f, 2, var, vars)),
      BB(A(e)) = g_ok(f) ? pop1(f) : nil;
 while (j);

 // now delete defined functions from the closure variable lists
 // they will be bound lazily when the function runs
 for (e = lam; twop(e); BB(A(e)) = ldels(f, lam, BB(A(e))), e = B(e));

 (*c)->lams = lam;
 f = append(gxl(g_push(f, 3, f->lambda, nom, exp)));

 if (!g_ok(f)) return forget();
 exp = pop1(f);

 //
 // all the code emissions are below here (??)
 //

 for (e = nom, v = def; twop(e); e = B(e), v = B(v))
  if (lambp(f, A(v))) {
   d = assq(f, lam, A(e));
   f = g_c0_lambda(f, c, BB(d), BA(v));
   if (!g_ok(f)) return forget();
   A(v) = B(d) = pop1(f); }

 nom = reverse(nom); // put in literal order
 f = analyze(f, b, exp);
 f = gxl(g_push(f, 2, nil, e = (*b)->stack)); // push function stack rep
 (*b)->stack = g_ok(f) ? pop1(f) : nil;
 for (def = reverse(def); twop(nom); nom = B(nom), def = B(def))
  f = analyze(f, b, A(def)),
  f = top_def ? g_c0_ix(f, b, g_vm_defglob, A(nom)) : f,
  f = gxl(g_push(f, 2, A(nom), (*b)->stack)),
  (*b)->stack = g_ok(f) ? pop1(f) : nil;
 return
  (*b)->stack = e,
  incl(*b, 2),
  f = g_push(f, 2, g_c1_apn, putnum(ll)),
  forget(); }

static word ldels(struct g *f, word lam, word l) {
 if (!twop(l)) return nil;
 word m = ldels(f, lam, B(l));
 if (!assq(f, lam, A(l))) B(l) = m, m = l;
 return m; }

g_vm(g_vm_defglob) {
 Have(3);
 Sp -= 3;
 struct g_tab *t = dict_of(f);
 word k = Ip[1].x, v = Sp[3];
 Sp[0] = k, Sp[1] = v, Sp[2] = (word) t;
 Pack(f);
 if (!g_ok(f = g_tput(f))) return f;
 Unpack(f);
 Sp += 1;
 Ip += 2;
 return Continue(); }

g_vm(g_vm_drop1) { return Ip++, Sp++, Continue(); }

g_vm(g_vm_freev) { return
 Ip[0].ap = g_vm_quote,
 Ip[1].x = g_tget(f, Ip[1].x, f->dict, Ip[1].x),
 Continue(); }

g_vm(g_vm_eval) { return
 Ip++,
 Pack(f),
 f = g_c0(f, g_vm_jump),
 !g_ok(f) ? f : (Unpack(f),
                 Continue()); }

struct ti { struct g_in in; char const *t; word i; } ;
static int _eof(struct g*f, struct ti *i) { return !i->t[i->i]; }
static int _getc(struct g*f, struct ti *i) { return _eof(f, i) ? EOF : i->t[i->i++]; }
static int _ungetc(struct g*f, int _, struct ti *i) { return i->t[i->i = i->i ? i->i - 1 : i->i]; }
g_noinline struct g *g_evals_(struct g*f, char const*s) {
 static char const *t = "((:(e a b)(? b(e(ev'ev(A b))(B b))a)e)0)";
 struct ti i = {{(void*)_getc, (void*)_ungetc, (void*)_eof}, t, 0};
 f = gev(g_reads(f, (void*) &i));
 f = g_push(f, 3, nil, g_ok(f) ? f->quote : NULL, nil);
 i.t = s, i.i = 0;
 f = gev(gxr(gxl(gxr(gxl(g_reads(f, (void*) &i))))));
 if (g_ok(f)) f->sp++;
 return f; }

