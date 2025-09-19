#include "i.h"

static g_core *mo_c(g_core *f, size_t n) {
  f = g_cells(f, n + Width(struct g_tag));
  if (g_ok(f)) {
    cell *k = (cell*) f->sp[0];
    struct g_tag *t = (struct g_tag*) (k + n);
    t->null = NULL;
    t->head = k; }
  return f; }

static g_word assq(g_core *f, g_word l, g_word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
  return 0; }

static g_word memq(g_core *f, g_word l, g_word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, A(l))) return l;
  return 0; }

static g_word ldels(g_core *f, g_word lam, g_word l) {
  if (!twop(l)) return nil;
  g_word m = ldels(f, lam, B(l));
  if (!assq(f, lam, A(l))) B(l) = m, m = l;
  return m; }

static size_t llen(g_word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

static Inline bool lambp(g_core *f, g_word x) {
  return twop(x) && A(x) == (g_word) f->lambda; }

static g_core *append(g_core *f) {
  int i = 0;
  for (g_word l; g_ok(f) && twop(f->sp[0]); i++)
    l = B(f->sp[0]),
    f->sp[0] = A(f->sp[0]),
    f = g_push(f, 1, l);
  if (!g_ok(f)) return f;
  if (i == 0) return f->sp++, f;
  if (g_ok(f)) f->sp[0] = f->sp[i + 1];
  while (i--) f = g_cons_r(f);
  if (g_ok(f)) f->sp[1] = f->sp[0], f->sp++;
  return f; }

static g_word reverse(g_core *f, g_word l) {
  g_word n = nil;
  for (g_word m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
  return n; }

// function state using this type
typedef struct env {
  // these parameters represent stack state at a point in compile process
  g_word args,  // list // function positional arguments
         imps,  // list // closure variables
         stack; // list // current values on stack
  // these are values of variables known at compile time
  g_word lams; // alist // known function definitions
  // two stacks of branch addresses for conditional expressions
  g_word alts, ends;
  // this is the enclosing function env* if any
  struct env *par; } env;

static Inline long index_of_symbol(g_core *f, env *c, g_word var) {
  long l, i = 0;
  for (l = c->imps; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  for (l = c->args; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  return -1; }

static g_core *enscope(g_core *f, env* par, g_word args, g_word imps) {
  f = g_push(f, 3, args, imps, par);
  f = mo_c(f, Width(env));
  if (g_ok(f)) {
    env *c = (env*) pop1(f);
    c->args = pop1(f), c->imps = pop1(f), c->par = (env*) pop1(f),
    c->stack = c->alts = c->ends = c->lams = nil;
    push1(f, (g_word) c); }
  return f; }

static Inline g_core *g_cons_2(g_core *f, g_word a, g_word b) {
  return g_cons_l(g_push(f, 2, a, b)); }

#define Ana(n, ...) g_core *n(g_core *f, env **c, g_word x, ##__VA_ARGS__)
#define Cata(n, ...) g_core *n(g_core *f, env **c, size_t m, ##__VA_ARGS__)
typedef Ana(ana);
typedef Cata(cata);
static ana analyze, ana_if, ana_let, ana_ap_args;
static cata pull, cata_i, cata_ix, cata_var_2, cata_var, cata_ap, cata_yield, cata_ret;
static g_core *ana_seq(g_core*, env**, g_word, g_word);

// generic instruction ana handlers
static Inline g_core *ana_ix(g_core *f, g_vm *i, g_word x) {
  return g_push(f, 3, cata_ix, i, x); }

// keep this separate and NoInline so g_eval can be tail call optimized if possible
NoInline g_core *g_ana(g_core *f, g_vm *y) {
  f = enscope(f, (env*) nil, nil, nil);
  if (!g_ok(f)) return f;
  env *c = (env*) pop1(f);
  g_word x = f->sp[0];
  return
    f->sp[0] = (g_word) cata_yield, // function that returns thread from code generation
    avec(f, c,
      avec(f, x, f = ana_ix(f, y, (g_word) f->ip)),
      f = analyze(f, &c, x),
      f = pull(f, &c, 0)),
    f; }

#define Kp (f->ip)
static Cata(cata_yield) {
  f = mo_c(f, m);
  if (g_ok(f))
    Kp = cell(pop1(f)),
    memset(Kp, -1, m * sizeof(g_word)),
    Kp += m;
  return f; }

static Cata(cata_if_push_exit) {
  f = pull(f, c, m);
  f = g_cons_2(f, (g_word) Kp, (*c)->ends);
  if (g_ok(f)) (*c)->ends = pop1(f);
  return f; }

static Cata(cata_if_pop_exit) {
  f = pull(f, c, m);
  if (g_ok(f)) (*c)->ends = B((*c)->ends); // pops cond expression exit address off env stack ends
  return f; }

static Cata(cata_if_pop_branch) {
  f = pull(f, c, m + 2);
  if (g_ok(f))
    Kp -= 2,
    Kp[0].ap = cond,
    Kp[1].x = A((*c)->alts),
    (*c)->alts = B((*c)->alts);
  return f; }

static Cata(cata_if_push_branch) {
  f = pull(f, c, m);
  f = g_cons_2(f, (g_word) Kp, (*c)->alts);
  if (g_ok(f)) (*c)->alts = pop1(f);
  return f; }

static Cata(cata_if_jump_out) {
  f = pull(f, c, m + 3);
  if (g_ok(f)) {
    cell *a = cell(A((*c)->ends));
    if (a->ap == ret || a->ap == tap)
      Kp = memcpy(Kp - 2, a, 2 * sizeof(g_word));
    else if (a->ap == tapn)
      Kp = memcpy(Kp - 3, a, 3 * sizeof(g_word));
    else
      Kp -= 2,
      Kp[0].ap = jump,
      Kp[1].x = (g_word) a; }
  return f; }

static Cata(cata_ap) {
  g_word arity = pop1(f);
  f = pull(f, c, m + 2);
  if (g_ok(f)) {
    if (getnum(arity) > 1) {
      if (Kp[0].ap == ret) Kp -= 1, Kp[0].ap = tapn, Kp[1].x = arity;
      else Kp -= 2, Kp[0].ap = apn, Kp[1].x = arity; }
    else {
      if (Kp[0].ap == ret) Kp[0].ap = tap;
      else Kp -= 1, Kp[0].ap = ap; } }
  return f; }

static Cata(cata_var_2) {
  g_word var = pop1(f), stack = pop1(f);
  long i = 0;
  while (twop(stack))
    if (eql(f, A(stack), var)) break;
    else stack = B(stack), i++;
  f = pull(f, c, m + 2);
  if (g_ok(f))
    Kp -= 2,
    Kp[0].ap = ref,
    Kp[1].x = putnum(i);
  return f; }

// emit stack reference instruction
static Cata(cata_var) {
  g_word var = pop1(f), // variable name
         ins = llen(pop1(f)), // stack inset
         i = index_of_symbol(f, *c, var);
  f = pull(f, c, m + 2);
  if (g_ok(f))
    Kp -= 2,
    Kp[0].ap = ref,
    Kp[1].x = putnum(i + ins);
  return f; }

static Inline Cata(pull) { return ((cata*) pop1(f))(f, c, m); }

static Cata(cata_i) {
  g_vm *i = (g_vm*) pop1(f);
  f = pull(f, c, m + 1);
  if (g_ok(f))
    Kp -= 1,
    Kp[0].ap = i;
  return f; }

static Cata(cata_ix) {
  g_vm *i = (g_vm*) pop1(f);
  g_word x = pop1(f);
  avec(f, x, f = pull(f, c, m + 2));
  if (g_ok(f))
    Kp -= 2,
    Kp[0].ap = i,
    Kp[1].x = x;
  return f; }

static size_t arity_of(env *c) { return llen(c->args) + llen(c->imps); }
static Cata(cata_curry) {
  size_t ar = arity_of((env*) pop1(f));
  f = pull(f, c, m + 2);
  if (g_ok(f) && ar > 1)
    Kp -= 2,
    Kp[0].ap = curry,
    Kp[1].x = putnum(ar);
  return f; }

static Cata(cata_ret) {
  size_t ar = arity_of((env*) pop1(f));
  f = pull(f, c, m + 2);
  if (g_ok(f))
    Kp -= 2,
    Kp[0].ap = ret,
    Kp[1].x = putnum(ar);
  return f; }

static g_core *ana_lambda(g_core *f, env **c, g_word imps, g_word exp);
static Ana(analyze) {
  if (!g_ok(f)) return f;
  // is it a variable?
  if (symp(x)) for (env *d = *c;; d = d->par) {
    if (nilp(d)) { // free variable?
      g_word y = g_hash_get(f, 0, f->dict, x);
      if (y) return ana_ix(f, imm, y);
      f = g_cons_2(f, x, (*c)->imps);
      if (g_ok(f)) (*c)->imps = pop1(f),
                   f = ana_ix(f, free_variable, (*c)->imps);
      return f; }
    // defined as a function by a local let form?
    g_word y;
    if ((y = assq(f, d->lams, x))) return
      avec(f, y, f = ana_ap_args(f, c, BB(y))),
      ana_ix(f, late_bind, y);
    // non function definition from local let form?
    if (memq(f, d->stack, x)) return
      g_push(f, 3, cata_var_2, x, d->stack);
    // closure or positional argument on stack?
    if (memq(f, d->imps, x) || memq(f, d->args, x)) {
      if (*c != d) { // if we have found the variable in an enclosing scope then import it
        f = g_cons_2(f, x, (*c)->imps);
        if (g_ok(f)) x = pop1(f), (*c)->imps = x, x = A(x); }
      return g_push(f, 3, cata_var, x, (*c)->stack); } }

  // if it's not a variable or a list then it evals to itself
  if (!twop(x)) return ana_ix(f, imm, x);

  // it's a list
  g_word a = A(x), b = B(x);

  // singleton list?
  if (!twop(b)) return analyze(f, c, a); // value of first element

  // special form?
  if (symp(a)) {
    g_symbol *y = sym(a);
    if (y == f->quote) return ana_ix(f, imm, !twop(b) ? b : A(b));
    if (y == f->let) return !twop(B(b)) ? analyze(f, c, A(b)) :
                                          ana_let(f, c, b);
    if (y == f->begin) return ana_seq(f, c, A(b), B(b));
    if (y == f->lambda) return f = ana_lambda(f, c, nil, b),
                               g_ok(f) ? analyze(f, c, pop1(f)) : 0;
    if (y == f->cond) return !twop(B(b)) ? analyze(f, c, A(b)) :
                                           ana_if(f, c, b); }

  // macro?
  g_word mac = g_hash_get(f, 0, f->macro, a);
  if (mac) return
    f = g_push(f, 5 , nil, b, f->quote, nil, mac),
    f = g_cons_r(f),
    f = g_cons_r(f),
    f = g_cons_l(f),
    f = g_cons_r(f),
    f = g_eva(f, g_yield),
    g_ok(f) ? analyze(f, c, pop1(f)) : f;

  // application.
  return avec(f, a, f = ana_ap_args(f, c, b)),
         analyze(f, c, a); }

static g_core *ana_lambda(g_core *f, env **c, g_word imps, g_word exp) {
  f = enscope(f, *c, exp, imps);
  env *d = (env*) pop1(f);
  MM(f, &d);
  g_word x = d->args;

  // push exp args onto stack
  if (!twop(x)) f = g_push(f, 2, nil, nil);
  else { // there's at least one argument
    MM(f, &x);
    int n = 0;
    while (twop(B(x))) f = g_push(f, 1, A(x)), n++, x = B(x);
    f = g_push(f, 1, nil);
    while (n--) f = g_cons_r(f);
    UM(f);
    f = g_push(f, 1, A(x)); }

  cell *k, *ip;
  if (g_ok(f))
    exp = pop1(f),
    d->args = f->sp[0],
    f->sp[0] = word(cata_yield),
    avec(f, exp, f = g_push(f, 2, cata_ret, d)),
    f = analyze(f, &d, exp),
    f = g_push(f, 2, cata_curry, d),
    ip = f->ip,
    avec(f, ip, f = pull(f, &d, 0));

  if (g_ok(f))
    k = f->ip,
    ttag(k)->head = k,
    f->ip = ip,
    f = g_cons_2(f, word(k), d->imps);

  UM(f);
  return f; }

static Ana(ana_if_r) {
  avec(f, x, f =
    !twop(x) || !twop(B(x)) ?
      g_push(f, 1, cata_if_jump_out) :
      (f = ana_if_r(f, c, BB(x)),
       f = g_push(f, 2, cata_if_jump_out, cata_if_push_branch),
       f = analyze(f, c, AB(x)),
       g_push(f, 1, cata_if_pop_branch)));
  return analyze(f, c, twop(x) ? A(x) : nil); }

static Ana(ana_if) {
  avec(f, x, f = g_push(f, 1, cata_if_push_exit));
  f = ana_if_r(f, c, x);
  return g_push(f, 1, cata_if_pop_exit); }

static g_core *ana_seq(g_core *f, env* *c, g_word a, g_word b) {
  if (!g_ok(f)) return f;
  if (twop(b)) avec(f, a, f = ana_seq(f, c, A(b), B(b)),
                          f = g_push(f, 2, cata_i, drop1));
  return analyze(f, c, a); }

static g_core *ana_ap_args_r(g_core *f, env**c, g_word x) {
  if (!twop(x)) return f;
  avec(f, x, f = ana_ap_args_r(f, c, B(x)),
             f = g_push(f, 2, cata_ap, putnum(1)));
  return analyze(f, c, A(x)); }

// evaluate function call arguments and apply
static g_core *ana_ap_args(g_core *f, env **c, g_word x) {
  avec(f, x, f = g_cons_2(f, nil, (*c)->stack));
  if (g_ok(f))
    (*c)->stack = pop1(f),
    f = ana_ap_args_r(f, c, x),
    (*c)->stack = B((*c)->stack);
  return f; }

#define BBA(o) B(BA(o))
// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.
static g_core *ana_let(g_core *f, env **b, g_word exp) {
  struct root *mm = f->safe;
#define forget() ((f)->safe=(mm),f)
  MM(f, &exp);
  f = enscope(f, *b, (*b)->args, (*b)->imps);
  if (!g_ok(f)) return forget();
  env *q = (env*) pop1(f),
      **c = &q;
  // lots of variables :(
  g_word nom = nil, // 511 - 592
         def = nil, // 514 - 593
         lam = nil, // 520 - 579
         v = nil,   // 541 - 542
         d = nil,   // 504 - 582
         e = nil;   // 504 - 545
  MM(f, &nom), MM(f, &def), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

  // collect vars and defs into two lists
  while (twop(exp) && twop(B(exp))) {
    for (d = A(exp), e = AB(exp); twop(d); e = pop1(f), d = A(d)) {
      f = g_cons_2(f, e, nil);
      f = g_cons_2(f, word(f->lambda), B(d));
      f = append(f);
      if (!g_ok(f)) return forget(); }
    f = g_cons_2(f, d, nom);
    f = g_cons_2(f, e, def);
    if (!g_ok(f)) return forget();
    def = pop1(f);
    nom = pop1(f);
    // if it's a lambda compile it and record in lam list
    if (lambp(f, e)) {
      f = g_push(f, 2, d, lam);
      f = ana_lambda(f, c, nil, B(e));
      f = g_cons_r(f);
      f = g_cons_l(f);
      if (!g_ok(f)) return forget();
      lam = pop1(f); }
    exp = BB(exp); }

  // if there's no body then evaluate the name of the last definition
  bool even = !twop(exp); // we check this again later to make global bindings at top level
  if (even) {
    f = g_cons_2(f, A(nom), nil);
    if (!g_ok(f)) return forget();
    exp = pop1(f); }

  // find closures
  // for each function f with closure C(f)
  // for each function g with closure C(g)
  // if f in C(g) then C(g) include C(f)
  long j;
  do for (j = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
    for (e = lam; twop(e); e = B(e)) // for each bound function variable
      if (d != e && memq(f, BBA(e), AA(d))) // if you need this function
        for (v = BBA(d); twop(v); v = B(v)) { // then you need its variables
          g_word vars = BBA(e), var = A(v);
          if (!memq(f, vars, var)) { // only add if it's not already there
            f = g_cons_2(f, var, vars);
            if (!g_ok(f)) return forget();
            j++, BBA(e) = pop1(f); } }
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e));

  (*c)->lams = lam;
  f = g_push(f, 3, f->lambda, nom, exp);
  f = g_cons_l(f);
  f = append(f);

  if (!g_ok(f)) return forget();
  exp = pop1(f);

  size_t ll = llen(nom);
  f = g_push(f, 2, cata_ap, putnum(ll));
  f = g_cons_2(f, nil, (*b)->stack); // push function stack rep
  (*b)->stack = pop1(f);
  nom = reverse(f, nom); // put in literal order
  for (v = nom; twop(B(v)); v = B(v)) // push initial variable stack reps
    f = g_cons_2(f, A(v), (*b)->stack),
    (*b)->stack = pop1(f);


  nom = reverse(f, nom); // back to reverse order
  for (; twop(nom); nom = B(nom), def = B(def)) {
    if (lambp(f, A(def))) {
      d = assq(f, lam, A(nom));
      f = ana_lambda(f, c, BB(d), BA(def));
      if (!g_ok(f)) return forget();
      A(def) = B(d) = pop1(f); }
    if (even && nilp((*b)->args)) {
      f = ana_ix(f, defglob, A(nom));
      if (!g_ok(f)) return forget(); }
    f = analyze(f, b, A(def));
    (*b)->stack = B((*b)->stack); }

  f = analyze(f, b, exp);
  return forget(); }
