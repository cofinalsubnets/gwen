#include "i.h"
#include <assert.h>

static g_core *mo_c(core *f, size_t n) {
  f = g_cells(f, n + Width(struct tag));
  if (g_ok(f)) {
    cell *k = (cell*) f->sp[0];
    struct tag *t = (struct tag*) (k + n);
    t->null = NULL;
    t->head = k; }
  return f; }

static word assq(core *f, word l, word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
  return 0; }

static word memq(core *f, word l, word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, A(l))) return l;
  return 0; }

static word ldels(core *f, word lam, word l) {
  if (!twop(l)) return nil;
  word m = ldels(f, lam, B(l));
  if (!assq(f, lam, A(l))) B(l) = m, m = l;
  return m; }

static size_t llen(word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

static Inline bool lambp(core *f, word x) {
  return twop(x) && A(x) == (word) f->lambda; }

static g_core *append(core *f) {
  int i = 0;
  for (word l; g_ok(f) && twop(f->sp[0]); i++)
    l = B(f->sp[0]),
    f->sp[0] = A(f->sp[0]),
    f = g_push(f, 1, l);
  if (!g_ok(f)) return f;
  if (i == 0) return f->sp++, f;
  if (g_ok(f)) f->sp[0] = f->sp[i + 1];
  while (i--) f = g_cons_r(f);
  if (g_ok(f)) f->sp[1] = f->sp[0], f->sp++;
  return f; }

static word reverse(core *f, word l) {
  word n = nil;
  for (word m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
  return n; }


// function state using this type
typedef struct env {
  // these parameters represent stack state at a point in compile process
  word args,  // list // function positional arguments
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

static g_core *ana_lambda(core *f, env **c, word imps, word exp);

static Inline long index_of_symbol(core *f, env *c, word var) {
  long l, i = 0;
  for (l = c->imps; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  for (l = c->args; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  return -1; }

static g_core *enscope(core *f, env* par, word args, word imps) {
  f = g_push(f, 3, args, imps, par);
  f = mo_c(f, Width(env));
  if (g_ok(f)) {
    env *c = (env*) pop1(f);
    args = pop1(f), imps = pop1(f), par = (env*) pop1(f);
    c->args = args, c->imps = imps, c->par = par,
    c->stack = c->alts = c->ends = c->lams = nil;
    *--f->sp = (word) c; }
  return f; }

static Inline g_core *g_cons_2(g_core *f, g_word a, g_word b) {
  return g_cons_l(g_push(f, 2, a, b)); }

#define Ana(n, ...) size_t n(core *f, struct env **c, size_t m, word x, ##__VA_ARGS__)
#define Cata(n, ...) g_core *n(core *f, struct env **c, size_t m, ##__VA_ARGS__)
typedef Ana(ana);
typedef Cata(cata);
static ana analyze, ana_if, ana_let, ana_ap_args;
static cata pull, cata_i, cata_ix, cata_var_2, cata_var, cata_ap, cata_yield, atp, cata_ret;
static size_t ana_seq(g_core*, env**, size_t, word, word);

// generic instruction ana handlers
static Inline size_t ana_ix(core *f, size_t m, vm *i, word x) {
  if (!m || !g_ok(f)) return 0;
  f = g_push(f, 3, cata_ix, i, x);
  return g_ok(f) ? m + 2 : 0; }

// keep this separate and NoInline so g_eval can be tail call optimized if possible
NoInline g_core *g_ana(core *f, vm *y) {
  f = enscope(f, (env*) nil, nil, nil);
  if (!g_ok(f)) return f;
  env *c = (env*) pop1(f);
  MM(f, &c);
  word x = f->sp[0];
  f->sp[0] = (word) cata_yield; // function that returns thread from code generation
  size_t m = ana_ix(f, 1, y, (word) f->ip);
  m = analyze(f, &c, m, x);
  f = atp(f, &c, m);
  UM(f);
  return f; }

#define Kp (f->ip)
static g_core *atp(g_core *f, env **c, size_t m) {
  f = mo_c(f, m);
  if (g_ok(f)) {
    Kp = cell(pop1(f));
    memset(Kp, -1, m * sizeof(word));
    Kp += m;
    f = pull(f, c, 0); }
  return f; }

static size_t arity_of(env *c) { return llen(c->args) + llen(c->imps); }

// atp will move in here
static Cata(cata_yield) {
  return f; }

static Cata(cata_if_push_exit) {
  f = pull(f, c, m);
  f = g_cons_2(f, (word) Kp, (*c)->ends);
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
  f = g_cons_2(f, (word) Kp, (*c)->alts);
  if (g_ok(f)) (*c)->alts = pop1(f);
  return f; }

static Cata(cata_if_jump_out) {
  f = pull(f, c, m + 3);
  if (g_ok(f)) {
    cell *addr = cell(A((*c)->ends));
    if (addr->ap == ret || addr->ap == tap)
      Kp = memcpy(Kp - 2, addr, 2 * sizeof(word));
    else if (addr->ap == tapn)
      Kp = memcpy(Kp - 3, addr, 3 * sizeof(word));
    else
      Kp -= 2,
      Kp[0].ap = jump,
      Kp[1].x = (word) addr; }
  return f; }


static Cata(cata_ap) {
  word arity = pop1(f);
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
  word var = pop1(f), stack = pop1(f);
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
  word var = pop1(f), // variable name
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
  vm *i = (vm*) pop1(f);
  f = pull(f, c, m + 1);
  if (g_ok(f))
    Kp -= 1,
    Kp[0].ap = i;
  return f; }

static Cata(cata_ix) {
  vm *i = (vm*) pop1(f);
  word x = pop1(f);
  avec(f, x, f = pull(f, c, m + 2));
  if (g_ok(f))
    Kp -= 2,
    Kp[0].ap = i,
    Kp[1].x = x;
  return f; }

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

static Ana(analyze) {
  if (!g_ok(f) || !m) return 0;

  // is it a variable?
  if (symp(x)) for (env *d = *c;; d = d->par) {
    if (nilp(d)) { // free variable?
      word y = g_hash_get(f, 0, f->dict, x);
      if (y) return ana_ix(f, m, imm, y);
      f = g_cons_2(f, x, (*c)->imps);
      (*c)->imps = pop1(f);
      m = ana_ix(f, m, free_variable, (*c)->imps);
      return m; }
    // defined as a function by a local let form?
    word y;
    if ((y = assq(f, d->lams, x))) return
      avec(f, y, m = ana_ap_args(f, c, m, BB(y))),
      ana_ix(f, m, late_bind, y);
    // non function definition from local let form?
    if (memq(f, d->stack, x)) return
      f = g_push(f, 3, cata_var_2, x, d->stack),
      g_ok(f) ? m + 2 : 0;
    // closure or positional argument on stack?
    if (memq(f, d->imps, x) || memq(f, d->args, x)) {
      if (*c != d) // if we have found the variable in an enclosing scope then import it
        f = g_cons_2(f, x, (*c)->imps),
        x = g_ok(f) ? pop1(f) : 0,
        x = x ? A((*c)->imps = x) : x;
      if (!x) m = 0;
      else f = g_push(f, 3, cata_var, x, (*c)->stack),
           m = g_ok(f) ? m + 2 : 0;
      return m; } }

  // if it's not a variable or a list then it evals to itself
  if (!twop(x)) return ana_ix(f, m, imm, x);

  // it's a list
  g_word a = A(x), b = B(x);

  // singleton list?
  if (!twop(b)) return analyze(f, c, m, a); // value of first element

  // special form?
  if (symp(a)) {
    g_symbol *y = sym(a);
    if (y == f->quote) return ana_ix(f, m, imm, !twop(b) ? b : A(b));
    if (y == f->let) return !twop(B(b)) ? analyze(f, c, m, A(b)) :
                                          ana_let(f, c, m, b);
    if (y == f->begin) return ana_seq(f, c, m, A(b), B(b));
    if (y == f->lambda) return f = ana_lambda(f, c, nil, b),
                               g_ok(f) ? analyze(f, c, m, pop1(f)) : 0;
    if (y == f->cond) return !twop(B(b)) ? analyze(f, c, m, A(b)) :
                                           ana_if(f, c, m, b); }

  // macro?
  word mac = g_hash_get(f, 0, f->macro, a);
  if (mac) return
    f = g_push(f, 5 , nil, b, f->quote, nil, mac),
    f = g_cons_r(f),
    f = g_cons_r(f),
    f = g_cons_l(f),
    f = g_cons_r(f),
    f = g_ana(f, g_yield),
    f = g_ok(f) ? f->ip->ap(f, f->ip, f->hp, f->sp) : f,
    g_ok(f) ? analyze(f, c, m, pop1(f)) : 0;

  // application.
  avec(f, a, m = ana_ap_args(f, c, m, b));
  return analyze(f, c, m, a); }

// XXX reverse all of this...
static Ana(ana_if_r) {
  MM(f, &x);
  if (!twop(x) || !twop(B(x)))
    f = g_push(f, 1, cata_if_jump_out);
  else
    m = ana_if_r(f, c, m, BB(x)),
    f = m ? g_push(f, 2, cata_if_jump_out, cata_if_push_branch) : encode(f, Oom),
    m = analyze(f, c, m + 3, AB(x)),
    f = g_push(f, 1, cata_if_pop_branch);
  m = g_ok(f) ? m : 0;
  if (m) m = analyze(f, c, m + 3, twop(x) ? A(x) : nil);
  return UM(f), m; }

static Ana(ana_if) {
  avec(f, x, f = g_push(f, 1, cata_if_push_exit));
  if (!g_ok(f)) return 0;
  m = ana_if_r(f, c, m, x);
  if (m) f = g_push(f, 1, cata_if_pop_exit) ,
         m = g_ok(f) ? m : 0;
  return m; }

static size_t ana_seq(core *f, env* *c, size_t m, word a, word b) {
  if (!m || !g_ok(f)) return 0;
  if (twop(b))
    avec(f, a, m = ana_seq(f, c, m + 1, A(b), B(b)),
               f = g_push(f, 2, cata_i, drop1));
  return analyze(f, c, m, a); }

static size_t ana_ap_args_r(core *f, env**c, size_t m, word x) {
  if (!twop(x)) return m;
  avec(f, x, m = ana_ap_args_r(f, c, m + 2, B(x)),
             f = g_push(f, 2, cata_ap, putnum(1)));
  return analyze(f, c, m, A(x)); }

// evaluate function call arguments and apply
static size_t ana_ap_args(core *f, env **c, size_t m, word x) {
  avec(f, x, f = g_cons_2(f, nil, (*c)->stack));
  (*c)->stack = pop1(f);
  m = ana_ap_args_r(f, c, m, x);
  (*c)->stack = B((*c)->stack);
  return m; }

static g_core *ana_lambda(core *f, env **c, word imps, word exp) {
  f = enscope(f, *c, exp, imps);
  env *d = (env*) pop1(f);
  MM(f, &d);
  word x = d->args;

  // push exp args onto stack
  if (!twop(x)) f = g_push(f, 2, nil, nil);
  else { // there's at least one argument
    MM(f, &x);
    int n = 0;
    while (twop(B(x))) f = g_push(f, 1, A(x)), n++, x = B(x);
    f = g_push(f, 1, nil);
    while (n--) f = g_cons_r(f);
    f = g_push(f, 1, A(x));
    UM(f); }

  if (g_ok(f)) {
    exp = pop1(f);
    d->args = f->sp[0];
    f->sp[0] = word(cata_yield);
    avec(f, exp, f = g_push(f, 2, cata_ret, d));
    size_t m = analyze(f, &d, 4, exp);
    f = g_push(f, 2, cata_curry, d);
    m = g_ok(f) ? m : 0;
    cell *k, *ip = f->ip;
    avec(f, ip, f = atp(f, &d, m));
    if (g_ok(f)) {
      k = f->ip;
      ttag(k)->head = k;
      f->ip = ip;
      f = g_cons_2(f, word(k), d->imps); } }

  UM(f);
  return f; }


// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.
static size_t ana_let(core *f, env* *b, size_t m, word exp) {
  struct root *mm = f->safe;
#define forget() ((f)->safe=(mm),0)
  MM(f, &exp);
  f = enscope(f, *b, (*b)->args, (*b)->imps);
  if (!g_ok(f)) return forget();
  env *q = (env*) pop1(f),
      **c = &q;
  // lots of variables :(
  word nom = nil, // 511 - 592
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
      f = ana_lambda(f, c, nil, B(e));
      f = g_push(f, 1, d);
      f = g_cons_l(f);
      f = g_push(f, 1, lam);
      f = g_cons_r(f);
      if (!g_ok(f)) return forget();
      lam = pop1(f); }
    exp = BB(exp); }

  // if there's no body then evaluate the name of the last definition
  bool even = !twop(exp); // we check this again later to make global bindings at top level
  if (even) {
    f = g_cons_2(f, A(nom), nil);
    if (g_ok(f)) exp = pop1(f);
    else return forget(); }

  // find closures
  // for each function f with closure C(f)
  // for each function g with closure C(g)
  // if f in C(g) then C(g) include C(f)
  long j;
  do for (j = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
    for (e = lam; twop(e); e = B(e)) // for each bound function variable
      if (d != e && memq(f, BBA(e), AA(d))) // if you need this function
        for (v = BBA(d); twop(v); v = B(v)) { // then you need its variables
          word vars = BBA(e), var = A(v);
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
      m = ana_ix(f, m, defglob, A(nom));
      if (!m) return forget(); }
    m = analyze(f, b, m, A(def));
    (*b)->stack = B((*b)->stack); }

  m = analyze(f, b, m, exp);
  f->safe = mm;
  return m; }
