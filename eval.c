#include "i.h"
// index of item in list
static long lidx(core *f, word l, word x) {
  for (long i = 0; !nilp(l); l = B(l), i++)
    if (eql(f, x, A(l)))
      return i;
  return -1; }

static word assoc(core *f, word l, word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
  return 0; }

static word memq(core *f, word l, word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, A(l))) return l;
  return 0; }

// DEFINE
// let expressions
static word ldels(core *f, word lam, word l) {
  if (!twop(l)) return nil;
  word m = ldels(f, lam, B(l));
  if (!assoc(f, lam, A(l))) B(l) = m, m = l;
  return m; }

// list length
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
static g_core *atp(g_core*, env**, size_t);
static g_core *atp2(g_core*, env**, size_t);

static g_core *enscope(core*, env*, word, word);
#define A2(n, ...) g_core *n(g_core *f, env **c, word x, ##__VA_ARGS__)
#define C2(n, ...) g_core *n(g_core *f, env **c, size_t m, ##__VA_ARGS__)
typedef A2(a2);
static a2 analyze_2, a2_if, a2_let, a2_seq, a2_ap_l2r;
static A2(ana_lambda_2, word b);
static A2(ana_mac_2, word b);
static A2(ana_var_2, env *d);

typedef C2(c2);
static c2 pull2, c2_i, c2_ix, c2_var_2, c2_var, c2_ap, c2_apn;
static C2(c2_yield) { return f; }

#define Ana(n, ...) size_t n(core *f, struct env **c, size_t m, word x, ##__VA_ARGS__)
#define Cata(n, ...) cell *n(core *f, struct env **c, cell *k, ##__VA_ARGS__)
typedef Ana(ana);
static ana analyze, ana_if, ana_let, ana_ap_l2r, ana_ap, ana_ap_r2l;
static size_t ana_seq(g_core*, env**, size_t, word, word);
static Ana(ana_lambda, word b);
static Ana(ana_mac, word b);
static Ana(ana_var, env *d);

typedef Cata(cata);
static cata pull, cata_i, cata_ix, cata_var_2, cata_var, cata_ap, cata_apn;
static Cata(yieldk) { return k; }

/*
NoInline g_core *g_ana_2(g_core*f, vm *y) {
  f = enscope(f, (env*) nil, nil, nil);
  if (!g_ok(f)) return f;
  env *c = (env*) pop1(f);
  word x = f->sp[0];
  f->sp[0] = (word) yieldk; // function that returns thread from code generation
  MM(f, &c);
  f = analyze_2(f, &c, x);
  f = g_push(f, 3, cata_ix, y, f->ip);
  f = atp2(f, &c, 2);
  if (g_ok(f)) f->ip = cell(pop1(f));
  UM(f);
  return f; }
  */

// keep this separate and NoInline so g_eval can be tail call optimized if possible
NoInline g_core *g_ana(core *f, vm *y) {
  f = enscope(f, (env*) nil, nil, nil);
  if (!g_ok(f)) return f;
  env *c = (env*) pop1(f);
  word x = f->sp[0];
  f->sp[0] = (word) yieldk; // function that returns thread from code generation
  MM(f, &c);
  size_t m = analyze(f, &c, 1, x);
  if (!m) f = encode(f, Oom);
  else {
    f = g_push(f, 3, cata_ix, y, f->ip);
    f = atp(f, &c, m + 2);
    if (g_ok(f)) f->ip = cell(pop1(f)); }
  UM(f);
  return f; }

static g_core *g_cons_2(g_core *f, g_word a, g_word b) {
  f = g_push(f, 2, a, b);
  return g_cons_l(f); }


static Inline g_core *a2_i(g_core *f, env **c, vm *i) {
  return g_push(f, 2, c2_i, i); }
static Inline g_core *a2_ix(g_core *f, env **c, vm *i, g_word x) {
  return g_push(f, 3, c2_ix, i, x); }
// generic instruction ana handlers
static Inline size_t ana_ix(core *f, env **c, size_t m, vm *i, word x) {
  f = g_push(f, 3, cata_ix, i, x);
  return g_ok(f) ? m + 2 : 0; }
static Inline size_t ana_i(core *f, env **c, size_t m, vm *i) {
  f = g_push(f, 2, cata_i, i);
  return g_ok(f) ? m + 1 : 0; }

static g_core *ana2(g_core *f, env **c, size_t m, word x) {
  m = analyze(f, c, m, x);
  return m ? f : encode(f, Oom); }

  /*
static NoInline A2(analyze_2) {
  if (symp(x))  return a2_var(f, c, x, *c);
  if (!twop(x)) return a2_ix(f, c, imm, x);

  g_word a = A(x), b = B(x);
  if (symp(a)) {
    g_symbol *y = sym(a);
    if (y == f->begin) return twop(b) ? a2_seq(f, c, A(b), B(b)) :
                                        a2_ix(f, c, imm, nil);
    if (y == f->let) return a2_let(f, c, b);
    if (y == f->cond) return a2_if(f, c, b);
    if (y == f->lambda) return a2_lambda(f, c, x, b);
    if (y == f->quote) return x = twop(b) ? A(b) : nil,
                              a2_ix(f, c, imm, x); }
  // singleton?
  if (!twop(b)) return analyze_2(f, c, a); // value of first element

  x = g_hash_get(f, 0, f->macro, a);
  if (x) return a2_mac(f, c, x, b);

  // apply.
  avec(f, b, f = analyze_2(f, c, a));
  return a2_ap_l2r(f, c, b); }
  */
static Ana(ana_two) {
  g_word a = A(x), b = B(x);
  if (symp(a)) {
    g_symbol *y = sym(a);
    if (y == f->begin) return !twop(b) ? ana_ix(f, c, m, imm, nil) :
                                         ana_seq(f, c, m, A(b), B(b));
    if (y == f->let) return ana_let(f, c, m, b);
    if (y == f->cond) return ana_if(f, c, m, b);
    if (y == f->lambda) return ana_lambda(f, c, m, x, b);
    if (y == f->quote) return ana_ix(f, c, m, imm, !twop(b) ? nil : A(b)); }

  // singleton?
  if (!twop(b)) return analyze(f, c, m, a); // value of first element

  a = g_hash_get(f, 0, f->macro, a);
  if (a) return ana_mac(f, c, m, a, b);

  return ana_ap(f, c, m, x); }

static Ana(analyze) {
  if (symp(x))  return ana_var(f, c, m, x, *c);
  if (!twop(x)) return ana_ix(f, c, m, imm, x);
  return ana_two(f, c, m, x); }

static Ana(ana_ap) {
//  return ana_ap_r2l(f, c, m, x);
  word y = B(x);
  x = A(x);
  avec(f, y, m = analyze(f, c, m, x));
  if (m) m = ana_ap_l2r(f, c, m, y);
  return m; }

  /*
static A2(a2_r2l) {
  if (!twop(x)) return f;
  avec(f, x, f = a2_r2l(f, c, B(x)));
  return analyze_2(f, c, A(x)); }

static A2(a2_ap) {
  size_t len = llen(x);
  MM(f, &x);
  while (len--) f = a2_i(f, c, apl);
  UM(f);
  return a2_r2l(f, c, x); }
*/
    

static cata
  cata_if_push_branch,
  cata_if_pop_branch,
  cata_if_push_exit,
  cata_if_pop_exit,
  cata_if_peek_exit;
static c2
  c2_if_push_branch,
  c2_if_pop_branch,
  c2_if_push_exit,
  c2_if_pop_exit,
  c2_if_peek_exit;

static Ana(ana_if) {
  f = g_push(f, 2, x, cata_if_pop_exit);
  if (!g_ok(f)) return 0;
  x = *f->sp++;
  MM(f, &x);
  for (;m;) {
    word y = twop(x) ? A(x) : x;
    m = analyze(f, c, m + 3, y);
    if (!m) break;
    if (!twop(x) || !twop(B(x))) { // this means we have just analyzed the default case
      f = g_push(f, 1, cata_if_peek_exit);
      m = g_ok(f) ? m : 0;
      break; }
    f = g_push(f, 1, cata_if_pop_branch);
    m = g_ok(f) ? analyze(f, c, m + 3, AB(x)) : 0;
    f = m ? g_push(f, 2, cata_if_push_branch, cata_if_peek_exit) : encode(f, Oom);
    m = g_ok(f) ? m : 0;
    x = BB(x); }
  UM(f);
  if (m) 
    f = g_push(f, 1, cata_if_push_exit),
    m = g_ok(f) ? m : 0;
  return m; }


static C2(c2_if_push_exit) {
  f = pull2(f, c, m);
  f = g_cons_2(f, word(f->ip), (*c)->ends);
  if (g_ok(f)) (*c)->ends = pop1(f);
  return f; }

static Cata(cata_if_push_exit) { // first emitter called for cond expression
  f = g_cons_2(f, W(k), (*c)->ends);
  if (!g_ok(f)) return 0;
  word w = pop1(f);
  k = cell(A((*c)->ends = w));
  return pull(f, c, k); }

static C2(c2_if_pop_exit) {
  f = pull2(f, c, m);
  if (g_ok(f)) (*c)->ends = B((*c)->ends);
  return f; }

static Cata(cata_if_pop_exit) { // last emitter called for cond expression
  (*c)->ends = B((*c)->ends); // pops cond expression exit address off env stack ends
  return pull(f, c, k); }


static C2(c2_if_pop_branch) {
  f = pull2(f, c, m + 2);
  if (g_ok(f))
    f->ip -= 2,
    f->ip[0].ap = cond,
    f->ip[1].x = A((*c)->alts),
    (*c)->alts = B((*c)->alts);
  return f; }

static Cata(cata_if_pop_branch) { // last emitter called for a branch
  k -= 2;
  k[0].ap = cond; // pops next branch address off env stack alts
  k[1].x = A((*c)->alts);
  (*c)->alts = B((*c)->alts);
  return pull(f, c, k); }

static C2(c2_if_push_branch) {
  f = g_cons_2(f, word(f->ip), (*c)->alts);
  if (g_ok(f)) (*c)->alts = pop1(f);
  return f; }

static Cata(cata_if_push_branch) {
  f = g_cons_2(f, W(k), (*c)->alts);
  if (!g_ok(f)) return 0;
  word w = pop1(f);
  (*c)->alts = w;
  k = cell(A(w));
  return pull(f, c, k); }

static C2(c2_if_peek_exit) {
  f = pull2(f, c, m + 3);
  if (g_ok(f)) {
    cell *addr = (cell*) A((*c)->ends);
    if (addr->ap == ret || addr->ap == tap)
      f->ip = memcpy(f->ip - 2, addr, 2 * sizeof(word));
    else if (addr->ap == tapn)
      f->ip = memcpy(f->ip - 3, addr, 3 * sizeof(word));
    else
      f->ip -= 2,
      f->ip[0].ap = jump,
      f->ip[1].x = (word) addr; }
  return f; }

static Cata(cata_if_peek_exit) {
  cell *addr = (cell*) A((*c)->ends);
  // if the destination is a return or tail call,
  // then copy it forward instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k = memcpy(k - 2, addr, 2 * sizeof(word));
  else if (addr->ap == tapn)
    k = memcpy(k - 3, addr, 3 * sizeof(word));
  else
    k -= 2,
    k[0].ap = jump,
    k[1].x = (word) addr;
  return pull(f, c, k); }


static size_t ana_seq(core *f, env* *c, size_t m, word a, word b) {
  if (!twop(b)) return analyze(f, c, m, a);
  avec(f, b,
      m = analyze(f, c, m + 1, a),
      f = encode(f, m ? Ok : Oom),
      f = g_push(f, 2, cata_i, drop1));
  if (!g_ok(f)) return 0;
  return ana_seq(f, c, m, A(b), B(b)); }

  /*
static g_core *a2_seq(g_core *f, env **c, word a, word b) {
  if (twop(b)) avec(f, a,
    f = a2_seq(f, c, A(b), B(b)),
    f = a2_i(f, c2_i, drop1));
  return analyze_2(f, c, a); }

static A2(a2_mac, word b) {
  f = g_push(f, 5 , nil, b, f->quote, nil, x);
  f = g_cons_r(f);
  f = g_cons_r(f);
  f = g_cons_l(f);
  f = g_cons_r(f);
  f = g_ana(f, g_yield);
  f = g_ok(f) ? f->ip->ap(f, f->ip, f->hp, f->sp) : f;
  return g_ok(f) ? analyze_2(f, c, pop1(f)) : 0; }
  */

static Ana(ana_mac, word b) {
  f = g_push(f, 5 , nil, b, f->quote, nil, x);
  f = g_cons_r(f);
  f = g_cons_r(f);
  f = g_cons_l(f);
  f = g_cons_r(f);
  f = g_ana(f, g_yield);
  f = g_ok(f) ? f->ip->ap(f, f->ip, f->hp, f->sp) : f;
  return g_ok(f) ? analyze(f, c, m, pop1(f)) : 0; }

// evaluate function call arguments and apply
static size_t ana_ap_l2r(core *f, env **c, size_t m, word x) {
  MM(f, &x); // handle oom here ..
  // push one anonymous argument on the stack representing the function
  f = g_cons_2(f, nil, (*c)->stack);
  if (g_ok(f)) {
    for ((*c)->stack = pop1(f); m && twop(x); x = B(x))
      m = analyze(f, c, m, A(x)), // eval each argument
      f = encode(f, m ? Ok : Oom),
      f = g_push(f, 1, cata_ap),
      m = g_ok(f) ? m + 1 : 0;
    // pop the argument
    (*c)->stack = B((*c)->stack); }
  UM(f);
  return m; }

static g_core *ana_lam(core*, env**, word, word);
static Ana(ana_lambda, word b) {
  if (!twop(b)) return ana_ix(f, c, m, imm, nil);
  if (!twop(B(b))) return analyze(f, c, m, A(b));
  f = ana_lam(f, c, nil, b);
  if (!g_ok(f)) return 0;
  return analyze(f, c, m, pop1(f)); }

static Ana(ana_let_var, env *d) {
  f = g_push(f, 3, cata_var_2, x, d->stack);
  return g_ok(f) ? m + 2 : 0; }

static Ana(ana_clo_pos_var, env *d) {
    if (*c != d) // if we have found the variable in an enclosing scope then import it
      f = g_cons_2(f, x, (*c)->imps),
      x = g_ok(f) ? pop1(f) : 0,
      x = x ? A((*c)->imps = x) : x;
    if (!x) return 0;
    f = g_push(f, 3, cata_var, x, (*c)->stack);
    return g_ok(f) ? m + 2 : 0; }

static Ana(ana_let_fn_var, word y) {
  m = ana_ix(f, c, m, late_bind, y);
  if (!m) return m;
  return ana_ap_l2r(f, c, m, BB(f->sp[2])); }

static Ana(ana_free_var) {
    word y = g_hash_get(f, 0, f->dict, x);
    if (y) return ana_ix(f, c, m, imm, y);
    f = g_cons_2(f, x, (*c)->imps);
    if (!g_ok(f)) return 0;
    x = A((*c)->imps = pop1(f));
    return ana_ix(f, c, m, free_variable, x); }

static Ana(ana_var, env *d) {
  // free symbol?
  if (nilp(d)) return ana_free_var(f, c, m, x);
  word y;
  // defined as a function by a local let form?
  if ((y = assoc(f, d->lams, x))) return ana_let_fn_var(f, c, m, x, y);
  // non function definition from local let form?
  if (memq(f, d->stack, x)) return ana_let_var(f, c, m, x, d);
  // closure or positional argument on stack?
  if (memq(f, d->imps, x) || memq(f, d->args, x))
    return ana_clo_pos_var(f, c, m, x, d);
  // otherwise recur on the enclosing env
  return ana_var(f, c, m, x, d->par); }

static g_core *ana_lam(core *f, env **c, word imps, word exp) {
  f = enscope(f, *c, exp, imps);
  if (!g_ok(f)) return f;
  env *d = (env*) pop1(f);
  MM(f, &d);
  word x = d->args;
  // get the real args
  if (!twop(x)) f = g_push(f, 2, nil, nil);
  else if (!twop(B(x))) f = g_push(f, 2, A(x), nil);
  else {
    MM(f, &x);
    int n = 0;
    do f = g_push(f, 1, A(x)), n++, x = B(x);
    while (twop(B(x)));
    f = g_push(f, 1, nil);
    while (n--) f = g_cons_r(f);
    UM(f);
    f = g_push(f, 1, A(x)); }
  if (!g_ok(f)) return UM(f), f;

  exp = pop1(f);
  d->args = f->sp[0];
  f->sp[0] = word(yieldk);
  size_t m = analyze(f, &d, 2, exp);
  if (!m) return UM(f), encode(f, Oom);
  size_t arity = llen(d->args) + llen(d->imps);
  m = ana_ix(f, c, m, ret, putnum(arity));
  f = m ? atp(f, &d, m) : encode(f, Oom);
  cell *k = g_ok(f) ? cell(pop1(f)) : 0;
  if (!k) return UM(f), encode(f, Oom);
  if (arity > 1)
    k -= 2, 
    k[0].ap = curry,
    k[1].x = putnum(arity);
  ttag(k)->head = k;
  UM(f);
  return g_cons_2(f, W(k), d->imps); }

// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.
static size_t ana_let(core *f, env* *b, size_t m, word exp) {
  if (!twop(exp)) return analyze(f, b, m, nil);
  if (!twop(B(exp))) return analyze(f, b, m, A(exp));
  f = g_push(f, 1, exp);
  f = enscope(f, *b, (*b)->args, (*b)->imps);
  if (!g_ok(f)) return 0;
  env *q = (env*) pop1(f), **c = &q;
  exp = pop1(f);
  // lots of variables :(
  struct root *mm = f->safe;
  word nom = nil, def = nil, lam = nil, v = nil, d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &exp), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

  // collect vars and defs into two lists
  for (; twop(exp) && twop(B(exp)); exp = BB(exp)) {
    for (d = A(exp), e = AB(exp); twop(d); e = pop1(f), d = A(d)) {
      f = g_cons_2(f, e, nil);
      f = g_push(f, 1, B(d));
      f = append(f);
      f = g_push(f, 1, (word) f->lambda);
      f = g_cons_l(f);
      if (!g_ok(f)) goto fail; }
    f = g_cons_2(f, d, nom);
    f = g_cons_2(f, e, def);
    if (!g_ok(f)) goto fail;
    def = pop1(f);
    nom = pop1(f);
    // if it's a lambda compile it and record in lam list
    if (lambp(f, e)) {
      f = ana_lam(f, c, nil, B(e));
      word x = g_ok(f) ? pop1(f) : 0;
      f = g_push(f, 3, d, x, lam);
      f = g_cons_l(f);
      f = g_cons_l(f);
      if (g_ok(f)) lam = pop1(f);
      else goto fail; } }

  // if there's no body then evaluate the name of the last definition
  bool even = !twop(exp); // we check this again later to make global bindings at top level
  if (even) {
    f = g_cons_2(f, A(nom), nil);
    if (g_ok(f)) exp = pop1(f);
    else goto fail; }

  // find closures
  // for each function f with closure C(f)
  // for each function g with closure C(g)
  // if f in C(g) then C(g) include C(f)
  long j;
  do for (j = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
    for (e = lam; twop(e); e = B(e)) // for each bound function variable
      if (d != e && // skip yourself
          lidx(f, BBA(e), AA(d)) >= 0) // if you need this function
        for (v = BBA(d); twop(v); v = B(v)) { // then you need its variables
          word vars = BBA(e), var = A(v);
          if (lidx(f, vars, var) < 0) { // only add if it's not already there
            f = g_cons_2(f, var, vars);
            if (g_ok(f)) j++, BBA(e) = pop1(f);
            else goto fail; } }
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e));

  (*c)->lams = lam;
  f = g_push(f, 2, nom, exp);
  f = append(f);
  f = g_push(f, 1, f->lambda);
  f = g_cons_l(f);
  if (!g_ok(f)) goto fail;
  // pull_m lambda with reversed argument list
  exp = pop1(f);
  m = analyze(f, b, m, exp); // exp is now the required lambda, analyze it
  f = m ? f : encode(f, Oom);
  f = g_cons_2(f, nil, (*b)->stack);
  if (!g_ok(f)) goto fail;
  (*b)->stack = pop1(f);

  // reverse the nom and def lists
  nom = reverse(f, nom);
  def = reverse(f, def);
  // evaluate definitions in order tracking var names on stack list
  // store lambdas on env for lazy binding and pull_m new application
  // - reverse noms onto exp
  // - reverse onto e = nil and recompile lambdas
  size_t nn = 0;
  while (twop(nom)) {
    // if lambda then recompile with the explicit closure
    // and put in arg list and lam list (latter is used for lazy binding)
    if (lambp(f, A(def))) {
      d = assoc(f, lam, A(nom));
      f = ana_lam(f, c, BB(d), BA(def));
      if (!g_ok(f)) goto fail;
      else A(def) = B(d) = pop1(f); }
    m = analyze(f, b, m, A(def));
    f = m ? f : encode(f, Oom);
    f = g_cons_2(f, A(nom), (*b)->stack);
    if (!g_ok(f)) goto fail;
    (*b)->stack = pop1(f);
    // if toplevel then bind
    if (even && nilp((*b)->args))
      if (!(m = ana_ix(f, c, m, defglob, A(nom))))
        goto fail;
    nom = B(nom);
    def = B(def);
    nn++; }

  if (nn <= 1)
    f = g_push(f, 1, cata_ap),
    m = g_ok(f) ? m + 1 : 0;
  else
    f = g_push(f, 2, cata_apn, putnum(nn)),
    m = g_ok(f) ? m + 2 : 0;

  for (nn++; nn--; (*b)->stack = B((*b)->stack));
end:
  f->safe = mm;
  return m;
fail:
  m = 0;
  goto end; }

static C2(c2_ap) {
  f = pull2(f, c, m + 1);
  if (g_ok(f)) {
    if (f->ip[0].ap == ret)
      f->ip[0].ap = tap;
    else
      f->ip -= 1,
      f->ip[0].ap = ap; }
  return f; }
// emits call instruction and modifies to tail call
// if next operation is return
static Cata(cata_ap) {
  if (k[0].ap == ret) k[0].ap = tap;
  else --k, k[0].ap = ap;
  return pull(f, c, k); }

static C2(c2_apn) {
  word n = *f->sp++;
  f = pull2(f, c, m + 2);
  if (g_ok(f)) {
    if (f->ip[0].ap == ret)
      f->ip -= 1,
      f->ip[0].ap = tapn;
    else
      f->ip -= 2,
      f->ip[0].ap = apn;
    f->ip[1].x = n; }
  return f; }

static Cata(cata_apn) {
  word n = *f->sp++;
  if (k[0].ap == ret) k[0].x = n, --k, k[0].ap = tapn;
  else --k, k[0].x = n, --k, k[0].ap = apn;
  return pull(f, c, k); }

static Cata(cata_var_2) {
  word var = *f->sp++, stack = *f->sp++;
  long i = lidx(f, stack, var);
  k -= 2;
  k[0].ap = ref;
  k[1].x = putnum(i);
  return pull(f, c, k); }

static long index_of_symbol(core *f, env *c, word var) {
  long l, i = 0;
  for (l = c->imps; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  for (l = c->args; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  return -1; }

static C2(c2_var_2) {
  word var = *f->sp++, stack = *f->sp++,
       i = putnum(lidx(f, stack, var));
  f = pull2(f, c, m + 2);
  if (g_ok(f))
    f->ip -= 2,
    f->ip[0].ap = ref,
    f->ip[1].x = i;
  return f; }

static C2(c2_var) {
  word var = *f->sp++, // variable name
       ins = llen(*f->sp++), // stack inset
       i = index_of_symbol(f, *c, var);
  f = pull2(f, c, m + 2);
  if (g_ok(f))
    f->ip -= 2,
    f->ip[0].ap = ref,
    f->ip[1].x = putnum(i + ins);
  return f; }

// emit stack reference instruction
static Cata(cata_var) {
  word var = *f->sp++, // variable name
       ins = llen(*f->sp++), // stack inset
       i = index_of_symbol(f, *c, var);
  k -= 2;
  k[0].ap = ref;
  k[1].x = putnum(i + ins);
  return pull(f, c, k); }

static Inline C2(pull2) {
  c2 *p = (c2*) f->sp[0];
  f->sp += 1;
  return p(f, c, m); }

static Inline Cata(pull) {
  cata *p = (cata*) f->sp[0];
  f->sp += 1;
  return p(f, c, k); }


static C2(c2_i) {
  vm *i = (vm*) pop1(f);
  f = pull2(f, c, m + 1);
  if (g_ok(f))
    f->ip -= 1,
    f->ip[0].ap = i;
  return f; }

static Cata(cata_i) {
  k -= 1;
  k[0].x = f->sp[0];
  f->sp += 1;
  return pull(f, c, k); }

static Cata(cata_ix) {
  k -= 2;
  k[0].x = f->sp[0];
  k[1].x = f->sp[1];
  f->sp += 2;
  return pull(f, c, k); }

static g_core *mo_c(core *f, size_t n) {
  f = g_cells(f, n + Width(struct tag));
  if (g_ok(f)) {
    cell *k = (cell*) f->sp[0];
    struct tag *t = (struct tag*) (k + n);
    t->null = NULL;
    t->head = k; }
  return f; }

NoInline Vm(ev0) {
  Ip++;
  Pack(f);
  f = g_ana(f, jump);
  if (!g_ok(f)) return f;
  return f->ip->ap(f, f->ip, f->hp, f->sp); }

Vm(defglob) {
  Have(3);
  Sp -= 3;
  Sp[0] = (g_word) f->dict;
  Sp[1] = Ip[1].x;
  Sp[2] = Sp[3];
  Pack(f);
  f = g_hash_put(f);
  if (!g_ok(f)) return f;
  Unpack(f);
  Sp += 1;
  Ip += 2;
  return Continue(); }

Vm(drop1) {
  Ip += 1;
  Sp += 1;
  return Continue(); }

Vm(free_variable) {
  Ip[0].ap = imm;
  Ip[1].x = g_hash_get(f, Ip[1].x, f->dict, Ip[1].x);
  return Continue(); }

Vm(late_bind) {
  word ref = Ip[1].x, lfd = ref;
  ref = AB(lfd);
  Ip[0].ap = imm;
  Ip[1].x = ref;
  return Continue(); }

static C2(c2_ix) {
  vm *i = (vm*) pop1(f);
  g_word x = pop1(f);
  avec(f, x, f = pull2(f, c, m + 2));
  if (g_ok(f))
    f->ip -= 2,
    f->ip[0].ap = i,
    f->ip[1].x = x;
  return f; }

static g_core *atp2(g_core *f, env **c, size_t m) {
  f = mo_c(f, m);
  if (g_ok(f))
    memset(cell(f->sp[0]), -1, m * sizeof(word)),
    f->sp[0] = word(cell(f->sp[0]) + m);
  return f; }

static g_core *atp(g_core *f, env **c, size_t m) {
  f = mo_c(f, m);
  if (g_ok(f)) {
    cell *k = cell(pop1(f));
    memset(k, -1, m * sizeof(word));
    k = pull(f, c, k + m);
    f = k ? g_push(f, 1, k) : encode(f, Oom); }
  return f; }

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
