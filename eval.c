#include "i.h"

NoInline Vm(ev0) {
  Ip++;
  Pack(f);
  f = g_ana(f, jump);
  if (!g_ok(f)) return f;
  return f->ip->ap(f, f->ip, f->hp, f->sp); }

static g_core *mo_c(core *f, size_t n) {
  f = g_cells(f, n + Width(struct tag));
  if (g_ok(f)) {
    cell *k = (cell*) f->sp[0];
    struct tag *t = (struct tag*) (k + n);
    t->null = NULL;
    t->head = k; }
  return f; }


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

static long index_of_symbol(core *f, env *c, word var) {
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
static g_core *ana_ix_atp(g_core *f, env **c, size_t m, vm *i, word x);
static g_core *atp2(g_core*, env**, size_t);

static g_core *enscope(core*, env*, word, word);

#define Ana(n, ...) size_t n(core *f, struct env **c, size_t m, word x, ##__VA_ARGS__)
typedef Ana(ana);
static ana analyze, ana_if, ana_let, ana_ap_l2r, ana_ap, ana_ap_r2l;
static size_t ana_seq(g_core*, env**, size_t, word, word);
static Ana(ana_lambda, word b);
static Ana(ana_mac, word b);
static Ana(ana_var, env *d);

typedef g_core *cata(core*, env**);
static cata pull, cata_i, cata_ix, cata_var_2, cata_var, cata_ap, cata_apn, cata_yield;

// keep this separate and NoInline so g_eval can be tail call optimized if possible
NoInline g_core *g_ana(core *f, vm *y) {
  f = enscope(f, (env*) nil, nil, nil);
  if (!g_ok(f)) return f;
  env *c = (env*) pop1(f);
  word x = f->sp[0];
  f->sp[0] = (word) cata_yield; // function that returns thread from code generation
  MM(f, &c);
  size_t m = analyze(f, &c, 1, x);
  if (!m) f = encode(f, Oom);
  else {
    f = ana_ix_atp(f, &c, m, y, (word) f->ip);
    if (g_ok(f)) f->ip = cell(pop1(f)); }
  UM(f);
  return f; }

static g_core *g_cons_2(g_core *f, g_word a, g_word b) {
  f = g_push(f, 2, a, b);
  return g_cons_l(f); }

// generic instruction ana handlers
static Inline size_t ana_ix(core *f, env **c, size_t m, vm *i, word x) {
  f = g_push(f, 3, cata_ix, i, x);
  return g_ok(f) ? m + 2 : 0; }

static g_core *ana_ix_atp(g_core *f, env **c, size_t m, vm *i, word x) {
  m = ana_ix(f, c, m, i, x);
  f = m ? mo_c(f, m) : encode(f, Oom);
  if (g_ok(f)) {
    cell *k = cell(f->sp[0]);
    memset(k, -1, m * sizeof(word));
    k += m;
    f->sp[0] = word(k);
    f = pull(f, c); }
  return f; }

static Inline size_t ana_i(core *f, env **c, size_t m, vm *i) {
  f = g_push(f, 2, cata_i, i);
  return g_ok(f) ? m + 1 : 0; }

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

static size_t ana_seq(core *f, env* *c, size_t m, word a, word b) {
  if (!twop(b)) return analyze(f, c, m, a);
  avec(f, b,
      m = analyze(f, c, m + 1, a),
      f = encode(f, m ? Ok : Oom),
      f = g_push(f, 2, cata_i, drop1));
  if (!g_ok(f)) return 0;
  return ana_seq(f, c, m, A(b), B(b)); }

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
  f->sp[0] = word(cata_yield);
  size_t m = analyze(f, &d, 2, exp);
  if (!m) return UM(f), encode(f, Oom);
  size_t arity = llen(d->args) + llen(d->imps);
  f = ana_ix_atp(f, &d, m, ret, putnum(arity));
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



static cata
  cata_if_push_branch,
  cata_if_pop_branch,
  cata_if_push_exit,
  cata_if_pop_exit,
  cata_if_peek_exit;

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

#define Cata(n, ...) g_core *n(core *f, struct env **c, ##__VA_ARGS__)
static Cata(cata_yield) {
//  puts("cata yield");
  return f; }
static Cata(cata_if_push_exit) { // first emitter called for cond expression
//  puts("cata if push exit");
  f = g_cons_2(f, f->sp[0], (*c)->ends);
  if (g_ok(f)) (*c)->ends = pop1(f);
  return pull(f, c); }

static Cata(cata_if_pop_exit) { // last emitter called for cond expression
//  puts("cata if pop exit");
  (*c)->ends = B((*c)->ends); // pops cond expression exit address off env stack ends
  return pull(f, c); }


static Cata(cata_if_pop_branch) { // last emitter called for a branch
//  puts("cata if pop branch");
  cell *k = cell(f->sp[0]);
  k -= 2;
  k[0].ap = cond; // pops next branch address off env stack alts
  k[1].x = A((*c)->alts);
  (*c)->alts = B((*c)->alts);
  f->sp[0] = word(k);
  return pull(f, c); }

static Cata(cata_if_push_branch) {
//  puts("cata if push branch");
  f = g_cons_2(f, f->sp[0], (*c)->alts);
  if (g_ok(f)) (*c)->alts = pop1(f);
  return pull(f, c); }

static Cata(cata_if_peek_exit) {
//  puts("cata if peek exit");
  cell *addr = cell(A((*c)->ends)),
       *k = cell(f->sp[0]);
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
  f->sp[0] = word(k);
  return pull(f, c); }
// emits call instruction and modifies to tail call
// if next operation is return
static Cata(cata_ap) {
//  puts("cata ap");
  cell *k = cell(f->sp[0]);
  if (k[0].ap == ret) k[0].ap = tap;
  else --k, k[0].ap = ap;
  f->sp[0] = word(k);
  return pull(f, c); }

static Cata(cata_apn) {
//  puts("cata apn");
  cell *k = cell(f->sp[0]);
  word n = f->sp[1];
  if (k[0].ap == ret) k[0].x = n, --k, k[0].ap = tapn;
  else --k, k[0].x = n, --k, k[0].ap = apn;
  f->sp[1] = word(k);
  f->sp += 1;
  return pull(f, c); }

static Cata(cata_var_2) {
//  puts("cata var 2");
  cell *k = cell(f->sp[0]);
  word var = f->sp[1], stack = f->sp[2];
  long i = lidx(f, stack, var);
  k -= 2;
  k[0].ap = ref;
  k[1].x = putnum(i);
  f->sp[2] = word(k);
  f->sp += 2;
  return pull(f, c); }

// emit stack reference instruction
static Cata(cata_var) {
//  puts("cata var");
  cell *k = cell(f->sp[0]);
  word var = f->sp[1], // variable name
       ins = llen(f->sp[2]), // stack inset
       i = index_of_symbol(f, *c, var);
  k -= 2;
  k[0].ap = ref;
  k[1].x = putnum(i + ins);
  f->sp[2] = word(k);
  f->sp += 2;
  return pull(f, c); }

static Inline Cata(pull) {
//  puts("cata pull");
  cata *p = (cata*) f->sp[1];
  f->sp[1] = f->sp[0];
  f->sp += 1;
  return p(f, c); }

static Cata(cata_i) {
//  puts("cata i");
  cell *k = cell(f->sp[0]);
  k -= 1;
  k[0].x = f->sp[1];
  f->sp += 1;
  f->sp[0] = word(k);
  return pull(f, c); }

static Cata(cata_ix) {
//  puts("cata ix");
  cell *k = cell(f->sp[0]);
  k -= 2;
  k[0].x = f->sp[1];
  k[1].x = f->sp[2];
  f->sp += 2;
  f->sp[0] = word(k);
  return pull(f, c); }
