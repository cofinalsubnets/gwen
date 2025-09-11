#include "i.h"

static g_core *g_cons_c(g_core *f, g_word a, g_word b) {
  f = g_push(f, 2, a, b);
  return g_cons_stack(f, 0, 1); }

static word pushs(core *f, uintptr_t m, ...) {
  va_list xs;
  va_start(xs, m);
  f = vpushc(f, m, xs);
  va_end(xs);
  return g_ok(f) ? f->sp[0] : 0; }

static g_core *p_ana(g_core*, vm*);
NoInline g_core *g_eval(g_core *f, vm *y) {
  f = p_ana(f, y);
  int s = code_of(f);
  f = core_of(f);
  if (s == Ok)
    s = f->ip->ap(f, f->ip, f->hp, f->sp);
  return encode(f, s); }

struct env;
static word lassoc(core*, word, word);
static word ldels(core*, word, word);
static long stack_index_of_symbol(core*, struct env*, word);
static size_t llen(word);
#define Ana(n, ...) size_t n(core *f, struct env **c, size_t m, word x, ##__VA_ARGS__)
#define Cata(n, ...) cell *n(core *f, struct env **c, cell *k, ##__VA_ARGS__)
typedef Ana(ana);
typedef Cata(cata);

// at all times vm is running a function. thread compiler tracks
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

static long lidx(core*, word, word);
static g_core*enscope_c(core*, env*, word, word);
static cata pull, cata_i, cata_ix, yieldk;
static size_t ana_i2(core*, env**, size_t, vm*, word);
static g_core *alloc_thread_c(core*, env**, size_t);

// general analyze function
static ana analyze;

// keep this separate and NoInline so p_eval can be tail call optimized if possible
static NoInline g_core *p_ana(core *f, vm *y) {
  f = enscope_c(f, (env*) nil, nil, nil);
  if (!g_ok(f)) return f;
  env *c = (env*) pop1(f);
  word x = f->sp[0];
  f->sp[0] = (word) yieldk; // function that returns thread from code generation
  MM(f, &c);
  size_t m = analyze(f, &c, 1, x);
  if (!m) f = encode(f, Oom);
  else {
    f = g_push(f, 3, cata_ix, y, f->ip);
    f = alloc_thread_c(f, &c, m + 2);
    if (g_ok(f)) {
      cell *k = pull(f, &c, (cell*) pop1(f));
      if (!k) f = encode(f, Oom);
      else f->ip = k; } }
  UM(f);
  return f; }

static ana ana_if, ana_let, ana_seq, ana_ap_l2r;
static Ana(ana_lambda, word b);
static Ana(ana_mac, word b);
static Ana(ana_var, env *d);

static NoInline Ana(analyze) {
  if (symp(x))  return ana_var(f, c, m, x, *c);
  if (!twop(x)) return ana_i2(f, c, m, imm, x);

  g_word a = A(x), b = B(x);
  if (symp(a)) {
    g_symbol *y = sym(a);
    if (y == f->begin) return ana_seq(f, c, m, b);
    if (y == f->let) return ana_let(f, c, m, b);
    if (y == f->cond) return ana_if(f, c, m, b);
    if (y == f->lambda) return ana_lambda(f, c, m, x, b);
    if (y == f->quote) return x = twop(b) ? A(b) : nil,
                              ana_i2(f, c, m, imm, x); }

  // singleton?
  if (!twop(b)) return analyze(f, c, m, a); // value of first element

  f = g_push(f, 3, 0, f->macro, a);
  if (!g_ok(f)) return 0;
  f = g_hash_get(f);
  x = pop1(f);
  // macro?
  if (x) return ana_mac(f, c, m, x, b);

  // apply.
  avec(f, b, m = analyze(f, c, m, a));
  if (m) m = ana_ap_l2r(f, c, m, b);
  return m; }

static cata cata_if_push_branch,
            cata_if_pop_branch,
            cata_if_push_exit,
            cata_if_pop_exit,
            cata_if_peek_exit;

static Ana(ana_if) {
  if (!pushs(f, 2, x, cata_if_pop_exit)) return 0;
  pair p = { 0, 0, nil, nil }; // this is weird :/
  x = pop1(f);
  MM(f, &x);
  for (; m; x = BB(x)) {
    if (!twop(x)) x = (word) &p;
    m = analyze(f, c, m + 3, A(x));
    if (!twop(B(x))) { // this means we have just analyzed the default case
      m = pushs(f, 1, cata_if_peek_exit) ? m : 0; // now branch to exit
      break; }
    // otherwise we analyzed a conditional test
    // pop the last branch address off the stack to be jumped to in case the test failed
    m = pushs(f, 1, cata_if_pop_branch) ? m : 0;
    // otherwise here is the consequent
    m = m ? analyze(f, c, m + 3, AB(x)) : m;
    // after consequent jump to exit, then push new branch address at present code position (these are emitted backwards)
    m = m && pushs(f, 2, cata_if_push_branch,
                         cata_if_peek_exit) ? m : 0; }
  UM(f);
  m = m && pushs(f, 1, cata_if_push_exit) ? m : 0; // push the exit address for the whole conditional
  return m; }

static Cata(cata_if_push_exit) { // first emitter called for cond expression
  f = g_cons_c(f, W(k), (*c)->ends);
  if (!g_ok(f)) return 0;
  word w = pop1(f);
  k = R(A((*c)->ends = w));
  return pull(f, c, k); }

static Cata(cata_if_pop_exit) { // last emitter called for cond expression
  (*c)->ends = B((*c)->ends); // pops cond expression exit address off env stack ends
  return pull(f, c, k); }

static Cata(cata_if_pop_branch) { // last emitter called for a branch
  k[-2].ap = cond; // pops next branch address off env stack alts
  k[-1].x = A((*c)->alts);
  (*c)->alts = B((*c)->alts);
  return pull(f, c, k - 2); }

static Cata(cata_if_push_branch) {
  f = g_cons_c(f, W(k), (*c)->alts);
  if (!g_ok(f)) return 0;
  word w = pop1(f);
  (*c)->alts = w;
  k = R(A(w));
  return pull(f, c, k); }

static Cata(cata_if_peek_exit) {
  k -= 2;
  cell *addr = (cell*) A((*c)->ends);
  // if the destination is a return or tail call,
  // then copy it forward instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k[0].ap = addr[0].ap,
    k[1].x = addr[1].x; // XXX optimization to remove ...
  else if (addr->ap == tapn)
    k--,
    k[0].ap = tapn,
    k[1].x = addr[1].x,
    k[2].x = addr[2].x;
  else k[0].ap = jump, k[1].x = (word) addr;
  return pull(f, c, k); }

static cata c2var, cata_var, cataap, cataapn;

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
  while (i--) f = g_cons_stack(f, 1, 0);
  if (g_ok(f)) f->sp[1] = f->sp[0], f->sp++;
  return f; }

static word reverse(core *f, word l) {
  word n = nil;
  for (word m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
  return n; }

static size_t ana_seq(core *f, env* *c, size_t m, word x) {
  if (!twop(x)) return ana_i2(f, c, m, imm, nil);
  MM(f, &x);
  while (m && twop(B(x))) {
    m = analyze(f, c, m, A(x));
    if (m) m = pushs(f, 2, cata_i, drop1) ? m + 1 : 0;
    x = B(x); }
  UM(f);
  m = m ? analyze(f, c, m, A(x)) : m;
  return m; }

static Ana(ana_mac, word b) {
  f = g_push(f, 5 , nil, b, f->quote, nil, x);
  f = g_cons_stack(f, 1, 0);
  f = g_cons_stack(f, 1, 0);
  f = g_cons_stack(f, 0, 1);
  f = g_cons_stack(f, 1, 0);
  f = g_eval(f, g_yield);
  m = g_ok(f) ? analyze(f, c, m, pop1(f)) : 0;
  return m; }

// evaluate function call arguments and apply
static size_t ana_ap_l2r(core *f, env **c, size_t m, word x) {
  MM(f, &x); // handle oom here ..
  // push one anonymous argument on the stack representing the function
  f = g_cons_c(f, nil, (*c)->stack);
  if (g_ok(f)) {
    (*c)->stack = pop1(f);
    while (m && twop(x))
      m = analyze(f, c, m + 1, A(x)), // eval each argument
      x = B(x),
      m = m && pushs(f, 1, cataap) ? m : 0; // and apply the function
    // pop the argument
    (*c)->stack = B((*c)->stack); }
  UM(f);
  return m; }

static g_core *ana_lam(core*, env**, word, word);
static Ana(ana_lambda, word b) {
  if (!twop(b)) return ana_i2(f, c, m, imm, nil);
  if (!twop(B(b))) return analyze(f, c, m, A(b));
  f = ana_lam(f, c, nil, b);
  if (!g_ok(f)) return 0;
  return analyze(f, c, m, pop1(f)); }

static Ana(ana_var, env *d) {
  // free symbol?
  if (nilp(d)) {
    f = g_push(f, 3, 0, f->dict, x);
    if (!g_ok(f)) return 0;
    f = g_hash_get(f);
    word y = pop1(f);
    if (y) return ana_i2(f, c, m, imm, y);
    f = g_cons_c(f, x, (*c)->imps);
    x = g_ok(f) ? pop1(f) : 0;
    x = x ? A((*c)->imps = x) : x;
    x = x ? ana_i2(f, c, m, free_variable, x) : x;
    return x;  }
  // defined as a function by a local let binding?
  word y = lassoc(f, d->lams, x);
  if (y) {
    m = ana_i2(f, c, m, late_bind, y);
    if (!m) return m;
    y = f->sp[2];
    word z = BB(y); // get the args
    return ana_ap_l2r(f, c, m, z); }
  // bound on the stack by a local let binding?
  if (lidx(f, d->stack, x) >= 0)
    return pushs(f, 3, c2var, x, d->stack) ? m + 2 : 0;
  // bound on the stack as a closure or positional argument?
  if (stack_index_of_symbol(f, d, x) >= 0) {
    if (*c != d) // if we have found the variable in an enclosing scope then import it
      f = g_cons_c(f, x, (*c)->imps),
      x = g_ok(f) ? pop1(f) : 0,
      x = x ? A((*c)->imps = x) : x;
    if (!x) return 0;
    f = g_push(f, 3, cata_var, x, (*c)->stack);
    return g_ok(f) ? m + 2 : 0; }
  // otherwise recur on the enclosing env
  return ana_var(f, c, m, x, d->par); }

static g_core *ana_lam(core *f, env **c, word imps, word exp) {
  f = enscope_c(f, *c, exp, imps);
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
    while (n--) f = g_cons_stack(f, 1, 0);
    UM(f);
    f = g_push(f, 1, A(x)); }
  if (!g_ok(f)) return UM(f), f;

  exp = pop1(f);
  d->args = f->sp[0];
  f->sp[0] = W(yieldk);
  size_t m = analyze(f, &d, 2, exp);
  if (!m) return UM(f), encode(f, Oom);
  size_t arity = llen(d->args) + llen(d->imps);
  m = ana_i2(f, c, m, ret, putnum(arity));
  f = m ? alloc_thread_c(f, &d, m) : encode(f, Oom);
  cell *k = g_ok(f) ? pull(f, &d, (cell*) pop1(f)) : 0;
  if (!k) return UM(f), encode(f, Oom);
  if (arity > 1) (--k)->x = putnum(arity), (--k)->ap = curry;
  ttag(k)->head = k;
  UM(f);
  return g_cons_c(f, W(k), d->imps); }

// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.
static size_t ana_let(core *f, env* *b, size_t m, word exp) {
  if (!twop(exp)) return analyze(f, b, m, nil);
  if (!twop(B(exp))) return analyze(f, b, m, A(exp));
  f = g_push(f, 1, exp);
  f = enscope_c(f, *b, (*b)->args, (*b)->imps);
  if (!g_ok(f)) return 0;
  env *q = (env*) pop1(f), **c = &q;
  exp = pop1(f);
  // lots of variables :(
#define fail() (f->safe = mm, 0)
  struct root *mm = f->safe;
  word nom = nil, def = nil, lam = nil, v = nil, d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &exp), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

  // collect vars and defs into two lists
  for (; twop(exp) && twop(B(exp)); exp = BB(exp)) {
    d = A(exp), e = AB(exp);
    while (twop(d)) { // desugar...
      f = g_cons_c(f, e, nil);
      f = g_push(f, 1, B(d));
      f = append(f);
      f = g_cons_c(f, (word) f->lambda, pop1(f));
      if (!g_ok(f)) return fail();
      e = pop1(f);
      d = A(d); }
    f = g_cons_c(f, d, nom);
    f = g_cons_c(f, e, def);
    if (!g_ok(f)) return fail();
    def = pop1(f);
    nom = pop1(f);
    // if it's a lambda compile it and record in lam list
    if (lambp(f, e)) {
      f = ana_lam(f, c, nil, B(e));
      word x = g_ok(f) ? pop1(f) : 0;
      f = g_push(f, 3, d, x, lam);
      f = g_cons_stack(f, 0, 1);
      f = g_cons_stack(f, 0, 1);
      if (g_ok(f)) lam = pop1(f);
      else return fail(); } }

  // if there's no body then evaluate the name of the last definition
  bool even = !twop(exp); // we check this again later to make global bindings at top level
  if (even) {
    f = g_cons_c(f, A(nom), nil);
    if (g_ok(f)) exp = pop1(f);
    else return fail(); }

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
            f = g_cons_c(f, var, vars);
            if (g_ok(f)) j++, BBA(e) = pop1(f);
            else return fail(); } }
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e));

  (*c)->lams = lam;
  f = g_push(f, 2, nom, exp);
  f = append(f);
  f = g_push(f, 1, f->lambda);
  f = g_cons_stack(f, 0, 1);
  if (!g_ok(f)) return fail();
  // pull_m lambda with reversed argument list
  exp = pop1(f);
  m = analyze(f, b, m, exp); // exp is now the required lambda, analyze it
  if (!m) return fail();
  f = g_cons_c(f, nil, (*b)->stack);
  if (!g_ok(f)) return fail();
  (*b)->stack = pop1(f);

  // reverse the nom and def lists
  nom = reverse(f, nom);
  def = reverse(f, def);
  // evaluate definitions in order tracking var names on stack list
  // store lambdas on env for lazy binding and pull_m new application
  // - reverse noms onto exp
  // - reverse onto e = nil and recompile lambdas
  size_t nn = 0;
  for (; twop(nom); nom = B(nom), def = B(def), nn++) {
    // if lambda then recompile with the explicit closure
    // and put in arg list and lam list (latter is used for lazy binding)
    if (lambp(f, A(def))) {
      d = lassoc(f, lam, A(nom));
      f = ana_lam(f, c, BB(d), BA(def));
      if (!g_ok(f)) return fail();
      else A(def) = B(d) = pop1(f); }
    m = analyze(f, b, m, A(def));
    f = m ? f : encode(f, Oom);
    f = g_cons_c(f, A(nom), (*b)->stack);
    if (!g_ok(f)) return fail();
    (*b)->stack = pop1(f);
    // if toplevel then bind
    if (even && nilp((*b)->args))
      if (!(m = ana_i2(f, c, m, defglob, A(nom))))
        return fail(); }

  m = nn <= 1 ? pushs(f, 1, cataap) ? m + 1 : 0 :
                pushs(f, 2, cataapn, putnum(nn)) ? m + 2 : 0;
  for (nn++; nn--; (*b)->stack = B((*b)->stack));
  f->safe = mm;
  return m; }

// emits call instruction and modifies to tail call
// if next operation is return
static Cata(cataap) {
  if (k[0].ap == ret) k[0].ap = tap;
  else --k, k[0].ap = ap;
  return pull(f, c, k); }

static Cata(cataapn) {
  word n = *f->sp++;
  if (k[0].ap == ret) k[0].x = n, --k, k[0].ap = tapn;
  else --k, k[0].x = n, --k, k[0].ap = apn;
  return pull(f, c, k); }

static Cata(c2var) {
  word var = *f->sp++, stack = *f->sp++;
  long i = lidx(f, stack, var);
  k[-2].ap = ref;
  k[-1].x = putnum(i);
  return pull(f, c, k - 2); }

// emit stack reference instruction
static Cata(cata_var) {
  word var = *f->sp++, // variable name
       ins = llen(*f->sp++), // stack inset
       idx = stack_index_of_symbol(f, *c, var);
  k[-2].ap = ref;
  k[-1].x = putnum(idx + ins);
  return pull(f, c, k - 2); }

static Inline Cata(pull) {
  return ((cata*) (*f->sp++))(f, c, k); }

static Cata(cata_i) {
  k[-1].x = *f->sp++;
  return pull(f, c, k - 1); }

static Cata(cata_ix) {
  k[-2].x = *f->sp++;
  k[-1].x = *f->sp++;
  return pull(f, c, k - 2); }

static Cata(yieldk) { return k; }

// index of item in list
static long lidx(core *f, word l, word x) {
  for (long i = 0; !nilp(l); l = B(l), i++) if (eql(f, x, A(l))) return i;
  return -1; }

static g_core *mo_c(core *f, size_t n) {
  f = g_cells(f, n + Width(struct tag));
  if (g_ok(f)) {
    cell *k = (cell*) f->sp[0];
    struct tag *t = (struct tag*) (k + n);
    t->null = NULL, t->head = k; }
  return f; }

NoInline Vm(ev0) {
  Ip++;
  Pack(f);
  f = g_eval(f, g_yield);
  if (!g_ok(f)) return code_of(f);
  Unpack(f);
  return Continue(); }

Vm(defglob) {
  Have(3);
  g_word v = Sp[0];
  *--Sp = v;
  *--Sp = Ip[1].x;
  *--Sp = (g_word) f->dict;
  Pack(f);
  f = g_hash_put(f);
  if (!g_ok(f)) return code_of(f); 
  Unpack(f);
  Sp++;
  Ip += 2;
  return Continue(); }

Vm(drop1) {
  Ip++;
  Sp++;
  return Continue(); }

Vm(free_variable) {
  word x = Ip[1].x;
  Pack(f);
  f = g_push(f, 3, x, f->dict, x);
  if (!g_ok(f)) return Oom;
  f = g_hash_get(f);
  Unpack(f);
  Ip[0].ap = imm;
  Ip[1].x = *Sp++;
  return Continue(); }

Vm(late_bind) {
  word ref = Ip[1].x, lfd = ref;
  ref = AB(lfd);
  Ip[0].ap = imm;
  Ip[1].x = ref;
  return Continue(); }

// generic instruction ana handlers
static size_t ana_i2(core *f, env **c, size_t m, vm *i, word x) {
  return pushs(f, 3, cata_ix, i, x) ? m + 2 : 0; }

static g_core *alloc_thread_c(core *f, env **c, size_t m) {
  f = mo_c(f, m);
  if (g_ok(f)) {
    cell *k = (cell*) f->sp[0];
    memset(k, -1, m * sizeof(word));
    f->sp[0] = (word) (k + m); }
  return f; }

static g_core *enscope_c(core *f, env* par, word args, word imps) {
  f = g_push(f, 3, args, imps, par);
  f = mo_c(f, Width(env));
  if (g_ok(f)) {
    env *c = (env*) pop1(f);
    args = pop1(f), imps = pop1(f), par = (env*) pop1(f);
    c->args = args, c->imps = imps, c->par = par,
    c->stack = c->alts = c->ends = c->lams = nil;
    f = g_push(f, 1, c); }
  return f; }

static word lassoc(core *f, word l, word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
  return 0; }


// DEFINE
// let expressions
static word ldels(core *f, word lam, word l) {
  if (!twop(l)) return nil;
  word m = ldels(f, lam, B(l));
  if (!lassoc(f, lam, A(l))) B(l) = m, m = l;
  return m; }

// list length
static size_t llen(word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

static long stack_index_of_symbol(core *f, env *c, word var) {
  long l, i = 0;
  for (l = c->imps; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  for (l = c->args; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  return -1; }
