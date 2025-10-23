#include "i.h"

static g_core *mo_c(g_core *f, size_t n) {
  f = g_cells(f, n + Width(struct g_tag));
  if (g_ok(f)) {
    g_cell *k = (g_cell*) f->sp[0];
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
static cata pull, cata_i, cata_ix, cata_var, cata_ap, cata_yield, cata_ret;
static g_core *ana_seq(g_core*, env**, g_word, g_word);

// generic instruction ana handlers
static g_core *ana_ix(g_core *f, g_vm *i, g_word x) {
  return g_push(f, 3, cata_ix, i, x); }

// keep this separate and NoInline so g_eval can be tail call optimized if possible
NoInline g_core *g_ana(g_core *f, g_vm *y) {
  f = enscope(f, (env*) nil, nil, nil);
  if (!g_ok(f)) return f;
  env *c = (env*) pop1(f);
  g_word x = f->sp[0];
  f->sp[0] = (g_word) cata_yield;
  avec(f, c,
    avec(f, x, f = ana_ix(f, y, (g_word) f->ip)),
    f = analyze(f, &c, x),
    f = pull(f, &c, 0));
  return f; }

static Vm(uncurry) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return Continue(); }

// currying
Vm(curry) {
  g_cell *k = cell(Hp), *j = k;
  size_t S = 3 + Width(struct g_tag),
         n = getnum(Ip[1].x);

  if (n == 2) Have(S);
  else {
    S += 2;
    Have(S);
    j += 2;
    k[0].ap = curry;
    k[1].x = putnum(n - 1); }

  j[0].ap = uncurry;
  j[1].x = *Sp++;
  j[2].m = Ip + 2;
  j[3].x = 0;
  j[4].m = k;
  Hp += S;
  Ip = cell(*Sp);
  Sp[0] = word(k);
  return Continue(); }

Vm(jump) {
  Ip = Ip[1].m;
  return Continue(); }

Vm(cond) {
  Ip = nilp(*Sp++) ? Ip[1].m : Ip + 2;
  return Continue(); }
// load instructions
//
// push an immediate value
Vm(imm) {
  Have1();
  Sp -= 1;
  Sp[0] = Ip[1].x;
  Ip += 2;
  return Continue(); }

static Vm(g_yield) {
  Ip = Ip[1].m;
  Pack(f);
  return encode(f, YieldStatus); }

g_core *g_eval(g_core *f) {
  return g_run(g_ana(f, g_yield)); }

NoInline g_core *g_apply(g_core *f) {
  f = mo_c(f, 3);
  if (g_ok(f)) {
    g_cell *k = (g_cell*) pop1(f);
    k[0].ap = ap;
    k[1].ap = g_yield;
    k[2].m = f->ip;
    f->ip = k;
    f = g_run(f); }
  return f; }

NoInline Vm(ev0) {
  Ip += 1;
  Pack(f);
  f = g_ana(f, jump);
  if (!g_ok(f)) return f;
  Unpack(f);
  return Continue(); }

#define Kp (f->ip)
static Cata(cata_yield) {
  f = mo_c(f, m);
  if (g_ok(f))
    Kp = cell(pop1(f)),
    memset(Kp, -1, m * sizeof(g_word)),
    Kp += m;
  return f; }

static Cata(cata_if_pop_exit) {
  f = pull(f, c, m);
  if (g_ok(f)) (*c)->ends = B((*c)->ends); // pops cond expression exit address off env stack ends
  return f; }

static Cata(cata_ix_, g_vm *i, g_word x) {
  avec(f, x, f = pull(f, c, m + 2));
  if (g_ok(f))
    Kp -= 2,
    Kp[0].ap = i,
    Kp[1].x = x;
  return f; }

static Cata(cata_if_pop_branch) {
  f = pull(f, c, m + 2);
  if (g_ok(f))
    Kp -= 2,
    Kp[0].ap = cond,
    Kp[1].x = A((*c)->alts);
  (*c)->alts = B((*c)->alts);
  return f; }

static Cata(cata_if_push_branch) {
  f = pull(f, c, m);
  f = g_cons_2(f, (g_word) Kp, (*c)->alts);
  if (g_ok(f)) (*c)->alts = pop1(f);
  return f; }

static Cata(cata_if_push_exit) {
  f = pull(f, c, m);
  f = g_cons_2(f, (g_word) Kp, (*c)->ends);
  if (g_ok(f)) (*c)->ends = pop1(f);
  return f; }

static Cata(cata_if_jump_out) {
  f = pull(f, c, m + 3);
  if (g_ok(f)) {
    g_cell *a = cell(A((*c)->ends));
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
  f = pull(f, c, m + 1);
  if (g_ok(f)) {
    if (Kp[0].ap == ret) Kp[0].ap = tap;
    else Kp -= 1, Kp[0].ap = ap; }
  return f; }

static Cata(cata_apn) {
  g_word arity = pop1(f);
  f = pull(f, c, m + 2);
  if (g_ok(f)) {
    if (Kp[0].ap == ret) Kp -= 1, Kp[0].ap = tapn, Kp[1].x = arity;
    else Kp -= 2, Kp[0].ap = apn, Kp[1].x = arity; }
  return f; }

static Cata(cata_var_2) {
  g_word var = pop1(f),
         stack = pop1(f),
         i = 0;
  while (twop(stack))
    if (eql(f, A(stack), var)) break;
    else stack = B(stack), i++;
  return cata_ix_(f, c, m, ref, putnum(i)); }

// emit stack reference instruction
static Cata(cata_var) {
  g_word v = pop1(f), // variable name
         i = llen(pop1(f)); // stack inset
  for (g_word l = (*c)->imps; !nilp(l); l = B(l), i++) if (eql(f, v, A(l))) goto out;
  for (g_word l = (*c)->args; !nilp(l); l = B(l), i++) if (eql(f, v, A(l))) goto out;
out:
  return cata_ix_(f, c, m, ref, putnum(i)); }

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
  return cata_ix_(f, c, m, i, x); }

static size_t arity_of(env *c) { return llen(c->args) + llen(c->imps); }
static Cata(cata_curry) {
  size_t ar = arity_of((env*) pop1(f));
  return ar > 1 ? cata_ix_(f, c, m, curry, putnum(ar)) : pull(f, c, m); }

static Cata(cata_ret) {
  size_t ar = arity_of((env*) pop1(f));
  return cata_ix_(f, c, m, ret, putnum(ar)); }

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
    // closure or positional argument?
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
    f = g_apply(g_push(f, 2 , b, mac)),
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

  g_cell *k, *ip;
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
  return g_push(ana_if_r(f, c, x), 1, cata_if_pop_exit); }

static g_core *ana_seq(g_core *f, env* *c, g_word a, g_word b) {
  if (!g_ok(f)) return f;
  if (twop(b)) avec(f, a, f = ana_seq(f, c, A(b), B(b)),
                          f = g_push(f, 2, cata_i, drop1));
  return analyze(f, c, a); }

static g_core *ana_ap_args_r(g_core *f, env**c, g_word x) {
  if (!twop(x)) return f;
  avec(f, x, f = ana_ap_args_r(f, c, B(x)),
             f = g_push(f, 1, cata_ap));
  return analyze(f, c, A(x)); }

// evaluate function call arguments and apply
static g_core *ana_ap_args(g_core *f, env **c, g_word x) {
  avec(f, x, f = g_cons_2(f, nil, (*c)->stack));
  if (g_ok(f))
    (*c)->stack = pop1(f),
    f = ana_ap_args_r(f, c, x),
    (*c)->stack = B((*c)->stack);
  return f; }

static g_word ldels(g_core *f, g_word lam, g_word l) {
  if (!twop(l)) return nil;
  g_word m = ldels(f, lam, B(l));
  if (!assq(f, lam, A(l))) B(l) = m, m = l;
  return m; }

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
  g_word nom = nil, def = nil, lam = nil,
         v = nil, d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

  // collect vars and defs into two lists
  while (twop(exp) && twop(B(exp))) {
    for (d = A(exp), e = AB(exp); twop(d); e = pop1(f), d = A(d)) {
      f = g_cons_2(f, e, nil);
      f = append(g_cons_2(f, word(f->lambda), B(d)));
      if (!g_ok(f)) return forget(); }
    f = g_cons_2(f, d, nom);
    f = g_cons_2(f, e, def);
    if (!g_ok(f)) return forget();
    def = pop1(f);
    nom = pop1(f);
    // if it's a lambda compile it and record in lam list
    if (lambp(f, e)) {
      f = g_push(f, 2, d, lam);
      f = g_cons_l(g_cons_r(ana_lambda(f, c, nil, B(e))));
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
  f = append(g_cons_l(g_push(f, 3, f->lambda, nom, exp)));

  if (!g_ok(f)) return forget();
  exp = pop1(f);

  size_t ll = llen(nom);
  f = ll > 1 ? g_push(f, 2, cata_apn, putnum(ll)) :
               g_push(f, 1, cata_ap);
  f = g_cons_2(f, nil, (*b)->stack); // push function stack rep
  (*b)->stack = pop1(f);
  nom = reverse(f, nom); // put in literal order
  for (v = nom; g_ok(f) && twop(B(v)); v = B(v)) { // push initial variable stack reps
    f = g_cons_2(f, A(v), (*b)->stack);
    if (g_ok(f)) (*b)->stack = pop1(f); }


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

g_core *g_evals(g_core *f, const char *s) {
  f = g_read1s(f, "(:(r x y)(? y(r(ev'ev(A y))(B y))x)r)");
  f = g_push(f, 4, nil, f->quote, nil, nil);
  f = g_cons_r(g_cons_r(g_cons_l(g_cons_r(g_cons_l(g_readss(f, s))))));
  return g_eval(f); }

g_core *g_eval_(g_core *f)                 { return g_pop(g_eval(f), 1); }
g_core *g_evals_(g_core *f, const char *s) { return g_pop(g_evals(f, s), 1); }
Vm(defglob) {
  Have(3);
  Sp -= 3;
  g_table *t = f->dict;
  g_word k = Ip[1].x,
         v = Sp[3];
  Sp[0] = k;
  Sp[1] = v;
  Sp[2] = (g_word) t;
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
  g_word y = Ip[1].x,
         v = g_hash_get(f, y, f->dict, y); // see if it's defined now...
  Ip[0].ap = imm;
  Ip[1].x = v;
  return Continue(); }

Vm(late_bind) {
  g_word v = AB(Ip[1].x);
  Ip[0].ap = imm;
  Ip[1].x = v;
  return Continue(); }

Vm(data) {
  return Ap(t_ap[typ(Ip)], f); }

Vm(self) {
  g_word x = word(Ip);
  Sp += 1;
  Ip = cell(Sp[0]);
  Sp[0] = x;
  return Continue(); }


// push a value from the stack
Vm(ref) {
  Have1();
  Sp[-1] = Sp[getnum(Ip[1].x)];
  Sp -= 1;
  Ip += 2;
  return Continue(); }

// call and return
// apply function to one argument
Vm(ap) {
  if (nump(Sp[1])) return Ip++, Sp++, Continue();
  g_cell *k = cell(Sp[1]);
  Sp[1] = word(Ip + 1);
  Ip = k;
  return Continue(); }

// tail call
Vm(tap) {
  g_word x = Sp[0], j = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) return
    Sp += 1,
    Ip = cell(Sp[0]),
    Sp[0] = j,
    Continue();
  return
    Ip = cell(j),
    Sp[0] = x,
    Continue(); }

// apply to multiple arguments
Vm(apn) {
  size_t n = getnum(Ip[1].x);
  g_cell *ra = Ip + 2; // return address
  // this instruction is only emitted when the callee is known to be a function
  // so putting a value off the stack into Ip is safe. the +2 is cause we leave
  // the currying instruction in there... should be skipped in compiler instead FIXME
  Ip = cell(Sp[n]);
  Ip += 2;
  Sp[n] = word(ra); // store return address
  return Continue(); }

// tail call
Vm(tapn) {
  size_t n = getnum(Ip[1].x),
         r = getnum(Ip[2].x);
  Ip = cell(Sp[n]) + 2;
  g_word *o = Sp;
  for (Sp += r + 1; n--; Sp[n] = o[n]);
  return Continue(); }

// return
Vm(ret) {
  g_word n = getnum(Ip[1].x) + 1;
  Ip = cell(Sp[n]);
  Sp[n] = Sp[0];
  Sp += n;
  return Continue(); }
Vm(ret0) {
  Ip = cell(Sp[1]);
  Sp[1] = Sp[0];
  Sp += 1;
  return Continue(); }


#define op(nom, n, x) Vm(nom) { g_word _ = (x); *(Sp += n-1) = _; Ip++; return Continue(); }
op(add, 2, (Sp[0]+Sp[1]-1)|1)
op(sub, 2, (Sp[0]-Sp[1])|1)
op(mul, 2, putnum(getnum(Sp[0])*getnum(Sp[1])))
op(quot, 2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])/getnum(Sp[1])))
op(rem, 2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])%getnum(Sp[1])))
op(eq, 2, eql(f, Sp[0], Sp[1]) ? putnum(-1) : nil)
op(lt, 2, Sp[0] < Sp[1] ? putnum(-1) : nil)
op(le, 2, Sp[0] <= Sp[1] ? putnum(-1) : nil)
op(gt, 2, Sp[0] > Sp[1] ? putnum(-1) : nil)
op(ge, 2, Sp[0] >= Sp[1] ? putnum(-1) : nil)
op(bnot, 1, ~Sp[0] | 1)
op(band, 2, (Sp[0] & Sp[1]) | 1)
op(bor, 2, (Sp[0] | Sp[1]) | 1)
op(bxor, 2, (Sp[0] ^ Sp[1]) | 1)
op(bsr, 2, putnum(getnum(Sp[0]) >> getnum(Sp[1])))
op(bsl, 2, putnum(getnum(Sp[0]) << getnum(Sp[1])))
op(fixnump, 1, nump(Sp[0]) ? putnum(-1) : nil)

g_core *g_tbl(g_core *f) {
  f = g_cells(f, Width(g_table) + 1);
  if (g_ok(f)) {
    g_table *t = tbl(f->sp[0]);
    struct entry **tab = (struct entry**) (t + 1);
    tab[0] = 0;
    ini_table(t, 0, 1, tab); }
  return f; }

// general hashing method...
uintptr_t hash(g_core *f, g_word x) {
  if (nump(x)) {
    const int shift = sizeof(g_word) * 4;
    return x *= mix, (x << shift) | (x >> shift); }
  if (datp(x)) return t_xx[typ(x)](f, x);
  if (!owns(f, x)) return mix ^ (mix * x);

  // it's a function, hash by length
  struct g_tag *t = ttag((g_cell*) x);
  g_word len = (g_cell*) t - t->head;
  return mix ^ (mix * len); }

g_core* em_tbl(g_core*, g_file, g_word);
void wk_tbl(g_core*, g_word, g_word*, g_word*);
g_word cp_tbl(g_core*, g_word, g_word*, g_word*);
uintptr_t xx_tbl(g_core*, g_word);

g_core *em_tbl(g_core *f, g_file o, g_word x) {
  g_table *t = (g_table*) x;
  g_fprintf(o, "#table:%ld/%ld@%lx", (long) t->len, (long) t->cap, (long) x);
  return f; }

void wk_tbl(g_core *f, g_word x, g_word *p0, g_word *t0) {
  g_table *t = (g_table*) x;
  f->cp += Width(g_table) + t->cap + t->len * Width(struct entry);
  for (g_word i = 0, lim = t->cap; i < lim; i++)
    for (struct entry*e = t->tab[i]; e;
      e->key = cp(f, e->key, p0, t0),
      e->val = cp(f, e->val, p0, t0),
      e = e->next); }

g_word cp_tbl(g_core *f, g_word x, g_word *p0, g_word *t0) {
  g_table *src = (g_table*) x;
  size_t len = src->len, cap = src->cap;
  g_table *dst = bump(f, Width(g_table) + cap + Width(struct entry) * len);
  struct entry **tab = (struct entry**) (dst + 1),
               *dd = (struct entry*) (tab + cap);
  ini_table(dst, len, cap, tab);
  src->ap = (g_vm*) dst;
  for (struct entry *d, *s, *last; cap--; tab[cap] = last)
    for (s = src->tab[cap], last = NULL; s;
      d = dd++,
      d->key = s->key,
      d->val = s->val,
      d->next = last,
      last = d,
      s = s->next);
  return word(dst); }

// FIXME very poor hashing method :(
uintptr_t xx_tbl(g_core *f, g_word h) { return mix; }

// relies on table capacity being a power of 2
static Inline g_word index_of_key(g_core *f, g_table *t, g_word k) {
  return (t->cap - 1) & hash(f, k); }

NoInline g_core *g_hash_put(g_core *f) {
  g_table *t = (g_table*) f->sp[2];
  g_word v = f->sp[1],
         k = f->sp[0];

  g_word i = index_of_key(f, t, k);
  struct entry *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;

  if (e) {
    e->val = v;
    goto done; }

  f = g_cells(f, Width(struct entry));
  if (!g_ok(f)) return f;

  e = (struct entry*) pop1(f);
  t = (g_table*) f->sp[2];
  k = f->sp[0];
  v = f->sp[1];

  e->key = k;
  e->val = v;
  e->next = t->tab[i];
  t->tab[i] = e;

  g_word cap0 = t->cap,
         load = ++t->len / cap0;

  if (load < 2) goto done;

  // grow the table
  g_word cap1 = 2 * cap0;
  struct entry **tab0, **tab1;
  f = g_cells(f, cap1);
  if (!g_ok(f)) return f;
  tab1 = (struct entry**) pop1(f);
  t = (g_table*) f->sp[2];
  tab0 = t->tab;
  memset(tab1, 0, cap1 * sizeof(g_word));
  for (t->cap = cap1, t->tab = tab1; cap0--;)
    for (struct entry *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = (cap1-1) & hash(f, e->key),
      e->next = tab1[i],
      tab1[i] = e);

done:
  f->sp += 2;
  return f; }

  
static struct entry *table_delete_r(g_core *f, g_table *t, g_word k, g_word *v, struct entry *e) {
  if (!e) return e;
  if (eql(f, e->key, k)) return
    t->len--,
    *v = e->val,
    e->next;
  e->next = table_delete_r(f, t, k, v, e->next);
  return e; }

static NoInline g_word table_delete(g_core *f, g_table *t, g_word k, g_word v) {
  g_word idx = index_of_key(f, t, k);
  t->tab[idx] = table_delete_r(f, t, k, &v, t->tab[idx]);
  if (t->cap > 1 && t->len / t->cap < 1) {
    g_word cap = t->cap;
    struct entry *coll = 0, *x, *y; // collect all entries in one list
    for (g_word i = 0; i < cap; i++)
      for (x = t->tab[i], t->tab[i] = 0; x;)
        y = x, x = x->next, y->next = coll, coll = y;
    t->cap = cap >>= 1;
    for (g_word i; coll;)
      i = (cap - 1) & hash(f, coll->key),
      x = coll->next,
      coll->next = t->tab[i],
      t->tab[i] = coll,
      coll = x; }
  return v; }

Vm(tnew) {
  Have(Width(g_table) + 1);
  g_table *t = (g_table*) Hp;
  struct entry **tab = (struct entry**) (t + 1);
  Hp += Width(g_table) + 1;
  tab[0] = 0;
  ini_table(t, 0, 1, tab);
  Sp[0] = (g_word) t;
  Ip++;
  return Continue(); }

g_word g_hash_get(g_core *f, g_word zero, g_table *t, g_word k) {
  size_t i = index_of_key(f, t, k);
  struct entry *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;
  return e ? e->val : zero; }

Vm(tget) {
  Sp[2] = g_hash_get(f, Sp[0], tbl(Sp[1]), Sp[2]);
  Sp += 2;
  Ip += 1;
  return Continue(); }

Vm(tset) {
  if (tblp(Sp[0])) {
    g_word t = Sp[0],
           k = Sp[1],
           v = Sp[2];
    Sp[0] = k;
    Sp[1] = v;
    Sp[2] = t;
    Pack(f);
    f = g_hash_put(f);
    if (!g_ok(f)) return f;
    Unpack(f); }
  Ip += 1;
  return Continue(); }

Vm(tdel) {
  Sp[2] = !tblp(Sp[1]) ? nil : table_delete(f, (g_table*) Sp[1], Sp[2], Sp[0]);
  Sp += 2;
  Ip += 1;
  return Continue(); }

Vm(tlen) {
  Sp[0] = tblp(Sp[0]) ? putnum(((g_table*)Sp[0])->len) : nil;
  Ip += 1;
  return Continue(); }

Vm(tkeys) {
  g_word list = nil;
  if (tblp(Sp[0])) {
    g_table *t = (g_table*) Sp[0];
    g_word len = t->len;
    Have(len * Width(g_pair));
    g_pair *pairs = (g_pair*) Hp;
    Hp += len * Width(g_pair);
    for (int i = t->cap; i;)
      for (struct entry *e = t->tab[--i]; e; e = e->next)
        ini_pair(pairs, e->key, list),
        list = (g_word) pairs, pairs++; }
  Sp[0] = list;
  Ip += 1;
  return Continue(); }
static symbol *g_intern_r(g_core *v, string *b, symbol **y) {
  symbol *z = *y;
  if (!z) return // found an empty spot, insert new symbol
    z = bump(v, Width(g_symbol)),
    ini_sym(z, b, hash(v, putnum(hash(v, (g_word) b)))),
    *y = z;
  string *a = z->nom;
  int i = a->len < b->len ? -1 :
          a->len > b->len ? 1 :
          strncmp(a->text, b->text, a->len);
  return i == 0 ? z :
    g_intern_r(v, b, i < 0 ? &z->l : &z->r); }

g_core *g_intern(g_core *f) {
  f = g_have(f, Width(symbol));
  if (g_ok(f)) f->sp[0] = (g_word) g_intern_r(f, str(f->sp[0]), &f->symbols);
  return f; }

Vm(nomsym) {
  Have(Width(symbol));
  symbol *y;
  Pack(f);
  y = g_intern_r(f, str(f->sp[0]), &f->symbols),
  Unpack(f);
  Sp[0] = word(y);
  Ip += 1;
  return Continue(); }

Vm(gensym) {
  if (strp(Sp[0])) return Ap(nomsym, f);
  const uintptr_t req = Width(symbol) - 2;
  Have(req);
  symbol *y = (symbol*) Hp;
  Hp += req;
  ini_anon(y, g_sys_clock());
  Sp[0] = word(y);
  Ip += 1;
  return Continue(); }

Vm(symnom) {
  g_word y = Sp[0];
  y = symp(y) && ((symbol*)y)->nom ? word(((symbol*)y)->nom) : nil;
  Sp[0] = y;
  Ip += 1;
  return Continue(); }

g_word cp_sym(g_core *f, g_word x, g_word *p0, g_word *t0);
uintptr_t xx_sym(g_core *v, g_word _);
void wk_sym(g_core *f, g_word x, g_word *p0, g_word *t0);
g_core * em_sym(g_core *f, g_file o, g_word x);

uintptr_t xx_sym(g_core *v, g_word _) { return sym(_)->code; }

g_word cp_sym(g_core *f, g_word x, g_word *p0, g_word *t0) {
  g_symbol *src = sym(x), *dst;
  if (src->nom) dst = g_intern_r(f, str(cp(f, word(src->nom), p0, t0)), &f->symbols);
  else dst = bump(f, Width(symbol) - 2),
       ini_anon(dst, src->code);
  return (g_word) (src->ap = (g_vm*) dst); }

void wk_sym(g_core *f, g_word x, g_word *p0, g_word *t0) {
  f->cp += Width(symbol) - (sym(x)->nom ? 0 : 2); }

g_core *em_sym(g_core *f, g_file o, g_word x) {
  string* s = sym(x)->nom;
  if (s) for (uintptr_t i = 0; i < s->len; g_fputc(s->text[i++], o));
  else g_fprintf(o, "#sym@%lx", (long) x);
  return f; }

Vm(symbolp) {
  Sp[0] = symp(Sp[0]) ? putnum(-1) : nil;
  Ip += 1;
  return Continue(); }
g_core *g_strof(g_core *f, const char *cs) {
  size_t bytes = strlen(cs),
         words = b2w(bytes),
         req = Width(string) + words;
  f = g_cells(f, req);
  if (g_ok(f)) {
    g_string *o = str(f->sp[0]);
    ini_str(o, bytes);
    memcpy(o->text, cs, bytes); }
  return f; }

g_word cp_str(g_core* v, g_word x, g_word *p0, g_word *t0) {
  string *src = str(x);
  size_t len = sizeof(string) + src->len;
  return (g_word) (src->ap = memcpy(bump(v, b2w(len)), src, len)); }

void wk_str(g_core* f, g_word x, g_word *p0, g_word *t0) {
  f->cp += Width(string) + b2w(str(x)->len); }

g_core *em_str(g_core* v, g_file o, g_word _) {
  size_t len = str(_)->len;
  const char *text = str(_)->text;
  g_fputc('"', o);
  for (char c; len--; g_fputc(c, o))
    if ((c = *text++) == '\\' || c == '"') g_fputc('\\', o);
  g_fputc('"', o);
  return v; }

uintptr_t xx_str(g_core *v, g_word _) {
  uintptr_t len = str(_)->len, h = 2166136261;
  unsigned char *bs = (unsigned char*) str(_)->text;
  while (len--) h ^= *bs++, h *= 16777619;
  return h; }

bool eq_str(g_core *f, g_word x, g_word y) {
  string *a = str(x), *b = str(y);
  return a->len == b->len &&
    0 == strncmp(a->text, b->text, a->len); }

Vm(slen) {
  Sp[0] = strp(Sp[0]) ? putnum(str(Sp[0])->len) : nil;
  Ip += 1;
  return Continue(); }

Vm(ssub) {
  if (!strp(Sp[0])) Sp[2] = nil;
  else {
    string *s = str(Sp[0]);
    intptr_t i = nump(Sp[1]) ? getnum(Sp[1]) : 0,
             j = nump(Sp[2]) ? getnum(Sp[2]) : 0;
    i = MAX(i, 0);
    i = MIN(i, (intptr_t) s->len);
    j = MAX(j, i);
    j = MIN(j, (intptr_t) s->len);
    if (i == j) Sp[2] = nil;
    else {
      size_t req = Width(string) + b2w(j - i);
      Have(req);
      string* t = str(Hp);
      Hp += req;
      ini_str(t, j - i);
      memcpy(t->text, s->text + i, j - i);
      Sp[2] = (g_word) t; } }
  Ip += 1;
  Sp += 2;
  return Continue(); }

static Inline size_t max(size_t a, size_t b) { return a > b ? a : b; }
static Inline size_t min(size_t a, size_t b) { return a < b ? a : b; }
Vm(sget) {
  if (!strp(Sp[0])) Sp[1] = nil;
  else {
    string *s = str(Sp[0]);
    size_t i = min(s->len - 1, getnum(Sp[1]));
    i = max(i, 0);
    Sp[1] = putnum(s->text[i]); }
  return Ip += 1,
         Sp += 1,
         Continue(); }

Vm(scat) {
  g_word a = Sp[0];
  if (!strp(a)) return Sp += 1,
                       Ip += 1,
                       Continue();
  g_word b = Sp[1];
  if (!strp(b)) return Sp[1] = a,
                       Sp += 1,
                       Ip += 1,
                       Continue();

  string *x = str(a), *y = str(b);
  size_t len = x->len + y->len,
         req = Width(string) + b2w(len);
  Have(req);
  string *z = str(Hp);
  return Hp += req,
         ini_str(z, len),
         memcpy(z->text, x->text, x->len),
         memcpy(z->text + x->len, y->text, y->len),
         Sp[1] = word(z),
         Ip += 1,
         Continue(); }

Vm(stringp) {
  Sp[0] = strp(Sp[0]) ? putnum(-1) : nil;
  Ip += 1;
  return Continue(); }

// FIXME could overflow the stack -- use off pool for this
bool eq_two(g_core *f, g_word x, g_word y) {
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

g_word cp_two(g_core *v, g_word x, g_word *p0, g_word *t0) {
  g_pair *src = (g_pair*) x,
         *dst = bump(v, Width(g_pair));
  ini_pair(dst, src->a, src->b);
  return word(src->ap = (g_vm*) dst); }

void wk_two(g_core *f, g_word x, g_word *p0, g_word *t0) {
  f->cp += Width(g_pair);
  A(x) = cp(f, A(x), p0, t0);
  B(x) = cp(f, B(x), p0, t0); }

uintptr_t xx_two(g_core *f, g_word x) {
  uintptr_t hc = hash(f, A(x)) * hash(f, B(x));
  return hc ^ mix; }

g_core *em_two(g_core *f, g_file o, g_word x) {
  if (A(x) == word(f->quote) && twop(B(x)))
    g_fputc('\'', o),
    transmit(f, o, AB(x));
  else for (g_fputc('(', o);; g_fputc(' ', o)) {
    transmit(f, o, A(x));
    if (!twop(x = B(x))) { g_fputc(')', o); break; } }
  return f; }

Vm(car) {
  Sp[0] = twop(Sp[0]) ? A(Sp[0]) : Sp[0];
  Ip++;
  return Continue(); }
Vm(cdr) {
  Sp[0] = twop(Sp[0]) ? B(Sp[0]) : nil;
  Ip++;
  return Continue(); }

Vm(cons) {
  Have(Width(g_pair));
  g_pair *w = (g_pair*) Hp;
  ini_pair(w, Sp[0], Sp[1]);
  Hp += Width(g_pair);
  Sp[1] = word(w);
  Sp++;
  Ip++;
  return Continue(); }

Vm(pairp) {
  Sp[0] = twop(Sp[0]) ? putnum(-1) : nil;
  Ip++;
  return Continue(); }

static g_core *g_cons_stack(g_core *f, int i, int j) {
  f = g_have(f, Width(g_pair));
  if (g_ok(f)) {
    g_pair *p = (g_pair*) f->hp;
    ini_pair(p, f->sp[i], f->sp[j]);
    f->hp += Width(g_pair);
    *++f->sp = (g_word) p; }
  return f; }

g_core *g_cons_l(g_core *f) { return g_cons_stack(f, 0, 1); }
g_core *g_cons_r(g_core *f) { return g_cons_stack(f, 1, 0); }

g_cp_t *t_cp[] = {
  [g_ty_two] = cp_two,
  [g_ty_sym] = cp_sym,
  [g_ty_str] = cp_str,
  [g_ty_tbl] = cp_tbl, };
g_wk_t *t_wk[] = {
  [g_ty_two] = wk_two,
  [g_ty_sym] = wk_sym,
  [g_ty_tbl] = wk_tbl,
  [g_ty_str] = wk_str, };
g_xx_t *t_xx[] = {
  [g_ty_two] = xx_two,
  [g_ty_sym] = xx_sym,
  [g_ty_str] = xx_str,
  [g_ty_tbl] = xx_tbl, };
g_em_t *t_em[] = {
  [g_ty_two] = em_two,
  [g_ty_tbl] = em_tbl,
  [g_ty_sym] = em_sym,
  [g_ty_str] = em_str, };
g_vm *t_ap[] = {
  [g_ty_two] = self,
  [g_ty_tbl] = self,
  [g_ty_sym] = self,
  [g_ty_str] = self, };
g_id_t *t_id[] = {
  [g_ty_two] = eq_two,
  [g_ty_sym] = neql,
  [g_ty_tbl] = neql,
  [g_ty_str] = eq_str, };

void ini_pair(g_pair *w, g_word a, g_word b) {
  w->ap = data;
  w->typ = g_ty_two;
  w->a = a;
  w->b = b; }

void ini_table(g_table *t, uintptr_t len, uintptr_t cap, struct entry**tab) {
  t->ap = data;
  t->typ = g_ty_tbl;
  t->len = len;
  t->cap = cap;
  t->tab = tab; }

void ini_str(g_string *s, uintptr_t len) {
  s->ap = data;
  s->typ = g_ty_str;
  s->len = len; }

void ini_sym(symbol *y, string *nom, uintptr_t code) {
  y->ap = data;
  y->typ = g_ty_sym;
  y->nom = nom;
  y->code = code;
  y->l = y->r = 0; }

void ini_anon(symbol *y, uintptr_t code) {
  y->ap = data;
  y->typ = g_ty_sym;
  y->nom = 0;
  y->code = code; }

bool twop(g_word _) { return celp(_) && typ(_) == g_ty_two; }
bool strp(g_word _) { return celp(_) && typ(_) == g_ty_str; }
bool tblp(g_word _) { return celp(_) && typ(_) == g_ty_tbl; }
bool symp(g_word _) { return celp(_) && typ(_) == g_ty_sym; }

static NoInline bool eql_neq(g_core *f, g_word a, g_word b) {
  return celp(a | b) &&
         cell(a)->ap == data &&
         cell(b)->ap == data &&
         typ(a) == typ(b) &&
         t_id[typ(a)](f, a, b); }

// default equality method for things that are only equal to themselves
bool neql(g_core *f, g_word a, g_word b) { return false; }

bool eql(g_core *f, g_word a, g_word b) {
  return a == b || eql_neq(f, a, b); }


#include <stdarg.h>

g_core *g_pop(g_core *f, uintptr_t m) {
  if (g_ok(f)) f->sp += m;
  return f; }

static g_core *g_pushr(g_core *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (n == m) return please(f, m);
  g_word x = va_arg(xs, g_word);
  MM(f, &x);
  f = g_pushr(f, m, n + 1, xs);
  UM(f);
  if (g_ok(f)) *--f->sp = x;
  return f; }

#define avail(f) ((uintptr_t)(f->sp-f->hp))
g_core *g_push(g_core *f, uintptr_t m, ...) {
  if (!g_ok(f)) return f;
  va_list xs;
  va_start(xs, m);
  uintptr_t n = 0;
  if (avail(f) < m) f = g_pushr(f, m, n, xs);
  else for (f->sp -= m; n < m; f->sp[n++] = va_arg(xs, g_word));
  va_end(xs);
  return f; }

g_core *g_have(g_core *f, uintptr_t n) {
  return !g_ok(f) || avail(f) >= n ? f : please(f, n); }

g_core *g_cells(g_core *f, uintptr_t n) {
  f = g_have(f, n + 1);
  if (g_ok(f)) {
    g_cell *k = (g_cell*) f->hp;
    f->hp += n;
    *--f->sp = word(k); }
  return f; }

NoInline Vm(gc, uintptr_t n) {
  Pack(f);
  f = please(f, n);
  if (g_ok(f)) return Unpack(f), Continue();
  return f; }

static g_core *copy_core(g_core*, g_word*, uintptr_t, g_core*);

// keep v between
#define v_lo 8
// and
#define v_hi (v_lo << 6)
// where
//   v = (t2 - t0) / (t2 - t1)
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
NoInline g_core *please(g_core *f, uintptr_t req0) {
  size_t t0 = f->t0, t1 = g_sys_clock(),
         len0 = f->len;
  g_word *w = (g_word*) f, *w0 = f->pool;
  g_core *g = (g_core*) (w == w0 ? w0 + len0 : w0);
  f = copy_core(g, f->pool, f->len, f);
  size_t t2 = f->t0,      // get and set last gc end time
         req = req0 + len0 - avail(f),
         v = t2 == t1 ?  v_hi : (t2 - t0) / (t2 - t1),
         len1 = len0;

#define too_small (len1 < req || v < v_lo)
#define grow() (len1<<=1,v<<=1)
#define too_big (len1 >> 1 > req && v > v_hi)
#define shrink() (len1>>=1,v>>=1)
  if      (too_big) do shrink(); while (too_big);   // too big -> calculate smaller size
  else if (too_small) do grow(); while (too_small); // too small -> calculate bigger size
  else return f;                                    // just right -> all done

  // allocate a new pool with target size
  g = f->malloc(f, len1 * 2 * sizeof(g_word));
  if (!g) {
    if (req <= len0) return f;
    return encode(f, g_status_oom); }
  g = copy_core(g, (g_word*) g, len1, f);
  f->free(f, f->pool);
  return g; }

static g_core *copy_core(g_core *g, g_word *p1, uintptr_t len1, g_core *f) {
  g->free = f->free;
  g->malloc = f->malloc;
  g->pool = p1;
  g->len = len1;

  uintptr_t len0 = f->len;
  g_word *p0 = (g_word*) f,
         *t0 = (g_word*) f + len0, // source pool top
         *t1 = (g_word*) g + len1, // target pool top
         ht = t0 - f->sp; // stack height

  // reset stack, heap, symbols
  g->sp = t1 - ht;
  g->hp = g->cp = g->end;
  g->symbols = 0;

  // copy variables
  for (int n = 0; n < g_var_N; n++)
    g->vars[n] = cp(g, f->vars[n], p0, t0);

  // copy stack
  for (int n = 0; n < ht; n++)
    g->sp[n] = cp(g, f->sp[n], p0, t0);

  // copy saved values
  for (struct root *r = g->safe = f->safe; r; r = r->next)
    *r->ptr = cp(g, *r->ptr, p0, t0);

  // use cheney's algorithm to avoid unbounded recursion
  while (g->cp < g->hp)
    if (datp(g->cp)) t_wk[typ(g->cp)](g, (g_word) g->cp, p0, t0);
    else for (g->cp += 2; g->cp[-2]; g->cp++)
      g->cp[-2] = cp(g, g->cp[-2], p0, t0);

  g->t0 = g_sys_clock();
  return g; }

NoInline g_word cp(g_core *f, g_word x, g_word *p0, g_word *t0) {
  // if it's a number or it's outside managed memory then return it
  if (nump(x) || !within(p0, x, t0)) return x;
  g_cell *src = (g_cell*) x;
  x = src->x; // get its contents
  // if it contains a pointer to the new space then return the pointer
  if (!nump(x) && within((g_word*) f, x, (g_word*) f + f->len)) return x;
  // if it's data then call the copy function
  if (x == (g_word) data) return t_cp[typ(src)](f, (g_word) src, p0, t0);
  // it's a thread, find the end to find the head
  struct g_tag *t = ttag(src);
  g_cell *ini = t->head,
         *d = bump(f, t->end - ini),
         *dst = d;
  // copy source contents to dest and write dest addresses to source
  for (g_cell *s = ini; (d->x = s->x); s++->x = (g_word) d++);
  ((struct g_tag*) d)->head = dst;
  return (g_word) (dst + (src - ini)); }

static Vm(sysclock) {
  Sp[0] = putnum(g_sys_clock());
  Ip += 1;
  return Continue(); }


static Vm(nullp) {
  Sp[0] = nilp(Sp[0]) ? putnum(-1) : nil;
  Ip += 1;
  return Continue(); }

#ifndef g_version
#define g_version ""
#endif
#define S1(i) {{i}, {ret0}}
#define S2(i) {{curry},{.x=putnum(2)},{i}, {ret0}}
#define S3(i) {{curry},{.x=putnum(3)},{i}, {ret0}}
#define bifs(_) \
  _(bif_clock, "clock", S1(sysclock))\
  _(bif_add, "+", S2(add)) _(bif_sub, "-", S2(sub)) _(bif_mul, "*", S2(mul)) _(bif_quot, "/", S2(quot)) _(bif_rem, "%", S2(rem)) \
  _(bif_lt, "<", S2(lt))  _(bif_le, "<=", S2(le)) _(bif_eq, "=", S2(eq)) _(bif_ge, ">=", S2(ge))  _(bif_gt, ">", S2(gt)) \
  _(bif_bnot, "~", S1(bnot)) _(bif_bsl, "<<", S2(bsl)) _(bif_bsr, ">>", S2(bsr))\
  _(bif_band, "&", S2(band)) _(bif_bor, "|", S2(bor)) _(bif_bxor, "^", S2(bxor))\
  _(bif_cons, "X", S2(cons)) _(bif_car, "A", S1(car)) _(bif_cdr, "B", S1(cdr)) \
  _(bif_sget, "sget", S2(sget)) _(bif_ssub, "ssub", S3(ssub)) _(bif_slen, "slen", S1(slen)) _(bif_scat, "scat", S2(scat)) \
  _(bif_dot, ".", S1(dot)) _(bif_putc, "putc", S1(prc)) \
  _(bif_sym, "sym", S1(gensym)) _(bif_nom, "nom", S1(symnom))\
  _(bif_thd, "thd", S1(thda)) _(bif_peek, "peek", S1(peek)) _(bif_poke, "poke", S2(poke)) _(bif_trim, "trim", S1(trim)) _(bif_seek, "seek", S2(seek)) \
  _(bif_tnew, "tnew", S1(tnew)) _(bif_tkeys, "tkeys", S1(tkeys)) _(bif_tlen, "tlen", S1(tlen)) _(bif_tset, "tset", S3(tset)) _(bif_tget, "tget", S3(tget)) _(bif_tdel, "tdel", S3(tdel))\
  _(bif_twop, "twop", S1(pairp)) _(bif_strp, "strp", S1(stringp)) _(bif_symp, "symp", S1(symbolp)) _(bif_nump, "nump", S1(fixnump)) _(bif_nilp, "nilp", S1(nullp))\
  _(bif_ev, "ev", S1(ev0))
#define built_in_function(n, _, d) static const union g_cell n[] = d;
#define biff(b, n, _) {n, b},
bifs(built_in_function);
static Vm(g_stop) { return Pack(f), f; }
static union g_cell bif_stop[] = { {g_stop} };
static const struct { const char *n; const union g_cell *x; } bifff[] = { bifs(biff) };

#define insts(_)\
  _(free_variable) _(ret) _(ap) _(tap) _(apn) _(tapn) _(jump) _(cond) _(ref) _(imm) _(drop1) _(curry) _(defglob) _(late_bind) _(ret0)
#define i_entry(i) {"i_"#i,i},
static const struct {
  const char *n;
  g_vm *i;
} i_dict[] = {
  insts(i_entry)
};

static g_core *g_symof(g_core *f, const char *nom) {
  return g_intern(g_strof(f, nom)); }

static g_core *g_ini_def(g_core *f, const char *k, g_word v) {
  return g_hash_put(g_symof(g_push(f, 1, v), k)); }

g_core *g_define(g_core *f, const char *s) {
  if (g_ok(f)) f = g_intern(g_strof(g_push(f, 1, f->dict), s));
  if (!g_ok(f)) return f;
  g_word w = f->sp[1];
  f->sp[1] = f->sp[2];
  f->sp[2] = w;
  return g_pop(g_hash_put(f), 1); }


static void *g_static_malloc(g_core*f, size_t n) { return NULL; }
static void g_static_free(g_core*f, void*x) {}
static void *g_libc_malloc(g_core*f, size_t n) { return malloc(n); }
static void g_libc_free(g_core*f, void*x) { return free(x); }

static g_core *g_ini_0(g_core*, g_malloc_t*, g_free_t*, size_t);
g_core *g_ini_static(size_t n, void *f) { return g_ini_0(f, g_static_malloc, g_static_free, n / (2 * sizeof(g_word))); }
g_core *g_ini(void) { return g_ini_dynamic(g_libc_malloc, g_libc_free); }
g_core *g_ini_dynamic(g_malloc_t *ma, g_free_t *fr) {
  const size_t len0 = 1 << 10;
  return g_ini_0(ma(NULL, 2 * len0 * sizeof(g_word)), ma, fr, len0); }
// this is the general initialization function. arguments are
// - ma: malloc function pointer
// - fr: free function pointer
// - len0: initial semispace size in words (== total_space_size / 2)
// - f: core pointer
static struct g *g_ini_0(g_core *f, g_malloc_t *ma, g_free_t *fr, size_t len0) {
  if (f == NULL                            || // fail if pointer is null
      g_code_of(f)                         || //   or if pointer is not word aligned
      len0 * sizeof(g_word) < sizeof(g_core)) //   or if space is not large enough
    return encode(NULL, g_status_oom);
  memset(f, 0, sizeof(g_core));
  f->pool = (g_word*) f;
  f->len = len0;
  f->malloc = ma;
  f->free = fr;
  f->hp = f->end;
  f->sp = (g_word*) f + len0;
  f->ip = bif_stop;
  f->t0 = g_sys_clock(); // this goes right before first allocation so gc always sees initialized t0
  f = g_symof(f, ":");
  f = g_symof(f, "?");
  f = g_symof(f, "`");
  f = g_symof(f, ",");
  f = g_symof(f, "\\");
  if (g_ok(f)) // these must be in reverse order from above
    f->lambda = sym(pop1(f)),
    f->begin = sym(pop1(f)),
    f->quote = sym(pop1(f)),
    f->cond = sym(pop1(f)),
    f->let = sym(pop1(f));
  f = g_tbl(f); // dict
  f = g_tbl(f); // macro
  if (g_ok(f))
    f->macro = tbl(f->sp[0]),
    f->dict = tbl(f->sp[1]),
    f = g_symof(f, "macros"),
    f = g_hash_put(f),
    f = g_ini_def(f, "globals", (g_word) f->dict);
  for (size_t i = 0; i < LEN(bifff); i++)
    f = g_ini_def(f, bifff[i].n, (g_word) bifff[i].x);
  for (size_t i = 0; i < LEN(i_dict); i++)
    f = g_ini_def(f, i_dict[i].n, (g_word) i_dict[i].i);
  return g_pop(f, 1); }

enum g_status g_fin(g_core *f) {
  enum g_status s = g_code_of(f);
  if ((f = g_core_of(f))) f->free(f, f->pool);
  return s; }


static Inline int p_in_getc(input *i) { return i->getc(i); }
static Inline int p_in_ungetc(input *i, int c) { return i->ungetc(i, c); }
static Inline int p_in_eof(input *i) { return i->eof(i); }
typedef struct text_input {
  g_input in;
  const char *text;
  int i;
} text_input;

static int p_text_getc(g_input *i) {
  text_input *t = ((text_input*) i);
  char c = t->text[t->i];
  if (c) t->i++;
  return c ? c : EOF; }

static int p_text_ungetc(g_input *i, int _) {
  text_input *t = ((text_input*) i);
  int idx = t->i;
  idx = idx ? idx - 1 : idx;
  t->i = idx;
  return t->text[idx]; }

static int p_text_eof(g_input *i) {
  text_input *t = (text_input*) i;
  return !t->text[t->i]; }


NoInline g_core *g_read1s(g_core *f, const char *cs) {
  f = g_readss(f, cs);
  if (g_ok(f)) f->sp[0] = A(f->sp[0]);
  return f; }

NoInline g_core *g_readss(g_core *f, const char *cs) {
  text_input t = {{p_text_getc, p_text_ungetc, p_text_eof}, cs, 0};
  f = g_readsi(f, (input*) &t);
  return f; }

////
/// " the parser "
//
//
// get the next significant character from the stream
static int read_char(input *i) {
  for (int c;;) switch (c = p_in_getc(i)) {
    default: return c;
    case '#': case ';': while (!p_in_eof(i) && (c = p_in_getc(i)) != '\n' && c != '\r');
    case ' ': case '\t': case '\n': case '\r': case '\f': continue; } }
static g_core *g_buf_new(g_core *f) {
  f = g_cells(f, Width(string) + 1);
  if (g_ok(f)) {
    string *o = (string*) f->sp[0];
    ini_str(o, sizeof(g_word)); }
  return f; }

static g_core *g_buf_grow(g_core *f) {
  size_t len = str(f->sp[0])->len,
         req = Width(string) + 2 * b2w(len);
  f = g_have(f, req);
  if (g_ok(f)) {
    string *o = (string*) f->hp;
    f->hp += req;
    ini_str(o, 2 * len);
    memcpy(o->text, str(f->sp[0])->text, len);
    f->sp[0] = (g_word) o; }
  return f; }


g_core *g_readsi(g_core *f, input* i) {
  intptr_t n = 0;
  for (int c; g_ok(f); n++) {
    c = read_char(i);
    if (c == EOF || c == ')') break;
    p_in_ungetc(i, c);
    f = g_read1i(f, i); }
  for (f = g_push(f, 1, nil); n--; f = g_cons_r(f));
  return f; }


g_core *g_read1i(g_core *f, input* i) {
  if (!g_ok(f)) return f;
  int c = read_char(i);
  size_t n = 0;
  switch (c) {
    case '(':  return g_readsi(f, i);
    case ')':  return g_push(f, 1, nil);
    case EOF:  return encode(f, g_status_eof);
    case '\'': return g_cons_r(g_cons_l(g_read1i(g_push(f, 2, nil, f->quote), i)));
    case '"':  
      f = g_buf_new(f);
      for (size_t lim = sizeof(g_word); g_ok(f); f = g_buf_grow(f), lim *= 2)
        for (string *b = str(f->sp[0]); n < lim; b->text[n++] = c)
          if ((c = p_in_getc(i)) == EOF || c == '"' ||
               (c == '\\' && (c = p_in_getc(i)) == EOF))
            return b->len = n, f;
      return f;
    default:
      p_in_ungetc(i, c);
      f = g_buf_new(f);
      for (size_t lim = sizeof(g_word); g_ok(f); f = g_buf_grow(f), lim *= 2)
        for (string *b = str(f->sp[0]); n < lim; b->text[n++] = c)
          switch (c = p_in_getc(i)) {
            default: continue;
            case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
            case '(': case ')': case '"': case '\'': case EOF:
              p_in_ungetc(i, c);
              b->len = n;
              b->text[n] = 0; // zero terminate for strtol ; n < lim so this is safe
              char *e;
              long j = strtol(b->text, &e, 0);
              if (*e == 0) f->sp[0] = putnum(j);
              else f = g_intern(f);
              return f; }
      return f; } }

Vm(prc) {
  g_fputc(getnum(*Sp), g_stdout);
  Ip += 1;
  return Continue(); }

Vm(dot) {
  transmit(f, g_stdout, Sp[0]);
  Ip += 1;
  return Continue(); }

void transmit(g_core *f, g_file out, g_word x) {
  if (nump(x)) g_fprintf(out, "%ld", (long) getnum(x));
  else if (datp(x)) t_em[typ(x)](f, out, x);
  else g_fprintf(out, "#%lx", (long) x); }

g_core *g_write1(g_core *f) { return transmit(f, g_stdout, f->sp[0]), f; }


Vm(seek) {
  Sp[1] = word(((g_cell*) Sp[1]) + getnum(Sp[0]));
  Sp += 1;
  Ip += 1;
  return Continue(); }

Vm(peek) {
  Sp[0] = cell(Sp[0])->x;
  Ip += 1;
  return Continue(); }

Vm(poke) {
  cell(Sp[1])->x = Sp[0];
  Sp += 1;
  Ip += 1;
  return Continue(); }

Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct g_tag));
  g_cell *k = cell(Hp);
  struct g_tag *t = (struct g_tag*) (k + n);
  Hp += n + Width(struct g_tag);
  t->null = NULL;
  t->head = k;
  memset(k, -1, n * sizeof(g_word));
  Sp[0] = (g_word) k;
  Ip += 1;
  return Continue(); }

Vm(trim) {
  g_cell *k = (g_cell*) Sp[0];
  ttag(k)->head = k;
  Ip += 1;
  return Continue(); }

