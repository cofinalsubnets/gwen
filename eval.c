#include "i.h"

// at all times vm is running a function. thread compiler tracks
// function state using this type
typedef struct env {
  // these parameters represent stack state at a point in compile process
  word args,  // list // function positional arguments (never empty)
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

// thread compiler functions are defined in two phases:
#define Ana(n, ...) size_t n(core *f, env **c, size_t m, word x, ##__VA_ARGS__)
// ana phase
// - takes an expression and returns an upper bound for the length of a thread that
//   pushes the input expression's value onto the stack.
// - side effect: top of stack is now a function pointer that when called with a
//   properly initialized thread object, starts next compiler phase.
#define Cata(n, ...) cell *n(core *f, env **c, cell *k, ##__VA_ARGS__)
// cata phase
// - takes a thread pointer, emits code immediately prior to the indexed instruction,
//   pops a precomputed (by phase 0) continuation off the stack and calls it with the
//   decremented pointer.

typedef Ana(ana); typedef Cata(cata);
static ana analyze, ana_if, ana_let;
static Inline Cata(pull) { return ((cata*) (*f->sp++))(f, c, k); }
static Cata(cata_i) { return k[-1].x = *f->sp++, pull(f, c, k - 1); }
static Cata(cata_ix) { return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull(f, c, k - 2); }

static cell *mo_n(core *f, size_t n) { // allocate a thread
  cell *k = cells(f, n + Width(struct tag));
  if (!k) return k;
  struct tag *t = (struct tag*) (k + n);
  return t->null = NULL, t->head = k; }

static cell *pull_m(core *f, env **c, size_t m) {
  cell *k = mo_n(f, m);
  return !k ? k : (memset(k, -1, m * sizeof(word)), pull(f, c, k + m)); }

// generic instruction ana handlers
static size_t ana_i1(core *f, env **c, size_t m, vm *i) {
  return pushs(f, 2, cata_i, i) ? m + 1 : 0; }
static size_t ana_i2(core *f, env **c, size_t m, vm *i, word x) {
  return pushs(f, 3, cata_ix, i, x) ? m + 2 : 0; }

static Cata(yieldk) { return k; }

static env *enscope(core *f, env* par, word args, word imps) {
  if (!pushs(f, 3, args, imps, par)) return 0;
  env *c = (env*) mo_n(f, Width(env));
  args = pop1(f), imps = pop1(f), par = (env*) pop1(f);
  if (c) c->args = args, c->imps = imps, c->par = par,
         c->stack = c->alts = c->ends = c->lams = nil;
  return c; }

// keep this separate and NoInline so p_eval can be tail call optimized if possible
static NoInline int p_ana(core *f, vm *y) {
  env *c = enscope(f, (env*) nil, nil, nil);
  if (!c) return Oom;
  word x = f->sp[0];
  f->sp[0] = (word) yieldk; // function that returns thread from code generation
  MM(f, &c);
  size_t m = analyze(f, &c, 1, x);
  m = m ? ana_i2(f, &c, m, y, W(f->ip)) : m;
  cell *k = m ? pull_m(f, &c, m) : 0;
  UM(f);
  return k ? (f->ip = k, Ok) : Oom; }

// compile and execute expression
// NoInline because we always want tail calls to jump into this if possible
NoInline int p_eval(core *f, vm *y) {
  int s = p_ana(f, y);
  if (s != Ok) return s;
#ifdef NTCO
  do s = f->ip->ap(f); while (s == Ok);
  return s == Eof ? Ok : s; }
#else
  return f->ip->ap(f, f->ip, f->hp, f->sp); }
#endif


static cata ana_if_push_branch, ana_if_pop_branch, ana_if_push_exit, ana_if_pop_exit, ana_if_peek_exit;

// conditional expression analyzer
static Ana(ana_if) {
  if (!pushs(f, 2, x, ana_if_pop_exit)) return 0;
  pair p = { 0, 0, nil, nil }; // this is weird :/
  x = pop1(f);
  MM(f, &x);
  for (; m; x = BB(x)) {
    if (!twop(x)) x = (word) &p;
    m = analyze(f, c, m + 3, A(x));
    if (!twop(B(x))) { // this means we have just analyzed the default case
      m = pushs(f, 1, ana_if_peek_exit) ? m : 0; // now branch to exit
      break; }
    // otherwise we analyzed a conditional test
    // pop the last branch address off the stack to be jumped to in case the test failed
    m = pushs(f, 1, ana_if_pop_branch) ? m : 0;
    // otherwise here is the consequent
    m = m ? analyze(f, c, m + 3, AB(x)) : m;
    // after consequent jump to exit, then push new branch address at present code position (these are emitted backwards)
    m = m && pushs(f, 2, ana_if_push_branch,
                         ana_if_peek_exit) ? m : 0; }
  UM(f);
  m = m && pushs(f, 1, ana_if_push_exit) ? m : 0; // push the exit address for the whole conditional
  return m; }

// first emitter called for cond expression
// pushes cond expression exit address onto env stack ends
static Cata(ana_if_push_exit) {
  pair *w = pairof(f, W(k), (*c)->ends);
  return !w ? 0 : pull(f, c, R(A((*c)->ends = (word) w))); }

// last emitter called for cond expression
// pops cond expression exit address off env stack ends
static Cata(ana_if_pop_exit) { return (*c)->ends = B((*c)->ends), pull(f, c, k); }

static Cata(ana_if_push_branch) {
  pair *w = pairof(f, W(k), (*c)->alts);
  return !w ? 0 : ((*c)->alts = W(w), k = R(w->a), pull(f, c, k)); }

static Cata(ana_if_peek_exit) {
  k -= 2;
  cell *addr = (cell*) A((*c)->ends);
  // if the destination is a return or tail call,
  // then copy it forward instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k[0].ap = addr[0].ap, k[1].x = addr[1].x; // XXX optimization to remove ...
  else if (addr->ap == tapn)
    k--, k[0].ap = tapn, k[1].x = addr[1].x, k[2].x = addr[2].x;
  else k[0].ap = jump, k[1].x = (word) addr;
  return pull(f, c, k); }

// last emitter called for a branch
// pops next branch address off env stack alts
static Cata(ana_if_pop_branch) {
  return k[-2].ap = cond,
         k[-1].x = A((*c)->alts),
         (*c)->alts = B((*c)->alts),
         pull(f, c, k - 2); }

static bool lambp(core *f, word x) { return twop(x) && A(x) == (word) f->lambda; }

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

static word desugr(core *f, word *d, word *e, word a) {
  if (!twop(a)) return wpairof(f, *e, nil);
  word b; avec(f, a, b = desugr(f, d, e, B(a)));
  return !b ? b : wpairof(f, A(a), b); }

static int desug(core *f, word *d, word *e) {
  if (!twop(*d)) return Ok;
  word x;
  if (!pushs(f, 1, f->lambda)) return Oom;
  do if (!(x = desugr(f, d, e, B(*d))) || !(x = wpairof(f, f->sp[0], x))) return Oom;
  else *d = A(*d), *e = x;
  while (twop(*d));
  return f->sp++, Ok; }

// list length
static size_t llen(word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

Vm(defglob) { return Pack(f),
  !table_set(f, f->dict, Ip[1].x, Sp[0]) ?  Oom :
   (Unpack(f), Ip += 2, Continue()); }

// emits call instruction and modifies to tail call
// if next operation is return
static Cata(cataap) { return k[0].ap == ret ?
  (k[0].ap = tap, pull(f, c, k)) :
  (k[-1].ap = ap, pull(f, c, k - 1)); }
static Cata(cataapn) { word n = *f->sp++; return k[0].ap == ret ?
 (k[0].x = n,  k[-1].ap = tapn, pull(f, c, k - 1)) :
 (k[-1].x = n, k[-2].ap = apn,  pull(f, c, k - 2)); }

static word append(core *f, word l, word n) { return !twop(l) ? n :
  (avec(f, l, n = append(f, B(l), n)),
   n ? wpairof(f, A(l), n) : n); }

static word reverse(core *f, word l) {
  word n = nil;
  for (word m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
  return n; }

static word ana_lam(core*, env**, word, word);

// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.
static size_t ana_let(core *f, env* *b, size_t m, word exp) {
  if (!twop(exp)) return analyze(f, b, m, nil);
  if (!twop(B(exp))) return analyze(f, b, m, A(exp));
  env *q = *b, **c = &q;
  avec(f, exp, q = enscope(f, q, q->args, q->imps));
  if (!q) return 0;
  // lots of variables :(
#define fail() (f->safe = mm, 0)
  struct root *mm = f->safe;
  word nom = nil, def = nil, lam = nil, v = nil, d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &exp), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

  // collect vars and defs into two lists
  for (; twop(exp) && twop(B(exp)); exp = BB(exp)) {
    d = A(exp), e = AB(exp), desug(f, &d, &e);
    if (!(nom = wpairof(f, d, nom)) || !(def = wpairof(f, e, def))) return fail();
    // if it's a lambda compile it and record in lam list
    if (lambp(f, e)) { word x = ana_lam(f, c, nil, B(e));
                       x = x ? wpairof(f, d, x) : x;
                       lam = x ? wpairof(f, x, lam) : x;
                       if (!lam) return fail(); } }

  // if there's no body then evaluate the name of the last definition
  bool even = !twop(exp); // we check this again later to make global bindings at top level
  if (even && !(exp = wpairof(f, A(nom), nil))) return fail();

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
            if (!(vars = wpairof(f, var, vars))) return fail(); // oom
            else j++, BBA(e) = vars; } }
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e));

  (*c)->lams = lam;
  // pull_m lambda with reversed argument list
  exp = append(f, nom, exp);
  symbol *l = exp ? f->lambda : 0;
  exp = l ? wpairof(f, W(l), exp) : 0;
  m = exp ? analyze(f, b, m, exp) : 0; // exp is now the required lambda, analyze it
  if (!m || !((*b)->stack = wpairof(f, nil, (*b)->stack))) return fail();

  // reverse the nom and def lists
  nom = reverse(f, nom), def = reverse(f, def);
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
      word _;
      if (!(_ = ana_lam(f, c, BB(d), BA(def)))) return fail();
      else A(def) = B(d) = _; }
    if (!(m = analyze(f, b, m, A(def))) ||
        !((*b)->stack = wpairof(f, A(nom), (*b)->stack)))
      return fail();
    // if toplevel then bind
    if (even && nilp((*b)->args))
      if (!(m = ana_i2(f, c, m, defglob, A(nom))))
        return fail(); }

  m = nn <= 1 ? pushs(f, 1, cataap) ? m + 1 : 0 :
                pushs(f, 2, cataapn, putnum(nn)) ? m + 2 : 0;
  for (nn++; nn--; (*b)->stack = B((*b)->stack));
  return f->safe = mm, m; }

Vm(drop1) { return Ip++, Sp++, Continue(); }

static Inline size_t ana_imm(core *f, env **c, size_t m, word x) {
  return ana_i2(f, c, m, imm, x); }

static size_t ana_seq(core *f, env* *c, size_t m, word x) {
  if (!twop(x)) return ana_imm(f, c, m, nil);
  MM(f, &x);
  while (m && twop(B(x))) m = analyze(f, c, m, A(x)),
                          m = m ? ana_i1(f, c, m, drop1) : m,
                          x = B(x);
  return UM(f), m ? analyze(f, c, m, A(x)) : m; }

// exit vm and return to C
static Ana(ana_mac, word b) {
  if (!pushs(f, 3, f->quote, x, b)) return 0;
  pair *mxp = (pair*) cells(f, 4 * Width(pair));
  if (!mxp) return 0;
  x = W(ini_pair(mxp + 3, f->sp[2], nil));
  x = W(ini_pair(mxp + 2, f->sp[0], x));
  x = W(ini_pair(mxp + 1, x, nil));
  x = W(ini_pair(mxp, f->sp[1], x));
  *(f->sp += 2) = x;
  return p_eval(f, yieldi) != Ok ? 0 : analyze(f, c, m, pop1(f)); }

// evaluate function call arguments and apply
static size_t ana_ap_l2r(core *f, env **c, size_t m, word x) {
  MM(f, &x); // handle oom here ..
  // push one anonymous argument on the stack representing the function
  m = ((*c)->stack = wpairof(f, nil, (*c)->stack)) ? m : 0;
  while (m && twop(x)) m = analyze(f, c, m + 1, A(x)), // eval each argument
                       x = B(x),
                       m = m && pushs(f, 1, cataap) ? m : 0; // and apply the function
  // pop the argument
  return (*c)->stack = B((*c)->stack), UM(f), m; }

// evaluate a function expression by applying the function to arguments
static size_t ana_ap(core *f, env* *c, size_t m, word fn, word args) {
  size_t argc = llen(args);
  avec(f, args, m = analyze(f, c, m, fn));
  return !argc || !m ? m : ana_ap_l2r(f, c, m, args); }

static Ana(ana_sym_r, env *d);
static Ana(ana_lambda, word);
static NoInline Ana(analyze) {
  if (symp(x)) return ana_sym_r(f, c, m, x, *c);
  if (!twop(x)) return ana_imm(f, c, m, x);
  word a = A(x), b = B(x);
  if (a == W(f->quote)) return ana_imm(f, c, m, twop(b) ? A(b) : nil);
  if (a == W(f->begin)) return ana_seq(f, c, m, b);
  if (a == W(f->let)) return ana_let(f, c, m, b);
  if (a == W(f->cond)) return ana_if(f, c, m, b);
  if (a == W(f->lambda)) return !twop(b) ? ana_imm(f, c, m, nil) :
                                !twop(B(b)) ? analyze(f, c, m, A(b)) :
                                ana_lambda(f, c, m, x, b);
  if (!twop(b)) return analyze(f, c, m, a); // singleton list has value of first element
  word macro = table_get(f, f->macro, a, 0);
  return macro ? ana_mac(f, c, m, macro, b) : ana_ap(f, c, m, a, b); }

Vm(free_variable) {
  word x = Ip[1].x;
  x = table_get(f, f->dict, x, x); // error here if you want on undefined variable
  return Ip[0].ap = imm, Ip[1].x = x, Continue(); }

// index of item in list
static long lidx(core *f, word l, word x) {
  for (long i = 0; !nilp(l); l = B(l), i++) if (eql(f, x, A(l))) return i;
  return -1; }

static long stack_index_of_symbol(core *f, env *c, word var) {
  long l, i = 0;
  for (l = c->imps; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  for (l = c->args; !nilp(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  return -1; }

static Cata(c2var) {
  word var = *f->sp++, stack = *f->sp++;
  long i = lidx(f, stack, var);
  return k[-2].ap = ref, k[-1].x = putnum(i), pull(f, c, k - 2); }

// emit stack reference instruction
static Cata(cata_var) {
  word var = *f->sp++, // variable name
       ins = llen(*f->sp++), // stack inset
       idx = stack_index_of_symbol(f, *c, var);
  return k[-2].ap = ref, k[-1].x = putnum(idx + ins), pull(f, c, k - 2); }

Vm(late_bind) {
  word ref = Ip[1].x, lfd = ref;
  ref = AB(lfd);
  return Ip[0].ap = imm, Ip[1].x = ref, Continue(); }

static Ana(ana_sym_r, env *d) {
  // free symbol?
  if (nilp(d)) {
    word y = table_get(f, f->dict, x, 0);
    return y ? ana_imm(f, c, m, y) :
     (x = wpairof(f, x, (*c)->imps), // XXX why is this needed???
      x = x ? A((*c)->imps = x) : x,
      x ? ana_i2(f, c, m, free_variable, x) : x);  }
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
      x = wpairof(f, x, (*c)->imps),
      x = x ? A((*c)->imps = x) : x;
    return pushs(f, 3, cata_var, x, (*c)->stack) ? m + 2 : 0; }
  // otherwise recur on the enclosing env
  return ana_sym_r(f, c, m, x, d->par); }


// lambda decons pushes last list item to stack returns init of list
static word linit(core *f, word x) {
  if (!twop(x)) return pushs(f, 1, nil) ? nil : 0;
  if (!twop(B(x))) return pushs(f, 1, A(x)) ? nil : 0;
  word y = A(x); return avec(f, y, x = linit(f, B(x))),
                        x ? wpairof(f, y, x) : x; }

static cell *trim_thread(cell *k) { return ttag(k)->head = k; }
Vm(trim) { return Sp[0] = W(trim_thread((cell*) Sp[0])), Ip += 1, Continue(); }

static Ana(ana_lambda, word b) {
  return (x = ana_lam(f, c, nil, b)) ? analyze(f, c, m, x) : x; }
static word ana_lam(core *f, env **c, word imps, word exp) {
  // expediently storing exp in args for the moment
  env *d = enscope(f, *c, exp, imps);
  if (!d) return 0;
  MM(f, &d);
  // get the real args
  word args = linit(f, d->args);
  if (!(d->args = args)) return UM(f), 0;
  exp = f->sp[0], f->sp[0] = W(yieldk);
  size_t m = analyze(f, &d, 2, exp);
  if (!m) return UM(f), 0;
  size_t arity = llen(d->args) + llen(d->imps);
  cell *k = (m = ana_i2(f, c, m, ret, putnum(arity))) ? pull_m(f, &d, m) : 0;
  if (!k) return UM(f), 0;
  if (arity > 1) (--k)->x = putnum(arity), (--k)->ap = curry;
  return UM(f), wpairof(f, W(trim_thread(k)), d->imps); }

Vm(seek) { return Sp[1] = W(((cell*) Sp[1]) + getnum(Sp[0])), Sp += 1, Ip += 1, Continue(); }
Vm(peek) { return Sp[0] = R(Sp[0])->x, Ip += 1, Continue(); }
Vm(poke) { return R(Sp[1])->x = Sp[0], Sp += 1, Ip += 1, Continue(); }
Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct tag));
  cell *k = R(Hp);
  struct tag *t = (struct tag*) (k + n);
  return Hp += n + Width(struct tag),
         t->null = NULL,
         t->head = k,
         memset(k, -1, n * sizeof(word)),
         Sp[0] = (word) k,
         Ip += 1,
         Continue(); }

struct tag *ttag(cell *k) {
  while (k->x) k++;
  return (struct tag*) k; }

NoInline Vm(ev0) { return Ip++, Pack(f), p_eval(f, jump); }
