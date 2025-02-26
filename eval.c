#include "i.h"

// at all times vm is running a function. thread compiler tracks
// function state using this type
typedef struct Env {
  // these parameters represent stack state at a point in compile process
  Word args,  // list // function positional arguments (never empty)
       imps,  // list // closure variables
       stack; // list // current values on stack
  // these are values of variables known at compile time
  Word lams; // alist // known function definitions
  // these are two stacks of jump target addresses for conditional expressions
  Word alts, // list // alternate branch address stack
       ends; // list // exit branch address stack
  // this is the enclosing function PEnv* if any
  struct Env *par;
} PEnv, Env;

static long lidx(PCore*, Word, Word);

// thread compiler functions are defined in two phases:
#define Ana(n, ...) size_t n(Core *f, Env **c, size_t m, Word x, ##__VA_ARGS__)
// ana phase 
// - takes an expression and returns an upper bound for the length of a thread that
//   pushes the input expression's value onto the stack.
// - side effect: top of stack is now a function pointer that when called with a
//   properly initialized thread object, starts next compiler phase.
#define Cata(n, ...) Cell *n(Core *f, Env **c, Cell *k, ##__VA_ARGS__)
// cata phase
// - takes a thread pointer, emits code immediately prior to the indexed instruction,
//   pops a precomputed (by phase 0) continuation off the stack and calls it with the
//   decremented pointer.

typedef Ana(ana);
typedef Cata(cata);

static ana analyze;
static Env *envup(Core*, Env*, Word, Word);
static size_t em1(Core*, Env**, size_t, PVm*);
static Cell *construct(Core*, Env**, size_t);

static Status run_vm(Core *f) {
  Status s;
#if TCO
  s = f->ip->ap(f, f->ip, f->hp, f->sp);
#else
  do s = f->ip->ap(f); while (s == Ok);
  s = s == Eof ? Ok : s;
#endif
  return s; }

static Cata(yieldk) { return k; }

// compile and execute expression
NoInline Status p_eval(Core *f) {
  Env *c = envup(f, (PEnv*) nil, nil, nil);
  if (!c) return Oom;
  Word x = f->sp[0];
  f->sp[0] = (Word) yieldk;
  Cell *k = 0;
  size_t m = 1;
  avec(f, c,
    m = analyze(f, &c, m, x),
    m = m ? em1(f, &c, m, yield) : m,
    k = m ? construct(f, &c, m) : k);
  k = k ? (Cell*) pushs(f, 1, k) : k;
  if (!k) return Oom;
  f->sp[0] = (Word) f->ip;
  f->ip = k;
  Status s = run_vm(f);
  if (s != Ok) f->ip = 0, f->sp = f->pool + f->len;
  else x = f->sp[0], f->ip = (Cell*) *++f->sp, f->sp[0] = x;
  return s; }

Vm(ev0) {
  Pack(f);
  Status s = p_eval(f);
  Unpack(f);
  return s == Ok ? op(1, *Sp) : s; }

// PEnv* constructor
static Env *envup(Core *f, Env* par, Word args, Word imps) {
  if (!pushs(f, 3, args, imps, par)) return 0;
  Env *c = (Env*) mo_n(f, Width(Env));
  args = pop1(f), imps = pop1(f), par = (Env*) pop1(f);
  if (c)
    c->args = args, c->imps = imps, c->par = par,
    c->stack = c->alts = c->ends = c->lams = nil;
  return c; }

static Word ana_lam(Core*, Env**, Word, Word);
static ana ana_if, ana_let, ana_list;
static cata cataap, cataapn, catavar;
// basic functions
static Inline Cata(pull) { return ((cata*) (*f->sp++))(f, c, k); }
static Cell *construct(Core *f, Env **c, size_t m) {
  Cell *k = mo_n(f, m);
  if (!k) return k;
  memset(k, -1, m * sizeof(Word));
  return pull(f, c, k + m); }
static Cata(catai) { return k[-1].x = *f->sp++, pull(f, c, k - 1); }
static Cata(cataix) { return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull(f, c, k - 2); }

// generic instruction ana handlers
static size_t em1(Core *f, Env **c, size_t m, PVm *i) {
  return pushs(f, 2, catai, i) ? m + 1 : 0; }
static size_t em2(Core *f, Env **c, size_t m, PVm *i, Word x) {
  return pushs(f, 3, cataix, i, x) ? m + 2 : 0; }
// analyzer
static Word
  lassoc(Core*, Word, Word),
  lconcat(Core*, Word, Word),
  rlconcat(Core*, Word, Word);


// conditionals
// to emit targeted jumps etc
static cata
  generate_cond_push_branch,
  generate_cond_pop_branch,
  generate_cond_push_exit,
  generate_cond_pop_exit,
  generate_cond_peek_exit;

// conditional expression analyzer
static Ana(ana_if) {
  if (!pushs(f, 2, x, generate_cond_pop_exit)) return 0;
  Pair p = { data, &pair_type, nil, nil };
  for (x = pop1(f), MM(f, &x); m; x = BB(x)) {
    if (!twop(x)) x = (Word) &p;
    m = analyze(f, c, m + 2, A(x));
    if (!twop(B(x))) { // this means we have just analyzed the default case
      m = pushs(f, 1, generate_cond_peek_exit) ? m : 0; // now branch to exit
      break; }
    // otherwise we analyzed a conditional test
    // pop the last branch address off the stack to be jumped to in case the test failed
    m = pushs(f, 1, generate_cond_pop_branch) ? m : 0;
    // otherwise here is the consequent
    m = m ? analyze(f, c, m + 2, AB(x)) : m;
    // after consequent jump to exit, then push new branch address at present code position (these are emitted backwards)
    m = m && pushs(f, 2, generate_cond_push_branch,
                         generate_cond_peek_exit) ? m : 0; }
  UM(f);
  m = m && pushs(f, 1, generate_cond_push_exit) ? m : 0; // push the exit address for the whole conditional
  return m; }

// first emitter called for cond expression
// pushes cond expression exit address onto PEnv* stack ends
static Cata(generate_cond_push_exit) {
  PPair *w = pairof(f, (PWord) k, (*c)->ends);
  return !w ? 0 : pull(f, c, (PCell*) A((*c)->ends = (PWord) w)); }

// last emitter called for cond expression
// pops cond expression exit address off PEnv* stack ends
static Cata(generate_cond_pop_exit) {
  return (*c)->ends = B((*c)->ends), pull(f, c, k); }

static Cata(generate_cond_push_branch) {
  PPair *w = pairof(f, (PWord) k, (*c)->alts);
  if (!w) return (PCell*) w;
  (*c)->alts = (PWord) w;
  k = (PCell*) w->a;
  return pull(f, c, k); }

static Cata(generate_cond_peek_exit) {
  k -= 2;
  PCell *addr = (PCell*) A((*c)->ends);
  // if the destination is a return or tail call,
  // then copy it forward instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k[0].ap = addr[0].ap, k[1].x = addr[1].x;
  else k[0].ap = jump, k[1].x = (PWord) addr;
  return pull(f, c, k); }

// last emitter called for a branch
// pops next branch address off PEnv* stack alts
static Cata(generate_cond_pop_branch) {
  return k[-2].ap = cond,
         k[-1].x = A((*c)->alts),
         (*c)->alts = B((*c)->alts),
         pull(f, c, k - 2); }

static bool lambp(PCore *f, PWord x) {
  if (!twop(x) || !symp(x = A(x))) return false;
  PString* s = ((PSymbol*) x)->nom;
  return s && s->len == 1 && s->text[0] == '\\'; }

// DEFINE
// let expressions

static Word ldels(Core *f, Word lam, Word l) {
  if (!twop(l)) return nil;
  Word m = ldels(f, lam, B(l));
  if (!lassoc(f, lam, A(l))) B(l) = m, m = l;
  return m; }

static Word desugr(Core *f, Word *d, Word *e, Word a) {
  if (!twop(a)) return (Word) pairof(f, *e, nil);
  Word b; avec(f, a, b = desugr(f, d, e, B(a)));
  return !b ? b : (Word) pairof(f, A(a), b); }

static Status desug(Core *f, Word *d, Word *e) {
  if (!twop(*d)) return Ok;
  Word x, l = (Word) literal_symbol(f, "\\");
  if (!l || !pushs(f, 1, l)) return Oom;
  do if (!(x = (Word) desugr(f, d, e, B(*d))) ||
         !(x = (Word) pairof(f, f->sp[0], x)))
    return Oom;
  else *d = A(*d), *e = x;
  while (twop(*d));
  return f->sp++, Ok; }

// list length
static size_t llen(Word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

static Vm(defglobal) {
  Pack(f);
  if (!table_set(f, f->dict, Ip[1].x, Sp[0])) return Oom;
  Unpack(f);
  return op(1, Sp[0]); }

static size_t ana_let(Core *f, Env* *b, size_t m, Word exp) {
  if (!twop(exp)) return analyze(f, b, m, nil);
  if (!twop(B(exp))) return analyze(f, b, m, A(exp));
  Env *q = *b, **c = &q;
  avec(f, exp, q = envup(f, q, q->args, q->imps));
  if (!q) return 0;
  // lots of variables :(
  Word nom = nil, def = nil, lam = nil, v = nil, d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &exp), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);
// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.

  // collect vars and defs into two lists
  for (; twop(exp) && twop(B(exp)); exp = BB(exp)) {
    d = A(exp), e = AB(exp), desug(f, &d, &e);
    if (!(nom = Z(pairof(f, d, nom))) ||
        !(def = Z(pairof(f, e, def))))
      goto fail;
    if (lambp(f, e)) {
      // if it's a lambda compile it and record in lam list
      Word x = ana_lam(f, c, nil, B(e));
      x = x ? Z(pairof(f, d, x)) : x;
      lam = x ? Z(pairof(f, x, lam)) : x;
      if (!lam) goto fail; } }

  // if there's no body then evaluate the name of the last definition
  bool even = !twop(exp); // we check this again later to make global bindings at top level
  if (even && !(exp = (PWord) pairof(f, A(nom), nil))) goto fail;

  // find closures
  // for each function f with closure C(f)
  // for each function g with closure C(g)
  // if f in C(g) then C(g) include C(f)
  long j;
  do for (j = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
    for (e = lam; twop(e); e = B(e)) // for each bound function variable
      if (d != e && // skip yourself
          lidx(f, BBA(e), AA(d)) >= 0) // if you need this function
        for (v = BA(d); twop(v); v = B(v)) { // then you need its variables
          Word vars = BBA(e), var = A(v);
          if (lidx(f, vars, var) < 0) { // only add if it's not already there
            if (!(vars = Z(pairof(f, var, vars)))) goto fail; // oom
            j++, BBA(e) = vars; } }
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e)) ;

  (*c)->lams = lam;
  // construct lambda with reversed argument list
  exp = lconcat(f, nom, exp);
  Symbol* l = exp ? literal_symbol(f, "\\") : 0;
  exp = l ? Z(pairof(f, Z(l), exp)) : 0;
  m = exp ? analyze(f, b, m, exp) : 0; // exp is now the required lambda, analyze it
  if (!m || !((*b)->stack = Z(pairof(f, nil, (*b)->stack)))) goto fail;

  // reverse the nom and def lists
  nom = rlconcat(f, nom, nil), def = rlconcat(f, def, nil);
  // evaluate definitions in order tracking var names on stack list
  // store lambdas on PEnv* for lazy binding and construct new application
  // - reverse noms onto exp
  // - reverse onto e = nil and recompile lambdas
  size_t nn = 0;
  for (; twop(nom); nom = B(nom), def = B(def), nn++) {
    // if lambda then recompile with the explicit closure
    // and put in arg list and lam list (latter is used for lazy binding)
    if (lambp(f, A(def))) {
      d = lassoc(f, lam, A(nom));
      Word _;
      if (!(_ = ana_lam(f, c, BB(d), BA(def)))) goto fail;
      else A(def) = B(d) = _; }
    // if toplevel then bind
    if (even && nilp((*b)->args)) {
      Cell *t = cells(f, 2 * Width(Pair) + 2 + Width(struct tag));
      if (!t) goto fail;
      Pair *w = (Pair*) t,
           *x = w + 1;
      t += 2 * Width(Pair);
      t[0].ap = defglobal, t[1].x = A(nom), t[2].x = 0, t[3].m = t;
      ini_pair(w, A(def), nil); // dict add
      ini_pair(x, Z(t), Z(w));
      A(def) = Z(x); }
    if (!(m = analyze(f, b, m, A(def))) ||
        !((*b)->stack = Z(pairof(f, A(nom), (*b)->stack))))
      goto fail; }

  m = nn <= 1 ? pushs(f, 1, cataap) ? m + 1 : 0 :
                pushs(f, 2, cataapn, putnum(nn)) ? m + 2 : 0;
  for (nn++; nn--; (*b)->stack = B((*b)->stack));
done: return UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), m;
fail: m = 0; goto done; }

static Vm(drop1) { return Ip++, Sp++, Continue(); }

static size_t ana_seq(Core *f, Env* *c, size_t m, Word x) {
  if (!twop(x)) return em2(f, c, m, imm, nil);
  for (MM(f, &x); m && twop(B(x)); x = B(x))
    m = analyze(f, c, m, A(x)),
    m = m ? em1(f, c, m, drop1) : m;
  return UM(f), m ? analyze(f, c, m, A(x)) : m; }

static Ana(ana_mac, Word b) {
  if (!pushs(f, 2, x, b)) return 0;
  x = Z(literal_symbol(f, "`"));
  if (!x || !pushs(f, 1, x)) return 0;
  Pair *mxp = (Pair*) cells(f, 4 * Width(PPair));
  if (!mxp) return 0;
  x = Z(ini_pair(mxp, f->sp[1], Z(ini_pair(mxp+1, Z(ini_pair(mxp+2, f->sp[0],  Z(ini_pair(mxp+3, f->sp[2], nil)))), nil))));
  f->sp += 2, *f->sp = x;
  return p_eval(f) != Ok ? 0 : analyze(f, c, m, pop1(f)); }

// evaluate function call arguments and apply
static size_t ana_args(Core *f, Env* *c, size_t m, Word x) {
  MM(f, &x); // handle oom here ..
  m = ((*c)->stack = Z(pairof(f, nil, (*c)->stack))) ? m : 0;
  while (m && twop(x))
    m = analyze(f, c, m + 1, A(x)),
    x = B(x),
    m = m && pushs(f, 1, cataap) ? m : 0;
  return
    (*c)->stack = B((*c)->stack),
    UM(f),
    m; }

static size_t ana_args_r2l(Core *f, Env* *c, size_t m, Word x) {
  if (!twop(x)) return m;
  Word a = A(x);
  MM(f, &a);
  m = ana_args_r2l(f, c, m, B(x));
  UM(f);
  if (!m) return m;
  m = analyze(f, c, m, a);
  if (!((*c)->stack = Z(pairof(f, nil, (*c)->stack)))) return 0;
  return m; }

static size_t ana_args_r(PCore *f, PEnv**c, size_t m, PWord x) {
  avec(f, x, m = ((*c)->stack = Z(pairof(f, nil, (*c)->stack))) ? m : 0);
  return m ? ana_args_r2l(f, c, m, x) : m; }


static Inline bool can_apn(Core *f, size_t argc) {
  return argc > 1 &&
    f->sp[0] == Z(cataix) &&
    f->sp[1] == Z(imm) &&
    homp(f->sp[2]) &&
    ptr(f->sp[2])[0].ap == curry &&
    getnum(ptr(f->sp[2])[1].x) == argc; }

static size_t ana_ap(Core *f, Env* *c, size_t m, Word fn, Word args) {
  size_t argc = llen(args);
  avec(f, args, m = analyze(f, c, m, fn));
  if (!m || !argc) return m;
  if (!can_apn(f, argc)) return ana_args(f, c, m, args);
  if (!(m = ana_args_r(f, c, m + 2, args)) ||
      !pushs(f, 2, cataapn, putnum(argc)))
    return 0;
  do (*c)->stack = B((*c)->stack); while (argc--);
  return m; }



static Ana(ana_list) {
  Word a = A(x), b = B(x);
  if (!twop(b)) return analyze(f, c, m, a); // singleton list has value of first element
  if (symp(a)) {
    Word macro = table_get(f, f->macro, a, 0);
    if (macro) return ana_mac(f, c, m, macro, b);
    String *n = ((Symbol*) a)->nom;
    if (n && n->len == 1) switch (n->text[0]) { // special form?
      case '`': return em2(f, c, m, imm, twop(b) ? A(b) : nil);
      case ',': return ana_seq(f, c, m, b);
      case ':': return ana_let(f, c, m, b);
      case '?': return ana_if(f, c, m, b);
      case '\\': return (x = ana_lam(f, c, nil, b)) ? analyze(f, c, m, x) : x; } }
  return ana_ap(f, c, m, a, b); }


static Vm(free_variable) {
  Word x = Ip[1].x, var = A(x);
  Table *t = (Table*) B(x);
  x = table_get(f, t, var, nil); // error here if you want
  Ip[0].ap = imm;
  Ip[1].x = x;
  return Continue(); }

static Vm(lazy_bind) {
  Word ref = Ip[1].x, var = A(ref);
  Env *env = (Env*) B(ref);
  var = AB(lassoc(f, env->lams, var));
  Ip[0].ap = imm;
  Ip[1].x = var;
  return Continue(); }

static Word lassoc(Core *f, Word l, Word k) {
  for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
  return 0; }

// list concat
static Word lconcat(Core *f, Word l, Word n) {
  if (!twop(l)) return n;
  avec(f, l, n = lconcat(f, B(l), n));
  return n ? (PWord) pairof(f, A(l), n) : n; }

// reverse list concat
static Word rlconcat(Core *f, Word l, Word n) {
  for (Word m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
  return n; }

// allocate a thread
Cell *mo_n(Core *f, uintptr_t n) {
  Cell *k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

static ana ana_sym;
static Ana(analyze) {
  if (homp(x)) {
    Type *y = dtyp(x);
    if (y == &pair_type) return ana_list(f, c, m, x);
    if (y == &symbol_type) return ana_sym(f, c, m, x); }
  return em2(f, c, m, imm, x); }

// index of item in list
static long lidx(Core *f, Word l, Word x) {
  for (long i = 0; twop(l); l = B(l), i++) if (eql(f, x, A(l))) return i;
  return -1; }

static long stack_index_of_symbol(Core *f, Env *c, Word var) {
  size_t i = 0;
  for (Word l = c->imps; twop(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  for (Word l = c->args; twop(l); l = B(l), i++) if (eql(f, var, A(l))) return i;
  return -1; }

static Ana(ana_sym_free) {
  Word y = table_get(f, f->dict, x, 0);
  if (y) return em2(f, c, m, imm, y);
  x = Z(pairof(f, x, (*c)->imps)),
  x = x ? A((*c)->imps = x) : x;
  return x ? em2(f, c, m, free_variable, x) : x;  }

static Ana(ana_sym_local_fn, Env *d) {
  x = Z(pairof(f, x, Z(d)));
  m = x ? em2(f, c, m, lazy_bind, x) : 0;
  if (!m) return m;
  x = f->sp[2]; // get the (symbol arg1 arg2 ...)
  Word y = BBA(x); // get the args
  A(x) = AA(x); // set car of pair to just the symbol -- (symbol . PEnv*)
  return ana_args(f, c, m, y); }

static Cata(c2var) {
  Word var = *f->sp++, stack = *f->sp++;
  size_t i = lidx(f, stack, var);
  return k[-2].ap = dup,
         k[-1].x = putnum(i),
         pull(f, c, k - 2); }

// emit stack reference instruction
static Cata(catavar) {
  Word
    var = *f->sp++, // variable name
    ins = llen(*f->sp++), // stack inset
    idx = stack_index_of_symbol(f, *c, var);
  return
    k[-2].ap = dup,
    k[-1].x = putnum(idx + ins),
    pull(f, c, k - 2); }

static Ana(ana_sym_local_def, Word stack) { 
  return pushs(f, 3, c2var, x, stack) ? m + 2 : 0; }

static Ana(ana_sym_stack_ref, Env *d) {
  if (*c != d) // if we have found the variable in an enclosing PEnv* then import it
    x = Z(pairof(f, x, (*c)->imps)),
    x = x ? A((*c)->imps = x) : x;
  return pushs(f, 3, catavar, x, (*c)->stack) ? m + 2 : 0; }

static Ana(ana_sym_r, Env* d) {
  // free symbol?
  if (nilp(d)) return ana_sym_free(f, c, m, x);
  // defined as a function by a local let binding?
  Word y = lassoc(f, d->lams, x);
  if (y) return ana_sym_local_fn(f, c, m, y, d);
  // bound on the stack by a local let binding?
  if (lidx(f, d->stack, x) >= 0)
    return ana_sym_local_def(f, c, m, x, d->stack);
  // bound on the stack as a closure or positional argument?
  if (stack_index_of_symbol(f, d, x) >= 0)
    return ana_sym_stack_ref(f, c, m, x, d);
  // otherwise recur on the enclosing env
  return ana_sym_r(f, c, m, x, d->par); }

static NoInline size_t ana_sym(Core *f, Env* *c, size_t m, Word x) {
  return ana_sym_r(f, c, m, x, *c); }

// emits call instruction and modifies to tail call
// if next operation is return
static Cata(cataap) {
  if (k->ap == ret) k->ap = tap; // tail call
  else (--k)->ap = ap; // regular call
  return pull(f, c, k); } // ok

static Cata(cataapn) {
  Word n = *f->sp++;
  if (k->ap == ret) k->x = n, (--k)->ap = tapn;
  else (--k)->x = n, (--k)->ap = apn;
  return pull(f, c, k); }

// lambda decons pushes last list item to stack returns init of list
static Word linit(Core *f, Word x) {
  if (!twop(x)) return pushs(f, 1, nil) ? nil : 0;
  if (!twop(B(x))) return pushs(f, 1, A(x)) ? nil : 0;
  Word y = A(x);
  avec(f, y, x = linit(f, B(x)));
  return x ? Z(pairof(f, y, x)) : x; }

static Word ana_lam(Core *f, Env **c, Word imps, Word exp) {
  // expediently storing exp in args for the moment
  Env *d = envup(f, *c, exp, imps);
  if (!d) return 0;
  MM(f, &d);
  // get the real args
  Word args = linit(f, d->args);
  if (!(d->args = args)) goto fail;
  exp = f->sp[0], f->sp[0] = Z(yieldk);
  size_t m = analyze(f, &d, 4, exp);
  if (!m) goto fail;
  size_t arity = llen(d->args) + llen(d->imps);
  Cell *k = pushs(f, 3, cataix, ret, putnum(arity)) ? construct(f, &d, m) : 0;
  if (!k) goto fail;
  if (arity > 1) (--k)->x = putnum(arity), (--k)->ap = curry;
  UM(f);
  return Z(pairof(f, Z(trim_thread(k)), d->imps));
fail:
  UM(f);
  return 0; }
