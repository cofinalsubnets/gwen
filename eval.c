#include "i.h"
// index of item in list
static long lidx(PCore *f, PWord l, PWord x) {
  for (long i = 0; twop(l); l = B(l), i++) if (eql(f, A(l), x)) return i;
  return -1; }
// list length
static size_t llen(PWord l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

static Vm(define) {
  Pack(f);
  if (!table_set(f, f->dict, Ip[1].x, Sp[0])) return Oom;
  Unpack(f);
  return op(1, Sp[0]); }

static Vm(drop) {
  return Ip++, Sp++, Continue(); }

// at all times vm is running a function. thread compiler tracks
// function state using this type
typedef struct scope {
  // these parameters represent stack state at a point in compile process
  PWord args, // list // function positional arguments (never empty)
       imps, // list // closure variables
       pals; // list // current state of stack
  // these are values of variables known at compile time
  PWord lams; // dict // known function definitions
  // these are two stacks of jump target addresses for conditional expressions
  PWord alts, // list // alternate branch address stack
       ends; // list // exit branch address stack
  // this is the enclosing function scope if any
  struct scope *par;
} *scope;
static size_t assemble_arguments(PCore*, scope*, size_t, PWord);
// thread compiler operates in two phases
//
// 1. analyze phase: analyze expression; assemble constructor on stack; compute code size bound
#define C0(n, ...) size_t n(PCore *f, scope *c, size_t m, PWord x, ##__VA_ARGS__)
typedef C0(c0);
// and
// - generate phase: allocate thread; call constructor; trim thread
#define C1(n, ...) PCell *n(PCore *f, scope *c, PCell *k, ##__VA_ARGS__)
typedef C1(c1);

static PWord analyze_lambda(PCore *f, scope *c, PWord imps, PWord exp);

// scope constructor
static scope enscope(PCore *f, scope par, PWord args, PWord imps) {
  if (!pushs(f, 3, args, imps, par)) return 0;
  scope c = (scope) mo_n(f, 7);
  if (c)
    c->args = pop1(f), c->imps = pop1(f),
    c->par = (scope) pop1(f),
    c->pals = c->alts = c->ends = nil,
    c->lams = nil;
  return c; }


static c0 analyze_if, analyze_let, analyze_arguments, analyze_list;
static c1 c1ap, c1ap1, c1apn, c1var, c2var;
// basic functions
static C1(yieldk) { return k; }
static C1(pull) { return ((c1*) (*f->sp++))(f, c, k); }
static PCell *construct(PCore *f, scope *c, size_t m) {
  PCell *k = mo_n(f, m);
  if (!k) return k;
  memset(k, -1, m * sizeof(PWord));
  return pull(f, c, k + m); }
static C1(c1i) { return k[-1].x = *f->sp++, pull(f, c, k - 1); }
static C1(c1ix) { return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull(f, c, k - 2); }
// generic instruction c0 handlers
static size_t em1(PCore *f, scope *c, size_t m, PVm *i) {
  return pushs(f, 2, c1i, i) ? m + 1 : 0; }
static size_t em2(PCore *f, scope *c, size_t m, PVm *i, PWord x) {
  return pushs(f, 3, c1ix, i, x) ? m + 2 : 0; }
// analyzer
static PVm pushr, cond, jump, pushk, yield, ret, ap, apn, tap, tapn;
static PWord
  lassoc(PCore*, PWord, PWord),
  lconcat(PCore*, PWord, PWord),
  rlconcat(PCore*, PWord, PWord);
Vm(ev0) {
  Pack(f);
  PStatus s = p_eval(f);
  Unpack(f);
  return s == Ok ? op(1, *Sp) : s; }

static Vm(pushk) {
  Have1();
  *--Sp = Ip[1].x;
  Ip += 2;
  return Continue(); }

static Vm(jump) {
  Ip = Ip[1].m;
  return Continue(); }

static Vm(cond) {
  Ip = nilp(*Sp) ? Ip[1].m : Ip + 2,
  Sp++;
  return Continue(); }

static Vm(pushr) {
  Have1();
  Sp[-1] = Sp[getnum(Ip[1].x)];
  Sp--;
  Ip += 2;
  return Continue(); }

static Vm(ret) {
  PWord n = getnum(Ip[1].x) + 1;
  return op(n, *Sp); }

static Vm(yield) { Pack(f); return YieldStatus; }

static Vm(ap1) {
  PWord k = Sp[0];
  if (nump(k)) return Ip++, *++Sp = k, Continue();
  Sp[0] = Sp[1];
  Sp[1] = (PWord) (Ip + 1);
  Ip = (PCell*) k;
  return Continue(); }

static Vm(ap) {
  if (nump(Sp[1])) return Ip++, Sp++, Continue();
  PCell *k = (PCell*) Sp[1];
  Sp[1] = (PWord) (Ip + 1);
  Ip = k;
  return Continue(); }

static Vm(apn) {
  size_t n = getnum(Ip[1].x);
  PCell *ra = Ip + 2; // return address
  Ip = ((PCell*) Sp[n]) + 2; // only used by let form so will not be num
  Sp[n] = (PWord) ra; // store return address
  return Continue(); }

static Vm(tap1) {
  PWord j = Sp[0], x = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) return op(1, j);
  Ip = (PCell*) j;
  *Sp = x;
  return Continue(); }
static Vm(tap) {
  PWord x = Sp[0], j = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) return op(1, j);
  Ip = (PCell*) j;
  *Sp = x;
  return Continue(); }

static Vm(tapn) {
  size_t n = getnum(Ip[1].x),
         r = getnum(Ip[2].x);
  Ip = ((PCell*) Sp[n]) + 2;
  PStack osp = Sp;
  Sp += r + 1;
  while (n--) Sp[n] = osp[n];
  return Continue(); }

static Vm(pushkj) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return Continue(); }

Vm(curry) {
  PCell *k;
  size_t n = getnum(Ip[1].x),
         S = 3 + Width(struct tag);
  if (n == 2) {
    Have(S);
    k = (PCell*) Hp;
    k[0].ap = pushkj, k[1].x = *Sp++, k[2].m = Ip + 2;
    k[3].x = 0,   k[4].m = k; }
  else {
    S += 2;
    Have(S);
    k = (PCell*) Hp;
    k[0].ap = curry, k[1].x = putnum(n - 1);
    k[2].ap = pushkj,  k[3].x = *Sp++, k[4].m = Ip + 2;
    k[5].x = 0,    k[6].m = k; }
  Hp += S;
  Ip = (PCell*) *Sp;
  *Sp = (PWord) k;
  return Continue(); }

// conditionals
// to emit targeted jumps etc
static c1
  generate_cond_push_branch,
  generate_cond_pop_branch,
  generate_cond_push_exit,
  generate_cond_pop_exit,
  generate_cond_peek_exit;

static c0 analyze;
// conditional expression analyzer
static C0(analyze_if) {
  if (!pushs(f, 2, x, generate_cond_pop_exit)) return 0;
  PPair p = { data, &pair_type, nil, nil };
  for (x = pop1(f), MM(f, &x); m; x = BB(x)) {
    if (!twop(x)) x = (PWord) &p;
    m = analyze(f, c, m + 2, A(x));
    if (!twop(B(x))) { // at end, default branch
      m = pushs(f, 1, generate_cond_peek_exit) ? m : 0;
      break; }
    m = pushs(f, 1, generate_cond_pop_branch) ? m : 0;
    m = m ? analyze(f, c, m + 2, AB(x)) : m;
    m = pushs(f, 2, generate_cond_push_branch, generate_cond_peek_exit) ? m : 0; }
  return UM(f), m && pushs(f, 1, generate_cond_push_exit) ? m : 0; }

// first emitter called for cond expression
// pushes cond expression exit address onto scope stack ends
static C1(generate_cond_push_exit) {
  PPair *w = pairof(f, (PWord) k, (*c)->ends);
  return !w ? 0 : pull(f, c, (PCell*) A((*c)->ends = (PWord) w)); }

// last emitter called for cond expression
// pops cond expression exit address off scope stack ends
static C1(generate_cond_pop_exit) {
  return (*c)->ends = B((*c)->ends), pull(f, c, k); }

static C1(generate_cond_push_branch) {
  PPair *w = pairof(f, (PWord) k, (*c)->alts);
  if (!w) return (PCell*) w;
  (*c)->alts = (PWord) w;
  k = (PCell*) w->a;
  return pull(f, c, k); }

static C1(generate_cond_peek_exit) {
  k -= 2;
  PCell *addr = (PCell*) A((*c)->ends);
  // if the destination is a return or tail call,
  // then copy it forward instead of emitting a jump.
  if (addr->ap == ret || addr->ap == tap)
    k[0].ap = addr[0].ap, k[1].x = addr[1].x;
  else k[0].ap = jump, k[1].x = (PWord) addr;
  return pull(f, c, k); }

// last emitter called for a branch
// pops next branch address off scope stack alts
static C1(generate_cond_pop_branch) {
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

static PWord ldels(PCore *f, PWord lam, PWord l) {
  if (!twop(l)) return nil;
  PWord m = ldels(f, lam, B(l));
  if (!lassoc(f, lam, A(l))) B(l) = m, m = l;
  return m; }

static PWord desugr(PCore *f, PWord *d, PWord *e, PWord a) {
  if (!twop(a)) return (PWord) pairof(f, *e, nil);
  PWord b; avec(f, a, b = desugr(f, d, e, B(a)));
  return !b ? b : (PWord) pairof(f, A(a), b); }

static PStatus desug(PCore *f, PWord *d, PWord *e) {
  if (!twop(*d)) return Ok;
  PWord x, l = (PWord) literal_symbol(f, "\\");
  if (!l || !pushs(f, 1, l)) return Oom;
  do if (!(x = (PWord) desugr(f, d, e, B(*d))) ||
         !(x = (PWord) pairof(f, f->sp[0], x)))
    return Oom;
  else *d = A(*d), *e = x;
  while (twop(*d));
  return f->sp++, Ok; }

static size_t analyze_let(PCore *f, scope *b, size_t m, PWord exp) {
  if (!twop(exp)) return analyze(f, b, m, nil);
  if (!twop(B(exp))) return analyze(f, b, m, A(exp));
  scope q = *b, *c = &q;
  avec(f, exp, q = enscope(f, q, q->args, q->imps));
  if (!q) return 0;
  // lots of variables :(
  PWord nom = nil, def = nil, lam = nil,
       v = nil, d = nil, e = nil;
  MM(f, &nom), MM(f, &def), MM(f, &exp), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);
// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.

  // collect vars and defs into two lists
  for (; twop(exp) && twop(B(exp)); exp = BB(exp)) {
    d = A(exp), e = AB(exp), desug(f, &d, &e);
    if (!(nom = (PWord) pairof(f, d, nom)) ||
        !(def = (PWord) pairof(f, e, def)))
      goto fail;
    if (lambp(f, e)) {
      // if it's a lambda compile it and record in lam list
      PWord x = analyze_lambda(f, c, nil, B(e));
      x = x ? (PWord) pairof(f, d, x) : x;
      lam = x ? (PWord) pairof(f, x, lam) : x;
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
          PWord vars = BBA(e), var = A(v);
          if (lidx(f, vars, var) < 0) { // only add if it's not already there
            if (!(vars = (PWord) pairof(f, var, vars))) goto fail; // oom
            j++, BBA(e) = vars; } }
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e)) ;

  (*c)->lams = lam;
  // construct lambda with reversed argument list
  exp = lconcat(f, nom, exp);
  PSymbol* l = exp ? literal_symbol(f, "\\") : 0;
  exp = l ? (PWord) pairof(f, (PWord) l, exp) : 0;
  m = exp ? analyze(f, b, m, exp) : 0; // exp is now the required lambda, analyze it
  if (!m || !((*b)->pals = (PWord) pairof(f, nil, (*b)->pals))) goto fail;

  // reverse the nom and def lists
  nom = rlconcat(f, nom, nil), def = rlconcat(f, def, nil);
  // evaluate definitions in order tracking var names on pals list
  // store lambdas on scope for lazy binding and construct new application
  // - reverse noms onto exp
  // - reverse onto e = nil and recompile lambdas
  size_t nn = 0;
  for (; twop(nom); nom = B(nom), def = B(def), nn++) {
    // if lambda then recompile with the explicit closure
    // and put in arg list and lam list (latter is used for lazy binding)
    if (lambp(f, A(def))) {
      d = lassoc(f, lam, A(nom));
      PWord _;
      if (!(_ = analyze_lambda(f, c, BB(d), BA(def)))) goto fail;
      else A(def) = B(d) = _; }
    // if toplevel then bind
    if (even && nilp((*b)->args)) {
      PCell *t = cells(f, 2 * Width(PPair) + 2 + Width(struct tag));
      if (!t) goto fail;
      PPair *w = (PPair*) t,
               *x = w + 1;
      t += 2 * Width(PPair);
      t[0].ap = define, t[1].x = A(nom), t[2].x = 0, t[3].m = t;
      ini_pair(w, A(def), nil); // dict add
      ini_pair(x, (PWord) t, (PWord) w);
      A(def) = (PWord) x; }
    if (!(m = analyze(f, b, m, A(def))) ||
        !((*b)->pals = (PWord) pairof(f, A(nom), (*b)->pals)))
      goto fail; }

  m = nn <= 1 ? pushs(f, 1, c1ap) ? m + 1 : 0 :
                pushs(f, 2, c1apn, putnum(nn)) ? m + 2 : 0;
  for (nn++; nn--; (*b)->pals = B((*b)->pals));
done: return UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), UM(f), m;
fail: m = 0; goto done; }

static size_t analyze_sequence(PCore *f, scope *c, size_t m, PWord x) {
  if (!twop(x)) return em2(f, c, m, pushk, nil);
  for (MM(f, &x); m && twop(B(x)); x = B(x))
    m = analyze(f, c, m, A(x)),
    m = m ? em1(f, c, m, drop) : m;
  return UM(f), m ? analyze(f, c, m, A(x)) : m; }

static C0(analyze_macro, PWord b) {
  if (!pushs(f, 2, x, b)) return 0;
  x = (PWord) literal_symbol(f, "`");
  if (!x || !pushs(f, 1, x)) return 0;
  PPair *mxp = (PPair*) cells(f, 4 * Width(PPair));
  if (!mxp) return 0;
  x = (PWord) ini_pair(mxp, f->sp[1], (PWord) ini_pair(mxp+1, (PWord) ini_pair(mxp+2, f->sp[0], (PWord) ini_pair(mxp+3, f->sp[2], nil)), nil));
  f->sp += 2, *f->sp = x;
  return p_eval(f) != Ok ? 0 : analyze(f, c, m, pop1(f)); }

static C0(analyze_list) {
  PWord a = A(x), b = B(x);
  if (!twop(b)) return analyze(f, c, m, a); // singleton list has value of first element
  if (symp(a)) {
    PWord macro = table_get(f, f->macro, a, 0);
    if (macro) return analyze_macro(f, c, m, macro, b);
    PString* n = ((PSymbol*) a)->nom;
    if (n && n->len == 1)
      switch (n->text[0]) { // special form?
        case '`': return em2(f, c, m, pushk, twop(b) ? A(b) : nil); // quote
        case ',': return analyze_sequence(f, c, m, b); // sequence
        case ':': return analyze_let(f, c, m, b);
        case '?': return analyze_if(f, c, m, b);
        case '\\': return (x = analyze_lambda(f, c, nil, b)) ? analyze(f, c, m, x) : x; } }

  size_t argc = llen(b);
  avec(f, a, m = assemble_arguments(f, c, m + argc, b));
  for (m = m ? analyze(f, c, m, a) : m; m && argc--;
    m = pushs(f, 1, c1ap1) ? m : 0,
    (*c)->pals = B((*c)->pals));
  return m; }

static Vm(g_bind) {
  PWord x = Ip[1].x, var = A(x);
  PTable *t = (PTable*) B(x);
  x = table_get(f, t, var, var);
  Ip[0].ap = pushk;
  Ip[1].x = x;
  return Continue(); }

static Vm(lazy_bind) {
  PWord ref = Ip[1].x, var = A(ref);
  scope env = (scope) B(ref);
  var = AB(lassoc(f, env->lams, var));
  Ip[0].ap = pushk;
  Ip[1].x = var;
  return Continue(); }

static PWord lassoc(PCore *f, PWord l, PWord k) {
  for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
  return 0; }

// list concat
static PWord lconcat(PCore *f, PWord l, PWord n) {
  if (!twop(l)) return n;
  avec(f, l, n = lconcat(f, B(l), n));
  return n ? (PWord) pairof(f, A(l), n) : n; }

// reverse list concat
static PWord rlconcat(PCore *f, PWord l, PWord n) {
  for (PWord m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
  return n; }

// allocate a thread
PCell *mo_n(PCore *f, uintptr_t n) {
  PCell *k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

// compile and execute expression
NoInline PStatus p_eval(PCore *f) {
  scope c = enscope(f, (scope) nil, nil, nil);
  if (!c) return Oom;
  PWord x = f->sp[0];
  f->sp[0] = (PWord) yieldk;
  PCell *k = 0;
  size_t m = 1;
  avec(f, c,
    m = analyze(f, &c, m, x),
    m = m ? em1(f, &c, m, yield) : m,
    k = m ? construct(f, &c, m) : k);
  k = k ? (PCell*) pushs(f, 1, k) : k;
  if (!k) return Oom;
  f->sp[0] = (PWord) f->ip;
  f->ip = k;
  PStatus s;
#if TCO
  s = f->ip->ap(f, f->ip, f->hp, f->sp);
#else
  for (s = Ok; s == Ok; s = f->ip->ap(f));
  s = s == Eof ? Ok : s;
#endif
  if (s != Ok) f->ip = 0, f->sp = f->pool + f->len;
  else x = f->sp[0], f->ip = (PCell*) *++f->sp, f->sp[0] = x;
  return s; }
static NoInline size_t analyze_symbol(PCore*, scope*, size_t, PWord, scope);
static C0(analyze) {
  if (homp(x) && ptr(x)->ap == data) {
    PType *y = dtyp(x);
    if (y == &pair_type) return analyze_list(f, c, m, x);
    if (y == &symbol_type) return analyze_symbol(f, c, m, x, *c); }
  return em2(f, c, m, pushk, x); }

static PVm lazy_bind, drop, define, g_bind;
static C0(analyze_variable_reference) {
  if (nilp((PWord) (*c)->par)) {
    x = (PWord) pairof(f, x, (PWord) f->dict);
    return !x ? x : em2(f, c, m, g_bind, x); }
  return pushs(f, 3, c1var, x, (*c)->pals) ? m + 2 : 0; }

static long index_of(PCore *f, scope c, PWord var) {
  size_t i = 0;
  // is it a closure variable?
  for (PWord l = c->imps; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  for (PWord l = c->args; twop(l); l = B(l), i++)
    if (eql(f, var, A(l))) return i;
  return -1; }

static C1(c2var) {
  PWord var = *f->sp++, pals = *f->sp++;
  size_t i = lidx(f, pals, var);
  return
    k[-2].ap = pushr,
    k[-1].x = putnum(i),
    pull(f, c, k - 2); }

// emit stack reference instruction
static C1(c1var) {
  PWord var = *f->sp++, // variable name
       ins = llen(*f->sp++), // stack inset
       idx = index_of(f, *c, var);
  return
    k[-2].ap = pushr,
    k[-1].x = putnum(idx + ins),
    pull(f, c, k - 2); }

#define P(x) (transmit(f, stdout, x), puts(""))
static C0(analyze_symbol, scope d) {
  PWord y;
  if (nilp((PWord) d)) {
    y = table_get(f, f->dict, x, 0);
    if (y) return em2(f, c, m, pushk, y);
    x = (PWord) pairof(f, x, (*c)->imps),
    x = x ? A((*c)->imps = x) : x;
    return x ? analyze_variable_reference(f, c, m, x) : 0; }

  // look in vals
  if ((y = lassoc(f, d->lams, x))) {
    // lazy bind
    bind(y, (PWord) pairof(f, y, (PWord) d));
    bind(m, em2(f, c, m, lazy_bind, y));
    x = f->sp[2];
    y = BBA(x); // get the closure args to pass in
    A(x) = AA(x);
    return analyze_arguments(f, c, m, y); } // XXX

  // look in pals
  if ((y = lidx(f, d->pals, x)) >= 0)
    return pushs(f, 3, c2var, x, d->pals) ? m + 2 : 0;

  // look in imps args
  y = index_of(f, d, x);
  if (y >= 0) {
    if (*c != d)
      x = (PWord) pairof(f, x, (*c)->imps),
      x = x ? A((*c)->imps = x) : x;
    return x ? analyze_variable_reference(f, c, m, x) : 0; }
  // recur on outer scope
  return analyze_symbol(f, c, m, x, d->par); }

// emits call instruction and modifies to tail call
// if next operation is return
static C1(c1ap) {
  if (k->ap == ret) k->ap = tap; // tail call
  else (--k)->ap = ap; // regular call
  return pull(f, c, k); } // ok

static C1(c1ap1) {
  if (k->ap == ret) k->ap = tap1; // tail call
  else (--k)->ap = ap1; // regular call
  return pull(f, c, k); } // ok
                          //
static C1(c1apn) {
  PWord n = *f->sp++;
  if (k->ap == ret) k->x = n, (--k)->ap = tapn;
  else (--k)->x = n, (--k)->ap = apn;
  return pull(f, c, k); }

static size_t assemble_arguments(PCore *f, scope *c, size_t m, PWord x) {
  if (!twop(x)) return m;
  PWord a = A(x);
  MM(f, &a);
  m = assemble_arguments(f, c, m, B(x));
  UM(f);
  if (!m) return m;
  m = analyze(f, c, m, a);
  if (!m || !((*c)->pals = (PWord) pairof(f, nil, (*c)->pals))) return 0;
  return m; }


// evaluate function call arguments and apply
static size_t analyze_arguments(PCore *f, scope *c, size_t m, PWord x) {
  MM(f, &x); // handle oom here ..
  if (!((*c)->pals = (PWord) pairof(f, nil, (*c)->pals))) m = 0;
  else {
    for (; m && twop(x); x = B(x))
      m = analyze(f, c, m + 1, A(x)),
      m = m && pushs(f, 1, c1ap) ? m : 0;
    (*c)->pals = B((*c)->pals); }
  UM(f);
  return m; }

// lambda decons pushes last list item to stack returns init of list
static PWord linit(PCore *f, PWord x) {
  if (!twop(x)) return pushs(f, 1, nil) ? nil : 0;
  if (!twop(B(x))) return pushs(f, 1, A(x)) ? nil : 0;
  PWord y = A(x);
  avec(f, y, x = linit(f, B(x)));
  return x ? (PWord) pairof(f, y, x) : x; }


static PWord analyze_lambda(PCore *f, scope *c, PWord imps, PWord exp) {
  // storing exp in scope->args for the moment is expedient
  scope d = enscope(f, *c, exp, imps);
  if (!d) return 0;
  MM(f, &d);
  // get the real args
  PWord args = linit(f, d->args);
  if (!(d->args = args)) goto fail;
  exp = f->sp[0], f->sp[0] = (PWord) yieldk;
  size_t m = analyze(f, &d, 4, exp);
  if (!m) goto fail;
  size_t arity = llen(d->args) + llen(d->imps);
  PCell *k = pushs(f, 3, c1ix, ret, putnum(arity)) ? construct(f, &d, m) : 0;
  if (!k) goto fail;
  if (arity > 1) (--k)->x = putnum(arity), (--k)->ap = curry;
  trim_thread(k);
  UM(f);
  return (PWord) pairof(f, (PWord) k, d->imps);
fail:
  UM(f);
  return 0; }

