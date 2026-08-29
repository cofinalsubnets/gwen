// ev.c -- ev, vm, the lisp help. one translation unit of the runtime;
// the shared layouts and the cross-TU seam are src/love_int.h.
#include "love_int.h"
struct ai_wait_fd;
// this file's own, forward-declared so order within it does not matter.
static Ana(ana_2, word a, word b);
static Ana(ana_c);
static Ana(ana_l);
static Ana(ana_q);
static Ana(ana_v);
static Ana(c0_cond_exit);
static Ana(c0_cond_r);
static Cata(c1);
static Cata(c1_apn);
static Cata(c1_ar, lvm_t *i, word ar);
static Cata(c1_cond_exit);
static Cata(c1_cond_pop_exit);
static Cata(c1_cur);
static Cata(c1_i);
static Cata(c1_ix);
static Cata(c1_recv);
static Cata(c1_ret);
static Cata(c1_var);
static Cata(c1_yield);
static ai_noinline Ana(analyze);
static ai_noinline int poll_parked(struct ai *g, uintptr_t now);
static ai_noinline int wake_parked(struct ai *g, uintptr_t now,
                                   struct ai_wait_fd const *fds, int nfds, int ask);
static ai_noinline union u *yield_sw_wait(struct ai *g, uintptr_t my_wake, int my_wait_fd, int my_events, int me_live);
static ai_noinline void wait_one(int fd, int events, uintptr_t ms);
static ai_noinline word missing_tag(struct ai *g);
static bool lambp(struct ai *g, word x);
static bool lexbound(struct ai *g, struct env *d, word x);
static int parked_ready(struct ai *g, union u *n, uintptr_t now,
                                  struct ai_wait_fd const *fds, int nfds, int *cur, int ask);
static int polled_ready(struct ai_wait_fd const *fds, int nfds, int *cur, int fd, int ev);
static int task_live(struct ai *g, union u *head, intptr_t pid, int me_live);
static intptr_t ai_ceilnet(struct ai *g, word x);
static lvm(ap_next);
static lvm(help_ret_more);
static lvm(help_ret_scare);
static lvm(lvm_add_coin);
static lvm(lvm_coin_op);
static lvm(lvm_mul_coin);
static lvm(lvm_numap);
static lvm(lvm_numtap);
static lvm(lvm_resume);
static struct ai *ai_raise(struct ai *c, word a, word b, union u const *K);
static struct ai *ana_ap(struct ai *g, struct env **c, intptr_t x);
static struct ai *ana_ap_r2l(struct ai *g, struct env **c, word x);
static struct ai *ana_d(struct ai *g, struct env **b, word exp);
static struct ai *c0_i(struct ai *g, struct env **c, lvm_t *i);
static struct ai *c0_ix(struct ai *g, struct env **c, lvm_t *i, word x);
static struct ai *c0_lambda(struct ai *g, struct env **c, intptr_t imps, intptr_t exp);
static struct ai *enscope(struct ai *g, struct env *par, word args, word imps);
static struct ai *eset(struct ai *g, struct env **c, int k, word v);
static struct ai *lbox(struct ai *g);
static struct ai *ldels(struct ai *g, word lam, word l);
static struct ai *lset(struct ai *g, word y, int k, word v);
static struct ai *pushl(struct ai*g);
static struct ai *rev(struct ai *g, word l);
static struct ai *sset(struct ai *g, word s, int k, word v);
static union u *parked_find(struct ai *g, intptr_t pid, union u **prevp);
static union u *run_splice_at(struct ai *g, union u *tail, union u *n);
static void parked_drop(struct ai *g, union u *prev, union u *n);
static word *task_help(struct ai *g);
static word assq(struct ai *g, word l, word k);
static word eget(struct ai *g, struct env *e, int k);
static word lget(struct ai *g, word y, int k);
static word lidx(struct ai*g, word x, word l);
static word memq(struct ai *g, word l, word k);
static word sget(struct ai *g, word s, int k);
// ============================================================================
// ev
// ============================================================================
static ai_inline struct ai *pushl(struct ai*g) { return intern(ai_strof(g, "\\")); }
static ai_noinline struct ai *c0(struct ai *g, lvm_t *y);
struct ai *ai_eval_(struct ai *g);

// function state using this type
struct env {
 struct env *par; // enclosing scope
 word tab, // the mutable scope, keyed by the E* fixnums: positional and closure variables,
           //   the stack of computed args and let bindings, the let's lambdas, the cond
           //   branch and exit addresses, the backpatch sites, a lambda's source \-expr,
           //   and fars, the binding names pinned before a let's lambdas compile. ev.l's
           //   own scope is a tablet read the same way -- (c 'stk), (c 'imp), (c 'lam).
  len,     // thread length accumulator: a fixnum, so no store to it needs a barrier
  end[]; };

typedef Ana(ana);
typedef Cata(cata);
static Ana(ana_2, word, word);
static Cata(pull) { return ai_ok(g) ? ((cata*) pop1(g))(g, c) : g; }

// generic instruction ana aps
static ai_inline struct ai *c0_ix(struct ai *g, struct env **c, lvm_t *i, word x) {
 return incl(*c, 2), ai_push(g, 3, c1_ix, i, x); }

static ai_inline struct ai *c0_i(struct ai *g, struct env **c, lvm_t *i) {
 return incl(*c, 1), ai_push(g, 2, c1_i, i); }

// the scope's mutable fields ride a tablet, the way ev.l's own scope does ((c 'stk),
// (c 'imp), ..). ai_mapput barriers its own stores, so none of these writes carries a
// barrier of its own -- the hand-kept invariant retires with them.
enum { EStack, EArgs, EImps, ELams, EBranch, EExit, ESites, ESrc, EFars };
static ai_inline word eget(struct ai *g, struct env *e, int k) {
 return ai_mapget(g, zero, putcharm(k), e->tab); }
static struct ai *eset(struct ai *g, struct env **c, int k, word v) {
 g = ai_push(g, 3, putcharm(k), v, (*c)->tab);   // sp0 key, sp1 val, sp2 map
 if (ai_ok(g = ai_mapput(g))) g->sp++;           // mapput leaves the map: drop it
 return g; }
// a let's lambda entry is (name . box). the box holds the closure's thread and its
// import row, and both a backpatch site and the capture fixpoint write THROUGH it, so
// the shared mutable cell is a real heap object rather than a cons somebody patches --
// ev.l's closure cell is the same thing (mkc/cof/cput, a tablet under key 0).
enum { LThread, LImps };
// a backpatch site is a box too: the entry whose thread fills the hole, and the hole.
// ⚠ the hole is an interior pointer and has to be -- c1's clip re-points the terminator
// at the entry once emission ends, so the head this cell was indexed from is no longer
// the thread's, and no base available here stays one. gcp relocates an interior pointer
// into a thread by preserving its offset, which is what carries this across a move.
enum { SEntry, SCell };
static ai_inline word sget(struct ai *g, word s, int k) {
 return ai_mapget(g, zero, putcharm(k), s); }
static struct ai *sset(struct ai *g, word s, int k, word v) {
 g = ai_push(g, 3, putcharm(k), v, s);
 if (ai_ok(g = ai_mapput(g))) g->sp++;
 return g; }
static ai_inline word lget(struct ai *g, word y, int k) {
 return ai_mapget(g, zero, putcharm(k), B(y)); }
static struct ai *lset(struct ai *g, word y, int k, word v) {   // ⚠ y must be rooted: a
 g = ai_push(g, 3, putcharm(k), v, B(y));                       //   growing put allocates
 if (ai_ok(g = ai_mapput(g))) g->sp++;
 return g; }
// sp0 is c0_lambda's (thread . imports); answers a box carrying it, in its place
static struct ai *lbox(struct ai *g) {
 g = map_new(g);                                       // sp0 map, sp1 pair
 if (!ai_ok(g)) return g;
 g = ai_push(g, 3, putcharm(LThread), A(g->sp[1]), g->sp[0]);
 if (ai_ok(g = ai_mapput(g))) g->sp++;                 // back to sp0 map, sp1 pair
 if (!ai_ok(g)) return g;
 g = ai_push(g, 3, putcharm(LImps), B(g->sp[1]), g->sp[0]);
 if (ai_ok(g = ai_mapput(g))) g->sp++;
 if (!ai_ok(g)) return g;
 return g->sp[1] = g->sp[0], g->sp++, g; }
static struct ai *enscope(struct ai *g, struct env *par, word args, word imps) {
 uintptr_t const n = Width(struct env) + Width(struct ai_tag);
 g = ai_push(g, 3, args, imps, par);
 g = map_new(g);                                   // sp0 = tab, then args/imps/par
 if (ai_ok(g = ai_have(g, n))) {
  struct env *c = bump(g, n);
  c->len = zero;
  c->tab = g->sp[0];
  c->par = (struct env*) g->sp[3];
  g->sp[3] = (word) tagthread((union u*)c, Width(struct env)); }   // env at sp3; tab/args/imps stay
 if (!ai_ok(g)) return g;
 { struct env *e = (struct env*) g->sp[3];                     // re-read after every put: a
   g = eset(g, &e, EArgs, g->sp[1]);                           // grow inside mapput would move it
   if (ai_ok(g)) e = (struct env*) g->sp[3], g = eset(g, &e, EImps, g->sp[2]); }
 return ai_ok(g) ? (g->sp += 3, g) : g; }


static word memq(struct ai *g, word l, word k) {
 for (; chainp(l); l = B(l)) if (eql(g, k, A(l))) return l;
 return 0; }

static word assq(struct ai *g, word l, word k) {
 for (; chainp(l); l = B(l)) if (eql(g, k, AA(l))) return A(l);
 return 0; }

static struct ai *append(struct ai *g) {
 uintptr_t i = 0;
 for (word l; ai_ok(g) && chainp(g->sp[0]); i++)
  l = B(g->sp[0]),
  g->sp[0] = A(g->sp[0]),
  g = ai_push(g, 1, l);
 if (!ai_ok(g)) return g;
 if (i == 0) return g->sp++, g;
 for (g->sp[0] = g->sp[i + 1]; i--; g = gxr(g));
 if (ai_ok(g)) g->sp[1] = g->sp[0], g->sp++;
 return g; }

// don't inline this so callers can tail call optimize
static ai_noinline struct ai *c0(struct ai *g, lvm_t *y) {
 // every in-place store below is precisely barriered (gen_wb_cell/two), so a
 // mid-compile collection stays minor. the opfix prepass runs first; a chain whose
 // head is already a top is a constructed direct application (never readable
 // source): skipped, which also terminates the recursion through ai_eval_.
 { word x0 = g->sp[0];
   if (chainp(x0) && (!lamp(A(x0)) || datp(A(x0)))) {
    word of = ai_core_of(g)->hot_opfix;          // sealed: a book rebind can't reach this lane;
    if (lamp(of)) {                              // pre-seal (mid-prel bootstrap) it is zero and
                                                 // the pass skips -- everything there is prefix
     g = ai_eval_(gxr(gxl(gxl(pushq(gxl(ai_push(g, 4, x0, zero, zero, of)))))));
     if (!ai_ok(g)) return g;
     g->sp[1] = g->sp[0], g->sp += 1; } } }
 if (!ai_ok(g = enscope(g, (struct env*) zero, zero, zero))) return g;
 struct env *c = (void*) ptr(pop1(g));
 word x = g->sp[0];
 g->sp[0] = (word) c1_yield;
 mm(g, &c); mm(g, &x);
 if (ai_ok(g = analyze(g, &c, x)))
   g = c1(c0_ix(g, &c, y, word(g->ip)), &c);
 um(g), um(g);
 return g; }

static Cata(c1) {
 uintptr_t l = getcharm((*c)->len);
 // a lambda carries its source \-expr: reserve one extra leading word for it so
 // it sits at value[-1] (the printer's discriminator) and rides inside the thread
 // span (head = src word) for free GC tracing. top-level/aux threads have no src.
 uintptr_t extra = zerop(eget(g, (*c), ESrc)) ? 0 : 1;
 g = ai_have(g, l + extra + Width(struct ai_tag));
 if (ai_ok(g)) {
  union u *k = bump(g, l + extra + Width(struct ai_tag));
  memset(k, -1, (l + extra) * sizeof(word));
  Kp = tagthread(k, l + extra) + l + extra;
  if (ai_ok(g = pull(g, c))) {           // pull emits l words (may GC); Kp now = entry
   // read src after all allocation: ai_have/pull can GC and relocate the env's src.
   if (extra) Kp[-1].x = eget(g, (*c), ESrc),     // value[-1] = source \-expr
              gen_wb_cell(g, Kp - 1, Kp[-1].x),
              clip(g, Kp - 1);          // tag head spans [src .. body]; value stays Kp
   else clip(g, Kp); } }
 return g; }

static Cata(c1_yield) { return g; }

static Cata(c1_cond_pop_exit) { return
 g = eset(g, c, EExit, B(eget(g, *c, EExit))), // pops cond expression exit address off env exits
 pull(g, c); }

static Cata(c1_apn) {
 word arity = pop1(g);
 if (arity == putcharm(1)) {
  if (Kp[0].ap == lvm_ret) Kp[0].ap = lvm_tap;
  else Kp -= 1, Kp[0].ap = lvm_ap; }
 else {
  if (Kp[0].ap == lvm_ret) Kp -= 1, Kp[0].ap = lvm_tapn, Kp[1].x = arity;
  else Kp -= 2, Kp[0].ap = lvm_apn, Kp[1].x = arity; }
 return pull(g, c); }


static Cata(c1_i) {
 lvm_t *i = (void*) pop1(g);
 Kp -= 1;
 Kp[0].ap = i;
 return pull(g, c); }

static Cata(c1_ix) {
 lvm_t *i = (void*) pop1(g);
 word x = pop1(g);
 Kp -= 2;
 Kp[0].ap = i;
 Kp[1].x = x;
 gen_wb_cell(g, Kp + 1, x);
 return pull(g, c); }

// emit a recursive-function ref: bake `quote <the box's thread>` if the closure is final, else
// `quote zero` + stash the operand cell in the site for ana_d to backpatch.
static Cata(c1_recv) {
 word y = pop1(g), site = pop1(g);
 Kp -= 2;
 Kp[0].ap = lvm_quote;
 if (zerop(site)) return
   Kp[1].x = lget(g, y, LThread), gen_wb_cell(g, Kp + 1, Kp[1].x), pull(g, c);
 { struct ai_r *mm0 = ai_core_of(g)->root;             // sset allocates: root site first
   mm(g, &site);
   Kp[1].x = zero;
   g = sset(g, site, SCell, (word) &Kp[1]);
   ai_core_of(g)->root = mm0; }
 return pull(g, c); }

static Cata(c1_ar, lvm_t *i, word ar) { return
 Kp -= 2,
 Kp[0].ap = i,
 Kp[1].x = putcharm(ar),
 pull(g, c); }

static Cata(c1_cur) {
 struct env *e = (void*) pop1(g);
 uintptr_t ar = llen(eget(g, e, EArgs)) + llen(eget(g, e, EImps));
 return ar == 1 ? pull(g, c) : c1_ar(g, c, lvm_cur, ar); }

static Cata(c1_ret) {
 struct env *e = (struct env*) pop1(g);
 uintptr_t ar = llen(eget(g, e, EArgs)) + llen(eget(g, e, EImps));
 return c1_ar(g, c, lvm_ret, ar); }

cata1(c1_cond_push_branch, g = gxl(ai_push(g, 2, Kp, eget(g, *c, EBranch))), g = eset(g, c, EBranch, ai_ok(g) ? pop1(g) : zero))
cata1(c1_cond_push_exit, g = gxl(ai_push(g, 2, Kp, eget(g, *c, EExit))), g = eset(g, c, EExit, ai_ok(g) ? pop1(g) : zero))
cata1(c1_cond_pop_branch, Kp -= 2, Kp[0].ap = lvm_cond, Kp[1].x = A(eget(g, (*c), EBranch)),   // Kp[1] = a same-thread address: no cross-gen edge
      g = eset(g, c, EBranch, B(eget(g, *c, EBranch))))

static Cata(c1_cond_exit) {
 union u *a = cell(A(eget(g, (*c), EExit)));
 if (a->ap == lvm_ret || a->ap == lvm_tap)
  Kp = memcpy(Kp - 2, a, 2 * sizeof(*Kp));
 else if (a->ap == lvm_tapn)
  Kp = memcpy(Kp - 3, a, 3 * sizeof(*Kp));
 else
  Kp -= 2, Kp[0].ap = lvm_jump, Kp[1].x = (word) a;
 return pull(g, c); }

lvm(_lvm_yieldk) { return
 Ip = Ip[1].m,
 Pack(g),
 encode(g, ai_status_yield); }


// a hardware fault is a crash on every target: no handler, no recovery -- a fault
// means an invariant is already broken, and the immediate core dump names the site.
// (a barrier here once turned that class into a silent per-call siglongjmp storm.)
struct ai *ai_eval_(struct ai *g) {
 if (!ai_ok(g)) return g;                        // c0 reads g->sp[0] before any guard of its own
 g = c0(g, _lvm_yieldk);
#if ai_tco
 if (ai_ok(g)) g = g->ip->ap(g, g->ip, g->hp, g->sp);
 return g;
#else
 while (ai_ok(g)) g = g->ip->ap(g);
 if (ai_code_of(g) == ai_status_eof) g = ai_core_of(g);
 return g;
#endif
}

static word lidx(struct ai*g, word x, word l) {
 word i = 0;
 for (; chainp(l); i++, l = B(l)) if (eql(g, x, A(l))) return i;
 return -1; }

static Ana(ana_v) {
 word y;
 if (!ai_ok(g)) return g;
 for (struct env *d = *c;; d = d->par) {
  if (zerop(d)) {
   if ((y = bookget(g, 0, x))) return ana_q(g, c, y);
   // undefined global: resolved by lvm_index at run time. record it as a captured
   // free variable only when nested -- at top level imps would alias an
   // uninitialized arg slot. re-read x from the imps hook: the push above can GC.
   if (!zerop((*c)->par))
    g = gxl(ai_push(g, 2, x, eget(g, *c, EImps))),
    g = eset(g, c, EImps, ai_ok(g) ? pop1(g) : zero),
    x = ai_ok(g) ? A(eget(g, *c, EImps)) : zero;
   return c0_ix(g, c, lvm_index, x); }
  // lambda definition of local let form?
  if ((y = assq(g, eget(g, d, ELams), x))) {
   // recursive-fn ref: record a backpatch site on d (the lams-owning scope) when
   // the closure isn't built yet, then apply the captured imports.
   word site = zero;
   if (zerop(lget(g, y, LThread))) {
    mm(g, &d), mm(g, &y);
    g = map_new(g);                                    // site = a box, at sp0
    if (ai_ok(g)) g = sset(g, g->sp[0], SEntry, y);    // re-read sp0: a grow can move it
    if (ai_ok(g)) g = sset(g, g->sp[0], SCell, zero);
    if (ai_ok(g)) {
     g = gxl(ai_push(g, 2, g->sp[0], eget(g, d, ESites))); // (site . eget(g, d, ESites))
     if (ai_ok(g)) g = eset(g, &d, ESites, pop1(g)), site = pop1(g); }
    um(g), um(g); }
   incl(*c, 2);
   if (ai_ok(g = ai_push(g, 3, c1_recv, y, site)))
    g = ana_ap(g, c, lget(g, g->sp[1], LImps));
   return g; }
  // let binding in the *current* scope -> a direct stack slot.
  if (d == *c && memq(g, eget(g, d, EStack), x)) return
    c0_ix(g, c, lvm_arg, putcharm(lidx(g, x, eget(g, d, EStack))));
  // the shadow guard: d's let binds x (fars) but x is not yet a lams entry or a
  // slot -- the nom is this let's, so the walk must not escape to an enclosing
  // binding of the same spelling. import it; the rebuild resolves it through lams.
  if (!zerop(eget(g, d, EFars)) && memq(g, eget(g, d, EFars), x) &&
      !(!zerop(d->par) && memq(g, eget(g, d->par, EStack), x))) {
   if (!zerop((*c)->par))
    g = gxl(ai_push(g, 2, x, eget(g, *c, EImps))),
    g = eset(g, c, EImps, ai_ok(g) ? pop1(g) : zero),
    x = ai_ok(g) ? A(eget(g, *c, EImps)) : zero;
   return c0_ix(g, c, lvm_index, x); }
  // a let binding, closure var, or lambda arg. if from an enclosing scope, import
  // it into this scope's imps so the offset c1_var emits is valid in this frame.
  if (memq(g, eget(g, d, EStack), x) || memq(g, eget(g, d, EImps), x) || memq(g, eget(g, d, EArgs), x)) {
   incl(*c, 2);
   if (d != *c) // found in an enclosing scope -> import (capture) it
    g = gxl(ai_push(g, 2, x, eget(g, *c, EImps))),
    g = eset(g, c, EImps, ai_ok(g) ? pop1(g) : zero),
    x = ai_ok(g) ? A(eget(g, *c, EImps)) : zero;
   return ai_push(g, 3, c1_var, x, eget(g, *c, EStack)); } } }


static Cata(c1_var) {
 word v = pop1(g), i = llen(pop1(g)); // stack inset
 for (word l = eget(g, (*c), EImps); !zerop(l); l = B(l), i++)
  if (eql(g, v, A(l))) goto out;
 for (word l = eget(g, (*c), EArgs); !zerop(l); l = B(l), i++)
  if (eql(g, v, A(l))) break;
out:
 return Kp -= 2,
        Kp[0].ap = lvm_arg,
        Kp[1].x = putcharm(i),
        pull(g, c); }

static ai_noinline Ana(analyze) {
 if (nomp(x) && x != ZeroPoint) return ana_v(g, c, x); // lookup symbol as variable
 if (!chainp(x)) return ana_q(g, c, x); // non-chains are self quoting
 word a = A(x), b = B(x);                        // it must be a chain
 // if it is a special form then do that
 struct ai_str *nm;                             // a special form is headed by a 1-char named symbol (\ : ?)
 if (chainp(b) && (nm = nom_str(g, a)) && len(nm) == 1)  // chainp: (\) (:) (?) hold no operand to
                                                // consume, so an empty form is not a special form at
                                                // all -- it falls to (f) == f like every other head.
                                                // nom_str is 0 for a bare mint / the core / a non-sym
  switch (*txt(nm)) {
   case '\\': return ana_l(g, c, b);
   case ':': return ana_d(g, c, b);
   case '?': return ana_c(g, c, b); }
 return ana_2(g, c, x, a, b); }


static struct ai *c0_lambda(struct ai *g, struct env **c, intptr_t imps, intptr_t exp) {
 union u *k, *ip;
 word ops = exp;             // the full operand list (params… body) for the stored src
 struct env *d = NULL;
 mm(g, &d); mm(g, &exp); mm(g, &ops);
 g = enscope(g, *c, exp, imps);

 if (ai_ok(g)) {
  d = (struct env*) pop1(g);
  exp = eget(g, d, EArgs);
  int n = 0; // push exp args onto stack
  for (; chainp(B(exp)); exp = B(exp), n++) g = ai_push(g, 1, A(exp));
  for (g = push0(g); n--; g = gxr(g));
  exp = A(exp); }

 if (ai_ok(g)) {
  g = eset(g, &d, EArgs, g->sp[0]);
  g->sp[0] = (word) c1_yield;
  incl(d, 4);
  g = ai_push(g, 2, c1_cur, d);
  g = analyze(g, &d, exp);
  // stash the source \-expr for the printer after analyze (imps now known),
  // prepending the imports as leading params so a closure round-trips
  if (ai_ok(g)) {
   word l = eget(g, d, EImps); int ni = 0;
   mm(g, &l);
   for (; chainp(l); l = B(l), ni++) g = ai_push(g, 1, A(l));  // push imp1..impN
   um(g);
   g = ai_push(g, 1, ops);                                   // tail = (params… body)
   while (ni-- > 0) g = gxr(g);                             // fold: imps ++ ops
   g = gxl(pushl(g));                                       // link '\ onto the front
   if (ai_ok(g)) g = eset(g, &d, ESrc, pop1(g)); }
  if (ai_ok(g = ai_push(g, 2, c1_ret, d)))
    ip = g->ip,
    avec(g, ip, g = c1(g, &d)); }

 if (ai_ok(g)) k = g->ip, g->ip = ip, g = gxl(ai_push(g, 2, k, eget(g, d, EImps)));

 return um(g), um(g), um(g), g; }

static Ana(c0_cond_exit) { return
 incl(*c, 3),
 ai_push(analyze(g, c, x), 1, c1_cond_exit); }

static Ana(c0_cond_r) { return
 !chainp(x) ? c0_cond_exit(g, c, ZeroPoint) :   // clauses ran out: implicit else -> () (zero-ontology: the same () the reader terminates lists with)
 !chainp(B(x)) ? c0_cond_exit(g, c, A(x)) :
 (avec(g, x,
  incl(*c, 2),
  g = analyze(g, c, A(x)),
  g = ai_push(g, 1, c1_cond_pop_branch),
  g = c0_cond_exit(g, c, AB(x)),
  g = ai_push(g, 1, c1_cond_push_branch),
  g = c0_cond_r(g, c, BB(x))), g); }


static struct ai *ana_ap_r2l(struct ai *g, struct env **c, word x);
static struct ai *ana_ap(struct ai *g, struct env **c, intptr_t x) {
 if (!ai_ok(g)) return g;
 bool imfp =
  g->sp[0] == (word) c1_ix &&
  g->sp[1] == (word) lvm_quote &&
  lamp(g->sp[2]);
 intptr_t
  ca = llen(x),
  va =
   imfp && cell(g->sp[2])->ap == lvm_cur ?
    getcharm(cell(g->sp[2])[1].x) :
    1;
 bool b1p = ca == 1 && imfp && cell(g->sp[2])[1].ap == lvm_ret0,
      anp = va == ca && ca > 1,
      bnp = anp && cell(g->sp[2])[3].ap == lvm_ret0;

 if (b1p) { // inline an instruction
  lvm_t *i = cell(g->sp[2])->ap;
  g->sp += 3;
  g = c0_i(analyze(g, c, A(x)), c, i);
  return g; }

 if (bnp) { // inline a curried instruction
  lvm_t *i = cell(g->sp[2])[2].ap;
  g->sp += 3;
  g = c0_i(ana_ap_r2l(g, c, x), c, i); // r2l arg eval
  if (ai_ok(g)) { word s = eget(g, *c, EStack); while (ca--) s = B(s); g = eset(g, c, EStack, s); }
  return g; }

 if (ai_ok(g = gxl(ai_push(g, 3, zero, eget(g, *c, EStack), x)))) {
  g = eset(g, c, EStack, pop1(g)), x = pop1(g), mm(g, &x);
  if (anp) { // r2l 1 n-ary ap
   g = ana_ap_r2l(g, c, x),
   incl(*c, 2),
   g = ai_push(g, 2, c1_apn, putcharm(ca));
   if (ai_ok(g)) { word s = eget(g, *c, EStack); while (ca--) s = B(s); g = eset(g, c, EStack, s); } }
  else while (chainp(x)) // l2r n 1-ary ap
   g = analyze(g, c, A(x)),
   incl(*c, 2),
   g = ai_push(g, 2, c1_apn, putcharm(1)),
   x = B(x);
  um(g), g = eset(g, c, EStack, B(eget(g, *c, EStack))); }

 return g; }


static struct ai *ana_ap_r2l(struct ai *g, struct env **c, word x) {
 if (chainp(x)) {
  word y = A(x);
  avec(g, y, g = ana_ap_r2l(g, c, B(x)));
  g = analyze(g, c, y);
  g = gxl(ai_push(g, 2, zero, eget(g, *c, EStack)));
  if (ai_ok(g)) g = eset(g, c, EStack, pop1(g)); }
 return g; }

static ai_inline bool lambp(struct ai *g, word x) {
 struct ai_str *n;                                      // headed by the named symbol \ (nom_str 0 for a bare mint / non-sym)
 return chainp(x) && chainp(B(x)) && chainp(B(B(x))) &&
  (n = nom_str(g, A(x))) && len(n) == 1 && txt(n)[0] == '\\'; }

// reversal onto a FRESH spine: the source is read and never written, so no holder of it
// sees a list turn around. l is rooted because gxl allocates and a collection moves it;
// ai_push roots its own argument (ai_pushr), so A(l) crossing one is safe.
static struct ai *rev(struct ai *g, word l) {          // answers the reversed copy, pushed
 struct ai_r *mm0 = ai_core_of(g)->root;
 mm(g, &l);
 g = ai_push(g, 1, zero);
 for (; ai_ok(g) && chainp(l); l = B(l)) g = gxl(ai_push(g, 1, A(l)));
 return forget(); }

static struct ai *ldels(struct ai *g, word lam, word l);

// a lexically bound nom shadows a macro of the same spelling (ev.l's wx/cprop
// carry the twin guard). binder rosters only -- imps may record undefined globals.
static bool lexbound(struct ai *g, struct env *d, word x) {
 for (; !zerop(d); d = d->par)
  if (memq(g, eget(g, d, EArgs), x) || memq(g, eget(g, d, EStack), x) ||
      memq(g, eget(g, d, EFars), x) || assq(g, eget(g, d, ELams), x)) return true;
 return false; }

static ai_inline Ana(ana_2, word a, word b) {
 if ((x = macroget(ai_core_of(g), a)) && !lexbound(g, *c, a))   // macro table = each layer's [zero] slot, walked; the scope walk only on a macro hit
  return g = ai_eval_(gxr(gxl(gxl(pushq(gxl(ai_push(g, 4, b, zero, zero, x))))))),
         analyze(g, c, ai_ok(g) ? pop1(g) : 0);
 if (!chainp(b)) return analyze(g, c, a);  // (f) == f -- below the macro lane, which has no value to be
 return avec(g, b, g = analyze(g, c, a)),
        ana_ap(g, c, b); }

ai_inline Ana(ana_q) { return c0_ix(g, c, lvm_quote, x); }
static ai_inline Ana(ana_l) {
  if (!chainp(B(x))) return ana_q(g, c, A(x)); // one operand, no params: quote
  return g = c0_lambda(g, c, zero, x),
         analyze(g, c, ai_ok(g) ? pop1(g) : 0); }
static Ana(c0_cond_r);
static ai_inline Ana(ana_c) {
 return !chainp(B(x)) ? analyze(g, c, A(x)) :
    (g = ai_push(g, 2, x, c1_cond_pop_exit),
     g = c0_cond_r(g, c, ai_ok(g) ? pop1(g) : zero),
     ai_push(g, 1, c1_cond_push_exit)); }
// this is the longest C function :(
// it handles the let special form in a way to support sequential and recursive binding.
static ai_inline struct ai *ana_d(struct ai *g, struct env **b, word exp) {
 if (!chainp(B(exp))) return analyze(g, b, A(exp));
 struct ai_r *mm0 = ai_core_of(g)->root;
 mm(g, &exp);
 // delegate the letrec*-value rewrite to the l `boxfix` prepass once that global
 // exists: forward-referenced bindings indirect through nom-keyed cells (prel.l).
 // ev.l runs the same pass in feel, so both lanes share one boxfix.
 if (ai_ok(g = intern(ai_strof(g, "boxfix")))) {
  word bf = bookget(g, 0, pop1(g));
  if (bf && lamp(bf)) {
   g = ai_eval_(gxr(gxl(gxl(pushq(gxl(ai_push(g, 4, exp, zero, zero, bf)))))));
   if (ai_ok(g)) exp = pop1(g); } }
 g = enscope(g, *b, eget(g, (*b), EArgs), eget(g, (*b), EImps));
 if (!ai_ok(g)) return forget();
 struct env *q = (struct env*) pop1(g), **c = &q;
 // lots of variables :(
 word nom = zero, def = zero, ndef = zero, lam = zero,
      v = zero, d = zero, e = zero, os = zero;
 mm(g, &nom), mm(g, &def), mm(g, &ndef), mm(g, &lam);
 mm(g, &d); mm(g, &e); mm(g, &v); mm(g, &q); mm(g, &os);

 // pin the let's binding names on q before any lambda compiles (the shadow set):
 // the walk must see an inner-bound nom as bound here while lams is still zero,
 // or it resolves to an enclosing sibling and under-applies (cf. ev.l avb's 'far guard)
 for (d = exp; chainp(d) && chainp(B(d)); d = BB(d)) {
  for (e = A(d); chainp(e) && !nomp(e); e = A(e)); // unroll (f x..) define-sugar to the name
  g = gxl(ai_push(g, 2, e, eget(g, q, EFars)));
  if (!ai_ok(g)) return forget();
  g = eset(g, &q, EFars, pop1(g)); }

 // collect vars and defs into two lists, exposing the preceding bindings on the
 // enclosing stack so a sibling ref captures as a free variable instead of a
 // same-named global; the stack is restored before any code is emitted.
 os = eget(g, *b, EStack);
 while (chainp(exp) && chainp(B(exp))) {
  for (d = A(exp), e = AB(exp); chainp(d) && !nomp(d); e = pop1(g), d = A(d)) {  // a named sym is a chain now: stop the (f x) define-sugar unroll at the name
   g = gxl(ai_push(g, 2, e, zero));
   g = append(gxl(pushl(ai_push(g, 1, B(d)))));
   if (!ai_ok(g)) return forget(); }
  g = gxl(ai_push(g, 2, d, nom));
  g = gxl(ai_push(g, 2, e, def));
  if (!ai_ok(g)) return forget();
  def = pop1(g), nom = pop1(g);
  // if it's a lambda compile it and record in lam list
  if (lambp(g, e)) {
   g = ai_push(g, 2, d, lam);
   g = gxl(gxr(lbox(c0_lambda(g, c, zero, B(e)))));
   if (!ai_ok(g)) return forget();
   lam = pop1(g); }
  g = gxl(ai_push(g, 2, d, eget(g, *b, EStack))); // expose this binding to later siblings
  g = eset(g, b, EStack, ai_ok(g) ? pop1(g) : zero);
  exp = BB(exp); }
 g = eset(g, b, EStack, os);  // restore: emission below rebuilds the real frame

 intptr_t l = llen(nom);
 bool oddp = chainp(exp),
      globp = !oddp && zerop(eget(g, (*b), EArgs)); // we check this again later to make global bindings at top level
 if (!oddp) { // if there's no body then evaluate the name of the last definition
  g = gxl(ai_push(g, 2, A(nom), zero));
  if (!ai_ok(g)) return forget();
  exp = pop1(g); }

 // find closures: for each pair of bound functions, if e needs d then e needs d's variables
 word j, vars, var;
 do for (j = 0, d = lam; chainp(d); d = B(d)) // for each bound function variable
  for (e = lam; chainp(e); e = B(e)) if (d != e) // for each other bound function variable
   if (memq(g, lget(g, A(e), LImps), AA(d))) // if you need this function
    for (v = lget(g, A(d), LImps); chainp(v); v = B(v)) // then you need its variables
     if (!memq(g, vars = lget(g, A(e), LImps), var = A(v))) // only add if it's not already there
      j++,
      g = gxl(ai_push(g, 2, var, vars)),
      g = lset(g, A(e), LImps, ai_ok(g) ? pop1(g) : zero);
 while (j);

 // now delete defined functions from the closure variable lists
 // they will be bound lazily when the function runs
 for (e = lam; ai_ok(g) && chainp(e); e = B(e)) {
  g = ldels(g, lam, lget(g, A(e), LImps));
  if (ai_ok(g)) g = lset(g, A(e), LImps, pop1(g)); }

 g = eset(g, c, ELams, lam);
 g = append(gxl(pushl(ai_push(g, 2, nom, exp))));

 if (!ai_ok(g)) return forget();
 exp = pop1(g);

 //
 // all the code emissions are below here (??)
 //

 // clear each function's provisional closure so a ref hit mid-rebuild defers to a
 // backpatch site rather than baking the stale closure; keep the import sets (BB).
 for (d = lam; ai_ok(g) && chainp(d); d = B(d)) g = lset(g, A(d), LThread, zero);

 // ndef is def with each closure standing where its source did. built by consing over a
 // def that is still in reverse order, so ndef lands in literal order and needs no rev.
 for (e = nom, v = def; ai_ok(g) && chainp(e); e = B(e), v = B(v)) {
  word nv;
  if (lambp(g, A(v))) {
   d = assq(g, lam, A(e));
   size_t nb = llen(lget(g, d, LImps)); // the import row is frozen here: sites already applied it
   g = c0_lambda(g, c, lget(g, d, LImps), BA(v));
   if (!ai_ok(g)) return forget();
   g = lset(g, d, LThread, A(g->sp[0]));        // the pair stays on the stack across both
   if (ai_ok(g)) g = lset(g, d, LImps, B(g->sp[0]));   // puts: either can move it
   if (!ai_ok(g)) return forget();
   if (llen(lget(g, d, LImps)) != nb) __builtin_trap(); // growth = those sites under-apply (cf. ev.l weave's 'imports-grew scare)
   nv = g->sp[0], g->sp++; }
  else nv = A(v);
  g = gxl(ai_push(g, 2, nv, ndef));
  if (ai_ok(g)) ndef = pop1(g); }
 if (!ai_ok(g)) return forget();

 // closures final -> backpatch each recorded recursive-fn ref with its thread.
 for (d = eget(g, (*c), ESites); chainp(d); d = B(d)) {
  union u *hole = cell(sget(g, A(d), SCell));
  hole->x = lget(g, sget(g, A(d), SEntry), LThread), gen_wb_cell(g, hole, hole->x); }
 g = eset(g, c, ESites, zero);

 g = rev(g, nom);   // put in literal order
 if (!ai_ok(g)) return forget();
 nom = pop1(g);
 g = analyze(g, b, exp);
 g = gxl(ai_push(g, 2, zero, e = eget(g, *b, EStack))); // push function stack rep
 g = eset(g, b, EStack, ai_ok(g) ? pop1(g) : zero);
 for (def = ndef; chainp(nom); nom = B(nom), def = B(def))
  g = analyze(g, b, A(def)),
  g = globp ? c0_ix(g, b, lvm_defglob, A(nom)) : g,
  g = gxl(ai_push(g, 2, A(nom), eget(g, *b, EStack))),
  g = eset(g, b, EStack, ai_ok(g) ? pop1(g) : zero);
 return
  g = eset(g, b, EStack, e),
  incl(*b, 2),
  g = ai_push(g, 2, c1_apn, putcharm(l)),
  forget(); }

// drop the bound functions from a closure-variable list, onto a FRESH spine: the source
// is read and never written. tail first, so the copy builds back to front and the kept
// cells cons onto an answer that is already whole. lam and l are rooted across it --
// gxl allocates, and the recursion carries both over that.
static struct ai *ldels(struct ai *g, word lam, word l) {
 if (!ai_ok(g)) return g;
 if (!chainp(l)) return ai_push(g, 1, zero);
 struct ai_r *mm0 = ai_core_of(g)->root;
 mm(g, &lam), mm(g, &l);
 g = ldels(g, lam, B(l));
 if (ai_ok(g) && !assq(g, lam, A(l))) g = gxl(ai_push(g, 1, A(l)));
 return forget(); }

lvm(lvm_defglob) {
 Have(3);
 Sp -= 3;
 word k = Ip[1].x, v = Sp[3];
 Sp[0] = k, Sp[1] = v, Sp[2] = A(g->book), Pack(g);          // a pin lands in the head layer
 if (!ai_ok(g = ai_mapput(g))) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g), Sp += 1, Ip += 2;
 ai_musttail return Continue(); }

// lvm_index (the late-bound global read) is defined below lvm_scare: its
// miss path is the missing condition and borrows the whole help apparatus.

lvm(lvm_eval) { Ip++; Pack(g);
 if (!ai_ok(g = c0(g, lvm_jump))) ai_musttail return Ap(_lvm_ghelp, g);
 ai_musttail return Resume(); }

// ai_evals_ lives with the boot stitch it shares its machinery with, at the
// foot of the reader section.

// ============================================================================
// vm
// ============================================================================
// the hooks (love.h): lisp the C lanes reach by slot, handed over by (seal-hook n f).
// hot_hook traps on an unsealed slot -- a clean failure, never a wild read.
ai_word hot_hook(ai_word h) { if (!lamp(h)) __builtin_trap(); return h; }
// hooks 5 and 6 are the running task's, so they ride its ring node -- the head (cf.
// lvm_myself). a write is a store into a maybe-tenured node: gen_wb_cell, on a packed g.
ai_inline word *task_help(struct ai *g) { return &g->tasks[6].x; }
word *task_io(struct ai *g) { return &g->tasks[7].x; }

// `+`/`*` of two functions build a new function (church add / composition) from
// hooks 2 and 3; the C aps reuse numap_drive to compute the partial.
// fixnum application dispatches to (num-ap n x): numeric x -> x**n, function x ->
// x iterated n times. the drive is [ap, ap_next, ret0] over [n, num-ap, x, ret];
// lvm_numap is the non-tail form, lvm_numtap the tail form; the fused arg/quote
// variants push their argument and bump Ip so the layout lines up, then divert.
// ap_next applies the partial to the next argument: swap the result into operator
// position, then ap -- one ap_next cell in a drive = one more curried argument.
static lvm(ap_next) {
 word t = Sp[0]; Sp[0] = Sp[1], Sp[1] = t;
 ai_musttail return Ap(lvm_ap, g); }
union u const numap_drive[] = { {lvm_ap}, {.ap = ap_next}, {.ap = lvm_ret0} };

// --- the stackless call-out bridge (the glaze call-out arc) ---
// a native blob applies clos to arg the way the VM does: build [arg, clos, RET] on
// Sp, point Ip at callout_drive, tail-jump. RET is the blob's own native resume
// point, so control returns entirely through Continue() tail-jumps -- no C frame is
// pinned across the sub-run, so a deep callee grows the VM stack, never the C stack.
union u const callout_drive[] = { {lvm_ap}, {.ap = lvm_ret0} };
// (calloutdrive x) -> the drive's address as a fixnum (a probe; a native reads it off g->jk)
lvm(lvm_calloutdrive) { ai_musttail return Answer(putcharm((intptr_t) callout_drive)); }
// the walkable resume: v1's RET was a stack-interior pointer, which a collection
// with a call-out pending fed to gcp. here the frame is [arg, clos, tag(bb - entry),
// entry] -- the resume rides as an odd charm offset (the walk skips it) plus the
// blob's raw out-of-pool base, and lvm_resume jumps base+offset. relocation-safe.
static union u const callout_resume[] = { {lvm_ap}, {.ap = lvm_resume} };
lvm(lvm_calloutresume) { ai_musttail return Answer(putcharm((intptr_t) callout_resume)); }
// the addresses a native reads off g (love_int.h's JkX): the kind sentinels its guards
// compare against and the two drives -- a blob carries none of them, so it rides an image
void jk_ini(struct ai *g) {
 g->jk[JkChain] = (word) lvm_chain, g->jk[JkStr] = (word) lvm_str, g->jk[JkMap] = (word) lvm_map_lookup;
 g->jk[JkNom] = (word) lvm_nom, g->jk[JkMint] = (word) lvm_sym, g->jk[JkGem] = (word) lvm_gembox;
 g->jk[JkCask] = (word) lvm_cask, g->jk[JkDrive] = (word) callout_drive, g->jk[JkResume] = (word) callout_resume;
 g->jk[JkCur] = (word) lvm_cur, g->jk[JkUnc] = (word) lvm_unc; }

// ============================================================================
// the lisp help calling convention
// ============================================================================
// an installed help makes a raise the call (help a b) through help_drive
// (numap_drive's 2-arg twin) into a per-class epilogue: help_ret_more delivers the
// help's result to the raise site's resume text -- despite the name, the
// deliberate-scare lane, what makes (scare a b) and `missing` resumable; a bare
// scare is observed, then takes the default escape to C.
// the epilogue's arithmetic is the raise site's 3-word frame [resume a b], not
// the help frame the drive consumes, so the two sizes move apart.
static lvm(help_ret_more) {   // [result resume a b ..] -> resume sees result
 Ip = cell(Sp[1]);
 Sp[3] = Sp[0];
 Sp += 3;
 ai_musttail return Continue(); }
static lvm(help_ret_scare) {  // result ignored: scares are not (yet) resumable
 return Pack(g), encode(g, ai_status_scare); }
static union u const help_more_k[] = { {help_ret_more} };
static union u const help_scare_k[] = { {help_ret_scare} };
static union u const help_drive[] =
 { {lvm_ap}, {.ap = ap_next}, {.ap = lvm_ret0} };

// raise a scare with data a/b at the heard help as (help a b); with nothing heard (or
// still too tight after a collect) hand the scare-encoded core back to C.
// callers Pack first (ip stays at the raise site); a/b survive the collect in
// the scare_a/b stash, so the raise buys its own frame and never allocates.
static struct ai *ai_raise(struct ai *c, word a, word b, union u const *K) {
 c->scare_a = a, c->scare_b = b;  // for the exit face
 word h = *task_help(c);
 if (!ai_nilp(c, h) && avail(c) < 4) {
  struct ai *p = ai_please(c, 4);
  if (!ai_ok(p)) return encode(ai_core_of(p), ai_status_scare);
  c = ai_core_of(p);                            // moved: re-derive every pointer
  a = c->scare_a, b = c->scare_b;
  h = *task_help(c); }
 if (!ai_nilp(c, h) && avail(c) >= 4) {
  word *sp = c->sp -= 4;          // [a h b K | raise site data ..]
  sp[0] = a, sp[1] = h;
  sp[2] = b;
  sp[3] = word(K);
  c->ip = (union u*) help_drive;
#if ai_tco
  return c->ip->ap(c, c->ip, c->hp, c->sp);
#else
  return c;                       // ok-g: the trampoline dispatches help_drive
#endif
 }
 return encode(c, ai_status_scare);
}
// re-raise a failed op's scare: bare data, observe-then-terminal.
lvm(_lvm_ghelp) { return ai_raise(ai_core_of(g), zero, zero, help_scare_k); }
// (scare a b): the deliberate raise. the raise point is a clean boundary, so the
// help's result is delivered back as the value via the more continuation; with nothing heard
// it is terminal.
lvm(lvm_scare) {
 Have1();                          // the resume push only: ai_raise buys its own frame
 word a = Sp[0], b = Sp[1];
 *--Sp = word(Ip + 1);             // [resume a b ..]: help_more_k's layout
 return Pack(g), ai_raise(g, a, b, help_more_k); }
// the missing miss sentinel: a private static address no book value can equal,
// so a name bound to zero stays distinct from no entry at all.
static union u const no_entry[1];
// the GC-free C-data emitters (defined below), forward-declared for lvm_index's unheard-miss face.
struct ai *ioputs(struct ai*, char const*),
                 *ioputc(struct ai*, int);
struct ai *zflush(struct ai*);
// a missing read with nothing heard answers ZeroPoint: absence is a point, not a
// quantity -- a number would exponentiate under a numeral where a unit absorbs, which
// is what keeps (i love you) = 1. distinct from 0 and "".
// the 'missing tag is minted where it is used: both callers are cold, so a short-lived
// string beats a core slot. may collect, so Pack first and hold no heap local across
// it; answers the tag, or 0 if the intern failed.
static ai_noinline word missing_tag(struct ai *g) {
 struct ai *h = intern(ai_strof(g, "missing"));
 return ai_ok(h) ? ai_pop1(h) : 0; }

// a read of the live book by name -- the global twin of boxfix's (missing cell
// 'nom). a miss raises (help 'missing nom); with nothing heard it reads the zero point.
// the site never self-patches: a later define is seen, a rebind honoured.
lvm(lvm_index) {
 Have1();                          // room for the push first (may GC; no live local held yet)
 word v = bookget(g, word(no_entry), Ip[1].x);
 if (v != word(no_entry)) return
  *--Sp = v,                       // present: push the live value, no quote patch
  Ip += 2,
  Continue();
 word h = *task_help(g);
 if (ai_nilp(g, h)) {
#if __STDC_HOSTED__
  // nothing heard (file mode): the zero point is silent, so surface ";; missing <nom>"
  // on err and still answer it. missing-specific -- a deliberate scare stays
  // terminal. nom_str + ioput* hold no heap operand -> no GC, so Sp/Ip survive.
  struct ai_str *nm = nom_str(g, Ip[1].x);
  if (nm) { struct ai_io *sv = g->io; g->io = &ai_stderr.io;
            struct ai *w = ioputs(g, ";; missing ");
            for (uintptr_t i = 0; ai_ok(w) && i < nm->len; i++) w = ioputc(w, nm->bytes[i]);
            if (ai_ok(w)) w = ioputc(w, '\n');
            if (ai_ok(w)) zflush(w);
            g->io = sv; }
#endif
  *--Sp = ZeroPoint; ai_musttail return Next(2); }
 Pack(g);                          // the tag is minted only on the lane that carries it
 word a = missing_tag(g);          // may collect
 if (!a) ai_musttail return Ap(_lvm_ghelp, g);   // no tag to be had: the bare scare, still packed
 Unpack(g);
 Have(3);                          // after the intern: a collect here re-dispatches the
                                   // whole op, so `a` is either untouched or never read
 word b = Ip[1].x;
 Sp -= 3;
 Sp[0] = word(Ip + 2), Sp[1] = a, Sp[2] = b;   // help_more_k's layout
 return Pack(g), ai_raise(g, a, b, help_more_k); }
// the fused aps bump Ip so it points at an operand, not a re-runnable instruction --
// a plain Have() would re-dispatch into it. gc by hand and re-Ap (idempotent up to here).
#define NumapHave(self) if (Sp < Hp + 2) { \
 Pack(g); g = ai_please(g, 2); if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g); \
 Unpack(g); ai_musttail return Ap(self, g); }
static lvm(lvm_numap) {
 NumapHave(lvm_numap);
 word h = hot_hook(g->hot_numap);
 word n = Sp[1], x = Sp[0], *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = n, dst[1] = h, dst[2] = x, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; ai_musttail return Continue(); }
static lvm(lvm_numtap) {
 NumapHave(lvm_numtap);
 word h = hot_hook(g->hot_numap);
 word fs = getcharm(Ip[1].x), n = Sp[1], x = Sp[0], *dst = &Sp[fs + 2] - 3, ret = Sp[fs + 2];
 dst[0] = n, dst[1] = h, dst[2] = x, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; ai_musttail return Continue(); }

// (seal-hook n f): install f as core hook n (0 read, 1 num-ap, 2 stack, 3 compose,
// 4 opfix, 5 the help, 6 the task's stdio, 7 show). 5 and 6 are the dynamic slots and
// alone skip the lambda gate -- 5 takes a help or (), 6 a 3-chain or (); anything else
// traps. a switch, not a table: a slot[] would be an address-taken local (the lvm
// scratch rule).
lvm(lvm_seal) {
 if (getcharm(Sp[0]) != 5 && getcharm(Sp[0]) != 6 && !lamp(Sp[1])) __builtin_trap();   // the two dynamic slots alone skip the gate
 Pack(g);                        // 5 and 6 store into the node, and their barrier reads g->hp
 switch (getcharm(Sp[0])) {
  case 0: g->hot_read = Sp[1]; break;
  case 1: g->hot_numap = Sp[1]; break;
  case 2: g->hot_stack = Sp[1]; break;
  case 3: g->hot_compose = Sp[1]; break;
  case 4: g->hot_opfix = Sp[1]; break;
  case 5: *task_help(g) = Sp[1], gen_wb_cell(g, task_help(g), Sp[1]); break;
  case 6: *task_io(g) = chainp(Sp[1]) ? Sp[1] : zero, gen_wb_cell(g, task_io(g), *task_io(g)); break;   // anything but a chain hands the console back
  case 7: g->hot_show = Sp[1]; break;
  default: __builtin_trap(); }
 Sp += 1, Sp[0] = zero, Ip += 1;
 ai_musttail return Continue(); }
// (heard x) -> the installed help (x ignored): the live read of hook 5, what
// prel's cellread and bao's launcher ask before choosing to raise or install.
op11(lvm_heard, (intptr_t) *task_help(g))
// (worn x) -> the stdio this task wears (x ignored): the live read of hook 6, the
// zero point when it wears the console. what a caller saves before re-seating.
op11(lvm_worn, (intptr_t) *task_io(g))
// (myself x) -> the running task's own id (x ignored): the charm `twirl` answered for it,
// and the zero point for the task nobody twirled. the run ring's head is the running
// task, so this is a read of its pid slot. what a per-task escape compares against
// before it jumps -- a help is inherited at spawn, so a child can hold a continuation
// captured in its parent's stack, and landing there tears both.
op11(lvm_myself, (intptr_t) g->tasks[2].x)

// `+`/`*` over a lambda operand: build the combinator partial (stack/compose g g)
// through numap_drive. Ip is at the re-runnable +/* opcode, so a plain Have is
// safe; the slots are read after it (v0..end is what the GC updates).
lvm(lvm_addh) {
 if (coinp(Sp[0]) || coinp(Sp[1])) ai_musttail return Ap(lvm_add_coin, g);
 Have(2);
 word h = hot_hook(g->hot_stack);
 word fa = Sp[0], ga = Sp[1], *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = fa, dst[1] = h, dst[2] = ga, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; ai_musttail return Continue(); }
lvm(lvm_mulh) {
 if (coinp(Sp[0]) || coinp(Sp[1])) ai_musttail return Ap(lvm_mul_coin, g);
 Have(2);
 word h = hot_hook(g->hot_compose);
 word fa = Sp[0], ga = Sp[1], *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = fa, dst[1] = h, dst[2] = ga, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; ai_musttail return Continue(); }

// coin +/*/-//: run the coin's die method over the raw operands via numap_drive.
// two distinct dies have no canonical combination -> zero (the method never sees
// a foreign payload); a missing method is zero too. the ()-identity never
// reaches here -- the dispatchers hoist the mint case. Ip is still the opcode
// (Ap preserves it), so word(Ip + 1) is the true return.
static lvm(lvm_coin_op) {
 intptr_t slot = g->b;                              // the die slot, off the scratch
 word a = Sp[0], b = Sp[1];
 if (coinp(a) && coinp(b) && coin_die(a) != coin_die(b))
  return Push(ZeroPoint);             // two distinct newtypes: no canonical +/*
 word f = die_get(g, coinp(a) ? coin_die(a) : coin_die(b), slot);
 if (ai_nilp(g, f)) return Push(ZeroPoint);   // no method -> zero
 Have(2);
 a = Sp[0], b = Sp[1];                              // re-read post-GC
 f = die_get(g, coinp(a) ? coin_die(a) : coin_die(b), slot);
 word *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = a, dst[1] = f, dst[2] = b, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; return Continue(); }
static lvm(lvm_add_coin) { { g->b = (ai_word) (DieAdd); ai_musttail return Ap(lvm_coin_op, g); } }
static lvm(lvm_mul_coin) { { g->b = (ai_word) (DieMul); ai_musttail return Ap(lvm_coin_op, g); } }
// `-` and `/` have no kind matrix; lvm_sub/lvm_quot intercept coins themselves and land here.
lvm(lvm_sub_coin) { { g->b = (ai_word) (DieSub); ai_musttail return Ap(lvm_coin_op, g); } }
lvm(lvm_quot_coin) { { g->b = (ai_word) (DieDiv); ai_musttail return Ap(lvm_coin_op, g); } }

// applying a coin: run the die's apply closure as `((f self) arg)`; absent, a coin
// is an opaque handle -- nothing to answer with, () -- like a cask/port. self is the value at Ip (the apply
// trampoline sets Ip = the applied object); arg/ret are on the stack.
lvm(lvm_coin) {
 if (ai_nilp(g, die_get(g, coin_die(word(Ip)), DieApply))) {   // default opaque-apply: ()
  Ip = cell(*++Sp); *Sp = ZeroPoint; ai_musttail return Continue(); }
 Have(2);
 word self = word(Ip), f = die_get(g, coin_die(self), DieApply);
 word arg = Sp[0], ret = Sp[1], *dst = Sp - 2;
 dst[0] = self, dst[1] = f, dst[2] = arg, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; ai_musttail return Continue(); }

// (coin die payload) -> a fresh coin struck from the die over the payload.
lvm(lvm_coinmk) {
 Have(Width(struct ai_coin) + Width(struct ai_tag));
 union u *k = (union u*) Hp;
 Hp += Width(struct ai_coin) + Width(struct ai_tag);
 ((struct ai_coin*) k)->ap = lvm_coin;
 ((struct ai_coin*) k)->die = Sp[0];
 ((struct ai_coin*) k)->payload = Sp[1];
 tagthread(k, Width(struct ai_coin));
 ai_musttail return Push(word(k)); }
// (load x) -> the payload of a coin, else x itself (a plain value loads as itself).
lvm(lvm_load) {
 Sp[0] = coinp(Sp[0]) ? coin_load(Sp[0]) : Sp[0];
 ai_musttail return Next(1); }
op11(lvm_dieof, coinp(Sp[0]) ? coin_die(Sp[0]) : zero)   // (die-of x): a coin's die, else ()
op11(lvm_coinp, coinp(Sp[0]) ? putcharm(1) : zero)   // (coin? x)

// apply function to one argument
lvm(lvm_ap) {
 union u *k;
 if (oddp(Sp[1])) ai_musttail return Ap(lvm_numap, g);
 k = cell(Sp[1]), Sp[1] = word(Ip + 1), Ip = k;
 YieldCheck();
 ai_musttail return Continue(); }

// tail call
lvm(lvm_tap) {
 if (oddp(Sp[1])) ai_musttail return Ap(lvm_numtap, g);         // fixnum operator -> num-ap, deliver to caller
 intptr_t x = Sp[0], j = Sp[1];
 Sp += getcharm(Ip[1].x) + 1;
 Ip = cell(j), Sp[0] = x;
 YieldCheck();
 ai_musttail return Continue(); }

// apply to multiple arguments
lvm(lvm_apn) {
 size_t n = getcharm(Ip[1].x);
 union u *r = Ip + 2; // return address
 // this instruction is only emitted when the callee is known to be a function
 // so putting a value off the stack into Ip is safe. the +2 is cause we leave
 // the currying instruction in there... should be skipped in compiler instead FIXME
 Ip = cell(Sp[n]) + 2;
 Sp[n] = word(r); // store return address
 YieldCheck();
 ai_musttail return Continue(); }

// tail call
lvm(lvm_tapn) {
 size_t n = getcharm(Ip[1].x),
        r = getcharm(Ip[2].x);
 Ip = cell(Sp[n]) + 2;
 word *o = Sp;
 for (Sp += r + 1; n--; Sp[n] = o[n]);
 YieldCheck();
 ai_musttail return Continue(); }

// return
lvm(lvm_ret) {
 word n = getcharm(Ip[1].x) + 1;
 Ip = cell(Sp[n]); Sp[n] = Sp[0]; Sp += n; ai_musttail return Continue(); }

lvm(lvm_ret0) { return
 Ip = cell(Sp[1]),
 Sp[1] = Sp[0],
 Sp += 1,
 Continue(); }
// the walkable call-out resume (see callout_resume above): Sp[0]=result, Sp[1]=tag(bb - entry),
// Sp[2]=entry (the blob's W^X base). deliver the result where the blob expects it (Sp[0] on entry,
// two frame words consumed -- the same landing as the retired retB path) and tail-jump the blob's
// resume label. Ip is dead across a call-out (the blobs cache their twin in a Sp slot), so it rides
// through unchanged.
static lvm(lvm_resume) {
 lvm_t *t = (lvm_t*) (Sp[2] + ((word) Sp[1] >> 1));
 Sp[2] = Sp[0];
 Sp += 2;
 ai_musttail return Ap(t, g); }

// kcall : x = Sp[0], k = Ip[1] -> Ip = k, Sp[0] = x
lvm(lvm_kcall) {
 word x = Sp[0];
 union u *stack = Ip + 2, *end = (union u*) ttag(g, stack);
 uintptr_t height = end - stack;
 Have(height);
 *(Sp = memmove(topof(g) - height, stack, height * sizeof(word))) = x;
 Ip = Ip[1].m;
 ai_musttail return Continue(); }

// callk : i = Sp[0], k = Ip + 1 -> Ip = i, Sp[0] = k
lvm(lvm_callk) {
 word f_val = Sp[0];                         // g, the call_cc arg
 if (oddp(f_val)) ai_musttail return Next(1);
 word height = topof(g) - Sp;
 uintptr_t n = 2 + height;                   // lvm_kcall + (ip + 1) + stack = thread_contents
 Have(n + Width(struct ai_tag) + 1);          // thread_contents + thread_tag + 1 stack = _mem_req
 union u *k = (union u*) Hp;
 Hp += n + Width(struct ai_tag);              // thread_contents + thread_tag = _heap_alloc
 k[0].ap = lvm_kcall;                       // 
 k[1].m  = Ip + 1;                           // resume at next instruction
 memcpy(k + 2, Sp, height * sizeof(word));
 Sp -= 1;
 Sp[0] = word(tagthread(k, n));
 Sp[1] = f_val;
 ai_musttail return Ap(lvm_ap, g); }

// lvm_yield_sw_mono can't call ai_wait_fds directly with a stack record
static ai_noinline void wait_one(int fd, int events, uintptr_t ms) {
  struct ai_wait_fd w = { .fd = fd, .events = (short) events };
  ai_wait_fds(&w, 1, ms); }

// monotask fast path
static lvm(lvm_yield_sw_mono) { uintptr_t my_wake = g->next_wake_at;
 int my_wait_fd = g->next_wait_fd, my_events = g->next_wait_events;
 g->next_wake_at = 0;
 g->next_wait_fd = -1;
 g->next_wait_events = ai_wait_in;
 g->yield_ctr = 0;
 if (my_wake) for (uintptr_t now; my_wake > (now = ai_clock());)
  my_wait_fd >= 0 ? wait_one(my_wait_fd, my_events, my_wake - now) : ai_sleep(my_wake - now);
 else if (my_wait_fd >= 0)
  while (!ai_ready(my_wait_fd, my_events)) wait_one(my_wait_fd, my_events, 0);
 ai_musttail return Continue(); }

// the parked ring by pid; the predecessor comes back too (singly linked, and an
// unsplice cannot go looking for it twice)
static ai_inline union u *parked_find(struct ai *g, intptr_t pid, union u **prevp) {
 union u *head = g->parked;
 if (!head) return NULL;
 union u *prev = head;
 do { union u *n = prev->m;
      if (getcharm(n[2].x) == pid) return *prevp = prev, n;
      prev = n; } while (prev != head);
 return NULL; }

// take `n` off the parked ring. called with g packed: gen_wb reads g->hp to tell
// young from old, and the live Hp runs ahead of the last Pack.
static ai_inline void parked_drop(struct ai *g, union u *prev, union u *n) {
 if (prev == n) return (void) (g->parked = NULL);   // it was the whole ring
 prev->m = n->m;
 gen_wb(g, (word) prev, (word) prev->m);            // an old node now links to a (maybe young) successor
 if (g->parked == n) g->parked = prev; }

// ...and onto the run ring behind `tail`, so a park-and-wake task queues behind its
// peers. answers the new tail, so a many-task wake walks the run ring once
// (finding the tail inside made a 200-client wake quadratic). the wait_fd is
// cleared on the way in -- the run ring's whole invariant: nothing there is fd-parked.
static ai_inline union u *run_splice_at(struct ai *g, union u *tail, union u *n) {
 n[0].m = g->tasks;
 n[4].x = putcharm(-1);
 gen_wb(g, (word) n, (word) n[0].m);
 tail->m = n;
 gen_wb(g, (word) tail, (word) tail->m);
 return n; }

// is the task named by pid still live? the ring head is the running task, whose
// saved ip is stale -- me_live carries its own yield's answer. a pid with no node
// is gone, not live: a catcher must not wait on a ghost.
static ai_inline int task_live(struct ai *g, union u *head, intptr_t pid, int me_live) {
 if (getcharm(head[2].x) == pid) return me_live;
 for (union u *n = head->m; n != head; n = n->m)
  if (getcharm(n[2].x) == pid) return n[1].m->ap != lvm_task_exit;
 union u *prev;   // and the parked ring: a caught task blocked on an fd is live, and
 union u *p = parked_find(g, pid, &prev);   // a catcher told otherwise stops waiting.
 return p ? p[1].m->ap != lvm_task_exit : 0; }

// is this parked task sitting on a port already holding bytes? bytes live in the
// port, not the fd; a reader parks with Ip unadvanced, so its port is the top of
// its saved stack. the ap guard is what makes reading n[8] legal: only these two
// ops park with a port at Sp[0]; every other parker answers false first.
bool wait_buffered(struct ai*, lvm_t*, word, int);

// readiness the wait already answered: poll(2) reports every ready fd in its set.
// -> 1 ready, 0 not, -1 don't know (no block, or fd not in it). match on the
// (fd, events) pair -- two tasks can park on one fd in opposite directions. any
// nonzero revents is ready: a hung-up fd wants waking to read the end. the cursor
// is speed, not correctness.
static ai_inline int polled_ready(struct ai_wait_fd const *fds, int nfds, int *cur, int fd, int ev) {
 for (int i = 0; i < nfds; i++) {
  int j = *cur + i < nfds ? *cur + i : *cur + i - nfds;
  if (fds[j].fd == fd && fds[j].events == (short) ev)
   return *cur = j + 1 < nfds ? j + 1 : 0, fds[j].revents != 0; }
 return -1; }

// first runnable peer, run ring only: nothing here is fd-parked, so the walk
// issues no syscall. the catch clause matters -- without it a catcher is always
// runnable and the scheduler never reaches its wait (the catch park carries no
// state: Ip is unadvanced, so the saved ip is the catch, the pid its stack top).
// the wait_fd arm is a floor, not a path: a slipped invariant costs a re-park.
static union u *find_runnable(struct ai *g, union u *head, uintptr_t now, int me_live) {
 for (union u *n = head->m; n != head; n = n->m)
  if (n[1].m->ap != lvm_task_exit && (uintptr_t) getcharm(n[3].x) <= now) {
   if (n[1].m->ap == lvm_wait && task_live(g, head, getcharm(n[8].x), me_live)) continue;
   int wf = (int) getcharm(n[4].x);
   if (wf < 0 || wait_buffered(g, n[1].m->ap, n[8].x, wf)
       || ai_ready(wf, (int) getcharm(n[5].x))) return n; }
 return NULL; }

// can this parked task run again? deadline come, port holding bytes, or fd ready
// (off the filled block, else ask). `ask` is whether the kernel may be asked:
// with it clear only the syscall-free terms count -- the pass yield_sw_wait makes
// before it builds a wait.
static ai_inline int parked_ready(struct ai *g, union u *n, uintptr_t now,
                                  struct ai_wait_fd const *fds, int nfds, int *cur, int ask) {
 if (n[1].m->ap == lvm_task_exit || (uintptr_t) getcharm(n[3].x) > now) return 0;
 int wf = (int) getcharm(n[4].x), ev = (int) getcharm(n[5].x);
 if (wf < 0 || wait_buffered(g, n[1].m->ap, n[8].x, wf)) return 1;
 int pr = polled_ready(fds, nfds, cur, wf, ev);
 return pr < 0 ? (ask && ai_ready(wf, ev)) : pr; }

// the wake pass: every parked task that can run again moves to the run ring; answers
// how many. walked by count -- a waking task is unspliced under the cursor, and the
// head is as free to leave as anyone. called with g packed (every relink barriers).
static ai_noinline int wake_parked(struct ai *g, uintptr_t now,
                                   struct ai_wait_fd const *fds, int nfds, int ask) {
 if (!g->parked) return 0;
 int n = 1, cur = 0, woke = 0;
 for (union u *q = g->parked->m; q != g->parked; q = q->m) n++;
 union u *prev = g->parked, *tail = NULL;
 for (int i = 0; i < n && g->parked; i++) {
  union u *t = prev->m;
  if (!parked_ready(g, t, now, fds, nfds, &cur, ask)) { prev = t; continue; }
  if (!tail) for (tail = g->tasks; tail->m != g->tasks; tail = tail->m);   // once, on the first wake
  parked_drop(g, prev, t);
  tail = run_splice_at(g, tail, t);
  woke++; }
 return woke; }

// the fairness path's ask: one sweep of every parked fd, then the wake. the block
// rides the [hp, sp) gap (called with g packed). the block is authoritative
// here, unlike the wait's: all-zero means "none ready", never "nobody said".
static ai_noinline int poll_parked(struct ai *g, uintptr_t now) {
 int n = 1;
 for (union u *q = g->parked->m; q != g->parked; q = q->m) n++;
 struct ai_wait_fd *fds = (struct ai_wait_fd*) g->hp;
 // no gap to lay them in (the heap at its fullest, a collection pending): ask the old
 // way rather than skip the sweep, which would leave a ready peer parked.
 if (avail(g) < b2w((uintptr_t) n * sizeof *fds)) return wake_parked(g, now, NULL, 0, 1);
 int k = 0;
 union u *q = g->parked;
 do { int wf = (int) getcharm(q[4].x);
      if (wf >= 0) fds[k].fd = wf, fds[k].revents = 0, fds[k++].events = (short) getcharm(q[5].x);
      q = q->m; } while (q != g->parked);
 ai_ready_fds(fds, k);
 return wake_parked(g, now, fds, k, 0); }

// the fd set is sized by the count, never a constant (kiosko parks a task per
// client); the block rides the uncommitted heap gap, so counting first retires the
// cap by construction. called with g packed. both rings are walked: the run
// ring holds the sleepers, the parked ring the fds -- one ring's terms alone
// oversleep the other's.
static ai_noinline union u *yield_sw_wait(struct ai *g, uintptr_t my_wake, int my_wait_fd, int my_events, int me_live) {
 // the syscall-free wakes first, load-bearing: a parked task whose port already
 // holds bytes is runnable over an fd with nothing left to say -- a wait built
 // while it is parked never returns and `catch` hangs (test/host/parked.l, law 2).
 if (wake_parked(g, ai_clock(), NULL, 0, 0)) {
  union u *n = find_runnable(g, g->tasks, ai_clock(), me_live);
  if (n) return n; }
 uintptr_t min_wake = my_wake;
 int nfds = my_wait_fd >= 0;
 for (union u *n = g->tasks->m; n != g->tasks; n = n->m)
  if (n[1].m->ap != lvm_task_exit) {
   uintptr_t wa = (uintptr_t) getcharm(n[3].x);
   if (wa && (!min_wake || wa < min_wake)) min_wake = wa; }
 if (g->parked) {
  union u *q = g->parked;
  do { uintptr_t wa = (uintptr_t) getcharm(q[3].x);
       if (wa && (!min_wake || wa < min_wake)) min_wake = wa;
       if (getcharm(q[4].x) >= 0) nfds++;
       q = q->m; } while (q != g->parked); }
 if (!min_wake && !nfds) return NULL;
 uintptr_t now = ai_clock(), ticks = min_wake ? min_wake - now : 0;
 // the filled block, once the wait answers it; stays NULL unless some entry came
 // back nonzero, so a frontend that fills nothing keeps working the old way
 struct ai_wait_fd const *pol = NULL;
 int npol = 0;
 if (!min_wake || min_wake > now) {
  struct ai_wait_fd *fds = (struct ai_wait_fd*) g->hp;
  // no gap to lay them in (the heap at its fullest, a collection pending): wait
  // on the clock alone and come straight back, rather than on a set we already
  // know is short -- the one thing this rung exists to stop.
  if (avail(g) < b2w((uintptr_t) nfds * sizeof *fds)) ai_wait_fds(NULL, 0, ticks ? ticks : 1);
  else {
   int k = 0;
   // revents is zeroed here and nowhere else. the block is raw heap gap, so an
   // unwritten slot would otherwise read as whatever the last allocation left, and
   // "ready" is exactly the wrong way to guess.
   if (my_wait_fd >= 0)
    fds[k].fd = my_wait_fd, fds[k].revents = 0, fds[k++].events = (short) my_events;
   if (g->parked) {
    union u *q = g->parked;
    do { int wf = (int) getcharm(q[4].x);
         if (wf >= 0)
          fds[k].fd = wf, fds[k].revents = 0, fds[k++].events = (short) getcharm(q[5].x);
         q = q->m; } while (q != g->parked); }
   ai_wait_fds(fds, k, ticks);
   for (int i = 0; i < k; i++) if (fds[i].revents) { pol = fds, npol = k; break; } }
  now = ai_clock(); }
 if (my_wait_fd >= 0) {
  int cur = 0, pr = polled_ready(pol, npol, &cur, my_wait_fd, my_events);
  if (pr < 0 ? ai_ready(my_wait_fd, my_events) : pr) return NULL; }
 wake_parked(g, now, pol, npol, 1);   // the wait answered the whole parked ring: collect it
 return find_runnable(g, g->tasks, now, me_live); }

lvm(lvm_yield_sw) {
 // the monotask door needs both rings empty. a lone runnable task with parked peers
 // reads as a self-ring now, and the mono path waits on its own fd only -- the peers
 // would sleep through every wake they were owed.
 if (g->tasks->m == g->tasks && !g->parked) ai_musttail return Ap(lvm_yield_sw_mono, g);
 // a task on its way out is not live, and its own node cannot say so yet -- the
 // snapshot that records the exit is written at the foot of this op.
 int me_live = Ip->ap != lvm_task_exit;
 uintptr_t now = ai_clock();
 uintptr_t my_wake = g->next_wake_at;
 int my_wait_fd = g->next_wait_fd, my_events = g->next_wait_events;
 // a fairness yield never reaches yield_sw_wait, so this counter is the only thing
 // asking on its behalf whether a parked peer woke; sweeping is a syscall, so it
 // rides sweep_interval. it must fire even with a runnable peer to hand the cpu
 // to -- two compute tasks trading turns would starve every parked peer for good.
 int fair = !my_wake && my_wait_fd < 0 && Ip->ap != lvm_wait;
 if (fair && g->parked && ++g->sweep_ctr >= sweep_interval) {
  g->sweep_ctr = 0;
  Pack(g);                     // the sweep lays its fd block in the [hp, sp) gap
  poll_parked(g, now);
  Unpack(g); }                 // nothing allocated, so these come back unchanged
 union u *next = find_runnable(g, g->tasks, now, me_live);
 if (!next) {
  // a fairness yield with no runnable peer just keeps running: falling into
  // yield_sw_wait would throttle compute to the slowest sleeping peer's period.
  // a blocked task still waits below. a catcher takes this arm only over its
  // own dead body: Ip still points at the catch, so it would spin.
  if (fair) { g->yield_ctr = 0; ai_musttail return Continue(); }
  Pack(g);                     // the wait lays its fd block in the [hp, sp) gap
  next = yield_sw_wait(g, my_wake, my_wait_fd, my_events, me_live);
  Unpack(g);                   // nothing allocated, so these come back unchanged
  if (!next) {
   g->next_wake_at = 0;
   g->next_wait_fd = -1;
   g->next_wait_events = ai_wait_in;
   if (g->yield_ctr >= yield_interval) g->yield_ctr = 0;
   ai_musttail return Continue(); } }
 word my_height = topof(g) - Sp;
 union u *next_stack = next + 8,
       *end = (union u*) ttag(g, next_stack);
 uintptr_t restore_h = end - next_stack,
           need = my_height + restore_h + 9;
 if (Sp < Hp + need) {
  Pack(g);
  if (!ai_ok(g = ai_please(ai_push(g, 1, next), need))) ai_musttail return Ap(_lvm_ghelp, g);
  next = cell(pop1(g));
  Unpack(g);
  next_stack = next + 8; }   // recompute: next was forwarded by gc
 g->next_wake_at = 0;
 g->next_wait_fd = -1;
 g->next_wait_events = ai_wait_in;
 union u *prev = next;
 while (prev->m != g->tasks) prev = prev->m;
 union u *N = (union u*) Hp;
 Hp += need - restore_h;
 // the snapshot's ring is decided by its wait_fd. a task giving up its turn for an fd
 // is not runnable and must not be walked as though it were -- it leaves the run ring
 // here, which is the whole rung, and comes back through wake_parked.
 int parking = my_wait_fd >= 0;
 N[0].m = parking ? (g->parked ? g->parked->m : N) : g->tasks->m;
 N[1].m = Ip;
 N[2].x = g->tasks[2].x;
 N[3].x = putcharm((intptr_t) my_wake);
 N[4].x = putcharm(my_wait_fd);
 N[5].x = putcharm(my_events);
 N[6].x = g->tasks[6].x;          // the help the departing task heard..
 N[7].x = g->tasks[7].x;          // ..and the stdio it wears ride the snapshot
 memcpy(N + 8, Sp, my_height * sizeof(word));
 tagthread(N, 8 + my_height);
 // the run ring closes over the departing head either way: onto the snapshot when it
 // stays, or over it entirely when it parks.
 prev->m = parking ? g->tasks->m : N;
 // Pack first: ai_young reads g->hp, and the live Hp runs ahead of the last Pack --
 // against a stale g->hp the fresh node reads as old, the barrier drops the edge, and
 // the next minor eats the ring (berth+ink froze in seconds on exactly this).
 Pack(g);
 gen_wb(g, (word) prev, (word) prev->m);   // task ring: an old node now links to the fresh (young) yield snapshot
 if (parking) {
  // N already points into the parked ring (or at itself): only the ring's own link
  // in is left, and only that one is an old->young edge worth a barrier.
  if (g->parked) { g->parked->m = N; gen_wb(g, (word) g->parked, (word) g->parked->m); }
  else g->parked = N; }
 g->yield_ctr = 0;
 g->tasks = next;
 Sp = memmove(topof(g) - restore_h, next_stack, restore_h * sizeof(word));
 Ip = next[1].m;
 ai_musttail return Continue(); }

lvm(lvm_yield_nif) { Ip++; ai_musttail return Ap(lvm_yield_sw, g); }
lvm(lvm_task_exit) { ai_musttail return Ap(lvm_yield_sw, g); }
static union u const spawn_body[] = { {lvm_ap}, {.ap = lvm_task_exit} };
lvm(lvm_spawn) {
 Have(11);
 // new task node N: [next, saved_ip=spawn_body, pid, wake_at, wait_fd, wait_events, help, stdio, stack[0..1]=x,fn, tag]
 union u *N = (union u*) Hp;
 Hp += 11;
 word fn = Sp[0], x = Sp[1];
 uintptr_t pid = ++g->next_serial;   // a pid is a fresh identity: drawn from the mint stream
 N[0].m = g->tasks->m;
 N[1].m = (union u*) spawn_body;
 N[2].x = Sp[1] = putcharm(pid);
 N[3].x = zero;         // wake_at: sentinel for "always runnable"
 N[4].x = putcharm(-1);  // wait_fd: -1 = not waiting on I/O
 N[5].x = putcharm(ai_wait_in);   // wait_events: the read direction, the default
 N[6].x = g->tasks[6].x;   // inherited: a child starts under its parent's help, never without one
 N[7].x = g->tasks[7].x;   // ...and under its parent's stdio, the console until it wears its own
 N[8].x = x;
 N[9].x = fn;
 g->tasks->m = tagthread(N, 10);
 Pack(g);   // sync: ai_young reads g->hp (see lvm_yield_sw)
 gen_wb(g, (word) g->tasks, (word) g->tasks->m);   // task ring: an old node now links to the fresh (young) spawned task
 ai_musttail return Nextp(1, 1); }

lvm(lvm_wait) {
 word pid_arg = Sp[0], ret = zero;
 intptr_t target = getcharm(pid_arg);
 for (union u *node = g->tasks->m; node != g->tasks; node = node->m) {
  if (getcharm(node[2].x) != target) continue;
  if (node[1].m->ap == lvm_task_exit) {
   // dormant: dormant task's stack is just [retval] at node[8]
   ret = node[8].x;
   union u *prev = node;
   while (prev->m != node) prev = prev->m;
   prev->m = node->m;
   Pack(g);   // sync: ai_young reads g->hp (see lvm_yield_sw)
   gen_wb(g, (word) prev, (word) prev->m);   // task ring: unsplicing relinks an old node to a (maybe young) successor
   break; }
   // still running: yield without advancing Ip -- both halves of the park (the
   // re-entry on resume, and the record: Ip here says "parked in catch", Sp[0]
   // names the peer). clear both wait intentions: a stale fd would gate the park.
   g->next_wake_at = 0;
   g->next_wait_fd = -1;
  ai_musttail return Ap(lvm_yield_sw, g); }
 // and the parked ring, or catching a task merely blocked on an fd answers the
 // zero point at once; it is live, so park exactly as above.
 { union u *prev, *p = parked_find(g, target, &prev);
   if (p) { g->next_wake_at = 0; g->next_wait_fd = -1; ai_musttail return Ap(lvm_yield_sw, g); } }
 ai_musttail return Answer(ret); }

lvm(lvm_donep) {
 word pid_arg = Sp[0], result = putcharm(1);
 intptr_t target = getcharm(pid_arg);
 for (union u *node = g->tasks->m; node != g->tasks; node = node->m)
  if (getcharm(node[2].x) == target) {
   if (node[1].m->ap != lvm_task_exit) result = zero;
   Sp[0] = result, Ip += 1;
   ai_musttail return Continue(); }
 // an unfound pid reads landed, so a task merely parked on an fd would report finished --
 // a collector would drop a live session's handle mid-request.
 { union u *prev;
   if (parked_find(g, target, &prev)) result = zero; }
 Sp[0] = result;
 Ip += 1;
 ai_musttail return Continue(); }

// (scoop _) -> (pid . retval) of one finished task, or () when none have -- the
// task-side twin of `glean` (host/posix.c). presence rides the pair, never the
// net: a retval is legitimately (), so `two?` is the test and ZeroPoint the empty
// answer. only the run ring is walked (parked = blocked = unfinished); the arg is
// a dummy, so a bare (scoop) curries -- call it (scoop 0).
lvm(lvm_scoop) {
 Have(Width(struct ai_chain));
 for (union u *prev = g->tasks, *node = prev->m; node != g->tasks; prev = node, node = node->m) {
  if (node[1].m->ap != lvm_task_exit) continue;
  word pid = node[2].x, ret = node[8].x;   // dormant: the stack is just [retval] at node[8]
  struct ai_chain *p = (struct ai_chain*) Hp;
  Hp += Width(struct ai_chain);
  ini_chain(p, pid, ret);
  prev->m = node->m;
  Pack(g);   // sync: ai_young reads g->hp (see lvm_yield_sw)
  gen_wb(g, (word) prev, (word) prev->m);   // task ring: unsplicing relinks an old node to a (maybe young) successor
  Sp[0] = (word) p, Ip += 1;
  ai_musttail return Continue(); }
 Sp[0] = ZeroPoint, Ip += 1;
 ai_musttail return Continue(); }

lvm(lvm_hush) {
 word pid_arg = Sp[0], result = zero;
 intptr_t target = getcharm(pid_arg);
 union u *prev = g->tasks;
 for (union u *node = prev->m; node != g->tasks; prev = node, node = node->m)
  if (getcharm(node[2].x) == target) {
   prev->m = node->m;
   Pack(g);   // sync: ai_young reads g->hp (see lvm_yield_sw)
   gen_wb(g, (word) prev, (word) prev->m);   // unsplice relinks an old node to a (maybe young) successor
   Sp[0] = putcharm(1), Ip += 1;
   ai_musttail return Continue(); }
 // freeze reaches the parked ring too -- a task blocked on a quiet fd is exactly the one
 // a caller most wants to be able to stop.
 { union u *pp, *p = parked_find(g, target, &pp);
   if (p) { Pack(g); parked_drop(g, pp, p); result = putcharm(1); } }
 Sp[0] = result;
 Ip += 1;
 ai_musttail return Continue(); }

lvm(lvm_sleep) {
 word n = Sp[0];
 Sp[0] = zero;
 Ip += 1;
 // rest waits on the clock alone: a lingering next_wait_fd would gate the timer on
 // that fd firing (a painter slept forever on a quiet port)
 g->next_wait_fd = -1;
 if (!charmp(n) || getcharm(n) <= 0) { g->next_wake_at = 0; ai_musttail return Ap(lvm_yield_sw, g); }
 g->next_wake_at = (uintptr_t) ai_clock() + getcharm(n);
 ai_musttail return Ap(lvm_yield_sw, g); }


lvm(lvm_jump) { Ip = Ip[1].m; ai_musttail return Continue(); }
// the only compiled truthiness branch (`?`, and the `&&`/`||` macros). uses the
// language falsy predicate so an all-zero tray (boxed 0.0, zero int box,
// all-zero array) takes the false arm, lifting "0 is the only false scalar".
lvm(lvm_cond) { Ip = ai_nilp(g, *Sp++) ? Ip[1].m : Ip + 2; ai_musttail return Continue(); }
lvm(lvm_unc) {
 Have1();
 *--Sp = Ip[1].x;
 Ip = Ip[2].m;
 ai_musttail return Continue(); }

lvm(lvm_cur) {
 size_t const S = 3 + Width(struct ai_tag);
 Have(S + 2);
 union u *k = (union u*) Hp, *j = k;
 Hp += S;
 size_t n = getcharm(Ip[1].x);
 // FIXME this does not always need to be a runtime check
 if (n > 2) Hp += 2,
            j += 2,
            k[0].ap = lvm_cur,
            k[1].x = putcharm(n - 1);
 return
  j[0].ap = lvm_unc,
  j[1].x = *Sp++,
  j[2].m = Ip + 2,
  Ip = cell(*Sp),
  Sp[0] = (word) tagthread(k, j + 3 - k),
  Continue(); }

// load instructions
//
lvm(lvm_quote) {
 Have1();
 Sp -= 1;
 Sp[0] = Ip[1].x;
 Ip += 2;
 ai_musttail return Continue(); }

// a port has no function meaning either: applying it behaves as 0 (yields 1), like
// a cask (byte-identical body, kept distinct by ai_noicf -- see lvm_cask).
lvm(lvm_port_io) {
  Ip = cell(*++Sp);
  *Sp = putcharm(1);
  ai_musttail return Continue(); }

// push a value from the stack
lvm(lvm_arg) {
 Have1();
 Sp[-1] = Sp[getcharm(Ip[1].x)];
 Sp -= 1;
 Ip += 2;
 ai_musttail return Continue(); }

// fused (arg <idx> ; ap): the dominant "call a function on a local" shape, one
// dispatch saved; resume is Ip+2 (2-word op)
lvm(lvm_argap) {
 if (oddp(Sp[0])) {                                  // fixnum operator -> num-ap, resume at Ip+2
  Have1();
  Sp[-1] = Sp[getcharm(Ip[1].x)], Sp -= 1, Ip += 1;   // push local under operator; resume now Ip+2
  ai_musttail return Ap(lvm_numap, g); }
 Have1();
 Sp[-1] = Sp[getcharm(Ip[1].x)];
 Sp -= 1;
 union u *k = cell(Sp[1]); Sp[1] = word(Ip + 2), Ip = k;
 YieldCheck();
 ai_musttail return Continue(); }

// fused (quote <v> ; ap): a call with a constant arg; resume Ip+2
lvm(lvm_quoteap) {
 if (oddp(Sp[0])) {                                  // fixnum operator -> num-ap, resume at Ip+2
  Have1();
  Sp[-1] = Ip[1].x, Sp -= 1, Ip += 1;               // push const under operator; resume now Ip+2
  ai_musttail return Ap(lvm_numap, g); }
 Have1();
 Sp -= 1;
 Sp[0] = Ip[1].x;
 union u *k = cell(Sp[1]); Sp[1] = word(Ip + 2), Ip = k;
 YieldCheck();
 ai_musttail return Continue(); }

// fused (arg <idx> ; tap <fs>): the single-arg tail-call shape, e.g. a tail (loop x)
lvm(lvm_argtap) {
 if (oddp(Sp[0])) {                                  // fixnum operator -> num-ap, deliver to caller
  Have1();
  Sp[-1] = Sp[getcharm(Ip[1].x)], Sp -= 1, Ip += 1;   // push local under operator; fs operand now Ip[1]
  ai_musttail return Ap(lvm_numtap, g); }
 Have1();
 Sp[-1] = Sp[getcharm(Ip[1].x)];
 Sp -= 1;
 intptr_t x = Sp[0], j = Sp[1];
 Sp += getcharm(Ip[2].x) + 1;
 Ip = cell(j), Sp[0] = x;
 YieldCheck();
 ai_musttail return Continue(); }

// operand-specialized arg/quote: 1-word ops for the hottest indices/constants
argn(lvm_arg0, 0) argn(lvm_arg1, 1) argn(lvm_arg2, 2) argn(lvm_arg3, 3)
quon(lvm_quo0, 0) quon(lvm_quo1, 1) quon(lvm_quo2, 2) quon(lvm_quo3, 3)
quon(lvm_quom1, -1) quon(lvm_quom2, -2)

// run fusion: one op for a whole run of consecutive loads, specialized on the
// shape of the run rather than on an operand's value. the name spells the run in
// source order -- `a` an arg (its index the operand), `q` a quote (its constant
// the operand) -- and a trailing `p` says the last load carries the apply (it was
// argap/quoteap). the apply can only sit at the end: an ap hands control away and
// resumes at a fixed Ip, and a run has no dispatchable point in its middle.
//
// the operands ride in source order, so an index is written as the compiler saw
// it -- relative to the Sp of its own load. pushing left to right off a moving Sp
// makes that come out right with no arithmetic: by the time PushA(2) runs, Sp has
// already dropped past the first push, which is exactly the frame the second load
// was compiled against.
#define PushA(k) (Sp[-1] = Sp[getcharm(Ip[k].x)], Sp -= 1)
#define PushQ(k) (Sp[-1] = Ip[k].x, Sp -= 1)
// pure run, 2 loads: op + 2 operands = 3 words.
#define frun2(nom, p1, p2) lvm(nom) { Have(2); p1(1); p2(2); Ip += 3; ai_musttail return Continue(); }
// ... with the apply on the second load. the operator is what the first load
// pushed (cf. lvm_argap, which reads it at Sp[0] before its own push), so the
// fixnum test sits between the two. the numap lane bumps Ip to leave numap's
// `ret = Ip+1` landing past the whole op.
#define frun2p(nom, p1, p2) lvm(nom) { \
 Have(2); p1(1); \
 if (oddp(Sp[0])) { p2(2); Ip += 2; ai_musttail return Ap(lvm_numap, g); } \
 p2(2); \
 union u *k = cell(Sp[1]); Sp[1] = word(Ip + 3), Ip = k; \
 YieldCheck(); \
 ai_musttail return Continue(); }
frun2(lvm_aa, PushA, PushA) frun2(lvm_aq, PushA, PushQ)
frun2(lvm_qa, PushQ, PushA) frun2(lvm_qq, PushQ, PushQ)
frun2p(lvm_aap, PushA, PushA) frun2p(lvm_aqp, PushA, PushQ)
frun2p(lvm_qap, PushQ, PushA) frun2p(lvm_qqp, PushQ, PushQ)
// load + consumer fusion -- the other axis. a run's loads are only half the story:
// something eats them, and measured on the corpus that consumer is overwhelmingly an
// accessor, a predicate or a branch, not arithmetic (cup 133.7M, `?` 128.8M, cap 80.7M,
// two? 77.5M vs + at 5.5M -- 64% of every load dispatch goes into the first four).
// so these fuse `arg` with the op that consumes it: 2 words, exactly what the
// operand-specialized arg0..3 plus a 1-word op already cost, for one dispatch instead
// of two. the parameter is not named `x`: the body says Ip[1].x, and a macro parameter
// of that name would substitute into the member access.
#define fld(nom, val) lvm(nom) { Have1(); word v = Sp[getcharm(Ip[1].x)]; Sp[-1] = (val); Sp -= 1; Ip += 2; ai_musttail return Continue(); }
fld(lvm_argcap, chainp(v) ? A(v) : v)
fld(lvm_argcup, chainp(v) ? B(v) : ZeroPoint)
fld(lvm_argtwo, (chainp(v) && !nomp(v)) ? putcharm(1) : zero)
// arg + cond: the test never reaches the stack at all -- no push, no pop, one op.
// layout [Ip]=argcond [Ip+1]=idx [Ip+2]=else-addr [Ip+3]=then, matching lvm_cond's
// own targets shifted by our operand (cf. the cmp_lt note).
lvm(lvm_argcond) { Ip = ai_nilp(g, Sp[getcharm(Ip[1].x)]) ? Ip[2].m : Ip + 3; ai_musttail return Continue(); }
// ... and the rung above: load + predicate + cond, all three in one op. measured, this
// is where `?` actually lives: only 7.1M conds test a bare local, while 86.1M test the
// result of a fused load+accessor -- `(? (two? b) ..)` is the shape, 64.4M of it. the
// whole test then costs one dispatch and no stack traffic. layout [Ip]=op [Ip+1]=idx
// [Ip+2]=else, so the emit consumes the predicate's op cell and the cond's, no new word.
#define fldc(nom, test) lvm(nom) { word v = Sp[getcharm(Ip[1].x)]; \
 Ip = (test) ? Ip + 3 : Ip[2].m; ai_musttail return Continue(); }
fldc(lvm_argtwocond, chainp(v) && !nomp(v))            // two? answers a charm: no ai_nilp needed

lvm(lvm_trim) { return
 clip(g, cell(Sp[0])), Ip++, Continue(); }

lvm(lvm_seek) { return
 Sp[1] = word(cell(Sp[1]) + getcharm(Sp[0])),
 Sp++, Ip++, Continue(); }

lvm(lvm_peek) { return
 Sp[1] = (cell(Sp[1]) + getcharm(Sp[0]))->x,
 Sp++, Ip++, Continue(); }

lvm(lvm_poke) {
 union u *c = cell(Sp[2]) + getcharm(Sp[0]);
 Pack(g);                    // ai_young reads g->hp -- the live Hp may be ahead (the lvm-context law)
 gen_wb_cell(g, c, Sp[1]);   // poke's contract: the target cell sits in a tagged span (a spin
                             // thread, an env) -- never a chain's field (ev boxes those; a chain
                             // has no terminator for the remembered cell-walk).
 c->x = Sp[1]; *(Sp += 2) = word(c); ai_musttail return Next(1); }

lvm(lvm_spin) {
 size_t n = getcharm(Sp[0]);
 Have(n + Width(struct ai_tag));
 union u *k = (union u*) Hp;
 Hp += n + Width(struct ai_tag);
 Sp[0] = word(memset(tagthread(k, n), -1, n * sizeof(word)));
 ai_musttail return Next(1); }

// the net: the complex-valued measure. a complex scalar nets itself (additivity
// needs phase, so the codomain is C and the order retraction happens once, in the
// observers); every other scalar nets real; a link nets net(car) + net(cdr) and a
// rank>=1 array the sum of its elements -- recursive, unclamped -- so negatives cancel
// and opposite phases annihilate by vector cancellation. net(asum v) = net(v).
struct ai_zn ai_net(struct ai *g, word x) {
  if (charmp(x)) return zn((ai_flo_t) getcharm(x), 0);               // fixnum: its value
  if (caskp(x)) { struct ai_str *b = cask(x)->str; ai_flo_t t = 0; // hot chars: Σ charms, like a string
    for (uintptr_t i = 0; i < b->len; i++) t += (uint8_t) b->bytes[i];
    return zn(t, 0); }
  if (tabp(x)) return zn((ai_flo_t) map_len(x), 0);              // table: key count
  if (coinp(x)) {                                              // a coin nets its payload (the monoid hom), unless
    word mode = die_get(g, coin_die(x), DieNet);              // its die pins a net mode.
    if (mode == putcharm(1))                                   // mode 1: net by tally, the count -- never negative,
      return zn((ai_flo_t) ai_count(g, coin_load(x)), 0);      // so truth is "has any"
    if (mode == putcharm(2)) {                                 // mode 2: ratio -- an (n d)-of-reals payload nets
      word p = coin_load(x);                                   // n/d, the sign exact (value truth for rationals):
      if (chainp(p) && chainp(B(p))) {                         // the division's sign is IEEE-true, and the two
        struct ai_zn n = ai_net(g, A(p)), d = ai_net(g, A(B(p)));  // loss lanes below restore it from the
        if (n.im == 0 && d.im == 0 && d.re != 0) {             // components' own exact signs.
          ai_flo_t s = (n.re < 0) != (d.re < 0) ? -1.0 : 1.0;
          ai_flo_t v = n.re / d.re;
          if (v != v) v = s;                                   // inf/inf (two giant bignums): sign carries
          else if (v == 0 && n.re != 0)                        // underflow: a live sign never reads as the floor
            v = s * (ai_flo_t) (Bits == 64 ? 1e-300 : 1e-37);  // (the rescue must be nonzero at ai_flo_t's width)
          return zn(v, 0); } } }                               // a malformed payload falls through to the hom
    return ai_net(g, coin_load(x)); }
  if (!datp(x)) return zn(1, 0);                                // opaque but present (fn / port): truthy
  switch (typ(x)) {
    case DString: { ai_flo_t t = 0;                                 // a string is packed chars: Σ charms
      for (uintptr_t i = 0; i < len(x); i++) t += (uint8_t) txt(x)[i];
      return zn(t, 0); }                                           // (the count moved to tally)
    case DChain: { struct ai_zn s = zn(0, 0); word p = x;           // chain: net a + net b at every link --
      do { struct ai_zn e = ai_net(g, A(p));                       // complex sums, so negatives cancel,
           s.re += e.re, s.im += e.im;                           // phases cancel, and a chain of
           p = B(p); } while (chainp(p));                          // nothings nets to nothing
      if (!mintp(p)) { struct ai_zn e = ai_net(g, p);              // ..and the TAIL, so the hom
        s.re += e.re, s.im += e.im; }                          // holds at a dotted one. every
                                                               // mint nets 0, so `()` skips
      return s; }
    case DBig: return zn(ai_big_to_flo(x), 0);                   // bignum: full magnitude, sign intact
    case DGem: return zn(gem_get(x), 0);                         // a boxed float nets its value
    case DSun: return zn((ai_flo_t) sun_get(x), 0);             // a sun nets its value
    case DTwin: return zn(twin_re(x), twin_im(x));               // a complex nets itself (phase intact)
    case DMint: return zn(0, 0);                                 // a bare point nets nothing (the distinct nothing)
    case DNom: { ai_flo_t t = 0; struct ai_str *s = str(nom(x)->name);  // a named point nets its spelling's charms
      for (uintptr_t i = 0; i < s->len; i++) t += (uint8_t) txt(s)[i];
      return zn(t, 0); }
    case DTray: { struct ai_tray *v = tray(x);                 // a rank>=1 tray (the scalar stars are DGem/DSun/DTwin)
      uintptr_t i, n = tray_nelem(v);
      struct ai_zn s = zn(0, 0);                                  // rank>=1 array -> Σ elem
      if (v->type == ai_C) { ai_flo_t *d = tray_data(v);
        for (i = 0; i < n; i++) s.re += d[2*i], s.im += d[2*i+1];
        return s; }
      if (v->type == ai_O)
        for (i = 0; i < n; i++) { struct ai_zn e = ai_net(g, tray_get_obj(v, i));
          s.re += e.re, s.im += e.im; }
      else for (i = 0; i < n; i++) s.re += tray_get_flo(v, i);
      return s; } }
  return zn(1, 0); }
// $: the net observed once -- max(0, ceil) of its real part
static intptr_t ai_saturate(struct ai *g, word x) {
  // the charm lane is exactness, not speed: the net is a double, so above 2^53 a
  // charm comes back rounded -- and $ is the identity on every green charm (spec.l).
  if (charmp(x)) { intptr_t n = getcharm(x); return n <= 0 ? 0 : n; }
  ai_flo_t re = ai_net(g, x).re;
  if (re <= 0) return 0;
  if (re >= (ai_flo_t) maxcharm) return maxcharm;
  intptr_t i = (intptr_t) re;
  return i + (re > (ai_flo_t) i ? 1 : 0); }
lvm(lvm_saturate) {
 if (ai_ratio_exact(g, Sp[0])) { Pack(g); g = ai_ratio_rung(g, 2);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 Sp[0] = putcharm(ai_saturate(g, Sp[0])); Ip += 1; ai_musttail return Continue(); }
// the tower's third rung: ceil(re(net x)) -- the measure retracted onto the integers, where
// saturate is this one with its floor raised to 0 and bit is it with the ceiling lowered to 1.
// it saturates at the charm bounds like every rung below it: a charm is the codomain, so a
// measure that will not fit lands on the edge rather than wrapping or widening.
static intptr_t ai_ceilnet(struct ai *g, word x) {
  if (charmp(x)) return getcharm(x);
  ai_flo_t re = ai_net(g, x).re;
  if (re >= (ai_flo_t) maxcharm) return maxcharm;
  if (re <= (ai_flo_t) mincharm) return mincharm;
  intptr_t i = (intptr_t) re;
  return i + (re > (ai_flo_t) i ? 1 : 0); }
lvm(lvm_ceil) {
 if (ai_ratio_exact(g, Sp[0])) { Pack(g); g = ai_ratio_rung(g, 1);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 Sp[0] = putcharm(ai_ceilnet(g, Sp[0])); Ip += 1; ai_musttail return Continue(); }

