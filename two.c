#include "i.h"

static uintptr_t xx_two(core*, word);
static void wk_two(core*, word, word*, word*), em_two(core*, FILE*, word);
static bool eq_two(core *f, word x, word y);

// FIXME could overflow the stack -- use off pool for this
static bool eq_two(core *f, word x, word y) {
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static word cp_two(core *v, word x, word *p0, word *t0) {
  pair *src = (pair*) x,
       *dst = bump(v, Width(pair));
  ini_pair(dst, src->a, src->b);
  return W(src->ap = (vm*) dst); }

static void wk_two(core *f, word x, word *p0, word *t0) {
  f->cp += Width(pair);
  A(x) = cp(f, A(x), p0, t0);
  B(x) = cp(f, B(x), p0, t0); }

type
  two_type = { .xx = xx_two, .cp = cp_two, .wk = wk_two, .eq = eq_two, .em = em_two, };
static uintptr_t xx_two(core *f, word x) {
  word hc = hash(f, A(x)) * hash(f, B(x));
  return hc ^ mix; }

static void em_two(core *f, FILE *o, word x) {
  if (A(x) == W(f->quote) && twop(B(x)))
    putc('\'', o),
    transmit(f, o, AB(x));
  else for (putc('(', o);; putc(' ', o)) {
    transmit(f, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

Vm(car) {
  Sp[0] = twop(Sp[0]) ? A(Sp[0]) : Sp[0];
  Ip++;
  return Continue(); }
Vm(cdr) {
  Sp[0] = twop(Sp[0]) ? B(Sp[0]) : nil;
  Ip++;
  return Continue(); }

Vm(cons) {
  Have(Width(pair));
  pair *w = (pair*) Hp;
  ini_pair(w, Sp[0], Sp[1]);
  Hp += Width(pair);
  Sp[1] = W(w);
  Sp++;
  Ip++;
  return Continue(); }

Vm(pairp) {
  Sp[0] = twop(Sp[0]) ? putnum(-1) : nil;
  Ip++;
  return Continue(); }

static g_core *g_cons_stack(g_core *f, int i, int j) {
  f = g_have(f, Width(pair));
  if (g_ok(f)) {
    pair *p = (pair*) f->hp;
    ini_pair(p, f->sp[i], f->sp[j]);
    f->hp += Width(pair);
    *++f->sp = (word) p; }
  return f; }

g_core *g_cons_l(g_core *f) { return g_cons_stack(f, 0, 1); }
g_core *g_cons_r(g_core *f) { return g_cons_stack(f, 1, 0); }
