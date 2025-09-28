#include "i.h"
static uintptr_t xx_two(g_core*, g_word);
static void wk_two(g_core*, g_word, g_word*, g_word*);
static g_core * em_two(g_core*, FILE*, g_word);
static bool eq_two(g_core*, g_word, g_word);

// FIXME could overflow the stack -- use off pool for this
static bool eq_two(g_core *f, g_word x, g_word y) {
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static g_word cp_two(g_core *v, g_word x, g_word *p0, g_word *t0) {
  g_pair *src = (g_pair*) x,
         *dst = bump(v, Width(g_pair));
  ini_pair(dst, src->a, src->b);
  return word(src->ap = (g_vm*) dst); }

static void wk_two(g_core *f, g_word x, g_word *p0, g_word *t0) {
  f->cp += Width(g_pair);
  A(x) = cp(f, A(x), p0, t0);
  B(x) = cp(f, B(x), p0, t0); }

g_type two_type = {
  .xx = xx_two,
  .cp = cp_two,
  .wk = wk_two,
  .eq = eq_two,
  .em = em_two,
  .ap = self, };
static uintptr_t xx_two(g_core *f, g_word x) {
  uintptr_t hc = hash(f, A(x)) * hash(f, B(x));
  return hc ^ mix; }

static g_core *em_two(g_core *f, FILE *o, g_word x) {
  if (A(x) == word(f->quote) && twop(B(x)))
    putc('\'', o),
    transmit(f, o, AB(x));
  else for (putc('(', o);; putc(' ', o)) {
    transmit(f, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } }
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
