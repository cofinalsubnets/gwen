#include "i.h"

static uintptr_t xx_two(core*, word);
static void wk_two(core*, word, word*, word*), em_two(core*, FILE*, word);
static bool eq_two(core *f, word x, word y);
// FIXME could overflow the stack -- use off pool for this
static bool eq_two(core *f, word x, word y) {
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }
static word cp_two(core *v, word x, word *p0, word *t0) {
  pair *src = (pair*) x,
       *dst = ini_pair(bump(v, Width(pair)), src->a, src->b);
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

pair *pairof(core *f, word a, word b) {
  f = pushc(f, 2, a, b);
  f = g_cons_stack(f, 0, 1);
  return g_ok(f) ? (pair*) pop1(f) : 0; }

Vm(car) { return Sp[0] = twop(Sp[0]) ? A(Sp[0]) : Sp[0], Ip += 1, Continue(); }
Vm(cdr) { return Sp[0] = twop(Sp[0]) ? B(Sp[0]) : nil, Ip += 1, Continue(); }
Vm(cons) {
  Have(Width(pair));
  pair *w = ini_pair((pair*) Hp, Sp[0], Sp[1]);
  return Hp += Width(pair), Sp[1] = W(w), Sp += 1, Ip += 1, Continue(); }

Vm(pairp) { return Sp[0] = twop(Sp[0]) ? putnum(-1) : nil, Ip += 1, Continue(); }

g_core *g_cons_stack(g_core *f, int i, int j) {
  if (!g_ok(f)) return f;
  if (avail(f) < Width(pair)) f = please(f, Width(pair));
  if (g_ok(f)) {
    pair *p = ini_pair((pair*) f->hp, f->sp[i], f->sp[j]);
    f->hp += Width(pair);
    *++f->sp = (word) p; }
  return f; }

g_core *g_cons_c(g_core *f, g_word a, g_word b) {
  if (!g_ok(f)) return f;
  if (avail(f) < Width(pair) + 1) f = please(f, Width(pair) + 1);
  if (g_ok(f)) {
    pair *p = ini_pair((pair*) f->hp, a, b);
    f->hp += Width(pair);
    *--f->sp = (word) p; }
  return f; }

pair *ini_pair(pair *w, word a, word b) {
  return w->ap = data, w->typ = &two_type, w->a = a, w->b = b, w; }

