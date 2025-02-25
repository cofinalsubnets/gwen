#include "i.h"

Pair *pairof(Core *f, Word a, Word b) {
  if (avail(f) < Width(Pair)) {
    bool ok;
    avec(f, a, avec(f, b, ok = p_please(f, Width(Pair))));
    if (!ok) return 0; }
  Pair *w = (Pair*) f->hp;
  f->hp += Width(Pair);
  return ini_pair(w, a, b); }

Vm(car) { return op(1, twop(Sp[0]) ? A(Sp[0]) : Sp[0]); }
Vm(cdr) { return op(1, twop(Sp[0]) ? B(Sp[0]) : nil); }
Vm(cons) {
  Have(Width(Pair));
  Pair *w = ini_pair((Pair*) Hp, Sp[0], Sp[1]);
  Hp += Width(Pair);
  return op(2, (Word) w); }

static Word cp_two(Core *v, Word x, Word *p0, Word *t0) {
  Pair *src = (Pair*) x,
       *dst = ini_pair(bump(v, Width(Pair)), src->a, src->b);
  return (Word) (src->ap = (PVm*) dst); }

static void wk_two(Core *f, Word x, Word *p0, Word *t0) {
  f->cp += Width(Pair);
  A(x) = cp(f, A(x), p0, t0);
  B(x) = cp(f, B(x), p0, t0); }

static void print_two(Core *f, PFile *o, Word x) {
  for (putc('(', o);; putc(' ', o)) {
    transmit(f, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

// FIXME could overflow the stack -- use off pool for this
static bool eq_two(Core *f, Word x, Word y) {
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static PWord hash_two(PCore *f, PWord x) {
  PWord hc = hash(f, A(x)) * hash(f, B(x));
  return hc ^ mix; }

Vm(pairp) { return op(1, twop(Sp[0]) ? putnum(-1) : nil); }
Pair *ini_pair(Pair *w, Word a, Word b) {
  return w->ap = data,
         w->typ = &pair_type,
         w->a = a,
         w->b = b,
         w; }

PType pair_type = {
  .hash = hash_two,
  .copy = cp_two,
  .evac = wk_two,
  .emit = print_two,
  .equal = eq_two,
};
