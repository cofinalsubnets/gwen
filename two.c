#include "i.h"

PPair *pairof(PCore *f, PWord a, PWord b) {
  if (avail(f) < Width(PPair)) {
    bool ok;
    avec(f, a, avec(f, b, ok = p_please(f, Width(PPair))));
    if (!ok) return 0; }
  PPair *w = (PPair*) f->hp;
  f->hp += Width(PPair);
  return ini_pair(w, a, b); }

Vm(car) { return op(1, twop(Sp[0]) ? A(Sp[0]) : Sp[0]); }
Vm(cdr) { return op(1, twop(Sp[0]) ? B(Sp[0]) : nil); }
Vm(cons) {
  Have(Width(PPair));
  PPair *w = ini_pair((PPair*) Hp, Sp[0], Sp[1]);
  Hp += Width(PPair);
  return op(2, (PWord) w); }

static PWord cp_two(PCore *v, PWord x, PWord *p0, PWord *t0) {
  PPair *src = (PPair*) x,
           *dst = ini_pair(bump(v, Width(PPair)), src->a, src->b);
  return (PWord) (src->ap = (PVm*) dst); }

static void wk_two(PCore *f, PWord x, PWord *p0, PWord *t0, PHeap *cptr) {
  *cptr += Width(PPair);
  A(x) = cp(f, A(x), p0, t0);
  B(x) = cp(f, B(x), p0, t0); }

static void print_two(PCore *f, PFile *o, PWord x) {
  for (putc('(', o);; putc(' ', o)) {
    transmit(f, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

// FIXME could overflow the stack -- use off pool for this
static bool eq_two(PCore *f, PWord x, PWord y) {
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static PWord hash_two(PCore *f, PWord x) {
  PWord hc = hash(f, A(x)) * hash(f, B(x));
  return hc ^ mix; }

PType pair_type = {
  .hash = hash_two,
  .copy = cp_two,
  .evac = wk_two,
  .emit = print_two,
  .equal = eq_two,
};
