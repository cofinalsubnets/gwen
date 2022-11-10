#include "la.h"
#include "two.h"
#include "gc.h"
#include "alloc.h"

// pairs and lists
static NoInline two pair_gc(la v, ob a, ob b) {
  bool ok;
  with(a, with(b, ok = please(v, wsizeof(struct two))));
  return ok ? pair(v, a, b) : 0; }

two pair(la v, ob a, ob b) {
  return Avail >= wsizeof(struct two) ?
    ini_two(bump(v, wsizeof(struct two)), a, b) :
    pair_gc(v, a, b); }

// index of item in list (-1 if absent)
intptr_t lidx(ob l, ob x) {
  for (intptr_t i = 0; twop(l); l = B(l), i++)
    if (x == A(l)) return i;
  return -1; }

// length of list
size_t llen(ob l) {
  size_t i = 0;
  while (twop(l)) l = B(l), i++;
  return i; }

#include "vm.h"
Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have(wsizeof(struct two));
  xp = (ob) ini_two(hp, xp, *sp++);
  hp += wsizeof(struct two);
  return ApN(1, xp); }

Vm(car_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(twop(xp));
  return ApC(ret, A(xp)); }

Vm(cdr_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(twop(xp));
  return ApC(ret, B(xp)); }

Vm(cons_f) {
  ArityCheck(2);
  Have(wsizeof(struct two));
  xp = (ob) ini_two(hp, fp->argv[0], fp->argv[1]);
  hp += wsizeof(struct two);
  return ApC(ret, xp); }

static Vm(ap_two) { return
  ApC(ret, fp->argc ? B(ip) : A(ip)); }

static Gc(cp_two) {
  two src = (two) x,
      dst = bump(v, wsizeof(struct two));
  src->head.disp = (vm*) dst;
  return (ob) ini_two(dst,
    cp(v, src->a, pool0, top0),
    cp(v, src->b, pool0, top0)); }

#include "tx.h"
static long tx_two(la v, FILE *o, ob x) {
  long r = 2;
  if (fputc('(', o) == EOF) return -1;
  for (;;) {
    long i = la_tx(v, o, A(x));
    if (i < 0) return i;
    else r += i;
    if (!twop(x = B(x))) break;
    else if (fputc(' ', o) == EOF) return -1;
    else r++; }
  if (fputc(')', o) == EOF) return -1;
  return r; }

#include "hash.h"
static intptr_t hx_two(la v, ob x) {
  intptr_t hc = hash(v, A(x)) * hash(v, B(x));
  return ror(hc, 4 * sizeof(intptr_t)); }

#include "cmp.h"
static bool eq_two(la v, ob x, ob y) {
  return twop(y) &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }

const struct mtbl mtbl_two = {
  .does = ap_two,
  .emit = tx_two,
  .evac = cp_two,
  .hash = hx_two,
  .equi = eq_two, };
