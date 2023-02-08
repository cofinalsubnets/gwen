#include "i.h"

// allocate a thread
mo mo_n(la v, U n) {
  mo k = cells(v, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

// push things onto the stack
static NoInline bool pushsr(la v, size_t i, va_list xs) {
  bool _; ob x = va_arg(xs, ob);
  return !x ? Avail >= i || please(v, i) :
    (with(x, _ = pushsr(v, i+1, xs)),
     _ && (*--v->sp = x, true)); }

NoInline bool pushs(la v, ...) {
  bool _; va_list xs; return
    va_start(xs, v),
    _ = pushsr(v, 0, xs),
    va_end(xs),
    _; }

static NoInline mo thdr(li v, size_t n, va_list xs) {
  vm *x = va_arg(xs, vm*);
  if (!x) return mo_n(v, n);
  mo k; with(x, k = thdr(v, n + 1, xs));
  if (k) k[n].ap = x;
  return k; }

NoInline mo thd(li v, ...) {
  mo k; va_list xs; return
    va_start(xs, v),
    k = thdr(v, 0, xs),
    va_end(xs),
    k; }

static NoInline two pair_gc(la v, ob a, ob b) {
  bool ok;
  with(a, with(b, ok = please(v, Width(struct two))));
  return ok ? pair(v, a, b) : 0; }

NoInline two pair(la v, ob a, ob b) {
  if (Avail < Width(struct two)) return pair_gc(v, a, b);
  return two_ini(bump(v, Width(struct two)), a, b); }