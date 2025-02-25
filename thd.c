#include "i.h"
PCell *trim_thread(PCell *k) { return ttag(k)->head = k; }
Thread *mo_ini(Thread* _, uintptr_t len) {
  struct tag *t = (struct tag*) (_ + len);
  return t->null = NULL, t->head = _; }

Vm(trim) {
  PCell *k = (PCell*) Sp[0];
  return op(1, Z(trim_thread(k))); }

Vm(seek) {
  PCell *k = (PCell*) Sp[1];
  return op(2, (PWord) (k + getnum(Sp[0]))); }

Vm(peek) {
  PCell *k = (PCell*) Sp[0];
  return op(1, k[0].x); }

Vm(poke) {
  PCell *k = (PCell*) Sp[1];
  k->x = Sp[0];
  return op(2, (PWord) k); }

Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct tag));
  PCell *k = mo_ini((PCell*) Hp, n);
  memset(k, -1, n * sizeof(PWord));
  Hp += n + Width(struct tag);
  return op(1, (PWord) k); }

struct tag *ttag(PCell *k) {
  while (k->x) k++;
  return (struct tag*) k; }
