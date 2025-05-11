#include "i.h"
Cell *trim_thread(Cell *k) { return ttag(k)->head = k; }
static thread *mo_ini(thread *_, size_t len) {
  struct tag *t = (struct tag*) (_ + len);
  return t->null = NULL, t->head = _; }

// allocate a thread
Cell *mo_n(Core *f, size_t n) {
  Cell *k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

Vm(trim) {
  Cell *k = (Cell*) Sp[0];
  return op(1, Z(trim_thread(k))); }

Vm(seek) {
  Cell *k = (Cell*) Sp[1];
  return op(2, (Word) (k + getnum(Sp[0]))); }

Vm(peek) {
  Cell *k = (Cell*) Sp[0];
  return op(1, k[0].x); }

Vm(poke) {
  Cell *k = (Cell*) Sp[1];
  k->x = Sp[0];
  return op(2, (Word) k); }

Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct tag));
  Cell *k = mo_ini((Cell*) Hp, n);
  memset(k, -1, n * sizeof(Word));
  Hp += n + Width(struct tag);
  return op(1, (Word) k); }

struct tag *ttag(Cell *k) {
  while (k->x) k++;
  return (struct tag*) k; }
