#include "i.h"

Vm(seek) { return Sp[1] = W(((cell*) Sp[1]) + getnum(Sp[0])), Sp += 1, Ip += 1, Continue(); }
Vm(peek) { return Sp[0] = R(Sp[0])->x, Ip += 1, Continue(); }
Vm(poke) { return R(Sp[1])->x = Sp[0], Sp += 1, Ip += 1, Continue(); }
Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct tag));
  cell *k = R(Hp);
  struct tag *t = (struct tag*) (k + n);
  return Hp += n + Width(struct tag),
         t->null = NULL,
         t->head = k,
         memset(k, -1, n * sizeof(word)),
         Sp[0] = (word) k,
         Ip += 1,
         Continue(); }

struct tag *ttag(cell *k) {
  while (k->x) k++;
  return (struct tag*) k; }

Vm(trim) {
  cell *k = (cell*) Sp[0];
  ttag(k)->head = k;
  return Ip += 1, Continue(); }
