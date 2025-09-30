#include "i.h"
#include <string.h>

Vm(seek) {
  Sp[1] = word(((g_cell*) Sp[1]) + getnum(Sp[0]));
  Sp += 1;
  Ip += 1;
  return Continue(); }

Vm(peek) {
  Sp[0] = cell(Sp[0])->x;
  Ip += 1;
  return Continue(); }

Vm(poke) {
  cell(Sp[1])->x = Sp[0];
  Sp += 1;
  Ip += 1;
  return Continue(); }

Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct g_tag));
  g_cell *k = cell(Hp);
  struct g_tag *t = (struct g_tag*) (k + n);
  Hp += n + Width(struct g_tag);
  t->null = NULL;
  t->head = k;
  memset(k, -1, n * sizeof(g_word));
  Sp[0] = (g_word) k;
  Ip += 1;
  return Continue(); }

Vm(trim) {
  g_cell *k = (g_cell*) Sp[0];
  ttag(k)->head = k;
  Ip += 1;
  return Continue(); }

