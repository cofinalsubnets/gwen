#include "../impl.h"

int sigaddset(sigset_t *s, int n) {
  s->__v[(n - 1) / 64] |= 1UL << ((unsigned) (n - 1) % 64);
  return 0; }
