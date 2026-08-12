#include "../impl.h"

void *calloc(size_t n, size_t sz) {
  size_t t = n * sz;
  void *p = malloc(t);
  if (p) memset(p, 0, t);
  return p; }
