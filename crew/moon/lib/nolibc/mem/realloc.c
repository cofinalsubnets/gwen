#include "../impl.h"

void *realloc(void *p, size_t n) {
  if (!p) return malloc(n);
  if (n == 0) { free(p); return 0; }
  size_t old = (((__mhdr *) p - 1)->size - 1) * sizeof(__mhdr);   /* payload bytes */
  if (old >= n) return p;
  void *q = malloc(n);
  if (!q) return 0;
  memcpy(q, p, old);
  free(p);
  return q; }
