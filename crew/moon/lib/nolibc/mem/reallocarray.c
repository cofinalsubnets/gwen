#include "../impl.h"

/* n*sz with the multiply CHECKED -- the whole reason the call exists, and a
 * plain realloc(p, n*sz) would be the overflow it was invented to stop */
void *reallocarray(void *p, size_t n, size_t sz) {
  if (sz && n > (size_t) -1 / sz) { __errno_v = ENOMEM; return 0; }
  return realloc(p, n * sz); }
