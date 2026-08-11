#include "../impl.h"

static __ablk *__ahead;
void *alloca(size_t n) {
  char here;
  while (__ahead && __ahead->mark < &here) { __ablk *d = __ahead; __ahead = d->next; free(d); }
  if (n == 0) return 0;                         /* alloca(0): reclaim only */
  __ablk *b = malloc(sizeof(__ablk) + n);
  if (!b) return 0;
  b->mark = &here;
  b->next = __ahead;
  __ahead = b;
  return (void *) (b + 1); }
