#include "../impl.h"

void __semit(void *ctx, int c) {
  struct __sctx *s = ctx;
  if (s->at + 1 < s->n) s->p[s->at] = (char) c;
  s->at++; }
