#include "../impl.h"

/* ⚠ the stride and the fill both ride sizeof(long): on a 32-bit seat a literal
 * `w <<= 32' is undefined and a literal 8 steps twice what it wrote. */
void *memset(void *d, int c, size_t n) {
  unsigned char *dp = d;
  unsigned char b = (unsigned char) c;
  size_t const w = sizeof(unsigned long);
  unsigned long v = b;
  for (size_t k = 8; k < w * 8; k <<= 1) v |= v << k;
  if (((unsigned long) dp & (w - 1)) == 0)
    while (n >= w) { *(unsigned long *) dp = v; dp += w; n -= w; }
  while (n--) *dp++ = b;
  return d; }
