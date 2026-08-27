#include "../impl.h"

/* ⚠ the stride and the fill both ride sizeof(long): on a 32-bit seat a literal
 * `w <<= 32' is undefined and a literal 8 steps twice what it wrote.
 * four words an iteration, like memcpy: the loop's own cost is the same
 * whether it lays one word or four. */
void *memset(void *d, int c, size_t n) {
  unsigned char *dp = d;
  unsigned char b = (unsigned char) c;
  unsigned long v = b;
  for (size_t k = 8; k < sizeof(unsigned long) * 8; k <<= 1) v |= v << k;
  if (((unsigned long) dp & (sizeof(unsigned long) - 1)) == 0) {
    unsigned long *dw = (unsigned long *) dp;
    while (n >= 4 * sizeof(unsigned long)) {
      dw[0] = v; dw[1] = v; dw[2] = v; dw[3] = v;
      dw += 4; n -= 4 * sizeof(unsigned long); }
    while (n >= sizeof(unsigned long)) { *dw++ = v; n -= sizeof(unsigned long); }
    dp = (unsigned char *) dw; }
  while (n--) *dp++ = b;
  return d; }
