#include "../impl.h"

void *memset(void *d, int c, size_t n) {
  unsigned char *dp = d;
  unsigned char b = (unsigned char) c;
  unsigned long w = b; w |= w << 8; w |= w << 16; w |= w << 32;
  if (((unsigned long) dp & 7) == 0)
    while (n >= 8) { *(unsigned long *) dp = w; dp += 8; n -= 8; }
  while (n--) *dp++ = b;
  return d; }
