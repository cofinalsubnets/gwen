#include "../impl.h"

void *memchr(void const *p, int c, size_t n) {
  unsigned char const *s = p;
  for (; n; n--, s++) if (*s == (unsigned char) c) return (void *) s;
  return 0; }
