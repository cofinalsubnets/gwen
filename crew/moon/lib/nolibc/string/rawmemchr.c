#include "../impl.h"

void *rawmemchr(void const *p, int c) {
  unsigned char const *q = p;
  while (*q != (unsigned char) c) q++;
  return (void *) q; }
