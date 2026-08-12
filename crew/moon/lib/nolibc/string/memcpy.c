#include "../impl.h"

/* ---- memory/string: word-wide where the pointers agree (the GC image and
 * string lanes move real volume through these) ---- */
void *memcpy(void *d, void const *s, size_t n) {
  unsigned char *dp = d; unsigned char const *sp = s;
  if ((((unsigned long) dp | (unsigned long) sp) & 7) == 0)
    while (n >= 8) { *(unsigned long *) dp = *(unsigned long const *) sp; dp += 8; sp += 8; n -= 8; }
  while (n--) *dp++ = *sp++;
  return d; }
