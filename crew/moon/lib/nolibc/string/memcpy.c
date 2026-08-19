#include "../impl.h"

/* ---- memory/string: word-wide where the pointers agree (the GC image and
 * string lanes move real volume through these). ⚠ the stride is sizeof(long),
 * never a literal 8: a 32-bit seat (the thumb boards) would else copy four
 * bytes and step eight, leaving every other word untouched. ---- */
void *memcpy(void *d, void const *s, size_t n) {
  unsigned char *dp = d; unsigned char const *sp = s;
  size_t const w = sizeof(unsigned long);
  if ((((unsigned long) dp | (unsigned long) sp) & (w - 1)) == 0)
    while (n >= w) { *(unsigned long *) dp = *(unsigned long const *) sp; dp += w; sp += w; n -= w; }
  while (n--) *dp++ = *sp++;
  return d; }
