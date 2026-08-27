#include "../impl.h"

/* ---- memory/string: word-wide where the pointers agree (the GC image and
 * string lanes move real volume through these). ⚠ the stride is sizeof(long),
 * never a literal 8: a 32-bit seat (the thumb boards) would else copy four
 * bytes and step eight, leaving every other word untouched.
 * FOUR words an iteration: the length compare, the two pointer bumps and the
 * back edge are the same whether one word moves or four, so they amortize over
 * 32 bytes instead of 8. the single-word loop drains what the quad leaves. ---- */
void *memcpy(void *d, void const *s, size_t n) {
  unsigned char *dp = d; unsigned char const *sp = s;
  if ((((unsigned long) dp | (unsigned long) sp) & (sizeof(unsigned long) - 1)) == 0) {
    unsigned long *dw = (unsigned long *) dp;
    unsigned long const *sw = (unsigned long const *) sp;
    while (n >= 4 * sizeof(unsigned long)) {
      dw[0] = sw[0]; dw[1] = sw[1]; dw[2] = sw[2]; dw[3] = sw[3];
      dw += 4; sw += 4; n -= 4 * sizeof(unsigned long); }
    while (n >= sizeof(unsigned long)) { *dw++ = *sw++; n -= sizeof(unsigned long); }
    dp = (unsigned char *) dw; sp = (unsigned char const *) sw; }
  while (n--) *dp++ = *sp++;
  return d; }
