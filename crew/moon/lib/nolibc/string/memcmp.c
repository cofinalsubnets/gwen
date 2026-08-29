#include "../impl.h"

/* the first difference, a word at a time: equal words hold no differing byte, so
 * only the word that differs falls to the byte loop, which then runs at most
 * sizeof(long) more times. n bounds every load, so the word lane needs no
 * alignment for safety -- it takes it for speed, and only where BOTH pointers
 * offer it, as memcpy does.
 * ⚠ THE WORD COMPARE SAYS WHETHER, NEVER WHICH WAY. a word holds its bytes in
 * the machine's order, so on a little-endian seat the wide compare orders them
 * backwards; the answer has to come from the byte loop that follows.
 * ⚠ the stride is sizeof(long), never a literal 8: a 32-bit seat (the thumb
 * boards) would else step eight bytes having compared four. */
int memcmp(void const *a, void const *b, size_t n) {
  unsigned char const *x = a, *y = b;
  if ((((unsigned long) x | (unsigned long) y) & (sizeof(unsigned long) - 1)) == 0) {
    unsigned long const *xw = (unsigned long const *) x, *yw = (unsigned long const *) y;
    while (n >= sizeof(unsigned long) && *xw == *yw) {
      xw++; yw++; n -= sizeof(unsigned long); }
    x = (unsigned char const *) xw; y = (unsigned char const *) yw; }
  while (n--) { if (*x != *y) return (int) *x - (int) *y; x++; y++; }
  return 0; }
