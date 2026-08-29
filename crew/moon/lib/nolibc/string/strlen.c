#include "../impl.h"

/* the NUL, a word at a time: (w - ones) & ~w & highs is nonzero exactly when
 * some byte of w is zero.
 * ⚠ THE ALIGN LOOP IS NOT AN OPTIMISATION. strlen carries no length to stop it,
 * so an unaligned word load can reach past the NUL and into a page that is not
 * ours -- the classic SWAR strlen fault, which segfaults rather than answering
 * wrong. an ALIGNED load cannot: the word lies wholly inside one page, and that
 * page is the one holding the NUL.
 * ⚠ the stride is sizeof(long), never a literal 8: a 32-bit seat (the thumb
 * boards) would else step eight bytes having tested four. */
size_t strlen(char const *s) {
  char const *p = s;
  while ((unsigned long) p & (sizeof(unsigned long) - 1)) {
    if (!*p) return (size_t) (p - s);
    p++; }
  for (;;) {
    unsigned long w = *(unsigned long const *) p;
    if ((w - AiOnes) & ~w & AiHighs) break;
    p += sizeof(unsigned long); }
  while (*p) p++;
  return (size_t) (p - s); }
