#include "../impl.h"

/* the needle, a word at a time: xor'ing the text with the needle in every lane
 * turns "is this byte the needle" into "is this byte zero", which is the same
 * (w - ones) & ~w & highs test strlen uses. a word that answers yes falls to the
 * byte loop, which then runs at most sizeof(long) more times.
 * n bounds every load here, so the alignment step is for speed and not for
 * safety -- strlen.c is where it is load-bearing. */
void *memchr(void const *p, int c, size_t n) {
  unsigned char const *s = p;
  unsigned char ch = (unsigned char) c;
  unsigned long k = AiOnes * ch;
  while (n && ((unsigned long) s & (sizeof(unsigned long) - 1))) {
    if (*s == ch) return (void *) s;
    s++; n--; }
  while (n >= sizeof(unsigned long)) {
    unsigned long w = *(unsigned long const *) s ^ k;
    if ((w - AiOnes) & ~w & AiHighs) break;
    s += sizeof(unsigned long); n -= sizeof(unsigned long); }
  for (; n; n--, s++) if (*s == ch) return (void *) s;
  return 0; }
