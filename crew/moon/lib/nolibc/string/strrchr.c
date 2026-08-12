#include "../impl.h"

char *strrchr(char const *s, int c) {
  char const *last = 0;
  do { if (*s == (char) c) last = s; } while (*s++);
  return (char *) last; }
