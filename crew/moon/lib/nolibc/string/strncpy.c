#include "../impl.h"

char *strncpy(char *d, char const *s, size_t n) {
  char *r = d;
  while (n && *s) { *d++ = *s++; n--; }
  while (n) { *d++ = 0; n--; }
  return r; }
