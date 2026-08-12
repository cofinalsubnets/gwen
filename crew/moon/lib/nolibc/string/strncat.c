#include "../impl.h"

char *strncat(char *d, char const *s, size_t n) {
  char *p = d + strlen(d);
  while (n-- && *s) *p++ = *s++;
  *p = 0;
  return d; }
