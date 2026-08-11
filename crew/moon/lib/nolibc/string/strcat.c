#include "../impl.h"

char *strcat(char *d, char const *s) {
  char *r = d;
  while (*d) d++;
  while ((*d++ = *s++)) ;
  return r; }
