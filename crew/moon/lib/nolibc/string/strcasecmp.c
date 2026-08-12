#include "../impl.h"

int strcasecmp(char const *a, char const *b) {
  while (*a && tolower((unsigned char) *a) == tolower((unsigned char) *b)) { a++; b++; }
  return tolower((unsigned char) *a) - tolower((unsigned char) *b); }
