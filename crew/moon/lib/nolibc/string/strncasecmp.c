#include "../impl.h"

int strncasecmp(char const *a, char const *b, size_t n) {
  while (n && *a && tolower((unsigned char) *a) == tolower((unsigned char) *b)) { a++; b++; n--; }
  return n ? tolower((unsigned char) *a) - tolower((unsigned char) *b) : 0; }
