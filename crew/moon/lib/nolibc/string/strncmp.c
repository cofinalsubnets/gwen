#include "../impl.h"

int strncmp(char const *a, char const *b, size_t n) {
  while (n && *a && *a == *b) { a++; b++; n--; }
  return n ? (int) (unsigned char) *a - (int) (unsigned char) *b : 0; }
