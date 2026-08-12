#include "../impl.h"

int memcmp(void const *a, void const *b, size_t n) {
  unsigned char const *x = a, *y = b;
  while (n--) { if (*x != *y) return (int) *x - (int) *y; x++; y++; }
  return 0; }
