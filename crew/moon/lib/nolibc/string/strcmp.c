#include "../impl.h"

int strcmp(char const *a, char const *b) {
  while (*a && *a == *b) { a++; b++; }
  return (int) (unsigned char) *a - (int) (unsigned char) *b; }
