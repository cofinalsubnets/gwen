#include "../impl.h"

int atoi(char const *s) {
  int sign = 1, v = 0;
  while (*s == ' ' || *s == 9) s++;
  if (*s == '-') { sign = -1; s++; } else if (*s == '+') s++;
  while (*s >= '0' && *s <= '9') { v = v * 10 + (*s - '0'); s++; }
  return sign * v; }
