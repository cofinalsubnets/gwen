#include "../impl.h"

size_t strspn(char const *s, char const *set) {
  size_t n = 0;
  for (; s[n]; n++) { char const *p = set; while (*p && *p != s[n]) p++; if (!*p) break; }
  return n; }
