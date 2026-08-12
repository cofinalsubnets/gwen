#include "../impl.h"

char *strstr(char const *h, char const *n) {
  size_t nl = strlen(n);
  if (!nl) return (char *) h;
  for (; *h; h++)
    if (*h == *n && !strncmp(h, n, nl)) return (char *) h;
  return 0; }
