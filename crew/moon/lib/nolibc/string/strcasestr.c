#include "../impl.h"

char *strcasestr(char const *h, char const *n) {
  size_t m = strlen(n);
  if (!m) return (char *) h;
  for (; *h; h++) if (!strncasecmp(h, n, m)) return (char *) h;
  return 0; }
