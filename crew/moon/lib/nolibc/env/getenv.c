#include "../impl.h"

/* ---- env ---- */
char *getenv(char const *k) {
  size_t n = strlen(k);
  if (!environ) return 0;
  for (char **e = environ; *e; e++)
    if (memcmp(*e, k, n) == 0 && (*e)[n] == '=') return *e + n + 1;
  return 0; }
