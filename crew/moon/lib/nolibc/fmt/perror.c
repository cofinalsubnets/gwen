#include "../impl.h"

void perror(char const *s) {
  int e = __errno_v;
  if (s && *s) fprintf(stderr, "%s: ", s);
  fprintf(stderr, "errno %d\n", e); }
