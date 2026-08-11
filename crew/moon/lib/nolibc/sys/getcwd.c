#include "../impl.h"

char *getcwd(char *b, unsigned long n) {
  long r = sc2(NR_getcwd, (long) b, (long) n);
  if (r < 0) { __errno_v = (int) -r; return 0; }
  return b; }
