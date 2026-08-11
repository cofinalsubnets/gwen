#include "../impl.h"

int utimensat(int dfd, char const *p, struct timespec const *ts, int fl) {
  return (int) er(sc4(NR_utimensat, dfd, (long) p, (long) ts, fl)); }
