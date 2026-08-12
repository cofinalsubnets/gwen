#include "../impl.h"

long clock(void) {                                 /* CLOCKS_PER_SEC is 1e6; clock 2 = CLOCK_PROCESS_CPUTIME_ID */
  struct timespec ts;
  if (clock_gettime(2, &ts) < 0) return -1;
  return ts.tv_sec * 1000000 + ts.tv_nsec / 1000; }
