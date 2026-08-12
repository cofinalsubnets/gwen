#include "../impl.h"

int gettimeofday(struct timeval *tv, void *tz) {
  struct timespec ts;
  (void) tz;
  if (clock_gettime(0, &ts) < 0) return -1;        /* CLOCK_REALTIME */
  tv->tv_sec = ts.tv_sec;
  tv->tv_usec = ts.tv_nsec / 1000;
  return 0; }
