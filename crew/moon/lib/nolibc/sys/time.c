#include "../impl.h"

time_t time(time_t *t) {
  struct timespec ts;
  clock_gettime(0, &ts);                       /* CLOCK_REALTIME */
  if (t) *t = ts.tv_sec;
  return ts.tv_sec; }
