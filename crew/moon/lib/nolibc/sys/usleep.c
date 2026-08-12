#include "../impl.h"

int usleep(unsigned int us) {
  struct timespec ts;
  ts.tv_sec = us / 1000000;
  ts.tv_nsec = (long) (us % 1000000) * 1000;
  return (int) er(sc2(NR_nanosleep, (long) &ts, 0)); }
