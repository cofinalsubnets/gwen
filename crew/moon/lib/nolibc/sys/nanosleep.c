#include "../impl.h"

int nanosleep(struct timespec const *req, struct timespec *rem) {
  return (int) er(sc2(NR_nanosleep, (long) req, (long) rem)); }
