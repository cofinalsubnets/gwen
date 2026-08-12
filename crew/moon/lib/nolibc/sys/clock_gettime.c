#include "../impl.h"

int clock_gettime(int ck, struct timespec *ts) { return (int) er(sc2(NR_clock_gettime, ck, (long) ts)); }
