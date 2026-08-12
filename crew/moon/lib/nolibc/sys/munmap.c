#include "../impl.h"

int munmap(void *a, long n) { return (int) er(sc2(NR_munmap, (long) a, n)); }
