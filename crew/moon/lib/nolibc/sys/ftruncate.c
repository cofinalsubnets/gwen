#include "../impl.h"

int ftruncate(int fd, long n) { return (int) er(sc2(NR_ftruncate, fd, n)); }
