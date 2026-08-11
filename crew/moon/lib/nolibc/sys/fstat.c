#include "../impl.h"

int fstat(int fd, struct stat *st) { return (int) er(sc2(NR_fstat, fd, (long) st)); }
