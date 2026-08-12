#include "../impl.h"

int fchmod(int fd, unsigned int m) { return (int) er(sc2(NR_fchmod, fd, m)); }
