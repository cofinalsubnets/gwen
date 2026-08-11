#include "../impl.h"

int chmod(char const *p, unsigned int m) { return (int) er(sc4(NR_fchmodat, AT_FDCWD, (long) p, m, 0)); }
