#include "../impl.h"

int mknod(char const *p, unsigned int mode, unsigned long dev) { return (int) er(sc4(NR_mknodat, AT_FDCWD, (long) p, mode, (long) dev)); }
