#include "../impl.h"

int access(char const *p, int m) { return (int) er(sc4(NR_faccessat, AT_FDCWD, (long) p, m, 0)); }
