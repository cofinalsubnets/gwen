#include "../impl.h"

int link(char const *a, char const *b) { return (int) er(sc5(NR_linkat, AT_FDCWD, (long) a, AT_FDCWD, (long) b, 0)); }
