#include "../impl.h"

int stat(char const *p, struct stat *st) { return (int) er(sc4(NR_newfstatat, AT_FDCWD, (long) p, (long) st, 0)); }
