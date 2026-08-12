#include "../impl.h"

int rename(char const *a, char const *b) { return (int) er(sc4(NR_renameat, AT_FDCWD, (long) a, AT_FDCWD, (long) b)); }
