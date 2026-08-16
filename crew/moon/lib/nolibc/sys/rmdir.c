#include "../impl.h"

int rmdir(char const *p) { return (int) er(sc3(NR_unlinkat, AT_FDCWD, (long) p, AT_REMOVEDIR)); }
