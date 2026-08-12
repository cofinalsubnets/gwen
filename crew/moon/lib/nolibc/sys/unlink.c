#include "../impl.h"

int unlink(char const *p) { return (int) er(sc3(NR_unlinkat, AT_FDCWD, (long) p, 0)); }
