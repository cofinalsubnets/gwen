#include "../impl.h"

long readlink(char const *p, char *b, unsigned long n) { return er(sc4(NR_readlinkat, AT_FDCWD, (long) p, (long) b, (long) n)); }
