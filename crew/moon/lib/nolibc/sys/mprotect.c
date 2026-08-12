#include "../impl.h"

int mprotect(void *a, long n, int prot) { return (int) er(sc3(NR_mprotect, (long) a, n, prot)); }
