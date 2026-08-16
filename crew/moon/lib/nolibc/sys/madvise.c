#include "../impl.h"

int madvise(void *p, long n, int adv) { return (int) er(sc3(NR_madvise, (long) p, n, adv)); }
