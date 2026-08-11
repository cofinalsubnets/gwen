#include "../impl.h"

long pwrite(int fd, void const *b, unsigned long n, long off) { return er(sc4(NR_pwrite64, fd, (long) b, (long) n, off)); }
