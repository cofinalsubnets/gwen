#include "../impl.h"

long pread(int fd, void *b, unsigned long n, long off) { return er(sc4(NR_pread64, fd, (long) b, (long) n, off)); }
