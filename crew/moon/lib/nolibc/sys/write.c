#include "../impl.h"

long write(int fd, void const *b, long n) { return er(sc3(NR_write, fd, (long) b, n)); }
