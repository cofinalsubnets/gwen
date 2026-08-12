#include "../impl.h"

long lseek(int fd, long off, int wh) { return er(sc3(NR_lseek, fd, off, wh)); }
