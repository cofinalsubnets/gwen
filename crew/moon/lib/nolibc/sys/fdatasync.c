#include "../impl.h"

int fdatasync(int fd) { return (int) er(sc1(NR_fdatasync, fd)); }
