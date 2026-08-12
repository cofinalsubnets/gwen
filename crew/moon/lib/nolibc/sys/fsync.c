#include "../impl.h"

int fsync(int fd) { return (int) er(sc1(NR_fsync, fd)); }
