#include "../impl.h"

int close(int fd) { return (int) er(sc1(NR_close, fd)); }
