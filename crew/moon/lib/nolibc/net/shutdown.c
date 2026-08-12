#include "../impl.h"

int shutdown(int fd, int how) { return (int) er(sc2(NR_shutdown, fd, how)); }
