#include "../impl.h"

int listen(int fd, int bl) { return (int) er(sc2(NR_listen, fd, bl)); }
