#include "../impl.h"

int fchown(int fd, unsigned int u, unsigned int g) { return (int) er(sc3(NR_fchown, fd, u, g)); }
