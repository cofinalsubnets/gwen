#include "../impl.h"

int setuid(unsigned int u) { return (int) er(sc1(NR_setuid, u)); }
