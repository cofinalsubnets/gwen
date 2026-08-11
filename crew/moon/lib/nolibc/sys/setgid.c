#include "../impl.h"

int setgid(unsigned int g) { return (int) er(sc1(NR_setgid, g)); }
