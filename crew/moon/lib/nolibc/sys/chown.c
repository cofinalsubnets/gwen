#include "../impl.h"

int chown(char const *p, unsigned int u, unsigned int g) { return (int) er(sc5(NR_fchownat, AT_FDCWD, (long) p, u, g, 0)); }
