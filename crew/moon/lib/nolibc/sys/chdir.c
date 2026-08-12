#include "../impl.h"

int chdir(char const *p) { return (int) er(sc1(NR_chdir, (long) p)); }
