#include "../impl.h"

int mkdir(char const *p, unsigned int m) { return (int) er(sc3(NR_mkdirat, AT_FDCWD, (long) p, m)); }
