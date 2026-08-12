#include "../impl.h"

int symlink(char const *a, char const *b) { return (int) er(sc3(NR_symlinkat, (long) a, AT_FDCWD, (long) b)); }
