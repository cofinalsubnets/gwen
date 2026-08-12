#include "../impl.h"

int lstat(char const *p, struct stat *st) { return (int) er(sc4(NR_newfstatat, AT_FDCWD, (long) p, (long) st, 256)); }   /* AT_SYMLINK_NOFOLLOW */
