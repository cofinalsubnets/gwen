#include "../impl.h"

int lstat(char const *p, struct stat *st) {
  if (__ai_osv == 2) {
    struct __fb_stat f;
    long r = er(sc4(NR_newfstatat, AT_FDCWD, (long) p, (long) &f, 0x200));   /* AT_SYMLINK_NOFOLLOW, freebsd's bit */
    if (r >= 0) __ai_fbstat(&f, st);
    return (int) r; }
  return (int) er(sc4(NR_newfstatat, AT_FDCWD, (long) p, (long) st, AT_SYMLINK_NOFOLLOW)); }
