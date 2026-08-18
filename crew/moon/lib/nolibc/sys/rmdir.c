#include "../impl.h"

int rmdir(char const *p) {
  long fl = __ai_osv == 2 ? 0x800 : AT_REMOVEDIR;   /* freebsd's AT_REMOVEDIR bit */
  return (int) er(sc3(NR_unlinkat, AT_FDCWD, (long) p, fl)); }
