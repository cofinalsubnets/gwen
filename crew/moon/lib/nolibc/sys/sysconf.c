#include "../impl.h"

long sysconf(int name) {
  if (name == _SC_PAGESIZE) return 4096;
  __errno_v = EINVAL; return -1; }
