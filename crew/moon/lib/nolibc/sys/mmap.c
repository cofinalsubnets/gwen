#include "../impl.h"

void *mmap(void *a, long n, int prot, int fl, int fd, long off) {
  if (__ai_osv == 2) fl = (int) __ai_mapfb(fl);
  long r = sc6(NR_mmap, (long) a, n, prot, fl, fd, off);
  if ((unsigned long) r > (unsigned long) -4096L) { __errno_v = (int) -r; return (void *) -1; }
  return (void *) r; }
