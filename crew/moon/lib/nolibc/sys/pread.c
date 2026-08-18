#include "../impl.h"

long pread(int fd, void *b, unsigned long n, long off) {
  if (__ai_osv == 3) return er(sc5(NR_pread64, fd, (long) b, (long) n, 0, off));   /* the classic PAD */
  return er(sc4(NR_pread64, fd, (long) b, (long) n, off)); }
