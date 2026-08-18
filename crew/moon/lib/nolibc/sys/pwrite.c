#include "../impl.h"

long pwrite(int fd, void const *b, unsigned long n, long off) {
  if (__ai_osv == 3) return er(sc5(NR_pwrite64, fd, (long) b, (long) n, 0, off));   /* the classic PAD */
  return er(sc4(NR_pwrite64, fd, (long) b, (long) n, off)); }
