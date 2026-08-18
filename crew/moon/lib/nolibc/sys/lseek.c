#include "../impl.h"

long lseek(int fd, long off, int wh) {
  if (__ai_osv == 3) return er(sc4(NR_lseek, fd, 0, off, wh));   /* the classic PAD */
  return er(sc3(NR_lseek, fd, off, wh)); }
