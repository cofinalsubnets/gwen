#include "../impl.h"

int unlockpt(int fd) {
  if (__ai_osv == 2) { (void) fd; return 0; }               /* pts(4): the kernel unlocks at open */
  int z = 0; return ioctl(fd, 1074025521UL, &z); }          /* TIOCSPTLCK */
