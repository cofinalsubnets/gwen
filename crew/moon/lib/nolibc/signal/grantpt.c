#include "../impl.h"

int grantpt(int fd) {
  if (__ai_osv == 3) return (int) er(fb3(NR_fb_ioctl, fd, 0x20007447L, 0));   /* TIOCGRANTPT */
  (void) fd; return 0; }                    /* devpts / pts(4) grant at open */
