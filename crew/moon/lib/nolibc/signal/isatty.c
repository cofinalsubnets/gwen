#include "../impl.h"

/* the probe wants only the verdict, so it asks the kernel straight and keeps
 * struct termios out of it: freebsd's TIOCGETA reads 44 bytes, linux's
 * TCGETS its own -- either way 0 means a tty answered. */
int isatty(int fd) {
  if (__ai_osv >= 2) {
    char t[44];
    return fb3(NR_fb_ioctl, fd, 0x402c7413L, (long) t) == 0; }   /* TIOCGETA */
  struct termios t;
  return sc3(NR_ioctl, fd, 21505, (long) &t) == 0; }             /* TCGETS */
