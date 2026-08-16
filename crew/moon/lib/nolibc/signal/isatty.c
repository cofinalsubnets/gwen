#include "../impl.h"

#if defined(__FreeBSD__)
/* the probe wants only the verdict, so it asks the kernel straight and keeps
 * our termios struct out of it: TIOCGETA = IOC_OUT | 44<<16 | 't'<<8 | 19
 * (freebsd's ioctl encoding; the termios family proper is rung 4). */
int isatty(int fd) {
  char t[44];
  return sc3(NR_ioctl, fd, 0x402c7413L, (long) t) == 0; }
#else
int isatty(int fd) {
  struct termios t;
  return tcgetattr(fd, &t) == 0; }
#endif
