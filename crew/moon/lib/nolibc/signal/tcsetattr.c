#include "../impl.h"

#if defined(__FreeBSD__)
int tcsetattr(int fd, int act, struct termios const *t) {
  if (act < 0 || act > 2) { __errno_v = EINVAL; return -1; }
  return ioctl(fd, 0x802c7414UL + (unsigned) act, t); }         /* TIOCSETA/AW/AF: IOC_IN | 44<<16 | 't'<<8 | 20+act */
#else
int tcsetattr(int fd, int act, struct termios const *t) {
  if (act < 0 || act > 2) { __errno_v = EINVAL; return -1; }
  return ioctl(fd, (unsigned long) (21506 + act), t); }                             /* TCSETS/W/F */
#endif
