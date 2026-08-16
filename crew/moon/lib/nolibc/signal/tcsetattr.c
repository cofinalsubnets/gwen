#include "../impl.h"
/* absent on freebsd until rung 4 supplies the body (another ioctl encoding /
   another signature) -- an empty member reads "undefined reference" */
#if !defined(__FreeBSD__)

int tcsetattr(int fd, int act, struct termios const *t) {
  if (act < 0 || act > 2) { __errno_v = EINVAL; return -1; }
  return ioctl(fd, (unsigned long) (21506 + act), t); }                             /* TCSETS/W/F */
#endif
