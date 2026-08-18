#include "../impl.h"

/* ---- the terminal ---- */
int tcgetattr(int fd, struct termios *t) {
  if (__ai_osv >= 2) {
    struct __fb_termios f;
    long r = er(fb3(NR_fb_ioctl, fd, 0x402c7413L, (long) &f));   /* TIOCGETA */
    if (r >= 0) __ai_tiocan(&f, t);
    return (int) r; }
  return ioctl(fd, 21505, t); }                                  /* TCGETS */
