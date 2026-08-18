#include "../impl.h"

int tcsetattr(int fd, int act, struct termios const *t) {
  if (act < 0 || act > 2) { __errno_v = EINVAL; return -1; }
  if (__ai_osv >= 2) {
    struct __fb_termios f;
    __ai_tiofb(t, &f);
    return (int) er(fb3(NR_fb_ioctl, fd, 0x802c7414L + act, (long) &f)); }   /* TIOCSETA/AW/AF */
  return ioctl(fd, (unsigned long) (21506 + act), t); }                      /* TCSETS/W/F */
