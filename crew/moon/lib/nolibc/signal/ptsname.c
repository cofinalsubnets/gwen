#include "../impl.h"

char *ptsname(int fd) {
  static char nb[32];
  if (__ai_osv == 3) {
    struct { int cfd, sfd; char cn[1024], sn[1024]; } g;      /* ptmget */
    if (fb3(NR_fb_ioctl, fd, 0x48087448L, (long) &g) < 0) return 0;   /* TIOCPTSNAME */
    snprintf(nb, sizeof nb, "%s", g.sn);
    return nb; }
  if (__ai_osv == 2) {
    struct { int len; void *buf; } a = { sizeof nb - 5, nb + 5 };
    if (fb3(NR_fb_ioctl, fd, 0x80106678L, (long) &a) < 0) return 0;   /* FIODGNAME */
    return memcpy(nb, "/dev/", 5), nb; }                              /* the kernel answers "pts/N", sans /dev/ */
  int n = 0;
  if (ioctl(fd, 2147767344UL, &n) < 0) return 0;                      /* TIOCGPTN */
  snprintf(nb, sizeof nb, "/dev/pts/%d", n);
  return nb; }
