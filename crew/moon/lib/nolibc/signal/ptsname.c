#include "../impl.h"

#if defined(__FreeBSD__)
char *ptsname(int fd) {
  static char nb[32];
  struct { int len; void *buf; } a = { sizeof nb - 5, nb + 5 };
  if (ioctl(fd, 0x80106678UL, &a) < 0) return 0;                /* FIODGNAME: IOC_IN | 16<<16 | 'f'<<8 | 120 */
  return memcpy(nb, "/dev/", 5), nb; }                          /* the kernel answers "pts/N", sans /dev/ */
#else
char *ptsname(int fd) {
  static char nb[32];
  int n = 0;
  if (ioctl(fd, 2147767344UL, &n) < 0) return 0;                /* TIOCGPTN */
  snprintf(nb, sizeof nb, "/dev/pts/%d", n);
  return nb; }
#endif
