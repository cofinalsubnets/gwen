#include "../impl.h"
/* absent on freebsd until rung 4 supplies the body (another ioctl encoding /
   another signature) -- an empty member reads "undefined reference" */
#if !defined(__FreeBSD__)

int unlockpt(int fd) { int z = 0; return ioctl(fd, 1074025521UL, &z); }   /* TIOCSPTLCK */
#endif
