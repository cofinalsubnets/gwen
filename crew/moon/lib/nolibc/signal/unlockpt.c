#include "../impl.h"

#if defined(__FreeBSD__)
int unlockpt(int fd) { (void) fd; return 0; }                   /* pts(4): the kernel unlocks at open */
#else
int unlockpt(int fd) { int z = 0; return ioctl(fd, 1074025521UL, &z); }   /* TIOCSPTLCK */
#endif
