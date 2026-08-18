#include "../impl.h"

/* ---- the terminal ---- */
#if defined(__FreeBSD__)
int tcgetattr(int fd, struct termios *t) { return ioctl(fd, 0x402c7413UL, t); }     /* TIOCGETA: IOC_OUT | 44<<16 | 't'<<8 | 19 */
#else
int tcgetattr(int fd, struct termios *t) { return ioctl(fd, 21505, t); }            /* TCGETS */
#endif
