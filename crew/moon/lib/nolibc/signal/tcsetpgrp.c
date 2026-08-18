#include "../impl.h"

#if defined(__FreeBSD__)
int tcsetpgrp(int fd, pid_t pg) { int p = (int) pg; return ioctl(fd, 0x80047476UL, &p); }  /* TIOCSPGRP: IOC_IN | 4<<16 | 't'<<8 | 118 */
#else
int tcsetpgrp(int fd, pid_t pg) { int p = (int) pg; return ioctl(fd, 21520, &p); }  /* TIOCSPGRP */
#endif
