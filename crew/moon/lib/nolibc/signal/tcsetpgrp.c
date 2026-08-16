#include "../impl.h"
/* absent on freebsd until rung 4 supplies the body (another ioctl encoding /
   another signature) -- an empty member reads "undefined reference" */
#if !defined(__FreeBSD__)

int tcsetpgrp(int fd, pid_t pg) { int p = (int) pg; return ioctl(fd, 21520, &p); }  /* TIOCSPGRP */
#endif
