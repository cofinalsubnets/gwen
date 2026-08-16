#include "../impl.h"
/* absent on freebsd until rung 4 supplies the body (another ioctl encoding /
   another signature) -- an empty member reads "undefined reference" */
#if !defined(__FreeBSD__)

/* ---- the terminal ---- */
int tcgetattr(int fd, struct termios *t) { return ioctl(fd, 21505, t); }            /* TCGETS */
#endif
