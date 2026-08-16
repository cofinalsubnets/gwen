#include "../impl.h"
/* absent on freebsd until rung 4 supplies the body (another ioctl encoding /
   another signature) -- an empty member reads "undefined reference" */
#if !defined(__FreeBSD__)

/* ---- the pty quartet ---- */
int posix_openpt(int fl) { return open("/dev/ptmx", fl, 0); }
#endif
