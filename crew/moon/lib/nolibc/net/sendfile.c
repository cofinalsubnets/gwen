#include "../impl.h"
/* absent on freebsd until rung 4 supplies the body (another ioctl encoding /
   another signature) -- an empty member reads "undefined reference" */
#if !defined(__FreeBSD__)

/* ---- sockets ---- */
long sendfile(int out, int in, long *off, unsigned long n) {
  return er(sc4(NR_sendfile, out, in, (long) off, (long) n)); }
#endif
