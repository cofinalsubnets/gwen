#include "../impl.h"
/* absent on freebsd until rung 4 supplies the body (another ioctl encoding /
   another signature) -- an empty member reads "undefined reference" */
#if !defined(__FreeBSD__)

int mount(char const *src, char const *tgt, char const *ty, unsigned long fl, void const *d) {
  return (int) er(sc5(NR_mount, (long) src, (long) tgt, (long) ty, (long) fl, (long) d)); }
#endif
