#include "../impl.h"

long recvfrom(int fd, void *b, unsigned long n, int fl, struct sockaddr *a, socklen_t *an) {
  long r;
  if (__ai_osv >= 2) fl = (int) __ai_msgfb(fl);
  r = er(sc6(NR_recvfrom, fd, (long) b, (long) n, fl, (long) a, (long) an));
  if (r >= 0 && __ai_osv >= 2 && a && an) __ai_saout(a, *an);
  return r; }
