#include "../impl.h"

long sendto(int fd, void const *b, unsigned long n, int fl, struct sockaddr const *a, socklen_t an) {
  if (__ai_osv == 2) {
    struct sockaddr_storage s;
    long sa = 0;
    if (a) {
      if (an > sizeof s) return er(-EINVAL);
      an = __ai_sain(a, an, &s);
      sa = (long) &s; }
    return er(sc6(NR_sendto, fd, (long) b, (long) n, (int) __ai_msgfb(fl), sa, an)); }
  return er(sc6(NR_sendto, fd, (long) b, (long) n, fl, (long) a, an)); }
