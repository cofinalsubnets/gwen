#include "../impl.h"

int connect(int fd, struct sockaddr const *a, socklen_t n) {
  if (__ai_osv == 2) {
    struct sockaddr_storage s;
    if (n > sizeof s) return (int) er(-EINVAL);
    n = __ai_sain(a, n, &s);
    return (int) er(sc3(NR_connect, fd, (long) &s, n)); }
  return (int) er(sc3(NR_connect, fd, (long) a, n)); }
