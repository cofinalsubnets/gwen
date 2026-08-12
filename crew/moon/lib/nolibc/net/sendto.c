#include "../impl.h"

long sendto(int fd, void const *b, unsigned long n, int fl, struct sockaddr const *a, socklen_t an) {
  return er(sc6(NR_sendto, fd, (long) b, (long) n, fl, (long) a, an)); }
