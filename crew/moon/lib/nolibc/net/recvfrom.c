#include "../impl.h"

long recvfrom(int fd, void *b, unsigned long n, int fl, struct sockaddr *a, socklen_t *an) {
  return er(sc6(NR_recvfrom, fd, (long) b, (long) n, fl, (long) a, (long) an)); }
