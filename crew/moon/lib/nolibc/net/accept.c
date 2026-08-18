#include "../impl.h"

int accept(int fd, struct sockaddr *a, socklen_t *n) {
  int r = (int) er(sc3(NR_accept, fd, (long) a, (long) n));
  if (r >= 0 && __ai_osv >= 2 && a && n) __ai_saout(a, *n);
  return r; }
