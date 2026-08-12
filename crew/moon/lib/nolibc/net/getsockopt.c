#include "../impl.h"

int getsockopt(int fd, int lv, int op, void *v, socklen_t *n) {
  return (int) er(sc5(NR_getsockopt, fd, lv, op, (long) v, (long) n)); }
