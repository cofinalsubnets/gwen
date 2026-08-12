#include "../impl.h"

int setsockopt(int fd, int lv, int op, void const *v, socklen_t n) {
  return (int) er(sc5(NR_setsockopt, fd, lv, op, (long) v, n)); }
