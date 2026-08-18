#include "../impl.h"

int getsockopt(int fd, int lv, int op, void *v, socklen_t *n) {
  if (__ai_osv == 2) {
    long l = lv, o = op;
    if (__ai_sofb(&l, &o)) return (int) er(-ENOSYS);   /* an unmapped name, loudly */
    return (int) er(sc5(NR_getsockopt, fd, (int) l, (int) o, (long) v, (long) n)); }
  return (int) er(sc5(NR_getsockopt, fd, lv, op, (long) v, (long) n)); }
