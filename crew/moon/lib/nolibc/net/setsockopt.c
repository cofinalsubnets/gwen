#include "../impl.h"

int setsockopt(int fd, int lv, int op, void const *v, socklen_t n) {
  if (__ai_osv == 2) {
    long l = lv, o = op;
    if (__ai_sofb(&l, &o)) return (int) er(-ENOSYS);   /* an unmapped name, loudly */
    return (int) er(sc5(NR_setsockopt, fd, (int) l, (int) o, (long) v, n)); }
  return (int) er(sc5(NR_setsockopt, fd, lv, op, (long) v, n)); }
