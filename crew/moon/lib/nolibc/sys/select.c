#include "../impl.h"

/* select over pselect6: the only lane asm-generic carries (no plain select on
 * arm64/riscv). linux's 6th arg is a {set, size} pair; freebsd's pselect
 * takes the bare sigset* -- and we carry none, so NULL rides both ways. */
int select(int n, fd_set *r, fd_set *w, fd_set *e, struct timeval *tv) {
  struct timespec ts, *tp = 0;
  long sm[2];
  if (tv) { ts.tv_sec = tv->tv_sec; ts.tv_nsec = tv->tv_usec * 1000; tp = &ts; }
  sm[0] = 0; sm[1] = 8;
  if (__ai_osv >= 2)
    return (int) er(sc6(NR_pselect6, n, (long) r, (long) w, (long) e, (long) tp, 0));
  return (int) er(sc6(NR_pselect6, n, (long) r, (long) w, (long) e, (long) tp, (long) sm)); }
