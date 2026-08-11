#include "../impl.h"

int sigprocmask(int how, sigset_t const *s, sigset_t *o) {
  unsigned long ks = s ? (unsigned long) s->__v[0] : 0, ko = 0;
  long r = sc4(NR_rt_sigprocmask, how, s ? (long) &ks : 0, o ? (long) &ko : 0, 8);
  if (r < 0) { __errno_v = (int) -r; return -1; }
  if (o) { memset(o, 0, sizeof *o); o->__v[0] = (long) ko; }
  return 0; }
