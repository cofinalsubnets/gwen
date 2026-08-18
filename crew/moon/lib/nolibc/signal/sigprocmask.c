#include "../impl.h"

int sigprocmask(int how, sigset_t const *s, sigset_t *o) {
  if (__ai_osv >= 2) {
    /* freebsd sigprocmask(340): 3 args, a 16-byte set, how 1/2/3 (ours + 1);
     * the mask bits carry signal numbers, so they translate bit by bit. */
    unsigned long ks[2] = {0, 0}, ko[2] = {0, 0};
    if (s) ks[0] = __ai_maskfb((unsigned long) s->__v[0]);
    long r = sc3(NR_rt_sigprocmask, how + 1, s ? (long) ks : 0, o ? (long) ko : 0);
    if (r < 0) { __errno_v = (int) -r; return -1; }
    if (o) { memset(o, 0, sizeof *o); o->__v[0] = (long) __ai_maskcan(ko[0]); }
    return 0; }
  unsigned long ks = s ? (unsigned long) s->__v[0] : 0, ko = 0;
  long r = sc4(NR_rt_sigprocmask, how, s ? (long) &ks : 0, o ? (long) &ko : 0, 8);
  if (r < 0) { __errno_v = (int) -r; return -1; }
  if (o) { memset(o, 0, sizeof *o); o->__v[0] = (long) ko; }
  return 0; }
