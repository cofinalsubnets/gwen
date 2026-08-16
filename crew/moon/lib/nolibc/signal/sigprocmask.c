#include "../impl.h"

#if defined(__FreeBSD__)
/* freebsd sigprocmask(340): 3 args, a 16-byte set, how already freebsd's
 * (signal.h forks SIG_BLOCK and kin on the OS). the userland sigset stays
 * glibc-sized; the kernel reads/writes its 16 bytes = __v[0..1] verbatim
 * (little-endian, bit (sig-1) -- one layout in both spellings). */
int sigprocmask(int how, sigset_t const *s, sigset_t *o) {
  unsigned long ks[2] = {0, 0}, ko[2] = {0, 0};
  if (s) { ks[0] = (unsigned long) s->__v[0]; ks[1] = (unsigned long) s->__v[1]; }
  long r = sc3(NR_rt_sigprocmask, how, s ? (long) ks : 0, o ? (long) ko : 0);
  if (r < 0) { __errno_v = (int) -r; return -1; }
  if (o) { memset(o, 0, sizeof *o); o->__v[0] = (long) ko[0]; o->__v[1] = (long) ko[1]; }
  return 0; }
#else
int sigprocmask(int how, sigset_t const *s, sigset_t *o) {
  unsigned long ks = s ? (unsigned long) s->__v[0] : 0, ko = 0;
  long r = sc4(NR_rt_sigprocmask, how, s ? (long) &ks : 0, o ? (long) &ko : 0, 8);
  if (r < 0) { __errno_v = (int) -r; return -1; }
  if (o) { memset(o, 0, sizeof *o); o->__v[0] = (long) ko; }
  return 0; }
#endif
