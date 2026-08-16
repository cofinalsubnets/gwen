#include "../impl.h"

#if defined(__FreeBSD__)
/* freebsd sigaction(416): userland == kernel there -- {handler(8), flags(4),
 * a 16-byte set at offset 12, no restorer (the kernel lays its own return
 * trampoline)}. our glibc-shaped struct sigaction translates at the seam;
 * the mask bytes copy verbatim (one bit law both sides, bit sig-1). */
struct __fbsd_sigaction { void *h; int flags; unsigned int mask[4]; };
int sigaction(int sig, struct sigaction const *a, struct sigaction *old) {
  struct __fbsd_sigaction ka, ko;
  memset(&ko, 0, sizeof ko);
  if (a) {
    memset(&ka, 0, sizeof ka);
    ka.h = (void *) a->sa_handler;
    ka.flags = a->sa_flags;
    memcpy(ka.mask, a->sa_mask.__v, 16); }
  long r = sc3(NR_rt_sigaction, sig, a ? (long) &ka : 0, old ? (long) &ko : 0);
  if (r < 0) { __errno_v = (int) -r; return -1; }
  if (old) {
    memset(old, 0, sizeof *old);
    old->sa_handler = (void (*)(int)) ko.h;
    old->sa_flags = ko.flags;
    memcpy(old->sa_mask.__v, ko.mask, 16); }
  return 0; }
#else
int sigaction(int sig, struct sigaction const *a, struct sigaction *old) {
  struct __ksigaction ka, ko;
  memset(&ko, 0, sizeof ko);
  if (a) {
    ka.h = (void *) a->sa_handler;
#if defined(__aarch64__) || defined(__riscv)
    ka.flags = (unsigned long) (unsigned int) a->sa_flags;
    ka.restorer = 0;
#else
    ka.flags = (unsigned long) (unsigned int) a->sa_flags | 67108864UL;   /* SA_RESTORER */
    ka.restorer = (void *) __ai_sigret;
#endif
    ka.mask = (unsigned long) a->sa_mask.__v[0]; }
  long r = sc4(NR_rt_sigaction, sig, a ? (long) &ka : 0, old ? (long) &ko : 0, 8);
  if (r < 0) { __errno_v = (int) -r; return -1; }
  if (old) {
    memset(old, 0, sizeof *old);
    old->sa_handler = (void (*)(int)) ko.h;
    old->sa_flags = (int) ko.flags;
    old->sa_mask.__v[0] = (long) ko.mask; }
  return 0; }
#endif
