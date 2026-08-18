#include "../impl.h"

/* one body, both kernels. on freebsd the numbers, the sa_flags and the
 * ksigaction shape translate -- and the HANDLER would land on the freebsd
 * number, so __ai_sigshim rides in front and hands the user's handler the
 * canonical one. SIG_DFL/SIG_IGN pass bare; the user handlers park in
 * __sighand by FREEBSD number, so the shim's lookup is one index. */
static void (*__sighand[64])(int);
static void __ai_sigshim(int s) {
  void (*h)(int) = (s > 0 && s < 64) ? __sighand[s] : 0;
  if (h) h((int) __ai_sigcan(s)); }

int sigaction(int sig, struct sigaction const *a, struct sigaction *old) {
  if (__ai_osv == 2) {
    long fs = __ai_sigfb(sig);
    if (fs <= 0) { __errno_v = EINVAL; return -1; }
    struct __fb_sigact ka, ko;
    memset(&ko, 0, sizeof ko);
    void (*prev)(int) = __sighand[fs];
    if (a) {
      memset(&ka, 0, sizeof ka);
      void (*h)(int) = a->sa_handler;
      if (h == (void (*)(int)) 0 || h == (void (*)(int)) 1) {
        ka.h = (void *) h; __sighand[fs] = 0; }
      else {
        ka.h = (void *) __ai_sigshim; __sighand[fs] = h; }
      ka.flags = (int) __ai_safb(a->sa_flags);
      ka.mask[0] = (unsigned int) __ai_maskfb((unsigned long) a->sa_mask.__v[0]);
      ka.mask[1] = (unsigned int) (__ai_maskfb((unsigned long) a->sa_mask.__v[0]) >> 32); }
    long r = sc3(NR_rt_sigaction, fs, a ? (long) &ka : 0, old ? (long) &ko : 0);
    if (r < 0) { if (a) __sighand[fs] = prev; __errno_v = (int) -r; return -1; }
    if (old) {
      memset(old, 0, sizeof *old);
      old->sa_handler = (ko.h == (void *) __ai_sigshim) ? prev : (void (*)(int)) ko.h;
      old->sa_flags = (int) __ai_sacan(ko.flags);
      old->sa_mask.__v[0] = (long) __ai_maskcan((unsigned long) ko.mask[0]
                                                | ((unsigned long) ko.mask[1] << 32)); }
    return 0; }
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
