#include "../impl.h"

int waitpid(int pid, int *st, int opt) {
  long r = er(sc4(NR_wait4, pid, (long) st, opt, 0));
  if (__ai_osv >= 2 && r > 0 && st) {
    /* the status word's layout agrees (BSD heritage), but the signal INSIDE
     * it is freebsd's number: translate the signaled and stopped forms. */
    int s = *st;
    if ((s & 0x7f) != 0 && (s & 0x7f) != 0x7f)
      *st = (s & ~0x7f) | (int) __ai_sigcan(s & 0x7f);
    else if ((s & 0xff) == 0x7f)
      *st = (s & 0xff) | ((int) __ai_sigcan((s >> 8) & 0xff) << 8); }
  return (int) r; }
