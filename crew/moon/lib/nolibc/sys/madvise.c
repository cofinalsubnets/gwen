#include "../impl.h"

int madvise(void *p, long n, int adv) {
  if (__ai_osv >= 2) {
    /* 0..4 agree; FREE moves (freebsd 5, netbsd 6, linux 8); DONTFORK/DOFORK
     * (10/11) have no twin -- and freebsd's 10 is MADV_PROTECT, so they no-op
     * rather than translate wrong. the spawn guard degrades, it does not
     * misfire. */
    if (adv == 8) adv = __ai_osv == 3 ? 6 : 5;
    else if (adv == 10 || adv == 11) return 0; }
  return (int) er(sc3(NR_madvise, (long) p, n, adv)); }
