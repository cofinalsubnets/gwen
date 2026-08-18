#include "../impl.h"

int utimensat(int dfd, char const *p, struct timespec const *ts, int fl) {
  if (__ai_osv == 2) {
    /* freebsd spells the specials -1/-2 where linux says 2^30-1/2^30-2, and
     * AT_SYMLINK_NOFOLLOW wears its bit */
    struct timespec t2[2];
    long a = ts ? (long) ts : 0;
    if (ts && (ts[0].tv_nsec == UTIME_NOW || ts[0].tv_nsec == UTIME_OMIT
            || ts[1].tv_nsec == UTIME_NOW || ts[1].tv_nsec == UTIME_OMIT)) {
      t2[0] = ts[0]; t2[1] = ts[1];
      for (int i = 0; i < 2; i++)
        if (t2[i].tv_nsec == UTIME_NOW) t2[i].tv_nsec = -1;
        else if (t2[i].tv_nsec == UTIME_OMIT) t2[i].tv_nsec = -2;
      a = (long) t2; }
    long f2 = (fl & AT_SYMLINK_NOFOLLOW) ? 0x200 : 0;
    return (int) er(sc4(NR_utimensat, dfd, (long) p, a, f2)); }
  if (__ai_osv == 3 && (fl & AT_SYMLINK_NOFOLLOW))
    fl = (fl & ~AT_SYMLINK_NOFOLLOW) | 0x200;   /* netbsd's bit; its specials are linux's */
  return (int) er(sc4(NR_utimensat, dfd, (long) p, (long) ts, fl)); }
