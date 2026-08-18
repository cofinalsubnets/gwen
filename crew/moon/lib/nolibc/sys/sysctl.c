#include <sys/sysctl.h>
#include "../impl.h"

/* the BSD door (__sysctl is 202 on both); on a linux kernel the name has no
 * meaning and answers ENOSYS -- the callers all guard on the OS. */
int sysctl(int const *name, unsigned int namelen, void *oldp, size_t *oldlenp,
           void const *newp, size_t newlen) {
  if (__ai_osv < 2) { __errno_v = ENOSYS; return -1; }
  return (int) er(fb6(NR_fb___sysctl, (long) name, namelen, (long) oldp,
                      (long) oldlenp, (long) newp, (long) newlen)); }
