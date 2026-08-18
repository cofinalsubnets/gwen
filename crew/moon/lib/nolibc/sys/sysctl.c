#include <sys/sysctl.h>
#include "../impl.h"

/* freebsd only (__sysctl, one table every arch); nothing on linux owes this name */
#if defined(__FreeBSD__)
int sysctl(int const *name, unsigned int namelen, void *oldp, size_t *oldlenp,
           void const *newp, size_t newlen) {
 return (int) er(sc6(NR___sysctl, (long) name, namelen, (long) oldp,
                     (long) oldlenp, (long) newp, (long) newlen)); }
#endif
