#include "../impl.h"

/* the msghdr and cmsghdr layouts differ (freebsd's iovlen is int, cmsg_len
 * 32-bit); nothing in the tree speaks them, so the freebsd lane refuses
 * loudly rather than handing the kernel a misread struct. */
long sendmsg(int fd, struct msghdr const *m, int fl) {
  if (__ai_osv >= 2) return er(-ENOSYS);
  return er(sc3(NR_sendmsg, fd, (long) m, fl)); }
