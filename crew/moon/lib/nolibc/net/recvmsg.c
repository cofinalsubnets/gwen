#include "../impl.h"

/* refused on freebsd like sendmsg: the struct layouts differ */
long recvmsg(int fd, struct msghdr *m, int fl) {
  if (__ai_osv >= 2) return er(-ENOSYS);
  return er(sc3(NR_recvmsg, fd, (long) m, fl)); }
