#include "../impl.h"

long recvmsg(int fd, struct msghdr *m, int fl) { return er(sc3(NR_recvmsg, fd, (long) m, fl)); }
