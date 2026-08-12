#include "../impl.h"

long sendmsg(int fd, struct msghdr const *m, int fl) { return er(sc3(NR_sendmsg, fd, (long) m, fl)); }
