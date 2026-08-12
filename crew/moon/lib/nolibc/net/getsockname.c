#include "../impl.h"

int getsockname(int fd, struct sockaddr *a, socklen_t *n) { return (int) er(sc3(NR_getsockname, fd, (long) a, (long) n)); }
