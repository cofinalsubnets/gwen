#include "../impl.h"

int getpeername(int fd, struct sockaddr *a, socklen_t *n) { return (int) er(sc3(NR_getpeername, fd, (long) a, (long) n)); }
