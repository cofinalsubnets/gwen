#include "../impl.h"

int accept(int fd, struct sockaddr *a, socklen_t *n) { return (int) er(sc3(NR_accept, fd, (long) a, (long) n)); }
