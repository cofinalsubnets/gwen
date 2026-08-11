#include "../impl.h"

int connect(int fd, struct sockaddr const *a, socklen_t n) { return (int) er(sc3(NR_connect, fd, (long) a, n)); }
