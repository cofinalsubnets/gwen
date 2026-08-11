#include "../impl.h"

int bind(int fd, struct sockaddr const *a, socklen_t n) { return (int) er(sc3(NR_bind, fd, (long) a, n)); }
