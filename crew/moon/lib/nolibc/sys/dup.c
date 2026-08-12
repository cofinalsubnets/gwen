#include "../impl.h"

int dup(int fd) { return (int) er(sc3(NR_fcntl, fd, 0, 0)); }                          /* F_DUPFD */
