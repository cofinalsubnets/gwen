#include "../impl.h"

/* ---- the plain syscall tail: one line each ---- */
long read(int fd, void *b, long n) { return er(sc3(NR_read, fd, (long) b, n)); }
