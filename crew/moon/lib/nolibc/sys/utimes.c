#include "../impl.h"

int utimes(char const *p, struct timeval const *tv) {
  struct timespec ts[2];
  if (!tv) return utimensat(AT_FDCWD, p, 0, 0);
  ts[0].tv_sec = tv[0].tv_sec; ts[0].tv_nsec = tv[0].tv_usec * 1000;
  ts[1].tv_sec = tv[1].tv_sec; ts[1].tv_nsec = tv[1].tv_usec * 1000;
  return utimensat(AT_FDCWD, p, ts, 0); }
