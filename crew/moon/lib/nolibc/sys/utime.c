#include "../impl.h"

int utime(char const *path, struct utimbuf const *t) {
  if (!t) return utimensat(AT_FDCWD, path, 0, 0);
  struct timespec ts[2];
  ts[0].tv_sec = t->actime;  ts[0].tv_nsec = 0;
  ts[1].tv_sec = t->modtime; ts[1].tv_nsec = 0;
  return utimensat(AT_FDCWD, path, ts, 0); }
