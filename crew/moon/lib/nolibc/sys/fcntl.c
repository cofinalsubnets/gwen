#include "../impl.h"

int fcntl(int fd, int cmd, ...) {
  va_list ap; va_start(ap, cmd);
  long arg = va_arg(ap, long);
  va_end(ap);
  return (int) er(sc3(NR_fcntl, fd, cmd, arg)); }
