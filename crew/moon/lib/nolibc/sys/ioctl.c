#include "../impl.h"

int ioctl(int fd, unsigned long req, ...) {
  va_list ap; va_start(ap, req);
  long arg = va_arg(ap, long);
  va_end(ap);
  return (int) er(sc3(NR_ioctl, fd, (long) req, arg)); }
