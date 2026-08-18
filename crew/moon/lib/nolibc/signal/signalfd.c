#include "../impl.h"
/* linux's mechanism; off the map, so a freebsd kernel answers ENOSYS
   (kqueue's EVFILT_SIGNAL is the body it awaits) */
int signalfd(int fd, sigset_t const *m, int fl) {
  unsigned long km = (unsigned long) m->__v[0];
  return (int) er(sc4(NR_signalfd4, fd, (long) &km, 8, fl)); }
