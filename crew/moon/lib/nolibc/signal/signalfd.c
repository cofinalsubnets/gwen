#include "../impl.h"
/* linux's mechanism; off the map, so a BSD kernel answers ENOSYS -- and the
   caller falls to kqueue's EVFILT_SIGNAL (sys/kevent.c, posix.c's sigfd) */
int signalfd(int fd, sigset_t const *m, int fl) {
  unsigned long km = (unsigned long) m->__v[0];
  return (int) er(sc4(NR_signalfd4, fd, (long) &km, 8, fl)); }
