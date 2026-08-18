#include "../impl.h"
/* linux's mechanism; off the map, so a freebsd kernel answers ENOSYS */
int unshare(int fl) { return (int) er(sc1(NR_unshare, fl)); }
