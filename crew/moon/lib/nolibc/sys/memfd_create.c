#include "../impl.h"
/* linux's mechanism; off the map, so a freebsd kernel answers ENOSYS
   (shm_open2 + SHM_ANON is the body it awaits) */
int memfd_create(char const *name, unsigned int fl) { return (int) er(sc2(NR_memfd_create, (long) name, fl)); }
