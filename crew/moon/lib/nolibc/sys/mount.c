#include "../impl.h"

/* linux's signature; freebsd's differs (type dir flags data), so its number
 * is off the map and a freebsd kernel answers ENOSYS until a body lands. */
int mount(char const *src, char const *tgt, char const *ty, unsigned long fl, void const *d) {
  return (int) er(sc5(NR_mount, (long) src, (long) tgt, (long) ty, (long) fl, (long) d)); }
