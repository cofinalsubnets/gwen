#include "../impl.h"

int chroot(char const *p) { return (int) er(sc1(NR_chroot, (long) p)); }
