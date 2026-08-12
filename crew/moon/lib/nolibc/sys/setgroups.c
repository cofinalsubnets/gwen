#include "../impl.h"

int setgroups(size_t n, gid_t const *l) { return (int) er(sc2(NR_setgroups, (long) n, (long) l)); }
