#include "../impl.h"

int setpgid(pid_t p, pid_t g) { return (int) er(sc2(NR_setpgid, p, g)); }
