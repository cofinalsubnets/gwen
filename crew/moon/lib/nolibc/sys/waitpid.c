#include "../impl.h"

int waitpid(int pid, int *st, int opt) { return (int) er(sc4(NR_wait4, pid, (long) st, opt, 0)); }
