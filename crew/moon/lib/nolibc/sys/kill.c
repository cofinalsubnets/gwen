#include "../impl.h"

int kill(pid_t pid, int sig) { return (int) er(sc2(NR_kill, pid, sig)); }
