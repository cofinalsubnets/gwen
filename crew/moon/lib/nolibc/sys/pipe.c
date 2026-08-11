#include "../impl.h"

int pipe(int *fds) { return (int) er(sc2(NR_pipe2, (long) fds, 0)); }
