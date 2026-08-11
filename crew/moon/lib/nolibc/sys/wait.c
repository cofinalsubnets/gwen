#include "../impl.h"

int wait(int *st) { return waitpid(-1, st, 0); }
