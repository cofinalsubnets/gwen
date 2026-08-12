#include "../impl.h"

int raise(int sig) { return kill(getpid(), sig); }
