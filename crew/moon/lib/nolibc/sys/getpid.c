#include "../impl.h"

int getpid(void) { return (int) sc0(NR_getpid); }
