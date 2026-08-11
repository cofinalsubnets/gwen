#include "../impl.h"

unsigned int getuid(void) { return (unsigned int) sc0(NR_getuid); }
