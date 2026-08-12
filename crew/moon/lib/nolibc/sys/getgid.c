#include "../impl.h"

unsigned int getgid(void) { return (unsigned int) sc0(NR_getgid); }
