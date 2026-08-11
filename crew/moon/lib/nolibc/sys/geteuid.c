#include "../impl.h"

unsigned int geteuid(void) { return (unsigned int) sc0(NR_geteuid); }
