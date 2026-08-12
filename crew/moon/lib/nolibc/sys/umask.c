#include "../impl.h"

unsigned int umask(unsigned int m) { return (unsigned int) sc1(NR_umask, m); }
