#include "../impl.h"

int getpgrp(void) { return (int) sc1(NR_getpgid, 0); }
