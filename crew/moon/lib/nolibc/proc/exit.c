#include "../impl.h"

void _exit(int c) { for (;;) sc1(NR_exit_group, c); }
