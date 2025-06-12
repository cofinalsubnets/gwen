#include "p.h"
#ifdef BOOT_H
#include BOOT_H
#else
#include "boot.h"
#endif
int main(int ac, const char **av) { return p_main(boot_prog, av); }
