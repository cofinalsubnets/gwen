#include "gw.h"
static const char boot_prog[] =
#ifdef BOOT_H
#include BOOT_H
#else
#include "boot.h"
#endif
;
int main(int ac, const char **av) { return gw_main(boot_prog, av); }
