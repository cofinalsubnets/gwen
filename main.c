static const char main_prog[] =
#ifdef MAIN_H
#include MAIN_H
#else
#include "main.h"
#endif
;
int gw_main(const char*, const char**);
int main(int ac, const char **av) { return gw_main(main_prog, av); }
