#include "../impl.h"
#if defined(__FreeBSD__)
int fork(void) { return (int) er(sc1(NR_fork, 0)); }   /* fork(2) is real here */
#else
int fork(void) { return (int) er(sc5(NR_clone, 17, 0, 0, 0, 0)); }   /* clone(SIGCHLD): the fork nobody dropped */
#endif
