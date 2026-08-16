#include "../impl.h"
/* absent on freebsd until rung 3 supplies the body -- an empty member defines
   nothing, so a consumer reads "undefined reference", the honest sentence */
#if !defined(__FreeBSD__)
int fork(void) { return (int) er(sc5(NR_clone, 17, 0, 0, 0, 0)); }   /* clone(SIGCHLD): the fork nobody dropped */
#endif
