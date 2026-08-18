#include "../impl.h"
int fork(void) {
  if (__ai_osv >= 2) return (int) er(fb1(NR_fb_fork, 0));   /* fork(2) is real there */
  return (int) er(sc5(NR_clone, 17, 0, 0, 0, 0)); }   /* clone(SIGCHLD): the fork nobody dropped */
