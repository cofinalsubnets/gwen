#include "../impl.h"
/* absent on freebsd until rung 3 supplies the body -- an empty member defines
   nothing, so a consumer reads "undefined reference", the honest sentence */
#if !defined(__FreeBSD__)
int unshare(int fl) { return (int) er(sc1(NR_unshare, fl)); }
#endif
