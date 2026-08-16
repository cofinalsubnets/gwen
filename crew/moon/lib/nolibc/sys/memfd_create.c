#include "../impl.h"
/* absent on freebsd until rung 3 supplies the body -- an empty member defines
   nothing, so a consumer reads "undefined reference", the honest sentence */
#if !defined(__FreeBSD__)
int memfd_create(char const *name, unsigned int fl) { return (int) er(sc2(NR_memfd_create, (long) name, fl)); }
#endif
