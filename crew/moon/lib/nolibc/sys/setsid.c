#include "../impl.h"

int setsid(void) { return (int) er(sc0(NR_setsid)); }
