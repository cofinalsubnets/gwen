#include "../impl.h"

/* ---- the pty quartet ---- */
int posix_openpt(int fl) { return open("/dev/ptmx", fl, 0); }
