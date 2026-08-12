#include "../impl.h"

int sigemptyset(sigset_t *s) { memset(s, 0, sizeof *s); return 0; }
