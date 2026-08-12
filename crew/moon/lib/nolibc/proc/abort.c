#include "../impl.h"

void abort(void) { raise(SIGABRT); _exit(127); }
