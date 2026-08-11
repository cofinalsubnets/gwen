#include "../impl.h"

int creat(char const *p, unsigned int mode) { return open(p, O_WRONLY | O_CREAT | O_TRUNC, (int) mode); }
