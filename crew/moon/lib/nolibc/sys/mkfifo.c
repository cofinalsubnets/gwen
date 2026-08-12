#include "../impl.h"

int mkfifo(char const *p, unsigned int mode) { return mknod(p, mode | 4096U, 0); }     /* S_IFIFO = 010000 */
