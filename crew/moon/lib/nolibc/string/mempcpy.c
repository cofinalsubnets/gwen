#include "../impl.h"

/* the two GNU string extensions the headers name: memcpy answering its END, and
 * memchr with no bound (the caller warrants the byte is there) */
void *mempcpy(void *d, void const *s, size_t n) { return (char *) memcpy(d, s, n) + n; }
