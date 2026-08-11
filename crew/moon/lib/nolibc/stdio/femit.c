#include "../impl.h"

/* ---- the one formatter under fprintf and snprintf: %s %c %d %u %x %o %p and
 * the float lanes %f %e %g %a, with l/z widths and the %[-0+ #]W.P flags.
 * sink+ctx so neither caller stages a bound buffer. */
void __femit(void *ctx, int c) { fputc(c, (FILE *) ctx); }
