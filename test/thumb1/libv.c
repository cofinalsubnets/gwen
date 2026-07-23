/* v6-M varargs, mooncc side: the pop-r3/bx epilogue + the r0-r3 push block
   over lr/fp/r4 (ovb 12). vsum/vnth are love.c's variadic shapes (ai_push /
   ioprintf: anonymous WORDS only), vnth2 the decayed-va_list helper
   (gvzprintf's shape), ovnamed the named-params-overflow edge, vpair the
   pair-returning variadic (r0:r1 must survive the r3 return hop). */
#include <stdarg.h>
typedef unsigned uptr;

int vsum(int n, ...) {                    /* ai_push's shape: anonymous words */
  va_list ap; int s, i;
  va_start(ap, n); s = 0;
  for (i = 0; i < n; i++) s += va_arg(ap, int);
  va_end(ap); return s; }
static int vnth2(int idx, va_list ap) {   /* the decayed-va_list helper */
  int i; uptr t;
  for (i = 0; i < idx; i++) t = va_arg(ap, uptr);
  t = va_arg(ap, uptr); return (int)t; }
int vnth(int idx, ...) {
  va_list ap; int r;
  va_start(ap, idx); r = vnth2(idx, ap); va_end(ap); return r; }
int vcall(void) {                         /* mooncc-caller -> mooncc-variadic, anon args past r0-r3 */
  return vsum(6, 1, 2, 3, 4, 5, 6)*100 + vnth(2, 7, 8, 9, 10); }
int ovnamed(int a, int b, int c, int d, int e, ...) {  /* named params OVERFLOW: e rides the caller stack, anon args follow it */
  va_list ap; int x, y;
  va_start(ap, e);
  x = va_arg(ap, int); y = va_arg(ap, int);
  va_end(ap);
  return a + b*10 + c*100 + d*1000 + e*10000 + x*100000 + y*1000000; }
