#include "../impl.h"

void __pad(void (*put)(void *, int), void *ctx, int n, int ch) { while (n-- > 0) put(ctx, ch); }
/* the sign a value wears: '-' when it is negative, else whatever + or space
 * asks for, else none at all. */
int __fmtsgn(int neg, int fl) {
  return neg ? '-' : (fl & FfPlus) ? '+' : (fl & FfSpc) ? ' ' : 0; }
/* one integer field with %[-0+ #]WIDTH: digits reversed into tmp, then sign +
 * base prefix + pad (zeros hug the digits, spaces sit outside) + digits, or
 * left-justified. */
void __fmtnum(void (*put)(void *, int), void *ctx, unsigned long v, unsigned base,
                     int neg, int width, int fl, int up) {
  char tmp[24];
  int nd = 0;
  int a = up ? 55 : 87;
  do { unsigned d = (unsigned) (v % base); tmp[nd++] = (char) (d < 10 ? 48 + d : a + d); v /= base; } while (v);
  int sgn = __fmtsgn(neg, fl);
  /* # asks the base to show itself: 0x on hex, a leading 0 on octal -- and on
     neither when the digits already start with a zero, which is the whole of
     the zero case. */
  char const *pre = "";
  if ((fl & FfAlt) && tmp[nd - 1] != '0') pre = base == 16 ? (up ? "0X" : "0x") : base == 8 ? "0" : "";
  int np = 0;
  while (pre[np]) np++;
  int len = nd + np + (sgn ? 1 : 0);
  int pad = width > len ? width - len : 0;
  if (!(fl & (FfLeft | FfZero))) __pad(put, ctx, pad, 32);
  if (sgn) put(ctx, sgn);
  for (int i = 0; i < np; i++) put(ctx, pre[i]);
  if (!(fl & FfLeft) && (fl & FfZero)) __pad(put, ctx, pad, 48);
  while (nd) put(ctx, tmp[--nd]);
  if (fl & FfLeft) __pad(put, ctx, pad, 32); }
