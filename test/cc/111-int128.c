/* the wide pair: __int128 / unsigned __int128 (x64 only -- gen's d128 lane).
 *
 * the lane's whole surface, one check each: widening 64x64 products (the ONE
 * mulq the bignum kernel lives on), general 128x128 low products, carry chains
 * across the half boundary, every compare at both signednesses (value AND
 * branch positions -- cgbin settles 0/1 via c128, cbranch rides the same op
 * through cmp/br), constant shifts on each side of 64, variable shifts (the
 * branchy v-forms, 0..127 exact), the divq two-step for u128/u64 divide and
 * remainder, negation's borrow, truth off either half, casts in and out, the
 * folded (double)((u128)1<<64) constant, wide ++/-- (step, store, step back),
 * compound assigns, and a static function RETURNING the pair (rdx:rax, the
 * SysV i128 return).
 *
 * every other target refuses this file loud (ccarch.sh expects that, like
 * 100-complex): parse accepts the type everywhere, gen carries it on x64.
 *
 * Each check contributes 1, so the exit code IS the number that passed. */

typedef unsigned __int128 u128;
typedef __int128 s128;

static u128 mk(unsigned long hi, unsigned long lo) { return ((u128) hi << 64) | lo; }

int main(void) {
 int ok = 0;
 unsigned long a = 0xdeadbeefcafebabeUL, b = 0x123456789abcdef1UL;

 /* widening product: (u128)u64 * u64 is one mulq */
 u128 p = (u128) a * b;
 if ((unsigned long) (p >> 64) == 0xfd5bdeeeb2a01d8UL) ok++;
 if ((unsigned long) p == 0xca165e3e6f4690deUL) ok++;

 /* general 128x128 low product (cross terms) */
 u128 m = p * (u128) 3;
 if ((unsigned long) (m >> 64) == 0x2f8139ccc17e058aUL) ok++;
 if ((unsigned long) m == 0x5e431abb4dd3b29aUL) ok++;

 /* add/sub carry across the boundary */
 u128 c1 = mk(0, 0xffffffffffffffffUL) + 1;
 if ((unsigned long) (c1 >> 64) == 1 && (unsigned long) c1 == 0) ok++;
 u128 c2 = mk(1, 0) - 1;
 if ((unsigned long) (c2 >> 64) == 0 && (unsigned long) c2 == 0xffffffffffffffffUL) ok++;
 if (c1 - c2 == 1) ok++;

 /* unsigned compares, value position (0/1 via c128) */
 u128 B = (u128) 1 << 64;
 if (((u128) 5 < B) == 1) ok++;
 if ((p >= B) == 1) ok++;
 if ((B <= p) == 1) ok++;
 if ((B > p - p) == 1) ok++;
 if ((B == mk(1, 0)) == 1) ok++;
 if ((B != mk(1, 1)) == 1) ok++;

 /* signed compares + negation's borrow */
 s128 n = -(s128) 7;
 if (n < 0) ok++;
 if ((long) n == -7) ok++;
 if (-(-n) == n) ok++;
 if ((s128) B > n) ok++;
 s128 nb = -(s128) B;                            /* borrow rides the halves */
 if ((long) (nb >> 64) == -1 && (unsigned long) nb == 0) ok++;

 /* branch position (cbranch's wide arms), both senses, const + general */
 if (p > B) ok++; else ok--;
 if (B < (u128) 5) ok--; else ok++;
 if (n < 1) ok++; else ok--;
 u128 q9 = B;
 while (q9 >= B) { q9 = q9 - 1; break; } if (q9 == B - 1) ok++;

 /* constant shifts: below, at, and past 64 -- both directions, sar's flood */
 if (((u128) a << 13 >> 13) == a) ok++;
 if ((B >> 64) == 1) ok++;
 if ((mk(0x8000000000000000UL, 0) >> 100) == (0x8000000000000000UL >> 36)) ok++;
 if (((u128) 1 << 100 >> 36) == B) ok++;
 if (((s128) nb >> 64) == -1) ok++;
 if (((s128) nb >> 127) == -1) ok++;

 /* variable shifts: the branchy v-forms */
 int sh = 13;
 u128 v = ((u128) a << sh) | ((u128) b >> (64 - sh));
 if ((unsigned long) (v >> 64) == 0x1bd5UL) ok++;
 if ((unsigned long) v == 0xb7ddf95fd757c246UL) ok++;
 sh = 64; if ((mk(0, a) << sh) == mk(a, 0)) ok++;
 sh = 99; if ((mk(a, 0) >> sh) == (u128) (a >> 35)) ok++;

 /* logicals, pairwise */
 if (((p & B) != 0) == 0) ok++;
 if (((p | B) >> 64) == (p >> 64 | 1)) ok++;
 if ((p ^ p) == 0) ok++;
 if ((~p & p) == 0) ok++;

 /* truth off either half */
 if (mk(1, 0)) ok++;
 if (mk(0, 1)) ok++;
 if (!mk(0, 0)) ok++;

 /* the divq two-step: u128 / u64 and %, checked by the division algebra */
 u128 num = mk(3, 0x8000000000000001UL);
 u128 qq = num / 7, rr = num % 7;
 if (qq * 7 + rr == num) ok++;
 if (rr < 7) ok++;
 if (qq > B) ok++;                                /* the quotient really is wide */
 if ((mk(0, 90) / 9) == 10 && (mk(0, 90) % 9) == 0) ok++;

 /* wide ++/-- : old answers, the slot steps */
 u128 pd = B;
 pd--; if (pd == B - 1) ok++;
 pd++; if (pd == B) ok++;

 /* compound assigns ride the binop lanes */
 pd += 5; if (pd == B + 5) ok++;
 pd -= 5; if (pd == B) ok++;
 pd >>= 4; pd <<= 4; if (pd == B) ok++;

 /* casts: truncation keeps the lo, widening keeps the sign */
 if ((unsigned long) mk(9, 42) == 42) ok++;
 if ((unsigned int) mk(9, 0x100000007UL) == 7) ok++;
 if ((s128) (long) -3 == -(s128) 3) ok++;
 if ((u128) (unsigned long) a == (u128) a) ok++;

 /* the folded constant double: 2^64 exactly */
 if ((double) ((u128) 1 << 64) == 18446744073709551616.0) ok++;

 /* a function returning the pair */
 if (mk(a, b) == (((u128) a << 64) | b)) ok++;

 /* sizeof */
 if (sizeof (u128) == 16 && sizeof (s128) == 16) ok++;

 return ok; }
