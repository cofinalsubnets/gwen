/* conditional compilation: #if const-expr, #ifdef/#ifndef, #elif */
#define LEVEL 3
#if LEVEL > 2
#define BONUS 20
#else
#define BONUS 0
#endif

#ifndef MISSING
#define HAVE 5
#endif

int main() {
  int r = 0;
#ifdef LEVEL
  r = r + LEVEL;      /* 3 */
#endif
  r = r + BONUS;      /* 20 */
  r = r + HAVE;       /* 5 */
#if 2 + 3 * 4 == 14
  r = r + 1;          /* 1 */
#elif 1
  r = r + 100;
#endif
/* suffixed literals evaluate by VALUE: ll/LL once lexed as number + identifier,
 * so 5ULL == 5 read false and love.c's own width probe (UINTPTR_MAX == UINT64_MAX,
 * a UL against a ULL) took NO branch */
#if 5ULL == 5 && 7ll == 7
  r = r + 2;          /* 2 */
#endif
#if 18446744073709551615ULL > 0
  r = r + 3;          /* 3 */
#endif
#if 18446744073709551615UL == 18446744073709551615ULL
  r = r + 4;          /* 4 */
#endif
  return r;           /* 3+20+5+1+2+3+4 = 38 */
}
