/* C11 6.2.1p7 -- a declarator's scope begins at the END OF ITS DECLARATOR, so the
 * initializers that follow it in the SAME declaration already see it. 78-enumshadow.c
 * covers the block case (a local hides an enum constant for the rest of its block);
 * this covers the declaration itself, which is a separate scope point and was the one
 * that broke: `unsigned long M = f(), N = g(), n = M * N;` folded N to the enum
 * constant and compiled `n = M * 4`. src/love.c has `enum { N = 4 }` in one function
 * and N as a local in another, so lvm_outer's outer product wrote 4*M of its M*N
 * elements and handed back a tray with uninitialized heap in the tail.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

extern int printf(const char *, ...);

enum { N = 4, K = 7 };
typedef int T;

static unsigned long idf(unsigned long x) { return x; }

/* the shape that broke: the product reads BOTH earlier declarators */
static unsigned long prod(unsigned long a, unsigned long b) {
  unsigned long M = idf(a), N = idf(b), n = M * N, rank = a + b;
  return n + 0 * rank;
}

/* a later declarator reading the one before it, plainly */
static int chain(int a) {
  int x = a, y = x + 1, z = y + 1;
  return z;
}

/* a name that is a TYPEDEF at file scope, reused as a local in one declaration */
static int tdef(int a) {
  int T = a, u = T * 2;
  return u;
}

/* the enum constant is still itself where nothing hides it */
static int unshadowed(void) { return N * 10 + K; }

/* and a local shadow ends with its block */
static int blockends(void) {
  int r = 0;
  { int N = 9; r += N; }
  r += N;
  return r;
}

int main(void) {
  int ok = 0;
  if (prod(3, 5) == 15) ok++;                 /* not 3*4 */
  if (prod(7, 9) == 63) ok++;
  if (prod(1, 5) == 5) ok++;
  if (chain(1) == 3) ok++;
  if (tdef(6) == 12) ok++;
  if (unshadowed() == 47) ok++;
  if (blockends() == 13) ok++;                /* 9 + 4 */
  printf("prod(3,5)=%lu prod(7,9)=%lu prod(1,5)=%lu chain=%d tdef=%d un=%d be=%d ok=%d\n",
         prod(3, 5), prod(7, 9), prod(1, 5), chain(1), tdef(6), unshadowed(), blockends(), ok);
  return ok;
}
