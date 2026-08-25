/* a block-scope `extern` declaration names the FILE-SCOPE object (C11 6.2.2p4):
 * it lays no slot and takes no local name, so a read sees what the rest of the
 * TU wrote and a write is seen by it. doom's d_net.c says
 * `extern boolean advancedemo;` inside a function and tested a slot nobody
 * wrote; every value below was garbage before that was fixed. */
#include <stdio.h>

int seen_early = 3;
long wide = 100;
char arr[4] = { 1, 2, 3, 4 };
struct pt { int x, y; };
struct pt org = { 11, 22 };

static int read_early(void) { extern int seen_early; return seen_early; }
static int bump_early(void) { extern int seen_early; return ++seen_early; }
/* declared here, DEFINED at the foot of the file */
static int read_late(void) { extern int seen_late; return seen_late; }
static long read_wide(void) { extern long wide; return wide + 1; }
static int read_arr(void) { extern char arr[]; return arr[0] + arr[3]; }
static int read_org(void) { extern struct pt org; return org.x * 10 + org.y; }
/* nested block, and a shadowing local in a sibling block that must NOT leak */
static int nested(int n) {
  int total = 0;
  if (n) { extern int seen_early; total += seen_early; }
  { int seen_early = 1000; total += seen_early; }
  { extern int seen_early; total += seen_early; }
  return total; }
/* a block-scope extern FUNCTION declaration was always right -- keep it honest */
static int call_it(void) { extern int seen_late_twice(void); return seen_late_twice(); }

int seen_late = 40;
int seen_late_twice(void) { return seen_late * 2; }

int main(void) {
  int a = read_early();          /* 3 */
  int b = bump_early();          /* 4, and seen_early is now 4 */
  int c = seen_early;            /* 4 -- the write went to the global */
  int d = read_late();           /* 40 */
  long e = read_wide();          /* 101 */
  int f = read_arr();            /* 5 */
  int g = read_org();            /* 132 */
  int h = nested(1);             /* 4 + 1000 + 4 */
  int i = call_it();             /* 80 */
  printf("%d %d %d %d %ld %d %d %d %d\n", a, b, c, d, e, f, g, h, i);
  return (a + b + c + d + (int) e + f + g + h + i) & 0x7f; }
