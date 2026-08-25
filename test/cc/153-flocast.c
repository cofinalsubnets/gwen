/* a float constant expression through a cast to an integer type, in a STATIC
 * initializer: C11 6.6p6's one float an integer constant expression may hold,
 * and it truncates toward zero. doom's am_map.c writes the idiom
 * ((int)(-.867 * (1 << 16))) and nothing here did. */
static int a = (int) (0.5 * 4);
static int b = (int) 1.5;
static int c = (int) (-0.867 * (1 << 16));
static int d = (int) (0.867 * (1 << 16));
static long e = (long) (2.5 * 3);
static short f = (short) (-1.9);
static unsigned g = (unsigned) (3.7);
static int h = (int) (7.0 / 2.0);
static int i = (int) -(1.5 + 1.5);
static int j = (int) (1e6 * 1e-3);
static _Bool k = (_Bool) 0.0;
static _Bool l = (_Bool) 0.25;
static int m[2] = { (int) (-0.5 * 4), (int) (1.5 * 2) };

/* the same fold in an array DIMENSION, where it has to be an integer constant
 * expression rather than merely an arithmetic one */
static char dim[(int) (3.9)];

struct s { int p; long q; };
static struct s t = { (int) (2.5 + 2.5), (long) (10.5 / 2) };

int main(void) {
  long r = a + b + c + d + e + f + g + h + i + j + k + l + m[0] + m[1]
         + (long) sizeof dim + t.p + t.q;
  return (int) (r & 0x7f); }
