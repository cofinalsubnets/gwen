/* designator chains (C99 6.7.9): .a.b = v, .a[i] = v, [i].f = v -- and the
 * backward-jumping designator ({ [2] = 9, [0] = 1 }), which must not end the
 * fill walk. globals image (placement-sorted), locals zero-then-store; a
 * consecutive same-head run (.lim[0], .lim[1]) coalesces into ONE member
 * image -- musl's sigaction/setrlimit/getaddrinfo shapes. */

struct in { int b; int c; };
struct sa { long h; struct in nest; int flags; long lim[2]; };
struct pt { int x, y; };

static struct sa G = { .nest.b = 3, .nest.c = 4, .lim[0] = 10, .lim[1] = 20, .flags = 7 };
static struct pt grid[3] = { [2].y = 9, [0] = { 1, 2 } };
static int back[4] = { [2] = 5, [0] = 1, 3 };   /* the backjump, then positional: back[1] = 3 */

union uv { void (*fp)(void); int iv; };
struct sig { union uv u; int fl; };
static struct sig S = { .u.iv = 6, .fl = 2 };   /* through a union member, musl's sa_handler shape */

int main(void)
{
	struct sa s = { .h = 5, .nest.c = 6, .lim[1] = 30 };
	struct pt lp[3] = { [2].x = 8, [0].y = 4 };
	int g = (int)G.nest.b + G.nest.c + (int)G.lim[0] + (int)G.lim[1] + G.flags;     /* 44 */
	int a = grid[2].y + grid[0].x + grid[0].y + grid[1].x + grid[2].x;              /* 12 */
	int b = back[0] + back[1] + back[2] + back[3];                                  /* 9 */
	int l = (int)s.h + s.nest.b + s.nest.c + (int)s.lim[0] + (int)s.lim[1];         /* 41 */
	int p = lp[2].x + lp[0].y + lp[0].x + lp[1].y;                                  /* 12 */
	int u = S.u.iv + S.fl;                                                          /* 8 */
	return g + a + b + l + p + u;                                                   /* 126 */
}
