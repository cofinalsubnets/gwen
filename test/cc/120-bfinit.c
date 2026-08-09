/* a brace initializer over a bitfield member lays the bits, not the unit.
 *
 * The fill reached a bitfield member with the member's own (bitfield uty bitoff w)
 * type and stored the whole declared unit flat -- unshifted, unmasked, and the
 * neighbours sharing that unit gone. `struct B { unsigned a:1, b:1; } g = {0,1}`
 * read back a=1 b=0. Assignment was always right, which is why it stood: nothing
 * in the tree braces a bitfield, it sets them.
 *
 * Both lanes were wrong and they are separate code: the frame fill emits a
 * read-modify-write now, and the static image merges the members of one unit into
 * that unit's VALUE -- two placements at one offset made the image lay the second
 * after the first, so a two-bitfield struct also imaged eight bytes wide.
 *
 * No corpus found this; it fell out of writing 118's regression.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

struct B { unsigned a:1; unsigned b:1; };
struct C { unsigned a:3; unsigned b:5; unsigned c:24; };
struct D { int a:4; int b:4; };                    /* signed: the value sign-extends back */
struct E { unsigned char pre; unsigned a:3; unsigned b:5; int post; };
struct F { unsigned long w:40; unsigned long x:24; };   /* one 8-byte unit, wide fields */
struct G { unsigned a:1; unsigned b:1; unsigned c:1; };

struct B sb = { 0, 1 };
struct C sc = { 5, 21, 1000000 };
struct D sd = { -3, 7 };
struct E se = { 200, 5, 21, -9 };
struct F sf = { 1099511627775UL, 16777215UL };
struct G sg = { .c = 1 };                          /* designated, and a & b stay zero */
struct B sarr[] = { {1,0}, {1,1}, {0,1} };
struct B sflat[] = { 1,0, 1,1, 0,1 };              /* the same, braces elided */
struct C spartial = { 7 };                         /* b and c unmentioned -> zero */

int main(void)
{
	struct B lb = { 0, 1 };
	struct C lc = { 5, 21, 1000000 };
	struct D ld = { -3, 7 };
	struct E le = { 200, 5, 21, -9 };
	struct G lg = { .c = 1 };
	struct B larr[] = { {1,0}, {0,1} };
	int r = 0;

	r += sb.a == 0 && sb.b == 1;
	r += sizeof sb == 4;                       /* the image is a unit wide, not two */
	r += sc.a == 5 && sc.b == 21 && sc.c == 1000000;
	r += sd.a == -3 && sd.b == 7;
	r += se.pre == 200 && se.a == 5 && se.b == 21 && se.post == -9;
	r += sf.w == 1099511627775UL && sf.x == 16777215UL;
	r += sg.a == 0 && sg.b == 0 && sg.c == 1;
	r += sarr[0].a == 1 && sarr[0].b == 0;
	r += sarr[2].a == 0 && sarr[2].b == 1;
	r += sflat[1].a == 1 && sflat[1].b == 1;
	r += sflat[2].a == 0 && sflat[2].b == 1;
	r += spartial.a == 7 && spartial.b == 0 && spartial.c == 0;

	r += lb.a == 0 && lb.b == 1;
	r += lc.a == 5 && lc.b == 21 && lc.c == 1000000;
	r += ld.a == -3 && ld.b == 7;
	r += le.pre == 200 && le.a == 5 && le.b == 21 && le.post == -9;
	r += lg.a == 0 && lg.b == 0 && lg.c == 1;
	r += larr[0].a == 1 && larr[0].b == 0;
	r += larr[1].a == 0 && larr[1].b == 1;

	lb.a = 1;                                  /* assignment beside it, unregressed */
	r += lb.a == 1 && lb.b == 1;
	return r;
}
