/* a compare or divide takes the COMMON converted type (C11 6.3.1.8), not
 * either-operand's: a lone unsigned int against int/long fits IN long and goes
 * SIGNED; int against uint meets at uint, unsigned and NARROWED to 32 bits
 * (the registers ride wider than the compare); ulong stays unsigned. and an
 * unsuffixed hex constant climbs the unsigned rungs of C11 6.4.4.1 --
 * 0xffffffff is a uint, 0xffffffffffffffff a ulong (c-testsuite's 00104).
 *
 * Every one of these compared unsigned whenever EITHER side was unsigned, at
 * full register width, so `l < u` read -6 as huge and `x != 0xffffffff` never
 * saw the int convert. A case label converts to the control's type too.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

unsigned gu = 5u;                 /* globals, so nothing folds at parse */
long gl = -6;
int gx = -1;
unsigned long gul = 0xffffffffffffffff;

static unsigned big(void) { return 0xffffffffu; }

int main(void)
{
	unsigned u = gu;
	long l = gl;
	int x = gx;
	unsigned v = big();
	int r = 0;

	r += l < u;                       /* uint fits in long: signed, -6 < 5 */
	r += u > l;
	r += l / u == -1;                 /* the divide takes the same law */
	r += l % u == -1;
	r += (x != 0xffffffff) == 0;      /* the 00104 shape: x converts to uint */
	r += x == 0xffffffffu;            /* its suffixed twin */
	r += v == ~0;                     /* ~0 reads int, converts to all-ones uint */
	r += u != ~0;
	r += (u > -1) == 0;               /* -1 converts to uint: nothing is above it */
	r += (v != -1) == 0;
	r += l != 0xffffffffffffffff;     /* ulong side: l sign-extends, 0xff..fa */
	r += gul == 0xffffffffffffffff;
	r += l == -6 && x == -1;          /* the signed lanes, unregressed */
	r += u / 2u == 2;
	r += sizeof(0xffffffff) == 4;     /* the ladder's types */
	r += sizeof(0xffffffffffffffff) == 8;
	switch (v) {
	case -1: r += 1; break;           /* the case converts to the control's uint */
	default: break;
	}
	return r;
}
