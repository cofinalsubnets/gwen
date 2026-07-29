/* ASSIGNING a double to an integer object truncates.
 *
 * this is the plainest C there is -- `long v; v = d;` -- and mooncc stored the
 * raw IEEE BIT PATTERN. v = 3.75 landed 4615626668101337088, which is
 * 0x400E000000000000. an `int` target was no better: the narrowing lane rides
 * tor0, which MOVES rather than converts, so it kept the low 32 bits of those
 * same bits and answered 0.
 *
 * the store-direct lane (a plain variable's address is a static frame offset, so
 * the value stores from wherever it sits) guarded against a float TARGET and
 * never looked at the float SOURCE.
 *
 * ⚠ WHY IT SURVIVED THIS LONG: the DECLARING form was always right.
 * `long v = d;` goes through cgdecl's own store, which converts. only the
 * separated assignment was broken -- and the declaring form is the one everybody
 * writes, so the whole corpus, the 108-program battery and every gate stayed
 * green. both forms are asserted below, side by side, for exactly that reason.
 *
 * found while fixing the double->unsigned-64 conversion (108-d2u.c); the
 * signedness bug is what led to this lane, and this one was underneath it and
 * worse -- it hits every integer width, signed and unsigned alike. */

static long g_l;
static unsigned long g_ul;
static int g_i;

static long asn_long(double d)       { long v;          v = d; return v; }
static unsigned long asn_ulong(double d) { unsigned long v; v = d; return v; }
static int asn_int(double d)         { int v;           v = d; return v; }
static short asn_short(double d)     { short v;         v = d; return v; }
static char asn_char(double d)       { char v;          v = d; return v; }

static long init_long(double d)      { long v = d;      return v; }   /* the control: always worked */

static long asn_global(double d)     { g_l = d;  return g_l; }        /* the glo lane, not loc */
static unsigned long asn_gul(double d) { g_ul = d; return g_ul; }
static int asn_gint(double d)        { g_i = d;  return g_i; }

static long asn_ptr(double d)        { long v; long *p = &v; *p = d; return v; }  /* computed address */

int main(void)
{
	int r = 0;

	/* ⚠ THE CASE THAT WAS BROKEN: a separated assignment, every width */
	r += asn_long(3.75) == 3L;
	r += asn_ulong(3.75) == 3UL;
	r += asn_int(3.75) == 3;
	r += asn_short(3.75) == 3;
	r += asn_char(3.75) == 3;

	/* the declaring form, which always converted -- both must agree */
	r += init_long(3.75) == 3L;
	r += init_long(3.75) == asn_long(3.75);

	/* truncation is toward zero, including negatives */
	r += asn_long(-3.75) == -3L;
	r += asn_int(-3.75) == -3;

	/* a global target takes the other half of the same lane */
	r += asn_global(3.75) == 3L;
	r += asn_gul(3.75) == 3UL;
	r += asn_gint(3.75) == 3;

	/* through a pointer: the computed-address store */
	r += asn_ptr(3.75) == 3L;

	/* larger magnitudes, still exact */
	r += asn_long(1.0e15) == 1000000000000000L;
	r += asn_ulong(1.0e19) == 10000000000000000000UL;   /* and past 2^63, cf 108-d2u.c */

	/* a zero and a value below one both land on 0, not on their bits */
	r += asn_long(0.0) == 0L;
	r += asn_long(0.5) == 0L;
	r += asn_int(0.5) == 0;

	return r;
}
