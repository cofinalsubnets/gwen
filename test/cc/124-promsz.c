/* the typing door (parse.l pprom/puac): sizeof over PROMOTED arithmetic answers
 * C's type, not the register's -- char+int is an int (4), shorts promote, ~ and
 * unary minus promote, a shift wears its promoted left type alone, ?: arms meet
 * by the usual conversions, float wins its lane. gen's cmpu consumes the same
 * door, so compare/divide signedness and these sizes cannot drift apart.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

char c; short s; unsigned short us; int x; unsigned u; long l; float f;

int main(void)
{
	__typeof__(c + x) tx;             /* typeof over arithmetic types now too */

	int r = 0;
	r += sizeof(c + x) == 4;
	r += sizeof(s + s) == 4;
	r += sizeof(us + us) == 4;        /* ushort fits in int: SIGNED promote */
	r += sizeof(~c) == 4;
	r += sizeof(-s) == 4;
	r += sizeof(c << 1) == 4;
	r += sizeof(l << c) == sizeof(long);   /* the left side alone */
	r += sizeof(x ? c : s) == 4;
	r += sizeof(f + c) == 4;          /* float wins the lane, c converts IN */
	r += sizeof(f + 1.0) == 8;        /* double wins over float */
	r += sizeof(u + c) == 4;
	r += sizeof(l + u) == sizeof(long);
	r += sizeof(&c + 1) == sizeof(void *);  /* ptr + int decays and stays a pointer */
	r += sizeof(tx) == 4;
	r += (char)(c + 1) == 1 && (~c | 1) == -1;   /* the values ride unchanged */
	return r;
}
