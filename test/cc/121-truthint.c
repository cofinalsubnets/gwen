/* a truth value is an int: !, the six relations, && and ||, whatever the
 * operand (C11 6.5.3.3p5, 6.5.8p6, 6.5.9p3, 6.5.13p3, 6.5.14p3).
 *
 * Every truth-valued node came out typed long, so sizeof(!a) answered 8
 * (c-testsuite's 00178) -- and a truth value in an array bound could not
 * fold at parse, so the bound refused. The values were always right (a
 * truth is 0 or 1 at any width); the TYPE is what these hold.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

char c = 1;
long l = 2;
double d = 0.5;
char *p = &c;

char pad[sizeof(0 == 0) + 1];             /* a truth value folds into a bound now */

int main(void)
{
	int r = 0;

	r += sizeof(!c) == sizeof(int);
	r += sizeof(!l) == sizeof(int);
	r += sizeof(!d) == sizeof(int);   /* a double's truth is an int too */
	r += sizeof(!p) == sizeof(int);   /* and a pointer's */
	r += sizeof(c == l) == sizeof(int);
	r += sizeof(c != d) == sizeof(int);
	r += sizeof(l < c) == sizeof(int);
	r += sizeof(l >= l) == sizeof(int);
	r += sizeof(c && d) == sizeof(int);
	r += sizeof(p || l) == sizeof(int);
	r += sizeof(pad) == 5;
	r += (!0) == 1;                   /* the values, unregressed */
	r += (!5) == 0;
	r += (l < c) == 0;
	r += (c && l) == 1;
	return r;
}
