/* an [] compound literal completes its bound from its initializer (C11 6.5.2.5p22):
 * sizeof folded to 0 and indexing read garbage until the clit site asked initcount,
 * the same door the [] declarators use.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

extern int printf(const char *, ...);

int main(void)
{
	int *p = (int[]){10, 20, 30};
	char *s = (char[]){"hi"};
	int r = 0;
	r += sizeof((int[]){1, 2}) == 8;
	r += sizeof((int[]){[3] = 1}) == 16;
	r += sizeof((char[]){"hi"}) == 3;
	r += sizeof((int[5]){1}) == 20;      /* an explicit bound stays its own */
	r += p[0] == 10 && p[1] == 20 && p[2] == 30;
	r += s[0] == 'h' && s[2] == 0;
	return r;
}
