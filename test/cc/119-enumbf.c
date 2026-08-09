/* an enum with no negative enumerator has an UNSIGNED underlying type.
 *
 * Every enum canonicalized to `int`, so an enum bit-field extracted with an
 * arithmetic shift: `enum tree_code code : 8` holding 148 loaded back -108, the
 * switch over it took `default`, and nothing said a word (c-testsuite's 00218).
 *
 * gcc's rule, and the one the corpus holds us to: unsigned when no enumerator is
 * negative, signed when one is. It rides the TAG, so a later `enum c` by tag
 * alone extracts the same way as the definition did -- which is how 00218 spells
 * it, and an anonymous enum written straight into the member must work too.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

enum pos { P0 = 0, P148 = 148, P149 };            /* bit 7 set, none negative */
enum neg { N1 = -1, N148 = 148 };                 /* one negative: stays signed */

struct bf {
	enum pos  a : 8;
	enum neg  b : 9;
	unsigned  c : 3;
	int       d : 4;                          /* the plain lanes, unregressed */
};

struct anon { enum { A148 = 148, A149 } y : 8; };

static int widen(enum pos p) { return p; }

int main(void)
{
	struct bf s;
	struct anon t;
	enum pos p = P148;
	enum neg n = N1;
	int r = 0;

	s.a = P148; s.b = N1; s.c = 5; s.d = -3;
	t.y = A148;

	r += s.a == 148;                          /* the 00218 shape */
	r += s.b == -1;                           /* a negative enumerator keeps the sign */
	r += s.c == 5;
	r += s.d == -3;
	r += t.y == 148;
	r += p == 148;
	r += n < 0;
	r += widen(P149) == 149;
	r += sizeof(enum pos) == 4;               /* the width does not move */
	switch (s.a) {
	case P148: r += 1; break;                 /* what took `default` before */
	default:   break;
	}
	return r;
}
