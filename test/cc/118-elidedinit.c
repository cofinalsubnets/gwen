/* an elided initializer fills whole ELEMENTS, and an implied [] bound counts those.
 *
 * The bound divided the item count by the element's scalar LEAVES, which only an
 * array element type can answer -- so a struct element got one item apiece and
 * `PT cases[] = { 1,2,3,4,5,6,7, 8,9,10,11,12,13,14 };` over a seven-member PT
 * sized to 14 where C says 2 (c-testsuite's 00205). The bytes were laid right and
 * only the length was wrong, which is the half every loop over the table reads.
 *
 * Leaf-counting cannot say the rule (C 6.7.9p20). A union takes ONE initializer,
 * for its first member; a bitfield member takes one; and a char array takes one
 * item when that item is a STRING -- which the array lane was missing too, so a
 * plain string table `char m[][8] = {"ab","cd","ef"}` sized to a single row.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

typedef struct { int a, b, c, d, e, f, g; } PT;
PT cases[] = { 1,2,3,4,5,6,7, 8,9,10,11,12,13,14 };

struct N { int x; struct { int y, z; } n; };
struct N nest[] = { 1,2,3, 4,5,6 };

struct B { unsigned a:1; unsigned b:1; };
struct B bits[] = { 1,0, 1,1, 0,1 };

union U { int a; int b; };
union U uni[] = { 1, 2, 3 };

char names[][8] = { "ab", "cd", "ef" };

struct S { char n[4]; int v; };
struct S tab[] = { "ab", 1, "cd", 2 };

int rows[][2] = { 1,2,3,4,5,6 };          /* the array lane, unregressed */
int deep[][2][3] = { 1,2,3,4,5,6,7,8,9,10,11,12 };
PT braced[] = { {1,2,3,4,5,6,7}, {8,9,10,11,12,13,14} };   /* an item an element */

#define N(a) ((int)(sizeof(a) / sizeof(*(a))))

int main(void)
{
	int r = 0;

	r += N(cases) == 2;
	r += cases[1].a == 8 && cases[1].g == 14;   /* the bytes were never the wrong half */
	r += N(nest) == 2;
	r += nest[1].n.z == 6;
	r += N(bits) == 3;      /* the VALUES are 120-bfinit's -- a bitfield brace fills wrong */
	r += N(uni) == 3;
	r += uni[2].a == 3;
	r += N(names) == 3;
	r += names[2][0] == 'e' && names[2][1] == 'f' && names[2][2] == 0;
	r += N(tab) == 2;
	r += tab[1].n[0] == 'c' && tab[1].v == 2;
	r += N(rows) == 3;
	r += N(deep) == 2;
	r += N(braced) == 2;
	return r;
}
