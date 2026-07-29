/* qsort -- ours is a shellsort with n/2 halving gaps, glibc's is a mergesort
 * with a quicksort fallback. NEITHER IS STABLE and the standard does not ask
 * them to be, so every case here sorts DISTINCT keys: a differential on equal
 * keys would compare an ordering neither library promises.
 *
 * the sizes walk past the gap sequence's turns, and the element size is varied
 * because the byte-swap lane is where a size-agnostic sort goes wrong. */
#include <stdlib.h>
#include <string.h>
#include "say.h"

static int cmp_int(void const *a, void const *b)
{
	int x = *(int const *) a, y = *(int const *) b;
	return x < y ? -1 : x > y ? 1 : 0;
}

static int cmp_desc(void const *a, void const *b) { return cmp_int(b, a); }

struct big { long k; char pad[24]; };

static int cmp_big(void const *a, void const *b)
{
	long x = ((struct big const *) a)->k, y = ((struct big const *) b)->k;
	return x < y ? -1 : x > y ? 1 : 0;
}

static int cmp_str(void const *a, void const *b)
{
	return strcmp(*(char const *const *) a, *(char const *const *) b);
}

static void show(char const *nm, int const *v, int n)
{
	for (int i = 0; i < n; i++) say_n(nm, v[i]);
}

int main(void)
{
	int v[64];

	/* a scrambler with no library call in it, so the INPUT is identical in
	   both builds however each one's rand() behaves */
	for (int n = 0; n <= 33; n++) {
		for (int i = 0; i < n; i++) v[i] = (i * 37 + 11) % 101;
		qsort(v, (size_t) n, sizeof v[0], cmp_int);
		show("int", v, n);
	}

	/* already sorted, exactly reversed, and a single displaced element --
	   the shapes a gap sequence handles differently */
	for (int i = 0; i < 20; i++) v[i] = i;
	qsort(v, 20, sizeof v[0], cmp_int);
	show("sorted", v, 20);
	for (int i = 0; i < 20; i++) v[i] = 19 - i;
	qsort(v, 20, sizeof v[0], cmp_int);
	show("reversed", v, 20);
	for (int i = 0; i < 20; i++) v[i] = i;
	v[0] = 99;
	qsort(v, 20, sizeof v[0], cmp_int);
	show("displaced", v, 20);

	/* the degenerate counts must not touch anything */
	v[0] = 7; v[1] = 3;
	qsort(v, 0, sizeof v[0], cmp_int);
	show("n0", v, 2);
	qsort(v, 1, sizeof v[0], cmp_int);
	show("n1", v, 2);
	qsort(v, 2, sizeof v[0], cmp_int);
	show("n2", v, 2);

	/* a descending comparator: the order comes from cmp, not from the sort */
	for (int i = 0; i < 12; i++) v[i] = (i * 5 + 3) % 13;
	qsort(v, 12, sizeof v[0], cmp_desc);
	show("desc", v, 12);

	/* a 32-byte element: the swap lane moves whole objects, payload included */
	{ struct big g[16];
	  for (int i = 0; i < 16; i++) {
		g[i].k = (i * 7 + 5) % 23;
		memset(g[i].pad, 'a' + i, sizeof g[i].pad); }
	  qsort(g, 16, sizeof g[0], cmp_big);
	  for (int i = 0; i < 16; i++) { say_n("big", g[i].k); say_n("big", (long) g[i].pad[0]); } }

	/* pointers, sorted by what they point at */
	{ char const *w[7];
	  w[0] = "pear"; w[1] = "apple"; w[2] = "fig"; w[3] = "date";
	  w[4] = "cherry"; w[5] = "banana"; w[6] = "grape";
	  qsort(w, 7, sizeof w[0], cmp_str);
	  for (int i = 0; i < 7; i++) say_s("str", w[i]); }

	return 0;
}
