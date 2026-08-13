/* a tentative definition COMPLETES an earlier `extern T x[];`.
 *
 * C's composite-type rule (6.2.7): where one declaration of an object has an
 * array type of unknown size and another has a size, the composite type has
 * the size. gen's tentative rule (6.9.2) was keeping whichever entry already
 * stood unless the newcomer carried an INITIALIZER -- so
 *
 *     extern char a[];      <- entered at size 0
 *     char a[1024];         <- tentative, no initializer: entry kept
 *
 * left `a` laid in .bss at SIZE ZERO. Which is not a refusal and not a wrong
 * answer at the site: it is the NEXT global getting the same address. Two
 * such arrays alias completely, and the link succeeds.
 *
 * ⚠ this is exactly the shape a header makes, so it is the common one rather
 * than a corner: gzip.h declares `extern char ifname[], ofname[];` and gzip.c
 * defines both. With them aliased, gzip 1.2.4 built, linked, ran, printed its
 * version and its compilation options -- and then opened its OUTPUT name for
 * reading, because setting ofname had overwritten ifname. `gzip -c f` died at
 * open("stdout") and `gzip f` at open("V.gz"): a plausible-looking errno from
 * a program whose first four syscalls were all correct.
 *
 * the sizes here are the two gzip uses. The witness is that a write through
 * one name leaves the other alone -- and the trailing scalars catch the
 * milder version where the arrays are distinct but one is laid short. */
#include <stdio.h>

extern char a[];
extern char b[];
extern int  tail;

char a[1024];
char b[1024];
int  tail = 0x5a5a;

/* the same shape the other way round: complete FIRST, then the open
 * declaration, which must not shrink what is already known. */
char c[256];
extern char c[];

int main(void)
{
	int i, bad = 0;

	for (i = 0; i < 1024; i++) { a[i] = 0; b[i] = 0; }
	for (i = 0; i < 256; i++) c[i] = 0;

	/* fill a entirely; b and the scalar past it must not move */
	for (i = 0; i < 1024; i++) a[i] = (char)(i & 0x7f);
	for (i = 0; i < 1024; i++) if (b[i] != 0) bad++;
	if (tail != 0x5a5a) bad += 1000;

	/* now fill b entirely; a must still read back what it was given */
	for (i = 0; i < 1024; i++) b[i] = (char)((i * 3) & 0x7f);
	for (i = 0; i < 1024; i++) if (a[i] != (char)(i & 0x7f)) bad++;
	if (tail != 0x5a5a) bad += 1000;

	/* and the complete-then-open direction */
	for (i = 0; i < 256; i++) c[i] = (char)(i & 0x3f);
	for (i = 0; i < 256; i++) if (c[i] != (char)(i & 0x3f)) bad++;

	printf("%d %d %d %d %d\n", bad, (int)a[7], (int)b[7], (int)c[7], tail);
	return bad;
}
