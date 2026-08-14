/* stdio's BUFFERING edges -- the fill boundary, at every alignment.
 *
 * ⚠ THE BYTES ALONE CANNOT SEE THIS FAMILY. a stream that overruns its buffer
 * writes its payload to the memory just past it and then hands that same
 * contiguous run to write(2) at the next flush, so the output is CORRECT while
 * the heap behind it is not -- the defect that prompted this file emitted 8448
 * byte-identical bytes before the runaway reached an unmapped page. so the
 * buffer is followed by a GUARD that is read back directly: the differential
 * still compares the payload, but the count below is what actually names it.
 *
 * ⚠ setvbuf is called ONCE, before any other use of the stream -- ISO C fixes
 * no behaviour for a second one, and a small buffer is the whole instrument:
 * it puts the boundary 128 times over in 20 kB instead of once in 8 kB.
 *
 * the law: a buffered stream is never LEFT full. filling it exactly must drain
 * it, or the next byte lands one past the end and a `len == cap` fill test
 * never matches again -- unbounded from there. fputc alone cannot reach it
 * (it drains on the byte that fills); it takes an fwrite/fputs landing flush
 * against the end, which is why the sweep pairs the two. */
#include <stdio.h>
#include <string.h>
#include "say.h"

#define CAP 64
#define GUARD 64

/* adjacency is the instrument: all-char members, so no padding may come between */
static struct { char buf[CAP]; char guard[GUARD]; } G;

static int touched(void)
{
	int n = 0;
	for (int i = 0; i < GUARD; i++)
		if (G.guard[i]) n++;
	memset(G.guard, 0, GUARD);
	return n;
}

int main(void)
{
	int over = 0;

	setvbuf(stdout, G.buf, _IOFBF, CAP);
	memset(G.guard, 0, GUARD);

	/* the exact-fill matrix: k single bytes, then a w-byte block. some (k,w)
	   lands the buffer flush against its end, which is the whole point. */
	for (int k = 0; k <= CAP + 4; k++) {
		for (int w = 1; w <= 8; w++) {
			char blk[8];
			for (int i = 0; i < w; i++) blk[i] = (char) ('A' + i);
			for (int i = 0; i < k; i++) putchar('a' + i % 26);
			fwrite(blk, 1, (size_t) w, stdout);
			putchar('!');            /* the byte that would land past the end */
			putchar('\n');
			fflush(stdout);
			over += touched();
		}
	}

	/* the same boundary through fputs, whose length the caller never states */
	for (int k = 0; k <= CAP + 4; k++) {
		for (int i = 0; i < k; i++) putchar('.');
		fputs("wxyz", stdout);
		putchar('!');
		putchar('\n');
		fflush(stdout);
		over += touched();
	}

	/* the direct lane: a block at or over the capacity bypasses the buffer,
	   and must still order itself behind whatever is already sitting in it */
	for (int k = 0; k <= 3; k++) {
		char big[CAP * 2];
		for (int i = 0; i < CAP * 2; i++) big[i] = (char) ('0' + i % 10);
		for (int i = 0; i < k; i++) putchar('<');
		say_u("direct.items", (unsigned long) fwrite(big, 1, CAP, stdout));
		say_u("direct.items", (unsigned long) fwrite(big, 1, CAP * 2, stdout));
		say_u("direct.items", (unsigned long) fwrite(big, CAP, 2, stdout));
		putchar('\n');
		fflush(stdout);
		over += touched();
	}

	/* the answers the standard does fix, at the edges around the boundary */
	say_u("fwrite.zero.count", (unsigned long) fwrite("x", 1, 0, stdout));
	say_u("fwrite.zero.size", (unsigned long) fwrite("x", 0, 1, stdout));
	say_n("fputs.ok", fputs("", stdout) >= 0 ? 0 : -1);
	say_n("fflush.ok", fflush(stdout));
	say_n("ferror", ferror(stdout) ? 1 : 0);

	/* ⚠ the real verdict. zero under any correct libc; nonzero says the stream
	   wrote past the buffer it was handed, whatever the payload above looked like. */
	say_n("guard.bytes.written", over);

	return 0;
}
