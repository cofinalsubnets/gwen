/* the GZIP floor -- host/deflate.c and host/inflate.c through mooncc, gcc and
 * clang, with the three reports diffed and the three builds timed. ccnif.sh
 * drives it.
 *
 * WHY THIS FILE. these two are the widest C in host/ and the least like the
 * rest of the tree: a 64-bit bit accumulator shifted by a runtime count, a
 * table indexed by a masked window, a greedy match finder walking a hash chain,
 * an insertion sort over packed keys, and an eight-in-order copy that is
 * DELIBERATELY not a word move. every one of those is a lane love.c never
 * exercises, and both files are compiled by mooncc in the shipped artifact.
 *
 * ⚠ A ROUND TRIP IS NOT ENOUGH and it is worth saying why. inflate(deflate(x))
 * == x holds under a great many wrong deflates -- any legal stream decodes --
 * so the compressed BYTES are reported too. deflate is a twin held to lib/gz.l
 * at the byte (test/host/gzc.l), so its output is a fixed answer and not a
 * licensed choice: a differing byte is a differing compiler.
 *
 * ⚠ THE SUMMARY IS THIS FILE'S OWN ARITHMETIC. an FNV-1a over the output, not
 * the crc32 next door in host/hash.c -- a summary computed by the code under
 * test can agree with itself while both halves are wrong.
 *
 * ⚠ AND THE MALFORMED STREAMS ARE PART OF THE SUBJECT, not a robustness check.
 * host/inflate.c reproduces gz-puff's answer for a stream that does not
 * describe a code -- first-writer-wins in the table, a zeroed symbol array --
 * so what it answers on garbage is as specified as what it answers on a valid
 * block, and the refusal paths are where the bit reader's edges live. */
#include "../../../host/deflate.c"
#include "../../../host/inflate.c"
#include "stub.h"
#include "say.h"

/* ⚠ NOT rand(): the two builds carry different libcs, so the corpus has to be
 * this file's own arithmetic or the programs do not see the same bytes. */
static unsigned lcg(unsigned *s) { return *s = *s * 1103515245u + 12345u; }

/* SRC holds the widest corpus below AND the timed lane's SPIN bytes; OUT holds a
 * deflate of it, which for incompressible input is the input plus block overhead */
#define SRC 262144u
#define OUT 524288u

static unsigned char src[SRC], out[OUT], back[OUT];
static uint64_t arena[DF_ARENA / 8];

/* an independent digest of a byte range -- see the header */
static uint64_t fnv(unsigned char const *p, uintptr_t n)
{
	uint64_t h = 1469598103934665603ull;
	for (uintptr_t i = 0; i < n; i++) { h ^= p[i]; h *= 1099511628211ull; }
	return h;
}

/* deflate n bytes of src, report the stream, inflate it back, report that */
static int64_t roll(unsigned n)
{
	int64_t want, got, cnt, back_n;

	say_u("in.len", n);
	say_u("in.fnv", fnv(src, n));

	want = df_go(src, n, 0, (uintptr_t) -1, (uint8_t *) arena);
	say_n("df.count", (long) want);
	if (want < 0) return want;

	got = df_go(src, n, out, (uintptr_t) want, (uint8_t *) arena);
	say_n("df.emit", (long) got);
	say_n("df.exact", got == want ? 1 : 0);
	if (got < 0) return got;

	say_u("df.fnv", fnv(out, (uintptr_t) got));
	/* the head in the clear: a divergence in the block header -- the code
	 * lengths, the hlit/hdist/hclen row -- shows here without a bisect */
	say_b("df.head", out, got < 48 ? (size_t) got : 48);

	/* ⚠ the counting pass must answer the same length with the stores
	 * dropped: it is the decode with `out` NULL, and nothing the buffer
	 * holds may reach a branch */
	cnt = inf_run(out, (uintptr_t) got, 0, (uintptr_t) -1);
	say_n("inf.count", (long) cnt);

	memset(back, 0, n ? n : 1);
	back_n = inf_run(out, (uintptr_t) got, back, OUT);
	say_n("inf.len", (long) back_n);
	say_n("inf.same", back_n == (int64_t) n && !memcmp(back, src, n) ? 1 : 0);
	say_u("inf.fnv", fnv(back, back_n > 0 ? (uintptr_t) back_n : 0));

	/* one byte short of the answer: the cap refusal, -2 and not a truncation */
	if (n) {
		say_n("inf.tight", (long) inf_run(out, (uintptr_t) got, back, n - 1));
		say_n("inf.exact", (long) inf_run(out, (uintptr_t) got, back, n));
	}
	return got;
}

static unsigned udec(char const *t)
{
	unsigned v = 0;
	while (*t >= '0' && *t <= '9') v = v * 10 + (unsigned) (*t++ - '0');
	return v;
}

/* THE TIMED LANE -- see nif/sum.c's for why the shell holds the clock.
 *
 * ⚠ THE TWO ROWS ARE DIFFERENT SHAPES and that is the reading. deflate is a
 * hash-chain walk over a 32 KB window: pointer chasing, a byte compare loop,
 * an insertion sort per block. inflate is a bit reader and a table lookup per
 * symbol -- branchy, and almost no arithmetic. a lane behind on one and level
 * on the other is losing to that shape and not to the codegen at large. */
#define SPIN 262144u

static int spin(char const *what, unsigned reps)
{
	unsigned long acc = 0;
	int64_t k, dn;
	unsigned i;
	char const *line = "the love artifact is deterministically reproducible; ";
	unsigned j = 0;
	unsigned t = 1u;
	for (i = 0; i < SPIN; i++) {
		/* text with noise in it: pure text codes to nothing and would time
		 * an empty token stream */
		t = t * 1103515245u + 12345u;
		src[i] = (t >> 16 & 15) ? (unsigned char) line[j] : (unsigned char) (t >> 20);
		if (!line[++j]) j = 0;
	}
	dn = df_go(src, SPIN, out, OUT, (uint8_t *) arena);
	if (dn < 0) return 2;
	for (i = 0; i < reps; i++) {
		switch (*what) {
		case 'd': k = df_go(src, SPIN, out, OUT, (uint8_t *) arena); break;
		case 'c': k = df_go(src, SPIN, 0, (uintptr_t) -1, (uint8_t *) arena); break;
		case 'i': k = inf_run(out, (uintptr_t) dn, back, OUT); break;
		default: return 2;
		}
		if (k < 0) return 2;
		acc += (unsigned long) k;
	}
	say_u("acc", acc);
	return 0;
}

int main(int argc, char **argv)
{
	unsigned s, n;

	if (argc > 1) return spin(argv[1], argc > 2 ? udec(argv[2]) : 16);

	/* --- the empty and the tiny: a block that is all end-of-block, and the
	 * lengths where stored beats both coded spellings --- */
	roll(0);
	src[0] = 'x';
	roll(1);
	for (n = 0; n < 8; n++) src[n] = (unsigned char) ('a' + n);
	roll(8);

	/* --- one byte, thirty thousand times: matches at the 258 ceiling, the
	 * overlapping copy at distance 1, and a token stream that costs almost
	 * nothing to code --- */
	for (n = 0; n < 30000; n++) src[n] = 'q';
	roll(30000);

	/* --- short cycles: distance 2..7 runs, which is the copy loop's tail
	 * lane and the dist ladder's bottom rungs --- */
	for (n = 0; n < 20000; n++) src[n] = (unsigned char) ('A' + n % 7);
	roll(20000);

	/* --- text: real matches at real distances, so the dynamic code wins and
	 * the code-length run coder has runs to code --- */
	{
		static char const line[] =
		 "the love artifact is deterministically reproducible from its source, ";
		unsigned k = 0;
		for (n = 0; n < 24000; n++) {
			src[n] = (unsigned char) line[k];
			if (!line[++k]) k = 0;
		}
		roll(24000);
	}

	/* --- incompressible: every block costed and STORED chosen, which is the
	 * only lane that aligns the sink to a byte and writes LEN/NLEN --- */
	s = 20260822u;
	for (n = 0; n < 40000; n++) src[n] = (unsigned char) (lcg(&s) >> 16);
	roll(40000);

	/* --- a four-letter alphabet: three-byte matches everywhere, most of them
	 * far, which is the far-3 refusal at 4096 and the chain limit at 32 --- */
	s = 7u;
	for (n = 0; n < 32768; n++) src[n] = (unsigned char) ('w' + (lcg(&s) >> 20 & 3));
	roll(32768);

	/* --- past DF_TOKMAX from both sides: the block boundary is a token count,
	 * not a byte count, so a literal-heavy input crosses it early --- */
	s = 99u;
	for (n = 0; n < 40000; n++)
		src[n] = (unsigned char) ((lcg(&s) >> 16) % 3 ? 'z' : (lcg(&s) >> 16));
	roll(16383);
	roll(16384);
	roll(16385);
	roll(40000);

	/* --- a window's worth of history, then a match reaching the far wall:
	 * distance 32768 is the last legal one and its code is the ladder's top --- */
	s = 4242u;
	for (n = 0; n < 300; n++) src[n] = (unsigned char) (lcg(&s) >> 16);
	for (n = 300; n < 33000; n++) src[n] = (unsigned char) ('m' + n % 3);
	for (n = 0; n < 300; n++) src[33000 + n] = src[n];
	roll(33300);

	/* ------------------------------------------------------------------
	 * the streams that are not ours. inflate answers a number here and the
	 * number is the subject: -1 refused, -2 the cap, anything else a length.
	 * ------------------------------------------------------------------ */
	{
		unsigned char st[64];
		int64_t got;
		unsigned i;

		/* a valid stream to cut up: letters with no structure, so it
		 * codes dynamically and the stream is long enough to cut */
		s = 31337u;
		for (n = 0; n < 4000; n++)
			src[n] = (unsigned char) ('a' + (lcg(&s) >> 16) % 26);
		got = df_go(src, 4000, out, OUT, (uint8_t *) arena);
		say_n("cut.len", (long) got);

		/* every truncation: the bit reader runs out at a different place in
		 * the header, the code lengths, and the symbol loop */
		for (i = 0; i < (unsigned) got; i++)
			say_n("cut", (long) inf_run(out, i, back, OUT));

		/* every single-byte corruption of the first 64: a code that does not
		 * describe a code, a length that overshoots, a distance past the
		 * output -- each has ONE right answer and it is the twin's */
		for (i = 0; i < 64 && i < (unsigned) got; i++) {
			unsigned char keep = out[i];
			out[i] ^= 0x55;
			say_n("bend", (long) inf_run(out, (uintptr_t) got, back, OUT));
			out[i] = keep;
		}

		/* hand-laid headers: the reserved block type, a stored block whose
		 * NLEN does not complement, a stored block that runs off the end,
		 * and an empty final stored block */
		st[0] = 0x07;                              /* last, typ 3 */
		say_n("typ3", (long) inf_run(st, 1, back, OUT));
		st[0] = 0x01; st[1] = 4; st[2] = 0; st[3] = 0xfb; st[4] = 0xff;
		st[5] = 'a'; st[6] = 'b'; st[7] = 'c'; st[8] = 'd';
		say_n("stored", (long) inf_run(st, 9, back, OUT));
		st[3] = 0x00;                              /* NLEN lies */
		say_n("stored.nlen", (long) inf_run(st, 9, back, OUT));
		st[3] = 0xfb;
		say_n("stored.short", (long) inf_run(st, 7, back, OUT));
		st[1] = 0; st[2] = 0; st[3] = 0xff; st[4] = 0xff;
		say_n("stored.empty", (long) inf_run(st, 5, back, OUT));

		/* a fixed-code block, by hand: 0x63 is last+typ1 with the first
		 * literal already begun, so the tail decides what it says */
		for (i = 0; i < 32; i++) {
			st[0] = 0x03; st[1] = (unsigned char) i; st[2] = 0x00;
			st[3] = (unsigned char) (i * 7); st[4] = 0xff;
			say_n("fixed", (long) inf_run(st, 5, back, OUT));
		}

		/* a dynamic header whose code-length code is over-subscribed: the
		 * twin does not check, so the answer is whatever first-writer-wins
		 * and a zeroed symbol array produce, and THAT is the law */
		for (i = 0; i < 24; i++) {
			memset(st, 0, sizeof st);
			st[0] = (unsigned char) (0x05 | (i << 3));
			st[1] = (unsigned char) (i * 37);
			st[2] = (unsigned char) (i * 11);
			st[3] = 0xff; st[4] = 0xff; st[5] = 0xaa; st[6] = 0x55;
			say_n("over", (long) inf_run(st, 8, back, OUT));
		}
	}

	/* --- the bit sink alone: a code of every width, reversed, back out
	 * through df_put, so the accumulator's carry across a byte is visible
	 * without a whole block around it --- */
	{
		struct df_sink t;
		unsigned k;
		t.out = out; t.op = 0; t.cap = OUT; t.acc = 0; t.nb = 0; t.err = 0;
		for (k = 1; k <= 24; k++) df_put(&t, 0xa5a5a5u, k);
		for (k = 1; k <= 15; k++) df_put(&t, df_rev(0x2b3du, k), k);
		df_align(&t);
		say_n("sink.err", t.err);
		say_u("sink.op", (unsigned long) t.op);
		say_b("sink", out, t.op);
	}

	/* --- the ladders: every length and every distance a token can carry,
	 * through the code that picks its rung and back --- */
	for (n = 3; n <= 258; n++) {
		unsigned c = df_lcode(n);
		say_u("lcode", c);
		say_u("lext", n - df_lbase[c]);
	}
	for (n = 1; n <= 32768; n <<= 1) {
		unsigned c = df_dcode(n);
		say_u("dcode", c);
		say_u("dext", n - df_dbase[c]);
	}
	for (n = 0; n < 30; n++) {
		say_u("dcode.base", df_dcode(df_dbase[n]));
		say_u("dcode.top", df_dcode(df_dbase[n] + (1u << df_dext[n]) - 1));
	}

	return 0;
}
