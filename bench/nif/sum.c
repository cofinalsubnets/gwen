/* the CHECKSUM floor -- host/hash.c through mooncc, gcc and clang, with the
 * three reports diffed and the three builds timed. ccnif.sh drives it.
 *
 * WHY THIS FILE. sha-256, md5, crc32 and cksum are the arithmetic our compiler
 * is thinnest on: 32-bit rotates, a wrapping add over eight registers, a
 * 64-bit bit count laid a byte at a time, and two table walks that read EIGHT
 * INDEPENDENT lookups per step. none of it is written anywhere else in the
 * tree -- love.c is a narrow style and never rotates a word -- so a fault in
 * the shift/rotate or wrap lane has no other place to show.
 *
 * ⚠ NO VECTOR IS WRITTEN DOWN HERE, on purpose. an answer every lane reaches is
 * host/hash.c's own and test/host/hash.l is where it is checked; an answer ONE
 * lane reaches is the code generator's, and that is the only thing this file can
 * see. test/host/hash.l reads these nifs through the RUNNING love, which is the
 * gcc build nearly everywhere -- so it asks whether the algorithm is right and
 * never which compiler built it.
 *
 * ⚠ the streaming lane is walked WITHOUT a cask. the state layout is host/
 * hash.c's own (its header spells all three), love only carries the bytes, so
 * a harness can lay the state directly and ride the same blk_feed/blk_done/
 * dig_ld/dig_st the nifs ride. going through the cask would need a heap and
 * would read the runtime instead of the code generator. */
#include "../../host/hash.c"
#include "stub.h"
#include "say.h"

/* ⚠ NOT rand(): the two builds carry different libcs, so the corpus has to be
 * this file's own arithmetic or the programs do not see the same bytes. */
static void fill(unsigned char *b, unsigned n, unsigned seed)
{
	unsigned s = seed;
	for (unsigned i = 0; i < n; i++) {
		s = s * 1103515245u + 12345u;
		b[i] = (unsigned char) (s >> 16);
	}
}

static void oneshot(unsigned char const *p, unsigned n)
{
	char h[65];
	say_u("len", n);
	sha256_hex(p, n, h);
	say_s("sha256", h);
	md5_hex(p, n, h);
	say_s("md5", h);
	say_u("crc32", crc32_of(p, n));
	say_u("cksum", cksum_of(p, n));
}

/* the resumable digest, fed `chunk` bytes at a time through the state layout */
static void stream(struct digspec const *d, unsigned char const *p, unsigned n,
                   unsigned chunk, char *out)
{
	uint8_t st[ShaSt];
	uint32_t h[8];
	uint64_t len;
	memset(st, 0, d->st);
	dig_st(st, d->h0, d->words, 0);
	for (unsigned i = 0; i < n; ) {
		unsigned k = n - i < chunk ? n - i : chunk;
		dig_ld(st, h, d->words, &len);
		len += k;
		st[d->remoff] = (uint8_t) blk_feed(h, st + d->bufoff, st[d->remoff],
		                                   d->f, p + i, k);
		dig_st(st, h, d->words, len);
		i += k;
	}
	dig_ld(st, h, d->words, &len);
	blk_done(h, st + d->bufoff, st[d->remoff], len, d->f, d->be);
	blk_hex(h, d->words, d->be, out);
}

static uint32_t ckstream(unsigned char const *p, unsigned n, unsigned chunk)
{
	uint8_t st[CkSt];
	uint32_t c;
	uint64_t len;
	memset(st, 0, CkSt);
	for (unsigned i = 0; i < n; ) {
		unsigned k = n - i < chunk ? n - i : chunk;
		dig_ld(st, &c, 1, &len);
		len += k;
		c = ck_run(c, p + i, k);
		dig_st(st, &c, 1, len);
		i += k;
	}
	dig_ld(st, &c, 1, &len);
	return ~ck_len(c, len);
}

static unsigned char buf[8192];

static unsigned udec(char const *s)
{
	unsigned v = 0;
	while (*s >= '0' && *s <= '9') v = v * 10 + (unsigned) (*s++ - '0');
	return v;
}

/* THE TIMED LANE: one algorithm over a megabyte, `reps` times, the running
 * answer printed so no lane can fold the work away.
 *
 * ⚠ the SHELL does the timing. the two builds carry different libcs, so a
 * program reading its own clock would be timing the clock as much as the code
 * -- and the whole point of the row is that the ONLY difference between two
 * runs is the code generator. */
static unsigned char big[1u << 20];

static int spin(char const *what, unsigned reps)
{
	unsigned long acc = 0;
	char h[65];
	fill(big, sizeof big, 424242u);
	for (unsigned i = 0; i < reps; i++) {
		big[i % sizeof big]++;              /* a fresh message per rep */
		switch (*what) {
		case 's': sha256_hex(big, sizeof big, h); acc += (unsigned char) h[3]; break;
		case 'm': md5_hex(big, sizeof big, h);    acc += (unsigned char) h[3]; break;
		case 'c': acc += crc32_of(big, sizeof big); break;
		case 'k': acc += cksum_of(big, sizeof big); break;
		default: return 2;
		}
	}
	say_u("acc", acc);
	return 0;
}

int main(int argc, char **argv)
{
	char a[65], b[65];

	if (argc > 1) return spin(argv[1], argc > 2 ? udec(argv[2]) : 16);

	fill(buf, sizeof buf, 20260822u);

	/* --- every length across the block boundaries. the pad picks its 64 or
	 * its 128 at 56, blk_feed's whole-block loop opens at 64, and the crc
	 * walks take eight at a time and then a tail of 0..7 --- */
	for (unsigned n = 0; n <= 200; n++) {
		say_u("crc32.n", crc32_of(buf, n));
		say_u("cksum.n", cksum_of(buf, n));
	}
	for (unsigned n = 0; n <= 136; n++) {
		sha256_hex(buf, n, a);
		say_s("sha256.n", a);
		md5_hex(buf, n, a);
		say_s("md5.n", a);
	}

	/* --- the lengths worth naming, digested whole --- */
	{
		static unsigned const ns[] = {0, 1, 55, 56, 57, 63, 64, 65, 119,
		                              120, 127, 128, 129, 1000, 4095,
		                              4096, 4097, 8192};
		for (unsigned i = 0; i < sizeof ns / sizeof *ns; i++)
			oneshot(buf, ns[i]);
	}

	/* --- a byte that is not the corpus: the tables are indexed by it, so a
	 * sign-extended load reads the wrong row for everything over 127 --- */
	for (unsigned v = 0; v < 256; v++) {
		unsigned char one = (unsigned char) v;
		say_u("crc32.byte", crc32_of(&one, 1));
		say_u("cksum.byte", cksum_of(&one, 1));
	}

	/* --- the same message, chunked every way. a streamed digest that drifts
	 * from its one-shot is the remainder arithmetic, and the chunk sizes are
	 * chosen to straddle 64 from both sides --- */
	{
		static unsigned const cs[] = {1, 2, 3, 7, 8, 31, 32, 63, 64, 65,
		                              127, 128, 129, 1000};
		static unsigned const ns[] = {0, 1, 63, 64, 65, 127, 128, 1000, 4096};
		for (unsigned i = 0; i < sizeof ns / sizeof *ns; i++)
			for (unsigned j = 0; j < sizeof cs / sizeof *cs; j++) {
				unsigned n = ns[i], c = cs[j];
				stream(&dig_sha, buf, n, c, a);
				sha256_hex(buf, n, b);
				say_s("sha.stream", a);
				say_n("sha.agrees", strcmp(a, b) ? 0 : 1);
				stream(&dig_md5, buf, n, c, a);
				md5_hex(buf, n, b);
				say_s("md5.stream", a);
				say_n("md5.agrees", strcmp(a, b) ? 0 : 1);
				say_u("ck.stream", ckstream(buf, n, c));
				say_n("ck.agrees",
				      ckstream(buf, n, c) == cksum_of(buf, n) ? 1 : 0);
			}
	}

	/* --- the count is 64 bits and the pad lays it a byte at a time, big-
	 * endian one way and little the other. an 8 KB message only reaches bit
	 * 16 of it, so the high half is laid by hand here --- */
	{
		uint32_t h[8];
		uint8_t rem[64];
		memset(rem, 0x5a, sizeof rem);
		static uint64_t const lens[] = {0, 1, 0xffull, 0x100ull, 0xffffull,
		                                0x1ffffffffull, 0x123456789abcull,
		                                0x1fffffffffffffffull};
		for (unsigned i = 0; i < sizeof lens / sizeof *lens; i++) {
			memcpy(h, sha_h0, sizeof sha_h0);
			blk_done(h, rem, 13, lens[i], sha_block, 1);
			blk_hex(h, 8, 1, a);
			say_s("pad.be", a);
			memcpy(h, md5_h0, sizeof md5_h0);
			blk_done(h, rem, 13, lens[i], md5_block, 0);
			blk_hex(h, 4, 0, a);
			say_s("pad.le", a);
		}
	}

	/* --- the state serializer on its own: big-endian words and a big-endian
	 * count, whatever the digest's own order --- */
	{
		uint8_t st[ShaSt];
		uint32_t h[8], k[8];
		uint64_t len;
		for (unsigned i = 0; i < 8; i++) h[i] = 0x80000000u >> i | (i + 1);
		memset(st, 0, sizeof st);
		dig_st(st, h, 8, 0x0123456789abcdefull);
		say_b("state", st, 40);
		dig_ld(st, k, 8, &len);
		for (unsigned i = 0; i < 8; i++) say_u("state.h", k[i]);
		say_u("state.len.hi", (unsigned long) (len >> 32));
		say_u("state.len.lo", (unsigned long) (len & 0xffffffffu));
	}

	return 0;
}
