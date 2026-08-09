/* __builtin_bswap16/32/64 -- the byte-order trio glibc's <byteswap.h> inlines
 * reach (every TU that includes <endian.h> compiles them). The results type
 * UNSIGNED at their exact width: a swapped high bit is real, so the compare
 * and the sizeof rows are load-bearing, not decoration.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

extern int printf(const char *, ...);

typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

u16 g16 = 0xaabb;
u32 g32 = 0x11223344;
u64 g64 = 0x0123456789abcdefULL;

/* the glibc shape: a static inline wrapping the builtin */
static __inline u16 bs16(u16 x) { return __builtin_bswap16(x); }
static __inline u32 bs32(u32 x) { return __builtin_bswap32(x); }
static __inline u64 bs64(u64 x) { return __builtin_bswap64(x); }

int main(void)
{
	u32 wide = 0x123456;   /* a 16-swap of a wider value truncates first */
	int r = 0;
	r += __builtin_bswap16(g16) == 0xbbaa;
	r += __builtin_bswap32(g32) == 0x44332211u;
	r += __builtin_bswap64(g64) == 0xefcdab8967452301ULL;
	r += bs16(bs16(g16)) == g16 && bs32(bs32(g32)) == g32 && bs64(bs64(g64)) == g64;
	r += __builtin_bswap16(wide) == 0x5634;
	r += __builtin_bswap64(g64) > g64;   /* unsigned: 0xefcd.. tops 0x0123.. */
	r += sizeof(__builtin_bswap16(g16)) == 2;
	r += sizeof(__builtin_bswap32(g32)) == 4;
	r += sizeof(__builtin_bswap64(g64)) == 8;
	return r;
}
