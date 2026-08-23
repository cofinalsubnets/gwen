/* the rotate idiom, (x >> n) | (x << (W - n)) in either order: recognized shapes
 * (constant and spliced-variable counts, both directions, both widths, an array
 * element operand) beside the near-misses that must stay plain shifts. */
typedef unsigned int u32;
typedef unsigned long u64;

static u32 rr(u32 x, int n) { return (x >> n) | (x << (32 - n)); }
static u32 rl(u32 x, int n) { return (x << n) | (x >> (32 - n)); }
static u64 rr64(u64 x, int n) { return (x >> n) | (x << (64 - n)); }

static u32 c1(u32 x) { return (x >> 7) | (x << 25); }
static u32 c2(u32 x) { return (x << 12) | (x >> 20); }
static u64 c3(u64 x) { return (x >> 17) | (x << 47); }

static int s1(int x) { return (x >> 3) | (x << 29); }
static u32 s2(u32 x, u32 y) { return (x >> 5) | (y << 27); }
static u32 s3(u32 x) { return (x >> 5) | (x << 26); }

static u32 a[4] = {0x01234567u, 0x89abcdefu, 0xdeadbeefu, 0x600df00du};

int main(void) {
  u32 acc = 0;
  u64 wacc = 0;
  int i, n;
  for (i = 0; i < 4; i++) {
    acc ^= c1(a[i]) + c2(a[i]) + (u32)s1((int)a[i]) + s2(a[i], a[3 - i]) + s3(a[i]);
    acc ^= (a[i] >> 16) | (a[i] << 16);
    for (n = 1; n < 32; n++) acc ^= rr(a[i], n) + rl(a[i], n);
    for (n = 1; n < 64; n++) wacc ^= rr64((u64)a[i] * 0x9e3779b97f4a7c15ul, n);
    wacc ^= c3((u64)a[i] << 13);
  }
  return (int)((acc ^ (u32)wacc ^ (u32)(wacc >> 32)) & 0x7f);
}
