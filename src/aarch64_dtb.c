// dtb -> kboot: the C half of mkboot.l's hand-off (x86_64/pvh.c's twin). Runs
// high, MMU on, with the stub's hhdm window (0xffff8000_00000000 over
// physical 0..4G) already mapped; reads the flat device tree qemu lays at
// the RAM base and fills kboot the way every door fills it before the
// door. Headless by construction -- the -kernel door hands over no
// framebuffer, so has_fb stays false and kmain runs on serial.
#include "k.h"

#define a64_hhdm 0xffff800000000000ull

// everything from the RAM base up to the image's far edge (the dtb, the
// 2 MiB hole, the image itself) is spoken for, and the qemu loader reserves
// nothing -- hand it to the heap and the kernel eats itself. a VALUE the
// projection patches into the file (tools/kproject.l), where the flat link's
// kimage_end symbol used to stand.
extern uintptr_t const k_image_top;

static uint32_t be32(uint8_t const *p) {
  return (uint32_t) p[0] << 24 | (uint32_t) p[1] << 16
       | (uint32_t) p[2] << 8  | p[3]; }
static uint64_t be64(uint8_t const *p) {
  return (uint64_t) be32(p) << 32 | be32(p + 4); }

static void give(uint64_t base, uint64_t len) {
  if (len < 2 * sizeof(uintptr_t) || kboot.ram_n >= k_boot_ram_max) return;
  kboot.ram[kboot.ram_n].base = base;
  kboot.ram[kboot.ram_n].len  = len;
  kboot.ram_n++; }

// flat-tree tokens (all fields big-endian, everything 4-aligned)
#define FDT_BEGIN_NODE 1
#define FDT_END_NODE   2
#define FDT_PROP       3
#define FDT_NOP        4
#define FDT_END        9

static int is(char const *a, char const *b) {      // tiny strcmp, self-contained
  while (*a && *a == *b) { a++; b++; }
  return *a == *b; }

void dtb_to_kboot(uint64_t dtb_pa) {
  uint8_t const *f = (uint8_t const *) (a64_hhdm + dtb_pa);
  if (be32(f) != 0xd00dfeed) return;
  uint8_t const *p   = f + be32(f + 8);            // off_dt_struct
  char const *str    = (char const *) (f + be32(f + 12));   // off_dt_strings
  uint64_t k1 = (k_image_top + 0xfff) & ~0xfffull;   // page-rounded physical far edge
  kboot.hhdm = a64_hhdm;
  uint32_t ac = 2, sc = 2;                         // root's cell counts (virt: 2/2)
  int depth = 0, memd = 0, chos = 0;               // memd/chos: the depth of a memory / chosen node we are inside
  for (;;) {
    uint32_t tok = be32(p); p += 4;
    if (tok == FDT_END) break;
    if (tok == FDT_NOP) continue;
    if (tok == FDT_BEGIN_NODE) {
      char const *nm = (char const *) p;
      uint32_t n = 0; while (nm[n]) n++;
      p += (n + 1 + 3) & ~3u;
      depth++;
      // a memory bank is a child of root named memory or memory@...
      if (depth == 2 && nm[0]=='m' && nm[1]=='e' && nm[2]=='m' && nm[3]=='o'
          && nm[4]=='r' && nm[5]=='y' && (nm[6] == 0 || nm[6] == '@'))
        memd = depth;
      if (depth == 2 && is(nm, "chosen")) chos = depth;
      continue; }
    if (tok == FDT_END_NODE) {
      if (depth == memd) memd = 0;
      if (depth == chos) chos = 0;
      depth--;
      continue; }
    if (tok != FDT_PROP) break;                    // a malformed tree: stop, keep what we have
    uint32_t len = be32(p), nameoff = be32(p + 4);
    p += 8;
    char const *pn = str + nameoff;
    if (depth == 1 && is(pn, "#address-cells")) ac = be32(p);
    if (depth == 1 && is(pn, "#size-cells"))    sc = be32(p);
    if (chos && depth == chos && is(pn, "bootargs") && len)
      k_cmdline((char const *) p, len);

    if (memd && depth == memd && is(pn, "reg")) {
      uint32_t step = (ac + sc) * 4;
      for (uint32_t o = 0; step && o + step <= len; o += step) {
        uint64_t a = ac == 2 ? be64(p + o) : be32(p + o);
        uint64_t s = sc == 2 ? be64(p + o + ac*4) : be32(p + o + ac*4);
        uint64_t b = a + s;
        if (a < k1) a = k1;                        // dtb + hole + image, one span
        if (b > 0x100000000ull) b = 0x100000000ull;   // above the mapped 4G
        if (b > a) give(a, b - a); } }
    p += (len + 3) & ~3u; } }
