// PVH -> kboot: the C half of boot.S's hand-off. Runs high, paging on, with
// the stub's hhdm window (0xffff8000_00000000 over physical 0..4G) already
// mapped; reads qemu's hvm_start_info (the e820 map) and fills kboot the way
// limine_to_kboot fills it on the iso door. Headless by construction -- PVH
// hands over no framebuffer, so has_fb stays false and kmain runs on serial.
#include "k.h"

#define pvh_hhdm 0xffff800000000000ull

struct hvm_start_info {
  uint32_t magic;                     // 0x336ec578 ("xEn3")
  uint32_t version, flags, nr_modules;
  uint64_t modlist_paddr, cmdline_paddr, rsdp_paddr, memmap_paddr;
  uint32_t memmap_entries, reserved;
};
struct hvm_memmap_entry { uint64_t addr, size; uint32_t type, reserved; };

// the linker lays this at the end of .bss; image - KVMA is the physical
// footprint qemu loaded us into, which e820 still calls usable RAM (the PVH
// loader reserves nothing) -- hand it to the heap and the kernel eats itself.
extern char kimage_end[];

static void give(uint64_t base, uint64_t len) {
  if (len < 2 * sizeof(uintptr_t) || kboot.ram_n >= k_boot_ram_max) return;
  kboot.ram[kboot.ram_n].base = base;
  kboot.ram[kboot.ram_n].len  = len;
  kboot.ram_n++; }

void pvh_to_kboot(uint32_t si_paddr) {
  struct hvm_start_info *si = (void *) (pvh_hhdm + si_paddr);
  if (si->magic != 0x336ec578 || si->version < 1 || !si->memmap_paddr) return;
  struct hvm_memmap_entry *mm = (void *) (pvh_hhdm + si->memmap_paddr);
  uint64_t k0 = 0x200000,             // k1 page-rounded: meminit lays a struct
           k1 = (((uintptr_t) kimage_end - 0xffffffff80000000ull) + 0xfff) & ~0xfffull;
  kboot.hhdm = pvh_hhdm;
  for (uint32_t i = 0; i < si->memmap_entries; i++) {
    if (mm[i].type != 1) continue;                   // 1 = usable RAM
    uint64_t a = mm[i].addr, b = a + mm[i].size;
    if (a < 0x100000) a = 0x100000;                  // low memory: the IVT, the
    if (b > 0x100000000ull) b = 0x100000000ull;      // start_info itself; >4G: unmapped
    if (b <= a) continue;
    if (b <= k0 || a >= k1) { give(a, b - a); continue; }
    if (a < k0) give(a, k0 - a);                     // the piece under the image
    if (b > k1) give(k1, b - k1); } }                // and the piece past it
