// blk -- the disk (rung 5): virtio-blk, polled, one request in flight.
// x86_64 finds it by PCI config-space walk (CF8/CFC) and drives the MODERN
// virtio-pci transport; aarch64 scans qemu virt's fixed virtio-mmio slots
// (version 2 -- the test lane passes force-legacy=false). Both transports
// share one split virtqueue and one synchronous request door: k_blk_rw posts
// a three-descriptor chain (header, data, status) and spins on the used ring
// -- a sector under qemu answers in microseconds, and the spin is BOUNDED so
// a dead device is a refusal, never a hang. no interrupts: the device's line
// stays masked and nobody listens.
//
// ⚠ every DMA address the device sees is PHYSICAL: pa = va - khhdm, which
// holds exactly for kmallocw/love-heap memory (meminit chains RAM at
// khhdm + base) and does NOT hold for image statics -- so the ring and the
// request header live in the block kmain hands k_blk_init, and data buffers
// are always heap (a love string's bytes). the filesystem over this stays in
// love (lib/fat.l); this file is the small part that must be C.
#include "k.h"
#include "asmops.h"
#include <stdint.h>

void serial_putc(int);
static void bputs(char const *s) { while (*s) serial_putc(*s++); }
static void bputn(uint64_t v) {
  char b[20]; int i = 0;
  do b[i++] = (char) ('0' + v % 10), v /= 10; while (v);
  while (i) serial_putc(b[--i]); }

// one request moves at most this many sectors; k_blk_rw chunks larger asks.
// 64K keeps a single data descriptor comfortably under every qemu bound.
#define k_blk_chunk 128

static struct {
  uint64_t sectors;                  // 0 = no disk
  volatile uint8_t *desc, *avail, *used, *hdr, *sts;
  uint16_t qsz, avail_idx;
  // pci notify door: address precomputed at init. mmio: base + 0x50.
  volatile uint16_t *notify16;       // pci writes the queue index here
  volatile uint32_t *notify32;       // mmio writes it here
} kblk;

// LE scalar access over device memory / the ring, all through volatile so the
// compiler keeps program order. both arches are little-endian, so a plain
// typed load IS the LE read.
static inline uint8_t  r8 (volatile uint8_t *p) { return *p; }
static inline uint16_t r16(volatile uint8_t *p) { return *(volatile uint16_t*) p; }
static inline uint32_t r32(volatile uint8_t *p) { return *(volatile uint32_t*) p; }
static inline void w8 (volatile uint8_t *p, uint8_t v)  { *p = v; }
static inline void w16(volatile uint8_t *p, uint16_t v) { *(volatile uint16_t*) p = v; }
static inline void w32(volatile uint8_t *p, uint32_t v) { *(volatile uint32_t*) p = v; }
static inline void w64(volatile uint8_t *p, uint64_t v) {   // two 32-bit halves: the
  w32(p, (uint32_t) v); w32(p + 4, (uint32_t) (v >> 32)); } // width every transport takes

// the hardware fence DMA needs around the avail publish and the used read.
// x86 is TSO and every ring access above is volatile, so program order is
// enough; aarch64 reorders Normal-vs-Device, so a real barrier stands there.
static inline void dma_fence(void) {
#if defined(__aarch64__)
  k_dsb_ish();
#endif
}

static inline uintptr_t vtop(volatile void *p) { return (uintptr_t) p - khhdm; }

// --- the split virtqueue, shared by both transports ------------------------
// lays desc/avail/used plus the request header and status byte into the DMA
// block, 16-aligned. sizes at qsz=8: desc 128, avail 22, used 70.
static int vq_lay(void *dma) {
  uintptr_t a = ((uintptr_t) dma + 15) & ~(uintptr_t) 15;
  kblk.qsz = 8;
  kblk.desc  = (volatile uint8_t*) a;
  kblk.avail = kblk.desc + 128;
  kblk.used  = kblk.desc + 192;
  kblk.hdr   = kblk.desc + 288;
  kblk.sts   = kblk.desc + 304;
  for (int i = 0; i < 320; i++) kblk.desc[i] = 0;
  // a polled driver takes no interrupt: VRING_AVAIL_F_NO_INTERRUPT, or the
  // completion's INTx lands on an unhandled vector stub and resets the machine
  // in silence -- exactly how it announced itself.
  w16(kblk.avail + 0, 1);
  return 1; }

// post one request and spin it home. type 0 = read (device fills data),
// 1 = write (device drains it). -> 0 ok, -1 refused/failed/timed out.
static int vq_go(uint32_t type, uint64_t lba, void *data, uint32_t bytes) {
  volatile uint8_t *d = kblk.desc;
  w32(kblk.hdr + 0, type);
  w32(kblk.hdr + 4, 0);
  w64(kblk.hdr + 8, lba);
  w8(kblk.sts, 0xff);                          // poison: the device must write 0
  // desc 0: the header, chained on
  w64(d + 0, vtop(kblk.hdr)); w32(d + 8, 16); w16(d + 12, 1); w16(d + 14, 1);
  // desc 1: the data, device-writable on a read
  w64(d + 16, vtop(data)); w32(d + 24, bytes);
  w16(d + 28, (uint16_t) (1 | (type ? 0 : 2))); w16(d + 30, 2);
  // desc 2: the status byte, always device-writable
  w64(d + 32, vtop(kblk.sts)); w32(d + 40, 1); w16(d + 44, 2); w16(d + 46, 0);
  w16(kblk.avail + 4 + 2 * (kblk.avail_idx & (kblk.qsz - 1)), 0);
  dma_fence();
  w16(kblk.avail + 2, ++kblk.avail_idx);
  dma_fence();
  if (kblk.notify16) w16((volatile uint8_t*) kblk.notify16, 0);
  else w32((volatile uint8_t*) kblk.notify32, 0);
  for (uint32_t spin = 0; r16(kblk.used + 2) != kblk.avail_idx; spin++)
    if (spin == (uint32_t) 1 << 28) return -1;    // a dead device is a refusal, not a hang
  dma_fence();
  return r8(kblk.sts) == 0 ? 0 : -1; }

#if defined(__x86_64__)
// --- PCI config space, and the modern virtio-pci transport ------------------
static uint32_t pci_r32(uint32_t bdf, uint32_t off) {
  k_outl(0xcf8, 0x80000000u | bdf << 8 | (off & 0xfc));
  return k_inl(0xcfc); }
static void pci_w32(uint32_t bdf, uint32_t off, uint32_t v) {
  k_outl(0xcf8, 0x80000000u | bdf << 8 | (off & 0xfc));
  k_outl(0xcfc, v); }
static uint32_t pci_r8(uint32_t bdf, uint32_t off) {
  return pci_r32(bdf, off) >> 8 * (off & 3) & 0xff; }

// a memory BAR's assigned address (the firmware laid it), 0 when empty/io.
static uint64_t pci_bar(uint32_t bdf, uint32_t bar) {
  uint32_t lo = pci_r32(bdf, 0x10 + 4 * bar);
  if (lo & 1) return 0;                        // io-space BAR: not this transport
  uint64_t a = lo & ~(uint64_t) 0xf;
  if ((lo & 6) == 4) a |= (uint64_t) pci_r32(bdf, 0x14 + 4 * bar) << 32;
  return a; }

static int blk_pci(uint32_t bdf, void *dma) {
  // the vendor capability walk: cfg_type 1 = common, 2 = notify, 4 = device
  uint64_t common = 0, notify = 0, devcfg = 0;
  uint32_t nmult = 0;
  for (uint32_t c = pci_r8(bdf, 0x34) & 0xfc; c; c = pci_r8(bdf, c + 1) & 0xfc) {
    if (pci_r8(bdf, c) != 9) continue;         // 9 = vendor-specific (virtio's)
    uint32_t type = pci_r8(bdf, c + 3);
    uint64_t at = pci_bar(bdf, pci_r8(bdf, c + 4)) + pci_r32(bdf, c + 8);
    if (type == 1) common = at;
    else if (type == 2) notify = at, nmult = pci_r32(bdf, c + 16);
    else if (type == 4) devcfg = at; }
  if (!common || !notify || !devcfg) return 0; // legacy-only device: not driven
  // every x86_64 door's map stops at 4G (pvh + uefi/loader.c lay 0..4G, limine
  // guarantees the same), so a BAR above it is out of reach -- OVMF parks 64-bit
  // BARs there unless the lane pins its MMIO window low (mk/tools/ktest.l's fw_cfg).
  // khhdm 0 would be a true identity door where everything is reachable.
  if (khhdm && (common >> 32 || notify >> 32 || devcfg >> 32))
    return bputs("disk: virtio-blk BAR above 4G, skipped\r\n"), 0;
  // memory decode + bus master on, INTx OFF (bit 10): this driver polls
  pci_w32(bdf, 4, pci_r32(bdf, 4) | 6 | 1 << 10);
  volatile uint8_t *cc = (volatile uint8_t*) (khhdm + common);
  w8(cc + 20, 0);                              // device_status: reset
  w8(cc + 20, 1); w8(cc + 20, 1 | 2);          // ACKNOWLEDGE, DRIVER
  w32(cc + 0, 1);                              // device_feature_select: high word
  if (!(r32(cc + 4) & 1)) return 0;            // no VIRTIO_F_VERSION_1: not modern
  w32(cc + 8, 1); w32(cc + 12, 1);             // driver features: VERSION_1 only
  w32(cc + 8, 0); w32(cc + 12, 0);
  w8(cc + 20, 1 | 2 | 8);                      // FEATURES_OK...
  if (!(r8(cc + 20) & 8)) return 0;            // ...must stick
  w16(cc + 22, 0);                             // queue_select 0
  uint16_t qm = r16(cc + 24);
  if (!qm) return 0;
  if (!vq_lay(dma)) return 0;
  if (qm > kblk.qsz) w16(cc + 24, kblk.qsz); else kblk.qsz = qm;
  w64(cc + 32, vtop(kblk.desc));
  w64(cc + 40, vtop(kblk.avail));
  w64(cc + 48, vtop(kblk.used));
  w16(cc + 28, 1);                             // queue_enable
  kblk.notify16 = (volatile uint16_t*)
    (khhdm + notify + (uint64_t) r16(cc + 30) * nmult);
  w8(cc + 20, 1 | 2 | 8 | 4);                  // DRIVER_OK: the device is live
  volatile uint8_t *dc = (volatile uint8_t*) (khhdm + devcfg);
  kblk.sectors = r32(dc) | (uint64_t) r32(dc + 4) << 32;
  return 1; }

static void blk_scan(void *dma) {
  for (uint32_t dev = 0; dev < 32; dev++) {
    uint32_t fns = pci_r8(dev << 3, 14) & 0x80 ? 8 : 1;
    for (uint32_t fn = 0; fn < fns; fn++) {
      uint32_t bdf = dev << 3 | fn, id = pci_r32(bdf, 0);
      if (id == 0xffffffffu) continue;
      // virtio vendor 1af4; 1042 the modern blk id, 1001 the transitional one
      if ((id & 0xffff) != 0x1af4) continue;
      uint32_t d = id >> 16;
      if (d != 0x1042 && d != 0x1001) continue;
      if (blk_pci(bdf, dma)) return; } } }

#elif defined(__aarch64__)
// --- qemu virt's virtio-mmio slots, the version-2 transport ------------------
// 32 fixed slots; the test lane runs -global virtio-mmio.force-legacy=false,
// so a populated slot speaks version 2. a version-1 slot is skipped with a
// word: it is a configuration face, not an absence.
#define VIRTIO_MMIO_PHYS 0x0a000000
#define VIRTIO_MMIO_N    32
#define VIRTIO_MMIO_STEP 0x200

static int blk_mmio(volatile uint8_t *m, void *dma) {
  w32(m + 0x70, 0);                            // status: reset
  w32(m + 0x70, 1); w32(m + 0x70, 1 | 2);      // ACKNOWLEDGE, DRIVER
  w32(m + 0x14, 1);                            // device_features_sel: high word
  if (!(r32(m + 0x10) & 1)) return 0;          // no VIRTIO_F_VERSION_1
  w32(m + 0x24, 1); w32(m + 0x20, 1);          // driver features: VERSION_1 only
  w32(m + 0x24, 0); w32(m + 0x20, 0);
  w32(m + 0x70, 1 | 2 | 8);                    // FEATURES_OK...
  if (!(r32(m + 0x70) & 8)) return 0;          // ...must stick
  w32(m + 0x30, 0);                            // queue_sel 0
  uint32_t qm = r32(m + 0x34);
  if (!qm) return 0;
  if (!vq_lay(dma)) return 0;
  if (qm < kblk.qsz) kblk.qsz = (uint16_t) qm;
  w32(m + 0x38, kblk.qsz);
  w64(m + 0x80, vtop(kblk.desc));
  w64(m + 0x90, vtop(kblk.avail));
  w64(m + 0xa0, vtop(kblk.used));
  w32(m + 0x44, 1);                            // queue_ready
  kblk.notify32 = (volatile uint32_t*) (m + 0x50);
  w32(m + 0x70, 1 | 2 | 8 | 4);                // DRIVER_OK
  kblk.sectors = r32(m + 0x100) | (uint64_t) r32(m + 0x104) << 32;
  return 1; }

static void blk_scan(void *dma) {
  for (uint32_t i = 0; i < VIRTIO_MMIO_N; i++) {
    volatile uint8_t *m =
      (volatile uint8_t*) (khhdm + VIRTIO_MMIO_PHYS + i * VIRTIO_MMIO_STEP);
    if (r32(m + 0) != 0x74726976 || r32(m + 8) != 2) continue;
    uint32_t v = r32(m + 4);
    if (v != 2) { bputs("disk: legacy virtio-mmio, skipped\r\n"); continue; }
    if (blk_mmio(m, dma)) return; } }

#else
static void blk_scan(void *dma) { (void) dma; }
#endif

// --- the three doors kmain wires to love ------------------------------------
// dma is one kmallocw block (>= 336 bytes; 16-align slack included) the ring
// and request header live in for the machine's life.
void k_blk_init(void *dma) {
  if (!dma) return;
  blk_scan(dma);
  if (kblk.sectors) {
    bputs("disk: virtio-blk, ");
    bputn(kblk.sectors);
    bputs(" sectors\r\n"); } }

uint64_t k_blk_sectors(void) { return kblk.sectors; }

// read (wr 0) or write (wr 1) n sectors at lba, chunked; buf is heap memory
// (a love string's bytes). -> 0 ok, -1 refused.
int k_blk_rw(uint64_t lba, uint32_t n, void *buf, int wr) {
  if (!kblk.sectors || lba + n < lba || lba + n > kblk.sectors) return -1;
  uint8_t *p = buf;
  while (n) {
    uint32_t c = n > k_blk_chunk ? k_blk_chunk : n;
    if (vq_go(wr ? 1 : 0, lba, p, c * 512) < 0) return -1;
    lba += c; p += (uintptr_t) c * 512; n -= c; }
  return 0; }
