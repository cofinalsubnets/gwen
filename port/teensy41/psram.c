// Teensy 4.1 external PSRAM bring-up: the FlexSPI2 controller + the QPI
// PSRAM chips on the two bottom pads, mapped at 0x70000000. A faithful
// transcription of PJRC cores/teensy4 startup.c configure_external_ram()
// (the silicon authority, per the bringup ledger) into this port's
// absolute-register house style; every offset re-derived from imxrt.h.
// The ROM does not touch FlexSPI2 -- without this init the region bus-faults.
// psram_init() sizes and QPI-enables up to two chips, answers total MB
// (0 / 8 / 16); psram_chip_id(n) answers the raw JEDEC id read during probe
// (0x5D0D = AP/ESP 8 MB, 0x5D9D = ISSI -- anything else is absent/bad solder).
#include <stdint.h>
#include "teensy41.h"

// --- FlexSPI2 (0x402A4000) ------------------------------------------------
#define FS2_BASE        0x402A4000u
#define FS2_MCR0        (FS2_BASE + 0x00u)
#define FS2_MCR1        (FS2_BASE + 0x04u)
#define FS2_MCR2        (FS2_BASE + 0x08u)
#define FS2_AHBCR       (FS2_BASE + 0x0Cu)
#define FS2_INTEN       (FS2_BASE + 0x10u)
#define FS2_INTR        (FS2_BASE + 0x14u)
#define FS2_LUTKEY      (FS2_BASE + 0x18u)
#define FS2_LUTCR       (FS2_BASE + 0x1Cu)
#define FS2_AHBRXBUF0CR0 (FS2_BASE + 0x20u)
#define FS2_AHBRXBUF1CR0 (FS2_BASE + 0x24u)
#define FS2_AHBRXBUF2CR0 (FS2_BASE + 0x28u)
#define FS2_AHBRXBUF3CR0 (FS2_BASE + 0x2Cu)
#define FS2_FLSHA1CR0   (FS2_BASE + 0x60u)
#define FS2_FLSHA2CR0   (FS2_BASE + 0x64u)
#define FS2_FLSHA1CR1   (FS2_BASE + 0x70u)
#define FS2_FLSHA2CR1   (FS2_BASE + 0x74u)
#define FS2_FLSHA1CR2   (FS2_BASE + 0x80u)
#define FS2_FLSHA2CR2   (FS2_BASE + 0x84u)
#define FS2_IPCR0       (FS2_BASE + 0xA0u)
#define FS2_IPCR1       (FS2_BASE + 0xA4u)
#define FS2_IPCMD       (FS2_BASE + 0xB0u)
#define FS2_IPRXFCR     (FS2_BASE + 0xB8u)
#define FS2_IPTXFCR     (FS2_BASE + 0xBCu)
#define FS2_RFDR0       (FS2_BASE + 0x100u)
#define FS2_LUT(n)      (FS2_BASE + 0x200u + 4u * (n))

#define MCR0_AHBGRANTWAIT_MASK (0xFFu << 24)
#define MCR0_IPGRANTWAIT_MASK  (0xFFu << 16)
#define MCR0_SCKFREERUNEN (1u << 14)
#define MCR0_COMBINATIONEN (1u << 13)
#define MCR0_DOZEEN     (1u << 12)
#define MCR0_HSEN       (1u << 11)
#define MCR0_ATDFEN     (1u << 7)
#define MCR0_ARDFEN     (1u << 6)
#define MCR0_RXCLKSRC_MASK (3u << 4)
#define MCR0_MDIS       (1u << 1)
#define MCR0_SWRESET    (1u << 0)
#define AHBCR_READADDROPT (1u << 6)
#define AHBCR_PREFETCHEN (1u << 5)
#define AHBCR_BUFFERABLEEN (1u << 4)
#define AHBCR_CACHABLEEN (1u << 3)
#define RXBUF_PREFETCHEN (1u << 31)
#define RXBUF_MASK      ((1u << 31) | (3u << 24) | (0xFu << 16) | 0xFFu)
#define IPRXFCR_CLRIPRXF (1u << 0)
#define IPTXFCR_CLRIPTXF (1u << 0)
#define INTR_IPCMDDONE  (1u << 0)
#define INTR_IPRXWA     (1u << 5)
#define IPCR1_ISEQID(n) ((uint32_t)(n) << 16)
#define IPCMD_TRG       (1u << 0)
#define FLSHCR1_TCSH(n) ((uint32_t)(n) << 5)
#define FLSHCR1_TCSS(n) ((uint32_t)(n) << 0)
#define FLSHCR2_AWRSEQID(n) ((uint32_t)(n) << 8)
#define FLSHCR2_ARDSEQID(n) ((uint32_t)(n) << 0)
#define LUTKEY_VALUE    0x5AF05AF0u
#define LUTCR_UNLOCK    (1u << 1)
// LUT instruction: opcode<<10 | pads<<8 | operand (and the same again <<16)
#define LI0(op, pads, o) ((((uint32_t)(op) & 0x3F) << 10) | (((uint32_t)(pads) & 3) << 8) | ((uint32_t)(o) & 0xFF))
#define LI1(op, pads, o) (LI0(op, pads, o) << 16)
#define OP_CMD   0x01
#define OP_RADDR 0x02
#define OP_WRITE 0x08
#define OP_READ  0x09
#define OP_DUMMY 0x0C
#define P1 0
#define P4 2

// --- IOMUXC: the eight FlexSPI2A pads (GPIO_EMC_22..29, ALT8) --------------
#define MUX_EMC(n)      (IOMUXC_BASE + 0x06Cu + 4u * ((n) - 22u))
#define PAD_EMC(n)      (IOMUXC_BASE + 0x25Cu + 4u * ((n) - 22u))
#define SEL_DQS_FA      (IOMUXC_BASE + 0x400u + 0x32Cu)
#define SEL_IO_FA(bit)  (IOMUXC_BASE + 0x400u + 0x330u + 4u * (bit))
#define SEL_SCK_FA      (IOMUXC_BASE + 0x400u + 0x350u)

// --- CCM: FlexSPI2 clock root + gate ---------------------------------------
#define CCM_CBCMR       (CCM_BASE + 0x18u)
#define CCM_CCGR7       (CCM_BASE + 0x84u)
#define CBCMR_FS2_PODF_MASK (7u << 29)
#define CBCMR_FS2_SEL_MASK  (3u << 8)
#define CBCMR_FS2_PODF(n)   ((uint32_t)(n) << 29)
#define CBCMR_FS2_SEL(n)    ((uint32_t)(n) << 8)

static uint32_t chip_id[2];

static void fs2_command(uint32_t index, uint32_t addr) {
  REG(FS2_IPCR0) = addr;
  REG(FS2_IPCR1) = IPCR1_ISEQID(index);
  REG(FS2_IPCMD) = IPCMD_TRG;
  while (!(REG(FS2_INTR) & INTR_IPCMDDONE)) {}
  REG(FS2_INTR) = INTR_IPCMDDONE; }

static uint32_t fs2_psram_id(uint32_t addr) {
  REG(FS2_IPCR0) = addr;
  REG(FS2_IPCR1) = IPCR1_ISEQID(3) | 4u;         // IDATSZ = 4 bytes
  REG(FS2_IPCMD) = IPCMD_TRG;
  while (!(REG(FS2_INTR) & INTR_IPCMDDONE)) {}
  uint32_t id = REG(FS2_RFDR0);
  REG(FS2_INTR) = INTR_IPCMDDONE | INTR_IPRXWA;
  return id; }

// probe one chip: exit-QPI, reset, read id -> size in MB (0 = absent)
static uint32_t fs2_psram_size(uint32_t addr, int slot) {
  fs2_command(0, addr);                          // exit QPI (harmless if SPI)
  fs2_command(1, addr);                          // reset enable
  fs2_command(2, addr);                          // reset
  uint32_t id = fs2_psram_id(addr);
  chip_id[slot] = id;
  switch (id & 0xFFFFu) {
    case 0x5D0D: return 8;                       // AP / Ipus / ESP / Lyontek
    case 0x5D9D:                                 // ISSI: size field, DS table 6.2
      switch ((id >> 21) & 7u) { case 3: return 8; case 4: return 16; }
      break; }
  return 0; }

uint32_t psram_chip_id(int slot) { return chip_id[slot]; }

uint32_t psram_init(void) {
  // pads: pullup/keeper + strong drive + max speed + hyst (PJRC's values)
  REG(PAD_EMC(22)) = 0x1B0F9u; REG(PAD_EMC(23)) = 0x110F9u;
  REG(PAD_EMC(24)) = 0x1B0F9u; REG(PAD_EMC(25)) = 0x100F9u;
  REG(PAD_EMC(26)) = 0x170F9u; REG(PAD_EMC(27)) = 0x170F9u;
  REG(PAD_EMC(28)) = 0x170F9u; REG(PAD_EMC(29)) = 0x170F9u;
  for (unsigned p = 22; p <= 29; p++) REG(MUX_EMC(p)) = 8u | 0x10u;  // ALT8 + SION
  REG(SEL_DQS_FA) = 1; REG(SEL_SCK_FA) = 1;
  for (unsigned b = 0; b < 4; b++) REG(SEL_IO_FA(b)) = 1;

  // clock root: 105.6 MHz (PODF 4, SEL 3 -- PJRC's shipping choice), gate on
  REG(CCM_CBCMR) = (REG(CCM_CBCMR) & ~(CBCMR_FS2_PODF_MASK | CBCMR_FS2_SEL_MASK))
    | CBCMR_FS2_PODF(4) | CBCMR_FS2_SEL(3);
  REG(CCM_CCGR7) |= 3u << 2;

  // controller: disable, configure, reset, LUT
  REG(FS2_MCR0) |= MCR0_MDIS;
  REG(FS2_MCR0) = (REG(FS2_MCR0) & ~(MCR0_AHBGRANTWAIT_MASK | MCR0_IPGRANTWAIT_MASK
      | MCR0_SCKFREERUNEN | MCR0_COMBINATIONEN | MCR0_DOZEEN | MCR0_HSEN
      | MCR0_ATDFEN | MCR0_ARDFEN | MCR0_RXCLKSRC_MASK | MCR0_SWRESET))
    | MCR0_AHBGRANTWAIT_MASK | MCR0_IPGRANTWAIT_MASK | (1u << 4) | MCR0_MDIS;
  REG(FS2_MCR1) = 0xFFFFFFFFu;                   // SEQWAIT | AHBBUSWAIT both max
  REG(FS2_MCR2) = (REG(FS2_MCR2) & ~((0xFFu << 24) | (1u << 19) | (1u << 15)
      | (1u << 14) | (1u << 11))) | (0x20u << 24);
  REG(FS2_AHBCR) = REG(FS2_AHBCR)
    & ~(AHBCR_READADDROPT | AHBCR_PREFETCHEN | AHBCR_BUFFERABLEEN | AHBCR_CACHABLEEN);
  REG(FS2_AHBRXBUF0CR0) = (REG(FS2_AHBRXBUF0CR0) & ~RXBUF_MASK) | RXBUF_PREFETCHEN | 64u;
  REG(FS2_AHBRXBUF1CR0) = (REG(FS2_AHBRXBUF0CR0) & ~RXBUF_MASK) | RXBUF_PREFETCHEN | 64u;
  REG(FS2_AHBRXBUF2CR0) = RXBUF_MASK;
  REG(FS2_AHBRXBUF3CR0) = RXBUF_MASK;
  REG(FS2_IPRXFCR) = (REG(FS2_IPRXFCR) & 0xFFFFFFC0u) | IPRXFCR_CLRIPRXF;
  REG(FS2_IPTXFCR) = (REG(FS2_IPTXFCR) & 0xFFFFFFC0u) | IPTXFCR_CLRIPTXF;
  REG(FS2_INTEN) = 0;
  REG(FS2_FLSHA1CR1) = FLSHCR1_TCSH(1) | FLSHCR1_TCSS(1);
  REG(FS2_FLSHA1CR2) = FLSHCR2_AWRSEQID(6) | FLSHCR2_ARDSEQID(5);
  REG(FS2_FLSHA2CR1) = FLSHCR1_TCSH(1) | FLSHCR1_TCSS(1);
  REG(FS2_FLSHA2CR2) = FLSHCR2_AWRSEQID(6) | FLSHCR2_ARDSEQID(5);
  REG(FS2_MCR0) &= ~MCR0_MDIS;

  REG(FS2_LUTKEY) = LUTKEY_VALUE;
  REG(FS2_LUTCR) = LUTCR_UNLOCK;
  for (unsigned i = 0; i < 64; i++) REG(FS2_LUT(i)) = 0;
  REG(FS2_MCR0) |= MCR0_SWRESET;
  while (REG(FS2_MCR0) & MCR0_SWRESET) {}

  REG(FS2_LUTKEY) = LUTKEY_VALUE;
  REG(FS2_LUTCR) = LUTCR_UNLOCK;
  REG(FS2_LUT(0))  = LI0(OP_CMD, P4, 0xF5);                            // 0: exit QPI
  REG(FS2_LUT(4))  = LI0(OP_CMD, P1, 0x66);                            // 1: reset enable
  REG(FS2_LUT(8))  = LI0(OP_CMD, P1, 0x99);                            // 2: reset
  REG(FS2_LUT(12)) = LI0(OP_CMD, P1, 0x9F) | LI1(OP_DUMMY, P1, 24);    // 3: read id
  REG(FS2_LUT(13)) = LI0(OP_READ, P1, 1);
  REG(FS2_LUT(16)) = LI0(OP_CMD, P1, 0x35);                            // 4: enter QPI
  REG(FS2_LUT(20)) = LI0(OP_CMD, P4, 0xEB) | LI1(OP_RADDR, P4, 24);    // 5: QPI read
  REG(FS2_LUT(21)) = LI0(OP_DUMMY, P4, 6) | LI1(OP_READ, P4, 1);
  REG(FS2_LUT(24)) = LI0(OP_CMD, P4, 0x38) | LI1(OP_RADDR, P4, 24);    // 6: QPI write
  REG(FS2_LUT(25)) = LI0(OP_WRITE, P4, 1);

  chip_id[0] = chip_id[1] = 0;
  uint32_t size1 = fs2_psram_size(0, 0);
  if (size1 == 0) return 0;
  REG(FS2_FLSHA1CR0) = size1 << 10;              // KB units
  fs2_command(4, 0);                             // chip 1 into QPI
  uint32_t size2 = fs2_psram_size(size1 << 20, 1);
  if (size2 > 0) {
    REG(FS2_FLSHA2CR0) = size2 << 10;
    fs2_command(4, size1 << 20); }               // chip 2 into QPI
  return size1 + size2; }
