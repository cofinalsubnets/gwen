// Teensy 4.1 bare-metal arch backend (no Teensyduino core): CCM clock
// bring-up, the LPUART6 console, GPT1 as the millisecond timer, and a thin
// GPIO layer for the on-board LED -- the C half, compiled by mooncc. The
// ROM-facing boot image (FlexSPI config block + IVT + boot data + vectors),
// the crt0, the HardFault shim, and the barrier/wfi/bkpt helpers live in
// boot.S: exact flash sections and bare instructions, gas-assembled. The
// FlexSPI/IVT lore (wrong-offset first-silicon stories) rides boot.S now.
#include <stdint.h>
#include "teensy41.h"

// Linker-provided bounds (teensy41.lds) + the boot.S vector table.
extern uint32_t __data_start__[], __data_end__[], __data_load__[];
extern uint32_t __bss_start__[], __bss_end__[];
extern void *const vectors[];

int main(void);

// --- fault diagnostics ----------------------------------------------------
// ARMv7E-M HardFault: capture the stacked exception frame so an attached SWD
// debugger lands on a known address. boot.S's isr_hardfault selects the
// active stack and branches here with the frame in r0.
volatile struct ai_fault {
  uint32_t r0, r1, r2, r3, r12, lr, pc, psr, sp, magic;
} ai_fault;

void hardfault_report(uint32_t *frame) {
  ai_fault.r0  = frame[0]; ai_fault.r1 = frame[1]; ai_fault.r2 = frame[2];
  ai_fault.r3  = frame[3]; ai_fault.r12 = frame[4]; ai_fault.lr = frame[5];
  ai_fault.pc  = frame[6]; ai_fault.psr = frame[7];
  ai_fault.sp  = (uint32_t)(uintptr_t) frame;
  ai_fault.magic = 0xFA017EDu;
  // say it on the wire while we still can (serial is polled, no IRQs needed);
  // the bootloader chip resets us shortly after the bkpt, and RAM re-zeroes.
  // CFSR/BFAR/MMFAR classify the fault (precise bus faults carry the address).
  { static const char hx[] = "0123456789abcdef";
    const char *tags[6] = { "\r\n; FAULT pc=", " lr=", " sp=", " cfsr=", " bfar=", " mmfar=" };
    uint32_t v[6] = { frame[6], frame[5], (uint32_t)(uintptr_t) frame,
                      REG(0xE000ED28u), REG(0xE000ED38u), REG(0xE000ED34u) };
    for (int k = 0; k < 6; k++) {
      for (const char *s = tags[k]; *s; s++) serial_putc(*s);
      for (int b = 28; b >= 0; b -= 4) serial_putc(hx[(v[k] >> b) & 15]); }
    serial_putc('\r'); serial_putc('\n'); }
  for (;;) {} }   // idle, report delivered (a bkpt here would re-enter DebugMon)

// --- caches ----------------------------------------------------------------
// XIP with the caches off fetches EVERY instruction over the 60 MHz QSPI: the
// egg bake crawls from seconds into hours (first silicon 2026-07-04: solid LED,
// no change -- not hung, CRAWLING; the slowness was the bug). The ARMv7-M
// default memory map already types both the flash window (0x60000000) and
// OCRAM (0x20200000) as Normal/cacheable, so enabling I+D at the SCB is the
// whole job -- no MPU regions needed. Sequence per the ARMv7-M ARM: invalidate,
// then enable. The D-cache invalidate walks sets x ways from CCSIDR (RT1062:
// 32 KB, 4-way, 32 B lines -> way field at bit 30, set field at bit 5).
#define SCB_CCR     0xE000ED14u
#define SCB_CCSIDR  0xE000ED80u
#define SCB_CSSELR  0xE000ED84u
#define SCB_ICIALLU 0xE000EF50u
#define SCB_DCISW   0xE000EF60u

static void caches_init(void) {
  arm_dsb_isb();
  REG(SCB_ICIALLU) = 0;
  arm_dsb_isb();
  REG(SCB_CCR) |= 1u << 17;                      // I-cache on
  arm_dsb_isb();
  REG(SCB_CSSELR) = 0;                           // select the L1 D-cache
  arm_dsb();
  uint32_t ccsidr = REG(SCB_CCSIDR);
  uint32_t sets = (ccsidr >> 13) & 0x7FFFu, ways = (ccsidr >> 3) & 0x3FFu;
  for (uint32_t s = 0; s <= sets; s++)
    for (uint32_t w = 0; w <= ways; w++)
      REG(SCB_DCISW) = (w << 30) | (s << 5);
  arm_dsb();
  REG(SCB_CCR) |= 1u << 16;                      // D-cache on
  arm_dsb_isb(); }

// boot.S's cstartup established our stack and falls in here.
void cmain(void) {
  // FPU on (CP10/CP11 full access) before any float-typed code runs.
  REG(SCB_CPACR) |= (0xFu << 20);
  // debug monitor ON: a bkpt with no debugger otherwise ESCALATES to lockup
  // (mute -- the supervisor just resets us). With MON_EN it vectors to the
  // fault reporter instead, so every __builtin_trap NAMES its pc on the wire.
  REG(0xE000EDFCu) |= (1u << 16);
  arm_dsb_isb();
  caches_init();
  clocks_init();          // pure MMIO, no .data/.bss reads -- so the big copy
                          // below already runs at 600 MHz
  // .data from its flash load address into OCRAM2; zero .bss.
  for (uint32_t *s = __data_load__, *d = __data_start__; d < __data_end__; ) *d++ = *s++;
  for (uint32_t *b = __bss_start__; b < __bss_end__; b++) *b = 0;
  REG(SCB_VTOR) = (uint32_t)(uintptr_t) vectors;
  serial_init();
  main();
  for (;;) arm_wfi(); }

// --- clocks ---------------------------------------------------------------
// The ARM core to 600 MHz (the ROM boots it at ~396), plus the two roots
// this frontend reads -- the LPUART clock (24 MHz osc) and GPT1 (24 MHz
// osc). Both ride the crystal, so the core retune changes neither the
// console baud nor the timebase math.
void clocks_init(void) {
  // Voltage BEFORE speed: DCDC core supply to 1.25 V, wait for it to settle.
  REG(DCDC_REG3) = (REG(DCDC_REG3) & ~DCDC_REG3_TRG_MASK) | DCDC_TRG_1V25;
  while (!(REG(DCDC_REG0) & DCDC_REG0_STS_DC_OK)) {}
  // Park periph_clk on the 24 MHz osc while the ARM PLL retunes.
  REG(CCM_CBCMR) = (REG(CCM_CBCMR) & ~CBCMR_PERIPH_CLK2_MASK) | CBCMR_PERIPH_CLK2_OSC;
  REG(CCM_CBCDR) |= CBCDR_PERIPH_CLK_SEL;
  while (REG(CCM_CDHIPR) & CDHIPR_PERIPH_CLK_SEL_BUSY) {}
  // ARM PLL: 24 MHz * 100 / 2 = 1200 MHz, then the core divider /2 = 600.
  REG(CCM_ANALOG_PLL_ARM) = PLL_ARM_POWERDOWN;
  REG(CCM_ANALOG_PLL_ARM) = PLL_ARM_ENABLE | 100u;
  while (!(REG(CCM_ANALOG_PLL_ARM) & PLL_ARM_LOCK)) {}
  REG(CCM_CCSR) &= ~CCSR_PLL1_SW_CLK_SEL;        // core rides pll1_main
  REG(CCM_CACRR) = 1u;                            // /2
  while (REG(CCM_CDHIPR) & CDHIPR_ARM_PODF_BUSY) {}
  // AHB /1 (600 MHz), IPG /4 (150 MHz, its ceiling); pre-periph = the
  // divided ARM PLL; then un-park.
  REG(CCM_CBCDR) = (REG(CCM_CBCDR) & ~(CBCDR_AHB_PODF_MASK | CBCDR_IPG_PODF_MASK))
                 | CBCDR_IPG_PODF_DIV4;
  while (REG(CCM_CDHIPR) & CDHIPR_AHB_PODF_BUSY) {}
  REG(CCM_CBCMR) = (REG(CCM_CBCMR) & ~CBCMR_PRE_PERIPH_MASK) | CBCMR_PRE_PERIPH_PLL1;
  REG(CCM_CBCDR) &= ~CBCDR_PERIPH_CLK_SEL;
  while (REG(CCM_CDHIPR) & CDHIPR_PERIPH_CLK_SEL_BUSY) {}

  // Gate LPUART6 (CCGR3 CG3) and GPT1 (CCGR1 CG10/CG11) on.
  REG(CCM_CCGR3) |= CCGR_ON(3);
  REG(CCM_CCGR1) |= CCGR_ON(GPT1_CCGR_BUS) | CCGR_ON(GPT1_CCGR_SERIAL);
  // LPUART clock = 24 MHz osc, no further divide.
  uint32_t c = REG(CCM_CSCDR1);
  c = (c & ~CSCDR1_UART_CLK_PODF_MASK) | CSCDR1_UART_CLK_SEL_OSC;
  REG(CCM_CSCDR1) = c;

  // GPT1: reset, then free-run off the 24 MHz osc with a /24 prescaler so the
  // counter ticks at 1 MHz (1 us). ai_clock() divides to milliseconds.
  REG(GPT1_CR) = GPT_CR_SWR;
  while (REG(GPT1_CR) & GPT_CR_SWR) {}
  REG(GPT1_PR) = 24u - 1u;
  REG(GPT1_CR) = GPT_CR_CLKSRC_24M | GPT_CR_EN_24M | GPT_CR_FRR | GPT_CR_ENMOD;
  REG(GPT1_CR) |= GPT_CR_EN; }

// --- LPUART6 console ------------------------------------------------------
void serial_init(void) {
  // pin1 -> LPUART6_TX, pin0 -> LPUART6_RX (both ALT2); daisy-chain the RX.
  REG(IOMUXC_SW_MUX_GPIO_AD_B0_02) = MUX_ALT(2);
  REG(IOMUXC_SW_MUX_GPIO_AD_B0_03) = MUX_ALT(2);
  REG(IOMUXC_LPUART6_RX_SELECT) = 1u;            // select GPIO_AD_B0_03
  // the pad's OUTPUT DRIVER. The mux alone routes the LPUART TX signal to the
  // pad, but a pad left at its reset-default drive strength does not drive the
  // line -- first silicon (2026-07-12) had TDRE asserting and bytes clocking out
  // with the wire dead idle until these were set. DSE6 + medium speed + keeper
  // (PJRC's UART pad config) gives TX a real driver; the RX pad takes the keeper
  // so a disconnected input does not float.
  REG(IOMUXC_SW_PAD_GPIO_AD_B0_02) = PAD_CTL_UART;   // TX drive
  REG(IOMUXC_SW_PAD_GPIO_AD_B0_03) = PAD_CTL_UART;   // RX keeper

  // 115200 8N1 from the 24 MHz UART clock: OSR=16, SBR=13 -> 115384 (+0.16%).
  REG(LPUART_CTRL) = 0;                           // disable while configuring
  REG(LPUART_BAUD) = LPUART_BAUD_OSR(16) | LPUART_BAUD_SBR(13) | LPUART_BAUD_BOTHEDGE;
  REG(LPUART_FIFO) |= LPUART_FIFO_TXFE | LPUART_FIFO_RXFE;
  REG(LPUART_CTRL) = LPUART_CTRL_TE | LPUART_CTRL_RE;
  // the RX pad floats until the mux above lands, and boot-window noise can
  // wedge the FIFO (garbage in flight at enable): flush RX, clear every
  // error flag, drain anything already latched -- start CLEAN
  REG(LPUART_FIFO) |= LPUART_FIFO_RXFLUSH;
  REG(LPUART_STAT) = LPUART_STAT_ERR;
  while (REG(LPUART_STAT) & LPUART_STAT_RDRF) (void) REG(LPUART_DATA); }

// The RX FIFO is 4 deep and the editor answers every received char with a
// ~20-byte redraw at the same baud, so a paste outruns the hardware 20:1 --
// no polling discipline can save it at the FIFO. The soft ring absorbs the
// difference: serial_putc's TDRE stalls are exactly where inbound bytes died,
// so the pump runs there (and in every RX poll). A latched overrun both
// halts reception AND desyncs the FIFO pointers (Kinetis-lineage block), so
// the recovery is save-what's-readable, RXFLUSH, clear every error flag.
static uint8_t  rx_ring[1024];
static uint32_t rx_w, rx_r;
static void rx_pump(void) {
  for (;;) {
    if (REG(LPUART_STAT) & LPUART_STAT_OR) {
      while (REG(LPUART_STAT) & LPUART_STAT_RDRF)
        rx_ring[rx_w++ & 1023u] = REG(LPUART_DATA) & 0xff;
      REG(LPUART_FIFO) |= LPUART_FIFO_RXFLUSH;
      REG(LPUART_STAT) = LPUART_STAT_ERR;
      continue; }
    if (!(REG(LPUART_STAT) & LPUART_STAT_RDRF)) return;
    rx_ring[rx_w++ & 1023u] = REG(LPUART_DATA) & 0xff; } }

void serial_putc(int c) {
  while (!(REG(LPUART_STAT) & LPUART_STAT_TDRE)) rx_pump();
  REG(LPUART_DATA) = (uint32_t)(c & 0xff); }

int serial_rx_ready(void) {
  rx_pump();
  return rx_w != rx_r; }

int serial_getc(void) {
  while (rx_w == rx_r) rx_pump();
  return rx_ring[rx_r++ & 1023u]; }

// --- clock: milliseconds since boot (GPT1 counts microseconds) -----------
uintptr_t ai_clock(void) { return REG(GPT1_CNT) / 1000u; }

// --- GPIO -----------------------------------------------------------------
// Scaffold scope: GPIO2 bit operations plus the IOMUXC mux for pin 13 (the
// LED). A full Teensy pin map (pad -> GPIO bank/bit -> ALT5 mux, all 55 pins)
// is a TODO; here `pin` is a GPIO2 bit index and pin 13 (LED_BIT) is the one
// pad we mux. Mirrors rp2040.c's gpio_* contract so main.c's nifs are shared.
void gpio_init(unsigned pin) {
  if (pin == LED_BIT) REG(IOMUXC_SW_MUX_GPIO_B0_03) = MUX_ALT(5);
  REG(GPIO2_GDIR) &= ~(1u << pin); }            // input until set_dir

void gpio_set_dir(unsigned pin, int out) {
  if (out) REG(GPIO2_GDIR) |= (1u << pin);
  else     REG(GPIO2_GDIR) &= ~(1u << pin); }

void gpio_put(unsigned pin, int hi) {
  REG(hi ? GPIO2_DR_SET : GPIO2_DR_CLEAR) = 1u << pin; }

int gpio_get(unsigned pin) { return (REG(GPIO2_PSR) >> pin) & 1u; }
