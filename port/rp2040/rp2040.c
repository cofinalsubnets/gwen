// RP2040 bare-metal arch backend (no Pico SDK): vector table + crt0, the
// XOSC+PLL clock bring-up, the UART0 serial console, the microsecond timer,
// and the SIO GPIO helpers. Plays the role of arch/<arch>/arch.c for the
// freestanding kernel, but for a Cortex-M0+ booting from flash XIP.
#include <stdint.h>
#include "rp2040.h"

// Linker-provided bounds (rp2040.lds).
extern uint32_t __data_start__[], __data_end__[], __data_load__[];
extern uint32_t __bss_start__[], __bss_end__[], __stack_top__[];

int main(void);

// --- fault diagnostics ----------------------------------------------------
// ARMv6-M (Cortex-M0+) has no CFSR/BFAR, so a fault says only where it stopped:
// halt on a breakpoint and let an attached SWD debugger read the frame the
// hardware stacked. ⚠ this handler used to select MSP/PSP and copy that frame
// into a struct, which wants `naked` plus mrs/tst -- neither in holo's thumb1
// lane. The frame is still on the stack for the debugger; only the copy is gone.
void isr_hardfault(void) { for (;;) asm volatile("trap"); }

// ⚠ the templates here are holo's NEUTRAL mnemonics, not ARM's: `trap` is the
// BKPT this backend lays (crew/holo/thumb1.l). wfe/wfi have no thumb1 row yet,
// so an idle handler spins instead of parking.
static void default_handler(void) { for (;;) asm volatile("trap"); }

// --- crt0 / reset ---------------------------------------------------------
// boot2 has already loaded SP from vectors[0] and handed control here. Copy
// .data from its flash load address into SRAM, zero .bss, point VTOR at our
// table, bring up the clocks, then enter main.
__attribute__((used, noreturn)) void reset_handler(void) {
  REG(VTOR_ADDR) = 0x10000100u;
  for (uint32_t *s = __data_load__, *d = __data_start__; d < __data_end__; ) *d++ = *s++;
  for (uint32_t *b = __bss_start__; b < __bss_end__; b++) *b = 0;
  clocks_init();
  main();
  for (;;) asm volatile("trap"); }

// --- vector table (at 0x10000100) -----------------------------------------
// Only the M0+ system vectors; no peripheral IRQs are enabled, so the rest
// park in default_handler. vectors[0] is the initial SP, not a function.
__attribute__((section(".vectors"), used))
void *const vectors[] = {
  (void *) __stack_top__,   // 0  initial SP
  reset_handler,            // 1  reset
  default_handler,          // 2  NMI
  isr_hardfault,            // 3  HardFault
  0, 0, 0, 0, 0, 0, 0,      // 4..10 reserved
  default_handler,          // 11 SVCall
  0, 0,                     // 12..13 reserved
  default_handler,          // 14 PendSV
  default_handler,          // 15 SysTick
};

// --- clocks: XOSC (12 MHz) + PLL_SYS -> 125 MHz clk_sys; clk_peri off XOSC --
void clocks_init(void) {
  // 1. start the 12 MHz crystal oscillator and wait for it to stabilise.
  REG(XOSC_CTRL) = XOSC_CTRL_1_15MHZ;
  REG(XOSC_STARTUP) = 47;                 // ~1 ms at 12 MHz: (12e6/1000)/256
  REG(XOSC_CTRL + REG_SET) = XOSC_CTRL_ENABLE;
  while (!(REG(XOSC_STATUS) & XOSC_STATUS_STABLE)) {}

  // 2. clk_ref -> xosc; park clk_sys on clk_ref while we touch the PLL.
  REG(CLK_REF_DIV) = CLK_DIV_1;
  REG(CLK_REF_CTRL) = CLK_REF_SRC_XOSC;
  while (REG(CLK_REF_SELECTED) != (1u << CLK_REF_SRC_XOSC)) {}
  REG(CLK_SYS_CTRL) = CLK_SYS_SRC_REF;
  while (REG(CLK_SYS_SELECTED) != (1u << CLK_SYS_SRC_REF)) {}

  // 3. PLL_SYS: refdiv 1, fbdiv 125 -> VCO 1500 MHz; postdiv 6/2 -> 125 MHz.
  REG(RESETS_RESET + REG_CLR) = RST_PLL_SYS;
  while (!(REG(RESETS_RESET_DONE) & RST_PLL_SYS)) {}
  REG(PLL_CS) = 1;                        // REFDIV = 1
  REG(PLL_FBDIV) = 125;
  REG(PLL_PWR + REG_CLR) = PLL_PWR_PD | PLL_PWR_VCOPD;   // power on main + VCO
  while (!(REG(PLL_CS) & PLL_CS_LOCK)) {}
  REG(PLL_PRIM) = (6u << 16) | (2u << 12);
  REG(PLL_PWR + REG_CLR) = PLL_PWR_POSTDIVPD;            // power on postdiv

  // 4. clk_sys -> PLL (glitchless: set AUXSRC with SRC=ref, then SRC=aux).
  REG(CLK_SYS_DIV) = CLK_DIV_1;
  REG(CLK_SYS_CTRL) = (CLK_SYS_AUX_PLLSYS << CLK_AUXSRC_LSB) | CLK_SYS_SRC_REF;
  REG(CLK_SYS_CTRL) = (CLK_SYS_AUX_PLLSYS << CLK_AUXSRC_LSB) | CLK_SYS_SRC_AUX;
  while (REG(CLK_SYS_SELECTED) != (1u << CLK_SYS_SRC_AUX)) {}

  // 5. clk_peri -> xosc (exact 12 MHz reference for the UART baud divisor).
  REG(CLK_PERI_CTRL) = 0;
  REG(CLK_PERI_CTRL) = CLK_PERI_ENABLE | (CLK_PERI_AUX_XOSC << CLK_AUXSRC_LSB); }

// --- UART0 (PL011) console -----------------------------------------------
void serial_init(void) {
  REG(RESETS_RESET + REG_CLR) = RST_UART0 | RST_IO_BANK0 | RST_PADS_BANK0;
  uint32_t want = RST_UART0 | RST_IO_BANK0 | RST_PADS_BANK0;
  while ((REG(RESETS_RESET_DONE) & want) != want) {}

  // GPIO0 -> UART0 TX, GPIO1 -> UART0 RX; enable the RX pad input.
  REG(IO_BANK0_GPIO_CTRL(0)) = IO_FUNC_UART;
  REG(IO_BANK0_GPIO_CTRL(1)) = IO_FUNC_UART;
  REG(PADS_BANK0_GPIO(1) + REG_CLR) = PADS_OD;
  REG(PADS_BANK0_GPIO(1) + REG_SET) = PADS_IE;

  // baud divisor from clk_peri (= XOSC, 12 MHz) at 115200, 8N1, FIFOs on.
  unsigned div = (8u * XOSC_HZ) / 115200u;
  unsigned ibrd = div >> 7, fbrd = ((div & 0x7fu) + 1u) >> 1;
  if (!ibrd) { ibrd = 1; fbrd = 0; }
  REG(UART_IBRD) = ibrd;
  REG(UART_FBRD) = fbrd;
  REG(UART_LCR_H) = UART_LCR_H_WLEN_8 | UART_LCR_H_FEN;   // write latches baud
  REG(UART_CR) = UART_CR_UARTEN | UART_CR_TXE | UART_CR_RXE; }

void serial_putc(int c) {
  while (REG(UART_FR) & UART_FR_TXFF) {}
  REG(UART_DR) = (uint32_t) (c & 0xff); }

int serial_rx_ready(void) { return !(REG(UART_FR) & UART_FR_RXFE); }

int serial_getc(void) {
  while (REG(UART_FR) & UART_FR_RXFE) {}
  return REG(UART_DR) & 0xff; }

// --- clock: milliseconds since boot (64-bit us timer) --------------------
uint32_t clock_ms(void) {
  uint32_t lo = REG(TIMER_TIMELR);        // reading LR latches HR
  uint32_t hi = REG(TIMER_TIMEHR);
  return (uint32_t) ((((uint64_t) hi << 32) | lo) / 1000u); }

// --- GPIO via SIO ---------------------------------------------------------
void gpio_init(unsigned pin) {
  REG(IO_BANK0_GPIO_CTRL(pin)) = IO_FUNC_SIO;
  REG(PADS_BANK0_GPIO(pin) + REG_CLR) = PADS_OD;
  REG(PADS_BANK0_GPIO(pin) + REG_SET) = PADS_IE;
  REG(SIO_GPIO_OE_CLR) = 1u << pin; }     // input until set_dir

void gpio_set_dir(unsigned pin, int out) {
  REG(out ? SIO_GPIO_OE_SET : SIO_GPIO_OE_CLR) = 1u << pin; }

void gpio_put(unsigned pin, int hi) {
  REG(hi ? SIO_GPIO_OUT_SET : SIO_GPIO_OUT_CLR) = 1u << pin; }

int gpio_get(unsigned pin) { return (REG(SIO_GPIO_IN) >> pin) & 1u; }
