// Nucleo-F446RE bare-metal arch backend (no HAL): the 180 MHz PLL bring-up,
// the USART2 console (the ST-LINK virtual COM port), TIM2 as the microsecond
// timer, and the LD2/button GPIO -- the C half, compiled by mooncc -t
// thumb2sp. The vector table, crt0, HardFault shim, and the barrier/wfi/
// semihosting helpers live in mkboot.l (bare instructions, laid from holo IR).
#include <stdint.h>
#include "nucleo446.h"

// Linker-provided bounds (nucleo446.lds) + the mkboot.l vector table.
extern uint32_t __data_start__[], __data_end__[], __data_load__[];
extern uint32_t __bss_start__[], __bss_end__[];
extern void *const vectors[];

int main(void);

// --- fault diagnostics ----------------------------------------------------
// mkboot.l's isr_hardfault selects the active stack and branches here with the
// exception frame in r0; the reporter runs on a fresh stack. Names the fault
// on the wire, then idles (QSMOKE: leaves through qemu as 98 -- loud, not a
// mute lockup).
void hardfault_report(uint32_t *frame) {
  static const char hx[] = "0123456789abcdef";
  const char *tags[5] = { "\r\n; FAULT pc=", " lr=", " cfsr=", " bfar=", " mmfar=" };
  uint32_t v[5] = { frame[6], frame[5],
                    REG(0xE000ED28u), REG(0xE000ED38u), REG(0xE000ED34u) };
  for (int k = 0; k < 5; k++) {
    for (const char *s = tags[k]; *s; s++) serial_putc(*s);
    for (int b = 28; b >= 0; b -= 4) serial_putc(hx[(v[k] >> b) & 15]); }
  serial_putc('\r'); serial_putc('\n');
#ifdef QSMOKE
  sh_exit(98);
#endif
  for (;;) {} }

// mkboot.l's cstartup established our stack and falls in here.
void cmain(void) {
  // FPU on (CP10/CP11 full access) before any float-typed code runs --
  // thumb2sp still rides the S/D registers for f32 arith and f64 transfers.
  REG(SCB_CPACR) |= (0xFu << 20);
  arm_dsb_isb();
  // .data from its flash load address into SRAM; zero .bss. BEFORE clocks:
  // clocks_init records nothing, but main's first read of any global must
  // not race the copy (the 16 MHz copy costs nothing here, unlike the
  // teensy's 600 MHz motive for the other order).
  for (uint32_t *s = __data_load__, *d = __data_start__; d < __data_end__; ) *d++ = *s++;
  for (uint32_t *b = __bss_start__; b < __bss_end__; b++) *b = 0;
  REG(SCB_VTOR) = (uint32_t)(uintptr_t) vectors;
  clocks_init();
  serial_init();
  main();
  for (;;) arm_wfi(); }

// --- clocks ---------------------------------------------------------------
// The core to 180 MHz: 8 MHz HSE-bypass (the ST-LINK MCO) -> PLL /4*180/2,
// regulator scale 1 + over-drive, flash at 5 wait states, APB1 /4 (45 MHz),
// APB2 /2 (90 MHz). EVERY wait is BOUNDED (the teensy41 lesson): a timeout
// bails back to the 16 MHz HSI reset clock and the firmware still runs --
// which is also exactly the lane qemu takes (its RCC is a stub reading 0, so
// HSERDY never sets and the smoke build lives its whole life on "HSI").
// The banner self-reports which path won.
static int wait_reg(uint32_t addr, uint32_t mask) {
  for (uint32_t i = 0; i < 4000000u; i++)
    if (REG(addr) & mask) return 1;
  return 0; }

void clocks_init(void) {
  REG(RCC_CR) |= RCC_CR_HSEBYP | RCC_CR_HSEON;
  if (!wait_reg(RCC_CR, RCC_CR_HSERDY)) return;      // no HSE: stay on HSI
  // Regulator scale 1 before the PLL spins up.
  REG(RCC_APB1ENR) |= APB1ENR_PWR;
  REG(PWR_CR) |= PWR_CR_VOS_SCALE1;
  REG(RCC_PLLCFGR) = PLLCFGR_180;
  REG(RCC_CR) |= RCC_CR_PLLON;
  // Over-drive (RM0390: required above 168 MHz), switched in while the PLL locks.
  REG(PWR_CR) |= PWR_CR_ODEN;
  if (!wait_reg(PWR_CSR, PWR_CSR_ODRDY)) { REG(RCC_CR) &= ~RCC_CR_PLLON; return; }
  REG(PWR_CR) |= PWR_CR_ODSWEN;
  if (!wait_reg(PWR_CSR, PWR_CSR_ODSWRDY)) { REG(RCC_CR) &= ~RCC_CR_PLLON; return; }
  if (!wait_reg(RCC_CR, RCC_CR_PLLRDY)) { REG(RCC_CR) &= ~RCC_CR_PLLON; return; }
  // Wait states BEFORE the switch; bus dividers with it (APB1/APB2 ceilings).
  REG(FLASH_ACR) = FLASH_ACR_180;
  uint32_t c = REG(RCC_CFGR) & ~(CFGR_PPRE1_MASK | CFGR_PPRE2_MASK | CFGR_SW_MASK);
  REG(RCC_CFGR) = c | CFGR_PPRE1_DIV4 | CFGR_PPRE2_DIV2 | CFGR_SW_PLL;
  for (uint32_t i = 0; i < 4000000u; i++)
    if ((REG(RCC_CFGR) & CFGR_SWS_MASK) == CFGR_SWS_PLL) break; }

// Self-reported core clock, derived from the LIVE RCC state (not from what
// clocks_init intended) -- a silently-failed retune is visible on every boot.
uint32_t clock_mhz(void) {
  if ((REG(RCC_CFGR) & CFGR_SWS_MASK) != CFGR_SWS_PLL) return 16;   // HSI reset clock
  uint32_t p = REG(RCC_PLLCFGR);
  uint32_t in = (p & (1u << 22)) ? 8u : 16u;         // HSE-bypass 8 / HSI 16
  uint32_t m = p & 0x3Fu, n = (p >> 6) & 0x1FFu;
  uint32_t div = 2u * (((p >> 16) & 3u) + 1u);       // PLLP: 00 = /2
  return in * n / m / div; }

// APB1 peripheral clock (USART2's), from the live PPRE1 field.
static uint32_t pclk1_hz(void) {
  uint32_t pre = (REG(RCC_CFGR) & CFGR_PPRE1_MASK) >> 10;
  uint32_t hz = clock_mhz() * 1000000u;
  return pre < 4u ? hz : hz >> (pre - 3u); }

// --- USART2 console (PA2 TX / PA3 RX, AF7 -- the ST-LINK VCP) -------------
void serial_init(void) {
  REG(RCC_AHB1ENR) |= AHB1ENR_GPIOA;
  REG(RCC_APB1ENR) |= APB1ENR_USART2;
  uint32_t m = REG(GPIO_MODER(GPIOA_BASE));
  m = (m & ~((3u << 4) | (3u << 6))) | (2u << 4) | (2u << 6);   // PA2/PA3 alternate
  REG(GPIO_MODER(GPIOA_BASE)) = m;
  uint32_t a = REG(GPIO_AFRL(GPIOA_BASE));
  a = (a & ~((0xFu << 8) | (0xFu << 12))) | (7u << 8) | (7u << 12);  // AF7 = USART2
  REG(GPIO_AFRL(GPIOA_BASE)) = a;
  // 115200 8N1, oversample 16: BRR = pclk1/115200 in 16ths (mantissa.frac).
  REG(USART2_CR1) = 0;
  REG(USART2_BRR) = (pclk1_hz() + 115200u / 2u) / 115200u;
  REG(USART2_CR1) = USART_CR1_UE | USART_CR1_TE | USART_CR1_RE; }

void serial_putc(int c) {
  while (!(REG(USART2_SR) & USART_SR_TXE)) {}
  REG(USART2_DR) = (uint32_t)(c & 0xff); }

int serial_rx_ready(void) { return (REG(USART2_SR) & USART_SR_RXNE) != 0; }

int serial_getc(void) {
  while (!serial_rx_ready()) {}
  return (int)(REG(USART2_DR) & 0xffu); }

// --- clock: milliseconds since boot (TIM2 counts microseconds) ------------
// TIM2 is 32-bit on APB1; its timer clock is 2x PCLK1 whenever PPRE1 divides
// (90 MHz on the PLL path, 16 MHz on HSI where PPRE1 = /1).
void timer_init(void) {
  REG(RCC_APB1ENR) |= APB1ENR_TIM2;
  uint32_t pre = (REG(RCC_CFGR) & CFGR_PPRE1_MASK) >> 10;
  uint32_t hz = pre < 4u ? pclk1_hz() : 2u * pclk1_hz();
  REG(TIM2_CR1) = 0;
  REG(TIM2_PSC) = hz / 1000000u - 1u;               // 1 MHz tick
  REG(TIM2_ARR) = 0xFFFFFFFFu;
  REG(TIM2_EGR) = TIM_EGR_UG;                       // latch the prescaler
  REG(TIM2_CR1) = TIM_CR1_CEN; }

uint32_t clock_ms(void) { return REG(TIM2_CNT) / 1000u; }

// --- GPIO: the LD2 LED (PA5) and the user button (PC13, low = pressed) ----
void led_init(void) {
  REG(RCC_AHB1ENR) |= AHB1ENR_GPIOA | AHB1ENR_GPIOC;
  uint32_t m = REG(GPIO_MODER(GPIOA_BASE));
  REG(GPIO_MODER(GPIOA_BASE)) = (m & ~(3u << (2u * LED_PIN))) | (1u << (2u * LED_PIN));
  REG(GPIO_MODER(GPIOC_BASE)) &= ~(3u << (2u * BTN_PIN)); }   // input

void led_put(int hi) {
  REG(GPIO_BSRR(GPIOA_BASE)) = hi ? (1u << LED_PIN) : (1u << (LED_PIN + 16u)); }

int btn_get(void) { return !((REG(GPIO_IDR(GPIOC_BASE)) >> BTN_PIN) & 1u); }
