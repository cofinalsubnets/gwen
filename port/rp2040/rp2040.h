// RP2040 register map (Cortex-M0+, bare metal -- no Pico SDK).
//
// Only the registers the bare-metal frontend touches: resets, the XOSC+PLL
// clock bring-up, UART0 (a PL011, same PrimeCell layout the aarch64 console
// drives), the 64-bit timer, SIO/IO_BANK0/PADS_BANK0 for GPIO, and the M0+
// VTOR. Values lifted from the RP2040 datasheet register listings; kept terse.
#pragma once
#include <stdint.h>

#define REG(a) (*(volatile uint32_t *)(a))
// Atomic register-access aliases (datasheet 2.1.2): write to base+offset.
#define REG_XOR 0x1000u
#define REG_SET 0x2000u
#define REG_CLR 0x3000u

// --- peripheral bases ----------------------------------------------------
#define CLOCKS_BASE     0x40008000u
#define RESETS_BASE     0x4000c000u
#define IO_BANK0_BASE   0x40014000u
#define PADS_BANK0_BASE 0x4001c000u
#define XOSC_BASE       0x40024000u
#define PLL_SYS_BASE    0x40028000u
#define UART0_BASE      0x40034000u
#define TIMER_BASE      0x40054000u
#define SIO_BASE        0xd0000000u
#define PPB_BASE        0xe0000000u
#define VTOR_ADDR       (PPB_BASE + 0xed08u)   // M0PLUS_VTOR

// --- RESETS --------------------------------------------------------------
#define RESETS_RESET      (RESETS_BASE + 0x0u)
#define RESETS_RESET_DONE (RESETS_BASE + 0x8u)
#define RST_IO_BANK0   (1u << 5)
#define RST_PADS_BANK0 (1u << 8)
#define RST_PLL_SYS    (1u << 12)
#define RST_UART0      (1u << 22)

// --- XOSC (12 MHz crystal) ----------------------------------------------
#define XOSC_CTRL    (XOSC_BASE + 0x0u)
#define XOSC_STATUS  (XOSC_BASE + 0x4u)
#define XOSC_STARTUP (XOSC_BASE + 0xcu)
#define XOSC_CTRL_ENABLE   (0xfabu << 12)
#define XOSC_CTRL_1_15MHZ  0xaa0u
#define XOSC_STATUS_STABLE (1u << 31)
#define XOSC_HZ            12000000u

// --- PLL_SYS -------------------------------------------------------------
#define PLL_CS       (PLL_SYS_BASE + 0x0u)
#define PLL_PWR      (PLL_SYS_BASE + 0x4u)
#define PLL_FBDIV    (PLL_SYS_BASE + 0x8u)
#define PLL_PRIM     (PLL_SYS_BASE + 0xcu)
#define PLL_CS_LOCK      (1u << 31)
#define PLL_PWR_VCOPD    (1u << 5)
#define PLL_PWR_POSTDIVPD (1u << 3)
#define PLL_PWR_DSMPD    (1u << 2)
#define PLL_PWR_PD       (1u << 0)

// --- CLOCKS --------------------------------------------------------------
#define CLK_REF_CTRL      (CLOCKS_BASE + 0x30u)
#define CLK_REF_DIV       (CLOCKS_BASE + 0x34u)
#define CLK_REF_SELECTED  (CLOCKS_BASE + 0x38u)
#define CLK_SYS_CTRL      (CLOCKS_BASE + 0x3cu)
#define CLK_SYS_DIV       (CLOCKS_BASE + 0x40u)
#define CLK_SYS_SELECTED  (CLOCKS_BASE + 0x44u)
#define CLK_PERI_CTRL     (CLOCKS_BASE + 0x48u)
#define CLK_PERI_SELECTED (CLOCKS_BASE + 0x50u)
#define CLK_REF_SRC_XOSC   0x2u          // CLK_REF_CTRL.SRC
#define CLK_SYS_SRC_REF    0x0u          // CLK_SYS_CTRL.SRC = clk_ref
#define CLK_SYS_SRC_AUX    0x1u          // CLK_SYS_CTRL.SRC = aux (PLL)
#define CLK_SYS_AUX_PLLSYS 0x0u          // CLK_SYS_CTRL.AUXSRC
#define CLK_PERI_AUX_XOSC  0x4u          // CLK_PERI_CTRL.AUXSRC = xosc
#define CLK_PERI_ENABLE    (1u << 11)
#define CLK_AUXSRC_LSB     5
#define CLK_DIV_1          (1u << 8)     // integer divisor 1 (frac in [31:8])

// --- UART0 (PL011) -------------------------------------------------------
#define UART_DR    (UART0_BASE + 0x00u)
#define UART_FR    (UART0_BASE + 0x18u)
#define UART_IBRD  (UART0_BASE + 0x24u)
#define UART_FBRD  (UART0_BASE + 0x28u)
#define UART_LCR_H (UART0_BASE + 0x2cu)
#define UART_CR    (UART0_BASE + 0x30u)
#define UART_FR_TXFF (1u << 5)
#define UART_FR_RXFE (1u << 4)
#define UART_LCR_H_WLEN_8 (0x3u << 5)
#define UART_LCR_H_FEN    (1u << 4)
#define UART_CR_UARTEN (1u << 0)
#define UART_CR_TXE    (1u << 8)
#define UART_CR_RXE    (1u << 9)

// --- IO_BANK0 / PADS_BANK0 ----------------------------------------------
#define IO_BANK0_GPIO_CTRL(n) (IO_BANK0_BASE + 0x4u + (n) * 0x8u)  // GPIOn_CTRL
#define PADS_BANK0_GPIO(n)    (PADS_BANK0_BASE + 0x4u + (n) * 0x4u)
#define PADS_IE (1u << 6)
#define PADS_OD (1u << 7)
#define IO_FUNC_UART 2u      // GPIO0/1 -> UART0 TX/RX
#define IO_FUNC_SIO  5u      // GPIO    -> SIO (software GPIO)

// --- SIO (single-cycle IO, GPIO) ----------------------------------------
#define SIO_GPIO_IN      (SIO_BASE + 0x04u)
#define SIO_GPIO_OUT_SET (SIO_BASE + 0x14u)
#define SIO_GPIO_OUT_CLR (SIO_BASE + 0x18u)
#define SIO_GPIO_OE_SET  (SIO_BASE + 0x24u)
#define SIO_GPIO_OE_CLR  (SIO_BASE + 0x28u)

// --- TIMER (64-bit microsecond) -----------------------------------------
#define TIMER_TIMEHR (TIMER_BASE + 0x08u)   // latched high (read TIMELR first)
#define TIMER_TIMELR (TIMER_BASE + 0x0cu)   // latched low (latches high)

// the board's onboard LED, the one output every Pico has
#define LED_PIN 25u

// arch backend (rp2040.c)
void clocks_init(void);
void serial_init(void);
void serial_putc(int c);
int  serial_getc(void);
int  serial_rx_ready(void);
uint32_t clock_ms(void);
void gpio_init(unsigned pin);
void gpio_set_dir(unsigned pin, int out);
void gpio_put(unsigned pin, int hi);
int  gpio_get(unsigned pin);
