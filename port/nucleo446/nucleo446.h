// Nucleo-F446RE (ST STM32F446RE) register map -- bare metal, no HAL.
//
// The F446 is a Cortex-M4 (ARMv7E-M, FPv4-SP-D16: a SINGLE-precision FPU, so
// the firmware compiles `mooncc -t thumb2sp` -- f64 transfers ride the d-regs,
// f64 arithmetic softens to __aeabi_* libgcc calls, the playdate lane). Boots
// from the 512 KB flash at 0x08000000 (aliased at 0), runs RAM out of the
// 128 KB SRAM at 0x20000000. Only the registers this port touches: RCC + PWR
// + FLASH for the 180 MHz PLL bring-up, USART2 for the ST-LINK VCP console,
// TIM2 as the microsecond timer, GPIOA/GPIOC for the LD2 LED and the user
// button. Values from RM0390; the Nucleo board facts (LD2 = PA5, button =
// PC13, USART2 PA2/PA3 = the VCP, 8 MHz HSE-bypass from the ST-LINK MCO)
// from UM1724.
#pragma once
#include <stdint.h>

#define REG(a) (*(volatile uint32_t *)(uintptr_t)(a))

// --- memory map ----------------------------------------------------------
#define FLASH_BASE 0x08000000u    // 512 KB, aliased at 0x00000000 for boot
#define SRAM_BASE  0x20000000u    // 112 KB SRAM1 + 16 KB SRAM2, contiguous
#define SRAM_SIZE  (128u * 1024u)

// --- RCC: reset and clock control ----------------------------------------
#define RCC_BASE     0x40023800u
#define RCC_CR       (RCC_BASE + 0x00u)
#define RCC_PLLCFGR  (RCC_BASE + 0x04u)
#define RCC_CFGR     (RCC_BASE + 0x08u)
#define RCC_AHB1ENR  (RCC_BASE + 0x30u)
#define RCC_APB1ENR  (RCC_BASE + 0x40u)
#define RCC_APB2ENR  (RCC_BASE + 0x44u)

#define RCC_CR_HSEON   (1u << 16)
#define RCC_CR_HSERDY  (1u << 17)
#define RCC_CR_HSEBYP  (1u << 18)   // the 8 MHz is the ST-LINK MCO, not a crystal
#define RCC_CR_PLLON   (1u << 24)
#define RCC_CR_PLLRDY  (1u << 25)

// PLL: 8 MHz HSE / M(4) * N(180) / P(2) = 180 MHz core; Q(8) idles (no USB).
#define PLLCFGR_180 (4u | (180u << 6) | (0u << 16) | (1u << 22) | (8u << 24))

#define CFGR_SW_MASK  3u
#define CFGR_SW_PLL   2u
#define CFGR_SWS_MASK (3u << 2)
#define CFGR_SWS_PLL  (2u << 2)
#define CFGR_PPRE1_MASK (7u << 10)
#define CFGR_PPRE1_DIV4 (5u << 10)  // APB1 45 MHz (its ceiling)
#define CFGR_PPRE2_MASK (7u << 13)
#define CFGR_PPRE2_DIV2 (4u << 13)  // APB2 90 MHz (its ceiling)

#define AHB1ENR_GPIOA (1u << 0)
#define AHB1ENR_GPIOC (1u << 2)
#define APB1ENR_TIM2  (1u << 0)
#define APB1ENR_USART2 (1u << 17)
#define APB1ENR_PWR   (1u << 28)

// --- PWR: regulator scale + over-drive (both wanted for 180 MHz) ----------
#define PWR_CR  0x40007000u
#define PWR_CSR 0x40007004u
#define PWR_CR_VOS_SCALE1 (3u << 14)
#define PWR_CR_ODEN       (1u << 16)
#define PWR_CR_ODSWEN     (1u << 17)
#define PWR_CSR_ODRDY     (1u << 16)
#define PWR_CSR_ODSWRDY   (1u << 17)

// --- FLASH: wait states for 180 MHz at 3.3 V ------------------------------
#define FLASH_ACR 0x40023C00u
#define FLASH_ACR_180 (5u | (1u << 8) | (1u << 9) | (1u << 10))  // 5 WS + prefetch + I/D cache

// --- GPIO ----------------------------------------------------------------
#define GPIOA_BASE 0x40020000u
#define GPIOC_BASE 0x40020800u
#define GPIO_MODER(b)  ((b) + 0x00u)
#define GPIO_IDR(b)    ((b) + 0x10u)
#define GPIO_BSRR(b)   ((b) + 0x18u)
#define GPIO_AFRL(b)   ((b) + 0x20u)
#define LED_PIN 5u       // LD2 = PA5
#define BTN_PIN 13u      // B1 (blue) = PC13, low when pressed

// --- USART2: the ST-LINK virtual COM port ---------------------------------
#define USART2_SR  0x40004400u
#define USART2_DR  0x40004404u
#define USART2_BRR 0x40004408u
#define USART2_CR1 0x4000440Cu
#define USART_SR_RXNE (1u << 5)
#define USART_SR_TXE  (1u << 7)
#define USART_CR1_RE  (1u << 2)
#define USART_CR1_TE  (1u << 3)
#define USART_CR1_UE  (1u << 13)

// --- TIM2: the 32-bit free-running microsecond counter --------------------
#define TIM2_CR1 0x40000000u
#define TIM2_EGR (0x40000000u + 0x14u)
#define TIM2_CNT (0x40000000u + 0x24u)
#define TIM2_PSC (0x40000000u + 0x28u)
#define TIM2_ARR (0x40000000u + 0x2Cu)
#define TIM_CR1_CEN (1u << 0)
#define TIM_EGR_UG  (1u << 0)

// --- core ----------------------------------------------------------------
#define SCB_VTOR  0xE000ED08u
#define SCB_CPACR 0xE000ED88u

// boot.S
void arm_dsb_isb(void);
void arm_wfi(void);
void sh_exit(uint32_t code);   // qemu semihosting exit (QSMOKE only; bkpt)

// nucleo446.c
void clocks_init(void);
void timer_init(void);
void led_init(void);
void serial_init(void);
void serial_putc(int c);
int  serial_rx_ready(void);
int  serial_getc(void);
uint32_t clock_ms(void);
uint32_t clock_mhz(void);      // self-reported, from the LIVE RCC state
void led_put(int hi);
int  btn_get(void);
