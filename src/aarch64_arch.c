// aarch64 architecture-specific C: the PL011 serial console, the GICv2
// interrupt controller, the ARM generic timer, and CPU-exception
// reporting. the exception vector table itself is laid by src/mkvec.l;
// archinit points VBAR_EL1 at it. this is the aarch64 counterpart of
// x86_64/arch.c -- same contract (archinit, serial_init, serial_putc,
// k_reset), different hardware.
#include <stdint.h>
#include "asmops.h"                    // the privileged instructions, both spellings
#include "k.h"                       // kboot, and kputc/kputs/kputn (src/kmain.c)

// khhdm is the higher-half direct map offset the door left; kmain sets it
// before archinit runs, so physical address P is reachable at khhdm+P.
// all device MMIO below goes through it.
extern uintptr_t khhdm;
extern uint64_t kticks;

// kq (k/main.c) enqueues one input byte; cb_putc / fbdraw render the
// console. fault reporting writes the framebuffer ring buffer (kcb) and
// mirrors to serial, exactly as x86_64 does.
void kq(uint8_t);
struct cb;
extern void cb_putc(struct cb*, char);
extern struct cb *kcb;
extern void fbdraw(void);

// the exception vector table (mkvec.l).
extern uint8_t vectors[];

// --- QEMU 'virt' machine fixed MMIO layout ---------------------------
// the virt machine's device addresses are stable across QEMU versions;
// a port to other boards would read these from the device tree the door
// hands us instead of hardcoding them.
#define UART_PHYS   0x09000000         // PL011 UART
#define RTC_PHYS    0x09010000         // PL031 RTC -- inside the UART's 2MiB block
#define GICD_PHYS   0x08000000         // GICv2 distributor
#define GICC_PHYS   0x08010000         // GICv2 CPU interface
#define VIRTIO_PHYS 0x0a000000         // the 32 virtio-mmio slots (blk.c scans them)
#define UART_INTID  33                 // PL011 -> SPI 1 -> INTID 32+1
#define TIMER_INTID 30                 // EL1 physical timer -> PPI INTID 30

static inline uint32_t mmio_rd(uintptr_t phys, uintptr_t off) {
  return *(volatile uint32_t*) (khhdm + phys + off); }
static inline void mmio_wr(uintptr_t phys, uintptr_t off, uint32_t v) {
  *(volatile uint32_t*) (khhdm + phys + off) = v; }
static inline void mmio_wr8(uintptr_t phys, uintptr_t off, uint8_t v) {
  *(volatile uint8_t*) (khhdm + phys + off) = v; }

// --- device MMIO mapping ---------------------------------------------
// the HHDM covers RAM but not device MMIO, so the GIC and UART
// pages are unmapped at boot. mmio_map adds them: it walks the live
// TTBR1_EL1 tables (reachable through the HHDM, since page tables live
// in RAM) and installs 2MiB block descriptors at the HHDM image of
// each device. both devices sit in the first 1GiB of physical space,
// so they share one L2 table; when the door left that slot empty we
// supply our own static l2_table. the attribute index is whatever
// Device (or, failing that, Normal) slot MAIR_EL1 already holds --
// MAIR is global, shared with the mappings already standing, so we must
// not edit it.
#define PA_MASK  0x0000fffffffff000ULL  // descriptor output addr, bits 47:12
#define BLK_MASK 0x0000ffffffe00000ULL  // 2MiB block output addr, bits 47:21

static uint64_t l2_table[512] __attribute__((aligned(4096)));

// translate a (mapped) virtual address to physical via the MMU.
static uintptr_t va2pa(void *va) {
  uint64_t par = k_at_s1e1w_par(va);
  return (par & PA_MASK) | ((uintptr_t) va & 0xfff); }

// the MAIR_EL1 slot to use for device memory: prefer a Device
// attribute, then Normal non-cacheable, then Normal write-back (still
// correct under QEMU). MAIR is the door's, and left as-is.
static uint32_t mmio_attr_index(void) {
  uint64_t mair = k_rd_mair_el1();
  static uint8_t const prefer[] = { 0x00, 0x04, 0x08, 0x0c, 0x44, 0xff };
  for (uint32_t p = 0; p < sizeof prefer; p++)
    for (uint32_t i = 0; i < 8; i++)
      if (((mair >> (i*8)) & 0xff) == prefer[p]) return i;
  return 0; }

static void mmio_map(void) {
  uint32_t attr = mmio_attr_index();
  uint64_t ttbr1 = k_rd_ttbr1_el1();
  uint64_t *l0 = (uint64_t*) (khhdm + (ttbr1 & PA_MASK));

  // the GIC and UART share L0/L1 indices (both in the first 1GiB).
  uintptr_t va = khhdm + GICD_PHYS;
  uint64_t l0e = l0[(va >> 39) & 0x1ff];
  uint64_t *l1 = (uint64_t*) (khhdm + (l0e & PA_MASK));
  uint64_t l1e = l1[(va >> 30) & 0x1ff];
  uint64_t *l2;
  if (l1e & 1) l2 = (uint64_t*) (khhdm + (l1e & PA_MASK));
  else {                                       // empty slot: supply our own L2
    l2 = l2_table;
    l1[(va >> 30) & 0x1ff] = (va2pa(l2_table) & PA_MASK) | 3; }

  uintptr_t const dev[] = { GICD_PHYS, UART_PHYS, VIRTIO_PHYS };
  for (uint32_t i = 0; i < 3; i++)
    l2[((khhdm + dev[i]) >> 21) & 0x1ff] =
        (dev[i] & BLK_MASK)
      | (1ULL << 54) | (1ULL << 53)            // UXN | PXN: never execute
      | (1ULL << 10)                           // AF: access flag
      | (3ULL << 8)                            // inner shareable
      | ((uint64_t) attr << 2)                 // MAIR attribute index
      | 1ULL;                                  // valid; bit 1 clear => block

  k_tlbi_all(); }

// --- PL011 serial console --------------------------------------------
// the aarch64 analogue of x86_64's COM1: a second console alongside the
// framebuffer, and the only console when no framebuffer is present.
// output is also the panic channel. input is interrupt-driven via the
// receive FIFO: a burst raises the receive interrupt at the FIFO
// threshold, and a lone keystroke raises the receive-timeout interrupt
// once the line goes idle. either way the GIC delivers UART_INTID,
// k_irq routes it to k_uart, and k_uart drains the whole FIFO into the
// same input queue the rest of the kernel reads. bytes pass through
// verbatim, exactly as on x86_64.
#define UARTDR    0x000                // data register
#define UARTFR    0x018                // flag register
#define UARTLCR_H 0x02c                // line control
#define UARTCR    0x030                // control register
#define UARTIMSC  0x038                // interrupt mask set/clear
#define UARTICR   0x044                // interrupt clear
#define FR_RXFE   (1u << 4)            // receive holding register empty
#define FR_TXFF   (1u << 5)            // transmit holding register full

// --- the wall clock: the PL031 RTC ------------------------------------
// one register, already UNIX SECONDS, and already mapped: RTC_PHYS shares the 2MiB
// block mmio_map lays for the UART. the aarch64 counterpart of x86_64's CMOS walk,
// and a tenth its size -- ⚠ so it must be called AFTER archinit, like everything
// else that touches device memory here.
uint64_t k_rtc(void) { return mmio_rd(RTC_PHYS, 0); }

// called once from kmain, just after archinit (so the GIC is up).
void serial_init(void) {
  mmio_wr(UART_PHYS, UARTCR, 0);                  // disable while configuring
  mmio_wr(UART_PHYS, UARTICR, 0x7ff);             // clear any pending interrupts
  mmio_wr(UART_PHYS, UARTLCR_H, (3u<<5)|(1u<<4)); // 8 bits, no parity, FIFO on
  mmio_wr(UART_PHYS, UARTIMSC, (1u<<4)|(1u<<6));  // receive + receive-timeout
  mmio_wr(UART_PHYS, UARTCR, (1u<<0)|(1u<<8)|(1u<<9)); }  // UARTEN|TXE|RXE

void serial_putc(int c) {
  if (c == '\n') serial_putc('\r');
  // bounded spin on "transmit holding register full" so an absent or
  // wedged port cannot hang output.
  for (int i = 0; i < 100000 && (mmio_rd(UART_PHYS, UARTFR) & FR_TXFF); i++) {}
  mmio_wr(UART_PHYS, UARTDR, (uint8_t) c); }

// reached from k_irq. drain every received byte, then clear the UART's
// receive interrupt (data + receive-timeout).
static void k_uart(void) {
  while (!(mmio_rd(UART_PHYS, UARTFR) & FR_RXFE))
    kq(mmio_rd(UART_PHYS, UARTDR) & 0xff);
  mmio_wr(UART_PHYS, UARTICR, (1u<<4) | (1u<<6)); }

// --- GICv2 interrupt controller --------------------------------------
#define GICD_CTLR       0x000          // distributor control
#define GICD_ISENABLER  0x100          // set-enable, +4*(intid/32)
#define GICD_IPRIORITYR 0x400          // priority, byte at +intid
#define GICD_ITARGETSR  0x800          // CPU target, byte at +intid
#define GICC_CTLR       0x000          // CPU interface control
#define GICC_PMR        0x004          // priority mask
#define GICC_IAR        0x00c          // interrupt acknowledge
#define GICC_EOIR       0x010          // end of interrupt

static void gic_enable(uint32_t intid, int is_spi) {
  mmio_wr8(GICD_PHYS, GICD_IPRIORITYR + intid, 0);     // highest priority
  if (is_spi)                                          // SPIs need CPU routing;
    mmio_wr8(GICD_PHYS, GICD_ITARGETSR + intid, 1);    // PPIs are banked per-CPU
  mmio_wr(GICD_PHYS, GICD_ISENABLER + 4*(intid/32), 1u << (intid%32)); }

static void gic_init(void) {
  mmio_wr(GICC_PHYS, GICC_CTLR, 0);          // CPU interface off while configuring
  mmio_wr(GICD_PHYS, GICD_CTLR, 0);          // distributor off
  gic_enable(TIMER_INTID, 0);
  gic_enable(UART_INTID, 1);
  mmio_wr(GICD_PHYS, GICD_CTLR, 1);          // enable distributor
  mmio_wr(GICC_PHYS, GICC_PMR, 0xf0);        // unmask all interrupt priorities
  mmio_wr(GICC_PHYS, GICC_CTLR, 1); }        // enable CPU interface

// --- ARM generic timer (EL1 physical timer) --------------------------
// fires TIMER_INTID at ~100 Hz, matching the x86_64 PIT rate; the
// handler reloads the countdown, which also deasserts the interrupt.
static uint64_t timer_interval;        // CNTFRQ_EL0 / 100

static inline void rearm_timer(void) { k_wr_cntp_tval_el0(timer_interval); }

static void timer_init(void) {
  timer_interval = k_rd_cntfrq_el0() / 100;
  rearm_timer();
  k_wr_cntp_ctl_el0(1); }               // enable

// --- interrupt dispatch ----------------------------------------------
// reached from the IRQ vector (mkvec.l). claim the interrupt, handle
// it, then signal completion. an INTID of 1020+ is the GIC's spurious
// marker and must not be acknowledged.
void k_irq(void) {
  uint32_t iar = mmio_rd(GICC_PHYS, GICC_IAR), intid = iar & 0x3ff;
  if (intid >= 1020) return;
  if (intid == TIMER_INTID) kticks++, rearm_timer();
  else if (intid == UART_INTID) k_uart();
  mmio_wr(GICC_PHYS, GICC_EOIR, iar); }

// --- CPU exception reporting -----------------------------------------
static char const *fault_kind(uint64_t esr) {
  switch (esr >> 26) {                 // ESR_EL1.EC -- exception class
    case 0x15: return "SVC";
    case 0x20: case 0x21: return "instruction abort";
    case 0x22: return "PC alignment";
    case 0x24: case 0x25: return "data abort";
    case 0x26: return "SP alignment";
    case 0x2c: return "FP";
    default:   return "?"; } }

// panic-path console output. kputc stamps a char into the framebuffer
// ring buffer (kcb, when present) and mirrors it to serial; it takes no
// l state, so it runs from a fault handler with no live `struct g`.

// reached from the sync/FIQ/SError vectors. report and halt -- faults
// are not resumed (returning would just re-fault). kput* reach the
// serial console even when no framebuffer is up.
void k_fault(uint64_t esr, uint64_t elr, uint64_t far) {
  k_daif_mask_all();                   // all interrupts off while reporting
  static int nested;
  if (nested) for (;;) k_wait();
  nested = 1;
  kputs("\n*** CPU exception ("), kputs(fault_kind(esr));
  kputs(") esr="), kputn(esr, 16);
  kputs(" elr="),  kputn(elr, 16);
  kputs(" far="),  kputn(far, 16);
  kputc('\n');
  fbdraw();
  for (;;) k_wait(); }

// --- bring-up and reset ----------------------------------------------
// archinit runs into a civilised environment -- EL1, MMU on, a stack and
// the HHDM in place, laid by a64boot's cold path or by the firmware on an
// MMU-on door -- so it only has to install our own vector table, interrupt
// controller and timer, then unmask IRQs.
void archinit(void) {
  // a bootloader may enter at EL1t (SP_EL0 selected, SP_EL1 unset) with
  // FP/SIMD trapping -- both must be settled before anything can fault or
  // touch a double. asmops.h carries why each one is written the way it is.
  k_sp_to_el1h();
  k_fpen_enable();
  k_wr_vbar_el1((uintptr_t) vectors);
  // The HHDM covers RAM only, so mmio_map() walks TTBR1 to add 2 MiB
  // block descriptors at HHDM+phys for the GIC and UART pages (skipped
  // when the HHDM response is absent and khhdm == 0).
  if (khhdm) mmio_map();
  gic_init();
  timer_init();
  k_daif_unmask_irq();                 // unmask IRQ (DAIF.I = 0)
}

// (fault n) backend: deliberately raise a CPU exception. n indexes the
// x86 vector numbers the builtin shares across arches; on aarch64 we
// map 3 -> breakpoint, 13/14 -> data abort, anything else -> undefined
// instruction. does not return -- k_fault reports and halts.
void k_fault_trigger(intptr_t n) {
  switch (n) {
    case 3:            // breakpoint
      k_brk0();
      break;
    case 13: case 14:  // data abort: write to an unmapped address
      *(volatile int*) 0x600000000000ULL = 0;
      break;
    default:           // undefined instruction
      k_udf0();
      break; } }

// PSCI SYSTEM_RESET. QEMU's 'virt' machine exposes PSCI over HVC.
void k_reset(void) {
  k_psci_system_reset();
  for (;;) k_wait(); }
