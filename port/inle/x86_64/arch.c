// x86_64 architecture-specific C: CPU-exception handling and the COM1
// serial console. the stubs in x86_64.asm (exc_stub_0 .. exc_stub_31,
// funnelling through exc_common) build the frame below and call
// k_exception; uart_isr funnels IRQ4 into k_uart (see the bottom).
#include <stdint.h>
#include "asmops.h"                    // the privileged instructions, both spellings
void k_halt(void);

// the frame exc_common hands us, lowest address (rsp) first:
//   the push15 saved registers, then the stub's (vector, error code),
//   then the iret frame the CPU pushed on exception entry.
struct k_frame {
  uint64_t r15, r14, r13, r12, r11, r10, r9, r8,
           rdi, rsi, rdx, rcx, rbx, rax, rbp,
           vector, error, rip, cs, rflags, rsp, ss; };

// panic-path console output. cb_putc stamps a char into the framebuffer
// ring buffer (kcb); serial_putc mirrors it to COM1; fbdraw renders kcb
// to the framebuffer. kputc/kputs/kputn take no l state, so they run
// from a fault handler with no live `struct g` -- the old gput* path
// dereferenced its (null here) f argument before reaching the console.
struct cb;
extern void cb_putc(struct cb*, char);
extern void serial_putc(int);
extern void fbdraw(void);
extern struct cb *kcb;

static void kputc(int c) { if (kcb) cb_putc(kcb, (char) c); serial_putc(c); }
static void kputs(char const *s) { while (*s) kputc(*s++); }
static void kputn(uintptr_t n, int base) {
  static char const d[] = "0123456789abcdef";
  char buf[24]; int i = 0;
  do buf[i++] = d[n % base], n /= base; while (n);
  while (i) kputc(buf[--i]); }

static char const *const exc_name[32] = {
  [0]  = "#DE", [1]  = "#DB", [2]  = "NMI", [3]  = "#BP", [4]  = "#OF",
  [5]  = "#BR", [6]  = "#UD", [7]  = "#NM", [8]  = "#DF", [10] = "#TS",
  [11] = "#NP", [12] = "#SS", [13] = "#GP", [14] = "#PF", [16] = "#MF",
  [17] = "#AC", [18] = "#MC", [19] = "#XM", [20] = "#VE", [21] = "#CP", };


// every CPU exception (vectors 0..31) arrives here via exc_common.
// fr->rip is the faulting instruction: a __builtin_trap() faults with
// fr->vector == 6 and fr->rip at its ud2, so it maps straight back to
// whichever guard fired. faults are not resumed -- returning from
// #UD/#GP/#PF would just re-execute the instruction and fault again.
void k_exception(struct k_frame *fr) {
  k_cli();                             // no interrupts while reporting
  static int nested;
  if (nested) k_halt();                // faulted while reporting -- stop
  nested = 1;

  char const *name = fr->vector < 32 ? exc_name[fr->vector] : 0;
  kputs("\n*** CPU exception ");
  kputn(fr->vector, 10);
  kputs(" ("), kputs(name ? name : "?"), kputs(") rip=");
  kputn(fr->rip, 16);
  kputs(" err=");
  kputn(fr->error, 16);
  if (fr->vector == 14) {              // #PF: also the faulting address
    kputs(" cr2="), kputn(k_rd_cr2(), 16); }
  kputc('\n');
  fbdraw();

  k_halt();
}

// --- COM1 serial console ---------------------------------------------
// a 16550 UART at the legacy COM1 I/O ports, used as a second console
// alongside the framebuffer -- and the only console when no framebuffer
// is present. output (serial_putc) is also the reliable panic channel:
// it touches nothing but I/O ports, so it still works when the heap,
// console buffer, or framebuffer are unusable. input is interrupt-
// driven: serial_init enables the UART receive interrupt (IRQ4),
// uart_isr (x86_64.asm) funnels it here, and k_uart drains every ready
// byte into the same input queue kb_int feeds. bytes pass through
// verbatim -- a serial terminal already sends CR for Enter, DEL for
// Backspace, and ESC-prefixed arrow sequences, all of which the l
// line editor decodes directly.
#define COM1 0x3f8

// called once from kmain, just after archinit (so the IDT is live).
void serial_init(void) {
  k_outb(COM1 + 1, 0x00);    // interrupts off while configuring
  k_outb(COM1 + 3, 0x80);    // DLAB: address the divisor latch
  k_outb(COM1 + 0, 0x01);    // divisor low  = 1  -> 115200 baud
  k_outb(COM1 + 1, 0x00);    // divisor high = 0
  k_outb(COM1 + 3, 0x03);    // 8 bits, no parity, 1 stop; DLAB off
  k_outb(COM1 + 2, 0xc7);    // FIFO: enable, clear, 14-byte threshold
  k_outb(COM1 + 4, 0x0b);    // DTR, RTS, OUT2 (OUT2 gates the IRQ line)
  k_outb(COM1 + 1, 0x01); }  // IER: interrupt when receive data arrives

#ifdef K_TEST
// Test build (make test_kernel): the corpus is baked into the kernel and run at
// boot; (exit code) calls this to quit qemu via the isa-debug-exit device.
void k_qemu_exit(int code) { k_outl(0xf4, (uint32_t) code); }
#endif

void serial_putc(int c) {
  if (c == '\n') serial_putc('\r');
  // bounded spin on "transmit holding register empty" so an absent or
  // wedged port cannot hang output.
  for (int i = 0; i < 100000 && !(k_inb(COM1 + 5) & 0x20); i++) {}
  k_outb(COM1, (uint8_t) c); }

// IRQ4 handler body, reached from uart_isr. one interrupt can cover
// several received bytes, so drain the FIFO completely. kq lives in
// k/main.c -- the same input queue the PS/2 keyboard path enqueues to.
void kq(uint8_t);
void k_uart(void) {
  while (k_inb(COM1 + 5) & 0x01)        // LSR bit 0: receive data ready
    kq(k_inb(COM1)); }

// (fault n) backend: deliberately raise a CPU exception, indexed by
// the x86 vector numbers that name it. does not return -- k_exception
// reports and halts.
void k_fault_trigger(intptr_t n) {
  switch (n) {
    case 0:   // #DE: integer divide by zero
      k_divzero();
      break;
    case 3:   // #BP: breakpoint
      k_int3();
      break;
    case 13:  // #GP: write through a non-canonical address
      *(volatile int*) 0xdeadbeefdeadbeefULL = 0;
      break;
    case 14:  // #PF: write to a canonical but unmapped address
      *(volatile int*) 0x600000000000ULL = 0;
      break;
    default:  // #UD: invalid opcode
      k_ud2();
      break; } }
