#include "k.h"
#include "log.h"
void k_stop(void) { asm ("hlt"); }
void keyboard_isr_stub(void),
     start_interrupts(void),
     timer_isr(void);

#define INTERRUPT 0x8e
#define TRAP 0x8f
#define FAULT TRAP
#define ABORT TRAP

struct idt_entry {
  uint16_t isr_low,
           kernel_cs;
  uint8_t ist,
          attributes;
  uint16_t isr_mid;
  uint32_t isr_high,
           reserved;
} __attribute__((packed));
_Static_assert(sizeof(struct idt_entry) == 16);

struct limit_base {
  uint16_t limit;
  uint64_t base;
} __attribute__((packed));
_Static_assert(sizeof(struct limit_base) == 10);

extern void ctx_switch(void*);
static struct idt_entry idt[256]; // TODO make this array as small as possible?
static struct {
  void (*isr)(void);
  int type_attr;
} built_in_isrs[48] = {
  // divide by zero
  [0] = {k_reset, FAULT},
  // debug exception
  [1] = {k_reset, TRAP},
  // non maskable interrup
  [2] = {k_reset, INTERRUPT},
  // breakpoint
  [3] = {k_reset, TRAP},
  // overflow
  [4] = {k_reset, TRAP},
  // BOUND range exceeded
  [5] = {k_reset, FAULT},
  // invalid opcode
  [6] = {k_reset, FAULT},
  // coprocessor device not accessible
  [7] = {k_reset, FAULT},
  // double fault
  [8] = {k_reset, ABORT},
  // coprocessor segment overrun
  [9] = {k_reset, FAULT},
  // invalid task switch segment
  [10] = {k_reset, FAULT},
  // segment not present
  [11] = {k_reset, FAULT},
  // stack segment fault
  [12] = {k_reset, FAULT},
  // general protection fault
  [13] = {k_reset, FAULT},
  // page fault
  [14] = {k_reset, FAULT},
  // reserved
  [15] = {k_reset, FAULT},
  // x87 floating point error
  [16] = {k_reset, FAULT},
  // alignment check
  [17] = {k_reset, FAULT},
  // machine check
  [18] = {k_reset, ABORT},
  // simd floating point exception
  [19] = {k_reset, FAULT},
  // virtualization exception
  [20] = {k_reset, FAULT},
  // control protection exception
  [21] = {k_reset, FAULT},

  // 22-31 reserved
  [22] = {k_reset, FAULT},
  [23] = {k_reset, FAULT},
  [24] = {k_reset, FAULT},
  [25] = {k_reset, FAULT},
  [26] = {k_reset, FAULT},
  [27] = {k_reset, FAULT},
  [28] = {k_reset, FAULT},
  [29] = {k_reset, FAULT},
  [30] = {k_reset, FAULT},
  [31] = {k_reset, FAULT},

  // PIC interrupts
  //
  // PIT interrupt
  [32] = {timer_isr, INTERRUPT},
  // keyboard interrupt
  [33] = {keyboard_isr_stub, INTERRUPT},
  [34] = {k_reset, FAULT},
  [35] = {k_reset, FAULT},
  [36] = {k_reset, FAULT},
  [37] = {k_reset, FAULT},
  [38] = {k_reset, FAULT},
  [39] = {k_reset, FAULT},
  [40] = {k_reset, FAULT},
  [41] = {k_reset, FAULT},
  [42] = {k_reset, FAULT},
  [43] = {k_reset, FAULT},
  [44] = {k_reset, FAULT},
  [45] = {k_reset, FAULT},
  [46] = {k_reset, FAULT},
  [47] = {k_reset, FAULT},
};

void resume(g_task *t) {
//  ctx_switch(t->sp);
}

#define LEN(x) (sizeof(x)/sizeof(*x))
void keyboard_interrupt_handler(void) {
  static const char scancode_ascii[128] = {
    0,  27, '1','2','3','4','5','6','7','8','9','0','-','=', '\b',
    '\t','q','w','e','r','t','y','u','i','o','p','[',']','\n', 0,
    'a','s','d','f','g','h','j','k','l',';','\'','`', 0, '\\',
    'z','x','c','v','b','n','m',',','.','/', 0, '*', 0, ' ', };
  uint8_t code;
  asm volatile ("inb $0x60, %0" : "=a"(code));
  // Handle key press / release
  if (code & 0x80) {
    // Key released
  } else if (code < LEN(scancode_ascii)) {
    // Key pressed
    code = scancode_ascii[code];
    k_log_char(code);
  }
  asm volatile (
    "mov $0x20, %al\n"
    "outb %al, $0x20\n"); }

void k_init(void) {
  for (uintptr_t p, i = 0; i < LEN(built_in_isrs); i++)
    p = (uintptr_t)  built_in_isrs[i].isr,
    idt[i].isr_low = p & 0xffff,
    idt[i].kernel_cs = 0x28, // determined by GDT index 5 of 64 bit code segment * 8 bytes per segment descriptor
    idt[i].ist = 0,
    idt[i].attributes = built_in_isrs[i].type_attr,
    idt[i].isr_mid = (p >> 16) & 0xffff,
    idt[i].isr_high = (p >> 32) & 0xffffffff,
    idt[i].reserved = 0;
  struct limit_base idtr = { sizeof(idt) - 1, (uint64_t) &idt };
  asm volatile ("lidt (%0)" :: "r"(&idtr));
  start_interrupts(); }
