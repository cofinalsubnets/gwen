#include "k.h"
void k_stop(void) { asm ("hlt"); }
void keyboard_isr_stub(void),
     timer_isr_stub(void);

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

void k_reset(void) {
  struct limit_base idtr0 = { 0, 0};
  asm volatile ("lidt (%0)\n"
                "int $0" :: "r"(&idtr0)); }

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
  [32] = {timer_isr_stub, INTERRUPT},
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



// CHATGPT CODE
static Inline void outb(uint16_t port, uint8_t val) {
  asm volatile ("outb %0, %1" : : "a"(val), "Nd"(port)); }

// Read a byte from an I/O port
static Inline uint8_t inb(uint16_t port) {
  uint8_t ret;
  asm volatile ("inb %1, %0" : "=a"(ret) : "Nd"(port));
  return ret; }

// Short I/O delay (optional)
static inline void io_wait(void) {
  asm volatile ("outb %%al, $0x80" : : "a"(0)); }

void pic_remap(void) {
  unsigned char // Save masks
    a1 = inb(0x21),
    a2 = inb(0xA1);

  // Start initialization (ICW1)
  outb(0x20, 0x11); // Start init for master
  outb(0xA0, 0x11); // Start init for slave

  // Set vector offsets (ICW2)
  outb(0x21, 0x20); // Master PIC vectors start at 0x20
  outb(0xA1, 0x28); // Slave PIC vectors start at 0x28

  outb(0x21, 4); // Tell Master that there is a Slave PIC at IRQ2 (ICW3)
  outb(0xA1, 2); // Tell Slave its cascade identity (ICW3)

  // Set mode (ICW4)
  outb(0x21, 0x01);
  outb(0xA1, 0x01);

  // Restore saved masks
  outb(0x21, a1);
  outb(0xA1, a2); }

void pic_unmask(uint8_t irq) {
  uint16_t port;

  if (irq < 8) port = 0x21;
  else port = 0xA1, irq -= 8;

  outb(port, inb(port) & ~(1 << irq)); }

void pic_mask(uint8_t irq) {
  uint16_t port;

  if (irq < 8) port = 0x21;
  else port = 0xA1, irq -= 8;

  outb(port, inb(port) | (1 << irq)); }

#define PIC1_COMMAND 0x20
#define PIC2_COMMAND 0xA0
#define PIC_EOI 0x20

void pic_send_eoi(uint8_t irq) {
  if (irq >= 8) outb(PIC2_COMMAND, PIC_EOI);
  outb(PIC1_COMMAND, PIC_EOI); }
static const char scancode_ascii[128] = {
  0,  27, '1','2','3','4','5','6','7','8','9','0','-','=', '\b',
  '\t','q','w','e','r','t','y','u','i','o','p','[',']','\n', 0,
  'a','s','d','f','g','h','j','k','l',';','\'','`', 0, '\\',
  'z','x','c','v','b','n','m',',','.','/', 0, '*', 0, ' ',
};

char scancode_to_ascii(uint8_t scancode) {
      if (scancode > 127) return 0;
          return scancode_ascii[scancode];
}
#define PIT_CHANNEL0 0x40
#define PIT_COMMAND  0x43
#define PIT_FREQUENCY 1193182

void pit_set_freq(uint32_t hz) {
  uint32_t divisor = (uint32_t)(PIT_FREQUENCY / hz);

  outb(PIT_COMMAND, 0x36); // Channel 0, LSB/MSB, mode 3, binary
  outb(PIT_CHANNEL0, 0xff); //(uint8_t)(divisor & 0xFF));       // Low byte
  outb(PIT_CHANNEL0, 0xff); //(uint8_t)((divisor >> 8) & 0xFF)); // High byte
}
// END CHATGPT
//

#define KEYBOARD_DATA_PORT 0x60
#define PIC1_COMMAND 0x20
#define PIC_EOI 0x20

void keyboard_interrupt_handler(void) {
  uint8_t scancode = inb(KEYBOARD_DATA_PORT);
  // Handle key press / release
  if (scancode & 0x80) {
  // Key released
  } else {
    // Key pressed
    char c = scancode_to_ascii(scancode), s[2] = {c, 0};
    k_log(s);
  }

  // Send EOI to PIC
  outb(PIC1_COMMAND, PIC_EOI); }

void timer_interrupt_handler(void) {
//  k_log("\ngot timer interrupt");
  outb(0x20, 0x20);
}

#define LEN(x) (sizeof(x)/sizeof(*x))
void k_init(void) {
  for (unsigned long i = 0; i < LEN(built_in_isrs); i++) {
    uintptr_t p = (uintptr_t)  built_in_isrs[i].isr;
    idt[i].isr_low = p & 0xffff;
    idt[i].kernel_cs = 0x28; // determined by GDT index 5 of 64 bit code segment * 8 bytes per segment descriptor
    idt[i].ist = 0;
    idt[i].attributes = built_in_isrs[i].type_attr;
    idt[i].isr_mid = (p >> 16) & 0xffff;
    idt[i].isr_high = (p >> 32) & 0xffffffff;
    idt[i].reserved = 0; }

  struct limit_base idtr = {
    .limit = sizeof(idt) - 1, 
    .base = (uint64_t) &idt, };
  asm volatile ("lidt (%0)" :: "r"(&idtr));
  pit_set_freq(1);
  pic_remap();   // move IRQs to 0x20–0x2F
  pic_unmask(1); // enable keyboard IRQ
  pic_unmask(0); // enable timer IRQ
  asm volatile("sti"); }
