#include "k.h"
void k_fin(void) { for (;;) asm ("hlt"); }
void k_reset(void) {
  uint64_t idtr0 = 0;
  asm volatile ("lidt (%0)\n"
                "int $0" :: "r"(&idtr0)); }
void
     default_isr_null(void);
void keyboard_isr_stub(void),
     default_isr(int),
     default_isr_0(void),
     default_isr_1(void),
     timer_isr_stub(void)
  ;

#define gdt_code_segment_index 5 // 5 == index of 64 bit code segment in GDT set up by limine
#define INTERRUPT 0xe
#define TRAP 0xf
#define FAULT TRAP
#define ABORT TRAP
#define present_bit (1<<7)

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

static struct idt_entry idt[256];

void default_isr_null(void) {
  k_log("\nunhandled interrupt");
  for (int i = INT16_MAX; i; i--);
  k_reset(); }

static struct {
  void (*isr)(void);
  int gate_type;
} built_in_isrs[48] = {
  // divide by zero
  [0] = {default_isr_0, FAULT},
  // debug exception
  [1] = {default_isr_1, TRAP},
  // non maskable interrup
  [2] = {default_isr_null, INTERRUPT},
  // breakpoint
  [3] = {default_isr_null, TRAP},
  // overflow
  [4] = {default_isr_null, TRAP},
  // BOUND range exceeded
  [5] = {default_isr_null, FAULT},
  // invalid opcode
  [6] = {default_isr_null, FAULT},
  // coprocessor device not accessible
  [7] = {default_isr_null, FAULT},
  // double fault
  [8] = {default_isr_null, ABORT},
  // coprocessor segment overrun
  [9] = {default_isr_null, FAULT},
  // invalid task switch segment
  [10] = {default_isr_null, FAULT},
  // segment not present
  [11] = {default_isr_null, FAULT},
  // stack segment fault
  [12] = {default_isr_null, FAULT},
  // general protection fault
  [13] = {default_isr_null, FAULT},
  // page fault
  [14] = {default_isr_null, FAULT},
  // reserved
  [15] = {default_isr_null, FAULT},
  // x87 floating point error
  [16] = {default_isr_null, FAULT},
  // alignment check
  [17] = {default_isr_null, FAULT},
  // machine check
  [18] = {default_isr_null, ABORT},
  // simd floating point exception
  [19] = {default_isr_null, FAULT},
  // virtualization exception
  [20] = {default_isr_null, FAULT},
  // control protection exception
  [21] = {default_isr_null, FAULT},

  // 22-31 reserved
  [22] = {default_isr_null, FAULT},
  [23] = {default_isr_null, FAULT},
  [24] = {default_isr_null, FAULT},
  [25] = {default_isr_null, FAULT},
  [26] = {default_isr_null, FAULT},
  [27] = {default_isr_null, FAULT},
  [28] = {default_isr_null, FAULT},
  [29] = {default_isr_null, FAULT},
  [30] = {default_isr_null, FAULT},
  [31] = {default_isr_null, FAULT},

  // PIC interrupts
  //
  // PIT interrupt
  [32] = {timer_isr_stub, INTERRUPT},
  // keyboard interrupt
  [33] = {keyboard_isr_stub, INTERRUPT},
  [33] = {default_isr_null, FAULT},
  [34] = {default_isr_null, FAULT},
  [35] = {default_isr_null, FAULT},
  [36] = {default_isr_null, FAULT},
  [37] = {default_isr_null, FAULT},
  [38] = {default_isr_null, FAULT},
  [39] = {default_isr_null, FAULT},
  [40] = {default_isr_null, FAULT},
  [41] = {default_isr_null, FAULT},
  [42] = {default_isr_null, FAULT},
  [43] = {default_isr_null, FAULT},
  [44] = {default_isr_null, FAULT},
  [45] = {default_isr_null, FAULT},
  [46] = {default_isr_null, FAULT},
  [47] = {default_isr_null, FAULT},
};

#define LEN(x) (sizeof(x)/sizeof(*x))
static void idt_init(void) {
  for (unsigned long i = 0; i < LEN(built_in_isrs); i++) {
    struct idt_entry *e = idt + i;
    uintptr_t p = (uintptr_t)  built_in_isrs[i].isr;
    e->isr_low = p & 0xffff;
    e->kernel_cs = gdt_code_segment_index << 3;
    e->ist = 0;
    e->attributes = present_bit | built_in_isrs[i].gate_type;
    e->isr_mid = (p >> 16) & 0xffff;
    e->isr_high = (p >> 32) & 0xffffffff;
    e->reserved = 0; }
  struct limit_base idtr = { sizeof(idt) - 1, (uint64_t) &idt[0], };
  asm volatile ("lidt (%0)" :: "r"(&idtr)); }


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
  unsigned char a1, a2;

  // Save masks
  a1 = inb(0x21);
  a2 = inb(0xA1);

  // Start initialization (ICW1)
  outb(0x20, 0x11); // Start init for master
  outb(0xA0, 0x11); // Start init for slave

  // Set vector offsets (ICW2)
  outb(0x21, 0x20); // Master PIC vectors start at 0x20
  outb(0xA1, 0x28); // Slave PIC vectors start at 0x28

  // Tell Master that there is a Slave PIC at IRQ2 (ICW3)
  outb(0x21, 4);
  // Tell Slave its cascade identity (ICW3)
  outb(0xA1, 2);

  // Set mode (ICW4)
  outb(0x21, 0x01);
  outb(0xA1, 0x01);

  // Restore saved masks
  outb(0x21, a1);
  outb(0xA1, a2); }

void pic_unmask(uint8_t irq) {
  uint16_t port;
  uint8_t value;

  if (irq < 8) {
    port = 0x21;
  } else {
    port = 0xA1;
    irq -= 8; }

  value = inb(port) & ~(1 << irq);
  outb(port, value); }

void pic_mask(uint8_t irq) {
  uint16_t port;
  uint8_t value;

  if (irq < 8) {
    port = 0x21;
  } else {
    port = 0xA1;
    irq -= 8; }

  value = inb(port) | (1 << irq);
  outb(port, value); }

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
                      /* rest unused */
};

char scancode_to_ascii(uint8_t scancode) {
      if (scancode > 127) return 0;
          return scancode_ascii[scancode];
}
#define PIT_CHANNEL0 0x40
#define PIT_COMMAND  0x43
#define PIT_FREQUENCY 1193182

void pit_set_freq(uint32_t hz) {
  uint16_t divisor = (uint16_t)(PIT_FREQUENCY / hz);

  outb(PIT_COMMAND, 0x36); // Channel 0, LSB/MSB, mode 3, binary
  outb(PIT_CHANNEL0, (uint8_t)(divisor & 0xFF));       // Low byte
  outb(PIT_CHANNEL0, (uint8_t)((divisor >> 8) & 0xFF)); // High byte
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

void default_isr(int i) {
  k_log("\ngot unhandled interrupt: 0x"), k_log_n(i, 16);
  k_reset(); }
void timer_interrupt_handler(void) {
  k_log("\ngot timer interrupt");
//  outb(0x20, 0x20);
}

void k_init(void) {
  idt_init();
  pit_set_freq(1);
  pic_remap();         // move IRQs to 0x20–0x2F
  pic_unmask(1);       // enable keyboard IRQ
//  pic_unmask(0); // timer interrupts?
  asm volatile("sti"); }
