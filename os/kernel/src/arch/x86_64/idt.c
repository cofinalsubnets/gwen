#include "k.h"
#define gdt_code_segment_index 5 // 5 == index of 64 bit code segment in GDT set up by limine
#define interrupt_stack_table_offset 0
#define INT 0xe
#define TRAP 0xf
#define FAULT TRAP
#define ABORT TRAP
#define sixteen_bits ((1<<16)-1)
#define present_bit (1<<7)

__attribute__((packed))
struct idt_entry {
  uint16_t isr_low,
           kernel_cs;
  uint8_t ist,
          attributes;
  uint16_t isr_mid;
  uint32_t isr_high,
           reserved; };
_Static_assert(sizeof(struct idt_entry) == 16);

static struct idt_entry idt[256];

static void set_idt_desc(struct idt_entry *e, unsigned int gate_type, uint64_t p) {
  e->isr_low = p & 0xffff;
  e->kernel_cs = gdt_code_segment_index << 3;
  e->ist = interrupt_stack_table_offset;
  e->attributes = present_bit | gate_type;
  e->isr_mid = (p >> 16) & 0xffff;
  e->isr_high = (p >> 32) & 0xffffffff;
  e->reserved = 0; }

void isr_0(void) {
  for (;;) asm ("hlt"); }

__attribute__((packed))
struct limit_base {
  uint16_t limit;
  uint64_t base; };
static struct limit_base idtr;
void idt_ini(void) {
  idtr.limit = sizeof(idt) - 1;
  idtr.base = (uint64_t) idt;
  for (int i = 0; i < 256; i++)
    set_idt_desc(idt + i, FAULT, (uintptr_t) isr_0);
  asm ("lidt %0" : :"m"(idtr)); }
