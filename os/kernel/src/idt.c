#include "k.h"
#define gdt_code_segment_index 5 // 5 == index of 64 bit code segment in GDT set up by limine
#define interrupt_stack_table_offset 0
#define INT 0xe
#define TRAP 0xf
#define FAULT TRAP
#define ABORT TRAP
#define sixteen_bits ((1<<16)-1)
#define present_bit (1<<7)
#define off1(x) ((uint16_t)((uintptr_t)x & 0xffff))
#define off2(x) ((uint16_t)(((uintptr_t)x >> 16) &0xffff))
#define off3(x) ((uint32_t)((uintptr_t)x >> 32))
#define idesc(gate_type, addr) {\
  off1(addr),\
  gdt_code_segment_index << 3, \
  interrupt_stack_table_offset, \
  present_bit | gate_type, \
  off2(addr), \
  off3(addr), \
  0 }

struct idt_entry idt[256] = {
  [0] = idesc(FAULT, NULL),  // divide error
  [1] = idesc(TRAP, NULL),   // debug exception
  [2] = idesc(INT, NULL),    // NMI
  [3] = idesc(TRAP, NULL),   // breakpoint
  [4] = idesc(TRAP, NULL),   // overflow
  [5] = idesc(FAULT, NULL),  // bound
  [6] = idesc(FAULT, NULL),  // invalid opcode
  [7] = idesc(FAULT, NULL),  // no math coprocessor
  [8] = idesc(ABORT, NULL),  // double fault
  [9] = idesc(FAULT, NULL),  // coprocessor segment overrun
  [10] = idesc(FAULT, NULL), // invalid task state segment
  [11] = idesc(FAULT, NULL), // segment not present
  [12] = idesc(FAULT, NULL), // stack segment fault
  [13] = idesc(FAULT, NULL), // general protection fault
  [14] = idesc(FAULT, NULL), // page fault
  // [15] reserved
  [16] = idesc(FAULT, NULL), // floating point error
  [17] = idesc(FAULT, NULL), // alignment check
  [18] = idesc(ABORT, NULL), // machine check
  [19] = idesc(FAULT, NULL), // SIMD FP exception
  [20] = idesc(FAULT, NULL), // virtualization exception
  [21] = idesc(FAULT, NULL), // control protection exception
  // [22] - [31] reserved
  // external interrupts...
  [32] = {},
};

void isr_0(void) {
  for (;;) asm ("hlt"); }

struct limit_base idtr;
void idt_ini(void) {
  idtr.limit = sizeof(idt) - 1;
  idtr.base = (uint64_t) idt;
  for (int i = 0; i < 256; i++)
    idt[i].offset_1 = off1(isr_0),
    idt[i].offset_2 = off2(isr_0),
    idt[i].offset_3 = off3(isr_0);
  asm ("lidt %0" : :"m"(idtr)); }

