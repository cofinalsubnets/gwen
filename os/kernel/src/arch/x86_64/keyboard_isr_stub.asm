; keyboard_isr.asm
global keyboard_isr_stub
extern keyboard_interrupt_handler  ; defined in C
global timer_isr_stub
extern timer_interrupt_handler  ; defined in C

%macro isr_stub 1
  push rbp
  push rax
  push rbx
  push rcx
  push rdx
  push rsi
  push rdi
  push r8
  push r9
  push r10
  push r11
  push r12
  push r13
  push r14
  push r15
  call %1
  pop r15
  pop r14
  pop r13
  pop r12
  pop r11
  pop r10
  pop r9
  pop r8
  pop rdi
  pop rsi
  pop rdx
  pop rcx
  pop rbx
  pop rax
  pop rbp
  iretq
%endmacro

section .text

align 8
keyboard_isr_stub:
  isr_stub keyboard_interrupt_handler

align 8
timer_isr_stub:
  isr_stub timer_interrupt_handler

align 8
ctx_switch:
  mov [rdi], rsp
  mov rsp, [rsi]
  ret
