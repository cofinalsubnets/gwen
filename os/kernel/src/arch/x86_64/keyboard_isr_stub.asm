; keyboard_isr.asm
global keyboard_isr_stub
extern keyboard_interrupt_handler  ; defined in C

global timer_isr_stub
extern timer_interrupt_handler  ; defined in C

global default_isr_stub
extern default_isr

global default_isr_1
global default_isr_0

global default_isr_fail
extern default_isr_null  ; defined in C

%macro pre_isr 0
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
%endmacro

%macro post_isr 0
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
default_isr_0:
  pre_isr
  mov rdi, 0
  call default_isr
  post_isr
align 8
default_isr_1:
  pre_isr
  mov rdi, 1
  call default_isr
  post_isr
align 8
keyboard_isr_stub:
iretq
  pre_isr
  call keyboard_interrupt_handler
  post_isr
align 8
timer_isr_stub:
  pre_isr
  call timer_interrupt_handler
_isr_ret:
  post_isr

default_isr_fail:
pre_isr
call default_isr_null
post_isr
