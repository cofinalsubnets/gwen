extern keyboard_interrupt_handler  ; defined in C
extern g_ticks

global keyboard_isr_stub
global timer_isr
global start_interrupts
global key_buffer
global key_buffer_idx
global k_reset

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
timer_isr:
  inc qword [rel g_ticks]
  push rax
  mov al, 0x20
  out 0x20, al
  pop rax
  iretq

align 8
keyboard_isr_stub:
  isr_stub keyboard_interrupt_handler

k_reset:
  push 0
  push 0
  lidt [rsp]
  int 0

ctx_switch:
  mov [rdi], rsp
  mov rsp, [rsi]
  ret

start_interrupts:
  ; configure PIT
  mov al, 0x36
  out 0x43, al
  mov dx, 0x40 ; store to dx instead of dl so dh is subsequently 0
  mov al, 0x9b
  out dx, al
  mov al, 0x2e
  out dx, al

  ; start init -- each will now want 3 more bytes
  mov al, 0x11
  out 0x20, al
  out 0xa0, al

  ; first two bytes to master
  mov dl, 0x21 ; master PIC data port number in dx
  mov al, 0x20
  out dx, al
  mov al, 4
  out dx, al

  ; first two bytes to slave
  mov dl, 0xa1 ; slave PIC data port number in dx
  mov al, 0x28
  out dx, al
  mov al, 2
  out dx, al

  ; last byte to each
  mov al, 1
  out 0x21, al
  out dx, al
  mov al, 0
  out 0x21, al
  out dx, al

  ; enable interrupts
  sti
  ret
