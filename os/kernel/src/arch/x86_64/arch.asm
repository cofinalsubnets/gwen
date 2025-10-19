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

section .data
align 8
key_buffer:
  times 64 db 0
key_buffer_idx:
  db 0

section .text

align 8
timer_isr:
  inc qword [rel g_ticks]
  push rax
  mov eax, 0x20
  out 0x20, al
  pop rax
  iretq

align 8
keyboard_isr_stub:
  isr_stub keyboard_interrupt_handler

k_reset:
  xor eax, eax
  push rax
  push rax
  lidt [rsp]
  int 0

ctx_switch:
  mov [rdi], rsp
  mov rsp, [rsi]
  ret

start_interrupts:
  ; configure PIT
  mov eax, 0x36
  out 0x43, al
  mov eax, 0x9b
  out 0x40, al
  mov eax, 0x2e
  out 0x40, al

  ; save original master and slave PIC masks
  in al, 0x21
  mov edi, eax
  in al, 0xa1
  mov esi, eax

  ; start init
  mov eax, 0x11
  out 0x20, al
  out 0xa0, al
  ; configure master PIC
  mov eax, 0x20
  out 0x21, al
  mov eax, 0x4
  out 0x21, al
  ; configure slave PIC
  mov eax, 0x28
  out 0xa1, al
  mov eax, 0x4
  out 0xa1, al
  ; set same mode on both
  mov eax, 0x1
  out 0x21, al
  out 0xa1, al
  ; restore original masks
  mov eax, edi
  out 0x21, al
  mov eax, esi
  out 0xa1, al
  ; unmask timer and keyboard interrupts
  in al, 0x21
  and al, 0xfc
  out 0x21, al
  sti
  ret
