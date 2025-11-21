global keyboard_isr
global timer_isr
global archinit
global kreset
extern K
extern kb_int

%define INTERRUPT 0x8e
%define TRAP 0x8f

%macro push15 0
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

%macro pop15 0
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
%endmacro

%macro isr_stub 1
  push15
  call %1
  pop15
  iretq
%endmacro

section .bss
align 8
idt:
  resq 512
kbq:
  resb 8
kbq_len:
  resb 1

section .rodata
align 8
isrs:
  times 32 dq kreset
  times  1 dq timer_isr
  times  1 dq keyboard_isr
  times 14 dq kreset

align 8
isr_types:
  times  2 db TRAP
  times  1 db INTERRUPT ; NMI
  times 29 db TRAP
  times  2 db INTERRUPT ; timer & keyboard interrupts
  times 14 db TRAP

section .text

align 8
timer_isr:
; this is basically minimal. increment tick counter and reopen timer interrupts.
; if we want to do more we will probably need to push/pop15.
  inc qword [rel K]
  push rax
  mov al, 0x20
  out 0x20, al
  pop rax
  iretq

align 8
keyboard_isr:
  push15
  in al, 0x60
  movzx rdi, al
  call kb_int
  mov al, 0x20
  out 0x20, al
  pop15
  iretq

align 8
archinit:
  ; populate IDT
  lea rdi, [rel idt]
  lea rcx, [rel isrs]
  lea rdx, [rel isr_types]
  mov rax, rdx
.idt_entry:
  ; store three parts of isr pointer
  mov rbx, qword [rcx]
  mov [rdi], word bx ; low
  sar rbx, 16
  mov [rdi + 6], word bx ; mid
  sar rbx, 16
  mov [rdi + 8], dword ebx ; high
  ; store other fields
  mov [rdi + 2], dword 0x28 ; 0x28 is the segment selector, mov dword zeroes out ist offset as well
  mov bl, byte [rdx] ; type attributes
  mov [rdi + 5], byte bl
  inc rdx
  add rcx, 8
  add rdi, 16
  cmp rcx, rax
  jne .idt_entry

  ; load IDT
  push idt
  push word 4095 ; sizeof idt - 1
  lidt [rsp]
  add rsp, 10

  ; configure PIT
  mov al, 0x36
  out 0x43, al
  mov dx, 0x40 ; store to dx instead of dl so dh is subsequently 0
  mov al, 0x9b
  out dx, al
  mov al, 0x2e
  out dx, al

  ; start PIC init -- each will now want 3 more bytes
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

kreset:
  push 0
  push 0
  lidt [rsp]
  int 0
