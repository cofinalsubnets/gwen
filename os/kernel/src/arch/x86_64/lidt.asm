idtr:
  dw 0
idtr_base:
  dq 0

global load_idt
align 8

load_idt:
  mov   [idtr], di
  mov   [idtr_base], rsi
  lidt  [idtr]
  ret
