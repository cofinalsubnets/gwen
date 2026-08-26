; src/mkvec.l -- lay vec.o, the kernel's INTERRUPT tail: the exception and IRQ entry points, which are register-exact leaves C
; cannot say. mksys.l is the precedent and the shape is the same -- one scaffold,
; a payload per ISA -- but this one is generated rather than transcribed: the 32
; x86 stubs and the 16 aarch64 vector slots were `.macro`/`.rept` loops in GAS,
; and a love loop says the same thing without an assembler's macro language.
;
;   (lay-vec dst "x86_64")   the IDT, the ISR tables, exc_stub_0..31, the three
;                            IRQ handlers, archinit (IDT + PIC + PIT), k_reset/k_halt
;   (lay-vec dst "aarch64")  the 2 KiB-aligned 16 x 0x80 vector table, the IRQ
;                            save/restore path, the fault report path
;
; runs after crew/holo/{elf,obj}.l in a (use 'holo) cat; src/kernel.mk
; drives it. register vocabulary is holo's -- x86: r0=rax r1=rcx r2=rdx r3=rbx
; r4=rbp r5=rsi r6=rdi r7..r14=r8..r15; aarch64: rN = xN.
;
; ⚠ x86 port i/o here is REGISTER-CONTRACTED (dx the port, al the datum), so
; every `out 0x21, al` becomes a `li r2 <port>` then `outb`. GAS could reach
; ports 0..255 with the immediate form and hold dx still across two writes; we
; reload it, which is the same bus traffic and one instruction more.
(:
   ;; the four list helpers the loops want. `iota` answers an ARRAY and these
   ;; streams are lists, so they are spelled here rather than borrowed.
   (v-upto n)  (: (go i) (? (n <= i) () (i >< (go (i + 1)))) (go 0))
   (v-rep n x) (? (n < 1) () (x >< (v-rep (n - 1) x)))
   (v-zeros n) (v-rep n 0)
   (v-mapi f l) (: (go l2 i) (? !(two? l2) () ((f i <l2) >< (go >l2 (i + 1)))) (go l 0))

   ;; -- x86_64 ------------------------------------------------------------
   ;; the vectors the CPU pushes an error code for. every other one pushes a
   ;; dummy 0 first, so every frame reaching k_exception has ONE shape --
   ;; that is the whole reason the stubs exist.
   xerr-vecs '(8 10 11 12 13 14 17 21 29 30)
   ;; push15/pop15: every GPR but rsp, in the order struct k_frame (arch.c)
   ;; reads them -- lowest address first is the REVERSE of the push order.
   x-save '(r4 r0 r3 r1 r2 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14)   ; rbp rax rbx rcx rdx rsi rdi r8..r15
   x-push (map (\ r ['push r]) x-save)
   x-pop  (map (\ r ['pop r]) (rev x-save))
   ;; an EOI to the primary PIC. the port rides dx, the datum al.
   x-eoi '((li r0 0x20) (li r2 0x20) (outb))
   (x-stub v)
    ([['label (intern ("exc_stub_" + (show v)))]]
     + (? (member? v xerr-vecs) () [['push 0]])       ; the dummy error code
     + [['push v] ['jmp 'exc_common]])
   x-stub-names (map (\ v ("exc_stub_" + (show v))) (v-upto 32))

   ;; the ISR table archinit walks: 0..31 the exception stubs, then the 16
   ;; remapped PIC vectors. isrs and isr_types are walked in LOCKSTEP, and
   ;; isrs ending right before isr_types is what lets the loop exit on
   ;; `cmp rcx, rax` -- so the two must stay adjacent, in this order.
   x-isrs ((map (\ v (intern ("exc_stub_" + (show v)))) (v-upto 32))
           + '(timer_isr        ; 32: IRQ0  PIT
               keyboard_isr     ; 33: IRQ1  PS/2
               k_reset          ; 34: IRQ2
               k_reset          ; 35: IRQ3
               uart_isr)        ; 36: IRQ4  COM1
           + (v-rep 11 'k_reset))                       ; 37..47: a stray IRQ reboots
   x-trap 0x8f                                        ; present, DPL 0, 64-bit TRAP gate
   x-intr 0x8e                                        ; ..and INTERRUPT gate (clears IF on entry)
   x-types ([x-trap x-trap x-intr]                    ; 0 #DE, 1 #DB, 2 NMI
            + (v-rep 29 x-trap)                         ; 3..31
            + [x-intr x-intr x-trap x-trap x-intr]    ; 32 PIT, 33 PS/2, 34, 35, 36 COM1
            + (v-rep 11 x-trap))                        ; 37..47

   ;; the two lockstep tables, then the two LIDT operands. an entry is an
   ;; 8-byte absolute pointer -- a relocation in the .o, since the address is
   ;; the linker's to know. ⚠ laid as .data, not .rodata: under the fused -pie
   ;; artifact these sites ride the love_rela table and __ai_reloc WRITES them at
   ;; a hosted start -- a read-only segment there is a startup segfault, and
   ;; our linker has no relro middle ground. metal never cared either way.
   (x-quad nm) ['raw [['fix 8 'abs64 nm 0]]]
   x-rodata
    ([['label 'isrs]]
     + (map x-quad x-isrs)
     + [['label 'isr_types] ['raw x-types]]
     ;; the LIDT operands: a 2-byte limit then the 8-byte base. `.balign 2` in
     ;; GAS, and the offset is already even, so this pads nothing -- it says
     ;; the requirement out loud.
     + [['align 2] ['label 'idtreg] ['raw (le16 4095)]]       ; the limit: sizeof idt - 1
     + [(x-quad 'idt)]
     + [['label 'idtreg_null] ['raw [0 0]]]                   ; an EMPTY IDT: int 0 triple-faults
     + [['raw (v-zeros 8)]])

   ;; .bss: the IDT itself, 256 entries of 16 bytes. NOBITS -- the loader hands
   ;; it over zeroed and archinit fills every entry before LIDT.
   x-bss [['label 'idt] ['raw (v-zeros 4096)]]

   ;; .text.
   x-text
    ((foldl (\ a v (a + (x-stub v))) () (v-upto 32))
     ;; the common tail. after push15 the stack from rsp upward IS struct
     ;; k_frame, so rdi = rsp is the whole argument marshalling. rsp is
     ;; 16-byte aligned at the call: the CPU aligns it on entry and pushes
     ;; five qwords, the stub two, push15 fifteen -- 176 bytes, 11 x 16.
     + [['align 8] ['label 'exc_common]]
     + x-push
     + '((mov r6 sp) (call k_exception))
     + x-pop
     + '((add sp sp 16)          ; discard the vector + error code
         (iretq))                ; (the fatal handler never returns)
     ;; the timer (IRQ0). GAS bumped the counter with one `inc qword ptr
     ;; [rip + kticks]`; the neutral surface has no read-modify-write to
     ;; memory, so it is a load, an add and a store through two registers we
     ;; save ourselves. no call, so no stack-alignment duty.
     + [['align 8] ['label 'timer_isr]]
     + '((push r0) (push r2)
         (la r0 kticks) (ld r2 r0 0) (add r2 r2 1) (st r0 0 r2))
     + x-eoi
     + '((pop r2) (pop r0) (iretq))
     ;; the PS/2 keyboard (IRQ1): read the scancode off port 0x60 and hand it
     ;; to kb_int as an unsigned byte.
     + [['align 8] ['label 'keyboard_isr]]
     + x-push
     + '((li r2 0x60) (inb)        ; al <- scancode
         (mov r6 r0) (zx1 r6)    ; rdi = (uint8_t) al
         (call kb_int))
     + x-eoi
     + x-pop
     + '((iretq))
     ;; COM1 RX (IRQ4).
     + [['align 8] ['label 'uart_isr]]
     + x-push
     + '((call k_uart))
     + x-eoi
     + x-pop
     + '((iretq))
     ;; archinit: walk isrs/isr_types in lockstep packing each (handler, type)
     ;; pair into a 16-byte IDT descriptor, LIDT, then the PIT and the PIC.
     ;; x87/SSE is enabled earlier, at the top of kmain -- it must be on before
     ;; any C runs at all, so there is nothing to do for the FPU here.
     + [['align 8] ['label 'archinit]]
     + '((la r6 idt)             ; rdi = the entry being filled
         (la r1 isrs)            ; rcx = the handler cursor
         (la r2 isr_types)       ; rdx = the type cursor
         (mov r0 r2)             ; rax = the sentinel: rcx reaches isr_types when done
         ;; the bootloader's code selector. IDT entries must name a segment
         ;; valid in the CURRENT GDT, and the doors disagree -- OVMF leaves
         ;; cs = 0x38, our own PVH stub 0x08. reading it is the only way that
         ;; is true of both.
         (ldseg r8 1)            ; r9 = cs
         (label idt_entry)
         (ld r3 r1 0)            ; rbx = the handler address
         (st2 r6 0 r3)           ; [rdi]    = its low 16
         (sar r3 16)
         (st2 r6 6 r3)           ; [rdi+6]  = its mid 16
         (sar r3 16)
         (st4 r6 8 r3)           ; [rdi+8]  = its high 32
         (st4 r6 2 r8)           ; [rdi+2]  = selector + IST/zero (the attr byte lands below)
         (ldu1 r3 r2 0)          ; the type byte (rbx is dead here -- the address is spent)
         (st1 r6 5 r3)           ; [rdi+5]  = type/attr
         (inc r2)
         (add r1 r1 8)
         (add r6 r6 16)
         (cmp r1 r0)
         (br ne idt_entry)
         (la r0 idtreg)
         (lidt r0 0)
         ;; PIT channel 0, rate-generator mode (0x36), divisor 0x2e9b -> ~100 Hz
         (li r0 0x36) (li r2 0x43) (outb)
         (li r2 0x40) (li r0 0x9b) (outb) (li r0 0x2e) (outb)
         ;; PIC ICW1, both controllers
         (li r0 0x11) (li r2 0x20) (outb) (li r2 0xa0) (outb)
         ;; primary ICW2/ICW3: vector base 0x20, secondary cascaded on IRQ2
         (li r2 0x21) (li r0 0x20) (outb) (li r0 4) (outb)
         ;; secondary ICW2/ICW3: vector base 0x28, cascade id 2
         (li r2 0xa1) (li r0 0x28) (outb) (li r0 2) (outb)
         ;; ICW4 (8086 mode) on both, then an all-zero mask: everything unmasked
         (li r0 1) (li r2 0x21) (outb) (li r2 0xa1) (outb)
         (li r0 0) (li r2 0x21) (outb) (li r2 0xa1) (outb)
         (sti)
         (ret))
     ;; reset by TRIPLE FAULT: LIDT a zero-size IDT, then int 0 -- the CPU
     ;; cannot dispatch the vector, escalates through #DF, and the host reboots.
     + [['align 8] ['label 'k_reset]]
     + '((la r0 idtreg_null)
         (lidt r0 0)
         (int 0)
         (label k_halt)
         (cli) (hlt) (jmp k_halt)))

   ;; -- aarch64 -----------------------------------------------------------
   ;; the table is 16 entries of 0x80 bytes and must be 2 KiB-aligned. we run
   ;; at EL1 on SP_EL1, so only the "current EL with SP_ELx" group can fire:
   ;; 0x200 sync, 0x280 IRQ, 0x300 FIQ, 0x380 SError. IRQs funnel to k_irq;
   ;; everything else funnels to a_fault (report + halt). the other twelve
   ;; entries cannot fire by construction but are routed to a_fault too, so a
   ;; surprise still reports instead of running off into padding.
   a-slots '(a_fault a_fault a_fault a_fault      ; current EL, SP_EL0: sync IRQ FIQ SError
             a_fault a_irq   a_fault a_fault      ; current EL, SP_ELx
             a_fault a_fault a_fault a_fault      ; lower EL, aarch64
             a_fault a_fault a_fault a_fault)     ; lower EL, aarch32
   ;; the IRQ path saves the caller-saved GPRs (x0..x18) plus x29/x30 -- k_irq
   ;; is AAPCS-compliant, so it preserves x19..x28 itself. 21 slots rounded to
   ;; 176 keeps sp 16-byte aligned. GAS paired them with stp/ldp; single
   ;; str/ldr say the same thing in twice the words and no scratch, and every
   ;; offset stays inside the 12-bit scaled immediate (so no far lane, so x16
   ;; -- which holo borrows for that -- is never touched while it holds a
   ;; saved value).
   a-save '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 fp lr)
   a-frame 176
   (a-slot-off i) (8 * i)
   a-push (v-mapi (\ i r ['st 'sp (a-slot-off i) r]) a-save)
   a-pop  (v-mapi (\ i r ['ld r 'sp (a-slot-off i)]) a-save)
   ;; the align rides AFTER each slot, the last one included, so the table is
   ;; exactly the 16 x 0x80 the architecture defines and the handlers start past
   ;; it. GAS packed .Lirq into the tail of the sixteenth slot (its branch is the
   ;; only live word there) and saved the 124 bytes; a whole table is worth them.
   a-text
    ((foldl (\ a s (a + [['jmp s] ['align 128]])) [['label 'vectors]] a-slots)
     ;; the CPU masks DAIF.I on entry, so the handler cannot nest and
     ;; ELR_EL1/SPSR_EL1 stay valid across the call; eret returns through them.
     + [['label 'a_irq] ['sub 'sp 'sp a-frame]]
     + a-push
     + '((call k_irq))
     + a-pop
     + [['add 'sp 'sp a-frame] ['eret]]
     ;; the fault path. pass ESR/ELR/FAR to k_fault, which reports and halts --
     ;; like x86_64's k_exception, faults are not resumed.
     + '((label a_fault)
         (mrs r0 esr_el1)
         (mrs r1 elr_el1)
         (mrs r2 far_el1)
         (call k_fault)
         (label a_halt)
         (wfi)
         (jmp a_halt)))

   ;; -- the two objects ---------------------------------------------------
   ;; SHF flags: 2 ALLOC, 3 ALLOC|WRITE, 6 ALLOC|EXEC. types: 1 PROGBITS,
   ;; 8 NOBITS. .text.vectors asks for 2048 because `vectors` goes straight
   ;; into VBAR_EL1, which ignores the low 11 bits -- a table off its
   ;; boundary would dispatch into the middle of another entry.
   (lay-vec dst arch) (write-bytes dst (lay-vec-bytes arch))
   (lay-vec-bytes arch)
    (? (arch = "x86_64")
       (objsecs 'amd64
                [[".text"   1 6 16 x-text]
                 [".data"   1 3 8  x-rodata]
                 [".bss"    8 3 16 x-bss]]
                '("archinit" "k_reset" "k_halt" "timer_isr" "keyboard_isr" "uart_isr")
                ()                       ; idt/isrs/idtreg are ours alone -- no C names them
                ()
                ;; the stubs and the common tail as LOCAL function symbols: nm,
                ;; gdb and a fault backtrace all name them, which a bare label
                ;; could not. GAS gave them the same visibility.
                (x-stub-names + '("exc_common")))
       (arch = "aarch64")
       (objsecs 'arm64
                [[".text.vectors" 1 6 2048 a-text]]
                '("vectors")             ; arch.c: extern uint8_t vectors[] -> VBAR_EL1
                () ()
                '("a_irq" "a_fault"))
       (scare 'mkvec-arch arch)))
