; crew/moon/lib/mksys.l -- lay sys.o, the raw seam's hand-written machine tail (rung 4):
; the five functions C cannot say. everything else in the gcc-free link is C compiled
; by mooncc (nolibc.c + math/); these are the register-exact leaves under it:
;   __ai_sys(n,a,b,c,d,e,f)  the 7-slot syscall trampoline (nolibc.c calls nothing else)
;   __sigsetjmp / siglongjmp our own jmp_buf INSIDE the glibc-sized 25-long buffer:
;                            [0..5] rbx rbp r12 r13 r14 r15, [6] rsp, [7] rip,
;                            [8] the signal mask (rt_sigprocmask saves/restores it --
;                            love.c's fault barrier is sigsetjmp(env,1), the mask is load-bearing)
;   __ai_sigret              the SA_RESTORER tail (rt_sigreturn) nolibc's sigaction installs
; (sqrt stays in C -- crew/moon/lib/math/am.c's am_sqrt, OURS and IEEE-exact; a thing
; C CAN say has no place here.) three targets, one layer: (mksys dst) lays x64,
; (mksys-arm64 dst) aarch64, (mksys-riscv dst) riscv64 -- all seed mksys-as, which
; picks the arch's IR table. the x64 lane serves TWO kernels from one object
; (doc/plan/seed-universal.md rung UV1): the tail is OS-blind -- CF cleared
; before the insn, and a carry answer (freebsd's error convention) parked
; below linux's band as -(errno+4096), so impl.h's __ai_call reads one law --
; and the sigprocmask leaves branch on __ai_osv (os.c's probe) between
; rt_sigprocmask 14 (4 args, 8-byte set, SIG_* 0/2) and sigprocmask 340
; (3 args, 16-byte set, SIG_* 1/3). (mksys-freebsd dst) survives as an alias.
; (the scaffold is shared; the payloads are irreducibly per-ISA). runs after
; crew/holo/{elf,obj}.l in a cat (the mooncc prefix layers; the assembler itself
; rides the baked holo book):
;   { cat crew/kore/{text,core,asbook}.l crew/holo/{elf,obj}.l crew/moon/lib/mksys.l;
;     echo '(mksys "out/.../sys.o")'; } | out/host/love
; register vocabulary is holo's: r0=rax r1=rcx r2=rdx r3=rbx r4=rbp r5=rsi r6=rdi
; r7=r8 r8=r9 r9=r10 r10=r11 r11=r12 r12=r13 r13=r14 r14=r15.
(module 'moon
(use 'holo) ; objelf, the relocatable writer
(: (mksys dst) (mksys-as 'x64 dst)
   (mksys-arm64 dst) (mksys-as 'arm64 dst)
   (mksys-riscv dst) (mksys-as 'riscv64 dst)
   (mksys-freebsd dst) (mksys-as 'x64 dst)   ; one object serves both kernels
   (mksys-as arch dst) (write-bytes dst (mksys-bytes arch))
   ; the bytes alone -- moon.l's runtime pull lays sys in-memory through this
   (mksys-bytes arch)
    (: ir (? (id? arch 'x64)
         '(
         ; -- __ai_sys: SysV args (rdi rsi rdx rcx r8 r9 [rsp+8]) -> the kernel's
         ;    (rax rdi rsi rdx r10 r8 r9). each mov reads a slot the previous one
         ;    just drained, so the chain needs no scratch. the tail is OS-BLIND:
         ;    CF goes in clear (linux hands it back untouched through r11), and a
         ;    carry answer -- freebsd's error, +errno in rax ('below IS jc) --
         ;    parks below linux's band as -(errno+4096). impl.h's __ai_call
         ;    unparks; the tail never learns which kernel answered.
         (label __ai_sys)
         (mov r0 r6)                 ; rax <- n
         (mov r6 r5)                 ; rdi <- a
         (mov r5 r2)                 ; rsi <- b
         (mov r2 r1)                 ; rdx <- c
         (mov r9 r7)                 ; r10 <- d
         (mov r7 r8)                 ; r8  <- e
         (ld r8 sp 8)                ; r9  <- f (the one stack arg)
         (cmp r0 0)                  ; clears CF; flags ride the insn both ways
         (sys)
         (br below sys-err)
         (ret)                       ; rax rides out (-errno included; C wraps it)
         (label sys-err)
         (neg r0)
         (lea r0 r0 -4096)           ; -(errno+4096): parked for __ai_call
         (ret)

         ; -- __sigsetjmp(buf, save): pin the callee-saved file + the return seam,
         ;    then read the current mask into buf[8] by the kernel under us
         ;    (__ai_osv, os.c's probe): rt_sigprocmask(0, NULL, &buf[8], 8), or
         ;    freebsd's sigprocmask(1, NULL, &buf[8]) -- a 16-byte set, so it
         ;    takes buf[8..9] (the glibc-sized buffer has room).
         (label __sigsetjmp)
         (st r6 0  r3)               ; rbx
         (st r6 8  r4)               ; rbp
         (st r6 16 r11)              ; r12
         (st r6 24 r12)              ; r13
         (st r6 32 r13)              ; r14
         (st r6 40 r14)              ; r15
         (ld r0 sp 0)                ; the return rip sits at [rsp]
         (st r6 56 r0)
         (lea r0 sp 8)               ; the caller's rsp once we return
         (st r6 48 r0)
         (lea r2 r6 64)              ; rdx = &buf[8] (oldset)
         (li r5 0)                   ; rsi = NULL (read only)
         (la r1 __ai_osv)
         (ld r1 r1 0)
         (cmp r1 2)
         (br eq sj-fb)
         (li r6 0)                   ; rdi = SIG_BLOCK
         (li r9 8)                   ; r10 = sigsetsize
         (li r0 14)                  ; rt_sigprocmask
         (sys)
         (li r0 0)                   ; the first arrival answers 0
         (ret)
         (label sj-fb)
         (li r6 1)                   ; rdi = SIG_BLOCK (freebsd: 1)
         (li r0 340)                 ; sigprocmask
         (sys)
         (li r0 0)
         (ret)

         ; -- siglongjmp(buf, val): restore the mask FIRST (the fault barrier jumps
         ;    out of a signal handler), then the register file, then jump the saved
         ;    rip. ⚠ val and buf ride rbp/rbx: freebsd's syscall exit ZEROES the
         ;    scratch registers (a leak mitigation linux does not have), so only
         ;    the callee-saved file survives the kernel -- and it is about to be
         ;    overwritten anyway. rbx restores LAST, off itself.
         (label siglongjmp)
         (mov r4 r5)                 ; rbp <- val
         (mov r3 r6)                 ; rbx <- buf
         (lea r5 r6 64)              ; rsi = &buf[8] (the saved mask)
         (li r2 0)                   ; rdx = NULL
         (la r1 __ai_osv)
         (ld r1 r1 0)
         (cmp r1 2)
         (br eq lj-fb)
         (li r6 2)                   ; rdi = SIG_SETMASK
         (li r9 8)                   ; r10 = sigsetsize
         (li r0 14)                  ; rt_sigprocmask
         (sys)
         (jmp lj-file)
         (label lj-fb)
         (li r6 3)                   ; rdi = SIG_SETMASK (freebsd: 3)
         (li r0 340)                 ; sigprocmask
         (sys)
         (label lj-file)
         (mov r0 r4)                 ; rax <- val, before rbp is restored
         (ld r4  r3 8)               ; rbp
         (ld r11 r3 16)              ; r12
         (ld r12 r3 24)              ; r13
         (ld r13 r3 32)              ; r14
         (ld r14 r3 40)              ; r15
         (ld sp r3 48)               ; the saved rsp
         (ld r1 r3 56)               ; the saved rip
         (ld r3 r3 0)                ; rbx, last, off itself
         (cmp r0 0)                  ; 0 lifted to 1 (the setjmp law)
         (br ne lj-live)
         (li r0 1)
         (label lj-live)
         (jmpr r1)

         ; -- __ai_sigret: the restorer a kernel sigaction wants under SA_RESTORER.
         ;    linux's number: only linux's sigaction installs it -- freebsd has no
         ;    SA_RESTORER, a handler there returns on the kernel's own trampoline.
         (label __ai_sigret)
         (li r0 15)                  ; rt_sigreturn
         (sys))

         (id? arch 'arm64)
         ; the aarch64 lane: same four leaves on the asm-generic table. registers are
         ; holo's arm64 file, rN = xN one-to-one (plus fp/lr/sp by name). the jmp_buf
         ; inside the same 25-long buffer: [0..9] x19..x28, [10] fp, [11] sp, [12] lr
         ; (the return pc -- BL parked it there, so it IS the return seam), [13..20]
         ; d8..d15 (AAPCS saves the LOW HALVES only -- str dN is exactly that),
         ; [21] the signal mask. rt_sigprocmask is 135; the kernel preserves every
         ; register but x0 across svc, so buf/val stage in x9/x10 and ride through.
         ; __ai_sigret stays laid (rt_sigreturn 139) though aarch64's sigaction never
         ; points at it -- the kernel supplies its own vdso trampoline (nolibc gates
         ; SA_RESTORER off); the symbol keeps the two sys.o surfaces identical.
         '(
         ; -- __ai_sys: args x0..x6 (n,a..f) -> the kernel's x8=nr, x0..x5.
         ;    x8 drains first, then each mov reads the slot ahead of it.
         (label __ai_sys)
         (mov r8 r0)                 ; x8 <- n
         (mov r0 r1)                 ; x0 <- a
         (mov r1 r2)                 ; x1 <- b
         (mov r2 r3)                 ; x2 <- c
         (mov r3 r4)                 ; x3 <- d
         (mov r4 r5)                 ; x4 <- e
         (mov r5 r6)                 ; x5 <- f
         (sys)
         (ret)                       ; x0 rides out (-errno included; C wraps it)

         ; -- __sigsetjmp(buf, save): the callee-saved file, the return seam,
         ;    then the current mask into buf[21] (rt_sigprocmask how=0 set=NULL).
         (label __sigsetjmp)
         (st r0 0   r19)
         (st r0 8   r20)
         (st r0 16  r21)
         (st r0 24  r22)
         (st r0 32  r23)
         (st r0 40  r24)
         (st r0 48  r25)
         (st r0 56  r26)
         (st r0 64  r27)
         (st r0 72  r28)
         (st r0 80  fp)
         (lea r9 sp 0)               ; sp reads through an add
         (st r0 88  r9)
         (st r0 96  lr)
         (stsd r0 104 f8)
         (stsd r0 112 f9)
         (stsd r0 120 f10)
         (stsd r0 128 f11)
         (stsd r0 136 f12)
         (stsd r0 144 f13)
         (stsd r0 152 f14)
         (stsd r0 160 f15)
         (lea r2 r0 168)             ; x2 = &buf[21] (oldset)
         (li r1 0)                   ; x1 = NULL (read only)
         (li r0 0)                   ; x0 = SIG_BLOCK
         (li r3 8)                   ; x3 = sigsetsize
         (li r8 135)                 ; rt_sigprocmask
         (sys)
         (li r0 0)                   ; the first arrival answers 0
         (ret)

         ; -- siglongjmp(buf, val): the mask FIRST (the fault barrier jumps out
         ;    of a signal handler), then the file, then ret through the saved lr.
         (label siglongjmp)
         (mov r9 r0)                 ; x9  <- buf
         (mov r10 r1)                ; x10 <- val
         (lea r1 r9 168)             ; x1 = &buf[21] (the saved mask)
         (li r0 2)                   ; x0 = SIG_SETMASK
         (li r2 0)                   ; x2 = NULL
         (li r3 8)                   ; x3 = sigsetsize
         (li r8 135)                 ; rt_sigprocmask
         (sys)
         (ld r19 r9 0)
         (ld r20 r9 8)
         (ld r21 r9 16)
         (ld r22 r9 24)
         (ld r23 r9 32)
         (ld r24 r9 40)
         (ld r25 r9 48)
         (ld r26 r9 56)
         (ld r27 r9 64)
         (ld r28 r9 72)
         (ld fp  r9 80)
         (ldsd f8  r9 104)
         (ldsd f9  r9 112)
         (ldsd f10 r9 120)
         (ldsd f11 r9 128)
         (ldsd f12 r9 136)
         (ldsd f13 r9 144)
         (ldsd f14 r9 152)
         (ldsd f15 r9 160)
         (mov r0 r10)                ; x0 <- val, 0 lifted to 1 (the setjmp law)
         (cmp r0 0)
         (br ne lj-live)
         (li r0 1)
         (label lj-live)
         (ld r11 r9 88)
         (lea sp r11 0)              ; sp <- the saved sp
         (ld lr r9 96)
         (ret)                       ; ret x30 = the saved return pc

         ; -- __ai_sigret: rt_sigreturn, laid for surface parity (see above).
         (label __ai_sigret)
         (li r8 139)                 ; rt_sigreturn
         (sys))

         ; the riscv64 lane: the same four leaves on the same asm-generic table
         ; (rt_sigprocmask 135, rt_sigreturn 139; ecall preserves every register
         ; but a0, so buf/val stage in t0/t1 and ride through). registers are
         ; holo's riscv file: r0..r7 = a0..a7, r8/r9 = t0/t1, fp = s0,
         ; r15/r13/r14 = s1/s2/s3, r19..r26 = s4..s11, lr = ra. the jmp_buf
         ; inside the 25-long buffer: [0] ra (the return pc), [1] sp,
         ; [2..13] s0..s11, [14] the signal mask. NO fp slots: the backend's
         ; fp file is all caller-saved (fa/ft; the fs bank is unmapped), so no
         ; mooncc-compiled frame can hold a live double in one across a call.
         '(
         ; -- __ai_sys: args a0..a6 (n,a..f) -> the kernel's a7=nr, a0..a5.
         ;    a7 drains n first, then each mov reads the slot ahead of it.
         (label __ai_sys)
         (mov r7 r0)                 ; a7 <- n
         (mov r0 r1)                 ; a0 <- a
         (mov r1 r2)                 ; a1 <- b
         (mov r2 r3)                 ; a2 <- c
         (mov r3 r4)                 ; a3 <- d
         (mov r4 r5)                 ; a4 <- e
         (mov r5 r6)                 ; a5 <- f
         (sys)
         (ret)                       ; a0 rides out (-errno included; C wraps it)

         ; -- __sigsetjmp(buf, save): the callee-saved file, the return seam,
         ;    then the current mask into buf[14] (rt_sigprocmask how=0 set=NULL).
         (label __sigsetjmp)
         (st r0 0   lr)
         (st r0 8   sp)
         (st r0 16  fp)
         (st r0 24  r15)
         (st r0 32  r13)
         (st r0 40  r14)
         (st r0 48  r19)
         (st r0 56  r20)
         (st r0 64  r21)
         (st r0 72  r22)
         (st r0 80  r23)
         (st r0 88  r24)
         (st r0 96  r25)
         (st r0 104 r26)
         (lea r2 r0 112)             ; a2 = &buf[14] (oldset)
         (li r1 0)                   ; a1 = NULL (read only)
         (li r0 0)                   ; a0 = SIG_BLOCK
         (li r3 8)                   ; a3 = sigsetsize
         (li r7 135)                 ; rt_sigprocmask
         (sys)
         (li r0 0)                   ; the first arrival answers 0
         (ret)

         ; -- siglongjmp(buf, val): the mask FIRST (the fault barrier jumps out
         ;    of a signal handler), then the file, then ret through the saved ra.
         (label siglongjmp)
         (mov r8 r0)                 ; t0 <- buf
         (mov r9 r1)                 ; t1 <- val
         (lea r1 r8 112)             ; a1 = &buf[14] (the saved mask)
         (li r0 2)                   ; a0 = SIG_SETMASK
         (li r2 0)                   ; a2 = NULL
         (li r3 8)                   ; a3 = sigsetsize
         (li r7 135)                 ; rt_sigprocmask
         (sys)
         (ld fp  r8 16)
         (ld r15 r8 24)
         (ld r13 r8 32)
         (ld r14 r8 40)
         (ld r19 r8 48)
         (ld r20 r8 56)
         (ld r21 r8 64)
         (ld r22 r8 72)
         (ld r23 r8 80)
         (ld r24 r8 88)
         (ld r25 r8 96)
         (ld r26 r8 104)
         (mov r0 r9)                 ; a0 <- val, 0 lifted to 1 (the setjmp law)
         (cmp r0 0)
         (br ne lj-live)
         (li r0 1)
         (label lj-live)
         (ld sp r8 8)                ; the saved sp
         (ld lr r8 0)                ; the saved return pc
         (ret)                       ; jalr x0, 0(ra)

         ; -- __ai_sigret: rt_sigreturn, laid for surface parity (riscv's
         ;    sigaction rides the kernel's vdso trampoline, like aarch64).
         (label __ai_sigret)
         (li r7 139)                 ; rt_sigreturn
         (sys)))
       (objelf arch ir () '("__ai_sys" "__sigsetjmp" "siglongjmp" "__ai_sigret") () () () () () () ())))
) ; module 'moon
