; test/uuhomgen.l -- GENERATED from doc/misc/proto/dest.l by tools/dest2uu.l. Do not
; edit; refresh with `make uuhomgen`. The law-site programs, cv (r0 protocol)
; and ck (destination die), lifted to uu nlist terms -- 4 nats per instruction
; (op d a b), operands parity-tagged (reg r = 2r, imm k = 2k+1), frame offsets
; and labels coded as met, opcodes li0 mov1 ld2 st3 si4 add5 sub6 cmp7 set8
; br9 jmp10 label11 call12 ret13 push14 pop15 (the sp shuttle rides push/pop).
; Each source form is printed over its term. Laws: test/uuhomlaw.l; substrate:
; test/uuhom.l over test/uu.l (the kernel).
(assert
 ; ds1cv  (mov r0 r9)
 ; ds1cv  (push r0)
 ; ds1cv  (li r0 7)
 ; ds1cv  (pop r1)
 ; ds1cv  (st r1 8 r0)
 (defn ds1cv nlist
   (ncons 1 (ncons 0 (ncons 18 (ncons 0
    (ncons 14 (ncons 0 (ncons 0 (ncons 0
    (ncons 0 (ncons 0 (ncons 15 (ncons 0
    (ncons 15 (ncons 2 (ncons 0 (ncons 0
    (ncons 3 (ncons 2 (ncons 1 (ncons 0
    nnil)))))))))))))))))))))
 ; ds1ck  (si r9 8 7)
 (defn ds1ck nlist
   (ncons 4 (ncons 18 (ncons 1 (ncons 15
    nnil)))))
 ; ds2cv  (mov r0 r8)
 ; ds2cv  (push r0)
 ; ds2cv  (li r0 1)
 ; ds2cv  (pop r1)
 ; ds2cv  (add r0 r1 r0)
 ; ds2cv  (mov r8 r0)
 (defn ds2cv nlist
   (ncons 1 (ncons 0 (ncons 16 (ncons 0
    (ncons 14 (ncons 0 (ncons 0 (ncons 0
    (ncons 0 (ncons 0 (ncons 3 (ncons 0
    (ncons 15 (ncons 2 (ncons 0 (ncons 0
    (ncons 5 (ncons 0 (ncons 2 (ncons 0
    (ncons 1 (ncons 16 (ncons 0 (ncons 0
    nnil)))))))))))))))))))))))))
 ; ds2ck  (add r8 r8 1)
 (defn ds2ck nlist
   (ncons 5 (ncons 16 (ncons 16 (ncons 3
    nnil)))))
 ; ds3cv  (mov r0 r8)
 ; ds3cv  (push r0)
 ; ds3cv  (mov r0 r9)
 ; ds3cv  (pop r1)
 ; ds3cv  (cmp r1 r0)
 ; ds3cv  (set lt r0)
 ; ds3cv  (cmp r0 0)
 ; ds3cv  (br eq l0)
 ; ds3cv  (mov r0 r9)
 ; ds3cv  (push r0)
 ; ds3cv  (li r0 3)
 ; ds3cv  (pop r1)
 ; ds3cv  (cmp r1 r0)
 ; ds3cv  (set lt r0)
 ; ds3cv  (cmp r0 0)
 ; ds3cv  (br eq l0)
 ; ds3cv  (li r0 1)
 ; ds3cv  (jmp l1)
 ; ds3cv  (label l0)
 ; ds3cv  (li r0 0)
 ; ds3cv  (label l1)
 ; ds3cv  (cmp r0 0)
 ; ds3cv  (br eq l2)
 ; ds3cv  (li r0 1)
 ; ds3cv  (st r4 -8 r0)
 ; ds3cv  (jmp l3)
 ; ds3cv  (label l2)
 ; ds3cv  (li r0 0)
 ; ds3cv  (st r4 -8 r0)
 ; ds3cv  (label l3)
 (defn ds3cv nlist
   (ncons 1 (ncons 0 (ncons 16 (ncons 0
    (ncons 14 (ncons 0 (ncons 0 (ncons 0
    (ncons 1 (ncons 0 (ncons 18 (ncons 0
    (ncons 15 (ncons 2 (ncons 0 (ncons 0
    (ncons 7 (ncons 2 (ncons 0 (ncons 0
    (ncons 8 (ncons 1 (ncons 0 (ncons 0
    (ncons 7 (ncons 0 (ncons 1 (ncons 0
    (ncons 9 (ncons 9 (ncons 1 (ncons 0
    (ncons 1 (ncons 0 (ncons 18 (ncons 0
    (ncons 14 (ncons 0 (ncons 0 (ncons 0
    (ncons 0 (ncons 0 (ncons 7 (ncons 0
    (ncons 15 (ncons 2 (ncons 0 (ncons 0
    (ncons 7 (ncons 2 (ncons 0 (ncons 0
    (ncons 8 (ncons 1 (ncons 0 (ncons 0
    (ncons 7 (ncons 0 (ncons 1 (ncons 0
    (ncons 9 (ncons 9 (ncons 1 (ncons 0
    (ncons 0 (ncons 0 (ncons 3 (ncons 0
    (ncons 10 (ncons 3 (ncons 0 (ncons 0
    (ncons 11 (ncons 1 (ncons 0 (ncons 0
    (ncons 0 (ncons 0 (ncons 1 (ncons 0
    (ncons 11 (ncons 3 (ncons 0 (ncons 0
    (ncons 7 (ncons 0 (ncons 1 (ncons 0
    (ncons 9 (ncons 9 (ncons 5 (ncons 0
    (ncons 0 (ncons 0 (ncons 3 (ncons 0
    (ncons 3 (ncons 8 (ncons 3 (ncons 0
    (ncons 10 (ncons 7 (ncons 0 (ncons 0
    (ncons 11 (ncons 5 (ncons 0 (ncons 0
    (ncons 0 (ncons 0 (ncons 1 (ncons 0
    (ncons 3 (ncons 8 (ncons 3 (ncons 0
    (ncons 11 (ncons 7 (ncons 0 (ncons 0
    nnil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 ; ds3ck  (cmp r8 r9)
 ; ds3ck  (br ge l0)
 ; ds3ck  (cmp r9 3)
 ; ds3ck  (br ge l0)
 ; ds3ck  (si r4 -8 1)
 ; ds3ck  (jmp l1)
 ; ds3ck  (label l0)
 ; ds3ck  (si r4 -8 0)
 ; ds3ck  (label l1)
 (defn ds3ck nlist
   (ncons 7 (ncons 16 (ncons 18 (ncons 0
    (ncons 9 (ncons 3 (ncons 1 (ncons 0
    (ncons 7 (ncons 18 (ncons 7 (ncons 0
    (ncons 9 (ncons 3 (ncons 1 (ncons 0
    (ncons 4 (ncons 8 (ncons 3 (ncons 3
    (ncons 10 (ncons 3 (ncons 0 (ncons 0
    (ncons 11 (ncons 1 (ncons 0 (ncons 0
    (ncons 4 (ncons 8 (ncons 3 (ncons 1
    (ncons 11 (ncons 3 (ncons 0 (ncons 0
    nnil)))))))))))))))))))))))))))))))))))))
 ; ds4cv  (mov r0 r8)
 ; ds4cv  (mov r6 r0)
 ; ds4cv  (li r0 3)
 ; ds4cv  (mov r5 r0)
 ; ds4cv  (call f)
 ; ds4cv  (ret)
 (defn ds4cv nlist
   (ncons 1 (ncons 0 (ncons 16 (ncons 0
    (ncons 1 (ncons 12 (ncons 0 (ncons 0
    (ncons 0 (ncons 0 (ncons 7 (ncons 0
    (ncons 1 (ncons 10 (ncons 0 (ncons 0
    (ncons 12 (ncons 9 (ncons 0 (ncons 0
    (ncons 13 (ncons 0 (ncons 0 (ncons 0
    nnil)))))))))))))))))))))))))
 ; ds4ck  (mov r6 r8)
 ; ds4ck  (li r5 3)
 ; ds4ck  (jmp f)
 (defn ds4ck nlist
   (ncons 1 (ncons 12 (ncons 16 (ncons 0
    (ncons 0 (ncons 10 (ncons 7 (ncons 0
    (ncons 10 (ncons 9 (ncons 0 (ncons 0
    nnil)))))))))))))
 ; ds4cvargs  (mov r0 r8)
 ; ds4cvargs  (mov r6 r0)
 ; ds4cvargs  (li r0 3)
 ; ds4cvargs  (mov r5 r0)
 (defn ds4cvargs nlist
   (ncons 1 (ncons 0 (ncons 16 (ncons 0
    (ncons 1 (ncons 12 (ncons 0 (ncons 0
    (ncons 0 (ncons 0 (ncons 7 (ncons 0
    (ncons 1 (ncons 10 (ncons 0 (ncons 0
    nnil)))))))))))))))))
 ; ds4ckargs  (mov r6 r8)
 ; ds4ckargs  (li r5 3)
 (defn ds4ckargs nlist
   (ncons 1 (ncons 12 (ncons 16 (ncons 0
    (ncons 0 (ncons 10 (ncons 7 (ncons 0
    nnil)))))))))
 ; de5kcv  (li r0 7)
 (defn de5kcv nlist
   (ncons 0 (ncons 0 (ncons 15 (ncons 0
    nnil)))))
 ; de5kck  (li r0 7)
 (defn de5kck nlist
   (ncons 0 (ncons 0 (ncons 15 (ncons 0
    nnil)))))
 ; de5vcv  (ld r0 r4 -8)
 (defn de5vcv nlist
   (ncons 2 (ncons 0 (ncons 8 (ncons 3
    nnil)))))
 ; de5vck  (ld r0 r4 -8)
 (defn de5vck nlist
   (ncons 2 (ncons 0 (ncons 8 (ncons 3
    nnil)))))
 ; de5bcv  (mov r0 r9)
 ; de5bcv  (push r0)
 ; de5bcv  (li r0 1)
 ; de5bcv  (pop r1)
 ; de5bcv  (add r0 r1 r0)
 (defn de5bcv nlist
   (ncons 1 (ncons 0 (ncons 18 (ncons 0
    (ncons 14 (ncons 0 (ncons 0 (ncons 0
    (ncons 0 (ncons 0 (ncons 3 (ncons 0
    (ncons 15 (ncons 2 (ncons 0 (ncons 0
    (ncons 5 (ncons 0 (ncons 2 (ncons 0
    nnil)))))))))))))))))))))
 ; de5bck  (mov r0 r9)
 ; de5bck  (add r0 r0 1)
 (defn de5bck nlist
   (ncons 1 (ncons 0 (ncons 18 (ncons 0
    (ncons 5 (ncons 0 (ncons 0 (ncons 3
    nnil))))))))))
