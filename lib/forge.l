; lib/forge.l -- NIFS WRITTEN IN LOVE. a kernel is a crew/holo IR program plus an honest
; interpreted TWIN. forge assembles the program for the cpu it is running on and installs it
; through the `nif` seam; the twin is what runs everywhere the native cannot. `(use 'forge)`:
;
;   (forge-put nom arity mkir twin) -> register one kernel; mkir is a THUNK, twin the fallback
;   (forge-fit ())                  -> assemble + install the whole roster -> the fitted count
;   (forge-of nom)                  -> the installed value: native if it built, the twin if not
;   (forge-why ())                  -> ((reason nom ..) . count) rows -- why a decline declined
;   (forge-fitted? ())              -> how many kernels are running native right now
;
; ⚠ THE TWIN IS THE DEFINITION and the native is an optimisation, never the other way round.
; every guard in a kernel deopts INTO the twin, so a shape the IR does not say is not a bug and
; not a raise -- it is the twin's turn. that is what makes a kernel writable at all: it says the
; one monomorphic lane it is fast at and hands back everything else. crew/sat/flat.l is the
; same arrangement one layer up, where the twins are also the differential oracle.
;
; ⚠ mkir IS A THUNK because a kernel's IR carries LIVE ADDRESSES -- `forge-trayk` and friends
; are read out of THIS process with `apof`, and under ASLR they are a different number every
; run. so the IR is built at fit time, never earlier, and never kept.
;
; ⚠ A NATIVE CELL DOES NOT SURVIVE A BAKE, and it does not say so. the image codec reverts it
; to its twin (core/love.c img_nif_interp) because the W^X mapping dies with the baking
; process, so the bake SUCCEEDS, the value wakes, it answers correctly -- and it is the twin.
; measured on the 4096-cell battery: 4 ms native, 13 ms after a bake and a wake, which is the C
; nif's own 13. what bakes is the ROSTER -- thunks and twins, ordinary love data -- so a woken
; image is correct and slow until someone calls `forge-fit` again. that call is the whole cost
; of waking native: ~0.1 ms per kernel (50 fits of one kernel take 5 ms), against a re-derived
; nothing. the IR is kept rather than the bytes for the reason above it: a kernel's addresses
; are this process's, and the wake is where they are read.
;
; ⚠ THE FIT IS A BATCH -- every kernel assembles before any installs -- because `nif` mmaps a
; page-rounded arena PER CALL (core/love.c code_maplen), so a roster of n costs n pages and n
; vmas today. one arena for the batch is a change to that seam and this shape is what will feed it.
;
; ⚠ AND IT NEEDS THE GLAZE MODULE, not the glaze hook. the seal pulls `nif`/`nifx` off the book
; on every boot (host/main.c) and `(from 'glaze 'nif)` is the one door left. LOVE_NO_GLAZE=1
; clears the jit hook and keeps the module, so a pure-interpreter session still forges.
(:
   ; --- the ABI kit -------------------------------------------------------------------
   ; the lvm calling convention, as holo's abstract registers:
   ;   g=rdi(r6)  Ip=rsi(r5)  Hp=rdx(r2)  Sp=rcx(r1);  argument i arrives TAGGED on Sp[i]
   ; r0/r7/r8/r9/r10 are caller-saved and free; r3/r11-r14 are callee-saved and must be
   ; pushed. every guard jumps to the label `deopt`, which the epilogue below plants.
   ; ⚠ guards come BEFORE any push, so the machine sp is balanced on the deopt path too.
   forge-trayk (apof (iota 0))                      ; a tray's ap word, live in this process
   (forge-guard i k)                                ; Sp[i] must be an object of kind k
    [['ld 'r0 'r1 (8 * i)] '(test r0 1) '(br ne deopt)
      '(ld r7 r0 0) ['li 'r8 k] '(cmp r7 r8) '(br ne deopt)]
   (forge-fixg i)                                   ; Sp[i] must be a fixnum
    [['ld 'r0 'r1 (8 * i)] '(test r0 1) '(br eq deopt)]
   ; the two epilogues, both answering in Sp[0] and stepping Ip past the interp slot to lvm_ret
   ; (which pops the extra arguments). the kernel picks: `forge-epi` where r0 holds an UNTAGGED
   ; number, `forge-epi-ptr` where it holds a heap pointer -- an even pointer IS the value, and
   ; tagging one answers a fixnum somewhere in the middle of the heap.
   forge-epi '((shl r0 1) (or r0 r0 1) (st r1 0 r0) (add r5 r5 16) (ld r7 r5 0) (jmpr r7))
   forge-epi-ptr '((st r1 0 r0) (add r5 r5 16) (ld r7 r5 0) (jmpr r7))
   ; -- THE ROOM GUARD, and it is how a kernel allocates. a native body's registers are NOT GC
   ; roots (the collector walks g, Sp and Ip), so a collection fired mid-kernel leaves every
   ; pointer it is holding pointing into the old semispace. so a kernel never calls the
   ; collector: it proves the room FIRST -- Sp - Hp >= b bytes -- and DEOPTS to the twin when
   ; the room is not there, and the twin allocates the ordinary way with an ordinary `Have`.
   ; the output size has to be known before any Hp motion, which for a shaped result it is.
   ; ⚠ FIRST, with the type guards, ahead of any store: the deopt path must have touched
   ; nothing. ⚠ and `b` is the reservation PLUS slack, never the exact need -- love/glaze/emit.l
   ; spends 88 bytes on a 3-word cons for the same reason.
   ; ⚠ THE OTHER LANE IS REAL AND NOT TAKEN. `Have` does not return, it TAIL-CALLS lvm_gc, which
   ; packs, collects and Resumes at g->ip -- and in a nif cell Ip IS the cell, so a collection
   ; re-enters the kernel from the top and the second go finds room. A kernel whose guard comes
   ; first holds nothing across it, so that lane is as open to it as it is to the C nifs. It is
   ; not taken because reaching lvm_gc needs its LIVE ADDRESS -- this binary's own symtab and
   ; load bias, lib/splice.l's bind-live-tab, host-only machinery -- for a path that fires at
   ; most once a collection and lands in a twin that was going to be right anyway. Three
   ; instructions, every target, no self-inspection. Take the other lane for a kernel whose
   ; twin is expensive, not for one whose twin is the definition.
   (forge-room b) [['lea 'r7 'r2 b] '(cmp r1 r7) '(br below deopt)]
   ; the deopt tail: Ip <- the interp twin (Ip[1]), then enter it with the args still on Sp.
   ; arity 1 enters at its first word; arity>=2 skips the twin's own curry preamble.
   (forge-deopt ar)
    (? (ar < 2) '((label deopt) (ld r7 r5 8) (mov r5 r7) (ld r7 r5 0) (jmpr r7))
       '((label deopt) (ld r7 r5 8) (lea r5 r7 16) (ld r7 r5 0) (jmpr r7)))

   ; --- the roster --------------------------------------------------------------------
   forge-roll #()                                   ; (nom arity mkir twin) rows, in fit order
   forge-fits (tablet 0)                            ; nom -> the value in force
   forge-nat (tablet 0)                             ; nom -> 1 where that value is native
   forge-tally (tablet 0)                           ; decline reason -> count
   (forge-no k nom) (: _ (pin forge-tally [k nom] (1 + peep forge-tally [k nom] 0)) ())
   (forge-why _) (map (\ k (k >< peep forge-tally k 0)) (keys forge-tally))
   (forge-fitted? _) (tally (keys forge-nat))
   (forge-put nom arity mkir twin)
    (: _ (pin forge-roll () (peep forge-roll () () + [[nom arity mkir twin]]))
       _ (pin forge-fits nom twin) nom)
   (forge-of nom) (peep forge-fits nom ())

   ; --- the fit -----------------------------------------------------------------------
   ; ⚠ ONE DOOR, bound once: no glaze answers (), and (() 'nif) is 1, which `lit?` declines --
   ; so presence is the guard and a build without the module simply stays interpreted.
   forge-seam (from 'glaze 'nif)
   forge-arch (intern love-arch)
   forge-asm (from 'holo 'assemble)
   ; assemble one kernel -> its bytes | (). the DEOPT TAIL is the module's, because it is
   ; arity-determined and mechanical; the EPILOGUE is the kernel's, because only the kernel
   ; knows whether r0 ends up holding a number or a pointer.
   ; ⚠ TRAPPED, and it has to be: a kernel's IR is a PROGRAM, holo raises on an op it cannot
   ; lower ('bad-op) or a target with no backend ('no-backend), and an unguarded assemble takes
   ; the whole fit down -- every other kernel in the roster with it. the raise's own tag is
   ; what the census records, so a mis-said kernel names itself and its neighbours still fit.
   (forge-lay row)
    (: nom <row  ar <>row  mkir <>>row
       bs (trap (\ _ (peep (forge-asm forge-arch ((mkir 0) + forge-deopt ar)) 'bytes 0))
                () (\ a b (: _ (forge-no a nom) 'raised)))
       ; ⚠ the raise arm answers a NOM, not (): a decline must be counted ONCE, and a bare ()
       ; here reads as "assembled to nothing" and tallies the miss a second time under its name.
       (? (two? bs) bs  (id? bs 'raised) ()  (forge-no 'no-bytes nom)))
   ; the whole roster: LAY every kernel, then INSTALL every kernel. the two halves are
   ; separate on purpose -- see the batch note in the header.
   (forge-fit _)
    (? !(lit? forge-seam) (: _ (forge-no 'no-seam forge-arch) 0)
       (: rows (peep forge-roll () ())
          laid (map (\ r [r (forge-lay r)]) rows)
          (foldl (\ n lr
             (: r <lr  bs <>lr  nom <r  ar <>r  twin <>>>r
                (? !(two? bs) n
                   (: j (forge-seam (cask bs) twin ['\ 'q '(q)] ar)
                      (? !(lit? j) (: _ (forge-no 'declined nom) n)
                         (: _ (pin forge-fits nom j) _ (pin forge-nat nom 1) (n + 1)))))))
             0 laid)))

   ; =======================================================================================
   ; THE KERNELS
   ; =======================================================================================
   ; (aall v) -- the conjunction reduction over a tray: no zero element. the native says the
   ; ai_Z lane and nothing else; a scalar, a float/complex/object tray and a fixnum all deopt.
   ; tray layout: [0] ap, [8] type, [16] rank, [24] shape.., data at 24+rank*8.
   ; ⚠ the C nif re-reads the element type per element (love.c lvm_aall's `fdom ?` sits INSIDE
   ; the loop); specialised to one lane this is a load and a compare, and measures ~3x on a
   ; 4096-cell array. that gap is the argument for the whole file: the specialisation is what
   ; the IR can have and the general C cannot.
   (forge-aall-ir _)
    (forge-guard 0 forge-trayk
    + '((ld r7 r0 8) (cmp r7 0) (br ne deopt)                ; type must be ai_Z
        (ld r7 r0 16) (li r8 1) (li r10 0)                   ; rank; n=1; i=0
        (label pl) (cmp r10 r7) (br ge ple)                  ; n = product of the shape words
        (ldx r9 r0 r10 8 24) (imul r8 r8 r9) (add r10 r10 1) (jmp pl)
        (label ple) (leax r9 r0 r7 8 24) (li r10 0)          ; data = v + 24 + rank*8
        (label el) (cmp r10 r8) (br ge yes)
        (ldx r0 r9 r10 8 0) (cmp r0 0) (br eq no)
        (add r10 r10 1) (jmp el)
        (label no) (li r0 0) (jmp fin)
        (label yes) (li r0 1)
        (label fin))
    + forge-epi)
   _ (forge-put 'aall 1 forge-aall-ir aall))
