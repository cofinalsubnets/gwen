; lib/forge.l -- nifs written in love. a kernel is a crew/holo IR program plus an honest
; interpreted twin. forge assembles the program for the cpu it is running on and installs it
; through the `nif` seam; the twin is what runs everywhere the native cannot. `(use "lib/forge.l")`:
;
;   (forge-put nom arity mkir twin) -> register one kernel; mkir is a thunk, twin the fallback
;   (forge-fit ())                  -> assemble + install the whole roster -> the fitted count
;   (forge-of nom)                  -> the installed value: native if it built, the twin if not
;   (forge-why ())                  -> ((reason nom ..) . count) rows -- why a decline declined
;   (forge-fitted? ())              -> how many kernels are running native right now
;
; what this buys, controlled. `love` is mooncc-built and `love0` is gcc-built, so running a
; kernel under both separates "faster than C" from "faster than our C". ms, same box:
;
;                        C (mooncc)   holo    C (gcc)      what it says
;   aall  n=4096 x2000        14        5         3        gcc wins; the 2.8x is mooncc's gap
;   outer 200x200 x2000      151       71        79        a real ~10% over gcc, 2.1x over ours
;   inner 120x120 x40        168       37        28        a tie with gcc, 4.5x over ours
;   (sort  n=4000 x200        34       29        24        dropped: a tie with gcc for the most
;                                                          IR of the four -- 1.2x is not worth
;                                                          65 lines of hand-written x86-64)
;
; so the kernels are about gcc-quality machine code and not better than C. against the binary
; the tree ships they are 2-4.5x, but most of that is a mooncc loop-codegen gap -- it recomputes
; i*N+j per element where these walk a pointer, and never gets the induction variables out of
; their stack slots -- and closing that pays in every loop in love.c rather than in three. say
; the controlled number or say nothing: "3x the C nif" was true of one compiler and false of C.
;
; so the rule for a new kernel is not "is it hot", and the three below meet neither reason that
; holds up. one: does it hold a constant known only at run time? crew/sat/flat.l bakes the
; instance's nvars in as immediates and rebuilds per solver, which C cannot do at any -O. two:
; is the twin interpreted love rather than C? then the kernel is not racing a compiler at all,
; and what it buys is vocabulary -- lib/gz.l's match finder compares bytes where `(s i)` is a VM
; apply, and no improvement to mooncc hands love code an instruction the VM has no name for.
; that one is 2.7x on the whole of `gz-deflate` and is the shape to copy. every kernel below is
; a fixed program with a C twin, which is why each merely draws with gcc. they stay because they
; pay on the binary the tree ships, and they are the first thing to delete the day the residency
; lever lands: a hedge, not an asset. forge is a library and not this roster -- lib/gz.l registers
; its kernel from its own module through `forge-putx`, which is the arrangement to reach for.
;
; the twin is the definition and the native is an optimisation, never the other way round. every
; guard in a kernel deopts into the twin, so a shape the IR does not say is not a bug and not a
; raise -- it is the twin's turn. that is what makes a kernel writable at all: it says the one
; monomorphic lane it is fast at and hands back everything else. that is also why forging only
; where the ABI kit has a row costs nothing but speed: amd64 forges, every other target runs the
; twins and `forge-why` says `no-abi`.
;
; mkir is a thunk because a kernel's IR carries live addresses: `forge-trayk` and friends are
; read out of this process with `apof`, and under aslr they are a different number every run. so
; the IR is built at fit time, never earlier, and never kept.
;
; a native cell does not survive a bake, and it does not say so. the image codec reverts it to
; its twin (src/love.c img_nif_interp) because the w^x mapping dies with the baking process, so
; the bake succeeds, the value wakes, it answers correctly -- and it is the twin. measured on the
; 4096-cell battery: 4 ms native, 13 ms after a bake and a wake, which is the C nif's own 13.
; what bakes is the roster, thunks and twins being ordinary love data, so a woken image is correct
; and slow until someone calls `forge-fit` again -- ~0.1 ms per kernel.
;
; the fit is a batch, every kernel assembling before any installs, because `nif` mmaps a
; page-rounded arena per call (src/love.c code_maplen), so a roster of n costs n pages and n
; vmas today. one arena for the batch is a change to that seam, and this shape is what feeds it.
;
; it needs the glaze module, not the glaze hook: the seal pulls `nif`/`nifx` off the book on
; every boot (src/main.c) and `(from 'glaze 'nif)` is the one door left. LOVE_NO_GLAZE=1 clears
; the jit hook and keeps the module, so a pure-interpreter session still forges.
(:
   ; --- the ABI kit -------------------------------------------------------------------
   ; the lvm calling convention, as holo's abstract registers:
   ;   g=rdi(r6)  Ip=rsi(r5)  Hp=rdx(r2)  Sp=rcx(r1);  argument i arrives tagged on Sp[i]
   ; r0/r7/r8/r9/r10 are caller-saved and free; r3/r11-r14 are callee-saved and must be
   ; pushed. every guard jumps to the label `deopt`, which the epilogue below plants.
   ; guards come before any push, so the machine sp is balanced on the deopt path too.
   ;
   ; that binding is amd64'S, and it is spelled, not derived. the lvm ABI is each
   ; platform's own C convention, so the four roles land in different registers per
   ; target -- r1 is Sp here and Ip on arm64, and only Hp coincides. every guard,
   ; epilogue and deopt tail below names them by number, and holo lowers all of it on
   ; any backend, so an off-x64 assemble succeeds and installs a kernel reading Ip
   ; where it means Sp. what the roles need is a row per target -- love/scan.l has one
   ; -- and until they have it `forge-abi?` is what keeps the twins in force.
   forge-trayk (apof (iota 0))                      ; a tray's ap word, live in this process
   forge-chaink (apof '(1 2))                       ; a chain's
   (forge-guard i k)                                ; Sp[i] must be an object of kind k
    [['ld 'r0 'r1 (8 * i)] '(test r0 1) '(br ne deopt)
      '(ld r7 r0 0) ['li 'r8 k] '(cmp r7 r8) '(br ne deopt)]
   (forge-fixg i)                                   ; Sp[i] must be a fixnum
    [['ld 'r0 'r1 (8 * i)] '(test r0 1) '(br eq deopt)]
   ; the two epilogues, both answering in Sp[0] and stepping Ip past the interp slot to lvm_ret
   ; (which pops the extra arguments). the kernel picks: `forge-epi` where r0 holds an untagged
   ; number, `forge-epi-ptr` where it holds a heap pointer -- an even pointer is the value, and
   ; tagging one answers a fixnum somewhere in the middle of the heap.
   forge-epi '((shl r0 1) (or r0 r0 1) (st r1 0 r0) (add r5 r5 16) (ld r7 r5 0) (jmpr r7))
   forge-epi-ptr '((st r1 0 r0) (add r5 r5 16) (ld r7 r5 0) (jmpr r7))
   ; -- the room guard, and it is how a kernel allocates. a native body's registers are not GC
   ; roots (the collector walks g, Sp and Ip), so a collection fired mid-kernel leaves every
   ; pointer it is holding pointing into the old semispace. so a kernel never calls the
   ; collector: it proves the room first -- Sp - Hp >= b bytes -- and deopts to the twin when
   ; the room is not there, and the twin allocates the ordinary way with an ordinary `Have`.
   ; the output size has to be known before any Hp motion, which for a shaped result it is.
   ; the proof comes first, with the type guards and ahead of any store, so the deopt path has
   ; touched nothing; and `b` is the reservation plus slack, never the exact need.
   ; the other lane is real and not taken: `Have` tail-calls lvm_gc, which resumes at g->ip, and
   ; in a nif cell Ip is the cell, so a collection re-enters the kernel and the second go finds
   ; room. reaching lvm_gc needs its live address -- this binary's symtab and load bias, host-only
   ; machinery -- for a path landing in a twin that was going to be right anyway. take it for a
   ; kernel whose twin is expensive, not for one whose twin is the definition.
   (forge-room b) [['lea 'r7 'r2 b] '(cmp r1 r7) '(br below deopt)]
   ; the same guard where the size is only known at run time -- `r` holds the byte count and
   ; `slack` rides on top. a shaped result knows its size before it allocates, which is the
   ; whole reason an allocating kernel is writable: love.c's own lvm_inner computes its `bytes`
   ; before its `Have` too. it jumps to `deoptp`, the unwinding deopt: a kernel big enough to
   ; need a run-time size is big enough to have pushed the callee-saved file first.
   (forge-roomr r slack) [['leax 'r7 'r2 r 1 slack] '(cmp r1 r7) '(br below deoptp)]
   ; -- the callee-saved file. five registers, which is what takes a kernel from the five
   ; caller-saved scratch (r0/r7/r8/r9/r10) to a working set a nested loop fits in.
   ; save only after the type guards: those deopt to the bare `deopt` with the machine stack
   ; as the caller left it. everything after a save deopts through `forge-unwind` instead, which
   ; pops the file back and falls into the same tail -- an unbalanced stack is not a wrong
   ; answer, it is a return into the middle of somebody else's frame.
   forge-save '((push r3) (push r11) (push r12) (push r13) (push r14))
   forge-restore '((pop r14) (pop r13) (pop r12) (pop r11) (pop r3))
   forge-unwind ('(label deoptp) . (forge-restore + '((jmp deopt))))
   ; -- one love value handed to a kernel, through `nifx`'s extras slot: it rides in the cell
   ; at Ip+32 (love.c lays it at the same offset for arity 1 and arity>=2), is GC-walked, and
   ; is image-encoded -- so unlike an `li` immediate it is a heap reference the collector knows
   ; about. that is the door for a constant a kernel cannot spell: the zero point terminator a
   ; list-building kernel needs is `&ai_mint_zero`, and no book name hands its address out.
   ; the glaze uses the same slot for a closure (love/glaze/emit.l).
   (forge-extras d) [['ld d 'r5 32]]
   ; the deopt tail: Ip <- the interp twin (Ip[1]), then enter it with the args still on Sp.
   ; arity 1 enters at its first word; arity>=2 skips the twin's own curry preamble.
   (forge-deopt ar)
    (? (ar < 2) '((label deopt) (ld r7 r5 8) (mov r5 r7) (ld r7 r5 0) (jmpr r7))
       '((label deopt) (ld r7 r5 8) (lea r5 r7 16) (ld r7 r5 0) (jmpr r7)))

   ; --- the roster --------------------------------------------------------------------
   forge-roll {}                                    ; (nom arity mkir twin) rows, in fit order
   forge-fits (tablet 0)                            ; nom -> the value in force
   forge-nat (tablet 0)                             ; nom -> 1 where that value is native
   forge-tally (tablet 0)                           ; decline reason -> count
   (forge-no k nom) (: _ (pin forge-tally [k nom] (1 + peep forge-tally [k nom] 0)) ())
   (forge-why _) (map (k \ (k . peep forge-tally k 0)) (keys forge-tally))
   (forge-fitted? _) #(keys forge-nat)
   ; a row is (nom arity mkir twin extras). `extras` is the value the kernel reads at Ip+32,
   ; or 'none where it wants none -- the marker is a nom and never (), which is a legal value
   ; to hand a kernel and would install through the wrong seam if it read as absence.
   (forge-put nom arity mkir twin) (forge-putx nom arity mkir twin 'none)
   (forge-putx nom arity mkir twin extras)
    (: _ (pin forge-roll () (peep forge-roll () () + [[nom arity mkir twin extras]]))
       _ (pin forge-fits nom twin) nom)
   (forge-of nom) (forge-fits nom)

   ; --- the fit -----------------------------------------------------------------------
   ; one door, bound once: a missing module answers (), so `nil?` on the module is the
   ; presence test and a build without the glaze simply stays interpreted.
   forge-glaze (from 'glaze)
   forge-seam (from 'glaze 'nif)
   forge-seamx (from 'glaze 'nifx)                  ; the same door, one extras slot wider
   forge-arch love-arch
   forge-abi? (id? forge-arch 'amd64)               ; the targets the kit above spells
   forge-asm (from 'holo 'assemble)
   ; assemble one kernel -> its bytes | (). the deopt tail is the module's, because it is
   ; arity-determined and mechanical; the epilogue is the kernel's, because only the kernel
   ; knows whether r0 ends up holding a number or a pointer.
   ; trapped, and it has to be: a kernel's IR is a program, holo raises on an op it cannot
   ; lower ('bad-op) or a target with no backend ('no-backend), and an unguarded assemble takes
   ; the whole fit down -- every other kernel in the roster with it. the raise's own tag is
   ; what the census records, so a mis-said kernel names itself and its neighbours still fit.
   (forge-lay row)
    (: nom <row  ar <>row  mkir <>>row
       bs (trap (\ _ (peep (forge-asm forge-arch ((mkir 0) + forge-deopt ar)) 'bytes 0))
                () (\ a b (: _ (forge-no a nom) 'raised)))
       ; the raise arm answers a nom, not (): a decline must be counted once, and a bare ()
       ; here reads as "assembled to nothing" and tallies the miss a second time under its name.
       (? (two? bs) bs  (id? bs 'raised) ()  (forge-no 'no-bytes nom)))
   ; the whole roster: lay every kernel, then install every kernel. the two halves are
   ; separate on purpose -- see the batch note in the header.
   ; a decline is not a failure and never a raise: the twin is the definition, so a
   ; missing glaze and a target with no ABI row both leave the roster interpreted and
   ; say so by name in `forge-why`.
   (forge-fit _)
    (? (nil? forge-glaze) (: _ (forge-no 'no-seam forge-arch) 0)
       !forge-abi? (: _ (forge-no 'no-abi forge-arch) 0)
       (: rows (forge-roll ())
          laid (map (r \ [r (forge-lay r)]) rows)
          (foldl (\ n lr
             (: r <lr  bs <>lr  nom <r  ar <>r  twin <>>>r  xt <>>>>r
                (? !(two? bs) n
                   (: j (? (id? xt 'none) (forge-seam (cask bs) twin ['\ 'q '(q)] ar)
                           (forge-seamx (cask bs) twin ['\ 'q '(q)] ar xt))
                      (? !(lit? j) (: _ (forge-no 'declined nom) n)
                         (: _ (pin forge-fits nom j) _ (pin forge-nat nom 1) (n + 1)))))))
             0 laid)))

   ; =======================================================================================
   ; the kernels
   ; =======================================================================================
   ; (aall v) -- the conjunction reduction over a tray: no zero element. the native says the
   ; ai_Z lane and nothing else; a scalar, a float/complex/object tray and a fixnum all deopt.
   ; tray layout: [0] ap, [8] type, [16] rank, [24] shape.., data at 24+rank*8.
   ; the C nif re-reads the element type per element (love.c lvm_aall's `fdom ?` sits inside
   ; the loop); specialised to one lane this is a load and a compare. 2.8x against the shipping
   ; compiler -- and slower than gcc's build of the same C, which hoists that ternary out of the
   ; loop by itself. the header's table is the whole claim; this row is the weakest of the three.
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
   _ (forge-put 'aall 1 forge-aall-ir aall)

   ; (outer a b) -- the outer product over two ai_Z trays: rank ra+rb, M*N cells, written
   ; in one sequential sweep (j fastest) so the destination pointer only ever walks.
   ; regs: r3=M r11=a-data r12=b-data r13=dest r14=N, r0/r7/r8/r9/r10 scratch.
   ; the size is known before a byte is allocated -- M, N and the rank come off the two
   ; shapes -- which is the whole reason an allocating kernel is writable: `forge-roomr`
   ; proves the room and the twin takes the near-full heap. 2.1x the shipping nif at 200x200,
   ; and ~10% over gcc's build of the same C, which is the only row here that beats a real cc.
   (forge-outer-ir _)
    ([ '(ld r0 r1 0) '(test r0 1) '(br ne deopt)
       ['li 'r8 forge-trayk] '(ld r7 r0 0) '(cmp r7 r8) '(br ne deopt)
       '(ld r7 r0 8) '(cmp r7 0) '(br ne deopt)                  ; a must be ai_Z
       '(ld r9 r1 8) '(test r9 1) '(br ne deopt)
       '(ld r7 r9 0) '(cmp r7 r8) '(br ne deopt)
       '(ld r7 r9 8) '(cmp r7 0) '(br ne deopt)                  ; b must be ai_Z
       '(ld r7 r0 16) '(ld r8 r9 16)                             ; ra, rb
       '(mov r10 r7) '(add r10 r10 r8) '(cmp r10 8) '(br gt deopt)   ; rank <= maxrank
       '(cmp r7 1) '(br lt deopt) '(cmp r8 1) '(br lt deopt)]
     + (forge-save
     + [ '(mov r11 r0) '(mov r12 r9)
         '(li r3 1) '(li r0 0)                                   ; M = product of a's shape
         '(label ml) '(cmp r0 r7) '(br ge mle)
         '(ldx r8 r11 r0 8 24) '(imul r3 r3 r8) '(add r0 r0 1) '(jmp ml)
         '(label mle)
         '(ld r8 r12 16) '(li r14 1) '(li r0 0)                  ; N = product of b's shape
         '(label nl) '(cmp r0 r8) '(br ge nle)
         '(ldx r9 r12 r0 8 24) '(imul r14 r14 r9) '(add r0 r0 1) '(jmp nl)
         '(label nle)
         '(mov r9 r3) '(imul r9 r9 r14)                          ; n = M*N
         '(mov r0 r10) '(shl r0 3) '(add r0 r0 24)               ; bytes = 24 + rank*8 ..
         '(mov r7 r9) '(shl r7 3) '(add r0 r0 r7)]               ;       .. + 8*n
     + (forge-roomr 'r0 96
     + [ '(mov r13 r2) '(add r2 r2 r0) '(push r13)               ; result = Hp ; Hp += bytes
         ['li 'r7 forge-trayk] '(st r13 0 r7)
         '(li r7 0) '(st r13 8 r7) '(st r13 16 r10)              ; ap, type ai_Z, rank
         '(ld r8 r11 16) '(li r7 0)                              ; the shape: a's words ..
         '(label sa) '(cmp r7 r8) '(br ge sae)
         '(ldx r0 r11 r7 8 24) '(stx r13 r7 8 24 r0) '(add r7 r7 1) '(jmp sa)
         '(label sae)
         '(ld r9 r12 16) '(mov r10 r8) '(li r7 0)                ;            .. then b's
         '(label sb) '(cmp r7 r9) '(br ge sbe)
         '(ldx r0 r12 r7 8 24) '(stx r13 r10 8 24 r0)
         '(add r7 r7 1) '(add r10 r10 1) '(jmp sb)
         '(label sbe)
         '(leax r11 r11 r8 8 24) '(leax r12 r12 r9 8 24)         ; the three data pointers
         '(ld r7 r13 16) '(leax r13 r13 r7 8 24)
         '(li r9 0)
         '(label il) '(cmp r9 r3) '(br ge ile)
         '(ldx r0 r11 r9 8 0)                                    ; av = a[i]
         '(li r7 0)
         '(label jl) '(cmp r7 r14) '(br ge jle)
         '(ldx r8 r12 r7 8 0) '(imul r8 r8 r0) '(st r13 0 r8) '(add r13 r13 8)
         '(add r7 r7 1) '(jmp jl)
         '(label jle)
         '(add r9 r9 1) '(jmp il)
         '(label ile) '(pop r0)]
     + (forge-restore + (forge-epi-ptr + forge-unwind)))))
   _ (forge-put 'outer 2 forge-outer-ir outer)

   ; (inner a b) -- +.x over two ai_Z trays: contract a's last axis with b's first (1D.1D a
   ; dot, 2D.2D a matmul). ikj order, the accumulator in memory, and the addressing carried
   ; in walking pointers -- Arow by K*8, Brow by N*8, Crow by N*8 -- where the C recomputes
   ; i*N+j and l*N+j per element. that is the whole difference and it is worth 4.5x against
   ; the shipping compiler (see the header's table).
   ; regs: r11=Arow r13=Crow r14=N r3=K, r0/r7/r8/r9/r10/r12 scratch;
   ; [sp+0]=Cend [sp+8]=B data [sp+16]=the result.
   ; the shape guards sit after `forge-save` and deopt through `deoptp`: there are ten
   ; values to check and five caller-saved registers to check them in. only the type guards,
   ; which run before any push, can use the bare `deopt`.
   ; n<=1 hands back to the twin: the C answers a boxed scalar there, a different object
   ; and a different allocation, and a kernel that says one shape says one shape.
   (forge-inner-ir _)
    ([ '(ld r0 r1 0) '(test r0 1) '(br ne deopt)
       ['li 'r8 forge-trayk] '(ld r7 r0 0) '(cmp r7 r8) '(br ne deopt)
       '(ld r7 r0 8) '(cmp r7 0) '(br ne deopt)
       '(ld r9 r1 8) '(test r9 1) '(br ne deopt)
       '(ld r7 r9 0) '(cmp r7 r8) '(br ne deopt)
       '(ld r7 r9 8) '(cmp r7 0) '(br ne deopt)]
     + (forge-save
     + [ '(mov r11 r0) '(mov r12 r9)
         '(ld r7 r11 16) '(cmp r7 1) '(br lt deoptp)
         '(ld r8 r12 16) '(cmp r8 1) '(br lt deoptp)
         '(mov r10 r7) '(sub r10 r10 1) '(ldx r3 r11 r10 8 24)   ; K = a.shape[ra-1]
         '(ld r9 r12 24) '(cmp r3 r9) '(br ne deoptp)            ; must equal b.shape[0]
         '(li r13 1) '(li r0 0)                                  ; M = prod a.shape[0..ra-2]
         '(label ml) '(mov r9 r7) '(sub r9 r9 1) '(cmp r0 r9) '(br ge mle)
         '(ldx r10 r11 r0 8 24) '(imul r13 r13 r10) '(add r0 r0 1) '(jmp ml)
         '(label mle)
         '(li r14 1) '(li r0 1)                                  ; N = prod b.shape[1..rb-1]
         '(label nl) '(cmp r0 r8) '(br ge nle)
         '(ldx r10 r12 r0 8 24) '(imul r14 r14 r10) '(add r0 r0 1) '(jmp nl)
         '(label nle)
         '(mov r9 r7) '(add r9 r9 r8) '(sub r9 r9 2)             ; rank = (ra-1)+(rb-1)
         '(cmp r9 8) '(br gt deoptp)
         '(mov r10 r13) '(imul r10 r10 r14) '(cmp r10 1) '(br le deoptp)   ; n; n<=1 -> the twin
         '(mov r0 r9) '(shl r0 3) '(add r0 r0 24)
         '(mov r8 r10) '(shl r8 3) '(add r0 r0 r8)               ; bytes
         '(leax r7 r2 r0 1 96) '(cmp r1 r7) '(br below deoptp)
         '(mov r7 r2) '(add r2 r2 r0) '(push r7)
         ['li 'r0 forge-trayk] '(st r7 0 r0)
         '(li r0 0) '(st r7 8 r0) '(st r7 16 r9)
         '(ld r8 r11 16) '(sub r8 r8 1) '(li r0 0)               ; shape: a's first ra-1 ..
         '(label sa) '(cmp r0 r8) '(br ge sae)
         '(ldx r10 r11 r0 8 24) '(stx r7 r0 8 24 r10) '(add r0 r0 1) '(jmp sa)
         '(label sae)
         '(mov r10 r8) '(ld r8 r12 16) '(li r0 1)                ;      .. then b's last rb-1
         '(label sb) '(cmp r0 r8) '(br ge sbe)
         '(ldx r9 r12 r0 8 24) '(stx r7 r10 8 24 r9) '(add r0 r0 1) '(add r10 r10 1) '(jmp sb)
         '(label sbe)
         '(ld r0 r11 16) '(leax r11 r11 r0 8 24)
         '(ld r0 r12 16) '(leax r12 r12 r0 8 24)
         '(ld r0 r7 16) '(leax r9 r7 r0 8 24)                    ; C data
         '(mov r0 r13) '(imul r0 r0 r14) '(shl r0 3) '(mov r8 r9) '(add r8 r8 r0)   ; Cend
         '(mov r10 r9)                                           ; the payload starts as zero
         '(label zl) '(cmp r10 r8) '(br ge zle)
         '(li r0 0) '(st r10 0 r0) '(add r10 r10 8) '(jmp zl)
         '(label zle)
         '(push r12) '(push r8) '(mov r13 r9)
         '(label il)
         '(ld r12 sp 0) '(cmp r13 r12) '(br ge ile)
         '(ld r9 sp 8) '(li r7 0)
         '(label ll) '(cmp r7 r3) '(br ge lle)
         '(ldx r0 r11 r7 8 0) '(li r8 0)
         '(label jl) '(cmp r8 r14) '(br ge jle)
         '(ldx r10 r9 r8 8 0) '(imul r10 r10 r0)
         '(ldx r12 r13 r8 8 0) '(add r12 r12 r10) '(stx r13 r8 8 0 r12)
         '(add r8 r8 1) '(jmp jl)
         '(label jle)
         '(mov r10 r14) '(shl r10 3) '(add r9 r9 r10)
         '(add r7 r7 1) '(jmp ll)
         '(label lle)
         '(mov r10 r3) '(shl r10 3) '(add r11 r11 r10)
         '(mov r10 r14) '(shl r10 3) '(add r13 r13 r10)
         '(jmp il)
         '(label ile) '(pop r0) '(pop r0) '(pop r0)]
     + (forge-restore + (forge-epi-ptr + forge-unwind))))
   _ (forge-put 'inner 2 forge-inner-ir inner))
