; scan -- find one byte in a text, natively where that is possible. love's own walk
; costs ~684 instructions a byte (a call, an index, a compare, a tail); the kernel
; below costs 1.876, which is 364x, and every tool that reads lines or matches a
; pattern spends most of its life in exactly this loop.
;
; the glaze does NOT reach this code: auto.l's lanes recognise integer-arithmetic
; leaves, counted loops, recursive arithmetic groups and float grids, so string work
; falls to the plain interpreter -- LOVE_NO_GLAZE moves `kore grep` by 0.02% while it
; moves a numeric loop by 4.5x. a hand-written kernel is the only way in.
;
; EIGHT BYTES A STEP: (w - ones) & ~w & highs is nonzero exactly when some byte of w
; is zero, so xor'ing the text with the needle in all eight lanes turns "is this byte
; the needle" into "is this byte zero". a hit inside a word falls to the byte loop,
; which is at most eight more steps.
;
; ⚠ BUILT ON FIRST USE, not at a module binding -- and the reason is the REGISTRY,
; not the bake. this module's text is evaluated inside the boot's own module registry
; (src/cats.c), which runs before the boot says (use 'holo): assembling there takes
; the whole registry down with it, and the symptom is a bare ";; no-module bao", the
; first module anyone asks for afterwards. so the assemble and the install both wait
; for a caller. one holo run and one arena blob per process, and never again.
;
; ⚠ AND THE BLOB NAMES NO ADDRESS OF THE BINARY. natives ride the image now, so this
; kernel can be carried into a bake and woken under another base -- which is sound
; only because the string-kind guard LOADS its sentinel from g's jk table rather than
; wearing an li of (apof ""). that is emit.l's `jk` law and it binds here too.
;
; ⚠ amd64 only for now, and not because the IR is x86: holo's IR is neutral. it is
; that the lvm ABI's register binding here (Sp=r1, Ip=r5) is the amd64 one, and
; arm64 lands Ip/Hp/Sp elsewhere (love/glaze/auto.l says x1/x2/x3). porting is a
; matter of the binding, not the instructions.
(module 'scan
(:
   ; the honest answer, and the differential oracle the laws hold the kernel to.
   ; it is also the ENGINE wherever the kernel cannot be built -- inle declines the
   ; install (its heap rides an NX window), and so does wasm
   (walk s c i)
    (: n #s
       (go k) (? (n <= k) -1 (s k = c ? k (go (k + 1))))
       (go (i < 0 ? 0 i)))

   ones 72340172838076673                        ; 0x0101010101010101
   ; highs is ones << 7 (0x8080808080808080), SHIFTED rather than written: as a
   ; literal it needs all 64 bits, and a tagged fixnum holds 62 -- so the spelled
   ; constant is a bignum, which is not what `li` wants
   ; Sp[0] the text, Sp[1] the charm, Sp[2] the start. r9 = the bytes, r10 = n,
   ; r7 = c, r8 = the cursor, r0 scratch; r11 = c in all eight lanes, r12 = ones,
   ; r13 = highs, r14 = ~w. the guards deopt BEFORE any push, so the machine sp is
   ; balanced on every way out
   (scan-ir _)
    (: ([['ld 'r0 'r1 0] '(test r0 1) '(br ne deopt)             ; Sp[0] a pointer ...
          '(ld r7 r0 0)
          ; the string kind off g's jk table (slot 1), never an li of (apof ""):
          ; that is a C code address, and a blob that names one is not the same
          ; bytes under another base. emit.l's `jk` law, and the same reason
          ['ld 'r8 'r6 ((jkoff 0) + 8)]
          '(cmp r7 r8) '(br ne deopt)]                                   ; ... of string kind
        + [['ld 'r0 'r1 8] '(test r0 1) '(br eq deopt)]           ; Sp[1] a charm
        + [['ld 'r0 'r1 16] '(test r0 1) '(br eq deopt)]          ; Sp[2] a charm
        + '((ld r9 r1 0) (ld r10 r9 8) (lea r9 r9 16)             ; the bytes, and n
               (ld r7 r1 8) (sar r7 1)                               ; c, untagged
               (ld r8 r1 16) (sar r8 1)                              ; the start, untagged
               (cmp r8 0) (br lt deopt)                              ; a negative start is the twin's
               (cmp r7 0) (br lt deopt)
               (cmp r7 255) (br gt deopt))                           ; a charm past a byte: the twin's
        + '((push r11) (push r12) (push r13) (push r14))
        + [['li 'r12 ones]] + '((mov r13 r12) (shl r13 7))
        + '((mov r11 r7)                                          ; the needle into all eight lanes,
               (mov r0 r11) (shl r0 8) (or r11 r11 r0)                ; by doubling: 1 -> 2 -> 4 -> 8
               (mov r0 r11) (shl r0 16) (or r11 r11 r0)
               (mov r0 r11) (shl r0 32) (or r11 r11 r0))
        + '((label al)                                            ; up to the first 8-boundary
               (cmp r8 r10) (br ge nf)
               (mov r0 r8) (and r0 r0 7) (cmp r0 0) (br eq wl)
               (ldxb r0 r9 r8 1 0) (cmp r0 r7) (br eq hit)
               (add r8 r8 1) (jmp al))
        + '((label wl)                                            ; eight at a time
               (mov r0 r8) (add r0 r0 8) (cmp r0 r10) (br gt tl)
               (ldx r0 r9 r8 1 0) (xor r0 r0 r11)
               (mov r14 r0) (not r14) (sub r0 r0 r12)
               (and r0 r0 r14) (and r0 r0 r13)
               (cmp r0 0) (br ne tl)                                 ; the needle is in this word
               (add r8 r8 8) (jmp wl))
        + '((label tl)                                            ; the tail, and the word that hit
               (cmp r8 r10) (br ge nf)
               (ldxb r0 r9 r8 1 0) (cmp r0 r7) (br eq hit)
               (add r8 r8 1) (jmp tl))
        + '((label hit) (mov r0 r8) (jmp fin)
               (label nf) (li r0 -1)
               (label fin) (pop r14) (pop r13) (pop r12) (pop r11))
        + '((shl r0 1) (or r0 r0 1) (st r1 0 r0)                  ; tag, land on Sp[0], step Ip
               (add r5 r5 16) (ld r7 r5 0) (jmpr r7))
        + '((label deopt) (ld r7 r5 8) (lea r5 r7 16)             ; arity >= 2: Ip <- the twin
               (ld r7 r5 0) (jmpr r7))))

   ; ⚠ NOTHING HERE RUNS AT REGISTRATION. this module's text is evaluated inside the
   ; boot's own module registry (src/cats.c, ai_cat_mods), which is before the boot
   ; says (use 'holo) -- assembling there takes the whole registry down with it, and
   ; the symptom is a bare ";; no-module bao", the first module anyone asks for after.
   ; so the assemble and the install both wait for a caller
   (mk _) (: h (from 'holo) g (from 'glaze)
             (? (nil? h || nil? g || !(id? love-arch 'amd64)) walk
                (: bs (peep ((h 'assemble) 'amd64 (scan-ir ())) 'bytes 0)
                   ((g 'nif) (cask bs) walk '(\ q (q)) 3))))
   box (tablet 0)                                ; one install for the process, not one per ask
   ; (scanner ()) -> (f s c i), answering the index of charm c at-or-after i, or -1.
   ; a caller with many texts asks ONCE and holds f: this is a tablet read, which is
   ; ~1000 instructions, and belongs outside any loop
   (scanner _) (: k (peep box 'k 'none)
                  (k !== 'none ? k (: k2 (mk ()) _ (pin box 'k k2) k2)))
)

) ; module 'scan
