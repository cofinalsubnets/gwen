; emit -- the codegen backend: a love-level x86-64 emitter that compiles a love
; closure to NATIVE machine code and installs it as a TRANSPARENT applicable value.
; This is "ev but faster": the result applies by juxtaposition ((f x), no verb), is
; =/show IDENTICAL to its source, and DEOPTS to the interpreter on overflow -- so
; native is never wrong, only faster. The C seam is `nat` (love.c): it W^X-installs
; the bytes and wraps them in a closure cell [code, src, code, interp, lvm_ret, 0]
; whose value[-1]=src (=/show) and value[1]=interp (the deopt fallback).
;
; ⚠ x86-64 ONLY (emits x86-64; load with -l on an x86-64 host). WIP toward the auto
; hook (ev installing native for hot regions, transparently); `nat` then goes
; internal (mopped like boxfix/feel) and there is no user-facing verb at all.
;
; grammar: (\ x E), E := x | int | (E +/-/* E) | (cmp E E) | (? E E E), cmp in
; < <= > >= = (-> 0/1 like the interpreter); ? is strictly-positive-test then/else.
; lvm ABI: g=rdi Ip=rsi Hp=rdx Sp=rcx. The body accumulates UNTAGGED in rax, the
; machine stack (push/pop) holds nested operands, r8 the popped temp; the loop uses
; r9=n r10=i r11=acc. Every +/-/* and the final putfix carry a `jno` over an inline
; deopt (mov rax,[rsi+8](=interp); mov rsi,rax; mov rax,[rsi]; jmp rax) -- a tail
; apply of interp to the original arg (still in Sp[0]). putfix is add rax,rax (its OF
; flag is exactly the 62-bit fixnum boundary); jno; or rax,1.

; nif/nifx -- the C install seam (love.c), taken into the module as members. The book loses
; them on EVERY boot now (src/main.c), so this is the one door left onto raw byte
; installation, and it is the glaze's: the glaze is the only thing that installs machine code.
; ⚠ the RHS is the OUTER nif, by the sequence law -- a body-less `:` rebinds, it does not letrec.
(: nif nif  nifx nifx)

(: ; the glaze consumes the crew/holo/ assembler by BARE NAME -- src/main.c brackets this whole
   ; load with (use 'holo) .. ai_unsplice_, so assemble/assemble-at resolve through the spliced
   ; module at compile and FOLD (the capture law): every glaze closure is self-contained after
   ; the unsplice. (the byte-piece concat helper is `bcat` -- it must NEVER be named `holo`: this :
   ; is body-less, so its bindings leak to the `glaze` module's own book, and a `holo` binding
   ; there once clobbered the assembler.)
   (le8 n) (: u (? (n < 0) (n + 18446744073709551616) n)                            ; two's-complement; & is broken on
              ((: (go i) (? (i < 8) ((u // (i 256)) % 256 >< go (i + 1)) ())) 0))   ; a bignum >= 2^63, so use //% (n -> 8 LE bytes)
   (le4 n) ((: (go i) (? (i < 4) ((n >> (8 * i)) & 255 >< go (i + 1)) ())) 0)  ; n -> 4 LE bytes (offsets < 2^31, & ok)
   ; the raw-byte x86 lane is fully RETIRED: every codegen helper now emits neutral crew/holo/ IR (cgir/cgvir for
   ; arith, the float lanes cgf/jitfr, the grid kernels gridn/grid2), assembled per target. The old byte
   ; blobs (deopt/guard/guardz/fetch/untag/fxv/epilogue) are gone -- their IR twins are deoptbody/ldeopt/
   ; zdeopt/fxvir/loopepi, which serve BOTH x86 and arm64.
   (emit l) (cask l)                       ; the charlist lane fills in C -- this was a pin per byte
   ; (jit '(\ x E)) -- compile an arithmetic single-arg closure to native.
   (jit lam) (jitir lam 'amd64)  ; arity-1 leaf -> the neutral-IR lane (cgir)
   ; argix finds a param var's frame index in ps (shared with the IR lane's fxir/cgir).
   (argix e ps i) (? !(two? ps) 0  (e = <ps) i  (argix e >ps (i + 1)))   ; param -> index
   ; (jitn '(\ p0 .. body)) -- an arity>=2 leaf -> native (natn curries to saturation). Now an ALIAS for
   ; the neutral-IR lane (jitnir): cgir already serves the n-ary case in IR, so the old byte cgn/-mn
   ; family below is retired (deleted) -- one lane, label-resolved, and arm64 falls out for free.
   (jitn lam) (jitnir lam 'amd64)
   (jit2 lam) (jitn lam)                                                  ; arity-2 alias (self-test/auto.l)
   (gfind p l) (? (atom? l) () (p <l) <l (gfind p >l))     ; first elt matching p
   ; a blob names no address of the binary: the kind sentinels its guards compare against and the
   ; callout drives are read off g at run time (love_int.h's JkX slots, in this order), so the bytes
   ; are the same under any base and ride an image as they are
   (jk tgt r k) ['ld r (greg tgt 'g) ((jkoff 0) + (8 * k))]   ; 0 chain 1 str 2 map 3 nom 4 mint 5 gem 6 cask 7 drive 8 resume 9 cur
   mapmix 0x9e3779b97f4a7c15                                                        ; the hash multiplier (= love.c's map hash mult), for the inline probe
   ; -- the map probe, via the crew/holo/ assembler SERVICE (baked into the post-egg layer; the glaze is its
   ;    client). crew/holo/ resolves the hit/miss/loop labels to rel32 -- no hand-counted je/jmp offsets, the
   ;    fragile bit. reg map: holo r0=rax, r7=x86-r8, r8=x86-r9, r9=x86-r10, r10=x86-r11. The probe wants
   ;    the MAP in rax and the TAGGED key in r11 (holo r10); it ends branched to `hit` (match) / `miss` (gap).
   (asmx ir) (peep (assemble 'amd64 ir) 'bytes 0)                                     ; assemble neutral IR -> flat bytes, X86 (the post-egg `assemble` service, bound off the holo book above)
   (asmt tgt ir) (peep (assemble tgt ir) 'bytes 0)                                  ; ... target-aware (the loop/float lanes that run on both arches)
   mprobe-ir ['(ld r7 r0 8) '(ld r8 r7 16) '(shr r8 1) '(dec r8) '(add r7 r7 24)    ; r8=backing; r9=cap; >>1; --(=mask); r8=&slots
      ['li 'r0 mapmix] '(imul r0 r0 r10) '(rol r0 32) '(and r0 r0 r8) '(mov r9 r0)    ; rax=rot(mix*tagkey)&mask=I -> r10
      '(label loop) '(leax r0 r9 r9 1 0) '(ldx r0 r7 r0 8 0) '(cmp r0 r10) '(br eq hit)  ; slot[I].key == tagkey? -> hit
      '(test r0 1) '(br eq miss) '(inc r9) '(and r9 r9 r8) '(jmp loop)]                  ; gap (even) -> miss; else I=(I+1)&mask, loop
   (mpeep-ir d) (mprobe-ir                                                          ; READ tails: MISS -> default d; HIT -> slot[I].value, untagged
      + [['label 'miss] ['li 'r0 d] '(jmp end)
         '(label hit) '(leax r0 r9 r9 1 0) '(ldx r0 r7 r0 8 8) '(sar r0 1) '(label end)])
   (asmx-seed ir nm addr) (: t (tablet 0) _ (pin t nm addr) (peep (assemble-at 'amd64 ir t) 'bytes 0))   ; assemble with ONE pre-bound external label nm->addr (the glaze's deopt handler)
   ; the SAME probe; WRITE tails (the correct `pin` = insert-OR-update). MISS (absent key, a GAP at slot I):
   ; guard the load factor -- if (len+1)*4 >= cap*3 a grow is due, which native can't do, so DEOPT (an
   ; EXTERNAL label the caller seeds); else INSERT (store the tagged key + spilled value, bump len). HIT:
   ; update in place. `(tablet n)` presizes so a counted insert loop of n keys never hits the deopt. The
   ; key/value are FIXNUMS (the group compiler is integer-only), so no GC write-barrier is needed.
   mpin-ir (mprobe-ir + '((label miss)
      (ld r0 r7 -16) (sar r0 1) (cmp r0 r8) (br ge deopt)                           ; len >= mask(=cap-1) -> table near-full -> deopt (r8 still holds the mask)
      (leax r0 r9 r9 1 0) (stx r7 r0 8 0 r10)                                       ; slot[I].key = tagged key
      (pop r8) (leax r8 r8 r8 1 1) (stx r7 r0 8 8 r8)                               ; slot[I].value = tag(v)
      (ld r0 r7 -16) (add r0 r0 2) (st r7 -16 r0) (jmp end)                         ; len++ (putcharm: +2)
      (label hit) (leax r0 r9 r9 1 0) (pop r8) (leax r8 r8 r8 1 1) (stx r7 r0 8 8 r8)
      (label end)))
   (hasl e) (? (two? e) (id? <e 'link || foldl (\ a x (a || hasl x)) 0 >e) 0)   ; does e syntactically contain a (link ..)? -> the group is VALUE-typed
   (suse? e P) (? (two? e) (? (member? <e '(peep tally pin) && id? <>e P) 1 (foldl (\ a x (a || suse? x P)) 0 >e)) 0)   ; P used DIRECTLY as (peep P..)/(tally P)/(pin P..)?
   (buse? e P) (? (two? e) (? (id? <e 'pin && id? <>e P) 1 (foldl (\ a x (a || buse? x P)) 0 >e)) 0)   ; P used as a CASK -- (pin P..) -- so its guard wants the buf kind, not str
   (cuse? e P) (? (two? e) (? (member? <e '(cap cup two?) && id? <>e P) 1 (foldl (\ a x (a || cuse? x P)) 0 >e)) 0)   ; P used as a CHAIN -- <P/>P/(two? P) -- raw pointer, NO entry guard (two? is total: a non-chain reads false)
   (muse? e P) (? (two? e) (? ((id? <e 'mpeep || id? <e 'mpin) && id? <>e P) 1 (foldl (\ a x (a || muse? x P)) 0 >e)) 0)   ; P used as a MAP -- (mpeep P k d) READ or (mpin P k v) UPDATE -- raw pointer, guarded as a map
   (nuse? e P) (? (two? e) (? ((id? <e '=) && (3 = (tally e))
                               && (: x1 <>e y1 <>>e
                                     ((id? x1 P && two? y1 && member? <y1 '(cap cup))
                                      || (id? y1 P && two? x1 && member? <x1 '(cap cup)))))
                               1 (foldl (\ a x (a || nuse? x P)) 0 >e)) 0)   ; P a POINT key -- (P = <..)/(<.. = P): raw word identity, entry-guarded nom|mint (a point equals only itself; a charm could bridge (2 = 2.0) so charms deopt to interp)
   ; INTERPROCEDURAL string-param inference: a param is a string if used in peep/tally OR passed as the arg
   ; to a sibling's string param. (strhash's `s` is a string because it flows into strhash_go's peep.) A
   ; deduped fixpoint over the group's (name . smask) map -- smasks only grow, so the size stabilizes.
   (dedup l) (? (atom? l) () (member? <l >l) (dedup >l) (<l >< dedup >l))   ; unique elements
   (gsm nm smap) (? (atom? smap) () (id? nm <<smap) ><smap (gsm nm >smap))                ; nm's smask in the (name . smask) map (() absent)
   (fnsm fp body smap)
    (dedup ((: (walk e acc) (? (two? e)
                (: gm (gsm <e smap)                                                                 ; if <e is a group fn, its string-param indices
                   a2 (? (atom? gm) acc (foldl (\ ac j (: arg (nthl >e j) (? (member? arg fp) (posof arg fp 0 >< ac) ac))) acc gm))   ; a string arg that is OUR param -> our string param
                   (foldl (\ ac x (walk x ac)) a2 >e))
                acc))
              body (filter (\ k (suse? body (nthl fp k))) (rangeL 0 (tally fp)))))                        ; seed acc = DIRECT peep/tally params
   (sm-size smap) (foldl (\ a e (a + (tally >e))) 0 smap)
   ; INTERPROCEDURAL string-param inference as a projection of the shared `monofix` harness (prel): the
   ; step recomputes every fn's smask from the whole (name . smask) map; sm-size (total mask width, ascending)
   ; is the measure. The (name . smask)-map shape is the caller's, not the harness's -- monofix is generic.
   (smfix info smap) (monofix smap (\ st (map (\ f (<f >< fnsm (nthl f 1) (nthl f 2) st)) info)) sm-size)
   ; --- counted fold loop: acc = (FOLD over cv<n of body(cv)), cv the counter, limit
   ; at Sp[0]. FOLD is a (op . init) -- + folds from 0, * from 1 -- and cmp is the loop
   ; test's comparison (the loop runs while (cmp cv n)). Now fully neutral IR, both targets.
   (foldinit op)  (? (op = '*) 1 0)                               ; * folds from 1, + from 0
   ; loopregs: the 3 loop-state registers (n / counter / accumulator). CALLER-SAVED on both targets, so
   ; they survive the body's acc/tmp/scratch (and there is no anchor in the loop lane). arm64 picks x6/x7/x8
   ; to dodge scratch=x10 / anchor=x9; x86 keeps r9/r10/r11 (IR r8/r9/r10).
   (loopregs tgt) (? (id? tgt 'amd64) '(r8 r9 r10) '(r6 r7 r8))
   ; loopepi: result already in acc -- putfix (overflow -> inline deopt) + Continue (Ip+=16; jmp [Ip]).
   (loopepi tgt) (: A (greg tgt 'acc) S (greg tgt 'sp) IP (greg tgt 'ip) T (greg tgt 'tmp)
      ([['adds A A A]] + (ldeopt tgt 0) + [['or A A 1] ['st S 0 A] ['add IP IP 16] ['ld T IP 0] ['jmpr T]]))
   ; END condition = the INVERSE of the continue-comparison (exit when (cmp cv n) is false).
   (exitcc cmp) (? (cmp = '<) 'ge (cmp = '<=) 'gt (cmp = '>) 'le
                   (cmp = '>=) 'lt (cmp = '=) 'ne ())  ; ge/gt/le/lt/ne
   (loopcode body cv op cmp tgt)
    (: rs (loopregs tgt) N (nthl rs 0) CV (nthl rs 1) ACC (nthl rs 2) A (greg tgt 'acc) S (greg tgt 'sp)
       bodyir (cgvir tgt (\ e d (? (e = cv) [['mov A CV]] ())) body 0)      ; body -> acc; the counter is read from CV
       accir  (? (id? op '+) ([['adds ACC ACC A]] + (ldeopt tgt 0))                                  ; ACC += acc ; overflow -> inline deopt
                 (id? op '*) (: dl (mint 0) ok (mint 0)                                                ; ACC *= acc ; mulo (no arm64 flag) branches to the inline deopt
                    ([['mulo ACC ACC A (greg tgt 'scratch) dl]] + [['jmp ok] ['label dl]] + (deoptbody tgt 0) + [['label ok]])) ())
       pre  [['ld N S 0] ['sar N 1] ['li CV 0] ['li ACC (foldinit op)]]   ; n = Sp[0]>>1 (assumed fixnum) ; cv = 0 ; acc = init
       (asmt tgt (pre + ['(label top) ['cmp CV N] ['br (exitcc cmp) 'end]]   ; cmp cv,n ; exit when (cmp cv n) false
              + bodyir + accir + [['inc CV] '(jmp top) '(label end) ['mov A ACC]] + (loopepi tgt))))   ; cv++ ; loop ; acc <- ACC ; putfix + Continue
   ; (jitsum body) -- compile (\ n (sum_{i<n} body(i))) to a native counted loop (x86 self-test helper).
   (jitsum body)
    (: src ['\ 'n [[': ['go 'i 'a] ['? ['< 'i 'n] ['go ['+ 'i 1] ['+ 'a body]] 'a]] 0 0]]
       (nif (emit (loopcode body 'i '+ '< 'amd64)) (ev src) src 1))
   ; --- CALL-OUT BRIDGE, native side: the STACKLESS drive ------------------------------------------
   ; (jitcalloutsum clos) -- a native counted loop `(\ n (sum_{i<n} (clos i)))` that CALLS OUT to the
   ; closure each iteration WITHOUT a re-entrant C frame. The call threads through callout_drive (love.c,
   ; {lvm_ap, lvm_ret0}, address via `calloutdrive`): the blob builds a [arg, clos, retB] frame on Sp,
   ; points Ip at the drive, and TAIL-jumps; lvm_ap applies clos to the tagged counter, lvm_ret0
   ; delivers the result to retB -- the blob's own resume label `bb`, whose address is materialized
   ; position-independently by `lar` (near pc-rel: rip-lea on x86, ADR on arm64 -- translation-invariant in
   ; the word-aligned mmap blob, where la's page-relative ADRP would mis-page; survives an image reload) into a Sp slot the
   ; callee cannot reach. So the callee runs entirely on the VM stack: a deep or re-entrant callee
   ; grows Sp, NEVER the C stack (the whole point -- the retired re-entrant ai_call1 pinned a C frame
   ; per nesting level). The loop state lives in 5 reserved Sp slots (a GC root, so a callee GC
   ; relocates it for free): R[0]=SA sum, R[1]=SC counter, R[2]=SN bound, R[3]=Sclos closure,
   ; R[4]=Sret resume addr, R[5]=Stwin the interp fallback; the arg n rides at R[6], the caller
   ; continuation at R[7].
   ; --- SOUNDNESS GATES (the production bridge): the naive prototype assumed a fixnum callee and an
   ; in-range sum. Two inline guards make it sound, each DEOPTING to a faithful interp twin (below):
   ;   GATE 1 (result-kind): the resume `sar`s the callout result assuming a tagged fixnum. A heap
   ;     result (gem/big/list) is EVEN -> `test A 1` -> deopt. So a callee returning a non-fixnum
   ;     re-runs the whole sum in interp (generic +), never corrupts on the untag.
   ;   GATE 2 (sum-overflow): `SA += result` is `adds` (flags) + `br vc` -> a fixnum-boundary overflow
   ;     deopts (cf. loopepi/ldeopt), so an out-of-range sum re-runs in interp -> bignum. EXACT vs interp.
   ; THE TWIN: value[1] is the PLAIN twin (the image-dump revert reads it as-is -- never a pack:
   ; a pack chain there made a woken dead native revert to the CHAIN, which data-applies -- the
   ; kore-diff bug); clos rides the nifx EXTRAS slot (Ip+32). Both are cached in GC-root
   ; Sp slots (R[3]/R[5]) at the prologue -- read once, IP-relative, before the loop clobbers Ip. The
   ; shared `codeopt` restores Sp to entry (Sp[0]=the original tagged n) and Continues into the twin,
   ; which recomputes the whole sum correctly (the native partial SA is discarded, as any loop deopt).
   ; A chain ref points at its ap-word, so car is +8, cdr is +16. x64-only (the drive design is arch-neutral).
   ; jitcalloutx stamps the caller's `src` at value[-1] (=/show transparency -- post-egg has no `poke`); the
   ; recognizer (hook.l calloutinfo) passes the closure's real surface \-expr. jitcalloutsum is the 1-arg
   ; test-helper alias with a placeholder src.
   ; calloutir: the drive's IR, parameterized by target (greg tgt). The Sp offsets are VM-WORD (8-byte)
   ; slots -- the SAME on x64 and arm64 (the machine-stack slotsz only matters for push/pop, which the
   ; callout never uses). Every op (leax/lea/test/adds/br-vc/la/sub) is already arm64-proven by the loop/
   ; map lanes, so `(assemble 'arm64 (calloutir 'arm64))` lays real aarch64. arm64 exec rides the corpus.
   ; coacc / coend: the FOLD-OP accumulator, shared by calloutir/calloutframeir (op in {+,*}). The callout
   ; result rides TAGGED at Sp[0] (Sp=R-8 at the resume label bb); gate 1 (fixnum?) already ran in A.
   ;   coacc folds the result into SA @ Sp[8]. `+` keeps SA TAGGED (init tag(0)=1): add the even 2r=(result-1),
   ;     deopt on the adds-vc TAG-boundary overflow. `*` keeps SA RAW (init 1): untag r via `sar`, `mulo`
   ;     (arm64 has no mul-flag, so mulo branches on overflow) straight to codeopt.
   ;   coend puts SA into A tagged for delivery. `+` is a bare load (SA already tagged); `*` retags the raw SA
   ;     (`adds A A A` = 2·SA, vc-checked -> the product's own fixnum-boundary gate, `or ..1`). Both deopt to
   ;     `codeopt`, which wants Sp=R-8; coend runs at Sp=R (end), so its `*` path subs 8 before the jmp.
   (coacc tgt op) (: A (greg tgt 'acc) SP (greg tgt 'sp) T (greg tgt 'tmp) SC (greg tgt 'scratch)
      (? (id? op '*) [['sar A 1] ['ld T SP 8] ['mulo T T A SC 'codeopt] ['st SP 8 T]]                        ; SA(raw) *= untag(r) ; overflow -> deopt
         [['sub A A 1] ['ld T SP 8] ['adds T T A] ['br 'vc 'sok] ['jmp 'codeopt] ['label 'sok] ['st SP 8 T]]))  ; SA(tagged) += 2r ; overflow -> deopt
   (coend tgt op) (: A (greg tgt 'acc) SP (greg tgt 'sp)
      (? (id? op '*) [['ld A SP 0] ['adds A A A] ['br 'vc 'etag] ['sub SP SP 8] ['jmp 'codeopt] ['label 'etag] ['or A A 1]]  ; SA raw -> tag(SA) ; overflow -> deopt (Sp R -> R-8 first)
         [['ld A SP 0]]))                                                                                       ; SA already tagged
   ; --- COMPUTED CALL-OUT ARG: the per-step term is (F ARG), ARG an exprp over the counter (not just the bare
   ; counter). caargir compiles ARG -> acc RAW, mirroring cgir's arith grammar but with the counter as the sole
   ; leaf (raw at VM Sp[coff]; exprp guarantees the ONLY variable is the counter, so a name IS the counter -- no
   ; fixnum-guard/untag, the counter is always a valid raw fixnum). Guards / overflow deopt to the callout's own
   ; `deopt` label (caargdeopt), which restores the machine sp (anchored) then routes to codeopt -> the interp
   ; twin (which recomputes ARG faithfully -> bignum if it overflowed). Nesting rides the MACHINE stack (push/pop),
   ; independent of VM Sp, so coff stays constant across sub-exprs.
   (caargir tgt e coff) (: A (greg tgt 'acc) S (greg tgt 'sp) T (greg tgt 'tmp)
      (? (name? e) [['ld A S coff]]                                                                          ; the counter, raw
         (whole? e) [['li A e]]
         (two? e) (: op <e a <>e b <>>e
            (? (member? op '(+ - *))
                 ((caargir tgt a coff) + [['push A]] + (caargir tgt b coff) + [['pop T]] + (arithir tgt op))
               (member? op '(// %))
                 ((caargir tgt a coff) + [['push A]] + (caargir tgt b coff) + [['test A A] '(br eq deopt) ['pop T]] + (divir tgt op))
               (member? op '(& | ^))
                 ((caargir tgt a coff) + [['push A]] + (caargir tgt b coff) + [['pop T] [(bitop op) A A T]])
               (member? op '(< <= > >= =))
                 ((caargir tgt a coff) + [['push A]] + (caargir tgt b coff) + [['pop T] ['cmp T A] ['set (ccof op) A]])
               (id? op '?)
                 (: el (mint 0) en (mint 0)
                    ((caargir tgt a coff) + [['cmp A 0] ['br 'le el]] + (caargir tgt b coff) + [['jmp en] ['label el]] + (caargir tgt <>>>e coff) + [['label en]]))
               ()))
         ()))
   ; caarg: leave tag(ARG(counter)) in the frame slot F[0] = Sp[-32] (the 4-word walkable frame). A BARE counter tags directly (never
   ; overflows -- a loop index is always a valid fixnum), byte-identical to the pre-extension code. A COMPUTED
   ; ARG anchors the machine sp, runs caargir, then tags with an overflow gate (both -> `deopt`).
   (caarg tgt arg) (: A (greg tgt 'acc) SP (greg tgt 'sp) T (greg tgt 'tmp) AN (greg tgt 'anchor)
      (? (name? arg) [['ld T SP 8] ['leax T T T 1 1] ['st SP -32 T]]                                         ; bare counter: F[0] = tag(i)
         ([['lea AN 'sp 0]]                                                                                     ; anchor the machine sp for the arg-expr deopt unwind
          + (caargir tgt arg 8)                                                                                   ; A = ARG(counter) raw ; guards/overflow -> deopt
          + [['adds A A A] ['br 'vs 'deopt] ['or A A 1] ['st SP -32 A]])))                                   ; F[0] = tag(ARG) ; tag overflow -> deopt
   ; caargdeopt: the `deopt` label the computed-arg lane branches to -- restore the machine sp from the anchor
   ; (discarding any pending arg-expr pushes), bring VM Sp R -> R-8, and fall into the shared codeopt (-> twin).
   ; Appended to the blob ONLY when the arg is computed (a bare arg never deopts here).
   (caargdeopt tgt) (: SP (greg tgt 'sp) AN (greg tgt 'anchor)
      ['(label deopt) ['lea 'sp AN 0] ['sub SP SP 8] ['jmp 'codeopt]])
   (calloutir tgt op arg)
    (: A (greg tgt 'acc)  IP (greg tgt 'ip)  SP (greg tgt 'sp)  T (greg tgt 'tmp)  U (greg tgt 'scratch)
       (['(label entry0)                                      ; the resume-offset anchor (lvm_resume jumps base + tag'd offset)
          ['sub SP SP 48]                                     ; reserve 6 loop-state slots (Sp = R)
          ['ld U IP 32] ['st SP 24 U]                        ; Sclos = the nifx EXTRAS slot = clos     -> R[3]  (IP still &value[0])
          ['ld U IP 8]  ['st SP 40 U]                         ; Stwin = value[1] = twin (interp)        -> R[5]
          ['ld T SP 48] ['sar T 1] ['st SP 16 T]            ; SN = untag(arg @ R[6])                  -> R[2]
          ['li T 1] ['st SP 0 T] ['li T 0] ['st SP 8 T]   ; SA = tag(0)=1 (kept TAGGED so gate 2 checks the fixnum boundary) ; SC = 0   -> R[0] R[1]
          ['lar T 'entry0] ['st SP 32 T]]                     ; Sbase = the blob base (out-of-pool W^X; lar = ADR on arm64)  -> R[4]
        + ['(label top)
           ['ld T SP 8] ['ld U SP 16] ['cmp T U] ['br 'ge 'end]]   ; i >= n -> end
        + (caarg tgt arg)                                       ; F[0] = tag(ARG(counter)) -- bare i, or a computed expr (deopts to `deopt`)
        + [['ld T SP 24] ['st SP -24 T]                      ; F[1] = clos
           ['lar T 'bb] ['ld U SP 32] ['subs T T U]          ; F[2] = tag(&bb - base): an ODD CHARM, GC-walk-clean
           ['leax T T T 1 1] ['st SP -16 T]
           ['st SP -8 U]                                       ; F[3] = base (out-of-pool -- the walk returns it unharmed)
           (jk tgt IP 8) ['sub SP SP 32]         ; Ip = callout_resume ; Sp = frame
           ['ld T IP 0] ['jmpr T]]                            ; Continue -> lvm_ap -> clos -> lvm_resume -> bb
        + ['(label bb)
           ['ld A SP 0]                                        ; A = result (Sp[0] on resume, Sp = R-8 -- same landing as the retired retB path)
           ['test A 1] ['br 'eq 'codeopt]]                    ; GATE 1: non-fixnum (even) result -> deopt
        + (coacc tgt op)                                        ; GATE 2: fold the result into SA @ Sp[8] (op-specific, overflow -> deopt)
        + [['ld T SP 16] ['add T T 1] ['st SP 16 T]         ; SC += 1
           ['add SP SP 8] ['jmp 'top]]                        ; Sp = R ; next iteration
        + ['(label end)]
        + (coend tgt op)                                        ; A = SA tagged (`*` retags here, overflow -> deopt)
        + [['add SP SP 48] ['st SP 0 A]                      ; Sp = entry ; result into arg slot
           ['ld IP SP 8] ['st SP 8 A] ['add SP SP 8]         ; inline ret0: Ip=caller-cont; Sp[1]=result; Sp++
           ['ld T IP 0] ['jmpr T]]                            ; Continue -> caller
        + ['(label codeopt)                                    ; shared deopt (both gates); Sp = R-8 on entry
           ['ld A SP 48]                                       ; A = Stwin (R[5] @ Sp+48) -- BEFORE restoring Sp
           ['add SP SP 56]                                     ; Sp = entry (R+48); Sp[0] = the original tagged n
           ['mov IP A] ['ld A IP 0] ['jmpr A]]               ; Continue into twin, applied to Sp[0] = n
        + (? (name? arg) () (caargdeopt tgt))))                  ; the computed-arg `deopt` label, only when ARG is an expr
   (jitcalloutsum clos) (jitcalloutx clos ['\ 'n 0] '+ 'i 'i 'amd64)
   (jitcalloutx clos src op iv arg tgt)
    (: fop (? (id? op '*) (*) (+))  a0 (foldinit op)                             ; the fold op as a VALUE (parenthesized: a bare `* +` mid-list mis-factors) + its identity (0 / 1)
       argfn (? (name? arg) 0 (ev ['\ iv arg]))                               ; computed ARG -> a closure (\ counter ARG); bare arg needs none
       twin (? (name? arg) (\ n ((: (go i a) (? (i < n) (go (i + 1) (fop a (clos i))) a)) 0 a0))            ; bare: clos applied to the counter
                           (\ n ((: (go i a) (? (i < n) (go (i + 1) (fop a (clos (argfn i)))) a)) 0 a0)))   ; computed: clos applied to ARG(counter)
       (nifx (emit (asmt tgt (calloutir tgt op arg))) twin src 1 clos))   ; value[1] = the twin (image-revert-clean); clos = the extras slot; value[-1]=src
   ; --- CAPTURED / multi-arg CALL-OUT: the frame-based bridge (njit-calloutframe) --------------------
   ; (njit-calloutframe src Fidx Nidx a e) -- the same stackless sum-over-(F i), but F and the bound N are
   ; FRAME params (a captured import or a genuine 2nd arg), read from the [imps args] frame at Sp[8*idx]
   ; (F raw = a closure ref; N tagged-fixnum, untagged) rather than baked. `a` = the FULL frame size (imps +
   ; args, ala's `ary`), so the entry frame is [p0..p_{a-1}] at Sp[0..a-1] with the caller-cont at Sp[a];
   ; the return HAND-ROLLS lvm_ret(n=a) (the call-out clobbers Ip, so the standard putfix+Continue epilogue
   ; is unreachable). DEOPT (either gate, or a non-fixnum N at the prologue) goes to value[1]=e -- the
   ; bytecode twin ala built, which CAPTURES F, so it recomputes correctly. a>=2 here (a=1 no-imps is the
   ; global lane), so e is an lvm_cur cell: enter its BODY at e+16 with the saturated frame on Sp (deoptir's
   ; bodyp path). Reserved 6 Sp slots (Sp=R): R[0]=SA R[1]=SC R[2]=SN R[3]=SF R[4]=Sbase R[5]=Se(=e).
   ; The frame rides above R (E=R+48). arm64 lays via (calloutframeir 'arm64 ..); exec rides the corpus.
   (calloutframeir tgt Fidx Nidx a op arg)
    (: A (greg tgt 'acc)  IP (greg tgt 'ip)  SP (greg tgt 'sp)  T (greg tgt 'tmp)  U (greg tgt 'scratch)
       (['(label entry0)                                      ; the resume-offset anchor
          ['ld U SP (8 * Fidx)]                               ; U = F (raw closure) from the frame
          ['ld A SP (8 * Nidx)] ['test A 1] ['br 'ne 'ngood]]   ; N a fixnum? -> ngood
        + [['ld T IP 8] ['lea IP T 16] ['ld T IP 0] ['jmpr T]]   ; N NOT a fixnum -> deopt to e's body (Sp = E, IP valid)
        + ['(label ngood) ['sar A 1]                          ; A = N untagged
           ['ld T IP 8]                                        ; T = e (deopt twin = value[1], IP+8 at body entry)
           ['sub SP SP 48]                                     ; reserve 6 slots; Sp = R
           ['st SP 24 U] ['st SP 16 A] ['st SP 40 T]         ; SF = F ; SN = N ; Se = e
           ['li T 1] ['st SP 0 T] ['li T 0] ['st SP 8 T]   ; SA = tag(0)=1 (kept TAGGED so gate 2 checks the fixnum boundary) ; SC = 0
           ['lar T 'entry0] ['st SP 32 T]]                     ; Sbase = the blob base (out-of-pool W^X)
        + ['(label top)
           ['ld T SP 8] ['ld U SP 16] ['cmp T U] ['br 'ge 'end]]   ; SC >= SN -> end
        + (caarg tgt arg)                                       ; F[0] = tag(ARG(counter)) -- bare SC, or a computed expr (deopts to `deopt`)
        + [['ld T SP 24] ['st SP -24 T]                      ; F[1] = SF (clos)
           ['lar T 'bb] ['ld U SP 32] ['subs T T U]          ; F[2] = tag(&bb - base)
           ['leax T T T 1 1] ['st SP -16 T]
           ['st SP -8 U]                                       ; F[3] = base
           (jk tgt IP 8) ['sub SP SP 32]         ; Ip = callout_resume ; Sp = frame
           ['ld T IP 0] ['jmpr T]]                            ; Continue -> lvm_ap -> F -> lvm_resume -> bb
        + ['(label bb)
           ['ld A SP 0] ['test A 1] ['br 'eq 'codeopt]]      ; GATE 1: non-fixnum result -> deopt
        + (coacc tgt op)                                        ; GATE 2: fold the result into SA @ Sp[8] (op-specific, overflow -> deopt)
        + [['ld T SP 16] ['add T T 1] ['st SP 16 T]         ; SC += 1
           ['add SP SP 8] ['jmp 'top]]                        ; Sp = R ; next iteration
        + ['(label end)]
        + (coend tgt op)                                        ; A = SA tagged (`*` retags here, overflow -> deopt)
        + [['add SP SP 48] ['st SP 0 A]                      ; Sp = E (frame) ; result into Sp[0]
           ['ld IP SP (8 * a)] ['st SP (8 * a) A] ['add SP SP (8 * a)]   ; hand-rolled lvm_ret(n=a): cont@Sp[a] ; deliver ; pop a
           ['ld T IP 0] ['jmpr T]]                            ; Continue -> caller
        + ['(label codeopt)                                    ; shared gate deopt; Sp = R-8 on entry
           ['ld T SP 48]                                       ; T = Se (= e, R[5] @ Sp+48) -- BEFORE restoring Sp
           ['add SP SP 56]                                     ; Sp = E (frame intact for e's body)
           ['lea IP T 16] ['ld T IP 0] ['jmpr T]]            ; enter e's body (a>=2: e+16), saturated frame on Sp
        + (? (name? arg) () (caargdeopt tgt))))                 ; the computed-arg `deopt` label, only when ARG is an expr
   (njit-calloutframe src Fidx Nidx a e op arg tgt)
    (nif (emit (asmt tgt (calloutframeir tgt Fidx Nidx a op arg))) e src a)   ; value[1] = e (the bytecode twin, deopt target -- source-shaped, so op- AND arg-correct); arity a ; src for =/show
   ; --- AMBLE: the universal boring-tier translator -------------------------------------------------
   ; the LAST natjit lane: a closure every other lane declined gets a PER-OP native translation.
   ; values stay TAGGED (heap refs ride whole -- no result gates), every temporary lives in a
   ; reserved VM-Sp slot (a GC root, so a callee GC relocates mid-expression state for free; the
   ; machine stack is NEVER used), and threaded dispatch becomes straight-line code. calls leave
   ; via the stackless callout_drive and resume at a lar label -- any callee value works (lvm_ap
   ; is total: data self-applies), multi-arg calls chain 1-arg drives (application IS currying).
   ; every runtime surprise -- a non-fixnum under arith/cmp, a heap value under ?, overflow, a
   ; full heap under link -- DEOPTS by RESTART: read the twin from its reserved slot (a call
   ; clobbers Ip, so the epilogue hand-rolls lvm_ret and the deopt never reads Ip-relative),
   ; restore Sp, enter e on the ORIGINAL frame (arg slots are never mutated; loop state lives in
   ; the reserved slots). a restart re-runs any call-outs already made -- the same bet the fold
   ; lanes' twins take. grammar = the probed families: fixnum/() consts, two? cap cup id? link,
   ; + - * // % & | ^ and comparisons (fixnum-guarded; = is TOTAL -- points and fixnums inline, the rest
   ; drives the = value), ? (2- or 3-arm), ONE applied named-let (self tail-calls -> a jump,
   ; both def sugars), calls to frame/loop names, RECURSION on the closure's OWN letrec name
   ; (a saturated tail call re-enters as a loop, any other takes the direct self frame), in
   ; LOOP bodies calls to resolvable global heads, QUOTED DATA (any datum: the source node rides a nifx extras slot, and
   ; (a = 'sym) is raw identity by the point-= law). strings, floats, inner \,
   ; effects, global reads in OPERAND position: DECLINE (bytecode, exactly as before). a global
   ; callee resolves via gv at compile (redefinition-stale -- the callout lanes' bet; NEVER a
   ; farq name, ala's letrec-local predicate) and rides the nifx extras chain into a reserved
   ; slot beside () and the quotes (GC-walked, reloaded per call -- no baked absolute).
   ; reserved region: [loopvars.. | Se(twin) | ()+globals+quotes.. | spills..],
   ; frame above it, cont at frame+8a. known limit: a ? over a HEAP test value (bare (? l ..)
   ; on a list) deopts EVERY call -- correct, just a small entry tax over pure bytecode.
   amble-nil (:)
   (amax a b) (? (a < b) b a)
   (adistinct l) (? !(two? l) 1  (member? <l >l) 0  (adistinct >l))
   (aconst? e) (whole? e || id? e amble-nil)
   (aop2? op) (member? op '(+ - * // % & | ^ < <= > >= = id? link))
   (acmp? op) (member? op '(< <= > >= =))
   (aquote? e) (two? e && id? <e '\ && 2 = (tally e))            ; the reader's quote: (\ X) at EXACTLY two -- a datum constant
   (aqnom? e) (aquote? e && name? <>e)                          ; a quoted NOM: a point, so (a = 'sym) is raw identity
   (amemq x l) (? !(two? l) 0  (id? x <l) 1  (amemq x >l))     ; id?-membership: two =-equal quote NODES stay DISTINCT slots
   (aposq x l i) (? (id? x <l) i (aposq x >l (i + 1)))         ; (interp's quote answers the source node itself, id?-visibly)
   (aq e acc) (? (aquote? e) (? (amemq e acc) acc (e >< acc))          ; collect quote nodes; NEVER walk INTO quoted data
                 (two? e) (foldl (\ ac x (aq x ac)) acc e)
                 acc)
   (ahascall? env go e)                                                  ; does e contain a call (any non-op head)? (the bisect knob's test)
    (? (aquote? e) 0
       (two? e) (: op <e
          ((name? op && !(id? op go) && !(member? op '(? two? cap cup + - * < <= > >= = id? link)))
           || (foldl (\ a x (a || ahascall? env go x)) 0 >e)))
       0)
   ; aheads: collect the FREE call heads of e -- the global-callee candidates. within the ACCEPTED
   ; grammar the only binders are the frame and the ONE named-let, so a head outside env/go IS a
   ; global; go and the frame/loop names are NEVER candidates -- a named-let binds go over its
   ; whole body, so a non-tail (go ..) is LOCAL and must decline, never resolve a stale leaked
   ; namesake (rc4's bug). collection over-approximates (a head under a node the grammar later
   ; declines is harmless: amble-ok? is the gate, gn merely feeds it); quoted data stays unwalked.
   (aheads env go e acc)
    (? (aquote? e) acc
       (two? e)
        (: op <e                                                    ; a2, not a rebind of acc: a `:` is ONE SCOPE, so `acc (.. acc ..)`
           a2 (? ((name? op) && !(id? op go) && !(member? op env) && !(member? op acc)
                  && !(member? op '(? two? cap cup + - * < <= > >= = id? link)))
                 (op >< acc) acc)                                      ; reads the UNPINNED local -- a silent missing per node (help.l heard 14)
           (foldl (\ ac x (aheads env go x ac)) a2 >e))
       acc)
   ; amble-ok?: does e compile? env = visible names (loop vars shadow params), gn = resolvable
   ; global call heads, go/k = the loop name + its arity (0 outside a loop), tl = tail position
   ; ((go ..) is a jump, so it is only sound where its value IS the loop's value).
   (amble-ok? env gn go k tl e)
    (? (name? e) (member? e env)
       (aconst? e) 1
       (aquote? e) 1
       (two? e)
        (: op <e  as >e  n (tally as)
           (? (id? op '?) (((n = 3) || (n = 2)) && (amble-ok? env gn go k 0 <as)   ; 2-arm ?: the missing else is (), acg's amble-nil slot
                           && (amble-ok? env gn go k tl <>as)
                           && (amble-ok? env gn go k tl <>>as))
              (member? op '(two? cap cup)) (n = 1 && amble-ok? env gn go k 0 <as)
              (aop2? op) (n = 2 && amble-ok? env gn go k 0 <as && amble-ok? env gn go k 0 <>as)
              (id? op '|| && !(member? op env)) (2 <= n && all (\ x (amble-ok? env gn go k 0 x)) as)
              (id? op go) (tl && n = k && all (\ x (amble-ok? env gn go k 0 x)) as)
              (name? op) ((0 < n) && (member? op env || member? op gn)
                          && (all (\ x (amble-ok? env gn go k 0 x)) as))
              0))
       0)
   ; amble-depth: an upper bound on the spill slots e needs (operand nesting rides VM slots).
   ; cx op = the EXTRA curry steps a head carries (a sibling callee's import count -- its chain
   ; is entry(imports.. args..)); 0 for every other head.
   (amble-depth go cx e)
    (? !(two? e) 0
       (aquote? e) 0
       (: op <e  as >e
          (? (id? op '?) (amax (amble-depth go cx <as) (amax (amble-depth go cx <>as) (amble-depth go cx <>>as)))
             (member? op '(two? cap cup)) (amble-depth go cx <as)
             (id? op '||) ((tally as) + (foldl (\ a x (amax a (amble-depth go cx x))) 0 as))   ; aor needs no spills, but a SHADOWED || is a call --
                                                                                              ; the call formula covers both (over-reserve is safe)
             (id? op '=) (2 + (amax (amble-depth go cx <as) (1 + (amble-depth go cx <>as))))   ; aeq2's slow lane spills the partial + the operand
             (aop2? op) (amax (amble-depth go cx <as) (1 + (amble-depth go cx <>as)))
             (id? op go) ((tally as) + (foldl (\ a x (amax a (amble-depth go cx x))) 0 as))
             (((cx op) + (tally as)) + (foldl (\ a x (amax a (amble-depth go cx x))) 0 as)))))   ; a CALL spills a partial PER CURRY STEP: an n-arg call touches
                                                                                    ; spills d..d+n-2 (+1 was the g4-chain cont-clobber: the 4th arg's
                                                                                    ; partial landed ON the caller-cont -- Ip=3, the oracle fuzz fault)
   ; amble-loop: match the named-let body, BOTH def sugars x BOTH spellings --
   ;   applied:  ((: (go v..) GB) i..)  |  ((: go (\ v.. GB)) i..)
   ;   trigger:  (: (go v..) GB (go i..))  |  (: go (\ v.. GB) (go i..))    (the body-having
   ;   let whose result IS the fired loop -- moon's lexer spelling)         -> (go vars GB inits) | 0
   (amble-let1 S inits)
    (? (id? <S ': && 3 = (tally S))
       (: D1 <>S  D2 <>>S
          (? (two? D1 && name? <D1 && all name? >D1 && 0 < (tally >D1))
               [<D1 >D1 D2 inits]
             (name? D1 && two? D2 && id? <D2 '\ && two? >D2 && two? >>D2)
               [D1 (init >D2) (last >D2) inits]
             0))
       0)
   (amble-loop M)
    (? !(two? M) 0
       (two? <M) (amble-let1 <M >M)                        ; applied
       (id? <M ': && 4 = (tally M))                              ; trigger: (: DEF GB (go i..))
        (: r (amble-let1 [': <>M <>>M] ())
           trg <>>>M
           (? (r && two? trg && id? <trg <r)                   ; the trigger fires the SAME name; its args are the inits
              [<r <>r <>>r >trg]               ; (amble-ok? gates their content -- a (go ..) inside one declines)
              0))
       0)
   ; asub: naive whole-form substitution -- SAFE here because the amble grammar admits no inner
   ; binders (no nested \ or : inside a loop body), so there is nothing to capture. used to
   ; alpha-rename a loop var that SHADOWS a frame param (the walker idiom ((: (go l a) ..) l a)):
   ; the inits then read the params, the body the renamed vars, and one flat env serves both.
   (asub old new e) (? (id? e old) new  (aquote? e) e  (two? e) (map (\ x (asub old new x)) e)  e)   ; quoted DATA is inert -- never rename inside it
   (afresh v avoid) (: c (intern ((string v) + "~")) (? (member? c avoid) (afresh c avoid) c))
   ; adesug: rewrite what amble can own by PURE SOURCE IDENTITY, so the twin/deopt story is
   ; untouched -- (X) is X ((f) == f at zero operands: an INFIX span reads as the wrapped
   ; application, (k < n) -> ((k < n)), so every infix-written body wears these singletons),
   ; (list a..) is nested links, (&& a..) is nested (? a .. ()) (short-circuit included; &&
   ; answers the last value or the unit -- probed AND spec'd). env-shadowed names stay calls;
   ; quoted data stays inert. (|| ..) is NOT a rewrite (its value is the first truthy ARG --
   ; a rewrite would double-eval); aor emits it.
   ; aopv: macro expansions splice op VALUES, not noms (the list/backtick macro emits the link
   ; NIF as a head; show prints it as `link`, invisibly -- the forensic dump showed (atom link
   ; other)). normalize a value head back to its nom -- id?-keyed, and only when the nom is NOT
   ; env-shadowed (a param named link must win, as it does in bytecode).
   aopv [[two? 'two?] [cap 'cap] [cup 'cup] [id? 'id?] [link 'link]
           [tally 'tally] [(+) '+] [(-) '-] [(*) '*] [(<) '<] [(<=) '<=]
           [(>) '>] [(>=) '>=] [(=) '=] [(//) '//] [(%) '%] [(&) '&] [(|) '|] [(^) '^]]
   ; a miss falls through to `opnom`, the glued lane's own value -> word map (prel), so every
   ; sigil-backed head is named here without this list having to carry all fourteen.
   (aopn x l) (? !(two? l) (opnom x)  (id? x <<l) <><l  (aopn x >l))
   ; adesug: rewrite what amble can own by PURE SOURCE IDENTITY, so the twin/deopt story is
   ; untouched -- (X) is X ((f) == f at zero operands: an INFIX span reads as the wrapped
   ; application, (k < n) -> ((k < n)), so every infix-written body wears these singletons),
   ; a VALUE head is its nom (aopv above), (list a..) is nested links, (&& a..) is nested
   ; (? a .. ()) (short-circuit included; && answers the last value or the unit -- probed AND
   ; spec'd). env-shadowed names stay calls; quoted data stays inert. (|| ..) is NOT a rewrite
   ; (its value is the first truthy ARG -- a rewrite would double-eval); aor emits it.
   (adesug env e)
    (? (aquote? e) e
       !(two? e) e
       (atom? >e) (adesug env <e)                       ; the (X) singleton -- EMPTINESS, never nil?: a tail of
                                                        ; NAMELESS mints (bsub's fresh binders) NETS to 0, and the
                                                        ; net test unwrapped (\ m1 m2 m2) to bare \ -- a quote-shaped
                                                        ; wreck amble then compiled as the constant \ (miscompile)
       (: op0 <e
          vn (? (name? op0 || two? op0) 0 (aopn op0 aopv))
          op (? (vn && !(member? vn env)) vn op0)
          as >e
          (? (id? op 'list && 0 < (tally as) && !(member? op env))
               ((: (go l) (? (two? l) ['link (adesug env <l) (go >l)] ())) as)
             (id? op '&& && 2 <= (tally as) && !(member? op env))
               ((: (go l) (? (two? >l) ['? (adesug env <l) (go >l) ()] (adesug env <l))) as)
             ((? (id? op op0) (adesug env op0) op) >< map (\ x (adesug env x)) as))))
   ; njit-amble: the hook entry -- compile s (deopt -> e) or answer 0 (decline; the hook keeps e).
   ; farq = ala's letrec-local predicate (see hook.l): a truthy (farq nom) names a binding that
   ; is free-LOOKING in a weave re-analysis surface, wire-resolved in truth -- never a gn candidate.
   ; lams = the scope's lambda-sibling list ((name . cell)..): a cell holds (entry . import-names),
   ; lz's frozen site.
   ; adrv?: does e contain a DRIVER call -- a self-call or an applied frame/loop name? the
   ; non-loop cost gate: a body whose only calls go to resolvable globals is a one-shot
   ; wrapper (its compile is nearly always a waste); a body that drives a parameter (a
   ; walker's f) or itself amortizes like a loop.
   (adrv? env sf e)
    (? (aquote? e) 0
       (two? e) (: op <e
          ((name? op && (id? op sf || (member? op env && two? >e)))
           || (foldl (\ a x (a || adrv? env sf x)) 0 >e)))
       0)
   (njit-amble s a e gv farq lams nm tgt)
    (: ps (init >s)  M (last >s)
       lp (amble-loop M)
       go   (? lp <lp 0)     vars0 (? lp <>lp ())
       GB0  (? lp (nthl lp 2) 0)  inits (? lp (nthl lp 3) ())
       shad (filter (\ v (member? v ps)) vars0)
       ren  (map (\ v [v (afresh v (go >< vars0 + ps))]) shad)
       vars (map (\ v (: r (assoc v ren) (? r <>r v))) vars0)
       GB   (? lp (foldl (\ b pr (asub <pr <>pr b)) GB0 ren) 0)
       k    (tally vars)
       body (? lp GB M)
       env  (vars + ps)
       body (adesug env body)                                ; own list/&& by definition (nested links / nested ?s)
       inits (map (\ i (adesug env i)) inits)
       ; SELF: the letrec name this closure is being bound to (ala's nm; 0 when anonymous).
       ; a saturated tail self-call jumps the body top on a rewritten frame (the recursion
       ; becomes a loop); any other self-call drives the blob's OWN cell (read off Ip at the
       ; prologue) -- early-bound, exactly the wire's backpatched site. shadowed names decline.
       sc   (assoc nm lams)                                  ; the self lams cell: its kept import row names the frame's leading slots,
       scc  (? (two? sc) (peep >sc 0 0) 0)                   ; so a self-call re-supplies the imports the way the wire's site does
       simp (? (two? scc) >scc ())
       ni   (tally simp)
       self (? (id? nm 0) 0  (member? nm ps) 0  (member? nm vars0) 0  (id? nm go) 0
               (member? nm '(? two? cap cup id? link + - * < <= > >= = || tally list &&)) 0
               !(simp = (take ni ps)) 0
               nm)
       sfq  !(id? self 0)
       ; the free-head seams: LOOP bodies, and (drok) bodies that drive a param or themselves
       ; -- the shapes that amortize the call and compile taxes. a one-shot wrapper whose only
       ; calls go to globals -- (\ t k (peep t k ())) and kin -- stays bytecode.
       ; GLOBALS (v4): a free head that is NOT farq resolves via gv at COMPILE and rides an
       ; extras slot -- redefinition-stale, exactly the callout lanes' bet. a farq name (a
       ; letrec local the weave RE-analysis dropped from the import row) must never gv-resolve:
       ; the book holds at best a stale leaked namesake.
       ; SIBLINGS (v6): a farq name that lamq resolves is a woven lambda sibling of this very
       ; let -- (entry . import-names), the SAME pair lz freezes into a bytecode reference site
       ; (lvm_quote entry applied to the import names re-read in the referrer's frame) -- so
       ; amble compiles the identical application with NO staleness bet: the entry rides an
       ; extras slot and the call curries entry(imports.. args..). fixcaps' pour guarantees the
       ; import names sit in this frame; entry 0 (a forward/self ref, lz's backpatch case) or a
       ; stray import just isn't in cn -> amble-ok? declines that body.
       cand (filter (\ f !(id? f self)) (foldl (\ ac i (aheads env go i ac)) (aheads env go body ()) inits))
       drok (? lp 1 (? (two? cand) (adrv? env self body) 1))   ; the non-loop cost gate: free heads ride only a body that drives a param
                                                               ; or itself -- and a declining body skips the resolution below entirely
       csx  (? drok (map (\ f [f (? (farq f)                 ; only a letrec-local can be a sibling: farq (a cheap chain peep) gates
                              (: lfd (assoc f lams)          ; the lam-list assoc. the cell READ mirrors ev.l's cof: box slot 0.
                                 (? (two? lfd) (peep >lfd 0 0) 0))
                              0)])
                 cand) ())
       cgood (filter (\ p (: cc <>p                     ; import names must be FRAME params (ps, where fixcaps pours them) --
                             (two? cc && <cc && all (\ x (member? x ps)) >cc))) csx)   ; never loop vars (bytecode reads the propagated capture)
       cn    (map cap cgood)
       cvals (map (\ p <<>p) cgood)
       (cimp f) ><>(assoc f cgood)
       gps  (? drok (filter (\ p (lit? <>p))
             (map (\ f [f (gv f 0)])
              (filter (\ f !(farq f)) (filter (\ f !(member? f cn)) cand)))) ())
       gn   (map cap gps)
       gvals (map caup gps)
       qs   (foldl (\ ac i (aq i ac)) (aq body ()) inits)   ; the quoted-datum constants (id?-deduped source nodes)
       nq   (tally qs)
       eqv  (gv '= 0)                                       ; the = value: aeq2's generic lane drives it off an extras slot
       cx   (\ op (? (id? op self) ni (member? op cn) (tally (cimp op)) 0))
       nd   (amax (amble-depth go cx body) (foldl (\ acd i (amax acd (amble-depth 0 cx i))) 0 inits))
       kn   (? sfq (self >< (gn + cn)) (gn + cn))         ; every resolvable free head + the self name (amble-ok?'s membership test)
       lpok (? lp ((all name? vars) && (adistinct vars) && (k = (tally inits))
                   && !(member? go vars) && !(member? go ps)) 1)
       ok ((two? s) && (two? >s) && (all name? ps) && (adistinct ps)
           && (a = (tally ps)) && (0 < a) && lpok && drok && (two? body)
           && (? (getenv "LOVE_AMBLE_NOCALL") !(ahascall? env go body) 1)              ; the call-lane bisect knob (forensics: splits call machinery from the rest)
           && (all lit? gvals) && (nd < 48) && ((k + (tally gn) + (tally cn) + nq) < 48)   ; slots are cheap once the prologue GUARDS the room (below)
           && (all (\ i (amble-ok? ps kn 0 0 0 i)) inits)
           && (amble-ok? env kn go k 1 body))
       (? ok
          (: _ (? (getenv "LOVE_AMBLE_TRACE") (puts ("amble: " + (show s) + "\n")) 0)   ; the compile trace (forensics: names every ambled source)
             ng (1 + (tally gn) + (tally cn) + nq + (? (lit? eqv) 1 0))   ; extras region = [() | globals | sibling entries | quoted data | =]: () is
                                                                ; the empty MINT (li 1 would be the charm 0-fossil), entries + quotes
                                                                ; are HEAP values -- all ride the nifx extras chain into GC-walked slots
             R (k + 3 + ng + nd)                            ; loopvars | Se | So | BUDGET | extras | spills
             A (greg tgt 'acc)  T (greg tgt 'tmp)  X (greg tgt 'anchor)  SC (greg tgt 'scratch)
             SP (greg tgt 'sp)  IP (greg tgt 'ip)  HP (greg tgt 'hp)
             seo (8 * k)  soo (8 * (k + 1))  budo (8 * (k + 2))  gbo (8 * (k + 3))  spo (8 * (k + 3 + ng))  fro (8 * R)
             (voff v) (? (member? v vars) (8 * (posof v vars 0)) (fro + (8 * (posof v ps 0))))
             (spill d) (spo + (8 * d))
             eqo (gbo + (8 * (1 + (tally gn) + (tally cn) + nq)))   ; the = value's extras slot (past globals, siblings, quotes)
             (fx2 lbl) [['mov X T] ['and X X A] ['test X 1] ['br 'eq lbl]]    ; both T,A fixnum, else lbl
             (abin sa sb d fin) ((acg sa d) + [['st SP (spill d) A]] + (acg sb (d + 1)) + [['ld T SP (spill d)]] + fin)   ; T = a, A = b
             (aar op sa sb d)
              (abin sa sb d
               (? (id? op '+) ((fx2 'deopt) + [['sub A A 1] ['adds A A T] ['br 'vs 'deopt]])         ; 2y + (2x+1) = tag(x+y)
                  (id? op '-) ((fx2 'deopt) + [['subs T T A] ['br 'vs 'deopt] ['lea A T 1]])         ; 2(x-y) then tag
                  ((fx2 'deopt) + [['sar A 1] ['sub T T 1] ['mulo A A T SC 'deopt] ['or A A 1]]))) ; y * 2x = 2xy (mulo IS the tag-range gate) then tag
             (acmp2 op sa sb d) (abin sa sb d ((fx2 'deopt) + [['cmp T A] ['set (ccof op) A] ['leax A A A 1 1]]))
             (adm op sa sb d) (abin sa sb d                                                        ; // %: untag, /-by-0 -> twin, divir (trunc toward zero), retag
              ((fx2 'deopt) + [['sar T 1] ['sar A 1] ['test A A] ['br 'eq 'deopt]]                 ; (the adds gate catches most-negative // -1)
               + (divir tgt op)
               + [['adds A A A] ['br 'vs 'deopt] ['or A A 1]]))
             (abit op sa sb d) (abin sa sb d                                                       ; & | ride the tags whole (2x+1 op 2y+1 = tag(x op y)); ^ re-tags
              ((fx2 'deopt) + (? (id? op '^) [['xor A A T] ['or A A 1]] [[(bitop op) A A T]])))
             ; aeq2: the GENERIC = -- total, never a deopt. equal fixnums and the same point
             ; answer 1; distinct fixnums, and a point against ANYTHING else, answer 0 (a
             ; point equals only itself); everything structural -- strings, gems (the numeric
             ; bridges and NaN), bigs, lists -- drives the = value off its extras slot.
             (aeq2 sa sb d) (: ys (mint 0)  no (mint 0)  sl (mint 0)  dn (mint 0)  ne1 (mint 0)  et (mint 0)
              (abin sa sb d
               ([['cmp T A] ['br 'ne ne1]
                 ['test A 1] ['br 'ne ys]                                                          ; equal fixnums -> 1
                 ['ld X A 0] (jk tgt SC 3) ['cmp X SC] ['br 'eq ys]                              ; the same point -> 1
                 (jk tgt SC 4) ['cmp X SC] ['br 'eq ys] ['jmp sl]                                ; the same heap box: structural (NaN) -> drive
                 ['label ne1] ['test T 1] ['br 'eq et]
                 ['test A 1] ['br 'ne no]                                                          ; distinct fixnums -> 0
                 ['ld X A 0] (jk tgt SC 3) ['cmp X SC] ['br 'eq no]                              ; a point equals only itself -> 0
                 (jk tgt SC 4) ['cmp X SC] ['br 'eq no] ['jmp sl]
                 ['label et] ['ld X T 0] (jk tgt SC 3) ['cmp X SC] ['br 'eq no]
                 (jk tgt SC 4) ['cmp X SC] ['br 'eq no] ['jmp sl]
                 ['label ys] ['li A 3] ['jmp dn]
                 ['label no] ['li A 1] ['jmp dn]
                 ['label sl] ['st SP (spill (d + 1)) A] ['mov A T]]                              ; slow: spill b ; A = a
                + (adrive eqo)                                                                     ; A = (= a)
                + [['st SP (spill d) A] ['ld A SP (spill (d + 1))]]                              ; spill the partial ; A = b
                + (adrive (spill d))                                                               ; A = (= a b)
                + [['label dn]])))
             (aid sa sb d) (abin sa sb d [['cmp T A] ['set 'eq A] ['leax A A A 1 1]])                ; raw word identity: total, no guard
             (atwo sx d) (: ln (mint 0)
              ((acg sx d) + [['li T 1] ['test A 1] ['br 'ne ln]
                              ['ld X A 0] (jk tgt A 0) ['cmp X A] ['br 'ne ln]
                              ['li T 3] ['label ln] ['mov A T]]))
             (acap sx d off) ((acg sx d) + [['test A 1] ['br 'ne 'deopt]
                                             ['ld X A 0] (jk tgt T 0) ['cmp X T] ['br 'ne 'deopt]
                                             ['ld A A off]])
             (alink sa sb d) (abin sa sb d [['lea X HP 88] ['cmp SP X] ['br 'below 'deopt]          ; room for 3w + floor, else deopt (no GC mid-native)
                                              (jk tgt X 0) ['st HP 0 X] ['st HP 8 T] ['st HP 16 A]
                                              ['mov A HP] ['add HP HP 24]])
             (acq st sth sel d) (: le (mint 0)  ln (mint 0)
              ((acg st d) + [['test A 1] ['br 'eq 'deopt] ['cmp A 1] ['br 'le le]]                 ; heap truth -> net -> twin; tag(x)<=1 <=> x<=0
               + (acg sth d) + [['jmp ln] ['label le]] + (acg sel d) + [['label ln]]))
             (aor as d) (: en (mint 0)                                                                      ; (|| a..): the first TRUTHY value, single-eval, short-circuit --
              (((: (go l) (? !(two? >l) (acg <l d)                                                 ; each arg lands in A; charm truth keeps it (heap truth -> the twin,
                             ((acg <l d)                                                                ; acq's bet); the last arg IS the value either way
                              + [['test A 1] ['br 'eq 'deopt] ['cmp A 1] ['br 'gt en]]
                              + (go >l)))) as)
               + [['label en]]))
             (adrive cso) (: bb (mint 0)                                                                    ; apply the value at Sp[cso] to the arg in A, via the WALKABLE resume:
              [['st SP -32 A] ['ld T SP cso] ['st SP -24 T]                                         ; frame [arg, callee, tag(bb-entry0), entry0] -- an odd charm + an out-of-
                 ['lar T bb] ['lar X 'entry0] ['subs T T X] ['leax T T T 1 1]                       ; pool code base, so a GC with the call pending walks it clean (no retB)
                 ['st SP -16 T] ['st SP -8 X]
                 (jk tgt IP 8) ['sub SP SP 32] ['ld T IP 0] ['jmpr T]                  ; -> lvm_ap -> callee -> lvm_resume -> bb
                 ['label bb] ['ld A SP 0] ['add SP SP 8]])                                            ; resume: result at Sp[0], pop it
             (achain as cso d) (? !(two? >as)
                                  ((acg <as d) + (adrive cso))
                                  ((acg <as d) + (adrive cso) + [['st SP (spill d) A]]
                                   + (achain >as (spill d) (d + 1))))                                  ; partial -> spill -> next arg (currying)
             (astr1 cso) (: ld (mint 0) ok (mint 0) en (mint 0)                                             ; the STRING fast lane at a 1-arg frame-callee site: (s i) with s a
              ([['ld T SP cso] ['test T 1] ['br 'ne ld]                                             ; text and i a charm is a byte index: in range, the unsigned byte,
                  ['ld X T 0] (jk tgt SC 1) ['cmp X SC] ['br 'ne ld]                             ; native. EVERYTHING ELSE falls to the DRIVE (never a deopt -- a
                  ['test A 1] ['br 'eq ld]                                                              ; mixed-type call site keeps its closure speed): another callee or
                  ['mov X A] ['sar X 1] ['ld SC T 8]                                                  ; index kind, and -- since the one unsigned compare wraps a negative
                  ['cmp X SC] ['br 'below ok]                                                           ; huge -- BOTH the from-the-end read and the out-of-range (). so the
                  ['jmp ld]                                                                             ; lane answers what C answers, at the cost of driving a negative.
                  ['label ok] ['ldxb A T X 1 16] ['leax A A A 1 1]
                  ['jmp en] ['label ld]]
               + (adrive cso)
               + [['label en]]))
             (atally sx d) (: ld (mint 0) en (mint 0)                                                       ; (tally X) with X a text: A = tag(len at [X+8]); anything else
              ((acg sx d)                                                                                   ; DRIVES the tally global through its gn slot (a chain tally is a
               + [['test A 1] ['br 'ne ld]                                                            ; walk -- C's, not a deopt storm)
                    ['ld X A 0] (jk tgt SC 1) ['cmp X SC] ['br 'ne ld]
                    ['ld X A 8] ['leax A X X 1 1] ['jmp en]
                    ['label ld]]
               + (adrive (gbo + (8 * (1 + (posof 'tally gn 0)))))
               + [['label en]]))
             ; asat: a SATURATED n-ary call site (n >= 2). if the callee is a cur cell of arity
             ; exactly n (bytecode closures, natives, and the nifs share the [cur arity body..]
             ; layout), lay the apn frame [args cont tag base] and enter its body at cell+2
             ; directly -- one call, no curry chain, no partials. any other callee -- a partial,
             ; an over-application, arity 1, data -- takes the drive chain over the SAME spilled
             ; args (evaluated once, either way).
             (asat as cso d) (: bb (mint 0)  sw (mint 0)  dn (mint 0)  n (tally as)  w (n + 3)
              ((agous as 0 d)
               + [['ld X SP cso] ['test X 1] ['br 'ne sw]
                  ['ld T X 0] (jk tgt SC 9) ['cmp T SC] ['br 'ne sw]                               ; entry = lvm_cur?
                  ['ld T X 8] ['cmp T ((2 * n) + 1)] ['br 'ne sw]]                                 ; arity = n?
               + (: (gsl j) (? (j < n) ([['ld A SP (spill (d + j))] ['st SP ((8 * j) - (8 * w)) A]] + (gsl (j + 1))) ()) (gsl 0))
               + [(jk tgt T 8) ['add T T 8] ['st SP ((8 * n) - (8 * w)) T]                         ; cont = &resume[1]
                  ['lar T bb] ['lar A 'entry0] ['subs T T A] ['leax T T T 1 1] ['st SP ((8 * (n + 1)) - (8 * w)) T]
                  ['st SP ((8 * (n + 2)) - (8 * w)) A]
                  ['lea IP X 16] ['sub SP SP (8 * w)] ['ld T IP 0] ['jmpr T]                      ; enter the body at cell+2
                  ['label bb] ['ld A SP 0] ['add SP SP 8] ['jmp dn]
                  ['label sw]]
               + (: (goj j so) (? (j < n)                                                          ; the chain, from the spills: each partial rolls through spill d
                                  ([['ld A SP (spill (d + j))]] + (adrive so)
                                   + (? (j < (n - 1)) [['st SP (spill d) A]] ())
                                   + (goj (j + 1) (spill d)))
                                  ()) (goj 0 cso))
               + [['label dn]]))
             (acall f as d)
              (? (id? f self)                                                                               ; SELF: saturated -> the direct frame jump; partial/over-applied ->
                 (? ((tally as) = (a - ni))                                                                 ; curry the own cell through its imports, then the args (a saturated
                    (aselfcall as d)                                                                        ; TAIL self-call took sgo instead)
                    (achain (simp + as) soo d))
                 (member? f cn)                                                                             ; a SIBLING: its frozen entry takes the import names (all in the frame
                 (: sas ((cimp f) + as)  sso (gbo + (8 * (1 + (tally gn) + (posof f cn 0))))            ; by fixcaps) then the args -- lz's site, saturated when it can be
                    (? (two? >sas) (asat sas sso d) (achain sas sso d)))
                 (: cso (? (member? f vars || member? f ps) (voff f) (gbo + (8 * (1 + (posof f gn 0)))))
                    (? ((? lp 1 sfq) && !(two? >as) && (member? f vars || member? f ps))              ; a 1-arg frame callee can be a STRING at run time (the lexer class) --
                       ((acg <as d) + (astr1 cso))                                                     ; LOOPING bodies only (named-let or self-call), the same cost gate as
                       (two? >as) (asat as cso d)                                                      ; gn; multi-arg saturates when the callee's arity agrees
                       (achain as cso d))))                                                                 ; the loop, but on a one-shot recursion wrapper it taxed EVERY drive
                                                                                                            ; (~7 insns x ~400M mooncc calls = +19%). a gn or sibling callee is
                                                                                                            ; never a string (lit?-gated / a woven entry), so those keep the bare drive.
             (agous us j d) (? (two? us) ((acg <us (d + j)) + [['st SP (spill (d + j)) A]] + (agous >us (j + 1) d)) ())
             (agomv j d) (? (j < 0) () ([['ld A SP (spill (d + j))] ['st SP (8 * j) A]] + (agomv (j - 1) d)))
             (ago as d) ((agous as 0 d) + (agomv (k - 1) d) + [['jmp 'top]])                            ; updates from OLD vars (spilled first), then the back-edge
             (asmv j d) (? (j < 0) () ([['ld A SP (spill (d + j))] ['st SP (fro + (8 * (ni + j))) A]] + (asmv (j - 1) d)))
             ; aselfcall: a SATURATED non-tail self-call jumps the blob's own entry directly --
             ; frame [imports.. args.. RESUME tag(bb-entry0) entry0] under Sp, Ip restored off So,
             ; and the callee's hand-rolled ret returns through lvm_resume to bb. no drive, no
             ; curry chain, no partials; the entry's room + budget guards run per level.
             (aselfcall as d) (: bb (mint 0)  w (a + 3)
              ((agous as 0 d)
               + (: (gimp l i) (? (two? l) ([['ld T SP (voff <l)] ['st SP ((8 * i) - (8 * w)) T]] + (gimp >l (i + 1))) ()) (gimp simp 0))
               + (: (garg j) (? (j < (a - ni)) ([['ld T SP (spill (d + j))] ['st SP ((8 * (ni + j)) - (8 * w)) T]] + (garg (j + 1))) ()) (garg 0))
               + [(jk tgt T 8) ['add T T 8] ['st SP ((8 * a) - (8 * w)) T]                          ; cont = &resume[1]: ret jumps the cell's word 0 = lvm_resume
                  ['lar T bb] ['lar X 'entry0] ['subs T T X] ['leax T T T 1 1] ['st SP ((8 * (a + 1)) - (8 * w)) T]
                  ['st SP ((8 * (a + 2)) - (8 * w)) X]
                  ['ld IP SP soo]]
               + (? (a = 1) () [['add IP IP 16]])
               + [['sub SP SP (8 * w)] ['jmp 'entry0]
                  ['label bb] ['ld A SP 0] ['add SP SP 8]]))
             (aselfgo as d) ((agous as 0 d) + (asmv (a - ni - 1) d) + [['jmp 'sgo]])               ; saturated tail self-call: new args (spilled first) over the arg slots (imports keep), jump the body top
             (acqt st sth sel d) (: le (mint 0)  ln (mint 0)                                       ; acq, tails threaded
              ((acg st d) + [['test A 1] ['br 'eq 'deopt] ['cmp A 1] ['br 'le le]]
               + (acgt sth d) + [['jmp ln] ['label le]] + (acgt sel d) + [['label ln]]))
             (acgt e d)                                                                             ; tail-position acg: thread tails through ? so a saturated self-call jumps
              (? (aquote? e) (acg e d)
                 !(two? e) (acg e d)
                 (: op <e  as >e
                    (? (id? op '?) (acqt <as <>as <>>as d)
                       (sfq && (id? op self) && ((tally as) = (a - ni))) (aselfgo as d)
                       (acg e d))))
             (acg e d)
              (? (name? e) [['ld A SP (voff e)]]
                 (whole? e) [['li A ((2 * e) + 1)]]
                 (id? e amble-nil) [['ld A SP gbo]]
                 (aquote? e) [['ld A SP (gbo + (8 * (1 + (tally gn) + (tally cn) + (aposq e qs 0))))]]   ; a quoted datum: its extras slot (past the globals + sibling entries)
                 (: op <e  as >e
                    (? (id? op '?) (acq <as <>as <>>as d)
                       (id? op 'two?) (atwo <as d)
                       (id? op 'cap) (acap <as d 8)
                       (id? op 'cup) (acap <as d 16)
                       (id? op 'id?) (aid <as <>as d)
                       (id? op 'link) (alink <as <>as d)
                       (id? op '= && (aqnom? <as || aqnom? <>as))
                        (aid <as <>as d)                                               ; (a = 'sym): a POINT equals only itself (the point-= law) -> raw identity, total
                       (id? op '= && (lit? eqv)) (aeq2 <as <>as d)
                       (acmp? op) (acmp2 op <as <>as d)
                       (member? op '(+ - *)) (aar op <as <>as d)
                       (member? op '(// %)) (adm op <as <>as d)
                       (member? op '(& | ^)) (abit op <as <>as d)
                       (id? op 'tally && !(member? 'tally env) && member? 'tally gn && !(two? >as))
                        (atally <as d)                                       ; the tally fast lane (env-shadowed or multi-arg falls to the call paths)
                       (id? op '|| && !(member? op env) && two? >as) (aor as d)
                       (id? op go) (ago as d)
                       (acall op as d))))
             initR (: (goi j) (? (j < R) ([['st SP (8 * j) T]] + (goi (j + 1))) ())
                      ([['li T 1]] + (goi 0)))                                                          ; every reserved slot a VALID value before any call (GC walks them)
             unpk ([['ld T IP 8] ['st SP seo T]]                                                  ; Se = value[1] = e (plain -- image-revert-clean)
                   + (? (a = 1) [['st SP soo IP]] [['mov T IP] ['sub T T 16] ['st SP soo T]])     ; So = the own cell (= Ip, or Ip-16 past the cur pair) -- the self-drive callee
                   + [['ld T IP 32]                                                               ; T = the nifx EXTRAS chain
                       ['st SP budo T] ['ld T T 16]]                                                    ; link 0 = the budget CELL (saved for the deopt decrement), slots after it
                   + (: (gou j) (? (j < ng) ([['ld X T 8] ['st SP (gbo + (8 * j)) X] ['ld T T 16]] + (gou (j + 1))) ()) (gou 0)))
             initsir (: (goj is j) (? (two? is) ((acg <is 0) + [['st SP (8 * j) A]] + (goj >is (j + 1))) ())
                        (goj inits 0))
             bodyir ((? sfq ['(label sgo)] ())                                                     ; the self-jump landing: a fresh invocation on the rewritten frame
                     + (? lp (initsir + ['(label top)] + (acgt body 0)) (acgt body 0)))   ; body = GB|M DESUGARED (adesug) -- never compile the raw forms
             epi [['add SP SP fro] ['st SP 0 A]                                                       ; hand-rolled lvm_ret (a drive call clobbered Ip)
                    ['ld IP SP (8 * a)] ['st SP (8 * a) A] ['add SP SP (8 * a)]
                    ['ld T IP 0] ['jmpr T]]
             dpt (['(label deopt)
                      ['ld X SP budo] ['ld SC X 8] ['sub SC SC 2] ['st X 8 SC]                      ; budget -= 1 (tagged: 2k+1 - 2): a RESTART is the profitability signal
                      ['ld T SP seo] ['add SP SP fro]]                                                  ; (d0 room-guard entries never count); twin from its slot BEFORE the restore
                  + (? (a = 1) [['mov IP T]] [['lea IP T 16]])                                      ; a=1: re-apply e; else: enter e's body on the saturated frame
                  + [['ld T IP 0] ['jmpr T]])
             prog (['(label entry0)                                                                       ; entry0 = the resume-offset anchor (any shared label works; the blob's first byte)
                      ['lea X HP (40 + (8 * (R + 4 + (amax a nd))))] ['cmp SP X] ['br 'below 'd0]    ; ROOM guard: the reservation + the widest call frame (nd bounds any site's arg count) + floor must fit Sp-Hp,
                      ['ld T IP 32] ['ld X T 8] ['cmp X 1] ['br 'le 'd0]                            ; the DEOPT BUDGET (extras link 0, a charm): spent -> the native is
                      ['sub SP SP fro]]                                                                   ; RETIRED, every call takes d0 straight to the twin (frame untouched
                                                                                                            ; -> the twin runs it whole; a perma-deopting native heals to bytecode)
                   + initR + unpk + bodyir + epi + dpt
                   + ['(label d0) ['ld T IP 8]] + (? (a = 1) [['mov IP T]] [['lea IP T 16]]) + [['ld T IP 0] ['jmpr T]])
             (nifx (emit (asmt tgt prog)) e s a (8 >< (amble-nil >< gvals + cvals + (map (\ q <>q) qs) + (? (lit? eqv) [eqv] ())))))
          0))
   ; --- N-VARIABLE loop: the general tail-recursive counted loop ------------------
   ; (\ n ((: (go v0..vk) (? TEST (go U0..Uk) RESULT)) I0..Ik)), up to 3 loop vars in
   ; r9/r10/r11 (RAW), the param n the tagged Sp[0] (fxv-guarded). each iteration: if
   ; TEST, compute EVERY Uj from the OLD regs -- push all, then pop into the regs in
   ; reverse -- so the updates read pre-update values (e.g. fib's `b` and `a+b`); else
   ; RESULT. r9/r10/r11 survive the expr scratch (rax/r8/stack), so no save needed.
   ; lvreg: loop var j -> its abstract register (arch-aware, the loopregs bank: x86 IR r8/r9/r10 = phys
   ; r9/r10/r11 ; arm64 r6/r7/r8 = x6/x7/x8, dodging scratch/anchor).
   (lvreg tgt j) (nthl (loopregs tgt) j)
   (loopcode-n n vars updates test result inits tgt)
    ; the whole N-var loop in neutral IR, both targets. nvld loads a var: n from Sp[0] (fxvir, guarded), a
    ; loop var from its register. updates are computed from the OLD regs (all pushed at depth j, then popped
    ; reverse so reg(j) <- Uj). control flow + per-depth deopt + the putfix epilogue are all label-resolved.
    (: A (greg tgt 'acc)
       nvld (\ e d (? (e = n) (fxvir tgt 0 d)
                       (member? e vars) [['mov A (lvreg tgt (argix e vars 0))]]
                       ()))
       pre  ((: (go j is) (? (two? is) ([['li (lvreg tgt j) <is]] + (go (j + 1) >is)) ())) 0 inits)        ; reg-init
       ups  ((: (go j us) (? (two? us) ((cgvir tgt nvld <us j) + [['push A]] + (go (j + 1) >us)) ())) 0 updates)   ; push Uj at depth j
       pops ((: (go j) (? (j < 0) () ([['pop (lvreg tgt j)]] + (go (j - 1))))) ((tally updates) - 1))                ; pop reg(k-1)..reg(0)
       tb   (cgvir tgt nvld test 0)
       rb   (cgvir tgt nvld result 0)
       (asmt tgt (pre + '((label top))
              + tb + [['cmp A 0] '(br eq end)]           ; <test>; acc==0 (false) -> end
              + ups + pops + '((jmp top) (label end))      ; updates ; loop top
              + rb + (loopepi tgt))))                      ; result -> acc ; putfix + Continue
   ; (jitloop-n lam vars updates test result inits) -- a general N-var loop -> native (x86 self-test helper).
   (jitloop-n lam vars updates test result inits)
    (nif (emit (loopcode-n <>lam vars updates test result inits 'amd64)) (ev lam) lam 1)
   ; --- FLOAT lane (SSE) -- unboxed doubles in xmm ------------------------------
   ; the LuaJIT lever: keep float values in registers, never boxed. xmm0 is the float
   ; accumulator; a nested operand spills to the machine stack (movsd [rsp]). peek is
   ; mopped, so a float CONSTANT is materialized arithmetically -- fbits computes its
   ; IEEE-754 bits (the 52-bit mantissa makes frac*2^52 EXACT, no rounding). LANE LAW:
   ; the float lane carries NO overflow guard (doubles saturate to inf, never promote
   ; to bignum); only the INTEGER lane keeps the jno->deopt that preserves bignum
   ; promotion. this first cut: a float PREDICATE leaf (\ x (cmp <fexpr> <fexpr>)) over
   ; a FIXNUM arg -> 0/1 -- no gem args (deferred: needs the lvm_gembox type guard); a
   ; float RESULT is now `jitfr` below (the Have-checked box). a non-fixnum arg DEOPTS (fxv).
   (fbits x)                                                          ; finite double -> its IEEE-754 u64 bits
    (? (x = 0.0) 0
       (: s (? (x < 0.0) 1 0) ax (? (x < 0.0) (0.0 - x) x)
          eu ((: (go e) (? (((e + 1) 2.0) <= ax) (go (e + 1)) e)) (int ((log ax) / (log 2.0))))   ; raise (log error)
          e  ((: (go e) (? (ax < (e 2.0)) (go (e - 1)) e)) eu)                                     ; lower
          ((s * 9223372036854775808) + ((e + 1023) * 4503599627370496) + (int (((ax / (e 2.0)) - 1.0) * 4503599627370496.0)))))
   ; the float spill rides the machine stack at the loop/group slot size (x86 8 / arm64 16, for sp
   ; alignment), so a mid-nest deopt (deoptbody, slotsz*d) unwinds it correctly.
   (xpushir tgt) [['sub 'sp 'sp (slotsz tgt)] '(stsd sp 0 f0)]   ; spill xmm0/d0 -> [sp]
   (xpop1ir tgt) ['(ldsd f1 sp 0) ['add 'sp 'sp (slotsz tgt)]]   ; reload [sp] -> xmm1/d1
   (fconst tgt c) (: A (greg tgt 'acc) [['li A (fbits c)] ['movqxr 'f0 A]])   ; bits -> gpr ; gpr -> xmm0 (movq / fmov)
   (fbinop op) (? (op = '+) '((addsd f0 f1))                         ; f0 = f0 + f1
                  (op = '*) '((mulsd f0 f1))                         ; f0 = f0 * f1
                  (op = '-) '((subsd f1 f0) (movsd f0 f1))           ; f1 = f1 - f0 ; f0 = f1
                  (op = '/) '((divsd f1 f0) (movsd f0 f1))           ; f1 = f1 / f0 ; f0 = f1
                  ())                                                ; reg-reg double IR (two-address: x86 SSE / arm64 scalar-double), both targets
   ; cgf: a FLOAT subexpr over {x (the fixnum param -> cvtsi2sd) ; numeric consts} -> xmm0/d0, as neutral
   ; crew/holo/ IR (both targets). the param load is fxvir (fixnum-guarded, inline deopt) then int->double.
   (cgf tgt v e d) (? (e = v)   ((fxvir tgt 0 d) + [['cvtsi2sd 'f0 (greg tgt 'acc)]])   ; param: fixnum-guarded untag in acc, cvtsi2sd
                  (gem? e)  (fconst tgt e)                           ; a float constant
                  (whole? e)(fconst tgt (e + 0.0))                   ; an int constant -> promote to double
                  (two? e) (: op <e a <>e b <>>e
                     (? (member? op '(+ - * /)) ((cgf tgt v a d) + (xpushir tgt) + (cgf tgt v b (d + 1)) + (xpop1ir tgt) + (fbinop op)) ()))
                  ())
   ; cgfp: a float PREDICATE (cmp A B) -> acc 0/1 (A->xmm1, B->xmm0; ucomisd then set). x86-only (the
   ; ucomisd-flag conditions are x86 unsigned cc); jitf (its sole caller) is an x86 self-test helper.
   (cgfp tgt v e d) (: op <e a <>e b <>>e
      cc (? (op = '<) 'below (op = '<=) 'be (op = '>) 'above (op = '>=) 'ae 'eq)
      ((cgf tgt v a d) + (xpushir tgt) + (cgf tgt v b (d + 1)) + (xpop1ir tgt) + '((ucomisd f1 f0)) + [['set cc (greg tgt 'acc)]]))   ; ucomisd xmm1,xmm0 ; acc = (cc) ? 1 : 0
   ; (jitf '(\ x (cmp <fexpr> <fexpr>))) -- a float predicate over a fixnum arg -> native 0/1 (x86 helper).
   (jitf lam) (: v <>lam (nif (emit (asmx ((cgfp 'amd64 v <>>lam 0) + (loopepi 'amd64)))) (ev lam) lam 1))
   ; --- HEAP RESULT (the deferred "Have-checked box"): (jitfr '(\ x <fexpr>)) returns a GEM, not 0/1.
   ; the lane never allocates because it can't GC (its raw regs aren't roots) -- so it does the alloc
   ; twin of the overflow guard: a ROOM GUARD (Sp - Hp >= gem_req+floor words) DEOPTS to interp when
   ; the heap is too full to fit the box, exactly as `jo` deopts when a fixnum result leaves range.
   ; fast path (room proven): write [Hp]=lvm_gembox (the jk gem slot, read off g like the
   ; string guard), the double at [Hp+8] with NaN->0 (mk_gem's law), return Hp UNTAGGED (an even heap
   ; ptr IS the gem), bump Hp by gem_req. the epilogue stores the POINTER (NO putfix) and threads the
   ; advanced Hp on through Continue, so the box is live to the next op and the GC at the next safepoint.
   ; nanc: NaN -> 0.0 in f0 (mk_gem's law). x86: ucomisd xmm0,xmm0 ; jnp +4 ; xorpd (the parity cc +
   ; self-jump as a raw blob). arm64: fcmp d0,d0 ; b.vc ok ; (d0 <- 0) ; ok -- unordered sets V, so vc
   ; skips when not-NaN; zero via a gpr-0 moved into d0.
   (nanc tgt) (? (id? tgt 'amd64) [['raw '(102 15 46 192  123 4  102 15 87 192)]]
                 (: A (greg tgt 'acc) ok (mint 0)
                    (['(ucomisd f0 f0) ['br 'vc ok]] + [['li A 0] ['movqxr 'f0 A]] + [['label ok]])))
   (jitfr lam tgt) (jitfrx lam tgt (ev lam))                        ; 2-arg public: bakes the interpreter deopt (auto-ev + the glaze-x86 tests)
   ; jitfrx: the deopt-parameterized core. auto-ev/tests reach it through jitfr ((ev lam)); the natjit
   ; creation hook (love/glaze/hook.l) calls it DIRECTLY with `e`, the bytecode twin ala already built --
   ; re-interpreting lam there would re-enter ala -> natjit -> ... (the leaf lane's `e`-deopt law).
   (jitfrx lam tgt interp)
    (: v <>lam
       A (greg tgt 'acc) S (greg tgt 'sp) HP (greg tgt 'hp) IP (greg tgt 'ip) T (greg tgt 'tmp) ok (mint 0)
       ; neutral crew/holo/ IR (both targets): the room-guard skips the deopt by LABEL; the box store is `stsd`;
       ; the deopt is deoptbody (no pending spills here -- cgf balances its own); nanc is arch-aware.
       (nif (emit (asmt tgt ((cgf tgt v <>>lam 0)
          + [['lea A HP 80] ['cmp S A] ['br 'ae ok]]                  ; cmp Sp,Hp+10w ; room? skip deopt
          + (deoptbody tgt 0) + [['label ok]] + (nanc tgt)             ; heap-full -> deopt ; NaN -> 0.0
          + [(jk tgt T 5) ['st HP 0 T]                                  ; [Hp] = lvm_gembox (box header)
              ['stsd HP 8 'f0]                                           ; [Hp+8] = the double
              ['mov A HP] ['add HP HP 16]                               ; acc = Hp (the gem ptr) ; Hp += 16
              ['st S 0 A] ['add IP IP 16] ['ld T IP 0] ['jmpr T]])))   ; [Sp] = gem ; Continue (NO putfix: acc IS the gem)
        interp lam 1))
   (clook k ce) (? (atom? ce) () (k = <<ce) <ce (clook k >ce))   ; counter env: name -> (name regbytes; cgFd/rgof read it)
   ; --- CONSTANT HOISTING: peek is mopped so a float constant costs a movabs+movq to materialize;
   ;     re-emitting that inside a loop body is the dominant cost. so collect the DISTINCT float
   ;     constants of a kernel, preload each into a dedicated xmm reg (xmm2..7, free -- scratch is
   ;     xmm0/xmm1, locals xmm8..) ONCE before the loops, and reference the register (movsd xmm0,xmm_r)
   ;     in the body. >6 distinct constants spill back to inline fconst (graceful).
   (memv? v l) (? (atom? l) 0 (v = <l) 1 (memv? v >l))
   (fcc e acc) (? (gem? e) (? (memv? e acc) acc (e >< acc))                    ; collect distinct constants of e
                  (whole? e) (: v (e + 0.0) (? (memv? v acc) acc (v >< acc)))
                  (two? e) (fcc <>>e (fcc <>e acc))
                  acc)
   (fccl es acc) (? (two? es) (fccl >es (fcc <es acc)) acc)            ; collect over a list of exprs
   (htbl cs i) (? (atom? cs) () (i > 7) () ([<cs i] >< htbl >cs (i + 1)))   ; value -> reg (2..7)
   (hlook v ht) (? (atom? ht) -1 (v = <<ht) <><ht (hlook v >ht))
   (hcon tgt v ht) (: r (hlook v ht) (? (-1 < r) [['movsd 'f0 (fr r)]] (fconst tgt v)))   ; hoisted const reg -> f0 (movsd) ; else inline fconst -- neutral IR, both targets
   ; --- the OPTIMIZING float-body allocator: value-numbered DAG (CSE) + linear-scan register
   ;     allocation. compiles a SET of root exprs (the inner-loop mag + the fvar updates, which
   ;     SHARE subexprs like zx*zx) to register-only SSE, each value in its own xmm reg so the CPU
   ;     runs independent ops in parallel (ILP). leaves (fvars/consts) live in fixed regs (env);
   ;     binop results allocate from the working pool {0,1,12..15}, freed at last use, reused in
   ;     place where an operand dies -- exactly what the hand kernel did, which cgF above does not.
   (snoc l x) (l + (list x))
   (nthl l i) (? (i = 0) <l (nthl >l (i - 1)))
   (dfind nd dag i) (? (atom? dag) -1 (nd = <dag) i (dfind nd >dag (i + 1)))   ; CSE: existing node?
   (dadd nd dag) (: i (dfind nd dag 0) (? (-1 < i) [i dag] [(tally dag) (snoc dag nd)]))
   (vnum e dag) (? (two? e) (: ra (vnum <>e dag) rb (vnum <>>e <>ra)
                              (dadd [<e <ra <rb] <>rb))            ; binop: (op vn-a vn-b)
                            (dadd ['L e 0] dag))                                       ; leaf: `(datum 0)
   (vnall rs dag acc) (? (two? rs) (: r (vnum <rs dag) (vnall >rs <>r (snoc acc <r))) [acc dag])
   (binop? dag v) !(id? <(nthl dag v) 'L)
   (luse v dag rv n) ((: (go i ds best) (? (atom? ds) (? (member? v rv) n best)         ; last index using v (roots live to n)
       (go (i + 1) >ds (? (binop? dag i && (v = <><ds || v = <>><ds)) i best)))) 0 dag -1)
   (dies? v i dag rv n) (binop? dag v && ((luse v dag rv n) = i && !(member? v rv)))
   (rgof leaf env) (: e (clook leaf env) (? (two? e) <>e (scare 'glaze-leaf leaf)))   ; leaf -> its fixed reg
   (bcat ps) (foldl (\ a b (a + b)) () ps)                                                  ; concat a LIST of byte pieces (is + associative)
   (efold names i) (? (two? names) ([<names i] >< efold >names (i + 1)) ())   ; names -> (name reg) from reg i
   ; --- NESTED GRID OWNER (general, D-dimensional): own a WHOLE counted-loop NEST as ONE native
   ;     kernel -- D nested integer loops over fixnum coords -> M derived-float invariants -> a float
   ;     recurrence over N float-vars, accumulating an integer count -- one boundary crossing per grid,
   ;     not one per cell. Mandelbrot's 64x64 grid is the D=2,M=2,N=2 instance; this handles any D>=1
   ;     and any M,N within the xmm budget (M+N<=6). REGISTERS: counters r10.., invariants xmm8..,
   ;     fvars next, hoisted consts xmm2..7, the inner recurrence's working pool = the leftover xmm.
   ;     r12.. are CALLEE-SAVED (D>=3) -> push/pop, and the acc-overflow deopt restores them. The acc
   ;     (r9) add is GUARDED (lane law -- overflow deopts to the interp nest -> bignum); counters and
   ;     the float lane need no guard. arity 1 (a dummy arg ignored), integer checksum out. offsets COMPUTED.
   (rangeL a b) (? (a < b) (a >< rangeL (a + 1) b) ())
   (map2 f a b) (? (two? a) (f <a <b >< map2 f >a >b) ())
   ; cgFd: a derive expr over the COUNTER env ce (int loop counters -> cvtsi2sd, the IR carried by cenv) +
   ; hoisted consts, using [sp] scratch (xpushir/xpop1ir) so it never touches an xmm8.. local reg -- run
   ; once per cell (cold). Neutral IR, both targets.
   (cgFd tgt ht ce e) (? (gem? e)   (hcon tgt e ht)
                     (whole? e) (hcon tgt (e + 0.0) ht)
                     (atom? e)  (: cb (clook e ce) (? (two? cb) <>cb ()))           ; counter -> its (cvtsi2sd f0 cntr) IR
                     (two? e)   (: op <e a <>e b <>>e
                                   (? (member? op '(+ - * /)) ((cgFd tgt ht ce a) + (xpushir tgt) + (cgFd tgt ht ce b) + (xpop1ir tgt) + (fbinop op)) ()))
                     ())
   (loopcode-gridn dims cnames invs invd fvars updates testexpr cmpop bound lim tgt)
    ; the scalar D-dim grid kernel as a neutral crew/holo/ IR PROGRAM, BOTH targets: control flow is LABELS (no
    ; hand offsets), the recurrence is allocp's CSE'd named-SSE forms, the cold derives (cgFd) are IR too.
    ; k=r7(iter), acc=r8, counters=r9..(gr); SCR=scratch (greg acc), the deopt tail uses greg ip. xmm: consts
    ; f2.., invariants f8.., fvars next, the rest the DAG pool. On x86 D>=3 spills the callee-saved counters
    ; (r12..); on arm64 the counters are caller-saved (x8..), so none. The acc add is GUARDED (lane law).
    (: D (tally dims)  M (tally invs)  N (tally fvars)  SCR (greg tgt 'acc)    ; SCR: integer scratch (fbits load + the result)
       cregs (rangeL 10 (10 + D))                                             ; counter regs (phys r10..), outer->inner
       saved (? (id? tgt 'amd64) (rangeL 12 (10 + D)) ())                       ; x86 r12.. callee-saved (D>=3); arm64 counters caller-saved -> none
       fenv (invs + fvars)
       cenv (map2 (\ nm r [nm [['cvtsi2sd 'f0 (gr r)]]]) cnames cregs)    ; counter name -> its (cvtsi2sd f0 cntr) IR (cgFd reads it)
       ht (htbl (fcc bound (fccl invd (fccl updates (fcc testexpr ())))) 2)   ; distinct consts -> xmm2..7
       derives (foldl (\ a p (a + (cgFd tgt ht cenv >p) + [['movsd (fr (8 + <p)) 'f0]])) () (map2 (\ i d (i >< d)) (rangeL 0 M) invd))   ; inv i -> xmm(8+i)
       zerof (foldl (\ a j (a + [['xorpd (fr (8 + M + j)) (fr (8 + M + j))]])) () (rangeL 0 N))   ; fvar j -> xmm(8+M+j) = 0
       aenv ((efold fenv 8) + ht)
       pool ([0 1] + (rangeL (8 + M + N) 16))
       AR (vnall (testexpr >< updates) () ())  rootv <AR  dag <>AR  nn (tally dag)
       AL (allocp dag rootv aenv nn pool 'movsd sseops)  bodyc <AL  rg <>AL
       magr (peep rg <rootv 0)  breg (fr (hlook bound ht))
       wb (foldl (\ a j (a + [['movsd (fr (8 + M + j)) (fr (peep rg (nthl rootv (1 + j)) 0))]])) () (rangeL 0 N))   ; fvar <- its update's reg
       iter (['(label itop) ['cmp 'r7 lim] '(br ge iesc)]                   ; while k<lim AND (cmp mag bound)
             + bodyc + [['ucomisd (fr magr) breg] ['br (ucmpx cmpop) 'iesc]]
             + wb + '((inc r7) (jmp itop) (label iesc)))
       popsv (foldl (\ a r (a + [['pop (gr r)]])) () (rev saved))           ; x86 D>=3: restore callee-saved counters before deopt
       accb (['(adds r8 r8 r7) '(br vc cont)] + popsv + (deoptbody tgt 0) + '((label cont)))   ; ESC: acc += k ; overflow -> deopt
       cell (derives + zerof + '((li r7 0)) + iter + accb)                    ; derive invariants; zero fvars; k=0; recur; acc += k
       (loop-nest ci body) (: cr (gr (nthl cregs ci))  b (nthl dims ci)       ; counted loops outer->inner (single-step)
          tp (intern ("lt" + (show ci)))  en (intern ("le" + (show ci)))
          inner (? (ci = (D - 1)) body (loop-nest (ci + 1) body))
          ([['li cr 0] ['label tp] ['cmp cr b] ['br 'ge en]] + inner + [['inc cr] ['jmp tp] ['label en]]))
       pushsv (foldl (\ a r (a + [['push (gr r)]])) () saved)
       popsv2 (foldl (\ a r (a + [['pop (gr r)]])) () (rev saved))
       preload (foldl (\ a e (a + [['li SCR (fbits <e)] ['movqxr (fr <>e) SCR]])) () ht)   ; load each const
       (pushsv + preload + '((li r8 0)) + (loop-nest 0 cell) + popsv2 + [['mov SCR 'r8]]))   ; IR program: push saved; consts; acc=0; NEST; pop; result -> SCR(=acc)
   ; --- 2-WIDE PACKED-SSE2 grid kernel (loopcode-grid2): the SAME D-dim float-recurrence grid, but the
   ;     INNERMOST dim is processed TWO cells at a time -- lanes [c, c+1] packed [low,high] in each xmm.
   ;     The recurrence runs PACKED (addpd/mulpd/subpd via the crew/holo/ SSE2 lane) so both pixels advance per op; the
   ;     two independent chains hide the mul latency the SCALAR kernel (one z, serial dependency) is bound
   ;     by -- ~2.4x on mandelbrot. The escape test -> a per-lane mask (cmplepd) ANDed into a sticky `alive`;
   ;     the per-lane iteration count is a packed-int vector bumped by `psubq count,alive` (alive lane =
   ;     all-ones = -1, so -=(-1) = +1); the loop runs while ANY lane is alive (movmskpd) AND k<lim. BIT-EXACT
   ;     vs the scalar interp (identical IEEE per-lane ops, same count-until-first-escape-capped-at-lim).
   ;     Gated (grid2-ok?) to the mandelbrot/float shape: D<=2, M+N<=4 (invs/fvars at xmm8..11), <=2 inner
   ;     consts (xmm2,3 -> xmm4,5,6 free for count/alive/scratch), cmpop in {< <=}, even innermost dim. The
   ;     acc adds keep the lane-law overflow guard (deopt to the faithful interp nest). ⚠ x86-64 (SSE2).
   (fr n)   (intern ("f" + (show n)))                                                    ; an xmm NUMBER -> its crew/holo/ IR symbol f0..f15
   (gr p)   (intern ("r" + (show (p - 1))))                                              ; a physical GPR encoding (>=8) -> its abstract-file IR name (r10->r9 ..)
   (sseop op) (? (op = '+) 'addpd (op = '*) 'mulpd (op = '-) 'subpd (op = '/) 'divpd 'movupd)   ; DAG op -> PACKED-double mnemonic (grid2)
   (sseops op) (? (op = '+) 'addsd (op = '*) 'mulsd (op = '-) 'subsd (op = '/) 'divsd 'movsd)   ; DAG op -> SCALAR-double mnemonic (gridn)
   (ucmpx op) (? (op = '<=) 'above (op = '<) 'ae (op = '>=) 'be (op = '>) 'below ())     ; EXIT cc after ucomisd = inverse of the continue cmp
   (allocp dag rv env n pool mv opmap)                                                   ; the float-body allocator (CSE + linear-scan); emits crew/holo/ IR FORMS via mv (move) + opmap (op->mnemonic): scalar OR packed
    (: rg (tablet 0) pl (tablet 0) _ (pin pl 'p pool)
       (pop u) (: p (peep pl 'p ()) _ (pin pl 'p >p) <p)
       (push r) (pin pl 'p (r >< peep pl 'p ()))
       forms ((: (go i acc) (? (i = n) acc
          (: nd (nthl dag i)
             (? (id? <nd 'L) (: _ (pin rg i (rgof <>nd env)) (go (i + 1) acc))
                (: op <nd a <>nd b <>>nd ra (peep rg a 0) rb (peep rg b 0)
                   adie (dies? a i dag rv n) bdie (dies? b i dag rv n)
                   dst (? adie ra (pop 0))
                   _ (? (bdie && !(rb = dst)) (push rb) 0) _ (pin rg i dst)
                   mvc (? (dst = ra) () [[mv (fr dst) (fr ra)]])                       ; move only if dst != a's reg
                   (go (i + 1) (acc + mvc + [[(opmap op) (fr dst) (fr rb)]]))))))) 0 ())
       [forms rg])
   (cmppd-imm op) (? (op = '<=) 2 (op = '<) 1 ())                                        ; cmppd predicate: LE / LT (the "continue while small" escape tests)
   (grid2-ok? dims invs fvars updates testexpr cmpop bound)                              ; the packed shape fits?
    ((member? cmpop '(< <=)) && ((tally dims) < 3) && (1 < (last dims)) && (0 = ((last dims) % 2))
     && (((tally invs) + (tally fvars)) < 5)
     && ((tally (htbl (fcc bound (fccl updates (fcc testexpr ()))) 2)) < 3))
   ; the kernel as a neutral crew/holo/ IR PROGRAM (asmx): control flow is LABELS (no hand-counted offsets),
   ; the SSE ops are NAMED (no raw opcode bytes). GP IR regs: r0=rax(scratch), r5=rsi(Ip, the deopt
   ; tail), r7=k(iter), r8=acc, r9../r10..=the grid counters. XMM: f2,f3=consts, f4=count, f5=alive,
   ; f6=scratch, f8,f9=invariants, f10,f11=fvars, f0,f1,f12..15=the DAG pool. The cold per-cell DERIVES
   ; (cgFd) stay raw bytes spliced via `(raw ..)`.
   (loopcode-grid2 dims cnames invs invd fvars updates testexpr cmpop bound lim)
    (: D (tally dims)  M (tally invs)  N (tally fvars)
       cregs (rangeL 10 (10 + D))  rinner (last cregs)  iir (gr rinner)                  ; iir = inner counter's IR reg (bumped for lane1)
       fenv (invs + fvars)
       cenv (map2 (\ nm r [nm [['cvtsi2sd 'f0 (gr r)]]]) cnames cregs)               ; counter name -> its (cvtsi2sd f0 cntr) IR (cgFd reads these)
       ht (htbl (fcc bound (fccl updates (fcc testexpr ()))) 2)                          ; ONLY inner-recurrence consts (bound+..) -> f2,f3; derive consts inline -> f4,5,6 free
       breg (fr (hlook bound ht))  pred (cmppd-imm cmpop)
       idx (map2 (\ i d (i >< d)) (rangeL 0 M) invd)                                   ; (inv-index . derive-expr) pairs
       d0 (foldl (\ a p (a + (cgFd 'amd64 ht cenv >p) + [['movsd (fr (8 + <p)) 'f0]])) () idx)       ; inv i lane0 -> low of f(8+i)
       d1 (foldl (\ a p (a + (cgFd 'amd64 ht cenv >p) + [['unpcklpd (fr (8 + <p)) 'f0]])) () idx)    ; inv i lane1 (counter+1) -> high
       derives (d0 + [['inc iir]] + d1 + [['dec iir]])                               ; lane1 = the same derive at counter+1
       zerof (foldl (\ a j (a + [['xorpd (fr (8 + M + j)) (fr (8 + M + j))]])) () (rangeL 0 N))   ; fvars := 0 (both lanes)
       aenv ((efold fenv 8) + ht)
       pool ([0 1] + (rangeL (8 + M + N) 16))
       AR (vnall (testexpr >< updates) () ())  rootv <AR  dag <>AR  nn (tally dag)
       AL (allocp dag rootv aenv nn pool 'movupd sseop)  bodyc <AL  rg <>AL
       magr (peep rg <rootv 0)
       wb (foldl (\ a j (a + [['movupd (fr (8 + M + j)) (fr (peep rg (nthl rootv (1 + j)) 0))]])) () (rangeL 0 N))   ; fvar <- its update's reg
       maskc [['movupd 'f6 (fr magr)] ['cmppd 'f6 breg pred] '(andpd f5 f6) '(psubq f4 f5) '(movmskpd r0 f5)]
       iter (['(label itop) ['cmp 'r7 lim] '(br ge iesc)] + bodyc + maskc             ; while k<lim AND any lane alive
             + '((test r0 r0) (br eq iesc)) + wb + '((inc r7) (jmp itop) (label iesc)))
       deopt (deoptbody 'amd64 0)                                                          ; tail-apply the interp fallback (overflow)
       accb ('((movqrx r0 f4) (adds r8 r8 r0) (br vc cont0)) + deopt                     ; acc += count.lane0 ; guard
             + '((label cont0) (movhlps f6 f4) (movqrx r0 f6) (adds r8 r8 r0) (br vc cont1)) + deopt + '((label cont1)))   ; acc += count.lane1 ; guard
       cell (derives + zerof + '((xorpd f4 f4) (pcmpeqd f5 f5) (li r7 0)) + iter + accb)   ; count=0; alive=ones; k=0; loop; acc
       (loop-nest ci body) (: p (nthl cregs ci) cr (gr p) b (nthl dims ci)               ; counted loops outer->inner; innermost STEPS BY 2
          tp (intern ("lt" + (show ci))) en (intern ("le" + (show ci)))
          inner (? (ci = (D - 1)) body (loop-nest (ci + 1) body))
          step  (? (ci = (D - 1)) [['add cr cr 2]] [['inc cr]])
          ([['li cr 0] ['label tp] ['cmp cr b] ['br 'ge en]] + inner + step + [['jmp tp] ['label en]]))
       preload (foldl (\ a e (a + [['li 'r0 (fbits <e)] ['movqxr (fr <>e) 'r0] ['unpcklpd (fr <>e) (fr <>e)]])) () ht)   ; broadcast each const [c,c]
       (preload + '((li r8 0)) + (loop-nest 0 cell) + '((mov r0 r8))))                    ; IR program (x86): acc=0 ; NEST ; result -> r0(=acc)
   ; (jitfgridn lam cnames dims invs invd fvars updates testexpr cmpop bound lim) -> native D-dim grid.
   ; Tries the 2-wide PACKED kernel when the shape fits (grid2-ok?), else the scalar grid kernel.
   ; the PACKED 2-wide kernel (grid2) is x86-only (NEON has no movmskpd); arm64 uses the scalar gridn.
   ; both kernels return a neutral IR PROGRAM ending with the checksum in acc; jitfgridn appends loopepi
   ; (putfix + Continue) and assembles for `tgt`.
   (jitfgridn lam cnames dims invs invd fvars updates testexpr cmpop bound lim tgt)
    (: pk (? (id? tgt 'amd64 && grid2-ok? dims invs fvars updates testexpr cmpop bound)
             (loopcode-grid2 dims cnames invs invd fvars updates testexpr cmpop bound lim) ())
       prog (? (atom? pk) (loopcode-gridn dims cnames invs invd fvars updates testexpr cmpop bound lim tgt) pk)
       (nif (emit (asmt tgt (prog + (loopepi tgt)))) (ev lam) lam 1))
   ; (jitfgrid src pxn pyn cxd cyd fvars updates testexpr cmpop bound lim tgt) -> the 2D mandelbrot-class
   ; delegate (the original signature) onto the general kernel: D=2 (px py), M=2 invariants (cx cy).
   (jitfgrid lam pxn pyn cxd cyd fvars updates testexpr cmpop bound lim tgt)
    (jitfgridn lam '(px py) [pxn pyn] '(cx cy) [cxd cyd] fvars updates testexpr cmpop bound lim tgt)
   ; --- the AUTO-HOOK recognizer: read a float-recurrence GRID nest from SOURCE and extract the
   ;     jitfgrid structure, so ordinary interpreted code gets glazed transparently (no hand-call).
   ;     matches the shape: a mand recurrence (\ cx cy zx zy it (? (&& (it < LIM) (cmp TEST BOUND))
   ;     (mand ..updates.. (it + 1)) it)) driven by two counted loops col/grid. extracts pxn pyn cxd
   ;     cyd fvars updates testexpr cmpop bound lim, then jitfgrid. exactly 2 invariants (the point)
   ;     + 2 fvars (the iterate); other shapes return () (stay interpreted). see tools/glaze-auto.l.
   (opd e i) (nthl >e i)                                       ; i-th operand of a form
   (classify ps args)                                               ; mand params vs recursive args -> (fvars updates invariants)
    (? (two? ps) (: p <ps a <args r (classify >ps >args)
                    (? (a = p)          [<r <>r (p >< nthl r 2)]            ; invariant (the point)
                       (a = ['+ p 1]) r                                                   ; counter (p + 1)
                                        [(p >< <r) (a >< <>r) (nthl r 2)])) ; fvar + update
                 [() () ()])
   (posof k l i) (? (k = <l) i (posof k >l (i + 1)))
   (grok glam mps mbody cbody gbody tgt)                           ; -> a native grid closure, or () if it doesn't match the shape
    (: cond (opd mbody 0)  then (opd mbody 1)
       c2 (opd cond 1)  lim (opd (opd cond 0) 1)  cmpop <c2  testexpr (opd c2 0)  bound (opd c2 1)
       cl (classify mps >then)  fvars <cl  updates <>cl  invs (nthl cl 2)
       pyn (opd (opd cbody 0) 1)  margs >(opd (nthl >(opd cbody 1) 2) 1)
       cxd (nthl margs (posof (nthl invs 0) mps 0))  cyd (nthl margs (posof (nthl invs 1) mps 0))
       pxn (opd (opd gbody 0) 1)
       (? (2 = (tally invs) && 2 = (tally fvars))
          (jitfgrid glam pxn pyn cxd cyd fvars updates testexpr cmpop bound lim tgt) ()))
   ; --- the NEUTRAL-IR retarget (leaf + n-ary lanes) ---------------------------------------------
   ; the byte-emitting lanes above are x86-64-only (raw byte lists). this lane emits the crew/holo/ neutral
   ; IR instead and assembles for a TARGET ('amd64 or 'arm64), so aarch64 falls out of crew/holo/arm64.l. it is
   ; ADDITIVE -- the live byte path is untouched; auto.l still drives `jit`/`njit`. spec in.
   ; greg: a logical role -> the abstract holo register, PER TARGET. x86 lands g/Ip/Hp/Sp in rdi/rsi/rdx/rcx
   ; (= abstract r6/r5/r2/r1, SysV's first 4 args -- the convention IS the calling convention); aarch64's
   ; AAPCS lands them in x0/x1/x2/x3 (= r0/r1/r2/r3), so the accumulator moves off r0. (anchor = the machine
   ; stack pointer snapshot for the deopt unwind; scratch = mulo/rem's extra reg, unused on x86.)
   (greg tgt role)
    (: x (id? tgt 'amd64)
       (? (id? role 'g)       (? x 'r6 'r0)   (id? role 'ip)      (? x 'r5 'r1)
          (id? role 'hp)      (? x 'r2 'r2)   (id? role 'sp)      (? x 'r1 'r3)
          (id? role 'acc)     (? x 'r0 'r4)   (id? role 'tmp)     (? x 'r7 'r5)
          (id? role 'anchor)  (? x 'r8 'r9)   (id? role 'scratch) (? x 'r4 'r10) ()))
   (ccof op) (? (id? op '<) 'lt (id? op '<=) 'le (id? op '>) 'gt (id? op '>=) 'ge (id? op '=) 'eq ())
   (bitop op) (? (id? op '&) 'and (id? op '|) 'or (id? op '^) 'xor ())   ; source bitwise symbol -> the crew/holo/ IR ALU op

   ; fxir: fetch param i from Sp[i], fixnum-guard (low bit), untag -> acc. a non-fixnum jumps to deopt.
   (fxir tgt i) (: A (greg tgt 'acc) S (greg tgt 'sp) [['ld A S (8 * i)] ['test A 1] '(br eq deopt) ['sar A 1]])
   ; arithir: acc = (the popped a) OP acc, overflow -> deopt. - keeps a-b order; * uses the fused mulo.
   (arithir tgt op) (: A (greg tgt 'acc) T (greg tgt 'tmp) SC (greg tgt 'scratch)
      (? (id? op '+) [['adds A A T] '(br vs deopt)]
         (id? op '-) [['subs T T A] '(br vs deopt) ['mov A T]]
         (id? op '*) [['mulo A A T SC 'deopt]] ()))
   (divir tgt op) (: A (greg tgt 'acc) T (greg tgt 'tmp) SC (greg tgt 'scratch)   ; caller emits the /-by-0 guard
      (? (id? op '//) [['div A T A]] (id? op '%) [['rem A T A SC]] ()))
   ; cgir: an arith expr e over params ps -> IR ops, accumulating in acc; a param fetches from Sp[i] (by
   ; its index in ps). nested operands ride the machine stack (push acc / pop tmp); ? gets fresh mint
   ; labels; one shared deopt label. serves BOTH the leaf (ps = one param) and the n-ary lane.
   (cgir tgt ps e)
    (: A (greg tgt 'acc) T (greg tgt 'tmp)
      (? (member? e ps) (fxir tgt (argix e ps 0))
         (whole? e) [['li A e]]
         (two? e) (: op <e a <>e b <>>e
            (? (member? op '(+ - *))
                 ((cgir tgt ps a) + [['push A]] + (cgir tgt ps b) + [['pop T]] + (arithir tgt op))
               (member? op '(// %))
                 ((cgir tgt ps a) + [['push A]] + (cgir tgt ps b) + [['test A A] '(br eq deopt) ['pop T]] + (divir tgt op))
               (member? op '(& | ^))
                 ((cgir tgt ps a) + [['push A]] + (cgir tgt ps b) + [['pop T] [(bitop op) A A T]])
               (member? op '(< <= > >= =))
                 ((cgir tgt ps a) + [['push A]] + (cgir tgt ps b) + [['pop T] ['cmp T A] ['set (ccof op) A]])
               (id? op '?)
                 (: el (mint 0) en (mint 0)
                    ((cgir tgt ps a) + [['cmp A 0] ['br 'le el]] + (cgir tgt ps b) + [['jmp en] ['label el]] + (cgir tgt ps <>>>e) + [['label en]]))
               ()))
         ()))
   ; deoptir: the shared deopt block -- restore the machine sp from the anchor, then tail-apply the interp
   ; fallback. arity 1 jumps to value[1] (the closure, re-applied to the arg); arity>=2 jumps to value[1]+16
   ; (its BODY, where the saturated args already sit on Sp) -- the lone per-arity difference (mov vs lea).
   (deoptir tgt bodyp) (: AN (greg tgt 'anchor) IP (greg tgt 'ip) T (greg tgt 'tmp)
      [['label 'deopt] ['lea 'sp AN 0] ['ld T IP 8] (? bodyp ['lea IP T 16] ['mov IP T]) ['ld T IP 0] ['jmpr T]])
   ; coreir: the whole-function IR -- prologue (anchor = the machine sp) ; the body ; putfix epilogue +
   ; Continue (Ip+=16; jmp [Ip]) ; the deopt block. (jitcore assembles + installs it; tests assemble it.)
   (coreir tgt ps body deoptblk)
    (: AN (greg tgt 'anchor) S (greg tgt 'sp) A (greg tgt 'acc) IP (greg tgt 'ip) T (greg tgt 'tmp)
       ([['lea AN 'sp 0]] + (cgir tgt ps body) + [['adds A A A] '(br vs deopt) ['or A A 1] ['st S 0 A] ['add IP IP 16] ['ld T IP 0] ['jmpr T]] + deoptblk))
   ; jitcore: assemble coreir for tgt and install as a nat of the given arity (deopt -> interp).
   (jitcore tgt ps body deoptblk arity lam)
    (nif (emit (peep (assemble tgt (coreir tgt ps body deoptblk)) 'bytes 0)) (ev lam) lam arity)
   ; (jitir '(\ x E) tgt) -- an arity-1 leaf. (jitnir '(\ p0 .. E) tgt) -- arity>=2 (natn curries to saturation).
   (jitir lam tgt)  (jitcore tgt [<>lam] <>>lam (deoptir tgt 0) 1 lam)
   (jitnir lam tgt) (: ps (init >lam) body (last >lam) (jitcore tgt ps body (deoptir tgt 1) (tally ps) lam))


   ; --- the LOOP-BODY neutral IR (cgvir): the same arith grammar as cgir, but for the loop lanes, whose
   ; state (n/counter/acc) fills r9/r10/r11 leaving NO free reg for an anchor. So the overflow/div-by-0
   ; guard deopts INLINE -- unwind the d pending machine-stack pushes, then tail-apply interp (value[1] to
   ; the arg still in Sp[0]) -- instead of cgir's anchor-restore. A var is loaded by `varld` (the counter
   ; from its raw register; an n-loop param via fxir). Replaces the byte cgv/cgl + opseq/cmpseq/... lane.
   ; deoptbody: unwind d pending machine-stack pushes (slotsz each -- x86 8 / arm64 16), then tail-apply
   ; interp (value[1]) to the arg still in Sp[0]. NO branch/label of its own; ldeopt/zdeopt/the *-overflow
   ; lane wrap it with the guard branch + a skip label. The loop lanes have no free anchor reg (state fills
   ; the loop registers), so the unwind is by COUNT here, not the leaf/group's anchor-restore.
   (deoptbody tgt d) (: A (greg tgt 'acc) IP (greg tgt 'ip)
      ((? (d > 0) [['add 'sp 'sp ((slotsz tgt) * d)]] ()) + [['ld A IP 8] ['mov IP A] ['ld A IP 0] ['jmpr A]]))
   (ldeopt tgt d) (: ok (mint 0) ([['br 'vc ok]] + (deoptbody tgt d) + [['label ok]]))   ; skip the deopt unless OVerflow
   (zdeopt tgt d) (: ok (mint 0) ([['br 'ne ok]] + (deoptbody tgt d) + [['label ok]]))   ; skip unless nonzero (the /-by-0 AND fixnum-input guard)
   (fxvir tgt i d) (: A (greg tgt 'acc) S (greg tgt 'sp)   ; load Sp[i] TAGGED, fixnum-guard (inline deopt at depth d), untag
      ([['ld A S (8 * i)] ['test A 1]] + (zdeopt tgt d) + [['sar A 1]]))
   (arithl tgt op d) (: A (greg tgt 'acc) T (greg tgt 'tmp) SC (greg tgt 'scratch) dl (mint 0) ok (mint 0)   ; acc = (popped a) OP acc, overflow -> inline deopt
      (? (id? op '+) ([['adds A A T]] + (ldeopt tgt d))
         (id? op '*) ([['mulo A A T SC dl]] + [['jmp ok] ['label dl]] + (deoptbody tgt d) + [['label ok]])   ; mulo sets no arm64 flag -> it branches to the inline deopt (SC the smulh scratch)
         (id? op '-) ([['subs T T A]] + (ldeopt tgt d) + [['mov A T]]) ()))
   (divl tgt op d) (: A (greg tgt 'acc) T (greg tgt 'tmp) SC (greg tgt 'scratch)   ; a in tmp (popped), b in acc -> acc; b==0 deopts
      ([['pop T] ['test A A]] + (zdeopt tgt d) + (? (id? op '//) [['div A T A]] (id? op '%) [['rem A T A SC]] ())))
   (cgvir tgt varld e d) (: A (greg tgt 'acc) T (greg tgt 'tmp)
      (: vl (varld e d)
       (? vl vl                                              ; a var -> its loader IR (acc <- the var, at depth d)
          (whole? e) [['li A e]]
          (two? e) (: op <e a <>e b <>>e
             (? (member? op '(+ - *))
                  ((cgvir tgt varld a d) + [['push A]] + (cgvir tgt varld b (d + 1)) + [['pop T]] + (arithl tgt op d))
                (member? op '(// %))
                  ((cgvir tgt varld a d) + [['push A]] + (cgvir tgt varld b (d + 1)) + (divl tgt op d))
                (member? op '(& | ^))
                  ((cgvir tgt varld a d) + [['push A]] + (cgvir tgt varld b (d + 1)) + [['pop T] [(bitop op) A A T]])
                (member? op '(< <= > >= =))
                  ((cgvir tgt varld a d) + [['push A]] + (cgvir tgt varld b (d + 1)) + [['pop T] ['cmp T A] ['set (ccof op) A]])
                (id? op '?)
                  (: el (mint 0) en (mint 0)
                     ((cgvir tgt varld a d) + [['cmp A 0] ['br 'le el]] + (cgvir tgt varld b d) + [['jmp en] ['label el]] + (cgvir tgt varld <>>>e d) + [['label en]]))
                ()))
          ())))
   ; --- the NEUTRAL-IR group lane (jitgroupir) -- a GROUP of mutually-recursive first-order integer fns
   ;     to native, both targets. `call H_g` is `(call <name>)`, each H opens `(label <name>)`, and crew/holo/
   ;     resolves the rel offsets. IR chunks concatenate FLAT via `holo` (foldl-(+); on lists of IR forms,
   ;     + appends). Covers the full op grammar: int arith, TCO, value-mode cons, string/map/cask lanes.
   ; arg-register ABI: args in the CALLEE-SAVED bank (x86 rbx/r13/r14/r15 = r3/r12/r13/r14; arm64 x19..x22),
   ; the OVF anchor in another callee-saved reg (govf). a call passes args on the MACHINE STACK; each H
   ; saves/loads/restores its own arg regs (so a caller's args survive nested calls) + on arm64 saves `lr`
   ; (bl clobbers it; x86 CALL/RET keeps the return address on the stack). slotsz = the push/pop unit (x86 8,
   ; arm64 16). mov-to/from-sp is `lea` (the ORR-based mov reads/writes XZR for reg 31, never sp).
   (slotsz tgt) (? (id? tgt 'amd64) 8 16)
   (govf tgt)   (? (id? tgt 'amd64) 'r11 'r23)                                    ; the OVF anchor reg: x86 r12 / arm64 x23
   (argregir tgt i) (nthl (? (id? tgt 'amd64) '(r3 r12 r13 r14) '(r19 r20 r21 r22)) i)
   (hsave tgt)  (? (id? tgt 'amd64) () ['(push lr)])                            ; arm64 H spills lr; x86 uses CALL/RET's stack
   (hrest tgt)  (? (id? tgt 'amd64) () ['(pop lr)])
   (saveargsir tgt k) (foldl (\ a i (bcat [a [['push (argregir tgt i)]]])) () (rangeL 0 k))
   (restargsir tgt k) (foldl (\ a i (bcat [a [['pop (argregir tgt ((k - 1) - i))]]])) () (rangeL 0 k))
   (loadargsir tgt k) (foldl (\ a i (bcat [a [['ld (argregir tgt i) 'sp ((slotsz tgt) * ((2 * k) - i))]]])) () (rangeL 0 k))   ; argreg_i <- the passed arg at [sp + slotsz*(2k-i)]
   (garith tgt op) (: A (greg tgt 'acc) T (greg tgt 'tmp) SC (greg tgt 'scratch)   ; acc = (popped a) OP acc, overflow -> OVF
      (? (id? op '+) [['adds A A T] '(br vs OVF)]
         (id? op '-) [['subs T T A] '(br vs OVF) ['mov A T]]
         (id? op '*) [['mulo A A T SC 'OVF]] ()))
   ; --- register/immediate-operand peephole (the cgg lane's, ported into the neutral IR) ---
   ; the generic binary lane is push-acc..pop-tmp per op -- the gap vs a register machine. when one operand
   ; is already a PARAM (in an arg reg) or a small immediate, combine it DIRECTLY: acc OP= reg|imm, no stack
   ; traffic. e.g. primes' (d * d), (n < (d * d)), (d + 1), (0 = ..) all run flat. both targets: the crew/holo/ ALU
   ; takes a reg or an imm for adds/subs/cmp (arm64 imm is 12-bit -> pimm?); mulo is register-only on arm64,
   ; so the imm lane skips `*` (the param-`*` case -- d*d -- is the one that matters and IS a register).
   (pimm? x) (whole? x && (? (x < 0) (0 - x) x) <= 4095)                     ; a small immediate both arches add/sub/cmp directly
   (preg tgt ps x) (member? x ps && argregir tgt (argix x ps 0))            ; x a param -> its arg reg, else ()
   (garith-r tgt op R) (: A (greg tgt 'acc) SC (greg tgt 'scratch)              ; acc = acc OP R (R a reg), overflow -> OVF
      (? (id? op '+) [['adds A A R] '(br vs OVF)]
         (id? op '-) [['subs A A R] '(br vs OVF)]
         (id? op '*) [['mulo A A R SC 'OVF]] ()))
   (garith-i tgt op imm) (: A (greg tgt 'acc)                                   ; acc = acc OP imm (no '*'), overflow -> OVF
      (? (id? op '+) [['adds A A imm] '(br vs OVF)]
         (id? op '-) [['subs A A imm] '(br vs OVF)] ()))
   (flipc op) (? (id? op '<) '> (id? op '<=) '>= (id? op '>) '< (id? op '>=) '<= op)   ; mirror a cmp's operands (= stays)
   (deoptmn tgt bodyp) (: IP (greg tgt 'ip) T (greg tgt 'tmp)                    ; tail-apply interp (value[1]); bodyp -> its BODY (+16, arity>=2)
      [['ld T IP 8] (? bodyp ['lea IP T 16] ['mov IP T]) ['ld T IP 0] ['jmpr T]])
   ; cggir: a group expr e over params ps (in arg regs). syms = the group fn names (call targets). a call
   ; pushes its args, `call H_g`, pops them; arith/div overflow & /-by-0 jump to the shared OVF handler.
   (cggir tgt syms ps e)
    (: A (greg tgt 'acc) T (greg tgt 'tmp)
      (? (member? e ps) [['mov A (argregir tgt (argix e ps 0))]]
         (whole? e) [['li A e]]
         (two? e) (: op <e a <>e b <>>e
            (? (member? op syms)
                 (bcat [(gargsir tgt syms ps >e) [['call op]] [['add 'sp 'sp ((slotsz tgt) * (tally >e))]]])
               (member? op '(+ - *))
                 (: pa (preg tgt ps a) pb (preg tgt ps b)
                    (? pb                                (bcat [(cggir tgt syms ps a) (garith-r tgt op pb)])   ; (E op p): E->acc; acc OP= p_reg
                       (pimm? b && !(id? op '*))       (bcat [(cggir tgt syms ps a) (garith-i tgt op b)])    ; (E op C)
                       (pa && member? op '(+ *))       (bcat [(cggir tgt syms ps b) (garith-r tgt op pa)])   ; (p op E): commute
                       (pimm? a && id? op '+)         (bcat [(cggir tgt syms ps b) (garith-i tgt op a)])    ; (C + E): commute (imm '*' has no arm64 form -> generic)
                       (bcat [(cggir tgt syms ps a) [['push A]] (cggir tgt syms ps b) [['pop T]] (garith tgt op)])))   ; generic
               (member? op '(// %))
                 (bcat [(cggir tgt syms ps a) [['push A]] (cggir tgt syms ps b) [['test A A] '(br eq OVF) ['pop T]] (divir tgt op)])
               (member? op '(& | ^))
                 (bcat [(cggir tgt syms ps a) [['push A]] (cggir tgt syms ps b) [['pop T] [(bitop op) A A T]]])
               (member? op '(< <= > >= =))
                 (: pa (preg tgt ps a) pb (preg tgt ps b)
                    (? pb         (bcat [(cggir tgt syms ps a) [['cmp A pb] ['set (ccof op) A]]])             ; (E op p): cmp acc,p_reg
                       (pimm? b)  (bcat [(cggir tgt syms ps a) [['cmp A b]  ['set (ccof op) A]]])             ; (E op C)
                       pa         (bcat [(cggir tgt syms ps b) [['cmp pa A] ['set (ccof op) A]]])             ; (p op E): flags p-acc = a-b, cc kept
                       (pimm? a)  (bcat [(cggir tgt syms ps b) [['cmp A a]  ['set (ccof (flipc op)) A]]])     ; (C op E): cmp acc,C = b-a, flip cc
                       (bcat [(cggir tgt syms ps a) [['push A]] (cggir tgt syms ps b) [['pop T] ['cmp T A] ['set (ccof op) A]]])))   ; generic
               (id? op '?)
                 (: el (mint 0) en (mint 0)
                    (bcat [(cggir tgt syms ps a) [['cmp A 0] ['br 'le el]] (cggir tgt syms ps b)
                            [['jmp en] ['label el]] (cggir tgt syms ps <>>>e) [['label en]]]))
               (id? op 'tally) (: R (argregir tgt (argix a ps 0)) [['ld A R 8]])                        ; (tally STR): acc = the len at [STR+8] (raw)
               (id? op 'peep) (: R (argregir tgt (argix a ps 0)) d <>>>e inl (mint 0) endl (mint 0)   ; (peep STR i d): i unsigned-vs-len, OOB -> d, else byte [STR+i+16]
                  (bcat [(cggir tgt syms ps b)
                         [['ld T R 8] ['cmp A T] ['br 'below inl] ['li A (? (whole? d) d 0)] ['jmp endl] ['label inl] ['ldxb A R A 1 16] ['label endl]]]))
               (id? op 'pin) (: R (argregir tgt (argix a ps 0)) val <>>>e     ; (pin C i V): store V's low byte at C's backing[i] (r8=[C+8]=backing str), return C
                  (bcat [(cggir tgt syms ps b) [['push A]] (cggir tgt syms ps val)         ; i -> acc, push; V -> acc
                         [['pop T] ['ld 'r8 R 8] ['stxb 'r8 T 1 16 A] ['mov A R]]]))      ; pop T(=i); r8=backing; [backing+i+16]=V.lo; acc=C
               (id? op 'mpeep) (: R (argregir tgt (argix a ps 0)) d <>>>e lp (mint 0) ht (mint 0) ms (mint 0) en (mint 0)   ; (mpeep MAP k d): open-addressed probe -> the value (raw) in acc, gap-miss -> d
                  (bcat [(cggir tgt syms ps b) [['leax 'r10 A A 1 1] ['mov A R]] (relabel (mpeep-ir (? (whole? d) d 0)) lp ht ms en 'deopt)]))   ; k -> acc; r10 = tagged key (2k+1); acc = MAP; the probe (labels gensym'd)
               (id? op 'mpin) (: R (argregir tgt (argix a ps 0)) val <>>>e lp (mint 0) ht (mint 0) ms (mint 0) en (mint 0)   ; (mpin MAP k v): probe + UPDATE-in-place; a grow-due MISS -> OVF (full unwind+deopt); returns MAP
                  (bcat [(cggir tgt syms ps val) [['push A]] (cggir tgt syms ps b) [['leax 'r10 A A 1 1] ['mov A R]] (relabel mpin-ir lp ht ms en 'OVF) [['mov A R]]]))   ; v -> acc, push (spill); k -> acc; r10=tag(k); acc=MAP; probe+store (the `(pop r8)` tail pops v); acc=MAP (threaded)
               (id? op 'two?) (: z (mint 0) en (mint 0)                                       ; (two? E): is E a chain? -> 1/0, TOTAL (no deopt -- a non-chain reads 0)
                  (bcat [(cggir tgt syms ps a) [['test A 1] ['br 'ne z] ['ld T A 0] (jk tgt 'r8 0) ['cmp T 'r8] ['br 'ne z] ['li A 1] ['jmp en] ['label z] ['li A 0] ['label en]]]))
               (id? op 'cap) (bcat [(cggir tgt syms ps a) [['test A 1] ['br 'ne 'OVF] ['ld T A 0] (jk tgt 'r8 0) ['cmp T 'r8] ['br 'ne 'OVF] ['ld A A 8]]])    ; <E: chain-guard -> OVF (non-chain deopts; always under two?), then acc=[E+8]
               (id? op 'cup) (bcat [(cggir tgt syms ps a) [['test A 1] ['br 'ne 'OVF] ['ld T A 0] (jk tgt 'r8 0) ['cmp T 'r8] ['br 'ne 'OVF] ['ld A A 16]]])   ; >E: chain-guard, then acc=[E+16]
               ()))
         ()))
   (gargsir tgt syms ps as) (? (atom? as) () (bcat [(cggir tgt syms ps <as) [['push (greg tgt 'acc)]] (gargsir tgt syms ps >as)]))
   ; relabel: gensym the map probe's FIXED labels (loop/hit/miss/end) per call site, and point deopt at the
   ; given group label -- so the same mprobe-ir/mpeep-ir/mpin-ir IR can be spliced N times in ONE group
   ; assemble without label collisions.
   (relabel ir lp ht ms en dp) (map (\ f (map (\ x (? (id? x 'loop) lp (id? x 'hit) ht (id? x 'miss) ms (id? x 'end) en (id? x 'deopt) dp x)) f)) ir)
   ; consir: build a chain cell (cons) on the heap -> acc. ROOM-GUARD first (Sp - Hp >= 3w + floor, else
   ;   br OVF -> deopt, so no GC fires mid-native), then [Hp]=the chain kind (jk), [Hp+8]=cap(=tmp), [Hp+16]=cup(=acc),
   ;   acc=Hp (an even ptr IS the value, no putfix), Hp+=3w. r9/r10 are
   ;   free scratch in the group ABI (args r3/r12-r14, anchor r11, acc r0, tmp r7, sp r1, hp r2, ip r5).
   (consir tgt) (: A (greg tgt 'acc) T (greg tgt 'tmp) S (greg tgt 'sp) HP (greg tgt 'hp)
      [['lea 'r9 HP 88] ['cmp S 'r9] '(br below OVF)                             ; room for 3w+floor; Sp < Hp+88 -> heap full -> deopt
        (jk tgt 'r10 0) ['st HP 0 'r10]                                      ; [Hp] = chain header
        ['st HP 8 T] ['st HP 16 A]                                               ; [Hp+8] = cap (the popped a); [Hp+16] = cup (b)
        ['mov A HP] ['add HP HP 24]])                                            ; acc = Hp (result ptr); bump Hp by 3 words
   ; cggvir -- VALUE-mode codegen: e yields an AI VALUE (heap ptr or tagged scalar) in acc, NOT a raw int.
   ;   int-const C -> tagged value | param -> re-tag the raw arg reg (2p+1) | (link A B) cons (A,B values) |
   ;   (? T A B) int test via cggir, value branches | (g E..) a value-group CALL (args INT via cggir).
   (cggvir tgt syms ps e)
    (: A (greg tgt 'acc) T (greg tgt 'tmp)
      (? (whole? e) [['li A ((2 * e) + 1)]]                                        ; the tagged VALUE C
         (member? e ps) (: R (argregir tgt (argix e ps 0)) [['leax A R R 1 1]])    ; param: acc = 2*reg + 1 (re-tag)
         (two? e) (: op <e
            (? (member? op syms) (cggir tgt syms ps e)                              ; a value-group call -> cggir's call (result is the value)
               (id? op 'link) (bcat [(cggvir tgt syms ps <>e) [['push A]] (cggvir tgt syms ps <>>e) [['pop T]] (consir tgt)])   ; A;push;B;pop T(=A); cons(T,A)
               (id? op '?) (: el (mint 0) en (mint 0)
                  (bcat [(cggir tgt syms ps <>e) [['cmp A 0] ['br 'le el]] (cggvir tgt syms ps <>>e)
                         [['jmp en] ['label el]] (cggvir tgt syms ps <>>>e) [['label en]]]))
               ()))
         ()))
   ; cggtir: e in TAIL position. a tail SELF-call (selfnm, matching arity) -> compute the new args (from the
   ;   OLD arg regs) onto the machine stack, pop them into the arg regs, then jmp Lbody (the body-start, past
   ;   save/load) -- the frame is REUSED, so a counted loop is O(1) stack, no call/ret per iteration. a tail ?
   ;   recurses tail into both branches (the test stays non-tail). everything else -> cggir (value in acc),
   ;   falling through to restore+ret. self-only (a sibling tail-call stays a plain `call`, as the byte path).
   (cggtir tgt syms ps e selfnm Lbody)
    (: A (greg tgt 'acc) k (tally ps)
       (? (two? e && id? <e selfnm && (tally >e) = k)                    ; TAIL SELF-CALL -> loop in place
            (bcat [(gargsir tgt syms ps >e)                                      ; push the new args (all from the OLD regs)
                   (foldl (\ a i (bcat [a [['pop (argregir tgt ((k - 1) - i))]]])) () (rangeL 0 k))   ; pop into the arg regs, reverse
                   [['jmp Lbody]]])
          (two? e && id? <e '?)                                              ; TAIL ? -> test non-tail, both branches tail
            (: el (mint 0) en (mint 0)
               (bcat [(cggir tgt syms ps <>e) [['cmp A 0] ['br 'le el]] (cggtir tgt syms ps <>>e selfnm Lbody)
                      [['jmp en] ['label el]] (cggtir tgt syms ps <>>>e selfnm Lbody) [['label en]]]))
          (cggir tgt syms ps e)))                                                     ; any other tail expr -> value in acc
   ; mkhir: one function. label; [push lr]; save arg regs; load the passed args; Lbody:; body; restore; [pop lr]; ret.
   (mkhir tgt syms nm ps body)
    (: k (tally ps) Lbody (mint 0)
       (bcat [[['label nm]] (hsave tgt) (saveargsir tgt k) (loadargsir tgt k) [['label Lbody]]
               (? (hasl body) (cggvir tgt syms ps body) (cggtir tgt syms ps body nm Lbody))   ; VALUE body (conses) -> cggvir (no TCO); int body -> cggtir (TCO)
               (restargsir tgt k) (hrest tgt) ['(ret)]]))
   ; mkouterir: the entry's OUTER -- save anchor+arg regs; anchor=sp; fixnum-guard+push the args; call
   ; H_entry; pop args; restore; putfix + Continue; OVF (mov sp,anchor -> abandon frames; restore) -> DEOPT.
   (mkouterir tgt k entry arity valt smask mmask bufmask cmask pmask)              ; valt: VALUE-typed entry (heap ptr / tagged scalar) -> store acc AS-IS, no putfix. the masks
    (: A (greg tgt 'acc) S (greg tgt 'sp) IP (greg tgt 'ip) T (greg tgt 'tmp) OVFR (govf tgt) ss (slotsz tgt)   ; partition the entry's EXTERNAL args: int (sar) / string|map|cask (kind-guard, raw) / chain (raw, no guard) / point (nom|mint-guard, raw)
       strk 1 mapk 2 bufk 6                                                            ; the jk slots of the kind sentinels the raw-param guards check against
       (pusharg1 i) (: o (8 * i)                                                   ; one external arg: load Sp[i] -> acc, guard by type, push
          (? (member? i cmask) [['ld A S o] ['push A]]                          ; chain: raw, NO guard (two? total; cap/cup deopt a non-chain inline)
             (member? i smask || (member? i mmask || member? i bufmask))     ; string/map/cask: a fixnum can't be a heap obj (-> OVF), and [arg]'s kind must match (-> OVF); push RAW
               (: kind (? (member? i mmask) mapk (member? i bufmask) bufk strk)
                  [['ld A S o] ['test A 1] '(br ne OVF) ['ld T A 0] (jk tgt 'r8 kind) ['cmp T 'r8] '(br ne OVF) ['push A]])   ; a FIXNUM (odd, br ne) deopts; then the kind word must match (br ne)
             (member? i pmask)                                                     ; point key: nom OR mint, pushed RAW -- word identity IS its whole equality;
               (: pok (mint 0)                                                     ;   a charm deopts (only interp knows the numeric bridges, (2 = 2.0))
                  [['ld A S o] ['test A 1] '(br ne OVF)
                    ['ld T A 0] (jk tgt 'r8 3) ['cmp T 'r8] ['br 'eq pok]
                    (jk tgt 'r8 4) ['cmp T 'r8] '(br ne OVF) ['label pok] ['push A]])
             [['ld A S o] ['test A 1] '(br eq OVF) ['sar A 1] ['push A]]))    ; int: fixnum-guard (non-fixnum -> OVF), untag (sar), push
       pushargs (foldl (\ a i (bcat [a (pusharg1 i)])) () (rangeL 0 k))
       epi (? valt [['st S 0 A] ['add IP IP 16] ['ld T IP 0] ['jmpr T]]                                  ; VALUE: store acc as-is; Continue (no putfix)
                 [['adds A A A] '(br vs DEOPT) ['or A A 1] ['st S 0 A] ['add IP IP 16] ['ld T IP 0] ['jmpr T]])   ; INT: putfix (overflow -> deopt); Continue
       (bcat [(hsave tgt) [['push OVFR]] (saveargsir tgt k) [['lea OVFR 'sp 0]] pushargs
              [['call entry] ['add 'sp 'sp (ss * k)]] (restargsir tgt k) [['pop OVFR]] (hrest tgt)   ; save/restore lr around the `call entry`: on arm64 `bl` clobbers x30, but x30 is the live return addr the threaded tail-call chain eventually `ret`s through -- the OUTER tail-jumps onward (br), so it must hand x30 back intact (no-op on x86: CALL/RET ride the stack)
              epi
              [['label 'OVF] ['lea 'sp OVFR 0]] (restargsir tgt k) [['pop OVFR]] (hrest tgt)
              [['label 'DEOPT]] (deoptmn tgt (arity > 1))]))
   ; jgir-ok? -- the op-COVERAGE gate: does jitgroupir implement every op this group uses (jgir-supported)?
   ; autogroup routes here when true, else leaves the group INTERPRETED (the val fallback in auto.l). Reached
   ; ONLY after the recognizer (auto.l groupok?/vok?/cok?/grpfix) has proven the group glazeable -- shapes,
   ; arities, the int/chain/value typing, the interprocedural string inference; so jgir-ok? does NOT re-derive
   ; the grammar, its lone job is op coverage. jgir-supported now spans the full recognizer grammar, so the
   ; gate is vacuously true for any admitted group -- it stays as a soundness backstop.
   jgir-supported '(+ - * // % & | ^ < <= > >= = ? link peep tally pin mpeep mpin two? cap cup)   ; the full op set cggir/cggvir handle
   (jgir-ops-ok? syms e) (? (two? e)                                            ; every op head in e is a group call or a supported op; recurse into every operand
      (: op <e ((member? op syms || member? op jgir-supported) && all (\ x (jgir-ops-ok? syms x)) >e))
      (? (gem? e || twin? e) 0 1))                                          ; a SMOOTH-numeric leaf (float/complex) is out of the integer group lane -> deopt (auto.l's recognizer already excludes these; this backstops the natjit path, which has no recognizer)
   (jgir-ok? defs) (: syms (map (\ d <<d) defs)
      (all (\ d (jgir-ops-ok? syms >d)) defs))
   ; jgir-lrf -- the ((mpeep..)(peep..)(mpin..)(pin..) def..) tail the deopt fallback re-evaluates.
   (jgir-lrf entry ds) (? (atom? ds) [entry] (: d <ds (<d >< (>d >< jgir-lrf entry >ds))))
   ; (jitgroupir defs entry tgt) -- defs = ((nameform . body) ..); compile the group, entry as the value.
   ; deopt fallback = (ev <the WHOLE group>): post-rebind `ev` is the glaze, so this RE-GLAZES the group
   ; (harmless: a never-deopting native re-build) -- which is why the mpeep/mpin->peep/pin byte-lane aliases
   ; (ldxb/stxb) MUST assemble on EVERY target (arm64 included). mpeep/mpin are GLAZE-internal (not real
   ; fns), aliased to the core peep/pin (the body reads the PREVIOUS=global binding by the sequence law --
   ; not recursion); harmless for non-map lanes (peep/pin stay the core accessors).
   (jitgroupir defs entry tgt)
    (jitgroupirx defs entry tgt (ev (': >< ('(mpeep mc mk md) >< ('(peep mc mk md) >< ('(mpin mc mk mv) >< ('(pin mc mk mv) >< jgir-lrf entry defs)))))) ())
   ; (jitgroupirx defs entry tgt deopt) -- like jitgroupir but the caller supplies the DEOPT target directly
   ; (the njit-loop/jitfrx precedent). The natjit hook passes `e` (the bytecode twin) so a group-bearing
   ; closure native-backs WITHOUT re-entering the evaluator mid-ala (jitgroupir's (ev ..) fallback would).
   ; PURE otherwise -- holo/emit/assemble only, no ev.
   (jitgroupirx defs entry tgt deopt src)
    (: parse (map (\ d (: nf <d [<nf >nf >d])) defs)            ; (name params body) per fn
       syms (map cap parse)                                                            ; the fn names (call targets)
       efn (gfind (\ f (id? <f entry)) parse)  k (tally (nthl efn 1))  eps (nthl efn 1) eb (nthl efn 2)
       ord (efn >< filter (\ f !(id? <f entry)) parse)                      ; entry first (H_entry sits right after the OUTER)
       valt (hasl eb)                                                                 ; entry value-typed (body conses) -> the value epilogue stores acc as-is
       info (map (\ f [<f (nthl f 1) (nthl f 2) (tally (nthl f 1))]) parse)     ; (name params body arity) -- for the interprocedural string fixpoint
       smap0 (map (\ f (<f >< filter (\ j (suse? (nthl f 2) (nthl (nthl f 1) j))) (rangeL 0 (nthl f 3)))) info)   ; per-fn DIRECT string params (peep/tally/pin)
       smask (gsm entry (smfix info smap0))                                            ; the entry's STRING params, propagated to a fixpoint -> raw pointer, kind-guarded
       mmask (filter (\ j (muse? eb (nthl eps j))) (rangeL 0 k))                         ; entry MAP params (mpeep/mpin)
       bufmask (filter (\ j (buse? eb (nthl eps j))) (rangeL 0 k))                       ; entry CASK params (pin)
       cmask (filter (\ j (cuse? eb (nthl eps j))) (rangeL 0 k))                         ; entry CHAIN params (cap/cup/two?) -> raw, no guard
       pmask (filter (\ j (nuse? eb (nthl eps j) && !(member? j cmask))) (rangeL 0 k))   ; entry POINT keys ((P = <..)) -> raw, nom|mint-guarded; a chain-used param is never a point
       outer (mkouterir tgt k entry k valt smask mmask bufmask cmask pmask)
       Hs (foldl (\ a f (bcat [a (mkhir tgt syms <f (nthl f 1) (nthl f 2))])) () ord)
       elam ('\ >< (nthl efn 1) + [(nthl efn 2)])                              ; the entry's source lambda -- the =/show src FALLBACK
       (nif (emit (peep (assemble tgt (bcat [outer Hs])) 'bytes 0)) deopt (? (two? src) src elam) k))   ; src (the OUTER surface) if given -- transparent vs the bytecode twin; else elam
)
