; emit -- the codegen backend: a love closure compiled to native machine code and
; installed as a transparent applicable value. "ev but faster": the result applies
; by juxtaposition, is =/show identical to its source, and deopts to the interpreter
; on any surprise, so native is never wrong, only faster. the C seam is `nat`
; (love.c): it W^X-installs the bytes and wraps them in a closure cell
; [code, src, code, interp, lvm_ret, 0], value[-1]=src (=/show) and value[1]=interp.
;
; every lane below lays neutral crew/holo/ IR and assembles it per target, so the
; roles are named through `greg`: x86 lands g/Ip/Hp/Sp in rdi/rsi/rdx/rcx, aarch64
; in x0..x3. putfix is `adds A A A` then `or A A 1` -- the overflow flag IS the
; 62-bit fixnum boundary, which is what every arith result gates on.

; nif/nifx -- the C install seam (love.c). the book loses them on every boot
; (src/main.c), so this is the one door onto raw byte installation, and it is the
; glaze's. the RHS is the outer nif: a body-less `:` rebinds, it does not letrec.
(: nif nif  nifx nifx)

(: ; src/main.c brackets this load with (use 'holo) .. ai_unsplice_, so assemble and
   ; assemble-at resolve at compile and fold into every closure (the capture law).
   ; this `:` is body-less, so its bindings leak to the glaze module's own book --
   ; which is why the byte-piece concat helper is `bcat` and never `holo`.
   ; glazeasm holds the assembler hook.l pins at its own install (holo is evaluated
   ; again, native-backed, after the glaze); unpinned, the module's current one serves.
   ; it leads the letrec: a value a lambda reads must precede every lambda reaching it.
   glazeasm {}
   (le8 n) (: u (n < 0 ? (n + 18446744073709551616) n)                        ; & is broken on a bignum >= 2^63,
              ((: (go i) (i < 8 ? ((u // (i 256)) % 256 . go (i + 1)) ())) 0))   ; so //% splits n into 8 LE bytes
   (le4 n) ((: (go i) (i < 4 ? ((n >> (8 * i)) & 255 . go (i + 1)) ())) 0)  ; n -> 4 LE bytes (offsets < 2^31, & ok)
   (emit l) (cask l)                       ; the charlist lane fills in C -- this was a pin per byte
   (jit lam) (jitir lam 'amd64)            ; (jit '(\ x E)): an arity-1 arith leaf -> the neutral-IR lane
   (argix e ps i) (atom? ps ? 0  (e = <ps) i  (argix e >ps (i + 1)))   ; a param -> its frame index
   (jitn lam) (jitnir lam 'amd64)          ; (jitn '(\ p0 .. E)): arity>=2 (natn curries to saturation)
   (jit2 lam) (jitn lam)                                                  ; arity-2 alias (self-test/auto.l)
   (gfind p l) (atom? l ? () (p <l) <l (gfind p >l))     ; first elt matching p
   ; a blob names no address of the binary: the kind sentinels and the callout drives are read
   ; off g at run time (love_int.h's JkX slots), so the bytes ride any base and any image
   (jk tgt r k) ['ld r (greg tgt 'g) ((jkoff 0) + (8 * k))]   ; 0 chain 1 str 2 map 3 nom 4 mint 5 gem 6 cask 7 drive 8 resume 9 cur 10 unc
   mapmix 0x9e3779b97f4a7c15                                       ; = love.c's map hash multiplier, for the inline probe
   (hasm tgt ir) (: f (glazeasm 'f) ((lit? f ? f (from 'holo 'assemble)) tgt ir))   ; the pinned assembler, else the module's
   (asmx ir) ((hasm 'amd64 ir) 'bytes)                                          ; assemble neutral IR -> flat bytes, x86
   (asmt tgt ir) ((hasm tgt ir) 'bytes)                                       ; ... target-aware (the loop/float lanes that run on both arches)
   ; the map probe, laid by the crew/holo/ assembler so hit/miss/loop resolve as labels.
   ; reg map: holo r0=rax, r7=x86-r8, r8=x86-r9, r9=x86-r10, r10=x86-r11. it wants the map
   ; in rax and the tagged key in r11 (holo r10), and ends branched to hit (match) / miss (gap).
   mprobe-ir ['(ld r7 r0 8) '(ld r8 r7 16) '(shr r8 1) '(dec r8) '(add r7 r7 24)    ; r8=backing; r9=cap; >>1; --(=mask); r8=&slots
      ['li 'r0 mapmix] '(imul r0 r0 r10) '(rol r0 32) '(and r0 r0 r8) '(mov r9 r0)    ; rax=rot(mix*tagkey)&mask=I -> r10
      '(label loop) '(leax r0 r9 r9 1 0) '(ldx r0 r7 r0 8 0) '(cmp r0 r10) '(br eq hit)  ; slot[I].key == tagkey? -> hit
      '(test r0 1) '(br eq miss) '(inc r9) '(and r9 r9 r8) '(jmp loop)]                  ; gap (even) -> miss; else I=(I+1)&mask, loop
   (mpeep-ir d) (mprobe-ir                                                          ; read tails: miss -> default d; hit -> slot[I].value, untagged
      + [['label 'miss] ['li 'r0 d] '(jmp end)
         '(label hit) '(leax r0 r9 r9 1 0) '(ldx r0 r7 r0 8 8) '(sar r0 1) '(label end)])
   (asmx-seed ir nm addr) (: t (pin {} nm addr) f (glazeasm 'at)
                             (((lit? f ? f (from 'holo 'assemble-at)) 'amd64 ir t) 'bytes))   ; one pre-bound external label nm->addr (the glaze's deopt handler)
   ; the same probe with write tails, so `pin` is insert-or-update. a miss (a gap at slot I)
   ; with the table near full needs a grow, which native cannot do, so it deopts through an
   ; external label the caller seeds; `(tablet n)` presizes so a counted insert loop never gets
   ; there. keys and values are fixnums, so no GC write barrier is needed.
   mpin-ir (mprobe-ir + '((label miss)
      (ld r0 r7 -16) (sar r0 1) (cmp r0 r8) (br ge deopt)                           ; len >= mask(=cap-1) -> table near-full -> deopt (r8 still holds the mask)
      (leax r0 r9 r9 1 0) (stx r7 r0 8 0 r10)                                       ; slot[I].key = tagged key
      (pop r8) (leax r8 r8 r8 1 1) (stx r7 r0 8 8 r8)                               ; slot[I].value = tag(v)
      (ld r0 r7 -16) (add r0 r0 2) (st r7 -16 r0) (jmp end)                         ; len++ (putcharm: +2)
      (label hit) (leax r0 r9 r9 1 0) (pop r8) (leax r8 r8 r8 1 1) (stx r7 r0 8 8 r8)
      (label end)))
   ; the param-kind probes: how does the group body use P? each answer picks the entry
   ; guard mkouterir lays for that arg slot.
   (hasl e) (two? e ? (id? <e 'link || foldl (\ a x (a || hasl x)) 0 >e) 0)   ; e contains a (link ..)? -> the group is value-typed
   (suse? e P) (? (atom? e) 0  (member? <e '(peep tally pin) && id? <>e P) 1  (foldl (\ a x (a || suse? x P)) 0 >e))   ; a string: (peep P..)/(tally P)/(pin P..)
   (buse? e P) (? (atom? e) 0  (id? <e 'pin && id? <>e P) 1  (foldl (\ a x (a || buse? x P)) 0 >e))   ; a cask: (pin P..), so the guard wants the buf kind, not str
   (cuse? e P) (? (atom? e) 0  (member? <e '(cap cup two?) && id? <>e P) 1  (foldl (\ a x (a || cuse? x P)) 0 >e))   ; a chain: <P />P /(two? P) -- raw, no entry guard, since two? is total
   (muse? e P) (? (atom? e) 0  ((id? <e 'mpeep || id? <e 'mpin) && id? <>e P) 1  (foldl (\ a x (a || muse? x P)) 0 >e))   ; a map: (mpeep P k d) read or (mpin P k v) update -- raw, map-guarded
   ; a point key -- (P = <..) or (<.. = P) -- is raw word identity, guarded nom|mint: a point
   ; equals only itself, where a charm could bridge (2 = 2.0), so charms deopt to interp
   (nuse? e P)
    (? (atom? e) 0
       (e @ ('= x1 y1) ((id? x1 P && two? y1 && member? <y1 '(cap cup))
                        || (id? y1 P && two? x1 && member? <x1 '(cap cup)))
            0)
        1
       (foldl (\ a x (a || nuse? x P)) 0 >e))
   ; interprocedural string-param inference: a param is a string if used in peep/tally or
   ; passed as the arg to a sibling's string param, to a deduped fixpoint over the group's
   ; (name . smask) map. smasks only grow, so sm-size stabilizes and monofix (prel) converges.
   (dedup l) (atom? l ? () (member? <l >l) (dedup >l) (<l . dedup >l))   ; unique elements
   (gsm nm smap) (atom? smap ? () (id? nm <<smap) ><smap (gsm nm >smap))                ; nm's smask in the (name . smask) map (() absent)
   (fnsm fp body smap)
    (dedup ((: (walk e acc)
              (two? e ?
                (: gm (gsm <e smap)                                                                 ; if <e is a group fn, its string-param indices
                   a2 (atom? gm ? acc (foldl (\ ac j (: arg (>e j) (member? arg fp ? (posof arg fp 0 . ac) ac))) acc gm))   ; a string arg that is our param -> our string param
                   (foldl (\ ac x (walk x ac)) a2 >e))
                acc))
              body (filter (k \ suse? body (fp k)) (rangeL 0 #fp))))                        ; seed acc = the direct peep/tally params
   (sm-size smap) (foldl (\ a e (a + #>e)) 0 smap)                                          ; total mask width, the ascending measure
   (smfix info smap) (monofix smap (st \ map (f \ <f . fnsm (f 1) (f 2) st) info) sm-size)
   ; the counted fold loop: ACC = the fold of body(CV) over CV < N, N the tagged Sp[0], cmp the
   ; test the loop runs while.
   (foldinit op)  (op = '* ? 1 0)                                 ; * folds from 1, + from 0
   ; loopregs: N / counter / accumulator, caller-saved on both targets so they survive the body's
   ; acc/tmp/scratch. arm64 picks x6/x7/x8 to dodge scratch=x10 and anchor=x9.
   (loopregs tgt) (id? tgt 'amd64 ? '(r8 r9 r10) '(r6 r7 r8))
   ; loopepi: the result is already in acc -- putfix, then Continue (Ip += 16 ; jmp [Ip]).
   (loopepi tgt) (: A (greg tgt 'acc) S (greg tgt 'sp) IP (greg tgt 'ip) T (greg tgt 'tmp)
      ([['adds A A A]] + (ldeopt tgt 0) + [['or A A 1] ['st S 0 A] ['add IP IP 16] ['ld T IP 0] ['jmpr T]]))
   (exitcc cmp) (cmp @ '< 'ge  '<= 'gt  '> 'le  '>= 'lt  '= 'ne  ())   ; exit on the inverse of the continue-comparison
   (loopcode body cv op cmp tgt)
    (: rs (loopregs tgt) N (rs 0) CV (rs 1) ACC (rs 2) A (greg tgt 'acc) S (greg tgt 'sp)
       bodyir (cgvir tgt (\ e d (e = cv && [['mov A CV]])) body 0)      ; body -> acc; the counter is read from CV
       accir  (op @ '+ ([['adds ACC ACC A]] + (ldeopt tgt 0))                                  ; ACC += acc ; overflow -> inline deopt
                  '* (: dl (mint 0) ok (mint 0)                                                ; ACC *= acc ; mulo sets no arm64 flag, so it branches
                       ([['mulo ACC ACC A (greg tgt 'scratch) dl]] + [['jmp ok] ['label dl]] + (deoptbody tgt 0) + [['label ok]]))
                  ())
       pre  [['ld N S 0] ['sar N 1] ['li CV 0] ['li ACC (foldinit op)]]   ; N = Sp[0]>>1 (assumed fixnum) ; CV = 0 ; ACC = init
       (asmt tgt (pre + ['(label top) ['cmp CV N] ['br (exitcc cmp) 'end]]   ; cmp CV,N ; exit when (cmp CV N) false
              + bodyir + accir + [['inc CV] '(jmp top) '(label end) ['mov A ACC]] + (loopepi tgt))))   ; CV++ ; loop ; acc <- ACC ; putfix + Continue
   ; (jitsum body) -- compile (\ n (sum_{i<n} body(i))) to a native counted loop (x86 self-test helper).
   (jitsum body)
    (: src ['\ 'n [[': ['go 'i 'a] ['? ['< 'i 'n] ['go ['+ 'i 1] ['+ 'a body]] 'a]] 0 0]]
       (nif (emit (loopcode body 'i '+ '< 'amd64)) (ev src) src 1))
   ; --- the stackless call-out drive ---------------------------------------------------------
   ; a counted loop over (clos i) that calls out without a re-entrant C frame: the blob builds
   ; the frame on Sp, points Ip at callout_drive (love.c, {lvm_ap, lvm_ret0}) and tail-jumps, and
   ; the result lands back at the blob's own resume label bb. `lar` materializes bb's address
   ; pc-relatively (rip-lea / ADR) -- la's page-relative ADRP would mis-page in the word-aligned
   ; mmap blob. so the callee runs entirely on the VM stack: it grows Sp, never the C stack.
   ; the loop state is 6 reserved Sp slots, a GC root, so a callee GC relocates it for free:
   ;   R[0]=SA sum  R[1]=SC counter  R[2]=SN bound  R[3]=Sclos closure  R[4]=Sbase blob base
   ;   R[5]=Stwin the interp twin ; the arg n rides at R[6], the caller continuation at R[7].
   ; two inline gates deopt to that twin, so the native answer is exact or absent: the resume
   ; untags the callout result, so an even (heap) result deopts; and the SA fold is adds + br vc,
   ; so an out-of-range sum re-runs in interp and promotes to a bignum.
   ; value[1] is the PLAIN twin -- the image-dump revert reads it as-is, and a pack chain there
   ; data-applies. `codeopt` restores Sp to entry (Sp[0] = the original tagged n) and continues
   ; into the twin, discarding the native partial SA as any loop deopt does.
   ; jitcalloutx stamps the caller's src at value[-1] for =/show; post-egg has no `poke`.
   ; the Sp offsets are VM words, the same on both targets -- slotsz only matters for push/pop,
   ; which the callout never uses.
   ; coacc / coend fold the callout result (tagged at Sp[0], Sp = R-8 at bb) into SA @ Sp[8]:
   ; `+` keeps SA tagged from init tag(0)=1 and adds the even 2r; `*` keeps it raw from 1 and
   ; retags in coend. both deopt to codeopt, which wants Sp = R-8, so coend's `*` path subs 8.
   (coacc tgt op) (: A (greg tgt 'acc) SP (greg tgt 'sp) T (greg tgt 'tmp) SC (greg tgt 'scratch)
      (id? op '* ? [['sar A 1] ['ld T SP 8] ['mulo T T A SC 'codeopt] ['st SP 8 T]]                        ; SA(raw) *= untag(r) ; overflow -> deopt
         [['sub A A 1] ['ld T SP 8] ['adds T T A] ['br 'vc 'sok] ['jmp 'codeopt] ['label 'sok] ['st SP 8 T]]))  ; SA(tagged) += 2r ; overflow -> deopt
   (coend tgt op) (: A (greg tgt 'acc) SP (greg tgt 'sp)
      (id? op '* ? [['ld A SP 0] ['adds A A A] ['br 'vc 'etag] ['sub SP SP 8] ['jmp 'codeopt] ['label 'etag] ['or A A 1]]  ; SA raw -> tag(SA) ; overflow -> deopt (Sp R -> R-8 first)
         [['ld A SP 0]]))                                                                                       ; SA already tagged
   ; a computed call-out arg: the per-step term is (F ARG), ARG an expr over the counter rather than
   ; the bare counter. caargir lowers ARG to acc RAW -- exprp guarantees the counter is the only
   ; variable, so a name IS the counter and wants no fixnum guard. guards and overflow branch to the
   ; callout's own `deopt`, which unwinds to codeopt and the interp twin; nesting rides the machine
   ; stack, independent of VM Sp, so coff is constant across sub-exprs.
   (caargir tgt e coff)
    (: A (greg tgt 'acc) S (greg tgt 'sp) T (greg tgt 'tmp)
       (bin a b fin) ((caargir tgt a coff) + [['push A]] + (caargir tgt b coff) + fin)
       (? (name? e) [['ld A S coff]]                                                                          ; the counter, raw
          (whole? e) [['li A e]]
          (e @ ('? c t f) (: el (mint 0) en (mint 0)
                             ((caargir tgt c coff) + [['cmp A 0] ['br 'le el]] + (caargir tgt t coff)
                              + [['jmp en] ['label el]] + (caargir tgt f coff) + [['label en]]))
               (op a b) (? (member? op '(+ - *))       (bin a b ([['pop T]] + (arithir tgt op)))
                           (member? op '(// %))        (bin a b ([['test A A] '(br eq deopt) ['pop T]] + (divir tgt op)))
                           (member? op '(& | ^))       (bin a b [['pop T] [(bitop op) A A T]])
                           (member? op '(< <= > >= =)) (bin a b [['pop T] ['cmp T A] ['set (ccof op) A]])
                           ())
               ())))
   ; caarg: leave tag(ARG(counter)) in the frame slot F[0] = Sp[-32], the 4-word walkable frame. a bare
   ; counter tags directly (a loop index is always a valid fixnum); a computed arg anchors the machine
   ; sp first, and both its guards and the tag overflow go to `deopt`.
   (caarg tgt arg) (: A (greg tgt 'acc) SP (greg tgt 'sp) T (greg tgt 'tmp) AN (greg tgt 'anchor)
      (name? arg ? [['ld T SP 8] ['leax T T T 1 1] ['st SP -32 T]]                                         ; bare counter: F[0] = tag(i)
         ([['lea AN 'sp 0]]                                                                                     ; anchor the machine sp for the arg-expr deopt unwind
          + (caargir tgt arg 8)                                                                                   ; A = ARG(counter) raw ; guards/overflow -> deopt
          + [['adds A A A] ['br 'vs 'deopt] ['or A A 1] ['st SP -32 A]])))                                   ; F[0] = tag(ARG) ; tag overflow -> deopt
   ; caargdeopt: restore the machine sp from the anchor (dropping any pending arg-expr pushes), bring
   ; VM Sp from R to R-8, and fall into codeopt. appended only when the arg is computed.
   (caargdeopt tgt) (: SP (greg tgt 'sp) AN (greg tgt 'anchor)
      ['(label deopt) ['lea 'sp AN 0] ['sub SP SP 8] ['jmp 'codeopt]])
   (calloutir tgt op arg)
    (: A (greg tgt 'acc)  IP (greg tgt 'ip)  SP (greg tgt 'sp)  T (greg tgt 'tmp)  U (greg tgt 'scratch)
       (['(label entry0)                                      ; the resume-offset anchor (lvm_resume jumps base + tag'd offset)
          ['sub SP SP 48]                                     ; reserve 6 loop-state slots (Sp = R)
          ['ld U IP 32] ['st SP 24 U]                        ; Sclos = the nifx extras slot = clos     -> R[3]  (IP still &value[0])
          ['ld U IP 8]  ['st SP 40 U]                         ; Stwin = value[1] = twin (interp)        -> R[5]
          ['ld T SP 48] ['sar T 1] ['st SP 16 T]            ; SN = untag(arg @ R[6])                  -> R[2]
          ['li T 1] ['st SP 0 T] ['li T 0] ['st SP 8 T]   ; SA = tag(0)=1, tagged so gate 2 checks the fixnum boundary ; SC = 0   -> R[0] R[1]
          ['lar T 'entry0] ['st SP 32 T]]                     ; Sbase = the blob base (out-of-pool W^X; lar = ADR on arm64)  -> R[4]
        + ['(label top)
           ['ld T SP 8] ['ld U SP 16] ['cmp T U] ['br 'ge 'end]]   ; i >= n -> end
        + (caarg tgt arg)                                       ; F[0] = tag(ARG(counter)) -- bare i, or a computed expr (deopts to `deopt`)
        + [['ld T SP 24] ['st SP -24 T]                      ; F[1] = clos
           ['lar T 'bb] ['ld U SP 32] ['subs T T U]          ; F[2] = tag(&bb - base): an odd charm, GC-walk-clean
           ['leax T T T 1 1] ['st SP -16 T]
           ['st SP -8 U]                                       ; F[3] = base (out-of-pool -- the walk returns it unharmed)
           (jk tgt IP 8) ['sub SP SP 32]         ; Ip = callout_resume ; Sp = frame
           ['ld T IP 0] ['jmpr T]]                            ; Continue -> lvm_ap -> clos -> lvm_resume -> bb
        + ['(label bb)
           ['ld A SP 0]                                        ; A = result (Sp[0] on resume, Sp = R-8 -- same landing as the retired retB path)
           ['test A 1] ['br 'eq 'codeopt]]                    ; gate 1: non-fixnum (even) result -> deopt
        + (coacc tgt op)                                        ; gate 2: fold the result into SA @ Sp[8] (overflow -> deopt)
        + [['ld T SP 16] ['add T T 1] ['st SP 16 T]         ; SC += 1
           ['add SP SP 8] ['jmp 'top]]                        ; Sp = R ; next iteration
        + ['(label end)]
        + (coend tgt op)                                        ; A = SA tagged (`*` retags here, overflow -> deopt)
        + [['add SP SP 48] ['st SP 0 A]                      ; Sp = entry ; result into arg slot
           ['ld IP SP 8] ['st SP 8 A] ['add SP SP 8]         ; inline ret0: Ip=caller-cont; Sp[1]=result; Sp++
           ['ld T IP 0] ['jmpr T]]                            ; Continue -> caller
        + ['(label codeopt)                                    ; shared deopt (both gates); Sp = R-8 on entry
           ['ld A SP 48]                                       ; A = Stwin (R[5] @ Sp+48) -- before restoring Sp
           ['add SP SP 56]                                     ; Sp = entry (R+48); Sp[0] = the original tagged n
           ['mov IP A] ['ld A IP 0] ['jmpr A]]               ; Continue into twin, applied to Sp[0] = n
        + (name? arg ? () (caargdeopt tgt))))                  ; the computed-arg `deopt` label, only when ARG is an expr
   (jitcalloutsum clos) (jitcalloutx clos ['\ 'n 0] '+ 'i 'i 'amd64)
   (jitcalloutx clos src op iv arg tgt)
    (: fop (id? op '* ? (*) (+))  a0 (foldinit op)                             ; the fold op as a value -- a bare `* +` mid-list mis-factors -- and its identity
       argfn (name? arg ? 0 (ev ['\ iv arg]))                               ; a computed ARG -> a closure (\ counter ARG); a bare one needs none
       twin (name? arg ? (n \ (: (go i a) (i < n ? (go (i + 1) (fop a (clos i))) a)) 0 a0)            ; bare: clos applied to the counter
                         (n \ (: (go i a) (i < n ? (go (i + 1) (fop a (clos (argfn i)))) a)) 0 a0))   ; computed: clos applied to ARG(counter)
       (nifx (emit (asmt tgt (calloutir tgt op arg))) twin src 1 clos))   ; value[1] = the twin (image-revert-clean); clos = the extras slot; value[-1]=src
   ; --- the captured / multi-arg call-out ------------------------------------------------------
   ; the same stackless sum over (F i), but F and the bound N are frame params -- a captured import
   ; or a genuine second arg -- read at Sp[8*idx] rather than baked. `a` is the whole frame size
   ; (imports + args, ala's `ary`), so the entry frame is [p0..p_{a-1}] with the caller-cont at
   ; Sp[a], and the return hand-rolls lvm_ret(n=a): the call-out clobbers Ip, so the standard
   ; putfix + Continue epilogue is unreachable. either gate, or a non-fixnum N at the prologue,
   ; deopts to value[1]=e, the bytecode twin ala built -- it captures F, so it recomputes right.
   ; a>=2 here (a=1 no-imports is the global lane), so e is an lvm_cur cell: enter its body at
   ; e+16 with the saturated frame on Sp. the 6 reserved slots (Sp=R) are
   ;   R[0]=SA  R[1]=SC  R[2]=SN  R[3]=SF  R[4]=Sbase  R[5]=Se(=e), the frame above at E=R+48.
   (calloutframeir tgt Fidx Nidx a op arg)
    (: A (greg tgt 'acc)  IP (greg tgt 'ip)  SP (greg tgt 'sp)  T (greg tgt 'tmp)  U (greg tgt 'scratch)
       (['(label entry0)                                      ; the resume-offset anchor
          ['ld U SP (8 * Fidx)]                               ; U = F (raw closure) from the frame
          ['ld A SP (8 * Nidx)] ['test A 1] ['br 'ne 'ngood]]   ; N a fixnum? -> ngood
        + [['ld T IP 8] ['lea IP T 16] ['ld T IP 0] ['jmpr T]]   ; N not a fixnum -> deopt to e's body (Sp = E, IP valid)
        + ['(label ngood) ['sar A 1]                          ; A = N untagged
           ['ld T IP 8]                                        ; T = e (deopt twin = value[1], IP+8 at body entry)
           ['sub SP SP 48]                                     ; reserve 6 slots; Sp = R
           ['st SP 24 U] ['st SP 16 A] ['st SP 40 T]         ; SF = F ; SN = N ; Se = e
           ['li T 1] ['st SP 0 T] ['li T 0] ['st SP 8 T]   ; SA = tag(0)=1, kept tagged so gate 2 checks the fixnum boundary ; SC = 0
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
           ['ld A SP 0] ['test A 1] ['br 'eq 'codeopt]]      ; gate 1: a non-fixnum result -> deopt
        + (coacc tgt op)                                        ; gate 2: fold the result into SA @ Sp[8] (overflow -> deopt)
        + [['ld T SP 16] ['add T T 1] ['st SP 16 T]         ; SC += 1
           ['add SP SP 8] ['jmp 'top]]                        ; Sp = R ; next iteration
        + ['(label end)]
        + (coend tgt op)                                        ; A = SA tagged (`*` retags here, overflow -> deopt)
        + [['add SP SP 48] ['st SP 0 A]                      ; Sp = E (frame) ; result into Sp[0]
           ['ld IP SP (8 * a)] ['st SP (8 * a) A] ['add SP SP (8 * a)]   ; hand-rolled lvm_ret(n=a): cont@Sp[a] ; deliver ; pop a
           ['ld T IP 0] ['jmpr T]]                            ; Continue -> caller
        + ['(label codeopt)                                    ; shared gate deopt; Sp = R-8 on entry
           ['ld T SP 48]                                       ; T = Se (= e, R[5] @ Sp+48) -- before restoring Sp
           ['add SP SP 56]                                     ; Sp = E (frame intact for e's body)
           ['lea IP T 16] ['ld T IP 0] ['jmpr T]]            ; enter e's body (a>=2: e+16), saturated frame on Sp
        + (name? arg ? () (caargdeopt tgt))))                 ; the computed-arg `deopt` label, only when ARG is an expr
   (njit-calloutframe src Fidx Nidx a e op arg tgt)
    (nif (emit (asmt tgt (calloutframeir tgt Fidx Nidx a op arg))) e src a)   ; value[1] = e (the bytecode twin, deopt target -- source-shaped, so op- AND arg-correct); arity a ; src for =/show
   ; --- amble: the boring-tier translator ------------------------------------------------------
   ; the last natjit lane: a closure every other lane declined gets a per-op native translation.
   ; values stay TAGGED, so heap refs ride whole and there are no result gates; every temporary
   ; lives in a reserved VM-Sp slot, a GC root, so a callee GC relocates mid-expression state for
   ; free; the machine stack is never used. calls leave via the stackless callout_drive and resume
   ; at a lar label, so any callee value works (lvm_ap is total -- data self-applies) and a
   ; multi-arg call chains 1-arg drives, since application IS currying.
   ; every runtime surprise -- a non-fixnum under arith/cmp, a heap value under ?, overflow, a
   ; full heap under link -- deopts by RESTART: the twin comes from its reserved slot (a call
   ; clobbers Ip, so the epilogue hand-rolls lvm_ret and the deopt never reads Ip-relative), Sp
   ; is restored, and e re-runs from the restart point. a restart REPEATS any call-out already
   ; made from that point, which only a pure callee survives -- so asafe (njit-amble) declines a
   ; body where a deopting op sits after a call, and the grammar's calls stay ones no restart can
   ; reach.
   ; amble-ok? below is the grammar. strings, floats and global reads in operand position decline.
   ; a global callee resolves via gv at COMPILE -- redefinition-stale, the callout lanes' bet --
   ; and never through a farq name, whose book entry is at best a stale namesake.
   ; a ? over a heap test value, bare (? l ..) on a list, deopts every call: correct, and a small
   ; entry tax over pure bytecode.
   amble-nil (:)
   amble-lam (mint 0)                                             ; the inner-lambda site marker: (amble-lam i n v..) -- entry i of arity n, curried through the imports v..
   (amax a b) (a < b ? b a)
   (adistinct l) (l @ (a . u) (!(member? a u) && adistinct u) 1)
   (aconst? e) (whole? e || id? e amble-nil)
   (aop2? op) (member? op '(+ - * // % & | ^ < <= > >= = id? link))
   (acmp? op) (member? op '(< <= > >= =))
   (aquote? e) (two? e && id? <e '\ && 2 = #e)            ; the reader's quote: (\ X) at exactly two -- a datum constant
   (aqnom? e) (aquote? e && name? <>e)                          ; a quoted nom: a point, so (a = 'sym) is raw identity
   (amemq x l) (atom? l ? 0  (id? x <l) 1  (amemq x >l))     ; id?-membership: interp's quote answers the source NODE, so two
   (aposq x l i) (id? x <l ? i (aposq x >l (i + 1)))         ; =-equal quote nodes stay distinct slots
   (aq e acc) (? (aquote? e) (amemq e acc ? acc (e . acc))          ; collect quote nodes; never walk into quoted data
                 (two? e) (foldl (\ ac x (aq x ac)) acc e)
                 acc)
   aopheads '(? : two? cap cup + - * // % & | ^ < <= > >= = id? link)   ; the heads that are ops, not calls
   (ahascall? env go e)                                                  ; does e contain a call (any non-op head)? the bisect knob's test
    (? (aquote? e) 0
       (two? e) (: op <e
          ((name? op && !(id? op go) && !(member? op aopheads))
           || (foldl (\ a x (a || ahascall? env go x)) 0 >e)))
       0)
   ; aheads: collect the free call heads of e, the global-callee candidates. within the accepted
   ; grammar the only binders are the frame and the one named-let, so a head outside env/go is a
   ; global. go and the frame/loop names are never candidates: a named-let binds go over its whole
   ; body, so a non-tail (go ..) is local and must decline rather than resolve a leaked namesake.
   ; collection over-approximates -- amble-ok? is the gate, gn merely feeds it -- and quoted data
   ; stays unwalked.
   (aheads env go e acc)
    (? (aquote? e) acc
       (two? e)
        (: op <e                                                    ; a2, not a rebind of acc: a `:` is one scope, so `acc (.. acc ..)`
           a2 (? ((name? op) && !(id? op go) && !(member? op env) && !(member? op acc)
                  && !(member? op aopheads))
                 (op . acc) acc)                                      ; would read the unpinned local
           (foldl (\ ac x (aheads env go x ac)) a2 >e))
       acc)
   ; amble-ok?: does e compile? env = visible names (loop vars shadow params), gn = resolvable
   ; global call heads, go/k = the loop name + its arity (0 outside a loop), tl = tail position
   ; ((go ..) is a jump, so it is only sound where its value IS the loop's value).
   (amble-ok? env gn go k tl e)
    (? (name? e) (member? e env)
       (aconst? e) 1
       (nom? e) (member? e env)                                                      ; a mint: an inlined callee's let-bound arg
       (aquote? e) 1
       (two? e)
        (: op <e  as >e  n #as
           (? (id? op '?) (2 <= n <= 3 && (amble-ok? env gn go k 0 <as)   ; 2-arm ?: the missing else is (), acg's amble-nil slot
                           && (amble-ok? env gn go k tl <>as)
                           && (amble-ok? env gn go k tl <>>as))
              (id? op ':) (!(#e % 2)                                        ; a value let: binders enter env in order, so a forward
                           && ((: (gol l ev)                                       ; read declines rather than reading a stale slot
                                (? (atom? >l) (amble-ok? ev gn go k tl <l)
                                   ((amble-ok? ev gn go k 0 <>l)
                                    && (gol >>l (id? <l '_ ? ev (<l . ev))))))
                               as env))
              (member? op '(two? cap cup)) (n = 1 && amble-ok? env gn go k 0 <as)
              (aop2? op) (n = 2 && amble-ok? env gn go k 0 <as && amble-ok? env gn go k 0 <>as)
              (id? op '|| && !(member? op env)) (2 <= n && all (x \ amble-ok? env gn go k 0 x) as)
              (id? op amble-lam) (all (v \ member? v env) >>as)                 ; an inner lambda site: its imports are frame reads
              (id? op go) (tl && n = k && all (x \ amble-ok? env gn go k 0 x) as)
              (name? op) ((0 < n) && (member? op env || member? op gn)
                          && (all (x \ amble-ok? env gn go k 0 x) as))
              0))
       0)
   ; amble-depth: an upper bound on the spill slots e needs (operand nesting rides VM slots).
   ; cx op = the extra curry steps a head carries -- a sibling callee's import count, since its
   ; chain is entry(imports.. args..); 0 for every other head.
   (admax go cx as) (foldl (\ a x (amax a (amble-depth go cx x))) 0 as)   ; the widest operand
   (amble-depth go cx e)
    (? (atom? e) 0
       (aquote? e) 0
       (: op <e  as >e
          (? (id? op '?) (amax (amble-depth go cx <as) (amax (amble-depth go cx <>as) (amble-depth go cx <>>as)))
             (id? op ':) (admax go cx as)                             ; lets ride their own slots; parts run sequentially
             (member? op '(two? cap cup)) (amble-depth go cx <as)
             (id? op '||) (#as + admax go cx as)                      ; aor needs no spills, but a shadowed || is a call, and
                                                                      ; the call formula covers both -- over-reserving is safe
             (id? op amble-lam) 0                                     ; the site lays its chain straight onto the heap
             (id? op '=) (2 + (amax (amble-depth go cx <as) (1 + (amble-depth go cx <>as))))   ; aeq2's slow lane spills the partial + the operand
             (aop2? op) (amax (amble-depth go cx <as) (1 + (amble-depth go cx <>as)))
             (id? op go) (#as + admax go cx as)
             ((cx op) + #as + admax go cx as))))                      ; a call spills a partial per curry step, so an n-arg call reserves n
   ; amble-loop: match the named-let body, both def sugars x both spellings --
   ;   applied:  ((: (go v..) GB) i..)  |  ((: go (\ v.. GB)) i..)
   ;   trigger:  (: (go v..) GB (go i..))  |  (: go (\ v.. GB) (go i..))   the body-having let
   ; whose result IS the fired loop, moon's lexer spelling.  -> (go vars GB inits) | 0
   (amble-let1 S inits)
    (S @ (': D1 D2)
         (? (two? D1 && name? <D1 && all name? >D1 && 0 < #>D1)
              [<D1 >D1 D2 inits]
            (name? D1 && two? D2 && id? <D2 '\ && two? >D2 && two? >>D2)
              [D1 (init >D2) (last >D2) inits]
            0)
       0)
   (amble-loop M)
    (? (atom? M) 0
       (two? <M) (amble-let1 <M >M)                        ; applied
       (M @ (': DEF GB trg)                                ; trigger: the let's result IS the fired loop
            (: r (amble-let1 [': DEF GB] ())
               (r && two? trg && id? <trg <r ?              ; the trigger fires the same name; its args are the inits
                  [<r <>r <>>r >trg]                        ; amble-ok? gates their content -- a (go ..) inside one declines
                  0))
            0))
   ; asub: naive whole-form substitution, safe because the amble grammar admits no inner binders
   ; (no nested \ or : inside a loop body), so there is nothing to capture. it alpha-renames a
   ; loop var that shadows a frame param, the walker idiom ((: (go l a) ..) l a): the inits then
   ; read the params, the body the renamed vars, and one flat env serves both.
   (asub old new e) (? (id? e old) new  (aquote? e) e  (two? e) (map (x \ asub old new x) e)  e)   ; quoted data is inert -- never rename inside it
   (afresh v avoid) (: c (intern ((string v) + "~")) (member? c avoid ? (afresh c avoid) c))
   ; aopv: a macro expansion splices op VALUES, not noms -- the list/backtick macro emits the link
   ; nif as a head, and show prints that as `link`, invisibly. normalize a value head back to its
   ; nom, id?-keyed, and only when the nom is not env-shadowed: a param named link must win, as it
   ; does in bytecode. a miss falls through to `opnom`, the glued lane's own value -> word map
   ; (prel), so every sigil-backed head is named without this list carrying all fourteen.
   aopv [[two? 'two?] [cap 'cap] [cup 'cup] [id? 'id?] [link 'link]
           [tally 'tally] [(+) '+] [(-) '-] [(*) '*] [(<) '<] [(<=) '<=]
           [(>) '>] [(>=) '>=] [(=) '=] [(//) '//] [(%) '%] [(&) '&] [(|) '|] [(^) '^]]
   (aopn x l) (atom? l ? (opnom x)  (id? x <<l) <><l  (aopn x >l))
   ; adesug: rewrite what amble can own by pure source identity, so the twin/deopt story is
   ; untouched. (X) is X -- an infix span reads as the wrapped application, (k < n) -> ((k < n)),
   ; so every infix-written body wears these singletons -- a value head is its nom, (list a..) is
   ; nested links, and (&& a..) is nested (? a .. ()), short-circuit included, since && answers
   ; its last value or the unit. env-shadowed names stay calls and quoted data stays inert.
   ; (|| ..) is NOT rewritten: its value is the first truthy arg, so a rewrite would double-eval.
   ; aor emits it.
   (adesug env ti ta e)
    (? (aquote? e) e
       (atom? e) e
       ; the analyzer's tags, worn by synthesized surfaces (a letrec body-lambda has no source
       ; stash): (apx n head args..) is the apply, (iop VALUE args..) carries the VM op as a
       ; value aopv/opnom name back. ti/ta are ev.l's own uninterned markers, handed down by
       ; ala, so the match is exact and unforgeable; an unmapped or shadowed op value declines.
       (id? <e ta) (two? >e && whole? <>e && two? >>e ? (adesug env ti ta >>e) e)
       (id? <e ti) (two? >e && two? >>e ?
                      (: vn (aopn <>e aopv)
                         (vn && !(member? vn env) ? (adesug env ti ta (vn . >>e)) e))
                      e)
       ; the (X) singleton, by EMPTINESS and never nil?: a tail of nameless mints (bsub's fresh
       ; binders) nets to 0, and the net test unwraps (\ m1 m2 m2) to a bare \, a quote-shaped
       ; wreck amble would then compile as the constant \
       (atom? >e) (adesug env ti ta <e)
       (: op0 <e
          vn (name? op0 || two? op0 ? 0 (aopn op0 aopv))
          op (vn && !(member? vn env) ? vn op0)
          as >e
          (? (id? op 'list && 0 < #as && !(member? op env))
               ((: (go l) (two? l ? ['link (adesug env ti ta <l) (go >l)] ())) as)
             (id? op '? && 3 < #as)                                            ; a cond chain nests: (? c a c2 b e) is (? c a (? c2 b e))
               ((: (go l) (atom? >l ? (adesug env ti ta <l)
                             (['? (adesug env ti ta <l) (adesug env ti ta <>l)] + (two? >>l ? [(go >>l)] ()))))
                as)
             (id? op 'nil? && 1 = #as && !(member? op env))    ; opfix spells the negations as ?s: (nil? x) is (? x 0 1),
               ['? (adesug env ti ta <as) 0 1]                 ; (atom? x) is (? (two? x) 0 1), (!= a b) is (? (= a b) 0 1)
             (id? op 'atom? && 1 = #as && !(member? op env))
               ['? ['two? (adesug env ti ta <as)] 0 1]
             (id? op '!= && 2 = #as && !(member? op env))
               ['? ['= (adesug env ti ta <as) (adesug env ti ta <>as)] 0 1]
             (id? op '&& && 2 <= #as && !(member? op env))
               ((: (go l) (two? >l ? ['? (adesug env ti ta <l) (go >l) ()] (adesug env ti ta <l))) as)
             ((id? op op0 ? (adesug env ti ta op0) op) . map (x \ adesug env ti ta x) as))))
   ; njit-amble: the hook entry -- compile s (deopt -> e) or answer 0 (decline; the hook keeps e).
   ; farq = ala's letrec-local predicate (see hook.l): a truthy (farq nom) names a binding that
   ; is free-looking in a weave re-analysis surface, wire-resolved in truth -- never a gn candidate.
   ; lams = the scope's lambda-sibling list ((name . cell)..): a cell holds (entry . import-names),
   ; lz's frozen site.
   ; alets: collect the value-let binders of e -- every (: p v .. body) node -- and validate
   ; what amble can own: an even tally (the body-less form binds the ENCLOSING scope), name
   ; binders (a define-sugar pair is a lambda), and no shadowing, where enc is the binders
   ; still in scope. one slot serves a name wherever it is bound, so the `(: r ..)` of one arm
   ; and the `(: r ..)` of its sibling share it, each written before it is read. `_` evaluates
   ; and drops, never binds. answers the name list, or 'bad (the body declines).
   (alets e acc) (alt e acc ())
   (alt e acc enc)
    (? (id? acc 'bad) 'bad
       (aquote? e) acc
       (atom? e) acc
       (id? <e ':)
        (#e % 2 ? 'bad
           ((: (gol l ac en)
               (? (id? ac 'bad) 'bad
                  (atom? >l) (alt <l ac en)
                  (: p <l  v <>l
                     (!(nom? p) || id? p () ? 'bad                           ; a binder is a name, or the mint an inlined callee's arg wears
                        (: a2 (alt v ac en)
                           (? (id? a2 'bad) 'bad
                              (id? p '_) (gol >>l a2 en)
                              (member? p en) 'bad
                              (gol >>l (member? p a2 ? a2 (p . a2)) (p . en))))))))
            >e acc enc))
       (foldl (\ ac x (alt x ac enc)) acc e))
   ; adrv?: does e contain a driver call -- a self-call or an applied frame/loop name? the
   ; non-loop cost gate: a body whose only calls go to resolvable globals is a one-shot
   ; wrapper, whose compile is nearly always a waste, where a body that drives a parameter
   ; (a walker's f) or itself amortizes like a loop.
   (adrv? env sf e)
    (? (aquote? e) 0
       (two? e) (: op <e
          ((name? op && (id? op sf || (member? op env && two? >e)))
           || (foldl (\ a x (a || adrv? env sf x)) 0 >e)))
       0)
   ; ainl: rewrite every inner lambda (\ q.. B) whose creation ala recorded -- inl is this
   ; lambda's subtree ((exp src-tail (entry . imports))..), matched by the node itself -- into
   ; the site marker (amble-lam i n v..): entry i enters st's list, n is its arity (imports +
   ; params), v.. the import names the site reads off the frame. an unrecorded lambda stays and
   ; declines; the inner body is never walked, since its binders are its own, and quoted data
   ; is inert. a body with no record and no inner lambda is answered as it is, unwalked.
   (ainlr t inl) (atom? inl ? 0  (id? t <<inl || id? t <><inl) <>><inl  (ainlr t >inl))
   (alam? e) (? (aquote? e) 0  (atom? e) 0  (id? <e '\ && two? >e && two? >>e) 1  (foldl (\ a x (a || alam? x)) 0 e))
   (ainl e inl st) (two? inl && alam? e ? (ainl0 e inl st) e)
   (ainl0 e inl st)
    (? (aquote? e) e
       (atom? e) e
       (id? <e '\ && two? >e && two? >>e)
        (: r (ainlr >e inl)
           (r ? (: i #(st 'l)
                   _ (pin st 'l ((st 'l) + [<r]))
                   (amble-lam . i . (#>r + #e - 2) . >r))
              e))
       (map (x \ ainl0 x inl st) e))
   (njit-amble s a e gv farq lams nm ti ta inl tgt)
    (: ps (init >s)  M (last >s)
       lp (amble-loop M)
       go   (lp ? <lp 0)     vars0 (lp ? <>lp ())
       GB0  (lp ? (lp 2) 0)  inits (lp ? (lp 3) ())
       shad (filter (v \ member? v ps) vars0)
       ren  (map (v \ [v (afresh v (go . vars0 + ps))]) shad)
       vars (map (v \ (: r (assoc v ren) (r ? <>r v))) vars0)
       st   {}                                                 ; the inner-lambda sites: 'l = their entries, in site order (the marker's index)
       GB   (lp ? (foldl (\ b pr (asub <pr <>pr b)) (ainl GB0 inl st) ren) 0)   ; the sites first, so a shadowing loop var renames their imports too
       k    #vars
       body (lp ? GB (ainl M inl st))
       env  (vars + ps)
       body (adesug env ti ta body)                          ; own list/&& by definition (nested links / nested ?s) + untag
       inits (map (i \ adesug env ti ta (ainl i inl st)) inits)
       ins  (st 'l)
       nin  #ins
       ; self: the letrec name this closure is being bound to (ala's nm; 0 when anonymous).
       ; a saturated tail self-call jumps the body top on a rewritten frame, so the recursion
       ; becomes a loop; any other self-call drives the blob's OWN cell, read off Ip at the
       ; prologue -- early-bound, exactly the wire's backpatched site. a shadowed name declines.
       sc   (assoc nm lams)                                  ; the self lams cell: its kept import row names the frame's leading slots,
       scc  (two? sc ? (peep >sc 0 0) 0)                     ; so a self-call re-supplies the imports the way the wire's site does
       simp (two? scc ? >scc ())
       ni   #simp
       self (? (id? nm 0) 0  (member? nm ps) 0  (member? nm vars0) 0  (id? nm go) 0
               (member? nm '(? two? cap cup id? link + - * < <= > >= = || tally list &&)) 0
               !(simp = (take ni ps)) 0
               nm)
       sfq  !(id? self 0)
       ; the value lets ride pre-allocated slots past the spills: one flat namespace (alets
       ; declines a rebind or a shadow), binders visible in order.
       lts0 (foldl (\ ac i (alets i ac)) (alets body ()) inits)
       lts  (? (id? lts0 'bad) 'bad
               (foldl (\ ac p (ac || member? p env || id? p go || id? p self)) 0 lts0) 'bad
               lts0)
       nl   (id? lts 'bad ? 0 #lts)
       envl (id? lts 'bad ? env (env + lts))
       ; the free-head seams: loop bodies, and drok bodies that drive a param or themselves --
       ; the shapes that amortize the call and compile taxes. a one-shot wrapper whose only
       ; calls go to globals, (\ t k (peep t k ())) and kin, stays bytecode.
       ; a free head that is not farq resolves via gv at COMPILE and rides an extras slot,
       ; redefinition-stale, exactly the callout lanes' bet. a farq name -- a letrec local the
       ; weave re-analysis dropped from the import row -- must never gv-resolve: the book holds
       ; at best a stale namesake.
       ; a farq name that lamq DOES resolve is a woven lambda sibling of this very let, and its
       ; (entry . import-names) is the same pair lz freezes into a bytecode reference site, so
       ; amble compiles the identical application with no staleness bet: the entry rides an
       ; extras slot and the call curries entry(imports.. args..). fixcaps' pour guarantees the
       ; import names sit in this frame; entry 0 (a forward or self ref, lz's backpatch case) or
       ; a stray import just isn't in cn, and amble-ok? declines that body.
       cand (filter (f \ !(id? f self)) (foldl (\ ac i (aheads envl go i ac)) (aheads envl go body ()) inits))
       drok (lp ? 1 (two? cand || two? ins ? (adrv? envl self body) 1))  ; the non-loop cost gate: free heads and inner lambdas ride only a body that
                                                                         ; drives a param or itself -- and a declining body skips the resolution below
       csx  (drok ? (map (f \ [f (farq f ?                   ; only a letrec-local can be a sibling, and farq is the cheap gate
                              (: lfd (assoc f lams)          ; the lam-list assoc; the cell read mirrors ev.l's cof, box slot 0
                                 (two? lfd ? (peep >lfd 0 0) 0))
                              0)])
                 cand) ())
       cgood (filter (p \ (: cc <>p                    ; import names must be frame params (ps, where fixcaps pours them)
                             (two? cc && <cc && all (x \ member? x ps) >cc))) csx)   ; and never loop vars -- bytecode reads the propagated capture
       cn    (map cap cgood)
       cvals (map (p \ <<>p) cgood)
       (cimp f) ><>(assoc f cgood)
       gps  (drok ? (filter (p \ lit? <>p)
             (map (f \ [f (gv f 0)])
              (filter (f \ !(farq f)) (filter (f \ !(member? f cn)) cand)))) ())
       gn   (map cap gps)
       gvals (map caup gps)
       qs   (drok ? (foldl (\ ac i (aq i ac)) (aq body ()) inits) ())   ; the quoted-datum constants (id?-deduped source nodes); a declined body skips the walk
       nq   #qs
       eqv  (gv '= 0)                                       ; the = value: aeq2's generic lane drives it off an extras slot
       cx   (op \ (? (id? op self) ni (member? op cn) #(cimp op) 0))
       nd   (drok ? (amax (amble-depth go cx body) (foldl (\ acd i (amax acd (amble-depth 0 cx i))) 0 inits)) 0)
       kn   (sfq ? (self . (gn + cn)) (gn + cn))         ; every resolvable free head + the self name (amble-ok?'s membership test)
       lpok (lp ? ((all name? vars) && (adistinct vars) && (k = #inits)
                   && !(member? go vars) && !(member? go ps)) 1)
       ; the restart law. a deopt RESTARTS: Sp goes back and the twin re-runs from the restart
       ; point, so every call-out the native already made is made AGAIN -- faithful only where
       ; the callee is pure, which the grammar cannot ask. so no deopting op may sit after a
       ; call. asafe walks acg's own evaluation order carrying "a call has run", answering that
       ; flag onward or 'bad where a guard follows one. the restart point is the arg slots: a
       ; saturated tail self-call rewrites them before the jump, so the flag clears at that back
       ; edge and only the running iteration can replay; a named-let's vars live in reserved
       ; slots the twin never sees, so its restart is the whole invocation and a call poisons
       ; every later iteration -- (go ..) under a set flag declines. the lanes that drive rather
       ; than guard -- =, tally -- resolve book primitives at compile, and two?/id? carry no
       ; guard at all, so none of them sets the flag.
       (asafe tl cl ex)
        (: (aseq c l) (foldl (\ ac x (asafe 0 ac x)) c l)                      ; the children, in order
           (aguard c) (? (id? c 'bad) 'bad  c 'bad  0)                         ; then the node's own guard
           (? (id? cl 'bad) 'bad
              (atom? ex) cl
              (aquote? ex) cl
              (: op <ex  as >ex
                 (? (id? op ':) ((: (gol l c) (atom? >l ? (asafe tl c <l) (gol >>l (asafe 0 c <>l)))) as cl)
                    (id? op '?)
                     (: ct (aguard (asafe 0 cl <as))
                        (? (id? ct 'bad) 'bad
                           (: ca (asafe tl 0 <>as)  cb (asafe tl 0 <>>as)      ; the arms are alternatives; either call poisons the sequel
                              (? (id? ca 'bad || id? cb 'bad) 'bad (ca || cb)))))
                    (sfq && tl && (id? op self) && (#as = (a - ni)))
                     (: c2 (aseq cl as) (id? c2 'bad ? 'bad 0))                ; the self back edge
                    (id? op go) (aguard (aseq cl as))                          ; the named-let back edge
                    (member? op '(two? id?)) (aseq cl as)
                    (id? op '= && (aqnom? <as || aqnom? <>as || lit? eqv)) (aseq cl as)
                    (id? op 'tally && !(member? 'tally env) && member? 'tally gn && atom? >as) (aseq cl as)
                    (id? op '|| && !(member? op env) && two? >as) (aguard (aseq cl as))
                    (id? op amble-lam) (two? >>as ? (aguard cl) cl)            ; the site's room guard, over frame reads
                    (member? op '(cap cup link + - * // % & | ^ < <= > >= =)) (aguard (aseq cl as))
                    (: c2 (aseq cl as) (id? c2 'bad ? 'bad 1))))))             ; a call: its args, then the call
       ok ((two? s) && (two? >s) && (all name? ps) && (adistinct ps)
           && (a = #ps) && (0 < a) && lpok && drok && !(id? lts 'bad) && (two? body)
           && (getenv "LOVE_AMBLE_NOCALL" ? !(ahascall? env go body) 1)                ; the call-lane bisect knob: splits the call machinery from the rest
           && (all lit? gvals) && (nd < 48) && ((k + #gn + #cn + nq + nin + nl) < 48)   ; slots are cheap once the prologue guards the room (below)
           && (all (i \ amble-ok? ps kn 0 0 0 i) inits)
           && (amble-ok? env kn go k 1 body)
           && !(id? (asafe 1 (foldl (\ ac i (asafe 0 ac i)) 0 inits) body) 'bad))   ; the restart law, over the gated body
       (? ok
          (: _ (getenv "LOVE_AMBLE_TRACE" ? (puts ("amble: " + show s + "\n")) 0)   ; the compile trace: names every ambled source
             ng (1 + #gn + #cn + nq + (lit? eqv ? 1 0) + nin)   ; extras = [() | globals | sibling entries | quoted data | = | inner entries]:
                                                                ; () is the empty mint -- li 1 would be the charm 0-fossil -- and entries and
                                                                ; quotes are heap values, all riding the nifx chain into GC-walked slots
             R (k + 3 + ng + nd + nl)                       ; loopvars | Se | So | budget | extras | spills | lets
             A (greg tgt 'acc)  T (greg tgt 'tmp)  X (greg tgt 'anchor)  SC (greg tgt 'scratch)
             SP (greg tgt 'sp)  IP (greg tgt 'ip)  HP (greg tgt 'hp)
             seo (8 * k)  soo (8 * (k + 1))  budo (8 * (k + 2))  gbo (8 * (k + 3))  spo (8 * (k + 3 + ng))  fro (8 * R)
             (voff v) (? (member? v vars) (8 * (posof v vars 0))  (member? v lts) (lvo v)  (fro + (8 * (posof v ps 0))))
             (spill d) (spo + (8 * d))
             eqo (gbo + (8 * (1 + #gn + #cn + nq)))   ; the = value's extras slot (past globals, siblings, quotes)
             ino (eqo + (8 * (lit? eqv ? 1 0)))                    ; the inner entries' slots (past =)
             lto (8 * (k + 3 + ng + nd))                    ; the let region, past the spills
             (lvo v) (lto + (8 * (posof v lts 0)))
             (fx2 lbl) [['mov X T] ['and X X A] ['test X 1] ['br 'eq lbl]]    ; both T and A fixnum, else lbl
             (abin sa sb d fin) ((acg sa d) + [['st SP (spill d) A]] + (acg sb (d + 1)) + [['ld T SP (spill d)]] + fin)   ; T = a, A = b
             (aar op sa sb d)
              (abin sa sb d
               (op @ '+ ((fx2 'deopt) + [['sub A A 1] ['adds A A T] ['br 'vs 'deopt]])         ; 2y + (2x+1) = tag(x+y)
                    '- ((fx2 'deopt) + [['subs T T A] ['br 'vs 'deopt] ['lea A T 1]])          ; 2(x-y) then tag
                    ((fx2 'deopt) + [['sar A 1] ['sub T T 1] ['mulo A A T SC 'deopt] ['or A A 1]])))   ; y * 2x = 2xy, where mulo IS the tag-range gate, then tag
             (acmp2 op sa sb d) (abin sa sb d ((fx2 'deopt) + [['cmp T A] ['set (ccof op) A] ['leax A A A 1 1]]))
             (adm op sa sb d) (abin sa sb d                                                        ; // %: untag, /-by-0 -> twin, divir (trunc toward zero), retag
              ((fx2 'deopt) + [['sar T 1] ['sar A 1] ['test A A] ['br 'eq 'deopt]]                 ; (the adds gate catches most-negative // -1)
               + (divir tgt op)
               + [['adds A A A] ['br 'vs 'deopt] ['or A A 1]]))
             (abit op sa sb d) (abin sa sb d                                                       ; & | ride the tags whole (2x+1 op 2y+1 = tag(x op y)); ^ re-tags
              ((fx2 'deopt) + (id? op '^ ? [['xor A A T] ['or A A 1]] [[(bitop op) A A T]])))
             ; aeq2: the generic =, total and never a deopt. equal fixnums and the same point
             ; answer 1; distinct fixnums, and a point against anything else, answer 0, since a
             ; point equals only itself. everything structural -- strings, gems (the numeric
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
             (aor as d) (: en (mint 0)                                                                      ; (|| a..): the first truthy value, single-eval, short-circuit --
              (((: (go l) (atom? >l ? (acg <l d)                                                 ; each arg lands in A; charm truth keeps it (heap truth -> the twin,
                             ((acg <l d)                                                                ; acq's bet); the last arg is the value either way
                              + [['test A 1] ['br 'eq 'deopt] ['cmp A 1] ['br 'gt en]]
                              + (go >l)))) as)
               + [['label en]]))
             (adrive cso) (: bb (mint 0)                                                                    ; apply the value at Sp[cso] to the arg in A, via the walkable resume:
              [['st SP -32 A] ['ld T SP cso] ['st SP -24 T]                                         ; frame [arg, callee, tag(bb-entry0), entry0] -- an odd charm + an out-of-
                 ['lar T bb] ['lar X 'entry0] ['subs T T X] ['leax T T T 1 1]                       ; pool code base, so a GC with the call pending walks it clean (no retB)
                 ['st SP -16 T] ['st SP -8 X]
                 (jk tgt IP 8) ['sub SP SP 32] ['ld T IP 0] ['jmpr T]                  ; -> lvm_ap -> callee -> lvm_resume -> bb
                 ['label bb] ['ld A SP 0] ['add SP SP 8]])                                            ; resume: result at Sp[0], pop it
             (achain as cso d) (atom? >as
                                  ? ((acg <as d) + (adrive cso))
                                    ((acg <as d) + (adrive cso) + [['st SP (spill d) A]]
                                     + (achain >as (spill d) (d + 1))))                                ; partial -> spill -> next arg (currying)
             ; asite: an inner lambda (amble-lam i n v..) -- entry i of arity n, curried through the
             ; imports v.. -- is the closure lz's site builds: lvm_cur's own partial cells laid
             ; straight onto the heap under one room guard, [cur n-1 unc arg cont tag] while the
             ; rest still curries and [unc arg cont tag] for the last, where cont is the callee
             ; cell + 2 and tag the cell's address | 2. a fresh chain per evaluation; a closed
             ; lambda is its cell.
             (asite as d) (: so (ino + (8 * <as))  n <>as  vs >>as
                             (acur v a) (a > 2 ?
                                           [(jk tgt X 9) ['st HP 0 X] ['li X ((2 * a) - 1)] ['st HP 8 X]
                                            (jk tgt X 10) ['st HP 16 X] ['ld X SP (voff v)] ['st HP 24 X] ['lea X T 16] ['st HP 32 X]
                                            ['mov X HP] ['or X X 2] ['st HP 40 X] ['mov T HP] ['add HP HP 48]]
                                           [(jk tgt X 10) ['st HP 0 X] ['ld X SP (voff v)] ['st HP 8 X] ['lea X T 16] ['st HP 16 X]
                                            ['mov X HP] ['or X X 2] ['st HP 24 X] ['mov T HP] ['add HP HP 32]])
                             (gov l a) (two? l ? ((acur <l a) + (gov >l (a - 1))) ())
                             (two? vs ?
                                ([['lea X HP (64 + (48 * #vs))] ['cmp SP X] ['br 'below 'deopt] ['ld T SP so]]
                                 + (gov vs n) + [['mov A T]])
                                [['ld A SP so]]))
             (astr1 cso) (: ld (mint 0) ok (mint 0) en (mint 0)                                             ; the string fast lane at a 1-arg frame-callee site: (s i) with s a
              ([['ld T SP cso] ['test T 1] ['br 'ne ld]                                             ; text and i a charm is a byte index: in range, the unsigned byte,
                  ['ld X T 0] (jk tgt SC 1) ['cmp X SC] ['br 'ne ld]                             ; native. everything else falls to the drive (never a deopt -- a
                  ['test A 1] ['br 'eq ld]                                                              ; mixed-type call site keeps its closure speed): another callee or
                  ['mov X A] ['sar X 1] ['ld SC T 8]                                                  ; index kind, and -- since the one unsigned compare wraps a negative
                  ['cmp X SC] ['br 'below ok]                                                           ; huge -- both the from-the-end read and the out-of-range (). so the
                  ['jmp ld]                                                                             ; lane answers what C answers, at the cost of driving a negative.
                  ['label ok] ['ldxb A T X 1 16] ['leax A A A 1 1]
                  ['jmp en] ['label ld]]
               + (adrive cso)
               + [['label en]]))
             (atally sx d) (: ld (mint 0) en (mint 0)                                                       ; (tally X) with X a text: A = tag(len at [X+8]); anything else
              ((acg sx d)                                                                                   ; drives the tally global through its gn slot (a chain tally is a
               + [['test A 1] ['br 'ne ld]                                                            ; walk -- C's, not a deopt storm)
                    ['ld X A 0] (jk tgt SC 1) ['cmp X SC] ['br 'ne ld]
                    ['ld X A 8] ['leax A X X 1 1] ['jmp en]
                    ['label ld]]
               + (adrive (gbo + (8 * (1 + (posof 'tally gn 0)))))
               + [['label en]]))
             ; asat: a saturated n-ary call site (n >= 2). if the callee is a cur cell of arity
             ; exactly n (bytecode closures, natives, and the nifs share the [cur arity body..]
             ; layout), lay the apn frame [args cont tag base] and enter its body at cell+2
             ; directly -- one call, no curry chain, no partials. any other callee -- a partial,
             ; an over-application, arity 1, data -- takes the drive chain over the same spilled
             ; args (evaluated once, either way).
             (asat as cso d) (: bb (mint 0)  sw (mint 0)  dn (mint 0)  n #as  w (n + 3)
              ((agous as 0 d)
               + [['ld X SP cso] ['test X 1] ['br 'ne sw]
                  ['ld T X 0] (jk tgt SC 9) ['cmp T SC] ['br 'ne sw]                               ; entry = lvm_cur?
                  ['ld T X 8] ['cmp T ((2 * n) + 1)] ['br 'ne sw]]                                 ; arity = n?
               + (: (gsl j) (j < n ? ([['ld A SP (spill (d + j))] ['st SP ((8 * j) - (8 * w)) A]] + (gsl (j + 1))) ()) (gsl 0))
               + [(jk tgt T 8) ['add T T 8] ['st SP ((8 * n) - (8 * w)) T]                         ; cont = &resume[1]
                  ['lar T bb] ['lar A 'entry0] ['subs T T A] ['leax T T T 1 1] ['st SP ((8 * (n + 1)) - (8 * w)) T]
                  ['st SP ((8 * (n + 2)) - (8 * w)) A]
                  ['lea IP X 16] ['sub SP SP (8 * w)] ['ld T IP 0] ['jmpr T]                      ; enter the body at cell+2
                  ['label bb] ['ld A SP 0] ['add SP SP 8] ['jmp dn]
                  ['label sw]]
               + (: (goj j so) (j < n ?                                                            ; the chain, from the spills: each partial rolls through spill d
                                  ([['ld A SP (spill (d + j))]] + (adrive so)
                                   + (j < (n - 1) ? [['st SP (spill d) A]] ())
                                   + (goj (j + 1) (spill d)))
                                  ()) (goj 0 cso))
               + [['label dn]]))
             ; a 1-arg frame callee can be a STRING at run time (the lexer class), so the string
             ; fast lane rides LOOPING bodies only -- named-let or self-call, the same cost gate as
             ; gn -- since on a one-shot recursion wrapper it taxed every drive. a gn or sibling
             ; callee is never a string (lit?-gated, or a woven entry), so those keep the bare drive.
             (acall f as d)
              (? (id? f self)                                                                               ; self: saturated -> the direct frame jump; partial or over-applied ->
                 (#as = (a - ni) ?                                                                  ; curry the own cell through its imports, then the args (a saturated
                    (aselfcall as d)                                                                        ; tail self-call took sgo instead)
                    (achain (simp + as) soo d))
                 (member? f cn)                                                                             ; a sibling: its frozen entry takes the import names (all in the frame
                 (: sas ((cimp f) + as)  sso (gbo + (8 * (1 + #gn + (posof f cn 0))))            ; by fixcaps) then the args -- lz's site, saturated when it can be
                    (two? >sas ? (asat sas sso d) (achain sas sso d)))
                 (: fr  (member? f vars || member? f ps || member? f lts)                           ; f in the frame, else a gn slot
                    cso (fr ? (voff f) (gbo + (8 * (1 + (posof f gn 0)))))
                    (? ((lp ? 1 sfq) && atom? >as && fr) ((acg <as d) + (astr1 cso))
                       (two? >as) (asat as cso d)                                                      ; multi-arg saturates when the callee's arity agrees
                       (achain as cso d))))
             (agous us j d) (two? us ? ((acg <us (d + j)) + [['st SP (spill (d + j)) A]] + (agous >us (j + 1) d)) ())
             (agomv j d) (j < 0 ? () ([['ld A SP (spill (d + j))] ['st SP (8 * j) A]] + (agomv (j - 1) d)))
             (ago as d) ((agous as 0 d) + (agomv (k - 1) d) + [['jmp 'top]])                            ; updates from the old vars (spilled first), then the back-edge
             (asmv j d) (j < 0 ? () ([['ld A SP (spill (d + j))] ['st SP (fro + (8 * (ni + j))) A]] + (asmv (j - 1) d)))
             ; aselfcall: a saturated non-tail self-call jumps the blob's own entry directly, on the
             ; frame [imports.. args.. resume tag(bb-entry0) entry0] under Sp with Ip restored off
             ; So, and the callee's hand-rolled ret returns through lvm_resume to bb. no drive, no
             ; curry chain, no partials; the entry's room and budget guards run per level.
             (aselfcall as d) (: bb (mint 0)  w (a + 3)
              ((agous as 0 d)
               + (: (gimp l i) (two? l ? ([['ld T SP (voff <l)] ['st SP ((8 * i) - (8 * w)) T]] + (gimp >l (i + 1))) ()) (gimp simp 0))
               + (: (garg j) (j < (a - ni) ? ([['ld T SP (spill (d + j))] ['st SP ((8 * (ni + j)) - (8 * w)) T]] + (garg (j + 1))) ()) (garg 0))
               + [(jk tgt T 8) ['add T T 8] ['st SP ((8 * a) - (8 * w)) T]                          ; cont = &resume[1]: ret jumps the cell's word 0 = lvm_resume
                  ['lar T bb] ['lar X 'entry0] ['subs T T X] ['leax T T T 1 1] ['st SP ((8 * (a + 1)) - (8 * w)) T]
                  ['st SP ((8 * (a + 2)) - (8 * w)) X]
                  ['ld IP SP soo]]
               + (a = 1 ? () [['add IP IP 16]])
               + [['sub SP SP (8 * w)] ['jmp 'entry0]
                  ['label bb] ['ld A SP 0] ['add SP SP 8]]))
             (aselfgo as d) ((agous as 0 d) + (asmv (a - ni - 1) d) + [['jmp 'sgo]])               ; saturated tail self-call: new args (spilled first) over the arg slots (imports keep), jump the body top
             (acqt st sth sel d) (: le (mint 0)  ln (mint 0)                                       ; acq, tails threaded
              ((acg st d) + [['test A 1] ['br 'eq 'deopt] ['cmp A 1] ['br 'le le]]
               + (acgt sth d) + [['jmp ln] ['label le]] + (acgt sel d) + [['label ln]]))
             (acgt e d)                                                                             ; tail-position acg: thread tails through ? and let bodies so a saturated self-call jumps
              (? (aquote? e) (acg e d)
                 (atom? e) (acg e d)
                 (: op <e  as >e
                    (? (id? op '?) (acqt <as <>as <>>as d)
                       (id? op ':) ((: (gol l) (atom? >l ? (acgt <l d)
                                                  ((acg <>l d)
                                                   + (id? <l '_ ? () [['st SP (lvo <l) A]])
                                                   + (gol >>l)))) as)
                       (sfq && (id? op self) && (#as = (a - ni))) (aselfgo as d)
                       (acg e d))))
             (acg e d)
              (? (name? e) [['ld A SP (voff e)]]
                 (whole? e) [['li A ((2 * e) + 1)]]
                 (id? e amble-nil) [['ld A SP gbo]]
                 (nom? e) [['ld A SP (voff e)]]
                 (aquote? e) [['ld A SP (gbo + (8 * (1 + #gn + #cn + (aposq e qs 0))))]]   ; a quoted datum: its extras slot (past the globals + sibling entries)
                 (: op <e  as >e
                    (? (id? op ':) ((: (gol l) (atom? >l ? (acg <l d)                            ; a value let: each binding lands in its slot, in order
                                                  ((acg <>l d)
                                                   + (id? <l '_ ? () [['st SP (lvo <l) A]])
                                                   + (gol >>l)))) as)
                       (id? op '?) (acq <as <>as <>>as d)
                       (id? op 'two?) (atwo <as d)
                       (id? op 'cap) (acap <as d 8)
                       (id? op 'cup) (acap <as d 16)
                       (id? op 'id?) (aid <as <>as d)
                       (id? op 'link) (alink <as <>as d)
                       (id? op '= && (aqnom? <as || aqnom? <>as))
                        (aid <as <>as d)                                               ; (a = 'sym): a point equals only itself (the point-= law) -> raw identity, total
                       (id? op '= && (lit? eqv)) (aeq2 <as <>as d)
                       (acmp? op) (acmp2 op <as <>as d)
                       (member? op '(+ - *)) (aar op <as <>as d)
                       (member? op '(// %)) (adm op <as <>as d)
                       (member? op '(& | ^)) (abit op <as <>as d)
                       (id? op 'tally && !(member? 'tally env) && member? 'tally gn && atom? >as)
                        (atally <as d)                                       ; the tally fast lane (env-shadowed or multi-arg falls to the call paths)
                       (id? op '|| && !(member? op env) && two? >as) (aor as d)
                       (id? op amble-lam) (asite as d)
                       (id? op go) (ago as d)
                       (acall op as d))))
             initR (: (goi j) (j < R ? ([['st SP (8 * j) T]] + (goi (j + 1))) ())
                      ([['li T 1]] + (goi 0)))                                                          ; every reserved slot a valid value before any call (GC walks them)
             unpk ([['ld T IP 8] ['st SP seo T]]                                                  ; Se = value[1] = e (plain -- image-revert-clean)
                   + (a = 1 ? [['st SP soo IP]] [['mov T IP] ['sub T T 16] ['st SP soo T]])     ; So = the own cell (= Ip, or Ip-16 past the cur pair) -- the self-drive callee
                   + [['ld T IP 32]                                                               ; T = the nifx extras chain
                       ['st SP budo T] ['ld T T 16]]                                                    ; link 0 = the budget cell (saved for the deopt decrement), slots after it
                   + (: (gou j) (j < ng ? ([['ld X T 8] ['st SP (gbo + (8 * j)) X] ['ld T T 16]] + (gou (j + 1))) ()) (gou 0)))
             initsir (: (goj is j) (two? is ? ((acg <is 0) + [['st SP (8 * j) A]] + (goj >is (j + 1))) ())
                        (goj inits 0))
             bodyir ((sfq ? ['(label sgo)] ())                                                     ; the self-jump landing: a fresh invocation on the rewritten frame
                     + (lp ? (initsir + ['(label top)] + (acgt body 0)) (acgt body 0)))   ; body = GB or M, desugared -- never the raw forms
             epi [['add SP SP fro] ['st SP 0 A]                                                       ; hand-rolled lvm_ret (a drive call clobbered Ip)
                    ['ld IP SP (8 * a)] ['st SP (8 * a) A] ['add SP SP (8 * a)]
                    ['ld T IP 0] ['jmpr T]]
             dpt (['(label deopt)
                      ['ld X SP budo] ['ld SC X 8] ['sub SC SC 2] ['st X 8 SC]                      ; budget -= 1 (tagged: 2k+1 - 2): a restart is the profitability signal
                      ['ld T SP seo] ['add SP SP fro]]                                                  ; (d0 room-guard entries never count); twin from its slot before the restore
                  + (a = 1 ? [['mov IP T]] [['lea IP T 16]])                                      ; a=1: re-apply e; else enter e's body on the saturated frame
                  + [['ld T IP 0] ['jmpr T]])
             prog (['(label entry0)                                                                       ; entry0 = the resume-offset anchor (any shared label works; the blob's first byte)
                      ['lea X HP (40 + (8 * (R + 4 + (amax a nd))))] ['cmp SP X] ['br 'below 'd0]    ; room guard: the reservation + the widest call frame (nd bounds any site's arg count) + floor must fit Sp-Hp,
                      ['ld T IP 32] ['ld X T 8] ['cmp X 1] ['br 'le 'd0]                            ; the deopt budget (extras link 0, a charm): spent -> the native is
                      ['sub SP SP fro]]                                                                   ; retired, every call takes d0 straight to the twin (frame untouched
                                                                                                            ; -> the twin runs it whole; a perma-deopting native heals to bytecode)
                   + initR + unpk + bodyir + epi + dpt
                   + ['(label d0) ['ld T IP 8]] + (a = 1 ? [['mov IP T]] [['lea IP T 16]]) + [['ld T IP 0] ['jmpr T]])
             (nifx (emit (asmt tgt prog)) e s a (8 . (amble-nil . gvals + cvals + (map (q \ <>q) qs) + (lit? eqv ? [eqv] ()) + ins))))
          0))
   ; --- the general tail-recursive counted loop, n loop vars ------------------------------------
   ; (\ n ((: (go v0..vk) (? TEST (go U0..Uk) RESULT)) I0..Ik)), up to 3 loop vars in the loopregs
   ; bank RAW, the param n the tagged Sp[0]. each iteration computes every Uj from the OLD regs --
   ; push all, then pop into the regs in reverse -- so the updates read pre-update values, as fib's
   ; `b` and `a+b` must. the loop regs survive the expr scratch, so nothing is saved.
   (lvreg tgt j) ((loopregs tgt) j)               ; loop var j -> its abstract register
   (loopcode-n n vars updates test result inits tgt)
    ; nvld loads a var: n from Sp[0] (fxvir, guarded), a loop var from its register. control flow,
    ; the per-depth deopt and the putfix epilogue are all label-resolved.
    (: A (greg tgt 'acc)
       nvld (\ e d (? (e = n) (fxvir tgt 0 d)
                       (member? e vars) [['mov A (lvreg tgt (argix e vars 0))]]
                       ()))
       pre  ((: (go j is) (two? is ? ([['li (lvreg tgt j) <is]] + (go (j + 1) >is)) ())) 0 inits)        ; reg-init
       ups  ((: (go j us) (two? us ? ((cgvir tgt nvld <us j) + [['push A]] + (go (j + 1) >us)) ())) 0 updates)   ; push Uj at depth j
       pops ((: (go j) (j < 0 ? () ([['pop (lvreg tgt j)]] + (go (j - 1))))) (#updates - 1))                ; pop reg(k-1)..reg(0)
       tb   (cgvir tgt nvld test 0)
       rb   (cgvir tgt nvld result 0)
       (asmt tgt (pre + '((label top))
              + tb + [['cmp A 0] '(br eq end)]           ; <test>; acc==0 (false) -> end
              + ups + pops + '((jmp top) (label end))      ; updates ; loop top
              + rb + (loopepi tgt))))                      ; result -> acc ; putfix + Continue
   ; (jitloop-n lam vars updates test result inits) -- a general N-var loop -> native (x86 self-test helper).
   (jitloop-n lam vars updates test result inits)
    (nif (emit (loopcode-n <>lam vars updates test result inits 'amd64)) (ev lam) lam 1)
   ; --- the float lane: unboxed doubles in xmm/d --------------------------------------------------
   ; float values stay in registers, never boxed. f0 is the accumulator and a nested operand spills
   ; to the machine stack. peek is mopped, so a float constant is materialized arithmetically:
   ; fbits computes its IEEE-754 bits, and the 52-bit mantissa makes frac*2^52 exact.
   ; LANE LAW: the float lane carries no overflow guard, since doubles saturate to inf and never
   ; promote to a bignum; only the integer lane keeps the deopt that preserves bignum promotion.
   ; a float predicate leaf over a fixnum arg answers 0/1 here; a float RESULT is jitfr below.
   ; a gem arg would want the lvm_gembox type guard, so a non-fixnum arg deopts instead.
   (fbits x)                                                          ; a finite double -> its IEEE-754 u64 bits
    (x = 0.0 ? 0
       (: s (x < 0.0 ? 1 0) ax (x < 0.0 ? (0.0 - x) x)
          eu ((: (go e) (((e + 1) 2.0) <= ax ? (go (e + 1)) e)) (int ((log ax) / (log 2.0))))   ; raise past the log's error
          e  ((: (go e) (ax < (e 2.0) ? (go (e - 1)) e)) eu)                                    ; then lower; (e 2.0) is the tower, 2.0 ** e
          ((s * 9223372036854775808) + ((e + 1023) * 4503599627370496) + (int (((ax / (e 2.0)) - 1.0) * 4503599627370496.0)))))
   ; the float spill rides the machine stack at the loop/group slot size (x86 8 / arm64 16, for sp
   ; alignment), so a mid-nest deopt (deoptbody, slotsz*d) unwinds it correctly.
   (xpushir tgt) [['sub 'sp 'sp (slotsz tgt)] '(stsd sp 0 f0)]   ; spill xmm0/d0 -> [sp]
   (xpop1ir tgt) ['(ldsd f1 sp 0) ['add 'sp 'sp (slotsz tgt)]]   ; reload [sp] -> xmm1/d1
   (fconst tgt c) (: A (greg tgt 'acc) [['li A (fbits c)] ['movqxr 'f0 A]])   ; bits -> GPR ; GPR -> xmm0 (movq / fmov)
   (fbinop op) (op @ '+ '((addsd f0 f1))                       ; f0 = f0 + f1
                     '* '((mulsd f0 f1))                       ; f0 = f0 * f1
                     '- '((subsd f1 f0) (movsd f0 f1))         ; f1 = f1 - f0 ; f0 = f1
                     '/ '((divsd f1 f0) (movsd f0 f1))         ; f1 = f1 / f0 ; f0 = f1
                     ())                                       ; two-address reg-reg double IR, both targets
   ; cgf: a float subexpr over the fixnum param (cvtsi2sd) and numeric consts -> f0, as neutral IR.
   ; the param load is fxvir, fixnum-guarded with an inline deopt, then int->double.
   (cgf tgt v e d) (? (e = v)   ((fxvir tgt 0 d) + [['cvtsi2sd 'f0 (greg tgt 'acc)]])   ; param: fixnum-guarded untag in acc, cvtsi2sd
                  (gem? e)  (fconst tgt e)                           ; a float constant
                  (whole? e)(fconst tgt (e + 0.0))                   ; an int constant -> promote to double
                  (e @ (op a b)
                       (member? op '(+ - * /)
                        && ((cgf tgt v a d) + (xpushir tgt) + (cgf tgt v b (d + 1)) + (xpop1ir tgt) + (fbinop op)))
                       ()))
   ; cgfp: a float predicate (cmp A B) -> acc 0/1 (A to f1, B to f0, ucomisd then set). x86-only, since
   ; the ucomisd-flag conditions are x86 unsigned cc; jitf, its sole caller, is an x86 self-test helper.
   (cgfp tgt v e d) (: op <e a <>e b <>>e
      cc (op @ '< 'below  '<= 'be  '> 'above  '>= 'ae  'eq)
      ((cgf tgt v a d) + (xpushir tgt) + (cgf tgt v b (d + 1)) + (xpop1ir tgt) + '((ucomisd f1 f0)) + [['set cc (greg tgt 'acc)]]))   ; ucomisd xmm1,xmm0 ; acc = (cc) ? 1 : 0
   ; (jitf '(\ x (cmp <fexpr> <fexpr>))) -- a float predicate over a fixnum arg -> native 0/1 (x86 helper).
   (jitf lam) (: v <>lam (nif (emit (asmx ((cgfp 'amd64 v <>>lam 0) + (loopepi 'amd64)))) (ev lam) lam 1))
   ; --- heap result (the deferred "Have-checked box"): (jitfr '(\ x <fexpr>)) returns a gem, not 0/1.
   ; the lane never allocates because it can't GC (its raw regs aren't roots) -- so it does the alloc
   ; twin of the overflow guard: a room guard (Sp - Hp >= gem_req+floor words) deopts to interp when
   ; the heap is too full to fit the box, exactly as `jo` deopts when a fixnum result leaves range.
   ; fast path (room proven): write [Hp]=lvm_gembox (the jk gem slot, read off g like the
   ; string guard), the double at [Hp+8] with NaN->0 (mk_gem's law), return Hp untagged (an even heap
   ; ptr IS the gem), bump Hp by gem_req. the epilogue stores the pointer (NO putfix) and threads the
   ; advanced Hp on through Continue, so the box is live to the next op and the GC at the next safepoint.
   ; nanc: NaN -> 0.0 in f0 (mk_gem's law). x86: ucomisd xmm0,xmm0 ; jnp +4 ; xorpd (the parity cc +
   ; self-jump as a raw blob). arm64: fcmp d0,d0 ; b.vc ok ; (d0 <- 0) ; ok -- unordered sets V, so vc
   ; skips when not-NaN; zero via a GPR 0 moved into d0.
   (nanc tgt) (id? tgt 'amd64 ? [['raw '(102 15 46 192  123 4  102 15 87 192)]]
                 (: A (greg tgt 'acc) ok (mint 0)
                    (['(ucomisd f0 f0) ['br 'vc ok]] + [['li A 0] ['movqxr 'f0 A]] + [['label ok]])))
   (jitfr lam tgt) (jitfrx lam tgt (ev lam))                        ; 2-arg public: bakes the interpreter deopt (auto-ev + the glaze-x86 tests)
   ; jitfrx: the deopt-parameterized core. auto-ev/tests reach it through jitfr ((ev lam)); the natjit
   ; creation hook (love/glaze/hook.l) calls it directly with `e`, the bytecode twin ala already built --
   ; re-interpreting lam there would re-enter ala -> natjit -> ... (the leaf lane's `e`-deopt law).
   (jitfrx lam tgt interp)
    (: v <>lam
       A (greg tgt 'acc) S (greg tgt 'sp) HP (greg tgt 'hp) IP (greg tgt 'ip) T (greg tgt 'tmp) ok (mint 0)
       ; neutral crew/holo/ IR (both targets): the room-guard skips the deopt by label; the box store is `stsd`;
       ; the deopt is deoptbody (no pending spills here -- cgf balances its own); nanc is arch-aware.
       (nif (emit (asmt tgt ((cgf tgt v <>>lam 0)
          + [['lea A HP 80] ['cmp S A] ['br 'ae ok]]                  ; cmp Sp,Hp+10w ; room? skip deopt
          + (deoptbody tgt 0) + [['label ok]] + (nanc tgt)             ; heap-full -> deopt ; NaN -> 0.0
          + [(jk tgt T 5) ['st HP 0 T]                                  ; [Hp] = lvm_gembox (box header)
              ['stsd HP 8 'f0]                                           ; [Hp+8] = the double
              ['mov A HP] ['add HP HP 16]                               ; acc = Hp (the gem ptr) ; Hp += 16
              ['st S 0 A] ['add IP IP 16] ['ld T IP 0] ['jmpr T]])))   ; [Sp] = gem ; Continue (NO putfix: acc IS the gem)
        interp lam 1))
   (clook k ce) (? (atom? ce) () (k = <<ce) <ce (clook k >ce))   ; counter env: name -> (name regbytes); cgFd/rgof read it
   ; constant hoisting: peek is mopped, so materializing a float constant costs a movabs + movq,
   ; and re-emitting that inside a loop body dominates. so the kernel's distinct float constants
   ; preload into f2..f7 once before the loops (f0/f1 are scratch, f8.. are locals) and the body
   ; references the register. past 6 distinct constants the rest fall back to inline fconst.
   (memv? v l) (? (atom? l) 0 (v = <l) 1 (memv? v >l))
   (fcc e acc) (? (gem? e) (memv? e acc ? acc (e . acc))                    ; collect the distinct constants of e
                  (whole? e) (: v (e + 0.0) (memv? v acc ? acc (v . acc)))
                  (two? e) (fcc <>>e (fcc <>e acc))
                  acc)
   (fccl es acc) (two? es ? (fccl >es (fcc <es acc)) acc)            ; ... over a list of exprs
   (htbl cs i) (? (atom? cs) () (i > 7) () ([<cs i] . htbl >cs (i + 1)))   ; value -> reg (2..7)
   (hlook v ht) (? (atom? ht) -1 (v = <<ht) <><ht (hlook v >ht))
   (hcon tgt v ht) (: r (hlook v ht) (-1 < r ? [['movsd 'f0 (fr r)]] (fconst tgt v)))   ; the hoisted const's reg -> f0, else an inline fconst
   ; the optimizing float-body allocator: a value-numbered DAG (CSE) plus linear-scan allocation.
   ; it compiles a SET of root exprs -- the inner-loop mag and the fvar updates, which share
   ; subexprs like zx*zx -- to register-only code, each value in its own xmm so the CPU runs
   ; independent ops in parallel. leaves (fvars, consts) live in fixed regs from env; binop results
   ; allocate from the working pool, are freed at last use, and are reused in place where an
   ; operand dies.
   (snoc l x) (l + (list x))
   (nthl l i) (i = 0 ? <l (nthl >l (i - 1)))
   (dfind nd dag i) (? (atom? dag) -1 (nd = <dag) i (dfind nd >dag (i + 1)))   ; CSE: an existing node?
   (dadd nd dag) (: i (dfind nd dag 0) (-1 < i ? [i dag] [#dag (snoc dag nd)]))
   (vnum e dag) (two? e ? (: ra (vnum <>e dag) rb (vnum <>>e <>ra)
                             (dadd [<e <ra <rb] <>rb))            ; a binop: (op vn-a vn-b)
                          (dadd ['L e 0] dag))                    ; a leaf: (L datum 0)
   (vnall rs dag acc) (two? rs ? (: r (vnum <rs dag) (vnall >rs <>r (snoc acc <r))) [acc dag])
   (binop? dag v) !(id? <(dag v) 'L)
   (luse v dag rv n) ((: (go i ds best) (? (atom? ds) (member? v rv ? n best)         ; the last index using v (roots live to n)
       (go (i + 1) >ds (binop? dag i && (v = <><ds || v = <>><ds) ? i best)))) 0 dag -1)
   (dies? v i dag rv n) (binop? dag v && ((luse v dag rv n) = i && !(member? v rv)))
   (rgof leaf env) (: e (clook leaf env) (two? e ? <>e (scare 'glaze-leaf leaf)))   ; a leaf -> its fixed reg
   (bcat ps) (foldl (\ a b (a + b)) () ps)                                          ; concat a list of IR pieces
   (efold names i) (two? names ? ([<names i] . efold >names (i + 1)) ())   ; names -> (name reg) from reg i
   ; --- the nested grid owner, D-dimensional -----------------------------------------------------
   ; own a WHOLE counted-loop nest as one native kernel: D nested integer loops over fixnum coords,
   ; M derived-float invariants per cell, then a float recurrence over N float-vars accumulating an
   ; integer count -- one boundary crossing per grid rather than one per cell. mandelbrot's 64x64
   ; grid is the D=2, M=2, N=2 instance; any D>=1 and any M+N<=6 fits.
   ; registers: counters r10.., invariants f8.., fvars next, hoisted consts f2..f7, and the inner
   ; recurrence's working pool is the leftover xmm. on x86 r12.. are callee-saved (D>=3) and the
   ; acc-overflow deopt restores them. the acc add is guarded by the lane law -- overflow deopts to
   ; the interp nest and promotes -- where counters and the float lane need no guard. arity 1, with
   ; a dummy arg ignored, and an integer checksum out.
   (rangeL a b) (a < b ? (a . rangeL (a + 1) b) ())
   (map2 f a b) (two? a ? (f <a <b . map2 f >a >b) ())
   ; cgFd: a derive expr over the counter env ce (loop counters -> cvtsi2sd, the IR carried by cenv)
   ; and the hoisted consts, on [sp] scratch so it never touches an f8.. local. cold, once per cell.
   (cgFd tgt ht ce e) (? (gem? e)   (hcon tgt e ht)
                     (whole? e) (hcon tgt (e + 0.0) ht)
                     (atom? e)  (: cb (clook e ce) (two? cb ? <>cb ()))           ; a counter -> its (cvtsi2sd f0 cntr) IR
                     (e @ (op a b)
                          (member? op '(+ - * /)
                           && ((cgFd tgt ht ce a) + (xpushir tgt) + (cgFd tgt ht ce b) + (xpop1ir tgt) + (fbinop op)))
                          ()))
   (loopcode-gridn dims cnames invs invd fvars updates testexpr cmpop bound lim tgt)
    ; the scalar D-dim kernel as one neutral IR program, both targets: control flow is labels, the
    ; recurrence is allocp's CSE'd scalar-double forms, the cold derives are IR too.
    ; k=r7, acc=r8, counters r9.. through gr; f2..=consts, f8..=invariants, fvars next, rest the
    ; DAG pool. x86 D>=3 spills the callee-saved counters; on arm64 the counters are caller-saved.
    (: D #dims  M #invs  N #fvars  SCR (greg tgt 'acc)    ; SCR: integer scratch (the fbits load + the result)
       cregs (rangeL 10 (10 + D))                                             ; counter regs (phys r10..), outer->inner
       saved (id? tgt 'amd64 ? (rangeL 12 (10 + D)) ())                       ; x86 r12.. callee-saved (D>=3); arm64's counters are caller-saved
       fenv (invs + fvars)
       cenv (map2 (\ nm r [nm [['cvtsi2sd 'f0 (gr r)]]]) cnames cregs)    ; counter name -> its (cvtsi2sd f0 cntr) IR (cgFd reads it)
       ht (htbl (fcc bound (fccl invd (fccl updates (fcc testexpr ())))) 2)   ; distinct consts -> xmm2..7
       derives (foldl (\ a p (a + (cgFd tgt ht cenv >p) + [['movsd (fr (8 + <p)) 'f0]])) () (map2 (\ i d (i . d)) (rangeL 0 M) invd))   ; inv i -> xmm(8+i)
       zerof (foldl (\ a j (a + [['xorpd (fr (8 + M + j)) (fr (8 + M + j))]])) () (rangeL 0 N))   ; fvar j -> xmm(8+M+j) = 0
       aenv ((efold fenv 8) + ht)
       pool ([0 1] + (rangeL (8 + M + N) 16))
       AR (vnall (testexpr . updates) () ())  rootv <AR  dag <>AR  nn #dag
       AL (allocp dag rootv aenv nn pool 'movsd sseops)  bodyc <AL  rg <>AL
       magr (peep rg <rootv 0)  breg (fr (hlook bound ht))
       wb (foldl (\ a j (a + [['movsd (fr (8 + M + j)) (fr (peep rg (rootv (1 + j)) 0))]])) () (rangeL 0 N))   ; fvar <- its update's reg
       iter (['(label itop) ['cmp 'r7 lim] '(br ge iesc)]                   ; while k<lim and (cmp mag bound)
             + bodyc + [['ucomisd (fr magr) breg] ['br (ucmpx cmpop) 'iesc]]
             + wb + '((inc r7) (jmp itop) (label iesc)))
       popsv (foldl (\ a r (a + [['pop (gr r)]])) () (rev saved))           ; x86 D>=3: restore callee-saved counters before deopt
       accb (['(adds r8 r8 r7) '(br vc cont)] + popsv + (deoptbody tgt 0) + '((label cont)))   ; escape: acc += k ; overflow -> deopt
       cell (derives + zerof + '((li r7 0)) + iter + accb)                    ; derive invariants; zero fvars; k=0; recur; acc += k
       (loop-nest ci body) (: cr (gr (cregs ci))  b (dims ci)                 ; counted loops outer->inner, single-step
          tp (intern ("lt" + (show ci)))  en (intern ("le" + (show ci)))
          inner (ci = (D - 1) ? body (loop-nest (ci + 1) body))
          ([['li cr 0] ['label tp] ['cmp cr b] ['br 'ge en]] + inner + [['inc cr] ['jmp tp] ['label en]]))
       pushsv (foldl (\ a r (a + [['push (gr r)]])) () saved)
       popsv2 (foldl (\ a r (a + [['pop (gr r)]])) () (rev saved))
       preload (foldl (\ a e (a + [['li SCR (fbits <e)] ['movqxr (fr <>e) SCR]])) () ht)   ; load each const
       (pushsv + preload + '((li r8 0)) + (loop-nest 0 cell) + popsv2 + [['mov SCR 'r8]]))   ; push saved; consts; acc=0; the nest; pop; result -> SCR
   ; --- the 2-wide packed grid kernel ------------------------------------------------------------
   ; the same D-dim float-recurrence grid, but the innermost dim runs two cells at a time: lanes
   ; [c, c+1] packed [low, high] in each xmm, the recurrence packed, so both pixels advance per op
   ; and the two independent chains hide the mul latency the scalar kernel's serial z is bound by.
   ; the escape test becomes a per-lane mask ANDed into a sticky `alive`, and the per-lane count is
   ; a packed-int vector bumped by `psubq count,alive` -- an alive lane is all-ones = -1, so
   ; subtracting it adds 1. the loop runs while any lane is alive and k < lim, and is bit-exact vs
   ; the scalar interp: identical IEEE per-lane ops, the same count until first escape, capped.
   ; grid2-ok? gates the shape: D<=2, M+N<=4 (invs and fvars at f8..f11), at most 2 inner consts
   ; (f2,f3, leaving f4,f5,f6 for count/alive/scratch), cmpop in {< <=}, an even innermost dim.
   ; SSE2, so x86-64 only.
   (fr n)   (intern ("f" + (show n)))                                                    ; an xmm number -> its IR symbol f0..f15
   (gr p)   (intern ("r" + (show (p - 1))))                                              ; a physical GPR encoding (>=8) -> its abstract-file IR name (r10 -> r9 ..)
   (sseop op)  (op @ '+ 'addpd  '* 'mulpd  '- 'subpd  '/ 'divpd  'movupd)   ; a DAG op -> its packed-double mnemonic (grid2)
   (sseops op) (op @ '+ 'addsd  '* 'mulsd  '- 'subsd  '/ 'divsd  'movsd)    ; ... its scalar-double mnemonic (gridn)
   (ucmpx op) (op @ '<= 'above  '< 'ae  '>= 'be  '> 'below  ())             ; the exit cc after ucomisd: the inverse of the continue cmp
   ; the float-body allocator: CSE plus linear scan, emitting IR forms through mv (the move) and
   ; opmap (op -> mnemonic), so the same walk serves the scalar and the packed kernel.
   (allocp dag rv env n pool mv opmap)
    (: rg {} pl (pin {} 'p pool)
       (pop u) (: p (pl 'p) _ (pin pl 'p >p) <p)
       (push r) (pin pl 'p (r . pl 'p))
       forms ((: (go i acc) (i = n ? acc
          (: nd (dag i)
             (? (id? <nd 'L) (: _ (pin rg i (rgof <>nd env)) (go (i + 1) acc))
                (: op <nd a <>nd b <>>nd ra (peep rg a 0) rb (peep rg b 0)
                   adie (dies? a i dag rv n) bdie (dies? b i dag rv n)
                   dst (adie ? ra (pop 0))
                   _ (bdie && !(rb = dst) ? (push rb) 0) _ (pin rg i dst)
                   mvc (dst = ra ? () [[mv (fr dst) (fr ra)]])                       ; move only when dst is not a's reg
                   (go (i + 1) (acc + mvc + [[(opmap op) (fr dst) (fr rb)]]))))))) 0 ())
       [forms rg])
   (cmppd-imm op) (op @ '<= 2  '< 1  ())                                    ; the cmppd predicate: le / lt, the "continue while small" escape tests
   (grid2-ok? dims invs fvars updates testexpr cmpop bound)                              ; the packed shape fits?
    ((member? cmpop '(< <=)) && (#dims < 3) && (1 < (last dims)) && (0 = ((last dims) % 2))
     && ((#invs + #fvars) < 5)
     && (#(htbl (fcc bound (fccl updates (fcc testexpr ()))) 2) < 3))
   ; GP IR regs: r0=scratch, r5=Ip (the deopt tail), r7=k, r8=acc, r9../r10..=the grid counters.
   ; xmm: f2,f3=consts, f4=count, f5=alive, f6=scratch, f8,f9=invariants, f10,f11=fvars, and
   ; f0,f1,f12..f15 the DAG pool.
   (loopcode-grid2 dims cnames invs invd fvars updates testexpr cmpop bound lim)
    (: D #dims  M #invs  N #fvars
       cregs (rangeL 10 (10 + D))  rinner (last cregs)  iir (gr rinner)                  ; iir = inner counter's IR reg (bumped for lane1)
       fenv (invs + fvars)
       cenv (map2 (\ nm r [nm [['cvtsi2sd 'f0 (gr r)]]]) cnames cregs)               ; counter name -> its (cvtsi2sd f0 cntr) IR (cgFd reads these)
       ht (htbl (fcc bound (fccl updates (fcc testexpr ()))) 2)                          ; only inner-recurrence consts (bound+..) -> f2,f3; derive consts inline -> f4,5,6 free
       breg (fr (hlook bound ht))  pred (cmppd-imm cmpop)
       idx (map2 (\ i d (i . d)) (rangeL 0 M) invd)                                   ; (inv-index . derive-expr) pairs
       d0 (foldl (\ a p (a + (cgFd 'amd64 ht cenv >p) + [['movsd (fr (8 + <p)) 'f0]])) () idx)       ; inv i lane0 -> low of f(8+i)
       d1 (foldl (\ a p (a + (cgFd 'amd64 ht cenv >p) + [['unpcklpd (fr (8 + <p)) 'f0]])) () idx)    ; inv i lane1 (counter+1) -> high
       derives (d0 + [['inc iir]] + d1 + [['dec iir]])                               ; lane1 = the same derive at counter+1
       zerof (foldl (\ a j (a + [['xorpd (fr (8 + M + j)) (fr (8 + M + j))]])) () (rangeL 0 N))   ; fvars := 0 (both lanes)
       aenv ((efold fenv 8) + ht)
       pool ([0 1] + (rangeL (8 + M + N) 16))
       AR (vnall (testexpr . updates) () ())  rootv <AR  dag <>AR  nn #dag
       AL (allocp dag rootv aenv nn pool 'movupd sseop)  bodyc <AL  rg <>AL
       magr (peep rg <rootv 0)
       wb (foldl (\ a j (a + [['movupd (fr (8 + M + j)) (fr (peep rg (rootv (1 + j)) 0))]])) () (rangeL 0 N))   ; fvar <- its update's reg
       maskc [['movupd 'f6 (fr magr)] ['cmppd 'f6 breg pred] '(andpd f5 f6) '(psubq f4 f5) '(movmskpd r0 f5)]
       iter (['(label itop) ['cmp 'r7 lim] '(br ge iesc)] + bodyc + maskc             ; while k<lim and any lane alive
             + '((test r0 r0) (br eq iesc)) + wb + '((inc r7) (jmp itop) (label iesc)))
       deopt (deoptbody 'amd64 0)                                                          ; tail-apply the interp fallback (overflow)
       accb ('((movqrx r0 f4) (adds r8 r8 r0) (br vc cont0)) + deopt                     ; acc += count.lane0 ; guard
             + '((label cont0) (movhlps f6 f4) (movqrx r0 f6) (adds r8 r8 r0) (br vc cont1)) + deopt + '((label cont1)))   ; acc += count.lane1 ; guard
       cell (derives + zerof + '((xorpd f4 f4) (pcmpeqd f5 f5) (li r7 0)) + iter + accb)   ; count=0; alive=ones; k=0; loop; acc
       (loop-nest ci body) (: p (nthl cregs ci) cr (gr p) b (nthl dims ci)               ; counted loops outer->inner; innermost steps by 2
          tp (intern ("lt" + (show ci))) en (intern ("le" + (show ci)))
          inner (? (ci = (D - 1)) body (loop-nest (ci + 1) body))
          step  (? (ci = (D - 1)) [['add cr cr 2]] [['inc cr]])
          ([['li cr 0] ['label tp] ['cmp cr b] ['br 'ge en]] + inner + step + [['jmp tp] ['label en]]))
       preload (foldl (\ a e (a + [['li 'r0 (fbits <e)] ['movqxr (fr <>e) 'r0] ['unpcklpd (fr <>e) (fr <>e)]])) () ht)   ; broadcast each const [c,c]
       (preload + '((li r8 0)) + (loop-nest 0 cell) + '((mov r0 r8))))                    ; IR program (x86): acc=0 ; nest ; result -> r0(=acc)
   ; jitfgridn tries the 2-wide packed kernel when the shape fits and takes the scalar one otherwise.
   ; grid2 is x86-only, since NEON has no movmskpd, so arm64 always rides gridn. both kernels answer an
   ; IR program ending with the checksum in acc, and jitfgridn appends loopepi and assembles for tgt.
   (jitfgridn lam cnames dims invs invd fvars updates testexpr cmpop bound lim tgt)
    (: pk (id? tgt 'amd64 && grid2-ok? dims invs fvars updates testexpr cmpop bound
             ? (loopcode-grid2 dims cnames invs invd fvars updates testexpr cmpop bound lim) ())
       prog (atom? pk ? (loopcode-gridn dims cnames invs invd fvars updates testexpr cmpop bound lim tgt) pk)
       (nif (emit (asmt tgt (prog + (loopepi tgt)))) (ev lam) lam 1))
   ; jitfgrid delegates the 2D mandelbrot-class signature onto the general kernel: D=2 over (px py),
   ; M=2 invariants (cx cy).
   (jitfgrid lam pxn pyn cxd cyd fvars updates testexpr cmpop bound lim tgt)
    (jitfgridn lam '(px py) [pxn pyn] '(cx cy) [cxd cyd] fvars updates testexpr cmpop bound lim tgt)
   ; --- the auto-hook grid recognizer ------------------------------------------------------------
   ; read a float-recurrence grid nest from SOURCE and extract the jitfgrid structure, so ordinary
   ; interpreted code glazes with no hand-call. the shape is a recurrence
   ;   (\ cx cy zx zy it (? (&& (it < LIM) (cmp TEST BOUND)) (mand ..updates.. (it + 1)) it))
   ; driven by two counted loops, with exactly 2 invariants (the point) and 2 fvars (the iterate).
   ; any other shape answers () and stays interpreted.
   (opd e i) (>e i)                                       ; the i-th operand of a form
   (classify ps args)                                               ; mand params vs recursive args -> (fvars updates invariants)
    (two? ps ? (: p <ps a <args r (classify >ps >args)
                  (? (a = p)       [<r <>r (p . (r 2))]            ; an invariant: the point
                     (a = ['+ p 1]) r                              ; a counter: (p + 1)
                                    [(p . <r) (a . <>r) (r 2)]))   ; an fvar and its update
               [() () ()])
   (posof k l i) (k = <l ? i (posof k >l (i + 1)))
   (grok glam mps mbody cbody gbody tgt)                           ; -> a native grid closure, or () when the shape misses
    (: cond (opd mbody 0)  then (opd mbody 1)
       c2 (opd cond 1)  lim (opd (opd cond 0) 1)  cmpop <c2  testexpr (opd c2 0)  bound (opd c2 1)
       cl (classify mps >then)  fvars <cl  updates <>cl  invs (cl 2)
       pyn (opd (opd cbody 0) 1)  margs >(opd ((>(opd cbody 1)) 2) 1)
       cxd (margs (posof (invs 0) mps 0))  cyd (margs (posof (invs 1) mps 0))
       pxn (opd (opd gbody 0) 1)
       (2 = #invs && 2 = #fvars
          ? (jitfgrid glam pxn pyn cxd cyd fvars updates testexpr cmpop bound lim tgt) ()))
   ; --- the neutral-IR lanes ---------------------------------------------------------------------
   ; greg maps a logical role to the abstract holo register, per target. x86 lands g/Ip/Hp/Sp in
   ; rdi/rsi/rdx/rcx (abstract r6/r5/r2/r1, SysV's first four args -- the convention IS the calling
   ; convention); aarch64's AAPCS lands them in x0..x3, so the accumulator moves off r0. anchor is
   ; the machine stack-pointer snapshot for the deopt unwind; scratch is mulo/rem's extra register,
   ; unused on x86.
   (greg tgt role)                                                     ; a role table, not a chain: read it in columns
    (: x (id? tgt 'amd64)
       (? (id? role 'g)       (x ? 'r6 'r0)   (id? role 'ip)      (x ? 'r5 'r1)
          (id? role 'hp)      (x ? 'r2 'r2)   (id? role 'sp)      (x ? 'r1 'r3)
          (id? role 'acc)     (x ? 'r0 'r4)   (id? role 'tmp)     (x ? 'r7 'r5)
          (id? role 'anchor)  (x ? 'r8 'r9)   (id? role 'scratch) (x ? 'r4 'r10) ()))
   (ccof op)  (op @ '< 'lt  '<= 'le  '> 'gt  '>= 'ge  '= 'eq  ())      ; a source comparison -> its IR condition code
   (bitop op) (op @ '& 'and  '| 'or  '^ 'xor  ())                      ; a source bitwise symbol -> its IR ALU op

   ; fxir: fetch param i from Sp[i], fixnum-guard the low bit, untag into acc. a non-fixnum jumps to deopt.
   (fxir tgt i) (: A (greg tgt 'acc) S (greg tgt 'sp) [['ld A S (8 * i)] ['test A 1] '(br eq deopt) ['sar A 1]])
   ; arithir: acc = (the popped a) OP acc, overflow -> deopt. - keeps the a-b order; * uses the fused mulo.
   (arithir tgt op) (: A (greg tgt 'acc) T (greg tgt 'tmp) SC (greg tgt 'scratch)
      (op @ '+ [['adds A A T] '(br vs deopt)]
            '- [['subs T T A] '(br vs deopt) ['mov A T]]
            '* [['mulo A A T SC 'deopt]]
            ()))
   (divir tgt op) (: A (greg tgt 'acc) T (greg tgt 'tmp) SC (greg tgt 'scratch)   ; the caller emits the /-by-0 guard
      (op @ '// [['div A T A]]  '% [['rem A T A SC]]  ()))
   ; cgir: an arith expr e over params ps -> IR ops accumulating in acc, a param fetched from its Sp
   ; slot. nested operands ride the machine stack, ? gets fresh mint labels, and one deopt label is
   ; shared. serves both the leaf (ps one param) and the n-ary lane.
   (cgir tgt ps e)
    (: A (greg tgt 'acc) T (greg tgt 'tmp)
       (bin a b fin) ((cgir tgt ps a) + [['push A]] + (cgir tgt ps b) + fin)
       (? (member? e ps) (fxir tgt (argix e ps 0))
          (whole? e) [['li A e]]
          (e @ ('? c t f) (: el (mint 0) en (mint 0)
                             ((cgir tgt ps c) + [['cmp A 0] ['br 'le el]] + (cgir tgt ps t)
                              + [['jmp en] ['label el]] + (cgir tgt ps f) + [['label en]]))
               (op a b) (? (member? op '(+ - *))       (bin a b ([['pop T]] + (arithir tgt op)))
                           (member? op '(// %))        (bin a b ([['test A A] '(br eq deopt) ['pop T]] + (divir tgt op)))
                           (member? op '(& | ^))       (bin a b [['pop T] [(bitop op) A A T]])
                           (member? op '(< <= > >= =)) (bin a b [['pop T] ['cmp T A] ['set (ccof op) A]])
                           ())
               ())))
   ; deoptir: the shared deopt block -- restore the machine sp from the anchor, then tail-apply the interp
   ; fallback. arity 1 jumps to value[1], the closure re-applied to the arg; arity>=2 jumps to value[1]+16,
   ; its body, where the saturated args already sit on Sp. mov vs lea is the lone per-arity difference.
   (deoptir tgt bodyp) (: AN (greg tgt 'anchor) IP (greg tgt 'ip) T (greg tgt 'tmp)
      [['label 'deopt] ['lea 'sp AN 0] ['ld T IP 8] (bodyp ? ['lea IP T 16] ['mov IP T]) ['ld T IP 0] ['jmpr T]])
   ; coreir: the whole-function IR -- prologue (anchor = the machine sp) ; the body ; putfix epilogue +
   ; Continue (Ip+=16; jmp [Ip]) ; the deopt block. (jitcore assembles + installs it; tests assemble it.)
   (coreir tgt ps body deoptblk)
    (: AN (greg tgt 'anchor) S (greg tgt 'sp) A (greg tgt 'acc) IP (greg tgt 'ip) T (greg tgt 'tmp)
       ([['lea AN 'sp 0]] + (cgir tgt ps body) + [['adds A A A] '(br vs deopt) ['or A A 1] ['st S 0 A] ['add IP IP 16] ['ld T IP 0] ['jmpr T]] + deoptblk))
   ; jitcore: assemble coreir for tgt and install as a nat of the given arity (deopt -> interp).
   (jitcore tgt ps body deoptblk arity lam)
    (nif (emit (asmt tgt (coreir tgt ps body deoptblk))) (ev lam) lam arity)
   ; (jitir '(\ x E) tgt) -- an arity-1 leaf. (jitnir '(\ p0 .. E) tgt) -- arity>=2 (natn curries to saturation).
   (jitir lam tgt)  (jitcore tgt [<>lam] <>>lam (deoptir tgt 0) 1 lam)
   (jitnir lam tgt) (: ps (init >lam) body (last >lam) (jitcore tgt ps body (deoptir tgt 1) #ps lam))


   ; --- the loop-body neutral IR ------------------------------------------------------------------
   ; cgvir is cgir's arith grammar for the loop lanes, whose state fills the loop registers and leaves
   ; no free reg for an anchor. so the overflow and /-by-0 guards deopt INLINE: unwind the d pending
   ; machine-stack pushes, then tail-apply interp (value[1]) to the arg still in Sp[0], where cgir
   ; restores from its anchor. a var is loaded by `varld` -- the counter from its raw register, an
   ; n-loop param via fxir.
   ; deoptbody carries no branch or label of its own; ldeopt, zdeopt and the *-overflow lane wrap it
   ; with the guard branch and a skip label.
   (deoptbody tgt d) (: A (greg tgt 'acc) IP (greg tgt 'ip)
      ((d > 0 ? [['add 'sp 'sp ((slotsz tgt) * d)]] ()) + [['ld A IP 8] ['mov IP A] ['ld A IP 0] ['jmpr A]]))
   (ldeopt tgt d) (: ok (mint 0) ([['br 'vc ok]] + (deoptbody tgt d) + [['label ok]]))   ; skip the deopt unless overflow
   (zdeopt tgt d) (: ok (mint 0) ([['br 'ne ok]] + (deoptbody tgt d) + [['label ok]]))   ; skip unless nonzero: the /-by-0 and fixnum-input guard
   (fxvir tgt i d) (: A (greg tgt 'acc) S (greg tgt 'sp)   ; load Sp[i] tagged, fixnum-guard with an inline deopt at depth d, untag
      ([['ld A S (8 * i)] ['test A 1]] + (zdeopt tgt d) + [['sar A 1]]))
   (arithl tgt op d) (: A (greg tgt 'acc) T (greg tgt 'tmp) SC (greg tgt 'scratch) dl (mint 0) ok (mint 0)   ; acc = (popped a) OP acc, overflow -> inline deopt
      (op @ '+ ([['adds A A T]] + (ldeopt tgt d))
            '* ([['mulo A A T SC dl]] + [['jmp ok] ['label dl]] + (deoptbody tgt d) + [['label ok]])   ; mulo sets no arm64 flag, so it branches; SC is the smulh scratch
            '- ([['subs T T A]] + (ldeopt tgt d) + [['mov A T]])
            ()))
   (divl tgt op d) (: A (greg tgt 'acc) T (greg tgt 'tmp) SC (greg tgt 'scratch)   ; a in tmp (popped), b in acc -> acc; b == 0 deopts
      ([['pop T] ['test A A]] + (zdeopt tgt d) + (op @ '// [['div A T A]]  '% [['rem A T A SC]]  ())))
   (cgvir tgt varld e d)
    (: A (greg tgt 'acc) T (greg tgt 'tmp)
       (bin a b fin) ((cgvir tgt varld a d) + [['push A]] + (cgvir tgt varld b (d + 1)) + fin)
       vl (varld e d)
       (? vl vl                                              ; a var -> its loader IR (acc <- the var, at depth d)
          (whole? e) [['li A e]]
          (e @ ('? c t f) (: el (mint 0) en (mint 0)
                             ((cgvir tgt varld c d) + [['cmp A 0] ['br 'le el]] + (cgvir tgt varld t d)
                              + [['jmp en] ['label el]] + (cgvir tgt varld f d) + [['label en]]))
               (op a b) (? (member? op '(+ - *))       (bin a b ([['pop T]] + (arithl tgt op d)))
                           (member? op '(// %))        (bin a b (divl tgt op d))
                           (member? op '(& | ^))       (bin a b [['pop T] [(bitop op) A A T]])
                           (member? op '(< <= > >= =)) (bin a b [['pop T] ['cmp T A] ['set (ccof op) A]])
                           ())
               ())))
   ; --- the neutral-IR group lane ----------------------------------------------------------------
   ; a group of mutually recursive first-order integer fns to native, both targets: `call H_g` is
   ; (call <name>), each H opens (label <name>), and the assembler resolves the rel offsets. IR
   ; chunks concatenate flat through bcat. it covers the full op grammar: int arith, TCO, value-mode
   ; cons, and the string/map/cask lanes.
   ; arg-register ABI: args in the callee-saved bank (x86 rbx/r13/r14/r15, arm64 x19..x22) and the
   ; OVF anchor in another callee-saved reg. a call passes args on the machine stack, and each H
   ; saves, loads and restores its own arg regs so a caller's args survive a nested call; on arm64 it
   ; also saves lr, which bl clobbers, where x86's CALL/RET keeps the return address on the stack.
   ; mov to or from sp is `lea`: the ORR-based mov reads and writes XZR for reg 31, never sp.
   (slotsz tgt) (id? tgt 'amd64 ? 8 16)                                          ; the push/pop unit
   (govf tgt)   (id? tgt 'amd64 ? 'r11 'r23)                                     ; the OVF anchor reg: x86 r12 / arm64 x23
   (argregir tgt i) ((id? tgt 'amd64 ? '(r3 r12 r13 r14) '(r19 r20 r21 r22)) i)
   (hsave tgt)  (id? tgt 'amd64 ? () ['(push lr)])                            ; arm64's H spills lr; x86 rides CALL/RET's stack
   (hrest tgt)  (id? tgt 'amd64 ? () ['(pop lr)])
   (saveargsir tgt k) (foldl (\ a i (bcat [a [['push (argregir tgt i)]]])) () (rangeL 0 k))
   (restargsir tgt k) (foldl (\ a i (bcat [a [['pop (argregir tgt ((k - 1) - i))]]])) () (rangeL 0 k))
   (loadargsir tgt k) (foldl (\ a i (bcat [a [['ld (argregir tgt i) 'sp ((slotsz tgt) * ((2 * k) - i))]]])) () (rangeL 0 k))   ; argreg_i <- the passed arg at [sp + slotsz*(2k-i)]
   (garith tgt op) (: A (greg tgt 'acc) T (greg tgt 'tmp) SC (greg tgt 'scratch)   ; acc = (popped a) OP acc, overflow -> OVF
      (op @ '+ [['adds A A T] '(br vs OVF)]
            '- [['subs T T A] '(br vs OVF) ['mov A T]]
            '* [['mulo A A T SC 'OVF]]
            ()))
   ; the register/immediate operand peephole: the generic binary lane is push-acc .. pop-tmp per op,
   ; which is the gap against a register machine. when one operand is already a param (so, in an arg
   ; reg) or a small immediate, combine it directly -- acc OP= reg|imm, no stack traffic -- and
   ; primes' (d * d), (n < (d * d)), (d + 1) and (0 = ..) all run flat. the ALU takes a reg or an imm
   ; for adds/subs/cmp on both targets, arm64's imm being 12-bit; mulo is register-only on arm64, so
   ; the imm lane skips `*`, and the case that matters -- d*d -- is a register anyway.
   (pimm? x) (whole? x && (x < 0 ? (0 - x) x) <= 4095)                      ; an immediate both arches add/sub/cmp directly
   (preg tgt ps x) (member? x ps && argregir tgt (argix x ps 0))            ; x a param -> its arg reg, else ()
   (garith-r tgt op R) (: A (greg tgt 'acc) SC (greg tgt 'scratch)              ; acc = acc OP R (R a reg), overflow -> OVF
      (? (id? op '+) [['adds A A R] '(br vs OVF)]
         (id? op '-) [['subs A A R] '(br vs OVF)]
         (id? op '*) [['mulo A A R SC 'OVF]] ()))
   (garith-i tgt op imm) (: A (greg tgt 'acc)                                   ; acc = acc OP imm (no `*`), overflow -> OVF
      (op @ '+ [['adds A A imm] '(br vs OVF)]
            '- [['subs A A imm] '(br vs OVF)]
            ()))
   (flipc op) (op @ '< '>  '<= '>=  '> '<  '>= '<=  op)   ; mirror a cmp's operands; = stays
   (deoptmn tgt bodyp) (: IP (greg tgt 'ip) T (greg tgt 'tmp)                    ; tail-apply interp (value[1]); bodyp -> its BODY (+16, arity>=2)
      [['ld T IP 8] (bodyp ? ['lea IP T 16] ['mov IP T]) ['ld T IP 0] ['jmpr T]])
   ; cggir: a group expr e over params ps, which live in arg regs; syms are the group fn names, the call
   ; targets. a call pushes its args, `call H_g`, pops them; arith overflow and a /-by-0 jump to the
   ; shared OVF handler. the operand patterns are exact-arity, matching auto.l's groupok? grammar.
   (cggir tgt syms ps e)
    (: A (greg tgt 'acc) T (greg tgt 'tmp)
       (go x) (cggir tgt syms ps x)
       (? (member? e ps) [['mov A (argregir tgt (argix e ps 0))]]
          (whole? e) [['li A e]]
          (atom? e) ()
          (member? <e syms)
            (bcat [(gargsir tgt syms ps >e) [['call <e]] [['add 'sp 'sp ((slotsz tgt) * #>e)]]])
          (e @ (op a b)
            (? (member? op '(+ - *))
                 (: pa (preg tgt ps a) pb (preg tgt ps b)
                    (? pb                             (bcat [(go a) (garith-r tgt op pb)])   ; (E op p): E->acc; acc OP= p_reg
                       (pimm? b && !(id? op '*))   (bcat [(go a) (garith-i tgt op b)])    ; (E op C)
                       (pa && member? op '(+ *))   (bcat [(go b) (garith-r tgt op pa)])   ; (p op E): commute
                       (pimm? a && id? op '+)     (bcat [(go b) (garith-i tgt op a)])    ; (C + E): commute; an imm `*` has no arm64 form, so it stays generic
                       (bcat [(go a) [['push A]] (go b) [['pop T]] (garith tgt op)])))   ; generic
               (member? op '(// %))
                 (bcat [(go a) [['push A]] (go b) [['test A A] '(br eq OVF) ['pop T]] (divir tgt op)])
               (member? op '(& | ^))
                 (bcat [(go a) [['push A]] (go b) [['pop T] [(bitop op) A A T]]])
               (member? op '(< <= > >= =))
                 (: pa (preg tgt ps a) pb (preg tgt ps b)
                    (? pb         (bcat [(go a) [['cmp A pb] ['set (ccof op) A]]])             ; (E op p): cmp acc,p_reg
                       (pimm? b)  (bcat [(go a) [['cmp A b]  ['set (ccof op) A]]])             ; (E op C)
                       pa         (bcat [(go b) [['cmp pa A] ['set (ccof op) A]]])             ; (p op E): flags p-acc = a-b, cc kept
                       (pimm? a)  (bcat [(go b) [['cmp A a]  ['set (ccof (flipc op)) A]]])     ; (C op E): cmp acc,C = b-a, flip cc
                       (bcat [(go a) [['push A]] (go b) [['pop T] ['cmp T A] ['set (ccof op) A]]])))   ; generic
               ())
            (op a b c)
            (? (id? op '?)
                 (: el (mint 0) en (mint 0)
                    (bcat [(go a) [['cmp A 0] ['br 'le el]] (go b)
                            [['jmp en] ['label el]] (go c) [['label en]]]))
               (id? op 'peep) (: R (argregir tgt (argix a ps 0)) inl (mint 0) endl (mint 0)   ; (peep STR i d): i unsigned-vs-len, out of range -> d, else the byte at [STR+i+16]
                  (bcat [(go b)
                         [['ld T R 8] ['cmp A T] ['br 'below inl] ['li A (whole? c ? c 0)] ['jmp endl] ['label inl] ['ldxb A R A 1 16] ['label endl]]]))
               (id? op 'pin) (: R (argregir tgt (argix a ps 0))                              ; (pin C i V): store V's low byte at C's backing[i], answer C
                  (bcat [(go b) [['push A]] (go c)                                            ; i -> acc, push; V -> acc
                         [['pop T] ['ld 'r8 R 8] ['stxb 'r8 T 1 16 A] ['mov A R]]]))          ; pop T(=i); r8 = the backing str; [backing+i+16] = V.lo; acc = C
               (id? op 'mpeep) (: R (argregir tgt (argix a ps 0)) lp (mint 0) ht (mint 0) ms (mint 0) en (mint 0)   ; (mpeep MAP k d): the open-addressed probe -> the raw value in acc, a gap-miss -> d
                  (bcat [(go b) [['leax 'r10 A A 1 1] ['mov A R]] (relabel (mpeep-ir (whole? c ? c 0)) lp ht ms en 'deopt)]))   ; k -> acc; r10 = tag(k); acc = the map; then the probe, labels gensym'd
               (id? op 'mpin) (: R (argregir tgt (argix a ps 0)) lp (mint 0) ht (mint 0) ms (mint 0) en (mint 0)   ; (mpin MAP k v): probe and update in place; a grow-due miss -> OVF; answers the map
                  (bcat [(go c) [['push A]] (go b) [['leax 'r10 A A 1 1] ['mov A R]] (relabel mpin-ir lp ht ms en 'OVF) [['mov A R]]]))   ; v -> acc, pushed as the spill mpin-ir's own (pop r8) takes; k -> acc; acc = the map, threaded back out
               ())
            (op a)
            (? (id? op 'tally) (: R (argregir tgt (argix a ps 0)) [['ld A R 8]])              ; (tally STR): acc = the raw len at [STR+8]
               (id? op 'two?) (: z (mint 0) en (mint 0)                                       ; (two? E): is E a chain? -> 1/0, total, since a non-chain reads 0
                  (bcat [(go a) [['test A 1] ['br 'ne z] ['ld T A 0] (jk tgt 'r8 0) ['cmp T 'r8] ['br 'ne z] ['li A 1] ['jmp en] ['label z] ['li A 0] ['label en]]]))
               (id? op 'cap) (bcat [(go a) [['test A 1] ['br 'ne 'OVF] ['ld T A 0] (jk tgt 'r8 0) ['cmp T 'r8] ['br 'ne 'OVF] ['ld A A 8]]])    ; <E: a non-chain deopts, and it always runs under a two?
               (id? op 'cup) (bcat [(go a) [['test A 1] ['br 'ne 'OVF] ['ld T A 0] (jk tgt 'r8 0) ['cmp T 'r8] ['br 'ne 'OVF] ['ld A A 16]]])   ; >E: the same guard, then acc = [E+16]
               ())
            ())))
   (gargsir tgt syms ps as) (atom? as ? () (bcat [(cggir tgt syms ps <as) [['push (greg tgt 'acc)]] (gargsir tgt syms ps >as)]))
   ; relabel: gensym the map probe's fixed labels per call site and point deopt at the given group
   ; label, so the same probe IR can be spliced N times into one group assemble without collisions.
   (relabel ir lp ht ms en dp) (map (f \ map (x \ (x @ 'loop lp  'hit ht  'miss ms  'end en  'deopt dp  x)) f) ir)
   ; consir: build a chain cell on the heap -> acc. the room guard comes first, so no GC fires
   ; mid-native, then [Hp] = the chain kind, [Hp+8] = cap (the popped a), [Hp+16] = cup, acc = Hp --
   ; an even pointer IS the value, so no putfix -- and Hp += 3 words. r9/r10 are free scratch in the
   ; group ABI (args r3/r12-r14, anchor r11, acc r0, tmp r7, sp r1, hp r2, ip r5).
   (consir tgt) (: A (greg tgt 'acc) T (greg tgt 'tmp) S (greg tgt 'sp) HP (greg tgt 'hp)
      [['lea 'r9 HP 88] ['cmp S 'r9] '(br below OVF)                             ; room for 3w+floor; Sp < Hp+88 -> heap full -> deopt
        (jk tgt 'r10 0) ['st HP 0 'r10]                                      ; [Hp] = chain header
        ['st HP 8 T] ['st HP 16 A]                                               ; [Hp+8] = cap (the popped a); [Hp+16] = cup (b)
        ['mov A HP] ['add HP HP 24]])                                            ; acc = Hp (result ptr); bump Hp by 3 words
   ; cggvir is VALUE-mode codegen: e yields a love value -- a heap pointer or a tagged scalar -- in acc
   ; rather than a raw int. an int const becomes the tagged value, a param re-tags its raw arg reg, and
   ; (? T A B) takes its test through cggir and its branches through here.
   (cggvir tgt syms ps e)
    (: A (greg tgt 'acc) T (greg tgt 'tmp)
      (? (whole? e) [['li A ((2 * e) + 1)]]                                        ; the tagged value C
         (member? e ps) (: R (argregir tgt (argix e ps 0)) [['leax A R R 1 1]])    ; a param: acc = 2*reg + 1
         (atom? e) ()
         (member? <e syms) (cggir tgt syms ps e)                                   ; a value-group call: cggir's call already answers the value
         (e @ ('link a b) (bcat [(cggvir tgt syms ps a) [['push A]] (cggvir tgt syms ps b) [['pop T]] (consir tgt)])   ; A ; push ; B ; pop T(=A) ; cons(T,A)
              ('? c t f) (: el (mint 0) en (mint 0)
                 (bcat [(cggir tgt syms ps c) [['cmp A 0] ['br 'le el]] (cggvir tgt syms ps t)
                        [['jmp en] ['label el]] (cggvir tgt syms ps f) [['label en]]]))
              ())))
   ; cggtir: e in TAIL position. a tail self-call of matching arity computes the new args from the OLD
   ; arg regs onto the machine stack, pops them into the arg regs, then jumps Lbody, past save/load --
   ; the frame is reused, so a counted loop is O(1) stack with no call/ret per iteration. a tail ?
   ; recurses tail into both branches, its test staying non-tail. anything else goes to cggir and falls
   ; through to restore + ret. self only: a sibling tail-call stays a plain `call`.
   (cggtir tgt syms ps e selfnm Lbody)
    (: A (greg tgt 'acc) k #ps
       (? (two? e && id? <e selfnm && #>e = k)                    ; a tail self-call -> loop in place
            (bcat [(gargsir tgt syms ps >e)                                      ; push the new args, all read from the OLD regs
                   (foldl (\ a i (bcat [a [['pop (argregir tgt ((k - 1) - i))]]])) () (rangeL 0 k))   ; pop into the arg regs, in reverse
                   [['jmp Lbody]]])
          (e @ ('? c t f) (: el (mint 0) en (mint 0)                            ; a tail ? -> test non-tail, both branches tail
                 (bcat [(cggir tgt syms ps c) [['cmp A 0] ['br 'le el]] (cggtir tgt syms ps t selfnm Lbody)
                        [['jmp en] ['label el]] (cggtir tgt syms ps f selfnm Lbody) [['label en]]]))
               (cggir tgt syms ps e))))                                          ; any other tail expr -> a value in acc
   ; mkhir: one function -- label; [push lr]; save arg regs; load the passed args; Lbody; body; restore;
   ; [pop lr]; ret. a value body (it conses) takes cggvir and no TCO; an int body takes cggtir.
   (mkhir tgt syms nm ps body)
    (: k #ps Lbody (mint 0)
       (bcat [[['label nm]] (hsave tgt) (saveargsir tgt k) (loadargsir tgt k) [['label Lbody]]
               (hasl body ? (cggvir tgt syms ps body) (cggtir tgt syms ps body nm Lbody))
               (restargsir tgt k) (hrest tgt) ['(ret)]]))
   ; mkouterir: the entry's OUTER -- save the anchor and arg regs; anchor = sp; fixnum-guard and push
   ; the args; call H_entry; pop; restore; putfix + Continue. OVF restores sp from the anchor, which
   ; abandons the group's frames, then falls into DEOPT.
   ; it also partitions the entry's external args by kind: int (untagged), string|map|cask (kind-guarded,
   ; raw), chain (raw, unguarded), point (nom|mint-guarded, raw).
   (mkouterir tgt k entry arity valt smask mmask bufmask cmask pmask)              ; valt: a value-typed entry stores acc as-is, with no putfix
    (: A (greg tgt 'acc) S (greg tgt 'sp) IP (greg tgt 'ip) T (greg tgt 'tmp) OVFR (govf tgt) ss (slotsz tgt)
       strk 1 mapk 2 bufk 6                                                            ; the jk slots of the kind sentinels the raw-param guards check against
       (pusharg1 i) (: o (8 * i)                                                   ; one external arg: load Sp[i] -> acc, guard by kind, push
          (? (member? i cmask) [['ld A S o] ['push A]]                          ; a chain: raw, no guard -- two? is total and cap/cup deopt a non-chain inline
             (member? i smask || (member? i mmask || member? i bufmask))     ; a string, map or cask: a fixnum cannot be a heap object, and [arg]'s kind must match
               (: kind (? (member? i mmask) mapk (member? i bufmask) bufk strk)
                  [['ld A S o] ['test A 1] '(br ne OVF) ['ld T A 0] (jk tgt 'r8 kind) ['cmp T 'r8] '(br ne OVF) ['push A]])   ; an odd (fixnum) word deopts, then the kind word must match
             (member? i pmask)                                                     ; a point key: nom or mint, pushed raw -- word identity is its whole equality,
               (: pok (mint 0)                                                     ; and a charm deopts, since only interp knows the numeric bridges like (2 = 2.0)
                  [['ld A S o] ['test A 1] '(br ne OVF)
                    ['ld T A 0] (jk tgt 'r8 3) ['cmp T 'r8] ['br 'eq pok]
                    (jk tgt 'r8 4) ['cmp T 'r8] '(br ne OVF) ['label pok] ['push A]])
             [['ld A S o] ['test A 1] '(br eq OVF) ['sar A 1] ['push A]]))    ; an int: fixnum-guarded, untagged, pushed
       pushargs (foldl (\ a i (bcat [a (pusharg1 i)])) () (rangeL 0 k))
       epi (valt ? [['st S 0 A] ['add IP IP 16] ['ld T IP 0] ['jmpr T]]                                  ; a value: store acc as-is, then Continue with no putfix
                 [['adds A A A] '(br vs DEOPT) ['or A A 1] ['st S 0 A] ['add IP IP 16] ['ld T IP 0] ['jmpr T]])   ; an int: putfix, overflow -> deopt, then Continue
       (bcat [(hsave tgt) [['push OVFR]] (saveargsir tgt k) [['lea OVFR 'sp 0]] pushargs
              [['call entry] ['add 'sp 'sp (ss * k)]] (restargsir tgt k) [['pop OVFR]] (hrest tgt)   ; lr is saved and restored around `call entry`: on arm64 bl clobbers x30, which is the live return address the threaded tail-call chain eventually rets through, and the outer tail-jumps onward, so it must hand x30 back intact
              epi
              [['label 'OVF] ['lea 'sp OVFR 0]] (restargsir tgt k) [['pop OVFR]] (hrest tgt)
              [['label 'DEOPT]] (deoptmn tgt (arity > 1))]))
   ; jgir-ok? is the op gate: does the group lane implement every op this group uses, at the arity
   ; cggir lowers? autogroup routes here when it answers true and leaves the group interpreted when
   ; it does not. it is reached only after auto.l's recognizer has proven the group glazeable --
   ; shapes, arities, the int/chain/value typing, the interprocedural string inference -- so it does
   ; not re-derive the grammar; it is the backstop for the natjit path, which has no recognizer.
   ; jgir-arity is the operand count cggir/cggvir read, form tally included, so a head at any other
   ; arity would have its extra operands silently dropped.
   jgir-arity {'+ 3 '- 3 '* 3 '// 3 '% 3 '& 3 '| 3 '^ 3 '< 3 '<= 3 '> 3 '>= 3 '= 3 'link 3
               '? 4 'peep 4 'pin 4 'mpeep 4 'mpin 4
               'tally 2 'two? 2 'cap 2 'cup 2}
   (jgir-ops-ok? syms e)                                          ; every head in e is a group call or a supported op at its own arity
    (? (atom? e) (gem? e || twin? e ? 0 1)                       ; a smooth-numeric leaf is out of the integer lane
       (: op <e
          n (jgir-arity op)
          ((member? op syms || (n && #e = n)) && all (x \ jgir-ops-ok? syms x) >e)))
   (jgir-ok? defs) (: syms (map (d \ <<d) defs)
      (all (d \ jgir-ops-ok? syms >d) defs))
   ; jgir-lrf -- the ((mpeep..) (peep..) (mpin..) (pin..) def..) tail the deopt fallback re-evaluates.
   (jgir-lrf entry ds) (atom? ds ? [entry] (: d <ds (<d . (>d . jgir-lrf entry >ds))))
   ; (jitgroupir defs entry tgt): defs are ((nameform . body) ..), and the entry is the group's value.
   ; the deopt fallback is (ev <the whole group>), and post-rebind `ev` IS the glaze, so it re-glazes
   ; the group -- harmless, a never-deopting native gets rebuilt -- which is why the mpeep/mpin ->
   ; peep/pin aliases (ldxb/stxb) must assemble on every target, arm64 included. mpeep and mpin are
   ; glaze-internal, not real fns: the aliases read the previous, global binding by the sequence law
   ; rather than recursing, so a non-map lane keeps the core accessors.
   (jitgroupir defs entry tgt)
    (jitgroupirx defs entry tgt (ev (': . ('(mpeep mc mk md) . ('(peep mc mk md) . ('(mpin mc mk mv) . ('(pin mc mk mv) . jgir-lrf entry defs)))))) ())
   ; jitgroupirx takes the deopt target directly instead. the natjit hook passes `e`, the bytecode
   ; twin, so a group-bearing closure native-backs without re-entering the evaluator mid-ala, which
   ; jitgroupir's (ev ..) fallback would. pure otherwise: assemble and install, no ev.
   (jitgroupirx defs entry tgt deopt src)
    (: parse (map (d \ (: nf <d [<nf >nf >d])) defs)            ; (name params body) per fn
       syms (map cap parse)                                                            ; the fn names, which are the call targets
       efn (gfind (f \ id? <f entry) parse)  eps (efn 1)  eb (efn 2)  k #eps
       ord (efn . filter (f \ !(id? <f entry)) parse)                      ; the entry first, so H_entry sits right after the OUTER
       valt (hasl eb)                                                                 ; a value-typed entry (its body conses) takes the value epilogue
       info (map (f \ [<f (f 1) (f 2) #(f 1)]) parse)                   ; (name params body arity), for the interprocedural string fixpoint
       smap0 (map (f \ <f . filter (j \ suse? (f 2) ((f 1) j)) (rangeL 0 (f 3))) info)   ; per-fn direct string params (peep/tally/pin)
       smask (gsm entry (smfix info smap0))                                            ; the entry's string params at the fixpoint -> raw pointer, kind-guarded
       mmask (filter (j \ muse? eb (eps j)) (rangeL 0 k))                         ; entry map params (mpeep/mpin)
       bufmask (filter (j \ buse? eb (eps j)) (rangeL 0 k))                       ; entry cask params (pin)
       cmask (filter (j \ cuse? eb (eps j)) (rangeL 0 k))                         ; entry chain params (cap/cup/two?) -> raw, no guard
       pmask (filter (j \ nuse? eb (eps j) && !(member? j cmask)) (rangeL 0 k))   ; entry point keys -> raw, nom|mint-guarded; a chain-used param is never a point
       outer (mkouterir tgt k entry k valt smask mmask bufmask cmask pmask)
       Hs (foldl (\ a f (bcat [a (mkhir tgt syms <f (f 1) (f 2))])) () ord)
       elam ('\ . eps + [eb])                                        ; the entry's source lambda, the =/show src fallback
       (nif (emit ((assemble tgt (bcat [outer Hs])) 'bytes)) deopt (two? src ? src elam) k))   ; the OUTER surface if the caller gave one, else elam
)
