; holo -- a tiny ASSEMBLER over a neutral, low-level IR, written in love. the IR is a flat list of
; instruction forms (a RISC-ish three-address surface over abstract registers r0..r14 + sp); a
; BACKEND lowers each form to machine-code bytes. this file is the arch-NEUTRAL half: the IR shape,
; the two-pass layout (assign addresses, then resolve label-relative fixups), the backend registry,
; and the byte/hex plumbing every target shares. two backends ride on it -- crew/holo/x64.l (x86-64) and
; crew/holo/arm64.l (aarch64) -- and a third drops in the same way: pin a `lower`/`patch` pair under a
; target name and the driver below never changes. load with `-l crew/holo/holo.l -l crew/holo/x64.l` (and/or
; `-l crew/holo/arm64.l`); the gate is crew/holo/holotest.l.
;
; the IR (each form a list, the head an op symbol):
;   (label name)            ; a named address (emits no bytes)
;   (li d imm)              ; d <- immediate
;   (mov d s)              ; d <- s            (register move)
;   (add d a b) (sub ..)   ; d <- a OP b       b a register OR an immediate; also and/or/xor/imul
;   (neg d) (not d) (inc d) (dec d)            ; in-place unary
;   (cmp a b)              ; set flags from a - b
;   (ld d base off)        ; d <- [base+off]   (64-bit load)
;   (st base off s)        ; [base+off] <- s   (64-bit store)
;   (ld1/ld2/ld4 d base off)                   ; SIGN-extending sized loads (C's char/short/int)
;   (st1/st2/st4 base off s)                   ; truncating sized stores
;   (sx1/sx2/sx4 d)                            ; sign-extend d's low 1/2/4 bytes in place (the C casts)
;   (shlv/shrv/sarv d c)   ; shift d by the count in c -- c PINNED to r1 (x86's CL), d must differ
;   (lea d base off)       ; d <- base+off     (address arithmetic)
;   (ldx d base ix sc dp)  ; d <- [base + ix*sc + dp]   (sc in 1/2/4/8)
;   (stx base ix sc dp s)  ; [base + ix*sc + dp] <- s
;   (leax d base ix sc dp) ; d <- base + ix*sc + dp
;   (shl/shr/sar/rol/ror d c)                  ; shift / rotate d in place by immediate count c
;   (test a b)             ; set flags from a AND b
;   (div d a b) (rem d a b t)                  ; signed truncating quotient / remainder; t a scratch (arm64 rem; x86 ignores)
;   (push s) (pop d)
;   (jmp label) (br cond label)                ; cond: eq ne lt le gt ge below above ae be s ns vs vc
;   (jmpr d)                                   ; indirect: jump to the address in register d
;   (adds d a b) (subs d a b)                  ; like add/sub but SET FLAGS -- pair with (br vs/vc ..)
;   (mulo d a b t lbl)                         ; d <- a*b (low 64); branch to lbl on signed overflow; t a scratch (clobbered)
;   (set cond d)                               ; d <- (cond holds) ? 1 : 0, from the last cmp's flags
;   (call label) (callr d) (ret) (sys)         ; callr: indirect call through register d; sys = syscall/svc
;   (la d label)           ; d <- the label's ADDRESS (pc-relative; data via (label)+(raw ..))
;   (nop) (trap)
;   -- SSE2 double lane (x86-64 only; the float glaze): xmm registers f0..f15 --
;   (addsd/mulsd/subsd/divsd d s) etc.         ; reg-reg packed/scalar double ops (see x64-sse)
;   (ucomisd a b)                              ; compare doubles, set flags
;   (cvtsi2sd f g) (movqxr f g) (movqrx g f)   ; int->double ; gp<->xmm bit moves
;   (stsd base disp f) (ldsd f base disp)      ; movsd [base+disp]<->xmm  (spill / box store)
;
; a register operand is a SYMBOL (r0..); an immediate is a NUMBER -- `reg?` tells them apart, so the
; ALU ops take a register or an immediate in the same slot with no syntax. the abstract register
; file is what makes the IR neutral: each backend maps r0..r14/sp onto its own encoding.

; the module scope opens here: everything crew/holo/{holo,x64,arm64,thumb2}.l binds lands in this
; layer, mutually visible by bare name; a (leave ()) after the last backend closes the layer, which REGISTERS it as
; the module `holo` -- a consumer either (use 'holo) and reads bare, or probes one member through
; the accessor: (from 'holo 'assemble). a test cat-stream simply never seals: the stream rides the
; open layer and reads everything bare.
(enter 'holo)

(: ;; -- a 0-based list accessor (prel has none; `nth` is not bound -- it absorbs as the zero
   ;; point). instruction forms read their operands through this. --
   (at l k) (? (k = 0) <l (at >l (k - 1)))

   ;; -- bytes, little-endian. the leaves a backend emits immediates with. byte/le16/le32 ride the fast
   ;; &/>> path (their inputs are offsets/imm32, always < 2^31, where & is exact). le64 must NOT: a movabs
   ;; immediate can be a full 64-bit word >= 2^63 -- a BIGNUM in love, on which `&` is broken (the high limb
   ;; is dropped). so le64 normalizes negatives into [0,2^64) and slices with //% (both bignum-exact). --
   (catall ls) (foldr (+) () ls)                     ; concat a list of byte-lists
   (byte n)  (n & 255)
   (le16 n)  `((byte n) (byte (n >> 8)))
   (le32 n)  `((byte n) (byte (n >> 8)) (byte (n >> 16)) (byte (n >> 24)))
   (le64 n)  (: u (? (n < 0) (n + 18446744073709551616) n)                            ; two's-complement into [0,2^64)
              ((: (go i) (? (i < 8) (((u // (i 256)) % 256) >< (go (i + 1))) ())) 0))   ; 8 LE bytes via //% (i 256)=256^i
   (fits8  n) (&& (n >= -128) (n <= 127))
   (fits32 n) (&& (n >= -2147483648) (n <= 2147483647))

   ;; -- a register operand is a symbol, an immediate a number. --
   (reg? x)  !(constellation? x)
   (imm? x)  (constellation? x)

   ;; -- a FIXUP: a relocation placeholder a backend drops where a label-relative field goes. it
   ;; occupies `w` bytes in the layout; the driver fills it in pass two via the backend's `patch`.
   ;; carried inline among the bytes as ('fix w kind label aux) so length and order just fall out.
   ;; `aux` is a backend scratch value -- x86 ignores it; a fixed-width arch (aarch64) stashes the
   ;; base instruction word there, since its branch offset is a bitfield WITHIN the word, not bytes.
   (fix w kind label aux) `('fix w kind label aux)
   (fix? x)   (? (two? x) (<x = 'fix) 0)
   (fix-w x)  (at x 1)
   (fix-k x)  (at x 2)
   (fix-l x)  (at x 3)
   (fix-a x)  (at x 4)
   (item-len x) (? (fix? x) (fix-w x) 1)              ; a byte is 1; a fixup is its field width
   ;; -- fold a freshly lowered chunk onto the REVERSED item stream, answering (len . items).
   ;; the obvious spelling -- measure the chunk, then (rev it), then (+) that copy onto items
   ;; -- walks the chunk THREE times and conses it TWICE; this walks once and conses once. the
   ;; layout passes run it per instruction, so it is the assembler's hottest loop, and halving
   ;; its garbage is what the embedded targets feel (measured: -52% of the assemble's words). --
   (revonto l items n) (? !(two? l) (n >< items)
                          (revonto >l (<l >< items) (n + (item-len <l))))

   ;; -- the little table kit every backend leans on: `tabof` builds a tablet from
   ;; ('k >< v) pairs (the register/cc/opcode tables), `fregs` the f0..fN-1 float
   ;; file, and `look` is the guarded read (a miss scares with the caller's tag). --
   (tabof prs) (: t (tablet 0) _ (foldl (\ _ p (pin t <p >p)) 0 prs) t)
   (fregs n)   (: t (tablet 0) _ (foldl (\ _ i (pin t (intern ("f" + (show i))) i)) 0 (jot n)) t)
   (look tab k tag) (: e (peep tab k -1) (? (e < 0) (scare tag k) e))

   ;; -- the IR's operand counts, one neutral table (the header's forms made executable).
   ;; a backend keeps an EMITTER tablet, each row partial-applied to everything but its IR
   ;; operands (an arity-0 row IS its chunk), and its 'lower is the two-line lower-via shim
   ;; -- so the op -> operand-shape mapping lives HERE, once, instead of four hand-kept
   ;; dispatch chains. `emsof` below stamps the count onto each row at bake time. --
   ir-arity (tabof
        `(('ret >< 0) ('sys >< 0) ('nop >< 0) ('trap >< 0) ('udivll >< 0) ('uremll >< 0)
           ; x86 PORT I/O, register-contracted like `sys` (dx the port, al/ax/eax
           ; the datum; the string forms walk rsi/rdi by rcx). zero operands: the
           ; contract is the register file, not the form.
           ('inb >< 0) ('inw >< 0) ('inl >< 0) ('outb >< 0) ('outw >< 0) ('outl >< 0)
           ('cld >< 0)
           ('insb >< 0) ('insw >< 0) ('insl >< 0) ('outsb >< 0) ('outsw >< 0) ('outsl >< 0)
           ('sx1 >< 1) ('sx2 >< 1) ('sx4 >< 1) ('zx1 >< 1) ('zx2 >< 1) ('zx4 >< 1)
           ('push >< 1) ('pop >< 1) ('neg >< 1) ('not >< 1) ('inc >< 1) ('dec >< 1) ('tp >< 1)
           ('jmp >< 1) ('jmp8 >< 1) ('jmpr >< 1) ('callr >< 1) ('call >< 1) ('raw >< 1) ('lapool >< 1)
           ('li >< 2) ('mov >< 2) ('cmp >< 2) ('test >< 2) ('set >< 2) ('br >< 2) ('br8 >< 2)
           ('la >< 2) ('lar >< 2) ('clz >< 2)
           ('shl >< 2) ('shr >< 2) ('sar >< 2) ('rol >< 2) ('ror >< 2)
           ('shlv >< 2) ('shrv >< 2) ('sarv >< 2)
           ('movupd >< 2) ('movsd >< 2) ('movhlps >< 2) ('unpcklpd >< 2)
           ('mulpd >< 2) ('addpd >< 2) ('subpd >< 2) ('divpd >< 2)
           ('mulsd >< 2) ('addsd >< 2) ('subsd >< 2) ('divsd >< 2)
           ('xorpd >< 2) ('pcmpeqd >< 2) ('andpd >< 2) ('psubq >< 2)
           ('cvtss2sd >< 2) ('cvtsd2ss >< 2) ('ucomisd >< 2) ('movmskpd >< 2)
           ('cvtsi2sd >< 2) ('cvtui2sd >< 2) ('cvttsd2si >< 2) ('cvttsd2ui >< 2)
           ('movqxr >< 2) ('movqrx >< 2)
           ('add >< 3) ('sub >< 3) ('and >< 3) ('or >< 3) ('xor >< 3) ('imul >< 3)
           ('adds >< 3) ('subs >< 3) ('adc >< 3) ('adcs >< 3) ('sbc >< 3) ('sbcs >< 3) ('ors >< 3)
           ('div >< 3) ('udiv >< 3)
           ('ld >< 3) ('st >< 3) ('lea >< 3) ('lean >< 3)
           ('cas >< 3) ('cas4 >< 3)
           ('ld1 >< 3) ('ld2 >< 3) ('ld4 >< 3) ('st1 >< 3) ('st2 >< 3) ('st4 >< 3)
           ('ldu1 >< 3) ('ldu2 >< 3) ('ldu4 >< 3)
           ('stsd >< 3) ('ldsd >< 3) ('stss >< 3) ('ldss >< 3) ('cmppd >< 3)
           ('rem >< 4) ('urem >< 4) ('umull >< 4) ('smull >< 4) ('mla >< 4)
           ('ldx >< 5) ('stx >< 5) ('leax >< 5) ('ldxb >< 5) ('stxb >< 5) ('mulo >< 5)))
   ;; an emitter ROW is (arity . emitter): `emsof` builds a backend's tablet from the same
   ;; ('op >< f) pairs `tabof` takes, stamping each row with its operand count off ir-arity,
   ;; and `emrow` pins one (the alu/sse groups). so ir-arity is read at BAKE time, once per
   ;; op, and lowering reads arity and emitter in a SINGLE lookup.
   (emsof prs) (: t (tablet 0) _ (foldl (\ _ p (emrow t <p >p)) 0 prs) t)
   (emrow t op f) (pin t op ((look ir-arity op 'bad-op) >< f))
   ;; feed the emitter its operands, left to right, straight off the form's own tail -- where
   ;; an index-per-operand walked the list again for each one. `n` counts the row's declared
   ;; arity down and must reach zero exactly as the operands run out: a form carrying the
   ;; wrong count SCARES here instead of answering a PARTIAL APPLICATION, which would lay as
   ;; zero bytes -- a silent hole in the output, the one failure this assembler can least
   ;; afford. (it caught a hand-written x64 golden that was short its ignored scratch operand.)
   (feed f l n op) (? (two? l) (? (n < 1) (scare 'arity-skew op) (feed (f <l) >l (n - 1) op))
                      (n = 0) f
                      (scare 'arity-skew op))
   (lower-via ems ins)
      (: op <ins  r (peep ems op 'no)
         _ (? (id? r 'no) (scare 'bad-op op) 0)
         (feed >r >ins <r op))

   ;; -- the backend registry. a backend is a tablet { 'lower 'patch }. --
   backends (tablet 0)
   (defbackend name be) (pin backends name be)
   (lower-with be ins)         ((peep be 'lower 0) ins)
   (patch-with be k t s w aux) ((peep be 'patch 0) k t s w aux)

   ;; -- pass one: walk the program, lower each form, assign addresses, record labels. returns
   ;; (items . labels): the flat byte/fixup stream and a name->address tablet. items accumulate
   ;; REVERSED (each chunk prepended, one rev at the end) so a whole-program lay stays linear --
   ;; appending to the tail re-copied the stream per instruction, and `mooncc -c love.c` paid it as
   ;; 15 of its 16 minutes. --
   ;; ('align n) pads the stream to the next n-boundary with the backend's nop unit
   ;; (peep be 'nop, a byte-list, cycled to the exact count) -- alignment lives HERE,
   ;; the only pass that knows the address. the pad is between-function filler, never
   ;; executed, so a torn unit at an odd grain is harmless.
   (pad-to be addr n)
      (: k ((n - (addr % n)) % n)
         unit (peep be 'nop `(144))
         (fill j u) (? (j < 1) ()
                       !(two? u) (fill j unit)
                       (<u >< (fill (j - 1) >u)))
         (fill k unit))
   (lay prog be addr items labels)
      (? !(two? prog) ((rev items) >< labels)
         (: ins <prog
            (? (<ins = 'label)
               (: _ (pin labels (at ins 1) addr) (lay >prog be addr items labels))
               (<ins = 'align)
               (: r (revonto (pad-to be addr (at ins 1)) items 0)
                  (lay >prog be (addr + <r) >r labels))
               (: r (revonto (lower-with be ins) items 0)
                  (lay >prog be (addr + <r) >r labels)))))

   ;; -- pass two: stream the items out, resolving each fixup against the now-known labels. --
   (resolve items labels be addr)
      (? !(two? items) ()
         (: x <items
            (? (fix? x)
               (: tgt (peep labels (fix-l x) ())
                  _ (? (imm? tgt) 0 (scare 'undef-label (fix-l x)))
                  bs (patch-with be (fix-k x) tgt addr (fix-w x) (fix-a x))
                  (bs + (resolve >items labels be (addr + (fix-w x)))))
               (x >< (resolve >items labels be (addr + 1))))))

   ;; -- branch relaxation, fused with layout: lay's shape (items . labels), but (jmp l) /
   ;; (br c l) land as the 2-byte short forms (jmp8 / br8) where the target provably fits
   ;; rel8. ONE pessimistic decision pass is sound: distances measured at the all-long layout
   ;; only SHRINK as branches shorten, so a fit stays a fit -- provided the span crosses no
   ;; 'align (a boundary holds downstream addresses still while the branch slides) and the
   ;; target is a label of THIS prog (an external label seeded by assemble-at holds still the
   ;; same way, so it never enters `spots` and stays long). the align guard rides an EPOCH
   ;; pinned beside each label: same count of 'align forms seen == no boundary inside the
   ;; span. each form lowers exactly ONCE (pass one caches its chunk; pass two splices the
   ;; cache, re-lowering only the branches it shortens). the LARGE COLD texts ride this --
   ;; the object writer (crew/holo/obj.l) and the static-image org path below; plain
   ;; `assemble` stays on lay, so a hot assembling client (the glaze, thousands of tiny
   ;; blobs per corpus run) pays nothing (measured: the walk alone taxed the corpus ~1%
   ;; of its instructions). a backend opts in by pinning 'short;
   ;; a fixed-width arch (aarch64) falls through to plain lay. `longs` is a tablet of label
   ;; names that must stay long-form -- the object writer passes its weak-defined set, whose
   ;; fixups must survive as relocations (a linker may redirect them arbitrarily far).
   ;; (the locals here are spelled to shadow NO caller-side global -- a `scan`/`rw`/`skip`
   ;; would collide with moon/kore file-scope names in a cat'd stream.)
   (laylax prog be at0 labels longs)
      (? !(peep be 'short 0) (lay prog be at0 () labels)
         (: spots (tablet 0) eras (tablet 0)
            ;; pass one: lower each form once, cache (len . REVERSED chunk) -- the length rides
            ;; along so pass two never re-walks a chunk (item-len per byte is the cost that
            ;; counts for a hot assembling client), and the reversal is already paid, since
            ;; pass two splices onto a reversed stream. labels ride (0 . ()); pessimistic
            ;; addresses + align epochs pin beside the internal labels.
            (plumb p addr era acc)
               (? !(two? p) (rev acc)
                  (: ins <p
                     (? (<ins = 'label)
                        (: _ (pin spots (at ins 1) addr) _ (pin eras (at ins 1) era)
                           (plumb >p addr era ((0 >< ()) >< acc)))
                        (<ins = 'align)
                        (: r (revonto (pad-to be addr (at ins 1)) () 0)
                           (plumb >p (addr + <r) (era + 1) (r >< acc)))
                        (: r (revonto (lower-with be ins) () 0)
                           (plumb >p (addr + <r) era (r >< acc))))))
            chunks (plumb prog at0 0 ())
            ;; the short branch is 2 bytes, so its rel8 measures from addr+2 -- forward spans
            ;; shrink toward it, backward spans shrink toward zero, both stay inside [-128,127].
            (near? l addr era)
               (: t (peep spots l ())
                  (? !(imm? t) ()
                     (peep longs l 0) ()
                     !(era = (peep eras l -1)) ()
                     (fits8 (t - (addr + 2)))))
            ;; pass two: splice the cached chunks -- pessimistic addresses (pa) drive the
            ;; shortening decisions, final addresses (fa) pin the labels and re-lay the pads.
            (stitch p cs pa fa era items)
               (? !(two? p) ((rev items) >< labels)
                  (: ins <p  k <<cs  c ><cs
                     (? (<ins = 'label)
                        (: _ (pin labels (at ins 1) fa) (stitch >p >cs pa fa era items))
                        (<ins = 'align)
                        (: pad (pad-to be fa (at ins 1))
                           (stitch >p >cs (pa + k) (fa + (tally pad)) (era + 1)
                                   ((rev pad) + items)))
                        (&& (<ins = 'jmp) (near? (at ins 1) pa era))
                        (: sc (lower-with be `('jmp8 (at ins 1)))
                           (stitch >p >cs (pa + k) (fa + 2) era ((rev sc) + items)))
                        (&& (<ins = 'br) (near? (at ins 2) pa era))
                        (: sc (lower-with be `('br8 (at ins 1) (at ins 2)))
                           (stitch >p >cs (pa + k) (fa + 2) era ((rev sc) + items)))
                        (stitch >p >cs (pa + k) (fa + k) era (c + items)))))   ; c is cached REVERSED
            (stitch prog chunks at0 at0 0 ())))

   ;; -- assemble PROG for TARGET. returns a tablet { 'bytes 'labels }; `holo-bytes` is the
   ;; byte-list shortcut, `holo-str` packs them into a machine-code string, `holo-hex` shows them. --
   ;; -- assemble-at: like assemble, but `lbls` is a PRE-BOUND label tablet (name -> address). A
   ;; client that emits a blob into a larger buffer seeds the EXTERNAL targets it jumps to -- e.g.
   ;; the glaze seeds its deopt handler as `deopt -> (OVF - blob-start)`, so a `(jmp deopt)` inside
   ;; the IR resolves to a rel32 reaching code OUTSIDE the assembled blob. The blob's own labels are
   ;; layered on top by `lay`; an address may be negative (a target before the blob). --
   (assemble-at target prog lbls)
      (: be (peep backends target ())
         _ (? (tablet? be) 0 (scare 'no-backend target))
         p (lay prog be 0 () lbls)
         res (tablet 0)
         _ (pin res 'bytes  (resolve <p >p be 0))
         _ (pin res 'labels >p)
         res)
   (assemble target prog) (assemble-at target prog (tablet 0))
   ;; -- assemble-org: like assemble, but lay/resolve start at ORG, so labels carry their ABSOLUTE
   ;; load address. pc-relative fixups are unchanged (the org cancels in target-site); an abs64 fixup
   ;; writes the address outright. elf64 sets ORG to the load vaddr so a data pointer resolves. --
   (assemble-org target prog org)
      (: be (peep backends target ())
         _ (? (tablet? be) 0 (scare 'no-backend target))
         p (laylax prog be org (tablet 0) (tablet 0))
         res (tablet 0)
         _ (pin res 'bytes  (resolve <p >p be org))
         _ (pin res 'labels >p)
         res)
   (holo-bytes target prog) (peep (assemble target prog) 'bytes 0)   ; (the module book owns the bare name `holo`)
   (holo-bytes-at target prog org) (peep (assemble-org target prog org) 'bytes 0)
   (holo-str target prog) (string (holo-bytes target prog))
   (label-at res name)   (peep (peep res 'labels 0) name ())

   ;; -- hex, for goldens and disassembly. --
   (hex1 d)  (snip "0123456789abcdef" d (d + 1))
   (hex2 b)  ((hex1 (b >> 4)) + (hex1 (b & 15)))
   (hexs bs) (foldl (\ a b (a + (hex2 b))) "" bs)
   (holo-hex target prog) (hexs (holo-bytes target prog)))
