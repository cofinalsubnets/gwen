; test/holo/fuzz/fuzz.l -- holo encoder differential fuzz: x86-64 + aarch64 + riscv64.
; for each generated IR form we know the intended instruction. holo (loaded in-process)
; encodes it; the bytes go through a FOREIGN decoder (x64: objdump -M intel, cross-read
; by llvm-mc; arm64/riscv: llvm-mc) and the decoded instruction must match intent --
; mnemonic, operands, and nothing undecodable. tolerates encoding non-uniqueness:
; registers compare by ABSTRACT identity (width-agnostic), immediates by value mod 2^64;
; a byte-exact oracle would false-alarm on legal 32-bit-mov-zero-extend and disp-size
; choices. a decode oracle checks meaning, not layout.
;
; knobs ride the environment: FUZZ_N (per class, default 8), FUZZ_SEED, FUZZ_ARCH
; (x64|arm64|riscv, default all three). run from the repo root:
;
;   out/host/love test/holo/fuzz/fuzz.l
;
; exit 1 on any failure; each failure names its class, IR, hex and reason.

(reads (open "crew/holo/holo.l" "r"))
(reads (open "crew/holo/amd64.l" "r"))
(reads (open "crew/holo/arm64.l" "r"))
(reads (open "crew/holo/rv64.l" "r"))
; holo rides module 'holo: reading the source REGISTERS it, and the names come
; through the door rather than off this file's layer.
(use 'holo)

(:
 ; ---- the small string kit (this file loads only holo, so it carries its own) ----
 (nthl l i) (? (i = 0) <l (nthl >l (i - 1)))
 (trim s) (: n (tally s)
             (a i) (? ((i < n) && (((s i) = 32) || ((s i) = 9))) (a (i + 1)) i)
             (b j) (? ((0 < j) && (((s (j - 1)) = 32) || ((s (j - 1)) = 9))) (b (j - 1)) j)
             i (a 0) j (b n)
             (? (i < j) (snip s i j) ""))
 (lines s) (: n (tally s)
              (go i j acc) (? (n <= j) (rev (? (i < j) ((snip s i j) >< acc) acc))
                              ((s j) = 10) (go (j + 1) (j + 1) ((snip s i j) >< acc))
                              (go i (j + 1) acc))
              (go 0 0 ()))
 (fields s c) (: n (tally s)                            ; split on the charm c, empties kept
                 (go i j acc) (? (n <= j) (rev ((snip s i j) >< acc))
                                 ((s j) = c) (go (j + 1) (j + 1) ((snip s i j) >< acc))
                                 (go i (j + 1) acc))
                 (go 0 0 ()))
 (toks s) (filter (\ t (0 < tally t)) (foldl (\ a t (a + (fields t 9))) () (fields s 32)))
 (idx-of s sub) (: n (tally s) k (tally sub)
                   (at? i j) (? (k <= j) 1 ((s (i + j)) = (sub j)) (at? i (j + 1)) 0)
                   (go i) (? (n < (i + k)) () (at? i 0) i (go (i + 1)))
                   (go 0))
 (has? s sub) (charm? (idx-of s sub))
 (pfx? s p) (: k (tally p) ((k <= tally s) && ((snip s 0 k) = p)))
 (sfx? s p) (: n (tally s) k (tally p) ((k <= n) && ((snip s (n - k) n) = p)))
 (digit? c) ((48 <= c) && (c <= 57))
 (hexd? c) ((digit? c) || ((97 <= c) && (c <= 102)))
 (hexdv c) (? (digit? c) (c - 48) (c - 87))
 (all-hex? s) (: n (tally s) (go i) (? (n <= i) 1 (hexd? (s i)) (go (i + 1)) 0) (go 0))
 (s2i s0) (: s (trim s0) n (tally s)                    ; int(s, 0): decimal or 0x hex, sign
            (? (n = 0) ()
               (: neg ((s 0) = 45)
                  b (? neg (snip s 1 n) s)
                  v (? (pfx? b "0x")
                       (: (h i acc) (? ((tally b) <= i) acc
                                       (hexd? (b i)) (h (i + 1) (16 * acc + hexdv (b i)))
                                       ())
                          (h 2 0))
                       (: (d i acc) (? ((tally b) <= i) acc
                                       (digit? (b i)) (d (i + 1) (10 * acc + ((b i) - 48)))
                                       ())
                          (d 0 0)))
                  (? (id? v ()) () (? neg (0 - v) v)))))
 ; ⚠ exact big division: int on an inexact big ratio rides a gem and drops digits
 (divp v d) (int ((v - (% v d)) / d))
 (show0x n) (: (h v acc) (? (v = 0) acc (h (divp v 16) ((nthl '(48 49 50 51 52 53 54 55 56 57 97 98 99 100 101 102) (% v 16)) >< acc)))
               (? (n = 0) "0x0" ("0x" + string (h n ()))))
 M64 18446744073709551616
 (mask64 x) (% ((% x M64) + M64) M64)
 (pow2 k) (: (go i a) (? (i = k) a (go (i + 1) (2 * a))) (go 0 1))

 ; ---- the boxed RNG (a tablet slot; every draw is one line at the use site) ----
 RS (tablet 0)
 (rseed s) (pin RS 0 (wheel s))
 (rnd n) (: r (random (peep RS 0 0) n) _ (pin RS 0 >r) <r)   ; [0, n)
 (rint a b) (a + rnd (b - a + 1))                            ; [a, b] inclusive
 (pick l) (nthl l (rnd (tally l)))
 (coin _) (rnd 2)
 (pick-n l k) (? (k = 0) ()                                  ; k DISTINCT draws
                 (: x (pick l) (x >< pick-n (filter (\ y !(y = x)) l) (k - 1))))

 ; ---- subprocess plumbing: sh with redirection; bytes laid via printf octal ----
 tmpd "out/host"
 (sh c) (: pid (spawn ["sh" "-c" c]) (? (pid < 0) 1 (wait pid)))
 (rdf p) (: q (open p "r") (? q (: t (slurp q) _ (close q) t) ""))
 (wrf p t) (: q (open p "w") _ (say q t) (close q))
 (oct3 c) (string [(48 + (int (c / 64))) (48 + (% (int (c / 8)) 8)) (48 + (% c 8))])
 (hex-bytes hx) (: n (tally hx)                              ; hex string -> charm list
                   (go i acc) (? (n <= i) (rev acc)
                                 (go (i + 2) ((16 * hexdv (hx i) + hexdv (hx (i + 1))) >< acc)))
                   (go 0 ()))
 (lay-bin hx p) (sh ("printf '" + foldl (\ a c (a + "\\" + oct3 c)) "" (hex-bytes hx) + "' > " + p))
 (spaced-hex hx) (: n (tally hx)
                    (a2 a b) (? (a = "") b (a + " " + b))
                    (go i acc) (? (n <= i) acc
                                  (go (i + 2) (a2 acc ("0x" + snip hx i (i + 2)))))
                    (go 0 ""))

 ; ---- decoded instruction records: [mnem ops nbytes bad] ----
 (i-mnem i) <i
 (i-ops i) <>i
 (i-bad i) (nthl i 3)
 (dump-ins ins) (foldl (\ a i (a + "; " + i-mnem i + " " + i-ops i)) "" ins)

 ; objdump folds a redundant prefix into a pseudo-mnemonic -- peel to the real op
 x64-prefixes '("rex" "data16" "data32" "addr32" "lock" "rep" "repz" "repnz"
                "bnd" "notrack" "cs" "ss" "ds" "es" "fs" "gs")
 (strip-pfx ts) (? !(two? ts) ts
                   ((member? <ts x64-prefixes) || (pfx? <ts "rex.")) (strip-pfx >ts)
                   ts)

 ; x64: objdump -D -b binary. a line is addr TAB bytes TAB text; a continuation
 ; line (long insn wrapped) has no text field and emits nothing, as the reference did.
 (disasm-x64 hx)
  (: _ (lay-bin hx (tmpd + "/.fuzz.bin"))
     _ (sh ("objdump -D -b binary -m i386:x86-64 -M intel " + tmpd + "/.fuzz.bin > " + tmpd + "/.fuzz.out 2>/dev/null"))
     (go m acc)
      (? !(two? m) (rev acc)
         (: fs (fields <m 9)
            (? ((tally fs) < 3) (go >m acc)
               (: ts (strip-pfx (toks (nthl fs 2)))
                  (? !(two? ts) (go >m acc)
                     (go >m ([(cap ts) (foldl (\ a t (? (a = "") t (a + " " + t))) "" >ts)
                              (tally (toks (nthl fs 1))) (has? <m "(bad)")] >< acc)))))))
     (go (lines (rdf (tmpd + "/.fuzz.out"))) ()))

 ; arm64/riscv: llvm-mc --disassemble. a fixed-width lane gets full consumption for free
 ; (count*4 must equal the byte count); riscv rides the C extension and is VARIABLE width, so
 ; vw walks the stream instead -- an instruction's first byte has its low two bits set on a
 ; 4-byte word and clear on a 2-byte one, which is the whole of RISC-V's length encoding here.
 (rv-icount bs)
  (: n (tally bs)
     (go i k) (? (n <= i) k (go (i + (? (((bs i) & 3) = 3) 4 2)) (k + 1)))
     (go 0 0))
 (disasm-mc hx triple mattr cmark vw)
  (: _ (wrf (tmpd + "/.fuzz.in") (spaced-hex hx))
     _ (sh ("llvm-mc --disassemble --triple=" + triple + mattr + " < " + tmpd + "/.fuzz.in > " + tmpd + "/.fuzz.out 2>/dev/null"))
     (one l0 acc)
      (: c (idx-of l0 cmark)
         ln (trim (? (charm? c) (snip l0 0 c) l0))
         (? ((tally ln) = 0) acc
            (pfx? ln ".text") acc
            (pfx? ln ".byte") ([".byte" "" 4 1] >< acc)
            (: ts (toks ln)
               ([(cap ts) (trim (snip ln (tally <ts) (tally ln))) 4 0] >< acc))))
     got (rev (foldl (\ a ln (one ln a)) () (lines (rdf (tmpd + "/.fuzz.out")))))
     want (? vw (rv-icount (hex-bytes hx)) (int ((tally hx) / 8)))
     (? ((tally got) = want) got
        (got + [["<count>" "" 4 1]])))

 ; the llvm-mc second opinion on x64 bytes (reject/.byte = undecodable)
 (llvm-ok? hx)
  (: _ (wrf (tmpd + "/.fuzz.in") (spaced-hex hx))
     r (sh ("llvm-mc --disassemble --triple=x86_64 < " + tmpd + "/.fuzz.in > " + tmpd + "/.fuzz.out 2>/dev/null"))
     (? (0 < r) 0 (has? (rdf (tmpd + "/.fuzz.out")) ".byte") 0 1))

 ; ---- operand parsers, per arch ----
 (ops-comma s) (? ((tally s) = 0) () (map trim (fields s 44)))
 (ops-arm s) (: n (tally s)                              ; comma split respecting [...]
                (go i j d acc) (? (n <= j) (rev ((trim (snip s i j)) >< acc))
                                  ((s j) = 91) (go i (j + 1) (d + 1) acc)
                                  ((s j) = 93) (go i (j + 1) (d - 1) acc)
                                  (((s j) = 44) && (d = 0)) (go (j + 1) (j + 1) d ((trim (snip s i j)) >< acc))
                                  (go i (j + 1) d acc))
                (? (n = 0) () (go 0 0 0 ())))

 ; x64 memory: 'QWORD PTR [rbp-0x8]' / '[rax+rcx*4+0x10]' -> [base index scale disp]
 (mem-x64 op)
  (: a (idx-of op "[") b (idx-of op "]")
     (? (!(charm? a) || !(charm? b)) ()
        (: in (snip op (a + 1) b)
           ; the scaled index: REG*S
           st (idx-of in "*")
           ix () sc 1
           _ 0
           r (? (charm? st)
                (: ib (: (bk i) (? ((0 < i) && (: c (in (i - 1)) ((digit? c) || ((97 <= c) && (c <= 122))))) (bk (i - 1)) i) (bk st))
                   ixs (snip in ib st) scs (snip in (st + 1) (st + 2))
                   rest ((snip in 0 ib) + (snip in (st + 2) (tally in)))
                   [ixs (s2i scs) rest])
                [() 1 in])
           ix <r sc <>r in2 (nthl r 2)
           ; the trailing displacement: +0x../-N at the end
           n2 (tally in2)
           de (: (dk j) (? ((0 < j) && (: c (in2 (j - 1)) ((hexd? c) || (c = 120)))) (dk (j - 1)) j) (dk n2))
           r2 (? ((de < n2) && (0 < de) && (((in2 (de - 1)) = 43) || ((in2 (de - 1)) = 45)))
                 (: v (s2i (snip in2 de n2))
                    [(? ((in2 (de - 1)) = 45) (0 - v) v) (snip in2 0 (de - 1))])
                 [0 in2])
           dsp <r2 in3 (nthl r2 1)
           bts (filter (\ t (0 < tally t)) (map trim (fields in3 43)))
           [(? (two? bts) (trim <bts) ()) ix sc dsp])))

 ; arm memory: '[x6, #8]' / '[sp, #-16]!' / '[x0, x1, lsl #3]'
 (mem-arm op)
  (: a (idx-of op "[") b (idx-of op "]")
     (? (!(charm? a) || !(charm? b)) ()
        (: ps (map trim (fields (snip op (a + 1) b) 44))
           base <ps
           (go m ix sc dsp)
            (? !(two? m) [base ix sc dsp]
               (pfx? <m "#") (go >m ix sc (s2i (snip <m 1 (tally <m))))
               (pfx? <m "lsl") (: hi (idx-of <m "#")
                                  (go >m ix (? (charm? hi) (pow2 (s2i (snip <m (hi + 1) (tally <m)))) sc) dsp))
               (go >m <m sc dsp))
           (go >ps () 1 0))))

 ; riscv memory: '8(a1)' / '-300(t6)'
 (mem-rv o0)
  (: op (trim o0) a (idx-of op "(") b (idx-of op ")")
     (? (!(charm? a) || !(charm? b)) ()
        [(snip op (a + 1) b) () 1 (? (a = 0) 0 (s2i (snip op 0 a)))]))

 (imm-x64 op) (: v (s2i op) (? (id? v ()) () (mask64 v)))   ; a decoded imm may be a big
 (imm-arm op) (s2i (? (pfx? (trim op) "#") (snip (trim op) 1 (tally (trim op))) op))
 (imm-rv op) (s2i op)

 ; ---- abstract register identity, per arch ----
 x64-fam '((r0 "rax" "eax" "ax" "al") (r1 "rcx" "ecx" "cx" "cl") (r2 "rdx" "edx" "dx" "dl")
           (r3 "rbx" "ebx" "bx" "bl") (sp "rsp" "esp" "sp" "spl") (r4 "rbp" "ebp" "bp" "bpl")
           (r5 "rsi" "esi" "si" "sil") (r6 "rdi" "edi" "di" "dil"))
 x64-abs ((foldl (\ a f (a + map (\ nm [nm <f]) >f)) () x64-fam)
          + (foldl (\ a n (: b ("r" + show n) s (intern ("r" + show (n - 1)))
                            (a + [[b s] [(b + "d") s] [(b + "w") s] [(b + "b") s]])))
                   () '(8 9 10 11 12 13 14 15)))
 (abs-x64 nm) (: p (assoc (trim nm) x64-abs) (? (two? p) <>p ()))
 (abs-arm n0) (: nm (trim n0)
                 (? ((nm = "sp") || (nm = "wsp")) 'sp
                    ((nm = "xzr") || (nm = "wzr")) 'zr
                    (: n (tally nm)
                       digits (: (dg i) (? (n <= i) 1 (digit? (nm i)) (dg (i + 1)) 0) (dg 1))
                       (? ((1 < n) && (((nm 0) = 120) || ((nm 0) = 119)) && digits)
                          (intern ("r" + snip nm 1 n)) ()))))
 (arm-width n0) (: nm (trim n0)
                   (? ((nm = "sp") || (nm = "wsp")) 'sp
                      ((0 < tally nm) && ((nm 0) = 120)) 'x
                      ((0 < tally nm) && ((nm 0) = 119)) 'w
                      '?))
 rv-abs ('(("a0" r0) ("a1" r1) ("a2" r2) ("a3" r3) ("a4" r4) ("a5" r5) ("a6" r6) ("a7" r7)
           ("t0" r8) ("t1" r9) ("t2" r10) ("t3" r11) ("t4" r12) ("s2" r13) ("s3" r14) ("s1" r15)
           ("sp" sp) ("s0" fp) ("ra" lr) ("zero" zero) ("t5" t5) ("t6" t6) ("gp" gp) ("tp" tp))
         + (map (\ i [("s" + show i) (intern ("r" + show (15 + i)))]) '(4 5 6 7 8 9 10 11))
         + (map (\ i [("fa" + show i) (intern ("f" + show i))]) ^8)
         + (map (\ i [("ft" + show i) (intern ("f" + show (8 + i)))]) ^12))
 (abs-rv nm) (: p (assoc (trim nm) rv-abs) (? (two? p) <>p ()))

 ; ---- the arch in force (a tablet of closures, rebound per lane) ----
 AR (tablet 0)
 (a-ops s) ((peep AR 'ops 0) s)
 (a-mem s) ((peep AR 'mem 0) s)
 (a-abs s) ((peep AR 'abs 0) s)
 (a-imm s) ((peep AR 'imm 0) s)

 ; ---- the spec interpreter: one description checks one decode ----
 ; spec = ([mnems checks] ..); a check is [r ABS] [rw ABS W] [i VAL] [m BASE DISP]
 ; [mx BASE DISP IX SC] [x FN] [lit "cl"]. strict demands the operand counts match
 ; (the riscv lane's law); the others read a prefix.
 (chk1 got w)
  (: k <w
     (? (id? k 'r) (? (id? (a-abs got) (nthl w 1)) () ("reg " + got + " != " + string (nthl w 1)))
        (id? k 'rw) (? !(id? (a-abs got) (nthl w 1)) ("reg " + got + " != " + string (nthl w 1))
                       !(id? (arm-width got) (nthl w 2)) ("width " + got + " != " + string (nthl w 2))
                       ())
        (id? k 'i) (: v (a-imm got)
                      (? (v = (nthl w 1)) () ("imm " + got + " != " + show0x (mask64 (nthl w 1)))))
        (id? k 'x) (: t (trim got)
                      (? ((pfx? t "xmm") && ((snip t 3 (tally t)) = show (nthl w 1))) ()
                         ("xmm " + got + " != xmm" + show (nthl w 1))))
        (id? k 'lit) (? ((trim got) = (nthl w 1)) () ("op " + got + " != " + (nthl w 1)))
        (: m (a-mem got)
           (? !(two? m) ("not a mem operand: " + got)
              !(id? (a-abs <m) (nthl w 1)) ("mem base " + <m + " != " + string (nthl w 1))
              !((nthl m 3) = (nthl w 2)) ("mem disp " + show0x (mask64 (nthl m 3)) + " != " + show0x (mask64 (nthl w 2)))
              (id? k 'm) ()
              (: ix (nthl m 1)
                 (? (!(string? ix) || !(id? (a-abs ix) (nthl w 3))) ("mem index != " + string (nthl w 3))
                    !((nthl m 2) = (nthl w 4)) ("mem scale " + show (nthl m 2) + " != " + show (nthl w 4))
                    ()))))))
 (run-spec ins spec strict)
  (? (any i-bad ins) "undecodable/bad byte"
     ((tally ins) != (tally spec)) ((show (tally ins)) + " insns, expected " + show (tally spec) + ":" + dump-ins ins)
     (: (goi is sp)
         (? !(two? is) ()
            (: i <is mns (cap <sp) cks (nthl <sp 1)
               o (a-ops (i-ops i))
               (? !(member? (i-mnem i) mns) ("mnem " + i-mnem i + " unexpected (" + i-ops i + ")")
                  (strict && ((tally o) != (tally cks))) (i-mnem i + ": " + show (tally o) + " ops != " + show (tally cks))
                  ((tally o) < (tally cks)) (i-mnem i + ": missing operands (" + i-ops i + ")")
                  (: (goc os ws) (? !(two? ws) ()
                                    (: r (chk1 <os <ws) (? (string? r) r (goc >os >ws))))
                     r (goc o cks)
                     (? (string? r) r (goi >is >sp))))))
        (goi ins spec)))
 (spec1 mns cks) (\ ins (run-spec ins [[mns cks]] 0))

 ; ---- shared draw pools ----
 gpregs '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 sp)
 (rrand _) (pick gpregs)
 (nrand _) (pick (filter (\ r !(id? r 'sp)) gpregs))          ; no sp in a value seat
 fregs (map (\ i (intern ("f" + show i))) ^16)
 (frand _) (pick fregs)
 (imm-small _) (rnd 128)
 (imm-s32 _) (rint (0 - 2147483648) 2147483647)
 (imm-u32 _) (rint 0 4294967295)
 (imm-big _) (rint 4294967296 9223372036854775807)
 (disp-any _) (: k (rnd 3) (? (k = 0) 0 (k = 1) (rint -128 127) (rint (0 - 2147483648) 2147483647)))

 ; ================================================================ x64 lane
 (fno f) (s2i (snip (string f) 1 (tally (string f))))       ; 'f7 -> 7
 x64-alu '("add" "sub" "and" "or" "xor" "imul")
 x64-sized-ld '(("ld1" ("movsx")) ("ld2" ("movsx")) ("ld4" ("movsxd"))
                ("ldu1" ("movzx")) ("ldu2" ("movzx")) ("ldu4" ("mov")))
 x64-setcc '((eq "sete") (ne "setne") (lt "setl") (le "setle") (gt "setg") (ge "setge")
             (below "setb") (above "seta") (ae "setae") (be "setbe") (s "sets") (ns "setns")
             (vs "seto") (vc "setno"))
 gens-x64
  [["mov_rr" (\ _ (: d (rrand 0) s (rrand 0)
                     [[['mov d s]] (spec1 '("mov") [['r d] ['r s]])]))]
   ["li" (\ _ (: d (rrand 0) k (rnd 4)
                 imm (? (k = 0) (imm-small 0) (k = 1) (imm-s32 0) (k = 2) (imm-u32 0) (imm-big 0))
                 [[['li d imm]] (spec1 '("mov" "movabs") [['r d] ['i (mask64 imm)]])]))]
   ["alu_rr" (\ _ (: op (pick x64-alu) d (rrand 0) b (rrand 0)
                     [[[(intern op) d d b]] (spec1 [op] [['r d] ['r b]])]))]
   ["alu_imm" (\ _ (: op (pick '("add" "sub" "and" "or" "xor")) d (rrand 0) imm (imm-s32 0)
                      [[[(intern op) d d imm]] (spec1 [op] [['r d] ['i (mask64 imm)]])]))]
   ["cmp" (\ _ (: a (rrand 0)
                  (? (coin 0)
                     (: b (rrand 0) [[['cmp a b]] (spec1 '("cmp") [['r a] ['r b]])])
                     (: imm (imm-s32 0) [[['cmp a imm]] (spec1 '("cmp") [['r a] ['i (mask64 imm)]])]))))]
   ["ld" (\ _ (: d (rrand 0) b (rrand 0) o (disp-any 0)
                 [[['ld d b o]] (spec1 '("mov") [['r d] ['m b o]])]))]
   ["st" (\ _ (: b (rrand 0) o (disp-any 0) s (rrand 0)
                 [[['st b o s]] (spec1 '("mov") [['m b o] ['r s]])]))]
   ["ld_sized" (\ _ (: row (pick x64-sized-ld) d (rrand 0) b (rrand 0) o (disp-any 0)
                       [[[(intern <row) d b o]] (spec1 (nthl row 1) [['r d] ['m b o]])]))]
   ["st_sized" (\ _ (: op (pick '("st1" "st2" "st4")) b (rrand 0) o (disp-any 0) s (rrand 0)
                       [[[(intern op) b o s]] (spec1 '("mov") [['m b o] ['r s]])]))]
   ["pushpop" (\ _ (? (coin 0)
                      (: s (rrand 0) [[['push s]] (spec1 '("push") [['r s]])])
                      (: d (nrand 0) [[['pop d]] (spec1 '("pop") [['r d]])])))]
   ["lea" (\ _ (: d (rrand 0) b (rrand 0) o (disp-any 0)
                  [[['lea d b o]] (spec1 '("lea") [['r d] ['m b o]])]))]
   ["shift" (\ _ (: op (pick '("shl" "shr" "sar" "rol" "ror")) d (rrand 0) c (rnd 64)
                    [[[(intern op) d c]] (spec1 [op] [['r d] ['i c]])]))]
   ["unary" (\ _ (: op (pick '("neg" "not" "inc" "dec")) d (rrand 0)
                    [[[(intern op) d]] (spec1 [op] [['r d]])]))]
   ["ldx" (\ _ (: d (rrand 0) b (rrand 0) ix (pick (filter (\ r !(id? r 'sp)) gpregs))
                  sc (pick '(1 2 4 8)) dp (disp-any 0)
                  [[['ldx d b ix sc dp]] (spec1 '("mov") [['r d] ['mx b dp ix sc]])]))]
   ["leax" (\ _ (: d (rrand 0) b (rrand 0) ix (pick (filter (\ r !(id? r 'sp)) gpregs))
                   sc (pick '(1 2 4 8)) dp (disp-any 0)
                   [[['leax d b ix sc dp]] (spec1 '("lea") [['r d] ['mx b dp ix sc]])]))]
   ["shiftv" (\ _ (: row (pick '(("shlv" "shl") ("shrv" "shr") ("sarv" "sar")))
                     d (pick (filter (\ r !(id? r 'r1)) gpregs))
                     [[[(intern <row) d 'r1]] (spec1 [(nthl row 1)] [['r d] ['lit "cl"]])]))]
   ["flagalu" (\ _ (: row (pick '(("adds" "add") ("subs" "sub"))) d (rrand 0)
                      (? (coin 0)
                         (: b (rrand 0) [[[(intern <row) d d b]] (spec1 [(nthl row 1)] [['r d] ['r b]])])
                         (: imm (imm-s32 0) [[[(intern <row) d d imm]] (spec1 [(nthl row 1)] [['r d] ['i (mask64 imm)]])]))))]
   ["setcc" (\ _ (: row (pick x64-setcc) d (rrand 0)
                    [[['set <row d]]
                     (\ ins (? (any i-bad ins) "undecodable/bad byte"
                               ((tally ins) != 2) ((show (tally ins)) + " insns, want setcc+movzx:" + dump-ins ins)
                               (: a <ins b (nthl ins 1)
                                  r1 (run-spec [a] [[[(nthl row 1)] [['r d]]]] 0)
                                  (? (string? r1) r1
                                     (run-spec [b] [['("movzx") [['r d]]]] 0)))))]))]
   ["jmpr" (\ _ (: d (rrand 0) [[['jmpr d]] (spec1 '("jmp") [['r d]])]))]
   ["callr" (\ _ (: d (rrand 0) [[['callr d]] (spec1 '("call") [['r d]])]))]
   ["ssealu" (\ _ (: op (pick '("addsd" "subsd" "mulsd" "divsd")) d (frand 0) s (frand 0)
                     [[[(intern op) d s]] (spec1 [op] [['x (fno d)] ['x (fno s)]])]))]
   ["cvt" (\ _ (: d (frand 0) g (rrand 0)
                  [[['cvtsi2sd d g]] (spec1 '("cvtsi2sd") [['x (fno d)] ['r g]])]))]
   ["movqxr" (\ _ (: d (frand 0) g (rrand 0)
                     [[['movqxr d g]] (spec1 '("movq") [['x (fno d)] ['r g]])]))]
   ["ldsd" (\ _ (: d (frand 0) b (rrand 0) o (disp-any 0)
                   [[['ldsd d b o]] (spec1 '("movsd") [['x (fno d)] ['m b o]])]))]
   ["stsd" (\ _ (: b (rrand 0) o (disp-any 0) s (frand 0)
                   [[['stsd b o s]] (spec1 '("movsd") [['m b o] ['x (fno s)]])]))]]

 ; ================================================================ arm64 lane
 arm-alu '(("add" "add") ("sub" "sub") ("and" "and") ("or" "orr") ("xor" "eor") ("imul" "mul"))
 arm-sized-ld '(("ld1" ("ldrsb" "ldursb") x) ("ld2" ("ldrsh" "ldursh") x)
                ("ld4" ("ldrsw" "ldursw") x) ("ldu1" ("ldrb" "ldurb") w)
                ("ldu2" ("ldrh" "ldurh") w) ("ldu4" ("ldr" "ldur") w))
 arm-cc '((eq "eq") (ne "ne") (lt "lt") (le "le") (gt "gt") (ge "ge") (below "lo")
          (above "hi") (ae "hs") (be "ls") (s "mi") (ns "pl") (vs "vs") (vc "vc"))
 (arm-disp _) (: k (rnd 3) (? (k = 0) 0 (k = 1) (rint -256 255) (8 * rnd 512)))
 ; the movz/movk(/movn/orr) chain reassembled into its 64-bit value; () = unreadable
 (recon-li2 ins d)
  (: (go m val)
      (? !(two? m) (mask64 val)
         (: i <m
            (? (i-bad i) ()
               (: o (ops-arm (i-ops i))
                  (? !(id? (abs-arm <o) d) ()
                     (: hi (idx-of (i-ops i) "lsl #")
                        sh (? (charm? hi) (s2i (snip (i-ops i) (hi + 5) (tally (i-ops i)))) 0)
                        imm (? (1 < tally o) (: v (imm-arm (nthl o 1)) (? (nil? v) 0 v)) 0)
                        mn (i-mnem i)
                        (? ((mn = "mov") || (mn = "movz")) (go >m (mask64 (imm * pow2 sh)))
                           (mn = "movk") (go >m (mask64 ((mask64 (val - (% (divp val (pow2 sh)) 65536) * pow2 sh)) + (% imm 65536) * pow2 sh)))
                           (mn = "movn") (go >m (mask64 (M64 - 1 - (% imm 65536) * pow2 sh)))
                           (mn = "orr") (go >m (mask64 imm))
                           ())))))))
     (go ins 0))
 gens-arm
  [["mov_rr" (\ _ (: d (nrand 0) s (nrand 0)
                     [[['mov d s]] (spec1 '("mov") [['r d] ['r s]])]))]
   ["li" (\ _ (: d (nrand 0) k (rnd 6)
                 imm (? (k = 0) (imm-small 0) (k = 1) (rnd 65536) (k = 2) (imm-s32 0)
                        (k = 3) (imm-u32 0) (k = 4) (imm-big 0) (0 - rint 1 65535))
                 [[['li d imm]]
                  (\ ins (? !(two? ins) "no insns"
                            (: got (recon-li2 ins d)
                               (? !(charm? got) "li chain unreadable"
                                  (got != mask64 imm) ("li value " + show0x got + " != " + show0x (mask64 imm))
                                  ()))))]))]
   ["alu_rr" (\ _ (: row (pick arm-alu) d (nrand 0) a (nrand 0) b (nrand 0)
                     [[[(intern <row) d a b]] (spec1 [(nthl row 1)] [['r d] ['r a] ['r b]])]))]
   ["alu_imm" (\ _ (: op (pick '("add" "sub")) d (rrand 0) a (rrand 0) imm (rnd 4096)
                      [[[(intern op) d a imm]] (spec1 [op] [['r d] ['r a] ['i imm]])]))]
   ["cmp" (\ _ (: a (nrand 0)
                  (? (coin 0)
                     (: b (nrand 0) [[['cmp a b]] (spec1 '("cmp") [['r a] ['r b]])])
                     (: imm (rnd 4096) [[['cmp a imm]] (spec1 '("cmp") [['r a] ['i imm]])]))))]
   ["ld" (\ _ (: d (nrand 0) b (rrand 0) o (arm-disp 0)
                 [[['ld d b o]] (spec1 '("ldr" "ldur") [['rw d 'x] ['m b o]])]))]
   ["st" (\ _ (: b (rrand 0) o (arm-disp 0) s (nrand 0)
                 [[['st b o s]] (spec1 '("str" "stur") [['rw s 'x] ['m b o]])]))]
   ["ld_sized" (\ _ (: row (pick arm-sized-ld) d (nrand 0) b (rrand 0) o (arm-disp 0)
                       [[[(intern <row) d b o]] (spec1 (nthl row 1) [['rw d (nthl row 2)] ['m b o]])]))]
   ["st_sized" (\ _ (: row (pick '(("st1" ("strb" "sturb")) ("st2" ("strh" "sturh")) ("st4" ("str" "stur"))))
                       b (rrand 0) o (arm-disp 0) s (nrand 0)
                       [[[(intern <row) b o s]] (spec1 (nthl row 1) [['rw s 'w] ['m b o]])]))]
   ["pushpop" (\ _ (: base-sp? (\ ins (: m (mem-arm (nthl (ops-arm (i-ops <ins)) 1))
                                          (? (!(two? m) || !(id? (abs-arm <m) 'sp)) "base != sp" ())))
                      (? (coin 0)
                         (: s (nrand 0)
                            [[['push s]]
                             (\ ins (: r (run-spec ins [['("str") [['r s]]]] 0)
                                       (? (string? r) r (base-sp? ins))))])
                         (: d (nrand 0)
                            [[['pop d]]
                             (\ ins (: r (run-spec ins [['("ldr") [['r d]]]] 0)
                                       (? (string? r) r (base-sp? ins))))]))))]
   ["shift" (\ _ (: row (pick '(("shl" "lsl") ("shr" "lsr") ("sar" "asr"))) d (nrand 0) c (rnd 64)
                    allowed (? (c = 0) '("lsl" "lsr" "asr") [(nthl row 1)])
                    [[[(intern <row) d c]] (spec1 allowed [['r d] ['r d] ['i c]])]))]
   ["unary" (\ _ (: op (pick '("neg" "not" "inc" "dec")) d (nrand 0)
                    (? ((op = "neg") || (op = "not"))
                       [[[(intern op) d]] (spec1 [(? (op = "neg") "neg" "mvn")] [['r d] ['r d]])]
                       [[[(intern op) d]] (spec1 [(? (op = "inc") "add" "sub")] [['r d] ['r d] ['i 1]])])))]
   ["jmpr" (\ _ (: d (nrand 0) [[['jmpr d]] (spec1 '("br") [['r d]])]))]
   ["callr" (\ _ (: d (nrand 0) [[['callr d]] (spec1 '("blr") [['r d]])]))]
   ["setcc" (\ _ (: row (pick arm-cc) d (nrand 0)
                    [[['set <row d]] (spec1 '("cset") [['r d] ['lit (nthl row 1)]])]))]
   ["sx" (\ _ (: row (pick '(("sx1" "sxtb") ("sx2" "sxth") ("sx4" "sxtw"))) d (nrand 0)
                 [[[(intern <row) d]] (spec1 [(nthl row 1)] [['rw d 'x] ['rw d 'w]])]))]]

 ; ================================================================ riscv lane
 rv-gpregs '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 sp fp lr)
 (rv-rand _) (pick rv-gpregs)
 (rv-frand _) (pick fregs)
 (rv-imm12 _) (rint -2048 2047)
 rv-alu '(("add" "add") ("sub" "sub") ("and" "and") ("or" "or") ("xor" "xor") ("imul" "mul"))
 rv-hop '((eq "bne" 0) (ne "beq" 0) (lt "bge" 0) (ge "blt" 0)
          (gt "bge" 1) (le "blt" 1) (below "bgeu" 0) (ae "bltu" 0)
          (above "bgeu" 1) (be "bltu" 1))
 rv-ld '(("ld" "ld") ("ld1" "lb") ("ld2" "lh") ("ld4" "lw") ("ldu1" "lbu") ("ldu2" "lhu") ("ldu4" "lwu"))
 rv-st '(("st" "sd") ("st1" "sb") ("st2" "sh") ("st4" "sw"))
 rv-fhop '((above "flt.d" 1 "beqz") (be "flt.d" 1 "bnez")
           (gt "flt.d" 1 "beqz") (le "flt.d" 1 "bnez")
           (ae "fle.d" 1 "beqz") (below "fle.d" 1 "bnez")
           (ge "fle.d" 1 "beqz") (lt "fle.d" 1 "bnez")
           (eq "feq.d" 0 "beqz") (ne "feq.d" 0 "bnez"))
 (rv-spec spec) (\ ins (run-spec ins spec 1))
 gens-rv
  [["mov_rr" (\ _ (: d (rv-rand 0) s (rv-rand 0)
                     [[['mov d s]] (rv-spec [['("mv") [['r d] ['r s]]]])]))]
   ["li" (\ _ (: d (rv-rand 0) k (rnd 5)
                 imm (? (k = 0) (rv-imm12 0) (k = 1) (imm-s32 0) (k = 2) (imm-u32 0) (k = 3) (imm-big 0)
                        (0 - rint 4294967296 4611686018427387904))
                 [[['li d imm]]
                  (\ ins
                     (? (!(two? ins) || any i-bad ins) "bad li decode"
                        (: (go m val)
                            (? !(two? m) (? ((mask64 val) = (mask64 imm)) ()
                                            ("li value " + show0x (mask64 val) + " != " + show0x (mask64 imm)))
                               (: i <m o (ops-comma (i-ops i)) mn (i-mnem i)
                                  (? !(id? (abs-rv <o) d) ("li dest " + <o + " != " + string d)
                                     (: v2 (? (mn = "li") (imm-rv (nthl o 1))
                                              (mn = "lui") (: v (* (imm-rv (nthl o 1)) 4096)
                                                              (? (2147483648 <= v) (v - 4294967296) v))
                                              (mn = "addiw") (: s (% (mask64 (val + imm-rv (nthl o 2))) 4294967296)
                                                                (? (2147483648 <= s) (s - 4294967296) s))
                                              (mn = "addi") (val + imm-rv (nthl o 2))
                                              (mn = "slli") (val * pow2 (imm-rv (nthl o 2)))
                                              'bad)
                                        (? (id? v2 'bad) ("li unexpected mnem " + mn)
                                           (go >m v2))))))
                           (go ins 0))))]))]
   ["alu_rr" (\ _ (: row (pick rv-alu) d (rv-rand 0) a (rv-rand 0) b (rv-rand 0)
                     [[[(intern <row) d a b]] (rv-spec [[[(nthl row 1)] [['r d] ['r a] ['r b]]]])]))]
   ["alu_imm" (\ _ (: op (pick '("add" "sub" "and" "or" "xor")) d (rv-rand 0) a (rv-rand 0)
                      i0 (rint 1 2046)
                      imm (? ((op = "and") && (i0 = 255)) 254 i0)
                      mn (? (op = "add") "addi" (op = "sub") "addi" (op = "and") "andi" (op = "or") "ori" "xori")
                      want (? (op = "sub") (0 - imm) imm)
                      [[[(intern op) d a imm]] (rv-spec [[[mn] [['r d] ['r a] ['i want]]]])]))]
   ["cmpbr" (\ _ (: row (pick rv-hop) a (rv-rand 0) b (rv-rand 0)
                    x (? (nthl row 2) b a) y (? (nthl row 2) a b)
                    [[['cmp a b] ['br <row 'l] ['label 'l]]
                     (rv-spec [[[(nthl row 1)] [['r x] ['r y] ['i 8]]] ['("j") [['i 4]]]])]))]
   ["cmpbr_imm" (\ _ (: row (pick rv-hop) a (rv-rand 0) imm (rint 1 2047)
                        x (? (nthl row 2) 't5 a) y (? (nthl row 2) a 't5)
                        [[['cmp a imm] ['br <row 'l] ['label 'l]]
                         (rv-spec [['("li") [['r 't5] ['i imm]]]
                                   [[(nthl row 1)] [['r x] ['r y] ['i 8]]]
                                   ['("j") [['i 4]]]])]))]
   ["setcc" (\ _ (: cond (pick '(eq ne lt ge gt le below above ae be s ns))
                    a (rv-rand 0) b (rv-rand 0) d (rv-rand 0)
                    x1 [['("xori") [['r d] ['r d] ['i 1]]]]
                    spec (? (id? cond 'eq) [['("xor") [['r 't6] ['r a] ['r b]]] ['("seqz") [['r d] ['r 't6]]]]
                            (id? cond 'ne) [['("xor") [['r 't6] ['r a] ['r b]]] ['("snez") [['r d] ['r 't6]]]]
                            (id? cond 'lt) [['("slt") [['r d] ['r a] ['r b]]]]
                            (id? cond 'ge) ([['("slt") [['r d] ['r a] ['r b]]]] + x1)
                            (id? cond 'gt) [['("slt") [['r d] ['r b] ['r a]]]]
                            (id? cond 'le) ([['("slt") [['r d] ['r b] ['r a]]]] + x1)
                            (id? cond 'below) [['("sltu") [['r d] ['r a] ['r b]]]]
                            (id? cond 'above) [['("sltu") [['r d] ['r b] ['r a]]]]
                            (id? cond 'ae) ([['("sltu") [['r d] ['r a] ['r b]]]] + x1)
                            (id? cond 'be) ([['("sltu") [['r d] ['r b] ['r a]]]] + x1)
                            (id? cond 's) [['("sub") [['r 't6] ['r a] ['r b]]] ['("sltz") [['r d] ['r 't6]]]]
                            ([['("sub") [['r 't6] ['r a] ['r b]]] ['("sltz") [['r d] ['r 't6]]]] + x1))
                    [[['cmp a b] ['set cond d]] (rv-spec spec)]))]
   ["ld" (\ _ (: row (pick rv-ld) d (rv-rand 0) b (rv-rand 0) o (rv-imm12 0)
                 [[[(intern <row) d b o]] (rv-spec [[[(nthl row 1)] [['r d] ['m b o]]]])]))]
   ["st" (\ _ (: row (pick rv-st) b (rv-rand 0) o (rv-imm12 0) s (rv-rand 0)
                 [[[(intern <row) b o s]] (rv-spec [[[(nthl row 1)] [['r s] ['m b o]]]])]))]
   ["ld_far" (\ _ (: d (rv-rand 0) b (rv-rand 0)
                     off ((? (coin 0) 1 -1) * rint 2048 2147479552)
                     l0 (% (mask64 off) 4096)
                     lo (? (2047 < l0) (l0 - 4096) l0)
                     hi (% (int ((mask64 (off - lo)) / 4096)) 1048576)
                     [[['ld d b off]]
                      (rv-spec [['("lui") [['r 't6] ['i hi]]]
                                ['("add") [['r 't6] ['r 't6] ['r b]]]
                                ['("ld") [['r d] ['m 't6 lo]]]])]))]
   ["ldx" (\ _ (: d (rv-rand 0) b (rv-rand 0) ix (rv-rand 0) sc (pick '(2 4 8)) dp (rv-imm12 0)
                  amt (? (sc = 2) 1 (sc = 4) 2 3)
                  [[['ldx d b ix sc dp]]
                   (rv-spec [['("slli") [['r 't6] ['r ix] ['i amt]]]
                             ['("add") [['r 't6] ['r 't6] ['r b]]]
                             ['("ld") [['r d] ['m 't6 dp]]]])]))]
   ["stx" (\ _ (: b (rv-rand 0) ix (rv-rand 0) s (rv-rand 0) sc (pick '(2 4 8)) dp (rv-imm12 0)
                  amt (? (sc = 2) 1 (sc = 4) 2 3)
                  [[['stx b ix sc dp s]]
                   (rv-spec [['("slli") [['r 't6] ['r ix] ['i amt]]]
                             ['("add") [['r 't6] ['r 't6] ['r b]]]
                             ['("sd") [['r s] ['m 't6 dp]]]])]))]
   ["shift" (\ _ (: row (pick '(("shl" "slli") ("shr" "srli") ("sar" "srai"))) d (rv-rand 0) c (rint 1 63)
                    [[[(intern <row) d c]] (rv-spec [[[(nthl row 1)] [['r d] ['r d] ['i c]]]])]))]
   ["rot" (\ _ (: op (pick '("rol" "ror")) d (rv-rand 0) c (rint 1 63)
                  k (? (op = "ror") c (% (64 - c) 64))
                  [[[(intern op) d c]]
                   (rv-spec [['("srli") [['r 't6] ['r d] ['i k]]]
                             ['("slli") [['r d] ['r d] ['i (% (64 - k) 64)]]]
                             ['("or") [['r d] ['r d] ['r 't6]]]])]))]
   ["shiftv" (\ _ (: row (pick '(("shlv" "sll") ("shrv" "srl") ("sarv" "sra")))
                     d (pick (filter (\ r !(id? r 'r1)) rv-gpregs))
                     [[[(intern <row) d 'r1]] (rv-spec [[[(nthl row 1)] [['r d] ['r d] ['r 'r1]]]])]))]
   ["unary" (\ _ (: op (pick '("neg" "not" "inc" "dec")) d (rv-rand 0)
                    spec (? (op = "neg") [['("neg") [['r d] ['r d]]]]
                            (op = "not") [['("not") [['r d] ['r d]]]]
                            (op = "inc") [['("addi") [['r d] ['r d] ['i 1]]]]
                            [['("addi") [['r d] ['r d] ['i -1]]]])
                    [[[(intern op) d]] (rv-spec spec)]))]
   ["sxzx" (\ _ (: row (pick '(("sx1" 56 "srai") ("sx2" 48 "srai") ("zx2" 48 "srli") ("zx4" 32 "srli")))
                   d (rv-rand 0) c (nthl row 1)
                   [[[(intern <row) d]]
                    (rv-spec [['("slli") [['r d] ['r d] ['i c]]]
                              [[(nthl row 2)] [['r d] ['r d] ['i c]]]])]))]
   ["divrem" (\ _ (: row (pick '(("div" "div") ("udiv" "divu") ("rem" "rem") ("urem" "remu")))
                     d (rv-rand 0) a (rv-rand 0) b (rv-rand 0) t (rv-rand 0)
                     ir (? ((<row = "div") || (<row = "udiv")) [[(intern <row) d a b]]
                           [[(intern <row) d a b t]])
                     [ir (rv-spec [[[(nthl row 1)] [['r d] ['r a] ['r b]]]])]))]
   ["jmpr" (\ _ (: d (rv-rand 0)
                   [[['jmpr d]]
                    (? (id? d 'lr) (\ ins (run-spec ins [['("ret") ()]] 0))
                       (rv-spec [['("jr") [['r d]]]]))]))]
   ["callr" (\ _ (: d (rv-rand 0) [[['callr d]] (rv-spec [['("jalr") [['r d]]]])]))]
   ["pushpop" (\ _ (? (coin 0)
                      (: s (rv-rand 0)
                         [[['push s]] (rv-spec [['("addi") [['r 'sp] ['r 'sp] ['i -16]]]
                                                ['("sd") [['r s] ['m 'sp 0]]]])])
                      (: d (rv-rand 0)
                         [[['pop d]] (rv-spec [['("ld") [['r d] ['m 'sp 0]]]
                                               ['("addi") [['r 'sp] ['r 'sp] ['i 16]]]])])))]
   ["fp3" (\ _ (: row (pick '(("addsd" "fadd.d") ("subsd" "fsub.d") ("mulsd" "fmul.d") ("divsd" "fdiv.d")))
                  d (rv-frand 0) s (rv-frand 0)
                  [[[(intern <row) d s]] (rv-spec [[[(nthl row 1)] [['r d] ['r d] ['r s]]]])]))]
   ["fmov" (\ _ (: d (rv-frand 0) s (rv-frand 0)
                   [[['movsd d s]] (rv-spec [['("fmv.d") [['r d] ['r s]]]])]))]
   ["fcvt" (\ _ (: which (pick '("cvtsi2sd" "cvttsd2si" "movqxr" "movqrx"))
                   f (rv-frand 0) g (rv-rand 0)
                   (? (which = "cvtsi2sd")
                      [[['cvtsi2sd f g]] (rv-spec [['("fcvt.d.l") [['r f] ['r g]]]])]
                      (which = "movqxr")
                      [[['movqxr f g]] (rv-spec [['("fmv.d.x") [['r f] ['r g]]]])]
                      (which = "movqrx")
                      [[['movqrx g f]] (rv-spec [['("fmv.x.d") [['r g] ['r f]]]])]
                      [[['cvttsd2si g f]]
                       (\ ins (: r (run-spec ins [['("fcvt.l.d") [['r g] ['r f]]]] 0)
                                 (? (string? r) r
                                    (: o (ops-comma (i-ops <ins))
                                       (? (((tally o) = 3) && ((nthl o 2) = "rtz")) ()
                                          "fcvt.l.d rm != rtz")))))])))]
   ["fldst" (\ _ (: which (pick '("ldsd" "stsd" "ldss" "stss"))
                    f (rv-frand 0) b (rv-rand 0) o (rv-imm12 0)
                    (? (which = "ldsd") [[['ldsd f b o]] (rv-spec [['("fld") [['r f] ['m b o]]]])]
                       (which = "stsd") [[['stsd b o f]] (rv-spec [['("fsd") [['r f] ['m b o]]]])]
                       (which = "ldss") [[['ldss f b o]] (rv-spec [['("flw") [['r f] ['m b o]]]])]
                       [[['stss b o f]] (rv-spec [['("fsw") [['r f] ['m b o]]]])])))]
   ["fcmpbr" (\ _ (: row (pick rv-fhop) a (rv-frand 0) b (rv-frand 0)
                     x (? (nthl row 2) b a) y (? (nthl row 2) a b)
                     [[['ucomisd a b] ['br <row 'l] ['label 'l]]
                      (rv-spec [[[(nthl row 1)] [['r 't6] ['r x] ['r y]]]
                                [[(nthl row 3)] [['r 't6] ['i 8]]]
                                ['("j") [['i 4]]]])]))]
   ["mulo" (\ _ (: rs (pick-n rv-gpregs 4) d <rs a (nthl rs 1) b (nthl rs 2) t (nthl rs 3)
                   [[['mulo d a b t 'l] ['label 'l]]
                    (rv-spec [['("mulh") [['r t] ['r a] ['r b]]]
                              ['("mul") [['r d] ['r a] ['r b]]]
                              ['("srai") [['r 't6] ['r d] ['i 63]]]
                              ['("beq") [['r t] ['r 't6] ['i 8]]]
                              ['("j") [['i 4]]]])]))]]

 ; ---- the lanes; the 10th column names the disassembler a lane needs ----
 arches
  [["amd64" 'amd64 gens-x64 (\ hx (disasm-x64 hx)) ops-comma mem-x64 (\ s (abs-x64 s)) imm-x64 1 "objdump"]
   ["arm64" 'arm64 gens-arm (\ hx (disasm-mc hx "aarch64" "" "//" 0)) ops-arm mem-arm (\ s (abs-arm s)) imm-arm 0 "llvm-mc"]
   ["riscv" 'rv64 gens-rv (\ hx (disasm-mc hx "riscv64" " --mattr=+m,+f,+d,+c" "#" 1)) ops-comma mem-rv (\ s (abs-rv s)) imm-rv 0 "llvm-mc"]]
 (tool? t) ((sh ("command -v " + t + " >/dev/null 2>&1")) = 0)

 fz-n (: v ("" + getenv "FUZZ_N") (? (0 < tally v) (s2i v) 8))
 fz-seed (: v ("" + getenv "FUZZ_SEED") (? (0 < tally v) (s2i v) 20250717))
 fz-arch ("" + getenv "FUZZ_ARCH")

 (run-lane row)
  (: nm <row tgt (nthl row 1) gens (nthl row 2)
     _ (pin AR 'ops (nthl row 4)) _ (pin AR 'mem (nthl row 5))
     _ (pin AR 'abs (nthl row 6)) _ (pin AR 'imm (nthl row 7))
     llvm2 ((nthl row 8) && (tool? "llvm-mc"))
     _ (rseed fz-seed)
     _ (puts ("\n=== holo " + nm + " encoder fuzz: seed " + show fz-seed + ", " + show fz-n + "/class ===\n"))
     (class cl) (: cn <cl gen (nthl cl 1)
                   (go k p fl)
                    (? (fz-n <= k) [p fl]
                       (: s (gen 0) ir <s chk (nthl s 1)
                          hx (holo-hex tgt ir)
                          r (? !(string? hx) "holo produced no output"
                               (: ins ((nthl row 3) hx)
                                  r1 (chk ins)
                                  (? (string? r1) r1
                                     (llvm2 && !(llvm-ok? hx)) "llvm-mc reject"
                                     ())))
                          (? (string? r)
                             (: _ (puts ("  [" + cn + "#" + show k + "] " + show ir + "\n      hex="
                                         + ("" + hx) + "  :: " + r + "\n"))
                                (go (k + 1) p (fl + 1)))
                             (go (k + 1) (p + 1) fl))))
                   (: r (go 0 0 0)
                      _ (puts ("  " + cn + "  " + show <r + " pass  " + show <>r + " fail"
                               + (? (0 < <>r) "  <-- FAIL" "") + "\n"))
                      <>r))
     (foldl (\ a cl (a + class cl)) 0 gens))

 lanes (? (0 < tally fz-arch) (filter (\ r (<r = fz-arch)) arches) arches)
 bad (foldl (\ a r (? (tool? (nthl r 9)) (a + run-lane r)
                      (: _ (puts ("  (" + <r + " skipped: no " + nthl r 9 + ")\n")) a)))
            0 lanes)
 _ (puts ("\nholo fuzz: " + (? (0 < bad) ((show bad) + " FAILURES\n") "all classes green\n")))
 (? (0 < bad) (quit 1) (quit 0)))
