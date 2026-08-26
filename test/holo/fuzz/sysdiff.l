; test/holo/fuzz/sysdiff.l -- holo SYSTEM-lane differential: every privileged encoding,
; byte-exact against llvm-mc. fuzz.l is a DECODE oracle (holo may legally pick among
; encodings); the system lane is the opposite -- each op has exactly one encoding and a
; small enumerable operand space, so the sharp oracle is ASSEMBLE-side: write the
; instruction llvm-mc's way, assemble, demand the same bytes holo produced.
;
; the arm64 op tables are read STRAIGHT OUT of crew/holo/arm64.l, so a row added to
; holo is checked the next run with no edit here -- the uncovered row is the one that
; ships wrong. the x86 side has no name tables (the operand space IS the register
; file), so it enumerates. SYSDIFF_ARCH=x64|arm64 narrows; default both.
;
;   out/host/love test/holo/fuzz/sysdiff.l          (exit 1 on any disagreement)

(reads (open "crew/holo/holo.l" "r"))
(reads (open "crew/holo/amd64.l" "r"))
(reads (open "crew/holo/arm64.l" "r"))
(reads (open "crew/holo/rv64.l" "r"))
; holo rides module 'holo: reading the source REGISTERS it, and the names come
; through the door rather than off this file's layer.
(use 'holo)

(:
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
 (idx-from s sub i0) (: n (tally s) k (tally sub)
                        (at? i j) (? (k <= j) 1 ((s (i + j)) = (sub j)) (at? i (j + 1)) 0)
                        (go i) (? (n < (i + k)) () (at? i 0) i (go (i + 1)))
                        (go i0))
 (idx-of s sub) (idx-from s sub 0)
 (has? s sub) (charm? (idx-of s sub))
 (digit? c) ((48 <= c) && (c <= 57))
 (s2d s) (: n (tally s)                                  ; decimal, () on a non-digit
            (go i acc) (? (n <= i) acc (digit? (s i)) (go (i + 1) (10 * acc + ((s i) - 48))) ())
            (? (n = 0) () (go 0 0)))
 (sh c) (: pid (spawn ["sh" "-c" c]) (? (pid < 0) 1 (wait pid)))
 (rdf p) (: q (open p "r") (? q (: t (slurp q) _ (close q) t) ""))
 (wrf p t) (: q (open p "w") _ (say q t) (close q))
 tmpd "out/host"

 ; --- the ('key >< ..) rows of a `name (tabof ...)` form in arm64.l, source order ---
 a64src (rdf "crew/holo/arm64.l")
 (table nm)
  (: i (idx-of a64src ("\n   " + nm + " (tabof"))
     _ (? (charm? i) 0 (: _ (puts ("sysdiff: no table " + nm + "\n")) (quit 1)))
     j0 (idx-from a64src "(tabof" i)
     ; walk to the (tabof ..) close, then harvest ('name >< inside [i, j)
     (close j d) (? ((a64src j) = 40) (close (j + 1) (d + 1))
                    ((a64src j) = 41) (? (d = 1) j (close (j + 1) (d - 1)))
                    (close (j + 1) d))
     j (close j0 0)
     (namec? c) ((digit? c) || ((65 <= c) && (c <= 90)) || ((97 <= c) && (c <= 122)) || (c = 95))
     (harvest k acc)
      (: h (idx-from a64src "('" k)
         (? (!(charm? h) || (j < h)) (rev acc)
            (: (e m) (? (namec? (a64src m)) (e (m + 1)) m)
               b (e (h + 2))
               (? (has? (snip a64src b (b + 3)) "><")
                  (harvest b ((snip a64src (h + 2) b) >< acc))
                  (harvest b acc)))))
     (harvest i ()))

 ; --- the cases: [ir-form asm-text pair-key] (pair-key () when unpaired) ---
 (cases-a64 _)
  (: sysregs (table "arm64-sysregs")
     out (foldl (\ a r (a + [[['mrs 'r5 (intern r)] ("mrs x5, " + r) ()]
                            [['msr (intern r) 'r5] ("msr " + r + ", x5") ()]]))
                () sysregs)
     out (foldl (\ a o (a + [[['dsb (intern o)] ("dsb " + o) ()]
                            [['dmb (intern o)] ("dmb " + o) ()]]))
                out (table "arm64-barrier-opts"))
     out (foldl (\ a f (a + foldl (\ b i (b + [[['msri (intern f) i] ("msr " + f + ", #" + show i) ()]])) () ^16))
                out (table "arm64-pstate"))
     ; the SYS family: llvm-mc knows which operations take a register and refuses the
     ; other spelling; offer BOTH, and `pair` demands the accepted one matched.
     out (foldl (\ a row (: ir <row
                            (foldl (\ b o (: pk (ir + " " + o)
                                             (b + [[[(intern ir) (intern o) 'r7] (ir + " " + o + ", x7") pk]
                                                   [[(intern ir) (intern o) 'zr] (ir + " " + o) pk]])))
                                   a (table (nthl row 1)))))
                out '(("tlbi" "arm64-tlbi-ops") ("ic" "arm64-ic-ops")
                      ("dc" "arm64-dc-ops") ("at" "arm64-at-ops")))
     out (foldl (\ a imm (a + [[['brk imm] ("brk #" + show imm) ()]
                              [['dbrk imm] ("hlt #" + show imm) ()]
                              [['hvc imm] ("hvc #" + show imm) ()]
                              [['smc imm] ("smc #" + show imm) ()]
                              [['udf imm] ("udf #" + show imm) ()]]))
                out '(0 1 2 15 255 4660 61440 65535))
     out (foldl (\ a n (a + [[[(intern n)] n ()]])) out '("isb" "wfi" "wfe" "eret"))
     (out + [[['lea 'r9 'sp 0] "mov x9, sp" ()]                 ; the SP move `mov` cannot make
             [['lea 'sp 'r9 0] "mov sp, x9" ()]]))

 x64-r2x '((r0 "rax") (r1 "rcx") (r2 "rdx") (r3 "rbx") (r4 "rbp") (r5 "rsi") (r6 "rdi")
           (r7 "r8") (r8 "r9") (r9 "r10") (r10 "r11") (r11 "r12") (r12 "r13") (r13 "r14")
           (r14 "r15") (sp "rsp"))
 x64-w '(("rax" "ax") ("rcx" "cx") ("rdx" "dx") ("rbx" "bx") ("rsp" "sp") ("rbp" "bp")
         ("rsi" "si") ("rdi" "di") ("r8" "r8w") ("r9" "r9w") ("r10" "r10w") ("r11" "r11w")
         ("r12" "r12w") ("r13" "r13w") ("r14" "r14w") ("r15" "r15w"))
 (cases-x64 _)
  (: gp (filter (\ p !(id? <p 'sp)) x64-r2x)               ; rsp is not a CR-move operand
     out (foldl (\ a n (a + foldl (\ b p (b + [[['ldcr <p n] ("mov " + <>p + ", cr" + show n) ()]
                                              [['stcr n <p] ("mov cr" + show n + ", " + <>p) ()]]))
                                  () gp))
                () '(0 1 2 3 4 5 6 7 8))
     out (foldl (\ a p (: r <p x <>p
                          a2 (foldl (\ b off (b + [[['lgdt r off] ("lgdt [" + x + " + " + show off + "]") ()]
                                                  [['lidt r off] ("lidt [" + x + " + " + show off + "]") ()]
                                                  [['invlpg r off] ("invlpg [" + x + " + " + show off + "]") ()]]))
                                    a '(0 8 16 200))
                          (a2 + [[['ltr r] ("ltr " + (: w (assoc x x64-w) <>w)) ()]])))
                out x64-r2x)
     out (foldl (\ a n (? (n = 3) a                        ; llvm folds `int 3` to CC; holo keeps CD 03
                          (a + [[['int n] ("int " + show n) ()]])))
                out ^256)
     (out + foldl (\ a n (a + [[[(intern n)] n ()]])) ()
            '("cli" "sti" "hlt" "ud2" "iretq" "swapgs" "rdmsr" "wrmsr" "cpuid" "rdtsc")))

 ; --- llvm-mc assembling: two passes (an error would slide the encodings) ---
 ro-reject "expected writable system register"
 (llvm arch asms)
  (: triple (? (arch = "arm64") "aarch64" "x86_64")
     nhead (? (arch = "arm64") 0 1)
     head (? (arch = "arm64") "" ".intel_syntax noprefix\n")
     (lay ls) (wrf (tmpd + "/.sysd.in") (head + foldl (\ a l (a + "\t" + l + "\n")) "" ls))
     _ (lay asms)
     _ (sh ("llvm-mc -triple=" + triple + " --show-encoding < " + tmpd + "/.sysd.in > "
            + tmpd + "/.sysd.out 2> " + tmpd + "/.sysd.err"))
     ; pass 1: the rejected line numbers (0-based case index) and their reasons
     bad (foldl (\ a ln (: i (idx-of ln "<stdin>:")
                           (? !(charm? i) a
                              (: j (idx-from ln ":" (i + 8))
                                 e (idx-of ln ": error: ")
                                 (? (!(charm? j) || !(charm? e)) a
                                    (: n (s2d (snip ln (i + 8) j))
                                       (? (id? n ()) a
                                          ([(n - 1 - nhead) (snip ln (e + 9) (tally ln))] >< a))))))))
                () (lines (rdf (tmpd + "/.sysd.err"))))
     good (: (go i m acc) (? !(two? m) (rev acc)
                             (? (two? (assoc i bad)) (go (i + 1) >m acc)
                                (go (i + 1) >m (<m >< acc))))
             (go 0 asms ()))
     _ (lay good)
     _ (sh ("llvm-mc -triple=" + triple + " --show-encoding < " + tmpd + "/.sysd.in > "
            + tmpd + "/.sysd.out 2>/dev/null"))
     enc (foldl (\ a ln (: i (idx-of ln "encoding: [")
                           (? !(charm? i) a
                              (: j (idx-from ln "]" i)
                                 raw (snip ln (i + 11) j)
                                 hx (: (go k acc) (? ((tally raw) <= k) (string (rev acc))
                                                     (: c (raw k)
                                                        (? ((c = 48) && (k + 1 < tally raw) && ((raw (k + 1)) = 120))
                                                           (go (k + 2) acc)
                                                           (c = 44) (go (k + 1) acc)
                                                           (go (k + 1) (c >< acc)))))
                                       (go 0 ()))
                                 (a + [hx])))))
                () (lines (rdf (tmpd + "/.sysd.out"))))
     _ (? ((tally enc) = (tally good)) 0
          (: _ (puts ("sysdiff: llvm-mc gave " + show (tally enc) + " encodings for "
                      + show (tally good) + " lines\n")) (quit 1)))
     ; reassemble the full column: [hex ()] accepted, [() why] rejected
     (: (go i m es acc)
         (? !(two? m) (rev acc)
            (: b (assoc i bad)
               (? (two? b) (go (i + 1) >m es ([() <>b] >< acc))
                  (go (i + 1) >m >es ([<es ()] >< acc)))))
        (go 0 asms enc ())))

 (sweep arch)
  (: cs (? (arch = "arm64") (cases-a64 0) (cases-x64 0))
     _ (puts ("\n=== holo " + arch + " system-lane differential vs llvm-mc ===\n"))
     tgt (? (arch = "arm64") 'arm64 'amd64)
     ref (llvm arch (map (\ c (nthl c 1)) cs))
     seen (tablet 0)
     _ (map (\ c (? (id? (nthl c 2) ()) 0
                    (? (peep seen (intern (nthl c 2)) -1) 0 (pin seen (intern (nthl c 2)) 0))))
            cs)
     (one c r st)                                         ; st = [npass nskip fails]
      (: ir <c asm (nthl c 1) pair (nthl c 2)
         want <r why (nthl r 1)
         got (holo-hex tgt [ir])
         (? (id? want ())
            (? ((has? why ro-reject) && (4 <= tally asm) && ((snip asm 0 4) = "msr ")) [<st (nthl st 1 + 1) (nthl st 2)]
               !(id? pair ()) [<st (nthl st 1 + 1) (nthl st 2)]
               [<st (nthl st 1) ((nthl st 2) + [[ir asm got ("llvm-mc reject: " + why)]])])
            (? (got = want)
               (: _ (? (id? pair ()) 0 (pin seen (intern pair) 1))
                  [(<st + 1) (nthl st 1) (nthl st 2)])
               [<st (nthl st 1) ((nthl st 2) + [[ir asm got ("want " + want)]])])))
     st (: (go m rs st) (? !(two? m) st (go >m >rs (one <m <rs st)))
           (go cs ref [0 0 ()]))
     ; a pair whose spellings llvm-mc BOTH refused hides a table typo -- fail it
     fails ((nthl st 2)
            + (foldl (\ a c (: pk (nthl c 2)
                               (? (id? pk ()) a
                                  (? (peep seen (intern pk) 0) a
                                     (: _ (pin seen (intern pk) 1)
                                        (a + [[pk pk () "llvm-mc took NEITHER spelling"]]))))))
                     () cs))
     _ (puts ("  " + show <st + " match  " + show (tally fails) + " fail  "
              + show (nthl st 1) + " skipped (read-only / wrong-arity spelling)\n"))
     _ (map (\ f (puts ("  " + show <f + "\n      as " + nthl f 1 + " got=" + ("" + nthl f 2)
                        + " :: " + nthl f 3 + "\n")))
            fails)
     (? (two? fails) 1 0))

 sd-arch ("" + getenv "SYSDIFF_ARCH")
 arches (? (sd-arch = "amd64") '("amd64") (sd-arch = "arm64") '("arm64") '("amd64" "arm64"))
 bad (foldl (\ a x (a + sweep x)) 0 arches)
 (? (0 < bad) (quit 1) (quit 0)))
