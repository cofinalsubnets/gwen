; test/holo/fuzz/rvc.l -- the RVC squeeze, checked by EQUIVALENCE rather than by intent.
; riscv.l collapses a lowered 32-bit word to its 2-byte C form where the operands fit
; (rvc-of). the two words must mean the SAME thing, so this walks the whole reachable
; operand space of every form -- in range and out, aligned and not -- and disassembles
; each compressed word beside the 32-bit word it replaced under llvm-mc. any pair that
; decodes differently is a miscompression.
;
; this is a different oracle from fuzz.l next door: that one asks whether holo encoded
; what the IR meant, over sampled forms; this one asks whether the squeeze preserved
; meaning, over ALL of them. a form rvc-of declines is exercised too -- the sweep runs
; the boundaries (offset 248/256, immediate 31/32) from both sides.
;
; part two covers the C forms the squeeze never sees: c.j and c.beqz/c.bnez, whose WIDTH
; turns on a distance, so riscv.l's 'short hook chooses them and rv-patch writes them.
; there is no 32-bit word to compare against -- the wide form was a whole extra
; instruction -- so these are checked by INTENT, every reachable even offset against the
; spelling llvm-mc gives back, plus the cases rv-cbase must REFUSE.
;
; ⚠ ONE alias is normalized: llvm-mc prints the 32-bit `add rd, x0, rs2` as itself but
; prints the C form as `mv`. c.mv IS that add by definition (RISC-V spec), so the two
; spellings are one instruction and the text compare must not part them.
;
; skips (exit 0, a note) without llvm-mc. run from the repo root:
;
;   out/host/love test/holo/fuzz/rvc.l
;
; exit 1 on any mismatch, naming the word, both spellings and the count.

(reads (open "crew/holo/holo.l" "r"))
(reads (open "crew/holo/riscv.l" "r"))

(: _ (use (name "holo"))

   tmpd "out/host"
   (sh c) (: pid (spawn ["sh" "-c" c]) (? (pid < 0) 1 (wait pid)))
   (rdf p) (: q (open p "r") (? q (: t (slurp q) _ (close q) t) ""))
   (wrf p t) (: q (open p "w") _ (say q t) (close q))
   (tool? t) ((sh ("command -v " + t + " >/dev/null 2>&1")) = 0)

   (lines s) (: n (tally s)
                (go i j acc) (? (n <= j) (rev (? (i < j) ((snip s i j) >< acc) acc))
                                ((s j) = 10) (go (j + 1) (j + 1) ((snip s i j) >< acc))
                                (go i (j + 1) acc))
                (go 0 0 ()))
   (ws? c) ((c = 32) || (c = 9))
   ; squash runs of blank to one space and strip the ends -- llvm-mc indents with a tab
   (norm s) (: n (tally s)
               (go i acc sp) (? (n <= i) (rev acc)
                                (ws? (s i)) (go (i + 1) acc 1)
                                (go (i + 1) ((s i) >< (? (sp && (two? acc)) (32 >< acc) acc)) 0))
               (string (go 0 () 0)))

   hexd "0123456789abcdef"
   (hx2 b) (: c (\ i (hexd i)) (string ((c ((b >> 4) & 15)) >< ((c (b & 15)) >< ()))))
   (bytes-of w n) (: (go i acc) (? (n <= i) acc (go (i + 1) (acc + ("0x" + hx2 ((w >> (8 * i)) & 255)) + " ")))
                     (go 0 ""))

   ;; -- the sweep. every (word32 . word16) pair rvc-of answers on the enumerated space. --
   (mk-i imm rs1 f3 rd op) (((imm & 4095) << 20) | ((rs1 << 15) | ((f3 << 12) | ((rd << 7) | op))))
   (mk-r f7 rs2 rs1 f3 rd op) ((f7 << 25) | ((rs2 << 20) | ((rs1 << 15) | ((f3 << 12) | ((rd << 7) | op)))))
   (mk-s imm rs1 rs2 f3 op) (: u (imm & 4095)
                               (((u >> 5) << 25) | ((rs2 << 20) | ((rs1 << 15) | ((f3 << 12) | (((u & 31) << 7) | op))))))

   (upto n) (jot (n + 1))
   regs  (upto 31)
   pregs '(8 9 10 11 12 13 14 15)
   imms  (map (\ i (i - 40)) (upto 80))          ; -40..40, straddling the 6-bit edges
   offs  (map (\ i (i * 8)) (upto 66))           ; 0..528 by 8, past both 248 and 504
   odds  '(4 12 20 250 252 500 508 512)          ; non-multiples of 8, and over range
   shs   (upto 63)
   alloff (offs + odds)

   ; every candidate 32-bit word the sweep visits
   words (: (fl ls) (foldl (\ a l (a + l)) () ls)
            (over l f) (fl (map f l))
            (fl `(
              (over regs (\ rd (over regs (\ rs1 (map (\ i (mk-i i rs1 0 rd 0x13)) imms)))))
              (over regs (\ rd (fl (map (\ sh `((mk-i sh rd 1 rd 0x13)
                                                 (mk-i (sh | 1024) rd 5 rd 0x13)
                                                 (mk-i sh rd 5 rd 0x13))) shs))))
              (over pregs (\ rd (map (\ i (mk-i i rd 7 rd 0x13)) imms)))
              (over regs (\ rd (over regs (\ rs1 (map (\ i (mk-i i rs1 0 rd 0x1b)) imms)))))
              (over regs (\ rd (over regs (\ rs1 (map (\ rs2 (mk-r 0 rs2 rs1 0 rd 0x33)) regs)))))
              (over pregs (\ rd (fl (map (\ rs2 `((mk-r 32 rs2 rd 0 rd 0x33)
                                                   (mk-r 0 rs2 rd 4 rd 0x33)
                                                   (mk-r 0 rs2 rd 6 rd 0x33)
                                                   (mk-r 0 rs2 rd 7 rd 0x33))) pregs))))
              (over regs (\ rd (fl (map (\ o `((mk-i o 2 3 rd 0x03) (mk-i o 2 3 rd 0x07))) alloff))))
              (over pregs (\ rd (over pregs (\ rs1 (fl (map (\ o `((mk-i o rs1 3 rd 0x03)
                                                                   (mk-i o rs1 3 rd 0x07))) alloff))))))
              (over regs (\ rs2 (fl (map (\ o `((mk-s o 2 rs2 3 0x23) (mk-s o 2 rs2 3 0x27))) alloff))))
              (over pregs (\ rs2 (over pregs (\ rs1 (fl (map (\ o `((mk-s o rs1 rs2 3 0x23)
                                                                    (mk-s o rs1 rs2 3 0x27))) alloff))))))
              (map (\ rs1 (mk-i 0 rs1 0 0 0x67)) regs)
              ; addi sp, sp, k by 8 across c.addi16sp's whole reach and past both ends: the
              ; multiples of 16 take the sp form, the odd ones must fall back or refuse
              (map (\ k (mk-i ((k * 8) - 544) 2 0 2 0x13)) (upto 136)))))

   ; keep only those that compress, paired with their C word
   pairs (foldl (\ a w (: c (rvc-of w)
                           (? (two? c) ([w ((at c 0) | ((at c 1) << 8))] >< a) a))) () words)
   npair (tally pairs)

   _ (? (tool? "llvm-mc") 0
        (: _ (say out "test_rvc: skipped (needs llvm-mc)\n") (quit 0)))
   _ (? (0 < npair) 0
        (: _ (say err "FAIL test_rvc: the sweep compressed nothing -- is rvc-of wired in?\n") (quit 1)))

   _ (wrf (tmpd + "/.rvc32.s") (foldl (\ a p (a + (bytes-of <p 4) + "\n")) "" pairs))
   _ (wrf (tmpd + "/.rvc16.s") (foldl (\ a p (a + (bytes-of (at p 1) 2) + "\n")) "" pairs))
   mc "llvm-mc --disassemble --triple=riscv64 --mattr=+m,+f,+d,+c"
   _ (sh (mc + " < " + tmpd + "/.rvc32.s > " + tmpd + "/.rvc32.out 2>/dev/null"))
   _ (sh (mc + " < " + tmpd + "/.rvc16.s > " + tmpd + "/.rvc16.out 2>/dev/null"))
   d32 (lines (rdf (tmpd + "/.rvc32.out")))
   d16 (lines (rdf (tmpd + "/.rvc16.out")))

   (pfx? s p) (: k (tally p) ((k <= tally s) && ((snip s 0 k) = p)))
   (idx-of s sub) (: n (tally s) k (tally sub)
                     (go i) (? ((n - k) < i) () ((snip s i (i + k)) = sub) i (go (i + 1)))
                     (go 0))
   ; `add rd, zero, rs2` and `mv rd, rs2` are ONE instruction -- see the header
   (demv s) (: t (norm s)
               (? (pfx? t "add ")
                  (: c (idx-of t ", zero, ")
                     (? (charm? c) ("mv " + (snip t 4 c) + ", " + (snip t (c + 8) (tally t))) t))
                  t))
   (firstn n l) (? ((n < 1) || !(two? l)) () (<l >< firstn (n - 1) >l))

   _ (? ((tally d32) = npair) 0
        (: _ (say err ("FAIL test_rvc: llvm-mc decoded " + show (tally d32)
                        + " of " + show npair + " full words\n")) (quit 1)))
   _ (? ((tally d16) = npair) 0
        (: _ (say err ("FAIL test_rvc: llvm-mc decoded " + show (tally d16)
                        + " of " + show npair + " compressed words -- an unencodable C form?\n")) (quit 1)))

   bad (foldl (\ a i (: x (demv (at d32 i)) y (norm (at d16 i))
                         (? (x = y) a ([(at (at pairs i) 0) x y] >< a)))) () (jot npair))
   _ (foldl (\ _ b (: _ (say err ("  word " + show (at b 0) + ": 32-bit `" + (at b 1)
                                   + "` vs C `" + (at b 2) + "`\n")) 0)) 0 (firstn 12 bad))
   _ (? !(two? bad) 0
        (: _ (say err ("FAIL test_rvc: " + show (tally bad) + " of " + show npair
                        + " compressions change meaning\n")) (quit 1)))

   ;; -- part two: the label-relative C forms. laid consecutively, so instruction i sits at 2i
   ;;    and its target at 2i + rel -- which is the offset llvm-mc prints back.
   (flat ls) (foldl (\ a l (a + l)) () ls)
   (bhex bs) (foldl (\ a b (a + "0x" + hx2 b + " ")) "" bs)
   (bform f3 rs1 rs2) ((f3 << 12) | ((rs1 << 15) | ((rs2 << 20) | 0x63)))
   abin   '("s0" "s1" "a0" "a1" "a2" "a3" "a4" "a5")     ; llvm-mc prints the ABI names
   cjrels (map (\ i ((i * 2) - 2048)) (upto 2047))       ; every even offset CJ reaches
   cbrels (map (\ i ((i * 2) - 256)) (upto 255))         ; and every even offset CB reaches
   cbcases (flat (map (\ r (flat (map (\ f3 (map (\ rl [r f3 rl]) cbrels)) '(0 1)))) pregs))
   cjw (map (\ i (rv-patch 'cj ((i * 2) + (at cjrels i)) (i * 2) 2 0xA001)) (jot (tally cjrels)))
   cbw (map (\ i (: c (at cbcases i)
                     (rv-patch 'cb ((i * 2) + (at c 2)) (i * 2) 2
                               (rv-cbase (bform (at c 1) (at c 0) 0) (at c 2)))))
            (jot (tally cbcases)))
   want (map (\ r ("j " + show r)) cjrels
          + map (\ c ((? ((at c 1) = 0) "beqz " "bnez ") + ((at abin ((at c 0) - 8))
                        + (", " + show (at c 2))))) cbcases)
   _ (wrf (tmpd + "/.cbr.s") (foldl (\ a b (a + (bhex b) + "\n")) "" (cjw + cbw)))
   _ (sh (mc + " < " + tmpd + "/.cbr.s > " + tmpd + "/.cbr.out 2>/dev/null"))
   dbr (map norm (lines (rdf (tmpd + "/.cbr.out"))))
   _ (? ((tally dbr) = tally want) 0
        (: _ (say err ("FAIL test_rvc: llvm-mc decoded " + show (tally dbr) + " of "
                        + show (tally want) + " compressed branches\n")) (quit 1)))
   bbad (foldl (\ a i (? ((at dbr i) = (at want i)) a ([(at want i) (at dbr i)] >< a)))
               () (jot (tally want)))
   _ (foldl (\ _ b (: _ (say err ("  wanted `" + (at b 0) + "` got `" + (at b 1) + "`\n")) 0))
            0 (firstn 12 bbad))
   _ (? !(two? bbad) 0
        (: _ (say err ("FAIL test_rvc: " + show (tally bbad) + " of " + show (tally want)
                        + " compressed branches carry the wrong offset\n")) (quit 1)))
   ;; the domain: rv-cbase must REFUSE what CB cannot say -- out of reach either way, a
   ;; register outside the window, a second source, a compare that is not beq/bne.
   nocb '((0 8 0 256) (0 8 0 -258) (0 7 0 100) (0 16 0 100) (0 8 9 100) (4 8 0 100) (6 8 0 100))
   loose (foldl (\ a c (? (imm? (rv-cbase (bform (at c 0) (at c 1) (at c 2)) (at c 3)))
                          (c >< a) a)) () nocb)
   _ (? !(two? loose) 0
        (: _ (say err ("FAIL test_rvc: rv-cbase took " + show (tally loose)
                        + " cases CB cannot encode\n")) (quit 1)))
   _ (say out ("test_rvc: " + show npair
                + " compressed words decode identically to the 32-bit words they replace, and "
                + show (tally want) + " c.j/c.beqz/c.bnez offsets read back exactly\n"))
   0)
