; compose.l -- rung 1 of the splice JIT automation: `dis` a live closure (love/ev.l,
; the emission interface's dual), map each row to its op's OWN C body -- op11/op/fld
; macro args extracted from love.c's text, a nif name bridged through nifs.h's rows --
; and emit a composed function: operands baked as immediates, one room guard up front,
; the glaze's deopt-to-twin (Ip = cell(Ip[1].x), BEFORE Sp moves, so the twin sees the
; untouched frame), and the nif-cell epilogue. mooncc compiles it, lift.l pulls the
; bytes, check.l installs and differentials against the interp twin.
;
;   LOVE_NO_GLAZE=1 love -l bench/vmsplice/samples.l bench/vmsplice/compose.l <outdir>
;
; a row the table cannot say DECLINES the whole sample and names itself -- the coverage
; report is what prices the next extraction rung. covered today: the load family
; (arg/argN/quote-of-charm/quoN/quomN, the aa..qq pairs), the fld consumers
; (argcap/argcup/argtwo), and every op11/op-declared nif body -- call-free bodies over
; love.h's public accessor macros. an op that jumps, applies or allocates is the seam
; a segment cannot cross yet (doc/moon-regalloc.md, the splice client).
(: (die m) (: _ (say err ("compose: " m "\n")) (quit 1))
   as (cuup cmdline)
   _ (? (two? as) 0 (die "usage: love -l samples.l compose.l <outdir>"))
   odir (last as)

   ; --- tiny text kit ---------------------------------------------------------
   (idxof s sub from) (: n (tally s) m (tally sub)
    (: (go i) (? ((i + m) > n) -1
                 (: (eq j) (? (j = m) 1 (? ((peep s (i + j) 0) = (peep sub j 0)) (eq (j + 1)) 0))
                    (? (eq 0) i (go (i + 1)))))
       (go from)))
   (s2n s) (: (go i a) (? (i < (tally s)) (: c (peep s i 0)
                            (? (&& (47 < c) (c < 58)) (go (i + 1) ((a * 10) + (c - 48))) a)) a)
             (go 0 0))
   (ws? c) (|| (c = 32) (|| (c = 10) (c = 9)))
   (trimt s) (: a (: (go i) (? (&& (i < (tally s)) (ws? (peep s i 0))) (go (i + 1)) i) (go 0))
                b (: (go i) (? (&& (a < i) (ws? (peep s (i - 1) 0))) (go (i - 1)) i) (go (tally s)))
                (snip s a b))
   ; one macro argument: from i, the index of the top-level `,` or the macro's own `)`.
   ; char literals ('X') are skipped; these bodies carry no strings.
   (arg1 s i) (: (go j d) (: c (peep s j 0)
                  (? (c = 39) (go (j + 3) d)
                     (c = 40) (go (j + 1) (d + 1))
                     (c = 41) (? (d = 0) j (go (j + 1) (d - 1)))
                     (&& (c = 44) (d = 0)) j
                     (go (j + 1) d)))
                 (go i 0))

   ; --- love.c: op11(nom, x) / op(nom, n, x) / op1(nom, i, x) / fld(nom, val) ---
   lovec (: q (open "love.c" "r") s (slurp q) _ (close q) s)
   (harv pat nargs tag) (: pl (tally pat)
    (: (go i acc) (: j (idxof lovec pat i)
        (? (j < 0) acc
           (: a0 (j + pl)
              e0 (arg1 lovec a0) nm (name (trimt (snip lovec a0 e0)))
              a1 (e0 + 1)
              e1 (arg1 lovec a1) x1 (trimt (snip lovec a1 e1))
              (? (nargs = 2) (go (j + pl) ((L nm tag x1) >< acc))
                 (: a2 (e1 + 1) e2 (arg1 lovec a2) x2 (trimt (snip lovec a2 e2))
                    (go (j + pl) ((L nm tag x1 x2) >< acc)))))))
       (go 0 ())))
   optab (((harv "\nop11(" 2 'e1) + (harv " op11(" 2 'e1)) +
          (((harv "\nfld(" 2 'fld) + (harv "\nop(" 3 'opn)) +
           ((harv " op(" 3 'opn) + (harv "\nop1(" 3 'ipn))))
   (findop nm) (assoc nm optab)

   ; --- nifs.h: "name" -> lvm nom (a def1 row aims at a nifs[] run; word 0 = the code)
   nifh (: q (open "nifs.h" "r") s (slurp q) _ (close q) s)
   nifend (idxof nifh "};" 0)
   nrows (: (go i acc) (: j (idxof nifh "\n {" i)
              (? (|| (j < 0) (j > nifend)) (rev acc)
                 (: k (j + 3)
                    e (idxof nifh "}" k)
                    (go (j + 3) ((trimt (snip nifh k e)) >< acc)))))
            (go 0 ()))
   (nrow i) (: (go l k) (? (atom? l) "" (k = i) (cap l) (go (cup l) (k + 1))) (go nrows 0))
   (lvmof nm) (: pat (+ "{ \"" (+ (string nm) "\","))
                 j (idxof nifh pat 0)
    (? (j < 0) ()
       (: k (idxof nifh "nifs + " j)
          (? (k < 0) ()
             (: r (nrow (s2n (snip nifh (k + 7) (k + 17))))
                (? (= "" r) () (name r)))))))

   ; --- the composer ----------------------------------------------------------
   ; per row: (stmt . push-delta) or () = decline. a push is Sp -= 1.
   (num v) (show v)
   (ld i)  ("  Sp[-1] = Sp[" (num i) "]; Sp -= 1;\n")
   (ldq w) ("  Sp[-1] = (intptr_t) " (num w) "; Sp -= 1;\n")    ; a charm operand: its tagged word, 2v+1
   (cw v) ((2 * v) + 1)
   (ok2 os) (&& (two? os) (charm? (cap os)))
   (okk os) (&& ((tally os) = 2) (&& (charm? (cap os)) (charm? (caup os))))
   (say1 r) (: nm (cap r) os (cup r)
    (? (id? nm 'lvm_arg)  (? (ok2 os) ((ld (cap os)) >< 1) ())
       (id? nm 'lvm_arg0) ((ld 0) >< 1)  (id? nm 'lvm_arg1) ((ld 1) >< 1)
       (id? nm 'lvm_arg2) ((ld 2) >< 1)  (id? nm 'lvm_arg3) ((ld 3) >< 1)
       (id? nm 'lvm_quote) (? (ok2 os) ((ldq (cw (cap os))) >< 1) ())
       (id? nm 'lvm_quo0) ((ldq 1) >< 1)   (id? nm 'lvm_quo1) ((ldq 3) >< 1)
       (id? nm 'lvm_quo2) ((ldq 5) >< 1)   (id? nm 'lvm_quo3) ((ldq 7) >< 1)
       (id? nm 'lvm_quom1) ((ldq -1) >< 1) (id? nm 'lvm_quom2) ((ldq -3) >< 1)
       (id? nm 'lvm_aa) (? (okk os) (((ld (cap os)) (ld (caup os))) >< 2) ())
       (id? nm 'lvm_qq) (? (okk os) (((ldq (cw (cap os))) (ldq (cw (caup os)))) >< 2) ())
       (id? nm 'lvm_aq) (? (okk os) (((ld (cap os)) (ldq (cw (caup os)))) >< 2) ())
       (id? nm 'lvm_qa) (? (okk os) (((ldq (cw (cap os))) (ld (caup os))) >< 2) ())
       (: cnm (? (= "lvm_" (snip (string nm) 0 4)) nm (lvmof nm))
          (? (nil? cnm) ()
             (: e (findop cnm)
                (? (nil? e) ()
                   (: kind (caup e) x1 (caup (cup e))
                      (? (id? kind 'fld)
                          (? (ok2 os)
                             ((("  { intptr_t v = Sp[" (num (cap os)) "]; Sp[-1] = (") (x1 "); Sp -= 1; }\n")) >< 1)
                             ())
                         (id? kind 'e1)
                          ((("  Sp[0] = (" x1 ");\n")) >< 0)
                         (id? kind 'opn)
                          (: n (s2n x1) x2 (caup (cuup e))
                             (? (n = 1) ((("  Sp[0] = (" x2 ");\n")) >< 0)
                                ((("  { intptr_t _v = (" x2 "); Sp += ") ((num (n - 1)) "; Sp[0] = _v; }\n"))
                                 >< (0 - (n - 1)))))
                         ()))))))))
   ; walk the rows to (stmts peak net), or () with the first uncovered nom said
   (build rs stmts pk net) (?
    (atom? rs) ()                                            ; ran out with no ret -> decline
    (: r (cap rs) nm (cap r)
     (? (id? nm 'lvm_ret) (L stmts pk net)
        (: p (say1 r)
           (? (nil? p) (: _ (say err ("compose: uncovered op " (show nm) "\n")) ())
              (: net2 (net + (cup p))
                 (build (cup rs) (stmts (cap p)) (? (pk < net2) net2 pk) net2)))))))
   (emit nm f) (: rows (dis f)
    (? (atom? rows) (: _ (say err ("compose: " (string nm) ": dis answers nothing\n")) 0)
       (id? (caap rows) 'lvm_cur) (: _ (say err ("compose: " (string nm) ": multi-arg not composed yet\n")) 0)
       (: b (build rows "" 0 0)
          (? (nil? b) (: _ (say err ("compose: " (string nm) " declined\n")) 0)
             (: stmts (cap b) pk (caup b) net (caup (cup b))
                p (odir ("/comp-" ((string nm) ".c")))
                o (open p "w")
                _ (? o 0 (die ("cannot write " p)))
                _ (say o ("// composed by bench/vmsplice/compose.l -- " (string nm) "; do not edit\n"))
                _ (say o "#include \"love.h\"\n")
                _ (say o "struct ai *jitbody(struct ai *restrict g, union u *Ip, ai_word *Hp, ai_word *restrict Sp) {\n")
                _ (? (0 < pk) (say o (("  if (Sp < Hp + " (num pk) " + ai_avail_floor)")
                                      " { Ip = cell(Ip[1].x); ai_musttail return Continue(); }\n")) 0)
                _ (say o stmts)
                _ (? (0 < net) (say o (("  Sp[" (num net) "] = Sp[0]; Sp += ") ((num net) ";\n"))) 0)
                _ (say o "  Ip += 2;\n  ai_musttail return Continue(); }\n")
                _ (close o)
                _ (say out ("compose: " (string nm) " -> " p "\n"))
                1)))))
   n (foldl (\ a s (a + (emit (cap s) (cup s)))) 0 samples)
   _ (say out ("compose: " (show n) " of " (show (tally samples)) " samples composed\n"))
   (quit (? (0 < n) 0 1)))
