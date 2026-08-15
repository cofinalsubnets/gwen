; crew/kore/sed.l -- sed-lite over the re.l engine: the stream editor's useful
; core. a toolbox file, no seat -- kore is its door; loads after re.l in the cat.
;
;   sed [-n] [-E] [-i] [-e SCRIPT].. SCRIPT [FILE..]
;
; the dialect: commands separated by ; or newlines, each [ADDR[,ADDR]] VERB.
; addresses: a line number, $ (the last line), /re/ (re.l's BRE, or its ERE under
; -E; \/ for a literal /); a range ADDR,ADDR opens at the first, closes at the
; first LATER line matching the second (a numeric end at-or-before the start
; keeps just the one line, like GNU). verbs: p (print), d (delete), q (quit
; after this line's print; one address only), and s/RE/REPL/[g][p] with any
; delimiter after the s. the replacement knows & (the whole match), \1..\9
; (group spans off refind, a repeated group reading as its last iteration),
; and \x for a literal x. -n holds the auto-print; p and the s p-flag still
; speak. -e stacks scripts in order, joined by newlines, and once any -e is
; given every positional word is a FILE.
;
; ⚠ -i CHANGES THE STREAM MODEL and that is GNU's doing, not a shortcut: without
; it the files are ONE stream (line numbers run through, $ is the last line of
; the whole thing); with it each file is its own stream, edited and written back,
; so numbers restart and $ is per-file. the two walks share `srun`, which takes
; its output PORT rather than saying into `out` -- that parameter is the whole of
; how -i captures. no backup suffix: GNU takes one only glued (-i.bak) and
; nothing in this tree asks for it.
;
; out of dialect (documented, unsmoked): GNU's empty-s-pattern reuse (an empty
; pattern here just matches the empty text), \n in replacements, hold space.
;
; the pure floor (lawed in crew/kore/law.l): (sparse sc ere?) -> (1 cmds) | (),
; and (usub nodes repl ln g?) -> (changed text), the one substitution walk.
(:
   ; -- the substitution: replace matches of nodes in ln by repl (g? = all) --
   (usub nodes repl ln g?)
    (: n (tally ln)
       ; the replacement text for one match at (ms me) with group spans sp
       (expand ms me sp)
        (: rn (tally repl)
           (xgo i acc)
            (? (rn <= i) acc
               (: c (repl i)
                  (? (c = 38) (xgo (i + 1) (acc + snip ln ms me))
                     ((c = 92) && ((i + 1) < rn))
                      (: d (repl (i + 1))
                         (? ((49 <= d) && (d <= 57))
                             (: e (assoc (d - 48) sp)
                                (xgo (i + 2)
                                     (? (two? e) (acc + snip ln <>e <>>e) acc)))
                            (xgo (i + 2) (acc + string d))))
                     (xgo (i + 1) (acc + string c)))))
           (xgo 0 ""))
       ; ate marks "a match ended exactly here": an empty match at that point is
       ; DISCARDED (the POSIX rule -- s/x*/-/g on "xbz" is "-b-z-", one dash)
       (walk i acc ate hit)
        (refind nodes ln i @ [_ ms me sp]
          (? (&& ate (me = ms) (ms = i))
             (? (i < n) (walk (i + 1) (acc + snip ln i (i + 1)) () hit)
                [hit acc])
             (: acc2 (acc + snip ln i ms + expand ms me sp)
                (? !g? [1 (acc2 + snip ln me n)]
                   ; g at an empty match: keep one charm and step past it
                   (me = ms) (? (me < n)
                                (walk (me + 1) (acc2 + snip ln me (me + 1)) () 1)
                                [1 acc2])
                   (walk me acc2 1 1))))
          [hit (acc + snip ln i n)])
       (walk 0 "" () ()))

   ; -- the script parser: text -> (1 cmds) | (). a command is
   ; (a1 a2 verb payload); an address is () | ('ln N) | ('last) | ('re nodes);
   ; an s payload is (nodes repl g? p?). ere? picks the regex dialect --
   (sparse sc ere?)
    (: n (tally sc)
       (rx p) ((? ere? reere rebre) p)
       (sep? c) ((c = 32) || (c = 9) || (c = 10) || (c = 59))
       (skip i) (? ((i < n) && sep? (sc i)) (skip (i + 1)) i)
       (spc i) (? ((i < n) && ((sc i = 32) || (sc i = 9))) (spc (i + 1)) i)
       (dig? c) ((48 <= c) && (c <= 57))
       (num i v) (? ((i < n) && dig? (sc i)) (num (i + 1) ((* 10 v) + (sc i - 48))) [v i])
       ; to the unescaped delimiter d: \d -> d, all else verbatim -> (text j) | ()
       (until i d acc)
        (? (n <= i) ()
           (: c (sc i)
              (? (c = d) [acc (i + 1)]
                 (&& (c = 92) ((i + 1) < n) (sc (i + 1) = d))
                  (until (i + 2) d (acc + string d))
                 ((c = 92) && ((i + 1) < n))
                  (until (i + 2) d (acc + string 92 + string (sc (i + 1))))
                 (until (i + 1) d (acc + string c)))))
       ; one address or none: (addr j) | () malformed
       (raddr i)
        (? (n <= i) [() i]
           (: c (sc i)
              (? (dig? c) (num i 0 @ [v j] [['ln v] j])
                 (c = 36) ['(last) (i + 1)]
                 (c = 47) (until (i + 1) 47 "" @ [txt j]
                            (rx txt @ [_ nds _] [['re nds] j] ())
                            ())
                 [() i])))
       (sepend? j) ((n <= j) || sep? (sc j))
       ; the s command's tail past the verb: delimiter, regex, replacement,
       ; flags -> (payload j) | ()
       (rsub j3)
        (? (n <= (j3 + 1)) ()
           (: dl (sc (j3 + 1))
              (? ((dl = 92) || (dl = 10)) ()
                 (until (j3 + 2) dl "" @ [re j4]
                   (rx re @ [_ nds _]
                     (until j4 dl "" @ [rp j5]
                       (: (fgo k g? p?)
                           (? (sepend? k) [k g? p?]
                              (sc k = 103) (fgo (k + 1) 1 p?)
                              (sc k = 112) (fgo (k + 1) g? 1)
                              ())
                          (fgo j5 () () @ [j6 g? p?] [[nds rp g? p?] j6] ()))
                       ())
                     ())
                   ()))))
       (rcmd i acc)
        (: j (skip i)
           (? (n <= j) [1 (rev acc)]
              (raddr j @ [a1 j2]
                (: comma ((j2 < n) && (sc j2 = 44))
                   (? (comma && !(two? a1)) ()               ; a , with no left leg
                      (: r2 (? comma (raddr (j2 + 1)) [() j2])
                         (? (!(two? r2) || (comma && !(two? <r2))) ()
                            (: a2 <r2 j3 (spc <>r2)
                               (? (n <= j3) ()
                                  (: v (sc j3)
                                     plain (\ verb (? (sepend? (j3 + 1))
                                                      (rcmd (j3 + 1) ([a1 a2 verb ()] >< acc))
                                                      ()))
                                     (? (v = 112) (plain 'p)
                                        (v = 100) (plain 'd)
                                        (v = 113) (? (two? a2) () (plain 'q))
                                        (v = 115) (rsub j3 @ [pay j4]
                                                    (rcmd j4 ([a1 a2 's pay] >< acc))
                                                    ())
                                        ()))))))))
                ())))
       (rcmd 0 ()))

   ; -- the cycle. op is the output PORT, which is how -i captures a file's own
   ; edit instead of saying it into out --
   ; (srun cmds lines n? op end?) -> the exit remembered, 0. end? is whether the SOURCE
   ; ended in a newline; a 0 there is data, and the last line must go out without one.
   (srun cmds all n? op end?)
    (: nl (tally all)
       t (tablet 0)                                  ; the range toggles, keyed by command index
       (amatch a no ln)
        (? !(two? a) 1
           (<a = 'ln) (no = <>a)
           (<a = 'last) (no = nl)
           (rehas <>a ln))
       ; is command c (at index idx) selected on line no with pattern space ln?
       ; a range opens on its first address and closes on a LATER line's second
       (sel? c idx no ln)
        (: a1 <c a2 <>c
           (? !(two? a2) (amatch a1 no ln)
              (peep t idx 0)
               (: fin (? (<a2 = 'ln) (<>a2 <= no) (amatch a2 no ln))
                  _ (? fin (pin t idx 0) ())
                  1)
              (amatch a1 no ln)
               (: one (? (<a2 = 'ln) (<>a2 <= no) ())
                  _ (? one () (pin t idx 1))
                  1)
              ()))
       ; one line through the commands -> (kind ps): 0 flows, 1 deleted, 2 quits
       (rline cs idx ps no q)
        (cs @ (c >< more)
           (? !(sel? c idx no ps) (rline more (idx + 1) ps no q)
              (: v <>>c
                 (? (v = 'd) [1 ps]
                    (v = 'q) [2 ps]
                    (v = 'p) (: _ (say q ps) _ (put q 10)
                                (rline more (idx + 1) ps no q))
                    (: pay <>>>c
                       pr >>pay
                       r (usub <pay <>pay ps <pr)
                       ps2 <>r
                       _ (? (<r && <>pr) (: _ (say q ps2) (put q 10)) ())
                       (rline more (idx + 1) ps2 no q)))))
           [0 ps])
       ; ⚠ THE LAST LINE OF AN UNTERMINATED SOURCE RIDES A JUG. the newline goes missing
       ; after the LAST WRITE and not after every one -- `sed -n 'p;p'` keeps the inner
       ; one -- so the line is rendered whole and chomped once, whatever the commands did.
       (mline ls no)
        (ls @ (x >< more)
           (: hold (? ((two? more) || end?) () (jug 0))
              q (? hold hold op)
              (rline cmds 0 x no q @ [kind ps]
                (: _ (? (kind = 1) () (? n? () (: _ (say q ps) (put q 10))))
                   _ (? hold (say op (chomp (slurp hold))) 0)
                   (? (kind = 2) 0 (mline more (no + 1))))
                0))
           0)
       (mline all 1))

   ; -- sed: the flags, then one of the two stream models --
   (sed-main as)
    (: t (tablet 0)
       o (uopts as t '(101) ())                      ; -e SCRIPT, stacking
       _ (? (two? o) () (udie 1 "sed: bad option"))
       eps <>o rest <>>o
       _ (? (two? eps || two? rest) ()
            (udie 1 "usage: sed [-n] [-E] [-i] [-e SCRIPT].. SCRIPT [FILE..]"))
       n? (peep t 110 0) i? (peep t 105 0)
       sc (? (two? eps) (ujoinc 10 eps) <rest)
       fs (? (two? eps) rest >rest)
       pc (sparse sc (peep t 69 0))
       _ (? (two? pc) () (udie 1 ("sed: invalid script: " + sc)))
       cmds <>pc
       ; -i: each file its own stream, edited and written back. a file that will
       ; not open complains and the rest still flow, exactly as the joined lane
       (inplace a bad)
        (a @ (f >< more)
           (uread f @ [_ txt]
             (: j (jug 0)
                _ (srun cmds (lines txt) n? j (nl-end? txt))
                _ (? (uwrite f (slurp j)) ()
                     (: _ (say err ("sed: couldn't open " + f)) (put err 10)))
                (inplace more bad))
             (: _ (say err ("sed: can't read " + f + ": No such file or directory"))
                _ (put err 10)
                (inplace more 1)))
           (: _ (flush err) (? bad 2 0)))
       ; the joined lane: every file one stream, so the numbers run through
       (rdall a acc bad)
        (a @ (f >< more)
           (: src (? (f = "-") [1 (slurp in)] (uread f))
              (src @ [_ txt] (rdall more (txt >< acc) bad)
                (: _ (say err ("sed: can't read " + f + ": No such file or directory"))
                   _ (put err 10)
                   (rdall more acc 1))))
           [(rev acc) bad])
       (? (i? && two? fs) (inplace fs ())
          (: rd (? (two? fs) (rdall fs () ()) [[(slurp in)] ()])
             _ (srun cmds (catmap lines <rd) n? out
                     (? (two? <rd) (nl-end? (last <rd)) 1))
             _ (flush out) _ (flush err)
             (? <>rd 2 0)))))
