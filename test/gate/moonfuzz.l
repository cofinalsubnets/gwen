; test/gate/moonfuzz.l -- moon's REFUSAL surface, fuzzed. run inside a woken mooncc
; image, where the front end lives; clay arrives by (use 'clay), a module, so its
; helper names never touch the compiler's book.
;
; the corpus (test/cc/) is C that COMPILES. this takes each file and BREAKS it --
; truncate, delete a byte, delete a run, flip a byte, insert punctuation, swap two,
; drop a line, double a line -- from a fixed seed, and asks what moon does with the
; wreckage. eight mutations over 111 files is a supply of ill-formed C nobody has to
; write by hand, and every one of them is C that was nearly valid, which is the shape
; a real mistake actually has.
;
; THREE PROPERTIES, and they are different in kind:
;
;   P1 the front end SURVIVES. no mutation may scare (an internal raise reaching the
;      installed help) and none may hang. this is the hard red -- a compiler that
;      falls over on bad input has no diagnostics to speak of.
;   P2 a refusal NAMES ITS CAUSE. every stage that says no must answer a gripe
;, not a bare bad. this is NOT a red -- lex and cpp
;      still answer bare (rungs left undone), so it is a CENSUS, printed by stage
;      every run. the number falling IS the rung landing.
;   P3 clay and the parser agree, on trees the corpus never had. for every mutant
;      that still parses, (cparse (clay-show ast)) == ast -- test_clay's G1 law, but
;      over mutated input, so the shower meets shapes 111 hand-written files cannot
;      reach. a red.
;
; ⚠ A VACUOUS PASS IS A FAIL, the clay gate's lesson: a run where nothing parsed and
; nothing refused proved nothing. the floors below are explicit.
(:
   _ (use 'clay)
   cshow (from 'clay 'clay-show)
   cok? (from 'clay 'clay-ok?)
   nl (string 10)
   (el l i) (? (i < 1) <l (el >l (i - 1)))
   (say1 s) (puts (s + nl))
   (dotc? s) (: n (tally s) (&& (2 < n) (&& ((s (n - 2)) = 46) ((s (n - 1)) = 99))))
   (mn a b) (? (a < b) a b)

   ; --- the scare trap. help resumes with its value (test/help.l), so a caught
   ; scare does not unwind -- it flips the flag and the stage carries on with the
   ; zero point. that is fine: the flag is read after the case either way, and a
   ; stage that then loops is P1's other face, a hang, which the recipe's timeout
   ; owns because nothing inside the process can.
   scared (tablet 0)
   _ (hear (\ a b (: _ (pin scared 0 1) 0)))
   (unscared _) (pin scared 0 0)
   (scared? _) (peep scared 0 0)

   ; --- the mutations. text in, text out, threading the rng state (test/fuzz.l's
   ; generator shape: st -> (value . st')). eight of them, chosen because each
   ; breaks the source at a DIFFERENT stage: a truncation dies in the reader, a
   ; punctuation insert in the parser, a dropped line in the type checker.
   punct "(){}[];,*&+-/%<>=!?:#\\\"'"
   (linelo txt i) (: (go k) (? (k < 1) 0 ((txt (k - 1)) = 10) k (go (k - 1))) (go i))
   (linehi txt i) (: n (tally txt)
                     (go k) (? (n <= k) n ((txt k) = 10) (k + 1) (go (k + 1)))
                     (go i))
   (mut txt st)
    (: n (tally txt)
       r0 (random st 8)      k <r0
       r1 (random >r0 n)     i <r1
       r2 (random >r1 24)    m (<r2 + 1)
       r3 (random >r2 (tally punct)) c (string (punct <r3))
       st' >r3
       a (snip txt 0 i)
       lo (linelo txt i) hi (linehi txt i)
       out (? (k = 0) a                                              ; truncate
              (k = 1) (a + (snip txt (mn n (i + 1)) n))              ; delete one byte
              (k = 2) (a + (snip txt (mn n (i + m)) n))              ; delete a run
              (k = 3) (a + c + (snip txt (mn n (i + 1)) n))          ; flip a byte
              (k = 4) (a + c + (snip txt i n))                       ; insert punctuation
              (k = 5) (: j (mn (n - 1) (i + 1))                      ; swap two
                         (? (j <= i) txt
                            (snip txt 0 i) + (snip txt j (j + 1)) + (snip txt i (i + 1))
                              + (snip txt (j + 1) n)))
              (k = 6) ((snip txt 0 lo) + (snip txt hi n))            ; drop a line
              ((snip txt 0 lo) + (snip txt lo hi) + (snip txt lo n)))  ; double a line
       (link out st'))

   ; --- the front end off TEXT, every stage's refusal kept apart and its CARRIER
   ; read. cc-parse's udie is deliberately not in the path: a refusal has to come
   ; back as a value here, since the whole point is counting how they arrive.
   ; answers ('bad stage carrier) or ('ok ast).
   ; the include hook is the DRIVER'S, rooted at test/cc/ -- a stub that resolves
   ; nothing would have counted the corpus's own `#include <stdarg.h>` as a cpp
   ; refusal in every mutant of every file that has one, and the census would have
   ; been measuring the harness.
   (front txt)
    (: lx (clex txt)
       (? (! (okv? lx)) `('bad 'lex lx)
          (: pp (cpp <>lx (\ n s (incload () () "test/cc/" n s)) "<fuzz>")
             (? (! (okv? pp)) `('bad 'cpp pp)
                (: ps (cparse-t <>pp 'x64)
                   (? (! (okv? ps)) `('bad 'parse ps)
                      (: g (cgen-obj <>ps (cauup ps) (caup (cuup ps)) (cauup (cuup ps)) () 'x64)
                         (? (! (okv? g)) `('bad 'gen g) `('ok <>ps)))))))))

   ; --- one mutant -> a verdict symbol, plus the stage it died in when it died.
   ; ('scare ..) beats everything: a case that scared is not a refusal, it is a fall.
   (one txt)
    (: _ (unscared ())
       r (front txt)
       sc (scared? ())
       ; ⚠ the scare's stage is 'late when the front end still ANSWERED -- (el r 1)
       ; there is the whole AST, and it is a census KEY. a red run has to stay
       ; readable, so a tree never becomes one.
       (? sc `('scare (? (id? <r 'bad) (el r 1) 'late))
          (id? <r 'bad)
          (: st (el r 1) cy (el r 2)
             (? (gripe? cy) `('named st) `('bare st)))
          ; it parsed. the clay leg -- G1 over a tree the corpus never had.
          (: a (el r 1)
             v (cok? a)
             (? (! (id? v 1)) `('badshape 'clay)
                (: t (cshow a)
                   (? (gripe? t) `('inexpr 'clay)
                      (: b (front t)
                         (? (! (id? <b 'ok)) `('reparse 'clay)
                            ((el b 1) = a) `('ok 'clay)
                            `('diff 'clay)))))))))

   files (filter dotc? (readdir "test/cc"))
   ; per file: MUTS mutants, from a seed keyed by the file's own index, so a red
   ; names a (file, case) pair that reproduces on its own.
   muts 8

   ; the census: one tablet keyed by "verdict/stage", so a new verdict cannot fall
   ; through a chain of ids into silence.
   cen (tablet 0)
   (ckey v s) ((string v) + "/" + (string s))
   (bump v s) (pin cen (ckey v s) (1 + (peep cen (ckey v s) 0)))
   (cnt v s) (peep cen (ckey v s) 0)
   sins (tablet 0)                       ; the named ones: verdict -> a list of cases
   (sin k f) (pin sins k (link f (peep sins k ())))

   (runfile idx f)
    (: r (uread ("test/cc/" + f))
       (? (! (two? r)) 0
          (: src <>r
             (go j st) (? (muts <= j) 0
                (: mr (mut src st)
                   v (one <mr)
                   _ (bump <v (el v 1))
                   _ (? (member? <v '(scare diff badshape reparse))
                        (sin <v (f + ":" + (show j))) 0)
                   (go (j + 1) >mr)))
             (go 0 (wheel (1 + idx))))))

   (walk l i) (? (! (two? l)) 0 (: _ (runfile i <l) (walk >l (i + 1))))
   _ (walk files 0)

   ; --- the report. the CENSUS first (P2, a measurement), then the reds (P1, P3).
   stages '(lex cpp parse gen)
   nbare (foldl (\ a s (a + (cnt 'bare s))) 0 stages)
   nnamed (foldl (\ a s (a + (cnt 'named s))) 0 stages)
   nok (cnt 'ok 'clay) nix (cnt 'inexpr 'clay)
   nscare ((foldl (\ a s (a + (cnt 'scare s))) 0 stages)
           + (cnt 'scare 'clay) + (cnt 'scare 'late))
   ndiff (cnt 'diff 'clay) nrep (cnt 'reparse 'clay) nbs (cnt 'badshape 'clay)
   nfile (tally files)
   total (nfile * muts)

   _ (each stages (\ s (: b (cnt 'bare s) g (cnt 'named s)
        (? (b + g) (say1 ("moonfuzz: " + (string s) + " refused " + (show (b + g))
                          + " -- " + (show g) + " named, " + (show b) + " bare")) 0))))
   _ (say1 ("moonfuzz: " + (show nok) + " still parsed and round-tripped, "
            + (show nix) + " inexpressible"))
   _ (say1 ("moonfuzz: " + (show (nbare + nnamed)) + " refusals over " + (show total)
            + " mutants of " + (show nfile) + " files"))

   ; the named reds, by name -- a count alone would let a regression hide in a bucket
   _ (each '(scare diff badshape reparse)
        (\ v (each (rev (peep sins v ())) (\ f (say1 ("moonfuzz: " + (string v) + " " + f))))))

   ; ⚠ the floors. a run where nothing refused, or nothing survived to reach clay,
   ; measured nothing and must not print pass.
   _ (? (|| ((nbare + nnamed) < 100) (nok < 100))
        (: _ (say1 ("moonfuzz: FAIL -- vacuous run (" + (show (nbare + nnamed))
                    + " refusals, " + (show nok) + " round-trips): the front end is not in scope"))
           (quit 1)) 0)
   (? (! (nscare + ndiff + nbs + nrep))
      (: _ (say1 "moonfuzz: pass") (quit 0))
      (: _ (say1 ("moonfuzz: FAIL -- " + (show nscare) + " scared, " + (show ndiff)
                  + " clay diff, " + (show nbs) + " malformed, " + (show nrep) + " unreadable"))
         (quit 1))))
