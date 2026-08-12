; check.l -- the in-process end of the pipeline: bind a composed .o against THIS live
; process (bind.l's bindcode), nif it, and differential against the interp twin --
; which IS the same closure (samples.l compiled by ev). a composed body is never
; wrong, only faster. All in ONE process: the JIT binds against its own addresses.
;   LOVE_NO_GLAZE=1 love -l samples.l -l bind.l check.l <obj> <sample-name>
(: nif (from 'glaze 'nif)
   _ (? (lit? nif) 0 (: _ (say err "check: no glaze nif on this build\n") (quit 1)))
   ; cmdline is the whole argv (flags included); our two positional args are the tail
   args (rev cmdline) nm (name (cap args)) obj (caup args)
   f (: (go l) (? (atom? l) () (id? nm (caap l)) (cuap l) (go (cup l))) (go samples))
   _ (? (lit? f) 0 (: _ (say err ("check: no sample " (string nm) "\n")) (quit 1)))
   code (bindcode obj)
   _ (? (hot? code) 0 (: _ (say err "check: bind produced no code\n") (quit 1)))   ; a cask is hot; () is not (never `two?`, a cask is no pair)
   j (nif code f (L '\ 'x 'x) 1)
   _ (? (lit? j) 0 (: _ (say err "check: nif declined the bytes\n") (quit 1)))
   ins (L 7 0 () '(1) '(1 2 3) '(1 2 3 4 5) '(1 2 3 4 5 6 7 8 9 10) '((1 2) (3 4) 5) "text")
   bad (filter (\ x (! (= (j x) (f x)))) ins)
   _ (? (nil? bad) (say out ("check: " (string nm) " agrees with its twin on " (show (tally ins)) " inputs\n"))
        (: _ (say err ("check: " (string nm) " DISAGREES on " (show bad) "\n")) (quit 1)))
   R 300000
   in '(1 2 3 4 5 6 7 8 9 10)
   (spin g k acc) (? (k = 0) acc (spin g (k - 1) (g in)))
   (bench lbl g) (: t0 (nclock ()) r (spin g R 0) t1 (nclock ())
                    ns ((t1 - t0) / R)
                    _ (say out ("  " lbl "  " (show ns) " ns/call\n")) ns)
   a (bench "composed (mooncc, bound, nif'd)" j)
   b (bench "thread   (the same ops, interp) " f)
   _ (say out ("  speedup " (show (b / a)) "x\n"))
   (quit 0))
