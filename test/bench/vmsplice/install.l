; install.l -- install a lifted body through `nif` and price it against the thread it
; replaces. Load the lift's output first:  love -l jitcode.l install.l
;
; ⚠ RUN IT BOTH WAYS. The twin below is an arithmetic closure, which is exactly the
; shape love/glaze/auto.l recognizes, so a default run does not time the interpreter --
; it times the glaze. LOVE_NO_GLAZE=1 is the true interpreter baseline. Both readings
; are wanted: the glaze row is the specializing tier this splice would sit under.
(: nif (from 'glaze 'nif)
   _ (? (lit? nif) 0 (: _ (say err "install: no glaze nif on this build\n") (quit 1)))
   N 64
   ; the twin, built rather than typed: (\ x (x + 1 + 1 ..)), N adds, left-folded.
   ; every partial is x-dependent, so nothing folds into a constant.
   (bump k acc) (? (k = 0) acc (bump (k - 1) `('+ acc 1)))
   src `('\ 'x (bump N 'x))
   twin (ev src)
   f (nif jitcode twin src 1)
   _ (? (lit? f) 0 (: _ (say err "install: nif declined the bytes\n") (quit 1)))
   ; the answer is the point: a native is never wrong, so it agrees with its twin
   ok (: (go xs) (? (atom? xs) 1
                    (? ((f (cap xs)) = (twin (cap xs))) (go (cup xs)) ()))
         (go '(7 0 1 -30 1000)))
   _ (say out ("  agree with twin: " (? ok "yes" "NO -- the lifted body is WRONG") "\n"))
   _ (? ok 0 (quit 1))
   R 300000
   (spin g k acc) (? (k = 0) acc (spin g (k - 1) (g k)))
   (bench nm g) (: t0 (nclock ())
                   r (spin g R 0)
                   t1 (nclock ())
                   ns ((t1 - t0) / R)
                   _ (say out ("  " nm "  " (show ns) " ns/call  " (show (ns / N)) " ns/op  last=" (show r) "\n"))
                   ns)
   a (bench "native (spliced, lifted from the .o)" f)
   b (bench "thread (the same N ops, via ev)    " twin)
   _ (say out ("  speedup " (show (b / a)) "x\n"))
   (quit 0))
