; auto.l -- the splice JIT on live closures, priced. ONE process: `(jit f)` (lib/splice.l)
; composes the closure's op bodies, compiles them with the mooncc in this image, binds the
; object against this very process and nifs it -- then this times the native against the
; closure it came from. The correctness half is test_splice; this half is the number.
;
;   LOVE_NO_GLAZE=1 love wake out/host/mooncc.image -l bench/vmsplice/samples.l bench/vmsplice/auto.l
;
; ⚠ LOVE_NO_GLAZE or the baseline is not the interpreter: the samples are shapes
; love/glaze/auto.l recognizes, and a glazed twin reads ~9x too fast. The glaze is the
; specializing tier this one sits UNDER, not its competition.
(: _ (use 'splice)
   R 300000
   in '(1 2 3 4 5 6 7 8 9 10)
   (spin g k acc) (? (k = 0) acc (spin g (k - 1) (g in)))
   (bench lbl g) (: t0 (nclock ()) r (spin g R 0) t1 (nclock ())
                    ns ((t1 - t0) / R)
                    _ (say out ("  " + lbl + "  " + show ns + " ns/call\n")) ns)
   (one s) (: nm (cap s) f (cup s) j (jit f)
              (? (nil? j) (say out (string nm + ": declined\n"))
                 (: _ (say out (string nm + ":\n"))
                    a (bench "composed (mooncc, bound, nif'd)" j)
                    b (bench "thread   (the same ops, interp) " f)
                    (say out ("  speedup " + show (b / a) + "x\n")))))
   _ (map one samples)
   _ (flush out)
   (quit 0))
