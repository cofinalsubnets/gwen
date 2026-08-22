; interp-spec.l -- the interpreter-specialization benchmark: does partial evaluation of an INTERPRETER
; pay off on this VM? (the real Futamura question -- the 1st projection specializes interp to a program.)
;
; ev1 is a tiny expression interpreter over an AST -- (lit n) (var) (add a b) (mul a b) -- that
; recurses on the tree and DISPATCHES on each node's head. the program P is the Horner form of
; 2x^3 + 3x^2 + 5x + 7; the residual a perfect specializer wants is the straight-line
;   (+ (* (+ (* (+ (* 2 x) 3) x) 5) x) 7).
; three loops of identical shape, only the inner expression differs:
;   interp -- ev1 over a DYNAMIC program (carried in via a param) => pure interpretation, no spec
;   pe     -- ev1 over the CONSTANT P inline => the in-compiler unfold (love/ev.l feel/cprop) may specialize
;   hand   -- the straight-line residual => the ideal the specializer is chasing
;
; RESULT (host, 2026-06-22, 1e6 iters):
;   interp ~610 ms | pe ~630 ms | hand ~84 ms
;   interpretation overhead interp/hand ~ 7x  -- THE PRIZE IS REAL AND LARGE
;   in-compiler capture interp/pe ~ 1.0x  -- cprop captures NONE of it (pe ~= interp)
; WHY the in-compiler cprop misses it, and why we stopped chasing it HERE: cprop folds the ARITHMETIC
;   ops (+ - * ...) but to drive recursion THROUGH a program it would also have to fold the structural
;   navigation (cap/cup/caup/cuup, id?) over a static AST and PRUNE the dispatch cond, then UNFOLD the
;   interpreter on each constant sub-AST. the bounded unfold that did the last step proved near-inert
;   on this VM (cheap tail-threaded calls -> the residual's reconstruction cost cancels the saving),
;   so it was dropped (ev.l cprop is now just fold + const-prop, which is why pe == interp here).
; WHERE THE GAP IS CLOSED: the OFFLINE partial evaluator doc/misc/proto/mix.l does exactly this -- static
;   DATA folding (quote + cap/cup/caup/cuup/id?) and a multi-clause cond whose static tests collapse
;   the dispatch -- and specializes ev1 to P down to the exact `hand` residual below, which then runs
;   ~8x faster than interp (the 1st Futamura projection, measured). this benchmark is the yardstick;
;   mix.l is where pe reaches ~hand.
;
; run:  out/host/love doc/misc/proto/interp-spec.l
((\ pd
  (: (ev1 nd x) (? (id? (cap nd) 'lit) (caup nd)
                   (id? (cap nd) 'var) x
                   (id? (cap nd) 'add) (+ (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))
                   (id? (cap nd) 'mul) (* (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))
                   0)
     P '(add (mul (add (mul (add (mul (lit 2) (var)) (lit 3)) (var)) (lit 5)) (var)) (lit 7))
     N 1000000
     (interp z) ((: (lp i acc) (? (< i N) (lp (+ i 1) (+ acc (ev1 pd (% i 13)))) acc)) z 0)   ; dynamic program
     (pe z)     ((: (lp i acc) (? (< i N) (lp (+ i 1) (+ acc (ev1 P  (% i 13)))) acc)) z 0)   ; constant program
     (hand z)   ((: (lp i acc) (? (< i N) (: x (% i 13) (lp (+ i 1) (+ acc (+ (* (+ (* (+ (* 2 x) 3) x) 5) x) 7)))) acc)) z 0)
     ci (interp 0) cp (pe 0) ch (hand 0)
     _ (. (+ (+ "checksums: interp=" (show ci)) (+ (+ " pe=" (show cp)) (+ " hand=" (show ch))))) _ (. "\n")
     _ (. (? (&& (= ci cp) (= ci ch)) "all match\n" "MISMATCH\n"))
     a0 (clock 0) _ (interp 0) a1 (clock 0) _ (pe 0) a2 (clock 0) _ (hand 0) a3 (clock 0)
     ti (- a1 a0) tp (- a2 a1) th (- a3 a2)
     _ (. (+ "interp (dynamic program):     " (+ (show ti) " ms\n")))
     _ (. (+ "pe     (constant P, cprop):   " (+ (show tp) " ms\n")))
     _ (. (+ "hand   (ideal residual):      " (+ (show th) " ms\n")))
     _ (. (+ "interpretation overhead (interp/hand): x" (+ (show (// (* 100 ti) (+ 1 th))) "/100\n")))
     (. (+ "in-compiler capture      (interp/pe):   x" (+ (show (// (* 100 ti) (+ 1 tp))) "/100\n")))))
 '(add (mul (add (mul (add (mul (lit 2) (var)) (lit 3)) (var)) (lit 5)) (var)) (lit 7)))
