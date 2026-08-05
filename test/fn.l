; functions print as value forms that reconstruct under eval: builtins -> the bare
; name; lambdas -> (\ …) source with \-bound vars as interned d<lvl> de Bruijn LEVELS
; (a-canonical: outermost binder = d0). Interned names -> the print form round-trips
; (see roundtrip.l). a-equivalent lambdas print identically. a CAPTURED value renders as a
; leading binder -- but THAT rendering is intensional and free to vary (cprop may inline a
; constant capture), so SHOW of a capture is asserted EXTENSIONALLY: show -> read -> eval rebuilds
; a function of the same behavior (rt below; the dedicated suite is roundtrip.l).
; `=` ITSELF, though, bridges the capture (the beta bridge, love.c clo_eq/nf_hash): a partial-app IS
; the no-capture lambda with its captured args substituted -- a held redex equal to its reduct --
; so a closure is also asserted INTENSIONALLY below ((adder 5) = (\ x (+ x 5))). General beta/eta
; stay unbridged (the line: `=` surfaces the beta ev ALREADY ran, it does not reduce on its own).
(: (rt x) (ev (cap (sound (show x))))
 (assert
  (= "\+" (show (+)))
  (= "link" (show link))
  (= "(\\ d0 d0)" (show (\ x x)))
  (= "(\\ d0 d1 (+ d0 d1))" (show (\ a b (+ a b))))
  (= (show (\ x x)) (show (\ y y)))               ; a-equivalent -> identical print
  (= "(\\ d0 (\\ d1 d1))" (show (\ x (\ x x))))      ; shadow: inner binder = d1
  (= "(\\ d0 'y)" (show (\ x 'y)))                   ; quoted datum not renamed
  (= "(+ 1)" (show (+ 1)))                            ; partial app of a builtin: stable form
  (= 3 ((rt ((\ a b (+ a b)) 1)) 2))                 ; partial-applied lambda round-trips (capture rendering free to vary)
  (= 9 (: y (+ 2 3) ((rt (\ x (+ x y))) 4)))         ; closure with a captured var round-trips to a working fn
  ; --- the beta bridge: a partial-app = its capture-substitution residual (the SAME function) ---
  (= ((\ a b (+ a b)) 1) (\ b (+ 1 b)))              ; held redex = its reduct (a number capture, substituted as its literal)
  (= ((\ a b c (+ a (* b c))) 2 3) (\ c (+ 2 (* 3 c))))  ; multi-capture: two args filled, one binder remains
  (= ((\ s x (+ s x)) "hi") (\ x (+ "hi" x)))        ; a string capture, substituted as its literal
  (= ((\ i (\ x (+ x i))) 5) (\ x (+ x 5)))          ; NESTED-written factory: the beta bridge holds (regression: a flatten that stashed a nested src over a flat capture had broken clo_eq)
  (= (\ i (\ x (+ x i))) (\ i x (+ x i)))            ; nested and multi-param spellings ARE the same function -- opfix (op-flat-ol) flattens both compilers (c0 + feel), so this holds under love0 too
  (= (\ x (\ x x)) (\ x (\ x x)))                    ; SHADOWING stays nested (not flattened to the ambiguous (\ x x x)); still self-equal
  (! (= ((\ a b (+ a b)) 1) (\ b (+ 2 b))))          ; a different capture stays distinct
  (! (= ((\ a b (+ a b)) 1) (\ b (- 1 b))))          ; a different body stays distinct
  (! (= (\ x (link x)) link))                         ; eta still NOT bridged -- the line holds (= surfaces no NEW reduction)
  (= (+ 1) (+ 1)) (! (= (+ 1) (+ 2)))                ; a source-less (builtin) base: the pairwise fallback, intact
  (= 9 (peep #(((\ a b (+ a b)) 5) 9) (\ b (+ 5 b)) 0))  ; HASH consistency: a map keyed on the partial-app, found by the residual
  ; worklist safety: a closure with a CHAIN capture nested in a compared structure -- equal closures
  ; but a differing tail must stay NOT equal (the bridge's re-entrant eqv must not clobber pending pairs)
  (! (= (link ((\ v x (+ x v)) '(1 2)) (link 5 6)) (link ((\ v x (+ x v)) '(1 2)) (link 5 7))))
  (= (link ((\ v x (+ x v)) '(1 2)) (link 5 6)) (link ((\ v x (+ x v)) '(1 2)) (link 5 6)))
  (string? (show map))
  (< 10 (tally (show map)))))
