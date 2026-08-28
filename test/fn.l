; functions print as value forms that reconstruct under eval: builtins -> the bare
; name; lambdas -> (\ …) source with \-bound vars as interned shortlex letters by de Bruijn
; LEVEL -- a b .. z aa ab .. -- skipping any letter spelled free in the source, since a name
; kept as written is one a rename would capture. Interned names -> the print form round-trips
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
  (= "(\\ a a)" (show (\ x x)))
  (= "(\\ a b (+ a b))" (show (\ a b (+ a b))))      ; already canonical: prints as written
  (= (show (\ x x)) (show (\ y y)))               ; a-equivalent -> identical print
  (= "(\\ a (\\ b b))" (show (\ x (\ x x))))         ; shadow: inner binder = b
  (= "(\\ a 'y)" (show (\ x 'y)))                    ; quoted datum not renamed
  ; a letter spelled free in the source is skipped, or the rename would capture it
  (= "(\\ a b c d f (+ e a))" (show (\ p q r s t (+ e p))))     ; e free -> binder 4 is f
  (= "(\\ a b c d f g h j k (+ e (+ i k)))"
     (show (\ p q r s t u v w x (+ e (+ i x)))))                ; e and i both skipped
  (= "(\\ a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab (+ aa ab))"
     (show (\ q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20
              q21 q22 q23 q24 q25 q26 q27 q28 (+ q27 q28))))   ; past z, shortlex rolls to aa
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
  (= 9 (peep {((\ a b (+ a b)) 5) 9} (\ b (+ 5 b)) 0))   ; HASH consistency: a map keyed on the partial-app, found by the residual
  ; worklist safety: a closure with a CHAIN capture nested in a compared structure -- equal closures
  ; but a differing tail must stay NOT equal (the bridge's re-entrant eqv must not clobber pending pairs)
  (! (= (link ((\ v x (+ x v)) '(1 2)) (link 5 6)) (link ((\ v x (+ x v)) '(1 2)) (link 5 7))))
  (= (link ((\ v x (+ x v)) '(1 2)) (link 5 6)) (link ((\ v x (+ x v)) '(1 2)) (link 5 6)))
  ; the same safety on the NO-CAPTURE path: salpha falls back to eqv for a quote body,
  ; and that fallback scratches above the pending pairs, so the differing tail still tells
  (! (= (link (\ a (\ 5)) (link 5 6)) (link (\ b (\ 5)) (link 5 7))))
  (= (link (\ a (\ 5)) (link 5 6)) (link (\ b (\ 5)) (link 5 6)))
  (string? (show map))
  (< 10 (tally (show map)))))
