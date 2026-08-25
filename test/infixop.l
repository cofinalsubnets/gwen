; infix -- an entry in a SPACED lane of the `operators` table (arity 2, or -1 for no
; bound). in operand position with a left operand available, the sigil captures it and collects ONE more datum,
; nesting RIGHT-associatively (a following operator steals the nearest operand).
; with no left operand it reads as the plain symbol, so (+ 1 2) prefix, '+ quote,
; and (+)-as-a-value all keep working. parenthesized infix double-wraps -- (1 + 2)
; factors to ((+ 1 2)) -- and evaluates via (f)==f. the factoring happens at
; COMPILE time (opfix); reading is structural.
; (the table itself is book-private since birth -- test/operator.l has the story;
; every entry's law reads through behavior below.)
(assert
 ; basics, every seeded operator
 (3 = (1 + 2)) (-1 = (1 - 2)) (6 = (2 * 3)) (2.5 = (5 / 2))
 (1 = (3 = 3.0)) (0 = (3 = 4))
 (1 = (1 < 2)) (1 = (1 <= 1)) (1 = (2 > 1)) (1 = (2 >= 2)) (0 = (2 < 1))
 (15 = (8 | 4 | 2 | 1)) (0 = (5 & 2)) (7 = (5 | 2))
 (1 = (5 % 2)) ('% = (caup '(5 % 2)))               ; % is plain infix at two (mod is gone); quotes stay data

 ; right-associativity; parens override
 (7 = (1 + 2 * 3))                              ; (+ 1 (* 2 3))
 (9 = ((1 + 2) * 3))
 (10 = (1 + 2 + 3 + 4))
 (1 = (5 = 2 + 3))                              ; (= 5 (+ 2 3))

 ; ? is DYADIC like every other row -- infix it is the one-armed cond, and its
 ; band (10, the loosest declared) still decides what the condition is
 ('a = (1 ? 'a)) (() = (0 ? 'a))
 ('big = ((1 < 2) ? 'big))
 ('big = (1 < 2 ? 'big))                        ; (1 < 2) binds first -> (? (< 1 2) 'big)
 ; an else arm is the PREFIX cond -- which is n-ary, where the infix form takes one arm
 ('small = (? (2 < 1) 'big 'small)) ('c = (? 0 'a 0 'b 'c))

 ; application binds tighter than infix: a run on either side of an operator is
 ; ONE operand (juxtaposition first), so parenthesize to seat a folded infix form
 ; as a list element. (1 + 2 3) is (+ 1 (2 3)), NOT ((+ 1 2) 3).
 ('(3 3) = [(1 + 2) 3])                         ; two elements: (+ 1 2) and 3
 ('(3 9) = [(1 + 2) (4 + 5)])
 ("(+ 1 (2 3))" = (show (opfix '(1 + 2 3))))  ; 2 3 is one operand, the whole to the right of +
 ("(= (a b) (c d))" = (show (opfix '(a b = c d))))

 ; operators as values and quoted operators are untouched
 (12 = (foldl (+) 0 '(3 4 5)))
 ('(1 2 3) = (sort '(3 1 2)))
 (nom? '+) (7 = ((+) 3 4))

 ; composes with sigils at one ($ folds eagerly -- binds tightest -- then + steals it)
 (197 = ($"ab" + 2))

 ; the curry law: operands missing at the end of a list fold to the partial
 ; application
 (3 = ((1 +) 2)) (id? nil? (!)))

; structural incompleteness still pends: an unclosed list comes back TORN.
; a trailing operator does NOT pend -- "1 +" is two complete datums whose factor
; folds to the partial (+ 1) at compile time, not a reader state.
(assert (id? 'torn (sound '(40 49 32 43))))              ; "(1 +" -> torn
(: r (sound '(49 32 43))                                 ; "1 +"  -> 1, then +
   (assert (1 = <r) ('+ = (cap (sound >r)))))

; a fresh dyadic operator needs no table at all -- define-then-use (the DEFAULT
; INFIX section below); ** here rides the default, right-associatively
(: ** (\ b n (n b))
 (assert (8.0 = (2 ** 3)) (512.0 = (2 ** 3 ** 2))))  ; right-assoc: 2^(3^2)

; DEFAULT INFIX: any all-punct symbol acts dyadic (arity two) with NO table pin --
; the table holds only the EXCEPTIONS -- the other grips and hands.
; a fresh punct symbol, never registered, reads infix all the same.
(: >< (\ a b [a b])                              ; a fresh punct op, never registered
 (assert
  ('(3 5) = (3 >< 5))                             ; infix: captures the left operand
  ('(7 (3 5)) = (7 >< 3 >< 5))                    ; nests RIGHT-associatively
  ('(9 (3 5)) = [9 (3 >< 5)])                    ; works inside a list
  (nom? '><) (id? >< (><))                        ; no left operand / escaped: the plain symbol
  ("(>< 3 5)" = (show (opfix '(3 >< 5))))))     ; a folded infix form IS the clause -- splices up, no wrapper (like (1 + 2))

; opfix does NOT rearrange a binding form: a `:` keeps its binder names (a symbol
; OR a define-sugar header) verbatim -- only each pair's value and the trailing
; body factor -- and a `\` keeps its args, factoring only the body. so a punct
; binder survives (define-then-use needs no pin) and the sequence law is untouched.
; (compared through show: opfix threads its spines with the 0-nil terminator, the
; reader with (); show renders both the same reparsable form, so the factor shows.)
(assert
 ((show '(: >< (\ a b [a b]) (>< 3 5))) = (show (opfix '(: >< (\ a b [a b]) (3 >< 5)))))      ; explicit binder kept
 ((show '(: (>< a b) [a b] (>< 3 5))) = (show (opfix '(: (>< a b) [a b] (3 >< 5)))))          ; sugar header kept
 ((show '(\ a b (+ a b))) = (show (opfix '(\ a b (a + b)))))                                  ; \ args verbatim, body factored
 ((show '(: a 1 b 2 (+ a b))) = (show (opfix '(: a 1 b 2 (a + b)))))                          ; : values/body factored, names kept
 (2 = (: x 1 x (x + 1) x)))                                                                     ; the sequence law, intact

; --- and the two binders read INFIX too. `\` is dyadic and sits with $; `:` is n-ary at the
; loosest band there is, so every operand stays one and a lambda value needs no parens. both
; fold back THROUGH the lowering above -- patterns lower, a nested lambda flattens, the body
; factors -- so an infix spelling is the same FORM as the prefix one, never a lookalike, and
; nothing is added: (a : b) is the plain body-less (: a b). `:=` is an ALIAS row on `:`, the
; same word by another spelling. the one rule the infix spelling owns is that a `:` CHAIN IS
; ONE SCOPE -- the nest op-steal builds is spliced flat, never left standing.
(assert
 ((show '(\ a b c))           = (show (opfix '(a \ b \ c))))             ; the chain flattens: one closure, not two
 ((show '(\ x (+ x 1)))       = (show (opfix '(x \ x + 1))))             ; the body extends right, under every tighter op
 ((show '(\ x (? c a b)))     = (show (opfix '(x \ c ? a b))))           ; ..a clause form included
 ((show '(: a b c))           = (show (opfix '(a : b c))))               ; n-ary: every operand stays one
 ((show '(: a b))             = (show (opfix '(a : b))))                 ; ..and at two it is the body-less form, which PINS
 ((show '(: (f x) (+ x 1)))   = (show (opfix '(f x : x + 1))))           ; define-sugar: the leading SPAN is the binder
 ((show '(: a 1 b 2 (+ a b))) = (show (opfix '(a : 1 b : 2 (a + b)))))   ; the chain is ONE scope -- the prefix form exactly
 ((show '(: a b c d))         = (show (opfix '(a : b c : d))))           ; ..so a chain of bare definitions is one body-less :
 ((show '(: f (\ x (+ x 1)))) = (show (opfix '(f : x \ x + 1))))         ; \ grips tighter than :, so a lambda is one value
 ((show (opfix '(a : b c)))   = (show (opfix '(a := b c))))              ; := names `:` and inherits the whole fold
 ((show (opfix '(a : 1 b : 2 (a + b)))) = (show (opfix '(a : 1 b := 2 (a + b)))))   ; ..either spelling, same chain
 (3 = ((x \ x + 1) 2)) (7 = ((x \ y \ x + y) 3 4))
 (9 = (a : 4 b : 5 (a + b))))                                            ; a body-HAVING : is one scope and pins nothing

; so an infix definition at top level pins, which is what a file of them is for. ⚠ distinct
; names: a body-less `:` leaks to the global book, and the corpus is one scope.
(ifx-sq x := x * x)
(ifx-hyp a b : (ifx-sq a + ifx-sq b))
(ifx-fst [a b] := a)                                    ; a define-sugar param is a pattern here too
(assert (9 = (ifx-sq 3)) (25 = (ifx-hyp 3 4)) (5 = (ifx-fst '(5 7)))
        (7 = (ifx-z : 7)))                              ; ..and the form answers what it bound

; a param that is not a nom is a PATTERN wherever it sits: infix, prefix, or merged up out of
; a nested lambda -- that last one used to carry it up as a param, where it bound nothing.
; ⚠ these read through BEHAVIOUR and not through show: each lowering mints its own fresh nom,
; so two spellings of one form never print the same text however identical they are.
(assert
 (7 = ((\ x [a b] (x + a + b)) 0 '(3 4)))                                ; prefix
 (7 = ((x \ [a b] \ x + a + b) 0 '(3 4)))                                ; ..infix
 (7 = ((\ x (\ [a b] (x + a + b))) 0 '(3 4)))                            ; ..and merged up out of a nested lambda
 (2 = ((\ [a b] (\ a a)) '(1 2) 2)))                                     ; the shadow still refuses to merge
