; infix -- an entry at two or more in the `dyadics` table. in operand position with a
; left operand available, the sigil captures it and collects ONE more datum,
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
 ; grip (10, the loosest declared) still decides what the condition is
 ('a = (1 ? 'a)) (() = (0 ? 'a))
 ('big = ((1 < 2) ? 'big))
 ('big = (1 < 2 ? 'big))                        ; (1 < 2) binds first at grip 40 -> (? (< 1 2) 'big)
 ; an else arm is the PREFIX cond -- which is n-ary, where the infix form takes one arm
 ('small = (? (2 < 1) 'big 'small)) ('c = (? 0 'a 0 'b 'c))

 ; application binds tighter than infix: a run on either side of an operator is
 ; ONE operand (juxtaposition first), so parenthesize to seat a folded infix form
 ; as a list element. (1 + 2 3) is (+ 1 (2 3)), NOT ((+ 1 2) 3).
 ('(3 3) = `((1 + 2) 3))                        ; two elements: (+ 1 2) and 3
 ('(3 9) = `((1 + 2) (4 + 5)))
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
(: >< (\ a b (L a b))                            ; a fresh punct op, never registered
 (assert
  ('(3 5) = (3 >< 5))                             ; infix: captures the left operand
  ('(7 (3 5)) = (7 >< 3 >< 5))                    ; nests RIGHT-associatively
  ('(9 (3 5)) = `(9 (3 >< 5)))                   ; works inside a list
  (nom? '><) (id? >< (><))                        ; no left operand / escaped: the plain symbol
  ("(>< 3 5)" = (show (opfix '(3 >< 5))))))     ; a folded infix form IS the clause -- splices up, no wrapper (like (1 + 2))

; opfix does NOT rearrange a binding form: a `:` keeps its binder names (a symbol
; OR a define-sugar header) verbatim -- only each pair's value and the trailing
; body factor -- and a `\` keeps its args, factoring only the body. so a punct
; binder survives (define-then-use needs no pin) and the sequence law is untouched.
; (compared through show: opfix threads its spines with the 0-nil terminator, the
; reader with (); show renders both the same reparsable form, so the factor shows.)
(assert
 ((show '(: >< (\ a b (L a b)) (>< 3 5))) = (show (opfix '(: >< (\ a b (L a b)) (3 >< 5)))))  ; explicit binder kept
 ((show '(: (>< a b) (L a b) (>< 3 5))) = (show (opfix '(: (>< a b) (L a b) (3 >< 5)))))      ; sugar header kept
 ((show '(\ a b (+ a b))) = (show (opfix '(\ a b (a + b)))))                                  ; \ args verbatim, body factored
 ((show '(: a 1 b 2 (+ a b))) = (show (opfix '(: a 1 b 2 (a + b)))))                          ; : values/body factored, names kept
 (2 = (: x 1 x (x + 1) x)))                                                                     ; the sequence law, intact
