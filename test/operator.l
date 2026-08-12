; the operator table -- ONE table, `dyadics`: sigil symbol -> grip[ hand[ nary]] (the
; symbol names itself) | (name grip[ hand[ nary]]) (an alias). a row is the whole fold,
; arity included: 0 dyadic, 1 N-ARY (the clause forms ? and @, whose arms stay operands
; rather than spanning into one). the reader is structural
; (a sigil -- a maximal all-punct token -- reads as one plain symbol); code looks
; sigils up in the table at COMPILE time (the opfix pass). the table is opfix's
; OWN -- book-private since birth (the egg mops it beside monadics/pureset: a live
; pin into a compile table would poison every later compile) -- so its laws read
; through BEHAVIOR: ? at its grip lives in test/infixop.l, its n-ary fold in
; test/precedence.l, the nom-led alias
; shape and the grip refusals in test/precedence.l's fixity door, greedy longest-prefix
; factorization (the GLUED lane, against monadics) in the valence laws, and a FRESH
; punct operator needs no entry at all (default dyadic; test/infixop.l's DEFAULT
; INFIX section).
(hear welp)                                    ; welp answers the misses quietly
(assert (? born !dyadics 1) (? born !monadics 1) (? born !pureset 1))
(hear 0)

; reading is structural: a sigil from a port is just the symbol, no table consulted
(assert ('^ = (cap (sound '(94)))) ('+ = (cap (sound '(43)))))

; ^^ has no entry, and a spaced sigil never splits, so in head position it stays
; the plain symbol -- and ^ is the xor nif all along
(assert (nom? '^^) ("(^^ 5)" = (show (opfix '(^^ 5))))
        (-2 = (^ 1 -1)) (6 = (^ 5 3)))

; --- the comma: the clause layer, loosest of all -----------------------------
; a lone one-char datum off the reader (never fuses, either side); within any
; list the datums split into clauses at commas, each clause factored on its
; own, the whole the `,` sequencing macro -- (a, b c) -> (, (a) (b c)) ->
; (: _ a (b c)): effects in source order, the last clause answers. empty
; clauses drop: a doubled interior comma composes to one, a leading or
; trailing comma to nothing. head position is the macro itself.
(assert
 (2 = (1 , 2))                                  ; spaced
 (3 = (1, 2, 3))                                ; glued left (C typography)
 (3 = (1,2,3))                                  ; fully glued
 (3 = (1 ,2 ,3))                                ; glued right (no mono lane)
 (7 = ((+ 1 2), (+ 3 4)))                       ; parenthesized phrases
 (7 = (+ 1 2, + 3 4))                           ; bare phrases: comma outranks infix
 (16 = (2 3 + 1, 2 4))                          ; ... and application: (2 (+ 3 1)), then (2 4)
 (3 = (1 ,, 3))                                 ; interior empties compose to one comma
 (5 = (5 ,))                                    ; a trailing comma is ignored
 (5 = (, 5))                                    ; leading comma: the sequencing macro,
 (2 = (, 1 2))                                  ; ... explicit head-position form
 ("(, (1) (2 3))" = (show (opfix '(1 , 2 3))))) ; the emission, reparsable
; effects run in source order and the last clause is the value
(: cq (tablet 0)
   (cnote v) (: _ (pin cq (tally cq) v) v)
   (: r ((cnote 'a), (cnote 'b), (cnote 'c))
      (assert ('c = r) ('a = (cq 0)) ('b = (cq 1)) ('c = (cq 2)) (3 = $cq))))
; quote interiors are data: the comma stays a plain symbol, and reading is
; structural -- a lone comma off a port is just the datum
(assert (3 = (tally '(1 , 2))) (', = (caup '(1 , 2)))
        (nom? ',) ("," = (string ',)) (', = (cap (sound '(44)))))
; clauses nest per list, and chain their effects in source order
(assert (6 = (: t #(0 1) _ ((pin t 0 2), (pin t 1 3)) ((t 0) * (t 1)))))
