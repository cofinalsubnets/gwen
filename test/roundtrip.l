; Closed-loop round trip: feed the printer's OUTPUT back through the reader (and
; evaluator), rather than checking print-string and read-literal separately.
;   rd: value -> show (print) -> read         (datum round-trip)
;   rt: value -> show (print) -> read -> eval  (value round-trip)
; Datums (symbols, lists, atoms) print as their literal form, so they re-READ
; equal but would re-EVAL as code -- those use rd. Self-evaluating atoms and the
; value forms (tuple/tray/hash/functions, and the complex form ~(re im))
; reconstruct the value under eval -- those use rt.
(:    ; through rd-test (test/00-init.l), the reader-under-test socket -- doc/io.md
   ; rung 3, so rung 6 points this loop at p1 without touching this file
   (rdr s) (: r (rd-test s) (? (two? r) <r r))
   (rd x) (rdr (show x))
   (rt x) (ev (rdr (show x))))
(assert
 ; --- datum round-trip (print -> read): the printed form re-reads equal ---
 (= 42 (rd 42))
 (= -7 (rd -7))
 (= 'foo (rd 'foo))
 (= "hi\n" (rd "hi\n"))
 (= '(a (b c) d) (rd '(a (b c) d)))
 (= ''x (rd ''x))                                   ; quote sugar 'x survives the loop
 ; --- value round-trip (print -> read -> eval) ---
 ; self-evaluating atoms
 (= 42 (rt 42))
 (= "hi\n" (rt "hi\n"))
 (= 1.5 (rt 1.5))
 ; ⚠ NEGATIVE ZERO, the one float whose round trip `=` CANNOT check: -0.0 = 0.0
 ; is true by IEEE, so a printer that drops the sign passes an equality check
 ; while losing the value -- and this one did, until it read the sign BIT rather
 ; than testing (x < 0), which is false for -0.0. love has a negative zero; the
 ; printer simply could not say it. compare through the RECIPROCAL, where the
 ; sign is the whole answer.
 ("-0.0" = (show -0.0)) ("0.0" = (show 0.0))
 ("0.0" = (show (0 - 0.0)))                          ; +0.0 stays unsigned: 0 - 0 is +0
 ("-ieee-inf" = (show (1.0 / (rd -0.0))))            ; the sign SURVIVED the loop
 ("ieee-inf" = (show (1.0 / (rd 0.0))))
 ; ,-prefixed constructor forms reconstruct an equal value
 (aall (= (tuple 1 2 3) (rt (tuple 1 2 3))))
 (aall (= (star-tray '(2 2) '(1 2 3 4)) (rt (star-tray '(2 2) '(1 2 3 4)))))
 (aall (= (gem-tray '(2) '(1.5 2.5)) (rt (gem-tray '(2) '(1.5 2.5)))))
 (= ~(2 -3) (rt ~(2 -3)))
 ; complex arrays round-trip: rank-1 via @((C…)…)->tuple, rank>=2 via (array…) (ca-* from complex.l)
 (aall (= ca-v (rt ca-v)))
 (aall (= ca-m (rt ca-m)))
 (= 100 (peep (rt (hash 1 100 2 200)) 1 0))
 (= 200 (peep (rt (hash 1 100 2 200)) 2 0))
 ; functions reconstruct a working function
 (= 3 ((rt (+)) 1 2))                                 ; builtin
 (= 5 ((rt (\ x (+ x 1))) 4))                       ; lambda
 (= 7 ((rt (+ 3)) 4))                               ; partial application of a builtin
 (= 3 ((rt ((\ a b (+ a b)) 1)) 2))                 ; partial app of a lambda (no capture)
 ; a closure (captures free var y) round-trips: the captured value is printed as a
 ; leading param, so re-applying the reconstructed base to it rebinds correctly.
 (= 9 (: y 5 ((rt (\ x (+ x y))) 4)))               ; (rt closure) is base needing x; apply 4
 (= 30 (: a 10 b 20 ((rt (\ z (+ z (+ a b)))) 0)))  ; two captured vars
 ; the lambda's source survives the loop (idempotent show)
 (= (show (\ a b (+ a b))) (show (rt (\ a b (+ a b))))))
