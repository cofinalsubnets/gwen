; mint serials -- every mint draws from ONE stream (pids share it), and the serial is what
; makes the total order TOTAL over points: symbols order by name, then interned-first, then
; serial (creation order); noms inherit the chain order through chain lex + the mint tiebreak.
; the serial is also the mint's hash, so same-name noms key a map apart. (the +/* lanes a
; symbol inherits are spec.l's; the fine dispatch cells are test/matrix.l's.)
(: a (nom 'q) b (nom 'q) c (nom 'q)
   n1 (mint 0) n2 (mint 0)
 (assert
  (a < b) (b < c) (a < c) !(b < a) !(c < b)     ; strict, transitive, by creation
  !(a = b) !(b = c)                             ; the mint inside keeps = identity-sharp
  (n1 < n2) !(n2 < n1) !(n1 = n2)               ; mints order by creation too
  (() < n1) !(n1 < ())                          ; the zero point sits first among the nameless
  (n1 < 'q)                                     ; nameless below named (lex on "")
  ('(1 2 3) = (map cup (sort `((link c 3) (link a 1) (link b 2)))))  ; sort sees the serials
  ; GC stability: the order survives heap churn (serials ride the copy)
  (: pre `((a < b) (n1 < n2) (() < n1))
     _ ((: (churn k) (? (< 0 k) (: _ (map (\ i (string (mint 0))) '(1 2 3 4 5 6 7 8)) (churn (- k 1))) 0)) 500)
     (pre = `((a < b) (n1 < n2) (() < n1))))
  ; the serial is the mint's hash: same-name noms key a map apart, stably
  (: t (tablet 0) _ (pin t a 'one) _ (pin t b 'two)
     (&& ('one = (t a)) ('two = (t b)) (2 = $t)))))
