; peg -- gw.js-style PARSER COMBINATORS. A parser is (\ s y n) over a string s, with a SUCCESS
; continuation y = (\ rest payload) and a FAILURE continuation n = (\ rest); payloads are lists that
; `seq` concatenates. The leaves are `lit` (a literal prefix), `one` (one char by predicate) and `span`
; (a maximal run) -- love has no regex. `parse` requires the whole input consumed; `runp` is the lenient
; twin (keeps the payload, ignores trailing slack). An EMPTY payload is the UNIT, never the 0 nil-tail
; fossil: `cat` propagates whatever tail it is handed, so a single 0 from eps/opt/skip/many rides all the
; way out and `=` tells the two terminators apart -- a payload that should equal a map-built list would
; not. The unit is the only nil that leaves here.
;
; a MODULE, and the BOOT DOES NOT LOAD IT: each consumer opens with its own top-level (use 'peg) and
; then reads the names bare off the splice -- crew/holo/text.l, crew/cook/cook.l, crew/lapiz/lapiz.l,
; crew/sat/dimacs.l. ⚠ the (use ..) must be its OWN top-level form: a `:` body compiles as one unit,
; so a use nested inside the form that reads the names runs too late to serve it.

(module 'peg
(: (eps s y n) (y s ())
   (lit l s y n) (? (tally l <= tally s && l = snip s 0 (tally l))
                  (y (snip s (tally l) (tally s)) [l])
                  (n s))
   ; ⚠ the payload element is a ONE-CHAR STRING, not the charm: every element a peg
   ; parser yields is then a string fragment, so a consumer's join is pure concat
   ; (the byte law that used to glue a charm into a fold is gone). the predicate
   ; still sees the charm.
   (one p s y n) (? (0 < tally s && p (s 0))
                  (y (snip s 1 (tally s)) [(snip s 0 1)])
                  (n s))
   (span p s y n) (:
    (g j)(j < tally s && p (s j) ? (g (1 + j)) j)
    i (g 0)
    (y (snip s i (tally s)) [(snip s 0 i)]))
   (alt a b s y n) (a s y (\ _ (b s y n)))
   (seq a b s y n) (a s (\ s1 p1 (b s1 (\ s2 p2 (y s2 (p1 + p2))) n)) n)
   (seqs l) (foldr seq eps l)
   (opt a s y n) (a s y (\ _ (y s ())))
   (skip a s y n) (a s (\ s1 _ (y s1 ())) n)
   (pmap f a s y n) (a s (\ s1 x (y s1 (f x))) n)
   (many a s y n) (a s
    (\ s1 p1 (many a s1 (\ s2 p2 (y s2 (p1 + p2))) (\ _ (y s1 p1))))
    (\ _ (y s ())))
   (parse p s) (p s (\ r x (? (0 = tally r) x ())) (\ _ ()))     ; STRICT: whole input must parse
   (runp p s)  (p s (\ r x x) (\ _ ())))                           ; LENIENT: keep the payload, ignore trailing slack

) ; module peg
