; A NAMED symbol INHERITS the string lane under +/* (the totality philosophy: make use of
; constructions others reject). a symbol acts as its SPELLING and RE-INTERNS the result,
; but DEMOTES to a string when a string operand is present (the rank min pulls to 0 --
; the spelling IS a monoid hom, so this crossing is lawful). a NUMBER against a symbol
; arrives as the unit (the degenerate law: no hom from a group into the free monoid),
; so the symbol answers whole. it still ADJOINS to a list (foo stays foo -- the list
; law), applying it is const-1, and the EMPTY symbol (intern "") is the zero point ():
; +'s unit and *'s annihilating zero. coins are the per-kind override.

(: sa-z (intern "")
 (assert
  ; --- + inherits the string lane: sym+sym cats spellings and RE-INTERNS ---
  ('efef = ('ef + 'ef)) ('ab = ('a + 'b))       ; sym + sym -> an interned symbol
  ("abef" = ("ab" + 'ef)) ("efab" = ('ef + "ab"))   ; sym <-> string -> DEMOTE to a string
  ('ef = (65 + 'ef)) ('ef = ('ef + 65))         ; sym <-> number -> the number degenerates
  ('ef = (@(1 2) + 'ef)) ('ef = ('ef + @(1 2))) ; sym <-> array: the tray is number-band, same law
  ; the EMPTY symbol (intern "") is () -- the UNIT, so it's +'s identity --
  ('ef = sa-z + 'ef) ('ef = 'ef + sa-z) (id? () (sa-z + sa-z))   ; () + x = x, either side; () + () = ()
  ; --- * repeats the spelling and re-interns; sym*sym stays nil (str*str); () is *'s ZERO ---
  ('xyxy = ('xy * 2)) ('xyxy = (2 * 'xy)) !('a * 'b) (() = sa-z * 2)
  ; --- apply: const-1, uniformly (the unit action; no more name indexing) ---
  (1 = ('xyz 1)) (1 = ('xyz 0)) (1 = (sa-z 5)) (1 = ((mint 0) 'x))
  ; --- lists still take any element, symbols included (the list law, not string algebra)
  ('(ef 1 2) = ('ef + '(1 2)))
  ; --- the explicit bridge survives ---
  ("asdf" = (string 'asdf)) ('asdf = (intern "asdf"))
  ('xy = (intern (string 'xy)))                 ; intern . string = identity on interned
  (id? sa-z (intern ""))
  ; --- noms carry their name as data: read it with `string` ---
  ("cd" = (string (nom "cd"))) ("cd" = (string (nom 'cd)))
  ; --- strings: concat stands; a number degenerates to "" against one ---
  ("abcd" = ("ab" + "cd")) ("ab" = (65 + "ab")) ("ab" = ("ab" + 65))
  ("ababab" = ("ab" * 3)) ("" = ("" + ""))))

; --- mint serials: the total order is TOTAL over symbols -------------------
; every mint draws a serial from the one stream (pids share it); symbols
; order by name, then interned-first, then serial (creation order); noms
; (chains) inherit the chain order through chain lex + the mint tiebreak.
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
