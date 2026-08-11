
(: s0 (nom 0)
   s1 (nom 0)
   s2 (nom 0)
 (assert !(= s0 s1)
         !(= s0 s2)
         !(= s1 s2)
         (= 'asdf (intern "asdf"))          ; intern: string -> the interned symbol
         (= 'asdf (intern 'asdf))           ; ...identity on any non-string arg
         !(= 'asdf (nom "asdf"))         ; a fresh nom (its own kind, KNom): never the interned symbol
         !(= (nom "asdf") (nom "asdf")) ; ...nor another nom (distinct serials)
         (= "asdf" (string (nom "asdf")))  ; the name via `string` -- the explicit bridge (a nom is not a chain)
         (= "asdf" (string (nom 'asdf)))   ; ...same for a symbol arg (string'd on the way in)
         (= "asdf" (string 'asdf))))
(:
 t (tablet 0)
 (Put k v) (pin t k v)
 (Del k) (pull t k 0)
 (Get k) (peep t k 0)
 (ln x) (foldl (\ n _ (+ 1 n)) 0 x)             ; spine length ($ on a list is the net sum)
 _ (Put 1 2)
 _ (Put 2 3)
 _ (Put 3 4)
 _ (Put 't 'f)
 _ (assert
    (= 4 (sat t))
    (= 4 (ln (keys t))))
 _ (Del 2)
 _ (Del 't)
 (assert
    (= 2 (sat t))
    (= 2 (ln (keys t)))
    (: (lll t) (foldl (\ l k (link k (link (Get k) l))) () (keys t))
     (= (* 2 (sat t)) (ln (lll t))))))

(:
 (stripPrefix p s) (? (= p (snip s 0 (tally p))) (snip s (tally p) (tally s)))
 (lit l y n s) ((\ ss (? ss (y ss (link l ())) (n s))) (stripPrefix l s))
 (drop p y) (p (\ s _ (y s ())))
 (opt p y _) (p y (\ s (y s ())))
 (cats a b y n) (a (\ s l (b (\ s m (y s (cat l m))) n s)) n)
 (alt a b y n) (a y (b y n))
 (rep a y n s) ((opt (cats a (rep a))) y n s)
 (assert
  (= "xx" (+ "xx" ""))
  (= "xx" (+ "x" "x"))
  (= "xx" (+ "" "xx"))
  (= 4 (tally "slen"))
  (= "bidden" (snip "forbidden planet" 3 9))
  (= "all of it" (snip "all of it" -9 100))
  ; applying a string indexes it: a byte 0..255, or 1 when out of range --
  ; matching the empty string, where ("" k) is Church-numeral k**0 == 1
  (= 104 ("hi" 0))                          ; 'h'
  (= 105 ("hi" 1))                          ; 'i'
  (= 1 ("hi" 2))                            ; past the end -> 1
  (= 105 ("hi" -1)) (= 104 ("hi" -2))       ; a negative index counts from the END
  (= 1 ("hi" -3))                           ; ... and past the front is out of range too
  (= 1 ("hi" 'x))                           ; non-numeric index -> 1
  (= 255 ((string 255) 0))                  ; high byte returns unsigned 0..255
  ; ⚠ apply and peep agree on the NON-NEGATIVE indexes only: peep answers its default on a
  ; negative (and snip clamps one to the front), so counting from the end is apply's alone.
  (= (peep "vaporeon" 3 0) ("vaporeon" 3))
  (= 0 (peep "vaporeon" -1 0)) (= 110 ("vaporeon" -1))
  ; pin writes exactly where peep reads, and text has no cell, so it answers a FRESH string
  (= "vapoReon" (pin "vaporeon" 4 82)) (= "vaporeon" (pin "vaporeon" -1 82))
  (= "eon" (stripPrefix "vapor" "vaporeon"))
 (= '("eon" "vapor")
  (lit "vapor" link 0 "vaporeon"))
 (= '("rrq" "R")
  (alt (lit "r") (lit "R") link 0 "Rrrq"))
 (= '("q" "R" "r" "r")
  (rep (alt (lit "r") (lit "R")) link 0 "Rrrq"))))

