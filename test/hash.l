; hash constructor (nested `put`) + #(…) printer/reader form
(: t (hash 1 100 2 200 3 300))
(assert
 ; the constructor builds a working hash
 (tablet? t) (nil? (tablet? 5)) (nil? (tablet? '(1 2)))   ; tablet?: only maps, not fixnums/lists
 (lit? t) (lit? cap) (nil? (lit? 5)) (nil? (lit? 0))  ; lit?: any heap object, not a fixnum/nil
 (= 100 (peep t 1 0))
 (= 200 (peep t 2 0))
 (= 300 (peep t 3 0))
 (= 0 (peep t 9 0))                              ; missing key -> default
 (= 3 (foldl (\ n _ (+ 1 n)) 0 (keys t)))      ; three entries
 ; empty + single-entry printer forms are deterministic (no bucket-order dependence)
 (= "#()" (show (tablet 0)))
 (= "#(1 100)" (show (hash 1 100)))
 (= "#(1 \"hi\")" (show (hash 1 "hi")))
 (string? (show t))                             ; multi-entry: just exercise the path
 ; (dig x) exposes the runtime hashing method: a fixnum, equal for eql keys
 (charm? (dig 'k))
 (= (dig 'k) (dig 'k))
 (= (dig '(1 2)) (dig '(1 2))))

; #(…) reader macro: splices a list operand into (hash …), wraps an atom as (hash x) --
; a BOX. #() reads to the EMPTY MAP directly: the reader emits (tablet 0) -- () is the
; zero-point, distinct from 0. hashes print as #(…), the empty map as #().
(: ht #(1 100 2 200 3 300))
(assert
 (tablet? ht)
 (= 100 (peep ht 1 0))
 (= 300 (peep ht 3 0))
 (tablet? (tablet 0))                              ; the empty hash
 !(tablet 0) !#()                               ; both the empty map (falsy); #() reads to (tablet 0)
 (= 50 (peep (pin (tablet 0) 5 50) 5 0))                ; the empty hash is mutable
 (= 3 (foldl (\ n _ (+ 1 n)) 0 (keys #(1 10 2 20 3 30))))   ; splice builds all entries
 (= "#()" (show #()))                           ; #() is the empty map, and round-trips
 (= "#(1 \"hi\")" (show #(1 "hi"))))

; # on a NON-LIST datum boxes it: #x = #(() x), a fresh mutable hash pinning x
; at () (the unit -- the do-nothing key is NOTHING, the 0->() flip) -- still distinct
; from #() the empty map (one ()-keyed entry vs no entries). key 0 is a MISS now.
(: bx #0)
(assert
 (tablet? #0) !!#0 (= 1 $#0)                       ; a box is a nonempty 1-entry hash
 (= 0 (peep bx () 9))                           ; the boxed value sits at key () (the unit)
 (= 9 (peep bx 0 9))                            ; ... and key 0 is a MISS (the flip's tell)
 (= 9 (: _ (pin bx () 9) (peep bx () 0)))       ; ... and is mutable in place
 (= 5 (peep #5 () 0)) (= "s" (peep #"s" () 0))  ; any scalar boxes (at the unit)
 (= 'a (peep (hash 'a) () 0))                   ; (NB #'a SPLICES -- a quote form is a chain)
 (= "#(() 5)" (show #5))                     ; a box prints as #(() v), reads back
 (= 7 (peep #(1 2 7) () 0))                     ; an odd trailing operand lands in the box slot (the unit key)
 (= 2 (peep #(1 2 7) 1 0)))
