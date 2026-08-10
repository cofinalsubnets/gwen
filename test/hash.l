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

; # reader macro wraps its operand in (pin …): #x -> (pin x)
(assert
 (= 6 $'(1 2 3))                                 ; #quoted-list -> the elements' net sum
 (= 532 $"hello")                                ; $string -> the charm sum (tally counts)
 (= 42 $42)                                      ; #positive number -> ceil magnitude
 (= 0 $-7)                                        ; #negative number -> 0 (<= 0 is falsy)
 (= 2 $(hash 1 10 2 20))                        ; #map-expr -> key count
 (= 12 $@(3 4 5)))                               ; #array -> the elements' net sum

; crew/sat/$ is ONE SATURATING CLAMP over the NET, max(0, ceil(net)): a number nets its own
; signed value (complex: the REAL part alone -- the magnitude is abs's), text its length, a map its key
; count, a list or array the SUM of its elements' nets -- so negatives push an aggregate
; toward nothing, per the law: positive to positive, zero-or-negative to zero, at every
; rank. abs is the plain magnitude -- the two DIVERGE on negatives: (abs -5) = 5, $-5 = 0.
(assert
 (= 4 (sat 3.9)) (= 0 (sat -3.9))               ; positive float -> ceil; negative -> 0
 (= 5 (abs -5)) (= 0 (sat -5))                  ; abs is magnitude; $ clamps a non-positive scalar to 0
 (= 3 (sat ~(3 4)))                            ; complex re>0 -> ceil(re), conjugate-blind
 (= 3 (sat ~(3 -4)))                           ; re>0 -> ceil(re), whatever the phase
 (= 0 (sat ~(-3 4)))                           ; re<0 sorts below 0 -> 0 (falsy)
 (= 0 (sat ~(0 -1)))                           ; -i: re=0, im<0 -> 0 (falsy)
 (= 0 (sat i))                                  ; i = ~(0 1): a PURE PHASE is blue -- gated by re, not the order
 (= 7 (sat @(3 4)))                             ; array -> element sum
 (= 0 (sat @(-3 -4)))                           ; a list or array of negatives nets <= 0 -> nothing
 (= 0 (sat '(-2 1))) !'(-2 1)                   ; negatives cancel positives -- net -1 -> 0, falsy
 (= 0 (sat (star-tray '(2 3) 0)))                   ; all-zero array -> net 0
 (= 1 (sat @(0.3 0.4)))                         ; ceil makes any positive net -> >=1
 (= 1 (sat ~(0.001 0)))                        ; re>0 -> ceil(0.001) = 1; sat=0 iff sorts <= 0
 (= 120 (sat (nom 'x)))                      ; a nom nets its spelling's charms + the mint's 0
 (= 0 (sat (mint 0))) !(mint 0)              ; the mint is materially EMPTY: a distinct nothing
 (= 0 (sat (intern "")))                     ; THE empty symbol -> 0 (falsy, prints as ())
 (= 2 (sat (hash 1 10 2 20)))                  ; table -> key count
 (= 2 (int (abs (hash 1 10 2 20))))            ; abs of a table is its key count too
 (= (sat (100 2)) (sat (200 2)))          ; any positive bignum saturates to FIX_MAX
 (= 0 (sat (- 0 (100 2)))) (nil? (- 0 (100 2)))  ; negative bignum -> 0, falsy
 (= 0 (sat (- 0 (62 2))))                 ; negative wide int -> 0
 (= 0 (sat -7)) (nil? -7))                      ; ordinary negative fixnum -> 0, falsy

; the defining invariant: (nil? x) == (= 0 (pin x)) for every value. empty
; containers and exact zeros are false; every present value (incl. functions,
; ports, symbols, bignums) is truthy with pin >= 1. (a MINT is the exception
; by design: the fresh point is materially empty -- a distinct NOTHING.)
(: same? (\ x (= (? (nil? x) 1 0) (? (= 0 (sat x)) 1 0)))
(assert
 (nil? "") (nil? (tablet 0)) (nil? (cask 0)) (nil? @0) (nil? 0) (nil? ())  ; empties/zeros are false
 !!"x" !!#(1 2) !(mint 0)                                        ; present values are true; the mint is nothing
 !!cap !!out !!(\ x x)                                                ; functions/ports too
 (same? "") (same? (tablet 0)) (same? (cask 0)) (same? @0) (same? 0)
 (same? "x") (same? #(1 2)) (same? (mint 0)) (same? cap) (same? out)
 (same? ~(0 0)) (same? ~(0.3 0.4)) (same? @(0 0)) (same? @(0.1 0))
 (same? (100 2)) (same? 'abc) (same? '(1 2))))
