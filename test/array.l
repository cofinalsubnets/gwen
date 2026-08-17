; step 5a: typed multi-rank arrays.
; - (star-tray shape 0) zero-fills; (star-tray shape vals) fills row-major (gem-/twin-/top-tray for r/c/o)
; - get: fixnum index (rank 1), shape-list index (rank N), oob -> default
; - rank / tally / shape / tier accessors
; - elementwise + - * / over arrays + scalar broadcast + numpy broadcasting
; - mixed element-type promotes to the widest type
; - < <= > >= = elementwise -> 0/1 bool array
; - reductions net prod max min aall aany; identity on scalars
; - vector falsiness: a tuple is false iff every element is 0
; - elementwise transcendentals (sqrt) over an array
; - print round-trip; non-conforming / non-numeric -> nil

(assert
 ; --- constructor + accessors ---
 (= 2 (rank (star-tray '(2 3) 0)))
 (= 6 (tally (star-tray '(2 3) 0)))
 (= star-tray (tier (star-tray '(2 3) 0)))
 (= gem-tray (tier (gem-tray '(4) 0)))
 (= '(2 3) (shape (star-tray '(2 3) 0)))
 (nil? (rank 5))                     ; non-tuple -> nil
 (nil? (tier "x"))

 ; --- only a RANK-0 point demotes to its lone scalar star (no rank-0 vec exists,
 ; which is what keeps the float/wide/complex boxes lean). a rank-1 length-1 array
 ; STAYS an array: the invariant is RANK, not count, so the surface is continuous
 ; across every tally rather than an array / scalar / array staircase ---
 (= 0 (star-tray 0 0))                    ; empty shape, zero-filled -> the fixnum 0
 (gem? (gem-tray 0 0)) (= 0.0 (gem-tray 0 0))      ; -> a boxed 0.0
 (tray? (star-tray '(1) '(5)))            ; rank-1 len-1 -> a ONE-CELL ARRAY, not the fixnum 5
 (5 = (peep (star-tray '(1) '(5)) 0 -1))  ; ...its lone cell reads back
 (tray? @(3.5)) !(= 3.5 @(3.5))           ; @(3.5) is a one-cell array, never the scalar 3.5
 (1 = (tally @(3.5))) (1 = (rank @(3.5)))
 (nil? (rank (star-tray 0 0)))           ; a demoted scalar is not a tuple
 (nil? (tally (gem-tray 0 0)))
 (nil? (shape (star-tray 0 0)))

 ; --- get: rank-1 fixnum index ---
 (= 10 (peep (star-tray '(3) '(10 20 30)) 0 -1))
 (= 20 (peep (star-tray '(3) '(10 20 30)) 1 -1))
 (= 30 (peep (star-tray '(3) '(10 20 30)) 2 -1))
 (= -1 (peep (star-tray '(3) '(10 20 30)) 3 -1))   ; out of bounds -> default
 (= -1 (peep (star-tray '(3) '(10 20 30)) -1 -1))  ; negative -> default

 ; --- get: rank-N shape-list index, row-major ---
 (= 1 (peep (star-tray '(2 2) '(1 2 3 4)) '(0 0) -1))
 (= 2 (peep (star-tray '(2 2) '(1 2 3 4)) '(0 1) -1))
 (= 3 (peep (star-tray '(2 2) '(1 2 3 4)) '(1 0) -1))
 (= 4 (peep (star-tray '(2 2) '(1 2 3 4)) '(1 1) -1))
 (= -1 (peep (star-tray '(2 2) '(1 2 3 4)) '(2 0) -1))  ; oob -> default
 (= -1 (peep (star-tray '(2 2) 0) 0 -1))                   ; wrong-rank key -> default
 (= -1 (peep (star-tray '(2 2) 0) '(0) -1))                ; too-few indices -> default

 ; --- rank-0 deref: empty/nil key yields the lone scalar ---
 (= 2.5 (peep 2.5 0 -9))

 ; --- float element read boxes an r ---
 (= 1.5 (peep (gem-tray '(2) '(1.5 2.5)) 0 -1))
 (gem? (peep (gem-tray '(2) '(1.5 2.5)) 0 -1))

 ; --- scalar broadcast: array (op) scalar ---
 (aall (= (star-tray '(3) '(11 21 31)) (+ (star-tray '(3) '(10 20 30)) 1)))
 (aall (= (star-tray '(3) '(9 19 29)) (- (star-tray '(3) '(10 20 30)) 1)))
 (aall (= (star-tray '(2) '(4 6)) (* (star-tray '(2) '(2 3)) 2)))
 (aall (= (star-tray '(2) '(3 5)) (// (star-tray '(2) '(7 11)) 2)))   ; `//` floor division, stays z
 (= gem-tray (tier (/ (star-tray '(2) '(7 11)) 2)))                     ; `/` promotes (7/2 inexact) -> r
 (aall (= (gem-tray '(2) '(3.5 5.5)) (/ (star-tray '(2) '(7 11)) 2)))
 (aall (= (star-tray '(2) '(4 6)) (/ (star-tray '(2) '(8 12)) 2)))   ; `/` stays z when every element divides evenly
 (aall (= (star-tray '(2) '(1 1)) (% (star-tray '(2) '(7 11)) 2)))
 (aall (= (star-tray '(3) '(2 4 6)) (+ 1 (star-tray '(3) '(1 3 5)))))  ; scalar on the left

 ; --- array (op) array, same shape ---
 (aall (= (star-tray '(3) '(11 22 33))
          (+ (star-tray '(3) '(10 20 30)) (star-tray '(3) '(1 2 3)))))
 (aall (= (star-tray '(3) '(9 18 27))
          (- (star-tray '(3) '(10 20 30)) (star-tray '(3) '(1 2 3)))))

 ; --- numpy broadcasting: outer sum of a column and a row ---
 (aall (= (star-tray '(3 3) '(11 21 31 12 22 32 13 23 33))
          (+ (star-tray '(3 1) '(1 2 3)) (star-tray '(1 3) '(10 20 30)))))
 ; size-1 axis stretches
 (aall (= (star-tray '(2 2) '(11 21 12 22))
          (+ (star-tray '(2 2) '(1 1 2 2)) (star-tray '(1 2) '(10 20)))))

 ; --- mixed element-type promotes to the widest ---
 (= gem-tray (tier (+ (star-tray '(2) 0) 1.5)))            ; z array + r scalar -> r
 (aall (= (gem-tray '(2) '(1.5 1.5)) (+ (star-tray '(2) 0) 1.5)))
 (= star-tray (tier (+ (star-tray '(2) 0) (star-tray '(2) 0))))  ; z + z -> z
 (= star-tray (tier (+ (star-tray '(2) 0) 1)))                ; scalar int never widens the array

 ; --- comparison -> 0/1 bool array ---
 (aall (< (star-tray '(3) '(1 2 3)) 10))
 !(aall (< (star-tray '(3) '(1 2 30)) 10))
 (sat (< (star-tray '(3) '(1 20 30)) 10))
 !(sat (< (star-tray '(3) '(10 20 30)) 5))
 (aall (= (star-tray '(3) '(1 0 1)) (< (star-tray '(3) '(1 5 2)) 3)))
 (= star-tray (tier (< (star-tray '(3) 0) 1)))               ; bool array is z
 ; whole-array equality is just (= a b) -- `=` answers {0,1} on every kind
 (aall (= (star-tray '(2) '(5 6)) (star-tray '(2) '(5 6))))
 !(aall (= (star-tray '(2) '(5 6)) (star-tray '(2) '(5 7))))

 ; --- reductions ---
 (= 60 (net (star-tray '(3) '(10 20 30))))
 (= 6000 (prod (star-tray '(3) '(10 20 30))))
 (= 30 (max (star-tray '(3) '(10 30 20))))
 (= 10 (min (star-tray '(3) '(20 10 30))))
 (= 6.0 (net (gem-tray '(3) '(1.0 2.0 3.0))))
 (= 21 (net (star-tray '(2 3) '(1 2 3 4 5 6))))    ; reduces over all axes
 (aall (star-tray '(3) '(1 2 3)))                   ; no zero element
 !(aall (star-tray '(3) '(1 0 3)))               ; a zero element
 (sat (star-tray '(3) '(0 0 3)))
 !(sat (star-tray '(3) 0))                         ; all zero

 ; --- reductions are the identity on a scalar (rank-agnostic idiom) ---
 (= 5 (net 5))
 (= 5 (max 5))
 (= 1 (aall 1))
 (= 0 (aall 0))
 (aall (< 1 2))                                    ; scalar: (< 1 2) = 1, (aall 1) = 1
 !(aall (< 2 1))
 (aall (< (star-tray '(2) '(1 2)) (star-tray '(2) '(3 4))))  ; array: same expression

 ; --- truthiness is the NET: a value is false iff its element sum nets <= 0 ---
 (nil? (star-tray '(3) 0))                  ; zero array -> false
 (nil? (gem-tray '(2 2) 0))                ; zero float array -> false
 (nil? (star-tray '(0) 0))                  ; empty array -> vacuously false
 (nil? 0.0)                             ; boxed 0.0 -> false (was a truthy wart)
 !(nil? 2.5)                         ; positive float -> true
 (nil? -5)                           ; a negative real is FALSE (<= 0)
 !(nil? (+ (star-tray '(2) 0) 1))        ; positive array -> true
 (nil? @(-3 -4))                     ; a NEGATIVE array is false too -- the sign law at rank 1
 (nil? @(-2 1)) !(nil? @(-2 3))      ; the elements SUM: -2+1 <= 0 < -2+3
 (= 1 (? (star-tray '(3) 0) 0 1))         ; zero array takes the false arm
 (= 0 (? (+ (star-tray '(3) 0) 1) 0 1))    ; positive array takes the true arm
 ; a positive net makes the array truthy, per the tower:
 !(nil? @(0 0 5))                    ; one positive element -> true
 (nil? @(0.0 0.0))                      ; all-zero float tuple -> false
 (nil? ~(0 0))                         ; complex 0+0i -> false
 (nil? ~(0 1))                      ; a PURE PHASE is blue: gated by the real part, not the order
 (nil? i)                            ; ...so the imaginary unit is nothing, though it still SORTS above 0

 ; --- elementwise transcendentals over an array ---
 (aall (= (gem-tray '(2) '(2.0 3.0)) ((/ 1 2) (gem-tray '(2) '(4.0 9.0)))))
 (= gem-tray (tier ((/ 1 2) (star-tray '(3) 0))))  ; result is a float array

 ; --- print as constructor forms ---
 ; rank-1 -> the @(…) sugar (@ reader splices into (tuple …)); rank>=2 ->
 ; (array '(shape) elem …) -- always a surface form, the type re-inferred
 (= "@(10 20 30)" (show (star-tray '(3) '(10 20 30))))
 (= "(array '(2 2) 1 2 3 4)" (show (star-tray '(2 2) '(1 2 3 4))))
 (= "@(1.5 2.5)" (show (gem-tray '(2) '(1.5 2.5))))
 (= "@(1 2 3)" (show (star-tray '(3) '(1 2 3))))   ; z aliases the word-int kind -> terse @
 ; @(…) reads back to an equal array; the explicit form is a plain constructor call
 (aall (= (star-tray '(3) '(10 20 30)) @(10 20 30)))
 (aall (= (gem-tray '(2) '(1.5 2.5)) @(1.5 2.5)))
 (aall (= (star-tray '(2 2) '(1 2 3 4)) (star-tray '(2 2) '(1 2 3 4))))
 ; the printed form reads back to an equal array
 (aall (= (star-tray '(3) '(10 20 30)) (tuple 10 20 30)))
 (aall (= (star-tray '(2 2) '(1 2 3 4)) (star-tray '(2 2) '(1 2 3 4))))
 (aall (= (gem-tray '(2) '(1.5 2.5)) (tuple 1.5 2.5)))

 ; --- non-conforming / non-numeric -> nil ---
 (nil? (+ (star-tray '(3) 0) (star-tray '(4) 0)))   ; shapes [3] and [4] don't conform
 (= "x" (+ (star-tray '(3) 0) "x"))             ; a tray is number-band: against a string it degenerates
 (nil? (star-tray '(3) 0)))                     ; (sanity) zero array is falsy

; --- empty arrays: the first stone of the APL buildout. a shape-(0) array
; prints the (array '(shape)) surface form (@ has no empty spelling and @0
; used to collide with the one-element zero array), broadcast keeps a
; 0-axis empty (a 1-axis takes the OTHER size including 0 -- max here once
; filled one element out of an empty operand), and empty reductions answer
; their monoid UNITS: the floor appears as the values of empty reductions.
(: e (star-tray '(0) 0)
 (assert
  ("@()" = (show e)) ('(0) = (shape e)) (1 = (rank e)) (0 = (tally e))   ; empty rank-1 prints @()
  (0 = (net e)) (1 = (prod e)) (aall e)        ; the units: empty sum 0, empty product 1, empty conjunction true
  !e (0 = $e)                                    ; an empty nets nothing
  (0 = (tally (e + e))) (0 = (tally (e * 2))) (0 = (tally (1 + e)))
  (0 = (tally (e < 1))) (0 = (tally (e = e)))      ; compare masks stay empty
  (0 = (tally (e 2)))                             ; an empty exponent array applies empty
  (0 = (tally (sine e))) (0 = (tally (2 e)))    ; elementwise maps stay empty
  (0 = (tally ((star-tray '(1) '(5)) + e)))           ; a 1-axis broadcasts to 0, not 1
  (-1 = (peep e 0 -1))                           ; out of bounds -> the default
  ('(0 3) = (shape ((gem-tray '(0 3) 0) + 1)))     ; rank-2 empties keep their shape
  (: rt @()                                      ; the printed form (@()) reads back
     (&& (0 = (tally rt)) ('(0) = (shape rt)) (star-tray = (tier rt))))))
