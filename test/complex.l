; step 7: complex arithmetic.
; - a complex is a rank-0 scalar (twin?), the widest numeric tier: complex >
;   float > int/bignum. ~(re im) builds one (the `~` reader macro splices into
;   (twin re im)); `i` = ~(0 1) is the unit.
; - + - * / promote a real operand to (r, 0); % is undefined (-> nil).
; - sticky: never demotes to a real, even when im is 0 (eql-distinct keys).
; - ordered: < <= > >= sort a complex lexicographically by (re, im); a real r is
;   (r, 0). `=` bridges to reals. (A rank-N c array operand stays elementwise.)
; - accessors re / im / conj / abs / arg; abs is type-aware.

(: (close? a b eps) (< (- (? (< a b) (- b a) (- a b)) 0) eps)
   (close a b)       (close? a b 1e-12)
   (cclose z w)      (? (close (re z) (re w)) (close (im z) (im w)) 0)
   B100 (100 2))

(assert
 ; --- printer: ~(re im) form, reads back by re-evaluation (splices into twin) ---
 (= "~(0.0 1.0)" (show i))
 (= "~(2.0 3.0)" (show ~(2 3)))
 (= "~(2.0 -3.0)" (show ~(2 -3)))
 (= "~(-1.0 -2.0)" (show ~(-1 -2)))

 ; --- the defining identity, and that `=` bridges complex to a real ---
 (= (* i i) -1)                           ; i^2 = -1+0i, equal to the real -1
 (= ~(2 0) 2)                          ; cross-real equality (im 0)
 (= ~(2 0) 2.0)
 (= (+ 2 (* 3 i)) ~(2 3))              ; 2+3i built through the arith lanes

 ; --- sticky: a real-valued complex stays complex (distinct from the real) ---
 (twin? ~(2 0))
 (twin? i)
 (nil? (twin? 5))
 (nil? (twin? 5.0))

 ; --- componentwise add / sub; Gaussian mul; div by conjugate ---
 (= (+ ~(1 2) ~(3 4)) ~(4 6))
 (= (- ~(1 2) ~(3 4)) ~(-2 -2))
 (= (* ~(1 2) ~(3 4)) ~(-5 10))
 (cclose (/ ~(1 2) ~(3 4)) ~(0.44 0.08))
 (= (* ~(0 2) i) -2)                   ; 2i * i = -2 (real), via cross-real =

 ; --- accessors ---
 (= 2.0 (re ~(2 3)))
 (= 3.0 (im ~(2 3)))
 (= (conj ~(2 3)) ~(2 -3))
 (= 7 (re 7))                              ; re of a real is itself
 (nil? (im 7))                             ; im of a real is 0
 (= 5 (conj 5))                            ; conj LIFTS a real to ~(r 0) = r (the tower bridges)

 ; --- monadic ~ (conj): bare ~x (the `~` sigil not before `(`) lifts a real to
 ;     (r 0) and conjugates a complex; the 2-operand ~(a b) still splices into twin ---
 (= ~(5 0) ~5)   (twin? ~5)                 ; ~r lifts a real to (r, 0) -- a true complex
 (= ~(0 0) ~0)   (= ~(-3.5 0) ~-3.5)       ; ~0 -> (0, 0); lift works on a negative real
 (= (conj ~(2 3)) ~~(2 3))                 ; ~z conjugates -- ~ generalizes conj
 (= ~(2 -3) ~~(2 3))   (= i ~~(0 -1))      ; ~~(r i) = ~(r -i)
 (= (~(2 3) 4) ~(2 3 4))                   ; 3+ operands curry: ~(a b c) = (~(a b) c)

 ; --- abs: type-aware. complex -> float magnitude; real stays in its tier ---
 (= 5.0 (abs ~(3 4)))
 (= 13.0 (abs ~(-5 12)))
 (= 5 (abs -5))   (charm? (abs -5))          ; fixnum abs stays a fixnum
 (= 5 (abs 5))
 (= 5.5 (abs -5.5))                        ; float abs stays a float
 (= B100 (abs (- 0 B100)))                 ; bignum abs flips the sign, stays a bignum
 (= B100 (abs B100))

 ; --- arg: phase. complex via atan2; real -> 0 (>=0) or pi (<0) ---
 (close (/ pi 2) (arg i))
 (close 0 (arg ~(1 0)))
 (close pi (arg -1))
 (close 0 (arg 5))

 ; --- ordered lexicographically by (re, im); a real r is (r, 0). i = (0,1) and
 ;     1 = (1,0): re 0 < 1, so i < 1; equal re falls back to im ---
 (< i 1)   !(> i 1)   (<= i 1)   !(>= i 1)
 !(< 1 i)   (< ~(1 1) ~(2 2))   (<= ~(1 1) ~(2 2))
 (< ~(2 1) ~(2 5))   !(< ~(2 5) ~(2 1))

 ; --- % is undefined on complex; sine stays real-domain (-> nil); sqrt is ((/ 1 2) x)
 ;     and now works on a complex (via power); twin rejects a complex part ---
 (nil? (% i 2))
 (nil? (sine i))   (close 0.7071067811865476 (re ((/ 1 2) i)))   (nil? (twin 1 i))
 ; power IS defined on complex: w^z = exp(z Log w). i^2 = -1, i^i = e^(-pi/2) (real)
 (close -1 (re (2 i)))   (? word (close 0 (im (2 i))) 1)   ; im residue exceeds close's tol under f32 powf
 (close 0.20787957635076193 (re (i i)))   (close 0 (im (i i)))

 ; --- truthiness: a complex is FALSE iff it sorts <= 0+0i by the total order (re, then
 ;     im) -- 0, a negative real part, or re=0 with im<=0. re>0, or re=0 with im>0, is true.
 ;     (Lockstep with #: #z = 0 iff z <= 0, so -i and ~(-3 4) are now falsy.) ---
 (nil? ~(0 0))   (nil? ~(0 0.0))           ; 0+0i is false
 (nil? ~(-3 4))  (nil? ~(0 -1))            ; re<0, and -i, are false
 ; THE TRUTH GATE IS THE REAL PART, not the total order -- the one place the two
 ; part. `i` and `-i` are swapped by a field automorphism (and C admits no order
 ; compatible with its arithmetic), so truth cannot tell them apart: both are
 ; BLUE, the zero floor. they have magnitude, but no positive amount. the ORDER
 ; still separates them, because sorting needs totality.
 (nil? i)  (nil? ~(0 1))  (nil? ~(0 8))    ; every pure phase is nothing...
 (i < 5)   (~(0 -1) < i)                   ; ...but the order still ranks them
 (? ~(3 4) 1 0)   (? ~(3 -4) 1 0)          ; re>0 -> true, whatever the phase
 (5 = $~(3 4))  (5 = $~(3 -4))              ; ...and the AMOUNT is the magnitude, conjugate-blind

 ; --- complex meets bignum: the bignum narrows to double (floating domain) ---
 (twin? (+ i B100))
 (= 1.0 (im (+ i B100)))                   ; imaginary part untouched
 (< 1e29 (re (+ i B100)))                  ; real part ~ 2^100

 ; --- complex as hash keys: eql-distinct from the equal real (sticky) ---
 (= 'hit (peep (pin (tablet 0) ~(2 0) 'hit) ~(2 0) 'miss))
 (= 'miss (peep (pin (tablet 0) ~(2 0) 'hit) 2 'miss))   ; 2 and 2+0i are different keys
 (= 'iv (peep (pin (tablet 0) i 'iv) i 'miss)))

; --- step 7b: full-rank complex arrays (packed (re,im) g_C tuples) -----------
; a complex value packs two floats per element into a `c` array (type code c = 2,
; the tier between r and o). twin-tray/array/@ all build them (a-type infers c);
; `get` returns a ~(re im) box; + - * / broadcast in the complex domain; `=` ->
; a 0/1 mask, the orderings -> nil; net/prod fold complex; aall/aany lift the
; 0+0i-is-falsy rule. (body-less `:` -> ca-* leak to global, reused in roundtrip.l)
(: ca-z (twin-tray '(3) 0)                             ; zero-filled: three 0+0i
   ca-v (twin-tray '(2) `(~(1 2) ~(3 4)))        ; explicit packed chain
   ca-a (array 3 ~(1 1) ~(2 2) ~(3 3))        ; `array` infers c from its args
   ca-r @(~(1 0) ~(0 1))                       ; `@` reader infers c too
   ca-m (array '(2 2) ~(1 0) ~(0 1) ~(2 0) ~(0 2)))  ; rank-2 complex

(assert
 ; --- construction / type / shape ---
 (= twin-tray (tier ca-v))   (= twin-tray (tier ca-z))   (= twin-tray (tier ca-a))
 (= twin-tray (tier ca-r))   (= twin-tray (tier ca-m))
 (= 1 (rank ca-v))   (= 2 (tally ca-v))
 (= 2 (rank ca-m))   (= 4 (tally ca-m))

 ; --- get returns a complex box; rank-N indexing; zero element ---
 (= ~(1 2) (peep ca-v 0 0))   (= ~(3 4) (peep ca-v 1 0))   (twin? (peep ca-v 0 0))
 (= ~(0 2) (peep ca-m `(1 1) 0))                ; row-major (1,1)
 (= ~(0 0) (peep ca-z 0 0))   (= ~(2 2) (peep ca-a 1 0))

 ; --- elementwise broadcast: array (op) array ---
 (= ~(2 4) (peep (+ ca-v ca-v) 0 0))             ; (1+2i)+(1+2i) elementwise
 (aall (= ca-v (+ ca-v (twin-tray '(2) 0))))           ; + the zero array is identity
 (= ~(-5 10) (peep (* (twin-tray '(2) `(~(1 1) ~(1 2))) ca-v) 1 0))  ; (1+2i)(3+4i)
 (= ~(1 2) (peep (/ (* ca-v ca-v) ca-v) 0 0))    ; (z*z)/z = z

 ; --- scalar broadcast: complex scalar (op) array, both orders ---
 (= ~(11 2) (peep (+ ~(10 0) ca-v) 0 0))
 (= ~(13 4) (peep (+ ca-v ~(10 0)) 1 0))
 (= ~(2 4) (peep (* ~(2 0) ca-v) 0 0))          ; scale by 2
 (= ~(0 1) (peep (* i ca-r) 0 0))                ; i*(1+0i) = i

 ; --- real array meets complex: the real promotes to (v, 0) ---
 (= ~(11 2) (peep (+ @(10 20) ca-v) 0 0))        ; z @(10 20) lifts to (10,0)/(20,0)
 (= ~(23 4) (peep (+ ca-v @(10 20)) 1 0))

 ; --- `=` over complex arrays -> a 0/1 mask; orderings/% -> nil ---
 (= 1 (peep (= ca-v ca-v) 0 0))
 (= 0 (peep (= ca-v (twin-tray '(2) `(~(9 9) ~(3 4)))) 0 0))
 (aall (= ca-v ca-v))
 (nil? (< ca-v ca-v))   (nil? (> ca-v ca-v))   (nil? (% ca-v ca-v))

 ; --- reductions: complex sum / product; max/min unordered -> nil ---
 (= ~(4 6) (net ca-v))                         ; (1+2i)+(3+4i)
 (= ~(-5 10) (prod ca-v))                      ; (1+2i)*(3+4i)
 (= ~(6 6) (net ca-a))   (= ~(0 0) (net ca-z))
 (nil? (max ca-v))   (nil? (min ca-v))

 ; --- aall/aany lift the 0+0i-is-falsy rule ---
 (aall ca-v)
 (nil? (aall (twin-tray '(2) `(~(0 0) ~(1 1)))))   ; a 0+0i element fails aall
 (sat (twin-tray '(2) `(~(0 0) ~(1 1))))          ; the element sum ~(1 1) sorts positive
 (nil? (sat ca-z))

 ; --- abs of a complex array = the flat L2 norm over every (re, im) ---
 (= 5.0 (abs (twin-tray '(1) `(~(3 4)))))          ; |3+4i| = 5
 (close ((/ 1 2) 30) (abs ca-v))                     ; sqrt(1+4+9+16)

 ; --- pin / nil?: the invariant (nil? x) == (= 0 (pin x)) holds here too ---
 (= 0 (sat ca-z))   (nil? ca-z)
 !!ca-v   (= 8 (sat ca-v))                        ; ceil |4+6i| = ceil(sqrt 52): the sum's modulus
 (= (? (nil? ca-v) 1 0) (? (= 0 (sat ca-v)) 1 0))
 (= (? (nil? ca-z) 1 0) (? (= 0 (sat ca-z)) 1 0))

 ; --- printer: rank-1 terse @((C…)…); rank>=2 (array '(…) (C…)…) ---
 (= "@(~(1.0 2.0) ~(3.0 4.0))" (show ca-v))
 (= "(array '(2 2) ~(1.0 0.0) ~(0.0 1.0) ~(2.0 0.0) ~(0.0 2.0))" (show ca-m)))
