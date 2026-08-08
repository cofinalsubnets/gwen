; `*` extended as the multiplicative analog of `+` (l.c g_mul_mx), dispatched
; through the same kind matrix. `*` is "repeated `+`": a sequence (string/symbol/
; list) times a scalar count n is n copies joined, just as (* 2 3) = 2+2+2. The
; count SATURATES (($ c) -- the count law, shared with numeral-apply and array
; shapes: non-positive -> 0 -> empty, a float ceils, a complex by total-order-
; guarded modulus); an array/non-number count is undefined
; (nil). chain x chain is the CARTESIAN PRODUCT (the semiring product whose `+` is cat --
; see test/spec.l); string x string / sym x sym stay nil; a thread anywhere -> composition.
(: sm-i (\ x (? (nom? x) (= x (intern (string x))) 0)))      ; interned symbol?
(assert
 ; --- numbers unchanged ---
 (6 = 2 * 3)   (7.5 = 3 * 2.5)
 ; FIXME = no longer returns a mask on arrays so is this test now vacuous?
 (aall (= @(2 4 6) (* @(1 2 3) 2)))             ; array broadcast

 ; --- string repetition (commutative), empty, count coercion ---
 ("ababab" = "ab" * 3)   ("ababab" = 3 * "ab")
 ("" = "ab" * 0)   (string? ("ab" * 0))          ; 0 -> the empty string
 ("" = "ab" * -2) (string? ("ab" * -2))          ; non-positive count saturates to 0 -> empty
 ("ababab" = "ab" * 2.9)                      ; float count ceils: ($ 2.9) = 3
 ("ababababab" = "ab" * ~(3 4))              ; |3+4i| = 5 copies ; FIXME seems wrong, ceil is saturate logic, saturate should drop imaginary component
 ("xxx" = "x" * 3)

 ; --- a symbol inherits the string lane: * repeats the spelling and RE-INTERNS -> a symbol ---
 ('xyxy = 'xy * 2) ('xyxy = 2 * 'xy)
 ('cdcd = 'cd * 2)

 ; --- list repetition ---
 ('(1 2 1 2 1 2) = '(1 2) * 3)
 ('(1 2 1 2) = 2 * '(1 2))                    ; commutative
 !('(1 2) * 0)                            ; 0 copies of a list -> nil

 ; --- chain x chain -> cartesian product; other seq x seq -> nil ---
 ('((1 2)) = '(1) * '(2))                     ; chain x chain: the cartesian product (1-pair list)
 (= '((1 3) (1 4) (2 3) (2 4)) (* '(1 2) '(3 4)))
 ; FIXME these laws seem not that well motivated
 !("ab" * "cd")                           ; string x string still nil
 !('a * 'b)                               ; sym x sym still nil
 !("ab" * @(1 2))                         ; array is not a count
 !(@(1 2) * "ab")
 ; a map is a lookup-lambda, so it takes the thread (composition) rule, not nil:
 ; (* m g) x = (m (g x)); here m maps 5->10, g adds 4, so (1) -> g 5 -> m 10
 (10 = (pin #() 5 10 * (\ x (x + 4))) 1)

 ; --- thread precedence: composition = Church mul ((* f g) x = f (g x)) ---
 (36 = ((\ x (x * x)) * (\ x (x + 1))) 5)     ; sq(inc 5) = sq 6 = 36
 (26 = ((\ x (x + 1)) * 2) 5)                 ; (inc (2 5)) = inc 25 = 26

 ; --- the semiring tie: s*n == s + s + … (n times). NB +/* are binary, so the
 ;     n-fold sum must be nested explicitly (over-applying a string indexes it) ---
 ("ab" * 3 = "ab" + "ab" + "ab")
 ('(1) * 3 = '(1) + '(1) + '(1)))
