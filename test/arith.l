; arithmetic, comparison, equality:
; - fixnum fast paths unchanged
; - mixed charm?/gem? promotes to float
; - integer overflow promotes to a boxed wide int (see test/box.l)
; - /0 promotes to IEEE inf/-inf/NaN
; - non-numeric operands return nil

(assert
 ; --- fixnum fast path ---
 (= 3 (+ 1 2))
 (= -1 (- 1 2))
 (= 6 (* 2 3))
 (= 2.5 (/ 5 2))                    ; `/` true division: inexact -> float
 (= 2 (/ 4 2))                      ; exact -> stays integer
 (charm? (/ 4 2))
 (= 0.5 (/ 1 2))
 (= 2 (// 5 2))                     ; `//` truncating integer division
 (= -2 (// -5 2))                   ; truncates toward zero (not floor -3)
 (= 1 (% 5 2))

 ; --- mixed charm? + gem? promotes ---
 (= 3.5 (+ 1 2.5))
 (= 3.5 (+ 2.5 1))
 (= -1.5 (- 0 1.5))
 (= 1.0 (* 2 0.5))
 (= 0.5 (/ 1.0 2))
 (= 0.5 (/ 1 2.0))

 ; --- both float ---
 (= 4.0 (+ 1.5 2.5))
 (= 1.0 (- 1.5 0.5))
 (= 3.0 (* 1.5 2.0))
 (= 3.0 (/ 1.5 0.5))

 ; --- integer overflow promotes to a boxed wide int (not a float) ---
 ; 2^61 is the largest power of two that's still a fixnum; doubling it
 ; overflows the 62-bit tag into a box that holds the exact integer
 ; (which then demotes back to a fixnum once it fits again).
 !(charm? (* 2 2305843009213693952))   ; 2^62: now a box, not a fixnum
 !(gem? (* 2 2305843009213693952))   ; ...and integer, not float
 (= 2305843009213693952 (/ (* 2 2305843009213693952) 2))           ; box / 2 demotes
 (= 2305843009213693952 (- (* 2 2305843009213693952) 2305843009213693952)) ; box - fix demotes
 (? word (charm? (- (* 2 2305843009213693952) 2305843009213693952)) 1) ; ...to a fixnum (64-bit: 2^61 is in range)
 (= (* 2 2305843009213693952) (* 2 2305843009213693952))           ; equal boxes compare =

 ; --- 1/0 promotes to IEEE inf / -inf; 0/0 collapses to 0 (NaN is gone) ---
 ; the result is a real, not a fixnum; inf is not equal to any finite value.
 (gem? (/ 1 0))
 (gem? (/ -1 0))
 (gem? (/ 0 0))                 ; 0/0 is 0.0 -- a real zero, not NaN
 (gem? (/ 1.0 0.0))
 !(= 0 (/ 1 0))
 (= 0 (/ 0 0))                  ; ... and equal to 0
 ; +inf is greater than every finite double we can write (f64 only -- a 32-bit
 ; port's f32 reals overflow 1e308 to inf, so the comparison is vacuous there)
 (? word (< 1e308 (/ 1 0)) 1)
 ; -inf is less than every finite double
 (? word (< (/ -1 0) -1e308) 1)
 ; 0/0 is 0, so it equals itself (NaN's non-reflexivity is gone)
 (= (/ 0 0) (/ 0 0))

 ; --- rem via fmod, works on floats and mixed ---
 (= 1.0 (% 7.0 2.0))
 (= 1 (% 7 2))
 (= 0.0 (% 7.5 2.5))
 (= -1.0 (% -7.0 2.0))
 ; % by zero is NaN (via fmod's identity), expressed as a real
 (gem? (% 7 0))
 (gem? (% 7.0 0.0))

 ; --- + is generic: string/list concat, order-preserving; - stays numeric (nil) ---
 ; MIXED BANDS DEGENERATE (the associativity law, 2026-08-16): a number against a
 ; string or a chain arrives as that band's UNIT, so the other operand answers
 ; whole -- the byte law and the element-adjoin law could never associate and are
 ; gone. (string c) and (l + [x]) are their lawful spellings.
 (= "x" (+ "x" 66))                 ; str + num -> the string (the number arrives as "")
 (= "x" (+ 66 "x"))                 ; num + str -> the same
 (= "x" (+ "x" 66.0))               ; ... a gem no differently
 (= "x" (+ "x" 66.9))
 (= "x" (+ "x" -66)) (= "x" (+ "x" 256))   ; no byte range: there is no byte
 (= "abcd" (+ "ab" "cd"))           ; str + str -> concat
 (= '(1 2 3 4) (+ '(1 2) '(3 4)))   ; list + list -> append
 (= '("x" 1 2) (+ "x" '(1 2)))      ; str + list -> (X str list)  [the charlist hom is the deferred rung]
 (= '(1 2 "x") (+ '(1 2) "x"))      ; list + str -> (append list (list str))
 (= '(1 2) (+ 5 '(1 2)))            ; num + list -> the list
 (= '(1 2) (+ '(1 2) 5))            ; list + num -> the list
 (nil? (- "a" "b"))

 ; --- equality with promotion across charm?/gem? ---
 (= 3 3)
 (= 3 3.0)
 (= 3.0 3)
 !(= 3 4)
 !(= 3 3.5)
 (= 1.5 1.5)

 ; --- ordered comparison with promotion ---
 (< 1 2)
 (< 1 1.5)
 (< 1.5 2)
 (< 1.5 2.5)
 !(< 2 1)
 !(< 1.5 1.0)
 (<= 3 3.0)
 (<= 3.0 3)
 (> 2 1.5)
 (>= 3.0 3)

 ; --- comparison is a TOTAL ORDER over all values: true-blue seats a string BELOW
 ;     numbers (mint < string < number < chain < lambda); full lattice in matrix.l ---
 (< "a" 1)
 !(< 1 "a"))
