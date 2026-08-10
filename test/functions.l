(: (f m n) 
    (: m_plus (+ m) n_plus (+ n)
              (g a) (? (< a 0) a (h (n_plus a)))
              (h a) (? (< a 0) a (g (m_plus a)))
              h)
   f3 (f -3)
   f37 (f3 -7)
 (assert (= -1 (f37 99)) (= -7 (f37 33))))

; applying a number is Church-numeral application: numeric operand -> exponentiation
; (operand ** operator), non-numeric operand -> the operator-fold composition.
(assert
 (= 2 (1 2))                                  ; (1 2) = 2 ** 1
 (= 1 (2 1))                                  ; (2 1) = 1 ** 2
 (= 9 ((\ f n (f n)) 2 3))                    ; (2 3) = 3 ** 2
 (= 1 ('abc 0))                               ; a symbol applies const-1: the name index left
 (= 1 ('abc 2))                               ;   with the string algebra (the mint round)
 (= 1 ('abc 9))                               ; every argument the same: a point is a unit
 (= 1 ('abc 'x))
 (= 1 ((mint 0) 5))                           ; the mint, the purest point, likewise
 (= 8 (3 2))                                  ; (3 2) = 2 ** 3
 (= 13 ((3 (+ 1)) 10)))                       ; non-numeric operand -> (+ 1) composed 3 times on 10
; the whole numeric tower is a Church numeral, not just fixnums (data.c data_num_apply
; routes float/box/complex/array/bignum operators through num-ap, like the odd-tag
; fixnum path). 0 applied to anything is 1 -- it *is* const-1, the zero numeral.
; the compose count is ($ n) -- SATURATING, true to the !!$ truth bits: a non-positive
; numeral acts as the zero numeral, a float ceils, a complex/array its saturated net.
(assert
 (= 8 (3 2.0))                                ; float operand: 2.0 ** 3 = 8.0, = bridges to 8
 (= 9 (2.0 3))                                ; float operator: 3 ** 2.0 via real power = 9.0
 (= 13 ((2.5 (+ 1)) 10))                      ; float operator on a fn: ($ 2.5)=3 compositions
 (= 10 (((- 0 2.5) (+ 1)) 10))                ; saturation first: ($ -2.5)=0 -> the zero numeral
 (= 10 ((-3 (+ 1)) 10)) (= 1 (-3 (+ 1)))      ; (-3 f) = (0 f) = 1 -- negatives have no church content
 (= 10 ((0 (+ 1)) 10))                        ; (0 f) = 1 = identity numeral -> 10 unchanged
 (= 1 (0 'anything))                          ; 0 is const-1: (0 x) = 1 for every x
 (= 1 ((cask 4) 'anything))                 ; opaque handles (cask/port) behave as 0 too
 (aall (= (2 (star-tray '(3) '(2 3 4))) (star-tray '(3) '(4 9 16))))  ; array operand: elementwise ** broadcast
 (aall (= ((star-tray '(3) '(2 3 4)) 5) (gem-tray '(3) '(25.0 125.0 625.0))))  ; array OPERATOR: 5 ** [2 3 4] elementwise (vmap2 power)
 (= 15 (((star-tray '(2) '(2 3)) (+ 1)) 10))   ; array operator on a fn: compose ($ [2 3]) = 2+3 = 5, the net sum
 (= 13 ((~(3 4) (+ 1)) 10))               ; complex operator on a fn: compose ($ ~(3 4)) = 3, the real part
 (= 10 ((~(-3 4) (+ 1)) 10))              ; ... but a complex sorting negative saturates to 0
 (twin? (~(0 1) 5))                       ; complex operator on a number -> complex power (5^i), no longer fenced
 (< (abs (- 1 (abs (~(0 1) 5)))) 0.0001)  ; |5^i| = 1 exactly (w^z = exp(z Log w))
 (< (abs (im (~(0 1) ~(0 1)))) 0.0001)) ; i^i = e^(-pi/2) is real
; basic arithmetic
(: zero? (= 0)
(assert
 (= 17 ((\ f (f 9)) (+ 8)))
 (= 13 (: a 9 b 7 c -3 (+ a (+ b c))))
 (: (even? n) (? (zero? n) 1 (odd? (+ -1 n)))
    (odd? n) (? (zero? n) 0 (even? (+ -1 n)))
  (odd? 99))

 (= -2 (^ 1 -1))
 (= 15 (| 8 (| 4 (| 2 1))))
 (= 16 (>> 64 2))
 (= 16 (<< 2 3))))

; ackermann function
(: (ack m n p) (?
  (= 0 p) (+ m n)
  (= 0 n) (? (= 1 p) 0
             (= 2 p) 1
             m)
  (ack m (ack m (+ -1 n) p) (+ -1 p)))
 (assert (= 65536 (ack 2 3 3))))
; tarai function
(: (たらい x y z)
   (? (<= x y) y (たらい (たらい (- x 1) y z)
                         (たらい (- y 1) z x)
                         (たらい (- z 1) x y)))
 (assert (= 1013 (たらい 1012 1013 1014))))

(: (c1895 m n) (+ m (/ (* (+ m n) (+ m (+ n 1))) 2))
   (c1895_inv p) (:
    (f t n) (? (> (+ t n) p) (link t (- n 1)) (f (+ t n) (+ n 1)))
    x (f 0 1)
    m (- p (cap x))
    n (- (cup x) m)
    `(m n))
   p (c1895 103 110)
   l (c1895_inv p)
   (assert (= 103 (cap l)) (= 110 (cap (cup l)))))

; heron's method for finding square roots (integer Newton -- floor division `//`,
; so it converges to the exact integer root and the `=` test terminates)
(: (heron x g) (? (= g (// x g)) g
    (heron x (// (+ g (// x g)) 2)))
   guess 1337
 (assert (= 6 (heron 36 guess))))

; church numerals
(: (add a b f x) (a f (b f x))
   (mul a b f) (a (b f))
   (zero a b) b
   one (zero zero)
   two (add one one)
   three (add one two)
   four (add two two)
   five (add two three)
   six (mul two three)
   seven (add one six)
 (assert (= 420 (mul (mul two five) (mul six seven) (+ 1) 0))))

; fibonacci
(: (fib n) (? (> 3 n) 1 (+ (fib (+ -1 n)) (fib (+ -2 n))))
 (assert (= 2178309 (fib 32))))
