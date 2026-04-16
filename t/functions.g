; basic arithmetic
(: 0? (= 0)
(assert
 (= 17 ((\ f (f 9)) (+ 8)))
 (= 13 (: a 9 b 7 c -3 (+ a (+ b c))))
 (: (even? n) (? (0? n) -1 (odd? (dec n)))
    (odd? n) (? (0? n) 0 (even? (dec n)))
  (odd? 99))

 (= -2 (~ 1))
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
   (c1895~ p) (:
    (f t n) (? (> (+ t n) p) (X t (- n 1)) (f (+ t n) (+ n 1)))
    x (f 0 1)
    m (- p (A x))
    n (- (B x) m)
    (L m n))
   p (c1895 103 110)
   l (c1895~ p)
   (assert (= 103 (A l)) (= 110 (A (B l)))))

; heron's method for finding square roots
(: (heron x g) (? (= g (/ x g)) g
    (heron x (/ (+ g (/ x g)) 2)))
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
 (assert (= 420 (mul two (mul five (mul six seven)) (+ 1) 0))))
