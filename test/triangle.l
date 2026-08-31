; triangular numbers, the long way -- and the blue floor that doubles the zero.
;
;   (sat (jot k)) sums 0 1 .. k-1, so it is the (k-1)th triangular number (jot is
;   0-origin). map it over (jot n) and the first two entries are 0: jot 0 = () is
;   the empty sum, jot 1 = (0) a lone zero, and nothing and 0 share one measure --
;   the blue floor. so the triangle opens  0 0 1 3 6 ..  and 1, the one you would
;   call first, lands third.

(: tri (flip compose jot (map (compose sat jot)))             ; n -> the first n partial sums of jot
   (assert
    ('(0 0 1 3 6 10 15 21 28 36 45 55) = (tri 12))   ; T(k-1); the double zero is the floor
    (0 = (sat (jot 0))) (0 = (sat (jot 1)))          ; () and (0): nothing == 0, both blue
    (1 = (sat (jot 2)))                              ; then 1, the first triangle, at k=2

    ('(0 1 3 6)  = (map (compose sat jot) (map (+ 1) (jot 4))))    ; from jot 1: a single zero
    ('(1 3 6 10) = (map (compose sat (compose jot (+ 2))) (jot 4)))))   ; from jot 2: 1 comes first
