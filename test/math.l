; transcendentals + sqrt + power + pi.
; host uses libm; precision is ~1 ulp so identities don't need a slop band
; except where two errors compound (e.g. (sine pi) is ~1e-16, not exactly 0).

(: (near? a b eps) (< (- (? (< a b) (- b a) (- a b)) 0) eps)
   (near a b)       (near? a b (? word 1e-12 1e-5))   ; f32 reals (wasm) need a looser eps
   (very-near a b)  (near? a b (? word 1e-15 1e-5))

   (assert
    ; --- pi from boot.l, basic identities ---
    (= 1.0 (cosine 0))
    (= 0.0 (sine 0))
    (= -1.0 (cosine pi))
    (near 0 (sine pi))
    (very-near 1.0 (sine (/ pi 2)))
    (very-near 0 (cosine (/ pi 2)))

    ; --- pythagorean identity ---
    (near 1.0 (+ (* (sine 1.234) (sine 1.234))
                  (* (cosine 1.234) (cosine 1.234))))
    (near 1.0 (+ (* (sine (- 0 0.7)) (sine (- 0 0.7)))
                  (* (cosine (- 0 0.7)) (cosine (- 0 0.7)))))

    ; --- atan inverse of tan in (-pi/2, pi/2); tan is now (/ (sine x) (cosine x)) ---
    (near 0.7 (arg ~(1 (/ (sine 0.7) (cosine 0.7)))))

    ; --- atan2 quadrants ---
    (very-near 0.0 (arg ~(1 0)))
    (very-near (/ pi 2) (arg ~(0 1)))
    (very-near pi (arg ~(-1 0)))
    (very-near (- 0 (/ pi 2)) (arg ~(0 -1)))
    (very-near (/ pi 4) (arg ~(1 1)))

    ; --- sqrt ---
    (= 0.0 ((/ 1 2) 0))
    (= 1.0 ((/ 1 2) 1))
    (= 2.0 ((/ 1 2) 4))
    (near 1.414213562373095 ((/ 1 2) 2))
    ; (sqrt (* x x)) ~ x for x > 0
    (near 7.5 ((/ 1 2) (* 7.5 7.5)))

    ; --- exp / log inverse ---
    (near 1.0 (0 e))
    (near 2.718281828459045 (1 e))
    (near 0 (log 1))
    (near 1.0 (log (1 e)))
    (near 5.5 (log (5.5 e)))

    ; --- power IS application: (n b) = b**n; a float numeral forces the float lane ---
    (= 1 (0 2))                  ; 2^0 (const-1)
    (= 2 (1 2))                  ; 2^1 (identity)
    (= 1024 (10 2))              ; 2^10 (exact, ipow)
    (near 0.5 (-1 2))           ; 2^-1 -> reciprocal
    (near 1.414213562373095 (0.5 2))   ; 2^0.5 = sqrt2 (the float lane)

    ; --- accepts fixnum args (promoted to float) ---
    (gem? (sine 0))
    (gem? ((/ 1 2) 4))
    (gem? (3.0 2))               ; 2^3.0 -> float

    ; --- non-numeric arg → nil ---
    ; (nil counts as numeric — it's putnum(0).)
    (nil? (sine "x"))
    (nil? (twin "x" 1))      ; twin of a non-number -> nil (so (arg (twin 1 "x")) folds to 0)

    ; --- generic over bignums: the operand widens full-magnitude (g_big_to_flo) ---
    (= 1125899906842624.0 ((/ 1 2) 1267650600228229401496703205376))  ; sqrt(2^100) = 2^50
    (gem? (2.0 1267650600228229401496703205376))   ; bignum^2.0 -> float
    (? word (< 1e300 (1e20 2)) 1)              ; 2^1e20, f64 only -- 1e300 is inf in f32

    ; --- power elementwise over arrays (numpy broadcast); float lane -> r ---
    (= gem-tray (tier (2.0 (star-tray '(2) '(2 3)))))                  ; array^2.0 -> float array
    (aall (= (2 (gem-tray '(3) '(1 2 3))) (gem-tray '(3) '(1 4 9))))  ; array ^ scalar
    (aall (= ((gem-tray '(3) '(1 2 3)) 2) (gem-tray '(3) '(2 4 8))))  ; scalar ^ array (array numeral)
    (aall (= ((gem-tray '(3) '(2 2 2)) (gem-tray '(3) '(1 2 3)))      ; array ^ array
             (gem-tray '(3) '(1 4 9))))
    (near 1.570796326794897 (peep (arg ~((gem-tray '(2) '(1 0)) (gem-tray '(2) '(1 1)))) 1 0))

    ; --- bignum meets array (arithmetic): the array wins, the bignum demotes to
    ;     the array's element form -- full magnitude in a float array, low bits in
    ;     an int array (modular, so it wraps: 2^64 + 5 contributes 5). Comparison
    ;     against a bignum stays magnitude-exact -- see test/bignum.l ---
    (aall (= (+ (star-tray '(2) '(1 2)) 18446744073709551621)        ; 2^64 + 5 -> low word 5
             (star-tray '(2) '(6 7))))
    (< 1e19 (peep (+ (gem-tray '(2) '(0 0)) 18446744073709551616) 0 0))))  ; 2^64 full magnitude
