; step 6: arbitrary-precision integers (bignums).
; - the numeric tower closes: fixnum -> wide int -> bignum
; - + - * overflow promotes to an exact bignum; / % are truncated division
; - canonical demotion: any result that fits a smaller tier shrinks back to it
;   (so = / eqv / hash keys stay well defined across tiers)
; - reader parses oversized decimal literals exactly; printer is base-10
; - mixed fixnum/box/bignum/float arithmetic, comparison, equality

(: (bfact n) (? (< n 2) 1 (* n (bfact (- n 1))))
   (bpow2 n) (? (< n 1) 1 (* 2 (bpow2 (- n 1))))
   B100 (bpow2 100)
   F25 (bfact 25)
   F24 (bfact 24)
   ; two dense, exactly-equal-length (KL-limb) operands: ka*KBb takes the resumable
   ; subquadratic Karatsuba lane (equal length, KL^2 > the one-shot chunk). Cross-check
   ; it against an independent schoolbook path -- split ka = Kahi*2^h + Kalo (each ~KL/2
   ; limbs), so Kahi*KBb and Kalo*KBb are unequal length and stay on schoolbook.
   KL 150  KE (* 64 KL)  KHE (* 64 (// KL 2))
   KA (- (KE 2) 1)  KBb (- (KE 2) 7)  KHB (KHE 2)   ; (KE 2) = 2^ke via exact power (no deep recursion)
   KP (* KA KBb)
   Kref (+ (* (% KA KHB) KBb) (* (* (// KA KHB) KBb) KHB)))

(assert
 ; --- overflow promotion + exact value, read round-trip against a literal ---
 (= F25 15511210043330985984000000)
 (= B100 1267650600228229401496703205376)
 (= (* 1000000000000 1000000000000) 1000000000000000000000000)
 (= (+ F25 1) 15511210043330985984000001)
 (= (- F25 1) 15511210043330985983999999)
 (= (bpow2 200) (* B100 B100))            ; identity instead of a transcribed literal
 (= (bpow2 64) 18446744073709551616)

 ; --- resumable subquadratic Karatsuba (equal-length large *) vs independent schoolbook ---
 (= KP Kref)                              ; Karatsuba product == schoolbook reference
 (= KP (* KBb KA))                        ; commutes
 (= KA (// KP KBb))                       ; divides back exactly (cross-checks against Knuth-D)
 (= 0 (% KP KBb))
 (= (* KA KA) (* KA KA))                  ; squaring is stable (the na==nb self-multiply lane)

 ; --- a bignum is its own tier: not a fixnum, truthy, not falsy, not a chain ---
 (nil? (charm? B100))                       ; (charm? bignum) is false
 (? B100 1 0)                             ; a bignum is truthy
 (nil? (nil? B100))                       ; ...hence not falsy
 (atom? B100)

 ; --- canonical demotion: shrinking results fall back to the smallest tier ---
 (= 0 (- F25 F25))
 (charm? (- F25 F25))                       ; demoted all the way to a fixnum
 ; exact bignum division stays an integer; an inexact one promotes to a float (`/`)
 (= 25 (/ F25 F24))
 (charm? (/ F25 F24))
 (= F24 (/ F25 25))
 (= 1 (/ B100 B100))
 (charm? (/ B100 B100))
 (gem? (/ B100 7))                         ; 2^100 / 7 has a remainder -> float
 (= 25 (// F25 F24))                       ; floor division `//` truncates, stays integer

 ; --- divmod identities (floor division `//`): a == (a//b)*b + a%b ---
 (= B100 (+ (* (// B100 1000000007) 1000000007) (% B100 1000000007)))
 (= B100 (+ (* (// B100 (bpow2 40)) (bpow2 40)) (% B100 (bpow2 40))))   ; multi-limb divisor
 (= F25 (+ (* (// F25 F24) F24) (% F25 F24)))
 (= 2 (% B100 7))                         ; 2^100 mod 7

 ; --- signs: `//` truncates toward zero, remainder follows the dividend ---
 (= -25 (// (- 0 F25) F24))
 (= 25 (// (- 0 F25) (- 0 F24)))
 (= -25 (// F25 (- 0 F24)))
 (= -2 (% (- 0 B100) 7))
 (= 2 (% B100 (- 0 7)))
 (= -2 (% (- 0 B100) (- 0 7)))
 (= (- 0 B100) (* -1 B100))
 (= B100 (- 0 (- 0 B100)))
 (= (- 0 B100) -1267650600228229401496703205376)

 ; --- the lone INT_MIN/-1 case: -2^63 // -1 is the exact 2^63, %-1 is 0 ---
 (= (bpow2 63) (// (- 0 (bpow2 63)) -1))
 (= 0 (% (- 0 (bpow2 63)) -1))

 ; --- mixed-tier comparison (fixnum / box / bignum) ---
 (< F24 F25)
 (> F25 F24)
 (<= F25 F25)
 (>= F25 F25)
 (< 5 B100)
 (< (- 0 B100) 5)
 (< (- 0 B100) B100)
 (= F25 (bfact 25))                       ; two independent computations are equal
 !(= F25 B100)
 !(= B100 (+ B100 1))
 (< B100 (+ B100 1))

 ; --- mixed with floats: a float operand widens the bignum to double ---
 (gem? (+ B100 0.5))
 (gem? (* B100 1.0))
 (< B100 1e40)
 (> B100 1e20)
 (= 0 (* B100 0))                         ; * fixnum 0 -> exact 0
 (charm? (* B100 0))

 ; --- printer round-trip (base 10, with sign) ---
 (= "1267650600228229401496703205376" (show B100))
 (= "-1267650600228229401496703205376" (show (- 0 B100)))
 (= "9223372036854775808" (show (bpow2 63)))
 (= "15511210043330985984000000" (show F25))

 ; --- bignums as hash keys (eqv over slen+limbs) ---
 (= 'hit (peep (pin (pin (tablet 0) F25 'fk) B100 'hit) B100 'miss))
 (= 'fk (peep (pin (pin (tablet 0) F25 'fk) B100 'hit) F25 'miss))
 (= 'miss (peep (pin (tablet 0) B100 'hit) (+ B100 1) 'miss))

 ; --- prel ** / gcd / modpow: exact across the tower via * / % ---
 ; (`**` is exact-integer power; the float `power` nif is tested in math.l)
 (= 8 (3 2))                           ; small ints stay fixnums
 (charm? (3 2))                          ; ...and stay integers (not float power)
 (= B100 (100 2))                      ; promotes to a bignum
 (= (bpow2 200) (200 2))
 (= 1 (0 999))                         ; e <= 0 -> 1
 (= 1024 (10 2))
 (= 21 (gcd 1071 462))
 (= 21 (gcd 462 1071))                    ; order-independent
 (= 4 (gcd -12 8))                        ; result normalized non-negative
 (= 4 (gcd 12 -8))
 (= F24 (gcd F25 F24))                    ; f24 divides f25, so gcd is f24
 (= 1 (gcd B100 (+ B100 1)))              ; consecutive ints are coprime
 (= 976371285 (modpow 2 100 1000000007)) ; 2^100 mod (1e9+7)
 (= 1 (modpow 7 0 100))
 (= (% B100 1000000007) (modpow 2 100 1000000007))

 ; --- int array vs bignum scalar: ordered by the bignum's true magnitude/sign,
 ;     not its low bits. 2^100's low word is 0, so a low-bits comparison would
 ;     wrongly treat b100 as 0 -- these assertions pin the magnitude-exact path. ---
 (aall (< (star-tray '(3) '(1 2 3)) B100))          ; every int < 2^100
 (aall (<= (star-tray '(3) '(1 2 3)) B100))
 (nil? (aall (> (star-tray '(3) '(1 2 3)) B100)))   ; nothing exceeds 2^100
 (nil? (aall (>= (star-tray '(3) '(1 2 3)) B100)))
 (aall (> (star-tray '(3) '(1 2 3)) (- 0 B100)))    ; every int > -2^100
 (aall (< (- 0 B100) (star-tray '(3) '(1 2 3))))    ; bignum on the left, too
 (nil? (aall (= (star-tray '(2) '(0 0)) B100))))    ; 0 != 2^100 (low word is 0!)
