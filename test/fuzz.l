; property-based fuzzer built on the step-8 RNG (xoshiro256++ functional stream).
;
; a GENERATOR is a function st -> (value . st'), the shape a curried (random st _) yields, so
; generators thread the random state purely and compose by plumbing the returned
; st onward. fz-find drives a generator N times against a predicate from a fixed
; seed (so any failure is reproducible) and reports the first counterexample;
; fz-ok is the boolean wrapper the asserts use. The properties below are
; invariants that must hold for EVERY draw -- algebraic laws of the numeric tower,
; list-combinator identities, the print->read round trip, and array broadcasting.
;
; fz-find returns (X 1 0) on success or (X 0 counterexample) on failure;
; to debug a red property interactively, eval (show (B (fz-find n seed g p)))
; to see the offending input.
(:
 ; --- generator monad: pure / map / bind over the functional RNG ---
 fz-pure link                          ; ignores the stream
 (fz-map f g) (\ st (: r (g st) (f <r >< >r)))
 (fz-bind g k) (\ st (: r (g st) ((k <r) >r))) ; feed value into next gen
 ; --- primitive draws (every range below keeps hi > lo so random never mods by 0) ---
 (fz-nat n) (\ st (random st n))                        ; [0, n)
 (fz-int lo hi) (fz-map (+ lo) (fz-nat (- hi lo)))       ; [lo, hi)
 (fz-elt l) (\ st (: r (random st (tally l)) (<(drop <r l) >< >r)))
 (fz-oneof gs) (fz-bind (fz-nat (tally gs)) (\ k <(drop k gs)))  ; pick a generator
 ; --- structured draws: fixed repetition, bounded-length list, tuples ---
 (fz-rep n g) (? (n < 1) (fz-pure ())
    (\ st (: a (g st) b ((fz-rep (n - 1) g) >a) ((<a >< <b) >< >b))))
 (fz-list mx g) (\ st (: r (random st (1 + mx)) ((fz-rep <r g) >r)))  ; pin in [0,mx]
 (fz-pair2 ga gb) (\ st (: a (ga st) b (gb >a) (`(<a <b) >< >b)))
 (fz-trip ga gb gc)
   (\ st (: a (ga st) b (gb >a) c (gc >b) (`(<a <b <c) >< >c)))
 ; --- the driver: N trials from seed `sd`, first counterexample wins ---
 (fz-find n sd g p)
   (: (go n st) (? (n < 1) (1 >< 0)
        (: r (g st) (? (p <r) (go (- n 1) >r) (0 >< <r))))
      (go n (wheel sd)))
 (fz-ok n sd g p) <(fz-find n sd g p))

; --- concrete value generators over the numeric tower ---
(: fz-fix    (fz-int -1000 1000)                         ; fixnums straddling 0
   fz-fix-nz (fz-map (\ x (? (x = 0) 1 x)) fz-fix)       ; nonzero fixnum (safe divisor)
   ; s * 2^e with e up to 160 forces the box / bignum tiers and both signs
   fz-bignum (fz-bind (fz-int 58 160) (\ e (fz-map (\ s (s * e 2)) (fz-elt '(-3 -1 1 3 5)))))
   fz-num    (fz-oneof `(fz-fix fz-bignum))             ; mixed-tier value
   fz-num-nz (fz-map (\ x (? (x = 0) 1 x)) fz-num)       ; nonzero mixed-tier (divisor)
   fz-elem   (fz-int -20 20)                             ; small list/array element
   fz-l      (fz-list 8 fz-elem))                        ; list of small ints, pin 0..8

; ---- numeric-tower algebra: laws that hold across fixnum / box / bignum ----
(assert
 ; commutativity of + and *
 (fz-ok 400 1 (fz-pair2 fz-num fz-num) (\ v (<v + <>v = <>v + <v)))
 (fz-ok 400 2 (fz-pair2 fz-num fz-num) (\ v (<v * <>v = <>v * <v)))
 ; associativity of + and *
 (fz-ok 300 3 (fz-trip fz-num fz-num fz-num)
   (\ v ((<v + <>v) + <>>v = <v + (<>v + <>>v))))
 (fz-ok 300 4 (fz-trip fz-num fz-num fz-num)
   (\ v ((<v * <>v) * <>>v = <v * (<>v * <>>v))))
 ; left distributivity: a*(b+c) = a*b + a*c
 (fz-ok 300 5 (fz-trip fz-num fz-num fz-num)
   (\ v (<v * (<>v + <>>v) = <v * <>v + <v * <>>v)))
 ; negation is involutive; subtraction anti-commutes
 (fz-ok 400 6 fz-num (\ a (a = 0 - (0 - a))))
 (fz-ok 300 7 (fz-pair2 fz-num fz-num) (\ v (<v - <>v = 0 - (<>v - <v))))
 ; multiplying by zero / one
 (fz-ok 300 8 fz-num (\ a (0 = a * 0)))
 (fz-ok 300 9 fz-num (\ a (a = a * 1)))
 ; divmod identity: a = (a//b)*b + a%b, and the remainder is smaller in magnitude
 (fz-ok 400 10 (fz-pair2 fz-num fz-num-nz)
   (\ v (<v = <v // <>v * <>v + (<v % <>v))))
 ; remainder sign follows the dividend (truncated division)
 (fz-ok 400 11 (fz-pair2 fz-fix fz-fix-nz)
   (\ v (: r (<v % <>v) (r = 0 || (r < 0) = (<v < 0)))))
 ; exact-power recurrence and the float-free modpow against ** then %
 (fz-ok 300 12 (fz-pair2 (fz-int -6 7) (fz-nat 14))
   (\ v ((1 + <>v) <v = (<v * <>v <v))))
 (fz-ok 250 13 (fz-trip (fz-nat 30) (fz-nat 14) (fz-int 2 500))
   (\ v (modpow <v <>v <>>v = <>v <v % <>>v)))
 ; gcd divides both arguments
 (fz-ok 300 14 (fz-pair2 fz-fix-nz fz-fix-nz)
   (\ v (: g (gcd <v <>v) (0 = <v % g && 0 = <>v % g)))))

; ---- number-as-function application (Church numerals), deliberately bounded ----
; applying a fixnum n is Church-numeral application: a numeric operand m gives
; m ** n, a function operand g gives g composed n times. The OPERATOR is always
; drawn as a small non-negative fixnum (fz-op, 0..6) so neither the exponent nor
; the composition depth can run away -- the fuzzer must never turn a random draw
; into a 2^160-sized exponent. With fz-op bounded, every result here stays tiny
; (|m ** n| <= 4 ** 6), so these properties cannot loop or blow up the heap.
(: fz-op (fz-int 0 7))
(assert
 ; numeric operand: (n m) == m ** n  (exponent n bounded to <= 6)
 (fz-ok 300 15 (fz-pair2 fz-op (fz-int -4 5))
   (\ v (<v <>v = <v <>v)))
 ; function operand: ((n (+ k)) x) == x + n*k  (composition depth n bounded to <= 6)
 (fz-ok 300 16 (fz-trip fz-op (fz-int -3 4) (fz-int -10 10))
   (\ v (<v * <>v + <>>v = (<v (+ <>v) <>>v)))))

; ---- list-combinator identities ----
(assert
 (fz-ok 300 20 fz-l (\ l (l = rev (rev l))))                          ; rev is involutive
 (fz-ok 300 21 fz-l (\ l (l = map id l)))                            ; map id = id
 (fz-ok 300 22 fz-l (\ l (l = foldr link () l)))                     ; foldr X () = id
 ; cat adds spine lengths, and the NET law holds exactly: $ of a cat is the
 ; saturated sum of the two lists' element sums (one clamp, at the top)
 (fz-ok 300 23 (fz-pair2 fz-l fz-l)
   (\ v (: (ln x) (foldl (\ n _ (1 + n)) 0 x)
           (sum x) (foldl (+) 0 x)
           a <v b <>v
           (ln (a + b) = ln a + ln b && $(a + b) = $(sum a + sum b)))))
 (fz-ok 300 24 (fz-pair2 fz-l fz-l)
   (\ v (rev (<v + <>v) = rev <>v + rev <v)))  ; rev of cat
 ; map preserves spine length ($ is the net sum, so count the spine explicitly)
 (fz-ok 300 25 fz-l (\ l (: (ln x) (foldl (\ n _ (1 + n)) 0 x) (ln (map (+ 1) l) = ln l))))
 ; take n ++ drop n = identity, for n drawn within [0, spine length]
 (fz-ok 300 26 (fz-bind fz-l (\ l (fz-map (\ n `(l n)) (fz-int 0 (+ 1 (foldl (\ n _ (1 + n)) 0 l))))))
   (\ v (<v = take <>v <v + drop <>v <v)))
 ; filter keeps only matching elements and never grows the spine (the clamped $
 ; can GROW under filter -- dropping a negative raises the net -- so fold)
 (fz-ok 300 27 fz-l
   (\ l (: (ln x) (foldl (\ n _ (1 + n)) 0 x)
           p (\ x (0 = x % 2))
           (all p (filter p l) && ln (filter p l) <= ln l))))
 ; an element consed on is found by member?
 (fz-ok 300 28 (fz-pair2 fz-elem fz-l) (\ v (member? <v (<v >< <>v)))))

; ---- print -> read round trip on random s-expressions ----
; serialize via show, re-read the bytes, expect equality -- through rd-test, the
; reader-under-test socket (doc/misc/io.md rung 3).
(: (fz-s2cl s) ((: (g i) (? (i < tally s) (peep s i 0 >< g (1 + i)))) 0)
   (fz-rd x) (: r (rd-test (fz-s2cl (show x))) (? (two? r) <r r))   ; the reader-under-test socket (test/00-init.l)
   fz-syms '(a b c d foo bar baz qux)
   fz-leaf (fz-oneof `((fz-elt fz-syms) (fz-int -500 500)))
   (fz-sexp d) (? (< d 1) fz-leaf
      (\ st (: r (random st 3)            ; 1/3 leaf, 2/3 a bounded list of sub-exprs
          (? (<r = 0) (fz-leaf >r)
             ((fz-list 4 (fz-sexp (- d 1))) >r))))))
(assert
 (fz-ok 200 30 fz-leaf (\ x (x = fz-rd x)))            ; leaves: ints + symbols
 (fz-ok 200 31 (fz-sexp 3) (\ x (x = fz-rd x)))        ; nested structure round-trips
 (fz-ok 150 32 (fz-sexp 4) (\ x (x = fz-rd x))))

; ---- array broadcasting / reduction laws (z elementwise) ----
; fz-arrpair draws two z vectors of the same random length so elementwise ops line up.
(: fz-arrpair (fz-bind (fz-int 1 6) (\ n (\ st
    (: a ((fz-rep n fz-elem) st) b ((fz-rep n fz-elem) >a)
       (`((star-tray `(n) <a) (star-tray `(n) <b)) >< >b))))))
(assert
 (fz-ok 250 40 fz-arrpair (\ v (aall (<v + <>v = <>v + <v))))  ; + commutes
 (fz-ok 250 41 fz-arrpair (\ v (aall (<v * <>v = <>v * <v))))  ; * commutes
 ; sum is linear: sum(a+b) = sum(a) + sum(b)
 (fz-ok 250 42 fz-arrpair (\ v (+(<v + <>v) = +<v + +<>v))))

; ---- string operations: + / snip / string / get / print->read ----
; fz-str draws a random byte string (length 0..mx, any byte 0..255 -- show's
; \xHH escaping round-trips them all). snip returns nil for an empty snip
; (i==j), which + and the old scat treat as "" but `=` does not, so any comparison
; that can land on an empty snip routes through fz-norm (+ x "" coerces a
; nil/string to a string). fz-s2cl / fz-rd are the helpers from the round-trip
; section above (string->charlist, print->read).
(: fz-byte (fz-int 0 256)                                  ; any byte 0..255
   (fz-cl->str cl) (? (two? cl) (string cl) "")            ; charlist -> string ("" for nil)
   (fz-norm x) (? x x "")                                  ; nil|string -> string
   (fz-str mx) (fz-map fz-cl->str (fz-list mx fz-byte))    ; string, pin 0..mx
   fz-s    (fz-str 12)
   fz-s2   (fz-pair2 fz-s fz-s)
   fz-s3   (fz-trip fz-s fz-s fz-s)
   fz-cl1  (fz-bind (fz-int 1 12) (\ n (fz-rep n fz-byte))) ; nonempty charlist
   ; a string paired with a split point i in [0, pin]
   fz-s-i  (fz-bind fz-s (\ s (fz-map (link s) (fz-int 0 (1 + tally s)))))
   ; a string with 0 <= i <= j <= pin, as `(s i j)
   fz-s-ij (fz-bind fz-s (\ s (fz-bind (fz-int 0 (1 + tally s))
              (\ i (fz-map (\ j `(s i j)) (fz-int i (1 + tally s))))))))
(assert
 ; +: length adds, is associative, and "" is a two-sided identity
 (fz-ok 300 50 fz-s2 (\ v ($(<v + <>v) = $<v + $<>v)))
 (fz-ok 250 51 fz-s3 (\ v ((<v + <>v) + <>>v = <v + (<>v + <>>v))))
 (fz-ok 300 52 fz-s (\ s (s = "" + s)))
 (fz-ok 300 53 fz-s (\ s (s = s + "")))
 ; snip: any split reconcatenates to the original; the full snip is the string.
 ; gated: on 32-bit the RNG stream diverges (turn truncates the 64-bit word,
 ; filed), so these draw inputs that trip a real failure -- skip on the wasm shim.
 (? word (fz-ok 300 54 fz-s-i (\ v (<v = fz-norm (fz-norm (snip <v 0 >v) +
                                                  fz-norm (snip <v >v (tally <v)))))) 1)
 (? word (fz-ok 300 55 fz-s (\ s (s = fz-norm (snip s 0 (tally s))))) 1)
 ; the snip length is j-i, and out-of-range indices clamp to [0, pin]
 (fz-ok 300 56 fz-s-ij (\ v (<>>v - <>v = tally (snip <v <>v <>>v))))
 (fz-ok 300 57 fz-s (\ s (fz-norm (snip s 0 (tally s)) = fz-norm (snip s -9 (99 + tally s)))))
 ; snip recovers the two operands of a scat
 (fz-ok 250 58 fz-s2 (\ v (<v = fz-norm (snip (<v + <>v) 0 (tally <v)))))
 (fz-ok 250 59 fz-s2 (\ v (<>v = (fz-norm (snip (<v + <>v) (tally <v) (tally <v + tally <>v))))))
 ; string is identity on strings; charlist->string round-trips by bytes and length
 (fz-ok 300 60 fz-s (\ s (s = string s)))
 (fz-ok 250 61 fz-cl1 (\ cl (cl = fz-s2cl (string cl))))
 (fz-ok 250 62 fz-cl1 (\ cl (: (ln x) (foldl (\ n _ (1 + n)) 0 x) (ln cl = tally (string cl)))))
 ; get yields the default outside [0, pin)
 (fz-ok 300 63 fz-s (\ s ('oob = peep s (tally s) 'oob && 'oob = peep s -1 'oob)))
 ; print -> read round trip over arbitrary byte strings
 (fz-ok 250 64 fz-s (\ s (s = fz-rd s))))
