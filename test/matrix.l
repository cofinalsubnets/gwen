; === the generic-op dispatch matrices, walked cell by cell ====================
; l's generic dyadic ops are NxN ap matrices indexed by the kind lattice
; (l.h enum q): kcharm | ksun kgem ktwin (the lean scalar-star boxes) | KBig |
; ktrayZ ktrayR ktrayC ktrayO (the tray tower -- ONE rep, DTray, refined by element
; tier) | kstring kmint | kchain(chain) | ktablet khot.
; a monadic op is that matrix's diagonal. Three matrices are explicit tables in
; l.c -- g_add_mx (+), g_mul_mx (*), g_apply_mx (apply); the rest (- / %
; < <= = >= >) are lane aps that refine the same lattice internally.
; This file holds the fine within-category cells: fix vs box/float/complex vs
; bignum, the four array tiers, the rep pairings a coarse law folds together. The
; coarse cell of each row -- what the band law already says -- is spec.l's, and
; the church-numeral rows are functions.l's; a row that is wholly theirs is a
; pointer here, not a copy. So a dispatch regression (e.g. a data-sentinel stride
; desync that mis-routes whole apply rows) still fails loudly, without the corpus
; asserting one cell twice.

; one representative value per kind -- the legend a row is read against. The three
; scalar-star boxes are distinct kinds (KGem/KTwin/KSun) but share one apply cell.
(: mx-fix 5                              ; kcharm
   mx-real 2.5                            ; kgem, the gem (float box)
   mx-cpx (2 ~ 3)                       ; ktwin, complex scalar (Cp)
   mx-box (<< 1 62)                      ; ksun, the sun (wide int)
   mx-big (100 2)                     ; KBig
   mx-az  @(10 20)                       ; ktrayZ (int tray)
   mx-ar  @(1.5 2.5)                     ; ktrayR (float tray)
   mx-ac  (array 2 (1 ~ 2) (3 ~ 4))    ; ktrayC (packed complex tray)
   mx-ao  (array 2 (100 2) 5)         ; ktrayO (object tray)
   mx-str "ab"                           ; kstring
   mx-sym 'ef                            ; kmint (interned)
   mx-lst '(1 2)                         ; kchain (chain / list)
   mx-fn  (+ 1)                          ; khot (nif partial application)
   mx-map (pin (pin (tablet 0) 1 10) 2 20)) ; ktablet (a tablet is a lookup-lambda)

; ============================================================================
; apply matrix: g_apply_mx[applied data kind = g_typ(v)][arg kind = g_kind(x)]
; ============================================================================
; Each data kind's sentinel tail-jumps through this. The arg-kind dimension is
; uniform today (a link for later), so we vary it to pin that. The index/append
; laws themselves are spec.l's (chains, strings, points); the church rows are
; functions.l's whole numeric tower. What is left here is the cells no band law
; reaches.
(assert
 ; --- row kchain: a chain is a sequence, the text law one lattice rung up. it was the
 ;     Church eliminator ((p f) == (f (a p) (B p))) until the tree was checked and nothing
 ;     at all applied a chain to a function -- so a lambda takes the default action, and
 ;     that is the cell worth pinning. the index/append rows are spec.l's chains section. ---
 (nil? ((link 1 2) 1))                            ; improper chain: the spine is one cell, so 1 is oob
 (nil? ('(1 2) (\ a b [a b])))                   ; arg = a lambda: the default action, not an eliminator
 (nil? ('(1 2) "ab"))                            ; ... as for every other kind

 ; --- row kstring: the byte index is spec.l's; the width and the empty row are here ---
 (= 255 ((string 255) 0))                     ; byte is unsigned 0..255
 (nil? ("" 5)) (nil? ("" -1))                   ; the empty string indexes to 1

 ; --- row kmint: a point applies as every unit does, const-1 -- spec.l's mints section
 ;     says it for both the bare mint and the named symbol, so the row is a pointer.

 ; --- the scalar-star + array rows (KGem/KTwin/KSun/KTray*, all -> data_num_apply):
 ;     a star or tray is a Church numeral (operand ** operator, or operator-fold over a
 ;     function). The whole tower, saturation included, is functions.l's. ---

 ; --- row KBig: a bignum is also a Church numeral, but its magnitude is always
 ;     astronomical (>= 2^62), so (bignum x) cannot terminate in a finite test.
 ;     The row exists (-> data_num_apply, same as the tray tiers) but is not exercised. ---

 ; --- non-matrix apply paths: the charm numeral, the lambda and the cask take their own
 ;     routes and are said elsewhere; the nif partial and the tablet miss are here ---
 (= 7 (((+) 3) 4))                            ; khot: a nif fired through the unit call
 (nil? (mx-map 9)))                           ; ktablet: a missing key -> nil, the zero point

; ============================================================================
; `+` matrix (g_add_mx): the fine within-category cells the spec's band laws fold together
; ============================================================================
; addn lane (numeric tower & arrays): the matrix routes here for any arith x
; arith chain; the lane then refines by representation. Each rep pairing:
(assert
 ; --- scalar tower: fix / float / wide int / bignum / complex, every mix ---
 (= 7 (+ 5 2))                                ; fix + fix (overflow-free fast path)
 (= 7.5 (+ 5 2.5))   (= 7.5 (+ 2.5 5))        ; fix + float (both orders)
 (= 5.0 (+ 2.5 2.5))                          ; float + float
 (? word (= (63 2) (+ (<< 1 62) (<< 1 62))) 1)   ; box + box -> 2^63 (64-bit word only)
 (? word (big? (+ (<< 1 62) (<< 1 62))) 1)
 (? word (sun? (+ (<< 1 62) 1)) 1)              ; box + fix stays a box
 (? word (= (+ (<< 1 62) 1) (+ 1 (<< 1 62))) 1) ; ...and commutes
 (= (+ 1 (100 2)) (+ (100 2) 1))        ; big + fix == fix + big (commutes)
 (= (* 2 (100 2)) (+ (100 2) (100 2))) ; big + big
 (gem? (+ (100 2) 0.5))                    ; big + float -> float (widens)
 (= (4 ~ 6) (+ (1 ~ 2) (3 ~ 4)))           ; complex + complex
 (= (7 ~ 3) (+ 5 (2 ~ 3)))                  ; fix + complex (real -> (5,0))
 (= (4.0 ~ 3.0) (+ 1.5 (2.5 ~ 3)))          ; float + complex
 (? word (twin? (+ (<< 1 62) (0 ~ 1))) 1)        ; box + complex -> complex (64-bit word only)
 (twin? (+ (100 2) (0 ~ 1)))                 ; big + complex -> complex (widens)

 ; --- array tower: each tier with a matching array and with a scalar broadcast ---
 (= gem-tray (tier (+ @(10 20) @(1.5 2.5))))      ; Z + R -> widens to R
 (aall (= @(11.5 22.5) (+ @(10 20) @(1.5 2.5))))
 (aall (= @(3.0 5.0) (+ @(1.5 2.5) @(1.5 2.5)))) ; R + R
 (= (2 ~ 4) (peep (+ mx-ac mx-ac) 0 0))       ; C + C (packed complex)
 (= top-tray (tier (+ mx-ao mx-ao)))                ; O + O stays exact/object
 (= (* 2 (100 2)) (peep (+ mx-ao mx-ao) 0 0))
 (aall (= @(12 22) (+ @(10 20) 2)))           ; Z + scalar (broadcast, no widen)
 (= (11 ~ 2) (peep (+ (10 ~ 0) mx-ac) 0 0))  ; C array + complex scalar
 (= (11 ~ 2) (peep (+ @(10 20) mx-ac) 0 0))   ; R/Z array + C array (real promotes)

 ; --- add_string lane: text x text; a number (tray included) arrives as the unit
 ;     and the text answers whole. the crossings the spec states once, the orders
 ;     it does not, and the reps it has no room for: ---
 ("efab" = (+ 'ef "ab"))                       ; sym + str demotes (the rank min pulls to string)
 (= "ab" (+ @(1 2) "ab"))   (= "ab" (+ "ab" @(1 2)))   ; a tray is number-band against text, either side
 ('ef = (+ @(1 2) 'ef))
 (= "ab" (+ mx-big "ab"))                     ; a bignum is a number like any other

 ; --- add_seq lane: a list against text (chain wins); a number degenerates ---
 (= '(1 2) (+ '(1 2) 5))                      ; list <-> num, the order the spec leaves open
 (= '(97 98 1 2) (+ "ab" '(1 2)))             ; str <-> list: the bytes splice (the charlist hom)
 (= "(7 8)" (show (+ @(1 2) '(7 8))))         ; array <-> list: the tray is number-band, it degenerates

 ; --- addl lane: a lambda operand anywhere -> church-numeral add (not pointwise):
 ;     (m + n) a x == m a (n a x), so the result is a function awaiting (f x).
 ;     "1" + "1" == "2": apply (+ 1) twice to 0. ---
 (= 2 (((+ (\ f x (f x)) (\ f x (f x))) (+ 1)) 0))
 (lit? (+ (+ 1) 2))   (lit? (+ 2 (+ 1)))      ; num + fn (either order) -> a function
 (lit? (+ (+ 1) (+ 1))))                      ; fn + fn -> a function

; ============================================================================
; `*` matrix (g_mul_mx): the multiplicative analog (semiring product of `+`)
; ============================================================================
; muln (arith), mul_rep (seq x count), lvm_0 (seq x seq -> nil), mull (compose).
(assert
 ; --- muln: scalar tower + arrays ---
 (= 6 (* 2 3))                                ; fix * fix
 (= 7.5 (* 3 2.5))                            ; fix * float
 (? word (= (124 2) (* (<< 1 62) (<< 1 62))) 1) ; box * box -> bignum (64-bit word only)
 (= (200 2) (* (100 2) (100 2)))     ; big * big
 (= -1 (* i i))                               ; complex * complex (i^2, bridges to real)
 (= (-5 ~ 10) (* (1 ~ 2) (3 ~ 4)))         ; Gaussian product
 (= (4 ~ 6) (* 2 (2 ~ 3)))                  ; fix * complex
 (aall (= @(20 60) (* @(10 30) 2)))           ; Z array * scalar
 (= (2 ~ 4) (peep (* (2 ~ 0) mx-ac) 0 0))    ; complex scalar * C array
 (= top-tray (tier (* mx-ao mx-ao)))                ; O * O exact
 (= (80 2)                                 ; 2^40 * 2^40, exact (an z array would wrap)
    (peep (* (top-tray '(2) [(40 2) (40 2)]) (top-tray '(2) [(40 2) (40 2)])) 0 0))

 ; --- mul_rep: the repeat law and its |count| are the spec's; the cells here are the
 ;     ones no band law reaches -- the reversed order, and a non-number count ---
 (= "ababab" (* 3 "ab"))                      ; str * count commutes
 (nil? (* "ab" @(1 2)))   (nil? (* @(1 2) "ab"))   ; an array is no count -> nil

 ; --- seq * seq: chain x chain is the cartesian product (spec.l); string stays nil,
 ;     and so does symbol, which the string lane might have been read to cover ---
 (nil? (* 'a 'b))

 ; --- mull: a lambda operand anywhere -> composition (Church mul) ---
 (= 36 ((* (\ x (* x x)) (\ x (+ x 1))) 5))   ; sq(inc 5) = sq 6 = 36
 (= 26 ((* (\ x (+ x 1)) 2) 5))               ; (inc (2 5)) = inc 25 = 26 (numeral then inc)
 (= 10 ((* mx-map (\ x (- x 1))) 2)))         ; a map composes too: m(2-1) = m 1 = 10

; ============================================================================
; the other dyadic generic ops (lane aps, not explicit tables): -, /, %,
; comparisons, =. Same lattice; these had no consolidated matrix test.
; ============================================================================
(assert
 ; --- subtraction `-` is numeric only (text/list/lambda operands -> nil) ---
 (= 3 (- 5 2))   (= 2.5 (- 5 2.5))            ; fix/float tower
 (= (100 2) (- (+ (100 2) 5) 5))        ; bignum
 (= (-2 ~ -2) (- (1 ~ 2) (3 ~ 4)))         ; complex
 (aall (= @(9 18) (- @(10 20) @(1 2))))       ; array elementwise
 (nil? (- '(1) '(2)))                         ; non-numeric -> nil

 ; --- division: `/` true (float on inexact), `//` truncating; array broadcast; complex defined ---
 (= 2.5 (/ 5 2))   (= 2 (// 5 2))   (= 2.5 (/ 5 2.0))   ; / promotes, // truncates
 (= 32 (/ (100 2) (95 2)))              ; bignum exact (2^5) stays integer
 (gem? (/ 1 0))                               ; /0 -> IEEE inf (float)
 (aall (= @(5 10) (/ @(10 20) 2)))            ; array / scalar
 (nil? (/ "a" 2))                             ; non-numeric -> nil

 ; --- modulo `mod` (fmod on floats); undefined on complex (-> nil) ---
 (= 1 (% 5 2))   (= 1.0 (% 7.0 2.0))
 (= 2 (% (100 2) 7))                     ; bignum mod fixnum
 (nil? (% (1 ~ 2) 2))                      ; complex -> nil
 (nil? (% "a" 2))                           ; non-numeric -> nil

 ; === ordered comparison (< <= > >=): a total order over all values. The bands -- points at
 ; the floor, then string, then the numbers, then tray < chain < map < top -- are spec.l's
 ; order section. Here: the reps that interleave within the number band, the complex rule,
 ; the lambda class, and the array operand, which broadcasts to a mask rather than ordering.
 ; `> >=` are `< <=` with operands reversed (a > b == b < a).
 ; --- within number: the whole tower orders by value (reps interleave) ---
 (< 1 2)   (< 1.5 2)                                    ; fix / float
 (? word (< (<< 1 62) (* 3 (<< 1 62))) 1)               ; box < box (64-bit word only)
 (< 5 (100 2))   (? word (< (<< 1 62) (100 2)) 1)  ; fix/box < bignum (magnitude-exact)
 (< (100 2) (101 2))                             ; bignum < bignum
 ; --- complex: lexicographic (re, im); a real r is (r, 0) ---
 (< i 1)   (< (1 ~ 9) (2 ~ 0))   (< (2 ~ 1) (2 ~ 2))   ; re decides, then im
 !(< (2 ~ 2) (2 ~ 2))   (<= (2 ~ 2) (2 ~ 2))    ; equal complexes
 ; --- the band crossings the spec's line does not carry a rep for ---
 (< "" 1.5)   (< 'z (100 2))   (< "x" (1 ~ 1))     ; any string < any number
 !(< '(0) 'z)                                     ; ...and a chain is above a point
 ; --- lambda is the top class; within it, ordered by representation hash ---
 (< 5 (+ 1))   (> (+ 1) '(0))   (< "z" (+ 1))         ; everything sorts below a lambda
 !(< (+ 1) (+ 1))   (<= (+ 1) (+ 1))   (>= (+ 1) (+ 1))   ; reflexive: a lambda equals itself
 (= (? (< (+ 1) (+ -1)) 1 0) (? (> (+ -1) (+ 1)) 1 0))    ; antisymmetric: a < b iff b > a
 (|| (<= (+ 1) (+ -1)) (>= (+ 1) (+ -1)))             ; total: some order holds between distinct fns
 ; --- all four orderings agree (<= >= include equality; > is the strict reverse) ---
 (<= 3 3)   (>= 3 3)   !(< 3 3)   !(> 3 3)   (> 5 3)   (>= 5 3)
 (<= "ab" "ab")   (> 'b 'a)   (>= '(1) '(1))
 ; --- array operand: comparison is elementwise (a 0/1 mask), not the scalar order ---
 (aall (< @(1 2) 10))   (= star-tray (tier (< @(1 2) 10)))   ; the mask is an z array
 (= 1 (peep (< @(1 5) @(3 2)) 0 0))   (= 0 (peep (< @(1 5) @(3 2)) 1 0))
 (aall (<= @(1 2) @(1 2)))   (aall (>= @(2 2) @(1 2)))
 (nil? (< @(1 2) "x"))                                 ; array vs non-number -> nil
 (nil? (< mx-ac mx-ac))                                ; a complex array is unordered -> nil
 ; --- 0/0 collapses to 0, so it is ordered like any zero (no NaN to be unordered) ---
 (nil? (< (/ 0 0) (/ 0 0)))   (<= (/ 0 0) (/ 0 0))   (nil? (> (/ 0 0) 1))

 ; --- equality `=`: bridges the numeric tower, else eql. the array rows are spec.l's ---
 (? word (= (<< 1 62) (<< 1 62)) 1)            ; box = box (64-bit word only)
 (? word !(= (<< 1 62) 0) 1)                ; ...never = a fixnum
 (= (100 2) (100 2))                    ; bignum = bignum
 (= (2 ~ 0) 2)                               ; complex bridges a real (im 0)
 !(= (2 ~ 1) 2)                           ; ...but not when im != 0
 (= 'ef 'ef)   !(= 'ef 'gh)                ; symbol identity
 (= "ab" "ab")                                ; string structural eq
 (= '(1 2) '(1 2))                            ; list structural eq
 !(= "ab" 'ab))                            ; cross-kind -> not equal

; --- inner (+.x contraction) & outer (o.x) products ---------------------------
(assert
 (11.0 = (inner @(1.0 2.0) @(3.0 4.0)))            ; float dot
 (11.0 = (inner @(1 2) @(3.0 4.0))) (gem? (inner @(1 2) @(3.0 4.0)))  ; mixed z.r widens to r
 (: A (star-tray '(2 3) '(1 2 3 4 5 6)) B (star-tray '(3 2) '(7 8 9 10 11 12)) C (inner A B)
    (&& ('(2 2) = (shape C)) (58 = (peep C '(0 0) 0)) (64 = (peep C '(0 1) 0))
        (139 = (peep C '(1 0) 0)) (154 = (peep C '(1 1) 0))))   ; 2x2 matmul, every cell
 (aall (@(6 15) = (inner (star-tray '(2 3) '(1 2 3 4 5 6)) @(1 1 1))))   ; matrix . vector
 (: O (outer @(1 2 3) @(10 20))
    (&& ('(3 2) = (shape O)) (10 = (peep O '(0 0) 0)) (60 = (peep O '(2 1) 0))))  ; outer
 (gem-tray = (tier (outer @(1 2) @(3.0 4.0))))     ; outer widens on an inexact operand
 !(inner @(1 2) 5) !(outer @(1 2) 5))              ; non-array operand -> nil
