; test/feel.l -- the feel pre-pass: macro expansion (exercised throughout the suite)
; and constant folding. Folding is semantics-preserving, so whether it FIRED is only
; observable via baked-literal identity (a folded (X 1 2) is shared across calls) --
; which differs between the feel compiler and the c0/l0 control, so those positive
; "fires" checks can't live in this cross-compiler suite (they're verified out of band).
; What CAN be asserted here: fold correctness, and that identity-minting heads stay
; excluded -- (tablet 0) must yield a fresh hash on every call under BOTH compilers.
(assert
 ; --- impure / identity-minting heads are NOT folded (fresh each call) ---
 (: (f _) (tablet 0) (nil? (id? (f 0) (f 0))))    ; `table` excluded -> distinct hashes

 ; --- arithmetic / numeric folds are correct (nested, bottom-up) ---
 (= 7  (+ 1 (* 2 3)))
 (= 4096 (* 2 3 4))                              ; binary * over-applied: ((* 2 3) 4) = (6 4) = 4 ** 6
 (= 8  (3 2))
 (= 6  (gcd 24 18))
 (= 6  ((+ 1) 5))                                ; (+ 1) -- a curried pure global folds

 ; --- X / A / B fold; element access is correct ---
 (= 9  (cap (link 9 8)))
 (= 8  (cup (link 9 8)))
 (= 2  (caup '(1 2 3)))
 (= 4  (tally (+ "ab" "cd")))                 ; string built + counted at compile time

 ; --- a TEXT head folds its KNOWN PREFIX: pure and total at every operand ---
 (= "abcd"   ("ab" "cd"))                     ; juxtaposition
 (= "abcdef" ("ab" "cd" "ef"))                ; ... curried, folded left to right
 (= "abcd"   (: a "ab" b "cd" (a b)))         ; ... through const-propagated binders, no literal in sight
 (= 97 ("abc" 0)) (= 1 ("hi" 9))              ; the byte index and its out-of-range unit
 (= "abcdef"   (: (f x) ("ab" "cd" x) (f "ef")))       ; known PREFIX folds, the unknown tail rides on
 (= "abcdefgh" (: (f x) ("ab" "cd" x "gh") (f "ef")))  ; ... and the operands after it still apply in order
 (= "abZcd"    (: (f x) ("ab" x "cd") (f "Z")))        ; nothing known past the head -> wholly residual
 (= "xy" (: (f s) (s "y") (f "x")))           ; an UNKNOWN head stays residual, and still joins at run time

 ; --- ... and the walk is the head VALUE's purity, not text: a point and a numeral fold too ---
 (= 256 (: (f x) (3 2 x) (f 2)))              ; (3 2) is the charm 8, folded; the unknown tail rides on
 (= 9   (: (f x) ('sym 5 x) (f 9)))           ; a point applies const-1 -> (1 x), the identity
 (= 9   (: (f x) (() 5 x) (f 9)))             ; ⚠ () folds too -- nom? is false of it, so it is named apart
 (= 5   (0 0 5)) (= 262144 (2 3 4))           ; const of const is id; the tower, both charm-sized
 (= 1.0 (: (f x) (2 1.5 x) (f 1)))            ; (2 1.5) is the gem 2.25 -- charm-sized, so it folds
 ; ⚠ the bound is owed by BOTH fold lanes. a pure-fn head cleared by pure-head? still reaches
 ; church application by OVER-application, and that is the unbounded one: (+ 1 200000 2) is
 ; ((200001) 2), a 60207-digit bignum. the safe prefix folds, the growing step residualizes.
 (= 60207 (tally (show (: (f x) (+ 1 200000 2) (f 0)))))
 (= '(1 2 3 4) (: (f x) ('(1 2) x) (f '(3 4))))   ; a chain joins like text, and folds the same way
 (= 256 (: (f x) ('(7 8 9) 1 x) (f 2)))       ; ... and INDEXES: ('(7 8 9) 1) folds to 8, leaving (8 x)
 (= 1 (: (f x) ((link 1 2) x) (f (\ a b (a + b)))))  ; a lambda operand takes the DEFAULT action
 ; ⚠ and a numeral whose answer OUTGROWS a charm stays residual rather than baking into the image:
 ; a composition here, 2**200000 in test/task.l -- whose yieldable-multiply gate leans on it
 (= 3 (: (f x) (3 (\ y (y + 1)) x) (f 0)))

 ; --- currying: under-application yields a usable closure; prefix+unknown tail ---
 (= 7  ((+ 3) 4))                                ; (+ 3) baked, applied at runtime
 (= 15 (: (g x) (+ 10 x) (g 5)))                 ; (+ 10) folded; x unknown -> residual

 ; --- shadowing: a locally-bound name is never folded as the global ---
 (= 99 (: (link a b) 99 (link 1 2)))             ; let-bound X
 (= 5  (: (cap x) 5 (cap '(1 2))))               ; let-bound A
 (= 7  (: (f link) (link 3 4) (f (+))))            ; PARAM X shadows the global; f + -> (+ 3 4)

 ; --- nil?-flip: (? (nil? e) a b) == (? e b a); behavior-preserving under BOTH compilers ---
 (= 'z  (: (f x) (? (nil? x) 'z 'nz) (f 0)))        ; falsy e -> consequent
 (= 'nz (: (f x) (? (nil? x) 'z 'nz) (f 5)))        ; truthy e -> alternative
 (= 'z  (: (f x) (? (nil? x) 'z 'nz) (f "")))       ; empty string is falsy
 (= 'nz (: (f x) (? (nil? x) 'z 'nz) (f '(1))))     ; non-empty list is truthy
 (= 'z  (: (f x) (? !x 'z 'nz) (f 0)))              ; the `!` sigil reads as (nil? x), flips too
 (= 'c  (: (f x) (? (< 5 x) 'a (nil? x) 'c 'b) (f 0)))  ; final-clause flip inside a chain
 (= 'b  (: (f x) (? (< 5 x) 'a (nil? x) 'c 'b) (f 3)))  ; ... fall-through to the else
 (= 'a  (: (f x) (? (< 5 x) 'a (nil? x) 'c 'b) (f 9)))  ; ... first clause still wins
 (id? () (: (f x) (? (nil? x) 'z) (f 5))))         ; no-else cond falls to the zero point () (nil-ontology)

; --- the PURITY GATE on beta's arguments --- an inlined lambda SUBSTITUTES its args, so a
; body naming a param twice would run an impure arg twice. `allp`/`purep` is what forbids it,
; and its failure is silent: the answer is simply wrong, no scare. this pins the gate itself.
; ⚠ it caught pure-head? being called under-applied -- a partial is TRUTHY, so every purity
; question answered yes and (dbl (bump 0)) bumped the counter twice. the arity IS the protocol.
(: pg-ctr (cask 1)
   (pg-bump _) (: v (peep pg-ctr 0 0) _ (pin pg-ctr 0 (+ v 1)) (+ v 1))
   (pg-dbl x) (x + x)                                ; names its param TWICE -- the duplication site
   (assert
    (: _ (pin pg-ctr 0 0) r (pg-dbl (pg-bump 0))
       (&& (= 2 r) (= 1 (peep pg-ctr 0 0))))))       ; evaluated ONCE: 1+1, counter at 1

; --- the inliner's under-saturated beta --- a known lambda applied SHORT of its params
; substitutes the given args and answers a \ of the rest -- so curried HOFs collapse
; statically when consumed (op=1), and still REVERT at value position (the escape law).
(: (usadd a b) (a + b)
   (us3 a b c) (a + b + c)
   (ustw f x) (f (f x))
   (assert
    (15 = (: (g i) ((ustw (usadd i)) i) (g 5)))          ; the doc's promise: collapses to first-order
    (7 = ((ustw (us3 1 2)) 1))                           ; two params short, then cascaded
    (9 = (: h (usadd 4) (h (h 1))))))                    ; value position: the partial REVERTS, still right
