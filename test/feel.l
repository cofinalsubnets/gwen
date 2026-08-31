; test/feel.l -- the feel pre-pass: macro expansion (exercised throughout the suite)
; and constant folding. Folding is semantics-preserving, so whether it fired is only
; observable via baked-literal identity (a folded (X 1 2) is shared across calls) --
; which differs between the feel compiler and the c0/l0 control, so those positive
; "fires" checks can't live in this cross-compiler suite (they're verified out of band).
; What can be asserted here: fold correctness, and that identity-minting heads stay
; excluded -- (tablet 0) must yield a fresh hash on every call under both compilers.
(assert
 ; --- impure / identity-minting heads are not folded (fresh each call) ---
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

 ; --- a TEXT head folds its known prefix: pure and total at every operand ---
 (= "abcd"   ("ab" "cd"))                     ; juxtaposition
 (= "abcdef" ("ab" "cd" "ef"))                ; ... curried, folded left to right
 (= "abcd"   (: a "ab" b "cd" (a b)))         ; ... through const-propagated binders, no literal in sight
 (= 97 ("abc" 0)) (nil? ("hi" 9))              ; the byte index and its out-of-range unit
 (= "abcdef"   (: (f x) ("ab" "cd" x) (f "ef")))       ; known prefix folds, the unknown tail rides on
 (= "abcdefgh" (: (f x) ("ab" "cd" x "gh") (f "ef")))  ; ... and the operands after it still apply in order
 (= "abZcd"    (: (f x) ("ab" x "cd") (f "Z")))        ; nothing known past the head -> wholly residual
 (= "xy" (: (f s) (s "y") (f "x")))           ; an unknown head stays residual, and still joins at run time

 ; --- ... and the walk is the head VALUE's purity, not text: a point and a numeral fold too ---
 (= 256 (: (f x) (3 2 x) (f 2)))              ; (3 2) is the charm 8, folded; the unknown tail rides on
 (nil? (: (f x) ('sym 5 x) (f 9)))            ; a point indexes its spelling; 5 is past "sym" -> ()
 (nil? (: (f x) (() 5 x) (f 9)))             ; () folds too -- nom? is false of it, so it is named apart
 (= 5   (0 0 5)) (= 262144 (2 3 4))           ; const of const is id; the tower, both charm-sized
 (= 1.0 (: (f x) (2 1.5 x) (f 1)))            ; (2 1.5) is the gem 2.25 -- charm-sized, so it folds
 ; the bound is owed by both fold lanes. a pure-fn head cleared by pure-head? still reaches
 ; church application by over-application, and that is the unbounded one: (+ 1 200000 2) is
 ; ((200001) 2), a 60207-digit bignum. the safe prefix folds, the growing step residualizes.
 (big? (: (f x) (+ 1 200000 2) (f 0)))       ; big?, not (tally (show ..)) -- rendering 60207 digits cost the gate 2.5s
 (= '(1 2 3 4) (: (f x) ('(1 2) x) (f '(3 4))))   ; a chain joins like text, and folds the same way
 ; --- readset: peep and pin fold on an immutable container, never on a tablet ---
 (= "aXcd" (: (f x) (pin "abcd" 1 x) (f 88)))     ; pin on text -- the functional update, folded whole
 (= '(1 X 3) (: (f x) (pin '(1 2 3) 1 x) (f 'X)))  ; ... and on a chain
 (= 8 (: (f x) (peep '(7 8 9) x 0) (f 1)))        ; peep reads a known chain at compile time
 (: (f _) (pin (tablet 0) 1 2) (nil? (id? (f 0) (f 0))))   ; a tablet is mutable: pin there is a write, never folded
 (= 256 (: (f x) ('(7 8 9) 1 x) (f 2)))       ; ... and indexes: ('(7 8 9) 1) folds to 8, leaving (8 x)
 (nil? (: (f x) ((link 1 2) x) (f (\ a b (a + b)))))  ; a lambda operand takes the default action
 ; and a numeral whose answer outgrows a charm stays residual rather than baking into the image:
 ; a composition here, 2**200000 in test/task.l -- whose yieldable-multiply gate leans on it
 (= 3 (: (f x) (3 (\ y (y + 1)) x) (f 0)))

 ; --- currying: under-application yields a usable closure; prefix+unknown tail ---
 (= 7  ((+ 3) 4))                                ; (+ 3) baked, applied at runtime
 (= 15 (: (g x) (+ 10 x) (g 5)))                 ; (+ 10) folded; x unknown -> residual

 ; --- shadowing: a locally-bound name is never folded as the global ---
 (= 99 (: (link a b) 99 (link 1 2)))             ; let-bound X
 (= 5  (: (cap x) 5 (cap '(1 2))))               ; let-bound A
 (= 7  (: (f link) (link 3 4) (f (+))))            ; param X shadows the global; f + -> (+ 3 4)

 ; --- nil?-flip: (? (nil? e) a b) == (? e b a); behavior-preserving under both compilers ---
 (= 'z  (: (f x) (? (nil? x) 'z 'nz) (f 0)))        ; falsy e -> consequent
 (= 'nz (: (f x) (? (nil? x) 'z 'nz) (f 5)))        ; truthy e -> alternative
 (= 'z  (: (f x) (? (nil? x) 'z 'nz) (f "")))       ; empty string is falsy
 (= 'nz (: (f x) (? (nil? x) 'z 'nz) (f '(1))))     ; non-empty list is truthy
 (= 'z  (: (f x) (? !x 'z 'nz) (f 0)))              ; the `!` sigil reads as (nil? x), flips too
 (= 'c  (: (f x) (? (< 5 x) 'a (nil? x) 'c 'b) (f 0)))  ; final-clause flip inside a chain
 (= 'b  (: (f x) (? (< 5 x) 'a (nil? x) 'c 'b) (f 3)))  ; ... fall-through to the else
 (= 'a  (: (f x) (? (< 5 x) 'a (nil? x) 'c 'b) (f 9)))  ; ... first clause still wins
 (id? () (: (f x) (? (nil? x) 'z) (f 5))))         ; no-else cond falls to the zero point () (nil-ontology)

; --- the purity gate on beta's arguments --- an inlined lambda substitutes only pure args:
; a body naming a param twice would run an impure arg twice (the flip, test/uuspllaw.l's
; sl-flip -- value equal, trace doubled, so the failure is silent: the answer is simply
; wrong, no scare). `purep` is what forbids it; an impure arg takes the binding splice
; below instead. this pins the gate itself.
; it caught pure-head? being called under-applied -- a partial is truthy, so every purity
; question answered yes and (dbl (bump 0)) bumped the counter twice. the arity is the protocol.
(: pg-ctr (cask 1)
   (pg-bump _) (: v (peep pg-ctr 0 0) _ (pin pg-ctr 0 (+ v 1)) (+ v 1))
   (pg-dbl x) (x + x)                                ; names its param twice -- the duplication site
   (assert
    (: _ (pin pg-ctr 0 0) r (pg-dbl (pg-bump 0))
       (&& (= 2 r) (= 1 (peep pg-ctr 0 0))))))       ; evaluated once: 1+1, counter at 1

; --- the binding splice (sl-lic/sl-ord, test/uuspllaw.l) --- an impure arg no longer
; declines the inline whole: it binds once to a fresh nom, in call order, ahead of the
; body, and the body reads the binding at every use site. pure args still substitute
; around it. behavioral pins only (whether a splice FIRED is out of band, per the header);
; each holds under both compilers and under decline, which is the point -- the license.
(: bs-ctr (cask 1)
   (bs-bump _) (: v (peep bs-ctr 0 0) _ (pin bs-ctr 0 (+ v 1)) (+ v 1))
   (bs-mix a b c) (a + b + c)
   (bs-ord a b) (b - a)                    ; reads its args in reverse; eval stays call order
   (bs-drop a b) b                         ; drops its first param; the effect must still run
   (bs-esc n) (\ x (x + n))                ; a lambda body -> escapes at value position
   (assert
    ; mixed: the constants substitute and the one impure arg binds -- value and trace as the call
    (: _ (pin bs-ctr 0 0) r (bs-mix 3 (bs-bump 0) 4)
       (&& (= 8 r) (= 1 (peep bs-ctr 0 0))))
    ; order: two impure args evaluate in call order however the body reads them
    (: _ (pin bs-ctr 0 0) r (bs-ord (bs-bump 0) (bs-bump 0))
       (&& (= 1 r) (= 2 (peep bs-ctr 0 0))))
    ; a dropped param's effect still runs -- bound, never substituted away
    (: _ (pin bs-ctr 0 0) r (bs-drop (bs-bump 0) 7)
       (&& (= 7 r) (= 1 (peep bs-ctr 0 0))))
    ; identity-minting arg: bound once, so the two reads see one mint
    (: (bs-same t) (id? t t) (bs-same (tablet 0)))
    ; under-saturated + impure: declined -- the partial's effect fires at build time, once
    (: _ (pin bs-ctr 0 0) p (bs-mix 3 (bs-bump 0))
       (&& (= 9 (p 5)) (= 1 (peep bs-ctr 0 0))))
    ; the escape law: a lambda body at value position declines; the closure still works
    (: _ (pin bs-ctr 0 0) h (bs-esc (bs-bump 0))
       (&& (= 3 (h 2)) (= 1 (peep bs-ctr 0 0))))))

; --- the read-once refinement (sl-sub-1) --- the last effectful arg substitutes when its
; param is read exactly once, unconditionally, in an otherwise-pure body -- the call is
; gone with no let. read-many still binds; a read under ? or \ declines the substitution
; (it could run zero or many times); order holds across a bind + a substitution.
(: r1-c1 (cask 1) r1-c2 (cask 1)
   (r1-b1 _) (: v (peep r1-c1 0 0) _ (pin r1-c1 0 (+ v 1)) (+ v 1))
   (r1-b2 x) (: v (peep r1-c2 0 0) _ (pin r1-c2 0 (+ v 1)) (+ x v))    ; answers x + own pre-count
   (r1-mix a b c) (a + c + b)               ; b read once -> substitutes; a c fold
   (r1-two a b) (a + a + b)                 ; a read twice -> binds; b once -> substitutes
   (r1-iff p b) (? p b 0)                   ; b read under ? -> declines; still runs once
   (assert
    ; read-once: the effect still fires exactly once, value as the call
    (: _ (pin r1-c1 0 0) r (r1-mix 3 (r1-b1 0) 4)
       (&& (= 8 r) (= 1 (peep r1-c1 0 0))))
    ; bind + substitution: a's binding fires before b's substituted read (call order) --
    ; b's expression reads a's counter, so a flip changes the VALUE, not just the trace
    (: _ (pin r1-c1 0 0) _ (pin r1-c2 0 0)
       r (r1-two (r1-b1 0) (r1-b2 (peep r1-c1 0 0)))
       (&& (= 3 r) (= 1 (peep r1-c1 0 0)) (= 1 (peep r1-c2 0 0))))
    ; conditional read: the strict call evaluates b always -- substitution under the
    ; branch would drop the effect on the false arm, so it must bind instead
    (: _ (pin r1-c1 0 0) r (r1-iff 0 (r1-b1 0))
       (&& (= 0 r) (= 1 (peep r1-c1 0 0))))))

; --- the inliner's under-saturated beta --- a known lambda applied short of its params
; substitutes the given args and answers a \ of the rest -- so curried hofs collapse
; statically when consumed (op=1), and still revert at value position (the escape law).
(: (usadd a b) (a + b)
   (us3 a b c) (a + b + c)
   (ustw f x) (f (f x))
   (assert
    (15 = (: (g i) ((ustw (usadd i)) i) (g 5)))          ; the doc's promise: collapses to first-order
    (7 = ((ustw (us3 1 2)) 1))                           ; two params short, then cascaded
    (9 = (: h (usadd 4) (h (h 1))))))                    ; value position: the partial reverts, still right

; --- the body-position `:` splices into its parent (lflat) --- one frame where the source
; nested two, and the same meaning either way. shadowing and capture are the gates: an inner
; name that collides with an outer one, or that an outer def reads, keeps its own frame.
(: (lf-h n) (? (n = 0) 4 (lf-h (n - 1)))         ; recursive, so cprop neither folds nor inlines it
   (assert
    (3 = (: a 1 (: b 2 (a + b))))                   ; the plain splice
    (6 = (: a 1 (: b 2 (: c 3 (a + b + c)))))       ; a chain of any depth lands in one frame
    (2 = (: a 1 (: a 2 a)))                         ; a collision -- the inner still shadows
    (4 = (: a 1 (: b a (: a 3 (b + a)))))           ; ... and the outer a is what b read
    (4 = (: (f _) (lf-h 1) (: lf-h (\ y 99) (f 0))))))   ; an outer def reads the name -> no capture

; --- an over-applied known-arity head splits (napof/napn) --- the n-ary apply takes the
; arity prefix and the surplus curries onto its answer, so (f 1 2 3) is ((f 1 2) 3). the
; heads recurse so the inliner declines them and the apply lane is what runs.
(: (oa2 a b) (? (a = 0) (\ c (b + c)) (oa2 (a - 1) b))
   (oa3 a b c) (? (a = 0) [b c] (oa3 (a - 1) b c))
   (oan x) (? (x = 0) 3 (oan (x - 1)))              ; an operand no fold knows -- 3
   (assert
    (7 = (oa2 1 3 4))                               ; arity 2, three args
    (7 = ((oa2 1 3) 4))                             ; ... the same, spelled split
    (8 = (oa3 1 9 8 1))                             ; arity 3, four args -- [9 8] indexed at 1
    ; a curried nif splits the same way -- the tower is the surplus applied to its answer
    (4096 = (* (oan 0) 2 4)) (1024 = (+ (oan 0) 2 4)) (4 = (- (oan 0) 1 2))))

; --- ..and a shadowed name is not the arity the outer one wore --- lvm_apn enters the
; callee past its lvm_cur prologue, so `apn n` is sound only where n is that callee's
; arity. falook answers by NAME out of a compile-time map, and its walk can reach an
; enclosing binding where an inner scope rebinds the name -- so a guess may be spent only
; on an exactly saturated call, never on a surplus. Guessing short here entered a 3-arity
; body with two arguments and answered the partial.
; the glaze hides this one. a native closure is compiled by its own emitter and never
; runs the apn, so the corpus is green on any glazed lane; love0 carries no glaze and a
; crew bake reverts its natives, which is where it surfaced (kore join, GNU-differential).
(: (oas-w a b) (? (two? a) (oas-w >a b) b)              ; the outer oas-w: arity 2
   (oas-in l) (: (oas-w x y z) (? (two? x) (oas-w >x y z) (y + z))   ; shadows it at arity 3
                 (oas-w l 1 2))
   (assert
    (3 = (oas-in [1 2 3]))                              ; the inner arity is the one that runs
    (3 = (oas-in ()))                                   ; ..including where the walk ends at once
    (9 = (oas-w [1 2] 9))))                             ; and the outer still answers for itself
