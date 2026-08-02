; love spec -- the executable semantic spec AND the language reference: each section states its
; laws in the comment and proves them in the asserts below, on every target. the workflow
; narrative -- how to work here, the traps, the vocabulary, the architecture -- lives in
; CLAUDE.md; deeper rationale in doc/ (doc/measures.md the net/tally design). it runs in the
; corpus SECOND (common.mk, right after test/00-init.l sets up the assert harness), so keep it
; green -- `make test` (host + love0, which must print the zz-fin summary twice). when in doubt,
; PROBE THE BINARY: never trust a prior over a one-line experiment.

; --- everything is a function --- (f x y) == ((f x) y), (f) == f; numbers are church
; numerals; data self-applies. asserts read INFIX, folding right-associatively (sound by (f) == f).
(assert
 (1 = (0 5)) (5 = (1 5))                         ; 0 is const-1, 1 is the identity
 (1 = (0 0)) (7 = (0 0 7))                       ; const of const is the identity: (0 0 x) = ((0 0) x) = (1 x) = x
 (8 = (3 2)) (262144 = (2 3 4))                  ; (n x) = x**n; the tower (2 3 4) = 4**(3**2)
 (12 = (3 (+ 1) 9)) ('(2 3 4) = (map (+ 1) '(1 2 3)))  ; (n f) composes f ($ n) times; currying *is* partial application
 (10 = (-3 (+ 1) 10)) (13 = (2.5 (+ 1) 10))      ; the count SATURATES, true to !!$: (-3 f) = (0 f) = 1; a float ceils
 (0.5 = (-1 2)) (3.0 = ((/ 1 2) 9)))             ; ... but exponents don't: (-1 x) = 1/x; (1/2 x) = sqrt x

; --- three special forms --- `:` letrec*/sequence, `?` cond, `\` lambda (one operand: quote).
; a body-having `:` is ONE SCOPE; an empty special form reads its head (the zero point).
(assert
 (3 = (: a 1 b 2 (a + b)))                       ; `:` binds in source order; the last form is the result
 (7 = ((\ x y (x + y)) 3 4))                     ; `\ args.. body` -- a lambda, auto-curried
 ('(1 2) = (\ (1 2)))                            ; one operand: `\` is quote, so 'x is just (\ x)
 ('big = (? 0 'a (1 < 2) 'big 'else)))           ; `?` -- test/result pairs, then a final else
(: (twice f x) (f (f x)) (assert (12 = (twice (+ 1) 10))))
(assert
 (id? (:) (?)) (id? (?) (\)) !(:) !(nom? (:))     ; the empty form is the head read, both lanes: the zero point (absence, NOT a nom)
 (id? (:) (? 0 'a)) (id? (:) (map (+ 1) ()))     ; a no-match `?` (and `map` of nothing) falls to the zero point () (= the empty form (:)), NOT charm 0
 (2 = (: x 1 x (x + 1) x)))                      ; the sequence law: a rebind reads the previous value
; one scope: a forward read raises missing, and welp answers it with the zero point -- the same
; law the asserts above show, under a help that keeps it quiet (file mode's default wears a face)
(: help welp)
(assert (id? (:) (: a b b 2 a)) (2 = (: a b b 2 b)))
(: help 0)

; --- true and false --- false is *nothing*: whatever NETS <= 0 (a complex-valued measure --
; numbers their value, text its charm sum, lists the sum of element nets, SPINE only). `!` (nil?) reads
; the net's sign; `$` (sat) is one saturating clamp, max(0, ceil); !x == (0 = $x). a twin gem is
; gated by its REAL part and measured by its MAGNITUDE ($~(3 4) = 5), so a pure phase is BLUE --
; the TRUTH GATE IS NOT THE TOTAL ORDER, the only place the two part (see the true-and-false asserts).
; `$` is the SOLE clamp (a clamped fold is mere composition); the COUNT -- how many, not how much --
; is `tally` (text/cask charms, list spine, array cells, map keys: what "length" always was, $ never
; was). every value wears a COLOR by its net-sign: GREEN nonnegative (what $ keeps), RED negative,
; BLUE the zero floor -- true iff POSITIVE green. the two-measure design: doc/measures.md.
; ⚠ TRUTH IS A THRESHOLD ON THE NET, NOT A HOMOMORPHISM, and it CANNOT be one. `net` is the
; hom (it sums); `!!$` thresholds it, and a threshold cannot preserve ring structure. the two
; connectives pull apart: `*` = AND wants the false set to be an IDEAL, which forces false =
; {0} (since -1 * -1 = 1, false*false = true under the current rule); `+` = OR wants it closed
; under +, and since x + -x = 0 in a ring, that forces at most one of {x, -x} true -- a
; POSITIVE CONE. those two sets differ, so on a ring you may have one or the other, never both.
; love takes the CONE: net sums, $ clamps once, ! reads the sign -- additive top to bottom. so
; "negative is false" is the CONSEQUENCE of truth being a measure, not an extra clause. and the
; logical connective was never `*` anyway: `&&` is (and answers the UNIT on failure).
(assert
 !0 !"" !(tablet 0) !@0 !() !0.0 !~(0 0)         ; zero / empty -> false
 !-5 !-3.9 !~(-3 4) !~(0 -1)                     ; <= 0 in the total order -> false (complex by re then im)
 !!5 !!"x" !!'a !!cap !!~(3 4) !!~(3 -4)         ; positive / nonempty -> true (a twin gem by its RE)
 !'(() ()) !'(0 0) !'((0) (())) (0 = $'(() () ()))  ; a chain of nothings is nothing, recursively
 !'(-5) !@(-3 -4) !'(-2 1) !!'(0 1)              ; negatives net <= 0 at every rank; -2+1 < 0 < 0+1
 (3 = $'(1 () 2)) (6 = $'(1 2 3)) (7 = $@(3 4))  ; $ sums the nets, then clamps once
 (1 = $(link 1 2)) !(link 0 2)                   ; the SPINE carries the measure: a dotted tail is not an element
 (0 = $`(~(3 4) ~(-3 4))) !`(~(0 5) -1)        ; phases sum as VECTORS, then the total is gated once:
 !i !~(0 -1) !~(0 8) (0 = $i)                    ; ...and a PURE PHASE is BLUE. `i` and `-i` are swapped by a
 (i < 5) (~(0 -1) < i)                           ; field automorphism, so truth cannot tell them apart -- but the
                                                 ; ORDER still must (sorting needs totality). the one place they part.
 (1 = $#0) !!#0                                  ; ... but a box stays truthy: maps count keys
 (1 = $'(1)) (0 = $'(0)) (0 = $0) (-1 = +'(-2 1))  ; the COLORS by net-sign: '(1) green, '(0)/0 blue, '(-2 1) red
 (1 = !!$5) (0 = !!$0) (0 = !!$-3)               ; !!$ is the truth bit (1 true, 0 false)
 (0 = !5) (1 = !0) (id? zero? nil?) (1 = (nil? 0))  ; ! negates it; non is nil?
 (star? 5) (star? -3.5) (star? ~(2 3)) (star? (62 2))         ; a STAR is a self-netting SCALAR: charm, sun, float, complex
 !(star? @(1 2 3)) (star? (net @(1 2 3)))                   ; a TRAY is NOT a star -- it nets DOWN to a scalar star (net is a fresh sum)
 !(star? "x") !(star? 'a) !(star? '(1 2))                    ; text/symbol/list are not stars -- they net to something else
 (? (charm? 5) (star? 5) 1)                                  ; a CHARM is a star, by spec (net = self)
 (charm? 5) !(charm? 3.5) !(charm? @(1 2 3)) !(charm? 'a) (? (charm? 5) (star? 5) 1)   ; a CHARM (charm?) is the word-sized star, and every charm is a star
 (tray? @(1 2 3)) (tray? @("a" "b"))                      ; a TRAY is an array of ANY element kind (numbers or objects)
 (galaxy? @(1 2 3)) !(galaxy? @("a" "b"))                   ; a GALAXY is a NUMERIC tray (a tray of stars); a tray of strings is not
 (galaxy? (array '(2) (200 2) (200 2)))                    ; a tray of BIGS is a galaxy -- bignum cells are stars (keyed on cells, not the O code)
 !(star? @(1 2 3)) !(galaxy? 5) !(galaxy? '(1 2))            ; star?/galaxy? are DISJOINT; a scalar / a list is neither a galaxy nor a tray
 (!"" = 0 = $"") (!-5 = 0 = $-5))                ; the invariant !x == (0 = $x), as an = chain

; --- types & predicates --- storage: charm? big? sun? gem? twin? tray? string? nom? name? two? tablet?;
; derived constellation? whole? atom?; lit? = a top-region reference (map/fn/hot, ai_kind>=KMap); hot? the opaque cask/port subset.
(assert
 (charm? 5) (two? '(1 2)) (string? "hi") (nom? 'x) (lit? cap) (tablet? #(1 2))
 (big? (100 2)) ((? word sun? big?) (62 2)) (gem? 1.5) (twin? i) (tray? @(1 2 3))
 (constellation? 1.5) (constellation? i) (constellation? (62 2)) (whole? (62 2)) (atom? 'x) !(atom? '(1))
 (hot? (cask 4)) (hot? out) (hot? (tap '(104)))   ; the hot zoo, named
 !(hot? #()) !(hot? cap) !(hot? 5) !(hot? "s")   ; maps/functions/data are not hots
 (lit? (cask 4)) (lit? cap) (lit? #()) !(lit? '(1)) !(lit? "s") !(lit? 5) !(lit? ()) ; lit? = the top region (map/fn/hot); not data leaves (chains/text/numbers) nor the floor
 (1 = $cap) (1 = $out) (1 = $(tap 0))            ; tops and ports net 1: present, drained or not
 (charm? (: p (twirl (\ x x) 0) _ (catch p) p))  ; a task id is a number, not a handle
 ((64 2) = 2 * (63 2))                           ; charm overflow -> exact big ((k b) = b**k)
 (5.0 = (abs ~(3 4))) (~(2.0 3.0) = 2 + 3 * i))  ; mixed arithmetic widens to the higher tier

; --- arithmetic --- + - * / // % infix; charm fast path, overflow grows charm->sun->big;
; `/` is true division (inexact int quotient -> float); 0/0 = 0; bitwise << >> & | ^ ride the
; WORD LANE (the width law, arrays): fixed-width two's complement even on a star -- (<< 1 63) is
; the SUN -9223372036854775808, wrapped into the sign bit; the shift count masks to the word, so
; (<< 3 200) is 3 << 8; >> is ARITHMETIC, sign-extending; and a big sits ABOVE the lane, refusing
; with the zero point. monadics
; ride the valence law (-(f x) negate, /4 reciprocal, ...) while sections ((- 0), (/ 1)) stay words.
(assert
 (3 = 1 + 2) (2.5 = 5 / 2) (2 = (// 5 2)) (0.5 = 1 / 2) (1 = 5 % 2) (3.5 = 1 + 2.5)
 (2 = 4 / 2) (charm? (4 / 2)) (-2 = (// -5 2))
 (-5 = ((- 0) 5)) (0.25 = ((/ 1) 4)) (~(-1 -2) = ((- 0) ~(1 2)))   ; the sections are the monadics
 !(charm? (2 * 2305843009213693952)) (gem? (1 / 0)) (? word (1e308 < 1 / 0) 1) (0 = (/ 0 0))
 (ieee-inf = 1 / 0) ("ieee-inf" = (show (1 / 0))) ("-ieee-inf" = (show (-1 / 0)))
 (? word ("1.7976931348623157e308" = (show 1.7976931348623157e308)) 1)  ; the WHOLE finite range prints digits -- only true infinity wears ieee-inf
 ; the printer is SHORTEST-ROUNDTRIP (exact Steele & White): the fewest digits
 ; that read back to the same gem, so show never conflates neighbors and
 ; (read (show x)) = x for every finite gem. the word guard: these faces are
 ; the DOUBLE tier's (a 32-bit ai_flo_t is a float, whose shortest faces differ)
 (? word ("0.30000000000000004" = (show (0.1 + 0.2))) 1) ("0.3" = (show 0.3))
 (? word ("5e-324" = (show 5e-324)) 1) (? word ("1e23" = (show 1e23)) 1)
 ; ... and the READ side is correctly rounded too (am_strtod, the printer's twin
 ; in the math floor -- every frontend, no glibc): neighbor literals stay distinct
 (? word !(0.3 = 0.30000000000000004) 1) (? word (0.5 = 0.49999999999999999999) 1)
 ((/ 0 0) = (/ 0 0)) !(/ 0 0) (0 = $(/ 0 0)) (nom? 'inf) (nom? 'nan) (nom? 'ieee-nan)
 ; bignum // and % ride Knuth-D long division, whose 128/64 step is one `divq` on x86-64 and a
 ; soft divide elsewhere; `show` on a big divides too (the decimal printer), so it checks the same lane.
 (1267650591354675262013 = (// (100 2) 1000000007)) (976371285 = (100 2) % 1000000007)
 ((100 2) = (// (200 2) (100 2))) ("1267650600228229401496703205376" = (show (100 2)))
 (-2 = (^ 1 -1)) (15 = (8 | 4 | 2 | 1)) (16 = (>> 64 2)) (16 = (<< 2 3))
 (768 = (<< 3 200))                              ; the count masks to the word (200 = 8 mod 32 AND mod 64)
 (-1 = (>> -1 1)) (-4 = (>> -8 1))               ; >> is ARITHMETIC: it sign-extends
 (() = (& (200 2) 255))                          ; a big is above the word lane -- it refuses
 (? word (sun? (<< 1 63)) 1))                    ; ...and (<< 1 63) wrapped into the sign bit: a SUN

; --- order & equality --- < <= > >= total over all values, the TRUE-BLUE bands: POINTS at the
; floor (() < bare mints < named noms -- mints by serial, names by name-lex then serial), then
; string, then ALL constellations as ONE band ordered by net (stars and galaxies INTERLEAVE, a
; star below a galaxy on a tie), then tray (object array) < chain < map < top, each band ordered
; within itself (text/chains lexicographically, maps and lambdas by an alpha-invariant hash).
; the tier spine inside the number band: N green charms < Z < R < C, nesting as usual. an array
; operand BROADCASTS < to a mask, so the scalar order is observed through sort and map keys.
; `=` bridges the numeric tower; `id?` is identity; for "not equal" write !(a = b).
(assert
 (3 = 3.0) !(3 = 4) (1 < 1.5) (3.0 >= 3) (id? 'a 'a) !(id? '(1) '(1))
 ('x < "a") ("a" < 1) (1 < '(0)) !(1 < "a")          ; true-blue order: mint < string < number < chain
 ('(0) < #(1 10)) (#(1 10) < cap) !(cap < #(1 10))    ; the map rung: chain < map < hot
 ("abc" < "abd") ("ab" < "abc") ('(1 2) < '(1 3))     ; within a band: text and chains order lexicographically
 ; a GALAXY folds into the number band by its net (probe via sort/show -- infix < on an array broadcasts to a mask).
 ("(@(1 1) 5 @(2 4))" = (show (sort `(@(2 4) 5 @(1 1)))))  ; nets 2 < 5 < 6, galaxies interleaving with the star
 ("(3 @(1 2))" = (show (sort `(@(1 2) 3))))          ; net tie -> a star seats below a galaxy
 ("(@('a 'b) (7 8))" = (show (sort `(@('a 'b) '(7 8)))))   ; a tray (object array) stays BELOW link (chain is the grammar substrate, seated high)
 (tray? @(5)) !(@(5) = 5))                            ; a ONE-CELL array is an array, never its scalar

; --- comparing functions --- `=` on functions is alpha + structural (binders by position,
; free vars by name, captures pairwise); eta/beta not bridged; a number NEVER equals a
; closure -- numerals ACT as their lambdas under apply ((1 x) = x), `=` doesn't say so.
; (\ y z) below holds a FREE var z; welp answers its read with the zero point so the closure
; builds with no warning surfaced -- it stays distinct from (\ x x) (free body /= bound body).
(: help welp)
(assert
 ((\ x x) = (\ y y)) ((\ a b (+ a b)) = (\ x y (+ x y)))           ; alpha: bound vars by position
 ((\ x (+ x 1)) = (\ x (+ x 1))) !(id? (\ x (+ x 1)) (\ x (+ x 1)))  ; equal, yet distinct objects
 !((\ x x) = (\ y z)) !((\ x (+ x 1)) = (\ x (+ x 2)))             ; free /= bound; different body
 (: g (\ a (\ b (+ a b))) (&& ((g 1) = (g 1)) !((g 1) = (g 2))))   ; closures: equal iff captures match
 ((+ 1) = (+ 1)) !((\ x x) < (\ y y)) !((\ x (cap x)) = cap)           ; partial apps too; eta not bridged
 ((+ 1.5) = (+ 1.5)) !((+ 1.5) = (+ 2.5)) ('(1.5) = '(1.5)) !('(1.5) = '(2.5))  ; nested float boxes compare by value (eqv KFlo)
 !(1 = (\ x x)) !((\ y y) = 1) !(id? 1 (\ x x))                    ; NO numeral bridge: a number never equals a closure
 !(0 = (\ _ 1)) !((\ q 1) = 0) (42 = ((\ x x) 42)) (1 = ((\ _ 1) 9)) ; ... they ACT alike under apply; = stays representation-strict
 !!(\ _ 1) (1 = $(\ _ 1)))     ; a top is present and nets 1
(: help 0)

; --- reduction --- `=` stops at alpha (above); `ev` goes further -- it REDUCES. these are the
; binary twins of proof/rocq/spec.v's reduction layer (step/subst): the proof says "valid by the
; calculus", the line here says "the binary computes exactly this". beta on the identity and
; const-1, a nested redex reduced all the way, and eta still unbridged (its de Bruijn reduct shifts).
(assert
 (5 = ((\ x x) 5))                               ; beta_id: (\ x x) a ~> a
 (1 = ((\ _ 1) 9))                               ; beta_const: (\ _ 1) a ~> 1 (drops its arg)
 (5 = ((\ x x) ((\ x x) 5)))                     ; beta_nested: (\ x x) ((\ x x) a) ~>* a
 (10 = ((\ q ((\ i (: q 7 (+ i q))) q)) 3))      ; capture-avoiding: a : binder shadowing the arg does NOT capture it (i stays 3, local q 7)
 (8  = ((\ q ((\ i (: i 5 (+ i q))) q)) 3))      ; a : REBINDING the lambda param shadows it -- the subst drops, not lands on the binder
 (99 = ((\ tuple (tuple 1 2 3)) (\ a b c 99)))   ; a binder SHADOWS a same-spelled macro: the macro must not fire (tuple is a prel macro)
 (5  = (: let (\ x 5) (let 9)))                  ; likewise for a : binder named like a macro
 !((\ x (cap x)) = cap))                         ; eta_redex_shifts' twin: eta stays unbridged

; --- + and * are generic --- `+` adds numbers, concatenates strings, appends lists; `-` numeric
; only. the BYTE LAW: a string + an exact integer 0..255 is one byte. a symbol inherits the string
; lane (acts as its spelling, re-interns; demotes on a string operand). `*` is repeated `+`: a sequence
; times a count repeats it, and the count SATURATES.
(assert
 ("abcd" = "ab" + "cd") ("xB" = "x" + 66) ('efef = 'ef + 'ef) ("abef" = "ab" + 'ef)
 ("xB" = "x" + 66.0) !("x" + 1.5) !("x" + -66) !("x" + 256)   ; the byte law: exact 0..255 or nil
 ('(1 2 3 4) = '(1 2) + '(3 4)) ('(5 1 2) = 5 + '(1 2)) !("a" - "b")
 ('(3) = '(3) + ()) ('(3) = () + '(3)) ('(1 2) = '(1 2) + (mint 0))   ; a bare mint -- the zero point () too -- is +'s
 ("ab" = "ab" + ()) ("ab" = () + "ab") (5 = 5 + ()) (5 = () + 5)      ; IDENTITY in every lane, not just on lists:
 (~(1 2) = () + ~(1 2)) ("@(1 2 3)" = show (() + @(1 2 3))) (() = () + ())  ; string/number/complex/array pass through
 (('ef + '(1 2)) = '(ef 1 2)) ((intern (+ "ef" 5)) = 'ef + 5)  ; a NAMED symbol still ADJOINS to a list (foo stays foo); + a number rides the byte law, re-interned
 ((\ x x) = () + (\ x x)) (2 = (((\ x x) + (\ x x)) (\ n (+ n 1)) 0))   ; the TOP lane too: () + hot = hot, and
 ("ababab" = "ab" * 3) ('xyxy = 'xy * 2) ('(1 2 1 2) = '(1 2) * 2) !("ab" * "cd")   ; sym * count re-interns; + on two hots IS church add (id+id acts as 2)
 !("ab" * -3) ("ababab" = "ab" * 2.5) !('(1 2) * 0)         ; the count saturates; the NUMBER 0 empties
 (5 = () * 5) (5 = 5 * ()) ("ab" = "ab" * ()) ("ab" = () * "ab")   ; () is *'s IDENTITY too -- the multiplicative-
 ('(1 2) = '(1 2) * (mint 0)) (() = () * ()) ("@(1 2 3)" = show (() * @(1 2 3)))   ; identity FACE (1) of the unit,
 ("" = "ab" * 0) ((\ x x) = () * (\ x x)))                  ; mirroring its + face (0); 0 stays the number; () * hot = hot too

; --- the unit rides through EVERY dyadic arithmetic lane --- () is the DO-NOTHING operand,
; either side, for - / // % & | ^ << >> just as for + and *: the op never happens. NOT the
; number 0 -- no face projection (() - 5 is 5, not -5) -- so an undefined op's () vanishes
; when chained through any arithmetic lane. comparisons and `=` stay strict: () keeps its
; floor seat in the total order.
(assert
 (5 = 5 - ()) (5 = () - 5) (2.5 = () - 2.5) ((64 2) = (64 2) - ())     ; - : both sides, every width
 (5 = 5 / ()) (5 = () / 5) (5 = (// 5 ())) (5 = 5 % ()) (5 = (% () 5)) ; / // %
 (5 = (5 | ())) (5 = (() & 5)) (5 = (^ 5 ())) (5 = (<< 5 ())) (5 = (>> () 5))  ; the bitwise lanes -- | & (grip 30) sit BELOW =, so parenthesize
 (~(1 2) = ~(1 2) - ()) ("@(1 2 3)" = show (@(1 2 3) - ()))            ; complex + array pass through
 (2 = (5 - ()) - 3) (5 = ((- 5) ()))                    ; chains heal; a partial fired with the unit yields its held operand
 !('ef - 3) !("a" - "b") (5 = ('a - 'b) - 5)            ; undefined still answers (): it now vanishes through - too
 !(5 = ()) (() < 5))                                    ; = and < stay strict: () keeps the floor

; --- list*list is the cartesian product --- the semiring product whose `+` is cat: each
; pair a proper 2-list (love bj), so `tally` is the homomorphism (tally(a*b)=ta*tb). infix `*`
; is LEFT-handed, so a*b*c nests left and the pairs come out ((a b) c) -- the product is not
; associative on the nose (the pairing shape follows the parse; the multiset is the same either
; way), so this is the one law the hand is visible in. RIGHT-distributivity ((a+b)*c=a*c+b*c) holds on
; the nose (the outer loop ranges the LEFT operand); LEFT-distributivity holds only up to a
; permutation (intrinsic to ordered pairs -- same multiset, different order). `net` is NOT the
; `*` measure (it's not multiplicative); `tally` is. () stays the * UNIT (identity), not the
; cartesian annihilator -- the unit law preempts the empty-operand lane.
(assert
 (('(1 2) * '(3 4)) = '((1 3) (1 4) (2 3) (2 4)))                ; the product: every ordered pairing
 (('(1 2) * '(3 4) * '(5 6)) = '(((1 3) 5) ((1 3) 6) ((1 4) 5) ((1 4) 6) ((2 3) 5) ((2 3) 6) ((2 4) 5) ((2 4) 6)))  ; the left hand nests left
 (('(1 2) * ('(3 4) * '(5 6))) = '((1 (3 5)) (1 (3 6)) (1 (4 5)) (1 (4 6)) (2 (3 5)) (2 (3 6)) (2 (4 5)) (2 (4 6))))  ; ... and parens still buy the other shape
 (6 = tally ('(1 2) * '(3 4 5))) ((tally ('(1 2) * '(3 4 5))) = (tally '(1 2)) * (tally '(3 4 5)))   ; tally is the homomorphism
 ((('(1 2) + '(8 9)) * '(3 4)) = (('(1 2) * '(3 4)) + ('(8 9) * '(3 4))))    ; right-distributive on the nose
 (!(('(1 2) * ('(3 4) + '(5 6))) = (('(1 2) * '(3 4)) + ('(1 2) * '(5 6)))))  ; left-distrib FAILS as ordered lists ...
 ((sort ('(1 2) * ('(3 4) + '(5 6)))) = (sort (('(1 2) * '(3 4)) + ('(1 2) * '(5 6)))))  ; ... but holds up to permutation
 ('(1 3) = cap ('(1 2) * '(3 4)))                               ; pairs are real 2-lists -- reversible via cap/cup
 (('(1 2) * ()) = '(1 2)) (() * '(3 4) = '(3 4))                ; () is the * UNIT (identity), not the annihilator
 !("ab" * '(1 2)) !('(1 2) * "ab"))                            ; only chain*chain -- string*chain stays nil

; --- numeric functions --- abs int (type-aware); constants e pi i; gcd modpow; the irreducible
; transcendental nifs are sine cosine log (bignums widen, arrays map elementwise; log climbs
; tiers -- a negative/complex gives the principal value). everything else is DERIVED, no nif --
; power IS application: (k b) = b**k, sqrt ((/ 1 2) x), exp (x e), nth root ((/ 1 n) x), tan
; sine/cosine, atan (arg ~(1 x)), atan2 (arg ~(x y)); a negative base to a non-integer power
; goes principal (the angle pi*e factored exactly), so the derived sqrt is total: ((/ 1 2) -1) = i.
(assert
 (5 = (abs -5)) (charm? (abs -5)) (5.0 = (abs ~(3 4))) (2 = (int 2.9))
 ((abs -9223372036854775808) = 9223372036854775808)   ; |sun min| overflows the word: the bignum lane (the big oracle caught the wrap)
 (1024 = (10 2)) (21 = (gcd 1071 462)) (976371285 = (modpow 2 100 1000000007))
 (2.0 = ((/ 1 2) 4)) (1024 = (10 2)) (1 = (0 e)) (2.718281828459045 = e))

; --- a few identities --- tetration is the tower with one base; i*i = -1; euler holds in the
; EXACT direction ((log -1) = i * pi -- `=` is exact, so e^(i*pi) does not hold forward); power's
; principal root is exact, ((/ 1 2) -1) = i on the nose. (only bit-exact claims on every target.)
(assert
 (27 = (3 3)) (7625597484987 = (3 3 3)) (16 = (2 2 2)) (65536 = (2 2 2 2))   ; tetration
 (-1 = i * i) (-1 = (2 i)) (1 = ((i * 0) e))                                 ; i^2 = -1; e^(i*0) = 1
 ((log -1) = i * pi) ((log i) = i * pi / 2)                                  ; euler, in the exact direction
 (twin? (log -1)) (gem? (log 2)) (twin? (log ~(2 3)))                          ; log widens only when it must
 (i = ((/ 1 2) -1)) (~(0 2) = ((/ 1 2) -4))                                  ; sqrt of a negative: principal, exact
 (-8 = (3 -2)) (twin? ((/ 1 3) -8))                                       ; integer powers stay real; odd roots go principal
 (0.0 = (sine 0) / (cosine 0)) (gem? (sine 0)))                                   ; tan 0 = 0

; --- complex --- a scalar at tier C; the `~` sigil: ~(re im) builds (twin re im), a bare ~x
; lifts a real or conjugates (an involution); i = ~(0 1). a rank-N complex array packs (re,im)
; into a `c`-typed array; the net is the complex SUM of the cells, $v = $(net v).
; re/im/arg read ELEMENTWISE over a tray: a twin-tray answers the gem-tray of that component,
; and a REAL tray lifts the scalar law -- re hands it back WHOLE (a real tray is its own real
; part, tier and all), im answers its shape zeroed. an object tray reads the zero point, like
; any non-number: the twin-tray must not fall through to re's identity face, which would answer
; the COMPLEX tray as its own real part.
(assert
 ((* i i) = -1) (~(2 0) = 2) (~(-5 10) = ~(1 2) * ~(3 4)) (twin? ~5) !(twin? 5)
 (~(0 0) = ~0) ((conj ~(2 3)) = ~~(2 3)) ((conj ~(2 3)) = ~(2 -3)) (~(2 3) = ~~~(2 3))
 ((conj (~(2 3) + ~(4 5))) = (conj ~(2 3)) + (conj ~(4 5)))      ; conj is additive
 ((conj (~(2 3) * ~(4 5))) = (conj ~(2 3)) * (conj ~(4 5)))      ; ... and multiplicative
 (~8 = ~3 + ~5) (~(0 0) < i) ((conj ~(2 3)) < ~(2 3))           ; reals embed; (re,im) order: 0<i, conj below
 (2.0 = (re ~(2 3))) (i < 1) !(1 < i) ("~(0.0 1.0)" = (show i))
 (0.0 = (peep (arg ~(1 @(1 0))) 1 0)))           ; arg broadcasts -- (arg ~(1 x)) = atan, elementwise
(assert
 (: v (array 2 ~(1 2) ~(3 4)) (&& (twin-tray = (tier v)) (~(1 2) = (peep v 0 0)) (~(4 6) = (net v))
    (~(2 4) = (peep (v + v) 0 0)) (~(2 4) = (peep (~(2 0) * v) 0 0)) !(v < v)
    ($v = $(net v)) ("@(~(1.0 2.0) ~(3.0 4.0))" = (show v))))
 ; the phases sum as VECTORS to ~(0 8) either way -- and a pure phase is BLUE:
 ; the twin gem is gated by its REAL part, so a sum with no real component
 ; measures 0 however large its magnitude (the truth gate is not the order)
 (0 = $(array 2 ~(3 4) ~(-3 4))) (0 = $`(~(3 4) ~(-3 4)))
 (5 = $(array 2 ~(3 4) ~(0 0))) (~(0.0 8.0) = (net (array 2 ~(3 4) ~(-3 4)))))
(assert
 (: m (twin (star-tray '(2 2) '(1 2 3 4)) (star-tray '(2 2) '(5 6 7 8)))
    (&& (aall ((re m) = (star-tray '(2 2) '(1 2 3 4)))) (aall ((im m) = (star-tray '(2 2) '(5 6 7 8))))
        (aall (m = (twin (re m) (im m)))) ('(2 2) = (shape (re m)))       ; the parts rebuild the whole
        (gem-tray = (tier (re m))) (gem-tray = (tier (im m)))))
 (aall ((re @(1 2 3)) = @(1 2 3))) (star-tray = (tier (re @(1 2 3))))     ; a real tray IS its own re,
 (aall ((im @(1 2 3)) = @(0 0 0))) (star-tray = (tier (im @(1.5 2.5))))   ; and its im is 0, shaped
 ("@()" = (show (re (twin-tray '(0) 0)))) ("@()" = (show (im @())))       ; an empty tray stays a tray
 !(re (top-tray '(2) 0)) !(im (top-tray '(2) 0)))                         ; an object tray is no number

; --- arrays --- (star-tray shape vals) typed ctor (vals 0 zero-fills; gem-/twin-/top-tray for r/c/o); (array shape elem..)
; infers; @(..) rank-1 literal; (iota n) the z-array 0..n-1. the invariant is RANK, not count:
; an array exists at RANK >= 1 and ANY tally, so @(5) is a one-cell array and (iota 1) is @(0).
; only a RANK-0 point demotes to its lone scalar -- there is still NO rank-0 kind, which is what
; keeps the float/wide/complex boxes lean (rank/tally/shape read nil on a scalar). EMPTIES stay
; arrays, carrying the reduction units, so the surface is continuous across every tally.
; + - * // < = and the bitwise << >> & | ^ broadcast numpy-style (`/` promotes the whole
; result to r on one inexact cell); the bitwise five are the WORD LANE, so they ride a
; z-tray and REFUSE anything else whole -- a gem/twin tray, or an object tray, answers the
; zero point rather than mapping elementwise (boxed cells are not words, and a big refuses
; the bits on a star too). masks are the everyday customer: `<` and `>` broadcast to 0/1,
; and & | ^ combine them;
; reduce net/prod/max/min/aall; contract inner (+.x) / outer (o.x); empty reductions = units.
; `=` is a BOOLEAN on EVERY kind, arrays included -- whole-array equality: same shape, every
; cell equal, comparing ACROSS TIERS the way (= 5 5.0) is true. it is NOT a mask; `<` and `>`
; are the mask makers. (an elementwise `=` made a PREDICATE mean "some cell matched", so
; (assert (@(2 4 7) = @(2 4 6))) passed.) a non-numeric operand beside an array, or a shape
; mismatch, reads 0 -- never the arithmetic ()-face (a point/text equals only itself).
; THE WIDTH LAW -- love runs two arithmetic regimes, and the REPRESENTATION picks which,
; never the operator. the GROWING lane is a star's + - * // %: exact forever, climbing
; charm -> sun -> big. the WORD lane is fixed-width two's complement, and it is exactly
; where the rep IS a machine word: (1) the bitwise ops << >> & | ^ always, even on a star
; (see arithmetic), and (2) a WORD-CELLED tray's cells -- a z-tray being contiguous limbs.
; so a z-tray's + - * WRAP where a star's grow, and a big operand TRUNCATES into the cell
; instead of refusing. the rule is the REP, not the kind: an OBJECT tray boxes every cell,
; so a galaxy of bigs routes through the promoting scalar lane and stays EXACT. this is
; the lattice's ALGEBRA reading, not an exception to it: Z/2^w is the strongest +/* theory
; contiguous limbs afford, the way Z is the strongest boxed cells afford. a gem-tray is
; NOT in the word lane either -- floats saturate to ieee-inf.
(assert
 (6 = (tally (star-tray '(2 3) 0))) (2 = (rank (star-tray '(2 3) 0))) ('(2 3) = (shape (star-tray '(2 3) 0)))
 (star-tray = (tier (star-tray '(2 3) 0))) (20 = (peep @(10 20 30) 1 -1)) (-1 = (peep @(10 20 30) 9 -1))
 (3 = (peep (star-tray '(2 2) '(1 2 3 4)) '(1 0) -1)) (@(11 22 33) = @(1 2 3) + @(10 20 30))
 (@(2 4 6) = @(1 2 3) * 2) (gem-tray = (tier ((star-tray '(2) 0) + 1.5))) (star-tray = (tier ((star-tray '(3) 0) < 1)))
 (60 = (net @(10 20 30))) (30 = (max @(10 30 20))) (5 = (net 5)) (aall (1 < 2))
 (@(0 1 2) = (iota 3)) (star-tray = (tier (iota 3))) !(iota 0) !(iota -1) (4950 = (net (iota 100)))   ; iota: jot's range as a z-array
 (aall (@(2.0 3.0) = ((/ 1 2) @(4.0 9.0)))) (@(3.5 5.5) = @(7 11) / 2) (@(3 5) = (// @(7 11) 2))
 (@(4 6) = @(8 12) / 2) !(star-tray '(3) 0) ("@(10 20 30)" = (show @(10 20 30)))
 (aall (@(10 20 30) = (array 3 10 20 30))) !(@(1 2 3) + @(1 2))
 (0 = (@(1 2 3) = @(1 2))) (0 = ('sym = @(1 2 3))) (0 = ("ab" = @(1 2)))   ; a shape/kind mismatch is 0
 (@(1 2 3) = @(1 2 3)) !(@(1 2 3) = @(1 2 9))                              ; whole-array `=`, a boolean
 (@(1 2) = (gem-tray '(2) '(1.0 2.0)))                                     ; ...across tiers, like (= 5 5.0)
 (0 = (net (star-tray '(0) 0))) (1 = (prod (star-tray '(0) 0))) (aall (star-tray '(0) 0))
 ("@()" = (show (star-tray '(0) 0))) (0 = (tally ((star-tray '(0) 0) + 1)))
 (32 = (inner @(1 2 3) @(4 5 6))) (star-tray = (tier (outer @(1 2) @(3 4))))   ; +.x dot ; o.x outer
 ; the width law: a galaxy's cells are machine words, so its arithmetic WRAPS
 (@(8 9) = (@(3 4) + ((200 2) + 5)))              ; a big TRUNCATES into the cell (2^200 = 0 mod 2^w)
 (star-tray = (tier (@(3 4) + (200 2))))          ; ...and the result never leaves the tray band
 (big? (3 + (200 2)))                             ; where the STAR lane grows exact instead
 (: o (array '(2) (200 2) (200 2))                ; ...as does an OBJECT tray: boxed cells, not limbs
    (aall (o = ((o + (array '(2) 1 1)) - (array '(2) 1 1)))))   ; exact round trip at 2^200
 (? word ("@(ieee-inf ieee-inf)" = (show (@(1.0e308 1.0e308) * @(10.0 10.0)))) 1)  ; a gem-tray SATURATES, never wraps
 (? word (@(-9223372036854775808 0) = (@(9223372036854775807 0) + @(1 0))) 1)   ; max+1 -> min
 (? word (@(9223372036854775807 0) = (@(-9223372036854775808 0) - @(1 0))) 1)   ; ...min-1 -> max
 ; the bitwise five broadcast too -- the word lane, elementwise
 (@(240 19) = (@(255 16) ^ @(15 3))) (@(15 0) = (@(255 16) & @(15 3))) (@(12 6) = (@(8 4) | @(4 2)))
 (@(8 16 24) = (@(1 2 3) << 3)) (@(4 -4) = (@(8 -8) >> 1))       ; >> stays ARITHMETIC per cell
 (@(240 252) = (255 ^ @(15 3))) (@(2 16) = (@(1 1) << @(1 4)))   ; a star broadcasts; counts can be per-cell
 (@(5 6) = (@(5 6) | ()))                                        ; ...and the unit still rides through
 (() = (@(1.5 2.5) ^ @(1 1)))                                    ; a GEM tray is not the word lane: refuses
 (() = ((array '(2) (200 2) (200 2)) & @(255 255)))              ; ...nor an OBJECT tray: boxed cells, not words
 (: m @(1 3 5 7 9) (aall (@(0 1 1 1 0) = ((m > 2) & (m < 8)))))  ; masks combine: the everyday customer
 (: M (inner (star-tray '(2 3) '(1 2 3 4 5 6)) (star-tray '(3 2) '(7 8 9 10 11 12)))  ; 2x2 matmul
    (&& ('(2 2) = (shape M)) (154 = (peep M '(1 1) 0)))) !(inner @(1 2 3) @(1 2)))

; --- chains & lists --- link builds the chain; cap/cup the two projections (caap..cuuup
; the compounds); sort/sortby by the total order; (jot n) the first n charms; the higher-order
; functions live in the prel.
(assert
 (1 = (cap '(1 2 3))) ('(2 3) = (cup '(1 2 3))) (3 = (caup '(2 3 4))) ('(1 2) = (link 1 (link 2 ())))
 !(cap 0) ("x" = (string 'x)) !(two? 'x)          ; no cap at the end of the list; a symbol is NOT a chain -- its name comes via `string`
 ('(0 1 2) = (jot 3)) !(jot 0) (3 = $(jot 3)) ; jot: the first n charms; the empty count is nothing
 ('(2 3 4) = (map (+ 1) '(1 2 3))) (24 = (foldl (*) 1 '(1 2 3 4))) (6 = (foldr (+) 0 '(1 2 3)))
 ('(1 3) = (filter (\ x (x % 2)) '(1 2 3 4))) ('(1 2 3) = (sort '(3 1 2))) ('(3 2 1) = (sortby (>) '(1 2 3)))
 ('(1 2 3 4) = (cat '(1 2) '(3 4))) ('(3 2 1) = (rev '(1 2 3))) ('(1 2) = (map cap (zip '(1 2) '(3 4))))
 (20 = (caup (assoc 2 (list `(1 10) `(2 20))))) ('(1 2) = (take 2 '(1 2 3 4))) ('(3 4) = (drop 2 '(1 2 3 4)))
 (3 = (last '(1 2 3))) ('(1 2) = (init '(1 2 3))) (member? 3 '(1 2 3)) !(member? 9 '(1 2 3))
 (all (\ x (0 < x)) '(1 2 3)) (any (\ x (2 < x)) '(1 2 3))
 (294 = $'(a b c)) (3 = (tally '(a b c))))  ; net sums spellings; tally counts the spine

; --- strings, symbols & mints --- a symbol is interned; (mint 0) adjoins a fresh nameless POINT
; (materially empty, applies const-1); a NOM is a named point (its own kind); () IS THE CORE -- the one
; true nothing (serial-0 mint, printed ()), DISTINCT from 0 and "" though all net 0;
; absence reads it; `$` clamps the net measure, `tally` counts; snip/+/string.
; a missing read answers the zero point under the floor handler welp (no warning surfaced)
(: help welp)
(assert (: z zz-never-pinned (&& !(nom? z) !z (id? z (:)) ("()" = (show z)))))   ; absence: the zero point, wearing ()
(: help 0)
(assert
 (() = (:)) !(nom? ()) !(name? ()) !(0 = ()) !(id? 0 '()) (() = (intern "")) (0 = $()) !() !!$'x
 ("()" = (show ()))
 (nom? (mint 0)) !(name? (mint 0)) (0 = $(mint 0)) !(mint 0)   ; the fresh point: nom? (a real point) but NOT name? (anonymous), materially empty
 (1 = ((mint 0) 5)) (1 = ('xyz 1))               ; units apply const-1; the name index is gone
 !((mint 0) = (mint 0)) (: m (mint 0) (&& (m = m) (id? m m)))  ; distinct always; itself only
 (nom? (nom 'x)) (name? (nom 'x)) (name? 'x) !(two? (nom 'x)) ("x" = (string (nom 'x)))  ; a nom is a NAMED point (nom? AND name?, an ATOM, NOT a chain); its name comes via `string`
 (: a (nom 'x) b (nom 'x) (&& (a < b) !(b < a) !(a = b)))   ; same-name noms order by serial (distinct points)
 ((mint 0) < (nom 'x))                           ; within the mint band: a bare mint below a named symbol
 ((mint 0) < 0)                                  ; the blue floor: every point sits BELOW the numbers (true-blue)
 (: a (nom 'k) b (nom 'k) t (tablet 0) _ (pin t a 1) _ (pin t b 2)
    (&& (1 = (t a)) (2 = (t b)) (2 = $t)))      ; same-name noms are distinct map keys
 (97 = ("abc" 0)) (294 = $"abc") (3 = (tally "abc")) (4 = $3.9) (0 = $-3.9) (7 = $@(3 4)) (5 = (abs -5)) (5.0 = (abs @(3 4)))
 !(string 0) !!(string 1) (1 = (tally (string 0)))   ; a NUL nets nothing: the chain of nothings reaches the BYTES, and (string 0) is one of them
 !(snip (+ "a" 0) 1 2) (97 = $(+ "a" 0))        ; ... an all-NUL text IS nothing, and the NUL byte appends for free
 ("bidden" = (snip "forbidden planet" 3 9)) ("abcd" = (+ "ab" "cd")) (1 = ("hi" 9))
 ('asdf = (intern "asdf")) !((nom 0) = (nom 0)) ("asdf" = (string 'asdf))
 ("\"a\\nb\"" = (show "a\nb")) ("(mint " = (snip (show (mint 0)) 0 6)))   ; a mint prints (mint <serial>) (diagnostic, not an exact reparse)

; --- hashes --- #(k v ..)/(hash ..) build, (tablet 0) the empty hash; #() = the EMPTY
; MAP (the reader emits (tablet 0); () is the zero-point, distinct from 0); #x boxes a datum;
; accessors collection-first peep/pin/pull (peep and pull share the default-if-absent; only pull
; mutates the key away). THREE ABSENCE LANES, one miss machinery: peep (the caller names absence),
; apply ((t k) == (peep t k ()) -- a map is a lookup function, absence the zero point), and
; (missing t k) -- a present k answers, a miss is the missing condition (k the payload).
(assert
 (: b #0 (&& (tablet? b) (1 = $b) (0 = (peep b () 9)) (: _ (pin b () 7) (7 = (b ())))))
 (tablet? #()) (0 = $#()) !#()                      ; #() is the empty map: 0 keys, falsy
 (: t #(1 10 2 20) (&& (20 = (peep t 2 0)) (20 = (t 2)) (99 = (peep t 9 99))))
 (10 = (missing #(1 10) 1)) (id? (:) (missing (tablet 0) 'k)) (0 = (missing #(1 0) 1))   ; missing: the conditioned read; presence beats value
 (50 = (peep (pin (tablet 0) 5 50) 5 0)) (: t #(1 10 2 20) _ (pull t 1 0) (1 = $t))
 (: t #(1 10 2 20) v (pull t 2 0) (&& (20 = v) (1 = $t) (99 = (peep t 2 99)) (-1 = (pull t 9 -1))))  ; pull: value or default, removes the key
 ('(1 2) = (sort (keys #(1 10 2 20)))) ((dig 'k) = (dig 'k)) (tablet? (tablet 0)) !(tablet? 5)
 !(#(1 10) = #(1 10)) (: t #(1 10) (t = t))      ; mutable -> identity =
 (: t (tablet 0) r (pins t 1 10 2 20) (&& (id? r t) (10 = (t 1)) (20 = (t 2)) (2 = $t)))  ; pins: multi-pin onto a LIVE tablet, returns it (the mutable twin of #(..))
 (: t (pins (tablet 0) 1 10 2) (&& (10 = (t 1)) (1 = $t)))    ; an odd trailing key is dropped
 (40 = (: t #(1 10) _ (t <- 4 40) (t -> 4 0))))  ; the infix accessors

; --- the EMPTY SPELLINGS (the nothing surface) --- each kind's empty by its sigil: () the
; chain (the zero-point, DISTINCT from the number 0), "" the string, @() the array, #() the
; map, ~(0 0) the zero complex, (cask 0) the buffer -- all net 0 -> blue -> false. spelled via
; 0/() too: @()/#() are the EMPTIES (the reader emits (iota 0)/(tablet 0)); @0/#0 wrap the
; VALUE 0 (a ONE-CELL array, a box) -- both are PRESENT and both still net 0, so the empty
; and the wrapped zero are told apart by tally, not by truth. every nothing is a MASK;
; () the bare-ish floor.
(assert
 !() !0 !"" !@() !#() !~(0 0) !(cask 0)           ; every empty is false (blue)
 (nil? ()) (nil? 0) !(= () 0) !(id? () 0)        ; () (zero-point) and 0 (number) are DISTINCT, both nothing
 ("@()" = (show @())) (1 = (rank @())) (0 = (tally @())) (star-tray = (tier @()))  ; @() the empty array, round-trips
 (tablet? #()) (0 = $#()) ("#()" = (show #()))   ; #() the empty map, show round-trips it
 (tray? @0) !(0 = @0) (1 = (rank @0)) (1 = (tally @0))   ; @0 wraps the VALUE 0 -> a ONE-CELL array
 ("@(0)" = (show @0)) (0 = (peep @0 0 -1))              ; ...which shows and reads back as one
 (tablet? #0) (1 = $#0) !!#0)                       ; #0 wraps the VALUE 0 -> a box (present)

; --- buffers --- (cask n) mutable zeroed bytes; (cask charlist) a cask HOLDING those bytes
; (the lane `string` wears for a charlist -- the bulk way in, so a byte blob never needs a
; string to ride); peep/pin a byte 0..255; `$` nets the charms (a zeroed cask is nothing);
; pour; identity equality.
(assert
 (: b (cask 3) _ (pin b 0 65) (65 = (peep b 0 0))) (0 = $(cask 4)) (4 = (tally (cask 4)))
 !(cask 3) (65 = (: b (cask 1) _ (pin b 0 65) $b))  ; zeroed -> nothing; bytes are charms (lockstep at bufs)
 (: b (cask 1) _ (pin b 0 257) (1 = (peep b 0 0))) (: b (cask 4) _ (pour b 0 "ABCD" 0 4) (68 = (peep b 3 0)))
 (: b (cask '(200 65 0 255)) (&& (4 = (tally b)) (&& (200 = (peep b 0 0)) (255 = (peep b 3 0)))))   ; the charlist lane: high bytes and an embedded NUL ride
 (: b (cask '(65 66)) ("AB" = (string b)))          ; ... and it reads back as those bytes (identity equality, so compare CONTENT)
 ("" = (cask ())) ("" = (cask 0))                   ; no empty cask: both empties ARE ""
 !((cask 2) = (cask 2)))

; --- reader operators --- reading is STRUCTURAL and environment-free: the reader knows tokens,
; parens, strings, and the value surface (' quote = one-operand \, ` list, # hash, @ tup, ~ twin)
; -- NO operator tables, so the same reader serves data and code. the LEXER LAW splits by leading
; char: a name token (alnum/_) keeps - ? ! and the prime inside (kebab law; a LEADING ' is still
; quote), a punctuation-led token is a SIGIL -- a maximal run of operator chars, ONE PLAIN SYMBOL
; when spaced, fused to its datum as (mono (run datum)) when GLUED mid-list. code then factors
; sigils at COMPILE time via `opfix` against the ONE `operators` table (symbol -> arity, or
; (name . arity) an alias; the DEFAULT is dyadic at house grip, right-handed, so a fresh punct
; operator needs no entry -- the tables themselves are book-private since birth, the egg mops them,
; and `fixity` is the one door left onto them: (fixity nm v) pins nm's row and answers the row it
; REPLACED in the same shape, so (fixity nm (fixity nm new)) restores and () both clears a row and
; reports an absent one. a shape the table refuses is rolled back and SCARED, never pinned. the
; table is read live, so a declaration governs every form compiled after it -- haskell's rule). the FACTORIZATION
; LAW: greedy longest-prefix, no backtracking; every factor but the last is arity ONE; the last
; takes the positional valence -- with a left operand it captures the whole APPLICATION SPAN on
; that side, because juxtaposition binds tighter than any infix operator ((a b = c d) is
; (= (a b) (c d)), (5 = abs -3) is (= 5 (abs -3))), nesting by GRIP and, at equal grip, by the
; operator's HAND -- arithmetic is LEFT-handed ((1 - 2 - 3) is -4) and everything else RIGHT;
; with none it reads plain, so (+ 1 2), '+, (+)-as-a-value all work, and a token that doesn't
; factor stays atomic (&& >>= :- ?-). a sigil at one binds TIGHTEST; the CURRY LAW folds operands
; missing at the end of a list to the partial application. the VALENCE LAW: glued is monadic,
; spaced is dyadic -- everywhere, HEAD INCLUDED ((<x = y) is (= (cap x) y)); only : ? \ stay the
; form's operator at head (they take the whole list); + and - are runs like every other
; punctuation, and the ONE exception is that a numeral has to start somewhere -- a DIGIT (or a `.`,
; for -.5) right after them makes a NUMBER instead (-3 a number, -x negate x, --5 -(-5), -(f x)
; negate). the monadic words: < cap, > cup (<< >> <> >< the compounds), + net,
; * prod, | abs, - negate, / reciprocal, % fraction, ? bit, $ sat, ! nil?, . dot. the COMMA is the
; loosest layer, above infix: a lone one-char datum (never fused), the list split into CLAUSES at
; top-level commas BEFORE factoring, the whole the `,` sequencing macro (do its word alias) --
; effects in source order, the last clause answers, empty clauses drop; in a FILE the chain needs
; its outer parens, a repl line is one expression. in anger: test/operator.l, test/infixop.l.
(assert
 ('(1 (\ x) 3) = `(1 ''x 3)) ('(1 2 3 4) = (: xs '(2 3) ('(1) + xs + '(4))))
 (532 = $"hello") (5 = (tally "hello")) (42 = $42) (1 = !0) (0 = !5) !!5
 (i = ~(0 1)) (~(2 3) = (twin 2 3)) ('~x = '(conj x)) (lit? dot)
 ("(dot x)" = (show (opfix '(. x)))) ('! = (caup '(a ! b)))   ; opfix factors -- a lone folded clause splices up (cmp via show: internal forms stay 0-tailed); quotes stay data
 (6 = ((\ x' (x' + 1)) 5)) (nom? 'ab') (7 = (: n' 7 n'))   ; the prime: x' is a name (binder, symbol, let); leading ' is still quote
 (5 = --5) (5 = ++5) (-5 = ---5) (: x 5 (-5 = -x))   ; + and - are runs: --5 is -(-5), -x is negate x -- symmetric with !!5 / !x
 (-3 = -3) (-0.5 = -.5) (5 = (2 - -3))               ; ...and a digit or . after them still starts a NUMERAL (the whole exception)
 (3 = 1 + 2) (7 = 1 + 2 * 3) ('b = 0 ? 'a 'b) ('big = (1 < 2) ? 'big 'small)
 (-4 = 1 - 2 - 3) (1 = 8 / 4 / 2)                ; the LEFT hand: ((1 - 2) - 3), ((8 / 4) / 2)
 (: was (fixity '<+> '(2 60 1)) t (opfix '(a <+> b <+> c)) _ (fixity '<+> was)   ; the fixity door,
    (&& (was = ()) (t = '(<+> (<+> a b) c))))     ;   restored inside the form: the next one reparses
 ('(1 2 3) = 1 >< 2 >< '(3))                     ; ... while a right-handed band still folds right
 (3 = abs -3) (66 = (2 + 3 4))                   ; application binds tighter than infix: (= 3 (abs -3)), (+ 2 (3 4))
 (1 = (3 != 4)) (0 = (3 != 3))                   ; the factorization law: != = ! of =
 (197 = ($"ab" + 2))                             ; one binds tightest: (+ ($ "ab") 2)
 (3 = ((1 +) 2))                                 ; the curry law: (1 +) folds to (+ 1)
 (1 = <'(1 2 3)) (2 = <>'(1 2 3)) ('(3) = >>'(1 2 3))   ; glued runs are MONADIC
 (6 = +'(1 2 3)) (-1 = +'(-2 1)) (24 = *'(1 2 3 4))     ; +/ and */ -- the net, unclamped
 (5 = |-5) (-3 = -(1 + 2)) (0.25 = /4) (1 = ?7) (0 = ?"")
 (3 = (+ 1 2)) (1 = ((< 5) 9))                   ; spaced head = prefix call; the section
 (0 = (: mr '(to 0) (<mr = 'lin))) (3 = (:(+ 1 2)))   ; head fuses ((<mr = 'lin)=(= (cap mr) 'lin)); : still whole-list
 ('(mono (< x)) = (caup (cap (sound '(40 102 32 60 120 41)))))  ; the emission, data-lane
 (7 = (+ 1 2, + 3 4)) (3 = (1 ,, 3, )) (7 = (, 6 7))   ; the comma layer: clauses in sequence, the last answers; empties drop
 (20 = (#(1 10 2 20) -> 2 0)) (7 = (((tablet 0) <- 'k 7) 'k)) (9 = ((tablet 0) -> 'k 9))
 (12 = (foldl (+) 0 '(3 4 5))))
; the compile tables (operators/monadics/pureset) are OFF the book post-birth --
; mopped beside macros (a live pin into a compile table would poison every later
; compile); the guarded asserts live in the bootstrapping section's welp window.

; --- macros --- a macro maps an arg list to code; install with `::`. quasiquote is GONE: the
; ` list ctor EVALUATES every element, so QUOTE the literal positions instead of unquoting the
; computed ones -- the convention inverts (numbers self-eval, so bare). the prel ships
; do/let/if/cond/quote, && and || (short-circuiting), L/list, tuple/map/array, :- and ?-, >>= <=<.
(:: 'unless (\ a `('? (cap a) 0 (caup a))))
(assert
 ('ok = (unless 0 'ok)) (0 = (unless 1 'ok)) (3 = (&& 1 2 3)) !(&& 1 0 3)
 ; the false answer is the UNIT, never a 0-fossil: a && with any failing
 ; operand, or a || with none passing, reads () -- blue like 0, but
 ; nothing, not a number
 (id? (&& () 5) ()) (id? (&& 1 0 3) ()) (id? (|| () ()) ()) (id? (|| () 0) ())
 (2 = (|| 0 2 3)) ('(1 2 3) = `(1 2 3)) (6 = (do 1 2 6)) (3 = (let a 1 b 2 (a + b)))
 (3 = (:- (a + b) a 1 b 2)) ('big = (?- 'else (1 < 2) 'big)) (9 = ((<=< (+ 1) (\ x (x * 2))) 4)))

; --- control --- ev compiles+runs; call-cc a one-shot escape; tasks twirl/catch/rest/freeze/
; landed?/scoop/cue? ((twirl f x) forks to an id, (catch p) blocks for the result, (rest 0) yields --
; always catch an orphan). `scoop` collects an orphan WITHOUT naming it: (pid . retval) for one
; task that has finished, () when none have, so a server that twirls a task per client drains
; what ended instead of polling a roster of what might have -- ⚠ and the PAIR carries the
; presence, since a task run for its effect answers () and is handed back all the same.
; the RNG is xoshiro256++, two layers: FUNCTIONAL seed/random
; (explicit-state, reproducible) and GLOBAL rand/randf (a hidden clock-seeded stream; (rng-set
; (wheel n)) pins it). a global `help` takes every raise as (help s a b) -- s the status word,
; scare (1) and more (2) bits; `welp` is the FLOOR HANDLER: keeps the read protocol, answers the
; zero point on a bare scare (the shell's default help is welp with a face). a missing name
; raises (scare 'missing nom) -- ONE LAW AT EVERY DEPTH: a `:` binding read before its pin,
; (missing t k) on a tablet miss, all the same condition; a closure captures its free globals at
; CREATION, so a lambda's missing read fires at the define, not the call. numeral COMPOSE
; counts are CAPPED by the `apcap` box; the numeric power lane caps the RESULT instead
; (~ exponent * log2|base| bits against the box as a word budget), so a size-fixed or
; growthless base runs free ((pin apcap 0 n) retunes both). in anger: test/help.l,
; test/missing.l, test/apcap.l.
(assert
 (3 = (ev '(+ 1 2))) (lit? ev) (41 = (call-cc (\ k (k 41)))) (42 = 1 + (call-cc (\ k 41)))
 (42 = (: p (twirl (\ x (do (rest 0) (x + 1))) 41) (catch p))) (0 = (catch 99999)) (landed? 99999)
 (: (drain _) (? (two? (scoop 0)) (drain ()) 0)                  ; the ring is shared: drain first
    _ (drain ()) p (twirl (\ _ ()) ()) _ (rest 0) s (scoop 0)
    (&& (two? s) (&& (p = (cap s)) (nil? (cup s)))))             ; a () retval is STILL a pair
 !(rest 0) (4 = (tally (wheel 1))) ((rand 10) < 10) (gem? (randf 0))
 (scare? 1) !(scare? 2) (lit? scare)
 (tablet? apcap) (1048576 = (apcap 0))              ; the apcap ceiling, a tunable box
 (: st (wheel 7) ((cap (random st 0)) = (cap (random st 0)))))
; a missing name reads the zero point under the floor handler welp (no warning surfaced)
(: help welp)
(assert (id? (:) not-in-the-book) !not-in-the-book)   ; a nom not in the book
(: help 0)

; --- i/o & ports --- in/out/err are the default ports; getc + putc/puts/putn/putx, per-port
; see/put/... , open/close/pad. the vessels: (tap cl) an input SOURCE over a colist, (jug 0) an
; output PORT with a growable buffer that pours both ways (a PORT, not a cask -- a cask is the
; mutable byte ARRAY); (sip o) draws ONE unit, (drink o) takes all that is READY, (slurp o) drains
; ALL to a value. the datum reader is `sound` and it takes TEXT -- a charlist -- so the whole read
; protocol IS its return value, three ways: the datum CONSED ONTO WHAT IS LEFT, () at a
; clean end, or `torn` where the text ran out inside a shape. ⚠ ask `two?` for the datum, never the
; net: the answer is a cons and a datum may perfectly well be a nothing (reading "()" answers
; (() . rest)). the RESIDUE IS THE ANSWER, so a torn read resumes by handing back more text and
; nothing is stashed in a port -- which is why the old more-bit/port-back protocol is gone.
; and A PORT IS A CHARLIST: (flow p) is one whose tail is a promise that drinks the next gulp,
; so the reader asks for more itself and there is nothing to bridge. a consumer REFLECTS on the
; tail -- a cons is more, () the end, anything else a thunk fired with the unit -- and nothing
; forces implicitly. (once f) is that promise on its own: it runs f at most once and remembers,
; through a tablet, wrapping the answer because a promise of NOTHING is the ordinary case.
; `read` rides sound (ONE form, EVALED); `reads` and `forms` walk a flowing port, evaling and
; collecting; `dot` (the `.` operator, .x) prints x to out and returns it.
; `sound0` is sound's BOOTSTRAP TWIN, same protocol over the PURE LISP SUBSET: parens, comments,
; strings, atoms, ' quote, and none of the sigil surface -- so #(a) is (hash a) to sound and the
; plain symbol # to sound0. it is what reads prel.l before there is a language (doc/io.md).
(assert
 (string? (show out)) ("hi" = (slurp (tap '(104 105)))) (: o (jug 0) _ (print o 42) ("42" = (slurp o)))
 (1 = (cap (sound '(49)))) (nil? (sound ())) (id? torn (sound '(40)))
 (: r (sound '(49 32 50)) (2 = (cap (sound >r))))    ; the residue reads the next datum
 (: r (sound '(40 41)) (&& (two? r) (nil? <r)))      ; ...and the datum () is still a datum
 (: r (sound (flow (tap '(40 49 32 50 41)))) ('(1 2) = <r))   ; a PORT reads as the charlist it is
 (: c (tablet 0) th (once (\ u (: k (peep c 0 0) _ (pin c 0 (k + 1)) k)))
    (&& ((th ()) = (th ())) (1 = (peep c 0 0))))     ; a promise forces ONCE and remembers
 ('(a) = (cap (sound0 '(40 97 41)))) (nil? (sound0 ()))
 (id? torn (sound0 '(40 97)))                        ; torn, unfinished shape
 ('(hash a) = (cap (sound '(35 40 97 41)))) ((intern "#") = (cap (sound0 '(35 40 97 41)))))

; --- bootstrapping --- the key semantics are l closures shared by both compilers (numap,
; add/mul, opfix, boxfix, feel, maps); the egg is WARMED and the evaluator sits on it twice;
; `born` records the hatch (unbound pre-egg); just before birth the egg mops up runtime names.
; the mopped names are GONE post-birth; the floor handler welp answers the miss with the zero point (no warning)
(: help welp)
(assert (lit? ev) (? born (charm? born) 1)
        (? born !macros 1) (? born !poke 1)     ; post-birth the names are gone
        (? born !boxfix 1) (? born !lvm_ret 1) ; (pre-egg they still exist,
        (? born !spin 1)                        ;  hence the born guards)
        (? born !operators 1) (? born !monadics 1) (? born !pureset 1)  ; the compile tables too (2026-07-07)
        (? born !enter 1) (? born !leave 1) (? born !lib 1))  ; the layer door + the source library: the runtime's, off the surface
(: help 0)

; --- modules --- one layer per load, the registry, and `use` the loader -- all the runtime's.
; there is NO enter/leave on the surface: the runtime opens a fresh layer around every module
; load and around the session itself (the base is read-only because it is never the head), and
; the layer verbs live on only inside the loader's closures, mopped at birth like books/setbooks.
; a module arrives by NAME: (use 'x) splices a registered module just below the top -- its names
; resolve on the walk but can never shadow the top's own -- and LOADS it on a miss: the baked
; source library (ai_lib_), or the filesystem walk -- lib/<x>.l off the cwd, then the SEAT
; rungs, <seat>/../lib/ and its love/ subfolder, the seat being the running binary's own home
; (readlink /proc/self/exe): PATH picks the love, the love carries its library. a STRING is an
; explicit path
; (the module named by its basename); a SLASHED name ('x/y) INCLUDES lib/x/y.l into the current
; head instead, no new layer, no registration. test/host/loader.l is the load law in anger (fs
; fixtures, so it lives outside this corpus); here the boot-loaded modules prove the registry
; surface, since every corpus frontend registers rng and kanren (bao too, except the wasm
; shim, whose test harness feeds bao.l bare -- the page is its own repl).
; (from 'name) is the OPAQUE accessor -- currying reaches a member, 'keys introspects, a
; missing module answers () (the presence guard), (from ()) lists the registered names.
(assert
  (lit? (from 'rng)) (lit? (from 'kanren))                      ; boot-loaded, registered
  (member? 'kanren (from ())) (member? 'rng (from ()))          ; the registry lists them
  (nil? (from 'spec-nope))                                      ; the presence guard
  (member? 'walk (from 'kanren 'keys))                          ; 'keys introspects the internals
  (8 = ((from 'kanren 'subst) '(()) 8))                         ; curried member reach
  (nil? (from 'kanren 'spec-missing))                           ; a member miss reads the zero point
  (lit? unify) (lit? rand) (lit? reads))                        ; the boot splices serve the bare names

; --- odds & ends --- show renders a value as its reparsable printed form; (clock t) is ms since
; t and (gauge 0) reports a VM stat; an opaque handle acts as a constant function.
; (nclock t) is clock's fine INTERVAL twin: monotonic ns minus t (ms-degraded where a frontend
; has no ns source, so monotone everywhere). (please x) is a collection on demand -- () asks for
; a minor, a positive charm for a major -- answering the collection count, so two asks order
; strictly; the worst pause of each kind rides gauge[14]/[15] in words (test/host/gcpause.l
; proves the minor's flat in the live set).
(assert (string? (show @(1 2 3))) (charm? (clock 0)) (1 = ((cask 4) 'x))
        (16 = tally (gauge 0)) (0 <= nclock (nclock 0)))
(: c1 (please ()) c2 (please 1)                  ; the sequence law orders the two asks
 (assert (c1 < c2)))
