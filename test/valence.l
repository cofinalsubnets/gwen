; the VALENCE LAW: a glued operator run is MONADIC -- the reader emits
; (mono (run datum)) and opfix factors the run against its own monadic
; table (then the operators table at one; both book-private since birth).
; spaced, a run keeps its dyadic/infix life. this holds EVERYWHERE, head
; position included: (<x = y) reads (= (cap x) y). the ONE exception is the
; special forms : and ? at head (\ too) -- they consume the whole list, so
; fusion (which swallows only the next datum) is suppressed, and minified
; (:(co ..) stays (: (co ..)). the words behind the sigils: < cap, > cup
; (<< >> <> >< the compounds), + net,
; * prod, | abs, - negate, / reciprocal, % fraction, ? bit; $ ! . ride the same lane.
(assert
 ; cap and cup, and the compounds by greedy monadic factorization
 (1 = <'(1 2 3)) ('(2 3) = >'(1 2 3))
 (2 = <>'(1 2 3)) ('(3) = >>'(1 2 3))
 (9 = <<'((9 8) 2)) ('(2) = ><'((1 2) 9))
 ; the content measure and its product twin, + and * turned inward
 (6 = +'(1 2 3)) (294 = +"abc") (0 = +"") (7 = +@(3 4))
 (-1 = +'(-2 1))                                ; the net, UNCLAMPED: $ would read 0
 (~(0 8) = +`(~(3 4) ~(-3 4)))                 ; phases sum as vectors: + glued to `` ` `` -> net over the evaluated list
 (3 = +`(1 2)) ("(saturate (list 1 2))" = (show (opfix '$`(1 2))))   ; + and - fuse to `` ` `` too (a datum-opener like ' # @): monadic net / negate over the evaluated list
 (24 = *'(1 2 3 4)) (7 = *7) (6 = *@(1 2 3))   ; * is prod inward: a star vacuously itself (like +5), a list/array its product
 ; the dyadics turned inward
 (5 = |-5) (5.0 = |~(3 4)) (-3 = -(1 + 2)) (0.25 = /4) (0.5 = %2.5)
 ; the truth panel: the bit is the Iverson bracket
 (1 = ?7) (0 = ?0) (0 = ?"") (0 = !7) (1 = !!7)
 ; glued monadics compose with infix; spaced runs keep their dyadic life
 (15 = (5 + <'(10 20))) (197 = ($"ab" + 2))
 (3 = (1 + 2)) (2 = (5 - 3)) (8 = (5 - -3))
 ; head fusion holds too: a glued run at head is monadic, so it can be an
 ; infix left operand -- (<l = x) is (= (cap l) x), not (< l (= x))
 (1 = (: l '(1 2 3) (<l = 1))) (0 = (: l '(1 2 3) (<l = 2)))
 (1 = (<'(7 8) = 7)) (1 = (: l '((a) 2) (<>l = 2)))
 ; but : and ? at head take the whole list (minified special forms stay legal)
 (3 = (:(+ 1 2))) (5 = (?(< 1 2) 5 9)) (9 = (?(< 2 1) 5 9))
 ; spaced heads stay prefix; the section and curry law are unchanged
 (3 = (+ 1 2)) (3 = ((1 +) 2)) (1 = ((< 5) 9)) (6 = (foldl (+) 0 '(1 2 3)))
 ; the data lane: a glued run READS as the mono node, and round-trips
 ('(f (mono (< x))) = (cap (sound '(40 102 32 60 120 41))))
 ; the words are real, the sigils their spellings
 (-7 = (negate 7)) (0.5 = (reciprocal 2)) (0.25 = (fraction 5.25)) (1 = (bit 9))
 (6 = (net '(1 2 3))) (24 = (prod '(2 3 4))))

; COHERENCE: the new words against the old laws, pinned as one system.
; $ is the clamp of the net (the word now states the law the sigil lived);
; the bit is !! is the sign of the same measure; the monadic words ARE their
; sections; net meets net on arrays and the fold on spines; prod twins
; prod; jot counts what it jots.
(assert
 (0 = $(net '(-2 1))) (-1 = (net '(-2 1)))      ; the net unclamped, $ its one clamp
 (all (\ v ($v = $(net v))) `(7 -5 '(1 -5) @(2 3) "ab" ~(0 2) #(1 10)))
 (all (\ v ((bit v) = !!v)) `(7 0 -3 "" "a" '(0) ~(0 2)))
 (negate = (- 0)) (reciprocal = (/ 1))               ; the words ARE the sections
 ((net @(1 2 3)) = (net @(1 2 3))) ((net @(1 2 3)) = (net '(1 2 3)))
 ((net '(1 2 3)) = (foldl (+) 0 '(1 2 3)))
 ((prod '(2 3 4)) = (prod @(2 3 4))) (24 = (prod '(2 3 4)))
 (5 = (tally (jot 5))) (10 = +(jot 5))          ; jot counts what it jots; gauss agrees
 ((fraction 5.25) = (5.25 % 1)) ((bit 9) = !!9) (!!9 = ?9)
 ; a glued monadic prints back as its source -- byte-exact, like ' / ~ / @
 ("*5" = (show (\ *5))) ("+(-3)" = (show (\ +(-3)))) ("$$0" = (show (\ $$0)))
 (5 = (ev (\ *5))))                                  ; ... and round-trips show -> read -> ev (now prod: *5 = 5)
