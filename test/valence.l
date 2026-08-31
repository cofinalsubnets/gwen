(assert
 ; cap and cup, and the compounds by greedy monadic factorization
 (1 = <'(1 2 3)) ('(2 3) = >'(1 2 3))
 (2 = <>'(1 2 3)) ('(3) = >>'(1 2 3))
 (9 = <<'((9 8) 2)) ('(2) = ><'((1 2) 9))
 ; the content measure and its product twin, + and * turned inward
 (6 = +'(1 2 3)) (294 = +"abc") (0 = +"") (7 = +@(3 4))
 (-1 = +'(-2 1))                                ; the net, unclamped: $ would read 0
 ((0 ~ 8) = +[(3 ~ 4) (-3 ~ 4)])               ; phases sum as vectors: + glued to [ -> net over the evaluated list
 (3 = +[1 2]) ("(saturate (list 1 2))" = show (opfix '$[1 2]))     ; + and - fuse to [ too (a datum-opener like ' # @): monadic net / negate over the evaluated list
 (24 = *'(1 2 3 4)) (7 = *7) (6 = *@(1 2 3))   ; * is prod inward: a star vacuously itself (like +5), a list/array its product
 ; the dyadics turned inward
 (5 = |-5) (5.0 = |(3 ~ 4)) (-3 = -(1 + 2)) (0.25 = /4) (0.5 = %2.5)
 ; the truth panel: the bit is the Iverson bracket
 (1 = ?7) (0 = ?0) (0 = ?"") (0 = !7) (1 = !!7)
 ; glued monadics compose with infix; spaced runs keep their dyadic life
 (15 = 5 + <'(10 20)) (197 = $"ab" + 2)
 (3 = 1 + 2) (2 = 5 - 3) (8 = 5 - -3)
 ; head fusion holds too: a glued run at head is monadic, so it can be an
 ; infix left operand -- (<l = x) is (= (cap l) x), not (< l (= x))
 (1 = (: l '(1 2 3) (<l = 1))) (0 = (: l '(1 2 3) (<l = 2)))
 (1 = (<'(7 8) = 7)) (1 = (: l '((a) 2) (<>l = 2)))
 (3 = (:(+ 1 2))) (1 = (?(< 1 2))) (0 = (?(< 2 1))) (5 = (? (< 1 2) 5 9))
 ; spaced heads stay prefix; the section and curry law are unchanged
 (3 = (+ 1 2)) (3 = ((1 +) 2)) (1 = ((< 5) 9)) (6 = (foldl (+) 0 '(1 2 3)))
 ; the data lane: a glued run reads as the mono node, and round-trips
 ('(f (mono (< x))) = (cap (sound '(40 102 32 60 120 41))))
 ; the words are real, the sigils their spellings
 (-7 = (negate 7)) (0.5 = (reciprocal 2)) (0.25 = (fraction 5.25)) (1 = (bit 9))
 (6 = (net '(1 2 3))) (24 = (prod '(2 3 4))))

(assert
 (-1 = (net '(-2 1)))                           ; the net unclamped ($ is law.l's sat-net)
 (negate = (- 0)) (reciprocal = (/ 1))               ; the words are the sections
 ((net @(1 2 3)) = (net @(1 2 3))) ((net @(1 2 3)) = (net '(1 2 3)))
 ((net '(1 2 3)) = (foldl (+) 0 '(1 2 3)))
 ((prod '(2 3 4)) = (prod @(2 3 4))) (24 = (prod '(2 3 4)))
 (5 = tally (jot 5)) (10 = +(jot 5))          ; jot counts what it jots; gauss agrees
 (^5 = (jot 5)) (^^3 = (jot (jot 3))) (^^^3 = (jot (jot (jot 3))))
 ('(0 1 2 3 4) = ^5) ('(() (0) (0 1)) = ^^3) ('(() (()) (() (0))) = ^^^3)
 (3 = tally ^^3) ((map tally ^^3) = (jot 3))  ; ..the k-th list is the k-th jot
 (7 = (3 ^ 4))                                ; ..while spaced ^ is xor: the lane is the key
 (fraction 5.25 = 5.25 % 1) (bit 9 = !!9) (!!9 = ?9)
 ; a glued monadic prints back as its source -- byte-exact, like ' / ~ / @
 ("*5" = show (\ *5)) ("+(-3)" = show (\ +(-3))) ("$$0" = show (\ $$0))
 (5 = ev (\ *5)))                                  ; ... and round-trips show -> read -> ev (now prod: *5 = 5)
