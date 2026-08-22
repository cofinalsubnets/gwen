; precedence (band) and associativity (hand) -- infix that binds like schoolbook math
; (the table is love/prel.l's). each operator's table entry carries a BAND and a HAND; an incoming operator
; steals a pending filled operand iff its band beats the frame's, so a TIGHTER operator climbs in,
; and EQUAL band goes to the incomer's hand: RIGHT-handed steals (its band folds right), LEFT-handed
; yields (its band folds left). ARITHMETIC IS LEFT-HANDED -- (1 - 2 - 3) is -4, as everywhere else --
; and so are the shifts and the bitwise ops; everything else stays right, which is the flat fold
; the surface started with. love/prel.l holds the numbers and the asserts below hold the order,
; which is all a band means. we assert BOTH the factored TREE (via the live `opfix`, the source pass both
; compilers run -- op-core proper is book-private) and the VALUE, on cases where the grouping is
; observable in the result.
(assert
 ; --- the tree: opfix factors a quoted form; quote interiors stay data, so '(2 * 3 + 4) is the raw
 ; datum list and (opfix ..) is the factored tree itself (a lone folded clause splices up, no wrapper). ---
 ((opfix '(2 * 3 + 4)) = '(+ (* 2 3) 4))            ; * tighter than + -- schoolbook
 ((opfix '(a * b + c)) = '(+ (* a b) c))            ; the tighter op on the LEFT climbs in
 ((opfix '(a + b * c)) = '(+ a (* b c)))            ; and on the right steals
 ((opfix '(a + b + c)) = '(+ (+ a b) c))            ; equal band, LEFT hand -> the additive band folds left
 ((opfix '(10 - 2 - 3)) = '(- (- 10 2) 3))          ; same, subtraction -- the reading every other language has
 ((opfix '(a / b / c)) = '(/ (/ a b) c))            ; the multiplicative band is left-handed too
 ((opfix '(a >< b >< c)) = '(>< a (>< b c)))        ; but cons stays RIGHT-handed -- a builder folds right
 ((opfix '(a <+> b <+> c)) = '(<+> a (<+> b c)))    ; and a coined operator defaults to the right hand at house band
 ((opfix '(3 = 1 + 2)) = '(= 3 (+ 1 2)))            ; + over = : the spec.l infix-assert law holds
 ((opfix '(a < b && c < d)) = '(&& (< a b) (< c d))) ; && below comparison
 ((opfix '(a < b || c < d)) = '(|| (< a b) (< c d))) ; || likewise, and a rung under &&
 ((opfix '(a && b || c && d)) = '(|| (&& a b) (&& c d)))   ; ..so a mix is the disjunction it reads as
 ((opfix '(c && 0 < c && c < 10))                   ; ..and a guard chain nests for pat.l to flatten
  = '(&& c (&& (< 0 c) (< c 10))))
 ; the bitwise ops and the shifts sit between comparison and arithmetic, in C's order.
 ((opfix '(a + b & c)) = '(& (+ a b) c))            ; + over & -- an align-up owes no parens
 ((opfix '(a | b + c)) = '(| a (+ b c)))            ; ..and over | the same way
 ((opfix '(a & b | c ^ d)) = '(| (& a b) (^ c d)))  ; & tighter than ^ tighter than |
 ((opfix '(a & b & c)) = '(& (& a b) c))            ; ..and the bitwise bands fold left
 ((opfix '(x & 15 = 15)) = '(= (& x 15) 15))        ; ..all of it above comparison, so a mask reads
 ((opfix '(x ^ y = z)) = '(= (^ x y) z))
 ((opfix '(v >> 8 & 255)) = '(& (>> v 8) 255))      ; shifts over the bitwise trio ..
 ((opfix '(f & 1 << n)) = '(& f (<< 1 n)))          ; ..which is the reading a bit test wants
 ((opfix '(x << 8 + y)) = '(<< x (+ 8 y)))          ; ..and under additive, so a shifted sum owes parens
 ((opfix '(a << b << c)) = '(<< (<< a b) c))        ; the shift band folds left
 ((opfix '(1 < 2 ? x)) = '(? (< 1 2) x))            ; ? loosest -> the whole comparison is the condition
 ; the N-ARY arity: a clause form collects its arms FLAT where a dyadic spans them into one, so
 ; the infix spelling reaches the SAME form the prefix one does -- an else arm included.
 ((opfix '(1 < 2 ? x y)) = '(? (< 1 2) x y))        ; ...and every further arm is another operand
 ((opfix '(c ? a b d e)) = '(? c a b d e))          ; test/result pairs then the else, as `?` always took them
 ((opfix '(x @ p1 b1 p2 b2)) = '(@ x p1 b1 p2 b2))  ; @ likewise: the scrutinee left, the clauses flat
 ((opfix '(f y z @ p b)) = '(@ (f y z) p b))        ; ..and the whole leading span is the scrutinee -- no parens owed
 ((opfix '(c ? x @ p b)) = '(? c (@ x p b)))        ; equal band, right hand: the inner clause form folds first
 ((opfix '(c ? a x @ p b)) = '(? c a (@ x p b)))    ; ..so a match nests in a cond's ELSE ..
 ((opfix '(x @ p c ? a b)) = '(@ x p (? c a b)))    ; ..and a cond in a match's ARM. EQUAL is the fixed point:
                                                    ;   ranking either one over the other breaks the other line
 ; ⚠ an n-ary form takes everything after it, so a clause form inside ANOTHER OF ITS OWN kind owes
 ; parens -- no band can help, the two are one operator. cond survives it, a nested ? meaning what the
 ; flat one means; MATCH DOES NOT, the inner one eating arms that belonged to the outer.
 ((opfix '(x @ p1 y @ q c p2 b2)) = '(@ x p1 (@ y q c p2 b2)))
 ((opfix '(x @ p1 (y @ q c) p2 b2)) = '(@ x p1 (@ y q c) p2 b2))
 ((opfix '(c1 ? a1 c2 ? a2 e)) = '(? c1 a1 (? c2 a2 e)))
 (7 = (0 ? 1 1 ? 7 9)) (7 = (? 0 1 1 7 9))         ; ..the cond equivalence, by value
 ((opfix '(x @ p b $ y)) = '($ (@ x p b) y))        ; $ is looser, so it folds the whole match and takes it
 ((opfix '(c ? a + b)) = '(? c (+ a b)))            ; ⚠ an n-ary frame yields its last RIGHT operand to a tighter op
 ((opfix '(? c a b)) = '(? c a b))                  ; ..and the prefix spelling is untouched, a fixed point as ever
 (: r (grip -1 '? ()) _ (grip -1 '? r) (r = 10))      ; the arity is the LANE, so the door names it and restores exactly
 ((opfix '(a + b >< c)) = '(>< (+ a b) c))          ; >< the loosest builder, below arithmetic
 ((opfix '(a :+ b >< c)) = '(>< (:+ a b) c))        ; a coined op at house band beats ><
 ((opfix '(a :+ b + c)) = '(+ (:+ a b) c))          ; ..and arithmetic, and every other band:
 ((opfix '(a :+ b ** c)) = '(** (:+ a b) c))        ;   house is the TIGHTEST, so an undeclared
 ((opfix '(a :+ b && c)) = '(&& (:+ a b) c))        ;   operator groups before anything can steal
 ((opfix '(a + b :+ c)) = '(+ a (:+ b c)))          ;   from it, on either side

 ; --- the two apply operators: ** flip-applies (a ** b = (b a)) at the TIGHTEST band, $ weak-
 ; applies (a $ b = (a b), the haskell $) near the loose end -- the infix lambda alone is looser.
 ; glued $x is saturate, a separate valence. ---
 ((opfix '(a ** b * c)) = '(* (** a b) c))          ; ** tighter than *: folds before the multiply steals
 ((opfix '(a * b ** c)) = '(* a (** b c)))          ; on the right too
 ((opfix '(f $ a + b)) = '($ f (+ a b)))            ; $ below arithmetic: the sum groups, then weak-applies
 ((opfix '(f $ g $ x)) = '($ f ($ g x)))            ; equal band -> right-assoc, exactly haskell's $
 ((opfix '(a ** b $ c)) = '($ (** a b) c))          ; ** binds, $ is the loose outer apply

 ; --- the infix lambda is LOOSER than $, so a body runs to the end of the form and swallows the
 ; $, which is the reading haskell's lambda has. handing a lambda TO $ spends the parens. ---
 ((opfix '(x \ f $ g x)) = '(\ x ($ f (g x))))      ; the body takes the $ ..
 ((opfix '(x \ a + b)) = '(\ x (+ a b)))            ; ..and everything tighter, which is all of them
 ((opfix '((x \ x + 1) $ 9)) = '($ (\ x (+ x 1)) 9))  ; ..so applying one owes its parens

 ; --- idempotence: factored output carries operators only in head position, so re-running is a no-op ---
 ((opfix (opfix '(a * b + c))) = (opfix '(a * b + c)))

 ; --- the value: pick numbers where the grouping shows in the result ---
 (10 = (2 * 3 + 4))                                   ; (+ (* 2 3) 4); the old flat read was (* 2 (+ 3 4)) = 14
 (14 = (2 * (3 + 4)))                                 ; parens override
 (7 = (1 + 2 * 3))
 (5 = (10 - 2 - 3))                                   ; left hand: ((10 - 2) - 3); the right hand would read 11
 (1 = (8 / 4 / 2))                                    ; ((8 / 4) / 2); the right hand would read 4
 (3 = (2 - 3 + 4))                                    ; the band folds left as one run: ((2 - 3) + 4)
 (1 = (3 = 1 + 2))                                    ; the assert relation itself, unparenthesized
 (1 = (0 < 5 && 5 < 10))                              ; (&& (< 0 5) (< 5 10)) -- both true
 ('big = (1 < 2 ? 'big))                              ; ? is dyadic: the one-armed cond, (< 1 2) first
 (4 = (3 ** (+ 1)))                                   ; ** flip-applies: ((+ 1) 3) = 4
 (8 = (2 * 3 ** (+ 1)))                               ; ** tighter than *: (* 2 ((+ 1) 3)) = (* 2 4)
 (6 = ((+ 1) $ 2 + 3))                                ; $ under arithmetic: ((+ 1) (+ 2 3)) = ((+ 1) 5)
 (10 = ((x \ x + 1) $ 9))                             ; a parenthesized infix lambda, weak-applied
 (10 = ((x \ x + 1) 9))                               ; ..and applied plainly, the same answer
 (13 = ((+ 1) $ (+ 2) $ 10))                          ; right-assoc: ((+ 1) ((+ 2) 10)) = ((+ 1) 12)
 (0 = $-5) (3 = $2.9)                                 ; glued $x is UNCHANGED -- saturate, not weak-apply

 ; --- the GRIP DOOR: (grip ar nm v) declares an operator's shape at run time and answers the
 ; row it replaced, in the shape it takes -- so (grip ar nm (grip ar nm new)) restores exactly.
 ; `ar` is the LANE, the operands the sigil takes from the form around it: 0 glued, 2 spaced, -1
 ; spaced with no bound. every assert here RESTORES inside its own form: the corpus is read form by
 ; form, so a live row would change how the NEXT form parses. the table is read live, so `opfix`
 ; shows the change at once. ---
 (: was (grip 2 '<+> -60)                           ; haskell's `infixl 7 <+>` -- the MINUS is the left hand
    tree (opfix '(a <+> b <+> c))
    _ (grip 2 '<+> was)
    (&& (was = ()) (tree = '(<+> (<+> a b) c))))      ; undeclared before; left-handed after
 (: _ (grip 2 '<+> 60)                              ; `infixr 7` -- a positive band is right-handed
    tree (opfix '(a <+> b <+> c))
    _ (grip 2 '<+> ())
    (tree = '(<+> a (<+> b c))))
 (: _ (grip 2 '<+> -60)
    prev (grip 2 '<+> ())                             ; () CLEARS, and answers what it cleared
    (&& (prev = -60) ((opfix '(a <+> b * c)) = '(* (<+> a b) c))))   ; back to house band -- above *
 ((grip 2 '<+> ()) = ())                              ; clearing an absent row is a no-op answering ()
 (: _ (grip 2 '=: '(pin 20))                        ; the (nom band) ALIAS: a target with no row to lend
    tree (opfix '(t =: k))
    _ (grip 2 '=: ())
    (tree = '(pin t k)))
 (: was (grip 2 '+ -70)                             ; a CORE operator can be re-gripped ..
    tree (opfix '(a + b * c))
    _ (grip 2 '+ was)                                 ; .. and put back
    (&& (tree = '(* (+ a b) c)) ((opfix '(a + b * c)) = '(+ a (* b c)))))
 ; a shape op-ent refuses is ROLLED BACK and scared -- never pinned, because a bad row does not
 ; error, it silently demotes its operator to the fresh-punct default.
 (!(catch (\ _ (grip 2 '^^ 0))))                      ; band 0 -- ⚠ a table MISS answers 0, so zero can never BE a row
 (!(catch (\ _ (grip 2 '^^ '(a b)))))                 ; a shape that is neither charm, nom nor (nom band)
 (!(catch (\ _ (grip 2 '^^ 'zzz))))                   ; an ALIAS to a word with no row of its own
 (!(catch (\ _ (grip 2 'plain 60))))                  ; and the name must be an all-punct sigil
 ((opfix '(x ^^ y ^^ z)) = '(^^ x (^^ y z))))         ; the table survived all four intact

(assert
 ; --- CHAINING, arity -2: a comparison run is the CONJUNCTION of its links, and the operand two
 ; links share is evaluated ONCE. the binds nest inside the && rather than sitting in front of it,
 ; so all three of python's promises hold together -- left to right, once each, and the && still
 ; short-circuits, so a false link never reaches what follows it. each link carries its own
 ; operator, so a run may mix them. ⚠ THE ESCAPE HATCH IS THE PARENS: (a < (b < c)) arrives as ONE
 ; folded datum and never enters the operator list, so it stays the nested comparison -- and the
 ; FACTORED form cannot tell the two apart, which is why the join lives in the walk and could
 ; never have been a rewrite over op-core's output. ---
 (1 < 2 < 3) !(2 < 1 < 3) !(1 < 3 < 2)                ; every link must hold
 (1 < 2 <= 2 < 5)                                     ; ..and they may differ, link by link
 (3 > 2 > 1) !(0 = 1 = 2)                             ; the two that used to be right by accident
 (1 < 1 + 1 < 3)                                      ; a TIGHTER frame between links folds first
 ; ⚠ A LINK MAY BE A SPAN. juxtaposition binds tighter than any operator, so the link is the whole
 ; application -- the frame's right operand has GROWN, and the join must read that as filled the
 ; same way a lone datum is. reading only the head instead is silent: the chain answers a bit
 ; either way, and (97 <= s 0 <= 122) quietly compared 97 to the string.
 (: s "a1" (97 <= s 0 <= 122))                        ; the letter, not the string it sits in
 (: s "a1" !(48 <= s 0 <= 57))                        ; ..and the digit test on that same letter fails
 (: s "ab" (s 0 < s 1 < 99))                          ; both ends span too -- the leading one already did
 (: t (tablet 0) _ (pin t 0 0)                        ; a span link is still evaluated ONCE
    (bump v) (: _ (pin t 0 (1 + (peep t 0 0))) 2)
    r (1 < bump () < 3)
    (&& r (1 = (peep t 0 0))))
 (0 = (1 < (2 < 3)))                                  ; parens opt out: (< 2 3) is 1, and (< 1 1) is 0
 ((show (opfix '(a < b))) = (show '(< a b)))          ; one link is a plain comparison, untouched
 ((show (opfix '(< a (< b c)))) = (show '(< a (< b c))))   ; ..and the prefix spelling stays a fixed point
 (: t (tablet 0) _ (pin t 0 0)                        ; the SHARED operand runs once, not twice
    (bump v) (: _ (pin t 0 (1 + (peep t 0 0))) 2)
    r (1 < (bump ()) < 3)
    (&& r (1 = (peep t 0 0))))
 (: t (tablet 0) _ (pin t 0 0)                        ; ..and a false link never reaches the rest
    (bump v) (: _ (pin t 0 999) 9)
    r (2 < 1 < (bump ()))
    (&& !r (0 = (peep t 0 0)))))
(assert
 (!(catch (\ _ (grip 7 '^^ 60))))                   ; ..and a lane with no key is refused, never minted

 ; --- the GLUED lane through the same door: arity 0, where a row is the WORD the sigil names.
 ; one nom lives in as many lanes as it has faces, and `?` is the pair:
 ; `bit` glued, cond spaced -- the valence law as two keys of one table. ---
 (: r (grip 0 '? ()) _ (grip 0 '? r) (opnom r = 'bit))  ; a clear-and-restore is the non-destructive READ
 ; ⚠ the fixture needs a sigil whose GLUED row is empty -- `was = ()` is half the law. it was
 ; `^` until ^ became jot; `&` and `=` are what is left free. fill one and move this.
 (: was (grip 0 '& 'cup)                            ; coin a glued sigil ..
    tree (opfix '&x)
    _ (grip 0 '& was)
    (&& (was = ()) (tree = '(cup x))))                 ; .. it factors, then clears
 ((unglue (opfix '?5)) = '(bit 5))                    ; the glued lane survived intact

 ; --- && / || stay lazy: the macro rides ?/: , so the right arm never fires when the left decides it.
 ; a mutable tablet witnesses the non-evaluation (kept LOCAL in a body-having `:`). ---
 (: t (tablet 0) _ (pin t 0 0)
    r (1 || (pin t 0 999))                            ; 1 is truthy -> the pin never runs
    (&& (r = 1) (0 = (peep t 0 0))))
 (: t (tablet 0) _ (pin t 0 0)
    r (0 && (pin t 0 999))                            ; 0 is false -> && short-circuits, the pin never runs
    (&& (! r) (0 = (peep t 0 0)))))
