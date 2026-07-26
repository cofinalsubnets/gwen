; precedence (grip) and associativity (hand) -- infix that binds like schoolbook math
; (doc/precedence.md). each operator's table entry carries a GRIP and a HAND; an incoming operator
; steals a pending filled operand iff its grip beats the frame's, so a TIGHTER operator climbs in,
; and EQUAL grip goes to the incomer's hand: RIGHT-handed steals (its band folds right), LEFT-handed
; yields (its band folds left). ARITHMETIC IS LEFT-HANDED -- (1 - 2 - 3) is -4, as everywhere else --
; and everything else stays right, which is the flat fold the surface started with. bands:
; multiplicative 60, additive 50, comparison 40, logical 30, house 27 (coined ops), cons 25,
; assignment 20, cond 10. we assert BOTH the factored TREE (via the live `opfix`, the source pass both
; compilers run -- op-core proper is book-private) and the VALUE, on cases where the grouping is
; observable in the result.
(assert
 ; --- the tree: opfix factors a quoted form; quote interiors stay data, so '(2 * 3 + 4) is the raw
 ; datum list and (opfix ..) is the factored tree itself (a lone folded clause splices up, no wrapper). ---
 ((opfix '(2 * 3 + 4)) = '(+ (* 2 3) 4))            ; * tighter than + -- schoolbook
 ((opfix '(a * b + c)) = '(+ (* a b) c))            ; the tighter op on the LEFT climbs in
 ((opfix '(a + b * c)) = '(+ a (* b c)))            ; and on the right steals
 ((opfix '(a + b + c)) = '(+ (+ a b) c))            ; equal grip, LEFT hand -> the additive band folds left
 ((opfix '(10 - 2 - 3)) = '(- (- 10 2) 3))          ; same, subtraction -- the reading every other language has
 ((opfix '(a / b / c)) = '(/ (/ a b) c))            ; the multiplicative band is left-handed too
 ((opfix '(a >< b >< c)) = '(>< a (>< b c)))        ; but cons stays RIGHT-handed -- a builder folds right
 ((opfix '(a <+> b <+> c)) = '(<+> a (<+> b c)))    ; and a coined operator defaults to the right hand at house grip
 ((opfix '(3 = 1 + 2)) = '(= 3 (+ 1 2)))            ; + (50) over = (40): the spec.l infix-assert law holds
 ((opfix '(a < b && c < d)) = '(&& (< a b) (< c d))) ; && (30) below comparison (40)
 ((opfix '(a < b || c < d)) = '(|| (< a b) (< c d))) ; || likewise
 ((opfix '(a < b & c < d)) = '(& (< a b) (< c d)))   ; bitwise & (30) likewise
 ((opfix '(1 < 2 ? x y)) = '(? (< 1 2) x y))        ; ? (10) loosest -> the C-ternary read
 ((opfix '(a + b >< c)) = '(>< (+ a b) c))          ; >< (25) the loosest builder, below arithmetic
 ((opfix '(a :+ b >< c)) = '(>< (:+ a b) c))        ; a coined op (house 27) beats >< (25)
 ((opfix '(a :+ b + c)) = '(:+ a (+ b c)))          ; arithmetic (50) beats a coined op (27)

 ; --- the two apply operators: ** flip-applies (a ** b = (b a)) at the TIGHTEST grip (70), $ weak-
 ; applies (a $ b = (a b), the haskell $) at the LOOSEST (5). glued $x is saturate, a separate valence. ---
 ((opfix '(a ** b * c)) = '(* (** a b) c))          ; ** (70) tighter than * (60): folds before the multiply steals
 ((opfix '(a * b ** c)) = '(* a (** b c)))          ; on the right too
 ((opfix '(f $ a + b)) = '($ f (+ a b)))            ; $ (5) below arithmetic: the sum groups, then weak-applies
 ((opfix '(f $ g $ x)) = '($ f ($ g x)))            ; equal grip -> right-assoc, exactly haskell's $
 ((opfix '(a ** b $ c)) = '($ (** a b) c))          ; ** binds, $ is the loose outer apply

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
 ('big = (1 < 2 ? 'big 'small))                       ; the C-ternary value
 (4 = (3 ** (+ 1)))                                   ; ** flip-applies: ((+ 1) 3) = 4
 (8 = (2 * 3 ** (+ 1)))                               ; ** tighter than *: (* 2 ((+ 1) 3)) = (* 2 4)
 (6 = ((+ 1) $ 2 + 3))                                ; $ loosest: ((+ 1) (+ 2 3)) = ((+ 1) 5)
 (13 = ((+ 1) $ (+ 2) $ 10))                          ; right-assoc: ((+ 1) ((+ 2) 10)) = ((+ 1) 12)
 (0 = $-5) (3 = $2.9)                                 ; glued $x is UNCHANGED -- saturate, not weak-apply

 ; --- the FIXITY DOOR: (fixity nm v) declares an operator's shape at run time and answers the row
 ; it replaced, in the shape it takes -- so (fixity nm (fixity nm new)) restores exactly. every
 ; assert here RESTORES inside its own form: the corpus is read form by form, so a live row would
 ; change how the NEXT form parses. the table is read live, so `opfix` shows the change at once. ---
 (: was (fixity '<+> '(2 60 1))                       ; haskell's `infixl 7 <+>`
    tree (opfix '(a <+> b <+> c))
    _ (fixity '<+> was)
    (&& (was = ()) (tree = '(<+> (<+> a b) c))))      ; undeclared before; left-handed after
 (: _ (fixity '<+> '(2 60))                           ; `infixr 7` -- the hand defaults right
    tree (opfix '(a <+> b <+> c))
    _ (fixity '<+> ())
    (tree = '(<+> a (<+> b c))))
 (: _ (fixity '<+> '(2 60 1))
    prev (fixity '<+> ())                             ; () CLEARS, and answers what it cleared
    (&& (prev = '(2 60 1)) ((opfix '(a <+> b * c)) = '(<+> a (* b c)))))   ; back to house grip
 ((fixity '<+> ()) = ())                              ; clearing an absent row is a no-op answering ()
 (: _ (fixity '=: '(pin 3 20 0))                      ; the nom-led ALIAS form, like <- itself
    tree (opfix '(t =: k v))
    _ (fixity '=: ())
    (tree = '(pin t k v)))
 (: was (fixity '+ '(2 70 1))                         ; a CORE operator can be re-gripped ..
    tree (opfix '(a + b * c))
    _ (fixity '+ was)                                 ; .. and put back
    (&& (tree = '(* (+ a b) c)) ((opfix '(a + b * c)) = '(+ a (* b c)))))
 ; a shape op-ent refuses is ROLLED BACK and scared -- never pinned, because a bad row does not
 ; error, it silently demotes its operator to the fresh-punct default.
 (!(catch (\ _ (fixity '^^ '(0 60 1)))))              ; arity 0
 (!(catch (\ _ (fixity '^^ '(9 60 1)))))              ; arity 8+
 (!(catch (\ _ (fixity '^^ '(2 zzz 1)))))             ; a grip that is not a charm
 (!(catch (\ _ (fixity 'plain '(2 60 1)))))           ; and the name must be an all-punct sigil
 ((opfix '(x ^^ y ^^ z)) = '(^^ x (^^ y z)))          ; the table survived all four intact

 ; --- && / || stay lazy: the macro rides ?/: , so the right arm never fires when the left decides it.
 ; a mutable tablet witnesses the non-evaluation (kept LOCAL in a body-having `:`). ---
 (: t (tablet 0) _ (pin t 0 0)
    r (1 || (pin t 0 999))                            ; 1 is truthy -> the pin never runs
    (&& (r = 1) (0 = (peep t 0 0))))
 (: t (tablet 0) _ (pin t 0 0)
    r (0 && (pin t 0 999))                            ; 0 is false -> && short-circuits, the pin never runs
    (&& (! r) (0 = (peep t 0 0)))))
