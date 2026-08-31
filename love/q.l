; q -- rationals: an exact field shipped as a coin over a reduced (num den) payload.
; a post-egg layer, not part of the language: prel is the language and stays minimal, so a
; library like this rides its own out/lib/q.h and each frontend includes the layers it wants
; (the host, love0's corpus, the inle test kernel and port/playdate's rune workbench all do;
; rp2040 does not). love/prel.l first -- q leans on gcd, coin and the die slots.
; +/*/-// cross-multiply and reduce (/ inverts and multiplies; by zero -> (), Q holds
; no inf, so reciprocal powers ride: (-1 (q 2 1)) is (q 1 2)); = is payload equality after reduction (so (q 2 4) =
; (q 1 2)); show is (q (num den)), which reads back since q also accepts the 2-list.
; the die declares its coins stars (slot 7: numerals power through *, the surface net
; answers the coin itself) and pins net-mode 2 (slot 6, RATIO): the C truth path
; reads n/d with the sign exact, so a rational's color is its VALUE's -- (q -1 2) is
; nothing, $ ceils the magnitude, the subtract-and-test sign idiom holds. order too:
; a ratio coin seats in the number band, interleaved with the closed tower by value
; (word-sized components compare by exact cross-multiply, so near-equal rationals
; never tie by double rounding; wider fall to the sign-exact quotients). a method
; gets raw operands, so it owns its domain: ok? admits a star or an (n d) of stars
; (an integer spells (n 1) through `parts` -- it used to ride the ()-*-unit as its
; denominator, which the zero law retired), and anything else
; answers () -- the undefined-op law, and the fence that keeps a foreign operand (a
; chain of coins, say) from dragging gcd into a loop. `reduce`/`ok?`/`parts` are local
; plumbing; q is the only name this leaks.
(module 'q
(: q (: (reduce n d) (? (0 = n) [0 1]                           ; gcd out, sign on num, den > 0
                        (: g (gcd (abs n) (abs d)) s (? (d < 0) -1 1)
                           [(s * n // g) (s * d // g)]))
        (parts v) (? (two? v) v [v 1])                          ; a bare star is n/1
        ; the domain test is the closed star (the fixpoint minus coins): star?
        ; is open now, and an open star as an (n d) component would drag gcd
        ; off the tower it terminates on -- a coin is never a coefficient.
        (nstar? v) (? (coin? v) 0 (id? v (net v)))
        (ok? v) (? (nstar? v) 1
                   (two? v) (nstar? <v && nstar? <>v)
                   0)
        st {0 'q 7 1 6 2}                                       ; slot 7: a rational is a star;
                                                                ; slot 6 mode 2: C truth by VALUE
        ; the four methods land in one `pins` (the mutable-accumulator twin of {..}) -- but as a
        ; separate step, not folded into the literal above: a method names `st` back, and binding a
        ; die that references itself compiles here yet fails in the love0 lane ((q 1 2) + (q 1 3)
        ; came out unequal to (q 5 6)). bind the die, then pin onto it. 1 + / 2 * / 5 - / 8 /.
        _ (pins st
             1 (\ a b (: x (load a) y (load b)                  ; (n1 d1) + (n2 d2)
                  (? (ok? x && ok? y)
                     (: p (parts x) r (parts y)
                        (coin st (reduce (<p * <>r + <r * <>p) (<>p * <>r))))
                     ())))
             2 (\ a b (: x (load a) y (load b)                  ; (n1 d1) * (n2 d2)
                  (? (ok? x && ok? y)
                     (: p (parts x) r (parts y)
                        (coin st (reduce (<p * <r) (<>p * <>r))))
                     ())))
             5 (\ a b (: x (load a) y (load b)                  ; (n1 d1) - (n2 d2)
                  (? (ok? x && ok? y)
                     (: p (parts x) r (parts y)
                        (coin st (reduce (<p * <>r - <r * <>p) (<>p * <>r))))
                     ())))
             8 (\ a b (: x (load a) y (load b)                  ; (n1 d1) / (n2 d2): invert and
                  (? (ok? x && ok? y && <y != 0)                ; multiply; by zero -> () (no inf in Q; cap of a bare star is itself)
                     (: p (parts x) r (parts y)
                        (coin st (reduce (<p * <>r) (<>p * <r))))
                     ()))))
        (\ n (? (two? n) (coin st (reduce <n <>n))              ; (q (n d)) -- the show form
                (\ d (coin st (reduce n d)))))))               ; (q n d) -- the ergonomic curried form

) ; module q
