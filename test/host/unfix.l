; --- test/host/unfix.l: lib/unfix.l, the right inverse to opfix ---
; the law under test is (opfix (sound (unfix-spell x))) = (opfix x). it is checked three
; ways: on the shapes that pin the algorithm, on the sigil map (BEHAVIOURALLY -- against
; the live compiler, since `monadics` is mopped and unfix has to keep a copy), and over
; real source, every top-level form of a handful of files that between them use every
; corner of the surface. run standalone from the repo root:
;   cat test/00-init.l test/host/unfix.l | out/host/love
;
; ⚠ this gate is the reason unfix may be trusted at all. the maximal spelling is a
; PROPOSAL -- if the grip algebra here ever drifts from op-steal's, the fallback quietly
; eats the difference and nothing else in the tree would notice, so the count of forms
; that KEPT the maximal spelling is asserted, not just the law.
(use 'unfix)

(: (u-rd s) (cap (sound s))                         ; the first datum of s
   (u-law? x) (: c (unfix-spell x) r (sound c)      ; spelled, read back, factored, same?
                 (? (two? r) (= (opfix (cap r)) (opfix x)) 0))
   (u-both? x) (&& (u-law? x) (unfix-max? x))       ; ..and it did not fall back
   (u-say s) (unfix-spell (u-rd s)))

; --- the shapes. the left column is what opfix must still answer; the right is the
; surface unfix chose, and it is the MINIMAL one for that tree.
(assert
 ("(2 * 3 + 4)"       = (u-say "(2 * 3 + 4)"))      ; grip: * climbs into +
 ("(a + b + c)"       = (u-say "(a + b + c)"))      ; hand: the additive band folds LEFT
 ("(a >< b >< c)"     = (u-say "(a >< b >< c)"))    ; ..and cons folds RIGHT
 ("(a + (b - c))"     = (u-say "(+ a (- b c))"))    ; a right operand at EQUAL grip needs them
 ("((a >< b) >< c)"   = (u-say "(>< (>< a b) c)"))  ; ..and so does a left one, right-handed
 ("(f x y)"           = (u-say "(f x y)"))          ; a span stays a span
 ("(f (g x) y)"       = (u-say "(f (g x) y)"))      ; juxtaposition is FLAT: a kid takes parens
 ("(a b = c d)"       = (u-say "(= (a b) (c d))"))  ; ..but a span rides bare as either operand
 ("(f (a = b))"       = (u-say "(f (= a b))"))      ; an infix kid of a span does not
 ("(+ a b c)"         = (u-say "(+ a b c)"))        ; three operands is an APPLICATION, stays prefix
 ("((mov r3 r12))"    = (u-say "((mov r3 r12))"))   ; a list of one may be data -- opfix's call
 ("(: a 1 b 2 (a + b))" = (u-say "(: a 1 b 2 (+ a b))"))   ; binder names verbatim, values factored
 ("(\\ x y (x + y))"  = (u-say "(\\ x y (+ x y))"))        ; \ args verbatim, body factored
 ("(<>x)"             = (u-say "((cap (cup x)))"))  ; a glued run, and the 1-list it needs
 ("(cap >x)"          = (u-say "(cap (cup x))"))    ; ⚠ NOT (<>x) -- that reads as the 1-list above,
                                                   ;   so the HEAD stays spelled and the operand glues
 ("(f <x)"            = (u-say "(f (cap x))"))      ; glue is free anywhere but the outermost
 ("'a"                = (u-say "'a"))               ; a quote form is already its own datum
 ("(f $ a + b)"       = (u-say "($ f (+ a b))")))   ; $ at the loosest grip

; --- the sigil map, asked of the COMPILER. unfix keeps a copy of the monadics rows
; (the table is mopped and has no read door); this is what stops the copy drifting.
; for each (nom . sigil): the glued sigil must factor to exactly that nom.
(assert
 (: (row? e) (: nm <e s <>e (= (opfix (u-rd (s + "zz"))) (list nm 'zz)))
    (all row? umono)))

; --- real source. these six use between them: infix at every band, glued runs, `,`,
; fixity, quoted data, define-sugar, nested lambdas and the pure-lisp subset.
(assert
 (: srcs '("lib/lint.l" "love/prel.l" "love/p1.l" "test/spec.l" "test/precedence.l" "test/valence.l")
    (rdall s a) (: r (sound s)
                   (? (two? r) (rdall >r (link <r a)) (id? r 'torn) 'torn (rev a)))
    (forms p) (: q (open p "r") s (? q (slurp q) ())
                 (? (string? s) (: ds (rdall s ()) (? (id? ds 'torn) () ds)) ()))
    (file? p) (: ds (forms p) (&& (two? ds) (all u-both? ds)))
    (all file? srcs)))

(: _ (puts "unfix: ok\n") ())
