; --- test/host/infix.l: lib/infix.l, the right inverse to opfix ---
; the law under test is (opfix (sound (infix-spell x))) = (opfix x). it is checked three
; ways: on the shapes that pin the algorithm, on the sigil map (BEHAVIOURALLY -- against
; the live compiler, since `monadics` is mopped and infix has to keep a copy), and over
; real source, every top-level form of a handful of files that between them use every
; corner of the surface. run standalone from the repo root:
;   cat test/00-init.l test/host/infix.l | out/host/love
;
; ⚠ this gate is the reason `libra infix` may be trusted at all. the maximal spelling is a
; PROPOSAL -- if the grip algebra here ever drifts from op-steal's, the fallback quietly
; eats the difference and nothing else in the tree would notice, so the count of forms
; that KEPT the maximal spelling is asserted, not just the law.
(use 'q)
(use 'infix)

(: (i-rd s) (cap (sound s))                         ; the first datum of s
   (i-law? x) (: c (infix-spell x) r (sound c)      ; spelled, read back, factored, same?
                 (? (two? r) (= (opfix (cap r)) (opfix x)) 0))
   (i-both? x) (&& (i-law? x) (infix-max? x))       ; ..and it did not fall back
   (i-say s) (infix-spell (i-rd s)))

; --- the shapes. the left column is what opfix must still answer; the right is the
; surface it chose, and it is the MINIMAL one for that tree.
(assert
 ("(2 * 3 + 4)"       = (i-say "(2 * 3 + 4)"))      ; grip: * climbs into +
 ("(a + b + c)"       = (i-say "(a + b + c)"))      ; hand: the additive band folds LEFT
 ("(a >< b >< c)"     = (i-say "(a >< b >< c)"))    ; ..and cons folds RIGHT
 ("(a + (b - c))"     = (i-say "(+ a (- b c))"))    ; a right operand at EQUAL grip needs them
 ("((a >< b) >< c)"   = (i-say "(>< (>< a b) c)"))  ; ..and so does a left one, right-handed
 ("(f x y)"           = (i-say "(f x y)"))          ; a span stays a span
 ("(f (g x) y)"       = (i-say "(f (g x) y)"))      ; juxtaposition is FLAT: a kid takes parens
 ("(a b = c d)"       = (i-say "(= (a b) (c d))"))  ; ..but a span rides bare as either operand
 ("(f (a = b))"       = (i-say "(f (= a b))"))      ; an infix kid of a span does not
 ("(+ a b c)"         = (i-say "(+ a b c)"))        ; three operands is an APPLICATION, stays prefix
 ("((mov r3 r12))"    = (i-say "((mov r3 r12))"))   ; a list of one may be data -- opfix's call
 ("(: a 1 b 2 (a + b))" = (i-say "(: a 1 b 2 (+ a b))"))   ; binder names verbatim, values factored
 ("(\\ x y (x + y))"  = (i-say "(\\ x y (+ x y))"))        ; \ args verbatim, body factored
 ("(<>x)"             = (i-say "((cap (cup x)))"))  ; a glued run, and the 1-list it needs
 ("(cap >x)"          = (i-say "(cap (cup x))"))    ; ⚠ NOT (<>x) -- that reads as the 1-list above,
                                                   ;   so the HEAD stays spelled and the operand glues
 ("(f <x)"            = (i-say "(f (cap x))"))      ; glue is free anywhere but the outermost
 ("'a"                = (i-say "'a"))               ; a quote form is already its own datum
 ("(f $ a + b)"       = (i-say "($ f (+ a b))")))   ; $ at the loosest grip

; --- a value that is not a chain but SPELLS as one. a coin shows as (q '(1 2)), the
; ctor applied to its payload, so as an OPERAND the span rule frees its parens -- but
; only there: as a span element it needs them, and alone it is already a form.
; ⚠ this is also what the law is for. `(+ x <coin>)` has no readable twin -- the text
; reads back as the ctor CALL, never as the coin -- so the check compares what the two
; spellings DENOTE, mine against `show`'s, and both sides go through text.
(assert
 (: c (q 1 2)
    (&& (&& ("(x + q '(1 2))" = (infix-spell (list '+ 'x c)))    ; operand: bare
            ("(f (q '(1 2)) y)" = (infix-spell (list 'f c 'y)))) ; span element: parens
        (&& ("(q '(1 2))" = (infix-spell c))                     ; alone: already a form
            (c = (ev (cap (sound (show c)))))))))                ; ..and it all reads back

; --- the sigil map, asked of the COMPILER. infix keeps a copy of the monadics rows
; (the table is mopped and has no read door); this is what stops the copy drifting.
; for each (nom . sigil): the glued sigil must factor to exactly that nom.
(assert
 (: (row? e) (: nm <e s <>e (= (opfix (i-rd (s + "zz"))) (list nm 'zz)))
    (all row? imono)))

; --- real source. these six use between them: infix at every band, glued runs, `,`,
; fixity, quoted data, define-sugar, nested lambdas and the pure-lisp subset.
(assert
 (: srcs '("lib/lint.l" "love/prel.l" "love/p1.l" "test/spec.l" "test/precedence.l" "test/valence.l")
    (rdall s a) (: r (sound s)
                   (? (two? r) (rdall >r (link <r a)) (id? r 'torn) 'torn (rev a)))
    (forms p) (: q (open p "r") s (? q (slurp q) ())
                 (? (string? s) (: ds (rdall s ()) (? (id? ds 'torn) () ds)) ()))
    (file? p) (: ds (forms p) (&& (two? ds) (all i-both? ds)))
    (all file? srcs)))

(: _ (puts "infix: ok\n") ())
