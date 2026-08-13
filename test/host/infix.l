; --- test/host/infix.l: lib/infix.l, the right inverse to opfix ---
; the law under test is (opfix (sound (infix-spell x))) = (opfix x), on every form whose
; FACTORED IMAGE IS SAYABLE -- opfix mints fresh noms for a pattern binder and for a chain's
; shared operand, and a fresh nom has no spelling at all, so such a form is outside the law by
; construction (the sweep below states that domain and skips it). it is checked three
; ways: on the shapes that pin the algorithm, on the sigil map (BEHAVIOURALLY -- infix now
; READS the glued lane through the grip door, and this asks the compiler whether that read
; agrees with what the sigil actually factors to), and over real source, every top-level form of a handful of files that between them use every
; corner of the surface. run standalone from the repo root:
;   cat test/00-init.l test/host/infix.l | out/host/love
;
; ⚠ this gate is the reason `libra infix` may be trusted at all. the maximal spelling is a
; PROPOSAL -- if the band algebra here ever drifts from op-steal's, the fallback quietly
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
 ("(2 * 3 + 4)"       = (i-say "(2 * 3 + 4)"))      ; band: * climbs into +
 ("(a + b + c)"       = (i-say "(a + b + c)"))      ; hand: the additive band folds LEFT
 ("(a >< b >< c)"     = (i-say "(a >< b >< c)"))    ; ..and cons folds RIGHT
 ("(a + (b - c))"     = (i-say "(+ a (- b c))"))    ; a right operand at EQUAL band needs them
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
 ("(f $ a + b)"       = (i-say "($ f (+ a b))")))   ; $ at the loosest band

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

; --- the sigil map, asked of the COMPILER. infix reads the glued lane live off `operators`
; through (grip 0 ..); this checks that read against the factor pass itself, which is the
; one thing the door cannot tell it. for each (nom . sigil): the glued sigil must factor to
; exactly that nom.
(assert
 (: (row? e) (: nm <e s <>e (= (opfix (i-rd (s + "zz"))) (list nm 'zz)))
    (all row? imono)))

; --- real source. these six use between them: infix at every band, glued runs, `,`,
; grip, quoted data, define-sugar, nested lambdas and the pure-lisp subset.
(assert
 (: srcs '("lib/lint.l" "love/prel.l" "love/p1.l" "test/spec.l" "test/precedence.l" "test/valence.l")
    (rdall s a) (: r (sound s)
                   (? (two? r) (rdall >r (link <r a)) (id? r 'torn) 'torn (rev a)))
    (forms p) (: q (open p "r") s (? q (slurp q) ())
                 (? (string? s) (: ds (rdall s ()) (? (id? ds 'torn) () ds)) ()))
    ; ⚠ THE LAW'S DOMAIN. opfix deliberately emits FRESH NOMS -- a pattern lambda's binder
    ; (prel's op-depat) and a comparison chain's shared operand (op-chn) -- and a fresh nom has
    ; NO SPELLING: it prints as (mint N), which reads back as a call. a form whose FACTORED image
    ; carries one is a form no text can say, so the round trip cannot hold for it by construction,
    ; here or anywhere. skip exactly those; every other form is held to the law.
    (minty? x) (? (two? x) (|| (minty? <x) (minty? >x)) (mint? x))
    (sayable? x) (! (minty? (opfix x)))
    ; ⚠ the (two? ds) guard is on the READ, not on the filtered list: a file every form of which
    ; is unsayable would otherwise read as an unreadable file. `all` over none is vacuously true.
    (file? p) (: ds (forms p) (&& (two? ds) (all i-both? (filter sayable? ds))))
    (all file? srcs)))

(: _ (test_fin ()) _ (puts "infix: ok\n") ())
