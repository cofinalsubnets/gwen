; lib/unfix.l -- a RIGHT INVERSE to opfix: a core form -> the max-infix, min-paren surface.
; the law is (opfix (sound (unfix-spell x))) = (opfix x), and it holds for every x the READER
; can produce -- which is the whole domain that matters, since a tool reads files. love has no
; dotted-pair syntax, so a read form is always proper and every subterm is writable.
;
; a LEFT inverse cannot exist: opfix is many-to-one, and `a + b`, `(+ a b)` and `(a + b)` all
; factor alike. this is the useful direction, and the one a printer wants.
;
; ⚠ this is NOT a formatter and must not become one. `sound` does not keep comments, so printing
; a file from its datums would strip every one -- which is why libra's fmt is a REINDENTER and its
; paren-minifier (lib/lint.l's lint-drops) works by DELETION over the source bytes. this answers
; the surface for a FORM, and its callers are code generators, one-shot conversions a hand has
; asked for, and the gate that holds opfix invertible.
;
; the fallback is `show`: prefix is a fixed point of opfix, so the fully-parenthesized spelling
; is ALWAYS a right inverse. what is below is the maximizing one, and it is CHECKED -- every
; datum is read back and factored before it is handed out, so a proposal that would re-group
; differently costs parens, never correctness.
(: ; the glued-sigil map, keyed by the nom a monadics row names (love/prel.l, and `.` from
   ; love/post.l). ⚠ a second source of truth: `monadics` is mopped at birth and has no read
   ; door, so this is copied. test/host/unfix.l holds it honest BEHAVIOURALLY -- it asks the
   ; live compiler what each sigil factors to rather than reading the table.
   umono '((cap "<") (cup ">") (net "+") (prod "*") (abs "|") (negate "-")
           (reciprocal "/") (fraction "%") (bit "?") (saturate "$") (nil? "!") (. "."))

   ; --- the punct test. op-puncp is book-private; these are its four ranges, in its order
   ; (commonest first -- most heads bail on the first byte).
   (upb c) (? (&& (97 <= c) (c <= 122)) 0  (&& (48 <= c) (c <= 57)) 0
              (&& (65 <= c) (c <= 90)) 0  (c = 95) 0  1)
   (upunc? x) (? (nom? x)
                 (: s (string x) n (tally s)
                    (? (0 < n) ((: (go i) (? (i < n) (? (upb (s i)) (go (i + 1)) 0) 1)) 0) 0))
                 0)

   ; --- the grip door. `dyadics` is mopped too, so a row is read THROUGH fixity: it answers the
   ; row it replaced, so pinning that straight back is a non-destructive read. this one needs no
   ; copy -- the live table is the answer, and a file's own (fixity ..) is already in it.
   (urow nm) (: r (fixity nm ()) _ (fixity nm r) r)
   (ugrip? g) (&& (charm? g) (0 < g))
   (ugh v) (? (ugrip? v) (>< v 0)
              (! (two? v)) 0
              (! (ugrip? (cap v))) 0
              (charm? (cup v)) v
              (&& (two? (cup v)) (charm? (caup v))) (>< (cap v) (caup v))
              (atom? (cup v)) (>< (cap v) 0))
   ; -> (grip . hand). an absent or unreadable row is the house default: 95, right-handed.
   (uent nm) (: v (urow nm)
                t (? (? (two? v) (nom? (cap v)) 0) (ugh (cup v)) (ugh v))
                (? (two? t) t (>< 95 0)))
   ; op-steal's predicate, verbatim: an incoming (g,h) takes the pending frame's last operand
   ; iff its grip beats that frame's -- and at equal grip the incomer's hand decides.
   (usteal g h pg) (? h (pg < g) (pg <= g))

   ; --- shapes. a binder is neither infix nor a span; glue outranks a span (max glue); infix
   ; wants EXACTLY three, since (+ a b c) is an application and has to stay prefix.
   (u3? x) (&& (two? (cup x)) (&& (two? (cuup x)) (! (two? (cuuup x)))))
   (u2? x) (&& (two? (cup x)) (! (two? (cuup x))))
   (ubind? x) (? (id? (cap x) ':) 1 (id? (cap x) '\) 1 0)
   (uquote? x) (&& (id? (cap x) '\) (u2? x))
   (uglue? x) (&& (u2? x) (? (nom? (cap x)) (two? (assoc (cap x) umono)) 0))
   (uinfix? x) (&& (u3? x) (upunc? (cap x)))
   ; a datum may take a glued sigil run only where the run cannot swallow it: a punct nom would
   ; join the run, and a negative number would eat the sign.
   (uglued? d) (? (two? d) 1  (nom? d) (! (upunc? d))  (string? d) 1  (0 <= d))

   ; --- the renderer --------------------------------------------------------------------
   (ujoin l) (: (go l) (? (two? (cup l)) ((cap l) + " " + (go (cup l))) (cap l))
                (? (two? l) (go l) ""))
   (uparen x) ("(" + (ufree x) + ")")
   (udat d) (? (two? d) (uparen d) (show d))          ; one glued datum: <>(a b) reads
   (uglue x) (: s (caup (assoc (cap x) umono)) d (caup x)
                (? (&& (uglue? d) (uglued? d)) (s + (uglue d))
                   (uglued? d) (s + (udat d))
                   (s + (uparen d))))                 ; the guard failed: space it back out
   ; ⚠ a uelem position is ONE DATUM, and that is exactly what makes a LOCAL check sound:
   ; propose, read it back, factor it, compare. a subterm that cannot be maximized then costs
   ; its own parens and nothing else -- checking whole forms instead sends a 500-line `:` back
   ; to `show` for one hard subterm, which is most of what a real file is.
   (uelem0 x) (? (uquote? x) (show x)
                 (&& (uglue? x) (uglued? (caup x))) (uglue x)
                 (uparen x))
   (uelem x) (? (! (two? x)) (show x)
                (: c (uelem0 x) r (sound c)
                   (? (? (two? r) (= (opfix (cap r)) x) 0) c (show x))))
   ; an element of an application span. juxtaposition is FLAT and binds tighter than any infix,
   ; so only an atom or a glued run rides bare -- every list takes its own parens.
   (uspan l) (ujoin (map uelem l))
   ; an operand of an operator at (g,h). an atom, a glued run and a SPAN all ride bare (op-steal
   ; takes a whole leading span, op-del extends a trailing one); an infix operand rides bare only
   ; where the read re-groups it the same way.
   (uopnd x g h left)
    (? (! (two? x)) (show x)
       (uquote? x) (show x)
       (ubind? x) (uparen x)
       (&& (uglue? x) (uglued? (caup x))) (uglue x)
       (uinfix? x)
        (: e (uent (cap x)) g2 (cap e) h2 (cup e)
           (? (? left (! (usteal g h g2)) (usteal g2 h2 g)) (ufree x) (uparen x)))
       (ufree x))
   ; binder positions are all ONE DATUM: opfix pairs a `:` by position and takes a `\` body as
   ; the last operand, so a bare infix there would break the pairing.
   (ucolonl fs) (? (! (two? fs)) ()
                   (! (two? (cup fs))) (>< (uelem (cap fs)) ())            ; the lone body
                   (>< (show (cap fs)) (>< (uelem (caup fs)) (ucolonl (cuup fs)))))
   (ulaml fs) (? (! (two? (cup fs))) (>< (uelem (cap fs)) ())              ; the body
                 (>< (show (cap fs)) (ulaml (cup fs))))
   (ubindf x) (? (id? (cap x) ':) (ujoin (>< ":" (ucolonl (cup x))))
                 (ujoin (>< "\\" (ulaml (cup x)))))
   (uinfixf x) (: e (uent (cap x)) g (cap e) h (cup e) t (cup x)
                  ((uopnd (cap t) g h 1) + " " + (show (cap x)) + " " + (uopnd (caup t) g h 0)))
   (ufree x)
    (? (! (two? x)) (show x)
       (uquote? x) (show x)                           ; show already spells quote as 'x
       (ubind? x) (ubindf x)
       (&& (uglue? x) (uglued? (caup x))) (uglue x)
       (uinfix? x) (uinfixf x)
       (uspan x))
   ; ⚠ a glued run may not be the OUTERMOST thing in a form: op-core's splice-up is gated on
   ; has-top-op?, which a (mono ..) node fails, so `(<x)` comes back as the 1-list ((love-cap x))
   ; and not (love-cap x). glue is free everywhere else; here the span form is the one.
   (ufree0 x)
    (? (! (two? x)) (show x)
       (uquote? x) (show x)
       (ubind? x) (ubindf x)
       (uinfix? x) (uinfixf x)
       (uspan x))
   ; the top of a form takes exactly one pair of parens; op-core's splice-up is what brings
   ; (= a b) back from (a = b). an atom stands alone, and a quote form is already its own datum:
   ; `show` spells it 'a, where ('a) would be a 1-list.
   (utop x) (? (! (two? x)) (show x)
               (uquote? x) (show x)
               ("(" + (ufree0 x) + ")"))

   ; --- the door. normalize, propose, VERIFY, and fall back to prefix on any mismatch. the
   ; whole-form check is a backstop over uelem's local ones; it costs one read per form.
   (unfix-spell x)
    (: n (opfix x) c (utop n) r (sound c)
       (? (? (two? r) (= (opfix (cap r)) n) 0) c (show n)))
   ; did the maximal spelling survive, or did the backstop fire? (for the gate and the metric)
   (unfix-max? x)
    (: n (opfix x) c (utop n) r (sound c)
       (? (? (two? r) (= (opfix (cap r)) n) 0) 1 0)))
