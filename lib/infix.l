; lib/infix.l -- a RIGHT INVERSE to opfix: a core form -> the max-infix, min-paren surface.
; the law is (opfix (sound (infix-spell x))) = (opfix x), and it holds for every x the READER
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
(: ; the glued-sigil map. the SIGILS are listed here; the nom each one names is read LIVE out
   ; of the operators table's arity-0 lane, through the same clear-and-restore door the grips
   ; use. so what a sigil MEANS has one source of truth (love/prel.l, and `.` from love/post.l)
   ; and this list only says which ones exist -- a sigil the table has dropped drops out here too.
   ; ⚠ `.` sits LAST: between two elements a dot is the pair marker, before the closer it is
   ; the plain symbol -- which is the only reason this is a list and not a run of them.
   imonos '(< > + * | - / % ? $ ! .)
   (irow0 s) (: r (grip 0 s ()) _ (grip 0 s r) r)
   imono (filter (\ e (nom? <e)) (map (\ s (list (irow0 s) (string s))) imonos))

   ; --- the punct test. op-puncp is book-private; these are its four ranges, in its order
   ; (commonest first -- most heads bail on the first byte).
   (ipb c) (? (&& (97 <= c) (c <= 122)) 0  (&& (48 <= c) (c <= 57)) 0
              (&& (65 <= c) (c <= 90)) 0  (c = 95) 0  1)
   (ipunc? x) (? (nom? x)
                 (: s (string x) n (tally s)
                    (? (0 < n) ((: (go i) (? (i < n) (? (ipb (s i)) (go (i + 1)) 0) 1)) 0) 0))
                 0)

   ; --- the grip door. `operators` is mopped, so a row is read THROUGH grip: it answers the
   ; row it replaced, so pinning that straight back is a non-destructive read. no copy anywhere --
   ; the live table is the answer, and a file's own (grip ..) is already in it.
   ; ⚠ the SPACED lanes, tightest first, exactly as op-ent asks them.
   ; ⚠ IDENTITY, never truth: a left-handed band is a NEGATIVE charm and nets red, so a truth
   ; test here would send every left-handed row on to the next lane as though it were absent.
   (irow nm) (: r (grip 2 nm ()) _ (grip 2 nm r)
                (? (id? () r) (: q (grip -1 nm ()) _ (grip -1 nm q) q) r))
   ; a BAND is a SIGNED charm -- magnitude the level, sign the hand (negative is LEFT) -- and the
   ; row wears one of op-lane's three kinds: a charm, a nom (an ALIAS, one hop), or (nom band).
   (iband? b) (&& (charm? b) (0 != b))
   (itail v) (? (charm? >v) >v  (&& (two? >v) (charm? <>v)) <>v  0)
   ; -> (band . hand). an absent or unreadable row is the house default: 95, right-handed.
   (ient nm) (: v (irow nm)
                b (? (iband? v) v
                     (nom? v) (: r (irow v) (? (iband? r) r 0))
                     (? (two? v) (nom? <v) 0) (itail v)
                     0)
                (? (iband? b) (>< |b (? (b < 0) 1 0)) (>< 95 0)))
   ; op-steal's predicate, verbatim: an incoming (g,h) takes the pending frame's last operand
   ; iff its band beats that frame's -- and at equal band the incomer's hand decides.
   (isteal g h pg) (? h (pg < g) (pg <= g))

   ; --- shapes. a binder is neither infix nor a span; glue outranks a span (max glue); infix
   ; wants EXACTLY three, since (+ a b c) is an application and has to stay prefix.
   (i3? x) (&& (two? (cup x)) (&& (two? (cuup x)) (! (two? (cuuup x)))))
   (i2? x) (&& (two? (cup x)) (! (two? (cuup x))))
   (ibind? x) (? (id? (cap x) ':) 1 (id? (cap x) '\) 1 0)
   (iquote? x) (&& (id? (cap x) '\) (i2? x))
   (iglue? x) (&& (i2? x) (? (nom? (cap x)) (two? (assoc (cap x) imono)) 0))
   (iinfix? x) (&& (i3? x) (ipunc? (cap x)))
   ; a datum may take a glued sigil run only where the run cannot swallow it: a punct nom would
   ; join the run, and a negative number would eat the sign.
   (iglued? d) (? (two? d) 1  (nom? d) (! (ipunc? d))  (string? d) 1  (0 <= d))

   ; --- the renderer --------------------------------------------------------------------
   ; --- the law, and what it compares -----------------------------------------------
   ; what a spelling DENOTES: read it back and factor it. the check compares this on both
   ; sides -- mine and `show`'s -- rather than mine against the value itself, because a
   ; value need not have a readable twin: a coin reads back as the ctor call that rebuilds
   ; it, never as itself. equal-THROUGH-TEXT is exactly "these two spellings say the same
   ; thing", which is all a printer owes, and on fully readable data it is plain equality.
   ; ⚠ it leans on `show` being faithful -- love/post.l's pcoin quotes the payload for
   ; this reason. a printer that lied here would licence a spelling that lied with it.
   (iread c) (: r (sound c) (? (two? r) (link 1 (link (opfix (cap r)) ())) ()))
   (isame? c n) (: a (iread c) b (iread (show n))
                   (&& (&& (two? a) (two? b)) (= (caup a) (caup b))))
   ; a value that is not a chain may still SPELL as one: a coin shows as (q '(1 2)), the
   ; ctor applied to its payload. pasting that text whole would keep parens the span rule
   ; makes free, so an operand re-reads the spelling and renders it like any other form.
   ; ⚠ a closure is excluded -- its show is de Bruijn source, a display and not a rebuild.
   (iform x) (? (lit? x) ()
                (: r (sound (show x))
                   (? (two? r) (: d (cap r) (? (two? d) d ())) ())))

   (ijoin l) (: (go l) (? (two? (cup l)) ((cap l) + " " + (go (cup l))) (cap l))
                (? (two? l) (go l) ""))
   (iparen x) ("(" + (ifree x) + ")")
   (idat d) (? (two? d) (iparen d) (show d))          ; one glued datum: <>(a b) reads
   (iglue x) (: s (caup (assoc (cap x) imono)) d (caup x)
                (? (&& (iglue? d) (iglued? d)) (s + (iglue d))
                   (iglued? d) (s + (idat d))
                   (s + (iparen d))))                 ; the guard failed: space it back out
   ; ⚠ a ielem position is ONE DATUM, and that is exactly what makes a LOCAL check sound:
   ; propose, read it back, factor it, compare. a subterm that cannot be maximized then costs
   ; its own parens and nothing else -- checking whole forms instead sends a 500-line `:` back
   ; to `show` for one hard subterm, which is most of what a real file is.
   (ielem0 x) (? (iquote? x) (show x)
                 (&& (iglue? x) (iglued? (caup x))) (iglue x)
                 (iparen x))
   (ielem x) (? (! (two? x)) (show x)
                (: c (ielem0 x) (? (isame? c x) c (show x))))
   ; an element of an application span. juxtaposition is FLAT and binds tighter than any infix,
   ; so only an atom or a glued run rides bare -- every list takes its own parens.
   (ispan l) (ijoin (map ielem l))
   ; an operand of an operator at (g,h). an atom, a glued run and a SPAN all ride bare (op-steal
   ; takes a whole leading span, op-del extends a trailing one); an infix operand rides bare only
   ; where the read re-groups it the same way.
   (iopnd x g h left)
    (? (! (two? x)) (: f (iform x) (? (two? f) (iopnd f g h left) (show x)))
       (iquote? x) (show x)
       (ibind? x) (iparen x)
       (&& (iglue? x) (iglued? (caup x))) (iglue x)
       (iinfix? x)
        (: e (ient (cap x)) g2 (cap e) h2 (cup e)
           (? (? left (! (isteal g h g2)) (isteal g2 h2 g)) (ifree x) (iparen x)))
       (ifree x))
   ; binder positions are all ONE DATUM: opfix pairs a `:` by position and takes a `\` body as
   ; the last operand, so a bare infix there would break the pairing.
   (icolonl fs) (? (! (two? fs)) ()
                   (! (two? (cup fs))) (>< (ielem (cap fs)) ())            ; the lone body
                   (>< (show (cap fs)) (>< (ielem (caup fs)) (icolonl (cuup fs)))))
   (ilaml fs) (? (! (two? (cup fs))) (>< (ielem (cap fs)) ())              ; the body
                 (>< (show (cap fs)) (ilaml (cup fs))))
   (ibindf x) (? (id? (cap x) ':) (ijoin (>< ":" (icolonl (cup x))))
                 (ijoin (>< "\\" (ilaml (cup x)))))
   (iinfixf x) (: e (ient (cap x)) g (cap e) h (cup e) t (cup x)
                  ((iopnd (cap t) g h 1) + " " + (show (cap x)) + " " + (iopnd (caup t) g h 0)))
   (ifree x)
    (? (! (two? x)) (show x)
       (iquote? x) (show x)                           ; show already spells quote as 'x
       (ibind? x) (ibindf x)
       (&& (iglue? x) (iglued? (caup x))) (iglue x)
       (iinfix? x) (iinfixf x)
       (ispan x))
   ; ⚠ a glued run may not be the OUTERMOST thing in a form: op-core's splice-up is gated on
   ; has-top-op?, which a (mono ..) node fails, so `(<x)` comes back as the 1-list ((love-cap x))
   ; and not (love-cap x). glue is free everywhere else; here the span form is the one.
   (ifree0 x)
    (? (! (two? x)) (show x)
       (iquote? x) (show x)
       (ibind? x) (ibindf x)
       (iinfix? x) (iinfixf x)
       (ispan x))
   ; the top of a form takes exactly one pair of parens; op-core's splice-up is what brings
   ; (= a b) back from (a = b). an atom stands alone, and a quote form is already its own datum:
   ; `show` spells it 'a, where ('a) would be a 1-list.
   (itop x) (? (! (two? x)) (show x)
               (iquote? x) (show x)
               ("(" + (ifree0 x) + ")"))

   ; --- the door. normalize, propose, VERIFY, and fall back to prefix on any mismatch. the
   ; whole-form check is a backstop over ielem's local ones; it costs one read per form.
   (infix-spell x) (: n (opfix x) c (itop n) (? (isame? c n) c (show n)))
   ; did the maximal spelling survive, or did the backstop fire? (for the gate and the metric)
   (infix-max? x)  (: n (opfix x) c (itop n) (? (isame? c n) 1 0)))
