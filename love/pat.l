; pat -- @ : pattern matching, ONE MACRO and no special form. a POST-EGG LAYER,
; not part of the language: prel stays minimal and each frontend includes the
; layers it wants (the host, love0, wasm and the K_TEST kernel do, since love/uu.l
; is written in it; a shipped kernel takes it for the same reason). ⚠ its `use`
; must PRECEDE every reader's compile -- a macro is only in the chain once its
; layer is spliced, and a consumer folds its refs at its own compile.
; the module exports no names: the macro IS the surface, and it rides the splice
; in this layer's macro slot. laws in test/pat.l.
;
; (@ x pat body .. else) reads like `?` with a SHAPE where the test goes: the
; first pattern that fits x runs its body with the pattern's noms bound, an odd
; trailing operand is the else, and no match with no else is the zero point.
; the reader hands `@` over SPACED (test/reader.l); glued it is still the tray
; sigil, so @(1 2) never changed hands and `(@ @(a b) ..)` says both in a breath.
;
; A PATTERN IS ITS CONSTRUCTOR, READ BACKWARDS: the same text that BUILDS a value
; MATCHES it, so nothing has to be learned twice and a bracket is as closed in a
; pattern as it is in a value. it is also ordinary surface, ALREADY FACTORED --
; opfix runs before macros, so the walk below sees core forms and never re-parses.
; a nom binds, `_` swallows, any other atom matches itself, and a form's HEAD is
; its destructuring tag:
;   [a b]  (list a b)      a chain of exactly two        <- a bracket IS its wrap
;   (a >< t)  (link a t)   the cons: head a, tail t. it CHAINS, which is how a
;                          pattern says "and more" -- ('lam >< x >< b >< _) fits
;                          (lam x b ..) at three or longer, ['lam x b] at three
;                          exactly. ⚠ [a b >< t] is (>< (list a b) t) -- the same
;                          datum as ([a b] >< t), so it reads the ONE way both
;                          spellings can: a head that IS the two-chain (a b).
;   ([a b] + t)            a SPLIT: the leading run, then t over what is left.
;                          a pattern is its constructor, so + means here what it
;                          means there -- A + B is the value that A and B make.
;                          ⚠ the LEFT side must be a bracket: its length is what
;                          picks the split point, so nothing is searched for. and
;                          ⚠ it needs its own parens inside @'s operands, like any
;                          infix -- a bare + would capture the whole span each side.
;                          this is how a pattern says "at least": ([a b] + _).
;   'a  (\ a)              the literal a, never a binding
;   {'f x}  (hash 'f x)    a map carrying key 'f -- the KEY is an expression, the
;                          value a pattern, and the match is by SUBSET (the one
;                          place a pattern is looser than its constructor)
;   @(a b)  (tuple a b)    a tray of exactly two
; ⚠ a map key is read for PRESENCE, not truth: a pinned () matches, an absent
; key does not. an unknown tag is a scare AT EXPAND TIME -- a pattern is code, so
; a typo in one is a compile error, never a clause that quietly never fires.
;
; A REPEATED NOM MATCHES NON-LINEARLY: the second sighting is an `=` against the
; first's access form, not a second binding, so [a a] fits a pair of equals and
; nothing else. the equality tests ride at the END of the conjunction, where the
; structural ones have already earned every accessor they read.
;
; the expansion is plain core: ((\ v (? test bound-body .. else)) x), each bound
; body a lambda over the pattern's noms applied to the accessor forms. the
; ARGUMENTS are read in the OUTER scope, so a pattern nom may wear the name of an
; accessor its own test needs -- [cap cup] binds both and still matches. cprop
; beta-reduces the wrapper away when the scrutinee is already a value, and keeps
; it when it is a call -- so the binding is free exactly when it is not needed.
(: gone (mint 0)                 ; absence out of band: no stored value poses as a fresh mint
   (lit d) ['\ d]               ; d, as the code that answers d
   ; a literal is compared with `=` -- EXCEPT a NOM, which is interned, so id? is
   ; the same answer off one pointer where = walks the generic table. ⚠ noms and
   ; nothing else: = on a charm is numeric ACROSS the band (5 matches the gem 5.0),
   ; and id? would quietly narrow that to one representation.
   (weq d v) (? (nom? d) [['id? (lit d) v]] [['= (lit d) v]])
   wnil [() ()]
   (wts ts) [ts ()]
   (wcat x y) [(cat <x <y) (cat <>x <>y)]

   ; (pw p v) -- one walk, two answers: the TESTS over the access form v, and the
   ; (nom access) pairs its body binds.
   (pw p v)
     (? (nom? p) (? (= p '_) wnil [() [[p v]]])
        (! (link? p)) (wts [['= p v]])
        (: h <p as >p
           (? (= h '\) (? (two? >as) (scare '@ p) (wts (weq <as v)))
              (= h 'list) (wlist as v)
              (h = 'link || h = '><) (? (= 2 (tally as))    ; ONE tag: >< IS link, prefix or infix
                                        (wcat (wts [['two? v]])
                                              (wcat (pw <as ['cap v]) (pw <>as ['cup v])))
                                        (scare '@ p))
              (= h '+) (? (= 2 (tally as))                ; a SPLIT: the left run, then the rest
                          (: l <as
                             (? (? (link? l) (= <l 'list) ()) (wrest >l <>as v) (scare '@ p)))
                          (scare '@ p))
              (= h 'hash) (wcat (wts [['tablet? v]]) (wkv as v))
              (= h 'tuple) (wcat (wts [['tray? v] ['= (tally as) ['tally v]]])
                                 (widx as v 0))
              (scare '@ p))))
   ; ⚠ a pattern longer than its value must never reach a `cap`, so the WHOLE run is
   ; claimed before any of it is read: `two?` on the head, then ONE `long?` for the rest
   ; of the depth. asking `two?` per step instead spends a cup, a load and a dispatch at
   ; each -- a quarter of what matching costs over hand-written accessors, measured.
   ; ⚠ the head's test stays FIRST and separate, and is NOT folded into the long?: it is
   ; what the decision tree hoists, and the tag compare has to be reachable before the
   ; arity walk, or 20 arms of a 21-arm dispatch walk to learn they are the wrong tag.
   ; one walk, TWO ENDS: a bracket ends on (id? () rest) -- exact length -- a split on
   ; the right pattern's own walk, wherever the run stopped.
   (warity k v) (? (k = 1) [['two? v]] [['long? k v]])   ; at one, two? is the same answer and fuses
   (wrun ps v end)
     (? (! (two? ps)) (end v)
        (: n (tally ps)
           (wcat (wcat (wts [['two? v]]) (pw <ps ['cap v]))
                 (? (n = 1) (end ['cup v])
                    (wcat (wts (warity (n - 1) ['cup v])) (wskip >ps ['cup v] end))))))
   (wskip ps v end) (? (two? ps) (wcat (pw <ps ['cap v]) (wskip >ps ['cup v] end))
                       (end v))
   (wlist as v) (wrun as v (\ r (wts [['id? () r]])))
   (wrest ps t v) (wrun ps v (\ r (pw t r)))
   (wkv as v) (? (two? >as)
                 (: g ['peep v <as (lit gone)]
                    (wcat (wcat (wts [['! ['id? (lit gone) g]]]) (pw <>as g))
                          (wkv (cuup as) v)))
                 wnil)
   (widx as v i) (? (two? as) (wcat (pw <as ['peep v i ()]) (widx >as v (i + 1))) wnil)

   ; the non-linear pass: a nom already sighted becomes an `=` against the access
   ; form that first bound it, and drops out of the binder list.
   (dedup bs seen)
     (? (! (two? bs)) wnil
        (: b <bs e (assoc <b seen)
           (wcat (? (atom? e) [() [b]] (wts [['= <>e <>b]]))
                 (dedup >bs (? (atom? e) (link b seen) seen)))))

   (pand ts) (? (! (two? ts)) 1  (! (two? >ts)) <ts  ('&& >< ts))

   ; --- the binder, and the wrapper it does not always need ------------------------
   ; a nom used ONCE has its accessor INLINED at the use site. cprop cannot do it for us:
   ; the argument is a call, so it keeps the wrapper rather than risk duplicating work --
   ; right in general, and wrong for the single use, which is most of them. that wrapper
   ; is a closure built and applied on every match that succeeds.
   ; ⚠ A QUOTE IS NOT A REFERENCE. (\ d) at ONE operand is 'd, so neither the count nor
   ; the substitution may descend into it -- a body reading (list 'np1 v) must not have
   ; its quoted np1 rewritten. A `\` at two or more IS a binder, and a body carrying one
   ; (or a `:`) keeps its wrapper whole: substituting under it could capture.
   ; ⚠ AND NOT IF A NOM WEARS AN ACCESSOR'S NAME. [cap cup] is a legal pattern; inlining
   ; (cap (cup v)) into a body where `cap` is itself a bound nom would rewrite the
   ; accessor we just built. The wrapper is correct for those, so they keep it.
   (quo? x) (? (link? x) (? (id? <x '\) (! (two? (cuup x))) ()) ())
   (opq? x) (? (link? x) (|| (id? <x ':) (? (id? <x '\) (two? (cuup x)) ())) ())
   (uses n x) (? (quo? x) 0 (link? x) (foldl (\ k e (k + (uses n e))) 0 x) (? (id? n x) 1 0))
   (bnds? x) (? (quo? x) () (opq? x) 1
                (link? x) (foldl (\ k e (? k 1 (bnds? e))) () x) ())
   (subst n r x) (? (quo? x) x (link? x) (map (\ e (subst n r e)) x) (? (id? n x) r x))
   (wrap bs body) (? (two? bs) (link (link '\ (cat (map cap bs) [body])) (map caup bs)) body)
   (bind bs body)
     (: shy (|| (bnds? body) (any (\ b (member? <b '(cap cup peep))) bs))
        (go bs body keep)
         (? (two? bs)
            (: b <bs
               (? (? shy () (= 1 (uses <b body)))
                  (go >bs (subst <b <>b body) keep)
                  (go >bs body (cat keep [b]))))
            (wrap keep body))
        (go bs body ()))

   ; --- THE DECISION TREE -------------------------------------------------------
   ; a clause is [tests body]. compiled FLAT, every clause re-runs the tests it
   ; shares with its neighbours: a 21-arm tag dispatch asks `two?` 21 times where
   ; the hand-written `?` asks once, and that repetition -- not the tag compare, not
   ; the accessor -- is the whole gap (measured 1.91x flat, 1.01x as a tree).
   ; so: FACTOR THE COMMON PREFIX. every clause of a dispatch opens with the same
   ; (two? v), so it is asked ONCE outside the chain and dropped from each clause.
   ; ⚠ the else is then reached two ways and appears TWICE in the source, which is
   ; why this only fires for a SMALL else -- a big one would double the code.
   ; ⚠ and it is one level, never recursive: the duplication stays at exactly 2.
   (same? a b) (? (link? a) (? (link? b) (? (same? <a <b) (same? >a >b) ()) ())
                  (link? b) ()
                  (id? a b))
   ; --- factoring, and its ONE safe shape --------------------------------------
   ; a full decision tree groups each level's clauses by their next test and recurses
   ; both ways -- into the group, and PAST it into the fallthrough. correct, and it
   ; writes the fallthrough TWICE PER LEVEL: with 21 distinct tags that is 2^21 nodes
   ; and the compile never finishes (it did not loop -- it exploded). a real compiler
   ; escapes by making the fallthrough a JOIN POINT; the nearest thing here is a thunk
   ; bound per level, which costs a closure per level per call -- more than the test
   ; it saves. so factoring pays only where the fallthrough does NOT branch:
   ; THE PREFIX EVERY CLAUSE SHARES. failing it fails the whole match, so its
   ; fallthrough IS the else -- one level, one extra copy, no tree to explode.
   ; that is the whole win here anyway: uu's clauses share (two? v) and then differ
   ; by tag, so the tree has exactly one interior node to find.
   (cpx cs)                                         ; the test-prefix common to EVERY clause
     (? (! (two? cs)) ()
        (! (two? <(<cs))) ()                        ; a clause with no tests left: stop
        (: t <<(<cs)
           (? (all (\ c (? (two? <c) (same? <<c t) ())) cs)
              (link t (cpx (map (\ c [>(<c) <>c]) cs)))
              ())))
   (nodes x) (? (link? x) (foldl (\ n e (n + (nodes e))) 1 x) 1)
   (flat cs els) (link '? ((catmap (\ c [(pand <c) <>c]) cs) + [els]))
   ; ⚠ TWO CLAUSES OR MORE, or there is nothing to discriminate: at one clause cpx eats
   ; ALL of its tests, so factoring would read (? p (? 1 body els) els) -- a vacuous test
   ; and the else written twice, for nothing. uu's hot sites are exactly that shape.
   (tree cs els)
     (: p (cpx cs)
        (? (&& (two? >cs) (two? p) (< (nodes els) 40))   ; >=2 clauses, a prefix, a small else
           ['? (pand p) (flat (map (\ c [(drop (tally p) <c) <>c]) cs) els) els]
           (flat cs els)))
   _ (:: '@ (\ a
       (: v (nom 0)
          (cls r) (? (! (two? r)) [() ()]              ; no clause and no else: the zero point
                     (! (two? >r)) [() <r]             ; the odd tail IS the else
                     (: w (pw <r v) d (dedup <>w ())
                        rest (cls (cuup r))
                        [(link [(cat <w <d) (bind <>d <>r)] <rest) <>rest]))
          cl (cls >a)
          [['\ v (tree <cl <>cl)] <a])))
   ())
