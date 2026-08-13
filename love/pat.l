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
; and it is INFIX at band 10, n-ary like `?`: (x @ p b .. else) is the same form,
; scrutinee on the left, where a compound one spends no parens -- (vof en <t @ ..)
; not (@ (vof en <t) ..). love/prel.l's `operators` row; this file sees neither.
;
; A PATTERN ALSO GOES IN A \ PARAM, and that half is opfix's, not this file's: a param
; that is not a nom becomes a fresh one and the body takes it apart, so (\ [a b] x) IS
; (\ v (@ v [a b] x)) and (map (\ [a b] (a * b)) ..) reads the way it should. A
; define-sugar binder is a \ too, at every level. love/prel.l's op-depat / op-dpb.
;
; A PATTERN IS ITS CONSTRUCTOR, READ BACKWARDS: the same text that BUILDS a value
; MATCHES it, so nothing has to be learned twice and a bracket is as closed in a
; pattern as it is in a value. it is also ordinary surface, ALREADY FACTORED --
; opfix runs before macros, so the walk below sees core forms and never re-parses.
; a nom binds, `_` swallows, any other atom matches itself, and a form's HEAD is
; its destructuring tag:
;   (a b c)                a FORM: the shape of a chain, lax on the tail -- so it fits
;                          at three OR LONGER, and ([a b c] + _) is what it says. this
;                          is the UNTAGGED case, the one a head that names no tag falls
;                          to, and it is the common one: ('lam x b) fits (lam x b ..).
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
;   (? g pat)              a GUARD: pat must fit AND g must hold
;   (? g)                  the guard alone -- fits anything g holds for
;   (p1 || p2)             ALTERNATIVES: either shape fits. `|` is the same tag
;   (p1 && p2)             AND: both fit. `&` too -- the dual, and the way to name
;                          the whole while taking it apart: (v && ['lam x b])
;   ()                     the form at ZERO length: asks nothing, so it fits anything
;   []                     the LIST at zero length: the empty chain, exactly
; ⚠ a map key is read for PRESENCE, not truth: a pinned () matches, an absent
; key does not.
;
; PARENS ARE THE FORM, BRACKETS ARE THE LIST, and the pair is the whole of it:
; [a b c] fits a chain of EXACTLY three, (a b c) one of three or longer. Both read
; as their constructor -- for the paren it is the constructor of the domain pat is
; mostly used on, SYNTAX: uu.l matches terms, a term IS a form, and ('lam x b)
; against (lam x b) is that form written down. lax on the tail because a term wears
; annotations there, which is why every one of those arms used to end `+ _`.
; `+` stays for the tail you want to NAME ((['a b] + t)); parens only drop the `_`.
; this RETIRES the unknown-tag scare, and that is the better half of the change: a
; head naming no tag used to be a rejected program, and we do not reject programs --
; the language is defined so every form can be READ ((f) is f, an empty form is its
; head, an undefined op answers the unit). A tag was the one place a pattern refused
; to mean anything, and now it means the plain thing it looks like.
;
; THE GUARD IS THE ONE PATTERN THAT IS NOT A CONSTRUCTOR, and it wears `?` because
; a side condition is what `?` already means everywhere else -- test then shape,
; where `?` elsewhere reads test then value. `&&` and `||` stay unspent, so an
; and-pattern and an or-pattern can have them.
; g is an ORDINARY EXPRESSION, not a pattern: it is read for truth, it binds
; nothing, and it sees every nom THE WHOLE ARM binds -- `(? (a < b) [a b])` reads
; the pair it is testing. That is because guards do not run where they are written:
; like the non-linear `=` tests, they ride at the END of the conjunction, in source
; order, once the structural tests have earned every accessor. So a guard costs
; nothing on an arm whose shape already failed, and a positional one
; (`[a (? (a > 0) b)]`) is a placement for the READER -- it does not short-circuit
; any earlier. It is the body's own binder wrapped around g, so a nom the arm does
; not bind still resolves outward, in the scope the `@` was written in.
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
   wnil [() () ()]
   (wts ts) [ts () ()]
   (wgd g) [() () [g]]
   (wcat x y) [(cat <x <y) (cat <>x <>y) (cat <>>x <>>y)]

   ; (pw p v) -- one walk, three answers: the TESTS over the access form v, the
   ; (nom access) pairs its body binds, and the GUARDS, which are neither.
   (pw p v)
     (? (nom? p) (? (= p '_) wnil [() [[p v]] ()])
        (id? p ()) wnil                                      ; () is the form at ZERO: it asks nothing
        (! (link? p)) (wts [['= p v]])
        (: h <p as >p
           (? (= h '?) (? (! (two? as)) (wrest p '_ v)        ; a GUARD: (? g1 .. gn pat), or (? g)
                          (! (two? >as)) (wgd <as)           ; ..one operand IS the lone guard
                          (wq as v))                         ; ..else every one but the last guards
              ; AND-PATTERNS, the dual of ||: both shapes must fit, and the walks simply
              ; CONCATENATE -- one conjunction, and no winner to pick, because both branches
              ; read the SAME value and their access forms are already rooted there.
              ; ⚠ so a nom in both is the ORDINARY non-linear nom, and dedup makes the second
              ; sighting an equality: ([a _] && [_ a]) fits a pair of equals, for free.
              ; the everyday one is naming the whole while taking it apart: (v && ['lam x b]).
              ((h = '&&) || (h = '&)) (foldl wcat wnil (map (\ q (pw q v)) as))
              ; ALTERNATIVES, anywhere: ONE test, the branches ||ed. each branch owns its own
              ; equalities and its own guards, so a disjunct is that branch entire.
              ; ⚠ a nom lives at a DIFFERENT PLACE in each branch, so it cannot be one access
              ; form -- it is read from whichever branch matched, and () where none binds it.
              ; ⚠ that conditional re-asks the branch tests. A branch that binds nothing adds
              ; no nom and so asks nothing again, which is why the literal alternative -- the
              ; common one -- stays exactly one test with no second look.
              ((h = '||) || (h = '|))
              (: ws (map (\ q (pw q v)) as)
                 ds (map (\ w (dedup <>w ())) ws)
                 ts (map (\ e (: w <e d <>e
                                  (pand (cat (cat <w <d) (map (\ g (bind <>d g)) <>>w)))))
                         (zip ws ds))
                 bss (map (\ d <>d) ds)
                 uni (foldl (\ u bs (adds u (map cap bs))) () bss)
                 [[('|| >< ts)] (map (\ n [n (pick n bss ts)]) uni) ()])
              (= h '\) (? (two? >as) (wrest p '_ v) (wts (weq <as v)))   ; (\ x b) is a LAMBDA, a form
              (= h 'list) (wlist as v)
              (h = 'link || h = '><) (? (= 2 (tally as))    ; ONE tag: >< IS link, prefix or infix
                                        (wcat (wts [['two? v]])
                                              (wcat (pw <as ['cap v]) (pw <>as ['cup v])))
                                        (wrest p '_ v))
              (= h '+) (? (! (= 2 (tally as))) (wrest p '_ v)   ; a SPLIT: the left run, then the rest
                          (: l <as t <>as
                             (? (? (link? l) (= <l 'list) ()) (wrest >l t v)
                                ; ⚠ `+` is LEFT-handed, so ([a b] + [c] + t) arrives as a + whose
                                ; LEFT is a +. cat is associative and so is the split: re-associate
                                ; right and the ordinary rule reads it. this is the multi-split.
                                (? (link? l) (= <l '+) ()) (pw ['+ <>l ['+ <>>l t]] v)
                                (wrest p '_ v))))
              (= h 'hash) (wcat (wts [['tablet? v]]) (wkv as v))
              (= h 'tuple) (wcat (wts [['tray? v] ['= (tally as) ['tally v]]])
                                 (widx as v 0))
              (wrest p '_ v))))                            ; UNTAGGED: the form's own shape, lax
   (wq as v) (? (two? >as) (wcat (wgd <as) (wq >as v)) (pw <as v))   ; guards, then the shape
   ; a nom's access across an alternative. ⚠ THE FIRST BRANCH THAT MATCHES WINS, and it
   ; supplies EVERY nom -- one it does not bind reads (), it does not fall through to a
   ; later branch. Otherwise one body could wear bindings from two different shapes at
   ; once, and the arm would answer for a value no single branch described.
   (pick n bss ts) (? (! (two? bss)) (lit ())
                      (: e (assoc n <bss)
                         ['? <ts (? (atom? e) (lit ()) <>e) (pick n >bss >ts)]))
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
           (wcat (? (atom? e) [() [b] ()] (wts [['= <>e <>b]]))
                 (dedup >bs (? (atom? e) (link b seen) seen)))))

   (pand ts) (? (! (two? ts)) 1  (! (two? >ts)) <ts  ('&& >< ts))

   ; ⚠ THE WRAPPER IS ev'S TO REMOVE, not ours. A nom read once could have its accessor
   ; inlined here, and that is what love/ev.l's read-once refinement already does to the
   ; whole tree: a lone read of an effectful argument substitutes and the call is gone. A
   ; second copy in this macro measured 3.5 M on a 32.5 G corpus -- noise -- so the one
   ; mechanism lives in the compiler, where every caller gets it and not just `@`.
   (bind bs body) (? (two? bs) (link (link '\ (cat (map cap bs) [body])) (map caup bs)) body)

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
   ; --- ALTERNATIVES, and why they split two ways ------------------------------
   ; (p1 || p2) fits when either does -- `|` too, the way `><` and `link` are one tag.
   ; ⚠ A NOM'S ACCESS FORM IS WHERE IT SAT, and it sits somewhere else in every branch,
   ; so one access cannot serve two shapes. Two lanes answer that, and which one runs is
   ; decided by WHERE the alternative is, both obeying first-branch-wins:
   ;   at the top of an arm -> ONE CLAUSE PER BRANCH, sharing the body. A clause is a
   ;     thing there can be more than one of, so each branch keeps its own plain accesses
   ;     and nothing is asked twice. The body is then written once per branch -- the same
   ;     trade `tree` makes for the else, and the reason an alternative is not the cheaper
   ;     spelling of two arms: it is the spelling that says the bodies are THE SAME one.
   ;   nested          -> ONE clause, the branches `||`ed, each nom read from the winner
   ;     (pw above). Costs a re-ask per BOUND nom, and a literal alternative binds none --
   ;     so the common one is exactly one test, at any depth.
   ; ⚠ THE BODY BINDS THE UNION, and a nom this branch did not bind is THE UNIT. That is
   ; the whole of the different-noms question: binding it to () is what stops the body
   ; reaching whatever wears that name OUTSIDE, which is the one way this could answer
   ; something wrong. absence reads as nothing here exactly as it does everywhere else.
   ; the arm, normalised: its BRANCHES, each with the guards standing over it. a guard
   ; wrapping an alternative belongs to every branch, so (? g (p1 || p2)) is two branches
   ; guarded alike -- the guard is written once and asked once per branch.
   (arms p) (? (! (link? p)) [[p ()]]
               (: h <p as >p
                  (? ((h = '||) || (h = '|)) (catmap arms as)
                     ((h = '?) && (= 2 (tally as)))
                       (map (\ b [<b (link <as <>b)]) (arms <>as))
                     [[p ()]])))
   (wgds w gs) (foldl (\ x g (wcat x (wgd g))) w gs)
   (adds u ns) (cat u (filter (\ n (! (member? n u))) ns))    ; u, then what it did not have
   _ (:: '@ (\ a
       (: v (nom 0)
          ; the clause a walk answers: its tests, then its non-linear equalities, then its
          ; guards -- and a body bound over `uni`, whatever this branch did not bind reading ().
          (clofu w uni b)
            (: d (dedup <>w ())
               bs (cat <>d (map (\ n [n (lit ())]) (filter (\ n (! (assoc n <>d))) uni)))
               gs (map (\ g (bind bs g)) <>>w)
               [(cat (cat <w <d) gs) (bind bs b)])
          (clof w b) (clofu w () b)
          (arm p b)                                    ; the clauses ONE arm answers for
            (: qs (arms p)
               (? (! (two? >qs)) [(clof (pw p v) b)]   ; one branch: pw takes any guard itself
                  (: ws (map (\ q (wgds (pw <q v) <>q)) qs)
                     (? (all (\ w (! (two? <>w))) ws) [(clof (pw p v) b)]   ; none binds: pw's one test
                        (: uni (foldl (\ u w (adds u (map cap <>(dedup <>w ())))) () ws)
                           (map (\ w (clofu w uni b)) ws))))))
          (cls r) (? (! (two? r)) [() ()]              ; no clause and no else: the zero point
                     (! (two? >r)) [() <r]             ; the odd tail IS the else
                     (: rest (cls (cuup r))
                        [(cat (arm <r <>r) <rest) <>rest]))
          cl (cls >a)
          [['\ v (tree <cl <>cl)] <a])))
   ())
