; kanren -- first-order unification + a lazy-colist search monad.
; a POST-EGG LAYER like love/q.l (see its head for why): prel stays the language, a library
; rides its own out/lib/kanren.h. Needs love/prel.l, and pat's @ (which rides the post text
; in every frontend that loads this). love/overlay.l's engine reads `unify`/`ufail?`/`var`
; from here, so this layer must be evaled BEFORE overlay.
; the UNIFIER is the compiler-grade core (a future relational rewrite layer sits
; on `unify`/`walk`); the SEARCH half (streams, `query`, `===`) is its userland
; face. `unify s u v` is PURE -- a substitution or `ufail` -- with NO occurs
; check: love's own values are non-well-founded (a chain is a mu-type, the book
; holds the book), so a self-referential binding is welcome -- it's a rational
; tree, and `walk_star` mu-marks a back-edge to reify it without looping. a goal
; is (\ st stream) over a STATE (substitution . constraints); est/=== unify, +
; interleaves (disjunction), * binds (conjunction) -- so a relation is a
; polynomial in unifications (see the ring).
; a REGISTERED MODULE with NO BRACKETS: the runtime supplies the layer -- a
; frontend lists this source in its ai_libs table and its boot says (use 'kanren),
; which loads, registers and splices in one motion. the splice serves every
; bare consumer (unify/ufail?/var/s_plus/s_star/===/=/=, and the goal macros'
; expansions); internals answer to (from 'kanren ..) -- subst, walk, query,
; the dict kit; (keys (from 'kanren)) introspects. love/overlay.l's engine
; reads subst through the registry, so kanren must be REGISTERED before overlay
; evals (boot order, not just splice order).
; ⚠ the scan asks two?, never ?: a key list is DATA, and `?` would read it for net --
; so a key that sums to nothing (a var labelled 0, a bare nom) would end the scan early
; and answer "unbound" for a binding that is right there.
(: (dict_has d k) (: (loop k ks vs) (? (two? ks) (? (k = <ks) vs (loop k >ks >vs))) (loop k <d >d))
   (dict_set d k v) ((k >< <d) >< v >< >d)
   empty_dict '(())
   has_s dict_has
   ext_s dict_set
   (mintp x) (mint? x)
   (var x) (x >< nom 0)                         ; (label . fresh-nom) -- identity by the nom
   ; ⚠ the TAIL is the nom, and the test says so: a nom sitting anywhere ELSE in a term
   ; -- an atom under `tie`, say -- is data, not a variable. (not &&: it answers () for a
   ; false LEFT, and a predicate owes a bit.)
   (var? x) (? (two? x) (mintp >x) 0)
   (walk s u) (? (nil? (var? u)) u (: t (has_s s u) (? (two? t) (walk s <t) u)))
   ufail (nom 0)
   (ufail? x) (id? x ufail)
   ; a BINDER, and the whole nominal half: the abstraction is a love lambda, so
   ; alpha-equivalence is the LANGUAGE's (= on closures is alpha + structural) and
   ; instantiation is application -- nothing to swap, rename, or capture.
   (tie f) ('tie >< f)
   (tie? t) (? (two? t) (id? <t 'tie) 0)
   (untie t a) (>t a)                            ; open the binder at a
   (unify s u v) (: x (walk s u) y (walk s v)       ; PURE: a substitution, or ufail
     (? (x = y) s                                   ; alpha-equal binders land HERE, for free
        (var? x) (ext_s s x y)
        (var? y) (ext_s s y x)
        (tie? x && tie? y) (: a (nom 0) (unify s (untie x a) (untie y a)))   ; ONE fresh nom opens both
        (two? x && two? y) (: s1 (unify s <x <y) (? (ufail? s1) ufail (unify s1 >x >y)))
        ufail))
   (subst s t) (? (var? t) (: w (walk s t) (? (var? w) w (subst s w)))   ; instantiate a template under s
                  (tie? t) (tie (\ a (subst s (untie t a))))             ; under a binder: at its opening
                  (two? t) (subst s <t >< subst s >t)
                  t)
   (rewrite pat tpl x) (: r (unify '(()) pat x) (? (ufail? r) x (subst r tpl)))   ; pattern -> template, or x unchanged

   ; --- the constraint store -------------------------------------------------
   ; a STATE is (substitution . constraints) -- the store rides BESIDE `unify`,
   ; which keeps its signature and its purity. a constraint is (kind . payload),
   ; re-verified against the whole store after every successful unification
   ; (cKanren's design); an unconstrained search never leaves the (nil? c) door.
   st0 (empty_dict >< ())
   (unify_all s p) (? (two? p) (: r (unify s <<p ><p) (? (ufail? r) ufail (unify_all r >p))) s)
   ; the bindings r has and s does not, as a (u . v) list -- ext_s prepends, so they lead
   (residual r s) (: (go k ks vs acc) (? k (go (k - 1) >ks >vs ((<ks >< <vs) >< acc)) acc)
                     (go ((tally <r) - (tally <s)) <r >r ()))
   ; does x occur in the (walked) term t? a tie is opened at a FRESH nom, which is
   ; exactly what freshness means: an outer atom is never the bound one.
   (inside s x t) (? (x = t) 1
                     (var? t) 0
                     (tie? t) (inside s x (walk s (untie t (nom 0))))
                     (two? t) (? (inside s x (walk s <t)) 1 (inside s x (walk s >t)))
                     0)
   (closed? s t) (? (var? t) 0
                    (tie? t) (closed? s (walk s (untie t (nom 0))))
                    (two? t) (? (closed? s (walk s <t)) (closed? s (walk s >t)) 0)
                    1)
   ; one constraint against s -> 'drop (satisfied for good), ufail (violated), or itself
   (vet1 s c) (: k <c
     (? (id? k 'dif) (: r (unify_all s >c)                    ; unify the prefix on a copy and read it three ways
                        (? (ufail? r) 'drop                   ;   can never hold -> nothing left to watch
                           (: q (residual r s)                ;   holds with no new binding -> violated
                              (? (two? q) ('dif >< q) ufail))))
        (id? k 'abs) (: x (walk s <>c) t (walk s >>c)         ; absento, and nominal freshness by the same door
                        (? (inside s x t) ufail
                           (closed? s t) 'drop
                           ('abs >< x >< t)))
        (id? k 'apt) (: w (walk s >>c)                        ; a type constraint over any predicate
                        (? (var? w) ('apt >< <>c >< w)
                           (<>c w) 'drop
                           ufail))
        c))
   (vet s c) (? (nil? c) c
     (: (go cs) (? (two? cs)
                   (: r (vet1 s <cs)
                      (? (ufail? r) ufail
                         (: t (go >cs)
                            (? (ufail? t) ufail
                               (id? r 'drop) t
                               (r >< t)))))
                   ())
        (go c)))
   (con st c) (: d (vet <st c) (? (ufail? d) ufail (<st >< d)))   ; a state with c added, or ufail

   ; --- streams: a COLIST, love's lazy list (love/p1.l's shape) --------------
   ; () is empty, (x >< s) is mature, a thunk is immature -- three shapes told
   ; apart by lit?/two?, which is canonical microkanren written in love's own
   ; idiom. (the church encoding this replaces spent no type test and paid for it
   ; in closures -- three per step -- for 26-35% more instructions on every shape
   ; measured. doc/kanren.md carries the numbers.)
   sno ()
   (sX x s) (x >< s)
   (sun x) (x >< ())
   (s_plus s t) (s @ (f & (lit? f)) (\ _ (s_plus t (f ())))       ; interleave (disj): the SWAP is the fairness
                     (x >< r)       (x >< s_plus r t)
                     t)
   (s_star s g) (s @ (f & (lit? f)) (\ _ (s_star (f ()) g))       ; bind (conj)
                     (x >< r)       (s_plus (g x) (s_star r g))
                     ())
   (s_app s t)  (s @ (f & (lit? f)) (\ _ (s_app (f ()) t))        ; ORDERED disj: no swap, so the left side runs
                     (x >< r)       (x >< s_app r t)              ; dry first. ⚠ incomplete by construction --
                     t)                                          ; that is the point, and every use owes the word
   (sfire s) (s @ (f & (lit? f)) (sfire (f ())) s)                ; force to a mature shape: () or a cons
   (stake i s) (? (nil? i) () (: m (sfire s) (? (two? m) (<m >< stake (i - 1) >m))))
   (smap f s) (s @ (g & (lit? g)) (\ _ (smap f (g ()))) (x >< r) (f x >< smap f r) ())
   (sfold f z s) (: m (sfire s) (? (two? m) (f <m (sfold f z >m)) z))
   slist (sfold link ())
   (scount s) (: (go a s) (: m (sfire s) (? (two? m) (go (a + 1) >m) a)) (go 0 s))

   ; --- goals ----------------------------------------------------------------
   ; conj2/disj2 are the WHOLE of the goal algebra: &&& and ||| expand to them, and
   ; the ring's * and + are them under the coin. one implementation, two spellings --
   ; and `load` at construction is what lets either spelling take either operand,
   ; a bare goal or a rel-wrapped one.
   (conj2 a b) (: g (load a) h (load b) (\ st (s_star (g st) h)))
   (disj2 a b) (: g (load a) h (load b) (\ st (s_plus (g st) (h st))))
   yea (\ st (sun st))                               ; the goal that holds, once
   nay (\ st ())                                     ; the goal that never does
   (est u v st) (: r (unify <st u v)
                   (? (ufail? r) ()
                      (: c (vet r >st) (? (ufail? c) () (sun (r >< c))))))
   ; DISEQUALITY: unify on a copy and read the outcome three ways -- ufail means
   ; the two can never be equal and there is nothing to remember; no new binding
   ; means they ARE equal, so the goal fails; anything else keeps the residual
   ; bindings as the constraint every later est re-verifies.
   (dif u v st) (: r (unify <st u v)
                   (? (ufail? r) (sun st)
                      (: p (residual r <st)
                         (? (two? p) (: t (con st (('dif >< p) >< >st)) (? (ufail? t) () (sun t)))
                            ()))))
   ; ABSENTO, and nominal freshness by the same machinery: x occurs nowhere in t,
   ; now or ever. for a nom x under `tie` this IS alphaKanren's a # t.
   (absent x t st) (: r (con st (('abs >< x >< t) >< >st)) (? (ufail? r) () (sun r)))
   ; a TYPE constraint over any predicate -- symbolo/numbero and their kin in one goal
   (apt p x st) (: r (con st (('apt >< p >< x) >< >st)) (? (ufail? r) () (sun r)))
   ; --- the extra-logical three. `ifte` is the primitive (the soft cut): if c
   ; holds at all, t runs over EVERY answer of c and e is dropped. `sole` commits
   ; to the first answer. `naf` is negation as FAILURE -- it answers "not proved",
   ; never "false", and only a ground terminating goal lets you read the two as one.
   (ifte c t e st) (: m (sfire (c st)) (? (two? m) (s_star m t) (e st)))
   (sole g st) (: m (sfire (g st)) (? (two? m) (sun <m) ()))
   (naf g st) (ifte g nay yea st)

   ; --- query: run a goal, reify the answers ---------------------------------
   (query n ks g) (:
    (walk_star s u path) (?                          ; cycle-safe: (mu label) on a back-edge
     (var? u) (: t (has_s s u)
                 (? (atom? t) u
                    (member? u path) ['mu <u]
                    (walk_star s <t (u >< path))))
     (tie? u) (: a (nom 0) ['tie a (walk_star s (untie u a) path)])
     (two? u) (walk_star s <u path >< walk_star s >u  path)
     u)
    ; the noms an answer carries -- a tie's binder, an atom a relation minted -- are
    ; RENAMED a1 a2 .. in first-sight order, so an answer reads back and compares.
    (noms t acc) (? (var? t) acc
                    (mintp t) (? (member? t acc) acc (t >< acc))
                    (two? t) (noms >t (noms <t acc))
                    acc)
    (renom d t) (? (var? t) t
                   (mintp t) (: v (dict_has d t) (? (two? v) <v t))
                   (two? t) (renom d <t >< renom d >t)
                   t)
    (dename t) (: ns (rev (noms t ()))
                  (? (two? ns)
                     (renom (foldl (\ d p (dict_set d <p <>p)) empty_dict
                                   (zip ns (map (\ i (intern (+ "a" (show (i + 1))))) (jot (tally ns)))))
                            t)
                     t))
    (keyvar keys k) (? (atom? keys) 0 (: kv <keys (? (k = <kv) kv (keyvar >keys k))))
    (reify1 st) (dename (flip catmap ks (\ k (: kv (keyvar <<st k) (? (two? kv) [k (walk_star <st kv ())])))))
    (map reify1 ((? n (stake n) slist) (g st0))))
   g-die (: d #(0 'goal)                             ; the goal-coin: + = disj = |||, * = conj = &&&
          (pins d 1 (\ A B (coin d (disj2 A B)))
                  2 (\ A B (coin d (conj2 A B)))))
   rel (coin g-die))                     ; wrap a goal as a ring element
(: (=== u v) (rel (est u v))                        ; the relation's infix unify -- and it needs no
   (=/= u v) (rel (dif u v)))                       ; row in prel's `operators`: that table holds only the EXCEPTIONS,
; and any all-punct head is already infix-dyadic at house band. (It did pin one while this lived in
; prel. A post-egg layer could not anyway -- `operators` is mopped at birth, so the operator GRAMMAR
; closes when the egg hatches.) ⚠ =/= shares a band with ===, so a chain folds by hand and wants parens.
; the GOAL LANGUAGE: `::` writes the head layer's macro table, so these four
; live in kanren's own book -- and a consumer's (use 'kanren) splice carries
; them: macroget walks the chain's macro slots, so &&&/|||/\\\/zz expand at the
; caller, and the names their expansions EMIT (var, s_plus, s_star) resolve
; down the same splice (the emit law, served by the walk instead of re-pins).
; the TRIPLED spellings are the point: a relation is the ordinary operator said
; three times -- === is =, =/= its negation, &&& and ||| are && and ||, \\\ is \
; with its params minted fresh as logic vars. all-punct, so all of them are infix
; at house band for free, and (a &&& b) reads as well as (&&& a b).
(:: 'zz (\ x (: s (nom 0) u (nom 0) x2 <x ['\ s u [x2 s]])))   ; delay a goal: the curried tail IS the thunk
(:: '&&& (\ xs (foldl (\ a b [conj2 a b]) <xs >xs)))   ; conj of goals -- the ring's *
(:: '||| (\ xs (foldl (\ a b [disj2 a b]) <xs >xs)))   ; disj of goals -- the ring's +
(:: '\\\ (\ a (('\ >< a) >< map (\ a [var ['\ a]]) (init a))))                         ; lambda w/ fresh logic vars
