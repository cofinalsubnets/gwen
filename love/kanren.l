; kanren -- first-order unification + a church-stream search monad.
; a POST-EGG LAYER like love/q.l (see its head for why): prel stays the language, a library
; rides its own out/lib/kanren.h. Needs love/prel.l only. love/overlay.l's engine reads
; `unify`/`ufail?`/`var` from here, so this layer must be evaled BEFORE overlay.
; the UNIFIER is the compiler-grade core (a future relational rewrite layer sits
; on `unify`/`walk`); the SEARCH half (streams, `query`, `===`) is its userland
; face. `unify s u v` is PURE -- a substitution or `ufail` -- with NO occurs
; check: love's own values are non-well-founded (a chain is a mu-type, the book
; holds the book), so a self-referential binding is welcome -- it's a rational
; tree, and `walk_star` mu-marks a back-edge to reify it without looping. a goal
; is (\ s stream); est/=== unify, + interleaves (disjunction), * binds
; (conjunction) -- so a relation is a polynomial in unifications (see the ring).
; a REGISTERED MODULE with NO BRACKETS: the runtime supplies the layer -- a
; frontend lists this source in its ai_libs table and its boot says (use 'kanren),
; which loads, registers and splices in one motion. the splice serves every
; bare consumer (unify/ufail?/var/s_plus/s_star/===, and the goal macros'
; expansions); internals answer to (from 'kanren ..) -- subst, walk, query,
; the dict kit; (from 'kanren 'keys) introspects. love/overlay.l's engine
; reads subst through the registry, so kanren must be REGISTERED before overlay
; evals (boot order, not just splice order).
(: (dict_has d k) (: (loop k ks vs) (? ks (? (= k <ks) vs (loop k >ks >vs))) (loop k <d >d))
   (dict_set d k v) ((k >< <d) >< v >< >d)
   empty_dict '(())
   has_s dict_has
   ext_s dict_set
   (mintp x) (mint? x)
   (var? x) (two? x && mintp <>x)
   (var x) (x >< nom 0)                         ; (label . fresh-nom) -- identity by the nom
   (walk s u) (? (nil? (var? u)) u (: t (has_s s u) (? (two? t) (walk s <t) u)))
   ufail (nom 0)
   (ufail? x) (id? x ufail)
   (unify s u v) (: x (walk s u) y (walk s v)       ; PURE: a substitution, or ufail
     (? (x = y) s
        (var? x) (ext_s s x y)
        (var? y) (ext_s s y x)
        (two? x && two? y) (: s1 (unify s <x <y) (? (ufail? s1) ufail (unify s1 >x >y)))
        ufail))
   (subst s t) (? (var? t) (: w (walk s t) (? (var? w) w (subst s w)))   ; instantiate a template under s
                  (two? t) (subst s <t >< subst s >t)
                  t)
   (rewrite pat tpl x) (: r (unify '(()) pat x) (? (ufail? r) x (subst r tpl)))   ; pattern -> template, or x unchanged
   (sX x s n m y) (y x s)                            ; stream: cons
   (sno n m y) (n sno)                               ; stream: empty
   (sun x) (sX x sno)
   (s_plus s t) (s (\ _ t) (\ s _ m _ (m (s_plus t s))) (\ x s (sX x (s_plus s t))))   ; interleave (disj)
   (s_star s g) (s (\ _ s) (\ s _ m _ (m (s_star s g))) (\ x s (s_plus (g x) (s_star s g))))  ; bind (conj)
   (stake i s) (? i (s (\ _ ()) (stake i) (\ x s (x >< stake (i - 1) s))))
   (smap f s) (s (\ _ s) (\ s _ m _ (m (smap f s))) (\ x s (sX (f x) (smap f s))))
   (sfold f z s) (s (\ _ z) (sfold f z) (\ x s (f x (sfold f z s))))
   slist (sfold link ())
   (est u v s) (: r (unify s u v) (? (ufail? r) sno (sun r)))        ; unify-as-goal
   (query n ks g) (:
    (walk_star s u path) (?                          ; cycle-safe: (mu label) on a back-edge
     (var? u) (: t (has_s s u)
                 (? (atom? t) u
                    (member? u path) `('mu <u)
                    (walk_star s <t (u >< path))))
     (two? u) (walk_star s <u path >< walk_star s >u  path)
     u)
    (keyvar keys k) (? (atom? keys) 0 (: kv <keys (? (k = <kv) kv (keyvar >keys k))))
    (reify1 st) (flip catmap ks (\ k (: kv (keyvar <st k) (? (two? kv) `(k (walk_star st kv ()))))))
    (map reify1 ((? n (stake n) slist) (g empty_dict))))
   g-die (: d #(0 'goal)                             ; the goal-coin: + = disj, * = conj
          (pins d 1 (\ A B (coin d (\ s (s_plus ((load A) s) ((load B) s)))))
                  2 (\ A B (coin d (\ s (s_star ((load A) s) (load B)))))))
   rel (coin g-die))                     ; wrap a goal as a ring element
(: === (\ u v (rel (est u v))))                       ; the relation's infix unify -- and it needs no
; row in prel's `dyadics`: that table holds only the EXCEPTIONS, and any all-punct head is already
; infix-dyadic at house grip. (It did pin one while this lived in prel. A post-egg layer could not
; anyway -- `dyadics` is mopped at birth, so the operator GRAMMAR closes when the egg hatches.)
; the GOAL LANGUAGE: `::` writes the head layer's macro table, so these four
; live in kanren's own book -- and a consumer's (use 'kanren) splice carries
; them: macroget walks the chain's macro slots, so et/vel/\\/zz expand at the
; caller, and the names their expansions EMIT (var, s_plus, s_star) resolve
; down the same splice (the emit law, served by the walk instead of re-pins).
(:: 'zz (\ x (: s (nom 0) n (nom 0) m (nom 0) y (nom 0) x2 <x `('\ s n m y `(m `(x2 s))))))  ; delay a goal (inverse-eta)
(:: 'et (\ xs (: x <xs r >xs (foldl (\ a b `((\ a b s (s_star (a s) b)) a b)) x r))))   ; conj of goals
(:: 'vel (\ xs (: x <xs r >xs (foldl (\ a b `((\ a b s (s_plus (a s) (b s))) a b)) x r)))) ; disj of goals
(:: '\\ (\ a (link (link '\ a) (map (\ a `(var `('\ a))) (init a)))))                             ; lambda w/ fresh logic vars
