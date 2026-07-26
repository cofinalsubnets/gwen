; kanren -- first-order unification + a church-stream search monad.
; a POST-EGG LAYER like love/q.l (see its head for why): prel stays the language, a library
; rides its own out/lib/kanren.h. Needs love/prel.l only. love/post.l's overlay half reads
; `unify`/`ufail?`/`var` from here, so this layer must be evaled BEFORE post.
; the UNIFIER is the compiler-grade core (a future relational rewrite layer sits
; on `unify`/`walk`); the SEARCH half (streams, `query`, `===`) is its userland
; face. `unify s u v` is PURE -- a substitution or `ufail` -- with NO occurs
; check: love's own values are non-well-founded (a chain is a mu-type, the book
; holds the book), so a self-referential binding is welcome -- it's a rational
; tree, and `walk_star` mu-marks a back-edge to reify it without looping. a goal
; is (\ s stream); est/=== unify, + interleaves (disjunction), * binds
; (conjunction) -- so a relation is a polynomial in unifications (see the ring).
; the module span opens here: the trailer below (names ()) sweeps everything this
; section leaks into the `kanren` book, keeping the public few on the global book.
(: kanren-mark (names ()))
(: (dict_has d k) (: (loop k ks vs) (? ks (? (= k (cap ks)) vs (loop k (cup ks) (cup vs)))) (loop k (cap d) (cup d)))
   (dict_set d k v) (link (link k (cap d)) (link v (cup d)))
   empty_dict '(())
   has_s dict_has
   ext_s dict_set
   (mintp x) (&& (nom? x) (nom? (string x)))       ; string of a named sym is its name; of a mint, itself
   (var? x) (&& (two? x) (mintp (caup x)))
   (var x) (link x (nom 0))                         ; (label . fresh-nom) -- identity by the nom
   (walk s u) (? (nil? (var? u)) u (: t (has_s s u) (? (two? t) (walk s (cap t)) u)))
   ufail (nom 0)
   (ufail? x) (id? x ufail)
   (unify s u v) (: x (walk s u) y (walk s v)       ; PURE: a substitution, or ufail
     (? (= x y) s
        (var? x) (ext_s s x y)
        (var? y) (ext_s s y x)
        (&& (two? x) (two? y)) (: s1 (unify s (cap x) (cap y)) (? (ufail? s1) ufail (unify s1 (cup x) (cup y))))
        ufail))
   (subst s t) (? (var? t) (: w (walk s t) (? (var? w) w (subst s w)))   ; instantiate a template under s
                  (two? t) (link (subst s (cap t)) (subst s (cup t)))
                  t)
   (rewrite pat tpl x) (: r (unify '(()) pat x) (? (ufail? r) x (subst r tpl)))   ; pattern -> template, or x unchanged
   (sX x s n m y) (y x s)                            ; stream: cons
   (sno n m y) (n sno)                               ; stream: empty
   (sun x) (sX x sno)
   (s_plus s t) (s (\ _ t) (\ s _ m _ (m (s_plus t s))) (\ x s (sX x (s_plus s t))))   ; interleave (disj)
   (s_star s g) (s (\ _ s) (\ s _ m _ (m (s_star s g))) (\ x s (s_plus (g x) (s_star s g))))  ; bind (conj)
   (stake i s) (? (nil? i) () (s (\ _ ()) (stake i) (\ x s (link x (stake (+ -1 i) s)))))
   (smap f s) (s (\ _ s) (\ s _ m _ (m (smap f s))) (\ x s (sX (f x) (smap f s))))
   (sfold f z s) (s (\ _ z) (sfold f z) (\ x s (f x (sfold f z s))))
   slist (sfold link ())
   (est u v s) (: r (unify s u v) (? (ufail? r) sno (sun r)))        ; unify-as-goal
   (query n ks g) (:
    (walk_star s u path) (?                          ; cycle-safe: (mu label) on a back-edge
     (var? u) (: t (has_s s u)
                 (? (nil? (two? t)) u
                    (member? u path) (L 'mu (cap u))
                    (walk_star s (cap t) (link u path))))
     (two? u) (link (walk_star s (cap u) path) (walk_star s (cup u) path))
     u)
    (keyvar keys k) (? (atom? keys) 0 (: kv (cap keys) (? (= (cap kv) k) kv (keyvar (cup keys) k))))
    (reify1 st) (flip catmap ks (\ k (: kv (keyvar (cap st) k) (? (two? kv) `(k (walk_star st kv ()))))))
    (map reify1 ((? n (stake n) slist) (g empty_dict))))
   g-die (: d #(0 'goal)                             ; the goal-coin: + = disj, * = conj
            _ (pin d 1 (\ A B (coin d (\ s (s_plus ((load A) s) ((load B) s))))))
            _ (pin d 2 (\ A B (coin d (\ s (s_star ((load A) s) (load B))))))
            d)
   (rel goal) (coin g-die goal))                     ; wrap a goal as a ring element
(:: 'zz (\ x (: s (nom 0) n (nom 0) m (nom 0) y (nom 0) x2 (cap x) `('\ s n m y `(m `(x2 s))))))  ; delay a goal (inverse-eta)
(:: 'et (\ xs (: x (cap xs) r (cup xs) (foldl (\ a b `((\ a b s (s_star (a s) b)) a b)) x r))))   ; conj of goals
(:: 'vel (\ xs (: x (cap xs) r (cup xs) (foldl (\ a b `((\ a b s (s_plus (a s) (b s))) a b)) x r)))) ; disj of goals
(:: '\\ (\ a (link (link '\ a) (map (\ a `(var `('\ a))) (init a)))))                             ; lambda w/ fresh logic vars
(: === (\ u v (rel (est u v))))                       ; the relation's infix unify -- and it needs no
; row in prel's `operators`: that table holds only the EXCEPTIONS, and any all-punct head is already
; infix-dyadic at house grip. (It did pin one while this lived in prel. A post-egg layer could not
; anyway -- `operators` is mopped at birth, so the operator GRAMMAR closes when the egg hatches.)
; the kanren MODULE BOUNDARY (the holo/glaze pattern, in-prel so it holds on every
; target): the span since kanren-mark sweeps into the `kanren` book -- (kanren 'query),
; (kanren 'walk) .. -- except the PUBLIC keepers: `unify` (the compiler-grade core)
; with its ufail/ufail? contract, and the names the goal macros + the === operator
; EMIT into user code (var, s_plus, s_star, ===) -- the emit law, the same reason the
; egg keeps spread/link/mono. re-evals (the egg warm) rebuild the book: idempotent.
(: seen (: t (tablet 0) _ (each kanren-mark (\ k (pin t k 1))) t)
   m (tablet 0)
   keep '(unify ufail ufail? === var s_plus s_star)
   _ (each (names ())
      (\ k (? (peep seen k 0) 0
              (member? k keep) 0
              (: _ (pin m k (peep book k 0)) _ (pull book k 0) 0))))
   _ (pull book 'kanren-mark 0)
   _ (pull m 'kanren-mark 0)
   _ (pin book 'kanren (\ k (? (id? k 'keys) (keys m) (peep m k 0))))  ; lookup-only closure: m private,
   0)                                                                  ;   unpoisonable; (kanren 'keys) introspects
