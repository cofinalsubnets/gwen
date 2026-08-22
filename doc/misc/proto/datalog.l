; seminaive datalog prototype -- pressure-testing the "unifier in prel" cut.
; the ENGINE below uses ONLY: var? walk unify (+ look/ext internal) and a
; cycle-safe reify. no streams, no search monad -- so the kanren SEARCH half is
; NOT what the compiler leans on; the UNIFIER is. that is the cut.
;
; occurs check is OPTIONAL (the `occ` arg to unify). relaxed -> unification over
; RATIONAL trees == recursive (equirecursive) types. ai's own value world is
; non-well-founded (the book holds the book, closures recurse, a chain is a
; mu-type), so the occurs check is the WRONG discipline here -- it would reject
; exactly the recursive structure ai inhabits. the price: every consumer of a
; substitution must be cycle-safe (reify carries a path and emits (mu n) on a
; back-edge) or it loops. that is the same "reach a fixpoint, don't forbid the
; cycle" stance as the seminaive saturation itself.

; the kanren machinery rides the `kanren` MODULE book now (love/prel.l)
(: walk (from 'kanren 'walk)  var? (from 'kanren 'var?))

(:
 ; ---- terms: a var is (v . name); consts are syms/nums; atoms are lists ----
 (vr n) (L 'v n)
 (var? x) (&& (two? x) (id? (cap x) 'v))
 (nm x) (caup x)
 (vsym? x) (&& (nom? x) (: c ((string x) 0) (&& (<= 65 c) (<= c 90))))   ; UPPER -> var
 (termify t) (? (atom? t) (? (vsym? t) (vr t) t) (map termify t))
 (mkrule spec) (: ts (map termify spec) (link (cap ts) (cup ts)))         ; (head . body)

 ; ---- substitution + unify, occurs check OPTIONAL (occ) ----
 bot (nom 'fail)
 (fail? s) (id? s bot)
 (look s k) (? (atom? s) 0 (: p (cap s) (? (id? k (cap p)) p (look (cup s) k))))
 (ext s k v) (link (link k v) s)
 (walk s t) (? (var? t) (: p (look s (nm t)) (? (two? p) (walk s (cup p)) t)) t)
 (occurs s k t) (: wt (walk s t)            ; fresh name: a body-`:` binding does NOT
                  (? (var? wt) (id? (nm wt) k)   ; read the param of the same name (one scope)
                     (two? wt) (|| (occurs s k (cap wt)) (occurs s k (cup wt)))
                     0))
 (unify s a b occ) (:
   wa (walk s a) wb (walk s b)
   (? (&& (var? wa) (var? wb) (id? (nm wa) (nm wb))) s
      (var? wa) (? (&& occ (occurs s (nm wa) wb)) bot (ext s (nm wa) wb))
      (var? wb) (? (&& occ (occurs s (nm wb) wa)) bot (ext s (nm wb) wa))
      (&& (two? wa) (two? wb)) (: s1 (unify s (cap wa) (cap wb) occ)
                               (? (fail? s1) bot (unify s1 (cup wa) (cup wb) occ)))
      (= wa wb) s
      bot))

 ; ---- cycle-safe reification (greatest-fixpoint walk; (mu n) on a back-edge) ----
 (reify s t path) (?
   (var? t) (: w (walk s t)
               (? (id? w t) t
                  (member? (nm t) path) (L 'mu (nm t))
                  (reify s w (link (nm t) path))))
   (two? t) (link (reify s (cap t) path) (reify s (cup t) path))
   t)
 (rei s t) (reify s t 0)

 ; ---- seminaive evaluation (set saturation; delta = last round's new facts) ----
 (matches s atom facts) (catmap (\ f (: s1 (unify s atom f 0) (? (fail? s1) 0 (L s1)))) facts)
 (solve s atoms facts) (? (atom? atoms) (L s)
   (catmap (\ s1 (solve s1 (cup atoms) facts)) (matches s (cap atoms) facts)))
 (solveD s atoms facts delta) (? (atom? atoms) 0        ; >=1 atom MUST come from delta
   (cat (catmap (\ s1 (solve  s1 (cup atoms) facts))       (matches s (cap atoms) delta))
        (catmap (\ s1 (solveD s1 (cup atoms) facts delta)) (matches s (cap atoms) facts))))
 (fire rules facts delta)
   (catmap (\ r (map (\ s (rei s (cap r))) (solveD 0 (cup r) facts delta))) rules)
 (dedup l) (foldl (\ a x (? (member? x a) a (link x a))) 0 l)
 (loop rules facts delta)
   (: fresh (filter (\ f (nil? (member? f facts))) (dedup (fire rules facts delta)))
      (? (atom? fresh) facts (loop rules (cat fresh facts) fresh)))
 (run edb rules) (loop rules edb edb)

 ; ---- coinductive variant: the GREATEST fixpoint (same operator, from ABOVE) ----
 ; lfp (run, above): start empty, grow by saturation -- inductive.
 ; gfp (cot, below): start FULL, shrink to the largest self-supporting set --
 ; coinductive. T(S) = heads derivable from edb+S; iterate S <- T(S) down to S=T(S).
 (step rules facts) (dedup (catmap (\ r (map (\ s (rei s (cap r))) (solve 0 (cup r) facts))) rules))
 (subset a b) (all (\ x (member? x b)) a)
 (seteq a b) (&& (subset a b) (subset b a))
 (cot edb rules top) (: nxt (step rules (cat edb top))
                        (? (seteq top nxt) top (cot edb rules nxt)))

 (only tag facts) (filter (\ f (id? (cap f) tag)) facts)
 (say lbl v) (: _ (puts lbl) _ (. v) _ (putc 10) 0)

 ; ============ demo A: transitive closure over a CYCLIC graph ============
 edbA  (map termify '((edge a b) (edge b c) (edge c a)))
 rulesA (map mkrule '(((path X Y) (edge X Y))
                      ((path X Z) (edge X Y) (path Y Z))))
 _ (say ";; A. transitive closure (a->b->c->a):  " (only 'path (run edbA rulesA)))

 ; ============ demo B: points-to / aliasing (the compiler win) ============
 edbB  (map termify '((assign p h1) (assign q h2) (copy p q) (copy q p)))
 rulesB (map mkrule '(((pt X H) (assign X H))
                      ((pt X H) (copy X Y) (pt Y H))))
 _ (say ";; B. points-to (p,q mutually copy):    " (only 'pt (run edbB rulesB)))

 ; ============ demo C: relax the occurs check -> recursive types ============
 ; infer the type of a chain:  list = (prod elem list)  -- `list` occurs in itself.
 a    (vr 'A)
 ty   (termify '(prod elem A))           ; the rhs:  (prod elem A), A free
 con  (unify 0 a ty 1)                   ; occurs check ON  -> the var occurs in the term
 coff (unify 0 a ty 0)                   ; occurs check OFF -> bind anyway (rational tree)
 _ (say ";; C. occurs ON,  A = (prod elem A):    " (? (fail? con) 'REJECTED 'bound))
 _ (say ";; C. occurs OFF, A = (prod elem A):    " (rei coff a))   ; the mu-type, no loop

 ; ============ demo D: coinductive (greatest) fixpoint iteration ============
 ; rule  inf(X) :- edge(X,Y), inf(Y)  -- "X can step forever".  NO base case, so
 ; the LEAST fixpoint is empty; the GREATEST fixpoint is every node on a cycle.
 infrule (map mkrule '(((inf X) (edge X Y) (inf Y))))
 cyc   (map termify '((edge a b) (edge b c) (edge c a)))    ; a 3-cycle
 dag   (map termify '((edge a b) (edge b c)))               ; a->b->c, no loop
 top   (map termify '((inf a) (inf b) (inf c)))             ; start: assume all loop
 _ (say ";; D. lfp inf (cycle), no base case:     " (only 'inf (run cyc infrule)))
 _ (say ";; D. gfp inf (cycle)  -> on a cycle:     " (cot cyc infrule top))
 _ (say ";; D. gfp inf (a->b->c) -> none loop:     " (cot dag infrule top))
 0)
