; layer-2 spike: abstract KIND INFERENCE over `:` bindings via a seminaive
; fixpoint, the kinds-lattice as the abstract domain. the payoff is DEVIRT --
; a +/* (or apply) site whose operand kinds are statically numeric can skip the
; generic NxN dispatch table and jump straight to the numeric lane.
;
; WHY a fixpoint (not just a tree walk, which const-fold needs): kinds flow
; THROUGH bindings, and `:` is letrec* -- bindings can reference each other (even
; cyclically), so the env must be iterated to a fixpoint. the lattice has finite
; height and the transfer functions are monotone, so `join`-update terminates.
; this is the doc/misc/proto/datalog.l engine specialized to a lattice domain.

(:
 ; --- the kinds-lattice abstract domain: the tier spine N<Z<R<C, str/list as
 ;     incomparable branches that join UP to top; bot = unreached/unknown. ---
 (rank k) (? (id? k 'bot) 0 (id? k 'charm) 1 (id? k 'whole) 2 (id? k 'real) 3 (id? k 'cplx) 4 (id? k 'top) 5 -1)
 (num? k) (: r (rank k) (&& (< 0 r) (< r 5)))               ; charm..cplx are the numeric lane
 (join a b) (? (id? a 'bot) b (id? b 'bot) a (id? a b) a
              (&& (num? a) (num? b)) (? (< (rank a) (rank b)) b a)   ; higher tier wins
              'top)                                          ; mixed branches -> top

 ; --- transfer functions: a kind for an op given its operands' kinds ---
 (tnum a b) (? (|| (id? a 'bot) (id? b 'bot)) 'bot          ; an operand not yet known -> stay bot
              (&& (num? a) (num? b)) (: j (join a b) (? (id? j 'charm) 'whole j))  ; +/* may OVERFLOW charm -> whole
              'top)                                          ; non-numeric +/* (cat/generic) -> give up

 ; --- env: an assoc (name . kind); a missing name reads bot ---
 (look env x) (? (atom? env) 'bot (: p (cap env) (? (id? x (cap p)) (cup p) (look (cup env) x))))
 (joininto env x k) (link (link x (join (look env x) k)) env)   ; monotone shadow-update (look finds newest)

 ; --- kindof: abstract-evaluate an expression under env ---
 (kindof env e) (?
   (charm? e) 'charm                                         ; a fixnum literal
   (string? e) 'str
   (nom? e) (look env e)                                     ; a variable reference
   (atom? e) 'top
   (: h (cap e) a (cap (cup e)) b (cap (cup (cup e)))
     (? (|| (id? h '+) (id? h '*)) (tnum (kindof env a) (kindof env b))
        (id? h '?) (join (kindof env b) (kindof env (cap (cup (cup (cup e))))))   ; (? c a b) -> join the branches
        'top)))                                              ; lambda / unknown form -> top

 ; --- the seminaive FIXPOINT over the flat (n1 e1 n2 e2 ..) binding list ---
 (pairs bs) (? (atom? bs) 0 (atom? (cup bs)) 0 (link (link (cap bs) (cap (cup bs))) (pairs (cup (cup bs)))))
 (alle a b) (? (atom? a) 1 (: p (cap a) (&& (id? (cup p) (look b (cap p))) (alle (cup a) b))))
 (enveq a b) (&& (alle a b) (alle b a))                      ; same name->kind both ways = fixpoint
 (step ps env) (foldl (\ env p (joininto env (cap p) (kindof env (cup p)))) env ps)
 (fix ps env) (: e2 (step ps env) (? (enveq env e2) env (fix ps e2)))
 (infer bs) (fix (pairs bs) 0)

 ; --- devirt: collect +/* sites whose operands are statically numeric ---
 (sites env e) (? (atom? e) 0
   (: h (cap e) sub (catmap (\ s (sites env s)) (cup e))
      (? (&& (|| (id? h '+) (id? h '*))
             (num? (kindof env (cap (cup e)))) (num? (kindof env (cap (cup (cup e))))))
         (link e sub) sub)))

 ; --- report ---
 (showenv bs) (: env (infer bs)
   (: _ (each (pairs bs) (\ p (: _ (puts "    ") _ (. (string (cap p))) _ (puts " : ")
                                _ (. (string (look env (cap p)))) (putc 10))))
      (: ss (catmap (\ p (sites env (cup p))) (pairs bs))
       (: _ (puts "    devirt (numeric, skip dispatch): ") _ (. (show ss)) (putc 10)))))

 _ (: _ (puts ";; A. linear chain  (: x 5 y (+ x 1) z (* y y))\n") (showenv '(x 5 y (+ x 1) z (* y y))))
 _ (: _ (puts ";; B. fixpoint join (: x (? cnd 5 x))  -- x is 5-or-itself -> charm\n") (showenv '(x (? cnd 5 x))))
 _ (: _ (puts ";; C. widening      (: n 100 m (+ n n) p (* m m))  -- overflow climbs charm->whole\n") (showenv '(n 100 m (+ n n) p (* m m))))
 _ (: _ (puts ";; D. non-numeric   (: s \"hi\" t (+ s s))  -- + on str -> top, NOT devirt\n") (showenv '(s "hi" t (+ s s))))
 0)
