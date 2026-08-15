; kanren-prove -- the PRODUCER leg of the verification bridge (doc/archive/kanren-prover.md,
; phases 0-2). the bridge so far makes ai a proof CHECKER (uu's `chk`) that two
; kernels re-certify (rocq/lean). this spike makes ai a proof PRODUCER: given a goal
; type T, kanren SEARCHES for a term e with e : T -- the relational typechecker run
; BACKWARD is the prover (miniKanren's inhabitation trick), specialized to uu's term
; syntax (quoted lists; kanren vars are the holes). the search is UNTRUSTED -- a
; buggy search cannot fake a theorem, because the found e still faces `chk` (and then
; rocq + lean). that asymmetry -- cheap to search, sound to check -- is the design.
;
; this is a SELF-CONTAINED spike: the `proves` relation + a search, no `chk` wired in
; yet (that is phase 4, the gate). it rides prel's kanren as-is: `\\` binds fresh
; logic vars, est/et/vel/zz build goals, `query` runs + reifies. the only risk the
; plan flags shows up immediately -- the app rule GENERATES (an inhabitant of `nat`
; is n, or (s n), or (s (s n)), ...) so the search is infinite; a DEPTH BUDGET d turns
; it into a semi-decision procedure ("found" or "gave up", never "false"), exactly the
; phase-2 shape. type-direction keeps it tight: lam fires only when T unifies with a
; pi, app only over the lemma DB, so a ground goal prunes hard.
;
; uu term syntax (the pieces this spike covers): (pi x A B) (lam x b) (paths A a b)
; (idpath a), an application spine (f a), points as bare syms. the rules, run backward:
;   var    -- e is a hypothesis in the context G whose type unifies with T
;   idpath -- T = (paths A a a) (reflexive) => e = (idpath a)
;   lam    -- T = (pi x A B)             => e = (lam x b), recurse with x:A in G
;   app    -- pick f:(pi _ A T) from the lemma DB => e = (f a), recurse to prove a:A
; (backward chaining: unify a lemma's CONCLUSION with the goal first, then prove its
; argument -- not blind generation. dependent lemmas / higher-order unification are
; phase 3: the pure `unify` is first-order, so the DB here is non-dependent lemmas.)

(:
 (say lbl v) (: _ (puts lbl) _ (. v) _ (putc 10) 0)
 (membero x l) (\\ h t (et (est l (link h t)) (vel (est x h) (zz (membero x t)))))

 ; (proves DB G T e d): in lemma-DB + context G (a list of (name type)), e : T, within
 ; a proof-depth budget d. var/idpath are the depth-0 base; lam/app recurse under d-1.
 (proves DB G T e d)
   (vel (membero (L e T) G)                                                ; var: a hypothesis
        (\\ Ap a (et (est T (L 'paths Ap a a)) (est e (L 'idpath a))))     ; idpath: reflexivity
        (? (< 0 d)
           (vel (\\ x A B b (et (est T (L 'pi x A B)) (est e (L 'lam x b)) ; lam: T = (pi x A B)
                                (zz (proves DB (link (L x A) G) B b (- d 1)))))
                (\\ f w A B a (et (membero (L f (L 'pi w A B)) DB)          ; app: backward-chain a DB lemma
                                  (est B T) (est e (L f a))
                                  (zz (proves DB G A a (- d 1))))))
           (\ _ sno)))                                                     ; out of budget: only the base rules

 ; --- demos: the relational typechecker, run backward, GENERATES inhabitants ---
 nat-DB '((s (pi _ nat nat)))                       ; one non-dependent lemma: a successor s : nat -> nat
 R1 (query 1 '(e) (\\ e (proves '() '() '(pi T UU (pi x T T)) e 4)))                    ; idfun's body
 R2 (query 1 '(e) (\\ e (proves '() '() '(pi A UU (pi B UU (pi x A (pi y B A)))) e 6))) ; the K combinator
 R3 (query 1 '(e) (\\ e (proves '() '((h A)) 'A e 2)))                                  ; hypothesis lookup
 R4 (query 1 '(e) (\\ e (proves '() '() '(paths nat 2 2) e 2)))                         ; a reflexive path
 R5 (query 1 '(e) (\\ e (proves '() '() '(paths nat 2 3) e 2)))                         ; 2=3: no proof exists
 R6 (query 4 '(e) (\\ e (proves nat-DB '((n nat)) 'nat e 3)))                           ; enumerate nat inhabitants
 _ (say ";; idfun   (pi T UU (pi x T T))        -> " R1)
 _ (say ";; K-comb  (pi A.. (pi y B A))         -> " R2)
 _ (say ";; hyp     ctx (h A), goal A           -> " R3)
 _ (say ";; path    (paths nat 2 2) reflexive   -> " R4)
 _ (say ";; path    (paths nat 2 3) false eqn   -> " R5)
 _ (say ";; nat-gen  s:nat->nat, n:nat |- nat   -> " R6)

 ; --- self-checks (the search is order-free, so test membership / the head term) ---
 (assert
  (= '((e (lam T (lam x x)))) R1)                          ; idfun's body, exactly
  (= '((e (lam A (lam B (lam x (lam y x)))))) R2)          ; K combinator
  (= '((e h)) R3)                                          ; the hypothesis h
  (= '((e (idpath 2))) R4)                                 ; reflexivity at 2
  (id? () R5)                                              ; 2=3 is not reflexive -> no proof (correctly empty)
  (member? '(e n) R6)                                      ; the hypothesis n : nat
  (member? '(e (s (s (s n)))) R6))                         ; ... and s applied thrice (backward-chained)
 0)
