; kanren-prove -- the PRODUCER leg of the verification bridge. the bridge so far makes ai a proof CHECKER (uu's `chk`) that two
; kernels re-certify (rocq/lean). this spike makes ai a proof PRODUCER: given a goal
; type T, kanren SEARCHES for a term e with e : T -- the relational typechecker run
; BACKWARD is the prover (miniKanren's inhabitation trick), specialized to uu's term
; syntax (quoted lists; kanren vars are the holes). the search is UNTRUSTED -- a
; buggy search cannot fake a theorem, because the found e still faces `chk` (and then
; rocq + lean). that asymmetry -- cheap to search, sound to check -- is the design.
;
; this is a SELF-CONTAINED spike: the `proves` relation + a search, no `chk` wired in
; yet (that is phase 4, the gate). it rides kanren as-is: `\\\` binds fresh
; logic vars, est/&&&/|||/zz build goals, `query` runs + reifies. the only risk the
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
;
; ⚠ THE BOUND NAME. a goal type carries its own binder names, so the lam rule usually
; reads one off the type and invents nothing. where it must invent -- a pi whose binder
; slot is still a hole -- `mintname` puts a NOM there, and that is the whole of this
; prover's alpha story: a nom is unequal to every name a term can spell, so no rule can
; capture, and `query` renames the noms an answer carries a1 a2 .. in first-sight order,
; so two proofs that differ only in their bound names REIFY THE SAME and `=` decides
; alpha-equivalence. (the other half of love's alpha law is for GROUND terms: `=` on
; closures is alpha + structural, so kanren's `tie` binder needs no swapping and its
; instantiation is application -- see the tie laws in test/kanren.l. it does not serve
; a SEARCH, because a binder whose body is still a logic var is not yet a function.)

(:
 (say lbl v) (: _ (puts lbl) _ (. v) _ (putc 10) 0)
 (membero x l) (\\\ h t (&&& (est l (link h t)) (||| (est x h) (zz (membero x t)))))
 ; if x is still a hole, name it with a fresh atom -- otherwise leave the type's name alone
 (mintname x) (\ st (: w (walk <st x) (? (var? w) ((est w (nom 0)) st) (sun st))))

 ; (proves DB G T e d): in lemma-DB + context G (a list of (name type)), e : T, within
 ; a proof-depth budget d. var/idpath are the depth-0 base; lam/app recurse under d-1.
 (proves DB G T e d)
   (||| (membero [e T] G)                                                  ; var: a hypothesis
        (\\\ Ap a (&&& (est T ['paths Ap a a]) (est e ['idpath a])))       ; idpath: reflexivity
        (? (< 0 d)
           (||| (\\\ x A B b (&&& (est T ['pi x A B]) (mintname x)         ; lam: T = (pi x A B)
                                  (est e ['lam x b])
                                  (zz (proves DB (link [x A] G) B b (- d 1)))))
                (\\\ f w A B a (&&& (membero [f ['pi w A B]] DB)           ; app: backward-chain a DB lemma
                                    (est B T) (est e [f a])
                                    (zz (proves DB G A a (- d 1))))))
           nay))                                                           ; out of budget: only the base rules

 ; --- demos: the relational typechecker, run backward, GENERATES inhabitants ---
 nat-DB '((s (pi _ nat nat)))                       ; one non-dependent lemma: a successor s : nat -> nat
 R1 (query 1 '(e) (\\\ e (proves '() '() '(pi T UU (pi x T T)) e 4)))                    ; idfun's body
 R2 (query 1 '(e) (\\\ e (proves '() '() '(pi A UU (pi B UU (pi x A (pi y B A)))) e 6))) ; the K combinator
 R3 (query 1 '(e) (\\\ e (proves '() '((h A)) 'A e 2)))                                  ; hypothesis lookup
 R4 (query 1 '(e) (\\\ e (proves '() '() '(paths nat 2 2) e 2)))                         ; a reflexive path
 R5 (query 1 '(e) (\\\ e (proves '() '() '(paths nat 2 3) e 2)))                         ; 2=3: no proof exists
 R6 (query 4 '(e) (\\\ e (proves nat-DB '((n nat)) 'nat e 3)))                           ; enumerate nat inhabitants
 ; the binder slot is a HOLE, so the prover names it: the answer comes back canonical
 R7 (query 1 '(e) (\\\ e x (proves '() '() ['pi x 'nat 'nat] e 2)))                      ; a name the prover invents
 R8 (query 1 '(e) (\\\ q e (proves '() '() ['pi q 'nat 'nat] e 2)))                      ; ..the same proof, other spelling
 _ (say ";; idfun   (pi T UU (pi x T T))        -> " R1)
 _ (say ";; K-comb  (pi A.. (pi y B A))         -> " R2)
 _ (say ";; hyp     ctx (h A), goal A           -> " R3)
 _ (say ";; path    (paths nat 2 2) reflexive   -> " R4)
 _ (say ";; path    (paths nat 2 3) false eqn   -> " R5)
 _ (say ";; nat-gen  s:nat->nat, n:nat |- nat   -> " R6)
 _ (say ";; minted  (pi _ nat nat), name a hole -> " R7)

 ; --- self-checks (the search is order-free, so test membership / the head term) ---
 (assert
  (= '((e (lam T (lam x x)))) R1)                          ; idfun's body, exactly
  (= '((e (lam A (lam B (lam x (lam y x)))))) R2)          ; K combinator
  (= '((e h)) R3)                                          ; the hypothesis h
  (= '((e (idpath 2))) R4)                                 ; reflexivity at 2
  (id? () R5)                                              ; 2=3 is not reflexive -> no proof (correctly empty)
  (member? '(e n) R6)                                      ; the hypothesis n : nat
  (member? '(e (s (s (s n)))) R6)                          ; ... and s applied thrice (backward-chained)
  (= '((e (lam a1 a1))) R7)                                ; the minted name reifies canonically..
  (= R7 R8))                                               ; ..so alpha-equivalence is `=`
 0)
