; uu2coq: love's internal proof corpus, exported to Rocq -- at scale. The proof half
; of the verification bridge (cf. tools/spec2coq.l, which exports concrete ASSERTS).
;
; This sweeps test/uu.l: it loads uu's kernel (so every defn there is type-checked
; against its stated type -- uu certifies them), then reads the same file as data,
; collects every (defn/defq nm T t), and translates the ones in the universe-safe
; MLTT fragment to Gallina. coqc re-checks the lot -- an independent second witness
; for each law, and (since uu runs type-in-type) the universe-consistency filter uu
; itself lacks. Untranslatable entries are skipped and counted, never silently
; dropped (the trailing comment reports the tally), exactly like spec2coq.
;
; a def exports iff it is in the handled fragment and every global it mentions also
; exports -- a dependency closure. So the whole univalence tower drops on its own
; (it bottoms out at univalenceAxiom, an `axm` we never emit), as does anything
; using a constructor we don't map (total2/coprod and friends). What survives is the
; predicative core: the path-algebra combinators and the nat/bool arithmetic.
;
; Run: out/host/love tools/uu2coq.l > test/proof/rocq/uugen.v   then  coqc test/proof/rocq/uugen.v

(: _ (reads (open "test/uu.l" "r"))
   _ (reads (open "test/uuhom.l" "r"))
   _ (reads (open "test/uuhomgen.l" "r"))
   _ (reads (open "test/uuhomlaw.l" "r"))
   _ (reads (open "test/uusplgen.l" "r"))
   _ (reads (open "test/uuspllaw.l" "r"))
   _ (reads (open "test/uulay.l" "r"))
   _ (reads (open "test/uupatch.l" "r"))
   _ (reads (open "test/uuwm.l" "r"))
   _ (reads (open "test/uuwmlaw.l" "r"))
   _ (reads (open "test/uukind.l" "r"))
   _ (reads (open "test/uumx.l" "r"))
   _ (reads (open "test/uumxlaw.l" "r"))
   _ (reads (open "test/uuval.l" "r"))
   _ (reads (open "test/uuvalband.l" "r"))
   _ (reads (open "test/uuvallaw.l" "r"))
   _ (reads (open "test/uukindlaw.l" "r"))          ; load + certify uu's corpus (populates GLOB)
   bs (intern "\\")
   (unq x) (? (&& (two? x) (id? (cap x) bs)) (cap (cup x)) x)   ; strip one ' (reads shows quote as (\ ..))
   (cat l) (foldl (+) "" l)
   (e2 l) (cap (cup l))  (e3 l) (cap (cup (cup l)))
   (e4 l) (cap (cup (cup (cup l))))  (e5 l) (cap (cup (cup (cup (cup l)))))

   ; read a file's top-level forms as data (the reader strips ; comments)

   ; a uu name -> a Rocq identifier: prefix uu_, kebab '-' -> '_'
   (sanit s) (: (g i acc) (? (< i (tally s)) (g (+ 1 i) (+ acc (: c (s i) (? (= c 45) 95 c)))) acc) (g 0 ""))
   (rocqn nm) (+ "uu_" (sanit (string nm)))

   ; --- collect (kind nm T t) entries from the uu.l forms (defns live inside asserts) ---
   (deq? c) (&& (two? c) (| (id? (cap c) 'defn) (id? (cap c) 'defq)))
   (axq? c) (&& (two? c) (id? (cap c) 'axm))
   (entry c) (? (deq? c) ['def (unq (e2 c)) (unq (e3 c)) (unq (e4 c))]
                (axq? c) ['axm (unq (e2 c)) (unq (e3 c)) ()])
   (kids ks acc) (? (two? ks) (kids (cup ks) (? (| (deq? (cap ks)) (axq? (cap ks))) (link (entry (cap ks)) acc) acc)) acc)
   (gather fs acc) (? (two? fs)
                      (gather (cup fs) (: F (cap fs)
                        (? (&& (two? F) (id? (cap F) 'assert)) (kids (cup F) acc)
                           (| (deq? F) (axq? F)) (link (entry F) acc) acc)))
                      (rev acc))
   (app2 a b) (? (two? a) (link (cap a) (app2 (cup a) b)) b)   ; not `cat` -- the local cat is the string fold
   ents (gather (app2 (forms (open "test/uu.l" "r")) (app2 (forms (open "test/uuhom.l" "r")) (app2 (forms (open "test/uuhomgen.l" "r")) (app2 (forms (open "test/uuhomlaw.l" "r")) (app2 (forms (open "test/uusplgen.l" "r")) (app2 (forms (open "test/uuspllaw.l" "r")) (app2 (forms (open "test/uulay.l" "r")) (app2 (forms (open "test/uupatch.l" "r")) (app2 (forms (open "test/uuwm.l" "r")) (app2 (forms (open "test/uuwmlaw.l" "r")) (app2 (forms (open "test/uukind.l" "r")) (app2 (forms (open "test/uukindlaw.l" "r")) (app2 (forms (open "test/uumx.l" "r")) (app2 (forms (open "test/uumxlaw.l" "r")) (app2 (forms (open "test/uuval.l" "r")) (app2 (forms (open "test/uuvalband.l" "r")) (forms (open "test/uuvallaw.l" "r")))))))))))))))))) ())

   ; the silent-no-op gate (defense in depth behind uu.l's arity-door macros):
   ; a def gathered off the file must be whole -- three operands; a paren slip
   ; that merges the type into the term reads () in the term seat -- and
   ; pinned, since a partially applied defn/defq is a truthy closure that pins
   ; nothing and paints its assert dot green. redden here instead. the empty seat is
   ; `id? ()` and not `!`: a term of 0 is a legal def (a code table's first row) and
   ; falsy, so truthiness would redden it.
   _ (map (\ e (? (| (&& (id? (cap e) 'def) (id? (e4 e) ()))
                     (! (peep GLOB (e2 e) 0)))
                  (: _ (puts "MALFORMED-OR-UNPINNED-DEF ") _ (puts (string (e2 e))) (quit 1))
                  0))
        ents)

   ; names that exist (corpus-wide) -- for telling a global reference from a bound var
   nt (tablet 0) _ (map (\ e (pin nt (e2 e) 1)) ents)
   (corpus? x) (peep nt x 0)

   ; explicit skips: the type-in-type detonations (sound MLTT can't host them, and
   ; coqc's universe check rightly rejects them -- the filter, working as intended)
   bl (tablet 0)
   _ (map (\ n (pin bl n 1)) '(the_world_explodes but_seriously_the_world_explodes whoa irref))

   ; --- the translator: a uu term -> a Gallina string ---
   ; every MLTT constructor is now mapped (Sigma -> sigT, coprod -> sum, the -> ascription);
   ; `un` stays as the safety net for any head u2c can't translate -- empty today, so the
   ; dependency closure + coqc are what filter (a stray head would emit a bad spine -> coqc red).
   un (tablet 0)
   bases (tablet 0)
   _ (pin bases 'nat "nat") _ (pin bases 'UU "Type") _ (pin bases 'bool "bool")
   _ (pin bases 'unit "unit") _ (pin bases 'empty "Empty_set")
   _ (pin bases 'true "true") _ (pin bases 'false "false") _ (pin bases 'tt "tt")
   (atom-c t) (peep bases t (? (corpus? t) (rocqn t) (string t)))   ; base type / global / bound var

   (spine-c l) (? (two? l) ((u2c (cap l)) + (? (two? (cup l)) " " "") + (spine-c (cup l))) "")
   (u2c t)
   (? (charm? t) (show t)
      (nom? t) (atom-c t)
      (two? t) (: h (cap t)
        (? (id? h 'pi)        ("(forall " + (string (e2 t)) + " : " + (u2c (e3 t)) + ", " + (u2c (e4 t)) + ")")
           (id? h 'lam)       ("(fun " + (string (e2 t)) + " => " + (u2c (e3 t)) + ")")
           (id? h 'let)       ("(let " + (string (e2 t)) + " := " + (u2c (e3 t)) + " in " + (u2c (e4 t)) + ")")
           (id? h 'succ)      ("(S " + (u2c (e2 t)) + ")")
           (id? h 'idpath)    ("(idpath " + (u2c (e2 t)) + ")")
           (id? h 'paths)     ("(@paths " + (u2c (e2 t)) + " " + (u2c (e3 t)) + " " + (u2c (e4 t)) + ")")
           (id? h 'nat_rect)  ("(nat_rect " + (u2c (e2 t)) + " " + (u2c (e3 t)) + " " + (u2c (e4 t)) + " " + (u2c (e5 t)) + ")")
           (id? h 'bool_rect) ("(bool_rect " + (u2c (e2 t)) + " " + (u2c (e3 t)) + " " + (u2c (e4 t)) + " " + (u2c (e5 t)) + ")")
           (id? h 'unit_rect) ("(unit_rect " + (u2c (e2 t)) + " " + (u2c (e3 t)) + " " + (u2c (e4 t)) + ")")
           (id? h 'empty_rect)("(Empty_set_rect " + (u2c (e2 t)) + " " + (u2c (e3 t)) + ")")
           ; Sigma -> a primitive-projection total2 record (preamble below): it has the
           ; same definitional eta uu's Sigma has, so eta-dependent proofs (idisweq ..)
           ; export soundly. total2 mirrors pi (e2 var, e3 domain, e4 body); tpair's
           ; family is implicit, inferred from the def's stated type.
           (id? h 'total2)    ("(total2 (fun " + (string (e2 t)) + " : " + (u2c (e3 t)) + " => " + (u2c (e4 t)) + "))")
           (id? h 'tpair)     ("(tpair " + (u2c (e2 t)) + " " + (u2c (e3 t)) + ")")
           (id? h 'pr1)       ("(pr1 " + (u2c (e2 t)) + ")")
           (id? h 'pr2)       ("(pr2 " + (u2c (e2 t)) + ")")
           ; coprod -> sum; ii1/ii2 -> inl/inr (other summand inferred), the eliminator -> sum_rect
           (id? h 'coprod)    ("(sum " + (u2c (e2 t)) + " " + (u2c (e3 t)) + ")")
           (id? h 'ii1)       ("(inl " + (u2c (e2 t)) + ")")
           (id? h 'ii2)       ("(inr " + (u2c (e2 t)) + ")")
           (id? h 'coprod_rect)("(sum_rect " + (u2c (e2 t)) + " " + (u2c (e3 t)) + " " + (u2c (e4 t)) + " " + (u2c (e5 t)) + ")")
           ; (the T t) is a type ascription -- emit it as one
           (id? h 'the)       ("(" + (u2c (e3 t)) + " : " + (u2c (e2 t)) + ")")
           ; full path induction (J): uu (paths_rect P pr e) -> match on the paths Id type
           (id? h 'paths_rect)
             ("(match " + (u2c (e4 t)) + " as ep0 in (paths _ bp0) return ("
              + (u2c (e2 t)) + " bp0 ep0) with idpath _ => " + (u2c (e3 t)) + " end)")
           ("(" + (spine-c t) + ")")))                              ; an application spine
      (show t))

   ; --- translatability: every head handled (no unhandled constructor anywhere) ---
   (alltr? l) (? (two? l) (&& (tr? (cap l)) (alltr? (cup l))) 1)
   (tr? t) (? (charm? t) 1  (nom? t) 1
              (two? t) (? (peep un (cap t) 0) 0 (alltr? (cup t)))  1)

   ; --- the global names a term mentions (corpus names only), accumulated into a set ---
   (ment t acc) (? (nom? t) (? (corpus? t) (: _ (pin acc t 1) acc) acc)
                   (two? t) (ment-l t acc)  acc)
   (ment-l l acc) (? (two? l) (ment-l (cup l) (ment (cap l) acc)) acc)

   ; candidates: a def, in the fragment, not blacklisted, present in glob (uu-certified)
   (cand? e) (&& (id? (cap e) 'def) (! (peep bl (e2 e) 0)) (peep GLOB (e2 e) 0) (tr? (e3 e)) (tr? (e4 e)))
   cands (foldl (\ a e (? (cand? e) (link e a) a)) () ents)   ; reversed; restore order at emit
   ; each candidate's mentioned-names list (its global dependencies)
   (mlist e) (keys (ment-l (link (e3 e) (link (e4 e) ())) (tablet 0)))

   ; --- dependency closure: keep only defs all of whose mentions also survive ---
   (allin? l ex) (? (two? l) (&& (peep ex (cap l) 0) (allin? (cup l) ex)) 1)
   (pass cs ex) (foldl (\ a e (? (allin? (mlist e) ex) (: _ (pin a (e2 e) 1) a) a)) (tablet 0) cs)
   (fix ex) (: ex2 (pass cands ex) (? (= $ex2 $ex) ex2 (fix ex2)))
   ex0 (foldl (\ a e (: _ (pin a (e2 e) 1) a)) (tablet 0) cands)    ; seed: all candidates
   keep (fix ex0)                                                   ; the closed exportable set

   ; --- emit, in source order ---
   (emit e) (cat ["Definition " (rocqn (e2 e)) " : " (u2c (e3 e)) " :=" (string 10)
                  "  " (u2c (e4 e)) "." (string 10)])
   ; foldr keeps ents' order, and coq wants every definition before its uses (foldl flips)
   exports (foldr (\ e a (? (peep keep (e2 e) 0) (link e a) a)) () ents)

   _ (dot (cat [
      "(* test/proof/rocq/uugen.v -- GENERATED from test/uu.l by tools/uu2coq.l. Do not edit;" (string 10)
      "   regenerate with `make test_uugen`. uu's kernel type-checked each term below" (string 10)
      "   against its theorem (test/uu.l, green under `make test`); this is the SAME term" (string 10)
      "   translated to Gallina, which coqc re-checks -- the independent second witness," (string 10)
      "   and the universe filter uu's type-in-type kernel lacks. Only the MLTT primitives" (string 10)
      "   are mapped (nat/bool/unit/empty + recursors, paths<->a Type-valued Id type + J-match," (string 10)
      "   Sigma<->a primitive-projection total2 record with eta, coprod<->sum); only the" (string 10)
      "   univalence tower (an axm) is skipped, on its own via the dependency closure" (string 10)
      "   (counted at the foot). *)" (string 10)
      "From Stdlib Require Import PeanoNat." (string 10)
      "Set Warnings \"-notation-overridden,-abstract-large-number\"." (string 10)
      "(* universe polymorphism -- the faithful encoding of uu's structures (large Sigmas like" (string 10)
      "   wfs/hProp reuse total2 AT the universe of UU). It does NOT re-enable type-in-type:" (string 10)
      "   Type@{i} : Type@{i+1} stays strict, so the cheats below are still rightly rejected. *)" (string 10)
      "Set Universe Polymorphism." (string 10)
      "Set Polymorphic Inductive Cumulativity.  (* Prop-valued paths flow into Type carriers *)" (string 10)
      "(* uu's Sigma has definitional eta; the faithful Coq target is a primitive-projection" (string 10)
      "   record (eta in the kernel, axiom-free), NOT stdlib sigT (no eta). pr1/pr2/tpair" (string 10)
      "   mirror uu's own vocabulary; coprod stays stdlib sum (no eta needed). *)" (string 10)
      "Set Primitive Projections." (string 10)
      "Record total2 {A : Type} (B : A -> Type) : Type := tpair { pr1 : A ; pr2 : B pr1 }." (string 10)
      "Unset Primitive Projections." (string 10)
      "Arguments tpair {A B} _ _.  Arguments pr1 {A B} _.  Arguments pr2 {A B} _." (string 10)
      "(* uu's paths is the Type-valued identity type (UniMath's `paths`), NOT Prop's eq:" (string 10)
      "   a path can be a Sigma CARRIER (an element of UU), as in the hProp-valued nat order." (string 10)
      "   paths_to_eq (below) bridges it back to Coq's = for the Nat.* headline laws. *)" (string 10)
      "Inductive paths {A : Type} (a : A) : A -> Type := idpath : paths a a." (string 10) (string 10)]))
   _ (map (\ e (dot (emit e))) exports)
   ; --- the bridge: a static tail tying uu's nat ops to Coq's standard ones, so the
   ;     headline laws restate on Nat.* (spec.v's vocabulary) and discharge from the
   ;     uu exports above. Tactic proofs (induction over the shared recursion); the
   ;     substantive content stays the exported uu terms. ---
   _ (dot (cat [(string 10)
     "(* === bridge: uu's nat ops ARE Coq's standard ones, so the laws above land on" (string 10)
     "   Nat.add / Nat.mul -- the operations spec.v's own laws speak. uu_add_std/uu_mul_std" (string 10)
     "   (induction over the shared recursion) tie them to the standard vocabulary, by which" (string 10)
     "   the headline arithmetic laws are RESTATED on Nat.* and discharged FROM love's exports. *)" (string 10)
     "Lemma paths_to_eq : forall (A : Type) (a b : A), paths a b -> a = b." (string 10)
     "Proof. intros A a b p; destruct p; reflexivity. Qed." (string 10)
     "Lemma uu_add_std : forall n m, uu_add n m = Nat.add n m." (string 10)
     "Proof. intros n m; induction n as [|n IHn]; [reflexivity|]." (string 10)
     "  change (uu_add (S n) m) with (S (uu_add n m)); rewrite IHn; reflexivity. Qed." (string 10)
     "Lemma uu_mul_std : forall n m, uu_mul n m = Nat.mul n m." (string 10)
     "Proof. intros n m; induction n as [|n IHn]; [reflexivity|]." (string 10)
     "  change (uu_mul (S n) m) with (uu_add (uu_mul n m) m);" (string 10)
     "  rewrite uu_add_std, IHn, Nat.add_comm; reflexivity. Qed." (string 10)
     "Theorem add_comm_std : forall n m, Nat.add n m = Nat.add m n." (string 10)
     "Proof. intros n m; rewrite <- (uu_add_std n m), <- (uu_add_std m n); apply paths_to_eq, uu_natpluscomm. Qed." (string 10)
     "Theorem add_assoc_std : forall n m k, Nat.add (Nat.add n m) k = Nat.add n (Nat.add m k)." (string 10)
     "Proof. intros n m k; rewrite <- !uu_add_std; apply paths_to_eq, uu_natplusassoc. Qed." (string 10)
     "Theorem mul_comm_std : forall n m, Nat.mul n m = Nat.mul m n." (string 10)
     "Proof. intros n m; rewrite <- (uu_mul_std n m), <- (uu_mul_std m n); apply paths_to_eq, uu_natmultcomm. Qed." (string 10)]))
   _ (dot (cat [(string 10) "Print Assumptions uu_natplusr0." (string 10)
"Print Assumptions uu_zcountins." (string 10)
"Print Assumptions uu_zinsane." (string 10)
"Print Assumptions uu_rtwinsrow." (string 10)
"Print Assumptions uu_commute_involutive." (string 10)
"Print Assumptions uu_invert_roundtrip." (string 10)
"Print Assumptions uu_commute_sound." (string 10)
"Print Assumptions uu_applall_swap." (string 10)
"Print Assumptions uu_hcommute_involutive." (string 10)
"Print Assumptions uu_hinv_roundtrip." (string 10)
"Print Assumptions uu_hcommute_sound." (string 10)
"Print Assumptions uu_hfold_swap." (string 10)
"Print Assumptions uu_etreeeta." (string 10)
"Print Assumptions uu_ekidsbeta." (string 10)
"Print Assumptions uu_ffindlay." (string 10)
"Print Assumptions uu_swal_involutive." (string 10)
"Print Assumptions uu_scell_comm." (string 10)
"Print Assumptions uu_splice_pad." (string 10)
"Print Assumptions uu_bubble_len." (string 10)
"Print Assumptions uu_mtch_sound." (string 10)
"Print Assumptions uu_reaim_involutive." (string 10)
"Print Assumptions uu_fid_swap." (string 10)
"Print Assumptions uu_repo_found." (string 10)
"Print Assumptions uu_repo_faithful." (string 10)
              "Print Assumptions add_comm_std." (string 10)
              "Print Assumptions add_assoc_std." (string 10)
              "Print Assumptions mul_comm_std." (string 10) (string 10)
              "(* " (show $keep) " exported / " (show (tally ents)) " corpus entries swept;" (string 10)
              "   3 headline laws (add_comm, add_assoc, mul_comm) landed on Coq's Nat.* via the bridge *)" (string 10)]))
   (quit 0))
