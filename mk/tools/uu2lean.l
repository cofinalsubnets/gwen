; uu2lean: love's internal proof corpus, exported to LEAN 4 -- the SECOND backend of
; the verification bridge (cf. mk/tools/uu2coq.l, the Rocq leg). Same uu.l, same
; dependency-closure machinery; a DIFFERENT kernel re-checks the SAME proof terms.
;
; This SWEEPS test/uu.l: loads uu's kernel (every defn type-checked against its
; stated type -- uu CERTIFIES them), reads the file as DATA, collects every
; (defn/defq nm T t), and translates the universe-safe MLTT fragment to Lean. `lean`
; re-checks the lot -- an INDEPENDENT THIRD WITNESS (uu, then Rocq, then Lean), so a
; law certified here and by uugen.v is agreed by two unrelated kernels (de Bruijn).
;
; A def exports iff in the handled fragment AND every global it mentions exports -- a
; DEPENDENCY CLOSURE (identical to uu2coq.l), so the univalence tower drops on its own
; (it bottoms at univalenceAxiom, an `axm` we never emit). Lean gives two things Coq
; needed flags for, FOR FREE: structure-eta (so idisweq exports with a plain `total2`
; structure) and universe polymorphism (`Type _` auto-binds). As in the Rocq leg,
; `paths` is the Type-valued identity type (Lean's `Eq` is Prop), so a path can be a
; Sigma carrier. Defs are `noncomputable` -- we CERTIFY the terms, never run them, so
; the code generator (which rejects bare recursors) is irrelevant.
;
; Run: out/host/love mk/tools/uu2lean.l > test/proof/lean/uugen.lean   then  lean test/proof/lean/uugen.lean

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
   _ (reads (open "test/uukindlaw.l" "r"))          ; load + CERTIFY uu's corpus (populates GLOB)
   bs (intern "\\")
   nl (string 10)
   b8 "⟨"  b9 "⟩"   ; Lean's anonymous-constructor angle brackets
   (unq x) (? (&& (two? x) (id? (cap x) bs)) (cap (cup x)) x)   ; strip one ' (reads shows quote as (\ ..))
   (lines l) (foldl (\ a s (a + s + nl)) "" l)   ; join a list of lines, newline after each
   (e2 l) (cap (cup l))  (e3 l) (cap (cup (cup l)))
   (e4 l) (cap (cup (cup (cup l))))  (e5 l) (cap (cup (cup (cup (cup l)))))

   ; read a file's top-level forms as DATA (the reader strips ; comments)

   ; a uu name -> a Lean identifier: prefix uu_, kebab '-' -> '_'
   (sanit s) (: (g i acc) (? (< i (tally s)) (g (+ 1 i) (+ acc (: c (s i) (? (= c 45) 95 c)))) acc) (g 0 ""))
   (leann nm) (+ "uu_" (sanit (string nm)))

   ; --- collect (kind nm T t) entries from the uu.l forms (defns live inside asserts) ---
   (deq? c) (&& (two? c) (| (id? (cap c) 'defn) (id? (cap c) 'defq)))
   (axq? c) (&& (two? c) (id? (cap c) 'axm))
   (entry c) (? (deq? c) (L 'def (unq (e2 c)) (unq (e3 c)) (unq (e4 c)))
                (axq? c) (L 'axm (unq (e2 c)) (unq (e3 c)) ()))
   (kids ks acc) (? (two? ks) (kids (cup ks) (? (| (deq? (cap ks)) (axq? (cap ks))) (link (entry (cap ks)) acc) acc)) acc)
   (gather fs acc) (? (two? fs)
                      (gather (cup fs) (: F (cap fs)
                        (? (&& (two? F) (id? (cap F) 'assert)) (kids (cup F) acc)
                           (| (deq? F) (axq? F)) (link (entry F) acc) acc)))
                      (rev acc))
   (app2 a b) (? (two? a) (link (cap a) (app2 (cup a) b)) b)   ; NOT `cat` -- the local cat is the STRING fold
   ents (gather (app2 (forms (open "test/uu.l" "r")) (app2 (forms (open "test/uuhom.l" "r")) (app2 (forms (open "test/uuhomgen.l" "r")) (app2 (forms (open "test/uuhomlaw.l" "r")) (app2 (forms (open "test/uusplgen.l" "r")) (app2 (forms (open "test/uuspllaw.l" "r")) (app2 (forms (open "test/uulay.l" "r")) (app2 (forms (open "test/uupatch.l" "r")) (app2 (forms (open "test/uuwm.l" "r")) (app2 (forms (open "test/uuwmlaw.l" "r")) (app2 (forms (open "test/uukind.l" "r")) (forms (open "test/uukindlaw.l" "r"))))))))))))) ())

   ; the SILENT-NO-OP gate (defense in depth behind uu.l's arity-door macros):
   ; a def gathered off the file must be WHOLE -- three operands; a paren slip
   ; that merges the type into the term reads () in the term seat -- and
   ; PINNED, since a partially applied defn/defq is a truthy closure that pins
   ; nothing and paints its assert dot green. redden here instead.
   _ (map (\ e (? (| (&& (id? (cap e) 'def) (! (e4 e)))
                     (! (peep GLOB (e2 e) 0)))
                  (: _ (puts "MALFORMED-OR-UNPINNED-DEF ") _ (puts (string (e2 e))) (quit 1))
                  0))
        ents)

   ; names that exist (corpus-wide) -- for telling a global reference from a bound var
   nt (tablet 0) _ (map (\ e (pin nt (e2 e) 1)) ents)
   (corpus? x) (peep nt x 0)

   ; explicit skips: the type-in-type detonations (sound MLTT can't host them, and
   ; lean's universe check rightly rejects them -- the filter, working as intended)
   bl (tablet 0)
   _ (map (\ n (pin bl n 1)) '(the_world_explodes but_seriously_the_world_explodes whoa irref))

   ; UU pinned to CONCRETE `Type` for these: a type-level nat_rect both declares
   ; and computes `Type _`, so the level is underdetermined, Lean generalizes it,
   ; and the universe strands at every downstream use (uu_zrow found it). ground
   ; the StackSet model's types at Type 0 and the whole tower's universes solve.
   mono (tablet 0)
   _ (map (\ n (pin mono n 1)) '(nvec nlist stk mstk patch tree ppair pvec plist cmside hunk hpair hvec hlist hcmside duo etree epatch spair scmside cell tmleaf tm svec slist sqside mvside store))

   ; transparent type-former/identity combinators: Lean checks a paths.rec minor premise
   ; at REDUCIBLE transparency and won't unfold a plain `def`, so an equivalence proof
   ; (idisweq: idfun T x must reduce to x) stalls. Mark these few `@[reducible]`.
   red (tablet 0)
   _ (map (\ n (pin red n 1)) '(idfun coconustot coconusfromt hfiber))

   ; backend-specific drop set (NOT soundness -- uu AND Rocq certify everything here). EMPTY:
   ; the two based-J equivalence roots Lean's elaborator can't reconstruct from the structural
   ; term (idisweq, iscontrcoconustot) are instead HAND-PROVEN below (the `manual` set), so the
   ; full 200 port -- matching the Rocq leg. Kept as the mechanism for any future holdout.
   np (tablet 0)
   _ (map (\ n (pin np n 1)) '())

   ; HAND-WRITTEN Lean proofs for the two based-J equivalence roots the structural
   ; translation can't reconstruct (see the note above). Tactic mode runs at full
   ; transparency, so `cases` does the path induction and idfun reduces -- where the
   ; emitted term stalled. Their ~17 dependents auto-port once these exist. The TYPE is
   ; still the auto-translated stated type; only the body is replaced.
   manual (tablet 0)
   _ (pin manual 'idisweq
       "  by intro T y; refine ⟨⟨y, @paths.idpath _ y⟩, ?_⟩; intro t; obtain ⟨x, p⟩ := t; cases p; exact @paths.idpath _ _")
   _ (pin manual 'iscontrcoconustot
       "  by intro T t; refine ⟨⟨t, @paths.idpath _ t⟩, ?_⟩; intro w; obtain ⟨x, p⟩ := w; cases p; exact @paths.idpath _ _")

   ; --- the translator: a uu term -> a Lean string ---
   ; every MLTT constructor is mapped; `un` is the safety net for any unhandled head
   ; (empty today -- the dependency closure + lean are the real filters).
   un (tablet 0)
   bases (tablet 0)
   _ (pin bases 'nat "Nat") _ (pin bases 'UU "(Type _)") _ (pin bases 'bool "Bool")
   ; unit rides at the CONCRETE universe (PUnit.{1} = Unit): lean >= 4.32 refuses to
   ; infer a polymorphic PUnit's level at a def's USE site (uu_sasq's Sum Nat PUnit
   ; auto-bound a level param, and every Sum.rec on its result stranded a metavar)
   _ (pin bases 'unit "PUnit.{1}") _ (pin bases 'empty "Empty")
   _ (pin bases 'true "true") _ (pin bases 'false "false") _ (pin bases 'tt "PUnit.unit")
   (atom-l t) (peep bases t (? (corpus? t) (leann t) (string t)))   ; base type / global / bound var

   (spine-l l) (? (two? l) ((u2l (cap l)) + (? (two? (cup l)) " " "") + (spine-l (cup l))) "")
   (u2l t)
   (? (charm? t) (show t)
      (nom? t) (atom-l t)
      (two? t) (: h (cap t)
        (? (id? h 'pi)        ("(forall " + (string (e2 t)) + " : " + (u2l (e3 t)) + ", " + (u2l (e4 t)) + ")")
           (id? h 'lam)       ("(fun " + (string (e2 t)) + " => " + (u2l (e3 t)) + ")")
           (id? h 'let)       ("(let " + (string (e2 t)) + " := " + (u2l (e3 t)) + "; " + (u2l (e4 t)) + ")")
           (id? h 'succ)      ("(Nat.succ " + (u2l (e2 t)) + ")")
           ; paths -- the Type-valued Id type (idpath's A and a are both implicit -> @ _ a)
           (id? h 'idpath)    ("(@paths.idpath _ " + (u2l (e2 t)) + ")")
           (id? h 'paths)     ("(@paths " + (u2l (e2 t)) + " " + (u2l (e3 t)) + " " + (u2l (e4 t)) + ")")
           (id? h 'nat_rect)  ("(@Nat.rec " + (u2l (e2 t)) + " " + (u2l (e3 t)) + " " + (u2l (e4 t)) + " " + (u2l (e5 t)) + ")")
           ; Bool.rec is false-case THEN true-case (Lean's ctor order); uu/Coq order is
           ; true-case THEN false-case -- so SWAP the two minor premises (e3<->e4)
           (id? h 'bool_rect) ("(@Bool.rec " + (u2l (e2 t)) + " " + (u2l (e4 t)) + " " + (u2l (e3 t)) + " " + (u2l (e5 t)) + ")")
           (id? h 'unit_rect) ("(@PUnit.rec " + (u2l (e2 t)) + " " + (u2l (e3 t)) + " " + (u2l (e4 t)) + ")")
           (id? h 'empty_rect)("(@Empty.rec " + (u2l (e2 t)) + " " + (u2l (e3 t)) + ")")
           ; Sigma -> a Lean structure (definitional eta is FREE -- idisweq exports plainly).
           ; total2 mirrors pi (e2 var, e3 domain, e4 body); tpair's family inferred.
           (id? h 'total2)    ("(total2 (fun " + (string (e2 t)) + " : " + (u2l (e3 t)) + " => " + (u2l (e4 t)) + "))")
           (id? h 'tpair)     ("(total2.tpair " + (u2l (e2 t)) + " " + (u2l (e3 t)) + ")")
           (id? h 'pr1)       ("(total2.pr1 " + (u2l (e2 t)) + ")")
           (id? h 'pr2)       ("(total2.pr2 " + (u2l (e2 t)) + ")")
           ; coprod -> Sum; ii1/ii2 -> Sum.inl/inr; the eliminator -> Sum.rec (A B as _)
           (id? h 'coprod)    ("(Sum " + (u2l (e2 t)) + " " + (u2l (e3 t)) + ")")
           (id? h 'ii1)       ("(Sum.inl " + (u2l (e2 t)) + ")")
           (id? h 'ii2)       ("(Sum.inr " + (u2l (e2 t)) + ")")
           (id? h 'coprod_rect)("(@Sum.rec _ _ " + (u2l (e2 t)) + " " + (u2l (e3 t)) + " " + (u2l (e4 t)) + " " + (u2l (e5 t)) + ")")
           ; (the T t) is a type ascription
           (id? h 'the)       ("(" + (u2l (e3 t)) + " : " + (u2l (e2 t)) + ")")
           ; full path induction (J): uu (paths_rect P pr e) -> paths.rec, based J (a fixed)
           (id? h 'paths_rect)("(paths.rec (motive := " + (u2l (e2 t)) + ") " + (u2l (e3 t)) + " " + (u2l (e4 t)) + ")")
           ("(" + (spine-l t) + ")")))                              ; an application spine
      (show t))

   ; --- translatability: every head handled (no unhandled constructor anywhere) ---
   (alltr? l) (? (two? l) (&& (tr? (cap l)) (alltr? (cup l))) 1)
   (tr? t) (? (charm? t) 1  (nom? t) 1
              (two? t) (? (peep un (cap t) 0) 0 (alltr? (cup t)))  1)

   ; --- the global names a term mentions (corpus names only), accumulated into a set ---
   (ment t acc) (? (nom? t) (? (corpus? t) (: _ (pin acc t 1) acc) acc)
                   (two? t) (ment-l t acc)  acc)
   (ment-l l acc) (? (two? l) (ment-l (cup l) (ment (cap l) acc)) acc)

   ; candidates: a def, in the fragment, not blacklisted, present in GLOB (uu-certified)
   (cand? e) (&& (id? (cap e) 'def) (! (peep bl (e2 e) 0)) (! (peep np (e2 e) 0)) (peep GLOB (e2 e) 0) (tr? (e3 e)) (tr? (e4 e)))
   cands (foldl (\ a e (? (cand? e) (link e a) a)) () ents)   ; reversed; restore order at emit
   ; each candidate's mentioned-names list (its global dependencies)
   (mlist e) (keys (ment-l (link (e3 e) (link (e4 e) ())) (tablet 0)))

   ; --- dependency closure: keep only defs all of whose mentions also survive ---
   (allin? l ex) (? (two? l) (&& (peep ex (cap l) 0) (allin? (cup l) ex)) 1)
   (pass cs ex) (foldl (\ a e (? (allin? (mlist e) ex) (: _ (pin a (e2 e) 1) a) a)) (tablet 0) cs)
   (fix ex) (: ex2 (pass cands ex) (? (= $ex2 $ex) ex2 (fix ex2)))
   ex0 (foldl (\ a e (: _ (pin a (e2 e) 1) a) a) (tablet 0) cands)   ; seed: all candidates
   keep (fix ex0)                                                   ; the closed exportable set

   ; --- emit, in source order ---
   (emit e) (: nm (e2 e) mb (peep manual nm 0)
     _ (pin bases 'UU (? (peep mono nm 0) "Type" "(Type _)"))
     out ((? (peep red nm 0) "@[reducible] " "") + "def " + (leann nm) + " : " + (u2l (e3 e)) + " :="
      + nl + (? mb mb ("  " + (u2l (e4 e)))) + nl)
     _ (pin bases 'UU "(Type _)")
     out)
   ; ⚠ foldr KEEPS ents' order, and lean wants every definition before its uses (foldl flips)
   exports (foldr (\ e a (? (peep keep (e2 e) 0) (link e a) a)) () ents)

   _ (. (lines `(
       "/- test/proof/lean/uugen.lean -- GENERATED from test/uu.l by mk/tools/uu2lean.l. Do not edit;"
       "   regenerate with `make test_uulean`. The SAME uu proof terms Rocq certifies"
       "   (test/proof/rocq/uugen.v) re-checked by a SECOND independent kernel, Lean 4 -- so each law"
       "   is agreed by two unrelated implementations (the de Bruijn criterion, diversified)."
       "   uu's kernel type-checked each term against its theorem (test/uu.l, green under"
       "   `make test`); this is the same term in Lean. Lean gives structure-eta and universe"
       "   polymorphism for free; `paths` is the Type-valued Id type (Lean's Eq is Prop) so a"
       "   path can be a Sigma carrier. Only the univalence tower (an axm) is skipped, on its"
       "   own via the dependency closure (counted at the foot). -/"
       "set_option linter.unusedVariables false"
       "set_option maxHeartbeats 4000000"
       "set_option maxRecDepth 300000  -- uu_app_tower reduces a unary-Nat Church tower to 262144"
       "universe u v"
       "/- uu's paths: the Type-valued identity type (UniMath's `paths`), NOT Prop's Eq. -/"
       "inductive paths {A : Type u} (a : A) : A -> Type u where"
       "  | idpath : paths a a"
       "/- Sigma as a structure -- Lean structures have definitional eta in the kernel. -/"
       "structure total2 {A : Type u} (B : A -> Type v) : Type (max u v) where"
       "  tpair ::"
       "  pr1 : A"
       "  pr2 : B pr1"
       "/- the bridge shim: a Type-valued path eliminates into Prop's = -/"
       "theorem paths_to_eq {A : Type u} {a b : A} (p : paths a b) : a = b := by cases p; rfl"
       "/- CERTIFY, never run: defs use bare recursors the code generator rejects. -/"
       "noncomputable section"
       "")))
   _ (map (\ e (. (emit e))) exports)
   _ (. (lines `(
       ""
       "end"
       "/- the de Bruijn witness: Lean agrees these carry NO axioms (closed under the"
       "   global context), the same property Rocq's `Print Assumptions` reports. -/"
       "#print axioms uu_natpluscomm"
"#print axioms uu_zcountins"
"#print axioms uu_zinsane"
"#print axioms uu_rtwinsrow"
"#print axioms uu_commute_involutive"
"#print axioms uu_invert_roundtrip"
"#print axioms uu_commute_sound"
"#print axioms uu_applall_swap"
"#print axioms uu_hcommute_involutive"
"#print axioms uu_hinv_roundtrip"
"#print axioms uu_hcommute_sound"
"#print axioms uu_hfold_swap"
"#print axioms uu_etreeeta"
"#print axioms uu_ekidsbeta"
"#print axioms uu_ffindlay"
"#print axioms uu_swal_involutive"
"#print axioms uu_scell_comm"
"#print axioms uu_splice_pad"
"#print axioms uu_bubble_len"
"#print axioms uu_mtch_sound"
"#print axioms uu_reaim_involutive"
"#print axioms uu_fid_swap"
"#print axioms uu_repo_found"
"#print axioms uu_repo_faithful"
       "#print axioms uu_natplusassoc"
       "#print axioms uu_natmultcomm"
       "#print axioms uu_total2_paths_f"
       "#print axioms uu_idisweq"
       "#print axioms uu_iscontrcoconustot"
       "")))
   _ (. ("/- " + (show $keep) + " exported / " + (show (tally ents)) + " corpus entries swept;" + nl
         + "   re-certified by Lean 4 -- a second kernel beside Rocq's uugen.v -/" + nl))
   (quit 0))
