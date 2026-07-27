(* proof/rocq/gc.v -- the generational MINOR is SOUND: under a complete write barrier,
   no live young object is lost. the minor's PAUSE has its shape: its work is
   bounded by the nursery alone, and tenured growth that keeps no young pointer
   leaves its survivor set identical (minor_work_bounded / minor_flat -- the
   theorems test/host/gcpause.l's gauge instance-checks). and the COPY LOOP has
   its shape: the Cheney drain terminates, copies each reachable object exactly
   once, exactly the reachable ones, and is a true fixpoint -- a second pass
   copies nothing (the drain_* theorems at the bottom; love.c's AI_GC_CHECK
   build instance-checks the fixpoint on every minor, gate test_gcheck).

   This is the Coq counterpart of doc/proto/gengc.l, the runnable ai model of the
   nursery+old collector. That model's load-bearing self-check is assert (3b): an
   old->young edge recorded WITHOUT the remembered-set entry lets a minor wrongly
   reap a live young object -- it reproduces, by construction, the exact bug a
   missing write barrier is. spec.l DEMONSTRATES; this file PROVES: the barrier is
   not just necessary on one example but sufficient in general -- if every old->young
   edge is remembered, the minor's nursery scan reaches every young object the
   mutator can reach (`barrier_sound`), so a young object the minor loses witnesses
   an INCOMPLETE barrier (`minor_loses_only_if_barrier_incomplete`, 3b's converse).

   The same split the rest of the tree uses: prove the MODEL here, keep the C
   connected by the differential oracle (the corpus runs byte-identical with the
   minor firing) and by gen_audit (0 unremembered edges across the corpus, the
   empirical form of rem_complete). A proof that love.c's pointer code REFINES this
   model is the larger separate effort doc/gengc.md flags.

   Universe-checked Rocq: the world does not explode here (cf. spec.v's preamble),
   so the theorems are unconditional.

   Written by Claude (Anthropic), the Opus 4.8 model. *)

From Stdlib Require Import List PeanoNat Bool Lia.
Import ListNotations.

(* ============================================================ *)
(* the model -- mirrors doc/proto/gengc.l                       *)
(* ============================================================ *)

(* a heap object is (addr . out-edges): its identity and the addresses it points
   at. a REGION (the nursery, the old space) is a list of such objects. *)
Definition addr := nat.
Definition region := list (addr * list addr).

(* the out-edges of address [a] in region [r] -- the first object carrying that
   addr (bump allocation never reuses one, but first-match is total regardless). *)
Fixpoint edges (a : addr) (r : region) : list addr :=
  match r with
  | [] => []
  | (b, es) :: t => if Nat.eqb a b then es else edges a t
  end.

(* membership: [a] names an object in [r]. [present] is the Prop, [presentb] the
   boolean the Seeds list filters with. *)
Definition present (a : addr) (r : region) : Prop := In a (map fst r).
Definition presentb (a : addr) (r : region) : bool := existsb (Nat.eqb a) (map fst r).

(* reachability WITHIN a region's graph from a seed set: a seed is reached; an
   edge out of a reached object reaches its target. (an edge to an absent object
   is a dead end -- its [edges] are []. matches the proto's reach1.) *)
Inductive Reach (r : region) (seeds : list addr) : addr -> Prop :=
| R_seed : forall a, In a seeds -> Reach r seeds a
| R_step : forall a b, Reach r seeds a -> In b (edges a r) -> Reach r seeds b.

(* the MINOR's seeds (the proto's `young-roots`): the roots that point into the
   nursery, plus the young addresses held by a REMEMBERED old object. everything
   young not so reached is dead and is reclaimed. *)
Definition young_root (nur : region) (roots : list addr) : list addr :=
  filter (fun a => presentb a nur) roots.
Definition rem_young (nur old : region) (rem : list addr) : list addr :=
  flat_map (fun oa => filter (fun y => presentb y nur) (edges oa old)) rem.
Definition Seeds (nur old : region) (rem roots : list addr) : list addr :=
  young_root nur roots ++ rem_young nur old rem.

(* the WRITE-BARRIER invariant: every old->young edge has its source remembered.
   (gen_audit checks exactly this on the real heap; here it is the hypothesis.) *)
Definition rem_complete (nur old : region) (rem : list addr) : Prop :=
  forall a y, present a old -> In y (edges a old) -> present y nur -> In a rem.

(* the two regions hold disjoint addresses -- generation is read from the address
   (ai carries no per-object age bit), so an address is young XOR old. *)
Definition disjoint (nur old : region) : Prop :=
  forall a, present a nur -> ~ present a old.

(* ============================================================ *)
(* lemmas on edges / membership                                 *)
(* ============================================================ *)

Lemma presentb_present : forall a r, presentb a r = true <-> present a r.
Proof.
  intros a r. unfold presentb, present. rewrite existsb_exists. split.
  - intros [x [Hin Heq]]. apply Nat.eqb_eq in Heq. subst x. exact Hin.
  - intros Hin. exists a. split; [exact Hin | apply Nat.eqb_refl].
Qed.

(* an absent address has no edges *)
Lemma edges_absent : forall a r, ~ present a r -> edges a r = [].
Proof.
  intros a r. induction r as [| [b es] t IH]; intros Hnp.
  - reflexivity.
  - simpl. destruct (Nat.eqb a b) eqn:Hab.
    + apply Nat.eqb_eq in Hab. subst b. exfalso. apply Hnp. left. reflexivity.
    + apply IH. intro Hp. apply Hnp. right. exact Hp.
Qed.

(* ... so an outgoing edge witnesses the source is present *)
Lemma edges_present : forall a b r, In b (edges a r) -> present a r.
Proof.
  intros a b r Hin.
  destruct (in_dec Nat.eq_dec a (map fst r)) as [Hp | Hnp].
  - exact Hp.
  - rewrite (edges_absent a r Hnp) in Hin. inversion Hin.
Qed.

(* edges read past an append: the first matching object wins, so a present source
   resolves in the left region, an absent one falls through to the right. *)
Lemma edges_app_left : forall a r1 r2, present a r1 -> edges a (r1 ++ r2) = edges a r1.
Proof.
  intros a r1 r2. induction r1 as [| [b es] t IH]; intros Hp.
  - inversion Hp.
  - simpl. destruct (Nat.eqb a b) eqn:Hab.
    + reflexivity.
    + apply IH. unfold present in *. simpl in Hp.
      destruct Hp as [Heq | Hp].
      * apply Nat.eqb_neq in Hab. subst b. exfalso. apply Hab. reflexivity.
      * exact Hp.
Qed.

Lemma edges_app_right : forall a r1 r2, ~ present a r1 -> edges a (r1 ++ r2) = edges a r2.
Proof.
  intros a r1 r2. induction r1 as [| [b es] t IH]; intros Hnp.
  - reflexivity.
  - simpl. destruct (Nat.eqb a b) eqn:Hab.
    + apply Nat.eqb_eq in Hab. subst b. exfalso. apply Hnp. left. reflexivity.
    + apply IH. intro Hp. apply Hnp. right. exact Hp.
Qed.

(* ============================================================ *)
(* the theorem: a complete barrier loses no live young object   *)
(* ============================================================ *)

(* Under [rem_complete], every YOUNG address the mutator can reach in the whole
   heap (nur ++ old) from the roots is reached by the MINOR's nursery scan from
   [Seeds]. The minor promotes exactly the young objects in that scan, so this says
   no live young object is collected -- the barrier is SUFFICIENT, not merely
   necessary-on-one-example (3b). *)
Theorem barrier_sound :
  forall nur old rem roots,
    disjoint nur old ->
    rem_complete nur old rem ->
    forall y, present y nur ->
      Reach (nur ++ old) roots y ->
      Reach nur (Seeds nur old rem roots) y.
Proof.
  intros nur old rem roots Hdisj Hrem.
  (* generalize over every reached address, conditioned on its being young *)
  assert (H : forall a, Reach (nur ++ old) roots a ->
                        present a nur -> Reach nur (Seeds nur old rem roots) a).
  { intros a HR. induction HR as [a Hin | a b HRa IHa Hedge]; intros Hpres.
    - (* a is a seed root, and young -> it is a young_root, hence a Seed *)
      apply R_seed. unfold Seeds. apply in_or_app. left.
      unfold young_root. apply filter_In. split.
      + exact Hin.
      + apply presentb_present. exact Hpres.
    - (* edge a -> b with b young. split on a's generation. *)
      destruct (in_dec Nat.eq_dec a (map fst nur)) as [Hanur | Hanur].
      + (* a young: the edge lives in the nursery graph; step there via the IH *)
        rewrite (edges_app_left a nur old Hanur) in Hedge.
        eapply R_step.
        * apply IHa. exact Hanur.
        * exact Hedge.
      + (* a old: a->b is an old->young edge, so the barrier remembered a *)
        rewrite (edges_app_right a nur old Hanur) in Hedge.
        assert (Haold : present a old) by (eapply edges_present; exact Hedge).
        assert (Hain : In a rem) by (apply (Hrem a b); assumption).
        apply R_seed. unfold Seeds. apply in_or_app. right.
        unfold rem_young. apply in_flat_map. exists a. split.
        * exact Hain.
        * apply filter_In. split; [exact Hedge | apply presentb_present; exact Hpres].
  }
  intros y Hpres HR. apply H; assumption.
Qed.

(* The contrapositive -- exactly the lesson of the proto's assert (3b): if the
   minor LOSES a live young object (it is young, the mutator can reach it, yet the
   nursery scan misses it), the remembered set MUST have been incomplete -- some
   old->young edge went unrecorded. A correct barrier is the only thing standing
   between the generational minor and silent heap corruption. *)
Corollary minor_loses_only_if_barrier_incomplete :
  forall nur old rem roots y,
    disjoint nur old ->
    present y nur ->
    Reach (nur ++ old) roots y ->
    ~ Reach nur (Seeds nur old rem roots) y ->
    ~ rem_complete nur old rem.
Proof.
  intros nur old rem roots y Hdisj Hpres HR Hlost Hrc.
  apply Hlost. apply barrier_sound; assumption.
Qed.

(* ============================================================ *)
(* the PAUSE bound -- the minor's work owes nothing to old      *)
(* ============================================================ *)

(* a copying collector's pause IS its copy volume, and the minor copies exactly
   its survivors: the young objects the nursery scan reaches from [Seeds]
   (barrier_sound says that scan covers everything live). test/host/gcpause.l
   MEASURES this shape -- gauge[14]/[15] carry the peak per-collection copy
   volume, and the worst minor sat bit-identical across 12x tenured growth --
   and the two theorems below are that shape in the model, so the gauge reads
   as an instance-check, not free-standing evidence. *)

Definition Promoted (nur : region) (seeds : list addr) (y : addr) : Prop :=
  present y nur /\ Reach nur seeds y.

(* (i) THE BOUND: however the survivors are enumerated (no address twice),
   there are at most as many as the nursery holds. the old space does not
   appear in the statement at all -- the minor's work is bounded by the
   nursery, whatever the tenured set grew to. *)
Theorem minor_work_bounded :
  forall nur seeds l,
    NoDup l ->
    (forall y, In y l -> Promoted nur seeds y) ->
    length l <= length nur.
Proof.
  intros nur seeds l Hnd Hall.
  rewrite <- (length_map fst nur).
  apply NoDup_incl_length; [exact Hnd |].
  intros y Hy. destruct (Hall y Hy) as [Hp _]. exact Hp.
Qed.

(* a filter that refuses every element answers [] *)
Lemma filter_none : forall (f : addr -> bool) es,
    (forall y, In y es -> f y = false) -> filter f es = [].
Proof.
  intros f es. induction es as [| e t IH]; intros H.
  - reflexivity.
  - simpl. rewrite (H e (or_introl eq_refl)).
    apply IH. intros y Hy. apply H. right. exact Hy.
Qed.

(* flat_map only reads its function on the list's own elements *)
Lemma flat_map_ext_in : forall (f g : addr -> list addr) l,
    (forall x, In x l -> f x = g x) -> flat_map f l = flat_map g l.
Proof.
  intros f g l. induction l as [| x t IH]; intros H.
  - reflexivity.
  - simpl. rewrite (H x (or_introl eq_refl)).
    rewrite (IH (fun y Hy => H y (or_intror Hy))). reflexivity.
Qed.

(* tenured growth that keeps no pointer into the nursery -- gcpause.l's levels
   exactly (a cask holds no pointers at all). an old object that DOES point
   young is the remembered set's business and is already in the seeds. *)
Definition tenure_blind (nur ext : region) : Prop :=
  forall a y, In y (edges a ext) -> ~ present y nur.

(* growing old by tenure-blind objects leaves the minor's seeds LITERALLY equal *)
Lemma seeds_tenure_blind :
  forall nur old ext rem roots,
    tenure_blind nur ext ->
    Seeds nur (old ++ ext) rem roots = Seeds nur old rem roots.
Proof.
  intros nur old ext rem roots Hblind.
  unfold Seeds. f_equal.
  unfold rem_young. apply flat_map_ext_in. intros oa _.
  destruct (in_dec Nat.eq_dec oa (map fst old)) as [Hp | Hnp].
  - rewrite (edges_app_left oa old ext Hp). reflexivity.
  - rewrite (edges_app_right oa old ext Hnp).
    rewrite (edges_absent oa old Hnp). simpl.
    apply filter_none. intros y Hy.
    destruct (presentb y nur) eqn:E; [| reflexivity].
    apply presentb_present in E. exfalso. exact (Hblind oa y Hy E).
Qed.

(* (ii) THE FLAT LINE: under tenure-blind growth the survivor set is THE SAME
   set -- same seeds, same scan, same copy volume, not merely a same-sized one.
   this is gcpause.l's assert made general: 12x the tenured set moved the worst
   minor by nothing, because every added word was tenure-blind. *)
Theorem minor_flat :
  forall nur old ext rem roots y,
    tenure_blind nur ext ->
    (Promoted nur (Seeds nur (old ++ ext) rem roots) y
     <-> Promoted nur (Seeds nur old rem roots) y).
Proof.
  intros nur old ext rem roots y Hblind.
  rewrite (seeds_tenure_blind nur old ext rem roots Hblind). tauto.
Qed.

(* ============================================================ *)
(* the COPY LOOP -- the drain is a fixpoint that loses nothing  *)
(* ============================================================ *)

(* gen_minor's drain,
       while (cp < major_hp) evac_*(g);
   abstracted: a Cheney worklist. [cp, major_hp) is the GRAY window (copied,
   not yet scanned), everything before cp is BLACK (scanned), and gcp's
   forwarding test -- "the first word already points into the forwarding
   window" -- is exactly "already copied". the model: black and gray are
   address lists; one step scans the first gray object, copying (GRAYING)
   each present, not-yet-copied edge target at the moment the edge is read
   -- gcp, in the model. fuel-bounded with None on fuel-out, so termination
   is a THEOREM (S (length r) fuel always drains), never a wish. the four
   shapes proved below:
     drain_terminates            -- the scan pointer catches the allocation
                                    pointer: the while loop exits.
     drain_copies_once           -- NoDup: the forwarding pointer's whole
                                    job, no object lands in to-space twice.
     drain_sound / drain_complete -- the survivors are EXACTLY the reachable
                                    present objects: nothing dead copied,
                                    nothing live lost.
     drain_second_pass_copies_nothing -- re-running the scan over the result
                                    copies not one word. this is the theorem
                                    love.c's AI_GC_CHECK build instance-checks:
                                    gen_minor re-runs its whole scan after the
                                    drain and traps if major_hp moved. *)

Definition inb (b : addr) (l : list addr) : bool := existsb (Nat.eqb b) l.

Lemma inb_in : forall b l, inb b l = true <-> In b l.
Proof.
  intros b l. unfold inb. rewrite existsb_exists. split.
  - intros [x [Hx He]]. apply Nat.eqb_eq in He. subst. exact Hx.
  - intro H. exists b. split; [exact H | apply Nat.eqb_refl].
Qed.

(* gcp over one object's out-edges, in order: copy (gray) a target iff it
   names a from-space object not yet copied -- and the moment it is copied it
   counts as seen for the NEXT edge (the forwarding pointer is installed
   before the scan reads on). *)
Fixpoint graying (r : region) (seen es : list addr) : list addr :=
  match es with
  | [] => []
  | b :: t => if presentb b r && negb (inb b seen)
              then b :: graying r (b :: seen) t
              else graying r seen t
  end.

Lemma graying_sub : forall r es seen b, In b (graying r seen es) -> In b es.
Proof.
  intros r es. induction es as [| e t IH]; intros seen b H; simpl in H.
  - inversion H.
  - destruct (presentb e r && negb (inb e seen)).
    + destruct H as [-> | H]; [left; reflexivity | right; eapply IH; exact H].
    + right. eapply IH. exact H.
Qed.

Lemma graying_present : forall r es seen b, In b (graying r seen es) -> present b r.
Proof.
  intros r es. induction es as [| e t IH]; intros seen b H; simpl in H.
  - inversion H.
  - destruct (presentb e r && negb (inb e seen)) eqn:E.
    + apply andb_prop in E as [Hp _]. destruct H as [-> | H].
      * apply presentb_present. exact Hp.
      * eapply IH. exact H.
    + eapply IH. exact H.
Qed.

Lemma graying_new : forall r es seen b, In b (graying r seen es) -> ~ In b seen.
Proof.
  intros r es. induction es as [| e t IH]; intros seen b H; simpl in H.
  - inversion H.
  - destruct (presentb e r && negb (inb e seen)) eqn:E.
    + apply andb_prop in E as [_ Hn]. destruct H as [-> | H].
      * intro Hin. apply negb_true_iff in Hn.
        apply inb_in in Hin. rewrite Hin in Hn. discriminate.
      * intro Hin. apply (IH (e :: seen) b H). right. exact Hin.
    + eapply IH. exact H.
Qed.

Lemma graying_nodup : forall r es seen, NoDup (graying r seen es).
Proof.
  intros r es. induction es as [| e t IH]; intros seen; simpl.
  - constructor.
  - destruct (presentb e r && negb (inb e seen)) eqn:E.
    + constructor.
      * intro Hin. apply graying_new in Hin. apply Hin. left. reflexivity.
      * apply IH.
    + apply IH.
Qed.

(* a present edge target is copied now or was copied before -- never dropped *)
Lemma graying_covers : forall r es seen b,
    In b es -> present b r -> In b seen \/ In b (graying r seen es).
Proof.
  intros r es. induction es as [| e t IH]; intros seen b Hin Hp; simpl.
  - inversion Hin.
  - destruct (presentb e r && negb (inb e seen)) eqn:E.
    + destruct Hin as [-> | Hin]; [right; left; reflexivity |].
      destruct (IH (e :: seen) b Hin Hp) as [[-> | Hs] | Hg].
      * right. left. reflexivity.
      * left. exact Hs.
      * right. right. exact Hg.
    + destruct Hin as [-> | Hin].
      * left. apply presentb_present in Hp. rewrite Hp in E. simpl in E.
        apply negb_false_iff in E. apply inb_in. exact E.
      * exact (IH seen b Hin Hp).
Qed.

(* if every edge target is dead or already copied, gcp copies nothing *)
Lemma graying_none : forall r es seen,
    (forall b, In b es -> presentb b r = false \/ In b seen) ->
    graying r seen es = [].
Proof.
  intros r es. induction es as [| e t IH]; intros seen H; simpl.
  - reflexivity.
  - destruct (H e (or_introl eq_refl)) as [Hp | Hs].
    + rewrite Hp. simpl. apply IH. intros b Hb. destruct (H b (or_intror Hb)); auto.
    + assert (Hi : inb e seen = true) by (apply inb_in; exact Hs).
      rewrite Hi. rewrite andb_false_r. apply IH.
      intros b Hb. destruct (H b (or_intror Hb)); auto.
Qed.

(* the worklist. gray first: an empty worklist is DONE whatever the fuel --
   fuel-out (None) can only mean a genuinely unfinished scan. *)
Fixpoint cheney (r : region) (fuel : nat) (black gray : list addr) : option (list addr) :=
  match gray with
  | [] => Some black
  | a :: gs =>
    match fuel with
    | 0 => None
    | S f => cheney r f (black ++ [a]) (gs ++ graying r (black ++ a :: gs) (edges a r))
    end
  end.

(* the drain: gen_minor's whole scan, from the copied roots *)
Definition drain (r : region) (roots : list addr) : option (list addr) :=
  cheney r (S (length r)) [] (graying r [] roots).

Lemma NoDup_app_disjoint : forall (l m : list addr),
    NoDup l -> NoDup m -> (forall x, In x m -> ~ In x l) -> NoDup (l ++ m).
Proof.
  induction l as [| a l IH]; intros m Hl Hm Hd; simpl.
  - exact Hm.
  - inversion Hl as [| ? ? Hnin Hl']; subst. constructor.
    + intro Hin. apply in_app_or in Hin. destruct Hin as [Hin | Hin].
      * exact (Hnin Hin).
      * apply (Hd a Hin). left. reflexivity.
    + apply IH; [exact Hl' | exact Hm |].
      intros x Hx Hin. apply (Hd x Hx). right. exact Hin.
Qed.

(* one step keeps the copied set duplicate-free: what graying adds is fresh *)
Lemma step_nodup : forall r black a gs,
    NoDup (black ++ a :: gs) ->
    NoDup ((black ++ a :: gs) ++ graying r (black ++ a :: gs) (edges a r)).
Proof.
  intros r black a gs Hnd.
  apply NoDup_app_disjoint; [exact Hnd | apply graying_nodup |].
  intros x Hx. exact (graying_new _ _ _ _ Hx).
Qed.

(* the recursion's state, reassociated to the shape step_nodup speaks *)
Lemma step_shape : forall (black : list addr) a gs G,
    (black ++ [a]) ++ (gs ++ G) = (black ++ a :: gs) ++ G.
Proof.
  intros. rewrite <- !app_assoc. simpl. reflexivity.
Qed.

(* the master invariant, one induction for every conclusion. P is any
   property of addresses that holds on the initial copied set and follows
   edges -- instantiated with (fun _ => True) for the counting facts and
   with (Reach r roots) for soundness. *)
Lemma cheney_inv : forall (r : region) (P : addr -> Prop) fuel black gray res,
    cheney r fuel black gray = Some res ->
    NoDup (black ++ gray) ->
    (forall b, In b black -> forall y, In y (edges b r) -> present y r -> In y (black ++ gray)) ->
    (forall b, In b (black ++ gray) -> P b) ->
    (forall a y, P a -> In y (edges a r) -> P y) ->
    NoDup res /\ incl (black ++ gray) res
    /\ (forall b, In b res -> forall y, In y (edges b r) -> present y r -> In y res)
    /\ (forall b, In b res -> P b).
Proof.
  intros r P fuel. induction fuel as [| f IH]; intros black gray res Hc Hnd Hcl HP Hstep.
  - destruct gray as [| a gs]; [| discriminate Hc].
    injection Hc as <-. rewrite app_nil_r in *.
    repeat split; auto. apply incl_refl.
  - destruct gray as [| a gs].
    + injection Hc as <-. rewrite app_nil_r in *.
      repeat split; auto. apply incl_refl.
    + simpl in Hc.
      set (G := graying r (black ++ a :: gs) (edges a r)) in *.
      assert (Hnd' : NoDup ((black ++ [a]) ++ (gs ++ G))).
      { rewrite step_shape. apply step_nodup. exact Hnd. }
      assert (Hcl' : forall b, In b (black ++ [a]) ->
                forall y, In y (edges b r) -> present y r -> In y ((black ++ [a]) ++ (gs ++ G))).
      { intros b Hb y Hy Hp. rewrite step_shape.
        apply in_app_or in Hb. destruct Hb as [Hb | [Heq | []]].
        - apply in_or_app. left. apply Hcl with (b := b); assumption.
        - subst b.
          destruct (graying_covers r (edges a r) (black ++ a :: gs) y Hy Hp) as [Hs | Hg].
          + apply in_or_app. left. exact Hs.
          + apply in_or_app. right. exact Hg. }
      assert (HP' : forall b, In b ((black ++ [a]) ++ (gs ++ G)) -> P b).
      { intros b Hb. rewrite step_shape in Hb.
        apply in_app_or in Hb. destruct Hb as [Hb | Hb].
        - apply HP. exact Hb.
        - apply graying_sub in Hb. apply Hstep with (a := a); [| exact Hb].
          apply HP. apply in_or_app. right. left. reflexivity. }
      destruct (IH _ _ _ Hc Hnd' Hcl' HP' Hstep) as [Hr1 [Hr2 [Hr3 Hr4]]].
      repeat split; auto.
      intros x Hx. apply Hr2. rewrite step_shape.
      apply in_or_app. left. exact Hx.
Qed.

(* every copied address names a from-space object, so NoDup bounds the copies
   by the from-space -- which is what lets the fuel argument close *)
Lemma cheney_enough : forall r fuel black gray,
    NoDup (black ++ gray) ->
    incl (black ++ gray) (map fst r) ->
    length r < fuel + length black ->
    exists res, cheney r fuel black gray = Some res.
Proof.
  intros r fuel. induction fuel as [| f IH]; intros black gray Hnd Hincl Hlen.
  - destruct gray as [| a gs].
    + exists black. reflexivity.
    + exfalso.
      assert (Hle : length (black ++ a :: gs) <= length (map fst r))
        by (apply NoDup_incl_length; assumption).
      rewrite length_app, length_map in Hle. simpl in Hle. lia.
  - destruct gray as [| a gs].
    + exists black. reflexivity.
    + simpl.
      set (G := graying r (black ++ a :: gs) (edges a r)).
      apply IH.
      * rewrite step_shape. apply step_nodup. exact Hnd.
      * intros x Hx. rewrite step_shape in Hx. apply in_app_or in Hx.
        destruct Hx as [Hx | Hx]; [apply Hincl; exact Hx |].
        apply graying_present in Hx. exact Hx.
      * rewrite length_app. simpl length. lia.
Qed.

(* (i) TERMINATION: the scan pointer catches the allocation pointer -- the
   while loop exits, with fuel one more than the from-space could ever need *)
Theorem drain_terminates : forall r roots, exists res, drain r roots = Some res.
Proof.
  intros r roots. unfold drain. apply cheney_enough; simpl.
  - apply graying_nodup.
  - intros x Hx. apply graying_present in Hx. exact Hx.
  - rewrite Nat.add_0_r. apply Nat.lt_succ_diag_r.
Qed.

(* (ii) NO DOUBLE COPY: the forwarding pointer's whole job -- no from-space
   object lands in to-space twice, so the survivors are each copied ONCE *)
Theorem drain_copies_once : forall r roots res,
    drain r roots = Some res -> NoDup res.
Proof.
  intros r roots res Hd.
  destruct (cheney_inv r (fun _ => True) _ _ _ _ Hd) as [Hnd _]; simpl; auto.
  - apply graying_nodup.
  - intros b Hb. inversion Hb.
Qed.

(* (iii) NOTHING DEAD COPIED: every survivor is reachable from the roots *)
Theorem drain_sound : forall r roots res y,
    drain r roots = Some res -> In y res -> Reach r roots y.
Proof.
  intros r roots res y Hd Hy.
  destruct (cheney_inv r (Reach r roots) _ _ _ _ Hd) as [_ [_ [_ HP]]]; simpl; auto.
  - apply graying_nodup.
  - intros b Hb. inversion Hb.
  - intros b Hb. apply R_seed. eapply graying_sub. exact Hb.
  - intros a b Ha Hb. eapply R_step; eauto.
Qed.

(* (iv) NOTHING LIVE LOST: every reachable from-space object is a survivor *)
Theorem drain_complete : forall r roots res y,
    drain r roots = Some res -> Reach r roots y -> present y r -> In y res.
Proof.
  intros r roots res y Hd HR Hp.
  destruct (cheney_inv r (fun _ => True) _ _ _ _ Hd) as [_ [Hincl [Hcl _]]]; simpl; auto;
    [apply graying_nodup | intros b Hb; inversion Hb |].
  induction HR as [y Hin | a y HRa IHa Hedge].
  - apply Hincl.
    destruct (graying_covers r roots [] y Hin Hp) as [Hs | Hg]; [inversion Hs | exact Hg].
  - assert (Hpa : present a r) by (eapply edges_present; exact Hedge).
    exact (Hcl a (IHa Hpa) y Hedge Hp).
Qed.

(* (v) THE FIXPOINT IS A FIXPOINT: scanning any survivor's edges against the
   final copied set grays nothing -- a second pass over the drained heap
   copies not one word. love.c's AI_GC_CHECK build runs this very check on
   every minor: re-drive the whole scan, trap if major_hp moved. *)
Theorem drain_second_pass_copies_nothing : forall r roots res a,
    drain r roots = Some res -> In a res ->
    graying r res (edges a r) = [].
Proof.
  intros r roots res a Hd Ha.
  destruct (cheney_inv r (fun _ => True) _ _ _ _ Hd) as [_ [_ [Hcl _]]]; simpl; auto;
    [apply graying_nodup | intros b Hb; inversion Hb |].
  apply graying_none. intros b Hb.
  destruct (presentb b r) eqn:E; [right | left; reflexivity].
  apply Hcl with (b := a); [exact Ha | exact Hb | apply presentb_present; exact E].
Qed.

(* axiom-free, like the rest of proof/rocq/: every result is closed under the global
   context (no Axiom, no Admitted, no classical/funext escape hatch). *)
Print Assumptions barrier_sound.
Print Assumptions minor_loses_only_if_barrier_incomplete.
Print Assumptions minor_work_bounded.
Print Assumptions minor_flat.
Print Assumptions drain_terminates.
Print Assumptions drain_copies_once.
Print Assumptions drain_sound.
Print Assumptions drain_complete.
Print Assumptions drain_second_pass_copies_nothing.
