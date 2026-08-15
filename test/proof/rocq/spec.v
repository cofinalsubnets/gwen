(* proof/spec.v -- ai's headline laws, machine-checked in Rocq.

   The executable spec is test/spec.l: every claim assert-backed, on every
   target, green under `make test`. That DEMONSTRATES the laws (it exhibits
   them at runtime). This file is the translation "back to coq" that upgrades
   demonstrate toward PROVE -- the same laws as theorems in a consistent
   metatheory.

   Scope: the pure combinator / numeral / absence core -- spec.l's first
   section ("everything is a function"). The numeric tower (float, complex,
   bignum) and the honest transcendental misses (e^(i*pi) misses -1 by ~1e-16
   ON PURPOSE) are deliberately out of this first slice.

   "up to explosion of world": the uu kernel (test/uu.l) runs type-in-type
   (UniMath's `Unset Universe Checking`), so there `empty` is inhabited
   -- `but_seriously_the_world_explodes : empty` (uu.l:1355) -- and ex falso
   gives every value. This file lives in UNIVERSE-CHECKED Rocq: the world does
   not explode here, so the theorems below are unconditional. The caveat is
   honest about the only escape hatch any formal demonstration has -- the
   consistency of its own metatheory.

   Written by Claude (Anthropic), the Opus 4.8 model. *)

From Stdlib Require Import PeanoNat List.
Import ListNotations.
(* the numeral tower (262144, 65536, 3^27, ..) are big nat literals; Coq parses
   them via of_num_uint to AVOID a parse-time stack overflow, then warns it did so.
   that warning is noise -- the safe thing shouldn't nag. silence it. *)
Set Warnings "-abstract-large-number".

(* ============================================================ *)
(* the numeral lane: (n x) = x ** n                              *)
(* ============================================================ *)

(* A numeral is an operator. Applied to a value it raises that value to the
   n: a single law from which 0 is const-1 (x^0 = 1), 1 is the identity
   (x^1 = x), and the whole tower fall out. *)

Definition app (n x : nat) : nat := Nat.pow x n.

(* (0 x) = 1 -- 0 is const-1 *)
Theorem const_one : forall x, app 0 x = 1.
Proof. intro x. apply Nat.pow_0_r. Qed.

(* (1 x) = x -- 1 is the identity *)
Theorem identity : forall x, app 1 x = x.
Proof. intro x. apply Nat.pow_1_r. Qed.

(* (0 0 x) = x -- const of const is the identity: ((0 0) x) = (1 x) = x *)
Theorem const_of_const : forall x, app (app 0 0) x = x.
Proof. intro x. unfold app. rewrite Nat.pow_0_r. apply Nat.pow_1_r. Qed.

(* (3 2) = 8 -- (n x) = x ** n *)
Theorem pow_3_2 : app 3 2 = 8.
Proof. reflexivity. Qed.

(* (2 3 4) = 262144 -- the tower 4 ** (3 ** 2), left-associated *)
Theorem tower : app (app 2 3) 4 = 262144.
Proof. reflexivity. Qed.

(* (2 2 2 2) = 65536 -- tetration *)
Theorem tetration : app (app (app 2 2) 2) 2 = 65536.
Proof. reflexivity. Qed.

(* ============================================================ *)
(* the function lane: (n f) iterates f n times                  *)
(* ============================================================ *)

(* The SAME church numeral, dispatched on the KIND of its argument. Applied
   to a function it composes: (n f) = f o f o ... o f, n times. `Nat.iter`
   is exactly this. (n x) = x ** n above is the instance where the argument
   is itself a numeral -- so x^n is not the whole story, only the
   numeral-on-numeral face of one operation. *)

Definition appf {A} (n : nat) (f : A -> A) : A -> A := fun x => Nat.iter n f x.

(* (2 f x) = f (f x) -- church two *)
Theorem church_two : forall (A:Type) (f:A->A) (x:A), appf 2 f x = f (f x).
Proof. reflexivity. Qed.

(* (3 (+ 1) 9) = 12 -- iterate the successor three times *)
Theorem inc_three : appf 3 S 9 = 12.
Proof. reflexivity. Qed.

(* The two lanes agree where they meet: iterating multiply-by-b, b times
   on 1, is b ** n -- the numeral-on-numeral identity made explicit. *)
Theorem lanes_agree : forall n x, app n x = appf n (Nat.mul x) 1.
Proof.
  intros n x. unfold app, appf.
  induction n as [|n IH]; [reflexivity|].
  simpl. rewrite IH. reflexivity.
Qed.

(* ============================================================ *)
(* currying and the empty form                                  *)
(* ============================================================ *)

(* Application is left-associative -- f x y = (f x) y -- and with no
   operands (f) = f, the empty-form law. A spine makes both definitional. *)

Fixpoint spine (f : nat) (xs : list nat) : nat :=
  match xs with
  | []       => f
  | x :: xs' => spine (app f x) xs'
  end.

(* (f) = f -- the empty form reads its head *)
Theorem empty_form : forall f, spine f [] = f.
Proof. reflexivity. Qed.

(* f x y = (f x) y -- currying is left-to-right application *)
Theorem curry : forall f x y, spine f [x; y] = app (app f x) y.
Proof. reflexivity. Qed.

(* ============================================================ *)
(* the zero point as a unit: it absorbs application both ways    *)
(* ============================================================ *)

(* A name not in the book reads as the zero point: a nameless unit, `Pt`.
   The binary shows it absorbs application two ways:
     (Pt x)        = 1     -- as operator it is const-1 (like 0)
     (numeric Pt)  = Pt    -- as a numeral's base it absorbs: Pt ** n = Pt
   vapp is TOTAL: a value for every pair -- no stuck state, no undefined
   behavior, up to explosion of world. *)

Inductive value := Num (n : nat) | Cx | Pt.   (* Cx stands in for i: numeric, not a nat numeral *)

Definition vapp (f a : value) : value :=
  match f, a with
  | Pt,    _      => Num 1                 (* the unit as operator: const-1 *)
  | Num _, Pt     => Pt                    (* the unit absorbs as a numeral's base *)
  | Cx,    Pt     => Pt                    (* ... i included *)
  | Num n, Num x  => Num (Nat.pow x n)     (* (n x) = x ** n, agreeing with `app` *)
  | Cx,    Num _  => Cx                    (* i applied to a numeral stays in the complex lane *)
  | Cx,    Cx     => Cx
  | Num _, Cx     => Cx
  end.

(* the unit as operator is const-1, like 0 *)
Theorem unit_is_const_one : forall a, vapp Pt a = Num 1.
Proof. reflexivity. Qed.

(* the nothing surface: () (the zero-point Pt) is DISTINCT from the number 0 (Num 0) --
   a mask of nothing is a FACE, not the bare value. Both are nothing by net (the V model
   below: Vnil and Vnum 0 both net 0); they are distinct VALUES wearing different masks. *)
Theorem unit_neq_zero : Pt <> Num 0.
Proof. discriminate. Qed.

(* ============================================================ *)
(* totality: no undefined behavior, up to explosion of world    *)
(* ============================================================ *)

(* vapp is a total function: defined on EVERY pair by an exhaustive match
   (Rocq checks the exhaustiveness). There is no error constructor and no
   stuck state -- "no undefined behavior" by construction. The model covers
   the absence/numeral fragment; full no-UB over the whole tower is the
   larger program this file seeds. *)
Theorem total : forall f a, exists v, vapp f a = v.
Proof. intros f a. exists (vapp f a). reflexivity. Qed.

(* ============================================================ *)
(* true and false: the net measure and its saturation          *)
(* ============================================================ *)

(* false is NOTHING: a value is false iff its net measure is <= 0. The net is
   a complex-valued content measure; this slice models the REAL fragment
   (net : V -> Z): a number is its own value, () is 0, and a product sums its
   SPINE -- the car always counts, the cdr continues the spine unless it is a
   dotted-tail atom, which is "not an element". The complex extension (! and $
   both read the REAL part alone; the magnitude is abs's business) needs
   reals and is the next slice; everything here is exact over the integer net. *)

From Stdlib Require Import ZArith Lia Permutation.
Open Scope Z_scope.

Inductive V := Vnum (z : Z) | Vnil | Vcons (a b : V).

Fixpoint net (v : V) : Z :=
  match v with
  | Vnum z              => z
  | Vnil                => 0
  | Vcons a Vnil        => net a            (* end of a proper list *)
  | Vcons a (Vnum _)    => net a            (* a DOTTED tail is not a spine element *)
  | Vcons a (Vcons _ _ as b) => net a + net b   (* the car counts; continue the spine *)
  end.

(* $ (sat): one saturating clamp, max(0, ceil net); integer net => ceil is id. *)
Definition sat (v : V) : Z := Z.max 0 (net v).
(* ! (nilp): false is nothing -- net <= 0. *)
Definition nilp (v : V) : bool := net v <=? 0.
(* !! : the truth bit -- positive green, net > 0. *)
Definition tru (v : V) : bool := 0 <? net v.

(* THE INVARIANT  !x == (0 = $x)  -- spec.l: (!"" = 0 = $"") *)
Theorem nilp_iff_sat0 : forall v, nilp v = (sat v =? 0).
Proof.
  intro v. unfold nilp, sat. destruct (net v <=? 0) eqn:H.
  - apply Z.leb_le in H. symmetry. apply Z.eqb_eq. lia.
  - apply Z.leb_gt in H. symmetry. apply Z.eqb_neq. lia.
Qed.

(* the saturation law: $ is nonnegative; it keeps the positive and clamps the rest *)
Theorem sat_nonneg : forall v, 0 <= sat v.                Proof. intro v. unfold sat. lia. Qed.
Theorem sat_keeps   : forall v, 0 < net v -> sat v = net v. Proof. intros v H. unfold sat. lia. Qed.

(* the nothing surface, by net: every mask of nothing nets 0 (green/false). The empty chain
   () (Vnil) and the number 0 (Vnum 0) are DISTINCT values yet both nilp -- masks of one
   nothing, not the same value. (spec.l: !() !0, !(= () 0).) *)
Theorem nil_nothing  : nilp Vnil = true.        Proof. reflexivity. Qed.
Theorem zero_nothing : nilp (Vnum 0) = true.    Proof. reflexivity. Qed.
Theorem nil_neq_zero : Vnil <> Vnum 0.          Proof. discriminate. Qed.
Theorem sat_clamps  : forall v, net v <= 0 -> sat v = 0.    Proof. intros v H. unfold sat. lia. Qed.

(* the COLORS, by the SIGN of the net (its real part -- not the total order, which
   parts from truth on the imaginary axis): GREEN nonneg (the kept band, what $ keeps;
   positive green = true), RED neg (below the floor, $ clamps it up), BLUE the floor
   (net 0). green and blue are DUAL, not disjoint: 0 is green by sign AND blue by measure
   -- the overlap where green meets nothing. (Reverts the b601b93b frequency flip.) *)
Definition green (v : V) := 0 <= net v.
Definition red   (v : V) := net v < 0.
Definition blue  (v : V) := net v = 0.

Theorem blue_is_green     : forall v, blue v -> green v.        Proof. unfold blue, green. lia. Qed.
Theorem green_or_red      : forall v, green v \/ red v.         Proof. intro v. unfold green, red. lia. Qed.
Theorem green_red_disjoint : forall v, ~ (green v /\ red v).    Proof. intro v. unfold green, red. lia. Qed.
(* truth is POSITIVE green: above the blue floor *)
Theorem truth_is_positive_green : forall v, tru v = true <-> (green v /\ net v <> 0).
Proof. intro v. unfold tru, green. rewrite Z.ltb_lt. lia. Qed.

(* the spine measure: a proper list nets the sum of its elements' nets *)
Definition vlist (xs : list V) : V := fold_right Vcons Vnil xs.
Fixpoint netl (xs : list V) : Z := match xs with nil => 0 | x :: xs => net x + netl xs end.

Lemma net_vlist : forall xs, net (vlist xs) = netl xs.
Proof.
  induction xs as [|x xs IH]; [reflexivity|].
  destruct xs as [|y ys].
  - simpl. lia.
  - change (vlist (x :: y :: ys)) with (Vcons x (vlist (y :: ys))).
    change (net (Vcons x (vlist (y :: ys)))) with (net x + net (vlist (y :: ys))).
    rewrite IH. reflexivity.
Qed.

(* + IS THE MEASURE HOMOMORPHISM: net distributes over append *)
Theorem netl_app : forall xs ys, netl (xs ++ ys) = netl xs + netl ys.
Proof. induction xs as [|x xs IH]; intro ys; simpl; [reflexivity | rewrite IH; lia]. Qed.

Theorem net_homomorphism : forall xs ys,
  net (vlist (xs ++ ys)) = net (vlist xs) + net (vlist ys).
Proof. intros xs ys. rewrite !net_vlist. apply netl_app. Qed.

(* the concrete corpus computations *)
Theorem sat_123    : sat (vlist [Vnum 1; Vnum 2; Vnum 3]) = 6.  Proof. reflexivity. Qed. (* $'(1 2 3) *)
Theorem sat_dotted : sat (Vcons (Vnum 1) (Vnum 2)) = 1.          Proof. reflexivity. Qed. (* $(cons 1 2) -- tail dropped *)
Theorem nil_dotted : nilp (Vcons (Vnum 0) (Vnum 2)) = true.      Proof. reflexivity. Qed. (* !(cons 0 2) *)
Theorem nothings   : nilp (vlist [Vnil; Vnil]) = true.           Proof. reflexivity. Qed. (* a product of nothings is nothing *)
Theorem net_red    : net (vlist [Vnum (-2); Vnum 1]) = -1.       Proof. reflexivity. Qed. (* +'(-2 1) -- the net is unclamped *)
Theorem red_red    : red (vlist [Vnum (-2); Vnum 1]).            Proof. reflexivity. Qed. (* ... and red *)
Theorem blue_zero  : blue (Vnum 0).                              Proof. reflexivity. Qed.

(* ============================================================ *)
(* order & equality: < is a TOTAL order over all values         *)
(* ============================================================ *)

(* The total order flattens the type lattice into BANDS, low to high (TRUE-BLUE:
   the symbol band -- mints AND noms -- is the floor, below string and number):
     symbol < string < number < product < map < top
   and orders within each band by value / lexicographically. This slice
   models the band structure with one comparable key per band (the within-band
   lex order for text and products collapsed to that key -- the lexicographic
   refinement is the next slice). The point proved here is the marquee claim:
   the relation is a genuine TOTAL ORDER (irreflexive + transitive + trichotomy
   for <, reflexive + antisymmetric + transitive + total for <=), and `=` is
   its Eq cell -- propositional equality, a linear order not a preorder.

   `=` bridges the numeric tower (3 = 3.0): a number band carries the
   mathematical value rep-blindly, so the float 3.0 and the fixnum 3 are the
   SAME Onum -- there is no separate rep to model. And `idp` (pointer identity)
   is FINER than `=` on heap values: '(1) = '(1) yet !(idp '(1) '(1)). Identity
   has no model here, where nothing is shared -- a deliberate omission. *)

Inductive O :=
  | Onum (z : Z) | Ostr (z : Z) | Osym (z : Z)
  | Oprod (z : Z) | Omap (z : Z) | Otop (z : Z).

(* TRUE-BLUE: the symbol band (mints + noms) is the FLOOR, below string and
   number; a point thus sits below every number (the blue floor). *)
Definition band (o : O) : Z :=
  match o with Osym _ => 0 | Ostr _ => 1 | Onum _ => 2
             | Oprod _ => 3 | Omap _ => 4 | Otop _ => 5 end.
Definition key (o : O) : Z :=
  match o with Onum z | Ostr z | Osym z | Oprod z | Omap z | Otop z => z end.

(* the order, lexicographic on (band, key) -- band across kinds, key within *)
Definition lt (a b : O) : Prop := band a < band b \/ (band a = band b /\ key a < key b).
Definition le (a b : O) : Prop := lt a b \/ a = b.

(* band and key together pin a value down: Eq is genuine equality *)
Lemma eq_from_band_key : forall a b, band a = band b -> key a = key b -> a = b.
Proof. intros a b Hb Hk. destruct a, b; cbn in Hb, Hk; try discriminate Hb; f_equal; lia. Qed.

(* < is a STRICT total order: irreflexive, transitive, asymmetric, trichotomous *)
Theorem lt_irrefl : forall a, ~ lt a a.
Proof. intro a. unfold lt. lia. Qed.

Theorem lt_trans : forall a b c, lt a b -> lt b c -> lt a c.
Proof. intros a b c. unfold lt. intros [H1|[H1a H1b]] [H2|[H2a H2b]]; lia. Qed.

Theorem lt_asym : forall a b, lt a b -> ~ lt b a.
Proof. intros a b. unfold lt. intros [H1|[H1a H1b]] [H2|[H2a H2b]]; lia. Qed.

Theorem lt_trichotomy : forall a b, lt a b \/ a = b \/ lt b a.
Proof.
  intros a b. unfold lt.
  destruct (Z.lt_trichotomy (band a) (band b)) as [Hb|[Hb|Hb]].
  - left. left. exact Hb.
  - destruct (Z.lt_trichotomy (key a) (key b)) as [Hk|[Hk|Hk]].
    + left.  right. split; assumption.
    + right. left.  apply eq_from_band_key; assumption.
    + right. right. right. split; [lia | exact Hk].
  - right. right. left. exact Hb.
Qed.

(* <= is a (non-strict) total order: reflexive, transitive, antisymmetric, total *)
Theorem le_refl : forall a, le a a.
Proof. intro a. right. reflexivity. Qed.

Theorem le_trans : forall a b c, le a b -> le b c -> le a c.
Proof.
  intros a b c [H1|H1] [H2|H2]; subst; unfold le; auto.
  left. eapply lt_trans; eassumption.
Qed.

Theorem le_antisym : forall a b, le a b -> le b a -> a = b.
Proof.
  intros a b [H1|H1] [H2|H2]; subst; auto.
  exfalso. eapply lt_asym; eassumption.
Qed.

Theorem le_total : forall a b, le a b \/ le b a.
Proof.
  intros a b. destruct (lt_trichotomy a b) as [H|[H|H]].
  - left.  left. exact H.
  - left.  right. exact H.
  - right. left. exact H.
Qed.

(* the LATTICE: the band chain symbol < string < number < product < map < top *)
Theorem symbol_lt_string  : forall x y, lt (Osym x)  (Ostr y).  Proof. intros. left. cbn. lia. Qed.
Theorem string_lt_number  : forall x y, lt (Ostr x)  (Onum y).  Proof. intros. left. cbn. lia. Qed.
Theorem number_lt_product : forall x y, lt (Onum x)  (Oprod y). Proof. intros. left. cbn. lia. Qed.
Theorem product_lt_map    : forall x y, lt (Oprod x) (Omap y).  Proof. intros. left. cbn. lia. Qed.
Theorem map_lt_top        : forall x y, lt (Omap x)  (Otop y).  Proof. intros. left. cbn. lia. Qed.

(* the 0-vs-() discipline, ORDER side: () is a FLOOR MINT (an Osym, the greenest
   point), so it sits strictly below the number 0 (an Onum) -- ORDER-distinct,
   while the value model has them =-equal-as-nothing (nil_neq_zero: both nilp, yet
   distinct values). Two of the three axes the flip thread asked the proof to keep
   apart; the third, CONTROL (() hands control to the runtime g, 0 is the inert
   value-false bit -- floor-is-the-runtime), is operational and lives outside this
   pure order/net math. *)
Theorem unit_lt_zero : forall z, lt (Osym z) (Onum 0).  Proof. intros. left. cbn. lia. Qed.

(* ============================================================ *)
(* comparing functions: = is alpha + structural                 *)
(* ============================================================ *)

(* `=` on functions is alpha-equivalence of their source. With de Bruijn
   indices alpha-equivalence IS syntactic equality (the names are gone), so it
   is DECIDABLE and structural -- exactly the spec's claim. A number NEVER
   equals a closure: numerals ACT as their lambdas under apply ((1 z) = z,
   (0 z) = 1 -- the reduction section below), but `=` stays representation-
   strict (bridging 0/1 would break congruence (2 * id <> 2), the total
   order (a lambda seats in the top band), and tower transitivity).

   Where the line sits, precisely (it is NOT "never reduce"): `=` cannot chase
   GENERAL beta -- beta-equality of lambda terms is undecidable (a common reduct
   may never appear), and `=` is total -- so on two SOURCE terms it stays α+structural
   and reduces nothing. eta likewise stays unbridged: (\ x (f x)) /= f (eta_not_bridged
   below), since bridging it means REDUCING an unreduced term, off the decidable side.
   BUT a CLOSURE VALUE is `ev`'s already-reduced artifact -- a partial-app is a held
   beta-redex with its argument captured RIGHT THERE -- so closure `=` (and the α-hash a
   map keys by) sees the ONE reduction `ev` performed to build it: (adder 5) = (\ x (+ x 5)).
   That is decidable (the redex is already contracted in the value -- finite captures, no
   search) and adds no reduction power: it is exactly st_beta surfaced into `=`
   (closure_beta_bridge, in the reduction section). So `=` never reduces on its OWN; it
   just declines to pretend the capture isn't there. *)

Open Scope nat_scope.
Inductive tm := Var (n : nat) | Lam (b : tm) | App (f a : tm).

Definition tm_one  : tm := Lam (Var 0).          (* (\ x x)     -- the identity *)
Definition tm_zero : tm := Lam (Lam (Var 0)).    (* (\ _ (\ x x)) = (\ _ 1) -- const-1 *)
Definition tm_plus : tm := Var 7.                (* a free operator, e.g. + *)

(* alpha-equivalence is decidable structural equality *)
Theorem alpha_dec : forall s t : tm, {s = t} + {s <> t}.
Proof. decide equality; apply Nat.eq_dec. Qed.

(* (\ x x) = (\ y y) -- alpha: bound vars by position *)
Theorem id_alpha : Lam (Var 0) = tm_one.
Proof. reflexivity. Qed.

(* (\ a b (+ a b)) = (\ x y (+ x y)) -- same de Bruijn skeleton *)
Theorem binders_by_position :
  Lam (Lam (App (App tm_plus (Var 1)) (Var 0)))
  = Lam (Lam (App (App tm_plus (Var 1)) (Var 0))).
Proof. reflexivity. Qed.

(* !((\ x x) = (\ y z)) -- a free var (Var 1) /= a bound one (Var 0) *)
Theorem free_neq_bound : tm_one <> Lam (Var 1).
Proof. discriminate. Qed.

(* const-1's body IS the identity term: (\ _ 1) and (\ _ (\ x x)) share one
   de Bruijn skeleton -- a fact about the term model (the runtime `=` never
   bridges a numeral to a lambda). *)
Theorem zero_is_const_one : tm_zero = Lam tm_one.
Proof. reflexivity. Qed.
Theorem zero_ne_const_two : tm_zero <> Lam (Lam (Lam (Var 0))).
Proof. discriminate. Qed.
Theorem one_ne_zero : tm_one <> tm_zero.
Proof. discriminate. Qed.

(* eta is NOT bridged: (\ x (f x)) /= f *)
Theorem eta_not_bridged : Lam (App tm_plus (Var 0)) <> tm_plus.
Proof. discriminate. Qed.

(* ============================================================ *)
(* reduction: the semantics `ev` RUNS (not what `=` observes)   *)
(* ============================================================ *)
(* `=` is alpha+structural and deliberately stops there (above). The runtime's
   evaluator `ev` goes further -- it REDUCES. This models that operational layer
   with de Bruijn beta+eta, so the numeral laws (1 acts as id, 0 as const-1, the
   Church tower) hold not just as an encoding but as actual reductions, and the
   distinctions `=` keeps survive reduction too. Each theorem here has a twin
   assertion against the real binary in test/spec.l (e.g. ((\ x x) 5) ; 5): the
   Rocq side says "valid by the calculus", the corpus side says "the binary
   computes exactly this". Two independent witnesses for one claim.

   shift bumps the free vars (index >= cutoff c) when we pass under a binder;
   subst j s t replaces var j by s and renumbers the vars that outlive the
   removed binder. The example proofs close by COMPUTATION (exact reduces the
   index `if`s), so an off-by-one in subst cannot typecheck -- the safety is in
   the conversion check, not in trusting the definition. *)

Fixpoint shift (d c : nat) (t : tm) : tm :=
  match t with
  | Var k   => if Nat.ltb k c then Var k else Var (k + d)
  | Lam b   => Lam (shift d (S c) b)
  | App f a => App (shift d c f) (shift d c a)
  end.

Fixpoint subst (j : nat) (s t : tm) : tm :=
  match t with
  | Var k   => if Nat.eqb k j then s
               else if Nat.ltb k j then Var k else Var (k - 1)
  | Lam b   => Lam (subst (S j) (shift 1 0 s) b)
  | App f a => App (subst j s f) (subst j s a)
  end.

(* single-step beta+eta with full congruence -- the one-step rewrite ev runs *)
Inductive step : tm -> tm -> Prop :=
  | st_beta : forall b a, step (App (Lam b) a) (subst 0 a b)
  | st_eta  : forall f,   step (Lam (App (shift 1 0 f) (Var 0))) f
  | st_app1 : forall f f' a, step f f' -> step (App f a) (App f' a)
  | st_app2 : forall f a a', step a a' -> step (App f a) (App f a')
  | st_lam  : forall b b',   step b b' -> step (Lam b) (Lam b').

Inductive steps : tm -> tm -> Prop :=
  | sn_refl : forall t, steps t t
  | sn_step : forall a b c, step a b -> steps b c -> steps a c.

(* (\ x x) a ~> a -- beta on the identity, for ANY argument (spec.l: ((\ x x) 5)) *)
Theorem beta_id : forall a, step (App tm_one a) a.
Proof. intro a. exact (st_beta (Var 0) a). Qed.

(* (\ _ 1) a ~> 1 -- const-1 drops its argument (spec.l: ((\ _ 1) 9) ; up to alpha) *)
Theorem beta_const : forall a, step (App tm_zero a) tm_one.
Proof. intro a. exact (st_beta (Lam (Var 0)) a). Qed.

(* THE BETA BRIDGE that closure `=` observes (love.c clo_eq / nf_hash): a held redex --
   a partial-app (App (Lam b) a), the captured arg a sitting in the closure -- and its
   contractum (subst 0 a b, the no-capture residual) are identified by value `=` and the
   α-hash. This is SOUND and decidable precisely because it is ONE st_beta step, already in
   the calculus: the value `=` agreeing with the reduction `ev` ran, never reducing on its
   own (so general beta / eta stay unbridged -- eta_not_bridged). The proof IS st_beta. *)
Theorem closure_beta_bridge : forall b a, step (App (Lam b) a) (subst 0 a b).
Proof. intros. apply st_beta. Qed.

(* nested: (\ x x) ((\ x x) a) ~>* a -- the closure reduces all the way down *)
Theorem beta_nested : forall a, steps (App tm_one (App tm_one a)) a.
Proof.
  intro a.
  eapply sn_step. exact (st_beta (Var 0) (App tm_one a)).
  eapply sn_step. apply beta_id.
  apply sn_refl.
Qed.

(* eta reinforces eta_not_bridged: under de Bruijn the eta-reduct of
   `Lam (App (Var 7) (Var 0))` is Var 6, NOT tm_plus = Var 7 -- the binder
   shifted the index, so reduction cannot collapse them either. *)
Theorem eta_redex_shifts : step (Lam (App (Var 7) (Var 0))) (Var 6).
Proof. exact (st_eta (Var 6)). Qed.

(* ============================================================ *)
(* + and * are generic: the sequence monoid and repetition      *)
(* ============================================================ *)

(* `+` on sequences (strings, lists) is concatenation -- a MONOID: associative,
   with the empty sequence as identity ("" and () are the units, cat_nil_l/r).
   The runtime LIFTS that unit out of the sequence lane: a bare mint -- the zero
   point () too -- is the UNIT, the do-nothing element, the IDENTITY of BOTH + and
   * in EVERY lane (() + x = () * x = x for a number, string, complex, array,
   anything), not just on sequences. 0 and 1 are its two FACES: the unit shows 0
   (the additive identity) in + and 1 (the multiplicative identity) in *, one
   nothing reading as each operation's identity -- yet it stays the unit, NOT the
   number 0 (which annihilates *: smul 0 = []). The model is typed-by-sequences,
   so cat_nil_l/r (the + unit) and smul_one (the * unit) are the witnessed
   fragments; the scalar lanes are that same unit extended by dispatch. `*` is
   REPEATED `+`: a sequence times a count is that many copies concatenated, and
   the count SATURATES (max 0, ceil) -- a non-positive count gives the empty
   sequence. `+` is also the MEASURE HOMOMORPHISM: the net of a concatenation
   is the sum of the nets. The BYTE LAW (a string + an exact integer 0..255 is
   one byte; rep-blind, so 66 and 66.0 alike) is the one PARTIAL case, modeled
   as an option -- None is nil. The other failures are the same None lane:
   NAMED-symbol-`+` (named symbols left the string algebra) and string-`-`
   (numeric only) are nil, not modeled separately. *)

Open Scope Z_scope.

(* + as concatenation: the sequence monoid (assoc + identity) *)
Theorem cat_assoc : forall (A:Type) (x y z : list A), (x ++ y) ++ z = x ++ (y ++ z).
Proof. intros A x y z. induction x as [|a x IH]; simpl; [reflexivity | rewrite IH; reflexivity]. Qed.
Theorem cat_nil_l : forall (A:Type) (x : list A), [] ++ x = x.  Proof. reflexivity. Qed.
Theorem cat_nil_r : forall (A:Type) (x : list A), x ++ [] = x.
Proof. intros A x. induction x as [|a x IH]; simpl; [reflexivity | rewrite IH; reflexivity]. Qed.

(* concrete: "ab"+"cd"="abcd"; '(1 2)+'(3 4)='(1 2 3 4); 5+'(1 2)='(5 1 2) (adjoin) *)
Theorem cat_strings : [97;98] ++ [99;100] = [97;98;99;100].    Proof. reflexivity. Qed.
Theorem cat_lists   : [1;2] ++ [3;4] = ([1;2;3;4] : list Z).    Proof. reflexivity. Qed.
Theorem adjoin      : 5 :: [1;2] = ([5;1;2] : list Z).          Proof. reflexivity. Qed.

(* * as repeated +: n copies concatenated *)
Definition srep {A} (n : nat) (s : list A) : list A := concat (repeat s n).

(* * IS repeated +: one more copy is one more concatenation *)
Theorem star_is_repeated_plus : forall (A:Type) (n:nat) (s:list A), srep (S n) s = s ++ srep n s.
Proof. intros. unfold srep. reflexivity. Qed.

(* the count SATURATES: max(0, ceil); integer count => ceil id, non-positive => empty *)
Definition scount (c : Z) : nat := Z.to_nat (Z.max 0 c).
Definition smul {A} (c : Z) (s : list A) : list A := srep (scount c) s.

Theorem star_ab_3   : smul 3 [97;98] = [97;98;97;98;97;98].     Proof. reflexivity. Qed. (* "ab"*3 *)
Theorem star_list_2 : smul 2 [1;2] = ([1;2;1;2] : list Z).      Proof. reflexivity. Qed. (* '(1 2)*2 *)
Theorem star_neg    : forall (A:Type) (s:list A), smul (-3) s = [].  Proof. reflexivity. Qed. (* "ab"*-3 = "" *)
Theorem star_zero   : forall (A:Type) (s:list A), smul 0 s = [].     Proof. reflexivity. Qed. (* '(1 2)*0 = () *)
(* the * UNIT: 1 is *'s identity (smul 1 = id) -- the face () shows in *, as
   the empty sequence is the face it shows in + (cat_nil_r). Contrast star_zero:
   the number 0 ANNIHILATES *, but the unit () does not (() * x = x). *)
Theorem smul_one    : forall (A:Type) (s:list A), smul 1 s = s.
Proof. intros A s. unfold smul, srep, scount. simpl. apply app_nil_r. Qed.

Theorem count_saturates : forall c, c <= 0 -> scount c = 0%nat.
Proof. intros c H. unfold scount. rewrite Z.max_l by lia. reflexivity. Qed.
Theorem count_keeps : forall c, 0 <= c -> Z.of_nat (scount c) = c.
Proof. intros c H. unfold scount. rewrite Z.max_r by lia. apply Z2Nat.id. lia. Qed.

(* ---- () is the IDENTITY OF BOTH MONOIDS, over ONE unit token -----------------
   cat_nil_l/r and smul_one above witness the two FACES of () separately: it shows
   as the empty sequence [] in + and as the count 1 in *. The prose claim is
   stronger -- a SINGLE () that is the do-nothing element of + AND * across every
   lane: () + x = () * x = x for a number, a sequence, anything. Model a value as
   the unit (), a number, or a (nestable) sequence, with + and * the GENERIC ops
   that send () to the OTHER operand and otherwise act per lane: numbers add /
   multiply, sequences concatenate, a number-by-sequence * is the saturating repeat
   -- * is iterated + -- and sequence-by-sequence * is the cartesian product, each
   pair a 2-list, which is why the carrier must NEST (GSeq holds gval). Then () is a
   two-sided identity of BOTH, over the SAME () token -- and is provably NOT the
   number 0, which ANNIHILATES * (0 * x = []) while () never does. *)
Inductive gval := GUnit | GNum (z : Z) | GSeq (xs : list gval).

Definition gplus (a b : gval) : gval :=
  match a, b with
  | GUnit,   _       => b
  | _,       GUnit   => a
  | GNum m,  GNum n  => GNum (m + n)
  | GSeq xs, GSeq ys => GSeq (xs ++ ys)
  | GNum n,  GSeq ys => GSeq (GNum n :: ys)        (* adjoin: 5 + '(1 2) = '(5 1 2) *)
  | GSeq xs, GNum n  => GSeq (xs ++ [GNum n])
  end.

Definition gtimes (a b : gval) : gval :=
  match a, b with
  | GUnit,   _       => b
  | _,       GUnit   => a
  | GNum m,  GNum n  => GNum (m * n)
  | GNum n,  GSeq ys => GSeq (concat (repeat ys (Z.to_nat (Z.max 0 n))))  (* repeat: iterated + *)
  | GSeq xs, GNum n  => GSeq (concat (repeat xs (Z.to_nat (Z.max 0 n))))
  | GSeq xs, GSeq ys => GSeq (flat_map (fun x => map (fun y => GSeq [x; y]) ys) xs)  (* cartesian *)
  end.

(* the + monoid's identity, the SAME () token on either side: () + x = x = x + () *)
Theorem gunit_plus_l  : forall x, gplus GUnit x = x.   Proof. reflexivity. Qed.
Theorem gunit_plus_r  : forall x, gplus x GUnit = x.   Proof. intro x; destruct x; reflexivity. Qed.
(* the * monoid's identity, over that same () token: () * x = x = x * () *)
Theorem gunit_times_l : forall x, gtimes GUnit x = x.  Proof. reflexivity. Qed.
Theorem gunit_times_r : forall x, gtimes x GUnit = x.  Proof. intro x; destruct x; reflexivity. Qed.

(* () is NOT the number 0: the number 0 ANNIHILATES * (0 * x = []), the unit does
   not (() * x = x) -- so they differ on the * lane (cf. unit_neq_zero). *)
Theorem gzero_annihilates : forall ys, gtimes (GNum 0) (GSeq ys) = GSeq [].
Proof. reflexivity. Qed.
Theorem gunit_ne_zero : gtimes GUnit (GSeq [GNum 1]) <> gtimes (GNum 0) (GSeq [GNum 1]).
Proof. cbn. congruence. Qed.

(* LIST * LIST is the CARTESIAN PRODUCT (the chain lane, lvm_mul_cart): every ordered
   pairing, each pair a 2-list. This is the semiring product whose + is ++ -- but only
   ON THE CHAIN, whose cells hold pairs; strings (atomic bytes) carry the * UNIT
   (smul, repeat) and no in-carrier product. tally (length) is the HOMOMORPHISM, and
   the product is RIGHT-distributive on the nose; LEFT-distributivity holds only up to
   a PERMUTATION (intrinsic to ordered pairs -- same multiset, different order). *)
Definition cart {A B} (a : list A) (b : list B) : list (A * B) :=
  flat_map (fun x => map (fun y => (x,y)) b) a.

Theorem cart_12_34 : cart [1;2] [3;4] = ([(1,3);(1,4);(2,3);(2,4)] : list (Z*Z)).
Proof. reflexivity. Qed.                                   (* '(1 2)*'(3 4) *)

(* tally is the homomorphism: length (a*b) = length a * length b *)
Theorem cart_length : forall (A B:Type) (a:list A) (b:list B),
  length (cart a b) = (length a * length b)%nat.
Proof. intros A B a b. unfold cart. induction a as [|x a IH]; simpl;
  [reflexivity | rewrite length_app, length_map, IH; reflexivity]. Qed.

(* right-distributive on the nose: (a++b)*c = a*c ++ b*c *)
Theorem cart_distr_r : forall (A B:Type) (a b:list A) (c:list B),
  cart (a ++ b) c = cart a c ++ cart b c.
Proof. intros A B a b c. unfold cart. induction a as [|x a IH]; simpl;
  [reflexivity | rewrite IH, app_assoc; reflexivity]. Qed.

(* left-distributive only UP TO PERMUTATION: a*(b++c) ~ a*b ++ a*c *)
Theorem cart_distr_l_perm : forall (A B:Type) (a:list A) (b c:list B),
  Permutation (cart a (b ++ c)) (cart a b ++ cart a c).
Proof. intros A B a b c. unfold cart. induction a as [|x a IH]; simpl.
  - apply Permutation_refl.
  - rewrite map_app. eapply Permutation_trans.
    + apply Permutation_app_head. exact IH.
    + rewrite <- !app_assoc. apply Permutation_app_head.
      rewrite !app_assoc. apply Permutation_app_tail. apply Permutation_app_comm. Qed.

(* THE COUNT LAW IS SHARED: numeral-apply (n f) composes f the SAME scount times.
   `*` repeats (smul) and a numeral composes (numap) through ONE saturated count,
   the same Z.max 0 that defines `sat` -- so count_saturates is BOTH the *-floor
   and the compose-floor. This is the count side of the one saturation bit; its
   value-side twin is sat_clamps (net <= 0 => sat = 0), and its third face is the
   n-ary closure's fire-vs-hold (test/oracle.l, test/proof/rocq/extract.v). (Only the
   COMPOSITION count saturates; the numeral-on-numeral exponent climbs to
   reciprocals -- (-1 x) = 1/x -- so this is appf-iteration, not `app`.) *)
Definition numap {A} (c : Z) (f : A -> A) (x : A) : A := Nat.iter (scount c) f x.

(* a non-positive count composes 0 times: the identity -- the compose-FLOOR,
   the eval-count reflection of sat_clamps, via the SAME count_saturates *)
Theorem numap_floor : forall A (c : Z) (f : A -> A) (x : A),
  c <= 0 -> numap c f x = x.
Proof. intros A c f x H. unfold numap. rewrite (count_saturates c H). reflexivity. Qed.

(* a nonnegative count agrees with the bare (n f) nat lane (appf) *)
Theorem numap_pos : forall A (n : nat) (f : A -> A) (x : A),
  numap (Z.of_nat n) f x = appf n f x.
Proof.
  intros A n f x. unfold numap, appf. f_equal.
  unfold scount. rewrite Z.max_r by lia. apply Nat2Z.id.
Qed.

(* + IS THE MEASURE HOMOMORPHISM, and * scales the measure by the count *)
Definition sum (s : list Z) : Z := fold_right Z.add 0 s.
Theorem sum_cat  : forall x y, sum (x ++ y) = sum x + sum y.
Proof. induction x as [|a x IH]; intro y; simpl; [reflexivity | rewrite IH; lia]. Qed.
Theorem sum_srep : forall n s, sum (srep n s) = Z.of_nat n * sum s.
Proof.
  induction n as [|n IH]; intro s.
  - cbn. ring.
  - rewrite star_is_repeated_plus, sum_cat, IH.
    replace (Z.of_nat (S n)) with (Z.of_nat n + 1) by lia. ring.
Qed.

(* the byte law: string + exact 0..255 is one byte (rep-blind); else nil (None) *)
Definition byte_add (s : list Z) (n : Z) : option (list Z) :=
  if andb (0 <=? n) (n <=? 255) then Some (s ++ [n]) else None.

Theorem byte_in_range : byte_add [120] 66 = Some [120; 66].   Proof. reflexivity. Qed. (* "x"+66="xB" *)
Theorem byte_neg_nil  : byte_add [120] (-66) = None.          Proof. reflexivity. Qed. (* "x"+-66=nil *)
Theorem byte_over_nil : byte_add [120] 256 = None.            Proof. reflexivity. Qed. (* "x"+256=nil *)

Theorem byte_iff : forall s n, byte_add s n <> None <-> 0 <= n <= 255.
Proof.
  intros s n. unfold byte_add. split.
  - destruct (0 <=? n) eqn:E1; destruct (n <=? 255) eqn:E2; cbn; intro H.
    + apply Z.leb_le in E1. apply Z.leb_le in E2. lia.
    + congruence.
    + congruence.
    + congruence.
  - intros [H1 H2]. apply Z.leb_le in H1. apply Z.leb_le in H2.
    rewrite H1, H2. cbn. discriminate.
Qed.

(* ============================================================ *)
(* arrays: shaped, broadcasting, reductions, contraction        *)
(* ============================================================ *)

(* A rank-1 array is a vector (list Z); a shape is its dimension sizes. The
   cell count is the PRODUCT of the shape (alen), the rank its length. Indexing
   is row-major and out-of-bounds reads the default (peep). + and scalar-* and
   the contractions are elementwise/bilinear, and the EMPTY REDUCTIONS ANSWER
   THEIR MONOID UNITS (asum []=0, aprod []=1, aall []=true). Shapes/indices are
   modeled as Z (via Z.to_nat for nth) to stay in one scope; the higher-rank
   layout is sketched by the row-major flatten and the matmul cell. *)

Open Scope Z_scope.

(* shape metadata: cell count = product of the shape; rank = its length *)
Definition alen  (shape : list Z) : Z := fold_right Z.mul 1 shape.
Definition arank (shape : list Z) : Z := Z.of_nat (length shape).
Theorem alen_23         : alen [2;3] = 6.    Proof. reflexivity. Qed.
Theorem arank_23        : arank [2;3] = 2.   Proof. reflexivity. Qed.
Theorem alen_empty_axis : alen [0] = 0.      Proof. reflexivity. Qed. (* a 0-axis -> 0 cells *)

(* peep: indexed read, out-of-bounds -> the default (nth's own behavior) *)
Definition znth (i : Z) (v : list Z) (d : Z) : Z := nth (Z.to_nat i) v d.
Theorem peep_in  : znth 1 [10;20;30] (-1) = 20.   Proof. reflexivity. Qed.
Theorem peep_oob : znth 9 [10;20;30] (-1) = -1.   Proof. reflexivity. Qed. (* OOB -> default *)
(* row-major 2D: flat index (i,j) in a cols-wide grid = i*cols + j *)
Definition znth2 (cols i j : Z) (v : list Z) (d : Z) : Z := znth (i*cols + j) v d.
Theorem peep_22 : znth2 2 1 0 [1;2;3;4] (-1) = 3.  Proof. reflexivity. Qed. (* [1][0] in 2x2 -> vals[2]=3 *)

(* reductions, and the empty-reduction-is-the-unit law *)
Definition asum  (v : list Z)    : Z    := fold_right Z.add 0 v.
Definition aprod (v : list Z)    : Z    := fold_right Z.mul 1 v.
Definition amax  (v : list Z)    : Z    := match v with [] => 0 | x :: xs => fold_right Z.max x xs end.
Definition aall  (v : list bool) : bool := forallb (fun b => b) v.
Theorem asum_empty  : asum [] = 0.     Proof. reflexivity. Qed. (* the monoid units, as empty reductions *)
Theorem aprod_empty : aprod [] = 1.    Proof. reflexivity. Qed.
Theorem aall_empty  : aall [] = true.  Proof. reflexivity. Qed.
Theorem asum_123    : asum [10;20;30] = 60.  Proof. reflexivity. Qed.
Theorem amax_30     : amax [10;30;20] = 30.  Proof. reflexivity. Qed.

(* broadcasting: + needs conforming shapes (else nil = None); scalar-* scales *)
Fixpoint vadd (a b : list Z) : option (list Z) :=
  match a, b with
  | [], []         => Some []
  | x :: a', y :: b' => match vadd a' b' with Some r => Some ((x + y) :: r) | None => None end
  | _, _           => None
  end.
Theorem vadd_ok       : vadd [1;2;3] [10;20;30] = Some [11;22;33].  Proof. reflexivity. Qed.
Theorem vadd_mismatch : vadd [1;2;3] [1;2] = None.                 Proof. reflexivity. Qed. (* shape mismatch -> nil *)

Definition vscale (c : Z) (v : list Z) : list Z := map (Z.mul c) v.
Theorem vscale_123 : vscale 2 [1;2;3] = [2;4;6].  Proof. reflexivity. Qed.

(* + is the MEASURE HOMOMORPHISM on arrays; * scales the measure by the count *)
Lemma asum_cons : forall x v, asum (x :: v) = x + asum v.
Proof. reflexivity. Qed.
Lemma asum_app : forall a b, asum (a ++ b) = asum a + asum b.
Proof. induction a as [|x a IH]; intro b; simpl; [reflexivity | unfold asum in *; simpl; rewrite IH; lia]. Qed.
Theorem asum_vadd : forall a b r, vadd a b = Some r -> asum r = asum a + asum b.
Proof.
  induction a as [|x a IH]; intros [|y b] r H; cbn in H; try discriminate.
  - injection H as H. subst r. reflexivity.
  - destruct (vadd a b) as [r'|] eqn:E; [|discriminate].
    injection H as H. subst r. rewrite !asum_cons. rewrite (IH b r' E). lia.
Qed.
Theorem asum_vscale : forall c v, asum (vscale c v) = c * asum v.
Proof. intros c v. unfold vscale, asum. induction v as [|x v IH]; simpl; [ring | rewrite IH; ring]. Qed.

(* iota: the z-array 0..n-1, jot's array twin, and its GAUSS SUM *)
Definition iota (n : nat) : list Z := map Z.of_nat (seq 0 n).
Theorem iota_3 : iota 3 = [0;1;2].  Proof. reflexivity. Qed.
Theorem iota_0 : iota 0 = [].       Proof. reflexivity. Qed. (* (iota 0) empty -> nothing *)
Lemma iota_S : forall n, iota (S n) = iota n ++ [Z.of_nat n].
Proof. intro n. unfold iota. rewrite seq_S, map_app. reflexivity. Qed.
Theorem asum_iota : forall n, 2 * asum (iota n) = Z.of_nat n * (Z.of_nat n - 1).
Proof.
  induction n as [|n IH].
  - reflexivity.
  - rewrite iota_S, asum_app. cbn [asum fold_right]. rewrite Nat2Z.inj_succ. nia.
Qed.
Theorem asum_iota_100 : asum (iota 100) = 4950.  Proof. now vm_compute. Qed.

(* ============================================================ *)
(* the Z lane: ONE model, shared with the generated test/proof/rocq/gen.v  *)
(* ============================================================ *)
(* The nat `app` (Nat.pow) above carries the numeral LAWS by clean unary induction.
   The GENERATED corpus checks (test/proof/rocq/gen.v, from tools/spec2coq.l) instead need Z --
   3^27 would blow unary-nat vm_compute -- so they run over `appZ`. These are not two
   models: `app_appZ` PROVES appZ and app are ONE function under the nat->Z embedding,
   so gen.v `Require Import spec` and checks its instances against THIS file's
   definitions (asum/aprod/amax/vscale/srep shared verbatim, appZ/iotaZ the Z faces),
   not a separate copy. The closing of the two-models gap. *)
Definition appZ  (n x : Z) : Z := Z.pow x n.
Definition iotaZ (n : Z) : list Z := iota (Z.to_nat n).   (* iota's Z-arg face, same data *)
Theorem app_appZ : forall n x, appZ (Z.of_nat n) (Z.of_nat x) = Z.of_nat (app n x).
Proof. intros n x. unfold appZ, app. symmetry. apply Nat2Z.inj_pow. Qed.

(* contraction: the dot product (+.x), commutative; inner needs conforming length *)
Fixpoint dot (a b : list Z) : Z :=
  match a, b with x :: a', y :: b' => x * y + dot a' b' | _, _ => 0 end.
Theorem dot_123_456 : dot [1;2;3] [4;5;6] = 32.  Proof. reflexivity. Qed.
Theorem dot_comm : forall a b, dot a b = dot b a.
Proof. induction a as [|x a IH]; intros [|y b]; cbn; try reflexivity; rewrite IH; lia. Qed.

Definition inner (a b : list Z) : option Z :=
  if Nat.eqb (length a) (length b) then Some (dot a b) else None.
Theorem inner_ok       : inner [1;2;3] [4;5;6] = Some 32.  Proof. reflexivity. Qed.
Theorem inner_mismatch : inner [1;2;3] [1;2] = None.       Proof. reflexivity. Qed. (* !(inner ..) *)

(* matmul cell M[i][j] = row i of A, dotted with column j of B (row-major) *)
Definition row (i : Z) (A : list (list Z)) : list Z := nth (Z.to_nat i) A [].
Definition col (j : Z) (B : list (list Z)) : list Z := map (fun r => znth j r 0) B.
Definition mm_cell (A B : list (list Z)) (i j : Z) : Z := dot (row i A) (col j B).
Theorem matmul_cell :
  mm_cell [[1;2;3];[4;5;6]] [[7;8];[9;10];[11;12]] 1 1 = 154.
Proof. reflexivity. Qed.

(* ============================================================ *)
(* strings, symbols & mints                                     *)
(* ============================================================ *)

(* A string is its bytes (list Z): it INDEXES them, its NET is the charm sum (a
   NUL byte nets nothing, so + reaches the bytes and an all-NUL text is
   nothing), and tally COUNTS them. A symbol is a POINT: a mint is a fresh
   nameless point -- serial-keyed, materially empty ($ = 0), applying const-1,
   DISTINCT from every other and equal only to itself; the zero point is the
   mint at serial 0, the face of absence. A nom is the explicit PRODUCT
   (name . mint) -- a chain of a name string and a mint, EXACTLY as internally
   (cap the name, cup the mint), ordered (name lex, then the mint's serial);
   same-name noms split by serial. The symbol band (mints AND noms) is the
   FLOOR: it sorts BELOW string and number, so every point sits below every
   number -- (mint 0) < 0, the blue floor -- and a bare mint below a named nom
   (proved via the order section's `lt`: a point or nom is Osym, a string Ostr,
   a number Onum). *)

Open Scope Z_scope.

(* --- strings: indexed bytes; net = charm sum; tally = count ---
   sidx is the SEQUENCE index law, shared with the chain: a negative i counts from the end,
   and out of range in EITHER direction answers the unit 1. *)
Definition sidx' (v : list Z) (i : Z) : Z :=
  let j := if i <? 0 then i + Z.of_nat (length v) else i in
  if j <? 0 then 1 else nth (Z.to_nat j) v 1.
Definition sidx  (v : list Z) (i : Z) : Z := nth (Z.to_nat i) v 1. (* OOB applies as the unit, 1 *)
Definition tally (v : list Z) : Z := Z.of_nat (length v).
Theorem str_index     : sidx [97;98;99] 0 = 97.   Proof. reflexivity. Qed. (* ("abc" 0) = 97 *)
Theorem str_index_oob : sidx [104;105] 9 = 1.     Proof. reflexivity. Qed. (* ("hi" 9) = 1 *)
Theorem str_net       : asum [97;98;99] = 294.    Proof. reflexivity. Qed. (* $"abc" = 294 *)
Theorem str_tally     : tally [97;98;99] = 3.     Proof. reflexivity. Qed. (* (tally "abc") = 3 *)

(* JUXTAPOSITION: a string applied to a string is the concatenation -- C's adjacent-literal
   law with the literal restriction lifted. It IS ++, so it inherits the monoid (associative
   with "" the unit) and hence agrees with `+` and curries to any arity. *)
Definition sjux (s t : list Z) : list Z := s ++ t.
Theorem str_juxt      : sjux [97;98] [99;100] = [97;98;99;100]. Proof. reflexivity. Qed. (* ("ab" "cd") = "abcd" *)
Theorem str_juxt_cat  : forall s t, sjux s t = s ++ t.          Proof. reflexivity. Qed. (* agrees with `+` *)
Theorem str_juxt_unitl: forall s, sjux [] s = s.                Proof. reflexivity. Qed. (* ("" s) = s *)
Theorem str_juxt_unitr: forall s, sjux s [] = s.                Proof. intros. apply app_nil_r. Qed. (* (s "") = s *)
Theorem str_juxt_assoc: forall s t u, sjux (sjux s t) u = sjux s (sjux t u).  (* so ("a" "b" "c") joins three *)
Proof. intros. unfold sjux. now rewrite app_assoc. Qed.

(* NEGATIVE INDEXES count from the end; past either end is the unit. The j < 0 guard is
   load-bearing: Z.to_nat sends every negative to 0, so without it an index below
   -(length v) would answer the FIRST element instead of the unit -- which is exactly the
   >= 0 test the C makes after adding the length. *)
Theorem str_idx_neg1  : sidx' [97;98;99] (-1) = 99.  Proof. reflexivity. Qed. (* ("abc" -1) = 'c' *)
Theorem str_idx_neg3  : sidx' [97;98;99] (-3) = 97.  Proof. reflexivity. Qed. (* ("abc" -3) = 'a' *)
Theorem str_idx_neg_oob : sidx' [97;98;99] (-4) = 1. Proof. reflexivity. Qed. (* past the front -> the unit *)
Theorem str_idx_pos_agrees : forall v i, 0 <= i -> sidx' v i = sidx v i.      (* non-negative: the old law *)
Proof. intros v i H. unfold sidx', sidx. apply Z.ltb_ge in H as E. rewrite E. cbn. now rewrite E. Qed.

(* --- chains: the SAME two laws one lattice rung up -- index an ELEMENT, juxtapose by
   APPEND. The Church eliminator ((a . b) g) == (g a b) was retired once the tree was
   checked and nothing applied a chain to a function; every non-index, non-chain operand
   takes the default action, const-1 (see mx.l). --- *)
Definition cidx (v : list Z) (i : Z) : Z := sidx' v i.
Definition cjux (s t : list Z) : list Z := s ++ t.
Theorem chain_idx      : cidx [7;8;9] 0 = 7.        Proof. reflexivity. Qed. (* ('(7 8 9) 0) = 7 *)
Theorem chain_idx_neg  : cidx [7;8;9] (-1) = 9.     Proof. reflexivity. Qed. (* ('(7 8 9) -1) = 9 *)
Theorem chain_idx_oob  : cidx [7;8;9] 3 = 1.        Proof. reflexivity. Qed. (* out of range -> the unit *)
Theorem chain_juxt     : cjux [1;2] [3;4] = [1;2;3;4].  Proof. reflexivity. Qed. (* ('(1 2) '(3 4)) *)
Theorem chain_juxt_cat : forall s t, cjux s t = s ++ t. Proof. reflexivity. Qed. (* agrees with `+` *)
(* the two sequence lanes are ONE law, read at two grains: bytes and elements *)
Theorem seq_one_law    : forall v i, cidx v i = sidx' v i /\ forall s t, cjux s t = sjux s t.
Proof. intros. split. reflexivity. intros. reflexivity. Qed.

(* the NUL byte nets nothing: + is the measure homomorphism down to the bytes *)
Theorem nul_appends_free : asum [97; 0] = 97.   Proof. reflexivity. Qed. (* $(+ "a" 0) = 97 *)
Theorem all_nul_nothing  : asum [0;0;0] = 0.    Proof. reflexivity. Qed. (* an all-NUL text IS nothing *)
Theorem string_nul       : asum [0] = 0.        Proof. reflexivity. Qed. (* !(string 0) *)
Theorem string_one       : asum [1] = 1.        Proof. reflexivity. Qed. (* !!(string 1) *)

(* slice: half-open [i,j) *)
Definition slice (v : list Z) (i j : Z) : list Z := firstn (Z.to_nat (j - i)) (skipn (Z.to_nat i) v).
Theorem slice_forbidden :
  slice [102;111;114;98;105;100;100;101;110;32;112;108;97;110;101;116] 3 9
  = [98;105;100;100;101;110].   (* "forbidden planet"[3,9) = "bidden" *)
Proof. reflexivity. Qed.
Theorem slice_nul : slice [97; 0] 1 2 = [0].   (* (slice (+ "a" 0) 1 2) is the NUL -> nothing *)
Proof. reflexivity. Qed.

(* --- mints: fresh, distinct, nameless points --- *)
Inductive mint := Mint (serial : nat).
Definition mnet (_ : mint) : Z := 0.              (* materially empty: $ = 0 *)
Definition mapp {A} (_ : mint) (_ : A) : Z := 1.  (* applies const-1 *)
Definition zero_point : mint := Mint 0.           (* the face of absence (serial 0) *)

Theorem mint_empty    : forall m, mnet m = 0.           Proof. reflexivity. Qed. (* $(mint 0) = 0 *)
Theorem mint_const1   : forall m (x:Z), mapp m x = 1.   Proof. reflexivity. Qed. (* ((mint 0) 5) = 1 *)
Theorem mint_self     : forall m : mint, m = m.         Proof. reflexivity. Qed. (* itself only *)
Theorem mint_distinct : forall a b, Mint a = Mint b <-> a = b.  (* distinct iff serials agree *)
Proof. intros a b. split; [intro H; injection H; auto | intro H; subst; reflexivity]. Qed.

(* --- noms: the explicit PRODUCT (name . mint) -- a chain of a name string and
   a mint, EXACTLY the internal representation: cap is the name, cup is the mint,
   and the mint (its serial) keys same-name noms. Reuses the `mint` type above,
   so a nom IS a name paired with a genuine point, not a flattened serial. --- *)
Definition nom := (list Z * mint)%type.
(* A nom is its OWN kind (KNom: a name + a serial), modeled here as the abstract
   pair name x identity -- NOT a surface chain. The projections below are the
   abstract pair components (the cell's name / serial fields); the SURFACE reads
   the name via `string` (a nom is not a chain, so `cap` no longer projects it). *)
Definition nom_name (n : nom) : list Z := fst n.        (* the name component *)
Definition nom_mint (n : nom) : mint  := snd n.         (* the identity component *)
Definition nom_serial (n : nom) : nat := match snd n with Mint s => s end.
Theorem nom_cap : nom_name ([120], Mint 7) = [120].   Proof. reflexivity. Qed. (* (string (nom 'x)) = "x" -- the name projection *)
Theorem nom_cup : nom_mint ([120], Mint 7) = Mint 7.  Proof. reflexivity. Qed. (* the identity (serial) component *)

Fixpoint lex_lt (a b : list Z) : Prop :=
  match a, b with
  | [], [] => False
  | [], _  => True
  | _, []  => False
  | x :: a', y :: b' => x < y \/ (x = y /\ lex_lt a' b')
  end.
Definition nom_lt (a b : nom) : Prop :=
  lex_lt (fst a) (fst b) \/ (fst a = fst b /\ (nom_serial a < nom_serial b)%nat).

(* same-name noms are DISTINCT and ordered by the mint's serial (the pair lex inherits it) *)
Theorem same_name_serial   : forall nm s1 s2, (s1 < s2)%nat -> nom_lt (nm, Mint s1) (nm, Mint s2).
Proof. intros nm s1 s2 H. right. split; [reflexivity | exact H]. Qed.
Theorem same_name_distinct : forall (nm : list Z) (s1 s2 : nat), s1 <> s2 -> (nm, Mint s1) <> (nm, Mint s2).
Proof. intros nm s1 s2 H C. apply H. injection C. auto. Qed.

(* the FLOOR: the symbol band (mints AND noms) sits BELOW the numbers -- the blue
   floor, every point under every number. Within the floor a bare mint (lower
   serial-key) sorts below a named nom; the nom_lt section refines that order. *)
Theorem point_below_number : lt (Osym 5) (Onum 0).   Proof. left.  cbn. lia. Qed. (* (mint 0) < 0 *)
Theorem mint_below_nom     : lt (Osym 3) (Osym 5).   Proof. right. cbn. split; [reflexivity | lia]. Qed. (* a bare mint < a nom *)

(* ============================================================ *)
(* the complex tier: EXACT complex as Gaussian integers Z[i]    *)
(* ============================================================ *)
(* The numeric tower's float/transcendental tier stays an honest miss
   (spec.v:10-12, not vm_compute-able: e^(iπ) misses -1 by ~1e-16). But the
   EXACT complex algebra the spec leans on -- i*i = -1 and the (re,im) order --
   IS exact integer arithmetic, machine-checked here over Z[i]. `twin re im` is
   the ~(re im) constructor; i = twin 0 1; a real lifts ~r = ~(r 0); and `~`
   bare conjugates (an involution). Ground-truthed against the binary: the
   conjugate of ~(2 3) is ~(2 -3), and conj is an involution (~~~(2 3)=~(2 3)). *)
Record Zi := twin { re : Z ; im : Z }.
Definition cadd (a b : Zi) : Zi := twin (re a + re b) (im a + im b).
Definition cmul (a b : Zi) : Zi :=
  twin (re a * re b - im a * im b) (re a * im b + im a * re b).
Definition cconj (z : Zi) : Zi := twin (re z) (- im z).
Definition I : Zi := twin 0 1.
Definition cof (r : Z) : Zi := twin r 0.

(* the algebraic heart of Euler. NOTE: cbn does NOT unfold these Definitions on
   its own -- `unfold` the ops first (reflexivity forces delta). *)
Theorem i_squared       : cmul I I = cof (-1).               Proof. reflexivity. Qed.  (* i * i = -1 *)
Theorem conj_involution : forall z, cconj (cconj z) = z.     Proof. intros [r k]; unfold cconj; cbn; f_equal; lia. Qed. (* ~~~(2 3) = ~(2 3) *)
Theorem conj_add        : forall a b, cconj (cadd a b) = cadd (cconj a) (cconj b).
Proof. intros [ar ai] [br bi]; unfold cconj, cadd; cbn; f_equal; lia. Qed.
Theorem conj_mul        : forall a b, cconj (cmul a b) = cmul (cconj a) (cconj b).
Proof. intros [ar ai] [br bi]; unfold cconj, cmul; cbn; f_equal; ring. Qed.
Theorem real_embeds_add : forall r s, cadd (cof r) (cof s) = cof (r + s).  (* ~3 + ~5 = ~8 *)
Proof. intros; unfold cadd, cof; cbn; f_equal; lia. Qed.

(* the (re,im) lexicographic order -- matches the binary's total order on complex *)
Definition clt (a b : Zi) : Prop := re a < re b \/ (re a = re b /\ im a < im b).
Theorem i_above_zero : clt (cof 0) I.                       Proof. right; cbn; lia. Qed. (* ~(0 0) < i *)
Theorem conj_below   : forall z, im z > 0 -> clt (cconj z) z. (* (conj ~(2 3)) < ~(2 3) *)
Proof. intros [r k] H; right; unfold cconj in *; cbn in *; lia. Qed.

(* ============================================================ *)
(* within a band: text and chains order LEXICOGRAPHICALLY        *)
(* ============================================================ *)
(* The O-model (spec.v:272-274) collapses within-band order to one key; this is
   the refinement it deferred. `lex_lt` (above, where noms reuse it) is the same
   relation that carries text (lists of char-keys) and chains (lists of
   element-keys). Ground-truthed: "abc"<"abd", "ab"<"abc", '(1 2)<'(1 3). *)
Theorem str_lex_lt    : lex_lt [97;98;99] [97;98;100].   Proof. cbn; lia. Qed. (* "abc" < "abd" *)
Theorem str_prefix_lt : lex_lt [97;98] [97;98;99].       Proof. cbn; lia. Qed. (* "ab"  < "abc" -- a prefix is lower *)
Theorem chain_lex_lt  : lex_lt [1;2] [1;3].              Proof. cbn; lia. Qed. (* '(1 2) < '(1 3) -- same lex on element-keys *)
Theorem str_lex_irrefl : forall s, ~ lex_lt s s.
Proof. induction s as [|x s IH]; cbn; [auto | intros [H|[_ H]]; [lia | auto]]. Qed.
Theorem str_lex_trans : forall a b c, lex_lt a b -> lex_lt b c -> lex_lt a c.
Proof. induction a as [|x a IH]; intros [|y b] [|z c]; cbn; try tauto.
  intros [H1|[H1a H1b]] [H2|[H2a H2b]]; subst; try (left; lia).
  right; split; [reflexivity | eapply IH; eauto]. Qed.

(* ============================================================ *)
(* the complex net: a complex GEM nets ITSELF, phase intact      *)
(* ============================================================ *)
(* Closes on the Z[i] tier above. The real-fragment net (spec.v:174-179) said
   the complex extension was the next slice; here it is. `cnet z = z` (a gem is
   its own measure); `cnilp` gates on the REAL PART, so it agrees with the real
   `nilp` (net <= 0) on lifted reals, and phases CANCEL AS VECTORS.

   THE GATE IS NOT THE TOTAL ORDER, and this is the one place the two part. It
   was lexicographic (re, then im) until 2026-07-27, which made `i` true and `-i`
   false -- but conjugation is a field AUTOMORPHISM, so nothing intrinsic
   separates them, and C admits no order compatible with its arithmetic at all
   (in an ordered field every square is >= 0, yet i^2 = -1). Truth cannot rest on
   which root we named. So a PURE PHASE is BLUE: it has magnitude, but no
   positive amount. The lexicographic order stays exactly as it was for SORTING,
   which needs totality and must keep i and -i apart. Ground-truthed: !i,
   !~(0 -1), !~(-3 4), !!~(3 4). *)
Definition cnet (z : Zi) : Zi := z.
Definition cnilp (z : Zi) : bool := re z <=? 0.
Theorem cnet_self    : forall z, cnet z = z.                Proof. reflexivity. Qed.
Theorem i_blue       : cnilp I = true.                      Proof. reflexivity. Qed. (* !i: a pure phase *)
Theorem negi_blue    : cnilp (twin 0 (-1)) = true.          Proof. reflexivity. Qed. (* !~(0 -1) *)
Theorem neg_re_false : cnilp (twin (-3) 4) = true.          Proof. reflexivity. Qed. (* !~(-3 4): re<0 *)
Theorem pos_re_true  : cnilp (twin 3 4) = false.            Proof. reflexivity. Qed. (* !!~(3 4) *)
(* CONJUGATE-BLIND, which the old gate was not: i and -i now agree, as they must *)
Theorem conj_blind : forall z, cnilp z = cnilp (twin (re z) (- im z)).
Proof. intro z. unfold cnilp. reflexivity. Qed.
(* phases cancel AS VECTORS: ~(3 4) + ~(-3 4) nets ~(0 8) -- a pure phase, so BLUE *)
Theorem phase_vector_add : cadd (twin 3 4) (twin (-3) 4) = twin 0 8.  Proof. reflexivity. Qed.
Theorem phase_sum_blue   : cnilp (cadd (twin 3 4) (twin (-3) 4)) = true.  Proof. reflexivity. Qed.
(* the tie-in: cnilp agrees with the real nilp (net <= 0) on lifted reals *)
Theorem cnilp_real : forall r, cnilp (cof r) = (r <=? 0).
Proof. reflexivity. Qed.

(* ============================================================ *)
(* the crew as FACES of `top`                    *)
(* ============================================================ *)
(* ai (the language) is `top` -- the universal object (everything applies; top is vacuous).
   The apps are FACES of it, in dual pairs glued by DIFFERENT universal shapes. The
   SOURCE faces COMPOSE: read (charms->forms) runs THROUGH feel (the weaver, forms->top),
   so the charm face factors through the lisp face -- chars -> top <- forms, the two lanes
   converging on one core. The WORLD faces are a COPRODUCT: one i/o trunk forks into bao
   (local) and ain (net). Both axiom-free; the coproduct's uniqueness is stated
   POINTWISE, so no functional extensionality is needed. *)
Section Faces.
  Variables Top Charm Form Out Loc Net : Type.

  (* SOURCE faces -- the charm face (read) composes THROUGH the lisp face (feel) *)
  Variable read : Charm -> Form.            (* the read face: charms -> forms *)
  Variable feel : Form  -> Top.             (* the feel face: forms -> top (the weaver) *)
  Definition source (c : Charm) : Top := feel (read c).
  Theorem source_factors : forall c, source c = feel (read c).
  Proof. reflexivity. Qed.                  (* definitional: source = feel o read, lanes meet at top *)

  (* WORLD faces -- one trunk forks; the coproduct universal property *)
  Variable bao    : Loc -> Out.             (* the local face (the shell/bridge) *)
  Variable ain : Net -> Out.             (* the net face (the cat on the wire) *)
  Definition fork (x : Loc + Net) : Out :=
    match x with inl l => bao l | inr n => ain n end.
  Theorem world_inl : forall l, fork (inl l) = bao l.     Proof. reflexivity. Qed.
  Theorem world_inr : forall n, fork (inr n) = ain n.  Proof. reflexivity. Qed.
  (* any mediating map agreeing on the injections IS fork (pointwise) -- the UP *)
  Theorem world_unique :
    forall h : Loc + Net -> Out,
      (forall l, h (inl l) = bao l) -> (forall n, h (inr n) = ain n) ->
      forall x, h x = fork x.
  Proof. intros h Hl Hr x. destruct x as [l | n]; cbn; [apply Hl | apply Hr]. Qed.

  (* the object itself: everything is top (top is vacuous) *)
  Definition is_top (_ : Top) : Prop := True.
  Theorem top_vacuous : forall x, is_top x.  Proof. intro x; unfold is_top; constructor. Qed.
End Faces.

(* ============================================================ *)
(* axiom audit: every headline law, closed under NO assumptions *)
(* ============================================================ *)
(* The cheapest attack on any machine-checked claim is "grep for Axiom/admit".
   This file has none -- and rather than ask the reader to take that on faith,
   we make coqc PRINT it. Each line below must report "Closed under the global
   context" in the build log; anything else (a stray Axiom, an axiom dragged in
   from the stdlib -- funext, classic, proof-irrelevance) would show here. One
   representative theorem per pillar: saturation, the total order, the function
   semantics, the reduction layer, and the complex net. *)
Print Assumptions sat_clamps.        (* the net/saturation law (value side) *)
Print Assumptions app_appZ.          (* the Z lane IS the nat lane: gen.v shares this model *)
Print Assumptions numap_floor.       (* the count-law floor (count side, same $) *)
Print Assumptions gunit_times_l.     (* () is the identity of BOTH monoids, + and * *)
Print Assumptions unit_lt_zero.      (* () < 0: the floor below value-false (order side) *)
Print Assumptions le_total.          (* the total order *)
Print Assumptions eta_not_bridged.   (* = is alpha+structural on source, no further *)
Print Assumptions closure_beta_bridge. (* ... but value = sees the one beta ev already ran *)
Print Assumptions beta_id.           (* the reduction layer ev runs *)
Print Assumptions cnilp_real.        (* the complex net agrees with the real one *)
Print Assumptions world_unique.      (* the i/o faces are a coproduct (faces of top) *)
Print Assumptions source_factors.    (* the source faces compose (source = feel o read) *)
