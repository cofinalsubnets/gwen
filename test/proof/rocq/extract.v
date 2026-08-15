(* extract.v -- the differential oracle's reference normalizer, EXTRACTED.

   test/oracle.l hand-transcribes spec.v's subst into ai; this closes that gap.
   Here the executable normalizer is built directly on spec.v's PROVEN `subst`
   and `shift` (Require Import spec), then extracted to OCaml. So the reference
   the fuzzer runs is, up to the standard nat->int extraction mapping, the very
   definitions the spec.v theorems are about -- machine-checked end to end.

   `nf` mirrors test/oracle.l's evaluator exactly: ai's strategy -- N-ary
   closures, CBV, weak (no reduction under a binder), saturating (a beta step
   fires only when all n args are present; an under-applied closure is held
   unreduced). The beta primitive it uses IS spec.v's st_beta (lemma below). *)

Require Import spec.
From Stdlib Require Import List.
Import ListNotations.

(* leading-lambda count = ai closure arity *)
Fixpoint arity (t : tm) : nat :=
  match t with Lam b => S (arity b) | _ => 0 end.

(* re-apply a spine of arg values, left to right *)
Definition rebuild (t : tm) (args : list tm) : tm :=
  fold_left (fun acc a => App acc a) args t.

(* fire n unary betas (t is a Lam of arity >= n); subst is spec.v's *)
Fixpoint apply_n (t : tm) (args : list tm) (n : nat) : tm :=
  match n, args, t with
  | S n', a :: rest, Lam b => apply_n (subst 0 a b) rest n'
  | _, _, _ => t
  end.

(* CBV / n-ary / weak / saturating evaluation, fuel-bounded *)
Fixpoint evs (fuel : nat) (t : tm) (args : list tm) : tm :=
  match fuel with
  | 0 => rebuild t args
  | S f =>
    match t with
    | App g a => let av := evs f a [] in evs f g (av :: args)
    | Lam b =>
        match args with
        | [] => t
        | _ => if Nat.ltb (length args) (arity t)
               then rebuild t args
               else evs f (apply_n t (firstn (arity t) args) (arity t))
                          (skipn (arity t) args)
        end
    | Var _ => rebuild t args
    end
  end.

Definition nf (fuel : nat) (t : tm) : tm := evs fuel t [].

(* the machine-checked LINK: the beta step `nf` performs (via apply_n -> subst
   0 a b) is exactly spec.v's proven st_beta. So the executable reference's one
   reduction primitive is the relation the theorems reason about. (Full
   normalization soundness vs `step` is the Tier-1 confluence project; this is
   the local correspondence, axiom-free.) *)
Theorem beta_is_step : forall b a, step (App (Lam b) a) (subst 0 a b).
Proof. intros; apply st_beta. Qed.

Print Assumptions beta_is_step.   (* must stay "Closed under the global context" *)

(* --- extraction --- nat -> int for a usable executable (the proofs above stay
   over real nat; this mapping is the standard pragmatic choice for an extracted
   test driver, not a proven-correct compiler). *)
From Stdlib Require Import Extraction.
Extraction Language OCaml.
(* keep the .ml next to this file (cwd); silences the 9.x default-output-dir warning *)
Set Extraction Output Directory ".".
Extract Inductive nat => "int"
  [ "0" "(fun x -> x + 1)" ]
  "(fun zero succ n -> if n = 0 then zero () else succ (n - 1))".
Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inlined Constant Nat.ltb => "(<)".
Extraction "normalizer.ml" nf arity.
