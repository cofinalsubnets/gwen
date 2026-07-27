(* big.v -- the bignum lane's reference: Coq's binary Z, EXTRACTED, with a
   PROVEN decimal codec to carry values across the process boundary.

   love's big rep (native limbs, Karatsuba, Knuth-D -- the scariest pure
   arithmetic in love.c) is held here against an independent, machine-checked
   implementation: stdlib Z, whose ring, quot/rem and gcd laws are theorems
   (two are restated below as named witnesses for the audit). The one piece
   this file must PROVE rather than import is the seam itself: the decimal
   print/parse pair the oracle speaks through. parse_print closes it, so a
   value crossing the boundary in either direction is exact -- the extracted
   reference computes on precisely the numbers love printed, and the strings
   it emits re-read to precisely the Z it computed.

   big_drive.ml generates operands (edge charms/suns/bigs + random digit
   strings crossing the limb, Karatsuba and Knuth-D thresholds), computes each
   op with the extracted reference, and emits a love program of decimal string
   comparisons -- so love's READER, limb arithmetic, and PRINTER are all three
   under test against the proven codec. love's conventions, probed and
   matched: // is Z.quot (truncated), % is Z.rem (the dividend's sign), gcd is
   nonnegative, < answers 1|0; division by zero escapes to the float lane and
   stays out of the cases. *)

From Stdlib Require Import ZArith Decimal DecimalString DecimalZ String Ascii.
Open Scope Z_scope.

Definition print (z : Z) : string := DecimalString.NilZero.string_of_int (Z.to_int z).
Definition parse (s : string) : option Z :=
  option_map Z.of_int (DecimalString.NilZero.int_of_string s).

(* Z.to_int lands in normal form (DecimalZ.to_of + of_to), and a normal form
   always carries at least one digit -- the side conditions NilZero.isi wants *)
Lemma to_int_no_nil (z : Z) :
  Z.to_int z <> Decimal.Pos Decimal.Nil /\ Z.to_int z <> Decimal.Neg Decimal.Nil.
Proof.
  assert (E : Z.to_int z = Decimal.norm (Z.to_int z)).
  { pose proof (DecimalZ.to_of (Z.to_int z)) as H.
    rewrite (DecimalZ.of_to z) in H. exact H. }
  rewrite E. destruct (Z.to_int z) as [u|u]; cbn; unfold Decimal.unorm;
    destruct (Decimal.nzhead u); split; discriminate.
Qed.

(* the SEAM theorem: what the oracle prints, it re-reads, exactly *)
Theorem parse_print (z : Z) : parse (print z) = Some z.
Proof.
  unfold parse, print.
  destruct (to_int_no_nil z) as [H1 H2].
  rewrite (DecimalString.NilZero.isi _ H1 H2). cbn.
  now rewrite DecimalZ.of_to.
Qed.

(* witnesses that the reference ops carry their laws (stdlib theorems, named
   here so the audit below covers the op suite, not just the codec) *)
Theorem quot_rem_law : forall a b, b <> 0 -> a = b * Z.quot a b + Z.rem a b.
Proof. intros a b Hb. now apply Z.quot_rem. Qed.

Theorem gcd_law : forall a b,
  (Z.gcd a b | a) /\ (Z.gcd a b | b) /\ 0 <= Z.gcd a b.
Proof.
  intros; repeat split;
    [apply Z.gcd_divide_l | apply Z.gcd_divide_r | apply Z.gcd_nonneg].
Qed.

Print Assumptions parse_print.   (* must stay "Closed under the global context" *)
Print Assumptions quot_rem_law.
Print Assumptions gcd_law.

(* --- extraction --- Z stays the binary inductive (no native-int mapping: the
   whole point is an independent rep); strings extract to OCaml char lists *)
From Stdlib Require Import Extraction ExtrOcamlString.
Extraction Language OCaml.
Set Extraction Output Directory ".".
Definition zadd := Z.add.    Definition zsub := Z.sub.    Definition zmul := Z.mul.
Definition zquot := Z.quot.  Definition zrem := Z.rem.    Definition zgcd := Z.gcd.
Definition zabs := Z.abs.    Definition zltb := Z.ltb.
Extraction "bigref.ml" parse print zadd zsub zmul zquot zrem zgcd zabs zltb.
