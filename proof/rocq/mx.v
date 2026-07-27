(* proof/rocq/mx.v -- GENERATED from love.c's dispatch matrices by tools/mx2coq.l
   (via tools/mxdump.c, a TU that includes the core whole and prints the static
   tables as data). Do not edit; regenerate with `make test_mx`.

   The +/* dispatch matrices, their SHAPE machine-checked -- what the narrative
   states as prose: every kind collapses to a BAND (the partition is derived
   from the dumped tables by row+column equality, both ops at once), the 256-
   cell tables factor through the band quotient with nothing left over, the
   dispatch COMMUTES (orientation lives in the lane fn, never the table), and
   the diagonal reads the lattice: one lane per band, the algebra ladder.
   KMint is its own band, the unit lane whole: the dispatchers early-out a
   bare mint as the UNIT before indexing (the fast path), and the matrix says
   the same thing (lvm_bin_unit), so nothing rests on the early-out and the
   theorems close by vm_compute over the WHOLE square. *)
From Stdlib Require Import List Arith.
Import ListNotations.

Inductive kind := KMint | KNom | KCharm | KWide | KFlo | KCplx | KBig | KVec | KArrZ | KArrR | KArrC | KArrO | KString | KChain | KMap | KHot.
Inductive lane := Lbin_unit | Ladd_string | Ladd_seq | Laddh | Laddn | Lzero | Lmul_rep | Lmulh | Lmuln | Lmul_cart.
Definition lane_eqb (x y : lane) : bool := match x, y with Lbin_unit, Lbin_unit => true | Ladd_string, Ladd_string => true | Ladd_seq, Ladd_seq => true | Laddh, Laddh => true | Laddn, Laddn => true | Lzero, Lzero => true | Lmul_rep, Lmul_rep => true | Lmulh, Lmulh => true | Lmuln, Lmuln => true | Lmul_cart, Lmul_cart => true | _, _ => false end.

Definition addmx (a b : kind) : lane :=
 match a with
 | KMint => match b with KMint => Lbin_unit | KNom => Lbin_unit | KCharm => Lbin_unit | KWide => Lbin_unit | KFlo => Lbin_unit | KCplx => Lbin_unit | KBig => Lbin_unit | KVec => Lbin_unit | KArrZ => Lbin_unit | KArrR => Lbin_unit | KArrC => Lbin_unit | KArrO => Lbin_unit | KString => Lbin_unit | KChain => Lbin_unit | KMap => Lbin_unit | KHot => Lbin_unit end
 | KNom => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Ladd_string | KWide => Ladd_string | KFlo => Ladd_string | KCplx => Ladd_string | KBig => Ladd_string | KVec => Ladd_string | KArrZ => Ladd_string | KArrR => Ladd_string | KArrC => Ladd_string | KArrO => Ladd_string | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KCharm => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Laddn | KWide => Laddn | KFlo => Laddn | KCplx => Laddn | KBig => Laddn | KVec => Laddn | KArrZ => Laddn | KArrR => Laddn | KArrC => Laddn | KArrO => Laddn | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KWide => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Laddn | KWide => Laddn | KFlo => Laddn | KCplx => Laddn | KBig => Laddn | KVec => Laddn | KArrZ => Laddn | KArrR => Laddn | KArrC => Laddn | KArrO => Laddn | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KFlo => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Laddn | KWide => Laddn | KFlo => Laddn | KCplx => Laddn | KBig => Laddn | KVec => Laddn | KArrZ => Laddn | KArrR => Laddn | KArrC => Laddn | KArrO => Laddn | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KCplx => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Laddn | KWide => Laddn | KFlo => Laddn | KCplx => Laddn | KBig => Laddn | KVec => Laddn | KArrZ => Laddn | KArrR => Laddn | KArrC => Laddn | KArrO => Laddn | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KBig => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Laddn | KWide => Laddn | KFlo => Laddn | KCplx => Laddn | KBig => Laddn | KVec => Laddn | KArrZ => Laddn | KArrR => Laddn | KArrC => Laddn | KArrO => Laddn | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KVec => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Laddn | KWide => Laddn | KFlo => Laddn | KCplx => Laddn | KBig => Laddn | KVec => Laddn | KArrZ => Laddn | KArrR => Laddn | KArrC => Laddn | KArrO => Laddn | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KArrZ => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Laddn | KWide => Laddn | KFlo => Laddn | KCplx => Laddn | KBig => Laddn | KVec => Laddn | KArrZ => Laddn | KArrR => Laddn | KArrC => Laddn | KArrO => Laddn | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KArrR => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Laddn | KWide => Laddn | KFlo => Laddn | KCplx => Laddn | KBig => Laddn | KVec => Laddn | KArrZ => Laddn | KArrR => Laddn | KArrC => Laddn | KArrO => Laddn | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KArrC => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Laddn | KWide => Laddn | KFlo => Laddn | KCplx => Laddn | KBig => Laddn | KVec => Laddn | KArrZ => Laddn | KArrR => Laddn | KArrC => Laddn | KArrO => Laddn | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KArrO => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Laddn | KWide => Laddn | KFlo => Laddn | KCplx => Laddn | KBig => Laddn | KVec => Laddn | KArrZ => Laddn | KArrR => Laddn | KArrC => Laddn | KArrO => Laddn | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KString => match b with KMint => Lbin_unit | KNom => Ladd_string | KCharm => Ladd_string | KWide => Ladd_string | KFlo => Ladd_string | KCplx => Ladd_string | KBig => Ladd_string | KVec => Ladd_string | KArrZ => Ladd_string | KArrR => Ladd_string | KArrC => Ladd_string | KArrO => Ladd_string | KString => Ladd_string | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KChain => match b with KMint => Lbin_unit | KNom => Ladd_seq | KCharm => Ladd_seq | KWide => Ladd_seq | KFlo => Ladd_seq | KCplx => Ladd_seq | KBig => Ladd_seq | KVec => Ladd_seq | KArrZ => Ladd_seq | KArrR => Ladd_seq | KArrC => Ladd_seq | KArrO => Ladd_seq | KString => Ladd_seq | KChain => Ladd_seq | KMap => Laddh | KHot => Laddh end
 | KMap => match b with KMint => Lbin_unit | KNom => Laddh | KCharm => Laddh | KWide => Laddh | KFlo => Laddh | KCplx => Laddh | KBig => Laddh | KVec => Laddh | KArrZ => Laddh | KArrR => Laddh | KArrC => Laddh | KArrO => Laddh | KString => Laddh | KChain => Laddh | KMap => Laddh | KHot => Laddh end
 | KHot => match b with KMint => Lbin_unit | KNom => Laddh | KCharm => Laddh | KWide => Laddh | KFlo => Laddh | KCplx => Laddh | KBig => Laddh | KVec => Laddh | KArrZ => Laddh | KArrR => Laddh | KArrC => Laddh | KArrO => Laddh | KString => Laddh | KChain => Laddh | KMap => Laddh | KHot => Laddh end
 end.
Definition mulmx (a b : kind) : lane :=
 match a with
 | KMint => match b with KMint => Lbin_unit | KNom => Lbin_unit | KCharm => Lbin_unit | KWide => Lbin_unit | KFlo => Lbin_unit | KCplx => Lbin_unit | KBig => Lbin_unit | KVec => Lbin_unit | KArrZ => Lbin_unit | KArrR => Lbin_unit | KArrC => Lbin_unit | KArrO => Lbin_unit | KString => Lbin_unit | KChain => Lbin_unit | KMap => Lbin_unit | KHot => Lbin_unit end
 | KNom => match b with KMint => Lbin_unit | KNom => Lzero | KCharm => Lmul_rep | KWide => Lmul_rep | KFlo => Lmul_rep | KCplx => Lmul_rep | KBig => Lmul_rep | KVec => Lmul_rep | KArrZ => Lmul_rep | KArrR => Lmul_rep | KArrC => Lmul_rep | KArrO => Lmul_rep | KString => Lzero | KChain => Lzero | KMap => Lmulh | KHot => Lmulh end
 | KCharm => match b with KMint => Lbin_unit | KNom => Lmul_rep | KCharm => Lmuln | KWide => Lmuln | KFlo => Lmuln | KCplx => Lmuln | KBig => Lmuln | KVec => Lmuln | KArrZ => Lmuln | KArrR => Lmuln | KArrC => Lmuln | KArrO => Lmuln | KString => Lmul_rep | KChain => Lmul_rep | KMap => Lmulh | KHot => Lmulh end
 | KWide => match b with KMint => Lbin_unit | KNom => Lmul_rep | KCharm => Lmuln | KWide => Lmuln | KFlo => Lmuln | KCplx => Lmuln | KBig => Lmuln | KVec => Lmuln | KArrZ => Lmuln | KArrR => Lmuln | KArrC => Lmuln | KArrO => Lmuln | KString => Lmul_rep | KChain => Lmul_rep | KMap => Lmulh | KHot => Lmulh end
 | KFlo => match b with KMint => Lbin_unit | KNom => Lmul_rep | KCharm => Lmuln | KWide => Lmuln | KFlo => Lmuln | KCplx => Lmuln | KBig => Lmuln | KVec => Lmuln | KArrZ => Lmuln | KArrR => Lmuln | KArrC => Lmuln | KArrO => Lmuln | KString => Lmul_rep | KChain => Lmul_rep | KMap => Lmulh | KHot => Lmulh end
 | KCplx => match b with KMint => Lbin_unit | KNom => Lmul_rep | KCharm => Lmuln | KWide => Lmuln | KFlo => Lmuln | KCplx => Lmuln | KBig => Lmuln | KVec => Lmuln | KArrZ => Lmuln | KArrR => Lmuln | KArrC => Lmuln | KArrO => Lmuln | KString => Lmul_rep | KChain => Lmul_rep | KMap => Lmulh | KHot => Lmulh end
 | KBig => match b with KMint => Lbin_unit | KNom => Lmul_rep | KCharm => Lmuln | KWide => Lmuln | KFlo => Lmuln | KCplx => Lmuln | KBig => Lmuln | KVec => Lmuln | KArrZ => Lmuln | KArrR => Lmuln | KArrC => Lmuln | KArrO => Lmuln | KString => Lmul_rep | KChain => Lmul_rep | KMap => Lmulh | KHot => Lmulh end
 | KVec => match b with KMint => Lbin_unit | KNom => Lmul_rep | KCharm => Lmuln | KWide => Lmuln | KFlo => Lmuln | KCplx => Lmuln | KBig => Lmuln | KVec => Lmuln | KArrZ => Lmuln | KArrR => Lmuln | KArrC => Lmuln | KArrO => Lmuln | KString => Lmul_rep | KChain => Lmul_rep | KMap => Lmulh | KHot => Lmulh end
 | KArrZ => match b with KMint => Lbin_unit | KNom => Lmul_rep | KCharm => Lmuln | KWide => Lmuln | KFlo => Lmuln | KCplx => Lmuln | KBig => Lmuln | KVec => Lmuln | KArrZ => Lmuln | KArrR => Lmuln | KArrC => Lmuln | KArrO => Lmuln | KString => Lmul_rep | KChain => Lmul_rep | KMap => Lmulh | KHot => Lmulh end
 | KArrR => match b with KMint => Lbin_unit | KNom => Lmul_rep | KCharm => Lmuln | KWide => Lmuln | KFlo => Lmuln | KCplx => Lmuln | KBig => Lmuln | KVec => Lmuln | KArrZ => Lmuln | KArrR => Lmuln | KArrC => Lmuln | KArrO => Lmuln | KString => Lmul_rep | KChain => Lmul_rep | KMap => Lmulh | KHot => Lmulh end
 | KArrC => match b with KMint => Lbin_unit | KNom => Lmul_rep | KCharm => Lmuln | KWide => Lmuln | KFlo => Lmuln | KCplx => Lmuln | KBig => Lmuln | KVec => Lmuln | KArrZ => Lmuln | KArrR => Lmuln | KArrC => Lmuln | KArrO => Lmuln | KString => Lmul_rep | KChain => Lmul_rep | KMap => Lmulh | KHot => Lmulh end
 | KArrO => match b with KMint => Lbin_unit | KNom => Lmul_rep | KCharm => Lmuln | KWide => Lmuln | KFlo => Lmuln | KCplx => Lmuln | KBig => Lmuln | KVec => Lmuln | KArrZ => Lmuln | KArrR => Lmuln | KArrC => Lmuln | KArrO => Lmuln | KString => Lmul_rep | KChain => Lmul_rep | KMap => Lmulh | KHot => Lmulh end
 | KString => match b with KMint => Lbin_unit | KNom => Lzero | KCharm => Lmul_rep | KWide => Lmul_rep | KFlo => Lmul_rep | KCplx => Lmul_rep | KBig => Lmul_rep | KVec => Lmul_rep | KArrZ => Lmul_rep | KArrR => Lmul_rep | KArrC => Lmul_rep | KArrO => Lmul_rep | KString => Lzero | KChain => Lzero | KMap => Lmulh | KHot => Lmulh end
 | KChain => match b with KMint => Lbin_unit | KNom => Lzero | KCharm => Lmul_rep | KWide => Lmul_rep | KFlo => Lmul_rep | KCplx => Lmul_rep | KBig => Lmul_rep | KVec => Lmul_rep | KArrZ => Lmul_rep | KArrR => Lmul_rep | KArrC => Lmul_rep | KArrO => Lmul_rep | KString => Lzero | KChain => Lmul_cart | KMap => Lmulh | KHot => Lmulh end
 | KMap => match b with KMint => Lbin_unit | KNom => Lmulh | KCharm => Lmulh | KWide => Lmulh | KFlo => Lmulh | KCplx => Lmulh | KBig => Lmulh | KVec => Lmulh | KArrZ => Lmulh | KArrR => Lmulh | KArrC => Lmulh | KArrO => Lmulh | KString => Lmulh | KChain => Lmulh | KMap => Lmulh | KHot => Lmulh end
 | KHot => match b with KMint => Lbin_unit | KNom => Lmulh | KCharm => Lmulh | KWide => Lmulh | KFlo => Lmulh | KCplx => Lmulh | KBig => Lmulh | KVec => Lmulh | KArrZ => Lmulh | KArrR => Lmulh | KArrC => Lmulh | KArrO => Lmulh | KString => Lmulh | KChain => Lmulh | KMap => Lmulh | KHot => Lmulh end
 end.
Definition band (k : kind) : nat := match k with KMint => 0 | KNom | KString => 1 | KCharm | KWide | KFlo | KCplx | KBig | KVec | KArrZ | KArrR | KArrC | KArrO => 2 | KChain => 3 | KMap | KHot => 4 end.
Definition rk : list kind := [KMint; KNom; KCharm; KWide; KFlo; KCplx; KBig; KVec; KArrZ; KArrR; KArrC; KArrO; KString; KChain; KMap; KHot].

Definition addb : list (list lane) := [
  [Lbin_unit; Lbin_unit; Lbin_unit; Lbin_unit; Lbin_unit];
  [Lbin_unit; Ladd_string; Ladd_string; Ladd_seq; Laddh];
  [Lbin_unit; Ladd_string; Laddn; Ladd_seq; Laddh];
  [Lbin_unit; Ladd_seq; Ladd_seq; Ladd_seq; Laddh];
  [Lbin_unit; Laddh; Laddh; Laddh; Laddh]
 ].
Definition mulb : list (list lane) := [
  [Lbin_unit; Lbin_unit; Lbin_unit; Lbin_unit; Lbin_unit];
  [Lbin_unit; Lzero; Lmul_rep; Lzero; Lmulh];
  [Lbin_unit; Lmul_rep; Lmuln; Lmul_rep; Lmulh];
  [Lbin_unit; Lzero; Lmul_rep; Lmul_cart; Lmulh];
  [Lbin_unit; Lmulh; Lmulh; Lmulh; Lmulh]
 ].
Definition addband (x y : nat) : lane := nth y (nth x addb []) Lbin_unit.
Definition mulband (x y : nat) : lane := nth y (nth x mulb []) Lbin_unit.

Theorem add_factors_through_bands : forallb (fun a => forallb (fun b => lane_eqb (addmx a b) (addband (band a) (band b))) rk) rk = true.
Proof. vm_compute. reflexivity. Qed.
Theorem mul_factors_through_bands : forallb (fun a => forallb (fun b => lane_eqb (mulmx a b) (mulband (band a) (band b))) rk) rk = true.
Proof. vm_compute. reflexivity. Qed.
Theorem add_dispatch_commutes : forallb (fun a => forallb (fun b => lane_eqb (addmx a b) (addmx b a)) rk) rk = true.
Proof. vm_compute. reflexivity. Qed.
Theorem mul_dispatch_commutes : forallb (fun a => forallb (fun b => lane_eqb (mulmx a b) (mulmx b a)) rk) rk = true.
Proof. vm_compute. reflexivity. Qed.

(* the lattice, read off the diagonal: one add/mul lane pair per band *)
Theorem the_diagonal_reads_the_lattice :
  map (fun k => (addmx k k, mulmx k k)) [KMint; KNom; KCharm; KChain; KMap]
  = [(Lbin_unit, Lbin_unit); (Ladd_string, Lzero); (Laddn, Lmuln); (Ladd_seq, Lmul_cart); (Laddh, Lmulh)].
Proof. vm_compute. reflexivity. Qed.

Print Assumptions add_factors_through_bands.   (* must stay "Closed under the global context" *)
Print Assumptions mul_dispatch_commutes.
