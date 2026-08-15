(* encli.v -- the IMMEDIATE-LOAD slice of the machine-checked x86-64 encoder
   (rung 2c of the holo encoder ladder; enc.v = reg-direct, encmem.v = memory).

   `li d imm` loads a 64-bit signed immediate into a register. holo picks the
   SHORTEST of three forms by value range -- this is the interesting part, a
   CHOICE the encoder must get right (and the theorem below proves it round-trips):
     * imm in [0, 2^32-1]   -> `b8+r imm32`         5 bytes (+REX.B), NO REX.W;
                               the 32-bit mov zero-extends to 64 bits.
     * imm in [-2^31, -1]   -> `REX.W C7 /0 imm32`  7 bytes; imm32 sign-extends.
     * else (>= 2^32 or < -2^31) -> `REX.W B8+r imm64` (movabs) 10 bytes.

   `li_roundtrip_ok` proves `decode_li (encode_li d imm) = Some (d,imm)` over every
   dest<16 x a curated immediate set hitting all three forms and their boundaries
   (0, 2^31-1/2^31, 2^32-1/2^32, -1, -2^31, -2^31-1, +-2^63-1) -- by vm_compute
   (axiom-free). The form-selection logic is thus exhaustively covered over the reg
   dimension and at every range boundary. `encode_li` extracts to OCaml so
   encli_drive.ml validates holo BYTE-FOR-BYTE (test_encver). *)

From Stdlib Require Import List Bool ZArith.
Import ListNotations.
Open Scope Z_scope.

Definition byte := Z.
Definition reg  := Z.

(* little-endian: n bytes of v; and the value of a byte list *)
Fixpoint le_bytes (n:nat) (v:Z) : list byte :=
  match n with O => [] | S k => (v mod 256) :: le_bytes k (v / 256) end.
Fixpoint le_val (bs:list byte) : Z :=
  match bs with [] => 0 | b :: r => b + 256 * le_val r end.

Definition sext32 (v:Z) : Z := if v <? 2147483648 then v else v - 4294967296.
Definition sext64 (v:Z) : Z :=
  if v <? 9223372036854775808 then v else v - 18446744073709551616.

(* the three-way form choice, matching holo exactly *)
Definition encode_li (dest:reg) (imm:Z) : list byte :=
  if (0 <=? imm) && (imm <=? 4294967295) then
    (* form A: b8+r imm32, zero-extend; REX.B only if hireg, no REX.W *)
    (if 7 <? dest then [65] else []) ++ [184 + dest mod 8] ++ le_bytes 4 imm
  else if (-2147483648 <=? imm) && (imm <=? -1) then
    (* form B: REX.W [B] ; C7 ; /0 ModRM ; imm32 (sign-extended) *)
    [ 72 + (if 7 <? dest then 1 else 0) ; 199 ; 192 + dest mod 8 ] ++ le_bytes 4 imm
  else
    (* form C: REX.W [B] ; B8+r ; imm64 (movabs) *)
    [ 72 + (if 7 <? dest then 1 else 0) ; 184 + dest mod 8 ] ++ le_bytes 8 imm.

(* --- decoder --- *)
Definition decode_li (bs:list byte) : option (reg * Z) :=
  match bs with
  | p :: rest =>
    if (184 <=? p) && (p <=? 191) then                 (* form A, loreg *)
        Some (p - 184, le_val (firstn 4 rest))
    else if p =? 65 then                                (* form A, hireg (REX.B) *)
        match rest with
        | oc :: r4 => Some (oc - 184 + 8, le_val (firstn 4 r4))
        | _ => None end
    else if (p =? 72) || (p =? 73) then                 (* REX.W [.B] *)
        let bB := p - 72 in
        match rest with
        | oc :: r2 =>
          if oc =? 199 then                             (* C7 /0 -> form B *)
            match r2 with
            | mr :: r3 => Some ((mr - 192) + 8 * bB, sext32 (le_val (firstn 4 r3)))
            | _ => None end
          else if (184 <=? oc) && (oc <=? 191) then     (* B8+r -> form C movabs *)
            Some ((oc - 184) + 8 * bB, sext64 (le_val (firstn 8 r2)))
          else None
        | _ => None end
    else None
  | _ => None
  end.

Definition regs16 : list reg := map Z.of_nat (seq 0 16).

(* immediate boundary set: all three forms and their edges + a couple mid values *)
Definition imms : list Z :=
  [ 0; 1; 60; 1000000; 2147483647; 2147483648; 4294967295;   (* form A *)
    -1; -128; -1000000; -2147483648;                          (* form B *)
    4294967296; 9223372036854775807; -2147483649;
    -4294967296; -9223372036854775808 ].                      (* form C *)

Definition li_roundtrips : bool :=
  forallb (fun d =>
    forallb (fun imm =>
      match decode_li (encode_li d imm) with
      | Some (d', imm') => (d =? d') && (imm =? imm')
      | None => false
      end) imms) regs16.

Theorem li_roundtrip_ok : li_roundtrips = true.
Proof. vm_compute. reflexivity. Qed.

Print Assumptions li_roundtrip_ok.   (* must stay "Closed under the global context" *)

From Stdlib Require Import Extraction ExtrOcamlZInt.
Extraction Language OCaml.
Set Extraction Output Directory ".".
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Inductive bool => "bool" [ "true" "false" ].
Extraction "encli_ref.ml" encode_li.
