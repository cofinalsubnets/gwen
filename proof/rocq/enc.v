(* enc.v -- a machine-checked x86-64 encoder for the register-direct core:
   64-bit mov + the ALU reg-reg ops (add/sub/and/or/xor/cmp), which all share
   the one REX + opcode + ModRM(mod=11) shape. The point of interest is the
   REX/ModRM BIT-PACKING -- the fiddliest, most error-prone part of x86 encoding
   (which register bit lands in REX.R vs REX.B, the mod=11 register-direct form).

   `roundtrip_ok` proves `decode (encode o d s) = Some (o,d,s)` over the FULL
   16x16 register matrix x all 7 ops. The register domain is finite, so the
   exhaustive check IS a complete proof -- computed by vm_compute (gen.v style),
   axiom-free (Print Assumptions below must stay "Closed under the global
   context"). `encode` then extracts to OCaml so enc_drive.ml can validate holo's
   own encoder BYTE-FOR-BYTE against this proven reference (test_encver).

   Scope: this is the register-direct core. Immediate loads and memory operands
   (SIB, the rbp-forced-disp / rsp-needs-SIB quirks) are the next slices. The
   theorem proves the encode/decode PAIR is internally consistent (invertible);
   grounding the decode model against the real ISA is the objdump/llvm-mc fuzz
   rung (test/holo/fuzz) -- the two rungs compose. *)

From Stdlib Require Import List Arith Bool.
Import ListNotations.

Definition byte := nat.
Definition reg  := nat.

Inductive op := Omov | Oadd | Osub | Oand | Oor | Oxor | Ocmp.

(* primary opcode for the "r/m64, r64" (/r) direction: r/m = dest, reg = src *)
Definition opcode (o:op) : byte :=
  match o with
  | Omov => 137 | Oadd => 1  | Osub => 41
  | Oand => 33  | Oor  => 9  | Oxor => 49 | Ocmp => 57
  end.

Definition op_eqb (a b:op) : bool :=
  match a, b with
  | Omov,Omov | Oadd,Oadd | Osub,Osub | Oand,Oand
  | Oor,Oor | Oxor,Oxor | Ocmp,Ocmp => true
  | _,_ => false
  end.

(* REX prefix 0100_WRXB: W=1 (64-bit) always; X=0; R set iff src>=8 (its bit
   extends the ModRM.reg field), B set iff dest>=8 (extends ModRM.rm). *)
Definition rex (dest src:reg) : byte :=
  64 + 8 + (if 7 <? src then 4 else 0) + (if 7 <? dest then 1 else 0).

(* ModRM: mod=11 (register direct), reg = src low 3 bits, rm = dest low 3 bits *)
Definition modrm (dest src:reg) : byte :=
  192 + (src mod 8) * 8 + (dest mod 8).

Definition encode (o:op) (dest src:reg) : list byte :=
  [ rex dest src ; opcode o ; modrm dest src ].

(* --- decoder (a small auditable model of the same encoding rules) --- *)
Definition decode_op (b:byte) : option op :=
  if b =? 137 then Some Omov else if b =? 1  then Some Oadd
  else if b =? 41 then Some Osub else if b =? 33 then Some Oand
  else if b =? 9  then Some Oor  else if b =? 49 then Some Oxor
  else if b =? 57 then Some Ocmp else None.

Definition decode (bs:list byte) : option (op * reg * reg) :=
  match bs with
  | [r; oc; mr] =>
    match decode_op oc with
    | None => None
    | Some o =>
      let bB := r mod 2 in           (* REX.B  -> dest high bit *)
      let bR := (r / 4) mod 2 in     (* REX.R  -> src  high bit *)
      let m  := mr - 192 in
      let rm_low  := m mod 8 in
      let reg_low := (m / 8) mod 8 in
      Some (o, rm_low + 8 * bB, reg_low + 8 * bR)
    end
  | _ => None
  end.

Definition all_ops : list op := [Omov;Oadd;Osub;Oand;Oor;Oxor;Ocmp].

(* exhaustive round-trip over the finite domain: every op, every dest, src < 16 *)
Definition roundtrips_all : bool :=
  forallb (fun o =>
    forallb (fun d =>
      forallb (fun s =>
        match decode (encode o d s) with
        | Some (o', d', s') => op_eqb o o' && (d =? d') && (s =? s')
        | None => false
        end) (seq 0 16)) (seq 0 16)) all_ops.

Theorem roundtrip_ok : roundtrips_all = true.
Proof. vm_compute. reflexivity. Qed.

Print Assumptions roundtrip_ok.   (* must stay "Closed under the global context" *)

(* --- extraction: nat -> int (the standard pragmatic mapping for an extracted
   test driver; the proof above stays over real nat). Only `encode` + the op
   list/constructors are needed by the driver. --- *)
From Stdlib Require Import Extraction.
Extraction Language OCaml.
Set Extraction Output Directory ".".
Extract Inductive nat => "int"
  [ "0" "(fun x -> x + 1)" ]
  "(fun zero succ n -> if n = 0 then zero () else succ (n - 1))".
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inlined Constant Nat.add     => "(+)".
Extract Inlined Constant Nat.mul     => "( * )".
Extract Inlined Constant Nat.modulo  => "(mod)".
Extract Inlined Constant Nat.ltb     => "(<)".
Extraction "enc_ref.ml" encode all_ops opcode.
