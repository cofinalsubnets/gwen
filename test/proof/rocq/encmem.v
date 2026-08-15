(* encmem.v -- the MEMORY-OPERAND slice of the machine-checked x86-64 encoder
   (rung 2b of the holo encoder ladder; enc.v is the register-direct core 2a).

   Covers the 64-bit base+displacement load/store: `ld d base off` (mov r64,[mem],
   opcode 8B /r) and `st base off s` (mov [mem],r64, 89 /r). This is where the
   fiddly encoding lives -- the ModRM r/m = memory form, with all of x86's quirks:
     * rsp / r12 (rm low 3 = 100) cannot sit in ModRM.rm directly -> a SIB byte
       (scale 0, index 100 = none, base = the reg) must follow;
     * rbp / r13 (rm low 3 = 101) has no mod=00 form (that slot means RIP-relative)
       -> a displacement is FORCED even when off = 0 (mod=01, disp8 = 0);
     * mod / disp size is chosen minimally: none (off=0, non-bp), disp8 (-128..127),
       else disp32 little-endian;
     * REX.R extends the data register, REX.B extends the base (whichever of
       ModRM.rm or SIB.base holds it).

   `mem_roundtrip_ok` proves `decode_mem (encode_mem o d b off) = Some (o,d,b,off)`
   over every op x data<16 x base<16 x a boundary offset set that exercises each
   mod class and the bp/sib interactions -- by vm_compute (gen.v style, axiom-free).
   The base x reg selection logic is thus exhaustively covered; offsets are sampled
   at the encoding boundaries (0, +-1, +-127/128/129, 255, disp32 limbs). `encode_mem`
   extracts to OCaml so encmem_drive.ml validates holo BYTE-FOR-BYTE over a wide
   offset range (test_encver). *)

From Stdlib Require Import List Bool ZArith.
Import ListNotations.
Open Scope Z_scope.

Definition byte := Z.
Definition reg  := Z.

Inductive memop := Mld | Mst.
Definition mopcode (o:memop) : byte := match o with Mld => 139 | Mst => 137 end.
Definition mop_eqb (a b:memop) : bool :=
  match a, b with Mld,Mld | Mst,Mst => true | _,_ => false end.

(* REX 0100_WRXB: W=1; R iff data>=8; X=0; B iff base>=8 *)
Definition rexb (data base:reg) : byte :=
  64 + 8 + (if 7 <? data then 4 else 0) + (if 7 <? base then 1 else 0).

Definition low3 (r:reg) : Z := r mod 8.

(* mod field: 0 (no disp) only when off=0 and base is not rbp-like; else disp8 if
   it fits, else disp32. bp-like bases force >=1. *)
Definition modsel (bp_like:bool) (off:Z) : Z :=
  if (off =? 0) && negb bp_like then 0
  else if (-128 <=? off) && (off <=? 127) then 1
  else 2.

Definition disp_bytes (m:Z) (off:Z) : list byte :=
  if m =? 0 then []
  else if m =? 1 then [off mod 256]
  else [ off mod 256 ; (off / 256) mod 256 ;
         (off / 65536) mod 256 ; (off / 16777216) mod 256 ].

Definition encode_mem (o:memop) (data base:reg) (off:Z) : list byte :=
  let l  := low3 base in
  let sib_needed := l =? 4 in           (* rsp / r12 *)
  let bp_like    := l =? 5 in           (* rbp / r13 *)
  let m  := modsel bp_like off in
  let rm := if sib_needed then 4 else l in
  [ rexb data base ; mopcode o ; m * 64 + (data mod 8) * 8 + rm ]
    ++ (if sib_needed then [ 4 * 8 + l ] else [])   (* SIB: scale0 index100 base=l *)
    ++ disp_bytes m off.

(* --- decoder: a small auditable model of the same rules --- *)
Definition decode_mop (b:byte) : option memop :=
  if b =? 139 then Some Mld else if b =? 137 then Some Mst else None.

(* signed disp reconstruction *)
Definition sdisp8 (b:byte) : Z := if b <? 128 then b else b - 256.
Definition sdisp32 (b0 b1 b2 b3 : byte) : Z :=
  let v := b0 + b1*256 + b2*65536 + b3*16777216 in
  if v <? 2147483648 then v else v - 4294967296.

Definition decode_mem (bs:list byte) : option (memop * reg * reg * Z) :=
  match bs with
  | r :: oc :: mr :: rest =>
    match decode_mop oc with
    | None => None
    | Some o =>
      let bB := r mod 2 in            (* REX.B *)
      let bR := (r / 4) mod 2 in      (* REX.R *)
      let md := mr / 64 in
      let reg_f := (mr / 8) mod 8 in
      let rm := mr mod 8 in
      let data := reg_f + 8 * bR in
      (* resolve base + the displacement tail, threading SIB if rm=4 *)
      let '(base_low, tail) :=
          if rm =? 4 then match rest with s :: t => (s mod 8, t) | [] => (0, []) end
          else (rm, rest) in
      let base := base_low + 8 * bB in
      let off :=
          if md =? 0 then 0
          else if md =? 1 then match tail with b :: _ => sdisp8 b | _ => 0 end
          else match tail with b0::b1::b2::b3::_ => sdisp32 b0 b1 b2 b3 | _ => 0 end in
      Some (o, data, base, off)
    end
  | _ => None
  end.

(* boundary offset set: hits mod0 / mod1 / mod2, both signs, byte edges, disp32 limbs *)
Definition offs : list Z :=
  [ 0; 1; -1; 8; -8; 100; -100; 127; -127; 128; -128; -129;
    255; -256; 4096; -4096; 100000; -100000; 2000000000; -2000000000 ].

Definition regs16 : list reg := map Z.of_nat (seq 0 16).

Definition mem_roundtrips : bool :=
  forallb (fun o =>
    forallb (fun d =>
      forallb (fun b =>
        forallb (fun off =>
          match decode_mem (encode_mem o d b off) with
          | Some (o',d',b',off') =>
              mop_eqb o o' && (d =? d') && (b =? b') && (off =? off')
          | None => false
          end) offs) regs16) regs16) [Mld; Mst].

Theorem mem_roundtrip_ok : mem_roundtrips = true.
Proof. vm_compute. reflexivity. Qed.

Print Assumptions mem_roundtrip_ok.   (* must stay "Closed under the global context" *)

(* --- extraction: Z -> OCaml int (ExtrOcamlZInt); only encode_mem is needed --- *)
From Stdlib Require Import Extraction ExtrOcamlZInt.
Extraction Language OCaml.
Set Extraction Output Directory ".".
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Inductive bool => "bool" [ "true" "false" ].
Extraction "encmem_ref.ml" encode_mem.
