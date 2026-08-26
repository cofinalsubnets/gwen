(* encmem_drive.ml -- drives the EXTRACTED, machine-checked memory encoder
   (encmem_ref.ml, coqc'd from test/proof/rocq/encmem.v with mem_roundtrip_ok proven
   axiom-free) against holo's own x86-64 encoder.

   For every op in {ld,st} x every (data,base) in the 16x16 register matrix x a
   spread of displacements (including the disp8/disp32 boundaries and both signs)
   it computes the reference bytes with the proven `encode_mem`, then prints a love
   program asserting holo's holo-hex is BYTE-IDENTICAL. "P / P PASS" means holo's
   base+disp load/store encodings -- SIB escapes, the rbp/r13 forced disp, disp
   sizing, REX.R/B -- are exactly what a machine-checked, round-tripping reference
   produces. *)

open Encmem_ref   (* memop = Mld | Mst ; encode_mem : memop -> int -> int -> int -> int list *)

let hex bs = String.concat "" (List.map (Printf.sprintf "%02x") bs)

(* holo abstract register name -> hardware x86 number (regmap.py) *)
let regs =
  [ "r0",0; "r1",1; "r2",2; "r3",3; "r4",5; "r5",6; "r6",7; "r7",8;
    "r8",9; "r9",10; "r10",11; "r11",12; "r12",13; "r13",14; "r14",15; "sp",4 ]

(* a representative displacement spread hitting every mod class + both signs +
   the disp8/disp32 edges. (The Rocq proof covers a wider boundary set; here we
   keep the full 16x16 register matrix -- the dimension the SIB/forced-disp quirks
   live in -- and sample offsets, to keep the gate a few seconds.) *)
let offs =
  [ 0; 1; -1; 8; -8; 127; 128; -128; -129; 255; 100000; -2000000000 ]

let prelude = {ai|
(: pass (tablet 0) _ (pin pass 0 0)
   fail (tablet 0) _ (pin fail 0 0)
   (bump t) (pin t 0 (+ 1 (peep t 0 0)))
   (say s) (: _ (puts s) _ (flush out) 0)
   (chk nm want prog)
     (: got (holo-hex 'amd64 prog)
        (? (= want got) (bump pass)
           (: _ (bump fail)
              (say (+ "FAIL " (+ nm (+ " want " (+ want (+ " got " (+ got "\n"))))))))))
   _ [|ai}

let footer =
  {ai|]
   total (+ (peep pass 0 0) (peep fail 0 0))
   _ (say (+ "encmem-oracle: " (+ (show (peep pass 0 0)) (+ " / " (+ (show total)
        (? (= 0 (peep fail 0 0)) " PASS\n" " FAIL\n")))))))
|ai}

let () =
  print_string prelude;
  List.iter (fun (dn, dh) ->
    List.iter (fun (bn, bh) ->
      List.iter (fun off ->
        (* ld d base off ; st base off s -- both: r/m = [base+off], reg = data *)
        let wl = hex (encode_mem Mld dh bh off) in
        Printf.printf "\n     (chk \"ld %s %s %d\" \"%s\" '((ld %s %s %d)))"
          dn bn off wl dn bn off;
        let ws = hex (encode_mem Mst dh bh off) in
        Printf.printf "\n     (chk \"st %s %s %d\" \"%s\" '((st %s %d %s)))"
          bn dn off ws bn off dn
      ) offs
    ) regs
  ) regs;
  print_string footer
