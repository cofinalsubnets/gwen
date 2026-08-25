(* encli_drive.ml -- drives the EXTRACTED, machine-checked immediate encoder
   (encli_ref.ml, coqc'd from test/proof/rocq/encli.v with li_roundtrip_ok proven
   axiom-free) against holo's own x86-64 encoder.

   For every dest in the 16-register file x a spread of immediates hitting all
   three forms (b8 imm32 zero-extend / C7 imm32 sign-extend / movabs imm64) and
   their range boundaries, it computes the reference bytes with the proven
   `encode_li`, then prints a love program asserting holo's holo-hex is BYTE-
   IDENTICAL. "P / P PASS" means holo's immediate-load FORM CHOICE + serialization
   is exactly what the machine-checked reference produces.

   Note: OCaml's native int is 63-bit and ExtrOcamlZInt's Euclidean mod/div
   overflows at min_int (-2^62), so this differential keeps immediates safely
   inside +-(2^60). The Rocq proof is over unbounded Z and covers the full 64-bit
   range including the movabs top-bit cases (2^63-1, -2^63). *)

open Encli_ref   (* encode_li : int -> int -> int list *)

let hex bs = String.concat "" (List.map (Printf.sprintf "%02x") bs)

(* holo abstract register name -> hardware x86 number (regmap.py) *)
let regs =
  [ "r0",0; "r1",1; "r2",2; "r3",3; "r4",5; "r5",6; "r6",7; "r7",8;
    "r8",9; "r9",10; "r10",11; "r11",12; "r12",13; "r13",14; "r14",15; "sp",4 ]

(* immediates hitting all three forms + boundaries, within OCaml's 63-bit int *)
let imms =
  [ 0; 1; 60; 255; 65535; 1000000; 2147483647; 2147483648; 4294967295; (* form A *)
    -1; -128; -1000000; -2147483648;                                    (* form B *)
    4294967296; 1099511627776; 1152921504606846976;                     (* form C +, up to 2^60 *)
    -2147483649; -4294967296; -1099511627776; -1152921504606846976 ]    (* form C -, down to -2^60 *)

let prelude = {ai|
(: pass (tablet 0) _ (pin pass 0 0)
   fail (tablet 0) _ (pin fail 0 0)
   (bump t) (pin t 0 (+ 1 (peep t 0 0)))
   (say s) (: _ (puts s) _ (flush out) 0)
   (chk nm want prog)
     (: got (holo-hex 'x64 prog)
        (? (= want got) (bump pass)
           (: _ (bump fail)
              (say (+ "FAIL " (+ nm (+ " want " (+ want (+ " got " (+ got "\n"))))))))))
   _ [|ai}

let footer =
  {ai|]
   total (+ (peep pass 0 0) (peep fail 0 0))
   _ (say (+ "encli-oracle: " (+ (show (peep pass 0 0)) (+ " / " (+ (show total)
        (? (= 0 (peep fail 0 0)) " PASS\n" " FAIL\n")))))))
|ai}

let () =
  print_string prelude;
  List.iter (fun (dn, dh) ->
    List.iter (fun imm ->
      let want = hex (encode_li dh imm) in
      Printf.printf "\n     (chk \"li %s %d\" \"%s\" '((li %s %d)))" dn imm want dn imm
    ) imms
  ) regs;
  print_string footer
