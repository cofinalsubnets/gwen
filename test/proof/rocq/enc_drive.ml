(* enc_drive.ml -- drives the EXTRACTED, machine-checked reference encoder
   (enc_ref.ml, coqc'd from test/proof/rocq/enc.v with roundtrip_ok proven axiom-free)
   against holo's own x86-64 encoder.

   For every op x every (dest,src) in the 16x16 register matrix it computes the
   reference bytes with the proven `encode`, then prints ONE love program whose
   asserts each check that holo's `holo-hex` for the same IR form is BYTE-
   IDENTICAL to the reference hex. The make recipe cats holo ahead of this output
   and runs it; "N / N PASS" means holo's reg-direct encodings are exactly the
   encodings a machine-checked, round-tripping reference produces. *)

open Enc_ref   (* op = Omov|Oadd|...; encode : op -> int -> int -> int list *)

let hex bs = String.concat "" (List.map (Printf.sprintf "%02x") bs)

(* holo abstract register name -> hardware x86 number (probed in regmap.py):
   holo r0..r3 = rax,rcx,rdx,rbx (0..3); r4..r6 = rbp,rsi,rdi (5,6,7);
   r7..r14 = r8..r15 (8..15); sp = rsp (4). *)
let regs =
  [ "r0",0; "r1",1; "r2",2; "r3",3; "r4",5; "r5",6; "r6",7; "r7",8;
    "r8",9; "r9",10; "r10",11; "r11",12; "r12",13; "r13",14; "r14",15; "sp",4 ]

(* op -> (reference constructor, holo mnemonic, IR form).
   `Mov` = (mov d s); `Alu` = (op d d s) two-address, dest==srcA single-insn;
   `Cmp` = (cmp d s) the two-operand flag-setter. All map to r/m=dest, reg=src. *)
type form = Mov | Alu | Cmp
let ops =
  [ Omov,"mov",Mov; Oadd,"add",Alu; Osub,"sub",Alu; Oand,"and",Alu;
    Oor,"or",Alu; Oxor,"xor",Alu; Ocmp,"cmp",Cmp ]

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
   _ (L|ai}

let footer =
  {ai|)
   total (+ (peep pass 0 0) (peep fail 0 0))
   _ (say (+ "encoder-oracle: " (+ (show (peep pass 0 0)) (+ " / " (+ (show total)
        (? (= 0 (peep fail 0 0)) " PASS\n" " FAIL\n")))))))
|ai}

let () =
  print_string prelude;
  List.iter (fun (o, mnem, form) ->
    List.iter (fun (dn, dh) ->
      List.iter (fun (sn, sh) ->
        let want = hex (encode o dh sh) in
        let ir = match form with
          | Mov -> Printf.sprintf "'((%s %s %s))" mnem dn sn
          | Alu -> Printf.sprintf "'((%s %s %s %s))" mnem dn dn sn
          | Cmp -> Printf.sprintf "'((%s %s %s))" mnem dn sn in
        Printf.printf "\n     (chk \"%s %s %s\" \"%s\" %s)" mnem dn sn want ir
      ) regs
    ) regs
  ) ops;
  print_string footer
