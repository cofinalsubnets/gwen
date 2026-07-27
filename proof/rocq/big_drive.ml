(* big_drive.ml -- drives the EXTRACTED bignum reference (bigref.ml, coqc'd
   from proof/rocq/big.v: stdlib binary Z + the PROVEN decimal codec) against
   love's limb lane.

   Generates operand pairs -- every pair of the edge values first (the
   charm/sun/word boundaries where the rep changes), then random decimal
   strings whose lengths cross the limb, Karatsuba and Knuth-D thresholds --
   computes each op with the extracted reference, and prints one love program:
   every line checks (show <op on literals>) against the reference's decimal
   string. So love's READER (the emitted literals), limb arithmetic, and
   PRINTER (show) all sit under test against the proven codec; the randomness
   lives here, the arithmetic there. The love binary runs the emitted program;
   green means the limbs compute what the machine-checked reference says. *)

open Bigref

(* Coq string extracts to char list (ExtrOcamlString) *)
let implode l = String.concat "" (List.map (String.make 1) l)
let explode s = List.init (String.length s) (String.get s)

let zs z = implode (print z)                          (* proven decimal print *)
let zparse s = match parse (explode s) with
  | Some z -> z | None -> failwith ("unparseable: " ^ s)

(* the rep boundaries: charm max, sun edges, word edges, 2^128, a 59-nine big *)
let edges = List.map zparse [
  "0"; "1"; "-1"; "9";
  "4611686018427387903"; "-4611686018427387904";
  "9223372036854775807"; "-9223372036854775808";
  "18446744073709551616";
  "340282366920938463463374607431768211455";
  "-340282366920938463463374607431768211456";
  "99999999999999999999999999999999999999999999999999999999999" ]

(* random decimal string: mostly small (the charm/limb boundary), a tail of
   multi-hundred-digit operands (Karatsuba / Knuth-D territory) *)
let rand_z () =
  let cls = Random.int 10 in
  let len = if cls < 3 then 1 + Random.int 19
            else if cls < 7 then 20 + Random.int 60
            else if cls < 9 then 80 + Random.int 300
            else 400 + Random.int 500 in
  let s = String.init len (fun i ->
    Char.chr (Char.code '0' + (if i = 0 then 1 + Random.int 9 else Random.int 10))) in
  let s = if Random.int 10 = 0 then "0" else s in
  let s = if s <> "0" && Random.int 2 = 0 then "-" ^ s else s in
  zparse s

let nonzero b = if zs b = "0" then zparse "7" else b

(* each op: the love expression over decimal literals, and the reference answer *)
let ops = [|
  (fun a b -> Printf.sprintf "(%s + %s)" (zs a) (zs b), zs (zadd a b));
  (fun a b -> Printf.sprintf "(%s - %s)" (zs a) (zs b), zs (zsub a b));
  (fun a b -> Printf.sprintf "(%s * %s)" (zs a) (zs b), zs (zmul a b));
  (fun a b -> let b = nonzero b in
              Printf.sprintf "(%s // %s)" (zs a) (zs b), zs (zquot a b));
  (fun a b -> let b = nonzero b in
              Printf.sprintf "(%s %% %s)" (zs a) (zs b), zs (zrem a b));
  (fun a b -> Printf.sprintf "(gcd %s %s)" (zs a) (zs b), zs (zgcd a b));
  (fun a b -> Printf.sprintf "(%s < %s)" (zs a) (zs b),
              if zltb a b then "1" else "0");
  (fun a _ -> Printf.sprintf "(abs %s)" (zs a), zs (zabs a));
|]

let prelude = {ai|
(: (oline v r) (? (= (show v) r) 1
                  (: _ (. "MISMATCH ") _ (. (show v)) _ (. " want ") _ (. r) _ (. "\n") 0))
   got (foldl (+) 0 (L|ai}

let footer n =
  Printf.sprintf {ai|))
   _ (. "big-oracle: ") _ (. (show got)) _ (. " / %d ")
   (. (? (= got %d) "PASS\n" "FAIL\n")))
|ai} n n

let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 2000 in
  let seed = try int_of_string Sys.argv.(2) with _ -> 1 in
  Random.init seed;
  print_string prelude;
  let ne = List.length edges in
  for i = 0 to n - 1 do
    let a, b =
      if i < ne * ne then (List.nth edges (i / ne), List.nth edges (i mod ne))
      else (rand_z (), rand_z ()) in
    let expr, expect = ops.(i mod Array.length ops) a b in
    Printf.printf "\n     (oline %s \"%s\")" expr expect
  done;
  print_string (footer n)
