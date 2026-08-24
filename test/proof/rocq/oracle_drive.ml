(* oracle_drive.ml -- drives the EXTRACTED reference normalizer (normalizer.ml,
   coqc'd from test/proof/rocq/extract.v on spec.v's proven subst/shift) against ev.

   Generates random closed AFFINE de Bruijn terms (each bound var used <=1 =>
   strongly normalizing), normalizes each with the extracted `nf`, and prints
   one love program: render helpers + an (assert ...) whose every line checks that
   ev's reduction of the term EXTENSIONALLY agrees with the extracted reference's
   normal form (apply both to marker symbols, then compare -- representation-
   independent, see test/oracle.l). The love binary runs the emitted program;
   green means the binary computes what the machine-checked reference says. *)

open Normalizer   (* tm = Var of int | Lam of tm | App of tm*tm ; nf : int->tm->tm *)

(* --- random closed affine generator (port of test/oracle.l's `gen`) --- *)
let rec partition = function
  | [] -> ([], [])
  | x :: rest ->
    let (a, b) = partition rest in
    if Random.int 2 = 0 then (x :: a, b) else (a, x :: b)

let rec gen g sc =
  let pick = Random.int 10 in
  if g < 1 then
    (match sc with [] -> (Lam (Var 0), []) | x :: r -> (Var x, r))
  else if sc <> [] && pick < 4 then
    (match sc with x :: r -> (Var x, r) | [] -> assert false)
  else if pick < 7 then begin
    let bsc = 0 :: List.map (fun i -> i + 1) sc in
    let (b, lo) = gen (g - 1) bsc in
    let lo' = List.map (fun i -> i - 1) (List.filter (fun i -> i > 0) lo) in
    (Lam b, lo')
  end else begin
    let (sc1, sc2) = partition sc in
    let (f, lof) = gen (g - 1) sc1 in
    let (a, loa) = gen (g - 1) (lof @ sc2) in
    (App (f, a), loa)
  end

let gen_closed g = fst (gen g [])

(* --- serialize a de Bruijn term as the love tagged chain (0 n)/(1 b)/(2 f a) --- *)
let rec ser = function
  | Var n -> Printf.sprintf "(0 %d)" n
  | Lam b -> Printf.sprintf "(1 %s)" (ser b)
  | App (f, a) -> Printf.sprintf "(2 %s %s)" (ser f) (ser a)

(* the love-side consumer: render de Bruijn -> \-expr, ground via marker symbols,
   compare ev of the term against ev of the (extracted) normal form *)
let prelude = {ai|
(: (dtag t) (cap t)  (dn t) (caup t)  (dbody t) (caup t)
   (dfun t) (caup t) (darg t) (cauup t)
   lam-sym (cap '(\ x))
   (vname i) (intern (+ "v" (show i)))
   (render d t)
     (? (= (dtag t) 0) (vname (- (- d 1) (dn t)))
        (= (dtag t) 1) [lam-sym (vname d) (render (+ d 1) (dbody t))]
        [(render d (dfun t)) (render d (darg t))])
   (ground v) (v 'm1 'm2 'm3 'm4 'm5)
   (oline t nf) (? (= (ground (ev (render 0 t))) (ground (ev (render 0 nf)))) 1 0)
   got (foldl (+) 0 [|ai}

(* footer: total count is spliced in so the verdict line is self-describing *)
let footer n =
  Printf.sprintf {ai|])
   _ (. "extracted-oracle: ") _ (. (show got)) _ (. " / %d ")
   (. (? (= got %d) "PASS\n" "FAIL\n")))
|ai} n n

let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 2000 in
  let gas = try int_of_string Sys.argv.(2) with _ -> 5 in
  let seed = try int_of_string Sys.argv.(3) with _ -> 1 in
  Random.init seed;
  print_string prelude;
  for _ = 1 to n do
    let t = gen_closed gas in
    let u = nf 4000 t in
    Printf.printf "\n     (oline '%s '%s)" (ser t) (ser u)
  done;
  print_string (footer n)
