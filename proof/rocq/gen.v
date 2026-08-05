(* proof/rocq/gen.v -- GENERATED from test/spec.l by tools/spec2coq.l. Do not edit;
   regenerate with `make test_gen`. Each integer-comparison corpus assert is
   translated to Coq, with its source assert shown in the comment above it.
   The proofs are `vm_compute. reflexivity.`: every goal is a CLOSED computation,
   so vm_compute RUNS both sides (Coq's bytecode VM -- fast even on bignums like
   2^64) to a normal form, and reflexivity checks the two results are identical.
   A church numeral n applied to x is x ** n (`appZ`); a list/string nets the
   sum of its elements/bytes (`asum`). The MODEL IS SHARED: this file imports
   proof/rocq/spec.v and checks against ITS definitions -- appZ/asum/aprod/amax/vscale/srep
   verbatim, iotaZ the Z-arg face, app_appZ proving appZ IS the nat `app` -- so the
   generated instances and the hand-written LAWS speak one model. The defs below are
   gen-only (no spec.v twin): amin, leqb, the TOTAL vaddt (spec.v's vadd is option-
   valued for its shape-mismatch law), vquot, hget, the syntactic kind lattice,
   the word shifts (count mod 64), the option-lifted UNIT lane (None rides through
   uop/ulop -- () the identity of every dyadic), insertion sort, lex order, the
   gaussian twins (exact complex over Z; sign and order LEX on (re, im)), and the
   de Bruijn lambda terms (function = is alpha + structural: lteqb). *)
From Stdlib Require Import ZArith List.
Require Import spec.
Import ListNotations.
Open Scope Z_scope.
Definition amin (l:list Z):Z := match l with nil=>0 | x::xs=>fold_right Z.min x xs end.
Fixpoint leqb (a b:list Z):bool := match a,b with nil,nil=>true | x::a',y::b'=>andb (Z.eqb x y) (leqb a' b') | _,_=>false end.
Fixpoint vaddz (a b:list Z):list Z := match a,b with x::a',y::b'=>(x+y)::vaddz a' b' | _,_=>nil end.
Definition vaddt (a b:list Z):list Z := if Nat.eqb (length a) (length b) then vaddz a b else nil.
Definition vquot (c:Z):list Z->list Z := map (fun e=>Z.quot e c).
Fixpoint hget (m:list (Z*Z)) (k d:Z):Z := match m with nil=>d | p::m' => if Z.eqb k (fst p) then snd p else hget m' k d end.
Definition shlz (a b:Z):Z := Z.shiftl a (Z.modulo b 64).  Definition shrz (a b:Z):Z := Z.shiftr a (Z.modulo b 64).
Definition uop (f:Z->Z->Z) (a b:option Z) : option Z := match a,b with None,x=>x | x,None=>x | Some x, Some y => Some (f x y) end.
Definition ueq (a b:option Z) : bool := match a,b with None,None=>true | Some x, Some y => Z.eqb x y | _,_=>false end.
Definition unet (a:option Z) : Z := match a with None => 0 | Some z => z end.
Definition ulop (a b:option (list Z)) : option (list Z) := match a,b with None,x=>x | x,None=>x | Some x, Some y => Some (x++y) end.
Definition uleq (a b:option (list Z)) : bool := match a,b with None,None=>true | Some x, Some y => leqb x y | _,_=>false end.
Fixpoint insZ (le:Z->Z->bool) (x:Z) (l:list Z) : list Z := match l with nil => x::nil | y::t => if le x y then x::y::t else y::insZ le x t end.
Fixpoint insortby (le:Z->Z->bool) (l:list Z) : list Z := match l with nil => nil | x::t => insZ le x (insortby le t) end.
Definition insort := insortby Z.leb.
Fixpoint lexltb (a b:list Z) : bool := match a,b with nil,nil=>false | nil,_=>true | _,nil=>false | x::a',y::b' => orb (Z.ltb x y) (andb (Z.eqb x y) (lexltb a' b')) end.
Definition gadd (a b:Z*Z):Z*Z := (fst a + fst b, snd a + snd b).
Definition gsub (a b:Z*Z):Z*Z := (fst a - fst b, snd a - snd b).
Definition gmul (a b:Z*Z):Z*Z := (fst a * fst b - snd a * snd b, fst a * snd b + snd a * fst b).
Definition gconj (a:Z*Z):Z*Z := (fst a, - snd a).
Definition geqb (a b:Z*Z):bool := andb (Z.eqb (fst a) (fst b)) (Z.eqb (snd a) (snd b)).
Definition gltb (a b:Z*Z):bool := orb (Z.ltb (fst a) (fst b)) (andb (Z.eqb (fst a) (fst b)) (Z.ltb (snd a) (snd b))).
Definition gpos (a:Z*Z):bool := Z.ltb 0 (fst a).
Definition gsat (a:Z*Z):Z := if gpos a then Z.sqrt (fst a*fst a + snd a*snd a) else 0.
Inductive ltm := LV (n:nat) | LO (o:Z) | LN (z:Z) | LAp (f a:ltm) | LLam (b:ltm).
Fixpoint lteqb (a b:ltm):bool := match a,b with LV n,LV m=>Nat.eqb n m | LO n,LO m=>Z.eqb n m | LN n,LN m=>Z.eqb n m | LAp f x,LAp g y=>andb (lteqb f g) (lteqb x y) | LLam s,LLam t=>lteqb s t | _,_=>false end.
Inductive vkind := Vz | Vflo | Vcx | Vstr | Vsym | Vpair | Vmap | Varr | Vunit | Vbot.
Definition fixp  v := match v with Vz   => true | _ => false end.
Definition flop  v := match v with Vflo => true | _ => false end.
Definition comp  v := match v with Vcx  => true | _ => false end.
Definition strp  v := match v with Vstr => true | _ => false end.
Definition nomp  v := match v with Vsym => true | _ => false end.
Definition namep v := match v with Vsym => true | _ => false end.
Definition chainp  v := match v with Vpair=> true | _ => false end.
Definition tabp  v := match v with Vmap => true | _ => false end.
Definition setp  v := match v with Varr => true | _ => false end.
Definition whole   v := match v with Vz   => true | _ => false end.
Definition nump  v := match v with Vz|Vflo|Vcx|Varr => true | _ => false end.
Definition packp v := match v with Vflo|Vcx|Varr => true | _ => false end.
Definition atomp v := match v with Vpair => false | _ => true end.
Definition lamp  v := match v with Vmap => true | _ => false end.
Definition vband v : Z := match v with Vsym => 0 | Vstr => 1 | Vz|Vflo|Vcx|Varr => 2 | Vpair => 3 | Vmap => 4 | Vunit|Vbot => 9 end.

(* (1 = (0 5)) *)
Theorem gen_1 : (Z.eqb 1 (appZ 0 5)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (1 5)) *)
Theorem gen_2 : (Z.eqb 5 (appZ 1 5)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = (0 0)) *)
Theorem gen_3 : (Z.eqb 1 (appZ 0 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (7 = (0 0 7)) *)
Theorem gen_4 : (Z.eqb 7 (appZ (appZ 0 0) 7)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (8 = (3 2)) *)
Theorem gen_5 : (Z.eqb 8 (appZ 3 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (262144 = (2 3 4)) *)
Theorem gen_6 : (Z.eqb 262144 (appZ (appZ 2 3) 4)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (12 = (3 (+ 1) 9)) *)
Theorem gen_7 : (Z.eqb 12 (Nat.iter (Z.to_nat 3) (Z.add 1) 9)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(2 3 4) = (map (+ 1) '(1 2 3))) *)
Theorem gen_8 : (leqb [2;3;4] (map (Z.add 1) [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (10 = (-3 (+ 1) 10)) *)
Theorem gen_9 : (Z.eqb 10 (Nat.iter (Z.to_nat (-3)) (Z.add 1) 10)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) = '(1 2)) *)
Theorem gen_10 : (leqb [1;2] [1;2]) = true.  Proof. vm_compute. reflexivity. Qed.
(* (id? (:) (?)) *)
Theorem gen_11 : true = true.  Proof. vm_compute. reflexivity. Qed.
(* (id? (?) (\)) *)
Theorem gen_12 : true = true.  Proof. vm_compute. reflexivity. Qed.
(* (id? (:) (? 0 'a)) *)
Theorem gen_13 : true = true.  Proof. vm_compute. reflexivity. Qed.
(* (id? (:) (map (+ 1) ())) *)
Theorem gen_14 : true = true.  Proof. vm_compute. reflexivity. Qed.
(* (i < 5) *)
Theorem gen_15 : (gltb (0, 1) (5, 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((twin 0 -1) < i) *)
Theorem gen_16 : (gltb (0, (-1)) (0, 1)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = (nil? 0)) *)
Theorem gen_17 : (Z.eqb 1 (Z.b2z (Z.leb 0 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (charm? 5) *)
Theorem gen_18 : (fixp Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (charm? 5) *)
Theorem gen_19 : (fixp Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (two? '(1 2)) *)
Theorem gen_20 : (chainp Vpair) = true.  Proof. vm_compute. reflexivity. Qed.
(* (string? "hi") *)
Theorem gen_21 : (strp Vstr) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'x) *)
Theorem gen_22 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (tablet? (hash 1 2)) *)
Theorem gen_23 : (tabp Vmap) = true.  Proof. vm_compute. reflexivity. Qed.
(* (gem? 1.5) *)
Theorem gen_24 : (flop Vflo) = true.  Proof. vm_compute. reflexivity. Qed.
(* (twin? i) *)
Theorem gen_25 : (comp Vcx) = true.  Proof. vm_compute. reflexivity. Qed.
(* (constellation? 1.5) *)
Theorem gen_26 : (nump Vflo) = true.  Proof. vm_compute. reflexivity. Qed.
(* (constellation? i) *)
Theorem gen_27 : (nump Vcx) = true.  Proof. vm_compute. reflexivity. Qed.
(* (constellation? (62 2)) *)
Theorem gen_28 : (nump Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (whole? (62 2)) *)
Theorem gen_29 : (whole Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (atom? 'x) *)
Theorem gen_30 : (atomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((64 2) = 2 * (63 2)) *)
Theorem gen_31 : (Z.eqb (appZ 64 2) (Z.mul 2 (appZ 63 2))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = 1 + 2) *)
Theorem gen_32 : (Z.eqb 3 (Z.add 1 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (2 = (// 5 2)) *)
Theorem gen_33 : (Z.eqb 2 (Z.quot 5 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = 5 % 2) *)
Theorem gen_34 : (Z.eqb 1 (Z.rem 5 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-2 = (// -5 2)) *)
Theorem gen_35 : (Z.eqb (-2) (Z.quot (-5) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-5 = ((- 0) 5)) *)
Theorem gen_36 : (Z.eqb (-5) ((Z.sub 0) 5)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((twin -1 -2) = ((- 0) (twin 1 2))) *)
Theorem gen_37 : (geqb ((-1), (-2)) (gsub (0, 0) (1, 2))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'inf) *)
Theorem gen_38 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'nan) *)
Theorem gen_39 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'ieee-nan) *)
Theorem gen_40 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (976371285 = (100 2) % 1000000007) *)
Theorem gen_41 : (Z.eqb 976371285 (Z.rem (appZ 100 2) 1000000007)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((100 2) = (// (200 2) (100 2))) *)
Theorem gen_42 : (Z.eqb (appZ 100 2) (Z.quot (appZ 200 2) (appZ 100 2))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-2 = (^ 1 -1)) *)
Theorem gen_43 : (Z.eqb (-2) (Z.lxor 1 (-1))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (15 = (8 | 4 | 2 | 1)) *)
Theorem gen_44 : (Z.eqb 15 (Z.lor 8 (Z.lor 4 (Z.lor 2 1)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (16 = (>> 64 2)) *)
Theorem gen_45 : (Z.eqb 16 (shrz 64 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (16 = (<< 2 3)) *)
Theorem gen_46 : (Z.eqb 16 (shlz 2 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (768 = (<< 3 200)) *)
Theorem gen_47 : (Z.eqb 768 (shlz 3 200)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-1 = (>> -1 1)) *)
Theorem gen_48 : (Z.eqb (-1) (shrz (-1) 1)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-4 = (>> -8 1)) *)
Theorem gen_49 : (Z.eqb (-4) (shrz (-8) 1)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (id? 'a 'a) *)
Theorem gen_50 : true = true.  Proof. vm_compute. reflexivity. Qed.
(* ('x < "a") *)
Theorem gen_51 : (Z.ltb (vband Vsym) (vband Vstr)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("a" < 1) *)
Theorem gen_52 : (Z.ltb (vband Vstr) (vband Vz)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 < '(0)) *)
Theorem gen_53 : (Z.ltb (vband Vz) (vband Vpair)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(0) < (hash 1 10)) *)
Theorem gen_54 : (Z.ltb (vband Vpair) (vband Vmap)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("abc" < "abd") *)
Theorem gen_55 : (lexltb [97;98;99] [97;98;100]) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ab" < "abc") *)
Theorem gen_56 : (lexltb [97;98] [97;98;99]) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) < '(1 3)) *)
Theorem gen_57 : (lexltb [1;2] [1;3]) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((\ x x) = (\ y y)) *)
Theorem gen_58 : (lteqb (LLam (LV 0)) (LLam (LV 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((\ a b (+ a b)) = (\ x y (+ x y))) *)
Theorem gen_59 : (lteqb (LLam (LLam (LAp (LAp (LO 1) (LV 1)) (LV 0)))) (LLam (LLam (LAp (LAp (LO 1) (LV 1)) (LV 0))))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((\ x (+ x 1)) = (\ x (+ x 1))) *)
Theorem gen_60 : (lteqb (LLam (LAp (LAp (LO 1) (LV 0)) (LN 1))) (LLam (LAp (LAp (LO 1) (LV 0)) (LN 1)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (42 = ((\ x x) 42)) *)
Theorem gen_61 : (Z.eqb 42 ((fun v2 : Z => v2) 42)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = ((\ _ 1) 9)) *)
Theorem gen_62 : (Z.eqb 1 ((fun v5 : Z => 1) 9)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = ((\ x x) 5)) *)
Theorem gen_63 : (Z.eqb 5 ((fun v8 : Z => v8) 5)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = ((\ _ 1) 9)) *)
Theorem gen_64 : (Z.eqb 1 ((fun v11 : Z => 1) 9)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = ((\ x x) ((\ x x) 5))) *)
Theorem gen_65 : (Z.eqb 5 ((fun v17 : Z => v17) ((fun v16 : Z => v16) 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("abcd" = "ab" + "cd") *)
Theorem gen_66 : (leqb [97;98;99;100] ([97;98] ++ [99;100])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("xB" = "x" + 66) *)
Theorem gen_67 : (leqb [120;66] ([120] ++ [66])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('efef = 'ef + 'ef) *)
Theorem gen_68 : (leqb [101;102;101;102] ([101;102] ++ [101;102])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("abef" = "ab" + 'ef) *)
Theorem gen_69 : (leqb [97;98;101;102] ([97;98] ++ [101;102])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2 3 4) = '(1 2) + '(3 4)) *)
Theorem gen_70 : (leqb [1;2;3;4] ([1;2] ++ [3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(5 1 2) = 5 + '(1 2)) *)
Theorem gen_71 : (leqb [5;1;2] (5 :: [1;2])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(3) = '(3) + ()) *)
Theorem gen_72 : (uleq (Some [3]) (ulop (Some [3]) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(3) = () + '(3)) *)
Theorem gen_73 : (uleq (Some [3]) (ulop None (Some [3]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) = '(1 2) + (mint 0)) *)
Theorem gen_74 : (uleq (Some [1;2]) (ulop (Some [1;2]) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ab" = "ab" + ()) *)
Theorem gen_75 : (uleq (Some [97;98]) (ulop (Some [97;98]) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ab" = () + "ab") *)
Theorem gen_76 : (uleq (Some [97;98]) (ulop None (Some [97;98]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = 5 + ()) *)
Theorem gen_77 : (ueq (Some 5) (uop Z.add (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = () + 5) *)
Theorem gen_78 : (ueq (Some 5) (uop Z.add None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((twin 1 2) = () + (twin 1 2)) *)
Theorem gen_79 : (geqb (1, 2) (1, 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (() = () + ()) *)
Theorem gen_80 : (ueq None (uop Z.add None None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ababab" = "ab" * 3) *)
Theorem gen_81 : (leqb [97;98;97;98;97;98] (srep (Z.to_nat 3) [97;98])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('xyxy = 'xy * 2) *)
Theorem gen_82 : (leqb [120;121;120;121] (srep (Z.to_nat 2) [120;121])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2 1 2) = '(1 2) * 2) *)
Theorem gen_83 : (leqb [1;2;1;2] (srep (Z.to_nat 2) [1;2])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = () * 5) *)
Theorem gen_84 : (ueq (Some 5) (uop Z.mul None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = 5 * ()) *)
Theorem gen_85 : (ueq (Some 5) (uop Z.mul (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ab" = "ab" * ()) *)
Theorem gen_86 : (uleq (Some [97;98]) (ulop (Some [97;98]) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ab" = () * "ab") *)
Theorem gen_87 : (uleq (Some [97;98]) (ulop None (Some [97;98]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) = '(1 2) * (mint 0)) *)
Theorem gen_88 : (uleq (Some [1;2]) (ulop (Some [1;2]) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (() = () * ()) *)
Theorem gen_89 : (ueq None (uop Z.mul None None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("" = "ab" * 0) *)
Theorem gen_90 : (leqb [] (srep (Z.to_nat 0) [97;98])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = 5 - ()) *)
Theorem gen_91 : (ueq (Some 5) (uop Z.sub (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = () - 5) *)
Theorem gen_92 : (ueq (Some 5) (uop Z.sub None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((64 2) = (64 2) - ()) *)
Theorem gen_93 : (ueq (Some (appZ 64 2)) (uop Z.sub (Some (appZ 64 2)) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = 5 / ()) *)
Theorem gen_94 : (ueq (Some 5) (uop Z.div (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = () / 5) *)
Theorem gen_95 : (ueq (Some 5) (uop Z.div None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (// 5 ())) *)
Theorem gen_96 : (ueq (Some 5) (uop Z.quot (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = 5 % ()) *)
Theorem gen_97 : (ueq (Some 5) (uop Z.rem (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (% () 5)) *)
Theorem gen_98 : (ueq (Some 5) (uop Z.rem None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (5 | ())) *)
Theorem gen_99 : (ueq (Some 5) (uop Z.lor (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (() & 5)) *)
Theorem gen_100 : (ueq (Some 5) (uop Z.land None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (^ 5 ())) *)
Theorem gen_101 : (ueq (Some 5) (uop Z.lxor (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (<< 5 ())) *)
Theorem gen_102 : (ueq (Some 5) (uop shlz (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (>> () 5)) *)
Theorem gen_103 : (ueq (Some 5) (uop shrz None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((twin 1 2) = (twin 1 2) - ()) *)
Theorem gen_104 : (geqb (1, 2) (1, 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (2 = (5 - ()) - 3) *)
Theorem gen_105 : (ueq (Some 2) (uop Z.sub (uop Z.sub (Some 5) None) (Some 3))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (() < 5) *)
Theorem gen_106 : (Z.ltb (unet None) (unet (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (('(1 2) * ()) = '(1 2)) *)
Theorem gen_107 : (uleq (ulop (Some [1;2]) None) (Some [1;2])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (() * '(3 4) = '(3 4)) *)
Theorem gen_108 : (uleq (ulop None (Some [3;4])) (Some [3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (abs -5)) *)
Theorem gen_109 : (Z.eqb 5 (Z.abs (-5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (charm? (abs -5)) *)
Theorem gen_110 : (fixp Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1024 = (10 2)) *)
Theorem gen_111 : (Z.eqb 1024 (appZ 10 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (21 = (gcd 1071 462)) *)
Theorem gen_112 : (Z.eqb 21 (Z.gcd 1071 462)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1024 = (10 2)) *)
Theorem gen_113 : (Z.eqb 1024 (appZ 10 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (27 = (3 3)) *)
Theorem gen_114 : (Z.eqb 27 (appZ 3 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (7625597484987 = (3 3 3)) *)
Theorem gen_115 : (Z.eqb 7625597484987 (appZ (appZ 3 3) 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (16 = (2 2 2)) *)
Theorem gen_116 : (Z.eqb 16 (appZ (appZ 2 2) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (65536 = (2 2 2 2)) *)
Theorem gen_117 : (Z.eqb 65536 (appZ (appZ (appZ 2 2) 2) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-1 = i * i) *)
Theorem gen_118 : (geqb ((-1), 0) (gmul (0, 1) (0, 1))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-8 = (3 -2)) *)
Theorem gen_119 : (Z.eqb (-8) (appZ 3 (-2))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (( * i i) = -1) *)
Theorem gen_120 : (geqb (gmul (0, 1) (0, 1)) ((-1), 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((twin 2 0) = 2) *)
Theorem gen_121 : (geqb (2, 0) (2, 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((twin -5 10) = (twin 1 2) * (twin 3 4)) *)
Theorem gen_122 : (geqb ((-5), 10) (gmul (1, 2) (3, 4))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (twin? (conj 5)) *)
Theorem gen_123 : (comp Vcx) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((twin 0 0) = (conj 0)) *)
Theorem gen_124 : (geqb (0, 0) (gconj (0, 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((conj (twin 2 3)) = (conj (twin 2 3))) *)
Theorem gen_125 : (geqb (gconj (2, 3)) (gconj (2, 3))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((conj (twin 2 3)) = (twin 2 -3)) *)
Theorem gen_126 : (geqb (gconj (2, 3)) (2, (-3))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((twin 2 3) = (conj (conj (twin 2 3)))) *)
Theorem gen_127 : (geqb (2, 3) (gconj (gconj (2, 3)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((conj ((twin 2 3) + (twin 4 5))) = (conj (twin 2 3)) + (conj (twin 4 5))) *)
Theorem gen_128 : (geqb (gconj (gadd (2, 3) (4, 5))) (gadd (gconj (2, 3)) (gconj (4, 5)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((conj ((twin 2 3) * (twin 4 5))) = (conj (twin 2 3)) * (conj (twin 4 5))) *)
Theorem gen_129 : (geqb (gconj (gmul (2, 3) (4, 5))) (gmul (gconj (2, 3)) (gconj (4, 5)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((conj 8) = (conj 3) + (conj 5)) *)
Theorem gen_130 : (geqb (gconj (8, 0)) (gadd (gconj (3, 0)) (gconj (5, 0)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((twin 0 0) < i) *)
Theorem gen_131 : (gltb (0, 0) (0, 1)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((conj (twin 2 3)) < (twin 2 3)) *)
Theorem gen_132 : (gltb (gconj (2, 3)) (2, 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (i < 1) *)
Theorem gen_133 : (gltb (0, 1) (1, 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (20 = (peep (tuple 10 20 30) 1 -1)) *)
Theorem gen_134 : (Z.eqb 20 (nth (Z.to_nat 1) [10;20;30] (-1))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-1 = (peep (tuple 10 20 30) 9 -1)) *)
Theorem gen_135 : (Z.eqb (-1) (nth (Z.to_nat 9) [10;20;30] (-1))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 11 22 33) = (tuple 1 2 3) + (tuple 10 20 30)) *)
Theorem gen_136 : (leqb [11;22;33] (vaddt [1;2;3] [10;20;30])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 2 4 6) = (tuple 1 2 3) * 2) *)
Theorem gen_137 : (leqb [2;4;6] (vscale 2 [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (60 = (net (tuple 10 20 30))) *)
Theorem gen_138 : (Z.eqb 60 (asum [10;20;30])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (30 = (max (tuple 10 30 20))) *)
Theorem gen_139 : (Z.eqb 30 (amax [10;30;20])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 0 1 2) = (iota 3)) *)
Theorem gen_140 : (leqb [0;1;2] (iotaZ 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (4950 = (net (iota 100))) *)
Theorem gen_141 : (Z.eqb 4950 (asum (iotaZ 100))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 3 5) = (// (tuple 7 11) 2)) *)
Theorem gen_142 : (leqb [3;5] (vquot 2 [7;11])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = ((tuple 1 2 3) = (tuple 1 2))) *)
Theorem gen_143 : (Z.eqb 0 (Z.b2z (leqb [1;2;3] [1;2]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = ('sym = (tuple 1 2 3))) *)
Theorem gen_144 : (Z.eqb 0 (Z.b2z false)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = ("ab" = (tuple 1 2))) *)
Theorem gen_145 : (Z.eqb 0 (Z.b2z (leqb [97;98] [1;2]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 1 2 3) = (tuple 1 2 3)) *)
Theorem gen_146 : (leqb [1;2;3] [1;2;3]) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = (cap '(1 2 3))) *)
Theorem gen_147 : (Z.eqb 1 (hd 0 [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(2 3) = (cup '(1 2 3))) *)
Theorem gen_148 : (leqb [2;3] (skipn (Z.to_nat 1) [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (caup '(2 3 4))) *)
Theorem gen_149 : (Z.eqb 3 (nth (Z.to_nat 1) [2;3;4] 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) = (link 1 (link 2 ()))) *)
Theorem gen_150 : (leqb [1;2] (1 :: (2 :: []))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("x" = (string 'x)) *)
Theorem gen_151 : (leqb [120] [120]) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(0 1 2) = (jot 3)) *)
Theorem gen_152 : (leqb [0;1;2] (iotaZ 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(2 3 4) = (map (+ 1) '(1 2 3))) *)
Theorem gen_153 : (leqb [2;3;4] (map (Z.add 1) [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (24 = (foldl ( * ) 1 '(1 2 3 4))) *)
Theorem gen_154 : (Z.eqb 24 (fold_left Z.mul [1;2;3;4] 1)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (6 = (foldr (+) 0 '(1 2 3))) *)
Theorem gen_155 : (Z.eqb 6 (fold_right Z.add 0 [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 3) = (filter (\ x (x % 2)) '(1 2 3 4))) *)
Theorem gen_156 : (leqb [1;3] (filter (fun v29 : Z => Z.ltb 0 (Z.rem v29 2)) [1;2;3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2 3) = (sort '(3 1 2))) *)
Theorem gen_157 : (leqb [1;2;3] (insort [3;1;2])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(3 2 1) = (sortby (>) '(1 2 3))) *)
Theorem gen_158 : (leqb [3;2;1] (insortby (fun a b : Z => Z.leb b a) [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2 3 4) = (cat '(1 2) '(3 4))) *)
Theorem gen_159 : (leqb [1;2;3;4] ([1;2] ++ [3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(3 2 1) = (rev '(1 2 3))) *)
Theorem gen_160 : (leqb [3;2;1] (rev [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) = (take 2 '(1 2 3 4))) *)
Theorem gen_161 : (leqb [1;2] (firstn (Z.to_nat 2) [1;2;3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(3 4) = (drop 2 '(1 2 3 4))) *)
Theorem gen_162 : (leqb [3;4] (skipn (Z.to_nat 2) [1;2;3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (last '(1 2 3))) *)
Theorem gen_163 : (Z.eqb 3 (last [1;2;3] 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) = (init '(1 2 3))) *)
Theorem gen_164 : (leqb [1;2] (removelast [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (member? 3 '(1 2 3)) *)
Theorem gen_165 : (existsb (Z.eqb 3) [1;2;3]) = true.  Proof. vm_compute. reflexivity. Qed.
(* (all (\ x (0 < x)) '(1 2 3)) *)
Theorem gen_166 : (forallb (fun v31 : Z => (Z.ltb 0 v31)) [1;2;3]) = true.  Proof. vm_compute. reflexivity. Qed.
(* (any (\ x (2 < x)) '(1 2 3)) *)
Theorem gen_167 : (existsb (fun v33 : Z => (Z.ltb 2 v33)) [1;2;3]) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (tally '(a b c))) *)
Theorem gen_168 : (Z.eqb 3 (Z.of_nat (length [97;98;99]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (name? 'x) *)
Theorem gen_169 : (namep Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (tally "abc")) *)
Theorem gen_170 : (Z.eqb 3 (Z.of_nat (length [97;98;99]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (abs -5)) *)
Theorem gen_171 : (Z.eqb 5 (Z.abs (-5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("abcd" = (+ "ab" "cd")) *)
Theorem gen_172 : (leqb [97;98;99;100] ([97;98] ++ [99;100])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('asdf = (intern "asdf")) *)
Theorem gen_173 : (leqb [97;115;100;102] [97;115;100;102]) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("asdf" = (string 'asdf)) *)
Theorem gen_174 : (leqb [97;115;100;102] [97;115;100;102]) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nil? ()) *)
Theorem gen_175 : true = true.  Proof. vm_compute. reflexivity. Qed.
(* (nil? 0) *)
Theorem gen_176 : (Z.leb 0 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = (tally (iota 0))) *)
Theorem gen_177 : (Z.eqb 0 (Z.of_nat (length (iotaZ 0)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = (tally (tuple 0))) *)
Theorem gen_178 : (Z.eqb 1 (Z.of_nat (length [0]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = (peep (tuple 0) 0 -1)) *)
Theorem gen_179 : (Z.eqb 0 (nth (Z.to_nat 0) [0] (-1))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (tablet? (hash 0)) *)
Theorem gen_180 : (tabp Vmap) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (tally "hello")) *)
Theorem gen_181 : (Z.eqb 5 (Z.of_nat (length [104;101;108;108;111]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (i = (twin 0 1)) *)
Theorem gen_182 : (geqb (0, 1) (0, 1)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((twin 2 3) = (twin 2 3)) *)
Theorem gen_183 : (geqb (2, 3) (2, 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (6 = ((\ x' (x' + 1)) 5)) *)
Theorem gen_184 : (Z.eqb 6 ((fun v36 : Z => (Z.add v36 1)) 5)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'ab') *)
Theorem gen_185 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-3 = -3) *)
Theorem gen_186 : (Z.eqb (-3) (-3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (2 - -3)) *)
Theorem gen_187 : (Z.eqb 5 (Z.sub 2 (-3))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = 1 + 2) *)
Theorem gen_188 : (Z.eqb 3 (Z.add 1 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (7 = 1 + 2 * 3) *)
Theorem gen_189 : (Z.eqb 7 (Z.add 1 (Z.mul 2 3))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-4 = 1 - 2 - 3) *)
Theorem gen_190 : (Z.eqb (-4) (Z.sub (Z.sub 1 2) 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = abs -3) *)
Theorem gen_191 : (Z.eqb 3 (Z.abs (-3))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (66 = (2 + 3 4)) *)
Theorem gen_192 : (Z.eqb 66 (Z.add 2 (appZ 3 4))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = ((1 +) 2)) *)
Theorem gen_193 : (Z.eqb 3 ((Z.add 1) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (+ 1 2)) *)
Theorem gen_194 : (Z.eqb 3 (Z.add 1 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (20 = ((hash 1 10 2 20) -> 2 0)) *)
Theorem gen_195 : (Z.eqb 20 (hget [(1, 10);(2, 20)] 2 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (12 = (foldl (+) 0 '(3 4 5))) *)
Theorem gen_196 : (Z.eqb 12 (fold_left Z.add [3;4;5] 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (id? (&& () 5) ()) *)
Theorem gen_197 : true = true.  Proof. vm_compute. reflexivity. Qed.
(* (id? (&& 1 0 3) ()) *)
Theorem gen_198 : true = true.  Proof. vm_compute. reflexivity. Qed.
(* (id? (|| () ()) ()) *)
Theorem gen_199 : true = true.  Proof. vm_compute. reflexivity. Qed.
(* (id? (|| () 0) ()) *)
Theorem gen_200 : true = true.  Proof. vm_compute. reflexivity. Qed.

(* 200 theorems generated from 728 asserts seen *)
