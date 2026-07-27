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
   uop/ulop -- () the identity of every dyadic), insertion sort, and lex order. *)
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
Inductive vkind := Vz | Vflo | Vcx | Vstr | Vsym | Vpair | Vmap | Varr | Vbot.
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
Definition vband v : Z := match v with Vsym => 0 | Vstr => 1 | Vz|Vflo|Vcx|Varr => 2 | Vpair => 3 | Vmap => 4 | Vbot => 9 end.

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
(* !0 *)
Theorem gen_11 : (Z.leb 0 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !"" *)
Theorem gen_12 : (Z.leb (asum []) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(tuple 0) *)
Theorem gen_13 : (Z.leb (asum [0]) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !-5 *)
Theorem gen_14 : (Z.leb (-5) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !!5 *)
Theorem gen_15 : (Z.leb (Z.b2z (Z.leb 5 0)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !!"x" *)
Theorem gen_16 : (Z.leb (Z.b2z (Z.leb (asum [120]) 0)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !'(() ()) *)
Theorem gen_17 : (Z.leb (asum [0;0]) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !'(0 0) *)
Theorem gen_18 : (Z.leb (asum [0;0]) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = $'(() () ())) *)
Theorem gen_19 : (Z.eqb 0 (Z.max 0 (asum [0;0;0]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* !'(-5) *)
Theorem gen_20 : (Z.leb (asum [(-5)]) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(tuple -3 -4) *)
Theorem gen_21 : (Z.leb (asum [(-3);(-4)]) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !'(-2 1) *)
Theorem gen_22 : (Z.leb (asum [(-2);1]) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !!'(0 1) *)
Theorem gen_23 : (Z.leb (Z.b2z (Z.leb (asum [0;1]) 0)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = $'(1 () 2)) *)
Theorem gen_24 : (Z.eqb 3 (Z.max 0 (asum [1;0;2]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (6 = $'(1 2 3)) *)
Theorem gen_25 : (Z.eqb 6 (Z.max 0 (asum [1;2;3]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (7 = $(tuple 3 4)) *)
Theorem gen_26 : (Z.eqb 7 (Z.max 0 (asum [3;4]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = $'(1)) *)
Theorem gen_27 : (Z.eqb 1 (Z.max 0 (asum [1]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = $'(0)) *)
Theorem gen_28 : (Z.eqb 0 (Z.max 0 (asum [0]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = $0) *)
Theorem gen_29 : (Z.eqb 0 (Z.max 0 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-1 = +'(-2 1)) *)
Theorem gen_30 : (Z.eqb (-1) (asum [(-2);1])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = !!$5) *)
Theorem gen_31 : (Z.eqb 1 (Z.b2z (Z.leb (Z.b2z (Z.leb (Z.max 0 5) 0)) 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = !!$0) *)
Theorem gen_32 : (Z.eqb 0 (Z.b2z (Z.leb (Z.b2z (Z.leb (Z.max 0 0) 0)) 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = !!$-3) *)
Theorem gen_33 : (Z.eqb 0 (Z.b2z (Z.leb (Z.b2z (Z.leb (Z.max 0 (-3)) 0)) 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = !5) *)
Theorem gen_34 : (Z.eqb 0 (Z.b2z (Z.leb 5 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = !0) *)
Theorem gen_35 : (Z.eqb 1 (Z.b2z (Z.leb 0 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = (nil? 0)) *)
Theorem gen_36 : (Z.eqb 1 (Z.b2z (Z.leb 0 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (charm? 5) *)
Theorem gen_37 : (fixp Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(charm? 3.5) *)
Theorem gen_38 : (Z.leb (Z.b2z (fixp Vflo)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(charm? (tuple 1 2 3)) *)
Theorem gen_39 : (Z.leb (Z.b2z (fixp Varr)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(charm? 'a) *)
Theorem gen_40 : (Z.leb (Z.b2z (fixp Vsym)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (!"" = 0 = $"") *)
Theorem gen_41 : (Z.eqb (Z.b2z (Z.leb (asum []) 0)) (Z.b2z (Z.eqb 0 (Z.max 0 (asum []))))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (!-5 = 0 = $-5) *)
Theorem gen_42 : (Z.eqb (Z.b2z (Z.leb (-5) 0)) (Z.b2z (Z.eqb 0 (Z.max 0 (-5))))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (charm? 5) *)
Theorem gen_43 : (fixp Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (two? '(1 2)) *)
Theorem gen_44 : (chainp Vpair) = true.  Proof. vm_compute. reflexivity. Qed.
(* (string? "hi") *)
Theorem gen_45 : (strp Vstr) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'x) *)
Theorem gen_46 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (book? (hash 1 2)) *)
Theorem gen_47 : (tabp Vmap) = true.  Proof. vm_compute. reflexivity. Qed.
(* (gem? 1.5) *)
Theorem gen_48 : (flop Vflo) = true.  Proof. vm_compute. reflexivity. Qed.
(* (twin? i) *)
Theorem gen_49 : (comp Vcx) = true.  Proof. vm_compute. reflexivity. Qed.
(* (constellation? 1.5) *)
Theorem gen_50 : (nump Vflo) = true.  Proof. vm_compute. reflexivity. Qed.
(* (constellation? i) *)
Theorem gen_51 : (nump Vcx) = true.  Proof. vm_compute. reflexivity. Qed.
(* (constellation? (62 2)) *)
Theorem gen_52 : (nump Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (whole? (62 2)) *)
Theorem gen_53 : (whole Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (atom? 'x) *)
Theorem gen_54 : (atomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(atom? '(1)) *)
Theorem gen_55 : (Z.leb (Z.b2z (atomp Vpair)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(lit? '(1)) *)
Theorem gen_56 : (Z.leb (Z.b2z (lamp Vpair)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(lit? "s") *)
Theorem gen_57 : (Z.leb (Z.b2z (lamp Vstr)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(lit? 5) *)
Theorem gen_58 : (Z.leb (Z.b2z (lamp Vz)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((64 2) = 2 * (63 2)) *)
Theorem gen_59 : (Z.eqb (appZ 64 2) (Z.mul 2 (appZ 63 2))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = 1 + 2) *)
Theorem gen_60 : (Z.eqb 3 (Z.add 1 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (2 = (// 5 2)) *)
Theorem gen_61 : (Z.eqb 2 (Z.quot 5 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = 5 % 2) *)
Theorem gen_62 : (Z.eqb 1 (Z.rem 5 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-2 = (// -5 2)) *)
Theorem gen_63 : (Z.eqb (-2) (Z.quot (-5) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-5 = ((- 0) 5)) *)
Theorem gen_64 : (Z.eqb (-5) ((Z.sub 0) 5)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'inf) *)
Theorem gen_65 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'nan) *)
Theorem gen_66 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'ieee-nan) *)
Theorem gen_67 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (976371285 = (100 2) % 1000000007) *)
Theorem gen_68 : (Z.eqb 976371285 (Z.rem (appZ 100 2) 1000000007)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((100 2) = (// (200 2) (100 2))) *)
Theorem gen_69 : (Z.eqb (appZ 100 2) (Z.quot (appZ 200 2) (appZ 100 2))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-2 = (^ 1 -1)) *)
Theorem gen_70 : (Z.eqb (-2) (Z.lxor 1 (-1))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (15 = (8 | 4 | 2 | 1)) *)
Theorem gen_71 : (Z.eqb 15 (Z.lor 8 (Z.lor 4 (Z.lor 2 1)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (16 = (>> 64 2)) *)
Theorem gen_72 : (Z.eqb 16 (shrz 64 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (16 = (<< 2 3)) *)
Theorem gen_73 : (Z.eqb 16 (shlz 2 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (768 = (<< 3 200)) *)
Theorem gen_74 : (Z.eqb 768 (shlz 3 200)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-1 = (>> -1 1)) *)
Theorem gen_75 : (Z.eqb (-1) (shrz (-1) 1)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-4 = (>> -8 1)) *)
Theorem gen_76 : (Z.eqb (-4) (shrz (-8) 1)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(3 = 4) *)
Theorem gen_77 : (Z.leb (Z.b2z (Z.eqb 3 4)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('x < "a") *)
Theorem gen_78 : (Z.ltb (vband Vsym) (vband Vstr)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("a" < 1) *)
Theorem gen_79 : (Z.ltb (vband Vstr) (vband Vz)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 < '(0)) *)
Theorem gen_80 : (Z.ltb (vband Vz) (vband Vpair)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(1 < "a") *)
Theorem gen_81 : (Z.leb (Z.b2z (Z.ltb (vband Vz) (vband Vstr))) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(0) < (hash 1 10)) *)
Theorem gen_82 : (Z.ltb (vband Vpair) (vband Vmap)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("abc" < "abd") *)
Theorem gen_83 : (lexltb [97;98;99] [97;98;100]) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ab" < "abc") *)
Theorem gen_84 : (lexltb [97;98] [97;98;99]) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) < '(1 3)) *)
Theorem gen_85 : (lexltb [1;2] [1;3]) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(1 = (\ x x)) *)
Theorem gen_86 : (Z.leb (Z.b2z false) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !((\ y y) = 1) *)
Theorem gen_87 : (Z.leb (Z.b2z false) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(0 = (\ _ 1)) *)
Theorem gen_88 : (Z.leb (Z.b2z false) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !((\ q 1) = 0) *)
Theorem gen_89 : (Z.leb (Z.b2z false) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (42 = ((\ x x) 42)) *)
Theorem gen_90 : (Z.eqb 42 ((fun v2 : Z => v2) 42)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = ((\ _ 1) 9)) *)
Theorem gen_91 : (Z.eqb 1 ((fun v5 : Z => 1) 9)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = ((\ x x) 5)) *)
Theorem gen_92 : (Z.eqb 5 ((fun v8 : Z => v8) 5)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = ((\ _ 1) 9)) *)
Theorem gen_93 : (Z.eqb 1 ((fun v11 : Z => 1) 9)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = ((\ x x) ((\ x x) 5))) *)
Theorem gen_94 : (Z.eqb 5 ((fun v17 : Z => v17) ((fun v16 : Z => v16) 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("abcd" = "ab" + "cd") *)
Theorem gen_95 : (leqb [97;98;99;100] ([97;98] ++ [99;100])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("xB" = "x" + 66) *)
Theorem gen_96 : (leqb [120;66] ([120] ++ [66])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('efef = 'ef + 'ef) *)
Theorem gen_97 : (leqb [101;102;101;102] ([101;102] ++ [101;102])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("abef" = "ab" + 'ef) *)
Theorem gen_98 : (leqb [97;98;101;102] ([97;98] ++ [101;102])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2 3 4) = '(1 2) + '(3 4)) *)
Theorem gen_99 : (leqb [1;2;3;4] ([1;2] ++ [3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(5 1 2) = 5 + '(1 2)) *)
Theorem gen_100 : (leqb [5;1;2] (5 :: [1;2])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(3) = '(3) + ()) *)
Theorem gen_101 : (uleq (Some [3]) (ulop (Some [3]) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(3) = () + '(3)) *)
Theorem gen_102 : (uleq (Some [3]) (ulop None (Some [3]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) = '(1 2) + (mint 0)) *)
Theorem gen_103 : (uleq (Some [1;2]) (ulop (Some [1;2]) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ab" = "ab" + ()) *)
Theorem gen_104 : (uleq (Some [97;98]) (ulop (Some [97;98]) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ab" = () + "ab") *)
Theorem gen_105 : (uleq (Some [97;98]) (ulop None (Some [97;98]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = 5 + ()) *)
Theorem gen_106 : (ueq (Some 5) (uop Z.add (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = () + 5) *)
Theorem gen_107 : (ueq (Some 5) (uop Z.add None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (() = () + ()) *)
Theorem gen_108 : (ueq None (uop Z.add None None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ababab" = "ab" * 3) *)
Theorem gen_109 : (leqb [97;98;97;98;97;98] (srep (Z.to_nat 3) [97;98])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('xyxy = 'xy * 2) *)
Theorem gen_110 : (leqb [120;121;120;121] (srep (Z.to_nat 2) [120;121])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2 1 2) = '(1 2) * 2) *)
Theorem gen_111 : (leqb [1;2;1;2] (srep (Z.to_nat 2) [1;2])) = true.  Proof. vm_compute. reflexivity. Qed.
(* !("ab" * -3) *)
Theorem gen_112 : (Z.leb (asum (srep (Z.to_nat (-3)) [97;98])) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !('(1 2) * 0) *)
Theorem gen_113 : (Z.leb (asum (srep (Z.to_nat 0) [1;2])) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = () * 5) *)
Theorem gen_114 : (ueq (Some 5) (uop Z.mul None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = 5 * ()) *)
Theorem gen_115 : (ueq (Some 5) (uop Z.mul (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ab" = "ab" * ()) *)
Theorem gen_116 : (uleq (Some [97;98]) (ulop (Some [97;98]) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ab" = () * "ab") *)
Theorem gen_117 : (uleq (Some [97;98]) (ulop None (Some [97;98]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) = '(1 2) * (mint 0)) *)
Theorem gen_118 : (uleq (Some [1;2]) (ulop (Some [1;2]) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (() = () * ()) *)
Theorem gen_119 : (ueq None (uop Z.mul None None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("" = "ab" * 0) *)
Theorem gen_120 : (leqb [] (srep (Z.to_nat 0) [97;98])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = 5 - ()) *)
Theorem gen_121 : (ueq (Some 5) (uop Z.sub (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = () - 5) *)
Theorem gen_122 : (ueq (Some 5) (uop Z.sub None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((64 2) = (64 2) - ()) *)
Theorem gen_123 : (ueq (Some (appZ 64 2)) (uop Z.sub (Some (appZ 64 2)) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = 5 / ()) *)
Theorem gen_124 : (ueq (Some 5) (uop Z.div (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = () / 5) *)
Theorem gen_125 : (ueq (Some 5) (uop Z.div None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (// 5 ())) *)
Theorem gen_126 : (ueq (Some 5) (uop Z.quot (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = 5 % ()) *)
Theorem gen_127 : (ueq (Some 5) (uop Z.rem (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (% () 5)) *)
Theorem gen_128 : (ueq (Some 5) (uop Z.rem None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (5 | ())) *)
Theorem gen_129 : (ueq (Some 5) (uop Z.lor (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (() & 5)) *)
Theorem gen_130 : (ueq (Some 5) (uop Z.land None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (^ 5 ())) *)
Theorem gen_131 : (ueq (Some 5) (uop Z.lxor (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (<< 5 ())) *)
Theorem gen_132 : (ueq (Some 5) (uop shlz (Some 5) None)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (>> () 5)) *)
Theorem gen_133 : (ueq (Some 5) (uop shrz None (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (2 = (5 - ()) - 3) *)
Theorem gen_134 : (ueq (Some 2) (uop Z.sub (uop Z.sub (Some 5) None) (Some 3))) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(5 = ()) *)
Theorem gen_135 : (Z.leb (Z.b2z (ueq (Some 5) None)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (() < 5) *)
Theorem gen_136 : (Z.ltb (unet None) (unet (Some 5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (('(1 2) * ()) = '(1 2)) *)
Theorem gen_137 : (uleq (ulop (Some [1;2]) None) (Some [1;2])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (() * '(3 4) = '(3 4)) *)
Theorem gen_138 : (uleq (ulop None (Some [3;4])) (Some [3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (abs -5)) *)
Theorem gen_139 : (Z.eqb 5 (Z.abs (-5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (charm? (abs -5)) *)
Theorem gen_140 : (fixp Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1024 = (10 2)) *)
Theorem gen_141 : (Z.eqb 1024 (appZ 10 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (21 = (gcd 1071 462)) *)
Theorem gen_142 : (Z.eqb 21 (Z.gcd 1071 462)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1024 = (10 2)) *)
Theorem gen_143 : (Z.eqb 1024 (appZ 10 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (27 = (3 3)) *)
Theorem gen_144 : (Z.eqb 27 (appZ 3 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (7625597484987 = (3 3 3)) *)
Theorem gen_145 : (Z.eqb 7625597484987 (appZ (appZ 3 3) 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (16 = (2 2 2)) *)
Theorem gen_146 : (Z.eqb 16 (appZ (appZ 2 2) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (65536 = (2 2 2 2)) *)
Theorem gen_147 : (Z.eqb 65536 (appZ (appZ (appZ 2 2) 2) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-8 = (3 -2)) *)
Theorem gen_148 : (Z.eqb (-8) (appZ 3 (-2))) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(twin? 5) *)
Theorem gen_149 : (Z.leb (Z.b2z (comp Vz)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (20 = (peep (tuple 10 20 30) 1 -1)) *)
Theorem gen_150 : (Z.eqb 20 (nth (Z.to_nat 1) [10;20;30] (-1))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-1 = (peep (tuple 10 20 30) 9 -1)) *)
Theorem gen_151 : (Z.eqb (-1) (nth (Z.to_nat 9) [10;20;30] (-1))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 11 22 33) = (tuple 1 2 3) + (tuple 10 20 30)) *)
Theorem gen_152 : (leqb [11;22;33] (vaddt [1;2;3] [10;20;30])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 2 4 6) = (tuple 1 2 3) * 2) *)
Theorem gen_153 : (leqb [2;4;6] (vscale 2 [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (60 = (net (tuple 10 20 30))) *)
Theorem gen_154 : (Z.eqb 60 (asum [10;20;30])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (30 = (max (tuple 10 30 20))) *)
Theorem gen_155 : (Z.eqb 30 (amax [10;30;20])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 0 1 2) = (iota 3)) *)
Theorem gen_156 : (leqb [0;1;2] (iotaZ 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(iota 0) *)
Theorem gen_157 : (Z.leb (asum (iotaZ 0)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(iota -1) *)
Theorem gen_158 : (Z.leb (asum (iotaZ (-1))) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (4950 = (net (iota 100))) *)
Theorem gen_159 : (Z.eqb 4950 (asum (iotaZ 100))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 3 5) = (// (tuple 7 11) 2)) *)
Theorem gen_160 : (leqb [3;5] (vquot 2 [7;11])) = true.  Proof. vm_compute. reflexivity. Qed.
(* !((tuple 1 2 3) + (tuple 1 2)) *)
Theorem gen_161 : (Z.leb (asum (vaddt [1;2;3] [1;2])) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = ((tuple 1 2 3) = (tuple 1 2))) *)
Theorem gen_162 : (Z.eqb 0 (Z.b2z (leqb [1;2;3] [1;2]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = ('sym = (tuple 1 2 3))) *)
Theorem gen_163 : (Z.eqb 0 (Z.b2z false)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = ("ab" = (tuple 1 2))) *)
Theorem gen_164 : (Z.eqb 0 (Z.b2z (leqb [97;98] [1;2]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = (cap '(1 2 3))) *)
Theorem gen_165 : (Z.eqb 1 (hd 0 [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(2 3) = (cup '(1 2 3))) *)
Theorem gen_166 : (leqb [2;3] (skipn (Z.to_nat 1) [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (caup '(2 3 4))) *)
Theorem gen_167 : (Z.eqb 3 (nth (Z.to_nat 1) [2;3;4] 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) = (link 1 (link 2 ()))) *)
Theorem gen_168 : (leqb [1;2] (1 :: (2 :: []))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("x" = (string 'x)) *)
Theorem gen_169 : (leqb [120] [120]) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(two? 'x) *)
Theorem gen_170 : (Z.leb (Z.b2z (chainp Vsym)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(0 1 2) = (jot 3)) *)
Theorem gen_171 : (leqb [0;1;2] (iotaZ 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(jot 0) *)
Theorem gen_172 : (Z.leb (asum (iotaZ 0)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = $(jot 3)) *)
Theorem gen_173 : (Z.eqb 3 (Z.max 0 (asum (iotaZ 3)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(2 3 4) = (map (+ 1) '(1 2 3))) *)
Theorem gen_174 : (leqb [2;3;4] (map (Z.add 1) [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (24 = (foldl ( * ) 1 '(1 2 3 4))) *)
Theorem gen_175 : (Z.eqb 24 (fold_left Z.mul [1;2;3;4] 1)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (6 = (foldr (+) 0 '(1 2 3))) *)
Theorem gen_176 : (Z.eqb 6 (fold_right Z.add 0 [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 3) = (filter (\ x (x % 2)) '(1 2 3 4))) *)
Theorem gen_177 : (leqb [1;3] (filter (fun v29 : Z => Z.ltb 0 (Z.rem v29 2)) [1;2;3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2 3) = (sort '(3 1 2))) *)
Theorem gen_178 : (leqb [1;2;3] (insort [3;1;2])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(3 2 1) = (sortby (>) '(1 2 3))) *)
Theorem gen_179 : (leqb [3;2;1] (insortby (fun a b : Z => Z.leb b a) [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2 3 4) = (cat '(1 2) '(3 4))) *)
Theorem gen_180 : (leqb [1;2;3;4] ([1;2] ++ [3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(3 2 1) = (rev '(1 2 3))) *)
Theorem gen_181 : (leqb [3;2;1] (rev [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) = (take 2 '(1 2 3 4))) *)
Theorem gen_182 : (leqb [1;2] (firstn (Z.to_nat 2) [1;2;3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(3 4) = (drop 2 '(1 2 3 4))) *)
Theorem gen_183 : (leqb [3;4] (skipn (Z.to_nat 2) [1;2;3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (last '(1 2 3))) *)
Theorem gen_184 : (Z.eqb 3 (last [1;2;3] 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2) = (init '(1 2 3))) *)
Theorem gen_185 : (leqb [1;2] (removelast [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (member? 3 '(1 2 3)) *)
Theorem gen_186 : (existsb (Z.eqb 3) [1;2;3]) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(member? 9 '(1 2 3)) *)
Theorem gen_187 : (Z.leb (Z.b2z (existsb (Z.eqb 9) [1;2;3])) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (all (\ x (0 < x)) '(1 2 3)) *)
Theorem gen_188 : (forallb (fun v31 : Z => (Z.ltb 0 v31)) [1;2;3]) = true.  Proof. vm_compute. reflexivity. Qed.
(* (any (\ x (2 < x)) '(1 2 3)) *)
Theorem gen_189 : (existsb (fun v33 : Z => (Z.ltb 2 v33)) [1;2;3]) = true.  Proof. vm_compute. reflexivity. Qed.
(* (294 = $'(a b c)) *)
Theorem gen_190 : (Z.eqb 294 (Z.max 0 (asum [97;98;99]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (tally '(a b c))) *)
Theorem gen_191 : (Z.eqb 3 (Z.of_nat (length [97;98;99]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(0 = ()) *)
Theorem gen_192 : (Z.leb (Z.b2z (ueq (Some 0) None)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (name? 'x) *)
Theorem gen_193 : (namep Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (294 = $"abc") *)
Theorem gen_194 : (Z.eqb 294 (Z.max 0 (asum [97;98;99]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (tally "abc")) *)
Theorem gen_195 : (Z.eqb 3 (Z.of_nat (length [97;98;99]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (7 = $(tuple 3 4)) *)
Theorem gen_196 : (Z.eqb 7 (Z.max 0 (asum [3;4]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (abs -5)) *)
Theorem gen_197 : (Z.eqb 5 (Z.abs (-5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (97 = $(+ "a" 0)) *)
Theorem gen_198 : (Z.eqb 97 (Z.max 0 (asum ([97] ++ [0])))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("abcd" = (+ "ab" "cd")) *)
Theorem gen_199 : (leqb [97;98;99;100] ([97;98] ++ [99;100])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('asdf = (intern "asdf")) *)
Theorem gen_200 : (leqb [97;115;100;102] [97;115;100;102]) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("asdf" = (string 'asdf)) *)
Theorem gen_201 : (leqb [97;115;100;102] [97;115;100;102]) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(book? 5) *)
Theorem gen_202 : (Z.leb (Z.b2z (tabp Vz)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !0 *)
Theorem gen_203 : (Z.leb 0 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !"" *)
Theorem gen_204 : (Z.leb (asum []) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(iota 0) *)
Theorem gen_205 : (Z.leb (asum (iotaZ 0)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nil? 0) *)
Theorem gen_206 : (Z.leb 0 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(= () 0) *)
Theorem gen_207 : (Z.leb (Z.b2z (ueq None (Some 0))) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = (tally (iota 0))) *)
Theorem gen_208 : (Z.eqb 0 (Z.of_nat (length (iotaZ 0)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (book? (hash 0)) *)
Theorem gen_209 : (tabp Vmap) = true.  Proof. vm_compute. reflexivity. Qed.
(* (532 = $"hello") *)
Theorem gen_210 : (Z.eqb 532 (Z.max 0 (asum [104;101;108;108;111]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (tally "hello")) *)
Theorem gen_211 : (Z.eqb 5 (Z.of_nat (length [104;101;108;108;111]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (42 = $42) *)
Theorem gen_212 : (Z.eqb 42 (Z.max 0 42)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = !0) *)
Theorem gen_213 : (Z.eqb 1 (Z.b2z (Z.leb 0 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = !5) *)
Theorem gen_214 : (Z.eqb 0 (Z.b2z (Z.leb 5 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* !!5 *)
Theorem gen_215 : (Z.leb (Z.b2z (Z.leb 5 0)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (6 = ((\ x' (x' + 1)) 5)) *)
Theorem gen_216 : (Z.eqb 6 ((fun v36 : Z => (Z.add v36 1)) 5)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'ab') *)
Theorem gen_217 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = 1 + 2) *)
Theorem gen_218 : (Z.eqb 3 (Z.add 1 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (7 = 1 + 2 * 3) *)
Theorem gen_219 : (Z.eqb 7 (Z.add 1 (Z.mul 2 3))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-4 = 1 - 2 - 3) *)
Theorem gen_220 : (Z.eqb (-4) (Z.sub (Z.sub 1 2) 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = abs -3) *)
Theorem gen_221 : (Z.eqb 3 (Z.abs (-3))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (66 = (2 + 3 4)) *)
Theorem gen_222 : (Z.eqb 66 (Z.add 2 (appZ 3 4))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = (3 != 4)) *)
Theorem gen_223 : (Z.eqb 1 (Z.b2z (Z.leb (Z.b2z (Z.eqb 3 4)) 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = (3 != 3)) *)
Theorem gen_224 : (Z.eqb 0 (Z.b2z (Z.leb (Z.b2z (Z.eqb 3 3)) 0))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (197 = ($"ab" + 2)) *)
Theorem gen_225 : (Z.eqb 197 (Z.add (Z.max 0 (asum [97;98])) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = ((1 +) 2)) *)
Theorem gen_226 : (Z.eqb 3 ((Z.add 1) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = <'(1 2 3)) *)
Theorem gen_227 : (Z.eqb 1 (hd 0 [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (2 = <>'(1 2 3)) *)
Theorem gen_228 : (Z.eqb 2 (hd 0 (skipn (Z.to_nat 1) [1;2;3]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (6 = +'(1 2 3)) *)
Theorem gen_229 : (Z.eqb 6 (asum [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-1 = +'(-2 1)) *)
Theorem gen_230 : (Z.eqb (-1) (asum [(-2);1])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (24 = *'(1 2 3 4)) *)
Theorem gen_231 : (Z.eqb 24 (aprod [1;2;3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = |-5) *)
Theorem gen_232 : (Z.eqb 5 (Z.abs (-5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (+ 1 2)) *)
Theorem gen_233 : (Z.eqb 3 (Z.add 1 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (20 = ((hash 1 10 2 20) -> 2 0)) *)
Theorem gen_234 : (Z.eqb 20 (hget [(1, 10);(2, 20)] 2 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (12 = (foldl (+) 0 '(3 4 5))) *)
Theorem gen_235 : (Z.eqb 12 (fold_left Z.add [3;4;5] 0)) = true.  Proof. vm_compute. reflexivity. Qed.

(* 235 theorems generated from 654 asserts seen *)
