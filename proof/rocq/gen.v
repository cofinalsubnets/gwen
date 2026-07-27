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
   valued for its shape-mismatch law), vquot, hget, and the syntactic kind lattice. *)
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
(* ('(1 2) = '(1 2)) *)
Theorem gen_7 : (leqb [1;2] [1;2]) = true.  Proof. vm_compute. reflexivity. Qed.
(* !0 *)
Theorem gen_8 : (Z.leb 0 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !"" *)
Theorem gen_9 : (Z.leb (asum []) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(tuple 0) *)
Theorem gen_10 : (Z.leb (asum [0]) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !-5 *)
Theorem gen_11 : (Z.leb (-5) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !!5 *)
Theorem gen_12 : (negb (Z.leb 5 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !!"x" *)
Theorem gen_13 : (negb (Z.leb (asum [120]) 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !'(0 0) *)
Theorem gen_14 : (Z.leb (asum [0;0]) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !'(-5) *)
Theorem gen_15 : (Z.leb (asum [(-5)]) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(tuple -3 -4) *)
Theorem gen_16 : (Z.leb (asum [(-3);(-4)]) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !'(-2 1) *)
Theorem gen_17 : (Z.leb (asum [(-2);1]) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !!'(0 1) *)
Theorem gen_18 : (negb (Z.leb (asum [0;1]) 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-1 = +'(-2 1)) *)
Theorem gen_19 : (Z.eqb (-1) (asum [(-2);1])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (charm? 5) *)
Theorem gen_20 : (fixp Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(charm? 3.5) *)
Theorem gen_21 : (negb (fixp Vflo)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(charm? (tuple 1 2 3)) *)
Theorem gen_22 : (negb (fixp Varr)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(charm? 'a) *)
Theorem gen_23 : (negb (fixp Vsym)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (charm? 5) *)
Theorem gen_24 : (fixp Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (two? '(1 2)) *)
Theorem gen_25 : (chainp Vpair) = true.  Proof. vm_compute. reflexivity. Qed.
(* (string? "hi") *)
Theorem gen_26 : (strp Vstr) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'x) *)
Theorem gen_27 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (book? (hash 1 2)) *)
Theorem gen_28 : (tabp Vmap) = true.  Proof. vm_compute. reflexivity. Qed.
(* (gem? 1.5) *)
Theorem gen_29 : (flop Vflo) = true.  Proof. vm_compute. reflexivity. Qed.
(* (twin? i) *)
Theorem gen_30 : (comp Vcx) = true.  Proof. vm_compute. reflexivity. Qed.
(* (constellation? 1.5) *)
Theorem gen_31 : (nump Vflo) = true.  Proof. vm_compute. reflexivity. Qed.
(* (constellation? i) *)
Theorem gen_32 : (nump Vcx) = true.  Proof. vm_compute. reflexivity. Qed.
(* (constellation? (62 2)) *)
Theorem gen_33 : (nump Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (whole? (62 2)) *)
Theorem gen_34 : (whole Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (atom? 'x) *)
Theorem gen_35 : (atomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(atom? '(1)) *)
Theorem gen_36 : (negb (atomp Vpair)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(lit? '(1)) *)
Theorem gen_37 : (negb (lamp Vpair)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(lit? "s") *)
Theorem gen_38 : (negb (lamp Vstr)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(lit? 5) *)
Theorem gen_39 : (negb (lamp Vz)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((64 2) = 2 * (63 2)) *)
Theorem gen_40 : (Z.eqb (appZ 64 2) (Z.mul 2 (appZ 63 2))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = 1 + 2) *)
Theorem gen_41 : (Z.eqb 3 (Z.add 1 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (2 = (// 5 2)) *)
Theorem gen_42 : (Z.eqb 2 (Z.quot 5 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 = 5 % 2) *)
Theorem gen_43 : (Z.eqb 1 (Z.rem 5 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-2 = (// -5 2)) *)
Theorem gen_44 : (Z.eqb (-2) (Z.quot (-5) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'inf) *)
Theorem gen_45 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'nan) *)
Theorem gen_46 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'ieee-nan) *)
Theorem gen_47 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (976371285 = (100 2) % 1000000007) *)
Theorem gen_48 : (Z.eqb 976371285 (Z.rem (appZ 100 2) 1000000007)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((100 2) = (// (200 2) (100 2))) *)
Theorem gen_49 : (Z.eqb (appZ 100 2) (Z.quot (appZ 200 2) (appZ 100 2))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-2 = (^ 1 -1)) *)
Theorem gen_50 : (Z.eqb (-2) (Z.lxor 1 (-1))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (15 = (8 | 4 | 2 | 1)) *)
Theorem gen_51 : (Z.eqb 15 (Z.lor 8 (Z.lor 4 (Z.lor 2 1)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(3 = 4) *)
Theorem gen_52 : (negb (Z.eqb 3 4)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('x < "a") *)
Theorem gen_53 : (Z.ltb (vband Vsym) (vband Vstr)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("a" < 1) *)
Theorem gen_54 : (Z.ltb (vband Vstr) (vband Vz)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1 < '(0)) *)
Theorem gen_55 : (Z.ltb (vband Vz) (vband Vpair)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(1 < "a") *)
Theorem gen_56 : (negb (Z.ltb (vband Vz) (vband Vstr))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(0) < (hash 1 10)) *)
Theorem gen_57 : (Z.ltb (vband Vpair) (vband Vmap)) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("abcd" = "ab" + "cd") *)
Theorem gen_58 : (leqb [97;98;99;100] ([97;98] ++ [99;100])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2 3 4) = '(1 2) + '(3 4)) *)
Theorem gen_59 : (leqb [1;2;3;4] ([1;2] ++ [3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(5 1 2) = 5 + '(1 2)) *)
Theorem gen_60 : (leqb [5;1;2] (5 :: [1;2])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("ababab" = "ab" * 3) *)
Theorem gen_61 : (leqb [97;98;97;98;97;98] (srep (Z.to_nat 3) [97;98])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ('(1 2 1 2) = '(1 2) * 2) *)
Theorem gen_62 : (leqb [1;2;1;2] (srep (Z.to_nat 2) [1;2])) = true.  Proof. vm_compute. reflexivity. Qed.
(* !("ab" * -3) *)
Theorem gen_63 : (Z.leb (asum (srep (Z.to_nat (-3)) [97;98])) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !('(1 2) * 0) *)
Theorem gen_64 : (Z.leb (asum (srep (Z.to_nat 0) [1;2])) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("" = "ab" * 0) *)
Theorem gen_65 : (leqb [] (srep (Z.to_nat 0) [97;98])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (abs -5)) *)
Theorem gen_66 : (Z.eqb 5 (Z.abs (-5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (charm? (abs -5)) *)
Theorem gen_67 : (fixp Vz) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1024 = (10 2)) *)
Theorem gen_68 : (Z.eqb 1024 (appZ 10 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (21 = (gcd 1071 462)) *)
Theorem gen_69 : (Z.eqb 21 (Z.gcd 1071 462)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (1024 = (10 2)) *)
Theorem gen_70 : (Z.eqb 1024 (appZ 10 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (27 = (3 3)) *)
Theorem gen_71 : (Z.eqb 27 (appZ 3 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (7625597484987 = (3 3 3)) *)
Theorem gen_72 : (Z.eqb 7625597484987 (appZ (appZ 3 3) 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (16 = (2 2 2)) *)
Theorem gen_73 : (Z.eqb 16 (appZ (appZ 2 2) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (65536 = (2 2 2 2)) *)
Theorem gen_74 : (Z.eqb 65536 (appZ (appZ (appZ 2 2) 2) 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-8 = (3 -2)) *)
Theorem gen_75 : (Z.eqb (-8) (appZ 3 (-2))) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(twin? 5) *)
Theorem gen_76 : (negb (comp Vz)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (20 = (peep (tuple 10 20 30) 1 -1)) *)
Theorem gen_77 : (Z.eqb 20 (nth (Z.to_nat 1) [10;20;30] (-1))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-1 = (peep (tuple 10 20 30) 9 -1)) *)
Theorem gen_78 : (Z.eqb (-1) (nth (Z.to_nat 9) [10;20;30] (-1))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 11 22 33) = (tuple 1 2 3) + (tuple 10 20 30)) *)
Theorem gen_79 : (leqb [11;22;33] (vaddt [1;2;3] [10;20;30])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 2 4 6) = (tuple 1 2 3) * 2) *)
Theorem gen_80 : (leqb [2;4;6] (vscale 2 [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (60 = (net (tuple 10 20 30))) *)
Theorem gen_81 : (Z.eqb 60 (asum [10;20;30])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (30 = (max (tuple 10 30 20))) *)
Theorem gen_82 : (Z.eqb 30 (amax [10;30;20])) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 0 1 2) = (iota 3)) *)
Theorem gen_83 : (leqb [0;1;2] (iotaZ 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(iota 0) *)
Theorem gen_84 : (Z.leb (asum (iotaZ 0)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(iota -1) *)
Theorem gen_85 : (Z.leb (asum (iotaZ (-1))) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (4950 = (net (iota 100))) *)
Theorem gen_86 : (Z.eqb 4950 (asum (iotaZ 100))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ((tuple 3 5) = (// (tuple 7 11) 2)) *)
Theorem gen_87 : (leqb [3;5] (vquot 2 [7;11])) = true.  Proof. vm_compute. reflexivity. Qed.
(* !((tuple 1 2 3) + (tuple 1 2)) *)
Theorem gen_88 : (Z.leb (asum (vaddt [1;2;3] [1;2])) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(two? 'x) *)
Theorem gen_89 : (negb (chainp Vsym)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (name? 'x) *)
Theorem gen_90 : (namep Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (tally "abc")) *)
Theorem gen_91 : (Z.eqb 3 (Z.of_nat (length [97;98;99]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (abs -5)) *)
Theorem gen_92 : (Z.eqb 5 (Z.abs (-5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* ("abcd" = (+ "ab" "cd")) *)
Theorem gen_93 : (leqb [97;98;99;100] ([97;98] ++ [99;100])) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(book? 5) *)
Theorem gen_94 : (negb (tabp Vz)) = true.  Proof. vm_compute. reflexivity. Qed.
(* !0 *)
Theorem gen_95 : (Z.leb 0 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !"" *)
Theorem gen_96 : (Z.leb (asum []) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* !(iota 0) *)
Theorem gen_97 : (Z.leb (asum (iotaZ 0)) 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nil? 0) *)
Theorem gen_98 : (Z.leb 0 0) = true.  Proof. vm_compute. reflexivity. Qed.
(* (0 = (tally (iota 0))) *)
Theorem gen_99 : (Z.eqb 0 (Z.of_nat (length (iotaZ 0)))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (book? (hash 0)) *)
Theorem gen_100 : (tabp Vmap) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = (tally "hello")) *)
Theorem gen_101 : (Z.eqb 5 (Z.of_nat (length [104;101;108;108;111]))) = true.  Proof. vm_compute. reflexivity. Qed.
(* !!5 *)
Theorem gen_102 : (negb (Z.leb 5 0)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (nom? 'ab') *)
Theorem gen_103 : (nomp Vsym) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = 1 + 2) *)
Theorem gen_104 : (Z.eqb 3 (Z.add 1 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (7 = 1 + 2 * 3) *)
Theorem gen_105 : (Z.eqb 7 (Z.add 1 (Z.mul 2 3))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-4 = 1 - 2 - 3) *)
Theorem gen_106 : (Z.eqb (-4) (Z.sub (Z.sub 1 2) 3)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = abs -3) *)
Theorem gen_107 : (Z.eqb 3 (Z.abs (-3))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (66 = (2 + 3 4)) *)
Theorem gen_108 : (Z.eqb 66 (Z.add 2 (appZ 3 4))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (6 = +'(1 2 3)) *)
Theorem gen_109 : (Z.eqb 6 (asum [1;2;3])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (-1 = +'(-2 1)) *)
Theorem gen_110 : (Z.eqb (-1) (asum [(-2);1])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (24 = *'(1 2 3 4)) *)
Theorem gen_111 : (Z.eqb 24 (aprod [1;2;3;4])) = true.  Proof. vm_compute. reflexivity. Qed.
(* (5 = |-5) *)
Theorem gen_112 : (Z.eqb 5 (Z.abs (-5))) = true.  Proof. vm_compute. reflexivity. Qed.
(* (3 = (+ 1 2)) *)
Theorem gen_113 : (Z.eqb 3 (Z.add 1 2)) = true.  Proof. vm_compute. reflexivity. Qed.
(* (20 = ((hash 1 10 2 20) -> 2 0)) *)
Theorem gen_114 : (Z.eqb 20 (hget [(1, 10);(2, 20)] 2 0)) = true.  Proof. vm_compute. reflexivity. Qed.

(* 114 theorems generated from 641 asserts seen *)
