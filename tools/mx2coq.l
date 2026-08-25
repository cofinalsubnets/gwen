; mx2coq: generate test/proof/rocq/mx.v from THE TABLE (mx.l), in love.
;
; the +/* dispatch matrices are love data now, and love.c's mx.h is laid from the
; same datum -- so this file and the C it models cannot drift, which is the whole
; reason rung 2 of doc/misc/clay.md exists. run it with the table catted ahead:
;   cat mx.l tools/mx2coq.l | out/host/love > test/proof/rocq/mx.v   (gate: test_mx)
;
; it emits Coq where the tables' SHAPE becomes machine-checked, the shape
; CLAUDE.md carries as prose:
;   * the BAND partition is DERIVED, not assumed: kinds are grouped by
;     row+column equality across BOTH matrices at once, and the factorization
;     theorems check in Coq that the tables carry no information
;     beyond that quotient: a band matrix plus the band assignment ("every
;     gem/array kind routes identically"). KMint is its own band, the unit
;     lane whole (the dispatchers early-out a mint first -- the fast path;
;     the matrix says the same thing, so nothing rests on the early-out).
;   * dispatch COMMUTES: mx[a][b] = mirror mx[b][a] over the WHOLE square,
;     where mirror swaps the positional pair (bin_a/bin_b, "answer the a/b
;     operand") and fixes every other lane -- a symmetric lane handles
;     orientation itself, the positional pair carries it in the name, and
;     either way the table adds none of its own.
;   * the diagonal reads the lattice: one lane per band, the algebra ladder.
; what the dump-shaped predecessor needed and this does not: tools/mxdump.c, the
; 47-line TU that #include'd love.c whole to read its own statics back out, the
; function-pointer comparison that named each lane, and the UNKNOWN case this file
; had to refuse on when it could not. the lane's symbol IS the datum now.

(: (join l) (foldl (+) "" l)
   nl (string 10)

   (at l i) (? (< 0 i) (at (cup l) (- i 1)) (cap l))

   ; the table comes from mx.l, catted ahead of this file -- love data, the
   ; same datum love.c's mx.h is laid from. it used to arrive as a DUMP: out/.mx.l,
   ; printed by tools/mxdump.c, a translation unit that #include'd love.c whole and
   ; recovered each lane by comparing function pointers, printing UNKNOWN when it
   ; could not name one -- a case this file had to refuse on. all of that is gone:
   ; here the lane's SYMBOL is the datum, so there is nothing to recover and nothing
   ; to fail to name.
   kinds mx-kinds                                       ; the kind noms, enum order
   addm  mx-add                                         ; one row of lane noms per kind
   mulm  mx-mul
   nk (tally kinds)

   ridx (jot nk)                                        ; every kind -- the whole square
   (cell m i j) (at (at m i) j)
   (rrow m i) (map (\ j (cell m i j)) ridx)
   (rcol m j) (map (\ i (cell m i j)) ridx)
   ; a kind's dispatch signature: its row AND column in BOTH matrices --
   ; equal signature = same band
   (sig i) (list (rrow addm i) (rcol addm i) (rrow mulm i) (rcol mulm i))
   (idxin x l k) (? (! (two? l)) -1 ((cap l) = x) k (idxin x (cup l) (+ 1 k)))
   sigs ((: (go is acc) (? (! is) (rev acc)
                           (: s (sig (cap is))
                              (go (cup is) (? (< -1 (idxin s acc 0)) acc (link s acc)))))) ridx ())
   (band i) (idxin (sig i) sigs 0)                      ; class id, enum-appearance order
   nb (tally sigs)
   reps (map (\ c ((: (go is) (? (= c (band (cap is))) (cap is) (go (cup is)))) ridx)) (jot nb))
   (members c) (filter (\ i (= c (band i))) ridx)

   (kn i) (show (at kinds i))                           ; KCharm ...
   ; lane nom -> Coq ctor. the table spells lanes as their C symbols (lvm_addn), and
   ; the prefix says nothing here, so it comes off: Laddn, not Llvm_addn.
   (ln x) (: t (show x) ("L" + (? (= "lvm_" (snip t 0 4)) (snip t 4 (tally t)) t)))
   lanes ((: (go rows acc)                              ; distinct lanes, appearance order
            (? (! rows) (rev acc)
               (go (cup rows) (foldl (\ a x (? (member? x a) a (link x a))) acc (cap rows)))))
          (cat addm mulm) ())

   (commas f l) (join (map (\ x (? (id? x (cap l)) (f x) ("; " + (f x)))) l))
   (bars f l)   (join (map (\ x (? (id? x (cap l)) (f x) (" | " + (f x)))) l))

   ; one dispatch table as nested matches: one row per kind, each an inner match on b
   (mxdef nm m)
     ("Definition " + nm + " (a b : kind) : lane :=" + nl + " match a with" + nl
      + (join (map (\ i (" | " + (kn i) + " => match b with "
                         + (join (map (\ j ((? (= 0 j) "" " | ") + (kn j) + " => " + (ln (cell m i j)))) (jot nk)))
                         + " end" + nl))
              (jot nk)))
      + " end." + nl)

   ; the band matrix as data: nb x nb lanes off the representatives
   (bandb nm m)
     ("Definition " + nm + " : list (list lane) := [" + nl
      + (join (map (\ c ("  [" + (commas (\ c2 (ln (cell m (at reps c) (at reps c2)))) (jot nb))
                         + "]" + (? (= c (- nb 1)) "" ";") + nl))
              (jot nb)))
      + " ]." + nl)

   _ (. (join [
"(* test/proof/rocq/mx.v -- GENERATED by tools/mx2coq.l from THE TABLE, mx.l --" nl
"   the same love datum love.c's mx.h is laid from, so the theorem and the code" nl
"   are two derivations of one source and cannot drift (doc/misc/clay.md rung 2)." nl
"   Do not edit; regenerate with `make test_mx`." nl
"" nl
"   The +/* dispatch matrices, their SHAPE machine-checked -- what the narrative" nl
"   states as prose: every kind collapses to a BAND (the partition is derived" nl
; the count is READ off the roster, so the sentence cannot outlive it
("   from the table by row+column equality, both ops at once), the "
+ (show ((tally kinds) * (tally kinds))) + "-") nl
"   cell tables factor through the band quotient with nothing left over, the" nl
"   dispatch COMMUTES up to mirror (a symmetric lane orients itself; the" nl
"   positional bin_a/bin_b pair carries orientation in the name), and" nl
"   the diagonal reads the lattice: one lane per band, the algebra ladder." nl
"   KMint is its own band, the unit lane whole: the dispatchers early-out a" nl
"   bare mint as the UNIT before indexing (the fast path), and the matrix says" nl
"   the same thing (lvm_bin_unit), so nothing rests on the early-out and the" nl
"   theorems close by vm_compute over the WHOLE square. *)" nl
"From Stdlib Require Import List Arith." nl
"Import ListNotations." nl nl]))

   _ (. ("Inductive kind := " + (bars (\ i (kn i)) (jot nk)) + "." + nl))
   _ (. ("Inductive lane := " + (bars ln lanes) + "." + nl))
   _ (. ("Definition lane_eqb (x y : lane) : bool := match x, y with "
         + (join (map (\ x ((? (id? x (cap lanes)) "" " | ") + (ln x) + ", " + (ln x) + " => true")) lanes))
         + " | _, _ => false end." + nl))
   ; mirror: the argument swap's action on lanes. the positional degenerate pair
   ; trades places (bin_a answers Sp[0], bin_b Sp[1]); every symmetric lane is fixed.
   (mir x) (: t (show x) (? (= t "lvm_bin_a") "Lbin_b" (= t "lvm_bin_b") "Lbin_a" (ln x)))
   _ (. ("Definition mirror (l : lane) : lane := match l with "
         + (join (map (\ x ((? (id? x (cap lanes)) "" " | ") + (ln x) + " => " + (mir x))) lanes))
         + " end." + nl + nl))
   _ (. (mxdef "addmx" addm))
   _ (. (mxdef "mulmx" mulm))
   _ (. ("Definition band (k : kind) : nat := match k with "
         + (join (map (\ c ((? (= 0 c) "" " | ") + (bars (\ i (kn i)) (members c)) + " => " + (show c))) (jot nb)))
         + " end." + nl))
   _ (. ("Definition rk : list kind := [" + (commas kn ridx) + "]." + nl + nl))
   _ (. (bandb "addb" addm))
   _ (. (bandb "mulb" mulm))
   _ (. ("Definition addband (x y : nat) : lane := nth y (nth x addb []) " + (ln (cap lanes)) + "." + nl))
   _ (. ("Definition mulband (x y : nat) : lane := nth y (nth x mulb []) " + (ln (cap lanes)) + "." + nl + nl))

   (thm nm s) ("Theorem " + nm + " : " + s + " = true." + nl + "Proof. vm_compute. reflexivity. Qed." + nl)
   (allsq f) ("forallb (fun a => forallb (fun b => " + f + ") rk) rk")
   _ (. ((thm "add_factors_through_bands" (allsq "lane_eqb (addmx a b) (addband (band a) (band b))"))
       + (thm "mul_factors_through_bands" (allsq "lane_eqb (mulmx a b) (mulband (band a) (band b))"))
       + (thm "add_dispatch_commutes"     (allsq "lane_eqb (addmx a b) (mirror (addmx b a))"))
       + (thm "mul_dispatch_commutes"     (allsq "lane_eqb (mulmx a b) (mirror (mulmx b a))")) + nl))
   _ (. ("(* the lattice, read off the diagonal: one add/mul lane pair per band *)" + nl
       + "Theorem the_diagonal_reads_the_lattice :" + nl
       + "  map (fun k => (addmx k k, mulmx k k)) [" + (commas (\ c (kn (at reps c))) (jot nb)) + "]" + nl
       + "  = [" + (commas (\ c ("(" + (ln (cell addm (at reps c) (at reps c))) + ", "
                                     + (ln (cell mulm (at reps c) (at reps c))) + ")")) (jot nb)) + "]." + nl
       + "Proof. vm_compute. reflexivity. Qed." + nl + nl))
   _ (. ("Print Assumptions add_factors_through_bands.   (* must stay \"Closed under the global context\" *)" + nl
       + "Print Assumptions mul_dispatch_commutes." + nl))
   (quit 0))
