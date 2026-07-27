; mx2coq: generate proof/rocq/mx.v from the dispatch-matrix dump, in love.
;
; tools/mxdump.c (a TU that includes love.c whole) prints the +/* dispatch
; matrices as data -- kind names and lane names in enum order -- to out/.mx.l.
; This tool reads that dump and emits Coq where the tables' SHAPE becomes
; machine-checked, the shape CLAUDE.md carries as prose today:
;   * the BAND partition is DERIVED, not assumed: kinds are grouped by
;     row+column equality across BOTH matrices at once, and the factorization
;     theorems check in Coq that the 256-cell tables carry no information
;     beyond that quotient: a band matrix plus the band assignment ("every
;     gem/array kind routes identically"). KMint is its own band, the unit
;     lane whole (the dispatchers early-out a mint first -- the fast path;
;     the matrix says the same thing, so nothing rests on the early-out).
;   * dispatch COMMUTES: mx[a][b] = mx[b][a] over the WHOLE square -- the
;     lane fn handles orientation, the matrix never does.
;   * the diagonal reads the lattice: one lane per band, the algebra ladder.
; An UNKNOWN lane in the dump (a pointer the dumper can't name) refuses here,
; loudly. Run: out/host/love tools/mx2coq.l > proof/rocq/mx.v (gate: test_mx).

(: (join l) (foldl (+) "" l)
   nl (string 10)

   (forms path) (: q (open path "r") e (mint 0)
                   ((: (g acc) (: r (sound q e) (? (= e r) (rev acc) (g (link r acc))))) ()))
   (at l i) (? (< 0 i) (at (cup l) (- i 1)) (cap l))

   d (forms "out/.mx.l")
   kinds (cup (at d 0))                                 ; 16 kind noms, enum order
   addm  (cup (at d 1))                                 ; 16 rows of 16 lane noms
   mulm  (cup (at d 2))
   nk (tally kinds)

   _ (? (any (\ r (member? 'UNKNOWN r)) (cat addm mulm))
        (: _ (say err ("mx2coq: UNKNOWN lane in the dump -- name it in mxdump.c" + nl)) (quit 1)) 0)

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
   (ln x) ("L" + (show x))                              ; lane nom -> Coq ctor
   lanes ((: (go rows acc)                              ; distinct lanes, appearance order
            (? (! rows) (rev acc)
               (go (cup rows) (foldl (\ a x (? (member? x a) a (link x a))) acc (cap rows)))))
          (cat addm mulm) ())

   (commas f l) (join (map (\ x (? (id? x (cap l)) (f x) ("; " + (f x)))) l))
   (bars f l)   (join (map (\ x (? (id? x (cap l)) (f x) (" | " + (f x)))) l))

   ; one dispatch table as nested matches: 16 rows, each an inner match on b
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

   _ (. (join `(
"(* proof/rocq/mx.v -- GENERATED from love.c's dispatch matrices by tools/mx2coq.l" nl
"   (via tools/mxdump.c, a TU that includes the core whole and prints the static" nl
"   tables as data). Do not edit; regenerate with `make test_mx`." nl
"" nl
"   The +/* dispatch matrices, their SHAPE machine-checked -- what the narrative" nl
"   states as prose: every kind collapses to a BAND (the partition is derived" nl
"   from the dumped tables by row+column equality, both ops at once), the 256-" nl
"   cell tables factor through the band quotient with nothing left over, the" nl
"   dispatch COMMUTES (orientation lives in the lane fn, never the table), and" nl
"   the diagonal reads the lattice: one lane per band, the algebra ladder." nl
"   KMint is its own band, the unit lane whole: the dispatchers early-out a" nl
"   bare mint as the UNIT before indexing (the fast path), and the matrix says" nl
"   the same thing (lvm_bin_unit), so nothing rests on the early-out and the" nl
"   theorems close by vm_compute over the WHOLE square. *)" nl
"From Stdlib Require Import List Arith." nl
"Import ListNotations." nl nl)))

   _ (. ("Inductive kind := " + (bars (\ i (kn i)) (jot nk)) + "." + nl))
   _ (. ("Inductive lane := " + (bars ln lanes) + "." + nl))
   _ (. ("Definition lane_eqb (x y : lane) : bool := match x, y with "
         + (join (map (\ x ((? (id? x (cap lanes)) "" " | ") + (ln x) + ", " + (ln x) + " => true")) lanes))
         + " | _, _ => false end." + nl + nl))
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
       + (thm "add_dispatch_commutes"     (allsq "lane_eqb (addmx a b) (addmx b a)"))
       + (thm "mul_dispatch_commutes"     (allsq "lane_eqb (mulmx a b) (mulmx b a)")) + nl))
   _ (. ("(* the lattice, read off the diagonal: one add/mul lane pair per band *)" + nl
       + "Theorem the_diagonal_reads_the_lattice :" + nl
       + "  map (fun k => (addmx k k, mulmx k k)) [" + (commas (\ c (kn (at reps c))) (jot nb)) + "]" + nl
       + "  = [" + (commas (\ c ("(" + (ln (cell addm (at reps c) (at reps c))) + ", "
                                     + (ln (cell mulm (at reps c) (at reps c))) + ")")) (jot nb)) + "]." + nl
       + "Proof. vm_compute. reflexivity. Qed." + nl + nl))
   _ (. ("Print Assumptions add_factors_through_bands.   (* must stay \"Closed under the global context\" *)" + nl
       + "Print Assumptions mul_dispatch_commutes." + nl))
   (quit 0))
