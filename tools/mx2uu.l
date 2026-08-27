; tools/mx2uu.l -- EMIT test/uumx.l: love's `+` and `*` dispatch matrices, read
; from THE TABLE (src/mx.l) and lifted to uu terms, so the BAND LATTICE is a
; theorem in the tree's own prover and not only in Rocq. Run on the host binary:
;
;   love tools/mx2uu.l > test/uumx.l        (or: make uumx)
;
; the uu twin of tools/mx2coq.l, and the band partition is DERIVED the same way
; -- kinds grouped by row+column equality across BOTH matrices at once -- so the
; two exports cannot disagree about what a band is. what differs is the metatheory
; and the shape of the proof: Rocq closes each square by vm_compute, uu closes it
; by CONVERSION, `idpath true` against a bounded forall the kernel runs. the terms
; then re-export through uu2coq/uu2lean, so mx.l's shape lands in three kernels.
;
; the lift: a kind is its index in the enum roster (0..14, mx.l's own order, which
; IS the dispatch lattice), a lane its index in appearance order, a band its class
; id. a square is a GRID -- a list of rows, one per kind -- and not a flat list
; indexed by arithmetic: every walk here rides `npred`, which the eager NbE prices
; at O(index), so a 225-long row costs 225x what a 15-long one does and the whole
; corpus feels it. hence mvec/mlist, nvec/nlist's shape one type up.
; names are emitted for every kind and lane (mxKCharm, mxLaddn) so test/uumxlaw.l
; can name a cell instead of counting to it. untrusted like every translator here:
; a wrong lift can miss a law, never fake one, and each row prints under its kind.
;
; load the table FIRST, in its own top-level form: the next form's bare mx-kinds /
; mx-add / mx-mul references resolve at ITS compile, which must follow this run.
(: _ (reads (open "src/mx.l" "r")) ())

(:
   nl (string 10)
   (join l) (foldl (+) "" l)
   (pl s) (puts (s + nl))
   (at l i) (? (0 < i) (at >l (i - 1)) <l)

   kinds mx-kinds                                    ; the kind noms, enum order
   addm mx-add                                       ; one row of lane noms per kind
   mulm mx-mul
   nk (tally kinds)
   ridx (jot nk)
   (cell m i j) (at (at m i) j)
   (rrow m i) (map (\ j (cell m i j)) ridx)
   (rcol m j) (map (\ i (cell m i j)) ridx)
   ; a kind's dispatch signature: its row AND column in BOTH matrices -- equal
   ; signature = same band. mx2coq derives the partition from this same datum.
   (sig i) (list (rrow addm i) (rcol addm i) (rrow mulm i) (rcol mulm i))
   (idxin x l k) (? (! (two? l)) -1 (<l = x) k (idxin x >l (k + 1)))
   sigs ((: (go is acc) (? (! (two? is)) (rev acc)
                           (: s (sig <is)
                              (go >is (? (-1 < (idxin s acc 0)) acc (link s acc))))))
         ridx ())
   (band i) (idxin (sig i) sigs 0)                   ; class id, enum-appearance order
   nb (tally sigs)
   reps (map (\ c ((: (go is) (? (c = (band <is)) <is (go >is))) ridx)) (jot nb))
   (members c) (filter (\ i (c = (band i))) ridx)

   ; --- the noms the artifact wears ---
   (kn i) ("mx" + string (at kinds i))               ; mxKCharm ..
   ; the table spells a lane as its C symbol (lvm_addn); the prefix says nothing
   ; here, so it comes off -- mxLaddn, matching mx.v's Laddn.
   (ln x) (: t (string x) ("mxL" + (? ("lvm_" = (snip t 0 4)) (snip t 4 (tally t)) t)))
   lanes ((: (go rows acc)                           ; distinct lanes, appearance order
            (? (! (two? rows)) (rev acc)
               (go >rows (foldl (\ a x (? (member? x a) a (link x a))) acc <rows))))
          (addm + mulm) ())
   (lcode x) (idxin x lanes 0)
   ; mirror: the argument swap's action on lanes. the positional degenerate pair
   ; trades places (bin_a answers the a operand, bin_b the b); the rest are fixed.
   (mir x) (: t (string x) (? (t = "lvm_bin_a") 'lvm_bin_b (t = "lvm_bin_b") 'lvm_bin_a x))

   ; --- literals: an nlist row, and a grid of them under their row names ---
   (rowlit cs) (: (go l) (? (two? l) (join ["(ncons " (show <l) " " (go >l) ")"]) "nnil")
                  (go cs))
   (flatdef nm cs cmt)
    (join [nl " ; " cmt nl " (defn " nm " nlist" nl "   " (rowlit cs) ")" nl])
   (griddef nm rows labs cmt)
    (: n (tally rows)
       (go r) (? (r = n) (join [nl "    mnil" (")" * n) ")" nl])
                 (join [nl "   ; " (at labs r) nl "   (mcons " (rowlit (at rows r)) (go (r + 1))]))
       (join [nl " ; " cmt nl " (defn " nm " mlist" (go 0)]))
   (rowsof m is js) (map (\ i (map (\ j (lcode (cell m i j))) js)) is)

   ; --- the header ---
   _ (pl "; test/uumx.l -- GENERATED from src/mx.l by tools/mx2uu.l. Do not edit;")
   _ (pl "; refresh with `make uumx`. love's `+` and `*` dispatch matrices as uu terms:")
   _ (pl "; a kind is its index in mx.l's enum roster (which IS the dispatch lattice), a")
   _ (pl "; lane its index in appearance order, a band its class id under row+column")
   _ (pl "; equality across BOTH matrices -- the partition tools/mx2coq.l derives too, off")
   _ (pl "; the same datum, so the Rocq model and this one cannot disagree about a band.")
   _ (pl "; test/uumxlaw.l then proves the laws OF THIS TABLE -- the same love datum")
   _ (pl "; love.c's mx.h is laid from -- so a dispatch edit that breaks the band quotient,")
   _ (pl "; or the mirror symmetry, fails a PROOF at `make test`. Substrate: test/uu.l.")
   _ (pl "")
   _ (pl "(assert (member? 'vof (names ())))")
   _ (pl "")
   _ (pl "; --- the codes ---")
   _ (pl "(assert")
   _ (map (\ i (pl (join [" (defn " (kn i) " nat " (show i) ")"]))) ridx)
   _ (map (\ x (pl (join [" (defn " (ln x) " nat " (show (lcode x)) ")"]))) lanes)
   _ (pl (join [" (defn mxnk nat " (show nk) ")"]))
   _ (pl (join [" (defn mxnl nat " (show (tally lanes)) ")"]))
   _ (pl (join [" (defn mxnb nat " (show nb) "))"]))
   _ (pl "")
   _ (pl "; --- mlist: nlist's shape one type up, a list of ROWS. the two walkers below")
   _ (pl "; --- take memb's form -- one nat_rect over the length, the index carried down")
   _ (pl "; --- -- and a grid keeps both of them short, which is the whole reason the")
   _ (pl "; --- square is not one flat list: npred is O(index) under the eager NbE ---")
   _ (pl "(assert")
   _ (pl " (defn mvec (pi n nat UU)")
   _ (pl "   (lam n (nat_rect (lam q UU) unit (lam k (lam r (total2 h nlist r))) n)))")
   _ (pl " (defn mlist UU (total2 n nat (mvec n)))")
   _ (pl " (defn mnil mlist (tpair 0 tt))")
   _ (pl " (defn mcons (pi x nlist (pi l mlist mlist))")
   _ (pl "   (lam x (lam l (tpair (succ (pr1 l)) (tpair x (pr2 l))))))")
   _ (pl " (defn mxnth (pi i nat (pi l nlist nat))")
   _ (pl "   (lam i (lam l ((nat_rect (lam q (pi v (nvec q) (pi j nat nat)))")
   _ (pl "                     (lam v (lam j 0))")
   _ (pl "                     (lam k (lam IH (lam v (lam j (bool_rect (lam x nat)")
   _ (pl "                              (pr1 v)")
   _ (pl "                              (IH (pr2 v) (npred j))")
   _ (pl "                              (niszero j))))))")
   _ (pl "                     (pr1 l))")
   _ (pl "                  (pr2 l) i))))")
   _ (pl " (defn mxrow (pi i nat (pi l mlist nlist))")
   _ (pl "   (lam i (lam l ((nat_rect (lam q (pi v (mvec q) (pi j nat nlist)))")
   _ (pl "                     (lam v (lam j nnil))")
   _ (pl "                     (lam k (lam IH (lam v (lam j (bool_rect (lam x nlist)")
   _ (pl "                              (pr1 v)")
   _ (pl "                              (IH (pr2 v) (npred j))")
   _ (pl "                              (niszero j))))))")
   _ (pl "                     (pr1 l))")
   _ (pl "                  (pr2 l) i))))")
   _ (pl " (2 = (vof 0 '(mxnth 1 (ncons 5 (ncons 2 (ncons 9 nnil))))))")
   _ (pl " (0 = (vof 0 '(mxnth 7 (ncons 5 nnil))))                        ; 0 off the end")
   _ (pl " (9 = (vof 0 '(mxnth 0 (mxrow 1 (mcons (ncons 5 nnil) (mcons (ncons 9 nnil) mnil)))))))")

   ; --- the tables ---
   klabs (map (\ i (string (at kinds i))) ridx)
   blabs (map (\ c (join ["band " (show c) " -- " (string (at kinds (at reps c)))])) (jot nb))
   bidx (jot nb)
   bimg (map (\ c (at reps c)) bidx)
   _ (pl "")
   _ (pl "; --- THE SQUARES, one row per kind in enum order ---")
   _ (puts "(assert")
   _ (puts (griddef "mxaddgrid" (rowsof addm ridx ridx) klabs
                    (join ["`+`: " (show nk) " kinds square, " (show (nk * nk)) " cells"])))
   _ (puts (griddef "mxmulgrid" (rowsof mulm ridx ridx) klabs
                    (join ["`*`: " (show nk) " kinds square, " (show (nk * nk)) " cells"])))
   _ (puts (flatdef "mxbandtbl" (map band ridx)
                    "the band partition, DERIVED by row+column equality over both squares"))
   _ (puts (flatdef "mxmirtbl" (map (\ x (lcode (mir x))) lanes)
                    "mirror: the argument swap's action on lanes, in code order"))
   _ (puts (griddef "mxaddbgrid" (rowsof addm bimg bimg) blabs
                    (join ["`+` on the band quotient, " (show (nb * nb)) " cells"])))
   _ (puts (griddef "mxmulbgrid" (rowsof mulm bimg bimg) blabs
                    (join ["`*` on the band quotient, " (show (nb * nb)) " cells"])))
   _ (pl "")
   _ (pl " ; --- and the six lookups they carry ---")
   _ (pl " (defn mxadd (pi a nat (pi b nat nat)) (lam a (lam b (mxnth b (mxrow a mxaddgrid)))))")
   _ (pl " (defn mxmul (pi a nat (pi b nat nat)) (lam a (lam b (mxnth b (mxrow a mxmulgrid)))))")
   _ (pl " (defn mxband (pi k nat nat) (lam k (mxnth k mxbandtbl)))")
   _ (pl " (defn mxmirror (pi l nat nat) (lam l (mxnth l mxmirtbl)))")
   _ (pl " (defn mxaddband (pi x nat (pi y nat nat))")
   _ (pl "   (lam x (lam y (mxnth y (mxrow x mxaddbgrid)))))")
   _ (pl " (defn mxmulband (pi x nat (pi y nat nat))")
   _ (pl "   (lam x (lam y (mxnth y (mxrow x mxmulbgrid))))))")
   _ (pl "")
   _ (pl "; --- the bounded forall the theorems close by: every kind, then the square ---")
   _ (pl "(assert")
   _ (pl " (defn mxall (pi n nat (pi f (pi k nat bool) bool))")
   _ (pl "   (lam n (lam f (nat_rect (lam q bool) true (lam k (lam r (andb (f k) r))) n))))")
   _ (pl " (defn mxsq (pi f (pi a nat (pi b nat bool)) bool)")
   _ (pl "   (lam f (mxall mxnk (lam a (mxall mxnk (lam b (f a b))))))))")
   (quit 0))
