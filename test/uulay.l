; test/uulay.l -- the layout ledger: crew/lux/layout.l's rt-tile, mirrored in
; uu at the window level, and the tiling theorem proved of it -- rt-tile places
; every window in the row exactly once, in row order (xmonad's prop_tiling).
; Geometry is erased to an opaque rect token (the nat-only model can't carry
; floats or pixel arithmetic -- the exact pixel partition stays natively
; asserted in crew/lux/law.l); what survives erasure is the ledger: which
; windows get placed, how many strips each split yields, how take/drop/zip
; move the row. Substrate: test/uu.l (this file loads after it by sort order).

; --- the list vocabulary rt-tile leans on: take, drop, and placeat's window
; --- column (zip, keeping the left list; it truncates at the shorter) ---
(assert
 (defn ltake (pi k nat (pi l nlist nlist))
   (lam k ((nat_rect (lam q (pi l nlist nlist))
              (lam l nnil)
              (lam j (lam IH (lam l (bool_rect (lam x nlist)
                       nnil
                       (ncons (hd0 l) (IH (tl0 l)))
                       (niszero (nlen l))))))
              k))))
 (defn ldrop (pi k nat (pi l nlist nlist))
   (lam k ((nat_rect (lam q (pi l nlist nlist))
              (lam l l)
              (lam j (lam IH (lam l (bool_rect (lam x nlist)
                       nnil
                       (IH (tl0 l))
                       (niszero (nlen l))))))
              k))))
 (defn lzipl (pi a nlist (pi b nlist nlist))
   (lam a (lam b ((nat_rect (lam q (pi v (nvec q) (pi b2 nlist nlist)))
                     (lam v (lam b2 nnil))
                     (lam k (lam IH (lam v (lam b2 (bool_rect (lam x nlist)
                              nnil
                              (ncons (pr1 v) (IH (pr2 v) (tl0 b2)))
                              (niszero (nlen b2)))))))
                     (pr1 a))
                   (pr2 a) b))))
 ((vof 0 '(ncons 7 (ncons 9 nnil))) = (vof 0 '(ltake 2 (ncons 7 (ncons 9 (ncons 4 nnil))))))
 ((vof 0 '(ncons 4 nnil)) = (vof 0 '(ldrop 2 (ncons 7 (ncons 9 (ncons 4 nnil))))))
 ((vof 0 '(ncons 7 nnil)) = (vof 0 '(lzipl (ncons 7 (ncons 9 nnil)) (ncons 1 nnil)))))

; --- the splits, ledger face: splitv-eq's loop yields one strip per master,
; --- splitv-w one per frac, padfr a length-ns frac list -- only the counts
; --- survive the geometry erasure, and the counts are what the theorem eats ---
(assert
 (defn msplitveq (pi n nat (pi r nat nlist))
   (lam n (lam r (nat_rect (lam q nlist) nnil (lam j (lam IH (ncons r IH))) n))))
 (defn msplitvw (pi fr nlist (pi r nat nlist))
   (lam fr (lam r (nat_rect (lam q nlist) nnil (lam j (lam IH (ncons r IH))) (pr1 fr)))))
 (defn mpadfr (pi fr nlist (pi ns nat nlist))
   (lam fr (lam ns ((nat_rect (lam q (pi f nlist nlist))
                       (lam f nnil)
                       (lam j (lam IH (lam f (ncons (bool_rect (lam x nat) 1 (hd0 f) (niszero (nlen f)))
                                              (IH (tl0 f))))))
                       ns)
                     fr))))
 (2 = (vof 0 '(nlen (msplitveq 2 0))))
 (3 = (vof 0 '(nlen (mpadfr (ncons 5 nnil) 3)))))

; --- rt-tile's window flow, mirrored: empty row places nothing; a row that
; --- fits the master count zips into the master column; otherwise masters
; --- (take nm) and slaves (drop nm) zip their own strips and append ---
(assert
 (defn rtwins (pi nm nat (pi fr nlist (pi r nat (pi row nlist nlist))))
   (lam nm (lam fr (lam r (lam row
      (let n (nlen row)
        (bool_rect (lam x nlist)
          nnil
          (bool_rect (lam x nlist)
            (lapp (lzipl (ltake nm row) (msplitveq nm r))
                  (lzipl (ldrop nm row) (msplitvw (mpadfr fr (natminus n nm)) r)))
            (lzipl row (msplitveq n r))
            (natgtb n nm))
          (niszero n))))))))
 ; the ledger computes: split branch (nm 1, row (10 20 30)) and master branch
 ((vof 0 '(ncons 10 (ncons 20 (ncons 30 nnil))))
  = (vof 0 '(rtwins 1 nnil 0 (ncons 10 (ncons 20 (ncons 30 nnil))))))
 ((vof 0 '(ncons 10 (ncons 20 nnil)))
  = (vof 0 '(rtwins 5 nnil 0 (ncons 10 (ncons 20 nnil)))))
 ((vof 0 'nnil) = (vof 0 '(rtwins 1 nnil 0 nnil))))

; --- the lemma stack. lnil0: a length-0 list is nnil (unit-eta through
; --- maponpaths). ldroplen: drop's length is truncated subtraction --
; --- double rect down natminus's own shape, every corner definitional but
; --- k=0 (natminusn0). lzipltake / lziplveq / lziplpadvw: zip-with-enough-
; --- strips keeps the whole left column -- each split's strip count is
; --- definitionally in lockstep with the row it zips (msplitveq peels with
; --- take, mpadfr's succ head feeds msplitvw one strip per window), so each
; --- is one induction whose steps are conversions under a maponpaths.
; --- lapptakedrop: take ++ drop = the row, the classic, by rect on k with
; --- the row generalized (the inner rect's own IH goes unused, nateqbsymm
; --- style) ---
(assert
 (defq lnil0 (pi v (nvec 0) (paths nlist (tpair 0 v) nnil))
   (lam v (maponpaths unit nlist (lam u (tpair 0 u)) v tt (uu-unitpath v))))
 (defq ldroplen
   (pi k nat (pi l nlist (paths nat (nlen (ldrop k l)) (natminus (nlen l) k))))
   (lam k ((nat_rect
      (lam kq (pi l nlist (paths nat (nlen (ldrop kq l)) (natminus (nlen l) kq))))
      (lam l (pathsinv0 nat (natminus (nlen l) 0) (nlen l) (natminusn0 (nlen l))))
      (lam j (lam IH (lam l
        ((nat_rect
           (lam q (pi v (nvec q) (paths nat (nlen (ldrop (succ j) (tpair q v))) (natminus (nlen (tpair q v)) (succ j)))))
           (lam v (idpath 0))
           (lam m (lam u (lam v (IH (tpair m (pr2 v))))))
           (pr1 l))
         (pr2 l)))))
      k))))
 (defq lzipltake
   (pi k nat (pi l nlist (pi r nat (paths nlist (lzipl (ltake k l) (msplitveq k r)) (ltake k l)))))
   (lam k ((nat_rect
      (lam kq (pi l nlist (pi r nat (paths nlist (lzipl (ltake kq l) (msplitveq kq r)) (ltake kq l)))))
      (lam l (lam r (idpath nnil)))
      (lam j (lam IH (lam l (lam r
        (bool_rect
          (lam b (paths nlist
             (lzipl (bool_rect (lam x nlist) nnil (ncons (hd0 l) (ltake j (tl0 l))) b) (msplitveq (succ j) r))
             (bool_rect (lam x nlist) nnil (ncons (hd0 l) (ltake j (tl0 l))) b)))
          (idpath nnil)
          (maponpaths nlist nlist (lam t (ncons (hd0 l) t))
             (lzipl (ltake j (tl0 l)) (msplitveq j r))
             (ltake j (tl0 l))
             (IH (tl0 l) r))
          (niszero (nlen l)))))))
      k))))
 (defq lziplveq
   (pi a nlist (pi r nat (paths nlist (lzipl a (msplitveq (nlen a) r)) a)))
   (lam a (lam r ((nat_rect
      (lam q (pi v (nvec q) (paths nlist (lzipl (tpair q v) (msplitveq q r)) (tpair q v))))
      (lam v (pathsinv0 nlist (tpair 0 v) nnil (lnil0 v)))
      (lam m (lam IH (lam v
        (maponpaths nlist nlist (lam t (ncons (pr1 v) t))
           (lzipl (tpair m (pr2 v)) (msplitveq m r))
           (tpair m (pr2 v))
           (IH (pr2 v))))))
      (pr1 a))
     (pr2 a)))))
 (defq lziplpadvw
   (pi a nlist (pi fr nlist (pi r nat (paths nlist (lzipl a (msplitvw (mpadfr fr (nlen a)) r)) a))))
   (lam a (lam fr (lam r ((nat_rect
      (lam q (pi v (nvec q) (pi f nlist (paths nlist (lzipl (tpair q v) (msplitvw (mpadfr f q) r)) (tpair q v)))))
      (lam v (lam f (pathsinv0 nlist (tpair 0 v) nnil (lnil0 v))))
      (lam m (lam IH (lam v (lam f
        (maponpaths nlist nlist (lam t (ncons (pr1 v) t))
           (lzipl (tpair m (pr2 v)) (msplitvw (mpadfr (tl0 f) m) r))
           (tpair m (pr2 v))
           (IH (pr2 v) (tl0 f)))))))
      (pr1 a))
     (pr2 a) fr)))))
 (defq lapptakedrop
   (pi k nat (pi l nlist (paths nlist (lapp (ltake k l) (ldrop k l)) l)))
   (lam k ((nat_rect
      (lam kq (pi l nlist (paths nlist (lapp (ltake kq l) (ldrop kq l)) l)))
      (lam l (idpath l))
      (lam j (lam IH (lam l
        ((nat_rect
           (lam q (pi v (nvec q) (paths nlist (lapp (ltake (succ j) (tpair q v)) (ldrop (succ j) (tpair q v))) (tpair q v))))
           (lam v (pathsinv0 nlist (tpair 0 v) nnil (lnil0 v)))
           (lam m (lam u (lam v
             (maponpaths nlist nlist (lam t (ncons (pr1 v) t))
               (lapp (ltake j (tpair m (pr2 v))) (ldrop j (tpair m (pr2 v))))
               (tpair m (pr2 v))
               (IH (tpair m (pr2 v)))))))
           (pr1 l))
         (pr2 l)))))
      k)))))

; --- the tiling theorem: rtwins hands the row back -- every window placed,
; --- none invented, row order kept -- for every master count, frac list and
; --- rect. By rect on the row: the empty row is unit-eta; a nonempty row
; --- splits on natgtb (the master-count test) with a dependent bool_rect --
; --- the fits-the-masters branch is lziplveq, the split branch rewrites the
; --- slave count ns = n - nm into drop's length (ldroplen backwards), peels
; --- both zips (lziplpadvw, lzipltake), and closes with take ++ drop = row.
; --- rtwinsane rides it: a duplicate-free row tiles duplicate-free -- with
; --- zsane, every window is placed exactly once ---
(assert
 (defq rtwinsrow
   (pi nm nat (pi fr nlist (pi r nat (pi row nlist (paths nlist (rtwins nm fr r row) row)))))
   (lam nm (lam fr (lam r (lam row
      ((nat_rect
         (lam q (pi v (nvec q) (paths nlist (rtwins nm fr r (tpair q v)) (tpair q v))))
         (lam v (pathsinv0 nlist (tpair 0 v) nnil (lnil0 v)))
         (lam m (lam u (lam v
           (bool_rect
             (lam b (paths nlist
                (bool_rect (lam x nlist)
                  (lapp (lzipl (ltake nm (tpair (succ m) v)) (msplitveq nm r))
                        (lzipl (ldrop nm (tpair (succ m) v)) (msplitvw (mpadfr fr (natminus (succ m) nm)) r)))
                  (lzipl (tpair (succ m) v) (msplitveq (succ m) r))
                  b)
                (tpair (succ m) v)))
             (let D (ldrop nm (tpair (succ m) v))
              (let M (lzipl (ltake nm (tpair (succ m) v)) (msplitveq nm r))
               (pathscomp0 nlist
                 (lapp M (lzipl D (msplitvw (mpadfr fr (natminus (succ m) nm)) r)))
                 (lapp M (lzipl D (msplitvw (mpadfr fr (nlen D)) r)))
                 (tpair (succ m) v)
                 (maponpaths nat nlist
                   (lam t (lapp M (lzipl D (msplitvw (mpadfr fr t) r))))
                   (natminus (succ m) nm)
                   (nlen D)
                   (pathsinv0 nat (nlen D) (natminus (succ m) nm) (ldroplen nm (tpair (succ m) v))))
                 (pathscomp0 nlist
                   (lapp M (lzipl D (msplitvw (mpadfr fr (nlen D)) r)))
                   (lapp M D)
                   (tpair (succ m) v)
                   (maponpaths nlist nlist (lam t (lapp M t))
                     (lzipl D (msplitvw (mpadfr fr (nlen D)) r))
                     D
                     (lziplpadvw D fr r))
                   (pathscomp0 nlist
                     (lapp M D)
                     (lapp (ltake nm (tpair (succ m) v)) D)
                     (tpair (succ m) v)
                     (maponpaths nlist nlist (lam t (lapp t D))
                       M
                       (ltake nm (tpair (succ m) v))
                       (lzipltake nm (tpair (succ m) v) r))
                     (lapptakedrop nm (tpair (succ m) v)))))))
             (lziplveq (tpair (succ m) v) r)
             (natgtb (succ m) nm)))))
         (pr1 row))
       (pr2 row)))))))
 (defq rtwinsane
   (pi nm nat (pi fr nlist (pi r nat (pi row nlist (pi hs (paths bool (nodupb row) true)
      (paths bool (nodupb (rtwins nm fr r row)) true))))))
   (lam nm (lam fr (lam r (lam row (lam hs
      (pathscomp0 bool (nodupb (rtwins nm fr r row)) (nodupb row) true
        (maponpaths nlist bool (lam t (nodupb t)) (rtwins nm fr r row) row (rtwinsrow nm fr r row))
        hs))))))))
