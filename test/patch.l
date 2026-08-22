; test/patch.l -- the PATCH GROUPOID, the executable SOURCE-OF-TRUTH spec.
;
; the drift anchor for the whole patch stack: doc/proto/patch.l is the design (the
; argument that a distribution / namespace / repo-state are ONE algebra of
; selectable sets, with COMMUTE the primitive underneath). the proofs run on two
; legs. the GENERATED leg is the destination and is OPEN: test/uupatch.l encodes
; the named-slot AND positional models below as uu proof TERMS -- the uu kernel
; certifies all eight laws (L1..L3b at both grains, the hunk re-aim included),
; tools/uu2coq.l + tools/uu2lean.l emit them into uugen.v/uugen.lean, and Rocq AND
; Lean audit them axiom-free (make test_uugen + test_uulean). the HAND leg,
; test/proof/rocq/patch.v, scaffolds the slices whose models are still settling, under
; `make test_proof` (the conflictor merge and the frontier
; identity in full; the tree lift's commute ALGEBRA -- L1 with the path re-aim --
; the content-rewrite lane's L1 at element grain, the clash-segments bubble
; at hash grain (creation symmetric, the length law, padding commutes with
; rendering -- the widening certificate), and the metavariable lane's two
; halves (matching sound, the seat re-aim involutive), with the rose-tree
; apply laws asserted here and queued for their own uu migration); a hand proof
; drifts (the wm2uu lesson), so each slice migrates to the generated leg as its
; model stabilizes, and this file is the truth they all regenerate off.
;
; the named-slot model: a tree is a (key . value) assoc, a patch a single
; slot-change carrying its CONTEXT (old -> new -- not a bare diff), commute the
; partial swap. the laws:
;   L1  commute is involutive              L3a commute is sound: independent
;   L2  inverse round-trips (in context)       patches are ORDER-FREE
;                                          L3b a FRONTIER is stable under an
;                                              adjacent swap of independent patches
; this form's uu TWIN is test/uupatch.l: the same model name for name (chg pk pa
; pb valid apon invert commute applall), the same four laws as checked proof
; terms riding to rocq + lean. helpers stay LOCAL (the corpus runs concatenated
; in one scope); the asserts run inside the body, so the whole spec is one
; leak-free form.

(:
 ; --- the model (mirror of doc/proto/patch.l) ---
 (chg k a b) (link 'chg (link k (link a (link b 0))))     ; patch: change slot k, old->new
 (pk p) (cap (cup p))                                     ; the slot it touches
 (pa p) (cap (cup (cup p)))                               ; its context (old value)
 (pb p) (cap (cup (cup (cup p))))                         ; the new value
 (tget t k) (? (atom? t) 0 (: c (cap t) (? (id? k (cap c)) (cup c) (tget (cup t) k))))
 (tset t k v) (link (link k v) t)                         ; newest shadows; a missing slot reads 0
 (valid t p) (id? (tget t (pk p)) (pa p))                 ; a patch belongs iff its context matches
 (apon t p) (tset t (pk p) (pb p))                        ; lay the patch's new value
 (applall t ps) (foldl apon t ps)                         ; fold a whole frontier onto a tree
 (invert p) (chg (pk p) (pb p) (pa p))                    ; the groupoid inverse (unpull)
 (commute p q) (? (id? (pk p) (pk q)) 0 (link q (link p 0)))  ; partial: 0 = a dependency

 ; --- spec-local equality: deep-structural (patches are lists), and tree agreement
 ;     at a witness key set (trees are maps -> compare pointwise, as the .v does) ---
 (leq a b) (? (&& (atom? a) (atom? b)) (id? a b)
              (|| (atom? a) (atom? b)) 0
              (&& (leq (cap a) (cap b)) (leq (cup a) (cup b))))
 (teq a b ks) (? (atom? ks) 1 (&& (id? (tget a (cap ks)) (tget b (cap ks))) (teq a b (cup ks))))
 (cat2 a b) (? (atom? a) b (link (cap a) (cat2 (cup a) b)))

 ; --- witnesses ---
 px (chg 'x 0 1) py (chg 'y 0 2)                          ; independent -- different slots
 p1 (chg 'x 0 1) p2 (chg 'x 1 2)                          ; dependent -- same slot

 ; (L1) commute is INVOLUTIVE: swap two independent patches, swap the result back,
 ;      and you are home. same-slot patches DON'T commute -- 0, a dependency.
 _ (assert
    (leq (commute px py) (link py (link px 0)))                       ; p;q -> q;p
    (: c (commute px py) (leq (commute (cap c) (cap (cup c)))
                              (link px (link py 0))))                 ; ...and back to p;q
    (! (commute p1 p2))                                              ; same slot -> no commute
    (leq p1 p1) (! (leq px py)))                                     ; leq itself is honest

 ; (L2) INVERSE round-trips IN CONTEXT: where the patch is valid, apply then unpull
 ;      (apply the inverse) restores every slot; invert is its own inverse.
 _ (: t0 (tset 0 'x 5) p (chg 'x 5 9)
      (assert
       (valid t0 p)                                                  ; p belongs on t0
       (: t1 (apon (apon t0 p) (invert p)) (teq t1 t0 '(x y)))       ; apply;unpull == id
       (leq p (invert (invert p)))))                                 ; invert involutive

 ; (L3a) independent patches are ORDER-FREE (the semantic heart -- what makes merge
 ;       and cherry-pick sound): both pull orders land the same tree.
 _ (: a (apon (apon 0 px) py) b (apon (apon 0 py) px)
      (assert (teq a b '(x y)) (= 1 (tget a 'x)) (= 2 (tget a 'y))))

 ; (L3b) a FRONTIER is stable under swapping two adjacent independent patches -- the
 ;       reproducibility tie: meaning is order-free, so the (unordered) patch set is
 ;       a sound version identity. and the distribution face: two disjoint COMPONENTS
 ;       install to the same frontier whichever is pulled first.
 _ (: pre (link (chg 'a 0 7) 0) post (link (chg 'b 0 8) 0)
      f1 (applall 0 (cat2 pre (link px (link py post))))             ; pre ++ [px py] ++ post
      f2 (applall 0 (cat2 pre (link py (link px post))))             ; ...px,py swapped
      _ (assert (teq f1 f2 '(a b x y)))
      (: holo (link (chg 'holo 0 1) 0) glaze (link (chg 'glaze 0 1) 0)
         (assert (teq (applall 0 (cat2 holo glaze))
                      (applall 0 (cat2 glaze holo)) '(holo glaze)))))
 0)

; --- the POSITIONAL rung: hunks over a SEQUENCE, where commute genuinely RE-AIMS ---
;
; the form-tree fork's first slice: a file is a sequence of forms, a patch a HUNK
; -- (hunk i olds news): at index i the segment olds (its context: a hunk BELONGS
; somewhere) becomes news. named slots never shift; hunks DO -- commuting past an
; unequal-length hunk moves the other's index by the length delta, so the swapped
; q is a genuine re-aim, not the identity. the four laws are the same; the new
; teeth: TOUCHING hunks refuse to commute (strict, the sound choice -- two inserts
; on one seam have no canonical order; classic darcs's ambiguity corner), and
; valid carries a BOUND (an out-of-range insert still splices -- at the end -- but
; its inverse aims past the end, so the round-trip would lie; the bound keeps L2
; honest). this form's uu TWIN is test/uupatch.l's positional slice: the same
; model (mkh hi ho hn splice hap hinv hcommute happl), the same four laws as
; checked proof terms riding to rocq + lean, the re-aim's index algebra sealed
; lemma by lemma. same helpers-local discipline; a second leak-free form.

(:
 ; --- the model: sequences and hunks ---
 (hunk i o n) (link 'hunk (link i (link o (link n ()))))  ; at i, the segment o becomes n
 (hi p) (cap (cup p))                                     ; where it aims
 (ho p) (cap (cup (cup p)))                               ; its context (the old segment)
 (hn p) (cap (cup (cup (cup p))))                         ; the new segment
 (htake s n) (? (< n 1) () (link (cap s) (htake (cup s) (- n 1))))
 (hdrop s n) (? (< n 1) s (hdrop (cup s) (- n 1)))
 (hcat a b) (? (atom? a) b (link (cap a) (hcat (cup a) b)))
 (seg s i n) (htake (hdrop s i) n)                        ; the n elements from index i
 (splice s i c news) (hcat (htake s i) (hcat news (hdrop s (+ i c))))
 (heq a b) (? (&& (atom? a) (atom? b)) (id? a b)
              (|| (atom? a) (atom? b)) 0
              (&& (heq (cap a) (cap b)) (heq (cup a) (cup b))))
 (valid s p) (&& (<= (+ (hi p) (tally (ho p))) (tally s)) ; in range -- the round-trip bound
                 (heq (seg s (hi p) (tally (ho p))) (ho p)))
 (apon s p) (splice s (hi p) (tally (ho p)) (hn p))
 (applall s ps) (foldl apon s ps)
 (invert p) (hunk (hi p) (hn p) (ho p))

 ; --- commute: the re-aim. p;q -> q';p'. q strictly PAST p's news shifts BACK by
 ;     p's delta (j + |o1| - |n1|); q strictly BEFORE shifts p FORWARD by q's
 ;     delta. overlap OR touch answers () -- a dependency. ---
 (commute p q)
   (: i (hi p) o1 (tally (ho p)) n1 (tally (hn p))
      j (hi q) o2 (tally (ho q)) n2 (tally (hn q))
      (? (> j (+ i n1)) (link (hunk (- (+ j o1) n1) (ho q) (hn q)) (link p ()))
         (< (+ j o2) i) (link q (link (hunk (- (+ i n2) o2) (ho p) (hn p)) ()))
         ()))

 ; --- witnesses: a file of five forms; p GROWS the file (delta +1) ---
 s0 '(a b c d e)
 p (hunk 1 '(b) '(x y))                                   ; at 1, b -> x y
 q (hunk 4 '(d) '(z))                                     ; post-p coords: d sits at 4

 ; (L1) commute is INVOLUTIVE -- and the swap is a GENUINE RE-AIM: q comes back
 ;      at 3 (its pre-p seat), p rides unmoved, and swapping again is home.
 _ (: c (commute p q) cq (cap c) cp (cap (cup c))
      (assert
       (= 4 (hi q)) (= 3 (hi cq))                         ; the shift is real
       (heq cp p)
       (: c2 (commute cq cp)
          (&& (heq (cap c2) p) (heq (cap (cup c2)) q)))))

 ; dependencies: overlap, and the SEAM -- touching refuses too (strict)
 _ (assert
    (! (commute p (hunk 2 '(y) '(w))))                    ; lands inside p's news
    (! (commute p (hunk 3 '(c) '(w))))                    ; starts exactly at p's end
    (! (commute (hunk 1 () '(u)) (hunk 1 () '(v)))))      ; two inserts, one seam

 ; (L2) inverse round-trips IN CONTEXT -- and valid's BOUND has teeth: the
 ;      out-of-range insert is exactly the hunk whose round-trip would lie.
 _ (assert
    (valid s0 p)
    (heq (apon (apon s0 p) (invert p)) s0)
    (heq p (invert (invert p)))
    (! (valid '(a) (hunk 5 () '(u)))))                    ; past the end: no home

 ; (L3a) commute is SOUND: p;q and q';p' land the SAME sequence -- while q
 ;       UN-shifted does not even belong on s0 (the point of the re-aim).
 _ (: c (commute p q) cq (cap c) cp (cap (cup c))
      (assert
       (heq (apon (apon s0 p) q) (apon (apon s0 cq) cp))
       (heq (apon (apon s0 p) q) '(a x y c z e))
       (! (valid s0 q))))                                 ; naive swap: context miss

 ; a SHRINKING p (delta -1) shifts q the OTHER way -- same law, other sign
 _ (: pn (hunk 1 '(b c) '(u)) qn (hunk 3 '(e) '(w))       ; post-pn coords: e at 3
      c (commute pn qn) cq (cap c) cp (cap (cup c))
      (assert
       (= 4 (hi cq))                                      ; back UP to its s0 seat
       (heq (apon (apon s0 pn) qn) (apon (apon s0 cq) cp))
       (heq (apon (apon s0 pn) qn) '(a u d w))))

 ; the BEFORE branch: q lands ahead of p, so p is the one re-aimed (by q's delta)
 _ (: pb (hunk 3 '(d) '(w)) qb (hunk 0 '(a b) '(u))       ; qb shrinks the prefix
      c (commute pb qb) cq (cap c) cp (cap (cup c))
      (assert
       (heq cq qb) (= 2 (hi cp))                          ; q unmoved; p slides down
       (heq (apon (apon s0 pb) qb) (apon (apon s0 cq) cp))
       (heq (apon (apon s0 pb) qb) '(u c w e))))

 ; the elements are FORMS, not lines -- the love-native grain: a hunk swaps a whole
 ; definition and nothing above cares what an element IS.
 _ (: fs '((f a) (g b) (main ()))
      fp (hunk 1 '((g b)) '((g c) (h d)))
      (assert (valid fs fp)
              (heq (apon fs fp) '((f a) (g c) (h d) (main ())))))

 ; (L3b) a FRONTIER is stable under the adjacent independent swap -- the
 ;       reproducibility tie again, now with real shifting inside the list.
 _ (: c (commute p q)
      f1 (applall s0 (link (hunk 0 '(a) '(aa)) (link p (link q ()))))
      f2 (applall s0 (link (hunk 0 '(a) '(aa)) c))
      (assert (heq f1 f2) (heq f1 '(aa x y c z e))))
 0)

; --- the CONFLICTOR rung: a real merge RECORDS, and the state enlarges ---
;
; commute REFUSES a dependency; merge may not. the sound move (pijul's, read
; through our habit -- classic darcs 1.x made conflicts exotic patch algebra and
; got the exponential/unsound corner): ENLARGE THE STATE until merge is total. a
; conflicted slot holds a CLASH cell -- the shared base plus a canonically-ordered
; set of RIVALS -- so merge is SYMMETRIC and ASSOCIATIVE by construction (a set
; has no pull order), neither side silently wins, and RESOLUTION needs no new
; machinery: a clash cell is a first-class VALUE, so resolving is an ordinary chg
; whose old value is the cell -- invertible like any patch (unpull a resolution
; and the clash returns). `pull` is MERGE machinery, not honest application: it
; trusts the co-valid discipline (every pulled patch reads the shared base or a
; rival of it -- what two branches of one repo guarantee); `valid` stays the gate
; for a lone patch. rivals here are charms, so < is the canon; a general value
; order arrives with the clash-segments rung (form seven: content-hash canon).
; named-slot model (the clean case first, as ever); the segments ride there too.

(:
 ; --- the model: rung 1's trees and patches, plus the clash cell ---
 (chg k a b) (link 'chg (link k (link a (link b ()))))
 (pk p) (cap (cup p)) (pa p) (cap (cup (cup p))) (pb p) (cap (cup (cup (cup p))))
 (tget t k) (? (atom? t) 0 (: c (cap t) (? (id? k (cap c)) (cup c) (tget (cup t) k))))
 (tset t k v) (link (link k v) t)
 (invert p) (chg (pk p) (pb p) (pa p))
 (heq a b) (? (&& (atom? a) (atom? b)) (id? a b)
              (|| (atom? a) (atom? b)) 0
              (&& (heq (cap a) (cap b)) (heq (cup a) (cup b))))
 (teq a b ks) (? (atom? ks) 1 (&& (heq (tget a (cap ks)) (tget b (cap ks)))
                                  (teq a b (cup ks))))
 (clash a rs) (link 'clash (link a (link rs ())))          ; base a, rivals rs (sorted)
 (clash? v) (? (atom? v) 0 (id? (cap v) 'clash))
 (cbase c) (cap (cup c))
 (rivals c) (cap (cup (cup c)))
 (cins x rs) (? (atom? rs) (link x ())                     ; canonical insert, dedup --
                (: h (cap rs)                              ; the set that makes merge
                   (? (= x h) rs                           ; order-free
                      (< x h) (link x rs)
                      (link h (cins x (cup rs))))))

 ; --- pull: total merge-application. five lanes; the LAST is the discipline
 ;     backstop (a cell-shaped mismatch no-ops rather than inventing a story). ---
 (pull t p)
   (: k (pk p) a (pa p) b (pb p) v (tget t k)
      (? (heq v a) (tset t k b)                            ; the plain lane: context matches
         (heq v b) t                                       ; the same edit again: settled
         (&& (clash? v) (&& (heq (cbase v) a) (! (clash? b))))
           (tset t k (clash a (cins b (rivals v))))        ; join the rivals
         (&& (! (clash? v)) (! (clash? a)) (! (clash? b)))
           (tset t k (clash a (cins b (cins v ()))))       ; a rival landed first
         t))                                               ; discipline breach: no-op
 (applall t ps) (foldl pull t ps)
 (cat2 a b) (? (atom? a) b (link (cap a) (cat2 (cup a) b)))

 ; --- witnesses: three rivals for x over base 1, and an independent bystander ---
 t0 (tset 0 'x 1)
 p (chg 'x 1 2) q (chg 'x 1 3) r (chg 'x 1 4)
 w (chg 'y 0 9)

 ; (M1) merge is TOTAL and SYMMETRIC: rivals or bystanders, both pull orders
 ;      land the same tree -- the pushout apex, unique.
 _ (assert
    (teq (pull (pull t0 p) q) (pull (pull t0 q) p) '(x y))
    (teq (pull (pull t0 p) w) (pull (pull t0 w) p) '(x y)))

 ; (M2) the plain lane IS rung 1: a matching context applies as ever, and the
 ;      same edit arriving twice settles (idempotent).
 _ (assert
    (= 2 (tget (pull t0 p) 'x))
    (teq (pull (pull t0 p) p) (pull t0 p) '(x y)))

 ; (M3) a conflict is HONEST: the slot carries the base AND both rivals,
 ;      canonically ordered -- neither side silently won.
 _ (: m (pull (pull t0 q) p) c (tget m 'x)
      (assert (clash? c) (= 1 (cbase c)) (heq (rivals c) '(2 3))))

 ; (M4) three rivals, ANY order: one cell, one set -- the associativity that
 ;      broke classic darcs, answered by a set instead of patch algebra.
 _ (: m1 (pull (pull (pull t0 p) q) r)
      m2 (pull (pull (pull t0 r) q) p)
      m3 (pull (pull (pull t0 q) r) p)
      (assert (teq m1 m2 '(x)) (teq m2 m3 '(x))
              (heq (rivals (tget m1 'x)) '(2 3 4))))

 ; (M5) RESOLUTION is just a patch: old value = the cell, new = the choice --
 ;      the plain lane applies it, invert UNPULLS it and the clash returns.
 _ (: m (pull (pull t0 p) q) cell (tget m 'x)
      res (chg 'x cell 5)
      m2 (pull m res)
      (assert
       (= 5 (tget m2 'x)) (! (clash? (tget m2 'x)))
       (teq (pull m2 (invert res)) m '(x y))))

 ; and the distribution face: two components pinning ONE name differently
 ;      install to the same recorded clash whichever is pulled first.
 _ (: holo (link (chg 'font 0 1) ()) glaze (link (chg 'font 0 2) ())
      fa (applall 0 (cat2 holo glaze))
      fb (applall 0 (cat2 glaze holo))
      (assert (teq fa fb '(font)) (clash? (tget fa 'font))))
 0)

; --- the IDENTITY rung: a version IS its patch set, hashed canonically ---
;
; the reproducibility handle the laws already point at. L3b/M4 proved a
; frontier's MEANING is order-free, M2 that multiplicity is dead -- so an
; identity that quotients by exactly order and repetition is SOUND: it forgets
; nothing the semantics keeps. (dependent patches carry their ORDER in their
; contexts -- a frontier is a downward-closed SET, replay order is derived, not
; stored -- so the set identity is the right grain even when patches chain.)
; the shape: hash each patch's printed form (show is the canonical
; serialization), fold the hashes through cins -- the SAME canonical set that
; carries a clash's rivals, so order-freedom IS cins-commutativity and
; dup-freedom IS cins-absorption (both proven in patch.v) -- then hash the
; canonical list once for the scalar name. collision-freedom is engineering
; (djb2 here, a real digest when the store lands); the LAWS live on the
; canonical list.

(:
 (hstr h s i n) (? (= i n) h (hstr (% (+ (* h 33) (s i)) 4294967296) s (+ i 1) n))
 (hval v) (: s (show v) (hstr 5381 s 0 (tally s)))         ; djb2 over the printed form
 (cins x rs) (? (atom? rs) (link x ())
                (: h (cap rs) (? (= x h) rs (< x h) (link x rs)
                                 (link h (cins x (cup rs))))))
 (fid ps) (foldl (\ acc p (cins (hval p) acc)) () ps)      ; the canonical hash set
 (ident ps) (hval (fid ps))                                 ; one scalar to name it
 (chg k a b) (link 'chg (link k (link a (link b ()))))

 ; --- witnesses ---
 p (chg 'x 1 2) q (chg 'x 1 3) w (chg 'y 0 9)

 ; (H1) ORDER-FREE: any pull order, one identity -- the M4 face, on the name.
 _ (assert
    (= (ident (link p (link q (link w ()))))
       (ident (link w (link q (link p ())))))
    (= (ident (link p (link w ()))) (ident (link w (link p ())))))

 ; (H2) MULTIPLICITY-FREE: pulling twice settles (M2), and the name agrees.
 _ (assert (= (ident (link p (link p (link q ()))))
              (ident (link p (link q ())))))

 ; (H3) CONTENT-SENSITIVE: touch any patch and the name moves -- including
 ;      direction (a patch and its inverse are different content).
 _ (assert
    (! (= (ident (link p ())) (ident (link q ()))))
    (! (= (ident (link p (link q ()))) (ident (link p (link w ())))))
    (! (= (ident (link p ())) (ident (link (chg 'x 2 1) ())))))
 0)

; --- the TREE LIFT rung: a hunk gets a PATH, and the shift climbs the levels ---
;
; the form-tree grain made whole: a file is a TREE of forms (nested sequences),
; a patch a hunk with a POSTCODE -- the path to the sequence it splices. the
; load-bearing fact: a deep patch never changes the length of any ANCESTOR
; sequence, so only same-sequence patches shift each other's INDICES -- but a
; shallow splice moves whole SUBTREES, so it re-aims a deeper patch's path
; COMPONENT at the crossing level. commute gets four lanes by path relation:
;   same    the two hunks share a sequence -- rung 2 verbatim, paths ride
;   inq     q is deeper, crossing p's sequence at child j: j past p's news
;           slides BACK by p's delta (a genuine PATH re-aim); j inside p's
;           news is a dependency (q edits content p just wrote)
;   inp     p is deeper, crossing q's sequence at child j: j past q's olds
;           slides FORWARD by q's delta; j inside q's olds is a dependency --
;           q SWALLOWS p's subtree, and commuting would need to rewrite q's
;           context (undo p inside it): THE DOOR THE NEXT RUNG OPENS (form six)
;   apart   the paths diverge: disjoint subtrees, both ride untouched
; strictness at the crossing differs from rung 2's seam ON PURPOSE: a path
; component >= the splice's far edge names a SURVIVOR (an element that moved),
; not a seam position -- there is no insert-order ambiguity to refuse, so >=
; is sound where the seam needed >.

(:
 ; --- the sequence kit (rung 2, local) ---
 (htake s n) (? (< n 1) () (link (cap s) (htake (cup s) (- n 1))))
 (hdrop s n) (? (< n 1) s (hdrop (cup s) (- n 1)))
 (hcat a b) (? (atom? a) b (link (cap a) (hcat (cup a) b)))
 (seg s i n) (htake (hdrop s i) n)
 (splice s i c news) (hcat (htake s i) (hcat news (hdrop s (+ i c))))
 (nth s j) (cap (hdrop s j))
 (heq a b) (? (&& (atom? a) (atom? b)) (id? a b)
              (|| (atom? a) (atom? b)) 0
              (&& (heq (cap a) (cap b)) (heq (cup a) (cup b))))

 ; --- the tree kit: navigate to a path's sequence, lay a new one back ---
 (tnav t path) (? (atom? path) t (tnav (nth t (cap path)) (cup path)))
 (tlay t path s2)
   (? (atom? path) s2
      (splice t (cap path) 1
              (link (tlay (nth t (cap path)) (cup path) s2) ())))

 ; --- patches: (tp path i olds news) -- a hunk with a postcode ---
 (tp path i o n) (link 'tp (link path (link i (link o (link n ())))))
 (pp p) (cap (cup p))            (pi p) (cap (cup (cup p)))
 (po p) (cap (cup (cup (cup p)))) (pn p) (cap (cup (cup (cup (cup p)))))
 (apon t p) (tlay t (pp p) (splice (tnav t (pp p)) (pi p) (tally (po p)) (pn p)))
 (applall t ps) (foldl apon t ps)
 (invert p) (tp (pp p) (pi p) (pn p) (po p))
 (pathok t path) (? (atom? path) 1
                    (: j (cap path)
                       (&& (< j (tally t))
                           (: e (nth t j)
                              (&& (! (atom? e)) (pathok e (cup path)))))))
 (valid t p) (&& (pathok t (pp p))
                 (: s (tnav t (pp p))
                    (&& (<= (+ (pi p) (tally (po p))) (tally s))
                        (heq (seg s (pi p) (tally (po p))) (po p)))))

 ; --- path surgery + the path relation ---
 (setidx path d v) (? (= d 0) (link v (cup path))
                      (link (cap path) (setidx (cup path) (- d 1) v)))
 (rel P Q d) (? (&& (atom? P) (atom? Q)) '(same)
                (atom? P) (link 'inq (link (cap Q) (link d ())))
                (atom? Q) (link 'inp (link (cap P) (link d ())))
                (id? (cap P) (cap Q)) (rel (cup P) (cup Q) (+ d 1))
                '(apart))

 ; --- commute: four lanes, two kinds of re-aim (index at 'same, PATH at the
 ;     crossings), () a dependency ---
 (commute p q)
   (: P (pp p) ip (pi p) o1 (tally (po p)) n1 (tally (pn p)) dp (- n1 o1)
      Q (pp q) iq (pi q) o2 (tally (po q)) n2 (tally (pn q)) dq (- n2 o2)
      r (rel P Q 0) tag (cap r)
      (? (id? tag 'same)
           (? (> iq (+ ip n1)) (link (tp Q (- (+ iq o1) n1) (po q) (pn q)) (link p ()))
              (< (+ iq o2) ip) (link q (link (tp P (- (+ ip n2) o2) (po p) (pn p)) ()))
              ())
         (id? tag 'inq)
           (: j (cap (cup r)) d (cap (cup (cup r)))
              (? (< j ip) (link q (link p ()))
                 (>= j (+ ip n1))
                   (link (tp (setidx Q d (- j dp)) iq (po q) (pn q)) (link p ()))
                 ()))
         (id? tag 'inp)
           (: j (cap (cup r)) d (cap (cup (cup r)))
              (? (< j iq) (link q (link p ()))
                 (>= j (+ iq o2))
                   (link q (link (tp (setidx P d (+ j dq)) ip (po p) (pn p)) ()))
                 ()))
         (link q (link p ()))))

 ; --- witnesses: a little program -- three top forms, one nested ---
 t0 '((: a 1) (: b 2) (main (f x)))
 pr (tp () 0 () '((use m)))                                ; root insert at 0 (dp +1)
 qd (tp '(3) 1 '((f x)) '((g y)))                          ; deep edit, aimed POST-pr

 ; (L1) commute is INVOLUTIVE, and the re-aim is a PATH re-aim: qd's postcode
 ;      slides 3 -> 2 (its pre-pr seat), and swapping back is home.
 _ (: c (commute pr qd) cq (cap c) cp (cap (cup c))
      (assert
       (heq (pp qd) '(3)) (heq (pp cq) '(2))               ; the path moved
       (heq cp pr)
       (: c2 (commute cq cp)
          (&& (heq (cap c2) pr) (heq (cap (cup c2)) qd)))))

 ; dependencies: a deep edit inside freshly-written news, and the swallow
 ;      (q replaces the subtree p edited -- shut HERE; form six opens it)
 _ (assert
    (! (commute pr (tp '(0) 0 '(use) '(want))))            ; inside pr's news
    (! (commute (tp '(1) 2 '(1) '(7)) (tp () 1 '((: a 7)) ()))))  ; swallowed

 ; (L2) inverse round-trips at depth
 _ (: pd (tp '(2) 1 '((f x)) '((g y) (h z)))
      (assert
       (valid t0 pd)
       (heq (apon (apon t0 pd) (invert pd)) t0)
       (valid t0 (tp '(2 1) 0 () '(u)))                    ; (f x) IS a sequence: descend
       (! (valid t0 (tp '(9) 0 () '(u))))                  ; no such child
       (! (valid t0 (tp '(2 0) 0 () '(u))))))              ; main is a LEAF: no descent

 ; (L3a) commute is SOUND across levels: root insert then deep edit == re-aimed
 ;       deep edit then root insert -- and the un-shifted qd has no home on t0.
 _ (: c (commute pr qd) cq (cap c) cp (cap (cup c))
      (assert
       (heq (apon (apon t0 pr) qd) (apon (apon t0 cq) cp))
       (heq (apon (apon t0 pr) qd)
            '((use m) (: a 1) (: b 2) (main (g y))))
       (! (valid t0 qd))))

 ; the INP lane: a deep edit rides a root shrink the other way (path 2 -> 1)
 _ (: pd (tp '(2) 1 '((f x)) '((g y))) qr (tp () 0 '((: a 1)) ())
      c (commute pd qr) cq (cap c) cp (cap (cup c))
      (assert
       (heq cq qr) (heq (pp cp) '(1))                      ; p slides down
       (heq (apon (apon t0 pd) qr) (apon (apon t0 cq) cp))
       (heq (apon (apon t0 pd) qr) '((: b 2) (main (g y))))))

 ; APART: sibling subtrees ride untouched, both orders agree
 _ (: pa (tp '(0) 2 '(1) '(9)) qa (tp '(2) 1 '((f x)) '((g y)))
      c (commute pa qa)
      (assert
       (heq (cap c) qa) (heq (cap (cup c)) pa)
       (heq (apon (apon t0 pa) qa) (apon (apon t0 qa) pa))))

 ; SAME sequence at depth: rung 2's shift verbatim, riding a postcode
 _ (: ts '((a b c d e))
      p1 (tp '(0) 1 '(b) '(x y)) q1 (tp '(0) 4 '(d) '(z))
      c (commute p1 q1) cq (cap c)
      (assert
       (= 3 (pi cq)) (heq (pp cq) '(0))                    ; index shifts, path rides
       (heq (apon (apon ts p1) q1) (apon (apon ts cq) (cap (cup c))))
       (heq (apon (apon ts p1) q1) '((a x y c z e)))))

 ; (L3b) a FRONTIER is stable under the adjacent independent swap, mixed grains
 _ (: c (commute pr qd)
      f1 (applall t0 (link pr (link qd ())))
      f2 (applall t0 c)
      (assert (heq f1 f2)))
 0)

; --- the CONTENT-REWRITE rung: the swallow commutes -- context rewrites THROUGH ---
;
; the door the tree lift shut by fiat: a shallow patch whose olds SWALLOW the
; subtree a deep patch edited (and the mirror: a deep patch aimed inside content
; a shallow patch just laid). the dependency was never real when the shallow
; patch MOVES the content: undo the deep edit inside the shallow context -- BOTH
; sides, olds and news, carry it -- and re-aim the deep patch at the content's
; new seat. the machinery is MATCHING: find where the edited subtree SURVIVES
; on the far side. the unit walk descends the deep path until a subtree of the
; swallowed element occurs over there (so a wrap or an unwrap commutes too --
; the survivor is found at any depth, and the re-aimed path grows or sheds a
; component), and four occurrence guards keep the match CANONICAL: the unit
; exactly once on each side in its post state, never in its pre state.
; ambiguity REFUSES -- a deleted subtree is a REAL dependency (the deep edit
; would be erased), a duplicate has no single home, and a news already carrying
; the pre state could not tell undo from content. matching ground terms is
; one-sided unification; when patches grow METAVARIABLES the full unifier takes
; this seat (form eight -- the variable abstracts the content there, and the
; rewrite vanishes into address algebra). the mirror lane rides the
; groupoid itself: p;q = (q^-1;p^-1)^-1 and q^-1;p^-1 is deep-first, so ONE
; rewrite core (swal) serves both directions, inverted in and out -- involution
; comes from the algebra, not from a second implementation. patch.v proves the
; swal involution at element grain (the occurrence/replace algebra); the full
; rose-tree laws are asserted here and queued for their uu migration (the
; uu-term route is OPEN -- test/uupatch.l carries the named-slot slice).

(:
 ; --- the tree kit (rung 5, local) ---
 (htake s n) (? (< n 1) () (link (cap s) (htake (cup s) (- n 1))))
 (hdrop s n) (? (< n 1) s (hdrop (cup s) (- n 1)))
 (hcat a b) (? (atom? a) b (link (cap a) (hcat (cup a) b)))
 (seg s i n) (htake (hdrop s i) n)
 (splice s i c news) (hcat (htake s i) (hcat news (hdrop s (+ i c))))
 (nth s j) (cap (hdrop s j))
 (heq a b) (? (&& (atom? a) (atom? b)) (id? a b)
              (|| (atom? a) (atom? b)) 0
              (&& (heq (cap a) (cap b)) (heq (cup a) (cup b))))
 (tnav t path) (? (atom? path) t (tnav (nth t (cap path)) (cup path)))
 (tlay t path s2)
   (? (atom? path) s2
      (splice t (cap path) 1
              (link (tlay (nth t (cap path)) (cup path) s2) ())))
 (tp path i o n) (link 'tp (link path (link i (link o (link n ())))))
 (pp p) (cap (cup p))            (pi p) (cap (cup (cup p)))
 (po p) (cap (cup (cup (cup p)))) (pn p) (cap (cup (cup (cup (cup p)))))
 (apon t p) (tlay t (pp p) (splice (tnav t (pp p)) (pi p) (tally (po p)) (pn p)))
 (applall t ps) (foldl apon t ps)
 (invert p) (tp (pp p) (pi p) (pn p) (po p))
 (pathok t path) (? (atom? path) 1
                    (: j (cap path)
                       (&& (< j (tally t))
                           (: e (nth t j)
                              (&& (! (atom? e)) (pathok e (cup path)))))))
 (valid t p) (&& (pathok t (pp p))
                 (: s (tnav t (pp p))
                    (&& (<= (+ (pi p) (tally (po p))) (tally s))
                        (heq (seg s (pi p) (tally (po p))) (po p)))))
 (setidx path d v) (? (= d 0) (link v (cup path))
                      (link (cap path) (setidx (cup path) (- d 1) v)))
 (rel P Q d) (? (&& (atom? P) (atom? Q)) '(same)
                (atom? P) (link 'inq (link (cap Q) (link d ())))
                (atom? Q) (link 'inp (link (cap P) (link d ())))
                (id? (cap P) (cap Q)) (rel (cup P) (cup Q) (+ d 1))
                '(apart))

 ; --- the matching kit: a subtree's occurrences, counted and found at every
 ;     depth (occs and ffind descend the same way -- their agreement is what
 ;     makes the ==1 guard name ffind's answer) ---
 (occs f x) (? (atom? f) 0
                (: h (cap f)
                   (+ (? (heq h x) 1 (atom? h) 0 (occs h x))
                      (occs (cup f) x))))
 (ffind f x) (? (atom? f) ()
                (: h (cap f)
                   (? (heq h x) '(0)
                      (: r (? (atom? h) () (ffind h x))
                         (? (! (atom? r)) (link 0 r)
                            (: d (ffind (cup f) x)
                               (? (atom? d) ()
                                  (link (+ (cap d) 1) (cup d)))))))))
 ; the unit walk: the SHALLOWEST prefix of the deep path whose subtree of the
 ; swallowed element survives into the far side -- (k), or () when nothing
 ; survives (erasure: the honest dependency)
 (sunit el R n2 k)
   (? (! (pathok el (htake R k))) ()
      (< 0 (occs n2 (tnav el (htake R k)))) (link k ())
      (<= (tally R) k) ()
      (sunit el R n2 (+ k 1)))

 ; --- swal: the deep-first swallow commute. dp ran first, so sp's olds carry
 ;     its effect (element off, unit at Rk below); undo dp inside the unit
 ;     (bak, dp's inverse below the crossing -- valid keeps the context
 ;     honest), lay the pre state into BOTH of sp's sides, and re-aim dp at
 ;     the survivor's seat in the news. the guards make the match canonical;
 ;     any miss answers () -- declining is always sound. ---
 (swal dp sp j d)
   (: S (pp sp) is (pi sp) O (po sp) N (pn sp)
      R (hdrop (pp dp) (+ d 1)) off (- j is) el (nth O off)
      (? (atom? el) ()
         (: u (sunit el R N 0)
            (? (atom? u) ()
               (: k (cap u) Rk (htake R k) Rrem (hdrop R k)
                  ep (tnav el Rk)
                  bak (tp Rrem (pi dp) (pn dp) (po dp))
                  (? (! (valid ep bak)) ()
                     (: e (apon ep bak)
                        (? (! (&& (= 1 (occs O ep))
                                  (&& (= 0 (occs O e))
                                      (&& (= 1 (occs N ep)) (= 0 (occs N e)))))) ()
                           (: fd (ffind N ep)
                              (link (tp S is (tlay O (link off Rk) e) (tlay N fd e))
                                    (link (tp (hcat S (hcat (setidx fd 0 (+ is (cap fd))) Rrem))
                                              (pi dp) (po dp) (pn dp))
                                          ())))))))))))

 ; --- commute: rung 5's four lanes, the two swallow refusals now doors --
 ;     inp calls swal straight (p IS deep-first), inq flips through the
 ;     groupoid inverse and back ---
 (commute p q)
   (: P (pp p) ip (pi p) o1 (tally (po p)) n1 (tally (pn p))
      Q (pp q) iq (pi q) o2 (tally (po q)) n2 (tally (pn q))
      r (rel P Q 0) tag (cap r)
      (? (id? tag 'same)
           (? (> iq (+ ip n1)) (link (tp Q (- (+ iq o1) n1) (po q) (pn q)) (link p ()))
              (< (+ iq o2) ip) (link q (link (tp P (- (+ ip n2) o2) (po p) (pn p)) ()))
              ())
         (id? tag 'inq)
           (: j (cap (cup r)) d (cap (cup (cup r)))
              (? (< j ip) (link q (link p ()))
                 (>= j (+ ip n1))
                   (link (tp (setidx Q d (- (+ j o1) n1)) iq (po q) (pn q)) (link p ()))
                 (: c (swal (invert q) (invert p) j d)
                    (? (atom? c) ()
                       (link (invert (cap (cup c))) (link (invert (cap c)) ()))))))
         (id? tag 'inp)
           (: j (cap (cup r)) d (cap (cup (cup r)))
              (? (< j iq) (link q (link p ()))
                 (>= j (+ iq o2))
                   (link q (link (tp (setidx P d (- (+ j n2) o2)) ip (po p) (pn p)) ()))
                 (swal p q j d)))
         (link q (link p ()))))

 ; --- witnesses: the same little program; pd edits deep, qm MOVES the edited
 ;     form (its context aimed post-pd, as a second branch would record it) ---
 t0 '((: a 1) (: b 2) (main (f x)))
 pd (tp '(1) 2 '(2) '(9))                                  ; deep: (: b 2) -> (: b 9)
 qm (tp () 1 '((: b 9) (main (f x))) '((main (f x)) (: b 9)))

 ; (L1) the swallow commutes: qm's context rewrites THROUGH (both sides read
 ;      (: b 2) now -- the pre state), the deep edit re-aims to the moved
 ;      seat (path 1 -> 2), and swapping back is home.
 _ (: c (commute pd qm) cq (cap c) cp (cap (cup c))
      (assert
       (heq (pp cp) '(2))                                  ; the survivor's new seat
       (heq (po cq) '((: b 2) (main (f x))))               ; undo laid into olds
       (heq (pn cq) '((main (f x)) (: b 2)))               ; ...and into news
       (: c2 (commute cq cp)
          (&& (heq (cap c2) pd) (heq (cap (cup c2)) qm)))))

 ; (L3a) sound across the rewrite: both orders land the same tree -- while the
 ;       un-rewritten qm has no home on t0 (its olds still carry the edit).
 _ (: c (commute pd qm) cq (cap c) cp (cap (cup c))
      (assert
       (heq (apon (apon t0 pd) qm) (apon (apon t0 cq) cp))
       (heq (apon (apon t0 pd) qm) '((: a 1) (main (f x)) (: b 9)))
       (! (valid t0 qm))))

 ; the WRAP: the survivor sits at DEPTH in the news (the unit walk descends),
 ;      so the re-aimed path GROWS a component (1 -> 1 1) -- and back is home,
 ;      the unwrap direction shedding it again.
 _ (: qw (tp () 1 '((: b 9)) '((guard (: b 9))))
      c (commute pd qw) cq (cap c) cp (cap (cup c))
      (assert
       (heq (pp cp) '(1 1))
       (heq (pn cq) '((guard (: b 2))))
       (heq (apon (apon t0 pd) qw) (apon (apon t0 cq) cp))
       (: c2 (commute cq cp)
          (&& (heq (cap c2) pd) (heq (cap (cup c2)) qw)))))

 ; the MIRROR lane (the groupoid ride): the move lands FIRST, the deep edit
 ;      aims into its news -- commute re-aims the edit to the pre-move seat
 ;      (path 2 -> 1) and advances the move's both sides to the edited content.
 _ (: pm (tp () 1 '((: b 2) (main (f x))) '((main (f x)) (: b 2)))
      qd (tp '(2) 2 '(2) '(9))
      c (commute pm qd) cq (cap c) cp (cap (cup c))
      (assert
       (heq (pp cq) '(1))
       (heq (po cp) '((: b 9) (main (f x))))
       (heq (apon (apon t0 pm) qd) (apon (apon t0 cq) cp))
       (heq (apon (apon t0 pm) qd) '((: a 1) (main (f x)) (: b 9)))
       (: c2 (commute cq cp)
          (&& (heq (cap c2) pm) (heq (cap (cup c2)) qd)))))

 ; the refusals keep the door honest: erasure is a REAL dependency, a
 ;      duplicate has no single home, a news already carrying the PRE state
 ;      cannot tell undo from content, and content the shallow patch wrote
 ;      FRESH was never anywhere else to move from.
 _ (assert
    (! (commute pd (tp () 1 '((: b 9)) ())))                       ; deleted: erased edit
    (! (commute pd (tp () 1 '((: b 9)) '((: b 9) (: b 9)))))       ; duplicated
    (! (commute pd (tp () 1 '((: b 9) (main (f x))) '((: b 2) (: b 9)))))  ; pre state in news
    (! (commute (tp () 1 '((: b 2)) '((let x))) (tp '(1) 1 '(x) '(y)))))   ; fresh content

 ; (L2) the rewritten pair are honest patches: each round-trips in context.
 _ (: c (commute pd qm) cq (cap c) cp (cap (cup c))
      t1 (apon t0 cq)
      (assert
       (valid t0 cq) (valid t1 cp)
       (heq (apon t1 (invert cq)) t0)
       (heq (apon (apon t1 cp) (invert cp)) t1)))

 ; (L3b) a frontier is stable under the swallow swap, a bystander riding along.
 _ (: w (tp '(0) 2 '(1) '(7))
      c (commute pd qm)
      f1 (applall t0 (link w (link pd (link qm ()))))
      f2 (applall t0 (link w c))
      (assert (heq f1 f2) (heq f1 '((: a 7) (main (f x)) (: b 9)))))
 0)

; --- the CLASH-SEGMENTS rung: the conflictor cell lifted to sequences ---
;
; rung 3 recorded rivals at named slots; here they rival over SPANS -- pijul's
; graggle corner, taken through our habit. the load-bearing fact this rung
; must respect: state alone cannot DELIMIT a first rival's footprint (a landed
; hunk left news of unknown length -- the EXTENT problem, the reason pijul
; carries provenance). so the machinery splits honestly in two:
;   merge2   the pushout of two co-valid hunks off the SHARED BASE -- both
;            patches in hand, so every footprint is known. independent hunks
;            ride commute (the darcs ride: invert one, commute the other
;            past it); a refusal -- overlap, touch, or the classic two
;            inserts on one seam -- collapses the UNION HULL of the two base
;            spans to ONE element, the BUBBLE: a clash cell holding the hull
;            segment and each rival as the hull with its hunk laid inside.
;   pull     state-only, for what state CAN answer: the plain lane (rung 2
;            verbatim), the settled lane (the news already in, gated to
;            non-empty news -- an empty segment is vacuously "present"), the
;            JOIN lane (a landed cell absorbs a same-span rival), and the
;            breach backstop (no-op rather than invent a story).
; the bubble FIXES the length: rivals of any sizes occupy one seat, so every
; downstream seat is determinate before anyone resolves -- the graggle's
; bubble, collapsed to one first-class element. rivals are SEGMENTS now,
; ordered by content hash (hval, the canon rung 3 deferred to "a general
; value order"; collision is engineering, as with the frontier identity).
; a resolution is an ordinary hunk whose olds is the one-element cell
; segment -- invertible, unpull returns the clash. the seam refusal that
; rung 2 closed by fiat is ANSWERED here: recorded as a cell with an EMPTY
; base. and the join lane WIDENS: a rival extending past a landed bubble
; re-derives the cell over the union hull, the pads riding in from beside
; the seat -- padding commutes with rendering, so mixed-span rivals land
; the same bubble in any arrival order.

(:
 ; --- the sequence kit (rung 2, local) ---
 (htake s n) (? (< n 1) () (link (cap s) (htake (cup s) (- n 1))))
 (hdrop s n) (? (< n 1) s (hdrop (cup s) (- n 1)))
 (hcat a b) (? (atom? a) b (link (cap a) (hcat (cup a) b)))
 (seg s i n) (htake (hdrop s i) n)
 (splice s i c news) (hcat (htake s i) (hcat news (hdrop s (+ i c))))
 (heq a b) (? (&& (atom? a) (atom? b)) (id? a b)
              (|| (atom? a) (atom? b)) 0
              (&& (heq (cap a) (cap b)) (heq (cup a) (cup b))))
 (hunk i o n) (link 'hunk (link i (link o (link n ()))))
 (hi p) (cap (cup p)) (ho p) (cap (cup (cup p))) (hn p) (cap (cup (cup (cup p))))
 (valid s p) (&& (<= (+ (hi p) (tally (ho p))) (tally s))
                 (heq (seg s (hi p) (tally (ho p))) (ho p)))
 (apon s p) (splice s (hi p) (tally (ho p)) (hn p))
 (invert p) (hunk (hi p) (hn p) (ho p))
 (commute p q)
   (: i (hi p) o1 (tally (ho p)) n1 (tally (hn p))
      j (hi q) o2 (tally (ho q)) n2 (tally (hn q))
      (? (> j (+ i n1)) (link (hunk (- (+ j o1) n1) (ho q) (hn q)) (link p ()))
         (< (+ j o2) i) (link q (link (hunk (- (+ i n2) o2) (ho p) (hn p)) ()))
         ()))

 ; --- the cell: hull base + rivals, canonically ordered by content hash ---
 (hstr h s i n) (? (= i n) h (hstr (% (+ (* h 33) (s i)) 4294967296) s (+ i 1) n))
 (hval v) (: s (show v) (hstr 5381 s 0 (tally s)))
 (clash b rs) (link 'clash (link b (link rs ())))
 (clash? v) (? (atom? v) 0 (id? (cap v) 'clash))
 (cbase c) (cap (cup c))
 (rivals c) (cap (cup (cup c)))
 (sins x rs) (? (atom? rs) (link x ())                      ; canonical insert --
                (: h (cap rs)                               ; dedup by CONTENT,
                   (? (heq x h) rs                          ; order by its hash
                      (< (hval x) (hval h)) (link x rs)
                      (link h (sins x (cup rs))))))
 (has rs x) (? (atom? rs) 0 (|| (heq (cap rs) x) (has (cup rs) x)))

 ; --- merge2: the pushout off the shared base. equal edits settle; commute
 ;     re-aims an independent q past p; a refusal makes the BUBBLE ---
 (merge2 s0 p q)
   (? (heq p q) (apon s0 p)
      (: c (commute (invert p) q)
         (? (! (atom? c)) (apon (apon s0 p) (cap c))
            (: lo (? (< (hi q) (hi p)) (hi q) (hi p))       ; the union hull
               e1 (+ (hi p) (tally (ho p))) e2 (+ (hi q) (tally (ho q)))
               hu (? (< e1 e2) e2 e1)
               base (seg s0 lo (- hu lo))
               rp (splice base (- (hi p) lo) (tally (ho p)) (hn p))
               rq (splice base (- (hi q) lo) (tally (ho q)) (hn q))
               (splice s0 lo (- hu lo)
                       (link (clash base (sins rp (sins rq ()))) ()))))))

 ; --- pull: plain / settled / the WIDENING join / breach -- what state CAN
 ;     answer. the join lane is general: any rival whose base span overlaps
 ;     (or touches -- the seam law is strict everywhere) a landed cell's hull
 ;     joins it. a same-span rival rides as before, a CONTAINED one lays into
 ;     the hull, an EXTENDING one WIDENS the bubble: the hull grows to the
 ;     union, the pads (base content beside the cell -- seats 1:1 by the
 ;     co-valid discipline) re-render every rival over the wider hull.
 ;     padding COMMUTES with rendering (patch.v splice_pad), so a rival
 ;     padded up equals the rival rendered over the wide hull directly --
 ;     arrival order stays dead at mixed spans. the honest guard reads the
 ;     rebuilt hull; any mismatch no-ops. ---
 (findc s q k)                                              ; first cell whose
   (? (<= (tally s) k) ()                                   ; hull meets q's span
      (: e (cap (hdrop s k))
         (? (&& (clash? e)
                (&& (<= (hi q) (+ k (tally (cbase e))))
                    (<= k (+ (hi q) (tally (ho q)))))) (link k ())
            (findc s q (+ k 1)))))
 (pull s q)
   (: i (hi q) o (ho q) n (hn q)
      (? (valid s q) (apon s q)                             ; plain: rung 2
         (&& (< 0 (tally n)) (heq (seg s i (tally n)) n)) s ; settled (non-empty)
         (: f (findc s q 0)
            (? (atom? f) s                                  ; no cell: breach, no-op
               (: k (cap f) e (cap (hdrop s k)) b (cbase e)
                  lo (? (< i k) i k)                        ; the union hull
                  eo (+ k (tally b)) eq (+ i (tally o))
                  hu (? (< eo eq) eq eo)
                  pl (seg s lo (- k lo))                    ; the pads, off the state
                  pr (seg s (+ k 1) (- hu eo))
                  hull (hcat pl (hcat b pr))
                  (? (! (heq (seg hull (- i lo) (tally o)) o)) s   ; dishonest: no-op
                     (: rs (foldl (\ a x (sins (hcat pl (hcat x pr)) a)) () (rivals e))
                        rv (splice hull (- i lo) (tally o) n)
                        (splice s (- k (tally pl)) (+ (tally pl) (+ 1 (tally pr)))
                                (link (clash hull (sins rv rs)) ())))))))))

 ; --- witnesses: one disputed span, rivals of three different SIZES (the
 ;     segment teeth), and an independent bystander ---
 s0 '(a b c d e)
 p (hunk 2 '(c) '(p1 p2)) q (hunk 2 '(c) '(q1)) r (hunk 2 '(c) '(r1 r2 r3))
 w (hunk 4 '(e) '(e9))

 ; (M1) merge is TOTAL and SYMMETRIC: rivals or bystanders, either order --
 ;      the independent lane riding commute, the rival lane riding the set.
 _ (assert
    (heq (merge2 s0 p q) (merge2 s0 q p))
    (heq (merge2 s0 p w) (merge2 s0 w p))
    (heq (merge2 s0 p w) '(a b p1 p2 d e9)))

 ; (M2) the same edit settles: merged with itself is a plain apply, pulled
 ;      again it is already in, and a rival re-arriving is absorbed.
 _ (assert
    (heq (merge2 s0 p p) (apon s0 p))
    (heq (pull (apon s0 p) p) (apon s0 p))
    (: m (merge2 s0 p q) (heq (pull m p) m)))

 ; (M3) the conflict is HONEST, and the bubble FIXES the length: rivals of
 ;      sizes 2 and 1 over a span of 1, and the sequence stays 5 long --
 ;      every downstream seat determinate before anyone resolves.
 _ (: m (merge2 s0 p q) e (cap (hdrop m 2))
      (assert
       (= 5 (tally m)) (clash? e)
       (heq (cbase e) '(c)) (= 2 (tally (rivals e)))
       (has (rivals e) '(p1 p2)) (has (rivals e) '(q1))
       (heq (hdrop m 3) '(d e))))

 ; (M4) three rivals, any order: one cell, one set -- pull's join lane
 ;      absorbing whichever arrives last.
 _ (: m1 (pull (merge2 s0 p q) r)
      m2 (pull (merge2 s0 q r) p)
      m3 (pull (merge2 s0 r p) q)
      (assert (heq m1 m2) (heq m2 m3)
              (= 3 (tally (rivals (cap (hdrop m1 2)))))))

 ; (M5) RESOLUTION is just a hunk: olds = the one-element cell segment --
 ;      the plain lane applies it, invert unpulls it and the clash returns.
 _ (: m (merge2 s0 p q) cell (cap (hdrop m 2))
      res (hunk 2 (link cell ()) '(z9))
      m2 (pull m res)
      (assert
       (heq m2 '(a b z9 d e)) (! (clash? (cap (hdrop m2 2))))
       (heq (pull m2 (invert res)) m)))

 ; the SEAM answered: two inserts on one seam -- rung 2 refused by fiat;
 ;      here the ambiguity is RECORDED, a cell with an EMPTY base.
 _ (: sp (hunk 1 () '(u)) sq (hunk 1 () '(v))
      m (merge2 s0 sp sq) e (cap (hdrop m 1))
      (assert
       (heq m (merge2 s0 sq sp))
       (= 6 (tally m)) (clash? e) (heq (cbase e) ())
       (has (rivals e) '(u)) (has (rivals e) '(v))
       (heq (hdrop m 2) '(b c d e))))

 ; OVERLAPPING spans: the union hull is the base, each rival the hull with
 ;      its hunk laid inside -- and downstream rides at a determinate seat.
 _ (: op (hunk 1 '(b c) '(x)) oq (hunk 2 '(c d) '(y z))
      m (merge2 s0 op oq) e (cap (hdrop m 1))
      (assert
       (heq m (merge2 s0 oq op))
       (= 3 (tally m)) (clash? e)
       (heq (cbase e) '(b c d))
       (has (rivals e) '(x d)) (has (rivals e) '(b y z))
       (heq (hdrop m 2) '(e))))

 ; the boundary, honest: a first rival arriving by pull NO-OPS (state cannot
 ;      delimit its footprint -- the extent problem; the first clash needs
 ;      merge2, both patches in hand), a deletion arriving twice rests in the
 ;      breach lane, and a mismatch that is neither settled nor a join rides
 ;      through untouched.
 _ (: dl (hunk 3 '(d) ()) s1 (apon s0 dl)
      (assert
       (heq (pull (apon s0 p) q) (apon s0 p))
       (heq (pull s1 dl) s1)
       (: m (merge2 s0 p q) (heq (pull m (hunk 0 '(zz) '(ww))) m))))

 ; the WIDENING: three rivals over three DIFFERENT overlapping spans (the
 ;      middle one bridging, the outer two merely touching) -- whichever pair
 ;      bubbles first, the third widens the cell to the union hull and every
 ;      rival re-renders over it: one bubble, one set, any arrival order.
 _ (: p2 (hunk 1 '(b c) '(x)) q2 (hunk 2 '(c d) '(y z)) r2 (hunk 3 '(d e) '(w))
      m1 (pull (merge2 s0 p2 q2) r2)
      m2 (pull (merge2 s0 q2 r2) p2)
      m3 (pull (merge2 s0 r2 p2) q2)
      e (cap (hdrop m1 1))
      (assert
       (heq m1 m2) (heq m2 m3)
       (= 2 (tally m1)) (clash? e)                          ; (a BUBBLE) -- length fixed
       (heq (cbase e) '(b c d e))
       (= 3 (tally (rivals e)))
       (has (rivals e) '(x d e)) (has (rivals e) '(b y z e)) (has (rivals e) '(b c w))))

 ; a CONTAINED rival lays into the landed hull -- same lane, pads empty.
 _ (: op (hunk 1 '(b c) '(x)) oq (hunk 2 '(c d) '(y z))
      m (pull (merge2 s0 op oq) (hunk 2 '(c) '(k1)))
      e (cap (hdrop m 1))
      (assert
       (= 3 (tally (rivals e)))
       (has (rivals e) '(b k1 d))))

 ; resolution off the WIDENED bubble stays ordinary -- and unpull returns it.
 _ (: p2 (hunk 1 '(b c) '(x)) q2 (hunk 2 '(c d) '(y z)) r2 (hunk 3 '(d e) '(w))
      m (pull (merge2 s0 p2 q2) r2)
      cell (cap (hdrop m 1))
      res (hunk 1 (link cell ()) '(z9))
      (assert
       (heq (pull m res) '(a z9))
       (heq (pull (pull m res) (invert res)) m)))
 0)

; --- the METAVARIABLE rung: patches grow variables, the unifier takes the seat ---
;
; the door the content-rewrite rung framed, opened: a patch's segments may
; carry METAVARIABLES ((mv v) cells), so a patch is now a rewrite RULE. the
; unifier enters at APPLICATION: valid MATCHES olds against the tree (one-
; sided unification -- a substitution threads through, and a NON-LINEAR
; pattern binds consistently or not at all, the kanren move), apon lays the
; news INSTANTIATED. a ground patch is the variable-free special case (match
; degenerates to heq, instantiate to the identity), so this model subsumes
; the tree lift's whole.
;
; and the swallow commute TRANSFORMS: a schematic shallow patch is BLIND to
; the content its variable binds, so a deep ground edit inside the binding
; commutes with the rule UNCHANGED -- form six rewrote the context through
; the content; the variable ABSTRACTED the content away, and what remains is
; pure address algebra. `bind` descends the rule's olds pattern along the
; deep path: entering the variable's seat names the binder (the edit rides
; inside the binding); landing beside it with a one-for-one swap replaces
; the binding whole; hitting the RIGID part refuses -- the rule's context
; genuinely mentions that content. the deep patch re-aims from the
; variable's olds seat to its news seat (mvat), and form six's occurrence
; guards become STRUCTURAL: the variable exactly once on each side, read
; off the rule itself -- a duplicating rule would need the edit twice, an
; erasing rule would lose it, both honest dependencies. the mirror rides
; the groupoid inverse as ever, one core. patch.v proves the two halves at
; their grains: matching is SOUND (a pattern instantiated with its own
; bindings reproduces the ground term -- mtch_sound) and the seat re-aim is
; involutive (reaim_involutive). the schematic-vs-schematic door opens in
; form NINE below -- the critical-pairs rung.

(:
 ; --- the tree kit (rung 5, local) ---
 (htake s n) (? (< n 1) () (link (cap s) (htake (cup s) (- n 1))))
 (hdrop s n) (? (< n 1) s (hdrop (cup s) (- n 1)))
 (hcat a b) (? (atom? a) b (link (cap a) (hcat (cup a) b)))
 (seg s i n) (htake (hdrop s i) n)
 (splice s i c news) (hcat (htake s i) (hcat news (hdrop s (+ i c))))
 (nth s j) (cap (hdrop s j))
 (heq a b) (? (&& (atom? a) (atom? b)) (id? a b)
              (|| (atom? a) (atom? b)) 0
              (&& (heq (cap a) (cap b)) (heq (cup a) (cup b))))
 (tnav t path) (? (atom? path) t (tnav (nth t (cap path)) (cup path)))
 (tlay t path s2)
   (? (atom? path) s2
      (splice t (cap path) 1
              (link (tlay (nth t (cap path)) (cup path) s2) ())))
 (tp path i o n) (link 'tp (link path (link i (link o (link n ())))))
 (pp p) (cap (cup p))            (pi p) (cap (cup (cup p)))
 (po p) (cap (cup (cup (cup p)))) (pn p) (cap (cup (cup (cup (cup p)))))
 (invert p) (tp (pp p) (pi p) (pn p) (po p))
 (pathok t path) (? (atom? path) 1
                    (: j (cap path)
                       (&& (< j (tally t))
                           (: e (nth t j)
                              (&& (! (atom? e)) (pathok e (cup path)))))))
 (setidx path d v) (? (= d 0) (link v (cup path))
                      (link (cap path) (setidx (cup path) (- d 1) v)))
 (rel P Q d) (? (&& (atom? P) (atom? Q)) '(same)
                (atom? P) (link 'inq (link (cap Q) (link d ())))
                (atom? Q) (link 'inp (link (cap P) (link d ())))
                (id? (cap P) (cap Q)) (rel (cup P) (cup Q) (+ d 1))
                '(apart))

 ; --- the pattern kit: metavariables, match, instantiate. a (mv v) cell is
 ;     a variable (data by discipline: ground content never spells one) ---
 (mv? x) (? (atom? x) 0 (id? (cap x) 'mv))
 (mvname x) (cap (cup x))
 (asq k s) (? (atom? s) () (: c (cap s) (? (id? k (cap c)) (link (cup c) ()) (asq k (cup s)))))
 (mtch p g s)                                              ; one-sided unification:
   (? (id? s 'no) 'no                                      ; pattern vs ground, the
      (mv? p) (: r (asq (mvname p) s)                      ; substitution threading --
                 (? (atom? r) (link (link (mvname p) g) s) ; a rebound variable must
                    (? (heq (cap r) g) s 'no)))            ; AGREE (the kanren move)
      (&& (atom? p) (atom? g)) (? (id? p g) s 'no)
      (|| (atom? p) (atom? g)) 'no
      (mtch (cup p) (cup g) (mtch (cap p) (cap g) s)))
 (inst p s) (? (mv? p) (: r (asq (mvname p) s) (? (atom? r) p (cap r)))
               (atom? p) p
               (link (inst (cap p) s) (inst (cup p) s)))

 ; --- apply: match the olds, lay the news instantiated ---
 (valid t q) (&& (pathok t (pp q))
                 (: s (tnav t (pp q))
                    (&& (<= (+ (pi q) (tally (po q))) (tally s))
                        (! (id? (mtch (po q) (seg s (pi q) (tally (po q))) ()) 'no)))))
 (apon t q) (: sq (tnav t (pp q))
               sg (mtch (po q) (seg sq (pi q) (tally (po q))) ())
               (tlay t (pp q) (splice sq (pi q) (tally (po q)) (inst (pn q) sg))))
 (applall t ps) (foldl apon t ps)

 ; --- the variable's seats: count and address in a pattern segment ---
 (mvn f v) (? (atom? f) 0
               (: h (cap f)
                  (+ (? (atom? h) 0 (&& (mv? h) (id? (mvname h) v)) 1 (mvn h v))
                     (mvn (cup f) v))))
 (mvat f v) (? (atom? f) ()
                (: h (cap f)
                   (? (&& (! (atom? h)) (&& (mv? h) (id? (mvname h) v))) '(0)
                      (: r (? (atom? h) () (mvat h v))
                         (? (! (atom? r)) (link 0 r)
                            (: d (mvat (cup f) v)
                               (? (atom? d) ()
                                  (link (+ (cap d) 1) (cup d)))))))))
 (alast a) (? (atom? (cup a)) (cap a) (alast (cup a)))
 (abut a) (? (atom? (cup a)) () (link (cap a) (abut (cup a))))

 ; --- bind: descend the rule's olds along the deep path. answers ('in mv
 ;     rest) -- the edit rides INSIDE the binding, rest below the seat;
 ;     ('at pat) -- the path landed on a pattern sequence (the whole-binding
 ;     swap checks there); 'no -- fell off the rigid structure ---
 (bind e R)
   (? (mv? e) (link 'in (link e (link R ())))
      (atom? R) (link 'at (link e ()))
      (: j (cap R)
         (? (|| (atom? e) (<= (tally e) j)) 'no
            (bind (nth e j) (cup R)))))

 ; --- mvswal: the schematic swallow. the rule rides UNCHANGED (its variable
 ;     abstracts the content); the deep patch re-aims from the variable's
 ;     olds seat to its news seat. linearity is the guard: the variable
 ;     exactly once on each side, or the dependency is real. ---
 (mvswal dp sp j d)
   (: S (pp sp) is (pi sp) O (po sp) N (pn sp)
      R (hdrop (pp dp) (+ d 1)) off (- j is)
      b (bind (nth O off) R)
      (? (atom? b) ()
         (id? (cap b) 'in)
           (: v (mvname (cap (cup b))) rest (cap (cup (cup b)))
              (? (! (&& (= 1 (mvn O v)) (= 1 (mvn N v)))) ()
                 (: an (mvat N v)
                    (link sp (link (tp (hcat S (link (+ is (cap an)) (hcat (cup an) rest)))
                                       (pi dp) (po dp) (pn dp)) ())))))
         (: pat (cap (cup b)) im (pi dp)                   ; the whole-binding swap:
            (? (! (&& (= 1 (tally (po dp))) (= 1 (tally (pn dp))))) ()   ; one for one,
               (<= (tally pat) im) ()                      ; exactly at the seat
               (: m (nth pat im)
                  (? (|| (atom? m) (! (mv? m))) ()         ; rigid part: a real dependency
                     (: v (mvname m)
                        (? (! (&& (= 1 (mvn O v)) (= 1 (mvn N v)))) ()
                           (: an (mvat N v)
                              (? (atom? (cup an))
                                 (link sp (link (tp S (+ is (cap an)) (po dp) (pn dp)) ()))
                                 (link sp (link (tp (hcat S (link (+ is (cap an)) (abut (cup an))))
                                                    (alast (cup an)) (po dp) (pn dp)) ()))))))))))))

 ; --- commute: the tree lift's lanes, the swallow arms schematic; a ground
 ;     swallow refuses HERE (form six owns the ground rewrite; a real vcs
 ;     composes both) ---
 (commute p q)
   (: P (pp p) ip (pi p) o1 (tally (po p)) n1 (tally (pn p))
      Q (pp q) iq (pi q) o2 (tally (po q)) n2 (tally (pn q))
      r (rel P Q 0) tag (cap r)
      (? (id? tag 'same)
           (? (> iq (+ ip n1)) (link (tp Q (- (+ iq o1) n1) (po q) (pn q)) (link p ()))
              (< (+ iq o2) ip) (link q (link (tp P (- (+ ip n2) o2) (po p) (pn p)) ()))
              ())
         (id? tag 'inq)
           (: j (cap (cup r)) d (cap (cup (cup r)))
              (? (< j ip) (link q (link p ()))
                 (>= j (+ ip n1))
                   (link (tp (setidx Q d (- (+ j o1) n1)) iq (po q) (pn q)) (link p ()))
                 (: c (mvswal (invert q) (invert p) j d)
                    (? (atom? c) ()
                       (link (invert (cap (cup c))) (link (invert (cap c)) ()))))))
         (id? tag 'inp)
           (: j (cap (cup r)) d (cap (cup (cup r)))
              (? (< j iq) (link q (link p ()))
                 (>= j (+ iq o2))
                   (link q (link (tp (setidx P d (- (+ j n2) o2)) ip (po p) (pn p)) ()))
                 (mvswal p q j d)))
         (link q (link p ()))))

 ; --- witnesses: the rule renames main -> run and MOVES the body (the
 ;     variable's seat slides from child 1 to child 2); the deep edit rides ---
 t0 '((: a 1) (: b 2) (main (f x)))
 q8 (tp () 2 '((main (mv v))) '((run () (mv v))))
 p8 (tp '(2 1) 0 '(f) '(g))

 ; (L1 + L3a) the edit rides INSIDE the binding: the rule commutes UNCHANGED,
 ;      the deep path follows the seat (2 1 -> 2 2), both orders land the
 ;      same tree, and swapping back is home.
 _ (: c (commute p8 q8) cq (cap c) cp (cap (cup c))
      (assert
       (heq cq q8)                                         ; the rule is BLIND: q' = q
       (heq (pp cp) '(2 2))                                ; the seat re-aim
       (heq (apon (apon t0 p8) q8) (apon (apon t0 cq) cp))
       (heq (apon (apon t0 p8) q8) '((: a 1) (: b 2) (run () (g x))))
       (: c2 (commute cq cp)
          (&& (heq (cap c2) p8) (heq (cap (cup c2)) q8)))))

 ; the WHOLE-BINDING swap: a one-for-one replacement at the variable's seat
 ;      re-seats the same way (path 2 index 1 -> path 2 index 2).
 _ (: p9 (tp '(2) 1 '((f x)) '((h y)))
      c (commute p9 q8) cq (cap c) cp (cap (cup c))
      (assert
       (heq cq q8)
       (heq (pp cp) '(2)) (= 2 (pi cp))
       (heq (apon (apon t0 p9) q8) (apon (apon t0 cq) cp))
       (heq (apon (apon t0 p9) q8) '((: a 1) (: b 2) (run () (h y))))
       (: c2 (commute cq cp)
          (&& (heq (cap c2) p9) (heq (cap (cup c2)) q8)))))

 ; the refusals read off the RULE: a rigid-part edit (the rule's context
 ;      mentions it), a duplicating rule (the edit would be needed twice),
 ;      an erasing rule (the edit would vanish), a span covering rigid
 ;      structure beside the binding.
 _ (assert
    (! (commute (tp '(2) 0 '(main) '(go)) q8))
    (! (commute p8 (tp () 2 '((main (mv v))) '((run (mv v) (mv v))))))
    (! (commute p8 (tp () 2 '((main (mv v))) '((halt)))))
    (! (commute (tp '(2) 0 '(main (f x)) '(go)) q8)))

 ; the unifier face: a NON-LINEAR pattern binds consistently or not at all,
 ;      and instantiation reads the bindings back out.
 _ (: t1 '((pair 5 5) (pair 5 6))
      qc (tp () 0 '((pair (mv w) (mv w))) '((mv w)))
      (assert
       (valid t1 qc)
       (! (valid t1 (tp () 1 '((pair (mv w) (mv w))) '((mv w)))))
       (heq (apon t1 qc) '(5 (pair 5 6)))))

 ; (L2) a variable-preserving rule round-trips through its inverse: the news
 ;      pattern matches what it laid, and the olds pattern restores.
 _ (: t2 (apon t0 q8)
      (assert
       (valid t0 q8) (valid t2 (invert q8))
       (heq (apon t2 (invert q8)) t0)))

 ; (L3b) a frontier is stable under the schematic swallow swap.
 _ (: w (tp '(0) 2 '(1) '(7))
      c (commute p8 q8)
      f1 (applall t0 (link w (link p8 (link q8 ()))))
      f2 (applall t0 (link w c))
      (assert (heq f1 f2) (heq f1 '((: a 7) (: b 2) (run () (g x))))))
 0)

; --- the CRITICAL-PAIRS rung: two rules meet on one tree ---
;
; the ninth form, the door form eight left shut: schematic-vs-schematic. a
; rule here is (olds news) applied at a SEAT -- match olds against the
; subtree, lay news instantiated over it -- so this is the rule grain with
; the sequence machinery stripped away (patch.v's ninth slice mirrors it
; term for term). the seat relation decides everything: DIVERGING seats
; are order-free, and with NO re-aim -- a tree address doesn't shift under
; a disjoint rewrite, so the sequence forms' index slide vanishes at this
; grain (rw_comm_div) -- while ANY overlap is critical-pair territory: two
; rules at one seat can both land, in either order, and DISAGREE, so no
; total commute exists at rule grain and a refusal is EARNED, never
; cautious (cp_earned). the uu-term leg re-proves both at code grain
; (test/uupatch.l), its deep demos on the lof lane. deliberately out,
; named in patch.v's header: the variable-overlap lane (a redex under the
; other rule's METAVARIABLE slides to its rhs seat for linear rules --
; mvswal's rule-rule face) and joinability of critical pairs (which
; overlaps are harmless is Knuth-Bendix completion territory, not a
; commute question).

(:
 ; --- the tree + pattern kit (form 8's, trimmed local) ---
 (htake s n) (? (< n 1) () (link (cap s) (htake (cup s) (- n 1))))
 (hdrop s n) (? (< n 1) s (hdrop (cup s) (- n 1)))
 (hcat a b) (? (atom? a) b (link (cap a) (hcat (cup a) b)))
 (splice s i c news) (hcat (htake s i) (hcat news (hdrop s (+ i c))))
 (nth s j) (cap (hdrop s j))
 (heq a b) (? (&& (atom? a) (atom? b)) (id? a b)
              (|| (atom? a) (atom? b)) 0
              (&& (heq (cap a) (cap b)) (heq (cup a) (cup b))))
 (tnav t path) (? (atom? path) t (tnav (nth t (cap path)) (cup path)))
 (tlay t path s2)
   (? (atom? path) s2
      (splice t (cap path) 1
              (link (tlay (nth t (cap path)) (cup path) s2) ())))
 (mv? x) (? (atom? x) 0 (id? (cap x) 'mv))
 (mvname x) (cap (cup x))
 (asq k s) (? (atom? s) () (: c (cap s) (? (id? k (cap c)) (link (cup c) ()) (asq k (cup s)))))
 (mtch p g s)
   (? (id? s 'no) 'no
      (mv? p) (: r (asq (mvname p) s)
                 (? (atom? r) (link (link (mvname p) g) s)
                    (? (heq (cap r) g) s 'no)))
      (&& (atom? p) (atom? g)) (? (id? p g) s 'no)
      (|| (atom? p) (atom? g)) 'no
      (mtch (cup p) (cup g) (mtch (cap p) (cap g) s)))
 (inst p s) (? (mv? p) (: r (asq (mvname p) s) (? (atom? r) p (cap r)))
               (atom? p) p
               (link (inst (cap p) s) (inst (cup p) s)))

 ; --- a rule at a seat: match olds against the subtree, lay news
 ;     instantiated; rw answers (tree) or (), rw2 sequences two ---
 (rw R path t)
   (: sub (tnav t path)
      sg (mtch (cap R) sub ())
      (? (id? sg 'no) ()
         (link (tlay t path (inst (cap (cup R)) sg)) ())))
 (rw2 Ra pa Rb pb t) (: r (rw Ra pa t) (? (atom? r) () (rw Rb pb (cap r))))

 t0 '((f 1) ((g 2) (h 3)))
 Ra '((f (mv x)) (f ((mv x) (mv x))))     ; duplicate under f
 Rb '((h (mv y)) (k (mv y)))

 ; (L3a at rule grain) DIVERGING seats are order-free, the addresses
 ;     unshifted -- both orders, one tree; deeper shared prefixes too
 _ (: c1 (rw2 Ra '(0) Rb '(1 1) t0) c2 (rw2 Rb '(1 1) Ra '(0) t0)
      (assert
       (! (atom? c1)) (heq (cap c1) (cap c2))
       (heq (cap c1) '((f (1 1)) ((g 2) (k 3))))))
 _ (: p9 '((g (mv u)) (g ((mv u)))) q9 '((h (mv v)) (h))
      c1 (rw2 p9 '(1 0) q9 '(1 1) t0) c2 (rw2 q9 '(1 1) p9 '(1 0) t0)
      (assert (! (atom? c1)) (heq (cap c1) (cap c2))))

 ; the CRITICAL PAIR, earned: one seat, two rules, both orders green --
 ;     and they disagree, so no total commute exists at rule grain
 _ (: R1 '((mv z) (a)) R2 '((mv z) (b))
      c1 (rw2 R1 () R2 () t0) c2 (rw2 R2 () R1 () t0)
      (assert
       (! (atom? c1)) (! (atom? c2))
       (! (heq (cap c1) (cap c2)))
       (heq (cap c1) '(b)) (heq (cap c2) '(a))))

 ; the destroyed redex, the overlap's other face: nested seats, and the
 ;     ORDER decides which rule survives at all
 _ (: Rc '((g (mv u)) (k 5)) Rd '((g (mv w)) (g 9))
      (assert
       (! (atom? (rw2 Rd '(1 0) Rc '(1 0) t0)))   ; d then c: c still sees a g
       (atom? (rw2 Rc '(1 0) Rd '(1 0) t0))))     ; c then d: the g is GONE
 0)
