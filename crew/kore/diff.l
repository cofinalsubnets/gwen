; crew/kore/diff.l -- myers diff: the shortest edit script between two lists (Myers 1986,
; the greedy O(ND) forward pass with a trace, then the backtrack). the seed of kore --
; the coming `diff` tool reads it, the vcs records it. pure love (no nif); element equality
; is the generic `=`, so it diffs lines, symbols, charms, whole trees alike.
;
; the script is a list of (op x) 2-lists, op one of 'keep 'del 'add, in order: the
; keep+del column spells the OLD list, keep+add the NEW, and the del+add count is
; MINIMAL -- the three laws crew/kore/law.l holds it to (an O(nm) LCS oracle for the
; third). a body-less top-level `:` (the wm's app shape): `myers` the entry,
; dold/dnew/dcost the script readers, d* the machinery. indexes and rounds ride TABLETS
; (charm keys, negatives fine) for O(1) reads; every loop is a tail call.
(module 'kore
(:
   ; a list -> a tablet keyed 0..n-1: O(1) element reads for the snake loop
   (dindex l) (: t (tablet 0)
                 (go i m) (? (two? m) (: _ (pin t i <m) (go (i + 1) >m)) t)
                 (go 0 l))
   ; the down/right choice at diagonal k of round d, read off the PREVIOUS round's row
   ; vp -- shared by the forward pass and the backtrack (they must agree or the path tears)
   (ddown? vp d k) ((k = (0 - d)) ||
                    (!(k = d) && (peep vp (k - 1) 0 < peep vp (k + 1) 0)))
   ; slide the snake: advance x,y while the elements agree; answers (x y)
   (dsnake ax bx n m x y) (? (&& (x < n) (y < m) (ax x = bx y))
                             (dsnake ax bx n m (x + 1) (y + 1))
                             `(x y))
   ; round d: fill row vd over k = -d..d step 2 from the previous row vp, pin vd into the
   ; trace, answer (L d) the moment (n m) is reached -- else recurse into round d+1.
   ; d is bounded by n+m, so the recursion always lands.
   (dfwd ax bx n m vp d tr)
    (: vd (tablet 0)
       (gok k)
        (? (d < k) ()
           (: x0 (? (ddown? vp d k) (peep vp (k + 1) 0) (1 + peep vp (k - 1) 0))
              xy (dsnake ax bx n m x0 (x0 - k))
              x <xy y <>xy
              _ (pin vd k x)
              (? ((n <= x) && (m <= y)) `(d) (gok (k + 2)))))
       r (gok (0 - d))
       _ (pin tr d vd)
       (? (two? r) r (dfwd ax bx n m vd (d + 1) tr)))
   ; the leading snake back to the origin: everything left is kept
   (dkeeps ax x acc) (? (0 < x) (dkeeps ax (x - 1) (`('keep (ax (x - 1))) >< acc)) acc)
   ; rewind one snake from (x y) toward (px py), prepending keeps; answers (x y acc)
   (dsnakeb ax x y px py acc)
    (? ((px < x) && (py < y))
       (dsnakeb ax (x - 1) (y - 1) px py (`('keep (ax (x - 1))) >< acc))
       `(x y acc))
   ; walk the trace back from (n m) at round dd, emitting the script front-first: rewind
   ; the round's snake, then the one edit that opened it (down took b's line, right a's)
   (dback ax bx tr dd x y acc)
    (? (dd < 1) (dkeeps ax x acc)
       (: vp (peep tr (dd - 1) 0)
          k (x - y)
          dn (ddown? vp dd k)
          pk (? dn (k + 1) (k - 1))
          px (peep vp pk 0)
          py (px - pk)
          acc2 <>>(dsnakeb ax x y px py acc)
          (? dn (dback ax bx tr (dd - 1) px py (`('add (bx py)) >< acc2))
             (dback ax bx tr (dd - 1) px py (`('del (ax px)) >< acc2)))))
   ; the entry: the shortest edit script from a to b
   (myers a b)
    (: ax (dindex a) bx (dindex b) n (tally a) m (tally b)
       v0 #(1 0)
       tr (tablet 0)
       r (dfwd ax bx n m v0 0 tr)
       (dback ax bx tr <r n m ()))
   ; the script read three ways: the old list, the new list, the edit distance
   (dold s) (map caup (filter (\ o !(<o = 'add)) s))
   (dnew s) (map caup (filter (\ o !(<o = 'del)) s))
   (dcost s) (tally (filter (\ o !(<o = 'keep)) s)))

; --- patience: the anchoring layer over myers (Bram Cohen). same script shape, so the
; projection laws carry over verbatim -- what changes is the SHAPE of the hunks: anchor
; on the lines a human would (unique in both sides), recurse between anchors, and let
; myers fill the gaps, trading minimality (dcost patience >= dcost myers) for hunks
; that align on landmarks instead of boilerplate. the pass: peel the common prefix and
; suffix (keeps for free, and they shrink the D the O(ND) core sees), census both sides
; (tablets key elements STRUCTURALLY, so string lines and whole trees census alike),
; pair the unique-in-both positions, keep the longest increasing subsequence of them
; (the patience-sorting step, an O(k^2) DP -- k is tiny), and split at the survivors.
(:
   ; occurrence census: element -> how many times it appears
   (dcensus l) (: t (tablet 0)
                  (go m) (? (two? m) (: _ (pin t <m (1 + peep t <m 0)) (go >m)) t)
                  (go l))
   ; the unique-in-both candidates as (i j e) pairs, i ascending
   (danchors a b)
    (: ca (dcensus a) cb (dcensus b)
       bj (tablet 0)
       (gob m j) (? (two? m)
                    (: e <m
                       _ (? ((ca e = 1) && (cb e = 1)) (pin bj e j) ())
                       (gob >m (j + 1)))
                    ())
       _ (gob b 0)
       (goa m i acc) (? (two? m)
                        (: e <m j (peep bj e -1)            ; -1 the absence face: j = 0 is a real seat
                           (? (j < 0) (goa >m (i + 1) acc)
                              (goa >m (i + 1) (`(i j e) >< acc))))
                        (rev acc))
       (goa a 0 ()))
   ; the longest increasing subsequence of the j's (the i's already ascend): O(k^2) DP
   (dlis ps)
    (: n (tally ps) pt (dindex ps)
       (jof t) <>(pt t)
       ln (tablet 0) pv (tablet 0)
       (gos t s bl bp)                                            ; best chain ending at t
        (? (t <= s) (: _ (pin ln t bl) _ (pin pv t bp) ())
           (: c (? (jof s < jof t) (1 + peep ln s 0) 0)
              (? (bl < c) (gos t (s + 1) c s) (gos t (s + 1) bl bp))))
       (got t) (? (n <= t) () (: _ (gos t 0 1 -1) (got (t + 1))))
       _ (got 0)
       (gom t best bt) (? (n <= t) bt                             ; the longest chain's tail
                          (: c (peep ln t 0)
                             (? (best < c) (gom (t + 1) c t) (gom (t + 1) best bt))))
       (gob t acc) (? (t < 0) acc (gob (peep pv t -1) (pt t >< acc)))
       (gob (gom 0 0 -1) ()))
   ; peel the common prefix; answers (a' b' keeps), the keeps REVERSED
   (dpre a b ks)
    (? (&& (two? a) (two? b) (<a = <b))
       (dpre >a >b (`('keep <a) >< ks))
       `(a b ks))
   ; split at the anchors: each between-segment recurses (uniqueness reappears locally
   ; once the outer context is cut away), each anchor lands as a keep
   (dsplit a b love bi ps)
    (? !(two? ps) (patience a b)
       (: h <ps i <h j <>h e <>>h
          na (i - love) nb (j - bi)
          (patience (take na a) (take nb b) + link `('keep e)
                    (dsplit (drop (na + 1) a) (drop (nb + 1) b) (i + 1) (j + 1) >ps))))
   ; the trimmed middle: anchor + split, or myers when nothing anchors
   (dmid a b) (: ps (danchors a b)
                 (? (two? ps) (dsplit a b 0 0 (dlis ps)) (myers a b)))
   ; the entry: patience diff, same script contract as myers
   (patience a b)
    (: p (dpre a b ())
       a1 <p b1 <>p pre (rev <>>p)
       s (dpre (rev a1) (rev b1) ())                              ; the suffix is the reversed prefix
       a2 (rev <s) b2 (rev <>s) suf <>>s      ; ... and its keeps come back forward
       (pre + dmid a2 b2 + suf)))

; --- the unified surface (diff -u): the script rendered as unified hunks, over jugs so
; big files stay linear. every entry tags as (op x i j) -- i/j the 1-based old/new seats
; it sits at -- then hunks gather with c lines of context, splitting where a keep run
; tops 2c (the coalescing law patch(1) expects); a side with no lines names the seat
; BEFORE it (the -0,0 convention). `udiff` is the tool's engine: two texts and their
; names -> the whole unified text, nothing when they agree.
(:
   (utag s)                                                       ; seat every entry: (op x) -> (op x i j)
    (: (go m i j acc)
        (? (two? m)
           (: op <<m
              (? (op = 'keep) (go >m (i + 1) (j + 1) ((<m + `(i j)) >< acc))
                 (op = 'del) (go >m (i + 1) j ((<m + `(i j)) >< acc))
                 (go >m i (j + 1) ((<m + `(i j)) >< acc))))
           (rev acc))
       (go s 1 1 ()))
   (eop e) <e (eln e) <>e                               ; the tagged entry's four seats
   (eoi e) <>>e (enj e) <>>>e
   (dspan p m acc) (? (two? m && p <m)                       ; the maximal p-prefix; answers (span rest)
                      (dspan p >m (<m >< acc))
                      `((rev acc) m))
   (lastn c l) (? (c < tally l) (drop (tally l - c) l) l)
   (uhunks s c)
    (: es (utag s)
       o (jug 0)
       keep? (\ e (<e = 'keep))
       change? (\ e !(<e = 'keep))
       (rng l n) (? (n = 1) (show l) (show l + "," + show n))       ; ",1" stays silent, the GNU face
       (emit h)                                                   ; one hunk: the @@ header, then the lines
        (: no (tally (filter (\ e !(eop e = 'add)) h))
           nn (tally (filter (\ e !(eop e = 'del)) h))
           e0 <h
           ol (? (no = 0) (eoi e0 - 1) (eoi e0))
           nl (? (nn = 0) (enj e0 - 1) (enj e0))
           _ (say o "@@ -") _ (say o (rng ol no))
           _ (say o " +") _ (say o (rng nl nn)) _ (say o " @@") _ (put o 10)
           (go m) (? (two? m)
                     (: e <m
                        _ (put o (? (eop e = 'keep) 32 (eop e = 'del) 45 43))
                        _ (say o (eln e)) _ (put o 10)
                        (go >m))
                     ())
           (go h))
       (grab m cur)                                               ; m opens on a change; cur the gathered context
        (: cs (dspan change? m ())
           cur2 (cur + <cs)
           ks (dspan keep? <>cs ())
           krun <ks rest <>ks
           (? !(two? rest) (emit (cur2 + take c krun))          ; the tail: close with c keeps
              ((* 2 c) < tally krun) (: _ (emit (cur2 + take c krun))       ; a wide gap: close, reopen
                                          (grab rest (lastn c krun)))
              (grab rest (cur2 + krun))))                       ; a narrow gap rides inside
       ks0 (dspan keep? es ())
       _ (? (two? <>ks0) (grab <>ks0 (lastn c <ks0)) ())
       (slurp o))
   (udiff an bn at bt)
    (: h (uhunks (patience (lines at) (lines bt)) 3)
       (? (nil? h) ()
          (: o (jug 0)
             _ (say o "--- ") _ (say o an) _ (put o 10)
             _ (say o "+++ ") _ (say o bn) _ (put o 10)
             _ (say o h)
             (slurp o)))))
) ; module 'kore
