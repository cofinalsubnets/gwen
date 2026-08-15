; sketch: THE PATCH GROUPOID -- the layer UNDER a distribution, and the missing
; piece that makes a distribution and a namespace the SAME thing rather than two
; that merely rhyme. a darcs, read through our lattice-and-algebra habit and sat
; on the self-hosting fixpoint.
;
; --- one algebra, read a fourth way --- we already lean on "one lattice, read
; four ways" (dispatch / order / algebra / abstract domain). the distribution
; story wants the SAME move one level up: every layer we stack is a SELECTION --
; a subset over a dependency relation, with a merge:
;   * a NAMESPACE   = a selected set of NAMES        (a book -- holo, glaze)
;   * a COMPONENT   = a selected set of DEFINITIONS  (a book's worth of source)
;   * a DISTRIBUTION= a selected set of COMPONENTS   (a book of books, layered)
;   * a REPO STATE  = a selected set of PATCHES       <- THIS is the missing floor
; these are not four analogous tools. they are one algebra. the patch layer is
; the DYNAMICS -- how the sets are formed, inverted, and combined -- for the
; floor under all the others. that is why version control slots in exactly here
; and nowhere else: it turns a "version" from an opaque atom into something
; COMPOSABLE, and composable is what collapses distribution and namespace into
; one mechanism.
;
; --- a version is a FRONTIER, not a snapshot --- the earlier distribution sketch
; keyed the store on named images (git/rustup snapshot thinking). that is the
; seam this closes. a version is a downward-closed SET OF PATCHES (a frontier);
; the baked image (`.image`) is a CACHE of replaying that frontier; the patch set
; is the truth, the manifest, AND the reproducibility handle. "install version X"
; = pull these patches, replay, and fixpoint-check the bake. rustup cannot verify
; a download; we can verify a REBUILD -- cc(cc(ai)) is already byte-identical, so
; a frontier's identity (a hash of its patch set) is checkable to the bit.
;
; --- why patches, not snapshots --- a component selection is LITERALLY a
; cherry-pick, and cherry-pick / merge / unpull / component-composition are all
; ONE operation: COMMUTE. "this toolchain = base u {these components} u {that
; fix}" is an arbitrary consistent subset of a patch bag. git's snapshot DAG
; cannot say "any consistent subset is a valid state"; a darcs patch bag is
; DEFINED that way. so the VCS model and the distribution model want the identical
; primitive -- build commute once (soundly), get the rest as bookkeeping.
;
; --- the groupoid --- patches form an inverse structure: partial composition
; (append when contexts line up), every patch INVERTIBLE (unpull is apply-inverse),
; and COMMUTE the partial swap that reorders independent patches. the three laws
; to prove, the same way we proved the tiling theorem and the fixpoint axiom-free:
;   (L1) commute is its own inverse:      commute(p,q)=(q',p') => commute(q',p')=(p,q)
;   (L2) inverse round-trips:             apon (apon t p) (invert p) == t   (on p's slot)
;   (L3) merge = commute into a common context (the ONLY definition of merge)
; classic darcs got these subtly wrong (the exponential / unsound conflict corner).
; building this in the one language with the Rocq/Lean harness, DON'T clone darcs
; 1.x -- clone the idea on the SOUND substrate (the inverse-semigroup formalization,
; or pijul's pushout-in-a-category), and let proof/ carry L1..L3. the hand .v
; (test/proof/rocq/patch.v) is SCAFFOLDING: a hand proof drifts (the wm2uu lesson).
; the destination is GENERATED like the rest of proof/, and the first TWO slices
; have ARRIVED: test/uupatch.l encodes the named-slot and positional models'
; laws as uu proof TERMS (pointwise, evidence-carrying coprods for every
; case-split -- no funext, no classical; the hunk re-aim's index algebra and
; splice reassociation sealed lemma by lemma), and mk/tools/uu2coq.l +
; mk/tools/uu2lean.l emit them into test/proof/rocq/uugen.v AND test/proof/lean/uugen.lean,
; audited axiom-free in both kernels; those slices' hand copies are deleted
; from patch.v. the remaining slices migrate the same way as their models
; stabilize. source of truth is the executable ai spec (test/patch.l),
; everything else drift-gated off it.
;
; --- the ai-native sharpening --- source is .l, so a patch can be a transform over
; the FORM TREE, not textual line ranges. then "do p and q commute?" is "do these
; two rewrites touch disjoint locations / are they confluent?" -- a question wev /
; boxfix and the kanren unifier already answer. darcs fought line-hunk commutation
; for years; we would commute AST edits with machinery we own. the toy below uses
; NAMED SLOTS (a file of named lines) so commute is clean; the comment at `commute`
; marks where positional / structural patches make q' a genuine re-aim -- and THAT
; is where the rewrite layer earns its keep.
;
; --- staging (keep it honest) --- build the snapshot store + shim + default now
; (independent, useful today), but make a version's identity a PATCH-SET HASH from
; day one -- even if the first VCS is a stub that only does linear apply. then the
; patch layer slots UNDER the store later instead of replacing it. the two forks
; are DECIDED (gwen, 2026-07-12): FORM-TREE + SOUND. (a) patch GRAIN: a patch is a
; rewrite over the form tree, commutation answered by machinery we own (wev /
; boxfix / the unifier); textual hunks are at most a later compatibility skin,
; never the model. (b) theory BASE: the sound substrate -- the inverse-semigroup
; reading (partial composition, invert, commute the partial swap) is the working
; formalization since it fits uu's MLTT (decidable eqb, list induction, no
; funext); pijul's pushout stays the cross-check reading, classic darcs 1.x a
; cautionary tale only.
;
; --- the runnable toy below --- two layers. first the NAMED-SLOT model: a tree, a
; chg patch carrying its own context (old->new), and the four primitives: apon /
; invert / commute / frontier. it DEMONSTRATES: independent patches commute and
; are order-free; a same-slot patch DEPENDS (commute answers ()); inverse
; round-trips; and two distribution COMPONENTS install to the same frontier
; regardless of pull order. then the POSITIONAL layer (demo E, the rung opened
; 2026-07-12): hunks over a sequence of forms, where commute genuinely RE-AIMS --
; the swapped hunk's index slides by the other's length delta, and TOUCHING hunks
; refuse (strict: a seam has no canonical order once inserts land on it; classic
; darcs's ambiguity corner, closed by fiat). then the CONFLICTOR layer (demo F,
; same day): commute refuses a dependency, but a real MERGE may not -- so the
; STATE enlarges (pijul's move, not darcs 1.x's exotic patch algebra): a
; conflicted slot holds a CLASH cell, the shared base + a canonically-ordered SET
; of rivals, making merge total, symmetric, and associative by construction; and
; since a cell is a first-class VALUE, resolution is an ordinary chg (old = the
; cell) -- invertible, so unpulling a resolution returns the clash. and the
; IDENTITY layer (demo G, same day): a version IS its patch set, hashed
; canonically -- per-patch content hashes folded through the SAME cins set, so
; the identity quotients by exactly what the semantics proved dead (order, M4;
; multiplicity, M2) and nothing more. and the TREE lift (demo H, 2026-07-13):
; a hunk gets a POSTCODE -- the path to the sequence it splices -- and the
; shift algebra climbs the levels: a deep patch never changes an ANCESTOR's
; length, so only same-sequence patches shift INDICES, while a shallow splice
; moves whole SUBTREES and re-aims a deeper patch's path COMPONENT at the
; crossing (four commute lanes: same / q-deeper / p-deeper / apart --
; test/patch.l's fifth form). and the CONTENT-REWRITE commute (demo I,
; 2026-07-13): the tree lift's swallow refusal opens -- a shallow patch that
; MOVES the subtree a deep patch edited commutes after all, by rewriting its
; context THROUGH the content: undo the deep edit inside both of its sides
; (olds AND news carry the moved subtree), re-aim the deep patch at the
; survivor's new seat, with occurrence guards keeping the match canonical
; (the spec's sixth form; a wrap/unwrap commutes too -- the unit walk finds
; the survivor at any depth). matching ground survivors is one-sided
; unification -- this is where the rewrite machinery starts earning its keep.
; and the CLASH-SEGMENTS layer (demo J, same day): the conflictor cell lifted
; to sequences -- pijul's graggle corner. two hunks whose spans overlap,
; touch, or share a seam collapse the UNION HULL of their base spans to ONE
; element, the BUBBLE: a clash cell holding the hull segment plus each rival
; as the hull with its hunk laid inside (rivals canonically ordered by
; content hash -- the general value order rung 3 deferred). the bubble FIXES
; the length: rivals of any sizes occupy one seat, so downstream seats are
; determinate before anyone resolves; the seam refusal rung 2 closed by fiat
; is recorded as a cell with an EMPTY base; resolution stays an ordinary
; invertible hunk off the cell. the honest split the extent problem forces:
; the FIRST clash needs both patches in hand (merge2, the pushout -- state
; alone cannot delimit a landed rival's footprint), while a landed cell
; absorbs rivals state-only (pull's join lane; the spec's seventh form) --
; and the join WIDENS: a rival extending past a landed bubble re-derives the
; cell over the union hull, the pads riding in from beside the seat; padding
; commutes with rendering (patch.v splice_pad), so mixed-span rivals land
; one bubble in any arrival order. and the METAVARIABLE layer (demo K, same
; day): patches grow variables ((mv v) cells) and become rewrite RULES --
; the unifier takes the application seat (valid MATCHES the olds pattern,
; one-sided unification with a substitution threading; a non-linear pattern
; binds consistently or not at all; apon lays the news INSTANTIATED), and
; the swallow commute transforms: a schematic rule is BLIND to the content
; its variable binds, so a deep edit inside the binding commutes with the
; rule UNCHANGED -- form six's rewrite-through vanishes into address
; algebra, the deep patch re-aiming from the variable's olds seat to its
; news seat, with form six's occurrence guards now STRUCTURAL (the variable
; exactly once on each side, read off the rule; the spec's eighth form).
; this is where wev/boxfix-grade rewrites become PATCHES: a rename-and-
; restructure rule commutes past every edit that stays inside its bindings.
; the laws for ALL layers live in test/patch.l, the source of truth. still
; deliberately OMITTED: schematic-vs-schematic commute (two overlapping
; REWRITE RULES are critical-pair confluence territory -- Knuth-Bendix's
; question, a different theorem set).

(:
 ; --- the patch: a groupoid element. (chg slot old new) carries its context
 ;     (old) so it knows where it BELONGS -- a patch is not a bare diff. ---
 (chg k a b) (link 'chg (link k (link a (link b 0))))
 (pk p) (cap (cup p))                                       ; the slot it touches
 (pa p) (cap (cup (cup p)))                                 ; its context (old value)
 (pb p) (cap (cup (cup (cup p))))                           ; the new value

 ; --- the tree: an assoc (k . v), newest shadows, a missing slot reads 0 (a
 ;     nothing -- the nil-is-0 floor). ---
 (tget t k) (? (atom? t) 0 (: c (cap t) (? (id? k (cap c)) (cup c) (tget (cup t) k))))
 (tset t k v) (link (link k v) t)

 ; --- apply: a patch belongs on t only if its context matches (valid); apon
 ;     lays the new value. applall folds a frontier onto the empty tree. ---
 (valid t p) (id? (tget t (pk p)) (pa p))
 (apon t p) (tset t (pk p) (pb p))
 (applall t ps) (foldl apon t ps)

 ; --- invert: every patch has an inverse (the groupoid law) -- unpull is just
 ;     apply-the-inverse. ---
 (invert p) (chg (pk p) (pb p) (pa p))

 ; --- commute: the ONE hard primitive. p;q -> q;p when INDEPENDENT (different
 ;     slots); () when q DEPENDS on p (same slot -- q's context is p's effect).
 ;     merge, cherry-pick, and unpull are all built on this. here the swapped
 ;     patches are unchanged because named slots do not SHIFT; with positional or
 ;     form-tree patches q' is a genuine re-aim (the rewrite/unifier lane). ---
 (commute p q) (? (id? (pk p) (pk q)) 0 (link q (link p 0)))
 (depends p q) (atom? (commute p q))                        ; q needs p iff they don't commute

 ; --- report helpers ---
 (line label v) (: _ (puts label) _ (. (show v)) (putc 10))
 (cat2 a b) (? (atom? a) b (link (cap a) (cat2 (cup a) b)))

 ; ---- demos ----
 _ (: px (chg 'x 0 1) py (chg 'y 0 2)
      t1 (applall 0 (link px (link py 0)))
      t2 (applall 0 (link py (link px 0)))                  ; opposite pull order
      _ (puts ";; A. independent patches commute -- both pull orders agree\n")
      _ (puts "     commute(px,py) = ")
      _ (. (show (commute px py)))
      _ (putc 10)
      _ (line "     order px;py -> x,y = " (link (tget t1 'x) (link (tget t1 'y) 0)))
      (line "     order py;px -> x,y = " (link (tget t2 'x) (link (tget t2 'y) 0))))

 _ (: p1 (chg 'x 0 1) p2 (chg 'x 1 2)                       ; both touch x
      _ (puts ";; B. same-slot patches DEPEND -- commute answers () -> a dependency\n")
      _ (line "     commute(p1,p2) = " (commute p1 p2))
      (line "     p2 depends on p1 = " (depends p1 p2)))

 _ (: px (chg 'x 0 7)
      t (apon (apon 0 px) (invert px))                      ; apply then unpull
      _ (puts ";; C. inverse round-trips (the groupoid law)\n")
      (line "     apon;invert -> x = " (tget t 'x)))

 _ (: holo (link (chg 'holo 0 1) 0)                         ; a component = a patch bag
      glaze (link (chg 'glaze 0 1) 0)
      fa (applall 0 (cat2 holo glaze))                      ; install holo then glaze
      fb (applall 0 (cat2 glaze holo))                      ; ...or glaze then holo
      _ (puts ";; D. two distribution COMPONENTS install to the same frontier\n")
      _ (line "     pull holo;glaze -> " (link (tget fa 'holo) (link (tget fa 'glaze) 0)))
      (line "     pull glaze;holo -> " (link (tget fb 'holo) (link (tget fb 'glaze) 0))))

 ; ---- demo E: the positional layer -- a self-contained hunk model, where the
 ;      swapped q is a genuine RE-AIM (its index slides by p's length delta) ----
 _ (: (htake s n) (? (< n 1) () (link (cap s) (htake (cup s) (- n 1))))
      (hdrop s n) (? (< n 1) s (hdrop (cup s) (- n 1)))
      (splice s i c news) (cat2 (htake s i) (cat2 news (hdrop s (+ i c))))
      (hunk i o n) (link 'hunk (link i (link o (link n ())))) ; at i, segment o -> n
      (hi p) (cap (cup p)) (ho p) (cap (cup (cup p))) (hn p) (cap (cup (cup (cup p))))
      (hpon s p) (splice s (hi p) (tally (ho p)) (hn p))
      (hcom p q)                                            ; p;q -> q';p', or () on a seam
        (: i (hi p) o1 (tally (ho p)) n1 (tally (hn p))
           j (hi q) o2 (tally (ho q)) n2 (tally (hn q))
           (? (> j (+ i n1)) (link (hunk (- (+ j o1) n1) (ho q) (hn q)) (link p ()))
              (< (+ j o2) i) (link q (link (hunk (- (+ i n2) o2) (ho p) (hn p)) ()))
              ()))
      s0 '(a b c d e)
      p (hunk 1 '(b) '(x y))                                ; grows the file by one
      q (hunk 4 '(d) '(z))                                  ; aimed in post-p coordinates
      c (hcom p q)
      _ (puts ";; E. positional hunks -- commute RE-AIMS: q slides 4 -> 3 past p's growth\n")
      _ (line "     commute(p,q) = " c)
      _ (line "     order p;q   -> " (hpon (hpon s0 p) q))
      _ (line "     order q';p' -> " (hpon (hpon s0 (cap c)) (cap (cup c))))
      (line "     the seam refuses: " (hcom p (hunk 3 '(c) '(w)))))

 ; ---- demo F: the conflictor layer -- merge is TOTAL: a dependency that commute
 ;      refuses becomes a recorded CLASH (base + rivals, a canonical set), and
 ;      resolving it is just another patch ----
 _ (: (clash a rs) (link 'clash (link a (link rs ())))
      (cins x rs) (? (atom? rs) (link x ())
                     (: h (cap rs) (? (= x h) rs (< x h) (link x rs)
                                      (link h (cins x (cup rs))))))
      (clash? v) (? (atom? v) 0 (id? (cap v) 'clash))
      (pull t p)                                            ; total merge-application
        (: k (pk p) a (pa p) b (pb p) v (tget t k)
           (? (id? v a) (tset t k b)                        ; context matches: plain
              (id? v b) t                                   ; same edit again: settled
              (clash? v) (tset t k (clash a (cins b (cap (cup (cup v)))))) ; join
              (tset t k (clash a (cins b (cins v ()))))))   ; a rival landed first
      mp (chg 'x 1 2) mq (chg 'x 1 3)                       ; two branches, one base
      m (pull (pull (tset 0 'x 1) mp) mq)
      _ (puts ";; F. conflictors -- both rivals RECORDED, either pull order, one cell\n")
      _ (line "     pull p;q -> x = " (tget m 'x))
      _ (line "     pull q;p -> x = " (tget (pull (pull (tset 0 'x 1) mq) mp) 'x))
      (line "     resolve (a plain chg off the cell) -> x = "
            (tget (pull m (chg 'x (tget m 'x) 5)) 'x)))

 ; ---- demo G: the frontier IDENTITY -- a version IS its patch set: hash each
 ;      patch's printed form, fold through the canonical set, name the result.
 ;      order and repetition vanish (exactly what M4/M2 proved dead); content
 ;      moves the name ----
 _ (: (hstr h s i n) (? (= i n) h (hstr (% (+ (* h 33) (s i)) 4294967296) s (+ i 1) n))
      (hval v) (: s (show v) (hstr 5381 s 0 (tally s)))
      (cins x rs) (? (atom? rs) (link x ())
                     (: h (cap rs) (? (= x h) rs (< x h) (link x rs)
                                      (link h (cins x (cup rs))))))
      (fid ps) (foldl (\ acc p (cins (hval p) acc)) () ps)
      (ident ps) (hval (fid ps))
      p (chg 'x 1 2) q (chg 'x 1 3)
      _ (puts ";; G. the frontier identity -- order and repetition dead, content alive\n")
      _ (line "     ident (p q)   = " (ident (link p (link q 0))))
      _ (line "     ident (q p)   = " (ident (link q (link p 0))))
      _ (line "     ident (p p q) = " (ident (link p (link p (link q 0)))))
      (line "     ident (q)     = " (ident (link q 0))))

 ; ---- demo H: the TREE lift -- a hunk gets a postcode, and a root insert
 ;      re-aims a deep edit's PATH on the way past (3 -> 2). the four-lane
 ;      commute lives in test/patch.l; here the two orders shown agreeing ----
 _ (: (htk s n) (? (< n 1) () (link (cap s) (htk (cup s) (- n 1))))
      (hdr s n) (? (< n 1) s (hdr (cup s) (- n 1)))
      (hct a b) (? (atom? a) b (link (cap a) (hct (cup a) b)))
      (spl s i c news) (hct (htk s i) (hct news (hdr s (+ i c))))
      (nth s j) (cap (hdr s j))
      (tnav t path) (? (atom? path) t (tnav (nth t (cap path)) (cup path)))
      (tlay t path s2)
        (? (atom? path) s2
           (spl t (cap path) 1 (link (tlay (nth t (cap path)) (cup path) s2) ())))
      (hap t path i o n) (tlay t path (spl (tnav t path) i (tally o) n))
      t0 '((: a 1) (: b 2) (main (f x)))
      _ (puts ";; H. the tree lift -- a root insert re-aims a deep edit's path 3 -> 2\n")
      _ (line "     order insert;edit-at-(3) -> "
              (hap (hap t0 () 0 () '((use m))) '(3) 1 '((f x)) '((g y))))
      (line "     order edit-at-(2);insert -> "
            (hap (hap t0 '(2) 1 '((f x)) '((g y))) () 0 () '((use m)))))

 ; ---- demo I: the CONTENT-REWRITE commute -- the swallow opens: a move that
 ;      swallowed a deep edit commutes by rewriting its context THROUGH the
 ;      content (undo the edit in olds AND news) and re-aiming the edit at the
 ;      moved seat (path 1 -> 2). the rewrite core (swal: the unit walk + the
 ;      occurrence guards) lives in test/patch.l's sixth form; here the two
 ;      orders agreeing, the commuted pair laid by hand ----
 _ (: (htk s n) (? (< n 1) () (link (cap s) (htk (cup s) (- n 1))))
      (hdr s n) (? (< n 1) s (hdr (cup s) (- n 1)))
      (hct a b) (? (atom? a) b (link (cap a) (hct (cup a) b)))
      (spl s i c news) (hct (htk s i) (hct news (hdr s (+ i c))))
      (nth s j) (cap (hdr s j))
      (tnav t path) (? (atom? path) t (tnav (nth t (cap path)) (cup path)))
      (tlay t path s2)
        (? (atom? path) s2
           (spl t (cap path) 1 (link (tlay (nth t (cap path)) (cup path) s2) ())))
      (hap t path i o n) (tlay t path (spl (tnav t path) i (tally o) n))
      t0 '((: a 1) (: b 2) (main (f x)))
      _ (puts ";; I. the content-rewrite commute -- the swallowed edit rides the move\n")
      _ (line "     order edit-at-(1) ; move          -> "
              (hap (hap t0 '(1) 2 '(2) '(9))
                   () 1 '((: b 9) (main (f x))) '((main (f x)) (: b 9))))
      (line "     order move-rewritten ; edit-at-(2) -> "
            (hap (hap t0 () 1 '((: b 2) (main (f x))) '((main (f x)) (: b 2)))
                 '(2) 2 '(2) '(9))))

 ; ---- demo J: CLASH SEGMENTS -- two rivals over one span become the BUBBLE:
 ;      one cell element holding the base and both rivals (canonically ordered
 ;      by content hash), so the length is FIXED before anyone resolves, and
 ;      resolving is an ordinary splice off the cell. the merge machinery
 ;      (merge2 the pushout, pull's join lane) lives in test/patch.l's
 ;      seventh form; here the bubble laid by hand ----
 _ (: (htk s n) (? (< n 1) () (link (cap s) (htk (cup s) (- n 1))))
      (hdr s n) (? (< n 1) s (hdr (cup s) (- n 1)))
      (hct a b) (? (atom? a) b (link (cap a) (hct (cup a) b)))
      (spl s i c news) (hct (htk s i) (hct news (hdr s (+ i c))))
      (heq a b) (? (&& (atom? a) (atom? b)) (id? a b)
                   (|| (atom? a) (atom? b)) 0
                   (&& (heq (cap a) (cap b)) (heq (cup a) (cup b))))
      (hstr h s i n) (? (= i n) h (hstr (% (+ (* h 33) (s i)) 4294967296) s (+ i 1) n))
      (hval v) (: s (show v) (hstr 5381 s 0 (tally s)))
      (sins x rs) (? (atom? rs) (link x ())
                     (: h (cap rs)
                        (? (heq x h) rs
                           (< (hval x) (hval h)) (link x rs)
                           (link h (sins x (cup rs))))))
      s0 '(a b c d e)
      cell (link 'clash (link '(c) (link (sins '(p1 p2) (sins '(q1) ())) ())))
      m (spl s0 2 1 (link cell ()))
      _ (puts ";; J. clash segments -- rivals of sizes 2 and 1, ONE seat: the bubble\n")
      _ (line "     merged (either order) -> " m)
      _ (line "     tally stays " (tally m))
      (line "     resolve (a splice off the cell) -> " (spl m 2 1 '(z9))))

 ; ---- demo K: METAVARIABLES -- a patch is a rewrite RULE: match the olds
 ;      pattern (the unifier's seat), lay the news instantiated. the rule is
 ;      blind to what ?v binds, so deep edits inside the binding commute with
 ;      it unchanged (the commute lives in test/patch.l's eighth form) ----
 _ (: (htk s n) (? (< n 1) () (link (cap s) (htk (cup s) (- n 1))))
      (hdr s n) (? (< n 1) s (hdr (cup s) (- n 1)))
      (hct a b) (? (atom? a) b (link (cap a) (hct (cup a) b)))
      (spl s i c news) (hct (htk s i) (hct news (hdr s (+ i c))))
      (heq a b) (? (&& (atom? a) (atom? b)) (id? a b)
                   (|| (atom? a) (atom? b)) 0
                   (&& (heq (cap a) (cap b)) (heq (cup a) (cup b))))
      (mv? x) (? (atom? x) 0 (id? (cap x) 'mv))
      (asq k s) (? (atom? s) () (: c (cap s) (? (id? k (cap c)) (link (cup c) ()) (asq k (cup s)))))
      (mtch p g s)
        (? (id? s 'no) 'no
           (mv? p) (: r (asq (cap (cup p)) s)
                      (? (atom? r) (link (link (cap (cup p)) g) s)
                         (? (heq (cap r) g) s 'no)))
           (&& (atom? p) (atom? g)) (? (id? p g) s 'no)
           (|| (atom? p) (atom? g)) 'no
           (mtch (cup p) (cup g) (mtch (cap p) (cap g) s)))
      (inst p s) (? (mv? p) (: r (asq (cap (cup p)) s) (? (atom? r) p (cap r)))
                    (atom? p) p
                    (link (inst (cap p) s) (inst (cup p) s)))
      t0 '((: a 1) (: b 2) (main (f x)))
      sg (mtch '((main (mv v))) (spl t0 0 2 ()) ())
      _ (puts ";; K. metavariables -- the rule (main ?v) -> (run () ?v), the unifier's seat\n")
      _ (line "     the match binds  -> " sg)
      (line "     rewritten        -> " (spl t0 2 1 (inst '((run () (mv v))) sg))))
 0)
