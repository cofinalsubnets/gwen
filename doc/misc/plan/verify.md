# verify — one ladder, uu at the top

the verification angles grew independently and there are too many. this plan
names them all, keeps one, and says what happens to each of the others.

## the stance

**the native uu prover (love/uu.l) is target number one.** a property worth
holding is worth stating as a uu term; rocq and lean are EXPORT targets
(tools/uu2coq.l, uu2lean.l -- both wired: test_uugen, test_uulean), not
homes. the fuzz lanes (test/law.l, test/fuzz.l, the seeded walks) stay -- a
fuzz binds an implementation to a model cheaply -- but they are the refutation
half of the same ladder, not separate programs.

## the angles, and their fates

| angle | home | fate |
|---|---|---|
| uu kernel + UniMath corpus | love/uu.l, test/uu*.l | THE TARGET |
| rocq export | uu2coq.l, spec2coq.l, mx2coq.l | keep: export leg |
| lean export | uu2lean.l | keep: export leg |
| verified lux (uuwm) | wm2uu.l, test/uuwm*.l | keep; FRESHENED 2026-08-16 |
| CLAUDE.md laws fuzz | test/law.l | keep; the uu leg LANDED (below) |
| CLAUDE.md laws proof | test/uuval.l + uuvaldiff.l | the measure tower, proved |
| CLAUDE.md laws, compiled | law2uu.l -> test/uuvallaw.l | ONE spelling, both lanes |
| love's `=` on values | veq, in test/uuval.l | structural, and the cap/cup/link laws |
| the +/* band lattice | mx2uu.l, test/uumx*.l | LANDED in uu beside mx.v |
| property fuzz | test/fuzz.l | keep |
| holo encoder fuzz | test/holo/fuzz/{fuzz,sysdiff}.l | PORTED 2026-08-16; py gone |
| vmret.l vs vmret.py differential | tools/py/ | RETIRED 2026-08-16 |
| uu-vs-UniMath parity audit | tools/uuparity.l | PORTED 2026-08-16 |

## no python in tree except benchmarks

the standing rule (gwen, 2026-08-16). done 2026-08-16:

- **test/holo/fuzz/fuzz.l** -- the encoder differential fuzz, holo in-process
  (the py shelled out to love per batch), 68 generator classes across the
  three arches, the same tolerance rules (abstract register identity,
  immediates mod 2^64). gated on a planted fault: a flipped nibble goes red
  on every lane (49/32/54 of ~50 samples caught). the x64 lane now ALSO runs
  the llvm-mc second opinion the old gate skipped (--no-llvm).
- **test/holo/fuzz/sysdiff.l** -- the system-lane byte-exact differential;
  output count-identical to the python (x64 743/0/0, arm64 167/0/19).
  regmap.py (a one-time register-map probe) deleted with them.
- **tools/py/** -- gone. vmret.l stands alone (its gate reads the tool's
  own ret-free verdict); uu_parity.py ported to tools/uuparity.l.

still standing, not verification:

- **tools/ccdb.py** -- compile_commands generator for clangd. dev-only;
  ports to love when touched next.
- **port/rp2040/tools/py/{elf2uf2,pad_checksum}.py** -- flasher utilities on
  a port lane. port to love with the next rp2040 ride.

## the uuwm freshen (landed 2026-08-16)

crew/lux/core.l is written in the new style (glued accessors, infix, !=,
bracket literals) and tools/wm2uu.l reads the post-opfix tree it makes:
cap/cup chains where caup was, ></+ beside link/cat, != as = with the arms
traded. the regenerated test/uuwm.l is BYTE-IDENTICAL to the pre-freshen
artifact, so the uuwmlaw theorems and the ten idpath bridges hold unchanged.
the whole crew/lux/ also dropped the backtick list sugar for [..] (a comment-
and string-aware love scanner did the sweep, proving (forms old) = (forms new)
per file before writing).

the sweep then reached the whole tree, and the sigil itself is retired: the
list wears [..] and (list ..) and nothing else. the backtick is an ordinary
name character in both readers now (p1's class table, p0's ioread1sym), so
lint's constructor set, vi's syntax table and libra's doc all lost a row.

## the rung that landed: the CLAUDE.md laws got a proof leg

test/law.l quantifies the laws as lambdas over a jot-generated corpus
(refutation only, and it says so). the proof leg now stands beside it:

- **test/uuval.l** -- the VALUE MODEL. a value is a binary tree of measures,
  depth-indexed the way nlist is (vtre by nat_rect; ii1 lifts, ii2 links), and
  the measure tower is a STACK OF RETRACTIONS: bool -> nat -> zed -> rea -> msr
  -> val, each rung a section whose round trip is definitional. so net-idem,
  re-idem, ceil-idem, sat-idem, sat-net, sat-ceil, ceil-net and bit-idem are
  idpaths, and bit-sat / nil?-bit / bit-nil are three-line rects. sat-green and
  bit-bool are not proved at all -- they are the TYPES of vsat and vbit.
- the carrier is the HALF-INTEGERS (an integer plus a bit): the smallest thing
  closed under addition on which ceil is not the identity, which is all the
  tower's shape asks. no ieee, and no gem value, enters -- as planned.
- the ARITHMETIC (zadd/radd/madd) carries no theorem, deliberately: the laws are
  about idempotence and factorisation, so a wrong zadd cannot fake one. planting
  a fault in zadd reddens the demos and the differential and leaves every proof
  term green -- which is why the differential exists.
- **test/uuvaldiff.l** -- the DIFFERENTIAL. love's own saturate/bit/nil?/ceil run
  beside the model's vsat/vbit/vnil/vceil over an encoding of law.l's corpus
  (33 values across every band: charm, gem, ratio, twin, charlist, symbol,
  array, tablet, jot, nested lists). a pair encodes as the model's link, an atom
  as a leaf carrying its measure -- so the fold, re, ceil, the clamp and the bit
  are all under test. the leaf measure is read off love's own `net`: that one
  primitive is the standing gap, the same one uuwm has with the C runtime.
- **test/uuval.l's planted faults** -- rejects rows, each with its positive
  twin: ceil is not the identity on the integer part, the clamp bites at 0, net
  sums over a link rather than reading the head, saturate is not idempotent one
  step off. the kernel refuses all four.
- the exporters carry it: tools/uu2coq.l and uu2lean.l list test/uuval.l and
  test/uuvallaw.l, so every uv-* and law-* entry re-checks in Rocq (axiom-free,
  universe-checked -- the filter uu's type-in-type kernel lacks) and in Lean 4,
  no sorryAx.

## the lawgen (landed)

the last of the three: **tools/law2uu.l** reads test/law.l's rows AS DATA and
compiles the ones the tower can state into test/uuvallaw.l. a law is spelled
once now -- edit the row in law.l and the obligation moves with it, `make
test_uuvallaw` regenerates and diffs, and the proof is found again. the
hand-written twins that used to sit in test/uuval.l are gone; that file keeps
the model, the retraction ladder, net's homomorphism and the planted faults,
and nothing else. (the old test/uuvallaw.l, the differential, is
test/uuvaldiff.l now, so the three sort model -> differential -> laws.)

- the lift is KIND-DIRECTED BY THE TOWER ITSELF. every expression has a level in
  bool -- nat -- zed -- rea -- msr -- val; a word fixes the level it wants and
  the level it answers; an argument at the wrong level is coerced along the
  sections going up and the retractions going down. so `(net (net x))`, whose
  inner net answers a measure where the outer wants a value, lifts to
  `(vnet (vnum (vnet v)))` with no rule of its own. crew/lux/sigs.l is wm2uu's
  oracle; here the tower is its own.
- the PROOF IS SEARCHED, not transcribed: the tool loads the kernel and the
  model and runs defq. by conversion first -- `(lam v (idpath LHS))`, which
  seven of the eleven take -- then by cases on the saturated measure, a nat_rect
  on `(vsat v)` whose motive is THE SAME TRANSLATION run at v := (nval n) and
  whose arms are it at (nval 0) and (nval (succ k)). `(vsat (nval n))` is n, so
  the motive at `(vsat v)` converts back to the obligation.
- a band guard erases (`(? (coin? x) 1 e)`), the way core.l's ()-lane erases
  under wm2uu -- the model has one uniform value, so the obligation is STRONGER
  than the row, and each such row names the guard it dropped.
- 11 obligations / 40 rows; 3 are carried by a TYPE (sat-green, bit-bool and
  nil?-total are range checks the tower's codomain already answers) and 26 are
  off the model, each listed with the word that stopped it. the eight glued
  rows read `(= e e)` after opfix -- they are surface laws about a sigil and its
  word, and the model has no sigils.
- gated on a planted fault: rewriting bit-idem's row to `?x = !?x` -- false but
  translatable -- and the tool reports `no proof found` and emits 10, not 11.
  a search that rubber-stamped would not.

what is still open: the assoc/dist family, which wants the band lattice AND a
real `+`/`*` on values, where uumx today models only the dispatch.

## the structural equality (landed)

`paths` is finer than love's `=`: the depth index on a tree is a HEIGHT BOUND,
so one value has many spellings and identity would separate a value from its own
padded self. **veq** is the equality love actually has, and with it the cap/cup/
link laws land.

- the ENCODING changed first, and it is the reason the rest is short. `vtre (S d)`
  was `coprod (vtre d) (vtre d x vtre d)` -- ii1 a LIFT -- which made a node's head
  ambiguous, cap/cup recursive, and equality a walk over two indexed trees at once.
  it is `coprod msr (vtre d x vtre d)` now: ii1 is a LEAF, at any height. so a node
  is unambiguously a leaf or a pair, and vtwo/vcap/vcup/vleaf read it in ONE step
  with no recursion at all.
- **veq** compares what a node observes -- is it a pair, its cap, its cup, its
  measure -- down a FUEL (`succ (add du dw)`, since a step drops both heights).
  one recursion on a nat, where a walk over two trees would be two.
- **the link is total**, and both its limbs are padded STRUCTURALLY, the recursion
  following the tree and never stacking a level on top of a neutral depth. that is
  what `mx2` buys over `add`: `(mx2 0 b)` is `b` and `(mx2 (succ j) 0)` is
  `(succ j)` DEFINITIONALLY, so a pad lands on one index from either side.
  `(add j 0)` is stuck on a neutral j, and with `add` one limb always loses.
- **the padding is invisible** (uv-padl-fwd/bwd, uv-padr-fwd/bwd): a value and its
  padded self answer veq alike. that is the theorem the depth index owes -- one
  induction on the fuel, then on the two heights, then on the tree, in both
  argument orders since a law names its two sides in its own order.
- the laws: cap-total, cup-total, link-back, link-apart, id?-finer and =-total,
  all hand-proved in test/uuval.l and all CITED from test/uuvallaw.l, whose
  obligations still come off law.l's rows. law2uu grew a third strategy for them,
  **by the lemma the model names** -- so if a row moves, the lemma stops applying
  and the tool says `no proof found` (checked: swapping cap and cup in link-back's
  row drops it to 16 obligations).
- law2uu also learned that a guard the tower CAN read (`two?`) stays, as the orb
  it always was -- only an unreadable one erases -- and that love's `=` between
  two VALUES is veq, not paths. 17 obligations / 40 rows now, from 11.
- veq DISCRIMINATES, and test/uuval.l demos say so: a constant-true equality would
  prove every law above, so `(veq (nval 2) (nval 3))` and `(veq (link a b)
  (link b a))` are asserted FALSE beside the positives.
- one export wrinkle worth keeping: the Rocq elaborator does a `sum_rect`'s
  BRANCHES before it unifies the scrutinee's type, so a bare `pr2 p` on a pair the
  branch destructures asks for a family that is still a metavariable, and the
  unification goes higher-order. `vfst`/`vsnd` -- named projections carrying their
  own argument type -- pin it first-order. this only bit once the leaf branch
  stopped mentioning the subtree type.

## the band lattice, in uu (landed)

add-assoc / mul-assoc / mul-dist rest on the BAND LATTICE, which lived only in
Rocq (test/proof/rocq/mx.v, from tools/mx2coq.l). it lives in uu now too:

- **tools/mx2uu.l** -- mx2coq's uu twin, reading THE TABLE (src/mx.l, the same
  love datum love.c's mx.h is laid from) and deriving the band partition by the
  same rule -- kinds grouped by row+column equality across BOTH matrices at
  once -- so the two exports cannot disagree about what a band is.
- **test/uumx.l** -- the generated corpus: a kind is its index in mx.l's enum
  roster, a lane its index in appearance order, a band its class id, and the
  square is a GRID (mvec/mlist, nvec/nlist's shape one type up). ⚠ NOT one flat
  225-cell list indexed by arithmetic: every walk rides `npred`, which the eager
  NbE prices at O(index), and the flat version cost the corpus 9 s where the
  grid costs 0.4.
- **test/uumxlaw.l** -- the laws, HAND-WRITTEN over a table nobody typed. both
  squares factor through the band quotient; dispatch commutes up to mirror (and
  mirror is an involution); the numeric nine are one band, KNom and KString one
  more, KMint alone; () is the unit under + and the zero under * in every lane;
  the diagonal reads the lattice, one add/mul pair per band; and the cells the
  narrative names one at a time (nom+str spells, chain*chain is the cartesian
  product, a tablet dominates everything but a mint). every proof is `idpath
  true` over a bounded forall -- the kernel RUNS the 225-cell square, where
  mx.v's twin closes by vm_compute.
- planted faults, each with its positive twin: the cartesian cell is not the
  zero, the band assignment has to be the derived one, mirror is not the
  identity. and flipping one cell of the generated table makes the kernel refuse
  a proof outright (uu-idpath-mismatch, exit 1).
- gated by `make test_uumx` off the uu_corpus roster (regenerate + diff, so a
  src/mx.l edit with no refresh reddens), and exported: all 28 mx entries
  re-check in Rocq and Lean 4. mx.l's shape now stands in three kernels, twice
  in Rocq by two independent roads.

it also found a live bug in the exporters: uu2coq/uu2lean's silent-no-op gate
read an absent term seat with `!`, so a legitimate `(defn nm nat 0)` -- a code
table's first row -- reddened as MALFORMED. the seat is tested with `id? ()`
now. nothing in the corpus had a 0-valued def before.

what stays fuzz-only, permanently: the C primitives' agreement with the
model -- the same gap uuwm has with the C runtime under core.l, and here the
gap that a lane nom names the C function it says it does. the table is the
interface; love.c is the other side of it.
