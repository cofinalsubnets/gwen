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
| love's `=` on values | veq, in test/uuval.l | structural, and an EQUIVALENCE |
| the +/* band lattice | mx2uu.l, test/uumx*.l | LANDED in uu beside mx.v |
| the band ALGEBRA | test/uuvalband.l | gval, ported: + and * on values |
| the charm ceiling, () vs 0 | test/uuval.l | spec.v's V, over the finer carrier |
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

the assoc/dist family this section left open is test/uuvalband.l's now, below.

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


## the band algebra (landed)

test/proof/rocq/spec.v had TWO value models the tree was not using together --
`V` (net/sat/nilp over Z) and `gval` (GUnit/GNum/GSeq with gplus and gtimes) --
both hand-written, both Rocq-only, and `gval`'s three bands typed in by hand
where test/uumx.l DERIVES the partition from mx.l's own table. this closes both.

**V, into the tower.** the two things `val` did not carry, and now does:

- **the charm ceiling.** zsat is the tower's retraction and stays unbounded --
  the round trip is a fact about the MEASURE. zsatc is what a word can hold, and
  the three clamp laws (keeps / ceils / clamps) are stated hypothesis-free:
  `(nmin chmax n)` IS a net that fits and `(add chmax n)` one that does not, so
  nothing rests on a decision procedure for the bound. the width is a parameter
  and uu names a small one; spec.v keeps the host's 2^62-1.
- **() is not 0.** a leaf carries a TAG (`atm = coprod unit msr`), so () and the
  number 0 net the same, read nil the same, and are still two values. this is
  the one place love's `=` is finer than the measure it would otherwise read by,
  and re-encoding the leaf was the whole cost -- nothing else in the model
  wanted the bit. it also sharpened cup-total: `cup` of an atom is () now, not
  the measure zero wearing ()'s name.
- the COLOURS came with it (green nonneg, red below the floor, blue the floor,
  green and blue dual not disjoint) and with them the sharpest of them:
  **truth is POSITIVE GREEN**, `?x` iff green and not blue -- CLAUDE.md's own
  lambda equation read as a colour. it needs the half bit, since a measure at 0
  with its half up already ceils to 1.

**gval, into uu.** test/uuvalband.l. the carrier needed no widening: after the
tag split, `val` already HAS gval's three shapes -- aunit is GUnit, a number
leaf is GNum, and a link spine is GSeq.

- **the integers really associate.** this is what + needed and uuval did not
  have. zadd is respelled by ITERATION (b counts the steps, each zsucc or
  zpred): four definitional equations, where the truncated-subtraction spelling
  computed the same sums and proved nothing. then zsucc/zpred inverse both ways,
  the step lemmas that carry a zsucc across an argument, associativity by
  induction on the RIGHT argument alone, and both units. the half-integers
  follow in eight cases on the three half bits -- the carry fires wherever two
  of them meet -- and the measures componentwise.
- **the bands are mx.l's bands.** () is KMint, a number KCharm, a chain KChain,
  and the cell `vadd` takes IS the lane `mxadd` dispatches to -- checked over all
  nine pairs against the table love.c's mx.h is laid from. the model's number is
  a numeric KIND, so kinds.l's tier lattice ranks WITHIN this one band rather
  than competing with it: the three vocabularies cut at different depths and
  disagree about nothing.
- **+ and * cell for cell.** the unit on either side, numbers add, chains
  APPEND, and a mixed band DEGENERATES -- and the net can SEE it degenerate
  (5 + '(1 2) nets 3, not 8), which is exactly what buys associativity over the
  whole carrier with no side conditions. append walks the left spine and
  replaces its tail whatever the tail is, so `(link 1 2) + '(3)` is `(1 3)`,
  which is what love answers. * has the annihilating zero on either side, the
  |count| repeat (absolute value -- love repeats twice for -2), and the
  cartesian, each pair a 2-list.
- the cell laws are stated over the SHAPE the cell is about, never over its band
  CODE: a code is a nat and a neutral one tells conv nothing, where a tree with
  a head reduces the whole dispatch. all but the right unit are one idpath.
- `x + ()` is a **veq** law and not a paths one: () + () answers the () the right
  operand spells, which sits at height 0 where the left one may not.
- **the differential runs the algebra**: love's own + and * beside vadd and vmul
  over every ordered pair of a twelve-value corpus, compared through the net,
  which is what sees a mixed band degenerate and what a wrong cell would move.

what * cannot have here: the halves are closed under + and NOT under * (a half
times a half is a quarter), so the multiplicative band is modelled at the
INTEGER rung. an exact * everywhere wants the dyadic rung, which is a different
carrier and not a lemma about this one.

+ associates over the whole carrier now, chain+chain included -- see the
transitivity rung below, which is what it was waiting on. what * does not have:
its numeric cell sits at the integer rung, the |count| action wants smul's
homomorphism over that rung, and the cartesian associates only up to the
canonical reassociation, which is a statement about ORDER that veq is too
coarse to make. spec.v holds gtimes_assoc_seq1/2/3 and law.l's mul-reassoc
holds the order seam meanwhile.

## the compose cell (landed)

test/uuhom.l proved the monoid laws of `homcomp` -- love's `*` on the top band,
prel's compose -- as a free-standing algebra about nothing in particular.
test/uumxlaw.l now NAMES the cell it is about: `mxLcompose` is the lane at the
hot diagonal, the mint there is the ZERO and not the monoid's unit, and the
Church sum is the band's other lane. a dispatch edit that moved compose off that
diagonal fails a proof rather than quietly leaving uuhom a monoid about nothing,
and the guard fails by name if uuhom's terms are not up.

## what spec.v keeps

not a strong/weak pair: uuval exports back to universe-checked Rocq through
uu2coq.l, so its theorems are available in a consistent metatheory too. what
stays in spec.v is the WIDTH (maxcharm at the host's 2^62-1, where uu names a
small one and proves the same three clamp laws against it), the order and colour
facts the rest of that file leans on, and the APPEND hom, which the list view
has and the tree view does not.


## the equality is an equivalence (landed)

veq was reflexive and nothing else, which is a preorder and not an equality --
and the missing half was the one every consequence needed. what stood in the way
was the FUEL: veq ran down `succ (add du dw)`, a number derived from a PAIR of
heights, so the three sides of a composition carried three different ones and
`veqf n u v` said nothing about `veqf m u w`.

**the fuel is gone.** veqd recurses on the LEFT value's index alone and the right
rides along, read one observation at a time -- is it a pair, its cap, its cup,
its atom. that is a structural recursion, it needs no bound, and it made every
proof already standing SHORTER: the four padding lemmas each dropped a level of
induction, and the link laws lost their fuel arithmetic. the right value never
has to be indexed at all, which is exactly why three of these compose.

- **each rung REFLECTS.** nateqb, beqb, zeqb, reqb, meqb and aeqb each hand back
  a path when they answer true. reflexivity alone gives a preorder; this is what
  makes `=` an equivalence, and every rung had to give it up before the tree's
  could. the two impossible crossings (a unit against a number, ii1 against ii2)
  are ex falso through the kernel's own `nopathsfalsetotrue`.
- **transitivity** is then the plain induction, with one cost: the MIDDLE value's
  shape has to be read, since veqd only reduces on a head. so each arm cases on
  the middle's index and tree, and the two arms where the middle shows a head the
  left does not are ex falso. the leaf case is shared by the base and the step.
- **the congruence** `uv-veq-link` is what transitivity was for: two links whose
  limbs answer alike answer alike. each limb meets the other at a height neither
  spells, so the proof goes down through the pad, across by the hypothesis, and
  back up the other pad -- three steps that only compose because veq composes.
- **the pad is invisible to APPEND** for the same reason it is invisible to veq,
  one induction each -- and then **append associates**, modulo veq. the left
  spine is walked once either way; what differs is only the height the middle
  result was built at.
- **and so + associates over the whole carrier.** twenty-seven cells over the
  three shapes a value has: twenty-five are one reflexivity, because both sides
  reduce to the SAME value -- which is precisely what the degeneration buys --
  and the two carrying mathematics are the numeric cell (the measure's own
  monoid) and the chain cell (append). the cells are written out one at a
  time because there is no VIEW lemma yet -- every value is veq-one of (), a
  number, or a link -- and rocq's `destruct x, y, z` on gval is exactly that
  view, free there only because gval is a declared inductive. the cells are the
  same content that destruct covers.

gated: transitivity with one hypothesis dropped is refused, the congruence given
only its left limb is refused, and append is asserted NOT commutative -- a law
that held of both would be holding of nothing.


## the pinned carrier, and the bridge (landed)

the equality apparatus is 385 of uuval.l's 1019 lines -- reflection 110,
transitivity 115, the four pad lemmas 120, the link congruence 40 -- and every
line of it exists because the index is a height BOUND, so one value has many
spellings and paths is too fine. this asks what the same carrier costs with the
index PINNED, and answers three things.

**the pin is not a new type family.** the plan said course-of-values
recursion, and that was wrong: within ONE index the spelling is ALREADY unique
-- a node either shows a leaf tag or a pair and the recursion is forced -- so
the whole multiplicity is the choice of d. `vtre`, `mx2`, `padl`, `padr` and
`netd` all carry over VERBATIM. what arrives is the true height and a path
saying the index is it:

    zht   : pi d nat (pi t (ztre d) nat)          -- the height a tree has
    ztight: (lam p (paths nat (zht (pr1 p) (pr2 p)) (pr1 p)))
    zval  : (total2 p zbare (ztight p))

the tightness is a path in nat, a value-level statement -- it never enters a
type index, so `mx2` stays exactly where it already is and the transport
worry does not arise.

**conv does not get heavier.** on the kernel alone the corpus runs 0.67 s;
with the pinned block alone, 0.68 s. uuval.l's 1019 lines cost
0.17 s in the same measurement. the eager-NbE hazard that bit nmin and the mx
table does not reappear -- zht folds the tree it is given and nothing else.

**and the congruence really is two maponpaths.** uv-veq-link is thirty lines
down through one pad, across by the hypothesis and back up the other, and it
stands on the 115-line transitivity block. here:

    (pathscomp0 zval (zlink u w) (zlink u2 w) (zlink u2 w2)
       (maponpaths zval zval (lam x (zlink x w)) u u2 p)
       (maponpaths zval zval (lam x (zlink u2 x)) w w2 q))

nine lines, no induction, and the same shape gives the congruence for net --
for ANY function, which is the whole point of paths being the equality.

what the pin costs in exchange: the pad must not move the height (two
inductions mirroring padl/padr), max needs a two-argument congruence, and the
link must carry its own tightness. that is 69 lines against 385.

gated: the pin does not move the net, the bridge below is refused with one
half dropped, and -- the point -- a leaf spelled at index 1 is a legal value of
uuval's carrier and a TYPE ERROR as a pinned one.

### what the spike got wrong about the restatement

the spike measured the carrier, the link and the congruence. it did not measure
the OBSERVERS, and the observers are where the bill is: `paths`, unlike veq,
cannot ignore the index, so `vcap` must hand back a limb at ITS height and the
cap/cup laws need the pad INVERTED. that wants the tighten/pad round trip, and
with it max-associativity, pad-composition in four flavours, and a transport in
every statement -- some six hundred lines to replace three hundred and eighty
five, with the whole model rewritten under it.

so the carrier was not swapped. it was ADDED, beside the one that was there,
with a bridge between them:

    uv-veq-paths : veq u w = true  ->  paths pval (pin u) (pin w)      SOUND
    uv-paths-veq : paths pval (pin u) (pin w)  ->  veq u w = true      COMPLETE

which is a better answer than the swap. veq was DEFINED and nothing said it was
the right relation; now love's `=` on values is proved to be exactly equality of
normal forms on a canonical carrier -- a decision procedure with a specification,
where before it was a specification with nothing behind it. and every law
crosses in one line, because the work is in the bridge:

- **pin is a homomorphism**: `pin (vlink a b) = plink (pin a) (pin b)`, the two
  leaf constructors pin on the nose, and `pnet (pin v) = vnet v` -- so pinning
  is a change of spelling and not of meaning. that last one is gated by a false
  twin; a normaliser that moved the net would carry every law to a different
  carrier.
- **the six cited laws** get paths twins in uuval.l. their SHAPES change, and
  that is the interesting part: an orb that carried a side condition becomes the
  side condition itself, and `=`-total -- which said the equality answers 1 --
  becomes the statement that veq DECIDES the equality. that is what a law reads
  like once its equality is an equality.
- **all twenty-seven associativity cells** get paths twins in uuvalband.l, each
  one line through the bridge, plus the right unit and append's own law. `+`
  associates over the whole carrier as an EQUALITY now, not as a decision.
- **tools/law2uu.l emits the twin mechanically** wherever a row's claim is an
  unguarded val-rung `=`. the guarded rows keep veq and must: an orb takes a
  bool, and a path is not one. that is stated in the generated header.

what did NOT change: `vtre`, `mx2`, both pads, `netd`, and every observer. the
pinned side is a wrapper over them, which is why it cost what it did.

### what it cost the fast gate, and the one line that cost most of it

the corpus run went 7.0 s to 13.6 s, and then to 10.2 s once the reason was
found. measured by ablation, in situ -- an isolated corpus tells you nothing
here, and run-to-run noise is about 0.4 s:

| block | seconds |
|---|---|
| uuval.l: the pinned carrier, the bridge, the homomorphism, the paths laws | +2.0 |
| uuvalband.l: the 27 cells and append's law, each at its own shape | +1.2 |
| **`uv-aa-paths`: ONE generic crossing over three neutral values** | **+3.4** |

the last row is the whole story, and it is the rule the cells were already
written to: state a cell law over the SHAPE it is about, never over a neutral.
`uv-aa-paths` quantified the bridge over three arbitrary values, so conv carried
`vadd`'s entire unreduced dispatch -- a bool_rect tree with a leaf per band pair
-- with `pin` unfolded inside every leaf, and paid for it on both sides of the
crossing. deleting it and letting each cell cross at its own shape, where `vadd`
reduces before `pin` ever sees it, gave back 3.4 s for no loss of content: the
27 cells say exactly what they said.

eager NbE has now billed this arc three times -- `nmin`'s b^a fold, the flat
225-cell mx table, and this. the shape is always the same: a term that would
reduce on a head is handed a neutral instead.
