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
| CLAUDE.md laws proof | test/uuval.l, uuvallaw.l | the measure tower, proved |
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
- **test/uuvallaw.l** -- the DIFFERENTIAL. love's own saturate/bit/nil?/ceil run
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
- the exporters carry it: tools/uu2coq.l and uu2lean.l list test/uuval.l, so
  all twenty uv-* entries re-check in Rocq (axiom-free, universe-checked -- the
  filter uu's type-in-type kernel lacks) and in Lean 4, no sorryAx.

still open on this rung:

- a lawgen tool (uuwmgen pattern) compiling test/law.l's law rows into the
   obligations, so one spelling feeds fuzz and proof both. today the two files
   are kept in step by hand.
- the laws the model cannot yet state: cap/cup/link-back/link-apart and
   id?-finer want a structural equality on val, which the depth index makes a
   setoid question (net is lift-invariant, so the tower never had to care).
   add-assoc / mul-assoc / mul-dist want the BAND LATTICE -- mx.l's dispatch
   matrices, which today are modelled only in Rocq (test/proof/rocq/mx.v, from
   tools/mx2coq.l). an mx2uu.l on the wm2uu pattern is the obvious next step.

what stays fuzz-only, permanently: the C primitives' agreement with the
model -- the same gap uuwm has with the C runtime under core.l.
