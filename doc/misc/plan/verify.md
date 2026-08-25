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
| CLAUDE.md laws fuzz | test/law.l | keep; grows a uu leg (below) |
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

## next rung: the CLAUDE.md laws get a proof leg

test/law.l already quantifies the laws as lambdas over a jot-generated corpus
(refutation only, and it says so). the proof leg mirrors uuwm:

1. a VALUE MODEL in the uu substrate: an inductive val (the seven preamble
   inductives suffice -- nlist precedent) and the measure tower over it with
   codomain a SIGN TRICHOTOMY (neg/zero/pos). the laws never consume a gem's
   value, only the sign of its measure, so ieee never enters the model.
2. the retraction laws (sat-idem, bit-idem, sat-ceil, bit-sat, nil?-bit)
   proved of the model in uu; exported to rocq/lean for free.
3. a lawgen tool (uuwmgen pattern) compiling test/law.l's law rows into the
   obligations, so one spelling feeds fuzz and proof both.

what stays fuzz-only, permanently: the C primitives' agreement with the
model -- the same gap uuwm has with the C runtime under core.l.
