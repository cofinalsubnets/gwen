# verify — one ladder, uu at the top

the verification angles grew independently and there are too many. this plan
names them all, keeps one, and says what happens to each of the others.

## the stance

**the native uu prover (love/uu.l) is target number one.** a property worth
holding is worth stating as a uu term; rocq and lean are EXPORT targets
(mk/tools/uu2coq.l, uu2lean.l -- both wired: test_uugen, test_uulean), not
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
| holo encoder fuzz | test/holo/fuzz/*.py | PORT TO LOVE, then rm the py |
| vmret.l vs vmret.py differential | mk/tools/py/ | RETIRE the py side |
| uu-vs-UniMath parity audit | mk/tools/py/uu_parity.py | PORT TO LOVE |

## no python in tree except benchmarks

the standing rule (gwen, 2026-08-16). the inventory outside test/bench/:

- **test/holo/fuzz/{fuzz,sysdiff,regmap}.py** -- the encoder differential
  fuzz (test_holofuzz). the oracle is objdump/llvm-mc, which stay; only the
  HARNESS is python. port shape: holo loads in-process (the py shells out to
  love per form; the love harness just calls the encoder), bytes to a temp
  file, spawn the disassembler, parse text, compare by abstract register
  identity and immediate value -- the same tolerance rules. gate the port on
  a planted fault: flip one encoder byte, the gate must go red.
- **mk/tools/py/vmret.py** -- was the golden reference for vmret.l. the
  differential paid while vmret.l was young; vmret.l has been the only tool
  the build runs for months. retired with the directory.
- **mk/tools/py/uu_parity.py** -- ported to mk/tools/uuparity.l (same audit:
  every unprefixed test/uu.l name must be published UniMath).
- **mk/tools/ccdb.py** -- compile_commands generator for clangd. dev-only,
  not verification; ports to love when touched next.
- **port/rp2040/tools/py/{elf2uf2,pad_checksum}.py** -- flasher utilities on
  a port lane. port to love with the next rp2040 ride.

## the uuwm freshen (landed 2026-08-16)

crew/lux/core.l is written in the new style (glued accessors, infix, !=,
bracket literals) and mk/tools/wm2uu.l reads the post-opfix tree it makes:
cap/cup chains where caup was, ></+ beside link/cat, != as = with the arms
traded. the regenerated test/uuwm.l is BYTE-IDENTICAL to the pre-freshen
artifact, so the uuwmlaw theorems and the ten idpath bridges hold unchanged.
the whole crew/lux/ also dropped the `(..) sugar for [..] (a comment- and
string-aware love scanner did the sweep, proving (forms old) = (forms new)
per file before writing).

⚠ the tree still spells `(L ..)` and `(..) widely (test/host/sh.l, the law
files, several mk/tools translators). the reader KEEPS both spellings; the
sweep is the canonical-spellings arc's business, and the corpus files that
TEST the ` sigil (test/valence.l, test/infixop.l) must keep it on purpose.

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
