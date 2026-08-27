# ssagap -- what the four bespoke passes miss that SSA would catch

An instrument, not a gate (ccnif's rule). Two pieces:

- `irdump.tpl.l` -- dumps mooncc's FINAL text forms per TU through the baked
  module (`use 'moon` reaches `cc-parse`/`cgen-obj`); sed `@FILE@` per file, one
  process per TU (an unresolvable include ccdies).
- `ssagap.py` -- rebuilds the CFG outside the compiler and runs, to a fixpoint
  with joins: a constant/copy/flags analysis (recording only AT the fixpoint --
  the optimistic passes lie), a backward byte-range slot liveness, and a
  mem2reg census. Three modes attribute each fact: LINEAR in cfoldir's own
  domain (kcap 2^30, back-edges reset) = recognizer gap; LINEAR at 64 bits =
  + the kmax gap; GLOBAL = + the fixpoint. Unmodeled ops invalidate everything
  they touch, so every count is a floor.

Findings 2026-08-27 are in doc/misc/moon-gauge.md ("the SSA question, measured").
⚠ known contaminations the current version already corrects: sys/raw are not
terminators; outgoing-arg stores before a call are live; pointer stores kill
escaped spaces; facts recorded mid-iteration are not facts.
