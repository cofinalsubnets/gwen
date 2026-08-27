# ssagap -- what the four bespoke passes miss that SSA would catch

An instrument, not a gate (ccnif's rule). Three pieces:

- `irdump.tpl.l` -- dumps mooncc's FINAL text forms per TU through the baked
  module (`use 'moon` reaches `cc-parse`/`cgen-obj`); sed `@FILE@` per file, one
  process per TU (an unresolvable include ccdies -- doom.c and kmain.c drop out
  of BOTH sides, symmetrically).
- `ssagap.py` -- rebuilds the CFG outside the compiler and runs, to a fixpoint
  with joins: a constant/copy/flags analysis (recording only AT the fixpoint --
  the optimistic passes lie), a backward byte-range slot liveness, and a
  mem2reg census. Three modes attribute each fact: LINEAR in cfoldir's own
  domain (kcap 2^30, back-edges reset) = recognizer gap; LINEAR at 64 bits =
  + the kmax gap; GLOBAL = + the fixpoint. Unmodeled ops invalidate everything
  they touch, so every count is a floor. With `--rows` it prints canonical
  per-fn rows (C census cells, D dead stores, H per-def chains) instead.
- `differ.sh` + `valdiff.tpl.l` -- the rung-0 differential: crew/moon/val.l
  (the in-tree port) emits the same rows from the same forms, and the two
  outputs must be byte-identical. 2026-08-28: 8,200 rows over 80 TUs, OK.

Findings 2026-08-27 are in doc/misc/moon-gauge.md ("the SSA question, measured").
⚠ known contaminations the current version already corrects: sys/raw are not
terminators; outgoing-arg stores before a call are live; pointer stores kill
escaped spaces; facts recorded mid-iteration are not facts.

The chains layer (2026-08-28) refined the census: the cell across-flag is a
LINEAR span while a chain's crossing is CFG liveness, and the two disagree both
ways -- 527 call-free chains sit inside across=1 cells (per-def recoverable
beyond the hull), 495 call-crossing chains inside "call-free span" cells. The
promotion pot is per-CHAIN: 2,471 call-free chains, not 1,954 cells.
