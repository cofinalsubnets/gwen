# c0: a baked global sharing a letrec local's name miscompiles the letrec — OPEN

Found 2026-08-17 landing seed-universal U2; worked around, not fixed. Delete this
file when the fix lands (and revert the workaround below with it).

## the symptom

With the crew baked, `love source` and `love seed` died at the call:

```
;; missing src-lay-love
```

`src-lay-love` is a sibling binding in lib/source.l's big top-level `:` (the
letrec form — it has a body, so its locals are not defglobbed). In the broken
compile, `src-lay`'s reference to it is emitted as `lvm_index` — a read of the
LIVE BOOK by name — instead of a letrec capture, and the book has no such
global. Which name goes missing depends on the session's book: the full love
said `src-lay-love`; love0 (smaller book, fewer folds) said `ujoin`.

## the trigger, bisected

Any **global named `want`, defined in any earlier layer** — a plain top-level
`(: (want a b) 0)`, or a module's own define (module 'moon's parse.l is the
production case; the module layer being left does not help, and module bindings
are otherwise invisible to later name resolution — verified with a `zzq` probe).
Then load a later file whose letrec matches source.l's shape. Two bisections:

- member-level: crew/moon/parse.l flipped the artifact bake bad; cutting
  parse.l at its `want` binding flipped it back. `(module 'probe (: (want a b)
  0))` alone reproduces; the same file with `blarg` does not. The name is the
  whole trigger.
- line-level, inside lib/source.l's seed-main: binding an inner-let local
  `want` and *reading* it was fine (cut G1); adding the next line,
  `cross? !(want = seed-arch ())` — the local read beside a sibling call —
  broke it (cut G2/G3 differential). A small synthetic with the same two lines
  does NOT reproduce: the surrounding big letrec is part of the surface.

## the repro (three commands, seconds)

```sh
printf '(: (want a b) 0)\n' > /tmp/probe.l
# broken: the probe global first          (checkout of 5fbe57f3 or earlier lib/source.l)
LOVE_NO_IMAGE=1 out/host/love -l /tmp/probe.l -l lib/gz.l -l lib/tar.l -l lib/source.l \
  -e "((peep (from 'verbs 'tab) 'src 0) (list \"/tmp/lay\"))"   # ;; missing src-lay-love
# control: drop the probe -l and it lays the tree
```

No bake and no image needed — the failure is **pre-save, at compile/eval in the
live session**, which rules the image codec out entirely.

## ruled out / observed

- not the macro table: `want` is in no `::` table, and `macros` only reads those.
- not module-layer name leakage: a module's `zzq` is invisible to a later `-e`.
- not boxfix's own analysis on its face — boxset/bref walk syntax and the macro
  table only. (A with/without diff of boxfix's *output* on source.l's form is
  still an open probe; boxfix is read off the book by c0, line ~1829.)
- **the compile diverges by exactly one resolution.** Instrumenting ana_v
  (core/love.c ~1585: print the lane — global-fold / global-index / lams /
  slot / fars-shadow / local / capture — for the noms want, src-lay-love,
  ujoin, seed-arch, src-lay) and loading probe+gz+tar+source under love0 gives
  byte-identical traces except ONE extra `want` resolved through imps/args in
  the probe session. Everything downstream of that single divergence is the
  mis-wired closure graph.

## where to look

The suspicious asymmetry is at ana_v's walk end (core/love.c ~1590): a BOUND
global folds as a quoted constant (`ana_q`) and is NOT recorded, while an
UNBOUND one is recorded into the scope's imps and emitted as `lvm_index`. So a
name's boundness changes a lambda's pass-1 import row. ana_d (the letrec
compile, ~1826) then runs closure propagation over those rows ("if e needs d
then e needs d's variables", keyed by name membership in the import lists),
deletes sibling fns for lazy binding, re-compiles each lambda against its
FROZEN row, and backpatches recorded sites. A row that differs by one member
between what the propagation saw and what the rebuild resolves would mis-slot a
capture — which is exactly what a sibling ref landing on lvm_index looks like.
The fars shadow guard and the `d->par->stack` exception in ana_v sit right on
this seam; ev.l's `feel` runs the same boxfix pass and may or may not share the
bug (untested — a differential worth running first).

## workaround + the gate that keeps it caught

lib/source.l names the local `aim` (comment at the site names this file).
test/gate/dist.sh's smoke now runs `love source`, so the class reddens in
test_slow instead of waiting for a release. A fix must hold test_fixpoint,
test_slow and test_distboot — c0 compiles the tree that compiles c0.
