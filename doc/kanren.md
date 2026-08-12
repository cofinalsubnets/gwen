# kanren — the constraint rung

⚠ **Plan, not built.** Written 2026-08-12, out of the moon-alloc arc asking whether kanren could
carry an allocator's alias partition. The answer was no, for a reason worth keeping (below), but
the asking exposed three things the module is actually missing. Companions: `love/kanren.l` (79
lines, the module), `test/kanren.l` (the gate), `doc/kanren-prover.md` (the sketch that would be
the obvious consumer and, on reading, is not), `doc/proto/datalog.l` (one that rolled its own).

## what exists

A pure first-order miniKanren in 79 lines. The split matters, because it decides where anything
new may land:

* **global** — `unify` (a substitution, or `ufail`), `ufail?`, `var`, `s_plus`, `s_star`, `===`.
  The boot splices kanren ambient, so the corpus reads these bare.
* **registered** — `walk`, `est`, `query`, `subst`, `empty_dict`, the streams, `rel`, reached
  through `(from 'kanren …)`.
* **macros** — `zz` `et` `vel` `\\`, in kanren's own book's macro slot, riding the splice.

`unify` is PURE and marked so in the source; the substitution is a triangular dict walked by
`dict_has`, a linear scan. `s_plus` interleaves — fair disjunction, so search is complete. There
is no occurs check, deliberately: `doc/proto/datalog.l` wants rational trees and makes the check
an argument in its own copy.

## what is missing

1. **Disequality (`=/=`).** There is no constraint store: `unify` answers a substitution or
   `ufail`, and that is the whole surface. So "these two must never be equal" is unsayable, and
   every relation with a negative side condition has to be written around it.
   ⚠ and note what does NOT ask for it: `doc/kanren-prover.md` names four risks — search
   explosion, termination, higher-order unification, trust — and disequality is none of them.
   That is the strongest argument against this rung, so it belongs here rather than buried.
2. **Ordered disjunction.** `s_plus` interleaves, which is right for a prover (completeness) and
   wrong for optimisation, where you want best-first and the first answer. Fair disjunction gives
   you every answer and no opinion.
3. **A fast find.** `walk` chases `dict_has`, so find is linear in the substitution.

⚠ (3) is a genuine tension and the plan's answer is **don't**. The substitution is pure *because*
search must undo it; path compression is fast *because* it destructively rewrites. A persistent
union-find buys back the asymptotics at a log factor and several times the code. A consumer that
needs a compressing find is not a kanren consumer — it should write fifteen lines of union-find
where it lives.

## what this rung is NOT for

It is not for the register allocator, and the evidence is on the record rather than assumed.

The alias question in `crew/moon/gen.l` — may this register be renamed to that one here — looks
search-shaped and is not. Its carrier is finite and small: 75 op shapes (one per op `rdsp`
knows), at most two read positions each, 15 registers in the gp file. `crew/moon/law.l` now
**exhausts** it — 390 forward renames through the real pass, 570 backward folds, every result
judged by the real assembler — for no measurable cost (6.97 s against 7.06 s for the law file
without it), and it goes red on all three deliberate falsifications. A finite carrier is run,
not searched. `test/uukindlaw.l` reached that first, in its own words: the domain is finite,
*"so the semilattice laws are DECIDABLE by exhausting the carrier."*

Where the shape does fit is the allocator's **global** coalescing choice (moon-alloc rung 5):
which move-related pairs to merge under interference is a real search, equality is `unify`,
interference is `=/=`. Even there kanren would be the oracle on small functions rather than the
engine — 640 functions in love.c, `gcp` alone 925 instructions, and real allocators go greedy
because that search is intractable. So: a reason to revisit, not a reason to build now.

## the rungs

* **rung 0, price it — and expect "not yet".** Count the sites that want `=/=` TODAY, not
  hypothetically. The prover sketch does not ask for it. `datalog.l` would want it for negation
  and rolled its own unify anyway. rune's matcher is positive-only. On today's evidence the
  honest count is ZERO, so the rung stops here until a consumer arrives — the cs-seat refusal in
  the moon-alloc ledger is the model, and that refused a rung already built and measured. The
  likeliest first consumer is moon-alloc rung 5's coalescing oracle; build when it asks.
* **rung 1, `=/=`.** cKanren's design, adapted: a state is `(s . c)` with `c` a list of
  constraint sets. `=/=` unifies on a copy and reads the outcome three ways — `ufail` means the
  constraint is permanently satisfied and drops; success with no new bindings means it is
  violated and the goal fails; anything else keeps the residual bindings as the constraint. Every
  later `est` re-verifies the store. ~30-40 lines. Laws in `test/kanren.l`.
  ⚠ `=/=` is all-punct, so it is infix-dyadic at house grip for free, exactly as `===` is — no
  `dyadics` row, and none available anyway (the table is mopped at the hatch). It shares grip
  with `===`, so a chain folds by hand and wants parens.
* **rung 2, ordered disjunction.** `s_app` beside `s_plus`, left-biased. ⚠ incomplete by
  construction — that is the point, and every use site owes the word.
* **rung 3, REFUSED pending evidence.** The fast find, per the tension above.

## the invariant that must not move

`unify s u v` answers a substitution or `ufail`, purely, under that global name.
`crew/rune/rune.l` calls it directly (its gate verifies the 2026 jacobian-conjecture disproof),
and it is the only in-tree caller of the bare name — `doc/proto/datalog.l` deliberately rolled
its own so the occurs check could be an argument. **A constraint store rides BESIDE it** — it does
not change `unify`'s signature. Chosen, revisable: the moment `=/=` is worth threading through
`unify` itself, this line is the thing to argue with.

## gates and risks

* `test/kanren.l` is the gate and rides the `make test` glob. `test/host/rune.l` is the consumer
  that must stay green — it is the reason for the invariant above.
* kanren rides host, love0, wasm and the K_TEST kernel, so a size increase lands in four
  frontends. A SHIPPED kernel does not carry it (uu + bao + holo + peg + the kore cat), so the
  freestanding budget is not at risk.
* ⚠ **the ambient splice is the real cost.** kanren's global half is in every corpus file's
  scope, and a name added there is hard to take back. Prefer the registry — `(from 'kanren '=/=)`
  — unless infix is genuinely wanted, and say why at the site if it is.
* ⚠ `s_plus` stays the default disjunction. An ordered one that quietly becomes the default
  trades completeness for speed without anyone deciding to.
