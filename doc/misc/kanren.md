# kanren — unification, search, and the constraint rung

Companions: `love/kanren.l` (the module), `test/kanren.l` (the gate), `doc/misc/proto/kanren-prove.l`
(the proof producer), `crew/sat/kanren-count.l` (#SAT out of the ring), `doc/misc/proto/datalog.l`
(one that rolled its own unifier).

Written 2026-08-12 as a plan for three missing rungs; rewritten 2026-08-16, when they were built.

## what exists

A first-order miniKanren. The split matters, because it decides where anything new may land:

* **the unifier** — `unify` (a substitution, or `ufail`), `ufail?`, `var`, `walk`, `subst`,
  `rewrite`. Pure, first-order, no occurs check (deliberately: love's values are non-well-founded,
  so a self-referential binding is a rational tree and `walk_star` mu-marks the back-edge).
* **the search** — streams, `est`, the goal combinators, `query`.
* **the constraint store** — `dif`/`=/=`, `absent`, `apt`, riding beside `unify`.
* **the goal language** — `===` `=/=` `&&&` `|||` `\\\` `zz`, macros in kanren's own book, carried
  to a consumer by the `(use 'kanren)` splice.

Every name is ambient after the splice; `(from 'kanren …)` reaches the same set by hand, and
`(keys (from 'kanren))` introspects.

### the spelling

A relation is the ordinary operator **said three times**:

| ordinary | relational | what it is                                     |
|----------|------------|------------------------------------------------|
| `=`      | `===`      | unify (as a ring element; `est` is the bare goal) |
| —        | `=/=`      | disequality (`dif` is the bare goal)            |
| `&&`     | `&&&`      | conjunction — the ring's `*`                    |
| `\|\|`   | `\|\|\|`   | disjunction — the ring's `+`                    |
| `\`      | `\\\`      | a lambda whose params are minted as logic vars  |

All of them are all-punct, so all of them are infix-dyadic at house band for free — no
`operators` row (and none available: the table is mopped at the hatch). `=/=` shares a band
with `===`, so a chain folds by hand and wants parens.

`&&&` and `|||` are not a second implementation of the ring's `*` and `+` — they expand to the
same `conj2`/`disj2` the goal-coin's die calls, and those `load` their operands, so either
spelling takes either kind of operand: a bare goal or a `rel`-wrapped one.

## the search monad: a colist, not a church encoding

A stream is love's ordinary lazy list — `()` empty, `(x >< s)` mature, a thunk immature — told
apart by `lit?`/`two?`. This is canonical microkanren, and it replaced a church encoding that
passed three continuations (`n m y`) so that no type test was ever spent.

That encoding was an aesthetic choice and had never been priced. Priced now, on one binary per
leg (`perf stat -e instructions:u`, totals at two sizes so a fixed lump can be told from a rate):

| workload                              | church  | colist  | ratio |
|---------------------------------------|---------|---------|-------|
| cyclic-CNF #SAT, n=12 (39202 answers)  | 3.71 G  | 2.94 G  | 1.26  |
| cyclic-CNF #SAT, n=14 (228486 answers) | 21.90 G | 16.42 G | 1.33  |
| infinite disjunction, 40000 answers    | 0.91 G  | 0.76 G  | 1.21  |
| …the same, less the 0.31 G both spend booting | 0.60 G | 0.44 G | 1.35 |

**The church encoding costs 26–35% more, at every size measured** (and the gap grows with the
workload, so it is a rate, not a lump). It avoids a type test and pays three closures per step for
it, which is the worse trade in this runtime. A control leg — both representations written
standalone in one script, so neither rides the baked image and neither can be the one the glaze
warmed — agreed in direction: 18% on the SAT counts, 1.7x on the enumeration. The colist also
reads as what it is: `sX` is `link` and `sno` is `()`, so a realized stream simply *is* its list,
and `slist` is the identity on one.

⚠ **the dict scan was the bigger number, and it was a bug.** `dict_has` ended its loop on `(? ks …)`
— and `?` reads a list for its *net*, so every step summed the whole remaining key list, and a key
that nets to nothing (a var labelled `0`, a bare nom) ended the scan early and answered "unbound"
for a binding that was right there. `(two? ks)` is the test it wanted. Alone, on the old church
module, that fix was worth **2.0–2.3x** on the SAT counts. The two changes are independent and both
are in: the table above is measured with the fix on both legs.

## the constraint store

A **state** is `(substitution . constraints)`. The store rides *beside* `unify`, which keeps its
signature and its purity — the invariant below. A constraint is `(kind . payload)`, and every
successful unification re-verifies the whole store (cKanren's design). An unconstrained search
pays one `(nil? c)` test per `est`.

* **`dif u v`** / **`=/= u v`** — disequality. Unify on a copy and read the outcome three ways:
  `ufail` means the two can never be equal, so nothing is remembered; success with no new binding
  means they *are* equal, so the goal fails; anything else keeps the residual bindings as the
  constraint. Order-free — `(dif a 1)` before or after `(est a 1)` fails either way.
* **`absent x t`** — absento: `x` occurs nowhere in `t`, now or ever. Looks deep, and drops itself
  once `t` is ground.
* **`apt p x`** — a type constraint over *any* predicate: `(apt nom? x)` is symbolo, and the shape
  generalises to whatever a domain needs.

### the extra-logical three

`ifte c t e` is the soft cut and the primitive: if `c` has any answer at all, `t` runs over *every*
answer of `c` and `e` is dropped. `sole g` commits to the first answer. `naf g` is negation as
**failure** — it answers "not proved", never "false", and only a ground, terminating goal lets you
read the two as one. `conda`/`condu` are these two, spelled by hand where they are wanted.

`s_app` is the ordered disjunction beside `s_plus`: left-biased, ⚠ **incomplete by construction** —
that is the point, and every use site owes the word. `s_plus` stays the default; the two differ
only *at a delay*, which is the only place fairness can be spent or kept.

## alpha-equivalence, and what love gives for free

love's `=` on closures is **alpha + structural**, with an alpha-invariant hash behind it. Two
halves follow, and they are not the same half:

* **ground terms — free.** `tie` wraps a love lambda as a binder, so alpha-equivalence *is* `=`,
  instantiation *is* application, and there is nothing to swap, rename, or capture. `unify` reaches
  two ties by opening both at **one fresh nom** and unifying the bodies, which is nominal
  unification with the swapping done by the language. `absent` over a nom is alphaKanren's `a # t`.
* **search — not free.** A binder whose body is still a logic variable is not yet a function, so
  the functional representation cannot carry a hole. What serves there is the other half of the
  same coin: **mint the bound name as a nom**. A nom is unequal to every name a term can spell, so
  no rule can capture, and `query` renames the noms an answer carries `a1 a2 …` in first-sight
  order — so two proofs differing only in bound names **reify identically**, and `=` decides
  alpha-equivalence of answers. `doc/misc/proto/kanren-prove.l` does exactly this (`mintname`), and its
  gate asserts that the same proof found under two spellings compares equal.

⚠ A nom is data wherever it is not a variable's tail. That is now true — `var?` tests the *tail*.
It used to test the *second element*, which quietly made any term with a nom in second position
(`(app a x)` with `a` an atom) a logic variable, so the whole term unified with anything. The
nominal half could not have been built on top of that.

## what this rung is NOT for

It is not for the register allocator, and the evidence is on the record rather than assumed.

The alias question in `crew/moon/gen.l` — may this register be renamed to that one here — looks
search-shaped and is not. Its carrier is finite and small: 75 op shapes (one per op `rdsp` knows),
at most two read positions each, 15 registers in the gp file. `crew/moon/law.l` **exhausts** it —
390 forward renames through the real pass, 570 backward folds, every result judged by the real
assembler — for no measurable cost, and it goes red on all three deliberate falsifications. A
finite carrier is run, not searched. `test/uukindlaw.l` reached that first, in its own words: the
domain is finite, *"so the semilattice laws are DECIDABLE by exhausting the carrier."*

Where the shape does fit is the allocator's **global** coalescing choice (moon-alloc rung 5):
which move-related pairs to merge under interference is a real search, equality is `unify`,
interference is `=/=`. Even there kanren would be the oracle on small functions rather than the
engine — 640 functions in love.c, `gcp` alone 925 instructions, and real allocators go greedy
because that search is intractable.

## still open

* **A fast find.** `walk` chases `dict_has`, a linear scan, so find is linear in the substitution.
  The answer stays **don't**: the substitution is pure *because* search must undo it; path
  compression is fast *because* it destructively rewrites. A persistent union-find buys the
  asymptotics back at a log factor and several times the code. A consumer that needs a compressing
  find is not a kanren consumer — it should write fifteen lines of union-find where it lives.
  (⚠ and the 2x above was not asymptotics, it was a constant that should never have been there.)
* **Higher-order unification.** `unify` is first-order; the prover's lemma DB is non-dependent for
  that reason. `tie` narrows the gap for ground binders and does not close it.
* **Reifying the store.** `query` reifies the substitution; a residual `=/=` or `absent` on an
  answer is not shown. cKanren prints them; nothing here has asked yet.

## the invariant that must not move

`unify s u v` answers a substitution or `ufail`, purely, under that global name.
`crew/rune/rune.l` calls it directly (its gate verifies the 2026 jacobian-conjecture disproof),
and `love/overlay.l` reads `subst` through the registry. **The constraint store rides BESIDE it** —
it does not change `unify`'s signature, and a goal's state is where the store lives. Chosen,
revisable: the moment a constraint is worth threading through `unify` itself, this line is the
thing to argue with.

## gates and risks

* `test/kanren.l` is the gate and rides the `make test` glob. `test/host/rune.l` is the consumer
  that must stay green — it is the reason for the invariant above.
* kanren rides host, love0, wasm and the K_TEST kernel, so a size increase lands in four
  frontends. A SHIPPED kernel does not carry it (uu + bao + holo + peg + the kore cat), so the
  freestanding budget is not at risk.
* ⚠ **the ambient splice is the real cost.** kanren's names are in every corpus file's scope, and
  a name added there is hard to take back. Prefer the registry — `(from 'kanren 'absent)` — unless
  infix is genuinely wanted, and say why at the site if it is.
* ⚠ kanren now compiles with `@`, so it needs pat spliced before it. Every frontend that loads
  kanren already evaluates pat's text with post (see the boot comments in `host/main.c`,
  `free/kmain.c`, `wasm/host.c`, `port/playdate/main.c`) — but a new frontend must keep that order.
