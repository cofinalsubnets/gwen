# uu's universe hierarchy

uu ships two checkers over one evaluator. The **kernel** (`love/uu.l`) is type-in-type: the sole
sort is the bare symbol `UU`, every type-former returns it flat, and it is sound only after
export — the Rocq/Lean bridge is what filters out an unsound proof. Beside it, in `test/uu.l`,
is a **predicative checker** (`pinf`/`pchk`) with a real universe hierarchy, and that is what
makes uu mean something on its own: under it, `but_seriously_the_world_explodes : empty` is
*rejected*.

The reference (UniMath/Foundations) is itself predicative — it only `Unset Universe Checking`
for convenience — so this is the intended metatheory, not an invented one.

It is a checker **beside** the type-in-type one rather than an in-place flip, because concrete
predicative levels break UniMath's universe-*polymorphic* tower, which is most of the corpus. The
sound checker verifies the predicative core; the tower stays parked in the old checker — exactly
the fragment `tools/uu2coq.l` already skips. `pinf`/`pchk`/`sortof`/`cleq`/`predpi` share
`vof`/`conv` verbatim, so the evaluator and NbE are untouched: **levels ride on the sort value**,
and neutral-application NbE does not care.

## sorts and levels

- `UU0` is the symbol `UU`; `UU_i` for i ≥ 1 is `('U i)`. `ulvl`/`umk` convert.
- A **level** is a charm, `(lsuc l)`, or a level VARIABLE `(lv x)` — level *expressions*, not
  just concrete numbers.
- The kernel's `vof` carries ONE inert case for this: `(UU level) → ('U level)`, so `(UU l)`
  normalizes everywhere, including under binders, which is what enables polymorphic *body*
  checking.

## the typing rules (predicative, no impredicative Prop)

- `UUᵢ : UUᵢ₊₁` — the sort's own type climbs.
- `nat, bool, unit, empty : UU₀`.
- **Π/Σ formation, the heart:** `A : UUᵢ`, `x:A ⊢ B : UUⱼ` ⟹ `∏(x:A),B : UU_{max(i,j)}`. So
  formation infers each component's sort, asserts it is a `('U _)` — `sortof`, which scares
  `uu-not-a-type` otherwise — and returns the max.
- `coprod`: `A+B : UU_{max(i,j)}`; `paths A a b : UUᵢ` where `A : UUᵢ`.
- `the`/`let`/recursor motives track the level in use rather than hard-coding `UU` (large
  elimination into any universe is fine).

Predicativity makes universe-quantifying definitions climb: `∏(T:UU),T` lives ONE level up, in
UU1, while `nat→nat` stays in UU0. `iscontr`, `weq`, `isweq`, `hProp` and the hlevel tower follow
to UU1/UU2…

**The exported core stays at level 0.** `add`/`mul`/`natplus*` are `nat→…→nat` (UU₀); the
path-algebra combinators (`idfun`, `maponpaths`, `pathscomp0`, …) have *types* in UU₁ but are
*instantiated at UU₀*, which checks fine. So the whole uugen-exported fragment type-checks at
concrete level 0 with no annotations — the part we export is exactly the part this makes sound.

## cumulativity and the constraint solver

`A : UUᵢ`, `i ≤ j` ⟹ `A : UUⱼ`, implemented as a subsumption case in `chk`'s fallback: when the
expected type is `('U j)` and the inferred is `('U i)`, accept `i ≤ j` instead of exact `conv`.

`cleq` decides that as "the constraint `i ≤ j` is **satisfiable**", through `usat?`:

- a level expr parses to `(base offset)` — `lparse`: `lsuc^k(atom) = (atom k)`;
- a constraint `x ≤ y` becomes the difference edge `base(x) → base(y)` of weight
  `off(x) − off(y)` (so `base(y) ≥ base(x) + w`);
- a POSITIVE cycle (some level `≥` itself + positive) is unsatisfiable;
- `usat?` is Bellman-Ford longest-path: relax `|atoms|` times, then once more — if it still
  moves, a positive cycle exists.

This is COMPLETE where a per-constraint check is conservative. Satisfiable: `[a≤b, b≤c]`;
`[a≤b, b≤a]` (which forces `a=b`). Unsatisfiable: `[a+1≤a]`; **`[a≤b, b+1≤a]` — a CROSS-SITE
cycle no local check sees**; `[1≤0]`, the keystone concretely.

## implicit level inference

`qdefn` runs `elab`, which replaces each bare `UU` with a fresh metavariable `(UU (mv i))`, then
checks the definition — so a def written with bare `UU` type-checks with **no annotation**, its
levels inferred. Demonstrated on the actual UniMath `idfun` (one inferred level) and `funcomp`
(three independent ones). For constraint-free defs — π-formation only, no cumulativity — that is
the whole story: the metavars stay unconstrained, i.e. fully polymorphic.

## what holds, and what is not built

The asserts that pin it (all break-tested — flip one and expect a `;; assert` scare):

- the hierarchy: `UU0 : UU1`, `nat : UU0`.
- predicativity: `sortof (pi T UU T) = 1`.
- **the keystone**: `(! (cleq 0 (pinf UU) UU))` — UU's type does not fit in UU0, so `UU : UU` is
  rejected and Girard cannot form the fixpoint; `(cleq 0 (pinf UU) (U 1))` holds.
- **the keystone over a variable**: `UU_{(lv a)} : UU_{(lv a)}` is rejected, so soundness is not
  a fact about concretes.
- polymorphism: `idfun`'s body checks against `∏(T:UU_{(lv a)}),T→T` for an arbitrary level
  variable — one body, every universe — and `sortof` computes its universe symbolically as
  `(lsuc (lv a))`. The same body `pdefn`s at both `(UU 0)` and `(UU 1)`.

Not built, and the whole-corpus migration is a genuine multi-session project — partly impossible:

1. **Constraint ACCUMULATION** for *cumulative* defs needs `cleq` to record into a global store
   instead of deciding locally, then one global `usat?`. ⚠ A mode-flag version worked on the host
   and **OOM'd under love0's baked self-test** (`oom@len=2^30`): a `peep` of mutable tablet state
   on every `cleq` miscompiles through c0 / the non-TCO trampoline. Re-landing it needs either a
   c0-robust accumulation or running it off the baked path. **Mutable-tablet state in a hot path
   is the concrete blocker to solve first.**
2. **generalize / instantiate-per-use** across the whole corpus (a polymorphic def used at
   several levels).
3. **the full recursor/Σ checker** — `pinf`/`pchk` handle only what the core and the landmarks
   need (π/Σ formation, λ, app-spine, `paths`/`idpath`, `the`/`let`/`succ`, cumulativity). The
   corpus's `nat_rect`/`total2`/`coprod_rect` inference is not built, and the core LAWS' proofs
   are `idpath`/lemma-application rather than raw recursors, so inference never needs them.
   `pdefn` checks-without-pinning (no GLOB pollution; uu2coq ignores it).
4. **The type-in-type-exploiting defs** (`the_world_explodes`, the ordinal descent) **cannot** be
   made predicatively consistent — they MUST stay rejected. So "the whole corpus green under
   predicative" is not a goal; the goal is "the predicatively-sound corpus green, the paradox
   defs rejected."

## gotchas

⚠ **uu installs a scare-SWALLOWING help for its negative-test window.** `assert` scares on a
false claim, so a `(assert …)` placed inside that window passes VACUOUSLY. Place predicative
asserts BEFORE the window, and express a rejection as a pure boolean `cleq` — not `rejects`,
which needs that window's HELPC-delegating help.

⚠ **A silent reader-stop also exits 0.** Confirm a section ran by probing a binding it defines
AND by break-testing an assert, never by exit code alone.

`tools/py/uu_parity.py` keeps names UniMath-compatible (`UU` kept; `(UU i)` is a local form).
uu2coq can keep mapping `('U i)` → `Type` and let Coq infer, or emit `Type@{i}`.

See doc/verify.md for the bridge this sits under.
