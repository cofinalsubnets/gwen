# kanren prover — search-then-certify

⚠ **Design sketch, not built.** What exists is the substrate below plus a self-contained spike,
`doc/proto/kanren-prove.l`. There is no `proves` relation in the library and no gate.

The verification bridge makes love a proof *checker* whose work two independent kernels
re-certify. This design would make love a proof **producer**: given a goal type `T`, kanren
*searches* for a term `e` with `e : T`, uu's kernel *checks* it, and the bridge *certifies* it
(Rocq + Lean). The search is **untrusted** — a buggy or incomplete search cannot fake a theorem;
uu's `chk` and then Rocq/Lean reject a wrong candidate. That asymmetry (cheap to search, sound to
check) is the whole design.

## The substrate that exists

- **kanren** (`love/kanren.l`, a registered module): a pure first-order `unify` (substitution or
  `ufail`, no occurs check, cycle-safe `walk`/`reify`) + a church-stream search monad. A goal is
  `(\ s stream)`; `===` unifies, `+` interleaves (disjunction — complete), `*` binds
  (conjunction); `query` runs it. So a relation is a polynomial in unifications.
- **uu** (`love/uu.l`, baked as the `uu` module; theorem corpus in `test/uu.l`): the trusted
  bidirectional checker `(chk cx en d t T)` (+ the predicative `pinf`/`pchk` for universes).
- **the bridge** (`tools/uu2coq.l`, `tools/uu2lean.l`): a uu-certified term → Rocq + Lean. A
  kanren-found proof would ride the SAME two-kernel certification a hand-written one does.

The pieces compose into a producer → checker → certifier loop; what is missing is the wiring.

## The shape: relational type inhabitation

Encode the typing judgment as a kanren relation `(proves G T e)` — "in context `G`, `e : T`".
Run it with `T` ground and `e` a fresh var and it *generates* inhabitants `e` of `T`. The rules,
run backward, ARE the prover:

- **var** — `e` is a hypothesis in `G` whose type unifies with `T`.
- **lam** — `T = (pi x A B)` ⟹ `e = (lam x b)`, recurse `(proves (G,x:A) B b)`.
- **app / backward-chain** — `e = (f a)`: pick a lemma/global `f : (pi _ A T)`, recurse
  `(proves G A a)` for the argument. (The arg type `A` is the search-explosion point.)
- **MLTT intro/elim** — `idpath` for a reflexive path, the recursors, `tpair` for Σ,
  `inl`/`inr` for coprod, `paths_rect` for J.

This is the relational typechecker run backward (miniKanren's inhabitation trick), specialized to
uu's term syntax — quoted lists, with kanren vars as the holes.

## The rungs

1. **relational checker, forward** — write `proves` for the MLTT core, run it FORWARD
   (`e`,`T` ground) and cross-check it agrees with uu's `chk` on the existing corpus. Builds
   and validates the relation with no search risk.
2. **backward search, bounded** — run `proves` with `e` fresh under iterative deepening.
   The easy fragment first: a direct inhabitant (a hypothesis, `idpath`, a one-lemma
   application) — hypothesis lookup, `idfun`/`funcomp`, a 1-step path proof.
3. **the path-algebra tactic** — specialize to `paths` goals: search by composing
   `pathscomp0` (transitivity), `pathsinv0` (symmetry), `maponpaths` (congruence) + the lemma
   database. "auto for equalities" — the most common obligation, and where the search is
   tractable. (A relational rewrite engine; `unify`/`reify` already backs `doc/proto/datalog.l`.)
4. **certify the found proof** — pipe a found `e` through uu's `chk` (trusted) THEN the bridge
   → Rocq + Lean, under a gate that no-ops without coqc/lean (like `test_uugen`/`test_uulean`).
5. **scale** — a `(solve goal)` entry; iterative deepening; the corpus as the lemma DB;
   memoization; a uu tactic `(auto goal) → term` usable INSIDE proofs.

## Risks

- **search explosion** (app guessing arg types): iterative deepening (bound proof depth);
  restrict app to the LEMMA DATABASE and unify a lemma's *conclusion* with the goal FIRST
  (backward chaining, not blind generation); type-directed pruning.
- **termination**: kanren's interleaving is complete but won't terminate on an UNPROVABLE
  goal. The prover is a SEMI-decision procedure under a depth/step budget — "found" or "gave
  up", never "false".
- **higher-order unification**: MLTT is dependent; full HOU is undecidable and the pure
  `unify` is first-order. Restrict to PATTERN (Miller) unification or a first-order skeleton,
  and let `chk` validate the dependent details the search approximated.
- **trust**: the search is untrusted — `chk` + Rocq + Lean are the anchors. A bug in `proves`
  yields a candidate that fails `chk`, never a false theorem. This is *why* a hairy search is
  safe here.

It complements, not replaces, the differential-oracle leg (`proof/rocq/extract.v`, which ties the
model to the binary): export ties DISCOVERED laws to independent kernels. See doc/uu-universes.md
for uu and the two-kernel bridge, doc/verify.md for the whole lattice.
