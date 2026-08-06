# the crew as faces of `top`

A categorical reading of how the apps relate to the core. Lore and organizing principle, with
the two gluings machine-checked in `proof/rocq/spec.v` (`Section Faces`, axiom-free).

**`top` = the language itself.** It is the top of its own lattice: everything applies,
every value has a kind, and "is-it-top" is vacuous (you may as well ask `0`). So the core is
a single universal object, and the crew are **faces** of it — each a way the core meets a
boundary. The *outward* faces come in **dual pairs**, glued each by *its own* universal shape
(there is no uniform functor — they associate by their own preferences); the *inward* face,
at love's own limit, stands alone.

**Source faces — `read` / `feel` — COMPOSE.** The two legs of `charms → top`:
- `read : charms → forms` (the reader — already the surface primitive; parse the chars).
- `feel : forms → top` (the weaver / the `wev` source pre-pass — sense what is static, fold it).
- The charm face reaches `top` by **composing through** the feel face: `feel ∘ read`. (The stream
  shell is exactly this — its body is `read` then `ev`, and `ev` is `opfix · feel · ana · cata`.)
  So **the charm face composes through the lisp face**: two input lanes, data and characters,
  meet at one core — `charms ─read→ forms ─feel→ top`, the cospan `charms → top ← forms`
  converging on the core.

**World faces — `bao` / `ain` — COPRODUCT.** They do not compose; one shared i/o trunk
**forks**: `bao` (the local face — the console, the shell/bridge) and `ain` (the net face —
the wire). `local ⊔ ain` — either/or off the trunk, not a pipeline.

**Help face — the third boundary, INWARD.** Source and world are *outward* — how love meets
what's outside it. The help face turns on love's own **help**: the boundary where the core *raises*
because it cannot answer, and calls out. It is the **dual of the honest face** ([[honest-face]]):
when help is missing the honest face shows the bare condition; the help face is help PRESENT — the
answer that absence was missing. The runtime `help` handles conditions in a *program*; this face
handles them in the *language* — probes the binary, finds the drift, keeps the words matched to the
work. That is why it is the **author's** face: when the language itself has no answer, the human is
the help of last resort.

**Around the faces:** `tele` the **mind** (the `decide`, the telescope reading the constellations);
`inle` the **body/substrate** it sails; `cook` the **keeper** that builds the ship.

So: one object (`top` = love, the language), three boundaries — **source** (read/feel, *compose*),
**world** (bao/ain, *coproduct*), **help** (the inward dual of the honest face). Two gluings
for the outward pairs, one inward face for the limit.

## The proof

`Section Faces` in `proof/rocq/spec.v`, gated by `test_proof`/coqc. The **composition** half is
*definitional* (`source := feel ∘ read` → `reflexivity`); the **coproduct** half is the one real
(standard) universal-property proof, its uniqueness stated **pointwise** to stay axiom-free (no
funext). `source_factors`, `world_inl`/`world_inr`, `world_unique`, `top_vacuous`; the axiom audit
runs `Print Assumptions world_unique` / `source_factors` → both "Closed under the global context."

```coq
Section Faces.
  Variables Top Charm Form Out Loc Net : Type.
  (* SOURCE faces compose: the charm face runs THROUGH the feel face *)
  Variable read : Charm -> Form.
  Variable feel : Form  -> Top.
  Definition source (c : Charm) : Top := feel (read c).
  Theorem source_factors : forall c, source c = feel (read c).
  Proof. reflexivity. Qed.

  (* WORLD faces coproduct: one trunk forks, local ⊔ net; the UP, pointwise-unique *)
  Variable bao : Loc -> Out.
  Variable ain : Net -> Out.
  Definition fork (x : Loc + Net) : Out :=
    match x with inl l => bao l | inr n => ain n end.
  Theorem world_unique :
    forall h : Loc + Net -> Out,
      (forall l, h (inl l) = bao l) -> (forall n, h (inr n) = ain n) ->
      forall x, h x = fork x.
  Proof. intros h Hl Hr x. destruct x; [apply Hl | apply Hr]. Qed.
End Faces.
```

(`I` is shadowed in spec.v by the imaginary unit `I : Zi`, so `top_vacuous` proves `True` via
`constructor`, not the `I` ctor.)

## The hourglass reading

Read the three theorems together and `top` is the **waist of an hourglass**:

1. **One center, nothing escapes it.** `top_vacuous`: *everything is top* — the language is the
   universal object; the faces are faces *of everything*.
2. **The world funnels IN by composition.** `source_factors`: `source = feel ∘ read`, proved by
   **`reflexivity`** — the convergence is not a discovered fact but *how the thing is built*. The
   charm face is a stage bolted in front of the lisp face; text → forms → top, many representations
   collapsing inward toward one core, by construction.
3. **The core fans OUT by coproduct.** `world_unique`: the fork off the i/o trunk is the **unique**
   mediating map — the local/net split is not arbitrary but *forced*; output branches, and the
   branching is determined.

So: source pours *in* through composed faces (a cone converging on the core — definitional); the
core pours *out* through co-producted faces (a cocone diverging from it — proven unique). **Input is
a limit (convergence, composition); output is a colimit (divergence, coproduct).** The same duality
runs through the value space — the numeric tower a **limit** (completion, converging), the string
monoid a **colimit** (free, diverging), meeting at the mint-NNO. The architecture and the type
theory rhyme: things converge inward by limits and composition, diverge outward by colimits and
coproducts, with a center between. The reader-into-weaver and the bao/ain fork are that one shape,
wearing work clothes.
