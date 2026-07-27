# hatch — distribution is cloning, install is a local rebuild

Status: **design / brainstorm.** Pick-up doc from the 2026-07-13 session; the model
below is now partly built — see [`doc/seed.md`](seed.md) for what `seed` does today.
The artifact this doc calls a *germ* was a "seed" until the VCS took that name.
Companion to the dock ([`port/inle/{serve,drive,patch}.l`](../port/inle/patch.l), the
adopt + two-generation re-exec machinery) and the bootstrap (the *egg*, [`love/egg.l`](../love/egg.l)).
`hatch` is a **working name** — see §Naming.

## The thesis

rustup downloads a prebuilt toolchain and keeps a clean line between *installing love*
(for users) and *checking out the source* (for contributors). We don't want that line —
because love already rebuilds itself from source with **no external toolchain** (mooncc +
[`crew/holo/link.l`](../crew/holo/link.l) + nolibc; `make test_raw` is gcc/glibc/ld-free).

So for us, **installing *is* cloning-and-hatching a local checkout.** The two acts
collapse into one:

- a plain **user install** = a checkout, pinned/read-only, hatched in place
- a **dev node** = the same checkout, left writable and tracking a channel

There is no separate "installer" program. There is one operation — *materialize a patch
set, then hatch it* — wearing different hats. Everything else is in service of or
downstream from that.

## What kind of program is this?

Naming the category, because it decides what we're copying:

- **toolchain multiplexer** (rustup, ghcup, nvm, pyenv, asdf) — manages versions/channels
  and drops PATH shims. This is *front-of-house* we may grow into later (`love +tip …`), not
  what we build first.
- **source-based installer / bootstrapper** (Gentoo `emerge`, Nix, `ghcup compile`) — ships
  a germ, rebuilds on the target machine. This is the axis we're on.
- at its root, the classic **stage0 bootstrap** — how GCC 3-stages itself. We already do
  this in `make test` (double-bake the egg); the "installer" is that bootstrap pointed at a
  remote source.

Honest description: **a bootstrapping installer folded into the VCS**, closest in spirit to
Nix (build-vs-fetch is one operation, "realize this") and **pijul** (a repo is a set of
patches over a dependency DAG, no privileged trunk).

## The model

Three layers, bottom up.

### 1. the patch DAG (the VCS — the patch.l thread)

The ground truth is a set of **content-addressed patch sets** with a **dependency DAG**
(partial order). This is pijul's model, not git's: no linear canonical history, no
privileged `main`. Any dependency-consistent subset of patches is a valid source tree.

A crucial discipline for what's above: **removal is an inverse patch, never a deletion.**
A revert adds a patch that undoes; the patch *set* only ever grows. This is what keeps the
release chain (§2) totally ordered by inclusion even when behavior rolls back. darcs/pijul
do exactly this.

### 2. hatch — the derivation

`hatch : (source_tree, arch) → native binary`.

This is the type error to keep straight: **source is content-addressed by the patch set;
the native binary is a *derivation* of `(patch set, arch)`, not itself a patch.** So a germ
cannot literally live "in" the VCS as a patch. It is either:

- **rebuilt locally** by an already-present binary (the fallback path — chicken/egg needs a
  prior binary; see §stage−1), or
- **fetched as a cached build output** keyed by `(patchset-hash, arch)` — the default.

**Default is cached, fallback is local hatch.** For any gate-green patch set, CI bakes a
deterministic germ per arch and caches it; a client normally fetches that. The fallback
rebuilds locally when the cache is cold or distrusted.

Two properties make cached-by-default safe rather than a trust compromise:

- **the cache is auditable.** hatch is reproducible — Stage 8 proved `cc(cc(love))`
  byte-identical (the fixpoint, `68fe4b0c`). So anything the CDN serves can be rebuilt
  bit-for-bit locally and diffed. The network can police the cache for free; a mismatch is
  *detectable*, not a matter of trust. This is Nix's substituter story with real
  bit-reproducibility underneath, which Nix mostly lacks.
- **cache population = the gate.** A patch set becomes cacheable exactly when it passes the
  gate (green + reproducible). "gate-green," "has cached germs," and "selectable by users"
  are **one status**, not three.

### 3. nest — the local home

The hatched binary lands in a **nest**: the local install/checkout directory. A user's nest
is pinned and read-only; a dev's nest is writable and tracks a channel. Upgrading a nest is
the dock's existing move — see §What we already have.

### stage −1: the one out-of-band germ

To run the VCS at all you need *a* native binary already. So there is exactly **one dumb
https GET** — the first germ, per arch — and everything after it self-hosts through the VCS.
"Distribution == cloning" holds in steady state; the bootstrap has a single download under
it. Tiny, rare, worth naming so it doesn't surprise us.

## Refs: one primitive, three policies

Above the DAG there is a single primitive — a **ref** (a selector over patch sets) — with
three mutability policies. `main` (git's human-maintained blessed trunk) **dissolves**:

- **release** — a *frozen* selection. A name pinned to one patch set, immutable. A snapshot.
- **dev branch** — a *moving* selection over a sub-DAG. Advances as patches land.
- **the default channel** — a *derived* selection, maintained by nobody.

Git needs a human to move `main` because it has no built-in notion of "good." Ours is
**mechanical**: gate-green + reproducible. So the default channel can be a *query* over that
gate rather than a maintained pointer.

Concretely, **the default channel is a particular sequence of releases ordered by
inclusion** — R₀ ⊆ R₁ ⊆ R₂ …. "Upgrade" is "move to a superset." This is well-defined
precisely because of the inverse-patch discipline (§1): the patch set only grows, so ⊆ is
total.

Two things do **not** go away when `main` does:

1. **the patch DAG itself** — dependencies / partial order, or "newest" and "on top of"
   have no meaning.
2. **one derived default** — or a naive `love hatch` has nothing to resolve to. It sits where
   `main` sat; it's just *computed*, not *maintained*.

## Near-term: this is a personal multi-machine sync tool

Public multi-user distribution is **deferred** — nothing above needs it, and everything
above works with a population of one. The real near-term job: **move love development between
gwen's own machines cheaply.**

The primitive is a **head DAG state**, not a HEAD pointer. The moment you edit on the laptop
and the desktop before syncing, head has **two tips** — two maximal patches with no order
between them. That's a genuine DAG, not a chain, and it's exactly where the patch model
earns its keep:

- git makes divergent tips a rebase/merge chore.
- a pijul-style patch model makes it a **set union** — if the patches commute (different
  files, independent edits: most of them), reconciling laptop + desktop is just "apply
  both." No conflict, no merge commit.

That painless union is the entire payoff of a patch DAG over a linear trunk, and it lands on
the thing we actually want.

**A release is a snapshot of a single-tip head.** *(Falsified in practice — see
`doc/seed.md` §bank: deps are per-path, so independent births stay maximal and a
multi-tip head is the normal steady state. A release freezes the tip **set**.)* Day to day you have a DAG with occasional
divergent tips; you **union the tips when you sync**, and *then* cut a release. Because the
work is append-mostly, successive snapshots come out inclusion-ordered for free — which *is*
the default channel of §Refs. The one discipline: **reconcile tips before cutting a
release**, so the release sits on the chain rather than off to the side of a fork.

## What we already have

The hard part exists. The dock cluster already:

- applies a patch — `drive.l` `lay`s a `(path body)` proposal onto the tree,
- gates it — rebuild + `make test`, red reverts via git,
- **adopts** it — `serve.l` lands the change and you **re-exec onto the new generation**
  (the two-generation adopt; see [`the-dock`](../port/inle/) and memory `the-dock`).

That "rebuild-self-from-a-patch, gate, swap onto the freshly-built binary" move **is**
install-and-upgrade. `hatch` is that machinery pointed at a **remote** patch source instead
of the local model proposer (`patch.l`, which asks claude for a file body). Same operation;
different source of the patch set.

## Deferred / open

- **public distribution** — the substituter-as-CDN, multi-user channels, signing beyond
  reproducibility-audit. Not needed for a population of one.
- **binary cache as a full substituter** — the default cached path is already this in
  miniature; the question of a shared, populated, garbage-collected cache is a later lever.
- **the toolchain-multiplexer front-of-house** — `love +release-N …`, multiple nests side by
  side. Grow into it if we ever want it; the ref model already accommodates it.

## Naming

`hatch` (the act — download the egg, it hatches locally) and `nest` (the local home) lean on
the vocabulary that already exists: **egg / hatch / born** is the bootstrap cluster; the
installer literally re-runs the hatch on the user's machine. Alternatives floated: `roost`
(the local home), `germ` (the stage−1 artifact). All provisional — honor the rename freeze;
gwen names it.
