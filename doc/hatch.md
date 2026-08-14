# hatch — distribution is cloning, install is a local rebuild

The **model** under [`doc/sb.md`](sb.md): what the objects are, and why installing and
cloning are one act. sb.md is the interface over it; this is the shape it implements.
`hatch` names the derivation step, not a program — there is no `hatch` binary, and the install
door that exists is `love up` (crew/sb/up.l = `sync` + `cook install`).

## The thesis

rustup downloads a prebuilt toolchain and keeps a clean line between *installing love* (for
users) and *checking out the source* (for contributors). There is no such line here — love
rebuilds itself from source with **no external toolchain** (mooncc +
[`crew/holo/link.l`](../crew/holo/link.l) + nolibc; `make test_raw` is gcc/glibc/ld-free).

So **installing *is* cloning-and-hatching a local checkout.** The two acts collapse:

- a plain **user install** = a checkout, pinned/read-only, hatched in place
- a **dev node** = the same checkout, left writable and tracking a channel

There is no separate "installer" program. There is one operation — *materialize a patch set,
then hatch it* — wearing different hats.

## What kind of program this is

- **toolchain multiplexer** (rustup, ghcup, nvm, pyenv, asdf) — manages versions/channels
  and drops PATH shims. Front-of-house we may grow into (`love +tip …`); not the base.
- **source-based installer / bootstrapper** (Gentoo `emerge`, Nix, `ghcup compile`) — ships
  a germ, rebuilds on the target machine. This is the axis.
- at its root, the classic **stage0 bootstrap** — how GCC 3-stages itself. `make test`
  double-bakes the egg already; the "installer" is that bootstrap pointed at a remote source.

Honestly: **a bootstrapping installer folded into the VCS**, closest in spirit to Nix
(build-vs-fetch is one operation, "realize this") and **pijul** (a repo is a set of patches over
a dependency DAG, no privileged trunk).

## The model

Three layers, bottom up.

### 1. the patch DAG

The ground truth is a set of **content-addressed patch sets** with a **dependency DAG**
(partial order). This is pijul's model, not git's: no linear canonical history, no privileged
`main`. Any dependency-consistent subset of patches is a valid source tree.

The discipline everything above rests on: **removal is an inverse patch, never a deletion.** A
revert adds a patch that undoes; the patch *set* only ever grows. That is what keeps the
release chain (§refs) totally ordered by inclusion even when behavior rolls back.

### 2. hatch — the derivation

`hatch : (source_tree, arch) → native binary`.

The type error to keep straight: **source is content-addressed by the patch set; the native
binary is a *derivation* of `(patch set, arch)`, not itself a patch.** So a germ cannot live
"in" the VCS as a patch. It is either

- **rebuilt locally** by an already-present binary (see §stage −1), or
- **fetched as a cached build output** keyed by `(patchset-hash, arch)`.

Cached-by-default is safe rather than a trust compromise, for two reasons:

- **the cache is auditable.** hatch is reproducible — `cc(cc(love))` is byte-identical
  (`make test_fixpoint`), so anything a CDN serves can be rebuilt bit-for-bit locally and
  diffed. A mismatch is *detectable*, not a matter of trust. This is Nix's substituter story
  with real bit-reproducibility underneath, which Nix mostly lacks.
- **cache population = the gate.** A patch set becomes cacheable exactly when it passes the
  gate (green + reproducible). "gate-green," "has cached germs," and "selectable by users" are
  **one status**, not three.

### 3. nest — the local home

The hatched binary lands in a **nest**: the local install/checkout directory (`~/.love`, with
`~/.local` links; `make install`'s layout owns it). A user's nest is pinned and read-only; a
dev's nest is writable and tracks a channel.

### stage −1: the one out-of-band germ

To run the VCS at all you need *a* native binary already. So there is exactly **one dumb https
GET** — the first germ, per arch (`make dist` bakes `out/dist/love-<arch>`) — and everything
after it self-hosts through the VCS. "Distribution == cloning" holds in steady state; the
bootstrap has a single download under it.

## Refs: one primitive, three policies

Above the DAG there is a single primitive — a **ref** (a selector over patch sets) — with three
mutability policies. `main` (git's human-maintained blessed trunk) **dissolves**:

- **release** — a *frozen* selection. A name pinned to one patch set, immutable. A snapshot.
- **dev branch** — a *moving* selection over a sub-DAG. Advances as patches land.
- **the default channel** — a *derived* selection, maintained by nobody.

Git needs a human to move `main` because it has no built-in notion of "good." Ours is
**mechanical**: gate-green + reproducible. So the default channel can be a *query* over that
gate rather than a maintained pointer.

Concretely, **the default channel is a sequence of releases ordered by inclusion** —
R₀ ⊆ R₁ ⊆ R₂ …. "Upgrade" is "move to a superset." This is well-defined precisely because of
the inverse-patch discipline: the patch set only grows, so ⊆ is total.

Two things do **not** go away when `main` does:

1. **the patch DAG itself** — without dependencies / partial order, "newest" and "on top of"
   have no meaning.
2. **one derived default** — or a naive install has nothing to resolve to. It sits where `main`
   sat; it is just *computed*, not *maintained*.

⚠ **A release freezes the tip SET, not a single tip.** Deps are per-path, so an independent
birth is never depended upon and stays maximal; a multi-tip head is the normal steady state,
not a fork to repair. See sb.md §bank.

## The near-term population is one

Public multi-user distribution is deferred — nothing above needs it, and everything above works
with a population of one. The near-term job is moving love development between machines
cheaply.

The primitive is a **head DAG state**, not a HEAD pointer. Edit on the laptop and the desktop
before syncing and head has **two tips** — two maximal patches with no order between them.
That is where the patch model earns its keep: git makes divergent tips a rebase/merge chore, a
pijul-style patch model makes it a **set union** — if the patches commute (different files,
independent edits: most of them), reconciling laptop and desktop is just "apply both." No
conflict, no merge commit. That painless union is the entire payoff of a patch DAG over a
linear trunk.

## The machinery this shares

The dock cluster ([`port/inle/`](../port/inle/)) does the hard half:

- applies a patch — `drive.l` `lay`s a `(path body)` proposal onto the tree,
- gates it — rebuild + `make test`, red reverts,
- **adopts** it — `serve.l` lands the change and re-execs onto the new generation (the
  two-generation adopt).

That "rebuild-self-from-a-patch, gate, swap onto the freshly-built binary" move **is**
install-and-upgrade, pointed at a local proposer instead of a remote patch source.

## Deferred

- **public distribution** — the substituter-as-CDN, multi-user channels, signing beyond
  reproducibility-audit.
- **binary cache as a full substituter** — the cached path above is this in miniature; a
  shared, populated, garbage-collected cache is a later lever.
- **the toolchain-multiplexer front-of-house** — `love +release-N …`, multiple nests side by
  side. The ref model already accommodates it.
