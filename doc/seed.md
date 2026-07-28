# seed — the patch-set vcs

Status: **the MVP verb set is live** (2026-07-14): `record` · `sync` ·
**`apply`** · **`undo`** · **`bank`** · `log` · `diff` over the content-addressed store — [`crew/seed/seed.l`](../crew/seed/seed.l),
gated by `make test_seed`; a hunk is test/patch.l's proven `chg` at file grain
(slot = path, context = old content hash).

- **`sync PEER`** **exchanges** patch sets with a peer nest (a directory holding a
  `.seed/`) — it is not a fetch: pull the blobs + patches we lack, push the ones
  the peer lacks (content-addressed, so a union in either direction just fills
  gaps), then **settle both nests** — re-derive tips + snap from the *whole* patch
  set (order-free — the DAG is a pure function of its patches) and materialize
  onto a **clean** working tree (a dirty tree refuses, exit 1). Because the derive
  is a pure function of the patch set, both ends land on the *same* snap: after
  one sync the two trees are identical, from whichever side you ran it. The peer's
  half needs its tree clean and writable; when it is not, sync still pulls (always
  safe), leaves the peer's store **whole** rather than half-fed, and says so with
  exit 1. **Refs travel too** — a ref is a single file rather than a
  content-addressed one, so sync **unions it by name** instead of gap-filling:
  the same name at the same head is idempotent, and the same name at *different*
  heads is a human error (a banked name is immutable, and neither nest may
  repoint the other's), so **each side keeps its own** and the clash is reported
  once. That is what makes a release the unit you propagate rather than a local
  bookmark. A *convergent* write
  (two nests reach the same content) is silent. Same-path divergence **merges**:
  the incoming hunk names the content hash it expected, so the common ancestor is
  already in the store and the three sides go to a diff3 line merge
  ([`crew/seed/merge.l`](../crew/seed/merge.l)) — disjoint edits to one file both
  survive, and only a true overlap lands in `<<<<<<<` markers naming both
  patches, whereupon sync reports and exits 1. The resolution is an ordinary
  `record`, so no new verb: the fix is a patch like any other, and it settles the
  clash for good. A delete meeting an edit, or a binary file, cannot line-merge —
  those keep the **content** (never the deletion), name both blobs, and flag.

> **The build left, 2026-07-27.** `hatch` used to sit here, and it never touched
> the patch DAG: it read a config, ran an argv, hashed a file. **How germplasm
> builds itself is none of the vault's business.** It was lifted into its own
> tool and then that tool was dropped too — the distribution design was not
> settled enough to be writing code against, and building ahead of it produced a
> vocabulary nobody could follow. **Seed is a vcs.** The build, the install, and
> everything downstream start over from a blank page; `psid` is the one hook a
> future one needs, and it is a vcs concept in its own right.

- **`apply [ID..]`** realizes a dependency-consistent **subset** of the store into
  the working tree. This is git's `checkout` *and* its `cherry-pick`, which are
  one act here rather than two: the tree is a pure function of a patch **set**, so
  "the state as of P" and "what I have plus P" differ only in the set you name —
  there is no replay-a-diff-onto-a-foreign-state step, and so nothing for that
  step to conflict on. An ID may be a prefix (what `log` prints). A named patch
  drags its **dep closure** along, because a patch may not travel without the
  patches that wrote its context — leaving one behind would silently realize less
  than you asked for, since `topo` only readies a patch whose deps are all
  present. With no ID it realizes the whole store again: the way back. It is a
  **view** — the store never shrinks, and the next `sync` re-derives the union.
  To drop a patch for good you `undo` it, growing an inverse rather than
  forgetting.

- **`undo ID [NOTE]`** adds the **inverse** patch. Removal is growth here, never
  deletion: the patch stays, its dependents stay valid, and the store only ever
  gets bigger — which is exactly what keeps releases inclusion-ordered and
  "upgrade = move to a superset" well-defined. So it is git's `revert`, never its
  `reset`. It works through the working tree and hands off to `record`, so the
  inverse is a patch like any other and needs no special case downstream. A path
  that has **moved on** since is three-way merged rather than clobbered — base is
  what the patch wrote, ours is what the path holds now, theirs is what it
  replaced — so later edits survive and only that patch's write is lifted out. A
  true overlap lands in markers and records **nothing**: resolve, then record.

- **`bank NAME`** freezes the current head under a name — an immutable release,
  and the unit you propagate. `apply NAME` then realizes it, since the dep
  closure derives the whole patch set from the tips. Re-banking a name at the
  same head is a no-op; at a different head it refuses, because a banked name is
  immutable. `log` shows each ref with its **psid** — `sha256` of the sorted
  tips — the name of that release's head DAG state.
  **A ref freezes the tip *set*, not a single tip** — see the note below.

> **Correction to hatch.md.** That doc asks for a *single-tip head* here
> ("reconcile tips before cutting a release"), and that is git thinking which
> does not survive this model. Deps are **per path**, so a patch depends only on
> what it *touched* — an independent birth is never depended upon and stays
> maximal forever. Two or three tips is what ordinary parallel work looks like,
> not a fork to repair, and the only way to collapse them is to write a patch
> touching every path every tip touched, i.e. to edit files to appease the check.
> So a release freezes the tip **set**, which is the head DAG state whatever its
> shape — and it is exactly what `psid` hashes, which never demanded one tip
> either.

The rest of this doc is the design brief from the 2026-07-14 session. The
**interface** layer over the model in [`doc/hatch.md`](hatch.md) (the patch DAG,
the derivation, the nest, refs) and the machinery in
[`port/inle/{serve,drive,patch}.l`](../port/inle/patch.l) (the dock — adopt +
two-generation re-exec). hatch.md says *what the objects are*; this says *what
you type*. `seed` is the crew name (🌱, the spirit of the Svalbard Global Seed
Vault); the command name is provisional — see §Naming.

## the one principle

seed used to be a version control system **and** a distribution/install system,
because the thesis of hatch.md is that these are the *same act*: **installing is
cloning-and-building a local checkout.** The design rule was:

> Do not design a vcs verb set and a distro verb set. Design the **vcs
> primitives** plus one derivation verb, and let the distro front-doors be
> *named compositions* of those.

**Withdrawn 2026-07-27.** Not falsified — untested. The rule presumes a settled
picture of what install *is* (where binaries go, what owns them, what the unit
of distribution is), and that picture never got settled; a derivation verb was
built on top of the unsettled version twice, and both times the vocabulary went
somewhere it could not be followed. So this section is a **note to a future
design**, not a rule in force: the collapse is worth wanting, and the smell to
watch for is "install" or "upgrade" turning into an irreducible verb rather than
a composition. Decide what install is *first*.

**Settled 2026-07-28 (self-host rung 3).** Install is `sync` + `cook install`:
binaries go in the `~/.love` nest, `make install`'s layout owns them, and the
unit of distribution is a `.seed/` store any static host serves. The
composition lives *outside* seed's verb set — `love up [URL]`
(crew/seed/up.l, a verb of the dist artifact's rail, not of seed) syncs
`~/.love/src` and cook-installs the nest; run again, it is the upgrade. That
install stayed a composition of `sync` and a build is the withdrawn rule
holding after all — it just needed the build side (cook-native install,
rung 1) to exist first. Gate: test/gate/dist.sh (`make test_up`).

## the primitives

| verb | does | vcs hat | distro hat |
|---|---|---|---|
| **`record`** | working changes → a patch in the DAG | commit | — |
| **`sync`** | union patch sets with another nest (peer *or* URL) | the divergent-tips → set-union payoff | clone / pull / fetch-a-release are all this |
| **`apply`** | pull a specific patch/ref out of the local store into the working tree | checkout / cherry-pick (any dep-consistent subset is valid) | select which release a nest realizes |
| **`bank`** | freeze the current head (its tip **set**) → a named, immutable release | tag | the unit you propagate/clone |
| **`log`** | view the DAG + tips + refs | inspect | inspect |
| **`diff`** | working tree vs a ref, or ref vs ref | inspect | inspect |
| **`undo`** | add the *inverse* patch — revert as growth, never deletion | revert | rollback-by-superset |

`sync` is the star. Making it the single verb for clone / pull / push /
multi-machine-union is what literally realizes "distribution == cloning" at the
CLI: whether the other end is a peer machine or a release CDN, the operation is
the same — *exchange patch sets*. `apply` stays separate because it is *local*
DAG surgery (materialize a subset into the working tree), which the network
exchange isn't.

## the front-doors (sugar, not primitives)

- **clone** = `sync` from empty
- **install** and **upgrade** = `sync` + `cook install`, one composition for
  both: `love up` (see the settlement note above). `love down` is its twin —
  the nest and its `~/.local` links removed, with seed refusing for unrecorded
  work in `~/.love/src`.

These are named compositions for humans, deliberately *not* new verbs. That they
compose is the proof the collapse held.

## MVP — what rides this cut

hatch.md is explicit that the near-term job is a **population of one**: move
development between gwen's own machines cheaply, union the tips when they
diverge (§"this is a personal multi-machine sync tool"). So the cut needs exactly:

> **`record` · `sync` · `log` · `diff`** + **`hatch`**

That is the difference from the first sketch (`record` / `log` / `apply` /
`diff`): it swaps in **`sync`**, because the actual payoff is peer union between
laptop and desktop, which local `apply` never reaches. `bank` / `undo` and the
install/upgrade front-doors are **reserve-the-names, land-later** — enough to
prove the model, not the whole distro story.

## why seed (the metaphor earns the invariants)

`seed` is not decoration; a seed vault names the model's two hardest invariants
more accurately than "tree" or "reef" did.

- **Append-only cold storage is the inverse-patch law.** The core discipline is
  "removal is an inverse patch, never a deletion; the patch set only ever grows,"
  which is what makes the default channel R₀ ⊆ R₁ ⊆ R₂ … well-defined (⊆ total).
  That is a seed vault's literal operating principle: Svalbard never withdraws
  and discards, it only ever accepts more, and a depositor retains what they put
  in. Even a rollback is a new deposit, never an erasure. The metaphor carries
  the single most important invariant for free.
- **Distribution *is* cloning.** hatch.md's thesis is that installing and
  cloning-and-hatching are one act. A seed vault exists for exactly that: it is
  the duplicate backup the world's genebanks restore *from*, and a restore is
  not a special operation — it is the same exchange running the other way. That
  is `install = sync <url> + hatch`, almost verbatim.
- **What is stored is not what runs.** The vault holds **germplasm**, not
  plants; the germ has to be taken somewhere and grown before it is a living
  thing. That is exactly the type distinction §2 insists on — source is
  content-addressed by the patch set, the native binary is a *derivation* of
  `(patch set, arch)` and cannot live in the VCS as a patch. The metaphor makes
  the one type error we must not commit obvious on sight.

And the vocabulary comes with it rather than being invented for it: what a vault
keeps is a **germ**, what it does to one is a **viability test**, and to freeze a
release is to **bank** it — one word for putting a thing somewhere safe and for
the institution that keeps it.

The one thing traded: `tree` read as "version control" on sight; `seed` leans on
the persona to carry that. Since the model isn't a tree and the persona does the
work anyway, that's the right trade.

## naming

- **`seed`** — the crew name / system (🌱, the spirit of the Svalbard Global Seed
  Vault). No `seed(1)` collision, the same box `tree` failed. The *command* name
  is still open: `seed <verb>` reads fine; whether the bare install command is
  `seed <url>` or a friendlier alias is a small later call.
- **the germ.** The build output is a `germ` (was "seed", which collided with the
  tool the moment the tool took that name — and *germplasm* is what a vault
  actually conserves, so the rename is toward the domain, not away). The audit
  over it is **viable** / NOT VIABLE.
- **`hatch` stays.** It is not a plant word, and that is deliberate: egg / hatch
  / `born` is love's own bootstrap cluster, and hatch.md's whole claim is that
  the installer *re-runs the hatch* on your machine. The vault half is `sync`;
  `hatch` is the bootstrap half; install is the two composed. Two clusters
  meeting at the install seam, which is where they always met.
- **verb flavor.** Plain (`record` / `apply` / `bank`) is primary for
  discoverability. `spin` was wanted for a vault-flavored set and is **not
  available** — love.c registers a nif under that string and the egg mops the
  nom, so it reads free on the book while the table entry stands.
- **the local home.** hatch.md floats `nest` / `roost`. A vault would say
  **box** (Svalbard stores one sealed box per depositor) — but the two clusters
  meet cleanly at the install seam, so either works. gwen picks; naming's her
  call under the freeze.

## open forks

1. ~~**`sync` unified, or split `push`/`pull`?**~~ **Settled: unified.** One
   `sync` exchanges both ways and settles both ends, which is the thesis made
   literal. A peer that cannot take the push degrades to a pull and says so, so
   the authoritative-remote case (fetching a release) falls out of the same verb
   rather than needing a directional pair.
2. **Does a nest need an explicit `pick`/`use` to switch its live ref, or is that
   just `apply <ref>`?** Lean fold-into-`apply`, skip the verb.
3. **the command surface** — bare `seed <url>` as install, or a front-of-house
   alias. Deferred with the toolchain-multiplexer question in hatch.md.

## where it lives

`crew/seed/` + a book (the holo/kore all-the-way-down precedent). The dock
cluster ([`port/inle/`](../port/inle/), memory `the-dock`) already does the hard
half — apply a patch, gate it (rebuild + `make test`, red reverts), adopt it
(re-exec onto the new generation). `hatch` is that machinery pointed at a
*remote* patch source instead of the local model proposer; `sync`/`record` are
the DAG surface over the same store. See hatch.md §"What we already have."
