# seed 🌱 — the patch-set vcs

A version control system whose object is a **set** of patches, not a chain of snapshots.
The tree is a pure function of the patch set, so order is not part of the state, "the state as
of P" and "what I have plus P" differ only in the set you name, and a union in either direction
just fills gaps. [`crew/seed/seed.l`](../crew/seed/seed.l) is the tool, `make test_seed` the
gate; a hunk is test/patch.l's proven `chg` at file grain (slot = path, context = old content
hash), and the store is content-addressed under `.seed/`.

The model — the patch DAG, the derivation, the nest, refs — is [`doc/hatch.md`](hatch.md);
this doc is the interface over it. hatch.md says *what the objects are*; this says *what you
type*.

## the verbs

| verb | does | vcs hat | distro hat |
|---|---|---|---|
| **`record [NOTE]`** | working changes → a patch in the DAG | commit | — |
| **`sync PEER`** | union patch sets with another nest (peer dir *or* http URL) | the divergent-tips → set-union payoff | clone / pull / fetch-a-release are all this |
| **`apply [ID..]`** | realize a dep-consistent subset of the local store into the working tree | checkout / cherry-pick, one act | select which release a nest realizes |
| **`bank NAME`** | freeze the current head (its tip **set**) → a named, immutable release | tag | the unit you propagate |
| **`undo ID [NOTE]`** | add the *inverse* patch — revert as growth, never deletion | revert | rollback-by-superset |
| **`log`** | the patches, newest first (`*` marks a tip); each ref with its psid | inspect | inspect |
| **`diff`** | working tree vs the recorded state (unified; exit 1 on change) | inspect | inspect |

`sync` is the star. Making it the single verb for clone / pull / push /
multi-machine-union is what realizes "distribution == cloning" at the CLI: whether the other
end is a peer machine or a release CDN, the operation is the same — *exchange patch sets*.
`apply` stays separate because it is *local* DAG surgery (materialize a subset into the working
tree), which the network exchange isn't.

### sync

`sync PEER` **exchanges** patch sets with a peer nest (a directory holding a `.seed/`) — it is
not a fetch: pull the blobs + patches we lack, push the ones the peer lacks (content-addressed,
so a union in either direction just fills gaps), then **settle both nests** — re-derive tips +
snap from the *whole* patch set (order-free — the DAG is a pure function of its patches) and
materialize onto a **clean** working tree (a dirty tree refuses, exit 1). Because the derive is
a pure function of the patch set, both ends land on the *same* snap: after one sync the two
trees are identical, from whichever side you ran it. The peer's half needs its tree clean and
writable; when it is not, sync still pulls (always safe), leaves the peer's store **whole**
rather than half-fed, and says so with exit 1. An `http://` remote is pull-only — any static
file tree serving a `.seed/` is a complete remote, and it takes no push.

**Refs travel too.** A ref is a single file rather than a content-addressed one, so sync
**unions it by name** instead of gap-filling: the same name at the same head is idempotent, and
the same name at *different* heads is a human error (a banked name is immutable, and neither
nest may repoint the other's), so **each side keeps its own** and the clash is reported once.
That is what makes a release the unit you propagate rather than a local bookmark.

A *convergent* write (two nests reach the same content) is silent. Same-path divergence
**merges**: the incoming hunk names the content hash it expected, so the common ancestor is
already in the store and the three sides go to a diff3 line merge
([`crew/seed/merge.l`](../crew/seed/merge.l)) — disjoint edits to one file both survive, and
only a true overlap lands in `<<<<<<<` markers naming both patches, whereupon sync reports and
exits 1. The resolution is an ordinary `record`, so no new verb: the fix is a patch like any
other, and it settles the clash for good. A delete meeting an edit, or a binary file, cannot
line-merge — those keep the **content** (never the deletion), name both blobs, and flag.

### apply

`apply [ID..]` realizes a dependency-consistent **subset** of the store into the working tree.
This is git's `checkout` *and* its `cherry-pick`, which are one act here rather than two: the
tree is a pure function of a patch **set**, so there is no replay-a-diff-onto-a-foreign-state
step, and so nothing for that step to conflict on. An ID may be a prefix (what `log` prints). A
named patch drags its **dep closure** along, because a patch may not travel without the patches
that wrote its context — leaving one behind would silently realize less than you asked for,
since `topo` only readies a patch whose deps are all present. With no ID it realizes the whole
store again: the way back. It is a **view** — the store never shrinks, and the next `sync`
re-derives the union. To drop a patch for good you `undo` it, growing an inverse rather than
forgetting.

### undo

`undo ID [NOTE]` adds the **inverse** patch. Removal is growth here, never deletion: the patch
stays, its dependents stay valid, and the store only ever gets bigger — which is exactly what
keeps releases inclusion-ordered and "upgrade = move to a superset" well-defined. So it is
git's `revert`, never its `reset`. It works through the working tree and hands off to `record`,
so the inverse is a patch like any other and needs no special case downstream. A path that has
**moved on** since is three-way merged rather than clobbered — base is what the patch wrote,
ours is what the path holds now, theirs is what it replaced — so later edits survive and only
that patch's write is lifted out. A true overlap lands in markers and records **nothing**:
resolve, then record.

### bank

`bank NAME` freezes the current head under a name — an immutable release, and the unit you
propagate. `apply NAME` then realizes it, since the dep closure derives the whole patch set
from the tips. Re-banking a name at the same head is a no-op; at a different head it refuses,
because a banked name is immutable. `log` shows each ref with its **psid** — `sha256` of the
sorted tips — the name of that release's head DAG state.

⚠ **A ref freezes the tip *set*, not a single tip.** Deps are **per path**, so a patch depends
only on what it *touched* — an independent birth is never depended upon and stays maximal
forever. Two or three tips is what ordinary parallel work looks like, not a fork to repair, and
the only way to collapse them would be to write a patch touching every path every tip touched,
i.e. to edit files to appease the check. So a release freezes the head DAG state whatever its
shape — which is exactly what `psid` hashes.

## install is a composition, not a verb

Install is `sync` + `cook install`: binaries go in the `~/.love` nest, `make install`'s layout
owns them, and the unit of distribution is a `.seed/` store any static host serves. The
composition lives *outside* seed's verb set — `love up [URL]` (crew/seed/up.l, a verb of the
dist artifact's rail, not of seed) syncs `~/.love/src` and cook-installs the nest; run again,
it is the upgrade. `love down` is its twin — the nest and its `~/.local` links removed, with
seed refusing for unrecorded work in `~/.love/src`. The build's CC defaults to the artifact's
own `mooncc` verb, so the download door carries its whole toolchain; an explicit env `CC` (the
git door's leg) always wins, and the embedding goods (liblove, love.h) are the cc lane's, so up
installs with `EMBED=0`. Gate: test/gate/dist.sh (`make test_up`).

That install stays a composition rather than an irreducible verb is the design rule in force:
**design the vcs primitives plus one derivation verb, and let the distro front-doors be named
compositions of those.** `clone` = `sync` from empty; `install` and `upgrade` are one
composition. The smell to watch for is "install" or "upgrade" turning back into a verb.

## why seed (the metaphor earns the invariants)

`seed` is not decoration; a seed vault names the model's two hardest invariants more accurately
than "tree" or "reef" would.

- **Append-only cold storage is the inverse-patch law.** The core discipline is "removal is an
  inverse patch, never a deletion; the patch set only ever grows," which is what makes the
  default channel R₀ ⊆ R₁ ⊆ R₂ … well-defined (⊆ total). That is a seed vault's literal
  operating principle: Svalbard never withdraws and discards, it only ever accepts more, and a
  depositor retains what they put in. Even a rollback is a new deposit, never an erasure.
- **Distribution *is* cloning.** A seed vault exists for exactly that: it is the duplicate
  backup the world's genebanks restore *from*, and a restore is not a special operation — it is
  the same exchange running the other way.
- **What is stored is not what runs.** The vault holds **germplasm**, not plants; the germ has
  to be taken somewhere and grown before it is a living thing. Source is content-addressed by
  the patch set; the native binary is a *derivation* of `(patch set, arch)` and cannot live in
  the vcs as a patch. The metaphor makes the one type error we must not commit obvious on sight.

The vocabulary comes with it rather than being invented for it: what a vault keeps is a
**germ**, what it does to one is a **viability test**, and to freeze a release is to **bank**
it — one word for putting a thing somewhere safe and for the institution that keeps it. The one
thing traded is that `tree` read as "version control" on sight; `seed` leans on the persona to
carry that, and since the model isn't a tree, that's the right trade.

`hatch` is not a plant word, deliberately: egg / hatch / `born` is love's own bootstrap cluster,
and the installer *re-runs the hatch* on your machine. The vault half is `sync`; `hatch` is the
bootstrap half; install is the two composed.

## open

- Does a nest need an explicit `pick`/`use` to switch its live ref, or is that just
  `apply <ref>`? Leaning fold-into-`apply`, skip the verb.
- The command surface: bare `seed <url>` as install, or a front-of-house alias.
- `spin` is **not available** as a verb flavor — love.c registers a nif under that string and
  the egg mops the nom, so it reads free on the book while the table entry stands.

## where it lives

`crew/seed/` + `lib/seed/` (the holo/kore all-the-way-down precedent). The dock cluster
([`port/inle/`](../port/inle/)) does the hard half — apply a patch, gate it (rebuild +
`make test`, red reverts), adopt it (re-exec onto the new generation); `sync`/`record` are the
DAG surface over the same store.
