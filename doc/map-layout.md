# the map's collision strategy — why open addressing stays

Open addressing is the right fit for love's load factor. This records the alternatives, the
numbers, and the why, so the question does not get re-litigated whenever the `hash` bench lands
behind node (0.76) / pypy (0.44) at love's ~1.07 ms/it.

## what love does

The map is **open-addressed, linear-probe**, with `(key, value)` slots **interleaved** in one
backing: `[lvm_map_data, len, cap, k0,v0, k1,v1, …]` (love.c, `map_slots`/`map_probe`/`ai_mapput`).
The hash is multiplicative — `rot(x * mix)`, `mix = 0x9e3779b97f4a7c15`, `rot` a half-word rotate —
masked to the capacity. It grows (doubling + rehash) at the **0.75 load factor**:
`(len+1)*4 >= cap*3`, which is also what lets an empty slot terminate every probe. Empty slots hold
`map_gap`, a unique out-of-pool address; delete backward-shifts the probe chain, so there are no
tombstones. The header never moves on growth — only `header[1]` (the backing) is swapped — so
aliased references (ev's scopes) see later inserts. The glaze compiles the probe to native
(`mpeep`/`mpin`, love/glaze/auto.l) for the scan/bump/insert loops. With `(tablet n)` presizing the
backing, the `hash` bench's 10000 sparse inserts never rehash.

## the access pattern measured

The `hash` bench, faithfully: into a presized table (cap 16384 for N=10000, ~61% load), **insert**
N keys `1+97*i` (sparse, so they actually hash), a **sum-scan**, a read-modify-write **bump**, a
second sum-scan. Same hash, same tagged keys (`2k+1`, odd; gap = even), pure C `-O2` so the numbers
are the **probe/cache cost** — what the native lane pays — not love's eval overhead.

## the candidates and the numbers

Read/update passes (scan+bump+scan) on a pre-built structure, ms/it, warm runs:

| design | ms/it | vs open addressing |
|---|---|---|
| **A — open addressing, interleaved (k,v)** *(what love does)* | 0.029 | baseline |
| **B — cdr-chaining, scattered** (insertion order, the write-burst reality) | 0.034 | **0.85×** (−15%) |
| **C — cdr-chaining, compacted** (per-bucket contiguous runs, post-GC ideal) | 0.072 | **0.38×** (−60%) |
| **split arrays** `keys[]` / `vals[]` (full workload, separate bench) | — | **0.94×** (−6%) |

Every alternative **loses**. Two distinct reasons:

- **split arrays** (probe a dense `keys[]`, value in a parallel `vals[]`): the denser key-probe
  doesn't help — at 61% load the chains are ~1–2 steps — and every hit now pays a **second cache
  line** to reach `vals[i]`. Value co-location wins. The bench does 3 lookup passes, so that second
  fetch lands 30000 times.

- **chaining** (cdr-coded or not): every lookup pays an **extra indirection** — hash → bucket-head
  array → entries — *before* it can touch data. Open addressing hashes **straight to the slot**.
  At short chains, that one head-pointer hop costs more than the clustering chaining avoids.
  Compacting the chains (cdr-coding: contiguous runs, no next-pointers) fixes *traversal* — but
  traversal is already ~1 step here, so it buys nothing while the indirection remains (and the C
  variant's two metadata arrays made it worse; even an ideal single-array head wouldn't beat A).

## the cdr-coding angle (clean here — just not a win at this load)

cdr-coding's general blocker is **shared tails**: the copying GC can't lay two lists' shared tail
out contiguously without breaking the sharing. A hash table **owns the only pointers to its
chains** — no entry is reachable from two buckets — so the collector is free to cdr-code/compact
them, which is why C (compacted) is reliably *achievable* post-GC, not just hypothetical: the
moving collector walks the live set anyway, so compacting the chains is ~free, not an extra O(n)
pass. **It's still not worth it**, because the win it unlocks (no clustering, sequential per-bucket
scan) only matters at **high load with long chains**, and love caps load at 0.75 — chains never get
long. The structural cost (bucket indirection, allocate-per-insert vs in-place store) is paid at
*every* load.

**Sorted chains** (keep each bucket ordered by the total order) are a further refinement on top of
chaining — early-exit on a MISS, or binary search in a compacted run. Same verdict, more so: the
payoff scales with chain length and only helps misses (the bench's scans are all hits), while
maintaining sorted order taxes the write path (insert into the middle of a compacted run, O(chain),
vs O(1) prepend). Worth it for a high-load, miss-heavy membership set — not for love's regime.

## where each WOULD win

Not academic — just not love's operating point:

- **chaining / cdr-compacted**: high load factor (≥0.9), where open addressing's clustering
  explodes and per-bucket isolation + sequential traversal pays off.
- **sorted chains**: the above + miss-heavy lookups (membership filters).
- **split arrays**: probe-heavy workloads that rarely read the value (e.g. `tally`/`keys`-style
  passes), or very long chains where touching skipped values wastes cache.

love runs none of these: 0.75 cap, value-co-located lookups, short chains.

## so where IS the gap to node/pypy?

**Not the collision strategy.** It's a different axis:

1. **JIT specialization** — pypy's tracing JIT specializes the *whole* int-keyed loop (inline hash,
   hoist, drop bounds checks); for small ints `hash(n)=n`. love's glaze compiles the loop but the
   probe stays a generic open-addressed scan.
2. **the compact/ordered dict** (CPython 3.6+ / V8) — a tiny **index array** (1–2 bytes/slot,
   cache-resident) into a **dense entries array**. The cache win is on the *index probe*, and the
   dense entries also give cheap **ordered iteration** (`keys`/`$`). That's a real structure win,
   but it's orthogonal to open-vs-chained, it complicates **delete** (tombstones + compact-on-
   resize vs love's clean backward-shift) and it can **regress the common small-map case** (the
   `book`, tablets) with its two-array indirection. Weighed on its own merits — but it is not an
   answer to the collision-strategy question, which is settled.

## reproduce

The microbenches are pure C (`gcc -O2`), three files under `doc/proto/`: split (`maplayout.c`),
open-vs-chained full workload (`maplayout2.c`), and read-isolated (`maplayout3.c`). They model
love's hash + tagged keys + presized cap exactly; swap the layout, keep everything else fixed.
Re-run before trusting any number here — they are cache-microarchitecture-sensitive, though the
*ordering* (A < B < C, split < A) has been stable across runs.
