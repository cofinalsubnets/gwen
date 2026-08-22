# the image chain: what a second image costs

`.love_image` carries an ARRAY of images (host/image.c, doc/misc/snapshot.md). The array
worked and it was not free: the sizes ADDED.

```
docs.image   1.8 MB     lint salt infix lapiz libra -- what `love libra ..` needs
full.image   7.2 MB     the whole crew
binary      13.2 MB     against 11.9 with the full image alone
```

The arc was to make the second image cost its DIFFERENCE. **Landed**: the docs rung is
now 14,800 bytes of patches, the binary is 12.0 MB, and `love libra` still starts in
23 ms against the full image's 107.

## what was actually duplicated

Not the dictionary. Each image is `{header, dictionary, token stream}` and `ImageNDict`
is 248 words -- 2 KB, and sharing it would have saved 2 KB.

The CONTENT was the duplication that cost: every object in the docs image is also in the
full one. But the two blobs shared almost no bytes, for two reasons, and both had to go:

* each image was dumped from its own `gen_major` compaction, so a shared object landed at
  a DIFFERENT offset in each, and the offset is what the encoding stores.
* the lane encodings were `hb`-relative (`hb` = the blob's own byte length), so even an
  object at the same offset encoded its ap differently in a bigger image.

## the shape that landed

Make the small image a PREFIX of the large one, in the one place where that is
achievable: a single bake process that builds the layers in inclusion order.

```
love bake -L docs-cat:libra,help -L rest-cat
  boot -> eval docs-cat -> FREEZE -> eval rest-cat -> save + derive
```

Three pieces:

1. **a pinned prefix in the major collection** (`g->froze`). Those words are memcpy'd
   into the to-space at the same offsets and scanned in place, never traced-and-copied,
   so frozen objects keep their offsets for the rest of the process. Inert when
   `froze == 0`, which is every session that is not a layered bake. The cost is that the
   frozen closure becomes immortal -- right for a bake, wrong for a session.

2. **a FIXED lane floor** (`ImageIdxBase`, 1 TB on 64-bit / 128 MB on 32-bit). The lanes
   used to start where the heap ended; now they start at a constant, so the encoding is a
   pure function of the heap and one blob can begin with another. A dump of a heap past
   the floor is refused rather than aliased.

3. **derived entries in the container** (`kind`/`base` in `struct image_ent`). The
   largest entry is stored whole; a smaller one is stored as its own header plus the
   words of the prefix that were MUTATED after its freeze. The loader expands the first
   `nwords` words of the parent's stream with the parent's dictionary, applies the
   patches, and decodes.

⚠ **the re-seating dead end.** The first cut kept the `hb`-relative lanes and re-seated
the baseline's words by adding `hb_full - hb_base` to every even word above the base.
That is arithmetically right for lanes and catastrophic in general: a string's payload
rides RAW, so an even 64-bit run of text is indistinguishable from an encoded pointer and
got 0x20050 added to it. It showed up as 2,926 spurious patches whose diffs were all
exactly the delta, and a derived image that woke into a session missing half its book.
No rule downstream of the encoder can tell a lane from a byte -- which is why the floor
has to be constant rather than translated.

## the cost it carries

The collector gained two branches on its hot path -- one in `gcp` (every pointer word),
one in `evac_thread` -- both testing `g->froze_lo`, which is 0 in every session that is
not a layered bake. Measured, not assumed: 0.53%. See "what is left".

## what it paid

```
                       whole images        chain
docs rung                 1.8 MB         14,800 B     the derived record
full image                7.2 MB          7.4 MB      (+73 KB: the pin holds some dead objects)
binary                   13.2 MB         12.0 MB
love libra doc FILE        23 ms           23 ms      unchanged: expanding N words costs
love -e 1                 107 ms          107 ms      the same from a long stream's head
```

The patch count is the number the design rests on and it came in at ~900 words of a
880,000-word prefix -- a tenth of a percent. A third and fourth rung are now nearly free.

## what gates it

`test_dist` (in test_slow) proves the derived entry wakes and lifts the same header as the
whole one. `test_imgchain` (test_extra, ~40 s) is the pin's own gate: nothing but
`bake -L` sets `g->froze`, so the branch in `gcp`, the verbatim block in `gen_major` and
the terminator fixup in `evac_thread` are dead code in every other lane -- test_gcstress
included. It bakes three layers under AiGcStress, where every allocation collects and a
major rides every 32nd, then wakes each entry. The size cap is what bites: with the pin
disabled a derived record goes from ~600 bytes to 10 MB.

## the bug this uncovered

`img_build` calls `gen_major` DIRECTLY, and a major leaves the remembered set full of
addresses into the half it just abandoned. Before layers that never mattered: the bake
was the last thing a process did. `ai_image_freeze` runs one mid-session, and the next
minor walked the stale set and died in it. The clear now lives in `gen_major` itself,
where a promote-everything collection makes it true by construction. The `(bake path)`
nif -- a mid-eval dump whose session runs on -- always had the same hole.

## what is left

* it is a CHAIN, not a general lattice. The pin freezes a contiguous prefix, so layer k
  must contain layer k-1; two images that do not contain each other cannot both be
  prefixes of the full one. They still work -- you order them and each carries everything
  below it -- but the lower one stops being minimal.
* a third rung, on demand: the CLI and the container take up to 8 layers, and three are
  gated (test/gate/imgchain.sh). The derived record for each is computed against the same
  final blob.
* the cross-arch artifact still bakes a single whole image (`crew/build.mk`'s x-lane); it
  has no second rung to want yet.
* `g->image_why` reports a refusal by number. If a bake ever fails in the field the
  number is what says which check bit -- give them names if that happens twice.
* the cost of the pin is 0.53% of instructions on an allocate-and-collect loop, measured
  against a twin built with the two hot-path branches compiled out (12.672e9 vs 12.606e9,
  ±8k across runs). Real work pays less, since that load does nothing but churn the heap.
  If it ever matters, the `gcp` test can be hoisted to once per collection -- at the cost
  of a second scan loop -- or compiled out outside the bake.
