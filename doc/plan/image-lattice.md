# the image lattice: what a second image should cost

`.love_image` carries an ARRAY of images and `love bake -a` lays it (host/image.c,
doc/snapshot.md). The array works and it is not free: today the sizes ADD.

```
docs.image   1.8 MB     lint salt infix lapiz libra -- what `love libra ..` needs
full.image   7.2 MB     the whole crew
binary      13.2 MB     against 11.9 with the full image alone
```

The arc is to make the second image cost its DIFFERENCE, so a third and a fourth rung
are nearly free and the lattice can grow to fit the verbs rather than the budget.

## what is actually duplicated

Not the dictionary. Each image is `{header, dictionary, token stream}` and
`ImageNDict` is 248 words -- 2 KB, and sharing it would save 2 KB.

The CONTENT is the duplication that costs: every object in the docs image is also in
the full one. But the two blobs share almost no bytes, and the reason is worth stating
because it decides the whole design:

* each image is dumped from its own `gen_major` compaction, so a shared object lands at
  a DIFFERENT offset in each, and the offset is what the encoding stores.
* the lane encodings are `hb`-relative (`hb` = the blob's own byte length), so even an
  object at the same offset encodes its ap differently in a bigger image.

So a byte-level delta between the two files finds nothing, and no amount of compression
finds it either. To dedup we have to make the sharing TRUE first, then store it.

## the shape

Make the small image a PREFIX of the large one, in the one place where that is
achievable: a single bake process that builds the layers in inclusion order.

```
boot -> eval docs-cat -> FREEZE -> eval the rest -> save
```

Three pieces:

1. **a pinned prefix in the major collection.** `g->froze` words at `major_base` are
   copied VERBATIM into the to-space at the same offsets and scanned in place, never
   traced-and-copied. Frozen objects keep their offsets for the rest of the process, so
   a later dump's blob starts with the earlier dump's. Inert when `froze == 0`, which
   is every session that is not a layered bake. The cost is that the frozen closure
   becomes immortal -- correct for a bake, wrong for a session, which is why nothing
   but the bake sets it.

2. **`encbase` in the header.** The decoder derives `hb` from the blob's own length
   today; a derived image is a PREFIX of a longer blob and must be decoded against the
   longer one's `hb`. One header field carries it; 0 keeps the old reading.

3. **derived entries in the container.** The array's largest entry is stored whole; a
   smaller one is stored as `{parent, nwords, patches}` -- its own header (its own
   roots, captured at the freeze) plus the words of the frozen prefix that were MUTATED
   after it. The loader expands the first `nwords` words of the parent's stream with the
   parent's dictionary, applies the patches, and decodes with `encbase`.

The wake stays the same speed: expanding N words costs the same whether the tokens came
from a short stream or the head of a long one.

## what it should pay

The docs entry falls from 1.8 MB to a header plus its patch list, so the binary should
land near where it was with one image (11.9 MB) while keeping the 5.5x fast path. The
patch count is the number that decides it and it is not knowable ahead of the
measurement -- pinned tablets (the verb table, the module registry) are mutated after
the freeze and every mutated word is a patch.

⚠ if the patch list turns out to be a large fraction of the prefix, this design is
wrong and the honest answer is to say so and keep the two whole images. Measure before
believing.

## rungs

0. measure: today's two blobs share ~nothing. (stated above; the reason is the design.)
1. `encbase`, and the freeze in the collector. Gate: `test_bakerep` still says two bakes
   of one binary are the same bytes, and a frozen session still collects correctly.
2. the freeze/derive codec doors, the derived container entry, and the loader.
3. `love bake -L CAT:verbs ..` -- one process, N layers -- and the dist wired to it.
4. measure again: binary size, patch count, and the wake of both entries.
