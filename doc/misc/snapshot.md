# the heap-image snapshot

love boots from a **serialized heap image** rather than by evaluating the corpus. A plain `love`
wakes the image baked into its own `.image` section (found via `selfpath`) and starts a
glaze-baked runtime in **~4–12 ms** instead of the ~230 ms an egg eval costs — the native JIT is
always on, no flags. `LOVE_NO_IMAGE` opts out; the gates whose subject is the fresh egg set it
themselves, and the bench controls the glaze with it. A bad, stale or cross-arch
image makes `image_load` answer NULL and the binary boots the ordinary way: never wrong, only
slower.

Three payoffs, in order:

1. **cold start** for the whole runtime — every script run, every repl, every bench wall-clock.
2. **the glaze bake is free.** Adding `love/glaze/emit.l`+`auto.l` to the boot corpus costs
   ~+810 ms when eval'd at startup. Inside a snapshot it is precompiled: always-on transparent
   JIT at zero startup cost, which is what makes the bake worth having at all.
3. **no GC-footprint tax.** The image lives in an out-of-pool immortal region, so the moving
   collector never copies the egg/glaze closures.

## the format

The heap is a two-space copying arena; every object's first word is its `ap` (a live external
reference: a C `lvm_*` pointer or another heap pointer); fixnums are odd-tagged, heap pointers
even. The blob is everything reachable from the root, **self-describing** — there are no
relocation tables. Every pointer-bearing word is RANGE-encoded in place:

- **heap pointer** → a byte offset into the blob;
- **an `lvm_` ap** → `hb + 2·idx`;
- **an immortal** → `hb + 2·NLVM + 2·ii`;
- **a native's code** → `CodeBase + 2·offset`, its place in the code segment;
- **a binary pointer** → absolute, ≥ TBOUND.

Even-vs-odd separates pointer from fixnum, so the load re-derives relocation by re-walking.
Thread sizing at load scans the encoded terminator (`off·8+2`, unique since object starts are
8-aligned), not `ttag`.

File = header + dictionary + **token stream** + the **code segment**: the live natives' blobs
packed in walk order, mapped executable at wake as a chunk of the code arena before the decode
walk names them. A blob holds no address of the binary — the kind sentinels and the callout
drives it needs are read off `g->jk` — so the bytes are the same under any base. The encoded words are wildly repetitive — half an
image is 25 distinct words, and the commonest single one is `lvm_chain`'s index at 23%, the `ap`
every pair wears — so each rides as one byte naming one of the 248 commonest, or as an escape
naming its own width. 3.8x off the file; the wake expands into the pool and then decodes there in
place. The dictionary is chosen by COUNT and not by lane: a fixed token budget per lane is the
obvious thing and it is also worse (3.63x), because the split would be tuned to whichever image
was measured, and the counts follow a kernel's image or an artifact's wherever those go.

⚠ **NULL is an immortal.** A live bio port carries undressed `rbuf`/`wbuf` zero words; a raw
zero is even and below the index bound, so it needs its own slot in `image_immortals`.

## the stamp is a DISTANCE

Not a build hash: `arch` (a compile-time tag) + `anchor`, the **gap** between `ai_image_save` and
`image_immortals`. A different binary — cross-arch, or a stale rebuild — lays symbols out
differently, the gap changes, and the image is refused.

⚠ it was two addresses, compared to check their ASLR deltas agreed — which is that gap being
preserved, said the long way round and at the price of writing the baker's mmap base into every
image. Two bakes of one tree then differed there and nowhere else. The distance discriminates
exactly as well and is the same number every run; `test_bakerep` is what holds it.

## the section carries an ARRAY

`.image` holds one image; its first word is the codec's own magic, and every verb wakes it
whole. (The layered directory — per-verb entries, derived records, `bake -L` — was built,
measured, and retired 2026-08-24: the shell's fork lane made the per-stage wake it existed
to shave disappear, and one plain image is simpler everywhere.)

## the section is GROWN, not reserved

`.image` is laid LAST — alone in the highest `PT_LOAD`, above `.bss`
(`-Wl,--section-start=.image=0x2000000`, src/build.mk) — so the bake APPENDS the blob at the
first page past every other allocated byte and rewrites the one phdr and one shdr that name it,
relaying the non-allocated tail (symtab/strtab/shstrtab) after it. No vaddr moves, so the
two-anchor stamp holds by construction. This is why there is no fixed reserve to bump whenever
the image outgrows it, and no shipped zeros.

It has to stay a real allocated section rather than loose bytes at EOF: `strip` (which
`install -s` runs) keeps the section and drops a bare trailer.

The rule `src/image.c` checks is only that **`.image` ENDS the segment carrying it**, which
covers both shapes with the same arithmetic: a section alone in the highest `PT_LOAD` (ld/lld)
and one riding the tail of the single segment holo lays. It reads that off the binary's own
section headers rather than a build flag, so neither lane is told which it is, and a link that
lays `.image` anywhere else is refused loudly with the flag it wants.

For the mooncc lane the bytes are a real section too: `.image` is a fourth stream through
cgdata → objelf → the `image` lane in link.l, beside `love_nifs` — the other named section whose
whole point is WHERE it lands.

## core/host split

The core owns the stdio-free buffer codec `ai_image_save` / `ai_image_load` (love.h); file I/O
lives in `src/image.c`. The codec sits OUTSIDE the one `#if __STDC_HOSTED__` region, so it
compiles into the freestanding kernel.

## `bake` and `wake`

`love bake` boots fully and lays the image into the binary's own `.image` section;
`love bake PATH` writes a plain file instead. `love wake PATH prog.l args..` boots from a
named image.

**`love-image` says which one woke.** The wake strips the path from `argv`, so a session that
must key on the identity of the compiler it is running (mooncc's runtime cache) can ask no other
way; the value is the path, or `"<baked>"` for the binary's own section. It is pinned **only
when a session actually woke one** — absence is the answer for an egg boot, asked out of band
with `(member? 'love-image (names ()))`.

⚠ **Read it as `(ev 'love-image)`, never bare.** A baked consumer folds its bare globals at its
own compile, and the bakes all egg-boot, so a straight read wires that session's answer — a `0` —
into the image forever. The nom has to reach the lookup as *data*. `cmdline` and `argv` answer the
same law from the other side: a bake pins neither, so their bare reads cannot fold either.

The glaze bake is the corpus eval, not a split assert-free lib: `bake` evals the glaze
(emit.l+auto.l) before dumping, and the asserts' transient natives die in `gen_major`. emit.l's
self-test fixtures are local (they would otherwise leak as globals); what survives, `memo`'s
natives included, rides the image as code.

## the live bake

`(bake "x.image")` snapshots the RUNNING session to an image file, mid-eval — no quiescent point
required — and answers 1 | (); the session rides on. The woken book carries every global pinned
before the bake, so an app loaded warm (`love -l app -e '(bake "app.image")'`) never pays its
load again: the mooncc image takes `mooncc -c love.c` from ~3.7 s to ~2.4 s, the whole per-run
load tax.

Three seams make mid-eval dumping honest where the boot bake could assume purity:

- **The stack is ballast, not state.** The running continuation's objects get traced (they're
  live) and ride into the blob; the load side resets `sp` and re-establishes `ip` regardless, so
  they are wake-unreachable garbage swept at the woken session's first major. `ai_image_save_`
  (the unguarded worker) does the dump; `ai_image_save` keeps the empty-stack guard for the boot
  path, where a non-quiescent dump is a bug.
- **Live finalizer nodes forge into dead chains.** An open port's close (or a nat's unmap) is a
  raw three-word `ai_fz` in the heap — no object header, so the blind walks (save's encode and
  load's decode) cannot stride it. The save walk recognizes the `g->fz` chain and overwrites each
  node's BLOB copy with a `(() . ())` chain of the same width; `fz` lives outside the serialized
  `v0..end` root window, so the woken session starts with no finalizables. The dump-time fds
  meant nothing in the new process anyway.
- **Natives ride.** A live native closure's cell names its code by the code rung, and the
  blob is bytes in the segment; the woken session runs it without a compile.

Smoke: test/host/bake.l (`test_hostnif`) round-trips a pinned marker through `bake` + `wake`
in a child process.

## the dump hash-conses

A chain's fields are immutable by convention, not by structure: `poke` writes whatever cell it is
handed, and c0 patches a cons in five places (`gen_wb_two`). Each of those patches a spine c0
consed during the compile running it — young, held by nobody — so no chain the bake can reach is
ever written, and two structurally equal ones are one value wearing two addresses. An image that
is mostly source AST holds a great many.
`img_hashcons` runs between the compaction and the encode — walk, merge, compact again — and the
crew image loses **35.0%** of its words (3,689,068 → 2,398,817), the wire **36.0%** (8,428 → 5,390
KB), the binary **24.2%** (12,517,064 → 9,483,528 B). The stream keeps the whole raw saving rather
than having been quietly paying for the redundancy already.

Resident size does not move by itself: the major pool is sized off the image with headroom, so a
smaller live set lands in the same pair. Wake first got SLOWER, 43.1 → 54.0 ms, and the decode is
not why — it roughly halves with the words. The woken session ran one major collection during boot
that the larger image did not, because `img_wake` seeded the nursery at `nw >> 1` while the pool
carried `nw >> 2` of slack, making `gen_please`'s `major_free < g->len` true by construction. A
boot's allocation is a fixed cost and does not shrink with the live set, so the smaller nursery no
longer swallowed it. The slack now clears the seeded nursery (`nw + (nw >> 1)`, and a floor for
the small end), and the wake clears the remembered set it invalidates — a forced first major had
been supplying that clear as a side effect. Together the wake is 30.7 ms, peak RSS 37 MB, and the
boot's one collection is a minor rather than a major over 2.4M words.

`=` is structural already and the printer spells binders `d0 d1 …` either way, so `id?` is the one
witness: `(id? '(1 2) '(1 2))` written at two sites answers 1 after a bake where it answered 0.
emit.l's quote pool indexes by `id?` (`amemq`/`aposq`) on the ground that the interpreter's quote
answers the source node itself — still true, and the merge is over before the glaze compiles that
source, so pool and interpreter agree on the one surviving node.

Three things carry it:

- **Bottom-up, so the hash decides nothing.** Both children are canonical before their parent is
  looked up, so a candidate compares by POINTER on both fields and no collision can merge
  unequals. The walk rides an explicit stack — a long list is a deep chain, and the recursion that
  shape asks for is the one a dump cannot afford. A child still in progress is a cycle,
  unreachable for a chain, and leaves its ring unmerged rather than guessed at.
- **A string is opt-in.** The non-code thread aps are a closed roster (`image_extra_aps`), and of
  them only a cask's payload and a port's buffers are memcpy'd through in place; every other slot
  in the heap replaces a POINTER and never a byte. Those two pin their strings, and the stack pins
  what it is still filling. ⚠ a byte-writable holder added to that roster has to be added there.
- **Roots are not rewritten.** A duplicate the stack still names simply survives — a few words, and
  a mid-eval bake's continuation keeps its values identical.

The pass is a pure function of the heap, which `test_bakerep` and `love seed`'s fixpoint both
hold it to.

## open

1. **A kernel-loadable image.** The codec is buffer-based and stdio-free, so it compiles into the
   freestanding kernel, and the kernel runs the generational collector bounded by `g->budget`,
   which the codec needs (it relocates into the major pool). What is left is that a host-dumped
   image will not load in the kernel: the arch+anchor stamp rejects a different binary (different
   `lvm_*` table order, different binary-pointer addresses). Options: dump from the KERNEL binary
   (unexec — boot in qemu, serialize, emit the bytes over serial, `objcopy` into the kernel,
   RELINK with image.o placed LAST so .text/.rodata addresses do not shift — the kernel is
   non-PIE, so absolute pointers stay valid only if nothing moves); or a layout-stable shared TU.
   Resolves cold start on the MCU too.

Relates: (the collector and the immortal region) (the bake's
codegen).
