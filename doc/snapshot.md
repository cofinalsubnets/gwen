# the heap-image snapshot

love boots from a **serialized heap image** rather than by evaluating the corpus. A plain `love`
wakes the image baked into its own `.image` section (found via `/proc/self/exe`) and starts a
glaze-baked runtime in **~4–12 ms** instead of the ~230 ms an egg eval costs — the native JIT is
always on, no flags. `LOVE_NO_IMAGE` opts out; the Makefile exports it for all recipes so the
gate tests the fresh egg and the bench controls the glaze itself. A bad, stale or cross-arch
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
- **a binary pointer** → absolute, ≥ TBOUND.

Even-vs-odd separates pointer from fixnum, so the load re-derives relocation by re-walking. File
= header + heap. Thread sizing at load scans the encoded terminator (`off·8+2`, unique since
object starts are 8-aligned), not `ttag`.

⚠ **NULL is an immortal.** A live bio port carries undressed `rbuf`/`wbuf` zero words; a raw
zero is even and below the index bound, so it needs its own slot in `image_immortals`.

## the stamp is two-anchor

Not a build hash: `arch` (a compile-time tag) + `anchor` (the dump-time address of
`ai_image_save`). Under ASLR the whole binary shifts by one base delta, so the immortals/refsym
delta must equal the `ai_image_save`/anchor delta. A different binary — cross-arch, or a stale
rebuild — lays symbols out differently, the deltas disagree, and the image is refused.

## the section is GROWN, not reserved

`.image` is laid LAST — alone in the highest `PT_LOAD`, above `.bss`
(`-Wl,--section-start=.image=0x2000000`, host/build.mk) — so the bake APPENDS the blob at the
first page past every other allocated byte and rewrites the one phdr and one shdr that name it,
relaying the non-allocated tail (symtab/strtab/shstrtab) after it. No vaddr moves, so the
two-anchor stamp holds by construction. This is why there is no fixed reserve to bump whenever
the image outgrows it, and no shipped zeros.

It has to stay a real allocated section rather than loose bytes at EOF: `strip` (which
`install -s` runs) keeps the section and drops a bare trailer.

The rule `host/image.c` checks is only that **`.image` ENDS the segment carrying it**, which
covers both shapes with the same arithmetic: a section alone in the highest `PT_LOAD` (ld/lld)
and one riding the tail of the single segment holo lays. It reads that off the binary's own
section headers rather than a build flag, so neither lane is told which it is, and a link that
lays `.image` anywhere else is refused loudly with the flag it wants.

For the mooncc lane the bytes are a real section too: `.image` is a fourth stream through
cgdata → objelf → the `image` lane in link.l, beside `ai_nifs` — the other named section whose
whole point is WHERE it lands.

⚠ There is no `__APPLE__` lane: `image.c` needs `<link.h>` + `dl_iterate_phdr`, so it does not
build on mach-o at all. A mac host owes it `_NSGetExecutablePath`.

## core/host split

The core owns the stdio-free buffer codec `ai_image_save` / `ai_image_load` (love.h); file I/O
lives in `host/image.c`. The codec sits OUTSIDE the one `#if __STDC_HOSTED__` region, so it
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
into the image forever. The nom has to reach the lookup as *data*. This is the same law that
keeps `cmdline` travelling to a baked app through the `-e` string rather than off the book.

The glaze bake is the corpus eval, not a split assert-free lib: `bake` evals the glaze
(emit.l+auto.l) before dumping, and the asserts' transient natives die in `gen_major`. emit.l's
self-test fixtures are local (they would otherwise leak as globals) and auto.l's `memo` cache is
cleared pre-dump.

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
- **The glaze cache is emptied first.** The `bake` global is a glaze wrapper
  (love/glaze/hook.l) over the host nif (host/image.c, the AI_NIF glob): a native closure cannot
  serialize, and entries re-JIT lazily in the woken session. Any OTHER live native at bake time
  is on the caller — the same contract as the boot bake.

Smoke: test/host/bake.l (`test_hostnif`) round-trips a pinned marker through `bake` + `wake`
in a child process.

## open

1. **Chain dedup at dump time is UNSOUND as things stand.** The compacted image is ~71% source-AST
   chains, and hash-consing structurally-equal sub-trees shrinks it by about a third — but the
   glaze reads source by IDENTITY, so merging two structurally-equal cells changes its native
   codegen. The fix is upstream: make the glaze key its CSE/codegen on structural equality
   (`=`), not cell `id?`, so `=`-equal sub-expressions are already one; then a union-find
   hash-cons over chain starts can land safely. Scope: audit emit.l/auto.l for `id?` on source
   sub-terms.
2. **A kernel-loadable image.** The codec is buffer-based and stdio-free, so it compiles into the
   freestanding kernel, and the kernel runs the generational collector bounded by `g->budget`,
   which the codec needs (it relocates into the major pool). What is left is that a host-dumped
   image will not load in the kernel: the arch+anchor stamp rejects a different binary (different
   `lvm_*` table order, different binary-pointer addresses). Options: dump from the KERNEL binary
   (unexec — boot in qemu, serialize, emit the bytes over serial, `objcopy` into the kernel,
   RELINK with image.o placed LAST so .text/.rodata addresses do not shift — the kernel is
   non-PIE, so absolute pointers stay valid only if nothing moves); or a layout-stable shared TU.
   Resolves cold start on the MCU too.

Relates: doc/gengc.md (the collector and the immortal region), doc/glaze-arm64.md (the bake's
codegen).
