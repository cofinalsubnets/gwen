# Plan: the heap-image snapshot (a precompiled boot image)

Goal: **boot from a serialized heap image instead of evaluating the corpus.** Today `boot()`
(host/main.c) hands `ai_evals_` the egg+prel+ev+post+asm+bao SOURCE string and the self-hosted
compiler MATERIALIZES the whole runtime at startup — measured **~233 ms cold start** (the setup-row
bench, love 2nd-worst). The snapshot makes that ~instant: dump the post-boot heap ONCE at build time,
embed it in the binary, and at startup `mmap`+relocate it in with no eval. (LANDED — see the
Status section below; this stays as the `--bake`/`--wake` design-of-record.)

## Why — three payoffs, in priority order

1. **Cold start ~233 ms → near-zero** for the WHOLE runtime (every script run, every repl, every
   bench wall-clock). This is the standalone win; it pays off even with no glaze.
2. **The glaze bake becomes free.** Adding `love/glaze/emit.l`+`auto.l` to the boot corpus costs
   ~+810 ms today (it is ~2000 lines of love + ~50 native-compiling asserts, all eval'd at startup —
   measured). Inside a snapshot it is precompiled: **always-on transparent JIT, zero startup cost,
   like luajit** — which is exactly what the bake needs (and why the naive bake was abandoned).
3. **The GC-footprint tax goes away.** The image lives in an out-of-pool IMMORTAL region (extend
   love.c's existing "out-of-pool short-circuit ... immortal, never copied", love.c:667), so the moving
   collector never copies the egg/glaze closures — no per-collection cost from a bigger baseline.

## Mechanism — a binary heap dump + relocating load

The heap is a two-space copying arena; every object's first word is its `ap` (a live external
reference: a C `lvm_*` pointer or another heap pointer); fixnums are odd-tagged, heap pointers even.
A snapshot is a flat blob of all objects reachable from the root (`book`), with two pointer classes
rewritten so it can be re-based and re-linked in a fresh process:

- **Internal heap pointers** → stored as OFFSETS into the blob (re-based to `image_base+offset` on
  load). ASLR-safe: nothing absolute is stored.
- **C `lvm_*` pointers** (the aps/hots/nifs — `lvm_chain`, `lvm_flo`, every nif entry, the C-resolved
  hooks num-ap/add/mul/help) → stored as a SYMBOLIC INDEX into a fixed table, re-resolved to the
  current `.text` address on load. The enumeration already exists: the egg `mop` (love/egg.l) walks and
  deletes every `lvm_*` nom — reuse that set as the relocation table.
- **Out-of-pool immortal constants** (`ZeroPoint`/`()`, the interned const region) → a tagged
  "resolve-to-C-const" entry, fixed up to the live const on load.

Load is a single linear pass over the blob (relocate offsets + re-resolve the lvm_* table), then
`pin book` from the image root. Far cheaper than eval (a memcpy + a fixup walk, not a compile).

## Phases (each: deliverable · gate · go/no-go)

**Phase 0 — SPIKE: prove the round-trip (de-risk before committing).**
Two debug nifs: `(image-dump path)` walks reachable-from-`book`, writes the blob; `(image-load path)`
reads it, relocates, installs `book`. Dump after a normal boot; in a FRESH `love`, load it and run a
handful of forms (`(+ 1 2)`, a captured closure, a map lookup, a bignum, a twin). · GATE: loaded heap
== eval'd heap on those forms. · GO/NO-GO: if relocation + lvm_* re-resolution round-trips cleanly,
proceed; if the pointer graph has a kind that won't serialize (a live port/task, a W^X toast), scope
it out (snapshot is taken at a quiescent point — see Phase 4).

**Phase 1 — The serializer.** Generalize the spike dumper to EVERY kind (chain, pack/gem/twin/tray,
str, mint, nom/KNom, map/tablet, closure with its `fn_src` cell, nif, cask). Header = {root offset,
lvm_* table, arch+pointer-size+version stamp}. · GATE: dump→load→`(= the whole book)` structurally;
run test/spec.l against a loaded image == against a booted one (2693 pass).

**Phase 2 — The relocating loader + the immortal image region.** Map the blob into a dedicated
out-of-pool region the collector skips (extend `gcp`'s out-of-pool check). · GATE: a full GC after
load leaves the image intact (the image is never copied); spec green; valgrind clean (`make valg`).

**Phase 3 — Build integration.** Build step: `love --bake` (boot fully, lay the image into the binary's own .image section; `love --bake PATH` writes a plain file instead).
Embed via `objcopy`/a linked C array → `image.o`. `boot()`: if the image stamp matches this binary,
`image-load` it; else FALL BACK to eval'ing the egg (so a stale/missing image is never fatal). Image
is arch+build-specific → a Makefile dep on prel/ev/post/asm + the binary. · GATE: `out/host/love`
boots from the image; cold start measured (target <20 ms, from 233); spec+love0 green.

**Phase 4 — Bake the glaze (the payoff).** Split emit.l/auto.l's inline asserts into
`emit-test.l`/`auto-test.l` (the crew/asm/asmtest.l precedent) so the baked lib is assert-free; add the
assert-free glaze to the boot corpus BEFORE the dump, x86-64-gated. The snapshot is taken with the
glaze loaded and `ev` already rebound to `auto-ev`, but BEFORE any closure is natively compiled (no
W^X arenas to serialize — natives JIT lazily at first `ev`, as today). Remove run.sh's glazed list;
the bench just runs `love bench.l`. · GATE: test_glaze green; ALL bench checksums unchanged; cold start
still <20 ms; every bench glazes transparently.

**Phase 5 — Cross-arch + cleanup.** aarch64 host → its own image (no glaze). wasm/kernel → keep the
eval path (or their own image later). Document; update CLAUDE.md (the egg section) + the setup-row
note. The "even on an MCU, try" frontier (a per-arch aarch64 emitter so the glaze runs everywhere)
rides on this once images are per-target.

## Risks / open questions

- **lvm_* completeness** — miss one C pointer and load crashes. Mitigation: derive the table from the
  mop's lvm_* enumeration; assert at dump that every non-heap word0 is in the table.
- **Quiescent dump point** — ports/tasks/finalizers/W^X natives must NOT be live at dump. The
  assert-free glaze + dumping right after boot (before any user ev) keeps the heap to pure closures.
- **Relocation correctness per kind** — the Phase-0 spike is exactly to flush this out cheaply.
- **Maintenance** — the image rebuilds whenever prel/ev/glaze change (a Makefile dep); the stamp +
  eval fallback make a stale image safe, never wrong.
- **Existing infra to lean on** — the generational collector already RELOCATES objects and has the
  out-of-pool/immortal short-circuit (love.c:667, the gen_*_relocate machinery); a
  source-level round-trip is an independent cross-check (a dumped closure should `show`-match its
  eval'd twin).

Relates: the egg (love/egg.l, the double-sat), [[glaze-float]] (the bake this unlocks), gengc.md
(the collector + immortal region).

## Status — landed (Phases 0–4 + cross-arch + the host/core split)

The snapshot ships. A plain `love` wakes the image baked into its own .image section (laid by `make host`'s `--bake` step; found
via `/proc/self/exe`) and boots a **glaze-baked** runtime in **~4–12 ms** instead of the ~230 ms egg eval
— the native JIT is always-on, no flags. Opt out with `LOVE_NO_IMAGE` (the Makefile exports it for all
recipes so the gate tests the fresh egg and the bench controls glaze itself). A bad/stale/cross-arch image
→ `image_load` NULL → normal boot, never wrong. The bench cold-start row reflects it (love 12 ms vs egg 230).

Departures from the original plan above, worth noting:
- **No reloc tables.** The blob is self-describing: every pointer-bearing word is RANGE-encoded in place
  (heap → byte offset; lvm ap → `hb+2·idx`; immortal → `hb+2·NLVM+2·ii`; binary → absolute ≥ TBOUND),
  even-vs-odd separates pointer from fixnum, so the load re-derives relocation by re-walking. File =
  header + heap (~2.5 MB, was 6.2 MB with tables). Thread sizing at load scans the encoded terminator
  (`off·8+2`, unique since object starts are 8-aligned), not `ttag`.
- **The stamp is two-anchor, not a build hash.** `arch` (compile-time tag) + `anchor` (dump-time address
  of `ai_image_save`): under ASLR the whole binary shifts by one base delta, so the immortals/refsym
  delta must equal the ai_image_save/anchor delta; a different binary (cross-arch or stale rebuild) lays
  symbols out differently → deltas disagree → refused. The 4-way {x86,aarch64}×{bin,img} matrix is clean.
- **Core/host split (stdio out of love.c).** The core owns the stdio-free buffer codec `ai_image_save` /
  `ai_image_load` (love.h); file I/O lives in `host/image.c`. The Phase-0 `image-check` and Phase-1
  `image-rt` spikes are retired.
- **The section is GROWN, not reserved.** `.image` is laid LAST — alone in the highest `PT_LOAD`,
  above `.bss` (`-Wl,--section-start=.image=0x2000000`, host/build.mk) — so the bake APPENDS the blob
  at the first page past every other allocated byte and rewrites the one phdr and one shdr that name
  it, relaying the non-allocated tail (symtab/strtab/shstrtab) after it. No vaddr moves, so the
  two-anchor stamp above still holds by construction. That retired a 6 MiB fixed reserve carrying
  2.2 MiB of shipped zeros and a `RESERVE_WORDS` to bump whenever the image outgrew it (7.5 MB → 5.3 MB).
  It has to stay a real allocated section rather than loose bytes at EOF: `strip` (which `install -s`
  runs) keeps the section and drops a bare trailer. The rule `host/image.c` checks is only that
  `.image` ENDS the segment carrying it, which covers both shapes with the same arithmetic: a section
  alone in the highest `PT_LOAD` (ld/lld) and one riding the tail of the single segment holo lays
  (`love-raw` 8.3 MB → 1.9 MB unbaked). It reads that off the binary's own section headers rather than
  a build flag, so neither lane is told which it is, and a link that lays `.image` anywhere else is
  refused loudly with the flag it wants — there is no reserve to fall back to, and no `__APPLE__`
  lane either (`image.c` needs `<link.h>` + `dl_iterate_phdr`, so it does not build on mach-o at all;
  a mac host owes it `_NSGetExecutablePath`). For the mooncc lane the bytes had to become a real section
  first: `.image` is now a fourth stream through cgdata → objelf → the `image` lane in link.l, beside
  `ai_nifs` — the other named section whose whole point is WHERE it lands.
- **The glaze bake is the corpus eval, not a split assert-free lib.** `--bake` evals the glaze
  (emit.l+auto.l, x86-gated) before dumping; the asserts' transient natives die in `gen_major`. emit.l's
  self-test fixtures were wrapped local (they'd leaked as globals); auto.l's `memo` cache is cleared
  pre-dump. (Dump-time chain hash-cons was BUILT then BACKED OUT — unsound with the glaze, see follow-up 2.)

## The live bake — (bake path), a nif (landed 2026-07-13)

`(bake "x.image")` snapshots the RUNNING session to an image file, mid-eval — no quiescent
point required — and answers 1 | (); the session rides on. Wake it with `love --wake x.image
prog.l args..`: the woken book carries every global pinned before the bake, so an app loaded
warm (`love -l app -e '(bake "app.image")'`) never pays its load again — the mooncc image took
`mooncc -c love.c` from ~3.7 s to ~2.4 s, the whole per-run load tax. Three seams make mid-eval
dumping honest where the boot bake could assume purity:

- **The stack is ballast, not state.** The running continuation's objects get traced (they're
  live) and ride into the blob; the load side resets `sp` and re-establishes `ip` regardless,
  so they're wake-unreachable garbage swept at the woken session's first major. `ai_image_save_`
  (the unguarded worker) does the dump; `ai_image_save` keeps the empty-stack guard for the
  boot path, where a non-quiescent dump is a bug.
- **Live finalizer nodes forge into dead chains.** An open port's close (or a nat's unmap) is a
  raw three-word `ai_fz` in the heap — no object header, so the blind walks (save's encode and
  load's decode) can't stride it. The save walk recognizes the `g->fz` chain and overwrites each
  node's BLOB copy with a `(() . ())` chain of the same width; `fz` lives outside the serialized
  v0..end root window, so the woken session starts with no finalizables (the dump-time fds meant
  nothing in the new process anyway).
- **NULL is an immortal.** A live bio port carries undressed `rbuf`/`wbuf` zero words; a raw
  zero is even and below the index bound, so it needed its own slot in `image_immortals`.

The `bake` global is a glaze wrapper (love/glaze/hook.l) over the host nif (host/image.c, the AI_NIF glob): it
empties the glaze compile cache first (a native closure cannot serialize; entries re-JIT lazily
in the woken session). Any OTHER live native at bake time is on the caller — same contract as
the boot bake. Smoke: boot/bake.l (test_hostnif) round-trips a pinned marker through bake +
`--wake` in a child process.

## Three follow-ups (open, in rough priority)

1. **A per-arch aarch64 glaze emitter — make the aarch64 image GLAZED too** ("even on an MCU, try"). The
   aarch64 host image works (cross-built + qemu-tested) but is glaze-LESS: the glaze emits x86-64 machine
   code, so `main.c` gates the dump-time glaze load on `__x86_64__`. The recognizers (`love/glaze/auto.l`)
   are arch-NEUTRAL (they analyze love source); only the codegen (`love/glaze/emit.l` — `cgv`/`cgn`/
   `loopcode`/the SSE/register layer) is x86. The `crew/asm/` assembler already has an arm64 backend
   (`crew/asm/arm64.l`, emits bytes as DATA), so the scope is a parallel arm64 instruction-selection path in
   emit.l + an arch dispatch in `auto-ev`'s `njit`, then dump a glazed aarch64 image. Biggest payoff for
   the embedded/Nerves target.

2. **The glaze-CSE refactor — make dump-time chain dedup SOUND.** The compacted image is ~71 % source-AST
   chains; merging structurally-equal sub-trees (hash-cons) shrank it ~36 % (6.2→4.3 MB, pre-table-
   removal) but is UNSOUND: the glaze reads source by IDENTITY, so merging bintrees' two `(mk (- d 1))`
   cells changed its native cons codegen (checksum 1600174→1582768). Fix upstream: make the glaze key its
   CSE/codegen on structural equality (`=`), not cell `id?`, so `=`-equal sub-expressions are already one.
   Then the reverted `image_dedup` (union-find hash-cons over chain starts) can re-land safely. Scope:
   audit emit.l/auto.l for `id?` on source sub-terms. Sibling caution to [[value-interning-dropped]].

3. **A freestanding kernel snapshot — boot the kernel from a baked image, no eval.** `ai_image_save`/
   `ai_image_load` are now buffer-based + stdio-free (confirmed: the codec sits OUTSIDE the one
   `#if __STDC_HOSTED__` region, so it compiles into the freestanding kernel), so the kernel (`port/inle/`,
   `kmain.c` — no filesystem) could `objcopy` a dumped image into a C byte array and `ai_image_load(array,
   len)` at startup instead of eval'ing the baked corpus. **Knot 1 (gen-in-kernel) RESOLVED 2026-06-26;
   knot 2 (a kernel-loadable image) still open.** The two knots:

   - **Knot 1 — gen-in-kernel. RESOLVED.** The codec relocates into the MAJOR pool, which only the
     generational collector has; the kernel used to build `-DAI_NOGEN` (single-pool gcg, no major pool), so
     `ai_image_save` couldn't run there. The kernel now runs **gen, bounded** (dropped `-DAI_NOGEN`;
     `meminit` sums the limine memmap into `kram_words`, `kmain` sets `ai_core_of(g)->budget = kram_words/8`
     after `ai_ini`). **K_TEST green with gen: 2569 tests pass in ~3.8 s** (`make test_kernel`). The earlier
     "hang" was actually a TRIPLE FAULT: the diagnosis (qemu `-d int,cpu_reset` → #GP at `k_reset`'s `int
     $0x0`, last live code in `gcp`; a serial GC trace then nailed it) was a heap OVERFLOW, not a malloc
     deadlock. Two real causes, both masked on the host by glibc's effectively-infinite, fragmentation-proof
     `malloc`: (a) with `budget=0` the nursery's copy-overhead resizer grew to ~33 MB, so `gen_major`'s
     worst-case (all-survive) sizing asked `kmallocw` for an ~86 MB CONTIGUOUS block — but free RAM is 10
     discontiguous memmap ranges, largest ~145 MB, and live pools pin its middle, so the request failed
     (`kfree` coalesces fine — it just can't span ranges or merge across a live block); (b) on that alloc
     failure `gen_major` fell back to the existing (smaller) spare half and "hoped it fits" — UNSOUND when
     the live set exceeds a half, overflowing it → corruption. Fix (love.c `gen_major`/`gen_please`, all
     `g->budget`-gated so the host is untouched): cap the major `to_len` by the budget (never below the tight
     `need`), and make the OOM path SOUND — retry at the tight size, use the spare only when `need <=
     major_len`, else a clean OOM scare (no overflow). The nursery cap is also tightened to reserve for the
     major that must hold it: `~(budget - 2*live)/4`, since `2*major + 2*minor <= budget` with `major ~ live
     + nursery`. Host gate + `make test_kernel` both green. The interactive kernel is already `tco=1`; only
     K_TEST forces `tco=0` (a separate freestanding-sibcall hang), orthogonal to gen.
   - **Knot 2 — a kernel-LOADABLE image.** Even past knot 1, a host-dumped image won't load in the kernel:
     the arch+anchor stamp rejects a different binary (different lvm_* table order + binary-pointer
     addresses). Options: dump from the KERNEL binary (unexec — boot in qemu, serialize, emit the bytes
     over serial, `objcopy` into the kernel, RELINK with image.o placed LAST so .text/.rodata addresses
     don't shift — the kernel is non-PIE so absolute pointers stay valid only if nothing moves); or a
     layout-stable shared TU. Resolves cold start on the MCU too.
