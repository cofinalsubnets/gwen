# one image — the whole crew in one binary

A plan (chosen, revisable), not a record: nothing here has landed. The aim is to retire the
three-image build and the shim/`--wake` zoo around it, so `love` is the one artifact and the
crew rides inside it — dispatched by argv[0], or by a verb in the first argument.

Most of this is a **promotion**, not an invention. `make dist` already bakes exactly this
binary; the work is making it the default and widening the verb table. Read crew/build.mk's
dist lane before anything else here.

## where we are

Four images, three of them shipped:

| image | built by | woken by | what's in it |
|---|---|---|---|
| `out/host/love`'s `.image` | `love --bake` | itself, automatically | the egg + glaze |
| `out/host/kore.image` | `$m -l .kore-cat.l -e (bake ..)` | the `out/host/kore` shim | kore, vi, lush, cook, ain, holo's linker half |
| `out/host/mooncc.image` | ditto, `.mooncc-cat.l` | the `out/host/mooncc` shim | moon + all five holo backends |
| `out/host/mooncc0.image` | **love0** | `$(moon0)` | the same cat, one generation back |

The two shims are generated `#!/bin/sh` wrappers that resolve their own directory and exec the
*sibling* love on the *sibling* image — never PATH's, so a fresh cat can never run under a
foreign older-baked binary. That discipline stays true under one image; it just stops needing
a wrapper to express it.

The cost of the split shows up as invocation patterns. Fifteen-odd gate scripts carry a
hand-rolled `moonrun()`/`korerun()`:

```sh
moonrun() { "$m" --wake "$ho/mooncc.image" -e '(moon-main (cuup (cup cmdline)))' "$@"; }
korerun() { "$m" --wake "$ho/kore.image" -e '(kore-main (link "kore" (cuup (cup cmdline))))' "$@"; }
```

and ~30 test.mk targets carry `out/host$(hsuf)/mooncc.image` prerequisites to match.
mk/install.mk's `binnames` is fourteen commands plus `lib/love/mooncc.image`.

## what already exists — do not rebuild it

* **the unified bake.** `out/dist/love-$a` (crew/build.mk) is the default love re-baked with
  cook + kore + lush + vi + mooncc + all five backends + seed + kiosko: 22 MB, one file. Its
  `$(distfiles)` roster **is** the correct load order — asbook before the backends (defbackend
  mutates the spliced holo), every main before kore.l's applet table, up.l last. Reuse the
  roster; do not re-derive it.
* **both dispatch doors.** love/cli.l's verb rail turns a leading positional into `(FN args)`,
  and argv[0] naming a verb lands the same table. Both work today on the dist artifact —
  `love mooncc ..` and a `mooncc` symlink are already the same binary.
* **`make`.** crew/kore/kore.l registers `'make cook-main 'cook cook-main`; `love kore make
  all` runs a Makefile through cook now. Nothing to add.

What is missing is only the **width** of the table: crew/seed/up.l's `verbs` is nine names (up
down seed cook kore kiosko mooncc sh lush), so `love cat` and a `cat` symlink fall through to
"run this file as a love program".

## the rungs

Rung 0 is **preliminary — the decode walk**, below: it is not part of the unification, it pays
on today's `--wake` gates whether or not any of this lands, and it is what stops the fat
image's boot cost being an argument against the rest.

1. **widen the verb table.** Splice kore's applet registry into `verbs` so every applet is a
   top-level verb and an argv[0] name. Add `cc` beside `mooncc` — that is what lets `CC=cc`
   work with no ambient toolchain. `love kore sed` keeps nesting; nothing is taken away.
2. **guard the shadow.** A verb wins over a same-named program file, and fifty applets shadow
   far more than nine — `make`, `env`, `install`, `true`, `cmp`, `seq` all stop being able to
   name a local script. The applet lane should win only when the positional carries no `/` and
   no `.l`.
3. **bake the crew into `out/host/love`.** `$(ho)/love.baked` grows the dist cat as a
   prerequisite. `kore.image`, `mooncc.image` and both shims go.
4. **convert the callers.** `moonrun`/`korerun` collapse to `mw cc ..` / `mw kore ..`; the
   image prerequisites in test.mk become plain `host`.
5. **collapse the install.** `binnames` becomes `$(BIN)` plus symlinks; `lib/love/mooncc.image`
   leaves the install set.

`mooncc0.image` and love0 **stay**. The default love is mooncc-built, so love's own image can
never drive love's build — that image is what breaks the circle, and it is why this is one
*shipped* image rather than one image.

## the costs, measured

On this box, booting `(quit 0)`:

| | size | boot | page faults | instructions |
|---|---|---|---|---|
| plain baked love | 5.4 MB | 25 ms | 4,088 | 184 M |
| dist (unified) | 22 MB | 45 ms | 6,559 | 362 M |

A full unified bake is **8.6 s**. For contrast, test.mk:207 records the lane the shims exist to
avoid: `~0.02s vs ~0.75s per spawn`.

**The corpus does not pay the +20 ms.** Makefile:11 exports `LOVE_NO_IMAGE := 1`, so every
recipe egg-boots on purpose and a fatter baked image is invisible to the gate. ⚠ the same fact
is a conversion trap: under that export `$m kore cat` egg-boots *without* kore, so every
converted runner must go through `mw` (`env -u LOVE_NO_IMAGE $m`, test.mk:20), not `$m`.

The +20 ms lands on `--wake` lanes and on real user invocations, and it is **linear in image
size, not a property of unification**: ~47% of a unified boot is the image decode walk
(`img_decode` 37.9% self, `ai_image_load_m` 9.2%, `gcp` 5.3%). See doc/snapshot.md — the wire
format is symbolic, so waking means touching every word. Three times the heap is three times
the walk. Anything dropped from the cat comes straight off the boot, and any speedup to the
walk pays here, on today's `--wake` gates, and on `love up`'s nest alike.

## preliminary — the decode walk

The walk that wakes an image is ~40% of every wake, in **both** compilers. Profiling a
mooncc-built and a gcc-built love waking the same cat:

| build | walk share | words | walk cost |
|---|---|---|---|
| mooncc (the default love) | `img_decode` 39.1% + `ai_image_load_m` 7.3% = 46.3% | 2.62 M | 7.5 ms/Mword |
| gcc (love0) | `ai_image_load_m` 38.1% — img_decode fully inlined, no symbol | 1.37 M | 4.3 ms/Mword |

Two independent wins sit in that table: the walk is expensive *as an algorithm*, and mooncc
leaves `img_decode` out of line where gcc fuses it.

**rung 0 — an honest instrument.** The walk already carries `ai_image_note` (a weak fn, one
call per 64K words). Count words reaching the decode, split by which ladder rung each takes,
and objects by kind; print under an env knob; delete it with the hunt.

⚠ this rung is not optional, because the obvious probe lies. Classifying the raw image words
statically reads 8% odd / 45% heap-offset / 47% index-or-absolute — worthless as a decode
profile, since string bytes, bignum limbs and glaze's native code are payload that
`image_objsize` skips wholesale and `img_decode` never sees. (That payload is also why the
mooncc image is 21 MB against love0's 11 MB for the identical cat: glaze roughly doubles it.)

**rung 0 ran** (2026-08-10, probe since deleted). Three images, one story — decode calls by
rung: heap-offset 42–45%, lvm-index 36–38%, immortal 8%, fixnum 7–9%, kept-absolute 1.2–1.5%,
nif-cell + bare-fn statistically zero (17 + 0 on the 2.05M-decode mooncc walk). Chains are
71–78% of all words (mooncc.image: 541K of 564K objects); the thread terminator scan
re-touches another 19–25% of words (395K on mooncc.image). Two consequences: rung 1's hot
head must carry the first *four* rungs — odd + heap + lvm + immortal, each a compare and at
most two loads, together 98.7% of decodes — not the two the plan guessed (52%); and rung 2's
fusion is worth its ~19% of touches.

**rung 1 — inline it, for mooncc parity.** The cheap, certain one, and it lands on the DEFAULT
love. Do not blanket-inline a 544-instruction body at six call sites: split it the way the
tree already splits things — a tiny `ai_inline` hot head (`oddp` → return; `uv < hb` →
`base + uv`) over an `ai_noinline` cold tail carrying the four index ranges. love.h:25 spells
`ai_inline` as `always_inline`, which mooncc honors.

**rung 1 landed**, with the census-corrected split: the hot head carries odd + heap + lvm +
immortal (`image_ap_resolve` marked `ai_inline` too — the lvm rung is 38% of decodes and the
resolve is two loads), `img_decode_cold` keeps cell/fn/absolute. Waking mooncc.image:
38.8 → 33.4 ms (−14%), 240.5 → 226.2 M instructions; `img_decode` leaves the profile,
`ai_image_load_m` absorbs it.

**rung 2 — fuse the thread double-pass.** ai_image_load_m scans a thread word by word for its
encoded terminator, then loops the same run again to decode it. One pass, decoding as it goes
and stopping on the terminator, halves the touches for thread objects — the bulk of a
code-heavy image. ⚠ the `kmax` bound is load-bearing and must survive the fusion: a mis-decoded
word0 has to refuse the load, never march off the pool (on metal the pool's edge is a dead bus,
and a dead bus is mute).

**rung 2 landed**, and the measurement is the story: on a cache-warm host it is −0.5%
instructions and wall-flat — the scan it removes was a tight loop prefetching for the decode
behind it, so the halved touches only pay where memory is slow (a metal wake). ⚠ measuring
this took a 4-fresh-bakes-per-variant protocol: a bake is never byte-stable, and bake-to-bake
image variance moves the wake's instruction count by more (±1%) than the fusion does. The
terminator now decodes by hand (`(word) p + ai_thread_tag` — its heap-offset decode lands
exactly there), and the bound check moved ahead of the read, one word stricter at the pool's
edge.

**rung 3 — hoist the ladder bounds.** `hb + 2*(IMAGE_NLVM+IMAGE_NIMM) + 2*IMAGE_NLVM*IMAGE_CELLW`
and its siblings are `hb + a compile-time constant`, recomputed per word off an argument. Read
the disassembly after rung 1 before writing any code: fusing into the loop may already have
hoisted them.

**rung 3 read the disassembly and stopped there.** Post-rung-1 the bounds are already
`add $const` off the hb slot — a source hoist trades that add-immediate for a slot load, a
wash. What the hot window actually shows is per-use slot reloads (`mov 0x1f0(%rsp)` before
every touch), which is mooncc's known no-cross-slot-residency story, not this walk's — the
lever lives in the regalloc arc, not here.

**rung 4 — only if 1–3 do not pay.** Walking less means either a pre-relocated wire format
(which loses the ASLR portability the symbolic encoding buys) or dropping glaze's cells from
the bake and re-glazing lazily (which loses "the glaze bake is free", the payoff
doc/snapshot.md names as the reason to have a snapshot at all). Both look like bad trades;
they are listed so they get refused on purpose rather than rediscovered.

Gates: `test_wake` (test.mk:754, the only bake-then-wake round trip), `test_kore`, `test_moon`,
and an image-equivalence check — ⚠ a bake is never byte-stable, so compare offset sets or the
`.o`, never the image bytes.

Gates ran green on rungs 1+2 (2026-08-10): `make test` (all three zz-fin lines), `test_wake`,
`test_kore`, `test_moon`. No equivalence check owed: every edit is load-side (`img_decode`,
the load walk); `img_encode` and the save walk are untouched, so the bake is unchanged by
construction. Ladder total on the mooncc.image wake: 38.8 → 33.1 ms, 240.5 → 226.0 M
instructions — the walk's share of a fat wake down from ~46% to ~39%.

## what to watch

* **rebuild coupling on a shared tree.** The split is deliberate: a cc edit rebakes
  `mooncc.image` alone, so a kore rebuild in a neighbouring session cannot tear the compiler.
  Unified, any crew edit rebakes the one binary every session and every gate executes, at 8.6 s
  a go — with a dozen live worktrees on this tree that is the real hazard. The bake renames
  atomically onto a new inode, so a *running* process is safe; the *next* exec is not. The
  answer already exists: host/build.mk's `candidate` lane builds and bakes at a side path and
  promotes on green.
* **test_fixpoint is unaffected.** It compares *linked* binaries before any bake, and builds
  its own compiler image under `LOVE_NO_IMAGE=1`. The bake must stay a post-link step.
* **the other frontends are not this.** wasm, playdate, mps2, teensy41 and the inle kernel
  embed their own lib subsets through the `ai_libs` tables. "One image" is a claim about the
  host frontend only.
* **the seats.** Every crew file's tail seat must stay quiet in a cat that is loaded under a
  neutral name. The dist cat already satisfies this — which is the evidence the ordering is
  right, and the reason to copy the roster rather than write a new one.
