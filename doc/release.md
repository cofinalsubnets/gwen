# release — the moon + reef + precedence cut

Status: **checklist / in progress.** Started 2026-07-14. The living checklist for the next
public cut. Prior release state (Juneteenth 2026-06-19) is archived at
[`doc/archive/release-state.md`](archive/release-state.md).

## the headline

Three features carry this release:

1. **moon — the self-hosting toolchain.** love builds itself from source with no external
   toolchain: `mooncc` (the C compiler, crew `moon`) + [`crew/holo/link.l`](../crew/holo/link.l)
   (our linker) + nolibc. `make test_raw` is gcc/glibc/ld-free.
2. **reef — the vcs.** New this cut. A patch-DAG version control folded together with the
   installer (hatch). Model in [`doc/hatch.md`](hatch.md), verb set in [`doc/reef.md`](reef.md);
   needs to get practically working.
3. **precedence — infix that binds like schoolbook math.** `*` tighter than `+` tighter than
   `=`. prel-only, no C. LANDED 2026-07-14 — shipped in [`doc/precedence.md`](precedence.md),
   gated by [`test/precedence.l`](../test/precedence.l).

Riding along: the long-deferred **namespaces** cleanup closes its phase-3 tail — the module
books (`holo`/`glaze`/`kanren`/`uu`/`overlay`/`parse`) stop handing out their raw backing
tablet, so a stray `(pin holo …)` can no longer poison a baked service. See [[namespace-modules]].

## where each stands

| feature | state | gap to release |
|---|---|---|
| moon/mooncc (x64) | landed — gcc-free love boots + passes the corpus; **rename to `moon`/`mooncc` landed 2026-07-15** (crew/moon/, moon.l, moon-main, test_moon, doc/moon.md) | polish + docs |
| moon (arm64) | rungs A–C landed (static exes + .o + our-linker, varargs + sibcalls, 88/88 battery 3 ways) | rung D (nolibc/mksys, the gcc-free arm64 path) about to land — **best-effort in, does NOT block** |
| reef/vcs | **MVP landed 2026-07-14** — `record`·`sync`·`hatch`·`log`·`diff` over the content-addressed `.reef/` store (crew/reef/, sha256 in host/hash.c); `sync` unions a peer nest + re-derives the DAG + materializes; `hatch` builds the recorded source into a seed keyed by patch-set + arch (reproducibility audit); `make test_reef` gates it | the cached-fetch CDN substituter (deferred with public distribution); `cut`/`undo` reserve-the-names |
| precedence | **landed 2026-07-14** — grip bands in prel.l opfix, corpus re-validated (3 asserts shifted, all `\|`/`&`-with-`=`), `test/precedence.l` gates it | done (the `grip` name + house=27 ship as working defaults, gwen's to bless) |
| namespaces | phases 1+3 landed — `(names ())` 820 → 327; **phase-3 tail landed 2026-07-14** — all six module books are lookup-only closures, poison-proof, with a `'keys` probe | done (optional `~327 → 323` curation trim aside); the abyss/scoped-layers arc stays deferred |

---

## checklist

### A. features

**moon / mooncc**
- [x] decide arm64 scope — **does NOT block the release.** Rungs A–C landed; rung D (nolibc/mksys, dirty in the
      arm64 worktree's `crew/moon/lib/` — the `crew/cc/`→`crew/moon/` rename landed here first, so that worktree
      reconciles the path on merge) is about to land and we'll try to fold it in, but the cut ships either way (see [[aicc-arm64]])
- [ ] `doc/moon.md` reads as a release doc, not a build log — a user can compile a hello-world and know the flags
- [ ] a headline one-liner ("a self-hosting C toolchain in ~N lines of love; no gcc, no glibc, no ld")

**as** — a real assembler, the front tooth of the gcc-free chain (pairs with link.l's `ld`)
- Reads standard GNU AT&T `.s` → obj.l-shaped relocatable `.o` → link.l. New front over holo:
  parse (post.l combinators) → physical-register encoder (the bulk) → per-section (bytes·relocs·labels).
- Reuse: link.l unchanged; obj.l's ELF back half (factor `objelf-raw` at the o-walk seam so cc + as
  share it); x64.l as the encoding reference + its rex/modrm/sib/le* primitives.
- Home: `crew/holo/as.l`, swept into the `holo` book like link.l. **kore-as-`as` execs the holo
  assembler binary** — same execvp pattern as kore-as-`cc`→mooncc (binary name is a small open item).
- Scope: **x64 this cut** (arm64 assembler folds into the arm64 parity arc, not now).
- Bar (gwen 2026-07-14): **THIS cut = battery + link-and-run** — hand-written `.s` snippets byte-identical
  to `/usr/bin/as`, then linked via link.l into binaries that run; the instruction tail grows one snippet
  at a time. **NEXT cut = `mooncc -S` round-trip** (teach mooncc an asm-text emitter, prove `mooncc -S | as` ==
  `mooncc -c` byte-for-byte). North star (later): assemble real `gcc -S love.c` → boot (needs link.l foreign-obj
  features — .eh_frame/.bss/comdat it skips today).
- Milestones: M1 ~15 core instrs → `.o` → link → runs (`.text` byte-diffed vs system as) · M2 width
  variants + memory operands + `.data`/relocs + **branch relaxation** (short/near) · M3 SSE + the long
  tail · M4 arm64 (deferred).
- **M1 ENCODER LANDED 2026-07-14** (`crew/holo/as.l`, committed `a717609a` + style pass `7ad6c4e6`):
  the AT&T parser +
  x86-64 encoder (64/32-bit GPR core) is **byte-identical to `/usr/bin/as`** across a 24-instruction
  straight-line battery — reg/imm ALU (incl. the `83 /ext` imm8 short form + `81 /ext` id), mov
  (`b8`/`c7`/movabs by width+range), test, unary, push/pop, `lea foo(%rip)` + `call foo` (internal,
  resolve in place), ret/nop/syscall/leave/cq[dt]o. Branches (`jmp`/`jcc`) emit **correct near forms**
  with right rel32 displacements; GAS relaxes to short — that byte-diff on branches is the **M2
  relaxation** item, code runs identically. Traps hit while building: `cat` is DYADIC (`(cat a b c)`
  church-applies the list → apcap; use `catall (L ..)`); table rows need `(link k v)` not `` `(k v) ``
  (the `(cap p)/(cup p)` idiom wants a cons, else the value nests one deep → `add` emitted `cmp`'s /7);
  a lone extra `)` closed the top-level `:` early → tail ran with unbound `src` at load.
- **Integration (deferred — same arm64-collision reason as moon):** there's already a `kore as` applet
  (`crew/kore/kore.l` `as-main`) that assembles holo's OWN IR → ELF via `elf64`; the real-AT&T `as`
  UPGRADES that path to read `.s` → `.o`. Wiring needs: obj.l `objelf-raw` (factor at the `o-walk` seam
  so it takes pre-laid `(items·labels)` sections, not IR forms) + as.l migrated bare→`holo`-book refs
  (to live in the kore cat beside obj.l/link.l, cf. `asbook.l`) + `as-main` reading AT&T + emit `.o`
  through obj.l + link via link.l. All touches `crew/holo/` + the kore cat — hold until the arm64 work
  in crew/holo/{obj,arm64,link}.l + crew/moon/ commits, to avoid the collision.
- **kore-as-`as` dispatch (gwen 2026-07-14):** kore invoked as `as` execs the holo assembler binary —
  same execvp pattern as kore-as-`cc`→mooncc (binary name a small open item, cf. mooncc).
- [x] **`test_as` byte-diff gate LANDED 2026-07-14** — `crew/holo/astest.l` (33-instr straight-line battery)
      byte-identical to `/usr/bin/as`, frozen goldens (no shell-out at gate time), same `", 0 failed"` sentinel
      as `test_holo`; wired into `.PHONY` + `test_slow`. Mirrors `test/holo.l`'s `(= golden (as-hex src))` style.
      Found + fixed an as.l bug along the way: `cdqe` emitted `99` (conflated with `cdq`) — it's REX.W+98 = `4898`.
      **KNOWN as.l gaps documented in astest.l's header (M2, correctly kept out of the byte-identical battery):**
      no AT&T size suffixes (base mnemonic + width-from-register only); branch relaxation (jmp/jcc-to-near-label);
      the accumulator short forms (`add $imm32,%rax` → GAS's `05`); external-symbol call/lea need the reloc path.
- [ ] **link-and-run + kore/holo cat integration — DEFERRED (blocked, confirmed 2026-07-14).** as.l emits already-
      lowered `items` (bytes + `(fix ..)`), but `objelf` (obj.l:90) takes holo IR and re-`lay`s it; the `o-walk`
      seam is internal. Reaching the `.o`→`ldlink`→`ld-write` path needs the `objelf-raw` factoring — which lives
      in `crew/holo/obj.l`, the arm64-churn collision zone the doc already holds `as` integration behind. So the
      link-and-run half of gwen's bar waits for the arm64 batch (with `objelf-raw` + the as.l bare→`holo`-book migration)

**reef (vcs)** — MVP LANDED 2026-07-14 (`record`·`sync`·`hatch`·`log`·`diff`; CDN-fetch deferred)
- [x] pick the minimum viable verb set — `record` · `sync` · `log` · `diff` + `hatch` (per reef.md; `sync` in over `apply` — the near-term job is multi-machine tip-union)
- [x] patch/commit object shape + the DAG (pijul-style patches, no privileged trunk — per hatch.md): the content-addressed `.reef/` store, a hunk = the proven named-slot `chg`; `sync` re-derives tips + snap over the whole set (order-free, the DAG is a pure function of its patches)
- [x] where it lives: `crew/reef/` (over the kore text+diff floor; sha256 in host/hash.c)
- [x] a real end-to-end run: record a change, `sync` a peer nest in, log it, materialize onto a clean tree, `hatch` the recorded source into a fingerprinted seed — `make test_reef` + the CLI smoke both green
- [x] decide how much of hatch (install = clone + hatch) rides in this cut vs. lands later — the LOCAL-REBUILD derivation + the reproducibility ledger ride this cut (hatch the recorded state → run recipe → seed keyed by patch-set + arch → re-hatch is a reproducibility audit); the cached-fetch CDN substituter is deferred with public distribution
- [x] `doc/reef.md` — first draft (verb set + composition story + MVP)

**precedence (grip)** — LANDED 2026-07-14
- [x] implement grips in the reader-operators block of `love/prel.l` (op-ent normalizes to the triple `(name arity . grip)`, op-fr carries a grip slot + `op-frgrip`, op-steal gained the climb: fold when the frame grips tighter than the incomer, steal otherwise)
- [x] assign the grip bands (multiplicative 60 / additive 50 / comparison 40 / logical 30 / house 27 / cons 25 / assignment 20 / cond 10)
- [x] `make test` ×3 green (3416 → 3439, +23 from `test/precedence.l`) + `test_slow` (glaze / holo / as / every crew app / wasm / x86 kernel / wake all green; the sole red is the pre-existing qemu-arm64 `uk-jj`, host-uukind passes, precedence.l passes on arm64)
- [x] the corpus audit: only THREE asserts shifted, all `|`/`&`-mixed-with-`=` (the doc's flagged risk the audit had cleared for `&&`/`||` but not single `|`/`&`) — `test/spec.l:88`,`:164` parenthesized (bitwise below `=`, grip 30 < 40, gwen blessed the C-style band), `test/infixop.l:27` updated to the C-ternary read (`1 < 2 ? 'big 'small` → `(? (< 1 2) 'big 'small)`)
- [x] `test/precedence.l` — tree asserts (via the live `opfix`, op-core being book-private) + value asserts + short-circuit + idempotence; non-vacuous
- [x] promote `doc/precedence.md` from design to shipped
- ( ) `grip` the name shipped as a working default (internal, not in `(names ())`, mechanically swappable) — still gwen's to bless/rename per precedence.md §Naming ([[decisions-never-locked]]). House settled at 95 on 2026-08-04, above every band, per haskell's undeclared-is-`infixl 9`

**namespaces** — close the phase-3 tail (phases 1 + 3 landed: `(names ())` 820 → 327; see [[namespace-modules]])
- **NO "sealed tablet" language feature** (decided 2026-07-14). The same way users can't reassign the
  top-level `book` — it's *mopped* out of the image, so `pin`/`pull` on it no-op — the module books just
  stop exposing their raw backing tablet. Regular tablets throughout; only the *access* is limited.
- [x] make each module book a **lookup-only closure over a private tablet** — LANDED 2026-07-14. All six
      trailers (`crew/holo/export.l`, `love/glaze/export.l`, `love/prel.l` kanren, `love/uu.l`, `love/post.l`
      parse+overlay) now bind `(\ k (? (id? k 'keys) (keys m) (peep m k 0)))` instead of the raw tablet `m`.
      `(holo 'assemble)` still looks up, `m` is closure-private, and `(pin holo ..)` has no tablet to poison
      (probed before/after: `(pin holo 'PWNED 666)` read back **666** → now **999**/default for all six). Bonus
      per gwen: `(book 'keys)` introspects the surface (holo 194 · glaze 360 · kanren 21 · uu 55 · parse 13 · overlay 4)
- [x] verified main.c's image-dump reach-through (`(peep book 'glaze 0) 'cache` → the closure returns the memo
      cache, `(keys c)` iterates the cache itself) — `--bake` writes a clean 3.6M image and `--wake` boots it
      (glaze/`ev` live). Grep found **zero** consumers that lean on a book being a tablet (the one `gfind … parse`
      in emit.l is a *local* `parse`, shadowing the book). `make test` green ×3 (host + love0 ×2, 3416 tests)
- [ ] (optional, gwen's word) a curation pass to trim the ~327 back toward 323 — new crew apps drip a few leaks
      each (post.l combinators, kanren internals, overlay `ov-*`, tele's `cuda-*`); or accept the drift
- [ ] **NOT this cut:** phase 2 "the abyss" (descending scope layers, the chain in `g->book`, per-layer macro
      tables) — designed with gwen 2026-07-07, a dual-compiler-parity architectural lift, its own arc

**housekeeping** — not a feature, but lands with the cut
- [ ] **break up the 1635-line root `Makefile`** — non-recursive `include` fragments, host/kernel builds
      split out next to their source (`host/build.mk`, `port/inle/build.mk`) + the crew test bulk to
      `mk/apps.mk`. Pure motion, `make -pqn` byte-identical at each step.

### B. naming & persona (gwen christens — offers only)

- [x] **aicc → moon** (crew name) / **mooncc** (the binary) — DECIDED. Creature = the glowing mycelium 🍄 on holo's cave walls; moon is a sibling to inle (the kernel). `mooncc` slots into the cc/gcc/tcc tradition and clears the `moon`(MoonBit)/`moonc`(MoonScript) collision
- [ ] **kore-as-`cc`** — moon left the au/kore cat, so kore invoked as `cc` execs `mooncc`. No new PATH-walker needed: the `exec` nif IS `execvp` (host/main.c:334), which searches $PATH itself. kore's `cc` case builds argv `("mooncc" . args)` and `(exec …)`; on success it never returns, on absence it returns an errno fixnum (ENOENT) → fall back to `cc: mooncc not found` + quit 127
- [x] **rename-all-the-way-down LANDED 2026-07-15** — `crew/cc/` → `crew/moon/`, `cc.l` → `moon.l`,
      `doc/cc.md` → `doc/moon.md`, the binary `aicc` → `mooncc` (+ `mooncc.image`, `bin/mooncc`), `cc-main`
      → `moon-main`, `test_cc` → `test_moon`, `__aicc__` → `__mooncc__` (doc-only, the macro isn't in `love.h`
      yet). The math floor moved with it (`crew/moon/lib/math/am.c`, tracked through `common.mk`/`wasm`/`ulp`/
      `arm64check`). `make test` ×3 + `test_moon` green; `Cookfile` regenerated clean. Done ahead of the arm64
      batch (gwen's call — accepting the `crew/moon/` + `crew/holo/` merge friction with the in-flight arm64 work).
- [ ] **kore-as-`cc` dispatch** — still pending (a feature, not the rename): kore invoked as `cc` execs `mooncc`
      via the `exec`/`execvp` nif. kore.l has no `cc` case yet. See item above and §B[2].
- [x] **reef** = 🪸 coral — the vcs/distro persona (the coral colony = the patch DAG). Chosen 2026-07-14 (was `tree`; dropped for the `tree(1)` collision + reef fits the rootless, accretion-only model better — see reef.md §why-reef). Command name still open, and like everything, revisable.
- [x] **pulchritude** persona coral → giant centipede (brightly-colored *Scolopendra*); keep the name (it carries its own layers). Done in the `5b3f84bf` roster refresh — `index.html` reads `🐛 pulchritude ◦ the editor / brightly banded centipede`. Emoji is 🐛 (gwen 2026-07-14, keeps over the earlier 🦂 shorthand)
- [x] **au → kore** 🦨 (skunk) — LANDED (`15f46d45`), all-the-way-down: crew/kore/, kore.l, bin/kore, test_kore, doc/kore.md. Follow-ups: a kore roster line in index.html; kore-as-`cc` execs mooncc (needs moon)
- [x] **phos → lux** — DECIDED. The WM; mantis shrimp stays as the creature. Own pass — plan in §appendix
- [x] name sweep for the `aicc`→`mooncc` / `crew/cc`→`crew/moon` rename — swept `.l` `.c` `.h` `.md` `.mk`
      `Makefile` + regenerated `Cookfile`; `index.html`/`README` had no `aicc` refs yet (crew list add is item C).
      Any FUTURE rename re-runs this (the naming-lore rule)

### C. docs & the public face

- [ ] `index.html` crew list: add moon/mooncc + reef (pulchritude's creature already moved — `5b3f84bf`). Probe examples against `out/host/love`, don't write from memory. Keep README's roster in sync when these land
- [x] ~~`crew/README.md` roster~~ — DROPPED (gwen 2026-07-14). The file was deliberately deleted in `c21a0de0` (site consolidation → one static `index.html`); the roster lives in `index.html` now, not a per-crew README
- [ ] blue paper (`tools/blue.l` → blue.md/html) — mention the toolchain + vcs if in scope
- [x] `README.md` full crew roster — expanded the curated ain/bao/inle sample to the full 13-member roster mirroring `index.html` (gwen chose full roster over aicc+reef-only, 2026-07-14). aicc/moon + reef join both README + `index.html` together at the crew-list refresh above
- [ ] **bench.html** — restyle to match the rest of the site (style.css / index.html look-and-feel)
- [ ] **regen the bench tables** — re-run the benches, refresh the numbers baked into bench.html
- [ ] **regen wasm** — rebuild `wasm/love.js` against the release binary (currently dirty in the tree)

### D. the gate

- [ ] `make test` green ×3 (host + love0 ×2, + test_proof + test_gen)
- [ ] `make test_slow` (proofs, gc/glaze/sat/holo/phos, tool diffs, arm64 + qemu kernel + wasm)
- [ ] `make valg` clean, `make vmret` green
- [ ] version stamp (`love_version.h` / `force_version`) — currently `9b45d8d2-dirty`

### E. the cut

- [ ] commit the working tree (currently dirty: prel/gen/index + the two new docs + uu promotion)
- [ ] `post` → `main` → `tau` (`git push tau post` per house habit)
- [ ] github + codeberg mirrors
- [ ] `index.html` on Pages

---

## appendix — the phos → lux rename plan

A pure token swap (`phos`→`lux`) plus file moves. Verified tree-wide: no false-positive
substrings (only compounds are `phosui`→`luxui`, `phosfiles`→`luxfiles`, both correct). The
C nif is `connectu` (not `phos`), so C idents are untouched. The uu **model** names stay
`uuwm`/`wm2uu` (generic "wm") — only phos *source references* inside them swap.

**1. move the files (git mv):**
- `crew/phos/` → `crew/lux/` (core/layout/wire/ewmh/manage/keys/config/law/sigs + `phos.l`→`lux.l`)
- `host/phos.c` → `host/lux.c`
- `test/host/phos.l`→`lux.l`, `phosui.l`→`luxui.l`, `phosui-probe.l`→`luxui-probe.l`
- `doc/phos-port.md` → `doc/lux-port.md`, `doc/proto/phos.l` → `doc/proto/lux.l`

**2. swap `phos`→`lux` in content** (the noms phos-cell/st/!/sock/session/moor/display/auth/
tags/keymap/startup/border/colors/dispnum, all in the same sweep so sibling cross-refs stay
consistent): `crew/lux/*.l`, `Makefile` (target `test_phos`→`test_lux` in .PHONY + test_slow +
the rule + its `crew/phos/law`→`crew/lux/law` grep string; `hostnif_tests` paths;
`phosfiles`→`luxfiles`; `bin/phos`→`bin/lux` rule + install list; the uuwm-rule comment path),
`host/lux.c` (2 comment lines only), `test/host/{lux,luxui,luxui-probe,haven,pier,drm,overlay}.l`,
`crew/haven/{haven,metal}.l`, `crew/quay/pier.l`, `CLAUDE.md`, `index.html` (roster line),
`doc/{cc,lux-port,proto/lux}`. Runtime socket path string `/phos-<n>.sock` → `/lux-` (surface).

**3. the delicate one — the uu proof bridge:** in `tools/uuwmgen.l` + `tools/wm2uu.l` swap the
input paths `crew/phos/{core,sigs}.l`→`crew/lux/` and the `phos-sigs` binding→`lux-sigs`; KEEP
`uuwm`/`wm2uu`. Then **`make uuwm`** to regenerate the committed `test/uuwm.l` (else `test_uuwm`
goes red — drift gate). Confirm `tools/uu2coq.l`/`uu2lean.l` gather-lists don't name the path.

**4. build + gate, in order:** `make out/host/love` (phos/lux is baked into no core, but `bin/lux`
+ tests rebuild) → `make uuwm` → `make test_lux` → `make test` ×3 → `make test_slow` (test_lux,
test_uuwm, test_uukind, wasm, kernel, arm64) → `make valg` + `make vmret`. The X UI layer
(`luxui`) needs a display — run under Xephyr/Xvfb per [[x11-wm-spike]], not the portable gate.
Live smoke: run `bin/lux` under Xephyr (gwen's daily WM). Land as ONE rename commit; `push tau`.

**5. after landing:** update the memory files that name phos ([[x11-wm-spike]], [[pluggable-types]],
[[naming-lore]] already done) + the index.html roster line (name lux, keep 🦐 mantis shrimp,
one-line `<dd>` — creature + role, no prose).

## open decisions (need gwen)

1. **arm64 scope** — DECIDED: does NOT block. Rung D (nolibc/mksys) best-effort into this cut, ships either way
2. **reef's minimum viable surface** — TENTATIVE: the `record · sync · log · diff + hatch` verb set (gwen
   2026-07-14); to be CONFIRMED with the patch thread once it finishes its current heavy proof work
3. **the names** — moon/mooncc landed (was aicc), reef/coral chosen (was tree), pulchritude→centipede, kore for au, phos→lux — all standing, none frozen.
4. **precedence in-scope** — DECIDED: YES (gwen 2026-07-14; prel-only, conservative extension, no C)
5. **namespaces scope** — DECIDED: no sealed-tablet feature; module books become lookup-only closures over
   private tablets (closes phase 3's named tail), abyss/scoped-layers held. names() curation trim optional
