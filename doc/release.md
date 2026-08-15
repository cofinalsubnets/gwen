# cutting a release

The standing procedure for a public cut. It is deliberately short: nothing here is
release-specific, so it does not rot between cuts.

## the gate

Green in this order — each tier is a superset of the one above it, and the fast one is not
evidence for the slow one.

- [ ] `make test` — ⚠ read the summary, not the exit code: host and love0 must **each** print the
      zz-fin "tests pass" line (love0 twice). A silent reader stop exits 0.
- [ ] `make test_slow` — the proofs, gc/glaze/sat/holo, the crew apps, tool diffs, arm64, the qemu
      kernels, wasm, the UEFI door.
- [ ] `make test_extra` — the really slow tier; this is the merge-to-main gate.
- [ ] `make valg` clean, `make vmret` green, `make waits` for the blocking roster.
- [ ] `make lint` — libra over every tracked `.l`.

## the artifacts

- [ ] **version stamp** — `out/lib/love_version.h` carries the build's vc id (surfaced as
      `love-version`); `make force_version` refreshes it. A `-dirty` suffix means the tree was not
      committed first.
- [ ] **the dist artifact** — `make dist` bakes `out/dist/love-<arch>`, the one-file download door
      (`love up` defaults CC to the artifact's own `mooncc` verb, so it needs no ambient
      toolchain). `make test_dist` and `make test_up` gate it.
- [ ] **wasm** — rebuild `wasm/love.js` against the release binary.
- [ ] **the benches** — re-run and refresh the numbers baked into `test/bench/bench.html`.
      ⚠ Bake first: a bare relink leaves `love` unbaked and its startup is an egg boot, which
      wrecks every timing number.

## the public face

`make site` renders `README.md` + `doc/` through papel into `out/site`; `make site-serve` is the
dev server. So **every file in `doc/` is published** — read it as public documentation, not as
notes.

- [ ] `index.html` — the crew roster, and the examples with their `; answers`. ⚠ Probe every
      example against `out/host/love`; never write one from memory, and re-probe on any rename or
      semantic change.
- [ ] `README.md` — keep its roster in step with `index.html`.
- [ ] the man pages (`doc/love.md`, `doc/cook.md`, `doc/lush.md`, `doc/kore.md`) — `tools/mkman.l`
      renders them; check the `@VERSION@` substitution landed.

## the cut

- [ ] commit the working tree.
- [ ] `post` → `main` locally, then push: `git push tau main` first, then the mirrors
      (`github`, `codeberg`). ⚠ `post` is local-only and is never pushed.
- [ ] `index.html` on Pages.

## naming, when a cut carries one

A rename goes all the way down in one commit: the sources, the crew directory, the Makefile
targets, `bin/` entries and install lists, the docs, `index.html`, `README.md`, and the generated
files that name a path (`make uuwm` regenerates `test/uuwm.l`, whose drift gate reddens
otherwise). ⚠ Python `\b` sweeps treat `-` as a word boundary, so kebab names with capital
segments mangle — sweep by hand.

C and docs embed love that a `.l` sweep misses: `host/main.c`, `free/kmain.c`,
`port/rp2040/main.c`, `port/playdate/`, `wasm/`, and `index.html`. Grep them on every rename.
