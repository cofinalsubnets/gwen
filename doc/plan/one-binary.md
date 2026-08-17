# plan: one binary in the tree

**THE CLAIM: the build tree manufactures one love too many of everything.** The
same crew source is evaluated and baked FOUR times into four containers --
kore.image (2.9 MB), mooncc.image (5.2 MB), mooncc0.image (2.4 MB), and the dist
seed's layered chain -- and kore.image ∩ mooncc.image is most of both (text, u,
asbook, the whole holo). The dist artifact already IS the union: one multi-call
binary, `love kore|mooncc|sh|cook|sb|..`, one layered image, the verb rail. The
tree should build that shape ONCE and test it, instead of building a thin love
plus a shim-and-image pair per app and testing those.

The end state, per environment:

- **love0** (cc-built) -- unchanged: breaks the self-host circle, lcats the
  headers, bakes mooncc0.image, drives mooncc over the TUs, lays sys.o, runs
  its own corpus lane.
- **out/host/love** -- the same link as today, but its bake is the LAYERED crew
  bake (the dist chain: docs prefix, then the rest), so it answers `love kore`,
  `love mooncc`, `love sh` itself. kore, kore.image, mooncc, mooncc.image, the
  lush/sb shebang cats, the sh/diff symlinks: all retired.
- **mooncc0.image** -- stays, and is the ONLY standalone image left. An image
  keeps its binary's own layout, so love0 must bake its own; the mooncc cat
  stays alive for exactly this.
- **the seed** -- stays a separate link (same $(moon_o), plus the src blob and
  readme), because the blob depends on the tarball and the tarball re-stages on
  any index change: that cost must not enter `make host`. After this arc the
  tree binary and the seed differ by two objects and nothing else.

What the machinery already grants: the image chain landed (doc/plan/image-chain.md)
so the layers cost their difference, a verbed spawn wakes only its prefix
(`love libra` 23 ms against the full image's 107), and `bake -L` + the picker
are the dist artifact's tested path -- this arc makes the tested path the only
path.

## the rungs

**Rung 0 -- measure before moving. CLIMBED, and the numbers ruled.** The layered
bake on a copy of out/host/love: **12.0 s** whole-chain (docs 7130 KB prefix ->
12464 B derived; rest 8012 KB whole; binary 9.2 MB against the plain bake's
2.5), where a kore.image bake alone is 3.5 s -- so a kore-only edit pays 8.5 s
more, and a compiler edit (already a ~2 min TU cascade) pays noise. Wakes,
hyperfine: bare 19.6 -> 104 ms; `kore true` 51.6 (wake shim) -> 104; `mooncc
--version` 74 -> 105; `libra` 22 (the docs layer earns its keep). The picker
question: `ai_baked_pick` with NO verb answers the LARGEST entry, so a bare
`love` pays the full-image wake. chosen (revisable): **(a) live with it** --
the corpus egg-boots under LOVE_NO_IMAGE and never sees the 104 ms, and the
follow-up door stays open: (c) a default-claiming entry so bare `love` wakes a
thin prefix, one line in the picker, when the loop feels it. Faces verified on
the chain binary before any edit: `love kore sed`, `love mooncc t.c -o t`
(exit 42), bare `-e` finds kore-main and moon-run on the book.

**Rung 1 -- the collapse.** The docs/rest cats move to $(ho) and serve the host
bake: love.baked's recipe becomes `$< bake -L .docs-cat.l:libra,help -L
.rest-cat.l` (the candidate lane rides the same pattern rule and MUST get the
same layers). Retire $(ho)/kore, kore.image, mooncc, mooncc.image; test.mk's
korerun/moonrun become `$(mw) kore` / `$(mw) mooncc` (⚠ the root exports
LOVE_NO_IMAGE=1, so every verb site needs the clear -- mw already carries it).
KEPT, revised from the first cut: the korefiles roster (distro.mk cats it over
love-raw), moonfiles (mooncc0.image's cat), and $(ho)/lush + $(ho)/sb -- source
cats, no image to retire, and cook's gate uses lush as a literal SHELL. The
argv0-dispatch smokes (kore.sh's diff/sh symlinks, nest.sh's PATH-fronting lane)
lay their own two-line shim, which is the distro's own mechanism anyway. Gate:
`make test` + test_kore + test_moon + test_cookdiff + test_seat + test_vi.

**Rung 2 -- dist reuses the tree's chain.** dist_seed bakes from the same $(ho)
cats (today's out/dist twins retire), so the seed is: relink + src blob +
readme + the identical bake. distfiles remains THE roster; docsfiles its
prefix. Gate: test_dist + test_distboot smoke.

**Rung 3 -- install and distro flatten.** mk/install.mk drops
lib/love/{kore,mooncc}.image and the wake shims: bin/kore, bin/mooncc, bin/sh
and the applet farm become argv-shims (or symlinks, if cli.l learns argv[0]
dispatch -- a separate, optional rung) onto the ONE installed binary.
mk/distro.mk's bin/ points at love the same way. Gate: make install into a
scratch prefix + the distro smoke.

**Rung 4 -- the gates and the edges.** test_wake/test_imgchain re-aim at the
default path they now describe (nothing standalone left to wake but
mooncc0.image); hue2vim/syntax.vim asks a host whose book now carries the crew
-- verify the vocabulary it paints is the SHIPPED one, or point the generator
at the layer that is.

## what the climb found (rungs 1-3 CLIMBED 2026-08-17, every test_slow phase green)

The collapse landed whole -- rungs 1+2+3 in one motion, since install.mk and the
dist lanes shared every seam -- and the re-aimed gates are the discovery story:

- **the mw lane now judges the crew book, and it convicted.** Three findings, in
  escalating order: (1) core/mx.l's generators under $(mw) captured KORE's
  two-arg `join` (mx-h came out a curried partial -> an EMPTY mx.h), so mxlay
  moved to the EGG lane -- core tables want the boot vocabulary. (2)
  crew/moon/cpp.l's `(flush buf macs out)` shadowed the core port flush and
  broke bao's pty pump -- renamed tokflush. (3) test/spec.l's bit-law census now
  swept 120 crew predicates that follow the ()-false idiom -- scoped to the egg
  lane (love-image on the book is the woken marker). The remaining SHADOW
  CENSUS, measured (show modulo mint serials, egg vs warm): `lof` `vof` (cpp.l's
  token accessors over uu's), `shell` `subst` (cook's over the spawn + kanren's),
  `walk` (kore's fs walk over kanren's). All pre-date this arc ON THE SHIPPED
  ARTIFACT -- the seed's bare book always carried them -- the tree just never
  tested that book. Renaming them is the BOOK-HYGIENE rung, open.
- **test_hostnif had not run since the + flip, and it was a nest of byte-law
  riders**: test/host/net.l's rdline (its comment still CITED the retired law),
  manifest/cb/berth's rowtx folds, luxui-probe's `("" + ch)`, crew/lux/wire.l's
  x-rstr + x-getclass, crew/quay/berth.l's reply fold, lib/json.l's ENTIRE
  writer (every quote/bracket/comma was a byte append -- json-show wrote "" for
  "[]"), crew/tls/bytes.l's hex, mk/tools/hue2vim.l's vim-brk -- and TWO LINES
  cc9e84ee's perl had EATEN AN OPERAND from (crew/manifest's column letters,
  `(+ 97 cx)` -> `(+ 97 )`). All fixed with string spellings; hostnif ALL-GREEN.
- **LOVE_NO_IMAGE discipline**: the root's corpus export means every verb site
  carries the `LOVE_NO_IMAGE=` clear -- mw has it, korerun/moonrun ride mw, the
  gate scripts' runners spell it, KCC spells it, the INSTALLED verb shims spell
  it (an inherited egg ask read `mooncc` as a filename in test_nest), and the
  two package tools go through `env` because a bare assignment prefix is not
  recognized after $var expansion.
- **kore.sh's `env print vs GNU`** wanted the oracle under korerun's own prefix,
  or the two children compare different environments.

Still open here: the book-hygiene renames above; option (c), a default-claiming
entry so a bare `love` wakes a thin prefix (today it pays the ~104 ms full
wake); test_wake/test_imgchain re-aim (both still green as-is); distro.mk's
argv0 farm could point at the one binary once cli.l learns argv[0] dispatch.

## ⚠ traps this plan already knows

- **images are binary-specific.** mooncc0.image is baked BY love0 and only
  love0 can wake it; no sharing with the mooncc-built love, ever.
- **membership is an input make cannot see.** Every moved cat keeps its .list
  content-stamp guard, same as today's five.
- **the blast radius widens.** Today a kore edit rebakes kore.image and the
  compiler shim survives; after rung 1 any crew edit rebakes love itself. The
  in-place bake renames over a fresh inode so running sessions survive on the
  old one -- but a concurrent session's NEXT spawn gets the new binary. Same
  hazard class as a core/love.c edit today; now it includes .l edits.
- **bundled_love must not see the tree's own binary.** After rung 1
  out/host/love IS bundle-shaped, but the bundle test keys on ./bin/love
  (a seed-laid tree), not on out/host -- keep it that way, or the tree's build
  would try to compile itself with the binary it is rebuilding.
- **the seats.** kore.l and lush's main.l fire on basename; the -L bake loads
  cats under a neutral name so seats stay quiet in the image. That is the dist
  lane's existing behavior -- rung 1 inherits it, test_seat checks it.
