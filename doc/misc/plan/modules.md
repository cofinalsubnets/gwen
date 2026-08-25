# plan: modules decouple from the filesystem

**THE CLAIM: module = file is a python idea riding a dead design.** The coupling
came in with the ~/.love nest -- a love finds its library on disk, so a module
had to BE a file and the loader had to walk paths to find it. The seed binary
made that obsolete: the one-binary arc (doc/misc/plan/one-binary.md) showed the
registry `g->mods` is C-held, GC-rooted, and IMAGE-CARRIED -- the warm binary's
`(from ())` answers fifteen modules with no filesystem anywhere, and `use`
already short-circuits on a registered name before any path walk. Modules are
already binary-internal in every mechanical sense; only their DEFINITION still
insists on a file. This arc finishes the thought:

- **(module 'nm form ..)** -- a macro, so text anywhere can define or REOPEN
  any module: open nm's layer (the registered tablet if nm is known, a fresh
  one otherwise), eval each form there exactly as the loader would, leave
  registers. A file joins a module by changing its head form, not its path.
- **the search collapses to the baked table** -- ai_libs() is the principled
  "baked-in libs" door; libsrc's two SEAT rungs (`<seat>/../lib/`, its `love/`
  subfolder) are the nest legacy and go. cwd `lib/` stays at first for the dev
  tree, with this note as its retirement paper.
- **ai_defn grows a module target** -- a `.mod` field on struct ai_def (NULL =
  the global book, today's behavior) registers a C nif under a module instead;
  with reopen, an app's C half and .l half land in ONE module.

The crew shadows the one-binary arc measured (`lof` `vof` `subst` `shell`
`walk`) dissolve here, not by rename: clean the namespace by modularizing,
don't avoid collisions by renaming.

## the rungs

**Rung 0 -- the module macro.** In prel, next to the loader: `mopen` (enter,
reopen-aware: a registered name gets its OWN tablet back with the charm
re-pinned; the pre-enter chain and name ride charm key 31337 as ever), and the
`module` macro expanding `(module 'nm f ..)` to a run over quoted forms --
each form compiles at ITS eval, after the open, so bindings defglob into the
module layer exactly as the loader lane does (one form compiled before the
open would defglob past it). Reopen is the one new semantic: leave re-pins the
same tablet, idempotent. Laws in test/spec.l's module block: define, member
reach, no leak to the global book, reopen sees the old members and extends the
same tablet, `use` splices it, anonymous `(module () ..)` is a scratch layer
that registers nothing. Nothing else moves. Gate: `make test`.

**Rung 1 -- ai_defn's module target. CLIMBED.** The module is the CALL's, not
the row's: ai_defn grew a fourth parameter (NULL = the book, so struct ai_def
stays two words), and ai_modtab (core/love.c) pushes the found-or-made module
tablet where the book map would sit -- the binding loop is one loop either
way. The registry is made C-side at boot (the drain runs BEFORE prel) and
FOUND over a woken image, where the drain re-pins the current addresses (the
same freshness the book lane always had). AiModNifs("mod", table) is the
section spelling: one exported struct ai_mod row = one (module, def table) =
one ai_defn call at the drain. The proof: host/mem.c's peepw/pinw moved into
module 'mem -- off the bare book everywhere -- and flat.l's presence probe
collapsed from the out-of-band (names ()) dance to an ordinary `(from 'mem)`
read. test/host/modnif.l holds the laws, including the C+.l one-module story:
(module 'mem ..) text reopens the drain-made tablet and its .l member reaches
the C nif. What the climb found: holo's linker resolved only its two
hand-laid brackets -- it now grants GNU ld's own generic rule, __start_X/
__stop_X over any named lane's laid extent (link.l's ld-extents, threaded
through cx), which the gcc lane always granted for free. (A suspected
mooncc gap -- statics dropped from named sections -- was differentialed
and CLEARED: file-scope statics lay their bytes, symbol local, as C asks.)

**Rung 2 -- the crew modularizes.** One app at a time, moon first (the
collision hotspot AND the love0 lane -- mooncc0.image bakes moon's cat under
love0's single-pass c0, so the macro-before-use ordering is proved on the
hardest lane by the first wrap). The wrap is per-file and the cats are
untouched: a module form rides a cat like any form. The book fence gate lands
with the first wrap: the shipped bare book stops carrying crew names, and the
measured shadows dissolve. Gate: per-app gates + test_slow.

*Moon CLIMBED.* The seven cat members (floor lex cpp parse gen mksys moon)
each wear a two-line wrap -- `(module 'moon` before the first form, `)` after
the last -- and nothing else in the files moved: gen.l's `(use 'pat)` rides
INSIDE the wrap now, scoped to the module's own compile, and moon.l's tail
still pins mooncc/cc into (from 'verbs 'tab), so the verb rail needed nothing.
lof/vof/subst answer uu's and kanren's own on the warm walk now (the id?
probe), cparse and mksys are off the bare book, and the rest layer shed ~1 MB.
What the climb taught: (1) `(: _ (use 'x) ..)` serves LATER BINDINGS in the
same form -- compile is per-binding -- so a one-form -e reads a module through
one use; the macro-needs-the-next-FORM rule is macros only. (2) The consumers
were exactly the -e/-l lanes: the sys.o recipes and package harnesses spell
`((from 'moon '<mksys>) ..)` (ten sh sites + host/build.mk), moon.sh's warm
lane binds moon-run by `from`, and the three in-process gates (gate/clay.l,
moonfuzz.l, clay-g2.l) lead with (use 'moon) -- moon.sh's law cat echoes the
same line between gen.l and law.l. (3) The one QUIET regression was lush's
in-image lane: sh-imgask resolved a word's main as a bare global, so with
moon-run in a module the lane silently fell back to spawning -- and the
byte-compare stayed green, because both sides were the spawn. test_dist's
engagement probe (peep sh-imgc after a real line) is what caught it; the
alias row grew a module slot, ("mooncc" "moon-run" "moon"). A wrap's blast
radius is exactly the (ev 'name)-style RUNTIME readers -- grep for those
before wrapping the next app. Remaining shadows ride the still-bare apps:
`shell` (cook), `walk` (sb).

*Kore CLIMBED -- and it flushed out the macro's one real bug.* The eleven
kore files (the text/u floors + the applets + kore.l) wear the wrap; the
wrapped moon files lead with a scoped (use 'kore); the three still-bare REAL
consumers (vi/config.l sb/merge.l holo/copy.l) carry an ambient (use 'kore)
head until their own wraps convert it -- cook and lush looked like consumers
and are not (cook defines its own lines/chomp; lush's reads are runtime
doors), and a head there BROKE the egg lane's file loads (papel loads cook;
'kore has no lib entry off the image), which test_seat caught. lush's resolver
grew the applets' home-module fallback (sh-imgask tries the book, then
(from 'kore nm)) and the urun wrap door reads the module -- a miss absorbs
to (), keeping the no-wrap lane. asbook.l stays bare on purpose: its ambient
(use 'holo) is load-bearing for the cats' holo copies (host/main.c:966's
double-carry note) -- that ambience is holo's own wrap to dissolve.
THE FIND: since rung 0 the module macro emitted `(\ ((f ..)))` -- ONE extra
list level -- so a body's forms evaluated as ONE APPLICATION CHAIN: every
form's effects ran and every gate stayed green, but each form's ANSWER was
applied to the next. It surfaced only when kore's use shifted moon.l's chain
so the big form's answer (moon-cc, a bodyless (:'s last value) met () --
udie 2 usage at the mooncc0.image bake, three files from the cause. The fix
is one level ((cup l) whole); the law is spec.l's smt probe: a
function-answering form followed by another form, and the side effect only a
spurious application could leave must not appear. Diagnosis notes that paid:
flush the markers (buffered puts lie about the death point), and make the
suspect call print its ARGUMENT -- ZF-ARG named the next form's answer and
with it the whole mechanism.

*Cook CLIMBED -- the smallest climb, because cook was already module-shaped.*
lib/cook.l symlinks the source, so `(use 'cook)` and papel's want lane
registered it long ago, and cook-main already opens by splicing its registered
self (the guard kiosko copied). The wrap closes the last bare lane, the kore
cat: `(module 'cook` with `(use 'peg)` as its first, now scoped, form -- the
~30 globals (`join` `date` `check` among them) leave the cat's session layer,
and the `shell` shadow dissolves (bare `shell` is bao's repl entry again). One
consumer respell: kore.l's applet rows read `(from 'cook 'cook-main)`. lush
needed nothing -- make/cook are deliberately off sh-imgok (a main that quits
and keeps state). THE SEAM the wrap exposed is the loader's: a path or lib use
enters a wrapper layer, the file's own module form registers its tablet, and
the loader's leave then pinned its EMPTY wrapper over it -- papel's
`(from 'cook 'recipe)` read () off the clobber. The fix is in prel's two load
lanes: capture what the file registered after rdev, and re-pin it over the
wrapper's registration -- a file's own registration wins; the loader's is the
fallback for bare files. The law lives in test/host/loader.l (both lanes,
a (module ..)-wrapped fixture).

*Sb CLIMBED.* The four files wear `(module 'sb`: merge.l's ambient
(use 'kore) head became the scoped use (two of the three middle-state heads
remain: vi/config.l, holo/copy.l), sb.l took one too (lines/udiff), http.l
scopes (use 'dns) for its compile-time `dial` read, and the `walk` shadow
dissolved. lib/sb.l's assembly needed only its comment updated -- the parts
self-register and the loader's re-pin keeps the file-made module; the
standalone script cat registers the same modules in the session. TWO false
alarms the climb paid for, both worth their notes. (1) The consumer sweep
flagged up.l's and source.l's bare `herald` as cook globals gone module-side
-- but herald is a HOST NIF (main.c), visible everywhere; grepping the
CALLERS found cook among them and read as the definer. The "fix"
((from 'cook 'herald), a missing member) was the real break: the absorbed
(() argv) call answers 1, and test_up printed "cook install failed (exit 1)"
with no child at all. Verify a name's HOME before respelling -- grep for the
definer (AiNif, the defglob), never conclude from call sites. (2) One red
test_up at the pre-sb HEAD would not reproduce: three subsequent virgin runs
(two by hand, one under make) built the door whole. The durable rule stands
regardless: wrap work runs the opt-in gates that exercise the app's runtime
lanes BY NAME -- test_up here -- since nothing on the default rosters covers
them. One environmental scare worth naming: the sb script's shebang takes
`love` off PATH, and an installed pre-module love answers
`;; missing module` -- that is the nest re-seat's business, not the wrap's.

*Lush CLIMBED.* The eight parts wear `(module 'lush`; lib/lush.l's assembly
and the catted bin register the same module, and the distro's /lib/sh.l lane
reads it through boot.l's one-use -e. The consumers, sorted by DEFINER this
time: kore.l's sh/lush rows read (from 'lush 'sh-main); find.l's fnmatch
capture rides a scoped (use 'lush); cook's two runtime doors -- cook-glob and
the in-image verdict -- became from-reads (a miss answers (), keeping the
spawn lane), the standalone -l splice registering the module off
lib/lush/glob.l; the host and kernel sh gates splice what their cats just
registered. The climb caught what the QUIET class predicts, twice over:
test_dist's own engagement probes read sh-oneline/sh-imgc/sh-imgfn bare in
their -e forms -- the gate that exists to catch the silent spawn-fallback was
itself the bare reader -- and running test_hostnif by name surfaced the glued
reader's stale downstream law (baotest's dotted-nom check; "1.2.3" is a RUN
now), which had sailed past the default rosters at the merge.

*Vi CLIMBED -- rung 2 closes.* The four files wear `(module 'vi`; core.l and
vi.l lead with the scoped (use 'kore); config.l's ambient head DROPPED
outright -- config reads no kore; the head had been serving the cat's
downstream, and the one lane still leaning on it (test_kore's law cat)
now echoes its own (use 'kore) before law.l, the moon.sh pattern. law.l and
hue2vim.l lead with (use 'vi); lib/hueweb.l splices it for its own compile
(outside-image tools' two path-uses register the same module -- the loader
re-pin); kore.l's vi row reads (from 'vi 'vi-main). Only the still-bare
holo/asbook ambience remains -- holo's own wrap, rung 2's coda, dissolves it.

*Holo CLIMBED -- the coda, and the last ambience dies.* The fourteen crew/holo
files wear `(module 'holo`, so the baked lib entry (holo + native backend +
elf/obj/link as one source) and every cat REOPEN one tablet -- the double-carry
is now a double-write into the same module, and defbackend's join is unchanged.
asbook.l became an ensure-load: `(? (tablet? (from 'holo)) 0 (: _ (enter ())
_ (use 'holo) (leave ())))` -- registered through an anonymous layer, spliced
nowhere. Readers by definer: gen.l (asm-text), moon.l + mksys.l (arbytes,
objelf, the linker) and kore.l (elf64, arbytes, ld-syms, at) lead with scoped
(use 'holo); kore.l's objcopy row and lush's alias row read the module; the
golden/as gates and hueweb-style outside tools splice what their cats
register; mksrc.l's unbaked-love guard asks (from 'holo 'objelf).
THREE FINDS the climb surfaced, each its own law now:
(1) kore.l's verb closure read its own module's kore-main through a
compile-time missing cell -- it only ever worked by bake-environment luck (the
minimal repro fails even on a pre-coda dist whose `kore true` passes). Pin
closures that outlive their module's load read through the registry:
(\ as ((from 'kore 'kore-main) ..)).
(2) copy.l's scoped head killed the LAST ambient splice -- and with it the
warm image's bare uread that gate scripts (clay.l, moonfuzz.l) had been
leaning on without saying so. The readers say (use 'kore) now; nothing else
leaned on the ambience, which is the coda's whole point.
(3) love0's c0 is single-pass: `(: _ (use 'x) body)` does NOT serve the body
there (the whole form compiles before the use runs) -- love0-lane probes spell
(from 'mod 'name), never one-use. moon.sh's asm-text probe is the law.

**Rung 3 -- the search collapses. CLIMBED.** libsrc walks the baked table and
cwd `lib/` -- the dev tree's lane, kept with its retirement note until the
tree stops being special. The seat rungs (`<seat>/../lib/`, its `love/`
subfolder) are gone, and `selfpath` leaves the loader with them: an artifact's
modules are baked or image-carried, never found beside the binary. The
installed lanes proved it whole at the time -- binaries that never touch the
walk they used to need. loader.l's seat laws flipped to retirement laws --
fixture where the rungs looked, the use scares no-module under trap, nothing
registers.

## ⚠ traps this plan already knows

- **splice, not nest.** A `(: ..)` in VALUE position binds locals (mx-h's nl2).
  The wrap is the file's top form changing HEAD -- `(:` becomes nothing, the
  file's forms become module's operands -- never the old form passed whole as
  a value.
- **one form, one compile.** Bindings inside a single form resolve their
  defglob target when THAT form compiles. The macro must defer each body form
  to runtime (quote + ev) so the open precedes every compile. Corollary: a
  macro defined in one body form is live for the NEXT form, not its own --
  the prel-macro-ordering rule, unchanged from file loading.
- **enter/leave are mopped at birth.** The macro's machinery compiles in prel
  while they are still bound; the emitted form carries the run function as a
  VALUE (the pins-macro idiom), never the names.
- **the loader stays.** use's string and slashy lanes, rdev, the baked table:
  untouched until rung 3, and cwd lib/ survives even that. Registered names
  already win before any walk, so rungs 0-2 change no load order.
- **reopen is live surgery.** The reopened layer sees the module's members as
  globals during the reopen (zz was 41, (+ zz 1) answered 42) -- that is the
  point, and it is the same divergence hazard `from` already documents:
  consumers compiled earlier keep what they folded.

## coda -- the baked table goes too

Rung 3 kept `ai_libs()` as "the principled baked-in libs door". It was not a
door, it was a second spelling of the registry. Once every baked source wore
its own `(module 'nm ..)` head, the table's whole job was to hand `use` a text
that would declare the very name the table had just been keyed by -- so the
frontends eval the cat at boot instead, the modules register themselves, and
every `(use 'x)` in a boot is a pure splice.

What went with it: `struct ai_lib`, `ai_libs()`, `k_libs`/`host_libs` and
seat.c's picker, the `lib` nif, and the whole `struct ti` C-string port
(`ti_athand`/`ti_readn`/`ai_ti_vt`) that existed only to read a table row.
Eight frontends lost their table and their ~20 `src_*` arrays for one
`src_mods` apiece. −113 lines, −4360 bytes of binary, image boot unchanged.

- **the laziness was never spent.** The table's stated worth was that "an entry
  nothing loads costs a row and no heap". Every row in every frontend turned
  out to be loaded -- by the C boot, or by a cat that runs at boot or bake.
  `peg` looked lazy on the host and is not: it rides the crew cat into the
  image (a real peg compile is 11M insns; `use 'peg` on the baked binary is
  1.0M, which is a splice). The kernel's `holo`/`peg` rows exist *because* the
  kore cat opens with them. So dropping the table cost nothing that was being
  used -- but it does mean **a header a frontend includes is now a module it
  COMPILES at boot**, where before it was a row it could ignore. That is why
  the generated headers stay per-module: the frontends take different subsets
  (host 10, kernel-test 7, kernel-ship 5, wasm 6, playdate 3, teensy 1), and
  one combined header would put all 116K of module source on a board that
  wants 7K of it.
- **a module has to be self-contained now.** The cat evals with nothing
  spliced, so a module reading another's names bare misses. `overlay.l` was
  the one -- `subst` through the registry, `unify`/`ufail?`/`var` bare -- and
  it says `(from 'kanren ..)` for all four now. ⚠ the symptom is a `;; missing`
  printed during the BUILD and no failure: a closure captures its free globals
  at creation, so the scare lands at the define and the build walks on.
- **glaze is the one text a flat cat cannot hold.** It folds `assemble` at its
  own compile, so holo must be SPLICED while it evals -- which the boot
  arranges. It also declares itself in host/main.c rather than in emit.l or
  auto.l, because neither file is the module: the pair is.
- **renumbering `image_immortals` is a wire-format change.** Dropping
  `ai_ti_vt` shifted every index after it, so `ImageMagic` moved to `AISNO05`.
  ⚠ `test/gate/bakerep.sh` greps the magic's SPELLING to corrupt a header --
  an undocumented coupling until it failed the gate.
- **love0's build tools pay for the eagerness, once per header.** Registering is
  compiling, so a love0 startup spends ~0.96G insns on the ten modules before it
  reads its first file, and lcat runs once per header: `make lib` over a fully
  touched tree goes 3.80s -> 5.50s. A batched lay took that to 2.50s and was
  REVERTED -- it needed a stamp with the headers hanging off it, and a stamp is
  not a thing this tree wants for two seconds. What the revert kept is the
  compare before the move (out/lib/corpus.list's discipline), so a touch that
  changes no bytes now rewrites nothing and rebuilds nothing, where before it
  rewrote every header and relinked love0. The standing option should holo's
  share ever matter: `crew/holo/holo.l` into `$(moonfiles)`, ahead of asbook.l
  and the backends already there, so that cat carries its own core and holo
  leaves love0's text.
