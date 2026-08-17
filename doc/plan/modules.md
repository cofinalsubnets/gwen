# plan: modules decouple from the filesystem

**THE CLAIM: module = file is a python idea riding a dead design.** The coupling
came in with the ~/.love nest -- a love finds its library on disk, so a module
had to BE a file and the loader had to walk paths to find it. The seed binary
made that obsolete: the one-binary arc (doc/plan/one-binary.md) showed the
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

**Rung 1 -- ai_defn's module target.** struct ai_def gets `.mod` (a string,
NULL = book); ai_defn find-or-makes the module tablet in g->mods (sharing
lvm_mods' lazy-create -- ai_defn runs at boot BEFORE prel, so the registry
must be creatable C-side) and mapputs there; an AiModNif(mod, nm, fn) variant
so the section drain needs no grouping. Proof: one host app's nifs move into
its own module. Gate: `make test` + that app's gate.

**Rung 2 -- the crew modularizes.** One app at a time, moon first (the
collision hotspot AND the love0 lane -- mooncc0.image bakes moon's cat under
love0's single-pass c0, so the macro-before-use ordering is proved on the
hardest lane by the first wrap). The wrap is per-file and the cats are
untouched: a module form rides a cat like any form. The book fence gate lands
with the first wrap: the shipped bare book stops carrying crew names, and the
measured shadows dissolve. Gate: per-app gates + test_slow.

**Rung 3 -- the search collapses.** libsrc drops the seat rungs; cwd `lib/`
stays with a retirement note; mk/install.mk's lib tree and the nest resolution
contract retire in coordination with the self-host arc's `love up` story, not
as a side effect here.

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
