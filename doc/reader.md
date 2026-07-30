# the reader off C -- THE PLAN

the reader is the last big island of C that is not the runtime: 470 raw / 351
code lines of `love.c` (5.7% by code line), a hand-stacklessed parser carrying a
grammar that has outgrown it. the arc: a tiny bootstrap C parser `p0` that reads
a PURE LISP subset, and the real reader `p1` written in love on top of it --
`c0`/`ev` again, one layer down. this ORIENTS; the laws live in test/reader.l,
and every doubt settles by probing `sound`. drafted 2026-07-29.

## what the C reader does today (the inventory)

the contiguous parser is love.c:4202-4511, plus `zgetc`/`zungetc` (3182),
`lvm_sound` (4125), `intern` (5653) and `ai_big_read_dec` (7562).

**structural, and all p0 needs**: delimiters, `;` and `#!` comments, `"…"` with
escapes, atoms, `'` quote.

**everything else, which p1 takes**: the operator run (`op_break` 4257,
`ioread1op` 4268); the valence law's `mono` wrap (4346-4390) with its head-fusion
rule and the `:`/`?` suppression; the constructor wraps `` ` `` `#` `@` `~` and
their SPLICE rewrites (`splicesym` 4292, deliver loop 4412); the `~` twin-vs-conj
peek (4307); the empty-collection direct-nif rewrite (4399); the comma datum
(4318); the trailing-`-` shed (4278); `[`/`{` synonyms; the prime inside names;
the number tower (all three integer bases at full precision via
`ai_big_read_dec`/`_hex`/`_oct`, floats via `am_strtod`); the `ieee-inf` named
literals.

**what it does NOT do, and this is the load-bearing surprise**: it does not
resume. on an unfinished shape it *discards the partial parse* -- love.c:4148-4153
resets `c->sp` to the depth recorded before the call, and the bytes are already
consumed (test/io.l:104-106 asserts the port comes back exhausted). `more` is only
a signal that the caller must own the text and re-parse the concatenation, and
every consumer already does: bao re-reads the **whole editor buffer** from byte 0
on each Enter (bao.l:274), lux and haven carry pending text forward and re-tap it
(lux.l:103-113), and `reads`/`use`/`-l`/cook/kore/salt simply end the stream. so
**p1 owes only "answer more", and p0 owes nothing.**

## the rungs (each green and useful on its own)

### 1. settle and state the laws -- LANDED 2026-07-29

a reader replacement is tractable only with an exact-equivalence oracle, and a
semantic change in flight destroys it: every diff becomes a hand triage of "the
new reader is broken" vs "right and deliberately different". so the laws get
settled first, and **test/reader.l** is the deliverable -- the conformance suite,
simultaneously the spec, this rung's gate, and the acceptance test for 5-6.

landed with it: `+` and `-` stop being a special case in the run lexer. they were
held out of the operator-run path because they also lead numerals, and the
carve-out was wider than its reason. now they are runs like every other
punctuation, and the ONE exception is that a numeral has to start somewhere -- a
digit (or a `.`, for `-.5`) right after them makes a NUMBER. so `--5` is `-(-5)`
and `-x` is negate x, symmetric with `!!5` and `!x`.

⚠ the kebab law was never the reason `-x` was a name. it governs the INSIDE of a
name-led token (`old-thing`, `nl->sp`) and spec.l defines it on tokens whose
LEADING char is alnum/`_`. "`-x` a name" was a separate provision riding alongside
it. measured before the change: over every tracked `.l`, comments and strings
stripped, 64 distinct `+`/`-`-led code tokens, 61 of them numeric literals (940
uses); the other three are `+@`, `->` and `-?>`, all standalone. bare `-foo`
tokens: **zero**.

### 2. an integer literal reads the same in every build ✅ LANDED

writing test/reader.l turned up **three** disagreements out of one path, all of
them the hex lane going through `strtol`: hex vs decimal, build vs build, and
width vs width.

```
                                    0xffffffff80200000 read as
out/host/love  (mooncc + nolibc)    -2145386496            (wrapped)
out/host/love0 (system cc + glibc)  9223372036854775807    (saturated)
wasm                                neither -- 32-bit `long`
18446744071564165120 (decimal)      the value               (promoted to a big)
```

so the *same source text* was three numbers. and the tree had voted around it
three times -- love/glaze/emit.l and test/holo.l wrote the hash multiplier in
decimal with the hex named only in a comment, and crew/holo/link.l ran the kernel
address split through an unsigned door under a five-line explanation, because
`0xffffffff80200000` reaching a layout that DIVIDES would overshoot by a page and
nothing downstream would look wrong. port/inle/klink.l's addresses were correct on
the default binary and **wrong on love0**, silently.

one cut closes all three: **every base gets the reader decimal already had.**
`is_hex_int`/`is_oct_int` → `ai_big_read_hex`/`_oct` (love.c), a radix parameter
over `ai_big_read_dec`, so a literal is a fixnum / box / bignum by its VALUE and
never by what a libc did with an overflow. all three workarounds are gone; the
law is asserted in test/reader.l's numbers section.

**and the three predicates turned out to be EXHAUSTIVE** over what a base-0
`strtol` accepted whole, so the reader stopped calling it: a token that is none of
them (`0x` with no digits, `08`, `1e5`, `abc`) is one strtol would have abandoned
mid-way anyway. checked, not argued -- the branch was instrumented and stayed
silent through the corpus and a 44-token adversarial sweep. **that took the last
caller of the freestanding `strtol` with it**, so `libc/ctype.c` is deleted and
`libc/str.c` is down to `strlen`. see doc/libc.md.

**beside it, both libcs we own SATURATE** like glibc/musl/newlib -- gwen's call,
and the right one: wrapping destroys the fact that it did not fit, saturating
hands it back. the reader no longer observes it, so it is gated where it belongs,
by `test/cc/105-strtol.c` in the mooncc battery: gcc links glibc, mooncc links
nolibc, the exit codes must agree.

**still open, and independent:** `&` `|` `^` `<<` `>>` on non-negative bigs,
limb-wise in `bit_slow` (love.c:6289 -- it widens to a *sun* and drops bigs to
nil). bigs are sign-magnitude limb arrays (love.c:356-359), so non-negative is the
easy half and covers every real use; negatives should keep refusing but **scare**
rather than answering `()`, the plausible-lie pattern. nothing in the reader needs
it now -- the big literals in the link path already flow through `%` and `//`, not
bit ops -- so this is a tower-completeness rung, not a blocker.

### 3. the reader differential ✅ LANDED

**test/host/rdiff.l**, in `hostnif_tests` so it rides test_slow. 1.8 s.

the SOCKET is one binding, `rd-test` in test/00-init.l, defaulting to `sound`.
test/roundtrip.l and test/fuzz.l read through it now, so rung 6 points every
print→read differential in the tree at p1 by rebinding that one name.

the INPUT SET is the tree: every tracked `.l`, walked off `readdir`, read form by
form -- 297 files, 1318 forms, carrying every construct the grammar has in anger.
that beats a generator and it was already here. two laws ride it: **A/B** (the
reader under test agrees with the C reader, form for form) and **print→read**
(`(rd (show f))` is `(f)`).

A/B is vacuous until p1 exists -- both legs are the same reader today -- so the
rig is gated on a **fault injector** instead of on the diff being green: `rd-hurt`
is the C reader with one form spoiled part-way through a file, and the sweep is
REQUIRED to catch it. that plus checked floors on the file and form counts is the
proof of work, and it is the honest version of it: green is also what a rig that
never ran prints, and a comparison that *cannot* fail prints green forever.

**its first run settled rung 1's one open question.** eighteen forms across seven
files failed print→read, all the same divergence: `#()` and `@()` came off the
reader as `(tablet . (0 . nil))` -- a `nil` tail where every other reader list
uses `ZeroPoint`, love.c:4452 against 4462 and 4466 six lines below it. rung 1
had already SEEN this and pinned it in test/reader.l as an explicit non-decision:
"a thing a reimplementation must reproduce or deliberately normalize". what the
differential added was the cost of reproducing it -- these were the only two
forms in the whole grammar that did not survive show → read -- and that settles
it toward normalizing. one word; the two asserts now compare WHOLE instead of
head-and-operand.

⚠ note how little was visible without the diff: the printer shows both tails as
`(0)`, `nilp` is true for both, and either one ends the ap, so **only `=` on two
trees could see it at all**. that is the shape of what rung 6 is up against --
not a crash, a plausible tree nobody can print the difference of. it is also why
`(show x)` is barred as a value test in this arc (rung 0's `-0.0`, doc/libc.md):
twice now the printer has been the thing standing in front of the bug.

### 4. hold prel.l to the p0 subset ✅ LANDED

measured before, code only: `` `( `` ×7, `~(` ×3, `',` ×5, glued mono runs ×3, and
**zero** `#(` or `@(` -- 18 sites on 15 lines. `` `(a b) `` → `(L a b)`, `~(0 1)` →
`(twin 0 1)`, `!v` → `(nil? v)`, `',` → `(intern ",")`. the scan is clean on all of
those now, and on brackets, bare commas, primes inside names and the trailing-`-`
shed besides. so what p0 owes prel is exactly: delimiters, `;` comments, `"…"` with
escapes, atoms, `'` quote, integers (negatives included -- rung 1 made a digit after
`-` the thing that starts a numeral) and **four** float literals (`pi`, `e`, `0.0`
×2). prel.l's header states the rule now, beside the no-infix one.

**the comma cost a design decision.** the obvious shape -- bind `op-cma (intern ",")`
once in the opfix binder chain and mop the nom at birth, exactly how `operators` and
`monadics` live -- **does not survive the mop**: `;; missing op-cma`, three times,
once each for `op-cm?`/`op-take`/`op-drop`, with a zero point where the comma should
be. a mopped VALUE-global is not a mopped closure; opfix holds the tablets by
capture and did not hold this.

⚠ and it is invisible to the gate. it shows up **only on a boot that compiles from
source** -- `LOVE_NO_IMAGE=1` reproduces it, a woken image never does -- so the whole
of `make test` is silent and the place it surfaced was `--bake`, which boots off the
egg in order to bake. (my first guess was the glaze re-analysing those closures
post-birth. wrong: `LOVE_NO_GLAZE=1` is byte-identical. the note in egg.l records what
was measured, not the story.)

so the call is INLINE at all four sites and **`intern` joins ev.l's `pureset`** --
cprop folds `(intern ",")` back to the constant symbol it used to be, measured
identical to the old `',` literal over a 3M-iteration loop.

**and `<<`/`>>` came out of `monadics` while we were in there.** their comment claimed
the rows were earned by SPEED -- one `cuup` call instead of `>` then `>`. three things
against it: the greedy factor gives the identical value either way, and `caap`/`cuup`
are prel closures the compiler inlines, so the row saves no call a timing loop can
see; they were the table's only COMPOSITE rows, every other being one run-PIECE → its
monadic word; and the tree's commonest glued run by an order of magnitude is `<>`
(1835 uses against 71 for `<<`), which never had a row and never wanted one.
README.md already described all four compounds as arriving "by factorization" -- that
is true now.

it also **bought a theorem**, which was not the point but is the best argument for
the change. `tools/spec2coq.l` models `cup` and does not model `cuup`, so spec.l's
`('(3) = >>'(1 2 3))` had always been UNMODELED; factoring the run into primitives
put it in reach, and proof/rocq/gen.v goes 312 translated → 313 (`gen_301`, proved
under test_gen). a composite row was hiding a law from the prover.

### 5. `p0` ✅ LANDED

**`sound0`** in love.c, sound's bootstrap twin: the same protocol, the pure lisp
subset, **30 code lines**. it reads all 53 forms of prel.l exactly as the C reader
does, which is rung 5's whole claim and was true on the first run.

it came out well under the estimate because it does not carry its own lexers. the leaf
scanners are already the structural ones -- `ai_z_getc` (comments), `ioread1str`
(escapes), `ioread1sym` (the atom lane) -- and p0 adds only the four structural
cases over them: `(`, `)`, `'`, and a default that falls through to the atom
lane. rung 2's number tower in particular is SHARED, not copied: it is the tree's
one answer for what a literal means, and a second copy in C is the divergence
rung 2 spent itself deleting.

**the parse nif became a TEMPLATE** rather than a second copy. `sound` and
`sound0` differ in exactly one call -- which parser runs -- and share the park
law, the transactional rollback and the read protocol's more/eof routing through
help; `sound_op(name, read1)` is that body once, the `bit_slow`/`op11` shape
already in the file.

⚠ **a reader list terminates in `ZeroPoint`, never `nil`** -- `nil` is the fixnum
0, and the two are indistinguishable through the printer. that is the `#()`/`@()`
divergence rung 3's first sweep found, and rung 6 faces the same choice the
moment p1 folds a list.

**p0 is a reader of a subset, not a validator.** a char outside the subset lands
in the atom lane and comes back a plain symbol -- `#(a b)` reads as `#` then
`(a b)` where the structural reader gives `(hash a b)`. adding a check would mean
keeping a second copy of a grammar p0 does not own; the DIFFERENTIAL is the
enforcement instead, and rung 4's "prel.l stays in the subset" rule and this gate
are now the same fact.

⚠ **the one property p0 gives up is DEPTH.** nesting rides the C stack -- the
trade that buys the size -- where the structural reader is stackless and bounded
only by the heap. measured: 100k deep reads, 200k faults; the deepest form in the
tree is **38** (test/uupatch.l), so the bound is documented and pinned at 20000
rather than capped. ⚠ revisit if p0 is ever pointed at text the tree does not
own; it is a bootstrap reader and that is the contract.

gated in test/host/rdiff.l, the rung 3 oracle, four ways: p0 vs the C reader over
prel.l (0 bad of 53 forms), print→read through p0, the same rig pointed at
**ev.l**, which is REQUIRED to go red (4 divergences), and the depth pin. the
ev.l leg is the rd-hurt discipline applied to a real second reader instead of a
spoiled one -- without it the agreement above is not a measurement. spec.l states
the law beside `sound`'s; a GC stress pass (60 reads of prel.l with a forced
major between each) covers the relocation p0's on-stack datums depend on.

---

what the rung was planned as, kept for the reasoning:

**do not write this from scratch -- port it forward.** the whole sigil surface is
recent: `3e8d8ebd` (2026-06-09, N-ary reader operators via `dict['operators]`),
`fb4a7920` (06-10, reader infix), `6ce0be64` (06-11, "the reader is structural"),
`3d8be764` (06-11, the valence law). before all of that, at **`2efbaa51^:g/g.c`,
lines 1286-1400** (2026-05-28, core 2618 lines), the reader was ~104 lines of
plain mutual recursion:

```c
g_z_getc   // whitespace + ; and # comments        9
gzread1    // ( , ' , "…" with \ , atom            71
gzreads    // the list loop                        12
           // + g_read1/g_reads/g_read wrappers    12   (g_read is the rollback)
```

⚠ take it from `2efbaa51^`, the commit BEFORE "rm lisp-side reader" -- that is the
minimal C reader, the one whose only job was to bootstrap `repl.g`. `2efbaa51`
itself grew it by 85 lines taking back what the lisp reader had been covering, and
those 85 lines are p1's half.

it also answers the GC question by demonstration. the current reader keeps its
frame stack on the l heap because it allocates on nearly every step and a copying
collector cannot trace love values in C locals -- but the May reader shows the
shape that works: **control flow on the C stack, values on `f->sp`**. `g_reads`
loops `f = g_read1(f, i)` letting datums pile on the l stack, then folds them with
`gxr`. no love value ever sits in a C local across an allocation. (love.c also has
the `mm`/`um` shadow-stack roots -- 121-122, love.h:148 -- if one ever must.)

p0 ≈ that, MINUS its number lane, PLUS today's. the historical atom lane ends in
`strtol` then `g_strtod`; rung 2 deleted the reader's last `strtol` call, so p0
takes the three radix predicates and `ai_big_read_*` instead -- fewer lines and the
literal reads the same in every build by construction. floats stay a `strtod`
fallback (prel has four: :15 π, :17 e, :21, :665 -- or move them past p1 and p0
stays integer-only). `()` reads the ZeroPoint. call it **~80 lines**. pure
addition: the existing reader stays, nothing calls p0.

### 6. `p1`, the reader in love -- THE PARSER LANDED, the boot is next

**love/p1.l**, ~150 code lines, and it reads the WHOLE TREE exactly as the C
reader does: 298 files, 1322 forms, `bad=0`, plus print→read. that is the A/B law
non-vacuous for the first time, which is what rung 3 built the rig for.

it is gated in test/host/rdiff.l beside the p0 leg, with the same fault injector
pointed at it -- and the zero was checked against three deliberate perturbations
before being believed: flattening the mono wrap gives 473 bad, dropping the
trailing-`-` shed gives 13, the injector gives 1.

**p1.l is itself held to the p0 subset**, which the plan implies but never says
outright: p0 is what reads p1. so no `` ` `` `#` `@` `~`, no comma, no brackets,
and never a glued operator run. gated -- p0 reads p1.l form for form, same as it
reads prel.l.

**and it is pre-prel, mechanically checked**: every name it references is a nif
or one of its own bindings. `||`/`&&` are open-coded to nested `?` (they are prel
MACROS, and prel is not loaded when the boot calls this); `map`/`jot` have a
local twin for the one place a text becomes a charlist.

three bugs are worth carrying forward, because each is a law of this tree rather
than a slip:

* **`(: cl (p1-skip cl) …)` reads the NEW `cl`.** a body-having `:` is one scope,
  so that is the missing condition and the value is the zero point -- silently.
  the historical reader is full of that idiom and it does not survive the port.
* **love has no `\f` escape** -- n t r e 0 x and nothing else -- so `"\f"` in a
  char class is the LETTER f, which quietly made `f` whitespace and ate it out of
  `ieee-inf` and `(f -1)`.
* **`-5` and `0.0` net to nothing**, so `nil?` cannot ask whether a token parsed.
  `p1-int` answers `(1 . n)` tested with `two?`; the float lane asks `gem?`, the
  TYPE, because `gem` answers the charm 0 on failure and the float 0.0 on "0.0".

**what is left is the BOOT STITCH**, which the plan already called the real work:

### 6b. the boot

p1's shape follows the historical lisp-side reader at `2efbaa51^:g/repl.g`
(removed 2026-05-28, "rm lisp-side reader"): a charlist in, `(value . rest)` out,
two sentinels, no port protocol. `flo`/`gem` (love.c:4116) survives as its
purpose-built hook, cited to a `repl.l` that no longer exists, and it is still the
float door.

⚠ **it was removed because it worked but did not pay** -- at that time the grammar
was still pure lisp, which C handled fine and faster. that history is the argument
for where this arc cuts: the removal reason applies exactly to the part p0 keeps
and not at all to the part p1 takes.

**the boot is the part still to build, and it is the real structural work.**
`ai_evals_`
(love.c:2220-2231) does ONE `ai_reads` over the whole egg string --
`"("` + egg.h + `" '("` + prel.h + ev.h + `"))"` -- so prel and ev are read
*together, before anything is evaluated*. every frontend calls it.

⚠ **do not bootstrap by eval'ing prel manually in C.** `(sit e z a)` (egg.l:18)
folds `e` over the corpus and answers the LAST form's value, and
`(sit (sit ev 0 egg) 0 egg)` (egg.l:24) feeds the inner fold's result -- ev₁ -- in
as the outer fold's evaluator. so `egg` must be the COMPLETE corpus as one
unevaluated list. hand the egg only ev's forms and prel is never recompiled by
ev₁: its globals stay c0-compiled, with no `feel` folding or inlining. egg.l:21-23
marks the double sit load-bearing and records that collapsing it was tried once
and reverted.

**the shape instead**: leave the egg expression exactly as it is and construct its
ARGUMENT by stitching rather than by one read -- interleaved reads / pushes /
`gxl` / `gxr` building the same `((\ egg …) '(<prel forms> <ev forms>))` on the l
stack, prel's half from p0 and ev's half from p1, then a single `ai_eval`. the
egg's internals never learn anything happened.

**the circularity that resolves it**: p1 must exist to read ev's half. so
bootstrap-eval p1 alone (p0 reads it, c0 evals it) purely to make it callable
during the stitch, and still put its forms in the egg list so the double sit
recompiles it. three p1s exist in sequence -- the bootstrap copy (c0, and the one
that actually reads ev.l), the inner sit's (c0 again), the outer sit's -- and only
the last survives, ev₁-compiled, exactly like prel and ev themselves.

⚠ **p1's forms go at the HEAD of the egg list, never the tail.** `sit` answers the
LAST form's value and that value is what gets pinned as `ev`. p1 at the tail would
install the reader as the evaluator.

⚠ **do not re-eval p1 after the egg instead.** it buys nothing -- ev₁ and ev₂ are
the same compiler source and emit the same code for p1, differing only in their
own speed -- and it would run *after* `mop`, so it could reference noms birth has
just deleted. p1 must survive the mop regardless (egg.l:14-17): capture references
at compile, or come off the mop list the way `turn`/`turnf` did.

⚠ p1 **cannot be a `(use)` module** -- `ai_evals_` reads its own driver text
before any love exists.

gated by rung 3, with the C reader alive as the differential twin behind a switch
(the `KLINK=lld` precedent). flip when the differential is silent over the whole
corpus, then delete the C reader.

## probes to run first

* **read doc/stream.md:118-135.** it already proposes deleting the more-bit
  machinery by making read a pure fold over a lookahead-cons stream. prior design
  work on exactly this question.
* **time p1 on the egg lane specifically**, not on a warm image. that is where the
  cost lands and where nothing optimizes it.
* **can p1 be written with nifs alone**, or does it want prel's helpers? the
  historical one needed only `||`/`&&`/`rev`, all trivially inlined -- but the
  sigil surface it must ADD is the part nobody has written in love yet.

## order and size

**1-4 are LANDED.** 1 went first because everything after it re-implements what
it states, and it was useful alone -- the tree gained a conformance file and lost
a special case. 2 deleted three existing workarounds rather than adding
machinery, and took the freestanding `strtol` with it. 4 was the 15-line refactor
it looked like, plus one design decision the mop forced. 3 turned out to be
slightly more than assembly -- the input set is the whole tree rather than the
fuzzers -- and it paid for itself immediately by finding the `#()` nil tail.

**1-5 are LANDED, and so is 6's parser.** 5 was a port of working code
(`2efbaa51^:g/g.c:1286-1400`) and came in at 30 code lines against the estimated
80, by sharing the leaf lexers instead of copying them -- the same move rung 2
made for the number tower, for the same reason. 6's parser then read the whole
tree identically on the strength of that same oracle.

**what is left: 6b, the boot stitch**, plus rung 2's bit ops. the plan's own
estimate held exactly -- "most of its risk is in the boot rather than in the
parser" -- and the parser is now the measured part. what remains is building the
egg's argument by stitching, the mop question for p1's noms, and then the flip:
rebind `rd-test` from `sound` to p1 so roundtrip.l and fuzz.l follow, and delete
the C reader.

**speed is bounded, not an open risk.** reading happens once per cold boot --
`ai_evals_` reads the corpus and the double `sit` folds over already-read forms.
so p1 reads ev.l once, only cold: ~230 ms total today against ~1.2 ms of C
parsing, and the baked image path (~4 ms wake) skips reading entirely, which is
what users actually run. ⚠ the glaze will NOT help: its grammar is integer
arithmetic over frame params, and a reader is strings, noms and cons cells,
exactly its declining set (test/test.mk:130-135). mooncc is the precedent that
this is fine anyway -- 9200 lines of love, 20 ms per TU warm.
