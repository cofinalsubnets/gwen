# the io substrate -- THE PLAN

**this is `doc/reader.md` and `doc/stream.md` merged (2026-07-31), because the two
arcs converged.** they were written apart -- one to get the reader off C, one to
fix a scheduler deadlock in the port surface -- and they turned out to be the same
work approached from opposite ends. stream.md's §4 (`read` as a pure fold, the
more-bit protocol collapsing) landed as rung 6c. its §3 (`source`: a lazy,
memoizing byte-chain over an fd) landed as rung 7 and is called `flow`. its §5
(`ready?` generalized off stdin) landed as `cue?`. its defect 1 was fixed
outright. **nobody noticed at the time, because the docs did not talk.** that is
the whole reason they are one file now.

part I is the reader ladder, LANDED, kept as the record. part II is the port
surface: what stream.md diagnosed, re-scored against the tree as it actually is.
part III is the question neither document asked, which is the live one. this
ORIENTS; the laws live in test/reader.l and test/io.l, and every doubt settles by
probing `sound`.

---

## part I -- the reader off C

the reader was the last big island of C that is not the runtime: 470 raw / 351
code lines of `love.c` (5.7% by code line), a hand-stacklessed parser carrying a
grammar that had outgrown it. the arc: a tiny bootstrap C parser `p0` that reads
a PURE LISP subset, and the real reader `p1` written in love on top of it --
`c0`/`ev` again, one layer down. drafted 2026-07-29.

**rungs 1-7 are LANDED and that arc is closed** -- the tree has one reader, and it
has one INPUT type, because a port IS a charlist with a promise for a tail. rung
8A landed 2026-07-31 (p0's lexers went charlist-native). **8B is NOT started, and
part III is why.** the reader was then MEASURED for the first time (2026-07-31)
and went 4x on a class table -- the speed paragraph at the foot of part I carries
the numbers, the trap and what is still on the floor.

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

**the parse nif became a TEMPLATE** rather than a second copy: `sound` and
`sound0` differed in exactly one call -- which parser runs -- and shared the park
law, the transactional rollback and the read protocol's more/eof routing through
help. (6c retired all of that with the C reader; the rollback is the one piece
that had to be carried forward by hand, and forgetting it segfaulted the corpus.)

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

### 6. `p1`, the reader in love ✅ LANDED (parser + boot)

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
local twin for the one place a text becomes a charlist. it is **pre-opfix**
besides -- see 6b, that one cost a rewrite.

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

### 6b. the boot ✅ LANDED

**`ai_egg_(g, egg, p1, prel, ev)`** (love.c, 40 code lines) replaces the one
juxtaposed string every frontend used to hand `ai_evals_`. The egg expression is
unchanged -- `((\ egg …) '(<p1 forms> <prel forms> <ev forms>))`, the double
`sit` still folding over the COMPLETE corpus -- but its ARGUMENT is now built by
interleaved reads and `gxl`/`gxr` on the l stack, **p0 reading the three files
held to the pure lisp subset and p1 reading ev.l**. So the C reader's sigil half
is off the boot path on every target.

the shape that made it small: **each half is read ONTO the list already on the
stack**, so the corpus stitches right to left with no append and no copy -- ev's
forms first, prel's onto those, p1's onto that. the same helper builds the
applications: a one-form driver text read onto its own quoted operand IS
`(driver '(list))`, which serves both the egg and the plain eval fold.

p1.l is read **twice**. the first read is c0-evaluated on the spot, purely so p1
is callable for ev.l; the second puts p1's forms in the corpus so the double sit
recompiles them ev₁-compiled like everything else. reading a 258-line file twice
costs ~2 ms and buys the whole circularity.

two things the plan had not seen:

* **p1.l has to be no-infix**, prel.l's rule and for prel.l's reason. "pre-prel
  in its NAMES" is not the same as "pre-opfix in its SYNTAX", and the bootstrap
  eval happens before prel defines `opfix` -- so `(n <= k)` would have compiled
  as a plain application, church-exponentiating where a comparison was meant,
  silently. ~60 prefix rewrites, LOC-neutral. it is self-gating, and that was
  CHECKED rather than assumed: one comparison put back infix (`(48 <= c)` in
  `p1-dig?`) makes p1 misread ev.l and the cold boot **dumps core**. loud, on
  the first build, on every target -- but a segfault, not a diagnostic, so a
  future session meeting one here should suspect this before anything else.
* **p0 has to read what `lcat` PRINTS, not only what the tree types.** the boot
  embeds the lcat'd headers -- canonical `show` output, minified against the
  *structural* reader's grammar (tools/lcat.l drops a space wherever `sound`
  parses the same either way). `show` spells a lambda `(\ egg …)`; lcat glues it
  to `(\egg …)`; p0's atom lane took that as one name. the fix is the run
  lexer's own rule, `\` never fuses, as one more case in `p0read1`. ⚠ the
  general form of this: **p0's subset must be closed under show → read**, and
  the boot itself is the gate that says so -- this failed on the first build,
  not silently.

**speed**, measured: reading ev.l is 62 ms through p1 against 4 ms through the C
reader, so a cold boot pays ~+58 ms of its ~1 s. a woken image reads nothing and
pays zero, which is what users run. (⚠ that 62 ms was the SCAN-based reader; the
class table took it to ~8 ms -- the speed paragraph at the foot of part I.)

**left open, deliberately, for the flip**: p1's 36 `p1-*` bindings are on the
book now -- the whole of what this rung adds to `(names ())`, and exactly what
vim/syntax.vim regenerated to. that is real vocabulary noise for something with
no caller yet, and there are three ways out -- mop them by prefix the way
`lvm_*` goes, seal the plumbing and re-pin only the doors (bao.l's shape), or
make p1 a module now that the stitch, not `ai_evals_`, is what loads it. all
three depend on **how `sound` reaches p1 after the flip**, which is 6c's first
question, so the answer belongs there and not here. ⚠ what is NOT open: the
plan's "p1 must survive the mop regardless" still holds, and mopping it before
anything calls it would make the corpus copy dead and untestable.

---

what the rung was planned as, kept for the reasoning:

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

the three probes this section opened with are all answered: p1 wants **nifs
alone** (the sigil surface needed no prel helper), the egg-lane cost is **+58 ms
cold and zero warm**, and doc/io.md part II's lookahead-cons stream is still
the standing proposal for the more-bit machinery -- which is 6c's problem now,
not 6b's.

### 6c. the flip ✅ LANDED

`sound` is `p1-read1`. it takes a charlist and answers three ways -- the datum
CONSED ONTO WHAT IS LEFT, `()` at a clean end, or `torn` where the text ran out
inside a shape. `sound0` moved with it, so the differential still compares like
with like, and p0-vs-p1 is now the only reader pair left in the tree.

deleted from love.c: `ioparse` (the 230-line structural parser), `ioread1op`,
`op_break`, `symeq`/`hashsym`/`splicesym`, `ai_reads`/`ai_read1`, the `sound_op`
template with its park law and transactional rollback, and the more-bit branches
in `lvm_help` and `ghelp2`. **love.c is 112 lines shorter and the .l side is
flat** -- the call sites shrank by about as much as p1 and `reads` grew.
kept: `ioread1sym`, `ioread1str`, `ai_z_getc`, which p0 shares.

**three things bit, all of them the tree's own documented traps:**

- **a `(nom 0)` sits on the BLUE FLOOR.** `torn` started as an anonymous nom, so
  `(nil? torn)` was TRUE and a torn answer read as a clean end. it is a NAMED
  symbol now: nets its spelling, stays un-two, prints as itself. this is
  "presence is the wrapper, never the net" wearing a fresh face.
- **`p0reads` piles a list's datums on the l stack** and folds them only at the
  close, so a TORN parse leaves the pile behind and the text slot is no longer
  `sp[0]`. the old parse nif had a transactional rollback; dropping it segfaulted
  the corpus. `p0text` records the depth and restores it.
- **`ci_ungetc` parks the pushed-back byte in `ungetc_buf`, not on the
  charlist**, so reading the port's `head` alone SWALLOWS the token terminator --
  `#(a)` came back as `#` with the `(` gone. (rung 8A retired that pushback for
  p0: the cursor just stays ON the terminator. the port's own remains.)

⚠ and the port must be ON THE HEAP: a `ci`'s head is a love value and `g->io`
rides the core's `v0..end` span, so the collector forwards it. the boot stitch
gets away with a C-stack `ti` only because ti's source is a plain C string.

**`reads` is the one place a port becomes text**, and the only refill loop in the
tree. it blocks for the first byte with `see`, then drains what `cue?` says is
ready, and re-parses ONCE PER DRINK -- crew/moon/gen.l is a single 360KB form, so
a byte-then-reparse loop is quadratic on it. `cue?` was generalized from
stdin-only to any port for this (it also omitted `bio_rpending` and so disagreed
with `see`'s own park guard). streaming is preserved: a form piped in evaluates
before the pipe closes.

lux, haven and port/inle/serve each hand-rolled the residue discipline to avoid
sounding a live socket -- re-`tap`ping and re-parsing the whole accumulation
every line. all three collapse into the returned residue, and each gained "every
form on a line runs" for free, where only the first used to.


### 7. one input: the charlist IS the port ✅ LANDED

gwen's, and the framing is the whole rung: **do not make laziness transparent.**
the caller REFLECTS on the tail -- a cons is more input, `()` is the end, anything
else is a thunk to force -- and nothing forces implicitly. that is what makes this
cheap, and it is worth saying why, because the transparent version is the obvious
guess and it is blocked:

```c
op11(lvm_cup, chainp(Sp[0]) ? B(Sp[0]) : ZeroPoint)
#define op1(nom, i, x) lvm(nom) { Sp[0] = (x); Ip += i; return Continue(); }
```

`op1` assigns a C EXPRESSION to `Sp[0]`. forcing means CALLING -- an ap frame and
a jump -- which an expression slot cannot build, so a `cup` that forces would have
to become a real `lvm_`, on the path where 30% of p1's profile already lives. the
reflecting version needs none of that: `cup` is untouched, the hot path is
untouched, and the only new C is whatever mints a thunk over a port.

**the point is not speed.** it is that "charlist" and "port" stop being two ideas.
a port, viewed as input, IS a charlist whose tail is a thunk that reads more.

#### the design already existed, and was thrown away at p1's door

bao.l stated it exactly, as the `char` colist -- "(cap cs) is a charm, (cup cs) a
top, the tail forced with the empty -- `((cup cs) ())` … consumers dispatch on
KIND: two? is a cell, anything else the end", and `char` was "the canonical lift
-- chars cell by cell, a port (the HOT lane) one byte per force (-1 ends it), a
colist passing through". the vocabulary was already ordinary: the force is
unit-fired, `(t ())`, per the zero-operand law.

then `char->chars` undid it, REALIZING the colist into a strict list before handing
it to `sound`, because p1 only ate strict lists. the tree built the thing and
discarded it at the one door that wanted it. so the rung was mostly a matter of
not throwing it away -- with two corrections. the force moved OUT of the lift and
into the reader (`char` forced for the caller; `p1-cup` lets the caller decide),
and the unit of laziness moved from the BYTE to the GULP.

#### what it deleted

two sites, and they are one seam:

| site | the workaround it was |
|---|---|
| `bao.l` `char->chars` | realize the colist before parsing |
| `bao.l` `(cat cl more)` in `reads`, and its twin in `forms` | refill OUTSIDE the reader, re-parse per gulp |

`reads` and `forms` collapsed from a refill loop to a walk -- each is now four
lines around `sound` -- and the 6c warning went with them: *"IT MUST RE-PARSE ONCE
PER DRINK, NEVER ONCE PER BYTE -- crew/moon/gen.l is a single 360KB form"*. that
hazard existed ONLY because the reader could not ask for more itself. `read` went
from a nested lift-and-realize to one line: `(sound (? (hot? s) (flow s) s))`.
`drink` STAYS -- it is a vessel verb in its own right (sip/drink/slurp, spec.l's
i/o section), and `flow` is the caller that turns it into a charlist.

and the deep one: **`torn` stopped being a maybe.** `reads` used to get `torn` and
be unable to tell "unfinished shape" from "unfinished shape, more is coming", so it
drank again, re-parsed, and scared only when the drink came back empty. now the
reader forces and finds out; `torn` means a genuine `()` tail, and the scare is one
line at the same level as the datum case. that is the unification paying, not
merely glue leaving.

**p0 does not change**, and it turned out that **the C does not either.** the plan
said `p0text` and `struct ci`/`ci_getc` would collapse with the rest. that was
wrong twice over, and worth writing down:

- `ci` is not p1's adapter, it is **p0's own**. `p0read1` reads through the port
  vt (`ai_z_getc` and the leaf lexers), and `sound0`'s protocol is charlist-in /
  residue-out, so something has to wear a charlist as a port. laziness never
  touched that mismatch. removing it means making p0's lexers charlist-native --
  a rung of its own, and one that also has to answer for the boot stitch, which
  reads a C string through `ti`.
- `4388/4391 are the only constructors` was simply false: **prel's `tap` builds a
  `ci`** (`(poke -1 -4 ..)`, fd = -4, the in-memory port every test and `flow`'s
  own witness leans on). the C stays because the facility does.

what remains true: p0 is the SOLE consumer of the leaf lexers (`ai_z_getc`,
`ioread1str`, `ioread1sym` have no caller outside `p0read1`/`p0reads`; 6c's
`ioparse` deletion took the other), so converting its input later strands nobody.

**so how small can `p0text` go?** measured, not estimated: **28 code lines to 23**,
and that is the floor while p0 reads through a port. it does four things and each
one is answering a real question:

| job | lines | why it cannot go |
|---|---|---|
| wear the charlist as a port | 5 | ⚠ ON THE HEAP -- a `ci`'s head is a love value and `g->io` rides the core's `v0..end` span, so the collector forwards it. a C-stack `ci` dangles the moment the parse allocates. |
| run `p0read1` | 1 | |
| unwind on partial input | 5 | `p0reads` piles a list's datums on the l stack and folds at the close, so a torn parse leaves the pile and the text slot is no longer `sp[0]`. |
| hand back what is left | 6 | the residue is `head` PLUS the byte `ci_ungetc` parked off-list. |

the 5 that left were the four separate field assignments (now one comma run, the
shape `p0onto` already uses for its `ti`), the three-step rollback, and a `{}` block
around the pushback. `ai_p0read1` went with them -- 2 lines, non-static, no
declaration in love.h and no caller in the tree; 6c's `ioparse` deletion took its
last one. love.c 9047 -> 9044.

two ways past the floor, neither of them this rung's:

* **charlist-native p0** -- the leaf lexers thread a cursor instead of pulling from
  `g->io`. `p0text` collapses to about 6 lines and the ungetc dance disappears
  entirely (a pushback is just not advancing). but the cursor is a LOVE VALUE that
  every leaf both reads and updates, so it has to live on `g->sp` -- and `g->io` does
  that job today, GC-traced, for free. the three lexers would each grow what
  `p0text` sheds. net could easily be zero.
  **this is rung 8A, LANDED**: `p0text` 22 -> 10, and the worry was the wrong
  way round -- the lexers SHED (`ioread1str` -29 -> 23, the other two flat), and
  what grew was the cursor's own five helpers. the depth, not `g->sp[0]`, is what
  makes it GC-safe.
* **give `struct ci` a `prev` word**, so a ci-specific ungetc rewinds onto the list
  instead of parking off it. that retires the residue's second half AND the trap
  above -- but the fd = -4 layout is built from LOVE (prel's `tap` pokes a 5-word
  spin), so it is a change to a shape two languages agree on, for ~3 lines.

#### ⚠ the thunk must MEMOIZE, and the sanctioned way is a tablet -- taken

p1 forces the same tail more than once. `p1.l:66-67`, the shebang lookahead:

```love
(? (= c 35) (? (two? (cup cl)) (= (cap (cup cl)) 33) ()) ())
  (p1-skip (p1-eol (cup (cup cl))))
```

three evaluations of `(cup cl)` in one expression. `p1-str`'s `\xNN` lane
(`:167-170`) has the same shape. against a `(see p)` thunk that is three reads
where one byte was meant -- and it is INVISIBLE today only because `char->chars`
realized first.

a one-shot thunk therefore cannot be the tail. the two ways to memoize:

* **mutate the cons tail in place** (the classic promise). ruled out: mutating a
  cons tail is the legacy pre-gen-gc design that has cost this tree headaches
  more than once, and mutation goes through TABLETS here.
* **a tablet-backed promise** -- the thunk closes over a tablet, the first force
  pins the result, later forces read the pin. pure from the caller's side, one
  tablet per gulp, and it uses the door the tree already sanctions. **taken**, as
  `once` in bao.l, six lines and a name of its own because it is worth stating
  alone.

the third option, proving p1 never re-forces a position, is not worth attempting:
the run/backtrack logic re-examines positions by construction, and a failure is
silent (two reads look like ordinary input).

⚠ and `once` has the presence trap in it, sharply: **the answer is wrapped `(1 v)`
and read back with `two?`**, because a promise of NOTHING is the ordinary case --
the last gulp's promise answers `()` at eof. a bare pin of `()` leaves the slot
reading exactly like an unforced one, so `f` re-runs forever. that is the fifth
face of "presence is the wrapper, never the net", inside the fix for it.

#### ⚠ two smaller traps

* **chunk size.** `char`'s hot lane forced ONE BYTE per force; `flow` puts the
  whole `drink` gulp inside the promise, so the amortization the refill loop had
  survives the move and the quadratic re-parse is not traded for a cons-and-a-call
  per character. the gulp is REALIZED; only the join is lazy.
* ⚠ **the gulp must be built BACKWARDS**, and this is the one that actually bit. the
  obvious `flow` is `(cat g (once ..))` -- and `cat` COPIES `g`, so a gulp holds two
  cells per byte at its peak instead of one. on a host that is invisible; on the
  freestanding targets it is the difference between fitting and not. it reddened
  **`test_uefi` only** -- `test_kernel` on the same corpus and the same kmain.c
  passed, because the UEFI lane hands over after the firmware has taken its share.
  the failure wore exactly the face ktest.l already documents for a budget wall:
  *"died SILENTLY mid-corpus at a deterministic dot, no fault text"*, no assert, no
  scare. the fix is `gulp` (the drink, still reversed) plus one
  `foldl (flip link) promise`, which lays the cells straight onto the tail; `drink`
  is `(rev (gulp p))` and pays the same single pass it always did.
* **`two?` is FALSE for a lambda**, so an unforced tail reads as a clean end to
  every test already written. a PARTIAL conversion fails by silently truncating
  input -- "presence is the wrapper, never the net" wearing its fifth face, and
  the one that was feared here. it did not bite, because p1's conversion went in
  FIRST and whole, one commit ahead of anything lazy existing to feed it: on
  strict input `p1-cup` is `cup`, so the gate that proved it green proved nothing
  had been missed either.

#### how it went, and its gate

1. `once`, the promise, and `flow`: a gulp laid onto `(once (\ u (flow p)))`, `()` at
   eof. both bao.l top-level, so they ride bao's splice like `drink`.
2. p1 reflects, through `p1-cup` -- `(? (lit? t) (t ()) t)`, one line. the rule is
   grep-able and TOTAL: after it, a bare `cup` on the INPUT cursor is a bug, and
   `grep '(cup ' love/p1.l` reads as an audit. six functions step the cursor
   (`p1-eol`, `p1-skip`, `p1-tok`, `p1-run`, `p1-str`, `p1-read1`) plus `p1-list`'s
   close, which is the one that hid: it walks its own `cl` but advances past the
   `)` in the RETURN. the token lists p1 builds itself stay strict, so `p1-int`
   and friends were untouched.
3. `sound` takes either, for free -- a strict charlist is the nil-tailed case, and
   nothing that passes one noticed. this step was empty, which is the sign the
   reflecting design was the right one.
4. `reads`/`forms`/`read` dropped the refill loop and `char->chars`.
5. the C: NOT DONE, and not to be done here -- see above. `p0text`/`ci` are p0's,
   not p1's.

net: **.l code down 6 lines, C unchanged, one new ambient name pair (`once`,
`flow`)** -- but the count is not the point and never was. what left is a whole
mode of failure: there is no longer a place where text is realized ahead of the
reader, so there is no longer a gulp size to get wrong.

the acceptance test already existed: **test/host/rdiff.l**, one rebindable name
over 297 files / 1318 forms, plus `test/io.l` for the live-port lane and
`test_kore`'s piped stdin. a memoization bug shows up as a MISREAD, not a crash,
so test/io.l gained a law that forces a position twice ON PURPOSE.

⚠ **that law needs a port with something still to give.** the obvious witness --
force a flow's last tail twice, get `()` both times -- passes with a bare thunk
too, because a second drink on a spent port answers `()` as well. the working one
`unsee`s a byte back AFTER the first gulp: memoized, both forces answer it;
re-drinking, the second swallows it and answers the clean end. sabotage-proved --
with `once` removed from `flow`, that assert is the ONLY one in the corpus that
reddens. ⚠ and it compares by `id?`, not `=`: a flowing cell's own tail is the
next promise, so `(= '(98) x)` is FALSE even though the printer shows `(98)` --
it stops at a tail that is not a cons, which is the one place a flow does not
look like a list.


### 8. charlist-native p0, and the port that nobody reads -- A LANDED, B planned

gwen's, and it is the arc's real tail: **`g->io` is a painful hack, and the
polymorphism it was buying is exactly what a charlist-with-a-promise-tail already
gives.** `cue?`, the more-bit, the pushback -- the awkward interface rung 6c spent
so long sitting on -- existed to make one reader work over several kinds of input
stream. that job has a better holder now.

#### the evidence is already in the tree

the buffered fd lane **is a colist, written by hand and then hidden.** `io_refill`
(love.c:3156) drains the write side, dresses a backing string, does ONE `readn`
into it, deep-waits if the gulp comes back dry -- then hands back byte 0 and
stashes the rest in `rbuf`/`rpos`/`rlen`. that triple plus "refill when exhausted"
is `(chunk . thunk)` spelled in C. and every one of `ungetc_buf`, `eof_seen`,
`bio_rpending` and `cue?` exists to RE-SERIALIZE it back to one byte at a time.

so this rung does not add a mechanism. it deletes the re-serialization and returns
the chunk.

#### ⚠ but `g->io` is not what it looks like, and this is the fact to keep

it is **not a reader hack. it is a single-slot GC SHADOW ROOT.** the vt functions
take `g` and re-read `g->io` because a C local pointing at a heap port dangles
across any allocation -- love.c:3288 says so at the site: *"GC may have moved it;
g->out is GC-traced"*. so "just pass the port as a parameter" was never available,
and `g->io` survives as long as ANY C code touches a port across an allocation.
**output does.** a colist cannot be a sink.

which reframes the dream, correctly rather than smaller: not *delete the port* --
**the READER never sees one, and the thunk does.** that is where `flow` already
put it (rung 7), closure-private. the port object also cannot go for a socket,
because the read path must drain the WRITE side before parking (`io_refill`'s
*"the crossover: our unsent ask goes first"*) -- and the thunk closes over the
port, so it can.

#### the decision, TAKEN 2026-07-31: one input representation

gwen's, and the alternatives are recorded because the middle one looks prudent and
is not. what was on the table:

| | **1. one input representation** | **2. readers only** | **3. stop at rung 7** |
|---|---|---|---|
| end state | nothing reads a port. ports are `readn` sources + sinks, `see` is gone, `g->io` is OUTPUT-ONLY | text readers walk charlists, devices keep `see`; `g->io` unchanged | today |
| C deleted | ~130 | ~35 | 0 |
| concepts deleted | the pushback, the eof bit, `cue?`, the readiness question, `tap` | `tap` | none |
| spent | the cursor refactor + **124 `(see ..)` sites** + a stdin-ownership answer | the cursor refactor + ~30 sites | nothing |

**1 is chosen.** 2 spends most of the refactor's risk and collects almost none of
the prize -- `cue?`, `ungetc_buf` and `eof_seen` all survive it, because a device is
still read a byte at a time, which is the thing this rung exists to stop.

⚠ **A is not a scope you can judge on its own** -- it deletes ~20 lines and buys
nothing. it is the first move of 1, and the only question was ever whether 1 is
worth its migration. the 124 sites are milder than the number: 13 are in bao.l,
11 are doc/proto (dead), and the byte-protocol clusters (lux's wire, kiosko,
seed's http) are each ONE function wanting "exactly n bytes", which a colist
gives with `take`. the real risk is stdin, and that is decision 3 below.

#### the two lands, and only the first stands alone

**A. p0 goes charlist-native.** ✅ **LANDED 2026-07-31.** the leaf lexers thread a
cursor instead of pulling from `g->io`. self-contained, gated by rdiff, and it is
the rung.

#### what A actually cost, against the ledger below

**the estimate held: exactly 20 code lines net** (love.c 5996 -> 5976, comments
and blanks excluded), and the shape of it was not quite the shape predicted.

| | code lines |
|---|---|
| `p0text` 22 -> **10**: the text slot IS the cursor, so the whole answer is `gxl` | -12 |
| `ai_z_getc` (16) -> `p0skip` (7) + five cursor helpers (13) | +4 |
| `ioread1str` 29 -> 23; `ioread1sym` and `p0read1`/`p0reads` flat, each shedding its `zungetc` | -8 |
| `p0onto` 14 -> `p0chars` + `p0onto` 25: the boot conses its C string | +11 |
| **`struct ti` and its lane, unplanned** -- `p0onto` was its ONLY constructor | -13 |
| net | **-20** |

* **the cursor is named by its DEPTH, never by an address.** a collection moves
  the stack (`p0text`'s own rollback recomputes `topof(g) - depth` and always
  did), so a `word*` into it dangles exactly like the C local `g->io` existed to
  avoid. one `uintptr_t d` parameter threads every p0 function; `p0cur` resolves
  it per access. that is the whole GC discipline of the rung, and it is three
  lines.
* **`p0peek2` is why the skipper more than halved.** `#!` was the only two-char
  decision in the grammar and it owned the pushback; with a list, the second char
  is one more `B` away and `unget` is simply not advancing. the token lexer's
  `zungetc` went the same way -- it now just LEAVES the cursor on the terminator,
  which is also what deleted `p0text`'s residue recovery.
* **`struct ti` fell out for free**, and that was not in the plan: p0onto's stack
  `ti` was the only thing that ever made one, so the C-string port is gone.
  ⚠ the fd = -1 ROW stays, as a hole -- the synth fd is a PROTOCOL number prel
  pokes by hand (`tap` writes -4, `jug` -2), so deleting the row would renumber
  its neighbours. `ti_ungetc`/`ti_eof` were the charlist lane's too and are
  renamed `ci_ungetc`/`ci_eof`.
* **the boot's cons is invisible in practice.** `p0chars` takes one `Have` for
  the whole run and then walks backwards, so nothing allocates mid-list and the
  cells need no root. the predicted ~424KB peak for prel.h on 64-bit is real and
  nothing noticed: `test_mps2` (32-bit, bakes the egg from source on the emulated
  M7) and `test_uefi` (the tight lane, and the one that caught rung 7's doubled
  gulp) are both green, as are test_kernel and the qemu doors.
* what did NOT move, exactly as the plan said: `struct ci`, `ci_getc`, prel's
  `tap`, `see`/`cue?`/`ungetc_buf`/`eof_seen`. those are B's.

⚠ **A DOES NOT PAY FOR ITSELF IN LINES, and the honest ledger matters here** --
it is about **20 lines** of net deletion:

| | code lines |
|---|---|
| `p0text` 23 -> ~8: no `ci` to build, no residue to recover -- the cursor IS the residue | -15 |
| `ai_z_getc` 16 -> ~13: the `#!` pushback becomes NOT ADVANCING | -3 |
| `ioread1str` (29) / `ioread1sym` (31) / `p0read1`+`p0reads` (27): convert, roughly flat, each shedding a `zungetc` | ~-5 |

⚠ and **`struct ci` / `ci_getc` / prel's `tap` are B's, not A's** -- `tap` has 32
call sites making a port out of a charlist for `reads`, `slurp`, `edraw`, and they
only dissolve when the READERS take charlists. an earlier draft of this section put
them under A; that was the same mistake rung 7's plan made about `p0text`, made
again one level down.

**so A's case is not its own size -- it is that A GATES B.** while p0 reads through
the vt, the vt cannot go. ~20 lines to unlock ~150.

what it costs: **the cursor is a LOVE VALUE that every leaf both reads and updates**,
so it lives on `g->sp`. that is the discipline p0 already has -- *"control flow on
the C stack, values on g->sp"* -- applied to one more value, and it is where the
whole risk of this rung sits. `g->io` did that job today, GC-traced, for free.

#### WHAT THE BOOT READS -- asked as a choice, answered as a measurement

p0 has two kinds of input, and `g->io` is what lets one lexer take both:

* the **boot** (`p0onto`) walks a C string through a stack `ti`, at **zero
  allocation**. it is called per text; the largest is prel.h at **17,646 bytes**.
* **`sound0`** takes a love charlist, because the differential needs it to.

so the cursor is ONE charlist and the boot conses its text first. a cons here is
`struct ai_chain { lvm_t *ap; intptr_t a, b; }` -- **three words** -- so prel.h
costs ~424KB on 64-bit and ~212KB on 32-bit, transient, and it is the PEAK rather
than the sum because the `p0onto` calls are sequential.

⚠ **this was written up as a decision about the tiny targets, and that was wrong.**
the reasoning was: teensy41 hatches the egg on device out of a 384KB pool, so
212KB would not fit. every part of that is a misread of the comment above the pool
(port/teensy41/main.c:194-201), which says the opposite:

* teensy41's arena of FIRST RESORT is the **16 MB external PSRAM** -- *"the same
  arena size the qemu-M7 port bakes in"*, and port/mps2's baker is 16 MB too.
* the 384KB OCRAM2 pool is the **no-PSRAM fallback, and it ALREADY cannot bake** --
  *"the old 384 KB OCRAM2 pool starved the bake (the first-silicon blocker)"*. it is
  a recorded failure, not a live budget.
* and teensy41 is **WAKE-FIRST** anyway: it wakes a flash image baked by mps2, and
  the on-device bake is the ~55s fallback path.

so the real budget is 16 MB and the charlist is **1.3% of it**. no decision, no
fork, and `test_mps2` -- which bakes from source on the emulated M7 at the same
16 MB -- is a faithful gate for it.

⚠ the pattern in the mistake is the one to keep: **a comment's WARNING read as a
CURRENT CONSTRAINT, without checking which branch is live.** that is now three
times in this rung's planning (the `tap`/`ci` constructor, the A-scope ledger, and
this), each one a claim about the tree made from a nearby sentence rather than from
the code under it.

(a `(text . index)` cursor is the option that stays rejected, on its own merits: it
makes `sound0` convert its charlist in and re-cons its residue out, per form, which
is quadratic over rdiff.)

**B. the input half of the port vt goes.** `ungetc_buf`, ~~`eof_seen`~~ (gone in
the device floor's rung 2b), `zgetc`/`zeof`/`zungetc` (44), `bio_rpending`,
`cue?` (6), `feof`/`fungetc`, and
`io_refill`'s re-serialization -- all replaced by ONE nif: blocking-for-first,
take-what-is-ready, answering a string. `sip`/`drink`/`slurp`/`end?` become list
ops in love, and `await` stays as the park primitive (it was already pulled out of
getc for exactly this -- *"the fds you CAN'T drain a byte at a time"*).

⚠ **B is a migration, not a deletion: 124 live `(see ...)` call sites** (plus 11 in
doc/proto). many are byte-protocol readers -- lux's X11 wire, kiosko, seed's http
-- that want "exactly n bytes" rather than a text stream, and a colist serves them
but only after a rewrite. so B is worth it only where the site genuinely wants a
STREAM, and `see` may well deserve to survive as the port door for the rest. do
not let A wait on that argument.

#### ⚠ and the question to settle before B: WHO OWNS THE BYTES

today one port is one buffer, so the editor and a program that reads `in` share a
position. with colists, **whoever holds the head owns whatever was gulped.** that
is already half-true (`drink` gulped ahead, `flow` does), but B makes it total, and
stdin is where it bites: the repl reads a line, evals it, and the evaled form reads
`in`.

fd lifetime is NOT a problem and was the first worry to check: `io_close`
(love.c:4050) is a GC finalizer, so an abandoned colist's fd closes when its thunk
is collected.

#### what the probes said (2026-07-31), and the hole they left

five probes, all in scratchpad -- nothing in the tree moved.

**the over-read is real and it is ONE call site.** `printf '(say out (slurp in))\n(say
out "second")\n' | love` answers empty and then runs the second form; bash on the
same shape hands the rest of the script to the reader and does not run it. but the
33 tracked stdin sites are two families and one exception: 18 `(slurp in)` in kore,
every one the `(? (f = "-") (slurp in) (uread f))` idiom, which takes ALL of it and
leaves no residue; 12 `(see in)` interactive key decoders, one byte used
immediately. **`love/cli.l:81` is the whole problem.** a session-wide ownership
protocol to fix one line is the wrong size of answer, and that retired the "slot"
shape before it was written down.

**bash's answer, measured:** an `lseek` probe at startup, then block reads plus
`lseek(0, -23, SEEK_CUR)` before a child on a seekable fd, and `read(0, …, 1)` per
byte forever on a pipe. `kore sh` on a piped script is 44 single-byte reads and is
correct -- including for a genuine spawned `/usr/bin/cat` that inherits fd 0.

**the seek handback works here too:** gulp the whole file, read one form, seek back
by the residue, spawn `cat` -- fd goes 50 -> 15 and the child reads exactly the
residue. the arithmetic is sound because the colist is byte-granular (42 cells for a
42-byte UTF-8 file), which the design would then silently depend on.

**vi.l's decoder over a colist agrees byte for byte**, and what it retires is not
lines: `(rd pend)` answers `(byte next-pend)` where `-1` means "nothing pending",
while `go` tests `(< c 0)` for end-of-input -- **the same sentinel meaning two
things in one function**, presence encoded in the net. over a colist neither is
expressible: the byte you did not want is still in the stream you did not walk past,
and the end is `()`. and `unsee` -- the C ungetc nif; the vt slot behind it went in
the device floor's rung 1 -- has NO users in the tree outside `test/io.l`, which
tests it.

**⚠ THE HOLE, and it is the reason B is still not started.** `in` as a colist bound
at boot does not answer ownership -- it MOVES it into "how much does one force
take". `flow` gulps, and after a gulp `(see in)` answers `-1` while the bytes sit in
the colist (probed). bao's editor reads through `getc` -> `(see in)`, one byte, which
is *why* lush on a pipe keeps fd 0 positioned for its children -- that correctness is
incidental, inherited from an editor written for a tty. make `in` a gulping colist
and lush's child inheritance breaks. a byte-per-force colist would be correct and is
exactly what rung 7 rejected on cost (bao.l:57: `crew/moon/gen.l` is a single 360KB
form).

so **gulp granularity IS the ownership question wearing a second face**, alongside
readiness -- `cue?` is gulp's stop condition, rove.l:178's ESC-vs-CSI discriminator,
and the thing that makes the tty case free. one input representation was the right
call and it retires real machinery; it is not by itself an answer here.


## order and size

**1-4 are LANDED.** 1 went first because everything after it re-implements what
it states, and it was useful alone -- the tree gained a conformance file and lost
a special case. 2 deleted three existing workarounds rather than adding
machinery, and took the freestanding `strtol` with it. 4 was the 15-line refactor
it looked like, plus one design decision the mop forced. 3 turned out to be
slightly more than assembly -- the input set is the whole tree rather than the
fuzzers -- and it paid for itself immediately by finding the `#()` nil tail.

**1-6 are LANDED, boot included.** 5 was a port of working code
(`2efbaa51^:g/g.c:1286-1400`) and came in at 30 code lines against the estimated
80, by sharing the leaf lexers instead of copying them -- the same move rung 2
made for the number tower, for the same reason. 6's parser then read the whole
tree identically on the strength of that same oracle, and 6b put it on the boot
path of every target for 40 more lines of C.

the plan's own estimate held exactly -- "most of its risk is in the boot rather
than in the parser". both of 6b's surprises were in the boot half and neither was
about parsing: p1.l had to go **pre-opfix** as well as pre-prel, and p0 had to
read **lcat's minified output** rather than the tree's source. both failed loudly
on the first build.

**6c LANDED, and the arc is closed** -- `sound` is p1, `ioparse` is gone, and the
tree has ONE reader. 6b's vocabulary question closed after it (2026-07-30): p1 and
the opfix factor pass CLOSE their scopes rather than leaking and being mopped, so
the book carries `sound`/`torn` and nothing else of p1's -- a name that never
reaches the book cannot be forgotten from a list.

**7 LANDED** (2026-07-30), the same insight run once more. 6c stopped bridging the
reader to the port by making `sound` take TEXT; 7 stops bridging the PORT to the
reader by making a port BE text -- a charlist with a promise for a tail. the
leftover glue named in 6c's paragraph below (`reads` as "the one place a port
becomes text", the refill loop, the re-parse-per-drink hazard) is exactly what it
retired. bao.l and p1.l came to **6 code lines fewer** between them (bao -7, p1 +1)
and 21 total lines more, all of it the ⚠ comments this rung's traps earned.

it was cheap for the reason the framing predicted: **reflecting rather than forcing
implicitly** kept the whole change on the .l side of the line. `cup` was never
touched, so neither was the hot path, and step 3 of the plan -- "`sound` takes
either" -- turned out to be no work at all. the estimate that missed was the other
direction: rung 7 was written expecting to take `p0text` and `struct ci` with it,
and neither is p1's to take (the rung's own section says why).

**8A LANDED** (2026-07-31) at exactly its estimated 20 lines, and the ledger's
honesty was the point -- A was never worth its own size, it was worth GATING B,
and it does: p0 no longer reads through the port vt, so the vt's input half is
now free to go. the two things it taught are in 8's section: the cursor is named
by DEPTH because the collector moves the stack, and a LIST needs no pushback,
which is what halved the skipper and deleted the residue recovery. `struct ti`
came out with it, unplanned -- p0onto was its only constructor.

what is left of the plan: rung 2's bit ops, still independent, and **rung 8's
scope B** -- the input half of the port vt, a 124-site migration and a question
about who owns stdin's bytes. 7 earned 8 and A cleared its way: the tree ALREADY
holds a colist in C (`io_refill`'s `rbuf`/`rpos`/`rlen`) and spends `cue?`, the
pushback and the eof bit hiding it behind a byte at a time. and one the plan
never had, because nobody had measured the reader until 2026-07-31: a **`chars`
nif**, text → charlist, the inverse `string` has never had -- see the speed
paragraph below, where it is now the largest single cost in reading anything.

the predicted hard half was the PORT PROTOCOL, and it was -- but not where the
plan looked. no hot slot was needed and no lookahead-cons either: the answer was
to stop bridging. `sound` takes TEXT and hands the RESIDUE back, so the caller
owns what is left and the more-bit / port-back / help-continuation protocol has
nothing to carry. `in`/`out`/`err` are static `struct ai_io` AND image immortals
(love.c), never GC-traced, so a residue slot ON the port was never available --
which is what forced the protocol change rather than a shim, and what makes this
a strict prefix of part II's path B: when that charlist goes lazy, no CALLER
of p1 changes again. ⚠ p1 itself does -- it has to reflect on the tail, and the
sentence above was written a shade too strong. rung 7 prices that honestly.

**speed is bounded, and now measured -- twice.** reading happens once per cold
boot: the stitch reads the corpus and the double `sit` folds over already-read
forms, and the baked image path (~4 ms wake) reads nothing at all, which is what
users run. at the flip p1 read ev.l in 62 ms against the C reader's 4 ms, so the
cold boot paid ~+58 ms of its ~1 s -- and the arc was willing to leave it there.

**the second measurement took most of it back** (2026-07-31, `4b5f71bf`). the
cost was not parsing, it was DECIDING WHAT KIND OF CHARACTER the reader was
holding: `p1-in?` walked a class string per test and minted a fresh `go` closure
and a `tally` before it looked at anything, so `p1-tend?` cost fifteen string
indexes for every character of every token. one 256-byte string, a bitmask per
char, and a membership test is an index and an `&`:

```
              ev.l   prel.l   gen.l   spec.l          (ns/char)
  before      716      810      849     1143
  after       178      207      222      540
```

ev.l reads in ~8 ms now and the cold boot pays ~+4 ms. the same table went into
crew/moon/lex.l (2.7x, where the bigger half is bucketing the multi-char
punctuators BY FIRST CHAR -- `firstat` walked all 21 on every punctuator token).
⚠ **bit 0 is left unused in both tables**: a string indexed past its end answers
the charm 1, so a char outside 0..255 would match whichever class held bit 0 --
silently. empty, `(& 1 m)` is 0 for every mask used and an out-of-range charm
matches NOTHING, which is what the scan it replaced answered. free, where a
bounds guard measured 15%. spec.l gains half what the others do, and that NAMES
THE NEXT SEAM rather than disappointing: its time is in the operator-run lane.

⚠ **the biggest item left is not in the parser at all.** text → charlist costs
63 ns/char against 178 to PARSE the same text, so a third of the reader's cost
is spent before it sees a byte. there is no `chars` nif -- `string` goes
charlist → text and its inverse does not exist -- so every caller of `sound`
pays `map s (jot n)` in love. one nif closes it.

**and rung 8A already paid for the next one.** p0's leaf lexers thread a
charlist cursor now, which is exactly the shape p1's token lane wants: over a
cursor, `ioread1sym` IS `(atom cl) → (value . rest)` and `ai_z_getc` IS
`(skip cl)`. what was a rung is a door. ⚠ price it honestly before building it,
though -- `string` + `intern` costs ~107 ns/token whoever does the scanning, so
the token lane has ~2x in it, not 10x.

⚠ **and "the glaze will NOT help" needs its caveat.** that was said of the
SCAN-based reader -- its grammar is integer arithmetic over frame params, and a
reader is strings, noms and cons cells, exactly its declining set
(test/test.mk). the class table is what changes it: the inner loops are now
`int | (OP E E) | (? E E E)` plus one byte load. what blocks the glaze is two
missing pieces rather than the shape of a reader -- a **string-index leaf op**
(a bounds-checked byte load) and a **scan-loop recognizer** (a tail-recursive
while answering an index; the existing LOOP recognizer takes counted SUMS only).
clex is already index-based over a string and would take both today; p1 cannot
while it walks conses, which is one more argument for a chunked cursor. mooncc
stays the precedent that this is fine either way -- 9200 lines of love, 20 ms
per TU warm.


---

## part II -- the port surface, re-scored

stream.md was written to explain a deadlock and proposed replacing the port
surface wholesale ("path B"). three of its pieces have since landed by another
road. what follows is its diagnosis checked against the tree as it is on
2026-07-31, line by line, by an audit that probed rather than inferred.

### the symptom it was written for

### the symptom it was written for -- ✅ GONE, re-tested 2026-07-31

`wrap` is a transparent pty pump and works: `stdin -> master` in one task,
`master -> stdout` in another, the two park on different fds and interleave. the
rlwrap upgrade swaps the input side for a line editor -- `feedlines` reads keys
via `edraw`, renders to `out`, and sends to the child only on Enter -- and it
**hung after one keystroke**. single-task `edraw` on `in` worked; a lone `see` in
a spawned task worked; the concurrent two-task park was what broke. built,
reverted.

**it does not hang any more.** the topology now runs green as a gate --
test/host/pty.l, in `test_hostnif`: two ptys (one plays the keyboard with echo
OFF so escape bytes arrive raw), `feedlines` rendering to `out` in one task,
`pump` parked on the other fd in a second, fed `"ab"` LEFT `"X"` Enter. the
arrow DECODES, the `X` lands before the `b`, and the child answers `"AXB\r\n"`.
clean teardown, both tasks joined.

TWO independent things had fixed it, neither of them noticed:

1. the C guard (defect 1 below) now consults the pushback and the buffered run
   before the fd.
2. **`besc` does not push back at all any more** -- it reads and DISCARDS
   (`love/bao.l:465`, `(? !(91 = see ip) 0 …)`). the editor was reworked at some
   point after the diagnosis was written, which is why its cited line numbers
   (`bao.l:69-86`) no longer point at it. so the mechanism the diagnosis named
   could not fire even if the guard were still wrong.

⚠ **no commit ever carried `feedlines`** -- `git log -S` finds nothing. the
snippet above and the gate are the only copies.

### the four defects, scored

**1. `ungetc_buf` invisible to the readiness check. -- FIXED, then the check
itself went.** this was the diagnosis's centrepiece: `lvm_fgetc` parked whenever
`!ai_ready(fd)` without consulting the pushback, so a task could park holding the
very byte it needed. the first fix made the guard read three terms in order --
pushback, then the buffered run, then the fd. **rung 2 deleted the guard
outright**, because that is the same three things `zgetc` reads, in the same
order, one call further down: pushback, pending run, `io_refill`. asking first was
a third copy of a test the read already makes, and on the kernel it was also a
LIE -- `k_sources[1].ready` is NULL, so reading an output fd parked forever on a
wait nothing could satisfy, where it now reads the end the dispatcher promises.

`cue?` and `await` still ask `ai_ready`, and must: neither has a read to answer
them. the asymmetry stream.md called "the bug" is gone by having one door, not by
keeping two in agreement.

**2. the `-1` EOF sentinel. -- STANDS, but confined.** `love.c:4005-4006` still
answers `putcharm(EOF)`, and byte loops still compare `(= c -1)` / `(< c 0)`
(`love/bao.l:29,31,425`, `crew/kiosko/kiosko.l:73`, `crew/lush/job.l:44`). what
changed is that the READER no longer sees it: `flow`/`sound` present input as a
charlist ending in `()` (test/io.l:91,114). the sentinel now lives only at the
`see` primitive and its direct callers. rung 8's probe found the sharpest example
in `crew/vi/vi.l:56-65`, where `-1` means BOTH "no pushback pending" and
"end of input" inside one function.

**3. fd/poll knowledge smeared across the VM. -- NARROWED to the scheduler and
its two questioners.** it was five sites and two of them have gone: `io_refill`'s
own blocking wait (defect 5) and `lvm_fgetc`'s readiness pre-guard (rung 2). what
is left is `lvm_await`, `cue?`, and the scheduler itself -- and that is not a
smear, it is the shape: **`ai_ready` is the SCHEDULER's question**, "would this
task make progress if I ran it?", asked by `find_runnable`, by the two wait
helpers, and by the two nifs that park without a read to answer them. the read
path no longer asks it at all. the doc's claim that `cue?` hardcodes `ai_stdin`
is only half true; it defaults there for a non-port.

**4. the write side never yields. -- ✅ FIXED, rungs 3 + 4, 2026-07-31.** rung 3
took the half NOBODY had named: the path above `fd_writen` threw the count away,
so the bytes a stalled or dying fd refused were silently dropped instead of kept.
rung 4 took the named half: a heap port's door is one nonblocking `write(2)` now
and answers what it took, so a peer that will not read stops the one task writing
to it instead of the whole VM. see "the write door, and where waiting still
lives" below -- the fix is not symmetric with the read side, and the reason is
worth reading before touching it.

### 5. the buffered read lane BLOCKED where the byte lane PARKS -- ✅ FIXED 2026-07-31

`io_refill`'s loop (love.c:3170-3177):

```c
intptr_t k = vt->readn(g, (unsigned char*) txt(r), r->len);
if (k > 0) { ... return g; }
if (k < 0) { b->io.eof_seen = putcharm(true); fc->b = EOF; return g; }
ai_wait_fd((int) getcharm(b->io.fd), 1, 0); } }
```

`fd_readn` (host/main.c:109-117) answers 0 **only** on EAGAIN -- a real EOF maps
to -1 -- so `k == 0` means "would block". and `ai_wait_fd(fd, 1, 0)` reaches
`poll_wait` with `ms == 0`, which is spelled `t = -1`: **poll blocks
indefinitely** (host/main.c:34-43).

so one port had two doors with opposite scheduling behaviour. the per-byte lane
parks the task and lets the scheduler run someone else; the bulk lane blocked the
whole VM on one fd.

**REACHABILITY, settled by probe rather than by argument.** no in-process schedule
can reach it: `lvm_fgetc`'s readiness guard and its refill are ONE op, and the vm
yields only at an `Ap`, so nothing can run between "poll said ready" and "read said
no". what CAN reach it is a second process sharing the open file description and
winning the race.

that was demonstrated once, under a temporary fault hook in `fd_readn`: armed
against a pty master with a ticker task alongside, **the whole process hung** --
ticker, main task, everything, not just the reader. so the failure was real, not
inferred.

**the fix is the answer to open question 2: a waiting reader PARKS.** `io_refill`
no longer waits at all -- on a would-block it answers `IO_WOULDBLOCK` (a sentinel
distinct from EOF and from any byte), and `lvm_fgetc` parks the task on the fd with
`Ip` unadvanced, so the op re-runs and re-checks readiness. that is precisely what
its own guard already did; the bulk lane had simply grown a second, worse answer to
the same question.

#### the branch was untested -- ✅ GATED 2026-07-31 by a test-only frontend

the debt this section recorded is paid, and by the remedy it named. the fault hook
was removed (gwen's call) and is not coming back in that form: **love must not carry
a feature whose only purpose is to let a test break it** -- neither an env var read
by the shipped binary nor a nif on the public surface is acceptable language surface
to buy a test.

**`test/front/` is the answer.** `liblove.a` is love.c ONLY (host/build.mk:140;
`host/*.c` links direct, :23), so **the port vt has always been the frontend's
responsibility, not the runtime's** -- and a frontend that lies to the runtime is
test code. `out/host/front` links the archive, supplies the whole contract itself,
and serves SYNTHETIC devices: a device is a byte queue fed from `.l`, so every
schedule is deterministic. `(stall p k)` arms k would-block answers on `readn` while
`ai_ready` keeps saying yes -- which IS the race, since the readiness check saying
go and the read saying no is the whole condition. `test/front/io.l` carries six laws
and `make test_front` gates them (in `test_slow`).

⚠ **it is a deadlock detector as well as a fault injector**, and that turned out to
be most of its value: a synthetic device can only be fed by another task, so a wait
with NO DEADLINE means every task is parked, and the frontend exits 97 with a named
reason instead of hanging until the harness kills it.

**sabotage-proved, both halves** (2026-07-31, against patched copies of love.c built
outside the tree):

* restore `io_refill`'s blocking wait and the gate dies `exit 97 -- a sleep with no
  deadline, every task is parked`. that is the whole-VM freeze, now loud.
* delete `lvm_fgetc`'s `IO_WOULDBLOCK` park and law 1 reddens with its own source
  form -- the sentinel leaks out as a byte and `"abc"` comes back wrong.

⚠ rejected alternative, still recorded so it is not re-proposed: a genuine
two-process race needs no surface but is non-deterministic, and a gate that usually
reddens teaches people to ignore it.

what is still NOT built, and is now cheap: the structural check in `vmret`'s spirit
-- `ai_wait_fd` has exactly two call sites, both in the scheduler (love.c:2696,
2698), so a build-time assertion that no io-path function calls it would pin that
the blocking call cannot come back. ⚠ on its own it gates the CAUSE, never the
effect; with `test_front` gating the effect, both halves are finally coverable.

### the read side, closed -- ✅ rung 2, 2026-07-31

**`readn` is the whole read door, on all seven frontends.** `getc` did not become
nonblocking; it was DELETED. `readn`'s contract was already the answer -- *">0 =
bytes, 0 = nothing waiting right now, -1 = end"* -- and widening `getc` to a fourth
answer would have meant two spellings of one fact in nine places.

what that bought, which is more than the line count says:

* **five frontends stopped spinning.** `kb_getc`'s `while ((b = kqpop()) < 0)
  fbdraw(), k_wait();` is the clean example: it COMPUTED the answer and threw it
  away, then waited inside a VM op. virt, mps2 and teensy did the same against
  their UART's own RX-ready flag. all four now read the flag and answer 0. (⚠ the
  kernel's spin was in fact unreachable, because the pre-guard parked first --
  which is the point: it was one edit away from being reachable, and nothing said
  so.)
* **the untested branch became the only branch.** `IO_WOULDBLOCK` used to need a
  fault injector to reach. now every quiet device on every frontend comes through
  it, so the whole corpus walks it and `test_front` gates the sharp case (ready
  said go, the device said no).
* **`struct k_source`'s "-1 = EOF / no data"** -- one sentinel, two meanings, in a
  document that records three such bugs -- is spelled apart.
* `ci_getc` -> `ci_readn`, dropping its private copy of the pushback dance (`zgetc`
  reads `ungetc_buf` before it dispatches, so the copy could never fire) -- **and
  fixing a forged end of stream in passing.** `readn` fills an `unsigned char*`, so
  a charlist element now lands as its low byte; `ci_getc` handed the raw charm to
  `g->b`, so a `-1` in the list read as EOF. probed against both binaries:
  `(flow (tap '(-1 65)))` came back EMPTY before and is 2 long now. the damage was
  never at `see` -- which answered -1 and carried on, so nothing looked wrong --
  but at `flow`/`sound`/`reads`/`slurp`, every reader that stops there. a port is
  a byte stream on every other row; this was the exception. law in test/io.l.
* `noop_getc` and the "unused slots get noop stubs so dispatch needs no NULL
  guards" rule go together. **the kernel's rule wins tree-wide**: a NULL slot means
  NO METHOD and the dispatcher answers for it (no `readn` reads the end).

vt: 5 slots -> 4. shipped C: **-54 code lines** (115 deleted, 61 written), which
is where the plan's -48 estimate landed.

⚠ **the price, measured and accepted.** the unbuffered statics now pay host's
per-call `O_NONBLOCK` toggle on every byte: 3 `fcntl` + 1 `read`, where the old
pre-guard cost 1 `poll` + 1 `read`. 26 reads for 26 bytes either way -- the
syscall COUNT per byte went 2 -> 4, the read count did not move. 53 KB of source
through stdin still lands in 0.05s, and every other fd is a heap port gulping
4096 at a time. we skip the `fcntl` pair when the fd already says nonblocking; we
do **not** cache flags for an inherited fd, and the statics do **not** get a
buffer -- that would make the repl swallow the line after the one it is reading
and re-open part III's ownership question.

⚠ **what is still ungated, unchanged by this rung**: no gate feeds a keystroke to
inle, virt, mps2 or teensy -- `boot.sh` runs qemu `</dev/null` on purpose (a
non-definite stdin hangs the harness). their `readn` is exercised by dispatch and
by review, not by a byte arriving. that was equally true of the `getc` it replaced.

### the port head, three words -- ✅ rung 2b, 2026-07-31

`eof_seen` is gone, and with it the idea that **the end is a state a port
remembers**. it never had to be: a spent device answers `-1` to every ask, and
every device in the tree does, so the latch only ever agreed with the thing that
set it. that promise moved to where it can be enforced -- love.h's `readn`
contract now states it as an obligation on the DEVICE, and test/front/io.l's law 3
is the witness that reddens if a frontend ever answers the end once and then goes
quiet.

what it cost is the layout, exactly as scheduled: `struct ai_io` 4 words -> **3**
(so `struct ai_bio` 9 -> 8, `struct ci` 5 -> 4, `struct to` 6 -> 5), and prel's
`tap`/`jug`/`slurp` renumbered to match (prel.l:253-259). ⚠ **the image did not
care**, as the rung-1 note predicted: it is binary-specific (love.c's anchor +
refsym check), so an image laid by any other binary is refused and falls back to a
normal boot. `test_wake`, `test_mps2_wake` and `test_dist` all green with no bump.

shipped delta: **-9 lines** against an estimate of -25, and the estimate was the
honest one to miss -- nine of the sites are a single-line static-port initializer
that stays a single line. the ledger this rung actually moves is a word off every
port and a state off the model, not a line count. ⚠ `port/rp2040/main.c` still
names `eof_seen`; it is dead (pre-rename `struct g` API, no in-tree build) and
stayed untouched here for the same reason rung 2 left it alone.

⚠ **and it turned up a GC bug that was never this arc's.** losing 24 bytes moved
allocation enough that the aarch64 K_TEST kernel jumped into the heap, at `-m 512M`
and nowhere else. `obin_elem` held `g->ip` in an unrooted C local across
`ai_big_binop`, which allocates: the collection promotes the running thread and
updates `g->ip`, then the restore writes the from-space address back, so the next
dispatch calls the forwarding pointer sitting there. one `avec` fixed it. it is
generic C on every target and had survived every gate -- `AI_GC_CHECK` verifies the
COLLECTOR and this is the MUTATOR breaking the rooting contract, so nothing looks
for it. the gate that would find its siblings (a GC-stress build in the
`AI_GC_CHECK` mould) is proposed and unbuilt.

### writen is the whole write door -- ✅ rung 3, 2026-07-31

the `putc` slot is gone and the vt is **three slots** -- `flush`, `writen`,
`readn` -- which is the shape this whole arc was aimed at. every implementation
of `putc` was its own `writen` at n = 1, and the six freestanding frontends had
no `writen` at all, so each traded a per-byte body for a bulk one.

the slot's REAL job turned out to be elsewhere, and naming it is what let it go:
`putc` was **the only write path allowed to allocate**. that is why the bulk lane
answered "0 = no room without an alloc" and bounced back through it. `writen`
allocates now -- it takes the frame BY ADDRESS (`struct ai **`, the shape
`obin_elem` already uses) so a scare rides out -- and there is nothing left to
bounce to. ⚠ the grow and the copy still cannot share a call: `str0` collects and
`src` may be the very heap string being printed, so `to_writen` grows, answers 0,
and lands nothing; the caller re-derives and comes back. zputc's one-byte lane
holds src in a C local, which the GC never moves, so its second ask always lands.

**two silent bugs closed, and each has a law that reddens on exactly its own fix**
(test/front/io.l laws 7 and 8, verified by putting each bug back):

- **truncation.** `io_wdrain` zeroed the pending length BEFORE the stroke and
  threw `writen`'s count away, so a device that took part of a run reported a
  clean write of bytes it never got. the ordinary EPIPE/ENOSPC path, on every
  port. the residue slides to the front of the buffer now and the next drain
  carries it; `zputc` grows the backing when a drain leaves it full, so nothing
  overruns and nothing is dropped.
- **shuffling.** `lvm_fputs` had two lanes -- a direct `writen` stroke and a
  buffering `zputc` fallback -- and took the direct one even with bytes already
  parked in the port. a refusal mid-string put byte 0 in the buffer and sent
  bytes 1.. straight past it. unreachable on a blocking host fd (once `write(2)`
  fails it keeps failing, so everything buffers), and routine the moment rung 4
  makes a refusal ordinary. the direct stroke is now only for an empty buffer.

also gone: host's stdout `fflush` dance (one door, no ordering to keep -- stdout
routes through `fwrite` for the buffering the static port cannot have), `noop_putc`,
`to_putc`, and `io_wdrain`'s per-byte fallback loop.

shipped delta: **+26 lines**, against an estimate of -12. the estimate assumed
deleting a slot subtracts; it doesn't when six of the eight frontends have to
GROW the surviving one, and when the residue needs `bio_wgrow` and a real loop
where a discard needed neither. judge the rung on the two bugs and the slot, which
is what the plan said to do -- the line count was the part of the plan that was
wrong.

### the write door, and where waiting still lives -- ✅ rung 4, 2026-07-31

a heap fd port's door is **one nonblocking `write(2)`** now: a per-call
`O_NONBLOCK` toggle (the same one `fd_readn` has carried since rung 2, for the
same reason -- the flags ride the open file description a pty child shares) and
an answer of what that stroke took. `io_wdrain` re-offers the rest at the next
write op. a peer that will not read now stalls the one task writing to it.

**the fix is NOT symmetric with the read side, and the asymmetry is the design.**
a read that finds nothing can always park: the byte is still on the device and
`lvm_fgetc` re-runs. a write that lands nothing has bytes IN HAND and must put
them somewhere -- so a door may only refuse a port that keeps a **write run**:

| port | keeps a run? | its door |
|---|---|---|
| heap fd port (`open`, `pipe`+`fdopen`, socket, pty master) | yes -- `bio` `wbuf` | nonblocking, may answer short |
| the three statics (in/out/err) | no -- nothing traces a static | lands what it takes |
| the `to` string sink (fd -2) | it IS the run | grows, never refuses twice |
| freestanding frontends (kernel, virt, mps2, teensy41, playdate, wasm) | no heap ports at all | per-byte over a UART, bounded by a device that drains |

the statics are the sharp one. `zputc` on a bufferless port has **nowhere to
park mid-shape** -- it is called from inside `ioput_map`/`ioput_chain`, which are
not op boundaries -- so a refusal there is a byte on the floor. their door lands
what it takes, and that is the one wait host/main.c is still allowed. bounded: a
console drains.

**what rung 4 traded, stated plainly.** `flush` used to DELIVER, by blocking. it
means TRY now. delivery finishes at the next write op on that port, at `close`
(`ai_io_wflush`, which hands what the try leaves to the blocking `ai_fd_drain`
-- a port being closed has no later op to retry in), or through the finalizer.
rung 5 is what makes an unwritten residue finish on its own.

⚠ **the remaining unbounded wait, named: the finalizer's drain.** a port that
becomes unreachable with a residue the device will not take blocks inside GC,
where nothing can park. this is not new code -- `io_close` -> `ai_fd_drain` has
been there all along -- but rung 4 is what makes a residue REACHABLE, so it is
newly reachable too. probed: a 4 MB `say` to a loopback socket nobody reads
returns in 19 ms and then the program hangs at teardown, where before rung 4 it
hung inside the `say` instead. **the hang moved; it did not go.** a bound would
be exactly the arbitrary constant this arc exists to delete, and dropping the
bytes would be the silent loss rung 3 exists to stop, so it waits for rung 5 --
after which a reachable port drains as the scheduler runs and the finalizer sees
a residue only when the peer truly never reads.

the gate is in test/host/run.l: `sh -c "sleep 1; cat"` reads nothing for a
second, a 200 K `say` into a 64 K pipe cannot land its tail, and the law is that
the op comes back in under 500 ms. ⚠ **the peer must be another PROCESS.** an
in-process reader task cannot drain the pipe while the writer sits inside a
blocking `write(2)` -- the scheduler is cooperative and never gets the turn -- so
the control DEADLOCKS instead of reddening, and a gate that hangs is worse than
one that fails. (the pty was the first instrument tried and is the wrong one: a
tty in canonical mode DISCARDS input past its queue rather than blocking, so
200 K through a pty master "succeeds" in 3 ms on either door.)

### the boundary principle -- keep, it is still right

> the core knows **generic-apply + scheduler-yield**. "bytes out of an fd" is a
> HOST concern, presented as a value of an existing kind -- the chain (a lazy
> one). fd-awareness meets the scheduler at EXACTLY ONE site: the thunk that
> forces a stream's tail.

### what of path B is left

`source` is `flow` (love/bao.l:66-68) and needs no C. `read`-as-a-fold is
`sound`. `ready?` is `cue?`. what has NOT landed:

- **`(select ss)`** -- block until one of several streams is ready. stream.md
  called it "the one genuinely-new primitive", and the reason stands on its own
  terms: you cannot wait on two streams with `cap`/`cup` alone, because forcing
  one commits you to it. ⚠ **but its MOTIVATION is gone.** it was justified by
  `wrap` needing to wait on the kbd/master pair, and `wrap` now runs that
  topology green without it -- two tasks parked on their own fds is what the
  scheduler already does correctly. build it when something actually asks;
  nothing does today.
- **`sink`** -- the write dual, so a pump reads symmetric. low value; `dot`
  already works.
- **the write-side park** (defect 4).
- **the blocking refill** (defect 5), which did not exist when the list was made.

### the delete ledger, corrected by measurement

stream.md's stage 4 said to remove `see`/`unsee`/`end?`/`key?`/`ungetc_buf`/
`eof_seen`. an audit on 2026-07-31 counted the actual users (tracked files only
-- a plain recursive grep hits `.claude/worktrees/` and inflates every number):

| name | real name | tracked `.l` sites | verdict |
|---|---|---|---|
| `see` | `lvm_fgetc` | ~82 across 29 files | the sole input lane, everywhere |
| `readn` | `fd_readn` | -- | **the sole read DOOR** since the device floor's rung 2 -- all seven frontends, buffered or not. it was host-only, and the other six fell through to a per-byte `vt->getc` that SPUN |
| `unsee` | `lvm_fungetc` | **3**, all `test/io.l` | load-bearing: `:100` is the ONLY witness in the corpus that `flow` memoizes |
| `empty?` | `lvm_feof` | **0** | genuinely unused -- and not free at the time: a bound global with nine `*_eof` bodies across seven frontends. ✅ **GONE 2026-07-31**, with the whole `eof` lane, once the device-floor arc moved all seven frontends anyway |
| `cue?` | `lvm_key` | 6 | `gulp`'s stop condition, `rove.l:178`'s ESC-vs-CSI discriminator, and what makes the tty case free |

what was removable on 2026-07-31 with zero behaviour change: a dead `lvm_getc`
extern declaration and a comment naming two deleted functions. **that is the
entire list** (commit `457f19d4`). `end?` and `key?` do not exist under those
names.

---

## part III -- who owns the bytes, and the way out

neither document asked this, and it is now the live question. stream.md's
`source` has it too: a lazy memoized chain over an fd is a COPY with its own
position, and the doc never considered two readers of one stdin.

### the problem, stated

`(reads in)` flows stdin, so a form inside the script that reads `in` finds
nothing -- and the next form still runs, because the colist held it:

```
$ printf '(say out (+ "rest: [" (+ (slurp in) "]")))\n(say out "second form ran")\n' | love
rest: []second form ran
```

**it is ONE call site.** `love/cli.l:81`. the 33 tracked stdin readers look like
a migration and are not: 18 are kore's `(? (f = "-") (slurp in) (uread f))` idiom,
which takes ALL of it and leaves no residue; 12 are interactive key decoders that
use one byte immediately. a session-wide ownership protocol to fix one line is
the wrong size of answer, and that killed the "slot" shape before it was written
down.

### what our peers do, measured

three camps, all probed rather than remembered:

| camp | who | behaviour | cost |
|---|---|---|---|
| **read the whole program first** | python, node, perl, ruby | script consumed entirely; `stdin` reads empty; every later line still runs | no ownership question, because there is no interleaving |
| **interleave, keep the FD exact** | bash, zsh, tclsh | a command that reads stdin gets the remainder and the shell stops | an `lseek` probe at startup, then seek-back on files and **one `read()` per byte** on pipes, forever |
| **interleave at DATUM granularity, one port** | guile | `(read)` takes exactly the next datum and execution carries on | none |

guile, verbatim:

```
$ printf '(display (list (quote got) (read)))(newline)\n(display "SECOND-RAN")(newline)\n' | guile
(got (newline))SECOND-RAN
```

form 1's `(read)` took `(newline)` -- the next datum, no more -- and `SECOND-RAN`
still ran. no seeking, no per-byte reads, no protocol. it works because the
REPL's reader and the user's `(read)` are THE SAME PORT WITH ONE POSITION.
nothing is copied, so nothing can disagree.

bash's two branches, under strace, for contrast: `lseek(0,0,SEEK_CUR)` then
`read(0,…,71)` then `lseek(0,-23,SEEK_CUR)` on a file; `read(0,"e",1)`,
`read(0,"c",1)`, `read(0,"h",1)` … on a pipe. `kore sh` on a piped script is 44
single-byte reads and is correct for a genuine spawned `/usr/bin/cat` -- **but
that correctness is an ACCIDENT.** lush does not read stdin at all; it goes
through bao's editor, which reads one byte at a time because it was written for a
TTY. nobody chose the discipline that keeps fd 0 positioned for a child.

### the semantics we want: camp 3

one port, one buffer, one position. a reader takes exactly what it needs;
in-process readers share coherently. **love had this before `flow`.** the culprit
is not the colist and not the reader arc -- it is THE GULP, which predates both
(`drink` gulped ahead, `flow` does): it takes everything ready and hands back a
head DETACHED from the port. two positions where there was one.

### ⚠ but it is not only an implementation question

two things are still SEMANTICS, and the guile model does not settle either:

**1. persistence versus position. -- DECIDED 2026-07-31 (gwen): persistence is a
BENEFIT.** a charlist is a persistent value; a port position is ephemeral. guile
has only the position -- its buffer is not a value. love has handed the value
out, and that is what made p1 clean (`once` exists precisely so forcing twice is
safe). so **the port is a mutable cell holding a persistent list**: one CURRENT
head, with old heads still readable. two readers holding DIFFERENT heads do not
share a position, and that is allowed.

what made it an easy call is the SHAPE OF THE FAILURE. with a persistent list
nothing is ever LOST from any holder's view -- a stale holder sees what was
consumed PLUS what came after, so divergence shows up as duplication (a form run
twice, a line processed again), which is loud. contrast today, where the failure
IS disappearance: after a gulp, `(see in)` answers `-1` while the bytes sit in
the colist, unreachable from there. duplication is a better failure than silent
loss, and silent loss is the bug this tree has hit four times.

⚠ **the real cost is SPACE, and it is accepted knowingly.** a retained head pins
every byte gulped since -- haskell's classic space leak, and a copying collector
cannot help, because reachable is reachable. the program that runs afoul is not a
script but something long-lived that stashes a head and forgets: a repl keeping
input history, a server retaining a request stream, an editor snapshotting for
undo. **the rule is: do not retain a stream head.** one localized rule about one
kind of value, checkable by looking at what a closure captures -- which is why
this beat the write-back protocol, where 33 sites each owed a duty.

the canonical instance is `reads` (love/bao.l:84-90), which holds `cl` across
`(ev 'ev <r)`. under the mutable cell it must RE-READ the cell each iteration
instead of threading its own head -- otherwise a form that consumes stdin is
invisible to it and the rest of the script re-runs. one line, and the example to
teach the rule from.

FUTURE RESEARCH, not needed yet (gwen): if the chains get too long to bear, roll
them into ROPES -- a compact backing the runtime unspools transparently, possibly
compacted AT GC TIME, since the copying collector already walks and rebuilds
every live object. ⚠ it would soften the space cost by a constant factor and does
NOT make a retained head collectable, and it does not bear on persistence versus
position at all: a rope is still a persistent value. pleasingly, a rope is a
shared compact backing with per-holder offsets -- which is exactly `io_refill`'s
`rbuf`/`rpos`/`rlen`, the port buffer rediscovered as a value. the two ends of
this arc converge on one structure from opposite directions.

**2. what a waiting reader does. -- ANSWERED 2026-07-31: IT PARKS.** guile has real
threads; love has cooperative tasks, and the guile model is silent on what happens
to a task that needs bytes that have not arrived. love used to answer this TWICE
AND DIFFERENTLY -- `lvm_fgetc` parked, `io_refill` blocked. defect 5's fix settles
it on parking, everywhere: **a reader that cannot proceed yields to the scheduler,
and the VM never waits on one fd.** 8B inherits this rather than re-deciding it.

everything else -- where the buffer lives, C or love, which vt slots survive --
is implementation.

### 8B, as it actually landed -- ✅ 2026-07-31

the cell was never built, because a measurement removed the need for it. **stdin
ALREADY reads one byte per syscall**: `ai_stdin` is a static struct outside the
live pool, so `bio_of` answers NULL and `io_refill` asks the device for exactly
one byte (it fell through to a per-byte `fd_getc` when this was measured; rung 2
made it `readn(g, &c, 1)`, same syscall count). strace on `love < script`: 26
`read(0,…,1)` for 26 bytes, before and after. so `flow`'s
gulp on stdin was buying NO syscalls -- it called `see` in a loop, one syscall per
byte, and then handed back a head detached from the port. it manufactured the
second position and saved nothing.

so 8B is `trickle` (love/bao.l), flow's careful twin: ONE byte per force, tail
memoized by `once` for exactly flow's reason. it cannot run ahead, so the port and
the list share a position BY CONSTRUCTION. `reads` picks by ownership:

```
(? (id? p in) (trickle p) (flow p))
```

⚠ **GULP ONLY WHAT YOU OWN.** a file (cli.l's `load1`) and a tap (the baked corpus,
host/main.c's runner) are ours alone, so running ahead is free and worth keeping --
measured 15 ms vs 28 ms over 60 KB of file. `in` is the one shared stream.

the probe that opened all this now answers like bash and guile: the first form's
`(slurp in)` gets the rest of the script, and the second form does not run.

#### ⚠ what it broke, and why that was the point

`test_host` was `cat $t | $m` -- **the corpus was fed on stdin, and the corpus
TESTS stdin** (test/io.l's see/unsee roundtrip, whose own comment says "otherwise
the REPL parser would consume the pushed-back byte itself"). under the gulp those
asserts were VACUOUS: stdin was drained whole before the first form ran, so
`(see in)` answered EOF and the pushback went nowhere. with the trickle they became
real again -- a discarded byte and a pushed-back `99` land mid-script and the reader
desyncs on the next comment.

that test was written for the shared-position world and rung 7 had quietly made it
inert. the fix follows a precedent already in test/test.mk: `test_love0` stopped
piping its corpus for the same class of reason. `test_host` now concatenates to a
file and runs it as a program with `</dev/null`, which keeps the one-global-scope
property and frees `in`. ⚠ love0's baked image and kore's image both carry bao, so
both need rebuilding when `trickle` changes.

### the one thing the value cannot reach

a child that inherits fd 0 sees the FD, not our buffer. no representation choice
helps. bash pays per byte for it; **lush should DECLARE it** rather than inherit
it from a tty editor by accident. proved workable on a seekable fd: gulp the
whole file, read one form, seek back by the residue, spawn `/usr/bin/cat` -- fd
goes 50 -> 15 and the child reads exactly the residue. the arithmetic is sound
because the colist is byte-granular (42 cells for a 42-byte UTF-8 file), which
such a design would then silently depend on.

### the way out, in order

**not** stream.md's stage ladder -- three of its five stages already landed by
another road, and its stage 4 is mostly not removable. what is actually next,
cheapest first:

**nothing is left to settle in the abstract.** question 1 is DECIDED above.
question 2 (what a waiting reader does) does not want a decision meeting -- it
gets answered BY step 2, since the blocking refill cannot be fixed without
choosing park-or-block. so:

1. ~~re-test the bao deadlock~~ ✅ **DONE 2026-07-31, and it is gone** -- gated in
   test/host/pty.l. this deleted more of the plan than it kept: the deadlock was
   path B's entire motivation, `select` loses its justification with it, and what
   remains of part II is defects 2, 4 and 5 on their own merits rather than as
   one project.
2. ~~fix defect 5~~ ✅ **DONE 2026-07-31** -- reproduced under a temporary fault
   hook (it hung the whole vm), fixed by parking instead of waiting. it answered
   question 2 by doing, as predicted. ⚠ the hook is GONE and the branch is
   UNTESTED -- the open debt, above.
3. ~~then 8B~~ ✅ **DONE 2026-07-31, and it needed NO CELL.** see below.
4. **defect 2** (the `-1` sentinel) rides along with 8B where it touches, and is
   not worth a pass of its own.
5. **defect 4** (writes never yield) and **`select`**: when something asks. after
   1, nothing does.

~~`empty?` is unused but not free; leave it until something else in this list moves
the frontends anyway.~~ ✅ that came due: the device-floor arc's first rung moved all
seven frontends, so `empty?`, `lvm_feof`, `zeof` and nine `*_eof` bodies went with the
`eof` vt slot. ⚠ what it left behind was worth knowing: **`eof_seen` had no reader
anywhere in the tree** -- every one of them was an `eof` method -- and it went in rung
2b (below).
