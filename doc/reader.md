# the reader off C -- THE PLAN

the reader is the last big island of C that is not the runtime: 470 raw / 351
code lines of `love.c` (5.7% by code line), a hand-stacklessed parser carrying a
grammar that has outgrown it. the arc: a tiny bootstrap C parser `p0` that reads
a PURE LISP subset, and the real reader `p1` written in love on top of it --
`c0`/`ev` again, one layer down. this ORIENTS; the laws live in test/reader.l,
and every doubt settles by probing `sound`. drafted 2026-07-29.

**rungs 1-7 are LANDED and the arc is closed** -- the tree has one reader, and it
has one INPUT type, because a port IS a charlist with a promise for a tail.

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
pays zero, which is what users run.

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
cold and zero warm**, and doc/stream.md:118-135's lookahead-cons stream is still
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
- **`ti_ungetc` parks the pushed-back byte in `ungetc_buf`, not on the
  charlist**, so reading the port's `head` alone SWALLOWS the token terminator --
  `#(a)` came back as `#` with the `(` gone.

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

what is left of the plan: rung 2's bit ops, still independent, and a rung nobody
has written -- making **p0's lexers charlist-native**, which is what would actually
retire `p0text`/`ci`, and which has to answer for the boot stitch too.

the predicted hard half was the PORT PROTOCOL, and it was -- but not where the
plan looked. no hot slot was needed and no lookahead-cons either: the answer was
to stop bridging. `sound` takes TEXT and hands the RESIDUE back, so the caller
owns what is left and the more-bit / port-back / help-continuation protocol has
nothing to carry. `in`/`out`/`err` are static `struct ai_io` AND image immortals
(love.c), never GC-traced, so a residue slot ON the port was never available --
which is what forced the protocol change rather than a shim, and what makes this
a strict prefix of doc/stream.md path B: when that charlist goes lazy, no CALLER
of p1 changes again. ⚠ p1 itself does -- it has to reflect on the tail, and the
sentence above was written a shade too strong. rung 7 prices that honestly.

**speed is bounded, and now measured.** reading happens once per cold boot --
the stitch reads the corpus and the double `sit` folds over already-read forms.
p1 reads ev.l in 62 ms against the C reader's 4 ms, so a cold boot pays ~+58 ms
of its ~1 s, and the baked image path (~4 ms wake) skips reading entirely, which
is what users actually run. ⚠ the glaze will NOT help: its grammar is integer
arithmetic over frame params, and a reader is strings, noms and cons cells,
exactly its declining set (test/test.mk:130-135). mooncc is the precedent that
this is fine anyway -- 9200 lines of love, 20 ms per TU warm.
