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

### 3. the reader differential

mostly assembly, not construction. `test_love0` (test/test.mk:23) already runs the
whole corpus twice through one binary -- c0 vs the self-hosted `ev` -- and demands
TWO `tests pass` summaries, so the two-implementation shape exists. test/roundtrip.l
and test/fuzz.l:131 are already print→read differentials going through `sound`;
point them at the second reader. copy test_glazefuzz's **checked proof-of-work
counter**: a green diff over a reader that never ran proves nothing.

### 4. hold prel.l to the p0 subset

measured, code only: `` `( `` ×7 (lines 358/653/667/670), `~(` ×3 (19/21/665),
`',` ×5 (579-581/600/640), glued mono runs ×3 (631/633/636), and **zero** `#(` or
`@(`. that is 18 sites on 15 lines -- `` `(a b) `` → `(list a b)`, `~(0 1)` →
`(twin 0 1)`, `',` → `(intern ",")`. prel.l:2 already forbids infix in its own
definitions, so the discipline exists; this completes it. pure refactor, gated by
the existing corpus, useful alone as a statement of what prel may assume.

### 5. `p0`

**do not write this from scratch -- port it forward.** the whole sigil surface is
recent: `3e8d8ebd` (2026-06-09, N-ary reader operators via `dict['operators]`),
`fb4a7920` (06-10, reader infix), `6ce0be64` (06-11, "the reader is structural"),
`3d8be764` (06-11, the valence law). before that, at `3e8d8ebd^` in `g/g.c` (the
whole core was 1597 lines), the reader was **66 lines** of plain mutual recursion:

```c
g_r_getc   // whitespace + ; and # comments        ~10
g_read1    // ( , ' , "…" with \ , atom            ~46
g_reads    // the list loop                        ~10
```

it also answers the GC question by demonstration. the current reader keeps its
frame stack on the l heap because it allocates on nearly every step and a copying
collector cannot trace love values in C locals -- but the May reader shows the
shape that works: **control flow on the C stack, values on `f->sp`**. `g_reads`
loops `f = g_read1(f, i)` letting datums pile on the l stack, then folds them with
`gxr`. no love value ever sits in a C local across an allocation. (love.c also has
the `mm`/`um` shadow-stack roots -- 121-122, love.h:148 -- if one ever must.)

p0 ≈ that + a `strtod` fallback (prel has four float literals: :15 π, :17 e, :21,
:665 -- or move them past p1 and p0 stays integer-only) + `()` as the ZeroPoint.
call it **~80 lines**. pure addition: the existing reader stays, nothing calls p0.

### 6. `p1`, the reader in love

**p1 goes pre-prel** -- settled, and it is how it was written the first time. the
working lisp-side reader is at `2efbaa51^:g/repl.g`, removed 2026-05-28 by a commit
named "rm lisp-side reader". ~120 lines. it reads a CHARLIST, not a port, and
answers `(value . rest)` with two sentinels (`e` no-datum, `m` incomplete) -- so no
port protocol at all. every primitive it used is a core nif today: `link` `cap`
`cup` `two?` `nil?` `intern` `nom` `mint` `string` `tally` `snip` `peep` `gem`
plus arithmetic and comparison. the only prel-level things it touched come out
trivially: `||`/`&&` are macros (→ nested `?`), `rev` is prel.l:222 (a three-line
fold), and `foldl`/`flip` appear only in `revcat`, which the reader never calls.

⚠ **it was removed because it worked but did not pay** -- at that time the grammar
was still pure lisp, which C handled fine and faster. that history is the argument
for where this arc cuts: the removal reason applies exactly to the part p0 keeps
and not at all to the part p1 takes. `flo`/`gem` (love.c:4116) survives as its
purpose-built hook, cited to a `repl.l` that no longer exists.

**what p1 must ADD** over the historical one is exactly the surface that did not
exist in May: operator runs, the `mono` wrap, the constructor wraps with their
splice rewrites, the comma datum, the trailing-`-` shed, `[`/`{`, the prime,
bignum literals. `'`, strings with escapes, hex and decimal integers, floats and
symbols arrive free. lib/lint.l's char classes are already transcribed from
love.c (`lopc?` :44 is the complement of `op_break`, `lctc?` :48 the constructor
sigils) and are directly reusable.

**⚠ the real structural work is the BOOT, not the parser.** `ai_evals_`
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

1 went first because everything after it re-implements what it states, and it is
useful alone -- the tree gained a conformance file and lost a special case. 2 is
independent of the rest and can land any time; it is the one that deletes existing
workarounds rather than adding machinery. 3 is assembly over gates that already
exist. 4 is a 15-line refactor. 5 is a port of working code. only 6 is genuinely
new, and most of its risk is in the boot stitch rather than in the parser.

**speed is bounded, not an open risk.** reading happens once per cold boot --
`ai_evals_` reads the corpus and the double `sit` folds over already-read forms.
so p1 reads ev.l once, only cold: ~230 ms total today against ~1.2 ms of C
parsing, and the baked image path (~4 ms wake) skips reading entirely, which is
what users actually run. ⚠ the glaze will NOT help: its grammar is integer
arithmetic over frame params, and a reader is strings, noms and cons cells,
exactly its declining set (test/test.mk:130-135). mooncc is the precedent that
this is fine anyway -- 9200 lines of love, 20 ms per TU warm.
