# moon-diag — teaching mooncc to name its causes

mooncc knows why it refused. It just doesn't say. Every stage discovers a precise
cause — a missing identifier, an unparseable token, an `#error` and its text — and then
returns a bare bad value across the stage boundary, where the driver prints a generic
line naming only the file. The cause is computed and then thrown away.

This doc specifies the pass that stops throwing it away, plus the mundane
header/declaration gaps the measurement below turned up alongside it.

## why now: the gnulib measurement

Building gzip-1.13's gnulib layer (2026-07-18, the [[moon-userland]] ladder). Of the 118
`lib/*.c`, automake actually builds **37** on Linux — the rest are Windows and
other-platform lanes. Against that real build set:

**20 of 37 compile with mooncc today.** The 17 failures:

| first blocker | n | files |
|---|---|---|
| undeclared function/constant (`cgfn refuses`) | 10 | cloexec, dup-safer, dup-safer-flag, filenamecat-lgpl, ialloc, opendir-safer, save-cwd, savedir, utimens, xmalloc |
| parse error, cause not isolated | 4 | freading, malloca, mbszero, openat-proc |
| no `float.h` at all | 2 | printf-frexp, printf-frexpl |
| codegen error → missing `EXIT_FAILURE` | 1 | exitfail |

Reproduce with **`tools/moon-sweep.sh PKGSRC=<a ./configure'd tree>`** — it compiles every
`lib/*.c`, scores against `lib/Makefile`'s `*_a_OBJECTS` (the 37, not the 118), prints the
per-file causes and the bucket census, and reports the other-platform lanes separately.
Skips cleanly without a tree, like `moon-tar`. Logs land in `out/host/moonsweep/logs/`.

⚠ THE DENOMINATOR IS THE POINT — a raw `for f in lib/*.c` count says 56/118 and is
meaningless, since gnulib's `lib/` carries every platform's lane. The script exists mostly
to stop that mistake being made twice (it was made once here).

These are FIRST blockers — clearing one may expose another behind it. And gzip pulls ~37
gnulib objects where coreutils pulls several hundred, so this is a lower bound on the
general gnulib distance, not a measure of it.

**The finding that matters: 15 of the 17 reported a symptom with no cause.** The work
itself is ordinary header completeness — the same thing every rung has been. The
diagnostics are what make it expensive. `exitfail.c` is *one declaration*
(`int volatile exit_failure = EXIT_FAILURE;`); it reported `cc: codegen error in
<path>` and took five minimal test programs to pin on a missing macro.

⚠ the one diagnostic that worked was `cc: cannot resolve #include <float.h>` — named its
cause instantly, and it's the one deliberately fixed to self-name during the `cppbad`
propagation work (doc/moon-userland.md, the stdckdint.h bug). **The precedent is already
set; it just never got applied anywhere else.** This pass generalizes it.

## the diagnosis

`moon.l:63-91` is the whole story. Each stage is called, then checked:

```
lx (clex ((deftext ds9) + <>r))
_  (? (two? lx) () (udie 1 ("cc: lex error in " + src)))
pp (cpp <>lx (\ name sys? (incload is (cdir src) name sys?)) src)
_  (? (two? pp) () (udie 1 ("cc: preprocessor error in " + src)))
ps (cparse-t <>pp tgt)
_  (? (two? ps) () (udie 1 ("cc: parse error in " + src)))
```

A stage answers a two-ish value on success and something else on failure. The something
else carries nothing, so the message can only be built from what the *driver* knows: the
stage it was in, and `src`. Meanwhile:

- `gen.l:4049` — `(puts (+ ";; cgfn refuses " (<>f + "\n")))` knows the enclosing
  function and prints it to stdout as a note, then answers `()`. The driver later prints
  an unrelated `codegen error` line to stderr. Two half-messages, neither sufficient.
- `cpp.l:290` — the `#error` handler has the directive in hand and prints only
  `"cc: #error directive"`, discarding the message text and the file it fired in.
- `cpp.l:328/330` — the include lane already does this right, interpolating `spelt`.

Tokens carry line numbers (`moon.l:59`, the shared-lex comment) — so position
information exists at lex and parse time and is dropped at the raise, not absent.

## the fix: a diagnostic carrier

One value a stage may answer instead of a bare bad, carrying its own cause. Working name
**`gripe`** — a complaint that remembers what it's about. (Name offered, not settled;
per [[decisions-never-locked]] and the `grip` precedent in [[precedence-grip]].)

```
(gripe stage file line col msg)   ; any field may be () when unknown
```

`udie` grows a gripe-aware form: given one, print `file:line:col: msg`; given a bare bad,
fall back to today's `cc: <stage> error in <src>` so **nothing regresses while the stages
are converted one at a time**. That fallback is what makes this landable in rungs rather
than as one flag day.

### rung 1 — `cgfn refuses` names the identifier — LANDED

Highest leverage by a wide margin: 10 of 17 failures here, and it's flagged as painful in
three prior rungs of doc/moon-userland.md ("the refusal names the ENCLOSING function, NOT
the missing token"). mooncc refuses undeclared calls by design (no C89 implicit `int`), so
*every* missing libc prototype in the whole ladder surfaces through this one path.

At the refusal site the undeclared node is in hand — it's what failed to resolve. Answer a
gripe naming **the identifier**, keeping the enclosing function as context:

```
cc: dup-safer.c: undeclared 'F_DUPFD_CLOEXEC' (in dup_safer_flag)
```

This also retires the must-revert `cgexpr` probe hack (doc/moon-userland.md's cgfn-refusal
forensics recipe) — that recipe exists solely to recover what the refusal already knew.

**How it landed.** `gen.l` grows the carrier (`gripe`/`gripe?`/`okv?`, beside `bad?`) and
the `g`-stashed-cause plumbing: the cgexpr `var` refusal (the one path both an undeclared
value-use AND an undeclared call funnel through — `call-fixed` evaluates its head) pins
`(g 'undecl <name>)` before its `'bad`; `gfns` clears that pin per function and, on a
refusal, mints `(gripe () () () "undeclared '<id>' (in <fn>)")` when the pin fired, else
keeps today's `;; cgfn refuses` note + bare bad. A gripe IS a chain, so `cgen`/`cgen-obj`
guard `(gripe? r) r` *before* their `two?` check and ride it out to the driver; `moon.l`'s
new `ccdie` formats `cc: <file>:<line>:<col>: <msg>` (unknown file ← the source path,
unknown line/col elide) and every stage check flips `two?` → `okv?`. The address-of-an-
undeclared-name path (`clval`) still answers a bare bad — left for a later touch; not in
rung 1's 10. Laws in `law.l` flipped from `(! (cgn …))` to `(gripe? (cgn …))` for the
three undeclared seams. No message-content law: rung 2 reshapes the string with positions,
so a strict golden would only churn.

### rung 2 — positions into parse errors — LANDED

Thread the failing token's position into the gripe. A bare position is most of the value,
since it converts "bisect a flattened TU with `gcc -E -P` over brace-balanced prefixes"
into reading a number.

**How it landed.** Three pieces, and the shape of each was set by one constraint:

- **the file, not just the line.** An `#include` splices the header's *own* line numbering
  into the one flat post-cpp stream, so a line number alone can't say which file it means
  — and lines run *backwards* at a splice. `cpp.l`'s `doinc` brackets each included run
  with `fmark` tokens; `strcat-pass` (the pass that was already rebuilding the whole
  spine) retires them and collects the **spans** — `((index file)..)`, a handful of
  entries. They ride out on one `fspans` marker token at the stream's head, so `cpp` keeps
  its single `(1 out)` channel and `parse` peels them in O(1). A caller feeding raw lexer
  output has none and reads `()`. File `()` means the TU, which `ccdie` fills the source
  path in for.
- **the furthest token, not the form's first.** A recursive-descent refusal answers a bare
  `()` and unwinds carrying no position, so the report site is a watermark: the deepest
  token any lookahead stood on (`mark`, called from `want`/`peekp`/`pprim` — `pprim` is
  the expression bottom, so a bad *operand* reports itself rather than the operator before
  it). The failing form's own first token is the fallback.
- **⚠ the successful parse must not pay for it.** There is no cheap mutable scalar here; a
  tablet peep+pin per lookahead cost **+25%** on `mooncc -c love.c`. So the first pass runs
  on *unstamped* tokens and `mark` costs a cell probe and nothing else; only the re-parse
  of an already-failing form (`pfail`) stamps `(kind val line seq file)` and runs the
  watermark for real. Slots 4 and 5 are free — the cpp hideset and the lexer's
  glued-paren flag both sit in slot 4 and are dead once parse holds the stream.

Verified against gcc: a bad form in a header reports `deep.h:6` where gcc reports
`deep.h:6:11`; a bad form in the TU *after* two includes reports its own line, unskewed.
Laws in `law.l` (the diagnostics section) pin both directions plus the join below.

**Residual cost: ~5% on `mooncc -c love.c`** (3.13s → 3.30s, output byte-identical). It is
front-end bookkeeping only — the marks flowing through `strcat-pass`, and `mark`'s three
call sites. Several attempts to buy it back moved nothing (folding the strip into
`strcat-pass`, a fast path for lone strings, dropping the file stack into a stash).

⚠ **The line map is a dead end — don't spend a rung on it.** The idea: renumber included
tokens into a virtual line space at splice time, so lines run monotone and both the spans
and the watermark's seq retire. Two things kill it. (1) *Monotone is unreachable without
restructuring cpp.* `clex` could take a base and mint header tokens already biased — that
much is free, the lexer builds each token anyway. But `moon.l` lexes the whole TU in one
shot before cpp sees its first `#include`, so the TU's post-include tokens are already
stamped at base 0 and the stream still runs backwards at the splice's far end. The fix is a
pull-based lexer interleaved with `cppgo`, or an O(n) restamp — and a restamp is exactly
what cpp goes out of its way not to do: the hot path `(revcat line buf)` relinks the same
token objects, so renumbering means minting a fresh token for every token in the program.
(2) *It would not retire the watermark anyway.* `mark` is cheap today only because
unstamped tokens let it bail on a cell probe; if every token carried a monotone line, `mark`
would have to peep+pin the tablet on every lookahead — the original +25% design. Pay-on-
failure survives the line map; the seq stamping is what makes it free.

If the 5% is ever worth attacking, the lever is threading a token counter through `cppgo`
(~6 mechanical call sites) so `doinc` records spans by index directly and the marker tokens
retire — that takes the marks back out of `strcat-pass`, and with them the egg-splice hazard
below. The wrinkle is that string merges shift the indices, so the spans need correcting by
the merge count before each boundary.

⚠ **`linesplit` reads line numbers.** cpp splits a logical line by "tokens sharing the
head's line number", so line numbers are load-bearing for directive parsing, not just for
diagnostics. Any renumbering scheme must stay uniform per source line.

⚠ **`-D` skew.** The driver prepends `-D` lines as text, so every TU line the lexer stamps
sits that many lines high; `moon.l`'s `deskew` takes them back off at the report. Any stage
that learns to mint a gripe has to come through it, or it reports skewed lines. This is
also why the message carries ONE position and no more — a second line number baked into
the message string would ride out uncorrected (an earlier draft's "in the declaration at
line N" did exactly that, and was cut).

⚠ **strings join ACROSS an include.** `host/main.c` splices the egg by `#include`-ing bare
string literals between two of its own (`"("` then `egg.h` then `"'("` ..), so the strings
a mark sits between are exactly the ones phase 6 has to join. The first cut of the marks
broke this and `test_raw` caught it — `mooncc -c host/main.c` failed with a parse error on
the egg. A law now pins it.

**Still bare:** lex errors (`clex` knows its line in the `go` loop — cheap, but no failure
in the sweep needs it yet) and the `clval` address-of-an-undeclared-name path.

### rung 3 — `#error` echoes its text and file

`cpp.l:290` has both. The six `#error` hits in the sweep were all gnulib *configuration*
signals ("This platform lacks a pipe function", "Please port gnulib fseterr.c to your
platform") — i.e. not compiler bugs at all, but unreadable as-is:

```
cc: fsync.c:29: #error "This platform lacks fsync function, and Gnulib doesn't provide a replacement."
```

Distinguishing "mooncc can't compile this" from "config.h says this platform lacks the
function" is the difference between a compiler bug and a config edit, and right now the
message doesn't let you tell them apart.

⚠ and the shape is the one rung 4 retired for codegen: `cpp.l`'s two self-naming messages
(`#error directive`, `cannot resolve #include`) both `say err` and *then* return a bare
`'cppbad`, so the driver appends its own `cc: preprocessor error in <file>`. **Two half-
messages, neither sufficient** — the same indictment, one stage over. Converting them is
what makes the census below fall.

### rung 4 — a DELIBERATE refusal names the feature — LANDED

Rung 1 named undeclared *identifiers*. It did not touch the other half of the codegen
refusals: a feature mooncc simply has no lane for on this target. Those said

```
;; cgfn refuses sum                      (stdout)
cc: codegen error in vla.c               (stderr)
```

and cost a source dive every time. The case that forced this: "why don't VLAs work on
arm64?" was a one-line answer sitting in `cgdecl`'s target gate, and reading it took
grepping `gen.l`. Now:

```
cc: vla.c: no lane for a variable-length array on riscv64 (in sum)
cc: b1.c: no lane for passing this 40-byte struct by value on arm64 (in take)
cc: b2.c: no lane for returning this 40-byte struct by value on arm64 (in give)
cc: c2.c: no lane for C99 complex arithmetic on arm64 (in fmul)
```

**The rung is a DISTINCTION, not a message.** `'bad` in `gen.l` is two different answers
wearing one face — "I decline, take another lane" (`cginl`'s fesc decline, a policy deopt:
an ordinary recoverable step) and "there is no lane for this, anywhere" (a target gate, an
ABI class never built). Only the second is a diagnosis. That is exactly why the `cgexpr`
debug probe in [[moon-userland]] could never be left in: it printed both, so it spammed
every clean compile. Converting the second kind is what makes the message free.

`nolane` pins its phrase and answers the same bare bad as before; `gfns` and `cgdata`
read the pin ONLY where the function refuses outright, so a recoverable bad that pinned
on its way past costs nothing. Last pin wins — the deepest site reached before the unwind
is the one that knows. Four sites converted, chosen because they are the ones that fire:
the VLA target gate, the complex-arithmetic gate, and the composite-by-value param and
return catch-alls. The `refuse LOUD` sites on the thumb lanes are the same class and take
one `nolane` each; nobody has needed them yet.

**And the message is now ONE message on ONE channel.** It used to be two halves on two
streams — the enclosing function to stdout, `codegen error in <file>` to stderr — so a
build log capturing either one kept half the sentence, and neither half was sufficient.
Every codegen refusal now mints a gripe, including the ones with no cause to name
(`cannot compile 'f' (cause unnamed — see doc/moon-diag.md)`), and the driver puts it on
stderr. The static-data lane joined the same door: its cause rides a pin (`globs` sets it)
and `gfns`'s tail mints the gripe, because `cgdata`'s callers thread a bare `'bad` through
a dozen `(bad? x)` joins and a gripe is not one.

⚠ **every codegen refusal is now a gripe, so `(! (cgn …))` is no longer how a law says
"refused".** `law.l` grows `refused?` (`(|| (! x) (gripe? x))`) and 27 laws moved onto it;
the laws that mean "refused, and here is exactly what it said" pin `gripe?` and the message
string. A message golden earns its churn here — the string IS the deliverable.

⚠ **the laws found a real bug the driver never could.** `gen.l`'s `varix` read the 4th
element with `at` — which is *holo's* (`holo.l`), not the language's. `gen.l` is cat'd
without holo for the laws, and an unbound name there is not an error but a SILENT no-op:
`(at ty 3)` reads `((0 ty) 3)` = `(1 3)` = `3`, so every VLA site in a TU shared one slot
index and the whole lane refused. It only ever worked because the real driver splices holo
for the linker. Now spelled `<>>>ty`. **A law harness that runs a stage in isolation is
worth having for exactly this** — the ambient dependency is invisible where the ambient is
always there.

## the declaration gaps (separate, mundane) — LANDED

Independent of diagnostics; the actual gnulib content work. Added to `crew/moon/include/`:

- **`float.h`** — was absent entirely; now the IEEE-754 characteristics (`FLT_*`/`DBL_*`/
  `LDBL_*`, x87 80-bit long double). Unblocked printf-frexp's *first* blocker (a later
  long-double codegen gap may still surface behind it — clearing one exposes the next).
- **`stdlib.h`** — `EXIT_FAILURE`/`EXIT_SUCCESS`, `reallocarray`, `getprogname`.
- **`unistd.h`** — `getprogname` mirrored here too (gnulib's progname reaches either).
- **`string.h`** — `mempcpy`, `rawmemchr` (GNU extensions gnulib reaches for).
- **`fcntl.h`** — `F_DUPFD_CLOEXEC` (1030) + `F_DUPFD` (0).
- **`sys/stat.h`** — `futimens` (`utimensat` was already there).

Cross-header provision where a header would otherwise fall through to `/usr/include` —
the standing rule from the tar rung. Verified: exitfail / cloexec / ialloc / dup-safer
one-liners compile; a genuinely-missing constant still names itself via rung 1.

## explicitly NOT doing yet

**`#include_next`.** Predicted as the top blocker before the sweep; it never fires.
gnulib's `include_next` lives in `.in.h` *templates* that the Makefile materializes into
real headers only when config.h says a replacement is needed — and this config.h describes
**glibc**, so gnulib stands aside almost entirely.

⚠ deferred, not dodged. The trigger is a config.h that describes **nolibc honestly**: at
that moment gnulib begins generating override headers and `#include_next` becomes
load-bearing. Two paths, and the measurement favors the first:

- keep config.h claiming glibc-like completeness, satisfy the claims in nolibc — gnulib
  stays out of the way (this is the current path, and it's what "config.h corrections are
  configuration, not patches" already amounts to);
- describe nolibc honestly — gnulib does more of the work but demands its full machinery.

**Defining `__GNUC__`.** Clang builds GNU projects by impersonating gcc (`__GNUC__ 4`,
`__GNUC_MINOR__ 2`) and implementing the GNU dialect; gnulib then special-cases
`__clang__` for the holes. That is the eventual path to "build everything" — the generic
non-GNU fallback paths in gnulib are bit-rotted from disuse. But claiming the contract
before `__typeof__` and statement expressions exist would push code *off* the fallbacks
and into holes, making failures worse. Revisit once the dialect surface is there;
`__MOONCC__` alongside it, so packages can special-case us the way they do clang.

## the refusal probe (2026-08-01)

The rungs above were each measured against the gnulib sweep — real C that mooncc *failed*
to compile. This is the other direction: C that mooncc **should** fail to compile, fed in
deliberately. Two instruments, and they found different things.

### the fuzz — `make test_moonfuzz`

`test/gate/moonfuzz.l`, wired into `test_slow`. It takes each of the 111 `test/cc/`
programs and breaks it eight ways from a fixed seed — truncate, delete a byte, delete a
run, flip a byte, insert punctuation, swap two, drop a line, double a line — 888 mutants
in 3.6s. Mutation and not generation on purpose: every mutant is C that was *nearly*
valid, which is the shape a real mistake has, and a generator would have to be taught the
grammar first.

Three properties, and only two of them are red:

- **the front end survives.** No scares (an internal raise reaching the installed `help`),
  no hangs. **888/888.** This is the load-bearing result — a compiler that falls over on
  bad input has no diagnostics to discuss — and it is not the result the probe expected.
- **clay agrees with the parser, on trees the corpus never had.** Every mutant that still
  parses gets `test_clay`'s G1 law: `(cparse (clay-show ast)) == ast`. **192 round-trips,
  0 diffs.** This is the answer to "can the fuzzer reach clay": yes, and the shower held.
  340 of 888 mutants still parsed, 192 of those expressible — the same 56% G1 rate the
  hand-written corpus gets, over shapes nobody wrote.
- **a refusal names its cause** — the CENSUS, not a red:

```
  lex   refused  52 --   0 named,  52 bare
  cpp   refused   4 --   0 named,   4 bare
  parse refused 420 -- 420 named,   0 bare
  gen   refused  72 --  72 named,   0 bare
```

Rungs 1/2/4 are **complete** where they landed: every parse and codegen refusal in 888
tries carried a gripe. Rungs 3-and-lex are **untouched**: every lex and cpp refusal
arrived bare, 56 of 548. That is the whole remaining diagnostics debt, counted. The gate
prints it every run so landing rung 3 shows up as a number falling rather than as a claim.

⚠ the fuzz hands cpp the driver's real include hook rooted at `test/cc/`. The first cut
passed `(\ n s ())` and counted the corpus's own `#include <stdarg.h>` as a cpp refusal in
every mutant of every file that had one — 78 instead of 4. **The census was measuring the
harness.** A fuzz whose baseline is broken reports its own scaffolding as a finding.

### the battery — programs gcc rejects and mooncc does not

**`tools/moon-reject.sh`** — 35 invalid programs across lex/cpp/parse/semantic/link, each
put to `gcc -c -std=c99` as the oracle and to mooncc, verdicts side by side. Skips cleanly
without a gcc, like `moon-sweep.sh` without a package tree; gcc is the oracle and nothing
it produces is used. **24 refused by both, 0 refused by mooncc alone** (it rejects no valid
C here), 11 accepted that gcc refuses. The refusals were good — `cc: f.c:5: parse error near ;`,
`undeclared 'nope' (in main)`, `call to 'f' wants 2 arguments, given 1 (in main)`. What
the battery is for is the other column. **mooncc exits 0 and lays an object for all of
these**, where gcc errors:

| source | gcc | mooncc lays |
|---|---|---|
| `goto nowhere;`, no such label | label used but not defined | an object with an **undefined GLOBAL `main.nowhere`** |
| duplicate `case 1:` | duplicate case value | an object with a **dangling `.k1` relocation** |
| `int x=3; x(1);` | called object is not a function | `mov $3,%eax` … `jmp *%rax` |
| two definitions of `f` | redefinition of 'f' | **both bodies**, the symbol on the second |
| `int x=1; int x=2;` | redefinition of 'x' | accepted |
| `struct s; struct s v;` | storage size isn't known | accepted |
| `int x = v;` (v a struct) | incompatible types | accepted |
| `(struct s)1` | conversion to non-scalar type | accepted |
| `void f(void){ return 1; }` | return with a value | accepted |
| `return 0x;` | invalid suffix on integer constant | accepted |
| `#frobnicate 3` | invalid preprocessing directive | ignored in silence |

Most of these are ordinary missing semantic checks — mooncc does no full C type analysis
and never claimed to, and the C it is *for* (this tree, the LFS ladder) does not contain
them. Two are a different kind, and they share one root:

**an unresolved local label leaves the object as an undefined external symbol.** `goto` to
a label that does not exist and a duplicate `case` value both reach the symbol table as
`main.nowhere` / `.k1`, GLOBAL and UND. Nothing in `-c` notices. What surfaces is a link
failure — and the link's message was the worst diagnostic the probe found. It said:

```
;; link-undef "main.nowhere"                        (from a source tree)
cc: the link owes main.nowhere, and no runtime was found (looked in crew/moon/, ..)
  -- run from a source tree, or install the nest    (from anywhere else)
```

The first is a `;;` love-side debug note, not a `cc:` diagnostic, and it leaks a compiler-
internal label spelling to the user. The second **reports a bug in the program as a bug in
the toolchain installation** — it sends you to check your `~/.love` nest when your source
has a typo'd label. `l_undef.c` (a plain `extern int nowhere(void);` never defined) got
the same treatment, where gcc's `undefined reference to 'nowhere'` is the whole message
anyone needs.

### the link names its undefined references — LANDED

The analysis was already there and the answer was being thrown away. `moon.l`'s `rtpull`
pulls runtime members by need in a worklist; when no remaining member satisfies anything,
`u` — the still-unsatisfied set — is *exactly* the noms nothing anywhere defines. It fell
off the end of the loop unread, and three lines later holo's `ldres` scared about whichever
one the patcher reached first. Now the loop reads it:

```
cc: undefined reference to 'main.nowhere'
cc: undefined reference to 'a', 'b', 'c' (+1 more)
```

Every owed nom at once, not whichever came first — the worklist knows the whole set, and a
link that owes four names should say four. Three lanes, one sentence:

- **the runtime resolved everything but these** — the worklist tail above.
- **`-nostdlib` and friends** — no runtime *by request*, so an owed nom is undefined
  outright. `rtpull` now takes the flag instead of the caller skipping the call, because
  the analysis is wanted there and only the pull is not. This is the lane where a bad name
  is most likely: freestanding code has no libc it could have meant.
- **no runtime in reach at all** — states both facts in the order they can be acted on:
  `cc: undefined reference to 'main.nowhere'; no runtime in reach to supply it either
  (looked in crew/moon/, ..) -- check the spelling, then the nest`. The undefined reference
  is the half that is certainly true. The old message led with the absent toolchain and
  prescribed installing the nest, which is the wrong repair for a misspelled label, and
  from inside the driver the two conditions are indistinguishable.

⚠ **`test_drv` pinned the exit code, not the sentence** — which is how `;; link-undef`
stood as long as it did. It now pins the message, so the next person to touch this path
finds out from a gate rather than from a user.

**Ranked, what is left.** (1) Rung 3 + the lex position, which the census counts — 56 bare
refusals, and `cpp.l`'s two `say err`-then-bare-bad sites are half-converted already.
(2) A local label that never resolves should refuse in `gen` and name the label, not escape
into the symbol table to be reported at link under its internal spelling; that is a real
check, not a message, and it is what gcc's `label 'nowhere' used but not defined` is. The
rest of the table is semantic analysis mooncc has never had, and wanting it is a separate
decision from wanting diagnostics.

## gates

`make test_moon` + `make test_raw` (the standing pair), and `make test_moonfuzz` for the
refusal surface. The diagnostics pass touches message construction, not codegen, so
`test_raw`'s byte-identical expectations are the guard that it stayed that way.

Re-run `tools/moon-sweep.sh` after each rung. The bucket table at the top is the
before-picture — and note the census keys are themselves the indictment:

```
  10 ;; cgfn refuses <fn>   (undeclared identifier, unnamed)
   4 cc: parse error   (no cause reported)
   2 cc: cannot resolve #include <hdr>
   1 cc: codegen error   (no cause reported)
```

The deliverable is those parentheticals disappearing — the failure list becoming
*self-explanatory* — more than the compile count going up. Rung 1 retired the first line;
rung 2 the second; rung 4 the fourth, and with it the census key itself (`cc: codegen
error` no longer exists as a message — a refusal names its cause or names the function it
gave up in, and either way says so once, on stderr). Only `#error` (rung 3) still throws
its text away.

⚠ the four parse failures have NOT been re-run against a real tree — there was no
configured gzip source here, so rung 2 was verified on synthetic cases and against gcc's
own file:line, and rung 4 on the shapes `test/cc/` already pins. Re-running the sweep is
the first thing to do with one.
