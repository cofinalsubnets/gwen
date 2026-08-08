# moon-diag — how mooncc names its causes

Every stage of the compiler discovers a precise cause — a missing identifier, an unparseable
token, an `#error` and its text. The problem a diagnostic pass solves is that a stage can
*compute* the cause and then throw it away, answering a bare bad across the stage boundary where
the driver can only print a generic line naming the file. **Two half-messages, neither
sufficient**, is the shape of every bad diagnostic here.

## the carrier

`gripe` (lex.l) is the one value a stage may answer instead of a bare bad:

```
(gripe file line col msg)   ; any field may be () when unknown
```

`gripe?` guards it, `okv?` is "a good value, not a gripe", and `moon.l`'s `ccdie` formats
`cc: <file>:<line>:<col>: <msg>` — an unknown file falls back to the source path, an unknown
line/col elides. Given a bare bad it prints the old generic `cc: <stage> error in <src>`, which
is what lets the stages convert one at a time instead of in a flag day.

⚠ **A gripe naming a FILE came from an `#include`,** which lexed fresh from its own text, so its
line is relative to that file. `deskew` corrects for the driver's own prepended `-D` lines —
every path that mints a gripe has to come through it, or it reports skewed lines.

⚠ **A gripe IS a chain**, so `cgen`/`cgen-obj` must guard `(gripe? r) r` *before* their `two?`
check, and ride it out to the driver.

## what names its cause

**Undeclared identifiers.** mooncc refuses undeclared calls by design (no C89 implicit `int`),
so *every* missing libc prototype in the userland ladder surfaces through one path. The `cgexpr`
`var` refusal — the one path both an undeclared value-use and an undeclared call funnel through,
since `call-fixed` evaluates its head — pins `(g 'undecl <name>)` before its `'bad`; `gfns`
clears that pin per function and, on a refusal, mints the gripe:

```
cc: dup-safer.c: undeclared 'F_DUPFD_CLOEXEC' (in dup_safer_flag)
```

(The address-of-an-undeclared-name path, `clval`, still answers a bare bad.)

**Positions in parse errors.** A bare position is most of the value: it converts "bisect a
flattened TU with `gcc -E -P` over brace-balanced prefixes" into reading a number.

**A deliberate refusal names the feature.** This is a **distinction, not a message**: `'bad` in
gen.l is two different answers wearing one face — "I decline, take another lane" (`cginl`'s fesc
decline, a policy deopt: an ordinary recoverable step) and "there is no lane for this, anywhere"
(a target gate, an ABI class never built). Only the second is a diagnosis, which is why a blanket
debug print of every `'bad` can never be left in — it spams every clean compile.

`nolane` pins its phrase and answers the same bare bad as before; `gfns` and `cgdata` read the
pin ONLY where the function refuses outright, so a recoverable bad that pinned on its way past
costs nothing. **Last pin wins** — the deepest site reached before the unwind is the one that
knows.

```
cc: vla.c: no lane for a variable-length array on riscv64 (in sum)
cc: b1.c: no lane for passing this 40-byte struct by value on arm64 (in take)
cc: c2.c: no lane for C99 complex arithmetic on arm64 (in fmul)
```

Four sites are converted, chosen because they fire: the VLA target gate, the
complex-arithmetic gate, and the composite-by-value param and return catch-alls. The
`refuse LOUD` sites on the thumb lanes are the same class and take one `nolane` each.

**Every codegen refusal is a gripe**, including the ones with no cause to name
(`cannot compile 'f' (cause unnamed — see doc/moon-diag.md)`), and the driver puts it on stderr —
one message on one channel. It used to be two halves on two streams (the enclosing function to
stdout, `codegen error in <file>` to stderr), so a build log capturing either kept half the
sentence. The static-data lane joined the same door: its cause rides a pin (`globs` sets it) and
`gfns`'s tail mints the gripe, because `cgdata`'s callers thread a bare `'bad` through a dozen
`(bad? x)` joins and a gripe is not one.

⚠ **`(! (cgn …))` is no longer how a law says "refused".** `law.l` has `refused?`
(`(|| (! x) (gripe? x))`); the laws that mean "refused, and here is exactly what it said" pin
`gripe?` and the message string. A message golden earns its churn there — the string IS the
deliverable.

**The link names its undefined references.** The analysis was always there: `moon.l`'s `rtpull`
pulls runtime members by need in a worklist, and when no remaining member satisfies anything, the
still-unsatisfied set is *exactly* the noms nothing anywhere defines. The loop reads it now.

```
cc: undefined reference to 'main.nowhere'
cc: undefined reference to 'a', 'b', 'c' (+1 more)
```

Every owed nom at once, not whichever the patcher reached first. Three lanes, one sentence:

- **the runtime resolved everything but these** — the worklist tail;
- **`-nostdlib` and friends** — no runtime *by request*, so an owed nom is undefined outright.
  `rtpull` takes the flag rather than the caller skipping the call, because the analysis is
  wanted there and only the pull is not. This is the lane where a bad name is most likely:
  freestanding code has no libc it could have meant;
- **no runtime in reach at all** — state both facts in the order they can be acted on:
  `cc: undefined reference to 'main.nowhere'; no runtime in reach to supply it either (looked in
  crew/moon/, ..) -- check the spelling, then the nest`. The undefined reference is the half that
  is certainly true. ⚠ Leading with the absent toolchain **reports a bug in the program as a bug
  in the toolchain installation** — it sends you to check your `~/.love` nest when your source
  has a typo'd label, and from inside the driver the two conditions are indistinguishable.

## what still arrives bare

**lex.** cpp went over 2026-08-08: the `'cppbad` sentinel is gone and every refusal it can
raise is a located gripe, so the driver names one file and one line rather than appending a
second, vaguer sentence to the first.

`#error` is the one that paid for the trip — it is a *configuration* signal, and the bare form
cost a whole afternoon. PDCLib's config says which construct it could not match, and
`cc: #error directive` threw that away, refusing 232 of 233 files with nothing to read:

```
cc: pdclib/include/_PDCLIB_config.h:248: #error: Please create your own _PDCLIB_config . h . ( Unsupported * INTn_C macros . )
```

⚠ **A `#error` inside a header is the common case, and the file it names is the header's** —
`doinc` stamps it on the way out, which is also what tells `deskew` to leave the line alone (a
header lexed from its own line 1 and never saw the `-D` preamble). A TU-level gripe carries no
file, gets the source path filled in, and gets the preamble subtracted. Getting that backwards
is how `#error` first shipped reporting line 6 of a one-line file.

Still owed: the **spelling**, which is pp-tokens joined by spaces (`a - b` for `a-b`) because
that is all `spellcat` can promise once the original text is gone. And **`#warning` has no
channel** — the gripe carrier is fatal by construction, so a warning says its text unlocated.
A non-fatal located channel is its own small rung.

Also open, and a real check rather than a message: **a local label that never resolves should
refuse in `gen` and name the label**, not escape into the symbol table as a GLOBAL UND under its
internal spelling (`main.nowhere`) to be reported at link. That is what gcc's
`label 'nowhere' used but not defined` is.

⚠ **A parse-error LINE can be far from the fault, and it points BACKWARD.** darkhttpd.c reported
`darkhttpd.c:786: parse error near ;` for a construct at 806-822 (an undeclared `u_char` in a
cast), and 786 is a blank line. Truncating the file to 804 lines parsed clean, which is how the
lie was caught. The number is not random — it is behind by roughly the lines the conditional
skipper consumed — so **when a reported line looks innocent, bisect by truncation before reading
it**. The same run showed the companion habit: `parse error near <filename>` prints the file
where the token should be. Both make a real gap read as a phantom, and both are cheap to
mistrust once you know.

## the instruments

**`tools/moon-sweep.sh PKGSRC=<a ./configure'd tree>`** — compiles every `lib/*.c` of a
package's gnulib layer, scores against `lib/Makefile`'s `*_a_OBJECTS`, prints the per-file
causes and a bucket census, and reports other-platform lanes separately. Skips cleanly without a
tree. Logs land in `out/host/moonsweep/logs/`.

⚠ **THE DENOMINATOR IS THE POINT.** A raw `for f in lib/*.c` count is meaningless — gnulib's
`lib/` carries every platform's lane, and automake builds a third of it on Linux. The script
exists mostly to stop that mistake being made twice.

⚠ These are FIRST blockers — clearing one may expose another behind it — and a small package
pulls an order of magnitude fewer gnulib objects than a large one, so any such number is a lower
bound on the general distance, not a measure of it.

**`make test_moonfuzz`** (`test/gate/moonfuzz.l`, in `test_slow`) takes each `test/cc/` program
and breaks it eight ways from a fixed seed — truncate, delete a byte, delete a run, flip a byte,
insert punctuation, swap two, drop a line, double a line. Mutation and not generation on purpose:
every mutant is C that was *nearly* valid, which is the shape a real mistake has, and a generator
would have to be taught the grammar first. It checks three properties:

- **the front end survives** — no scares (an internal raise reaching the installed `help`), no
  hangs. This is the load-bearing one: a compiler that falls over on bad input has no
  diagnostics to discuss.
- **clay agrees with the parser** on trees the corpus never had: every mutant that still parses
  gets `test_clay`'s G1 law, `(cparse (clay-show ast)) == ast`.
- **a refusal names its cause** — printed as a per-stage census (refused / named / bare), so
  landing the cpp and lex rungs shows up as a number falling rather than as a claim. It did:
  cpp went 0 named / 8 bare → **8 named / 0 bare** the day it converted, and lex's 55 bare are
  now the whole remainder.

⚠ The fuzz hands cpp **the driver's real include hook**. Passing `(\ n s ())` counts the corpus's
own `#include <stdarg.h>` as a cpp refusal in every mutant of every file that has one — a fuzz
whose baseline is broken reports its own scaffolding as a finding.

**`tools/moon-reject.sh`** — invalid programs across lex/cpp/parse/semantic/link, each put to
`gcc -c -std=c99` as the oracle and to mooncc, verdicts side by side. Skips cleanly without a
gcc; gcc is the oracle and nothing it produces is used. mooncc rejects no valid C here; the
column that matters is the other one — programs gcc refuses and mooncc lays an object for:

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

Most of these are ordinary missing semantic checks — mooncc does no full C type analysis and
never claimed to, and the C it is *for* does not contain them. Wanting them is a separate
decision from wanting diagnostics. The first two are the different kind, sharing one root (the
unresolved-label item above).

## explicitly not doing yet

**`#include_next`.** gnulib's `include_next` lives in `.in.h` *templates* that the Makefile
materializes into real headers only when config.h says a replacement is needed — and a config.h
describing **glibc** makes gnulib stand aside almost entirely, so it never fires.

⚠ Deferred, not dodged. The trigger is a config.h that describes **nolibc honestly**: at that
moment gnulib begins generating override headers and `#include_next` becomes load-bearing. Two
paths, and the measurement favors the first: keep config.h claiming glibc-like completeness and
satisfy the claims in nolibc (gnulib stays out of the way), or describe nolibc honestly (gnulib
does more of the work but demands its full machinery).

**Defining `__GNUC__`.** Clang builds GNU projects by impersonating gcc (`__GNUC__ 4`,
`__GNUC_MINOR__ 2`) and implementing the GNU dialect; gnulib then special-cases `__clang__` for
the holes. That is the eventual path to "build everything" — the generic non-GNU fallback paths
in gnulib are bit-rotted from disuse. But claiming the contract before statement expressions and
the rest of the dialect exist would push code *off* the fallbacks and into holes, making failures
worse. Revisit once the dialect surface is there, with `__MOONCC__` alongside so packages can
special-case us the way they do clang.

## lessons worth keeping

⚠ **A law harness that runs a stage in isolation is worth having.** `gen.l` is cat'd without
holo for the laws, and an unbound name there is not an error but a SILENT no-op: `(at ty 3)`
reads `((0 ty) 3)` = `3`, so every VLA site in a TU shared one slot index and the whole lane
refused. It only ever worked because the real driver splices holo for the linker. **The ambient
dependency is invisible where the ambient is always there.**

⚠ **Pin the sentence, not the exit code.** `test_drv` pinned the exit code, which is how a `;;`
love-side debug note stood in for a `cc:` diagnostic as long as it did. It pins the message now,
so the next person to touch that path finds out from a gate rather than from a user.

**The deliverable is the parentheticals disappearing** — a failure list becoming
self-explanatory — more than a compile count going up.

## gates

`make test_moon` + `make test_raw` (the standing pair), `make test_moonfuzz` for the refusal
surface, `make test_drv` for the driver's messages. Diagnostics work touches message
construction, not codegen, so `test_raw`'s byte-identical expectations are the guard that it
stayed that way. Re-run `tools/moon-sweep.sh` after each rung.
