# moon-userland — growing mooncc by building the LFS packages

The idea: build the conventional GNU/Linux userland (the Linux-From-Scratch package
set — bzip2, gzip, sed, grep, make, bash, coreutils, …) **with mooncc**, in LFS build
order, as a **conventional overlay over the love base system**. Two payoffs at once:

- each real package drives mooncc through real-world C it has never seen, growing the
  compiler the same way love.c grew it (doc/moon.md's "gen choke list" — refuse a
  construct, add it, move on), and
- the result is a familiar POSIX userland whose *entire toolchain underneath* is
  love/mooncc/holo — the "conventional overlay, love floor" the arc is aiming at.

This is the RIGHT ladder. The kernel (doc/moon.md:16, "the one imported artifact") is a
cliff — inline asm + a register allocator to bind its operands, `__attribute__` honesty,
bitfields, computed goto (see the gap table at the bottom). The LFS *userland* packages
are ordinary C: the first one is a day of header-shim + declaration work away, not a
compiler rewrite. So climb the userland ladder first; the kernel is its far end.

The base is [[love-distro]] rung 4 — the gcc-free, glibc-free static `love` (out/host/love-raw,
`make test_raw`) — booting as pid 1 on the Linux kernel via `init/boot.l` + `mk/distro.mk`
(`make distro-run`). The overlay is these packages, laid beside kore under /usr.

## first rung, MEASURED: bzip2 1.0.8

bzip2 is the ideal first package — it's *in* the LFS book, ~7.3k lines of plain C89, and
has **no `./configure`** (a plain Makefile), so it isolates mooncc's C coverage from the
shell/autotools bootstrap. Probed 2026-07-15 (`mooncc -c` per file):

```
  ALL 8 FILES COMPILE (rungs A + B + C + D landed): crctable randtable huffman
  blocksort compress decompress bzlib bzip2  ->  a real, working `bzip2` binary
```

**A complete, working `bzip2` built entirely by mooncc.** Compress/decompress round-trips
byte-perfect in BOTH file mode and `-c` stdout mode, the compressed bytes are **byte-identical
to a gcc -O0 build**, and it interoperates BOTH ways with the system tool (system `bunzip2`
decodes mooncc's output; mooncc decodes system `bzip2`'s). Linked with `gcc -no-pie` (the love.c
ladder's convention). So mooncc's codegen for the whole program — incl. decompress.c's
nested-case coroutine (rung A) — is verified against gcc AND the real bzip2 format.

**5 of 8 library files compile** with the landed header shims. The rungs:

1. **missing `<ctype.h>`** — LANDED (`crew/moon/include/ctype.h`). mooncc ships headers
   "sized to love.c/host" (doc/moon.md); ctype was not one, and love.c never needs it. bzip2's
   `#include <ctype.h>` then **fell through to glibc's `/usr/include/ctype.h`**
   (GNU-extension-laden) and mooncc choked there. The 15-decl shim took crctable.c from
   refuse → a real 1947-byte `.o`.
   *Methodology note (still open):* the silent fall-through to /usr/include is itself a gap
   — an LFS build must run mooncc with a complete sysroot (mooncc has no `-nostdinc` yet),
   OR mooncc should REFUSE a header it doesn't own rather than defer to glibc's.

2. **incomplete stdio decls** — LANDED (`ungetc`/`getc`/`getchar` added to
   `crew/moon/include/stdio.h`). Proven: `bzip2.c` advanced past `myfeof` (which calls
   `ungetc`) to the next function, `compressStream` — the shim ladder moves forward one
   decl at a time, as designed.

The remaining rungs:

- **rung A — nested `case` labels — LANDED 2026-07-15** (`crew/moon/{parse,gen}.l`).
  mooncc's parser used to accept `case`/`default` only as a **direct child of the switch's
  compound block**; a `case` at deeper nesting — inside an inner `{}` or a loop (bzip2's
  `GET_BITS` coroutine trick, decompress.c:43-44) — parse-errored. The fix, three parts:
  (1) parse `case`/`default` as label-statements at ANY depth (pstmt), (2) gen keys each
  switch's case-labels by VALUE in a per-switch tablet on a stack `g 'cases` (pushed/popped
  around the body, so a case ties to its NEAREST enclosing switch), (3) a recursive pre-scan
  `scasel` walks the body — through blocks/if/loops, STOPPING at a nested switch — to build
  the dispatch, while `cgstmt` emits each case/default label wherever it sits (`cgitems`
  handles the body, so nested emission is free). Verified: Duff's device + a nested-case /
  fallthrough / nested-switch battery are byte-identical to `gcc -O0`; `make test_moon` +
  `make test_raw` (3453 tests, gcc-free love) stay green. This un-parse-errors decompress.c
  (which now advances to a codegen rung, below). "Folded case labels" in mooncc's law tail
  is a different, simpler thing (`case 1: case 2:` fallthrough).

- **rung B — `NULL` undefined — LANDED 2026-07-15** (`crew/moon/include/{stdio,stdlib,
  string}.h`). Once rung A let BZ2_decompress parse, it refused at codegen on `(var "NULL")`
  — `s->save_gLimit = NULL`. Not a codegen gap at all: `NULL` was an *undeclared identifier*
  because mooncc defined it only in `<stddef.h>`, while glibc provides `NULL` from
  stdio/stdlib/string too (and bzip2 pulls it via `<stdio.h>`). Fix: a guarded
  `#ifndef NULL / #define NULL ((void*)0)` in those three headers. This took decompress.c
  fully across (a 49 959-byte `.o`) — another header-completeness rung, same family as the
  ctype/ungetc shims, NOT a compiler feature. (Method: a temporary recursive
  cgstmt/cgexpr wrapper that prints the form head on `bad` pinpointed `(var "NULL")` as the
  innermost failure — a reusable gen localizer.)

- **rung C — bzlib.c's undeclared stdio calls — LANDED 2026-07-15** (`crew/moon/include/
  stdio.h`). The localizer pinpointed `(var "ferror")` — `ferror(f)` — as bzlib.c's blocker:
  another missing stdio declaration (glibc has it; mooncc didn't). mooncc's "refuse an
  undeclared call rather than guess the ABI" strictness is arguably *correct*, so the fix is
  headers, not the compiler: added `ferror`/`feof`/`clearerr`/`fdopen`/`rewind` to stdio.h.
  bzlib.c compiled → the whole library builds and round-trips byte-perfect (above). NOTE the
  theme across A/B/C: only rung A was a real compiler feature; B and C were both
  header-completeness. The durable lesson — building third-party C mostly grows a COMPLETE,
  self-contained `crew/moon/include` (run under a sysroot / `-nostdinc`), not the compiler.

- **the bzip2.c CLI tail — LANDED 2026-07-15** (headers only): new `utime.h` + `sys/times.h`;
  `sys/stat.h` gained the permission bits (`S_IRUSR`…) and the `st_atime`/`st_mtime` glibc
  compat macro aliases (`#define st_atime st_atim.tv_sec`); `fchown`/`ferror`/`feof`/`rewind`
  added to unistd/stdio. All 8 files compile after this.

- **rung D — external DATA symbols mis-relocated (a REAL codegen bug, LANDED 2026-07-15)**
  (`crew/holo/obj.l`). Once bzip2.c compiled, the CLI's `-c` (write-to-stdout) mode SEGFAULTED.
  The localizer + `objdump` showed the cause: mooncc emitted `lea stdout(%rip)` with an
  **`R_X86_64_PLT32`** relocation — a *function-call* reloc — for `stdout`, which is external
  DATA. The linker routed it through the PLT (a code stub), so the deref read garbage. love.c
  never takes the address of an external data symbol (it uses raw `write(1,…)`), so this hid.
  Root: obj.l emitted PLT32 for ALL external refs, not distinguishing a `call`/`jmp` (control
  transfer, PLT32) from an `la`/`lea` (address-of, needs PC32). Fix: carry the byte preceding
  the reloc field (the opcode) into the reloc record; an `E8`/`E9` (call/jmp) → PLT32, anything
  else (a lea's modrm) → PC32. Contained in obj.l — our own linker is unaffected (link.l treats
  PC32≡PLT32), the baked glaze backend (x64.l) is untouched. Verified: `stdout`→PC32,
  `fputs`→PLT32; the full `-c` round-trip works; `test_moon` + `test_raw` (3453) + `test_holo`
  (219) stay green. This was the FIRST real compiler/linker bug the userland ladder surfaced
  beyond a header — every C program using `stdin`/`stdout`/`stderr` (or any `extern` data
  global) needed it.

### the takeaway

The first real LFS package is **~a header-shim day** from compiling, not a rewrite. That
validates the ladder: build a `crew/moon/include` that is self-contained + complete enough
for third-party code (with `-nostdinc`), then walk the LFS order — each package adds a few
header decls and, occasionally, one real parse/gen rung, exactly the love.c grind.

## second rung, MEASURED: gzip 1.13

gzip-1.13 is a **different animal from bzip2** — it is a gnulib+autotools package, not
plain C89. The 12 core compression files (bits/trees/deflate/inflate/zip/unzip/util/...)
are ordinary C; the 118 `lib/*.c` are gnulib portability glue (the "hard far end"). We ran
`mooncc -c` per core file against a gcc-`./configure`-generated `config.h` (the shell/autotools
bootstrap we lean on first). Probed 2026-07-15:

```
  ALL core files compile: gzip bits deflate inflate trees unlzh unlzw unpack unzip  ->  .o each
  (gzip.c, the CLI, was the last: cleared via the cpp fix + header sweep + abstract-array param, below)
```

Getting there surfaced **five real mooncc capability rungs** (all general, all gated green on
`make test_moon` + `make test_raw`), plus header-completeness:

- **rung E — octal/hex string+char escapes — LANDED** (`crew/moon/lex.l`). `escv` handled only
  named escapes + `\0`; a `\ooo` with a nonzero lead digit (`\120`) **refused**, and `\0NN`
  **silently mis-parsed** (`"\037"` → NUL + literal `'3''7'`, a latent miscompile — confirmed
  vs gcc). bzip2's magic was ASCII (`"BZh"`) so it never hit this; gzip's binary magic
  (`"\037\213"`, PKZIP `"\120\113\003\004"`) hits it head-on. Fix: a real `escseq` parsing
  `\ooo` (1–3 octal digits) and `\xHH` (hex), returning `(value nextpos)` so the variable
  length threads through both the string and char lexers. Byte-identical to gcc on octal, hex,
  and named escapes.

- **rung F — `bool`/`true`/`false` builtin — LANDED** (`crew/moon/cpp.l`, `include/stdbool.h`).
  Modern C (gnulib, gzip) uses `bool` **bare, without `<stdbool.h>`** (C23 makes them keywords).
  cc required the include. Fix: predefine them in cpp (an implicit stdbool) — `bool` → cc's
  `int` (the old stdbool.h choice, zero layout churn), `true`/`false` → 1/0; stdbool.h neutered
  to a no-op so an explicit include stays harmless. This alone unblocked inflate.c, unzip.c,
  and gnulib's dirname.h.

- **header completeness** (`include/{string,stdlib,stdio}.h`): `strspn`/`strcspn`/`strpbrk`/
  `strtok`/`strncat`, `strtoul`, `putc`. Same family as bzip2's ctype/ferror rungs — cc refuses
  an undeclared call rather than guess the ABI (util.c's `add_envopt`/`fprint_off` refused on
  `strspn`/`putc`). The durable lesson holds: growing a complete `crew/moon/include` is most of
  the work.

- **rung G — `__STDC_VERSION__` predefined (C11) — LANDED** (`crew/moon/cpp.l`). cc has
  `_Static_assert`; advertising `201112` makes gnulib's `verify.h` take its `_Static_assert`
  lane instead of a fallback struct-array trick. Unblocked deflate.c.

- **rung H — enum constant const-expression initializers — LANDED** (`crew/moon/parse.l`). The
  enum parser accepted only `= <literal>` (a single numeric token); `enum { A=5, B=A }` or
  `B = A+1` (an enumerator referencing a prior constant, or any const-expr) **refused** —
  timespec.h's `enum { TIMESPEC_RESOLUTION = TIMESPEC_HZ }`. Fix: parse the initializer with
  `pasn` (assignment-expr, stops at the `,`) and `cfold` it, exactly like array dims and case
  labels already do — enum constants resolve through `ps 'enums`. Values byte-match gcc.

### rung I — 16-byte struct-by-value parameters (SysV/AAPCS64) — LANDED 2026-07-15

`crew/moon/gen.l`. timespec.h's inline helpers take `struct timespec` (tv_sec+tv_nsec = 16 bytes)
**by value**. cc passed a ≤8-byte struct in one register but **refused** anything bigger — it
didn't wire the two-eightbyte register classification (only `sse:sse` and one-eightbyte lanes
existed; the classifier `aclass` already computed both eightbytes, but arg/param lanes refused
the rest). Now a 9..16-byte struct rides both the **param** (`spill`) and **call** (`kls`) lanes
by its eightbyte classes — each eightbyte into a gp (`int`) or xmm (`sse`) register, all four
combos (int:int, int:sse, sse:int, sse:sse). Arch-correct: x64 splits per eightbyte-class; arm64
(AAPCS64) sends a non-HFA composite wholly in X regs, only an all-float HFA in V regs. Verified
**byte-identical to gcc on both x86-64 and aarch64** (run under qemu) — call, return, and structs
interleaved with scalar args. love.c never passed a >8-byte struct by value, so it had hidden. A
register-exhausted arg or a >16-byte (MEMORY) struct still refuses (`crew/moon/law.l` asserts both
directions). This is a general rung (every modern struct-by-value API needs it); it un-refuses
timespec.h's helpers, though gzip.c itself hit a *separate* blocker — a cpp include bug, now
FIXED (below).

### gzip.c's "parse error" was a CPP INCLUDE-ACCUMULATION BUG (found + FIXED 2026-07-15)

The misleading "parse error at `typedef ptrdiff_t idx_t;`" was **a real, deterministic bug in
the preprocessor's `#include` machinery** (`crew/moon/cpp.l`, `doinc`) — NOT a GC heisenbug and
NOT a missing parser feature. The whole flakiness was an earlier, separate confound; under
`LOVE_NO_GLAZE=1 LOVE_NO_IMAGE=1` gzip.c fails 12/12 deterministically. The forensics that nailed it:

- The parse dies at `typedef ptrdiff_t idx_t;` (gnulib `idx.h`) because `ptrdiff_t` is **not
  registered** there (`types[ptrdiff_t]` = MISS) — even though gzip.c:61 `#include <stddef.h>`
  should have registered it (`typedef long ptrdiff_t;`).
- Instrumenting cpp's output-token order showed idx.h's `typedef ptrdiff_t idx_t` landing at
  **output position 62** — near the very front, *before* stddef ever defines ptrdiff_t. So the
  parser hits the USE before the DEFINITION → "not a type."
- The reorder traces to the include accumulator (`out`) going **non-monotonic**: it grew to 2314
  tokens, then `stat-time.h` returned `done=0`, wiping it to empty; everything after (version.h,
  xalloc.h→idx.h) was then re-accumulated onto an empty base and floated to the front.
- Root cause: `stat-time.h` includes **`stdckdint.h`** (C23), which mooncc doesn't provide. When
  a `#include` fails to resolve, `doinc` returned **`()`**. The ancestor `doinc` only special-cased
  the `'unbalanced` sentinel, so it treated `()` as a *valid empty include* and continued with
  `(rev ()) = ()` — **silently dropping its accumulated `out` and masking the failed include**.
  A missing header thus became either a bizarre downstream parse error or (worse) a silent
  token-reorder miscompile.

**The fix** (`crew/moon/cpp.l`): a single fatal-cpp sentinel `'cppbad` that **propagates** up
through every `doinc` level (unresolved include, lex failure, and nested-`'cppbad`), instead of
degrading to `()`. Now a missing header refuses cleanly and *names itself*:
`cc: cannot resolve #include <stdckdint.h>` → `cc: preprocessor error in gzip.c` — mooncc's
"refuse rather than miscompile" law, made honest. Gated green on `make test_moon` + `make test_raw`.

**Method note that mattered:** always probe under `LOVE_NO_GLAZE=1 LOVE_NO_IMAGE=1` for a
deterministic signal, and when a parser blames a symbol that "is" defined, suspect the token
STREAM (cpp order), not the parser state. Dumping cpp's output positions + the include
accumulator size per `#include` is what exposed the reorder.

### gzip.c COMPILES — all core `.c` files now `.o` (2026-07-15)

With the cpp fix in, gzip.c cleared its remaining blockers in a header-completeness sweep plus
one small parser rung. Every gzip-1.13 core `.c` (gzip bits deflate inflate trees unlzh unlzw
unpack unzip) now compiles to a valid x86-64 ELF `.o` under mooncc. The rungs:

- **`stdckdint.h` shim** (new) — C23 checked arithmetic; `ckd_{add,sub,mul}` over the
  `__builtin_*_overflow` gen.l already lowers.
- **abstract array-of-struct params** (`parse.l`) — `int fdutimens(int, char const*, struct
  timespec const[2])`: the unnamed-param path only accepted a bare abstract declarator (then
  `)`/`,`); it now takes an abstract `[n]` array suffix via `adims`, decaying to a pointer like
  the named case. Verified byte-equal to gcc (exit 55) on a struct-array-param repro.
- **header completeness**: `fdatasync`/`unlinkat` (unistd), `openat`/`O_SEARCH`(=O_RDONLY)/
  `O_BINARY`/`O_TEXT`(=0)/`O_NOFOLLOW`(arch-gated) (fcntl), `ELOOP` (errno), `struct tm` +
  `localtime`/`gmtime`/`mktime` (time, glibc-accurate layout for the hosted link), `S_IRWXUGO`/
  `S_IXUGO` (sys/stat), `fdopendir`/`dirfd` (dirent), `sigismember` (signal).

Method note: mooncc **refuses an undeclared function call** (no C89 implicit `int f()`), so each
missing libc/POSIX prototype surfaced as a `cgfn refuses` / `(var "name")` codegen refusal — a
clean, one-at-a-time header-completeness signal (instrument `cgitems`/`cgexpr` to print the bad
node; the deepest prints first). Gated green: `make test_moon` + `make test_raw` (3453).

### the other wall: the gnulib `lib/*.c` link tree — NOT DONE

Even with every core `.o`, LINKING a working gzip needs gnulib's implementations —
`xmalloc`/`xstrdup` (xalloc), `dir_name` (dirname), `getopt_long`, `savedir`, `yesno`,
`fcntl`-safer, quotearg, … — ~100 `lib/*.c`, each its own potential rung. This is the
"gnulib-heavy" far end the ladder always flagged; gzip-1.13 reaches it where bzip2 (plain
C89, no gnulib) never did. **A pre-gnulib gzip (1.2.4, plain C89) sidesteps this whole tree**
(and the C23/gnulib header cascade like `stdckdint.h`) — the practical path to a runnable gzip.

**Takeaway:** a *working gzip-1.13 binary* is a materially larger arc than bzip2 — it is a
gnulib userland port plus one real ABI codegen feature, not a header-shim day. The five rungs
above are pure profit regardless (general modern-C coverage). A faster path to a *runnable* gzip
is a pre-gnulib release (gzip-1.2.4, plain K&R/C89) which sidesteps both walls — the same shape
as bzip2.

## gzip-1.2.4 — a COMPLETE runnable binary (2026-07-16)

The pre-gnulib path paid off: **gzip-1.2.4 builds to a working, format-accurate binary** —
all 14 `.c` + `nolibc.c` + `sys.o` linked by our own holo linker, no gcc/glibc/ld. Verified
round-trip (empty/1-byte/binary/text/random), `-t` integrity, `-l`/`-v` output, and — the real
proof — **our `.gz` decodes byte-identical under the system `gunzip`, both directions**.

Compile blockers (each a durable mooncc fix; `test_moon` + `test_raw` green):
- **do_list refused** — `ctime` had no prototype → added `ctime`/`asctime` to `include/time.h`.
  gzip.c calls `ctime` *undeclared* (fine on 1993's 32-bit boxes; implicit-int truncates the
  returned pointer on x86-64), so the one app-side edit is `#include <time.h>` in gzip.c — a real
  64-bit portability fix, not a mooncc gap.
- **check_ofname refused** — `fgets` missing → `include/stdio.h`.
- **crypt.c "preprocessor error"** — an empty-after-cpp TU was rejected as failure. `cpp.l`: only
  `'cppbad` fails now (an empty token list is a valid empty TU); `#error` + unbalanced conditionals
  now raise `'cppbad` (were bare `()`). `parse.l`: an empty token list parses to `(prog ())`.
- **getopt.c "lex error"** — form-feed `\f` (GNU page-break, 5 of them) wasn't whitespace →
  `lex.l` `ws?` now includes `\f` and `\v`.

Link + run blockers:
- **⚠ octal integer literals** (`0644`/`0777`) lexed as **decimal** → gzip chmod'd its output to
  garbage modes (`----r-----`), so decompress hit `EACCES`. `lex.l` gained an octal branch
  (`0`+octal-digit → base 8; `0`/`0x`/`0.5`/`08` still route correctly). The last and subtlest
  bug — the compress/decompress *engine* was already byte-perfect via `-c` stdout mode; only the
  file-mode copy exposed it. Debug trap: a probe `printf("%o",…)` mis-read `struct stat` as
  garbage because nolibc's `__fmt` didn't grok `%o` and skipped the vararg — the struct was fine.
- **nolibc grew 16 libc functions** (it had `fprintf`/`malloc`/`memset`/`strlen`/`strcmp`): the
  `str*` family (`strcpy strcat strncmp strncpy strrchr strspn strcspn`), `atoi calloc isupper`,
  `printf perror putc fgets`, the calendar (`ctime`/`gmtime`/`localtime`/`asctime` — `localtime`
  **is** `gmtime`, UTC with no tz database; Hinnant's exact civil-from-days), and `utime` (on
  `utimensat`). Plus `__fmt` gained **field width / precision / `0`-pad / `-` / `%o`** — it had
  ignored width, so `%9ld`/`%08lx`/`%5s` printed literally and `-l`/`-v` columns were garbled.

Build: lay `sys.o` via `crew/moon/lib/mksys.l`, then one `mooncc -DSTDC_HEADERS -DHAVE_UNISTD_H
-DDIRENT -I crew/moon/include -I <src> -o gzip <src>/*.c crew/moon/lib/nolibc.c sys.o`.

## tar 1.13 — the header set (15/17 src compile, 2026-07-16)

tar is the third rung, chosen for its breadth (terminal/tape ioctls, locale, gettext,
`uintmax_t`). The point of this pass was **header completeness**: mooncc's `#include <>`
resolver falls through to `/usr/include` when it lacks a header, so a half-populated
`crew/moon/include/` silently pulls glibc's headers (with `__extension__`/`typeof`/statement-
exprs mooncc can't parse). The gzip model — give mooncc its **own** minimal versions — is the
fix. Landed:

- **9 new headers**: `limits.h` (+`PATH_MAX`), `inttypes.h` (PRI macros + `strto*max`),
  `locale.h` (LC_* + `struct lconv`), `libintl.h` (gettext as the identity — NLS on, no
  runtime), `assert.h`, `pwd.h`, `grp.h`, `sys/param.h`, `sys/sysmacros.h`, `sys/mtio.h`,
  `sgtty.h`. Plus `_IOC/_IOR/_IOW/_IOWR` in `sys/ioctl.h` (the tape ioctls compute from it),
  `BUFSIZ`/`FOPEN_MAX`/`FILENAME_MAX` in `stdio.h`, the missing errno constants
  (`ENXIO`/`EXDEV`/`ESPIPE`/… — an undefined `E*` in an expression is an undeclared identifier,
  which mooncc *refuses*, so this masqueraded as "cgfn refuses `<enclosing-fn>`"), and ~25
  prototypes across stdio/stdlib/unistd/fcntl/time (`vfprintf`+the v-family, `sprintf`,
  `scanf`/`fscanf`/`sscanf`, `execl`/`execlp`/`creat`, `system`, `qsort`, `lchown`, `setuid`,
  `truncate`, `localtime_r`, `strtoll`/`strtoull`, `getpwnam`/`getgrgid`, …). `nolibc.c` gained
  the v-printf family + `sprintf` (thin wrappers over the existing `__fmt`, which already
  threads a `va_list`).

- **`__FILE__` was BROKEN** — `cpp.l:15` advertised it as predefined but only `__LINE__` ever
  had a handler; any `__FILE__` use hit a codegen refusal (worst inside a void ternary arm, e.g.
  an `assert` expansion). Fixed by predefining it as an object macro — the TU path is threaded
  into `cpp` (a third arg; `moon.l`/`law.l` callers updated) and pinned like `__STDC__`.
  Includes share the macro table, so `__FILE__` reads the top file's name throughout — fine for
  assert/error diagnostics, which live in the `.c`. Added `__FILE__`/`__LINE__` laws (the gap
  existed *because* there was no law).

Both gates green (`test_moon` 88-program battery, `test_raw` gcc-free).

**`sizeof EXPR` array bound — LANDED** (`crew/moon/{parse,law}.l`). `list.c`'s
`char namebuf[sizeof h->prefix + 1 + sizeof h->name + 1]` sizes an array by the sizeof of a
struct member off a local pointer. `sizeof(type)` already folds at parse (`tsz`), but
`sizeof <lvalue>` was a deferred `szof` node gen sizes at codegen — too late for an array
bound (`adims`→`cfold` needs a `charm?` constant). Fix = a parse-time expr-typer + local-type
tracking:
- **`ps 'locals`** (name → declared type): block decls register in `pblock`'s items loop and
  params at the fn body, both threaded onto the block's shadow list so `unshadow` restores C
  scope (extended to *pull* when the saved old is the `'none` = fresh-name sentinel).
- **`ptype ps e`** — the UNDECAYED type of an lvalue/pointer expr, or `()` when parse can't
  settle it: a local/param `var`, a `.`/`->` member (via `ps 'stag`), a `*`/`a[i]` element,
  a cast. Arrays stay undecayed (sizeof of an array is its span) — matching gen's `szof` lane,
  which reads the same undecayed type via `clval`.
- **`punary` sizeof site**: when `ptype` settles the type, emit `('num (tsz ps ty))` — a real
  constant usable as an array bound; otherwise keep the deferred `('szof …)` so gen sizes it
  (identical behavior to before for every un-typeable expr). So the fold is a pure *extension*:
  `test_raw` compiles all of love.c + host/\*.c (many sizeofs) unchanged.

Verified `sizeof h->prefix + 1 + sizeof h->name + 1` = 257 (155+1+100+1) native == gcc; laws
in `law.l` prove both the member-sizeof bound *and* `sizeof buf` reading buf's own local array
type fold. **16/17 tar src now compile** (only `compare.c` remained — landed below).

**`compare.c` — LANDED (`crew/moon/include/linux/fd.h` shim).** NOT a cpp divergence (the earlier
guess was wrong — bisecting proved it). The real chain: `config.h` sets `HAVE_LINUX_FD_H 1`, so
`compare.c` does `#include <linux/fd.h>`; mooncc has no such header and falls through to the real
`/usr/include/linux/fd.h`, whose `struct floppy_fdc_state` carries **bitfield members**
(`unsigned int rawcmd:2;`) — and mooncc's `pmembers` doesn't parse the `: width` syntax, so the
parse fails DEEP in a kernel header. compare.c uses only `FDFLUSH` from it (`#ifdef FDFLUSH`
guards the one `ioctl` call), so a 3-line shim (`#include <sys/ioctl.h>` + `#define FDFLUSH
_IO(2,0x4b)`) stops the fallthrough. Verified `FDFLUSH` = 587 native == system. **17/17 tar src
compile.** ⚠ the bisection method: flatten the failing header with `gcc -E -P` (system headers,
so it resolves), then binary-search brace-BALANCED prefixes with mooncc (`-I` must be ABSOLUTE —
a relative `-I` breaks the moment the harness `cd`s elsewhere, the false-positive that first
fingered the wrong struct). **Bitfields stay an unimplemented parser+gen gap** — safe (refused,
not miscompiled), and no tar TU actually reads a bitfield VALUE; implement when a package needs
one, not for a header it only declares.

**tar 1.13 RUNS — the third rung, fully runnable (2026-07-16).** A 319 KB static ELF built
only by mooncc + nolibc + the holo linker (no gcc/glibc/ld), zero undefined symbols. Verified:
`tar cf`/`xf` byte-identical roundtrip, `tar czf`/`xzf` (forks gzip through a pipe), symlinks
preserved, and system tar reads our archives (interop both ways). Gates stay green (`test_raw`
3453, `test_moon`).

The distance was measured, not guessed: link all 15 tar objects + the 21 `libtar.a` objects,
`nm`-diff every referenced symbol against nolibc/sys/math → **39 external symbols, every one a
library gap, zero compiler features**. What filled them:

- **nolibc.c grew ~40 wrappers/bodies**: six new syscall NRs both arches (faccessat, mknodat,
  nanosleep, setuid, setgid, geteuid) behind `access`/`creat`/`dup`/`lchown`/`mknod`/`mkfifo`/
  `wait`/`usleep`/`time`/`setuid`/`setgid`/`geteuid`; ctype (`isalpha`/`isspace`/`isprint`/
  `tolower`) + `strchr`/`strerror`/`strcasecmp`/`strncasecmp`; `abort`/`atol`/`realloc`/
  `strtoul`/`strtoull`/`strtoumax`; a shellsort `qsort`; variadic `execl`/`execlp`; `system`
  (fork + `/bin/sh -c` + wait); a `"C"` `setlocale`; unbuffered `getc`/`fputs`/`ferror` + a
  minimal `fscanf` (tar's lone use is `%d`, no ungetc); and pwd/grp stubs that miss → tar's own
  numeric-owner fallback (a real `/etc/passwd` walk is a later rung).
- **`alloca`** has no native or `__builtin_` form, so nolibc hand-implements gnulib's **C_ALLOCA
  depth-GC** scheme: malloc-backed blocks tagged with the caller's probe address, reclaimed on
  the next call once a frame has unwound (both arches grow down → free blocks whose `mark <
  &here`). Leak-free without a stack-direction probe.

Two durable front-end/header findings (NOT compiler changes):

- **pre-C89 gnulib TUs omit `<config.h>` and `<stdlib.h>`** (argmatch.c is the case): they call
  `exit`/`strncasecmp` on the C89 implicit-`int` decl mooncc refuses. Fix, all keeping tar
  pristine: pass `-DSTDC_HEADERS=1` on the mooncc line (pulls argmatch's guarded `<string.h>` —
  the macro configure's own config.h sets, honest for our environment), surface `exit`/`abort`
  in `stdio.h` (the same "the LFS ladder leans on cross-header provision" note already there),
  and add `strcasecmp`/`strncasecmp` to `string.h` (POSIX, absent from the ANSI header).
- **`fnmatch`** is satisfied by linking tar's OWN bundled `lib/fnmatch.c` (it compiles clean) —
  configure omits it from `libtar.a` only because it found a system fnmatch.

NEXT: package this into a repeatable target (`mk/distro.mk` or the test harness) — build tar
from the pristine upstream tree with `-DSTDC_HEADERS=1`, the 15 src + 21 libtar objects +
`lib/fnmatch.c` + nolibc/sys/math, and gate a roundtrip against system tar. Then `less` or `m4`
(next rungs), or the deferred bitfield parser+gen if a package finally reads one.
(All three happened: `make moon-tar`, bitfields `908ed40c` when m4's obstack.h finally read
one, and m4 below.)

## m4 1.4 — the fourth rung, fully runnable (2026-07-16)

GNU m4 1.4 builds with mooncc + nolibc + holo into a 330 KB static ELF and passes **its own
57-check suite** (checks/ — every example lifted from the manual, stdout AND stderr compared).
`make moon-m4 M4SRC=<a ./configure'd m4-1.4 tree>` is the repeatable target (tools/moon-m4.sh,
the moon-tar model). m4 is the first *interesting* program up the ladder: diversions exercise
`tmpfile`/`rewind`, `esyscmd`/`syscmd` fork a shell through `popen`, and `format` drives float
formatting through `sprintf` — three whole subsystems tar never touched.

Two compiler-level finds, both fixed in mooncc proper:

- **bitfields** (`908ed40c`, the day's prerequisite): obstack.h reads them; parse `:width` +
  LSB-first bit-cursor packing to the SysV/gcc ABI, 2-shift load, RMW store — byte-identical
  to gcc. Took m4 src from 0/11 to 9/11 and unblocked everything below.
- **a `\<nl>` inside a `/* */` comment is a splice** — C phase 2 (splice) runs before phase 3
  (comments), so it never ends the logical line. mooncc's comment eater counted every newline,
  so a `#define` whose body carried a spliced multi-line comment (regex.c's PUSH_FAILURE_POINT)
  lost its tail to top level: cpp's `linesplit` groups a directive by line-number equality, the
  bumped count cut the macro at the comment, and the leaked body read as a parse error. The fix
  mirrors the splice rule the lexer already had outside comments; a law in law.l's cpp section
  pins it. **Forensic note**: the misleading first signal was `cc: lex error` — from the STALE
  installed `~/.local` mooncc.image (pre-`\f`-fix); probe with the fresh
  `out/host/mooncc.image` before believing a reproduction.

The rest was the usual two ladders, measured by probe + nm-diff (all library, zero compiler):

- **headers**: our own `alloca.h` (one prototype; without it the `<>` resolver fell through to
  glibc's and its `<features.h>` machinery — the compare.c lesson again), `popen`/`pclose`/
  `tmpfile` + the `strlen`/`strcmp`/`strncmp` trio in stdio.h (GNU getopt.c's non-GNU-libc
  branch includes ONLY stdio.h and calls them implicitly — do NOT `-D__GNU_LIBRARY__`, that
  compiles the whole file out), `atof`/`mktemp` in stdlib.h.
- **nolibc**: `setbuf` (m4 -e wants stdout unbuffered), `popen`/`pclose` (system()'s shape with
  a pipe spliced on; the child pid rides a new FILE field), `sscanf` (%d, strtol-backed),
  `mktemp`/`tmpfile` (mktemp'd, opened w+, unlinked at once), `ungetc` + a getc pushback byte
  (cleared on fseek, per ISO), `rewind`, the ctype sextet, `atof`, a real `strerror` table
  (errno 1–34 — the check suite string-compares "No such file or directory"), and **`__fmt`
  grew the float lanes**: `%f %e %g` + E/G with width/precision/rounding, classic
  digit-at-a-time (~1ulp floor, ties half-up where glibc rounds half-even; an integer part
  past 2^64 rides the %e lane). Differentially verified against glibc printf — identical on
  the battery except the 2.5-tie.
- **config corrections, not source patches** (config.h is a GENERATED file describing the
  target libc, and configure probed glibc): `HAVE_EFGCVT` off (nolibc has no `ecvt`; format.c's
  `#else` sprintf branch is the better code anyway — note m4-1.4's format has no `%g` case in
  either branch, faithful vintage behavior), `USE_STACKOVF` off (wants sigaltstack +
  `sys/resource.h`; a nicety). stackovf.o and alloca.o simply aren't linked — same as their
  Makefile under this configure answer.

One debug lesson: `cgfn refuses <fn>` names the function, not the node. A temporary probe
(print the failing expr at cgexpr's return, deepest-first) found `free`/`pclose`/`atof`/`ecvt`
in seconds — but it CANNOT be left in: recoverable bads are a normal lane (cginl's fesc
decline falls back to a real call), so the probe spams clean compiles. Re-add for a session,
revert before committing.

## Lua 5.4.7 — the fifth rung, and the first to leave x86-64

Lua builds with mooncc + nolibc + holo into a 513 KB static ELF and runs a battery over
closures, strings, patterns, tables, math, pcall/coroutines (setjmp/longjmp through sys.o's
leaves), metatables, gc, os date/time, io and `load`. `make moon-lua LUASRC=<an extracted
lua-5.4.x tree>` is the repeatable target (tools/moon-lua.sh). Lua needs **no configure**, and
it is the first package where every source file compiles unpatched — 33/33, first try.

**`make moon-lua-arm64` cross-builds the same sources with `mooncc -t arm64` and runs the
battery under qemu-aarch64** (642 KB; skips cleanly without qemu, like test_raw_arm64). Every
package rung before this one was x64-only, so for four of the five backends no package-scale
differential existed at all — the same gap ccarch.sh was written to close for test/cc.

It paid immediately. The first arm64 build produced an interpreter that ran its whole battery,
printed floats correctly and drove coroutines through setjmp — and in which `string.match`,
`string.gsub` and `string.find`-with-a-pattern **all silently answered nil**. Plain `find` was
fine, because a pattern with no specials shortcuts to `lmemfind` and never enters the matcher.

The cause is a codegen collision, now pinned by test/cc/110-param5.c:

- gen speaks `r4` for the frame base on every target, and `a4ize` retargets it to `fp` at the
  end of build **by position** — an `r4` in a memory op's BASE slot becomes `fp`, an `r4`
  anywhere else is left alone (it is the 5th argument).
- On AArch64 gp 4 arrives in x4. The moment the ride analysis let the 5th parameter stay in its
  arrival register, and the body used it the way a pointer is used, its base slot read as the
  frame pointer. `*p`, `p[i]` and `p + i` all addressed the FRAME.
- lstrlib's `prepstate` takes six parameters; the 6th spilled and the 5th rode x4, so
  `ms->p_end` came out as `sp + lp` and every `do_match` ran against a garbage pattern end.

The fix bars a rider whose arrival register collides with the build-time frame-base spelling
(`rideset`, crew/moon/gen.l). It costs nothing measurable: with the ride barred the param homes
to r14, `a4ize` renames the frame base, and `unhome` — which runs *after* `a4ize` — renames r14
back to r4 safely, so the param still rides its arrival register end to end.

x64 cannot reach this (r4 is rbp, never an argument register), thumb has only four argument
registers, and riscv has `nhome = 0` so nothing rides. That is why 110 single-file programs and
the whole love corpus under mooncc/arm64 stayed green over it. **The lesson is the one
ccarch.sh's header already states, one level up: a shared model's fault hides behind whichever
lane the other target lacks, and a 30k-line package is a much wider net than a test suite
someone wrote on purpose.** The shape that reaches this bug — a six-parameter function whose
5th is a pointer — is not one anybody writes into a compiler test deliberately.

## suggested order (LFS-shaped, easiest real C first)

bzip2 → gzip → less → m4 → make → sed/grep (gnulib-heavy, harder) → bash → coreutils.
Skip the two-pass cross-toolchain ritual entirely — mooncc/holo/nolibc already ARE the
self-hosting toolchain ([[love-distro]]). Link initially with a foreign `ld` + the host libc
(the "gcc appears once as ld" precedent), then move packages onto nolibc/holo as their
libc surface is filled in.

## reproduce

```
# base (already green): love boots as pid 1 on Linux
make distro-run          # or distro-smoke for the headless check

# the bzip2 probe (ctype/ungetc shims now landed in crew/moon/include):
mooncc -Icrew/moon/include -I<bzip2> -c <bzip2>/crctable.c out.o   # -> a real .o
# 5/8 library files compile; decompress.c = rung A (nested case), bzlib/bzip2.c = rung C
```

## the far end — the kernel (for reference)

The same ladder's cliff, from the mooncc-capability survey (2026-07-15): inline asm with
constraints/clobbers is **absent** and there's no register allocator to bind its operands
(stack-machine gen, doc/moon.md:141); `__attribute__` is balance-skipped, not honored (only
`weak`/`section` act); statement-exprs, `typeof`, computed goto, `__int128`,
VLAs, and hundreds of `__builtin_*` are absent (bitfields landed `908ed40c`); no
`linux/*`/`asm/*` headers, no atomics/
privileged instructions, the linker refuses foreign `.o` and has no linker-script support.
Don't start here. Fill the userland first; the extensions the last userland packages force
(more `__builtin_*`, attribute honesty) are the same ones the kernel needs — arrive there
having already built them.

Related: [[love-distro]], doc/moon.md (the compiler), init/boot.l + mk/distro.mk (the base).
