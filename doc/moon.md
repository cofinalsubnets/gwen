# moon — the C compiler, in love

`mooncc` is a chibicc-class C compiler written in love, emitting through the holo books. With
`crew/holo/link.l` (our static linker) and `crew/moon/lib/` (our libc, math floor and machine
tail) it is a **complete C toolchain that borrows nothing**: love builds itself with no gcc, no
glibc and no ld, and the kernel is built by it too.

The closure it buys: love-in-love compiles the compiler that compiles love.c.

**The one firm fence: NOT C++, ever.**

`make test_moon` is the gate; `make test_raw` runs the whole corpus on a gcc/glibc/ld-free build;
`make test_fixpoint` proves `mooncc(mooncc(love))` byte-identical to `mooncc(love)`;
`make test_drv` gates the driver conventions; `make test_libc` differentials the library
(doc/libc.md). See doc/moon-kernel.md for the kernel lane, doc/moon-diag.md for diagnostics and
doc/moon-c-gaps.md for the dialect's edges.

## the subset, measured off love.c

The dialect is not "C11-ish" by taste — it is what the target demands:

* the whole statement/expression core; switch, goto (plain labels only — NO computed goto),
  do/while/for, the comma operator.
* typedefs, structs, unions, enums, nested aggregates, a flexible array member, designated
  initializers, ANONYMOUS unions and structs, compound literals.
* function pointers as first-class citizens — the lvm dispatch tables ARE the program. Pointer
  arithmetic throughout, multidimensional arrays.
* varargs, in the real SysV shape (below).
* the preprocessor in anger: object + function-like macros, variadic macros, `##` token paste,
  `#if`/`#ifdef` trees, `#include`.
* `double` and `float`; NO long double.
* `_Static_assert` (both the two-argument and the C23 one-argument form); `__attribute__`
  parsed and, where it matters, honored (`section(..)`, `always_inline`, `noinline`).
* setjmp/sigsetjmp + signal handlers — the library's problem, not the compiler's; mooncc only
  needs the calls and the volatile discipline around them.
* the tail-threaded VM: `return Continue()` everywhere, with **guaranteed sibcalls** so the
  stack stays flat (below).

## the architecture

`crew/moon/`, the kore discipline: pure engines with law files, a thin driver, one gate per
piece. ~11k lines of love.

* **lex.l** — text → token list (pure). Tokens carry file/line for diagnostics, and a 4th field
  flagging a `(` GLUED to the preceding identifier — which is what distinguishes a function
  macro from an object macro whose body opens with a paren.
* **cpp.l** — token list → token list (pure). Token-based, so an identifier inside a string or
  char literal — already one opaque token — is never mistaken for a macro. Rescan to a fixpoint
  under Prosser's HIDESETS (the blue-paint token field, so `FOO`→`FOO` stops); `#` stringize;
  `##` paste (fold + relex); `...`/`__VA_ARGS__` and GNU's `, ##__VA_ARGS__` comma elision;
  the `#if` family over a precedence-climbing integer const-expr evaluator; `#include` through
  an `incf` hook so the laws can feed includes as data.
* **parse.l** — tokens → AST + the type layer. C cannot be parsed statelessly, so the parser
  carries state: typedef names, enum constants and the struct tag table, which ride out with
  the AST for gen's sizing. A local shadows a typedef or an enum constant for the rest of its
  block (pinned on the declaration, restored at `}`) — love.h makes `num`/`word` typedefs and
  love.c uses both as local variable names. A signature table (`ps 'sigs`: name → return type,
  parameter types, variadic bit) comes out as a fourth value.
  ⚠ **A read-modify-write may not duplicate its lvalue.** `x op= y` desugars to
  `(asn x (bin op x y))` and `++x` to the same — exact only when evaluating the lvalue leaves
  no trace, so `calm?` gates it and the other two doors take the address once: `++`/`--` ride
  `('post lv step)`, `op=` stays whole as `('rmw op lv rhs)` for gen to moor.
* **gen.l** — AST → holo IR (pure), chibicc-plain but **typed**: `cgexpr` answers
  `(type forms)`, so pointer arithmetic scales by pointee size, a dereference loads by pointee
  width, and an array decays to an address. Lvalues have one door (`clval`: the address in r0
  plus the pointee type), through which `x`, `*p` and `a[i]` all assign — and through which
  `('moor off ty)`, gen's own lvalue for an address already parked in a frame temp, lets an
  `('rmw ...)` reuse every store lane there is without evaluating its target twice. The ALU
  stays 64-bit —
  sound because signed overflow is UB — and widths bite only at memory and casts. This is also
  where the register allocator lives (doc/moon-regalloc territory).
* **fmt.l** — diagnostics.
* **clay.l** — C as love data (doc/clay.md).
* **stage.l** — the pipeline's stages, typed: each pass's signature (input stage → output
  stage). The moon gate checks gen.l *as data* against it, so a new pass declares its sig there
  and a bad recomposition is a clash naming its seam.
* **moon.l** — the driver.
* **law.l** — the laws, ~1360 of them.

## the driver

```
mooncc [-c] [-pie] [-nostdinc] [-t TARGET] [-Ttext addr] [-I dir] [-D name[=val]] [-o out] in.c|in.o ..
```

Several inputs need `-c` and land each in the cwd as `x.o` (gcc-shaped); the old positional pair
`mooncc [-c] IN OUT` still reads. `-I` dirs search before the system pair on both include forms.
A `-D` prepends a `#define` line to the source text before the one lex, so a function-like
`-DF(x)=..` rides the normal macro path (and diagnostics under `-D` skew by the define count).

Targets: `x64`/`amd64`, `arm64`/`aarch64`, `riscv64`, `thumb2`/`cortex-m7`,
`thumb1`/`cortex-m0`, `thumb2sp`/`playdate`.

Anything without `-c` is a **link**, through `crew/holo/link.l`.

**The cc conventions** — `CC=mooncc` drives a gcc-shaped recipe unchanged:

- the **advisory** families (`-W..` `-O..` `-g..` `-std=` `-f..` `-pipe` `-static`) ride through
  ignored;
- an exe link still owing strong symbols pulls the runtime **by need**, archive-fashion — nolibc
  + the am math + the mksys leaf, compiled from the toolchain root, so a set carrying its own
  `am.o` never meets a twin;
- `-nostdlib`/`-nodefaultlibs`/`-ffreestanding` turn that pull off;
- `-ffreestanding` ALSO says the standard's own word: it makes `__STDC_HOSTED__` 0, which is how
  a source asks (love.c asks it to choose the W^X mmap arena over the freestanding heap copy).
  ⚠ **Only that flag** — the rest of the family is a hosted program supplying its own runtime,
  which is exactly what `test_raw` is;
- `-nostdinc` is the INCLUDE half of that word and drops `/usr/include` off the search tail, so
  only our own headers answer. It is **loud, not advisory**: with the tail on, a header we do
  not carry resolves to glibc's, and a freestanding build taking a hosted declaration is the
  wrong artifact wearing a green face;
- the **semantic** refusals stay loud (`-shared`, `-Wl,`'s payload, `-m..`) — an ignored one
  would be the silent-no-op trap in a cc suit. ⚠ mooncc **refuses** a `-m` rather than ignoring
  it.

Errors speak on err and exit 1; usage exits 2.

## the toolchain root

mooncc's own files — our headers (`crew/moon/include/`, glibc-ABI-faithful but NOT glibc's) and
the runtime sources the implicit link pulls — are found through a two-rung walk, tried in order:

1. **the dev tree**, `crew/moon/` off the cwd;
2. **the installed nest**, `<seat>/../lib/love/moon/` — the loader's own seat walk, readlink
   `/proc/self/exe`. So `~/.love/bin/love` finds `~/.love/lib/love/moon/`, and a distro's
   `/usr/bin/love` finds `/usr/lib/love/moon/`. `mk/install.mk` lays them there.

Without this an installed mooncc outside a source tree cannot compile hello-world at all:
`<stdio.h>` falls through to `/usr/include` — *glibc's*, whose stdio.h wants the compiler's own
stddef.h — and the link finds no libc. Gate: `test/gate/nest.sh` compiles and links from a
scratch cwd against the installed nest (`cd` matters: from the repo root rung 1 serves and rung
2 is never exercised).

⚠ **The root is READ AT EACH CALL, never bound.** mooncc rides a baked image, and a captured
seat would fold the build tree's path into that image and ride it forever.

Owing symbols with NO root in reach is its own diagnostic, naming the owed symbols and the roots
searched — an absent toolchain and an incomplete link are different conditions and must not wear
the same face.

## the runtime (crew/moon/lib/)

* **nolibc.c** — the raw libc over one `__ai_sys` trampoline: a mini stdio (a FILE is a fd plus
  a flush buffer), a K&R first-fit malloc over mmap arenas, dirent over getdents64, the
  glibc-152B-to-kernel-32B sigaction fold with our own restorer, a numeric getaddrinfo,
  env/exec/termios/pty. Single-threaded like love: errno is one int, no locks. See doc/libc.md.
* **mksys.l** — lays `sys.o`, the things C cannot say: the 7-slot syscall trampoline,
  `__sigsetjmp`/`siglongjmp` over our own layout inside the glibc-sized 25-long buffer (the
  signal mask in `buf[8]`, saved/restored by rt_sigprocmask — love.c's fault barrier is
  `sigsetjmp(env,1)`, so the mask is load-bearing), and `__ai_sigret` (the SA_RESTORER tail).
  Every encoding objdump-checked, the holo house rule.
* **math/am.c** — our transcendentals. sqrt exact, the seven within a few ulp; `make ulp` is the
  differential gate. `-lm` appears in no link.

**The crt0 switch is one weak symbol.** `__ai_start` is defined WEAK in the crt0 object (the
bare call-main tail every small link gets), and nolibc overrides it STRONG to unpack
argv/envp/auxv before main — no link-time flag anywhere, the weak machinery IS the switch.

## the ABI

* **Arguments** ride the SysV registers (r6 r5 r2 r1 r7 r8 in holo's neutral file); args 7+ ride
  the caller stack, arg7 shallowest, caller cleans, odd counts padded for 16-alignment; the
  callee reads them at `rbp+16+8k`.
* **Varargs are real SysV.** The `va_list` is the 24-byte
  `{int gp_offset; int fp_offset; void *overflow_arg_area; void *reg_save_area;}`, a typedef to
  `__va_list_tag[1]` so it DECAYS to a pointer when handed on. The callee prologue lays a
  176-byte register save area (6 gp @ +0 step 8, 8 xmm @ +48 step 16) and addresses its named
  register-passed params inside it; `va_start` seeds the offsets, `va_arg` walks the register
  area until its offset passes the limit, then the overflow area. The xmm registers are saved
  unconditionally (reading an xmm never faults), so the callee needs no `al` guard; the caller
  sets `al` = #xmm args.
* **`float` is 4 bytes in memory but always a double in an xmm register** — a load widens
  (`ldss`+`cvtss2sd`), a store narrows. Only the DECLARED type drives the 4-vs-8-byte choice.
  This is self-consistent and matches gcc for values representable in both.
* ⚠ **16-byte stack alignment is ours to keep.** A spill that outlives a nested call reserves a
  16-byte cell, never an 8-byte push — `rsp` must be 16-aligned at every call, or the first
  callee that stores aligned SSE to an rbp-relative slot (glibc's `fork` child path is the
  classic) takes a #GP. This is invisible to love.c's own code and to a `-O0` gcc differential,
  so it is gated directly (`test_moon`'s `g=id(fork())` program).
* ⚠ **rbx (holo r3) is callee-saved** and every function owns frame slot -8 for it; the gate
  links a mooncc callee against an `-O2` caller holding a loop bound live in ebx.

## sibcalls, and the flat stack

A RET-position call **tail-jumps**: the epilogue reloads rbx and `jmp F` replaces `call F`, so
deep tail recursion runs flat and the lvm shape Continues by jmp. The rewrite is a LOCAL
peephole (a call immediately followed by the exact epilogue, or by a join label leading to it),
gated per function by an **escape analysis** — a frame address that becomes a VALUE (the `&`
lane, a local array decaying, a struct-value rep, `va_start`'s save area) pins `fesc` and the
function keeps all its calls; an lvalue's own load/store rides `lean` and stays eligible.
And `__attribute__((musttail))` on a return is OWED, not opportunistic: the annotation rides
the ret's PRE slot, gen marks the call, and a shape the rewrite cannot take REFUSES the
compile (`musttail-not-a-tail` / `musttail-escape`) — the clang/gcc-15 semantic, which is how
love.h holds every VM tail to the jump under all three compilers. `make vmret` stays as the
cross-check on the shipped binary.

Predefines worth knowing: `__mooncc__`, `__linux__`, `__x86_64__` (or the target's twin), and
`__STDC_HOSTED__` = 1 for a hosted link / 0 under `-ffreestanding`. `__SIZEOF_INT128__` is
predefined on x64 alone (gen's d128 lane), which is what love.c's limb seam reads.

## inline asm

The GNU statement form, with ONE deliberate twist: the template is holo's neutral TEXT
(`crew/holo/text.l`'s `asm-text` parses it, the baked assembler encodes it), not AT&T — so one
template rides both targets wherever it sticks to the neutral surface, and no new assembler
exists anywhere.

    asm [volatile] ("li %0, 40" : "=r"(v) : "r"(x), "i"(3) : "memory");

* Registers are the neutral file: x64 r0=rax r1=rcx r2=rdx r3=rbx r4=rbp (the frame) r5=rsi
  r6=rdi r7..r14=r8..r15; arm64 rN=xN. So a raw x64 syscall is
  `asm("sys" : : "r0"(nr), "r6"(a0), "r5"(a1), "r2"(a2))`.
* Constraints: `"r"`/`"=r"`/`"+r"` pick a register, `"rN"` forms pin one, `"i"` an immediate
  (parse-time constant). `%0..%9` substitute (outputs first), `%%` a literal `%`. Adjacent
  template strings concatenate.
* The body assembles AT CODEGEN into one opaque `('raw bytes)`: the IR passes barrier on raw,
  labels inside a template stay LOCAL to it, and no pass ever rewrites user instructions.
  External symbols cannot be named in a template — reach values through operands (`"r"(&x)`
  works, and the address-taken local also fences deadst).
* Operands stage through the machine stack, so calls inside operand expressions are safe, and
  any scalar lvalue output works (`*p`, `a[i]`). Float/struct/bitfield operands refuse.
* Allowed registers (operands + clobbers): x64 r0-r3 + r5-r10 (r3 rides every prologue's -8
  slot; r4 is the frame and refuses), arm64 adds r4 (x4, an argument register there — 5+-arg
  syscalls need it). `sp` and the callee-saved r11-r14 refuse. Clobbers
  (`"memory"`/`"cc"`/register names) are validated but need no action: an asm-containing function
  turns register HOMING off (`g 'hasasm`), so nothing lives in a register across any statement.
* ⚠ **A multi-instruction template separates on `\n`, NEVER `;`** — the neutral reader takes `;`
  as a comment to end of line, so a `;`-joined template assembles its first instruction and
  SILENTLY DROPS the rest. `\n` is also what GNU wants, so it is the separator that serves a
  two-spelling header.
* The first consumer is the kernel's `port/inle/<a>/asmops.h` (doc/moon-kernel.md), which
  carries both spellings behind the `__mooncc__` predefine. Worth reading for how far the two
  dialects agree: a bare mnemonic and a `mnemonic op, op` line are the SAME text in both.
* Deferred until a consumer demands them: an AT&T template front-end, `"f"` float operands, asm
  goto, named `[sym]` operands, top-level asm.

## the installed shape

`make install` does not ship the cat as `bin/mooncc`. The compiler bakes WARM into
`lib/love/mooncc.image` (the live bake nif, doc/snapshot.md) and `bin/mooncc` is a three-line sh
shim: `love --wake mooncc.image -e "(moon-main (cuup (cup cmdline)))" "$@"`. The whole-cat
re-eval every compile would otherwise pay (~1.5 s wall) is paid once, at bake — a small-file
compile drops from ~0.77 s to ~0.02 s, gcc-class invocation latency.

⚠ The image is binary-specific (anchor-checked) and installs from the same build as `bin/love`
(strip keeps vaddrs, so the stripped install wakes it); a mismatched pair falls back to a fresh
boot with no `moon-main`, so never mix builds by hand.

⚠ A catted app is `#!/usr/bin/env -S love` plus the cat, so a bare `mooncc` runs on the PATH
`love` — a STALE install mis-runs it. Probe the repo cat with `./out/host/love out/host/mooncc`,
never a bare `mooncc`, until `make install` refreshes the PATH binary.

## testing

* Every pure piece is lawed in `crew/moon/law.l`: lexer goldens, cpp expansions, parser ASTs
  printed and compared, layout/alignment tables, gen goldens.
* **The differential oracle is `gcc -O0`**: same source, run both, compare stdout + exit code.
  The battery lives in `test/cc/*.c` and ONLY grows — every bug fixed adds its regression.
  ⚠ Differential programs must be **UB-free**: `pick(++i,++i,++i)` is unsequenced, and gcc
  legitimately disagrees.
* A seeded expression fuzz against gcc (`test_moonfuzz`).
* **An OUTSIDE corpus, and its own answers** (`test_cts`, all three targets): c-testsuite's
  220 single-file programs, each held to the stdout it ships. Every `test/cc` file was written
  here to pin a fault we had already met, so the battery says what we already know; these were
  written by people compiling other compilers, and their first run found **nine** programs
  mooncc built clean and answered wrong — all nine landed, and the wrong-answer roster is
  **empty** today. What remains is refusals. They are rostered with a cause apiece in
  `test/gate/cts.sh`, refusals kept apart from wrong answers, and the roster is double-edged —
  a fix takes its line off, a regression cannot hide in a skip.
* Cross targets get their own gates (`test_ccarm64`, `test_ccriscv`, `test_thumb*`), and
  doc/mooncc-differentials records why a package on a cross target beats a test suite on one.
* The corpus itself is the deepest oracle: `test_raw` runs it over a gcc-free build,
  `test_fixpoint` pins the compile byte-for-byte.

## known gaps

`gen.l`'s header carries the live fence list — what is refused rather than fudged: `ptr+ptr`,
dereference of a non-pointer, assigning to an array. Also: `&bitfield`, and `++`/`--` on a
bitfield or a wide type. doc/moon-c-gaps.md is the fuller map, and carries the by-value
aggregate lanes the cross targets still lack.

## the basement, and the go borrowings

Two ideas kept warm, neither committed:

* **a basement language under C, in the spirit of historical B.** This compiler's core is
  B-shaped already — one word type, typeless 64-bit registers, C's types a
  checking-and-conversion layer laid ON TOP of the word core (sized memory ops at the edges,
  words in the middle). love itself is B-kin the same way. So the basement may want to become a
  real, nameable layer: the typeless word language gen already speaks internally, possibly with
  its own thin surface syntax — useful for runtime shims, the crt0, compiler self-tests, and as
  the honest semantic floor the C dialect desugars onto.
* **syntactic refinements borrowed from go.** Candidates to weigh when the grammar is fuller:
  unparenthesized conditions with mandatory braces, `:=` short declarations, cleaner declarator
  spellings for the gnarly cases (function-pointer types especially). The fence stands:
  extensions, opt-in, never needed to compile plain C — love.c stays the gate and it is written
  in C.

## naming

crew `moon` (🍄 the glowing mycelium on holo's cave walls, a sibling to inle) and the binary
`mooncc`, which slots into the cc/gcc/tcc tradition and clears the `moon`(MoonBit) /
`moonc`(MoonScript) collision.
