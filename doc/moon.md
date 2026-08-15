# moon — the C compiler, in love

`mooncc` is a C compiler written in love (chibicc was the seed), emitting through the holo books. With
`crew/holo/link.l` (our static linker) and `crew/moon/lib/` (our libc, math floor and machine
tail) it is a **complete C toolchain that borrows nothing**: love builds itself with no gcc, no
glibc and no ld, and the kernel is built by it too.

The closure it buys: love-in-love compiles the compiler that compiles love.c.

**The one firm fence: NOT C++, ever.**

`make test_moon` is the gate; `make test_raw` runs the whole corpus on a gcc/glibc/ld-free build;
`make test_fixpoint` proves `mooncc(mooncc(love))` byte-identical to `mooncc(love)`;
`make test_drv` gates the driver conventions; `make test_libc` differentials the library. See for the kernel lane for diagnostics and
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

* **floor.l** — the C type floor: the laws that are neither syntax nor codegen (the type shapes,
  `tysz`/`tyalign`, and the typing door — promotions and the usual arithmetic conversions),
  spelled once and read from both sides. Pure: the machine word and the struct-tag table arrive
  as PARAMETERS, so parse threads them off `ps`, gen off `g`, and neither owns the law. This is
  the file a front that is not `cparse` stands on — it names no token, parse state or register.
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
* **gen.l** — AST → holo IR (pure), **typed**: `cgexpr` answers
  `(type forms)`, so pointer arithmetic scales by pointee size, a dereference loads by pointee
  width, and an array decays to an address. Lvalues have one door (`clval`: the address in r0
  plus the pointee type), through which `x`, `*p` and `a[i]` all assign — and through which
  `('moor off ty)`, gen's own lvalue for an address already parked in a frame temp, lets an
  `('rmw ...)` reuse every store lane there is without evaluating its target twice. The ALU
  stays 64-bit —
  sound because signed overflow is UB — and widths bite only at memory and casts. This is also
  where the register story lives.
* **fmt.l** — diagnostics.
* **clay.l** — C as love data (doc/clay.md).
* **stage.l** — the pipeline's stages, typed: each pass's signature (input stage → output
  stage). The moon gate checks gen.l *as data* against it, so a new pass declares its sig there
  and a bad recomposition is a clash naming its seam.
* **moon.l** — the driver.
* **law.l** — the laws, ~1360 of them.

## the driver

```
mooncc [-c] [-pie] [-nostdinc] [-fno-inline] [-fir=PFX[,PFX..]] [-fno-ir=PFX[,PFX..]] [-t TARGET] [-Ttext addr] [-I dir] [-D name[=val]] [-o out] in.c|in.o ..
```

Several inputs need `-c` and land each in the cwd as `x.o` (gcc-shaped); the old positional pair
`mooncc [-c] IN OUT` still reads. `-I` dirs search before the system pair on both include forms.
A `-D` prepends a `#define` line to the source text before the one lex, so a function-like
`-DF(x)=..` rides the normal macro path (and diagnostics under `-D` skew by the define count).

Targets: `x64`/`amd64`, `arm64`/`aarch64`, `riscv64`, `thumb2`/`cortex-m7`,
`thumb1`/`cortex-m0`, `thumb2sp`/`playdate`.

**`-std=` is a rail, not an advisory.** It is the one flag that says what *language* the
input is, so an unknown value refuses the compile rather than riding through ignored.

| `-std=` | dialect |
|---|---|
| *(unsaid)*, `moon` | **moon — the default.** Modern C plus the extensions below. |
| `c`, and `c89 c90 c99 c11 c17 c18 c23`, and each `gnu*` | strict C, no extensions, at whatever iteration we hover around (C11; doc/moon-c-gaps.md) |

Every moon extension is syntax C **rejects outright**, so moon is a strict superset: no C
program means anything different under it, which is the whole licence for defaulting to it.
`-std=c` is the fence for when that matters.

⚠ there is deliberately **no `holyc`**. Struct labels are borrowed from HolyC, but a flag by
that name would refuse `U0`, `I64` and `class` — a parse error on line 1 of anything actually
written in HolyC, which is the one input that would reach for it. The name comes back when
it is true.

⚠ `-std=` is lifted off the command line by a **pre-pass**, never an accumulator on the flag
walk: that walk keeps one tablet now, and `-std=` names the *language* rather than a flag, so
it belongs before the line is read at all.

### struct labels

A label where a member declaration would start names an **offset**: a zero-width marker
that holds nothing and takes no space.

```c
struct S { char a; hdr: long b; int c; tail: };
```

`s.hdr` **is** the address — no `&` to write, which is the point of naming a position. It
is typed as a zero-length `char` array, so it decays the way `char hdr[0]` does while
staying an lvalue, which keeps `offsetof(S, hdr)` answering. It costs nothing: the struct
above has exactly the size and offsets of `struct { char a; long b; int c; };`.

⚠ **the alignment is the whole feature.** The idiom this replaces is a zero-length array
marker, `char hdr[0];` — a GCC/Clang extension — and it has alignment 1, so it lands where
the *previous* field ended and names the padding: gcc puts it at **4** where the `long`
sits at **8**. A label aligns as the member it precedes, so it names the member.

A **trailing** label aligns as the struct, so it is one past the *object* — `offsetof(S,
tail) == sizeof(struct S)`, `end()`'s convention. One past the *data* would sit inside the
trailing padding and point at nothing an array of `S` would ever reach. In a union every
label flattens to 0, like every other member.

`gen.l` never learned about any of this: a label is one more row in the stag table, which
is the seam holding. `test/cc/141-structlabel.c` is the battery's own check — guarded on
`__moon__`, so gcc compiles the label-free half and both compilers must still agree — and
the gate's oracle is the same struct with the labels deleted, compiled by gcc: identical
size, identical offsets.

### the macros we answer

`__mooncc__` says which **compiler**; `__moon__` says which **dialect**, and only the
second is a licence to use an extension — under `-std=c` this is still mooncc and the
extension is still refused.

Anything without `-c` is a **link**, through `crew/holo/link.l`.

**The cc conventions** — `CC=mooncc` drives a gcc-shaped recipe unchanged:

- the **advisory** families (`-W..` `-O..` `-g..` `-f..` `-pipe` `-static`) ride through
  ignored -- less `-fno-inline`, which is real here (below) -- and so do glued `-l..`/`-L..`: the runtime is pulled by need, so the libc/libm a
  recipe asks for is already in the artifact before it asks, and a name we cannot satisfy still
  lands as a *named* undefined reference at the link rather than going quiet (a real third-party
  library has its own door — give the `.a` as an input). Lua's own `LIBS=-lm` is why this
  matters. ⚠ glued only: a bare `-l` refuses, since taking it would eat the next word as a
  library name and the one after it as an input;
- an exe link still owing strong symbols pulls the runtime **by need**, archive-fashion — nolibc
  + the am math + the mksys leaf, compiled from the toolchain root and cached under
  `~/.love/cache/moon/` (below), so a set carrying its own `am.o` never meets a twin;
- `-nostdlib`/`-nodefaultlibs`/`-ffreestanding` turn that pull off;
- `-ffreestanding` ALSO says the standard's own word: it makes `__STDC_HOSTED__` 0, which is how
  a source asks (love.c asks it to choose the W^X mmap arena over the freestanding heap copy).
  ⚠ **Only that flag** — the rest of the family is a hosted program supplying its own runtime,
  which is exactly what `test_raw` is;
- `-nostdinc` is the INCLUDE half of that word and drops `/usr/include` off the search tail, so
  only our own headers answer. It is **loud, not advisory**: with the tail on, a header we do
  not carry resolves to glibc's, and a freestanding build taking a hosted declaration is the
  wrong artifact wearing a green face;
- `-fno-inline` (and gcc's `-fno-inline-functions`) is the one member of the `-f` family that is
  **real** here: it bars every splice for the whole TU, the same door
  `__attribute__((noinline))` opens one name at a time, and it **outranks `always_inline`** —
  the flag is an instrument before it is an optimization switch, and a source attribute that
  could override it would leave the reader with no way to say *read this function as written*.
  Splicing is semantics-neutral, so the answer never moves; what moves is what you can read.
  That is what it is for: comparing one function's codegen against another cc's is impossible
  when a splice has rewritten it into its caller. Gated by `test_moon`, both halves — that it
  bites, and that the answer is unchanged;
- `-fir=PREFIX[,PREFIX..]` and `-fno-ir=PREFIX[,PREFIX..]` are the second real one, and they are
  the compiler **writing down what it built**: the machine-form IR of the functions you name,
  laid in `.rodata` under this TU's own `ai_ir_<basename>` as one readable datum —
  `((name form..) ..)`, which `sound` reads and holo assembles back as it stands, because that is
  what those forms are.

  **The set algebra**: positives collect, then negatives carve, and with no positive at all the
  set opens — so `-fno-ir=` alone means *everything but*. Both flags repeat, and a repeated flag
  is byte-identical to the comma list (gated). Some shapes:

  ```sh
  mooncc -fir=lvm_ …                       # every VM op handler
  mooncc -fir=lvm_ -fir=ai_ …              # ..and the runtime's own
  mooncc -fir=lvm_,ai_ …                   # the same object, exactly
  mooncc -fir=lvm_ -fno-ir=lvm_arg,lvm_quo # all but the load family
  mooncc -fno-ir=ai_ …                     # everything except ai_
  mooncc -fir= …                           # everything, no exceptions
  mooncc -fir …                            # ..the same, bare
  mooncc -fir=lvm_ -fno-ir …               # nothing: bare -fno-ir wins from either end
  ```

  **Bare, they fall out of the algebra rather than needing a rule.** `-fir` is the empty
  *positive*, which every name carries as a prefix, so it opens the set from anywhere;
  `-fno-ir` is the empty *negative*, and since negatives carve after positives whatever their
  order, it empties the record from either end and beats a later `-fir=`. ⚠ before they were
  named they matched the advisory `-f` family and were accepted in **silence**, answering a
  binary with no record at all. ⚠ an empty set lays **no symbol**, never one holding `()` — a
  reader finding the symbol would conclude the compiler had written down that there was
  nothing, which is a different claim.

  **Reading one back**: `love mk/tools/ir.l FILE [NAME | -l]`, over `lib/irec.l`. ⚠ **any file, any
  target, from any machine** — a record is text and an ELF is a table, so an aarch64 object
  reads on x86-64 with no disassembler, no per-arch mnemonic table and no objdump that has to
  have been built with the right target list. All six targets and both ELF classes are gated
  (three of the six are 32-bit ARM, where every header offset moves). ⚠ the reader is
  deliberately **not** `lib/splice.l`: the creation hook lights its lane on `(from 'splice)`,
  and looking at a binary must not start a JIT.

  **It is a debugging instrument before it is a JIT input** — the compiler's IR sitting beside
  the code it emitted, for whoever opens the binary. Read as one it already found a bug in
  itself: cross-checked against `make vmret`'s objdump reading of the tail-jump law, the two
  disagreed, and the record was wrong. The splice JIT (`lib/splice.l`) is
  the other reader: a `love` carries its own op handlers and composes them with **no compiler, no
  source tree and no disassembler**.

  **The shipped build spends bare `-fir` on every TU** (`$(moon_fir)`), so the artifact carries the
  machine-form IR of *everything it is* — 757 functions, ~1.4 MB of `.rodata`, `.text` unmoved.
  ⚠ **the symbol is per-TU**, `ai_ir_<basename>`: one fixed name made `-fir` a one-TU flag, since a
  second object carrying it died at the link with `link-dup "ai_lvm_ir"`. `nm | grep ai_ir_` is now
  the manifest of translation units, in link order.
  ⚠ **no cap at record time.** The splice budget used to live in `gen.l` and cut the record at 64
  forms, which made the compiler's provenance answer to the JIT's appetite — and a function too big
  to splice is exactly the one a reader most wants, having no chance of following it in a
  disassembler. The 64-form budget now sits in `lib/splice.l`, which declines an over-budget body
  by name just as it declines one it cannot lower. Side effect worth knowing: the JIT went from
  **105 splice-able handlers to 167**, because the record now covers the host TUs' nifs
  (`host/posix.c` alone carries 45) and not just love.c's.
  ⚠ it must MIRROR into `test/gate/fixpoint.sh` — a flag on love.c in one rebuild and not the
  other is a byte difference that reads as a broken compiler;
- the **semantic** refusals stay loud (`-shared`, `-Wl,`'s payload, `-m..`) — an ignored one
  would be the silent-no-op trap in a cc suit. ⚠ mooncc **refuses** a `-m` rather than ignoring
  it.

Errors speak on err and exit 1; usage exits 2.

## `.comment`, the producer record

Every executable our linker writes carries a `.comment` section — the same one gcc and clang
write, NUL-separated strings, no `SHF_ALLOC`, laid past `p_filesz` beside the symbol table, so
the program is the same program and the kernel maps not a byte of it. It answers the first
question anyone opening a strange binary asks: **what built this?**

It is a **union**, not a claim. Every input object's own `.comment` rides in ahead of ours,
first-seen order, deduped, and only then does the linker add its own word — `love`, whichever
door drove it, because it is one linker and the door carries no information the file needs. So a
link mixing a foreign object names both TOOLCHAINS and neither claims the other's code:

```sh
$ mooncc m.o gcc-built.o -o mix && readelf -p .comment mix
  [     0]  GCC: (GNU) 16.1.1 20260625
  [    1b]  love 0.1
```

⚠ **the version is `love-version`'s BASE half, never the whole id, and that is a law.** The VCS
suffix names the commit that built *the compiler*, so it would make `love1` — built by love0's
mooncc — and `love2` — built by love1's — differ at `e_shoff` and name a broken fixpoint where
the two compilers agree on every byte they *emit*. The base moves with a release, which both
generations share; `love0` is stamped `$(love_base)+bootstrap` for exactly this, and
`out/host/0/.love0cc` content-stamps that compile line so a `./VERSION` bump rebuilds it (make
tracks files, not flag strings, and a stale love0 would fail the fixpoint at a byte offset with
nothing to say about the cause). A reader wanting the commit reads `love-version` in `.rodata`.

Read it back without any binutils at all: `lib/irec.l`'s `(irec-secof PATH ".comment")` is the
same ELF walk one table over, and answers the `(1 bytes)` wrapper — an empty section is a real
section. It works on gcc's objects and on every target mooncc emits, cross-machine, for the
reason the IR record does: a section table is a table. Gated by `test_moon`, both halves — the
union over a foreign `.o`, and the exact string on an all-ours link.

## the toolchain root

mooncc's own files — our headers (`crew/moon/include/`, glibc-ABI-faithful but NOT glibc's) and
the runtime sources the implicit link pulls — are found through a two-rung walk, tried in order:

1. **the dev tree**, `crew/moon/` off the cwd;
2. **the installed nest**, `<seat>/../lib/love/moon/` — the loader's own seat walk, the
   `selfpath` nif. So `~/.love/bin/love` finds `~/.love/lib/love/moon/`, and a distro's
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
  env/exec/termios/pty. Single-threaded like love: errno is one int, no locks. See.
* **mksys.l** — lays `sys.o`, the things C cannot say: the 7-slot syscall trampoline,
  `__sigsetjmp`/`siglongjmp` over our own layout inside the glibc-sized 25-long buffer (the
  signal mask in `buf[8]`, saved/restored by rt_sigprocmask — love.c's fault barrier is
  `sigsetjmp(env,1)`, so the mask is load-bearing), and `__ai_sigret` (the SA_RESTORER tail).
  Every encoding objdump-checked, the holo house rule.
* **math/am.c** — our transcendentals. sqrt exact, the seven within a few ulp; `make ulp` is the
  differential gate. `-lm` appears in no link.

**The pull is cached, content-addressed, as one archive.** A member has to be compiled before the
pull can see what it defines, so every link owing a libc nom paid for all 190 of them — ~23s of a
cold hello-world link's ~23s. They now ride `~/.love/cache/moon/<sha>.a`, ONE archive per
(compiler, target), keyed on the target, the runtime tree's whole text (headers included — an
edited `stdio.h` changes what `nolibc.c` means) and the compiler's own identity. A warm link is
~0.15s. An archive and not 190 objects because the ranlib index IS the "what does this member
define" answer, written once and read back rather than recomputed on every warm link — and
because one file is one generation, whole the moment it lands and countable when the sweep asks
which to keep. It is holo's own `arbytes`/`arpull` at both ends, the same pair `kore ar` and a
user-named `.a` on the command line use.

⚠ **The compiler's identity is the image it woke**, by stat, plus the love's own — `love-image`
(doc/snapshot.md), read as `(ev 'love-image)` because mooncc lives baked and a bare read folds.
A love that does not say falls back to every `*.image` beside it, which is what this was before
`love-image` existed: correct but far too eager, since re-baking a `kore.image` the link never
reads invalidated the runtime and cost a full rebuild. Hashing the compiler's `.l` sources
instead looks tighter and is a hole: edit `gen.l`, link once before the image catches up, and the
entry filed under the new sources holds the old image's codegen.
No identity — a love with no image file in reach — means no cache at all. Nor is anything else
owed it: no HOME, an unwritable directory, a mangled entry (each is checked for its archive
magic) all fall back to compiling, silently. Entries land by `rename`, so parallel links cannot
tear one, and a miss sweeps all but the six newest generations. ⚠ **count, not age**: the rate
is the tree's own — a day of rebuilds mints more generations than a month of use does, and a
clock cannot tell the two apart. The `-c` path is not cached, and neither is a `.c` the user
named — this is the *implicit* runtime only.

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
* The first consumer is the kernel's `free/<a>/asmops.h`, which
  carries both spellings behind the `__mooncc__` predefine. Worth reading for how far the two
  dialects agree: a bare mnemonic and a `mnemonic op, op` line are the SAME text in both.
* Deferred until a consumer demands them: an AT&T template front-end, `"f"` float operands, asm
  goto, named `[sym]` operands, top-level asm.

## the installed shape

`make install` does not ship the cat as `bin/mooncc`. The compiler bakes WARM into
`lib/love/mooncc.image` (the live bake nif, doc/snapshot.md) and `bin/mooncc` is a three-line sh
shim: `love wake mooncc.image mooncc "$@"`. The whole-cat
re-eval every compile would otherwise pay (~1.5 s wall) is paid once, at bake — a small-file
compile drops from ~0.77 s to ~0.02 s, gcc-class invocation latency.

What the shim still pays per compile is the wake and the process: ~56 ms each, measured over
eight one-function TUs (0.81 s through the shim, 0.37 s for the same eight through one image —
2.2×). `moon-run` is the door that skips it. It is the same compile as `moon-main` but it
ANSWERS its status as a charm instead of quitting, so one image compiles again after a compile
that failed, and a caller who holds the image pays the wake once for a whole build. `moon-main`
is `(quit (moon-run as))` — every shim above keeps its contract untouched. Objects laid warm
are byte-identical to the same compile run cold, including the ones laid after a failure;
test/gate/moon.sh holds both halves.

⚠ `moon-run` traps `'leave`, the u-floor's exit door (a `udie` anywhere in the compile rides it
out carrying the status), and passes its charm through. Every OTHER condition is a genuine
internal raise and gets the `cc: internal error:` sentence with status 1 — taking `'leave` for
one of those would print nonsense over every usage error and flatten its 2 to a 1.

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
