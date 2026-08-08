# moon-c-gaps — the C that mooncc refuses, and the C it gets wrong

A **living ledger** of mooncc's conformance gaps: what C it refuses, what it mishandles, and
where the root cause sits. Rows get deleted as they land — this is a status surface, not a
history.

Everything below was probed against `out/host/mooncc`. The recipes are included — reproduce
rather than trust, and re-verify any `parse.l`/`gen.l` anchor before editing.

Probe recipe (a TU containing only a `_Static_assert` is its own quirk — see below — so add a
trailing declaration):

```sh
printf 'int m(void){ return 0; }\n' >> q.c
out/host/love --wake out/host/mooncc.image -e '(moon-main (cuup (cup cmdline)))' \
  -c -t x64 -o /dev/null q.c
```

---

## the syntax ledger

All of C89 passes. What remains is C99/C11/GNU.

### absent — a parse error, loudly

| construct | probe |
|---|---|
| `_Bool` | `_Bool b;` — ⚠ see "bool is four bytes" below before aliasing it to `bool` |
| `_Alignof` | `_Alignof(int)` |
| `_Generic` | `_Generic(x, int: 1, default: 0)` |
| `_Thread_local` | `_Thread_local int e;` — no TLS anywhere, so the refusal is honest |
| `__extension__` | `__extension__ unsigned long long v;` — glibc's `__atomic_wide_counter` opens with it, so **`#include <pthread.h>` does not parse** (`gcc -E -P` it and mooncc stops at line 197). A no-op keyword: skipping it at declaration, member and expression position is the whole fix, and it would let pdclib's dlmalloc build. |
| statement expressions | `({ … })` |
| computed goto | `&&label`, `goto *p` |
| plain `typeof` | `typeof(x) y;` — ⚠ only `__typeof` / `__typeof__` are recognized |
| `asm goto` | see doc/moon-next.md |
| a block-scope `struct` tag | `{ struct T { int z; }; }` inside a function — tags are file-scoped here, so an inner one collides with the outer |
| a declarator list mixing a function and an object | `int f(int), a;` — two functions in one list is fine |
| a `case` label as a switch's whole body | `switch (x) case 0: ;` — a compound body is fine |
| a `*` bound in an array parameter | `void f(int x[*])` — `[static 3]` and `[const 3]` both pass |
| a function-typed parameter with a non-empty parameter list | `int f(int (int), int)` — `int f(int (), int)` passes |
| `__attribute__((packed))` **before** a union tag | `union __attribute__((packed)) U { … }` — after the body it passes |
| designated RANGE initializers | `[1 ... 5] = 9`, gcc's extension |
| the address of a compound literal in a **static** initializer | `struct S *p = &(struct S){1,2};` — inside a function it passes |
| brace elision continuing **past** an anonymous union member | `{1,2,3,{4,5}}` over `struct { int a,b; union { int c,d; }; struct S1 s; }` — elision *into* the union is fine |
| a `##` paste that makes a macro NAME | `CAT(A,B)(x)` where `AB` is itself a macro — the pasted name is not rescanned as an invocation |
| a `##` paste with an empty operand and trailing tokens | `#define P(A,B) A ## B ; bob` |
| a register-exhausted **SSE**-class by-value argument | five float HFAs — the gp twin landed 2026-08-08 (below), this one did not |

The last twelve are what `test_cts` found (doc/moon.md); `test/gate/cts.sh` names the program
each one came from.

### what passes, for contrast

The more surprising half. Variable-length arrays on **x64 and arm64** (differentiated against
gcc, including a run-time-sized one); every other target says `no lane for a variable-length
array on <tgt>`, and a VLA with an initializer refuses everywhere. `__typeof__` over locals, globals, struct members, dereferences and
function names. Designated initialisers (both `.field =` and `[i] =`), compound literals, K&R
definitions, bitfields including compound assignment, flexible array members, variadic macros,
`long long`, hex floats, wide/prefixed literals (`L`, `u`, `U`, `u8` — they *parse*; ⚠ mooncc
carries no distinct wide type, so the prefix drops and the elements come out as bytes, which is
a wrong answer and not a passing row), anonymous unions, `restrict`, `static inline`, mixed
declarations, `for`-scoped declarations, `_Static_assert` itself (including `&&`/`||`/`?:` in
the constant), string-literal concatenation, self-referential structs, enum trailing commas,
multidimensional arrays, brace elision in nested initialisers, pointer-to-array declarators,
functions returning function pointers. Multi-character constants (`'ab'` is 0x6162, gcc's
packing, signed at four chars), binary literals (`0b1010`, gcc's extension and C23's spelling),
and `__func__`.

### the directives, and which are ignored on purpose

`#pragma`, `#line`, `#ident`, `#sccs`, `#assert`, `#unassert`, a bare `#` (the null directive,
C11 6.10.7) and gcc `-E`'s `# 42 "f.c"` line marker all pass and do nothing. `#warning` says its
text and continues. **Everything else refuses** (C11 6.10p1) — the catch-all that used to ignore
an unknown directive let `#cmakedefine X 1` sail through, so an unconfigured template header
compiled clean and the name it owed was simply absent.

⚠ Two of those ignores cost a right answer rather than a feature, so "on purpose" is the
cheaper reading of them than the true one: **`#line` never moves the line number** a later
diagnostic or `__LINE__` reports, and **`#pragma push_macro` / `pop_macro`** drop the save, so
the macro never comes back and the `#undef` under it is permanent.

⚠ `#include_next` refuses *because* it is unimplemented — ignoring it drops a header in silence,
which is worse. doc/moon-userland.md carries when it becomes load-bearing.

### the predefine surface — the widest remaining hole

mooncc predefines `__STDC__`, `__STDC_HOSTED__`, `__mooncc__`, `__linux__`/`__linux`/`__unix__`/
`__unix`, the arch pair (`__x86_64__` **and** `__x86_64`, `__amd64__`/`__amd64`; `__aarch64__`;
`__arm__`/`__arm`; `__riscv`), `__INT_MAX__`, `__LONG_MAX__`, `__FLT_MAX__`, `__DBL_MAX__`,
`__SIZEOF_INT128__` on x64, and `bool`/`true`/`false`.

It does **not** carry gcc's `<stdint>`-shaped family — `__INT8_TYPE__`, `__UINT64_C`,
`__INT_FAST32_MAX__`, `__SIZE_TYPE__`, `__PTRDIFF_TYPE__`, `__WCHAR_TYPE__`, `__BYTE_ORDER__` —
nor the float traits (`__FLT_MANT_DIG__`, `__DBL_DECIMAL_DIG__`, the `__LDBL_*` set). PDCLib's
platform config is written straight against them and needs **117 `-D`s** handed over from
`gcc -dM -E` before it will configure at all. gnulib and musl read the same names. Landing them
is one table plus a 32-bit fork (`__LONG_MAX__` is already pinned 64-bit unconditionally, which
is wrong on t32 today), and it is the single change that would most widen what configures.
⚠ mooncc has no `long double`, so the `__LDBL_*` row has to answer double's values or a consumer
builds an 80-bit lane the compiler cannot speak.

### the `_Static_assert` quirks

- ⚠ **A translation unit containing ONLY a `_Static_assert` is a parse error**; adding any
  declaration makes it compile. `want` answers the remaining token list, which is `()` when the
  matched token was the last one, so `pstatic` cannot distinguish "consumed the final `;`" from
  "no `;` found". The house-style fix is already written for the same situation in `tdeflist`
  (*"the remainder may be EMPTY (a typedef at EOF)"*): peek for the `;`, then take the tail
  unconditionally.
- ⚠ **A failed static assert reports as `parse error near ;`.** The refusal is correct; the
  wording names the compiler's position rather than the program's fault. See doc/moon-diag.md.
- **`cfold` is deliberately partial** (no floats, no comma, no address constants) and `pstatic`
  **lets a non-constant assertion by**. Making non-foldable an error would convert every
  remaining fold gap into a hard failure across the userland ladder for no gain. Tightening it
  wants its own risk budget — instrument which real-world asserts fall through first.
- Accepted deviation: the ternary folds the **selected arm only** (C11 6.6), so
  `int a[1 ? 4 : x]` is accepted where gcc rejects it as variably-modified. Folding both arms
  is not merely stricter, it is *unsound* here — the divide guard answers "not constant" for
  `b = 0`, so `_Static_assert(1 ? 0 : 1/0, "boom")` would be let by. C11 6.6p3 gives `&&`/`||`
  the same latitude; short-circuiting them closes marginally more of the let-by hole, and is
  worth taking only if a consumer wants it.

---

## accepted, and WRONG — the rows that cost a right answer

A refusal is cheap; these are not. Everything here compiles clean and hands back the wrong
value, so nothing announces them but a differential. Found 2026-08-08 building darkhttpd, dwm,
st, xlander, PDCLib, limine and pdxlander (doc/moon-userland.md), and then six more the same
day when `test_cts` first ran.

### from an outside corpus

`test_cts` holds c-testsuite's 220 programs to the output they ship (doc/moon.md). Eight compile
clean and answer wrong; three of those are rows elsewhere on this page — the predefine surface,
the wide literal, `#pragma push_macro`. These five are their own:

| what | the shape | what it costs |
|---|---|---|
| **a by-value composite argument in a variadic function** | `int f(struct foo f, int n, ...)` with a MEMORY-class `foo` | the parameter reads garbage; 00140 segfaults. Non-variadic is right, so the fault is the incoming-stack offset past the register save area. x64 only: arm64 and riscv64 refuse the shape outright. |
| **an implied array bound counts initializers, not elements** | `PT cases[] = { 1,2,3,4,5,6,7, 8,9,10,11,12,13,14 };` over a 7-member `PT` | `sizeof(cases)/sizeof(*cases)` answers **14**, gcc **2**. The elements themselves are laid correctly, so only the length is wrong — and the length is what every `for` loop over the table reads. |
| **`!` yields a long** | `sizeof(!a)` | 8, where C says the result of `!` is an `int` (4). Same family as the `sizeof` row below: a type lost on the way out of a node. |
| **an unsuffixed constant too big for `long` wraps** | `x != 0xffffffffffffffff` | C says such a decimal/hex constant takes `unsigned long`; we wrap it to −1 and the comparison goes the other way. |
| **an enum bit-field sign-extends** | `enum tree_code code : 8;` where an enumerator has bit 7 set | the value comes back negative and the `switch` takes `default`. An enum whose values are all non-negative must load zero-extended. |

### bool is four bytes

`sizeof(bool)` is **4** where every other Linux C compiler makes `_Bool` **1**, and
`struct { char a; bool b; char c; }` is **12** bytes against gcc's **3**. A mooncc object and a
gcc object sharing a bool-bearing struct disagree on its layout in silence.

⚠ **This is why `_Bool` must keep refusing.** Aliasing it to today's `bool` would turn a loud
parse error into that silent ABI split — one line, and every consumer of `<stdbool.h>` (which
spells `bool` as `_Bool`) starts building the wrong artifact. The rung is making bool one byte:
a narrow store, a nonzero-normalizing load, and the struct layout that follows. Until then a
package wanting `<stdbool.h>` needs a config fork, which is what PDCLib got.

### sizeof answers an int, not a size_t

`sizeof(sizeof(int))` folds to **4**; gcc says 8. `ptype` reads a parse-folded `('num N)` as
`int`, and the `sizeof(TYPE)` lane emits exactly that, so the type is lost. It reaches any
`sizeof` over an already-folded constant — `sizeof(sizeof x)`, `sizeof(offsetof(...))` — and it
means **every `sizeof` expression is signed**, where C says unsigned.

The fix is three lines (fold to `('cast 'ulong ('num N))` in both `sizeof` lanes, the idiom
suffixed literals already arrive under) and it was **measured at +12% of love.o's `.text`**
(460447 → 515584 bytes) — the cast makes the surrounding arithmetic take unsigned lanes, which
is more correct C at a real size cost. Written and reverted 2026-08-08: it is a rung with a
price tag, not a patch. ⚠ measure again before believing the number; it was one build.

### what the %f hunt actually found — and the trap in it

⚠ **`printf("%f", 1.23e12)` answering `9AB0000000000.000000` under a mooncc-built PDCLib is
NOT a miscompile.** PDCLib's `_PDCLIB_print_fp` indexes `_PDCLIB_digits[ buffer[i] ]` over a
buffer that `_PDCLIB_print_fp_deci` filled with *characters*, so it reads ~12 bytes past a
37-byte array — undefined behaviour, in their source, on every compiler. It looks right under
gcc for one reason: gcc aligns that `.rodata` to 16, landing `_PDCLIB_Xdigits` at exactly
`_PDCLIB_digits + 48`, and `Xdigits` opens `"0123456789"` — so `digits['0' + d]` reads
`Xdigits[d]`, the correct character. mooncc aligns to 8, `Xdigits` lands at +40, and every
digit shifts by eight.

The lesson is the comparison, not the bug: **a gcc-built copy of the same library is the
control, and glibc is not**. Diffing against glibc's `printf` says only "these two libraries
disagree". A real defect was under it — the duplicated lvalue of `++*current++`, one character
wide (doc/moon.md, `calm?`) — and only visible once both builds ran the *same* patched source.

Same build, same file family, still open: `strtod("-0.000123e+6")` does not answer -123.0.

### an `f` suffix does not make a float constant

Already filed (a literal keeps 53 bits in an expression). It now has a consumer that turns it
into a wrong ANSWER rather than lost precision: PDCLib spells `INFINITY` as
`(_PDCLIB_FLT_MAX * 2)`, which in mooncc multiplies in **double** to a finite 6.8e38, so
`fmaxf(x, INFINITY) == INFINITY` is false and fdim/fmax/fmin all fail their own suites.

### `#if` bit operations still die on a big

`>>` and `<<` handle a non-negative big now (`ULONG_MAX >> 63 == 1`, the idiom every portable
header uses to ask a type's width, used to read false and take the `#error` arm). **`&`, `|`
and `^` do not** — love's bit ops answer nothing on a big, so `#if (0xffffffffffffffffUL & 0xff)
== 0xff` is false. Same root as the open item in the reader-bootstrap arc.

---

## target asymmetries

The 32-bit targets carry the live gaps. All of them are **loud scares, never silent**.

⚠ **The register-exhausted by-value composite is x64-only, and even there only the gp half.**
A 9..16B aggregate argument with too few *integer* registers left now goes wholly to the
overflow block on x64 (SysV's rule; the param side already bound it there, and the shape is
`xdrawcursor(int,int,Glyph,int,int,Glyph)` in st). ⚠ **The SSE twin still refuses**: five
`struct { float a,b,c; }` by value exhausts xmm0–7 and `cgfn` gives up — the same rule, the
other register file, and c-testsuite's 00204 is the probe.
**arm64, riscv64 and t32 refuse the gp case too** — deliberately, because each has a
*different* rule:
AAPCS64 closes the gp file behind a stack composite (C.13), riscv64 SPLITS one across the
register/stack seam, and t32 has no lane at all. Three rules, three rungs; do not fold them.

- **mixed/int-pair 8..16B composites on t32** — an aone-`int` 5..8B, or a two-eightbyte
  not-both-sse aggregate by value; register-exhausted stack HFAs (9+ double args); and
  doubles/pairs/structs across a t32 VARIADIC seam. love.c reaches none of them.
- **a 16B all-int composite RETURN on t32** refuses on thumb2 and thumb2sp; arm64, riscv64 and
  x64 all take it. ⚠ the probe must DEFINE one, not declare it —
  `typedef struct {int a,b,c,d;} R; static R mk(int x){ R r = {x,x,x,x}; return r; }` plus a
  caller; a bare prototype compiles everywhere. It is what stops the Playdate SDK's own
  header: `LCDMakeRect` returns an `LCDRect` by value, so `pd_api.h` cannot be compiled for the
  device — which is exactly why `port/playdate` routes it through `pdglue.c` on
  arm-none-eabi-gcc and calls that a "word-only seam". AAPCS32 wants the hidden-pointer memory
  return the v6-M lane already implements (`sretm?`); thumb2 has no such lane.
- **signed 64-bit `/` and `%` on t32** refuse (`cgfn refuses`) — love.c's lane is unsigned; wrap
  the unsigned expansion in an abs/refix sleeve when needed.
- **thumb1 varargs** — the pop-pc epilogue cannot drop the r0-r3 block; `vaspill-t32` refuses
  v6-M whole.
- **thumb1 `leax`** — the indexed-call variant (`a[i]()` over a local array) hits
  `;; lea-range (r0 r4 8)`, the scaled-indexed-address gap.

What t32 *does* carry, so it is not re-derived: 64-bit `long long` as register pairs (lo:hi on
r0:r1, r2:r3 the shuttle) with +, -, ×(UMULL/MLA), unsigned `/` and `%` (a self-contained
64-step restoring expansion — no `__aeabi_uldivmod`, no libgcc), all shifts across the word
boundary, every relation (SUBS/SBCS, exact at the 2^53 tie), widen/narrow, `__builtin_clzll`,
pair args (AAPCS32 even-odd pairs, 8-aligned stack slots) and pair returns, pair
globals/locals/members/derefs. VFP doubles on thumb2 (fpv5-d16 scalar, f0..f15 → d0..d15, d15
the reserved converter scratch; VCMP+VMRS for NaN-honest flags), with `am.c` running
BIT-IDENTICAL to the host on the M7. By-value composites + varargs on thumb2 (a {double,double}
HFA rides d-pairs per the AAPCS32-VFP rule; va_list is gcc's one running pointer). `la` on
thumb2 lowers to the MOVW/MOVT absolute pair, and `leax` to `ADD.W Rd,Rn,Rm,LSL#n`.

⚠ **Parse-side and gen-side type twins drift silently.** `tsz`/`talign` (parse) and `(wsize g)`
(gen) once disagreed on pointer width, mislaying every struct containing a pointer on both
32-bit targets with no scare. The target is threaded into the parse state now (`psnew tgt`,
`psword ps`), but the two copies are still kept in step by hand. Any new parse-side type
computation is the same hazard — extending `ptype` grows a second copy of gen's type
propagation, and a divergence there is a silent miscompile.

⚠ **Parse folds early on purpose** — an array bound needs the constant at parse time, and gen's
`szof` lane is too late. Deferring a fold to gen is not an available fix.

---

## external corpora

**c-testsuite is wired** — `test_cts`, `test_cts_arm64`, `test_cts_riscv` over
`test/gate/cts.sh` (doc/moon.md). 220 single-file programs held to the output they ship, on all
three targets, ~60 s each, opt-in on `make dl/c-testsuite` and skipping whole without it. Its
first run is where twelve rows of the syntax ledger above and six of the wrong-answer rows came
from. The roster of failures lives in the gate with a cause apiece.

The rest are still recommendations. `test/cc/` holds 115 gcc-differentiated files, so the
harness exists; this is a corpus question, not an infrastructure one.

- **gcc.c-torture/execute** — ~1500 self-contained self-checking files (`abort()` on failure,
  `return 0` on pass). The de facto bar; tcc, chibicc, cproc and lacc all run it. Ships in the
  gcc source tarball, not installed here.
- **csmith** — random program generation for differential testing against gcc/clang; fits the
  differential-fuzz habit already in holo. Not installed.
- The commercial ANSI/ISO conformance suites (Plum Hall, Perennial ACVS, Solid Sands SuperTest)
  are not realistically obtainable. Noted so nobody goes looking twice.

Nearer real-world targets that exercise this surface without the Linux cliff: busybox, sqlite,
lua, zlib, musl — see doc/moon-userland.md.
