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
out/host/love wake out/host/mooncc.image mooncc \
  -c -t x64 -o /dev/null q.c
```

---

## the syntax ledger

All of C89 passes. What remains is C99/C11/GNU.

### absent — a parse error, loudly

| construct | probe |
|---|---|
| `_Alignof` | `_Alignof(int)` |
| `_Generic` | `_Generic(x, int: 1, default: 0)` |
| `_Thread_local` | `_Thread_local int e;` — no TLS anywhere, so the refusal is honest |
| statement expressions | `({ … })` |
| computed goto | `&&label`, `goto *p` |
| plain `typeof` | `typeof(x) y;` — ⚠ only `__typeof` / `__typeof__` are recognized |
| `asm goto` | costed below — the one refusal carrying an estimate |
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

The more surprising half, and all of it on every target unless the parity table below says
otherwise: designated initialisers (both `.field =` and `[i] =`), compound literals, K&R
definitions, bitfields including compound assignment, flexible array members, variadic macros,
`long long`, hex floats, anonymous unions, `restrict`, `static inline`, mixed declarations,
`for`-scoped declarations, `_Static_assert` (including `&&`/`||`/`?:` in the constant),
string-literal concatenation, self-referential structs, enum trailing commas, multidimensional
arrays, brace elision in nested initialisers, pointer-to-array declarators, functions returning
function pointers, multi-character constants (`'ab'` is 0x6162, gcc's packing, signed at four
chars), binary literals (`0b1010`, gcc's extension and C23's spelling), `__func__`, and
`__typeof__` over locals, globals, struct members, dereferences and function names.

Three of them carry an edge worth knowing:

- **variable-length arrays** ride x64 and arm64 only; every other target says `no lane for a
  variable-length array on <tgt>`. ⚠ a VLA with an *initializer* refuses everywhere
  (`parse error near =`) — C's own rule, not a gap. `__builtin_alloca` is absent on every
  target, so a VLA is the only dynamic frame allocation here.
- **wide and prefixed literals** desugar to a *bounded compound literal* of the element type
  (`L` → wchar, `u` → char16 with surrogate pairs, `U` → char32, `u8` stays bytes), so globals,
  locals, braces, elision, concatenation across a prefix and `sizeof` all match gcc on every
  target, and a wide *char* constant decodes to its last code point as gcc reads it. ⚠ the
  storage is the compound literal's — automatic inside a function where C says static duration,
  so a pointer kept past the frame dangles, and `wchar_t *p = L"x"` at file scope refuses on the
  static-clit row above. A mixed-prefix concatenation `u"a" U"b"` takes the first prefix where
  gcc refuses, and universal character names `\uXXXX`/`\UXXXXXXXX` stay absent — the escape
  refuses, loudly.
- **`__extension__`** is a no-op at a declaration's head (file scope, block, member, before
  `typedef`) and as a cast-expression prefix, the typedef declarator's trailing attribute run
  skipping alongside — which is what opens `#include <pthread.h>`. gcc-refused spots like
  `int __extension__ x;` still refuse; ⚠ `sizeof(__extension__ T)` is accepted where gcc
  refuses, the one tolerance.

### the directives, and which are ignored on purpose

`#pragma`, `#line`, `#ident`, `#sccs`, `#assert`, `#unassert`, a bare `#` (the null directive,
C11 6.10.7) and gcc `-E`'s `# 42 "f.c"` line marker all pass and do nothing — except
**`#pragma push_macro("X")` / `pop_macro("X")`**, which save and restore the definition
(gcc's semantics: a per-name stack, a saved-undefined pops back to undefined, a pop with
nothing saved is a no-op, and the directive body reads raw so a user macro named `pop_macro`
cannot interfere — cts 00206). `#warning` says its
text and continues. **Everything else refuses** (C11 6.10p1) — the catch-all that used to ignore
an unknown directive let `#cmakedefine X 1` sail through, so an unconfigured template header
compiled clean and the name it owed was simply absent.

⚠ One of those ignores costs a right answer rather than a feature, so "on purpose" is the
cheaper reading of it than the true one: **`#line` never moves the line number** a later
diagnostic or `__LINE__` reports.

⚠ `#include_next` refuses *because* it is unimplemented — ignoring it drops a header in silence,
which is worse. doc/moon-userland.md carries when it becomes load-bearing.

### the predefine surface

The gcc-shaped `<stdint>`/`<limits>`/`<float.h>` family **landed 2026-08-09** — `moon.l`'s
`stddefs`, one table forked once on the word width plus the wchar ABI fork, ~165 rows riding
the driver's `-D` channel (cpp stays target-blind): the `__INTn_TYPE__`/`__UINTn_C`/`*_MAX__`/
`*_WIDTH__` ladders, `__SIZE_TYPE__`/`__PTRDIFF_TYPE__`/`__INTPTR_TYPE__`/`__INTMAX_TYPE__`,
`__WCHAR_TYPE__`/`__WINT_TYPE__`, `__BYTE_ORDER__` and the `__ORDER_*` trio, `__CHAR_BIT__`,
the `__SIZEOF_*__` set, `__LP64__`/`_LP64` (c-testsuite 00212), and the full `__FLT_*`/
`__DBL_*`/`__LDBL_*` trait sets. Every value is gcc's own spelling on that target (verified by
stringize-diff against `gcc -dM -E` on x64/riscv64/arm-none-eabi and clang's aarch64), and
`__LONG_MAX__` moved out of cpp into the fork, so t32 now answers `0x7fffffffL` instead of the
64-bit lie. On top of the older rows: `__STDC__`, `__STDC_HOSTED__`, `__mooncc__`, the linux/
unix spellings, the arch pairs, `__INT_MAX__`, `__FLT_MAX__`/`__DBL_MAX__`,
`__SIZEOF_INT128__` on x64, `bool`/`true`/`false`. Pinned by test/cc/123-predef.c (all four
compilers agree at 21) and the t32 `#if` checker run against arm-none-eabi-gcc.

Three deliberate deviations, all in the compiler's favor of honesty:
- `__CHAR_UNSIGNED__` stays **out** everywhere — gcc's arm/riscv char is unsigned, ours is
  signed on every target, and a predefine describes *this* compiler.
- the `__LDBL_*` rows answer **double's** values — no `long double` here, so a consumer takes
  its double lane, the one we can compile (`__DECIMAL_DIG__` is 17, not x87's 21).
- `__SIZEOF_INT128__` stays **x64-only** where real gcc also defines it on aarch64/riscv64 —
  only gen's x64 lane carries d128, and claiming it elsewhere invites code we refuse.

A user `-D` lands after the table and wins. What remains absent is the exotic tail: the
`__FLT16/32/64/128*` extended-float families, `__CHAR16/32_TYPE__`, decimal floats — nothing
in the userland ladder reads them yet.

Landing the table also made **`__LINE__` true**: the `-D` text used to skew it by its line
count (nothing compensated). `clexat` now stamps the prepended lines `1-k..0` so the TU's own
numbering starts at 1, and moon.l's `deskew` pay-back pass retired with the skew. `#line`
still does not move it (the row above stands).

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
value, so nothing announces them but a differential — which is why they arrive in batches,
each batch behind an outside package or an outside corpus (doc/moon-userland.md, `test_cts`)
rather than behind a test we thought to write.

### from an outside corpus

`test_cts` holds c-testsuite's 220 programs to the output they ship (doc/moon.md). **None
compile clean and answer wrong.** What remains on its roster is refusals, each loud and named.

### the residues the fixed rows left behind

Each of these rode in behind a row that has since landed, and each is still a real divergence
from gcc that nothing announces. None has found a consumer yet — which is why they sit here
rather than in a commit.

- **unary `+` vanishes at parse** (it exists only to promote), so `sizeof(+c)` is 1 where gcc
  says 4; and `sizeof(a = b)` still defers to gen's 8, the assignment wearing its unpromoted
  left type.
- **a bool param past the 6th, or one named in a variadic list**, binds straight to caller
  memory — no store, so no arrival conversion, and a wild caller value reads back raw.
- **a mooncc caller into a gcc-built bool-param callee** hands over the bare int where SysV
  promises 0/1; the entry `cvt` covers mooncc callees only.
- **`(bool)` of a pair/i128 value** tests the low word alone.
- **the VLA lane's runtime `dim * sizeof(elt)`** still multiplies bare (the dim is the runtime
  side), and **`offsetof` still folds signed** where every other `sizeof` wears the unsigned
  coat.

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

And the evaluator is **signed throughout**: C11 says `#if` arithmetic runs in intmax/uintmax
with a `U`-suffixed operand making the operation unsigned, so `#if 1UL - 2 < 0` must be false
(the subtraction wraps to huge) — ours reads the values and answers true. Found writing the
predefine table's t32 checker (`__UINT64_C(1) - 2 < 0`); no real header has tripped it yet.

---

## target asymmetries

Six targets: **x64, arm64, riscv64, thumb2, thumb2sp, thumb1**. The 32-bit ones carry most of
the live gaps, but not all of them — two lanes are x64-only. Everything here is a **loud scare,
never silent**.

| lane | x64 | arm64 | riscv64 | thumb2 | thumb2sp | thumb1 |
|---|:-:|:-:|:-:|:-:|:-:|:-:|
| `__int128` | ✓ | — | — | — | — | — |
| `_Complex` arithmetic | ✓ | — | — | — | — | — |
| variable-length array | ✓ | ✓ | — | — | — | — |
| by-value composite arg, ≤16B, registers free | ✓ | ✓ | ✓ | — | — | — |
| by-value composite arg, MEMORY class | ✓ | — | — | — | — | — |
| composite passed at a variadic call site | ✓ | ✓ | ✓ | — | — | — |
| composite NAMED in a variadic parameter list | ✓ | ✓ | — | — | — | — |
| composite return, 16B all-int | ✓ | ✓ | ✓ | — | — | ✓ |
| composite return, MEMORY class | ✓ | — | — | — | — | ✓ |
| `__builtin_bswap64` | ✓ | ✓ | ✓ | — | — | — |
| `__sync` spin-lock pair | ✓ | ✓ | ✓ | — | — | — |
| signed 64-bit `/` and `%` | ✓ | ✓ | ✓ | — | — | libgcc |
| 64-bit `*` and shifts | ✓ | ✓ | ✓ | ✓ | ✓ | libgcc |
| `double`/`float` arithmetic | ✓ | ✓ | ✓ | ✓ | libgcc | libgcc |

**The table is generated, not maintained: `tools/moon-parity.sh table` prints it and
`tools/moon-parity.sh check` fails if this doc and the compiler have drifted** (`why` prints
each refusal's cause). Regenerate it rather than editing a cell by hand.

⚠ **A ✓ means the lane exists, not that it is differentiated** — the sweep compiles (`-c`) and
reads the object's symbols, and only x64/arm64/riscv64 have running gates behind them. ⚠ several of
these refusals arrive as `cannot compile 'f' (cause unnamed)` rather than a named cause —
`__int128` and every composite-argument row among them. The refusal is real either way; what is
missing is the sentence naming it (doc/moon-diag.md's bare-diagnostic debt).

⚠ **`libgcc` is a cell value, and the two targets wearing it borrow for different reasons.**
thumb1 (v6-M) has no UMULL, no long shifts and no FPU, so 64-bit `*`/shifts/divide, int↔double
conversion and *all* float and double arithmetic lower to `__aeabi_*` calls (`gen.l`'s `v6m?`
lanes); `port/rp2040/Makefile` names a cortex-m0 libgcc.a on the link line and calls it "the one
foreign FILE". thumb2sp borrows for one row only — it is ARMv7E-M with an **SP-only** FPU (the
Playdate's STM32F746), so `float` rides the hardware and `double` softens, where thumb2's
fpv5-d16 does both. A borrow is a LINK-time dependency, invisible to a compile: it shows up as
an undefined `__aeabi_*` in the object, which is how the table finds it. Everywhere else the
lane is ours or there is no lane.

⚠ **The two struct rows do not move together, and thumb1 inverts them.** v6-M returns *any*
struct over 4 bytes through memory (`sretm?`), so thumb1 takes both composite returns while
refusing every composite *argument*; arm64 and riscv64 are the mirror image, taking arguments
and the 16B return but refusing the MEMORY-class return — which is what stops PDCLib's dlmalloc
on the cross targets.

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

⚠ **A by-value composite NAMED in a variadic parameter list rides x64 and arm64** (`vaspill`,
`vaspill-a64`); `vaspill-rv` and `vaspill-t32` refuse the shape, each for its own ABI's reason.
⚠ that is a different shape from *passing* a composite at a variadic call site, which riscv64
also takes — probe the one you mean.

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
- **a MEMORY-class composite RETURN on arm64 and riscv64** — `no lane for returning this
  80-byte struct by value on <tgt>`. Probe: `typedef struct { long a[10]; } R;` with a
  definition that returns one; a bare prototype compiles everywhere.
- **signed 64-bit `/` and `%` on thumb2 and thumb2sp** refuse (`cgfn refuses`) — love.c's lane
  is unsigned; wrap the unsigned expansion in an abs/refix sleeve when needed. thumb1 answers
  it, but through libgcc's `__aeabi_ldivmod`.
- **thumb1 varargs** — the pop-pc epilogue cannot drop the r0-r3 block; `vaspill-t32` refuses
  v6-M whole.
- **thumb1 `leax`** — the indexed-call variant (`a[i]()` over a local array) hits
  `;; lea-range (r0 r4 8)`, the scaled-indexed-address gap.
- **`__builtin_bswap64` and the `__sync` spin-lock pair on t32** — `no lane for <name> on
  <tgt>`; bswap16/32 and clz/ctz ride every target (t32 clz is the CLZ word / `__clzsi2`,
  ctz the isolate-and-clz / `__ctzsi2`), but the 64-bit swap wants the r0:r1 pair lane and
  the atomics want LDREX/STREX plumbing (v6-M has none), and nothing reaches either there yet.

What **thumb2** carries, so it is not re-derived (thumb1 reaches libgcc for most of this — the
⚠ above): 64-bit `long long` as register pairs (lo:hi on r0:r1, r2:r3 the shuttle) with +, -,
×(UMULL/MLA), unsigned `/` and `%` (a self-contained 64-step restoring expansion — no
`__aeabi_uldivmod`, no libgcc), all shifts across the word
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
`psword ps`), but those two copies are still kept in step by hand. The promotion/conversion
law stopped being a twin 2026-08-09: `pprom`/`puac` (parse.l, the typing door) is the one
spelling, `ptype` and gen's `cmpu` both consume it, and the width rides one parameter
(`!(pst32? ps)` / `!(t32? g)`). Any NEW parse-side type computation should go through or
beside the door, not grow a private ladder.

⚠ **Parse folds early on purpose** — an array bound needs the constant at parse time, and gen's
`szof` lane is too late. Deferring a fold to gen is not an available fix.

---

## asm goto — what building it would cost

The one refusal that has been costed rather than just filed. Still **not built**: it is close to
kernel-only, and it is worth doing when something we actually want to compile demands it, and
not before. ⚠ the references below were accurate when written — re-check them at the point of
edit rather than trusting them.

**The allocator is not the problem.** The obvious fear, that a terminator with multiple
successors would break the tuned register allocator, does not apply: `hasasm` already disables
register homing for any function containing asm, the vmap flushes at every label, and `alive`
already answers the whole universe for both `goto` and `asm`. Keep homing off under `hasasm` and
the allocator needs no change at all.

The three real blockers:

- **The raw blob cannot name an outer label.** `cgasm` assembles the body immediately via
  `holo-bytes` with an empty pre-bound label table, so a label not defined inside the template
  hits `(scare 'undef-label ..)`. The hook is clean, though — raw is lowered verbatim by every
  backend (`x64.l`, `arm64.l`, `thumb2.l`, `thumb1.l`), and `chunk-len`/`resolve` already handle
  an inline `('fix w kind label aux)` anywhere in the stream, so a raw carrying an unresolved fix
  would lay out against the **outer** function's label table for free. What is missing is a holo
  door — a variant of `assemble-at` that assembles while leaving a whitelist of external labels
  as fix placeholders instead of scaring. `laylax` would treat such a fix as its widest form.
- **`cfoldir`'s pend merge is the one correctness hazard.** A label with no recorded pending
  state that is linearly live inherits the fall-through state verbatim, so an invisible in-edge —
  a branch out of an opaque raw blob into a C label — makes that join unsound: constants assumed
  at L would not hold on the asm edge. The minimum fix is to collect the asm-goto target labels
  per function into the `backs` table, so they take the existing assume-nothing path. Cheap, and
  it mirrors back-edge handling exactly, which exists for precisely this reason.
- **Surface.** `pasm` hardcodes three colons as `s1`/`s2`/`s3`; a fourth (GotoLabels) needs an
  `s4` and a fifth field on the `('asm ..)` node, which ripples to every positional consumer in
  `gen.l` and to the goldens in `law.l`. `asmsub` must learn `%lN` — currently `'bad` — and
  substitute the *mangled* label `fn.NAME`, sharing the mangling with the label emitter.
  `asm goto` is implicitly volatile and, pre-GCC-14, takes no outputs.

Everything else already refuses or resets on raw: `unframe` bails, `deadcell` dirties, `deaddef`
treats it as a barrier. **The estimate is about a week**, touching parse, one gen pass and one
new holo door — and not the allocator.

⚠ **It does not bring Linux into range on its own.** The kernel additionally wants `__label__`,
computed goto, `_Generic`, and attribute semantics that change codegen.

---


## external corpora

**c-testsuite is wired** — `test_cts`, `test_cts_arm64`, `test_cts_riscv` over
`test/gate/cts.sh` (doc/moon.md). 220 single-file programs held to the output they ship, on all
three targets, ~60 s each, opt-in on `make dl/c-testsuite` and skipping whole without it. Its
first run is where twelve rows of the syntax ledger above and six of the wrong-answer rows came
from. The roster of failures lives in the gate with a cause apiece.

The rest are still recommendations. `test/cc/` holds 131 gcc-differentiated files, so the
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
