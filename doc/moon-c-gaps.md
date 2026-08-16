# moon-c-gaps — the C that mooncc refuses, and the C it gets wrong

A **living ledger** of mooncc's conformance gaps: what C it refuses, what it mishandles, and
where the root cause sits. Rows get deleted as they land — this is a status surface, not a
history.

Everything below was probed against `out/host/mooncc`. The recipes are included — reproduce
rather than trust, and re-verify any `parse.l`/`gen.l` anchor before editing.

Probe recipe:

```sh
printf 'int m(void){ return 0; }\n' >> q.c
out/host/love wake out/host/mooncc.image mooncc \
  -c -t x64 -o /dev/null q.c
```

---

## is it a conforming implementation?

Not yet, and the bar is worth stating exactly, because mooncc has answered
**`__STDC_VERSION__ 201112L` since before any of C11 was in it** — that claim is the thing to
make true or stop making.

C11 §4 asks two things of a *freestanding* implementation: accept every strictly conforming
program, and produce a **diagnostic** for every violation of a syntax rule or constraint. Nine
headers come with it — `<float.h> <iso646.h> <limits.h> <stdalign.h> <stdarg.h> <stdbool.h>
<stddef.h> <stdint.h> <stdnoreturn.h>`. All nine ship as of 2026-08-14 (`iso646.h` and
`stdalign.h` were the two missing). **Hosted** conformance is a different arc entirely — it is
a question about the C library, not the compiler.

Four of the biggest holes are not holes at all once declared, and that is now done: atomics,
threads, complex and VLAs each have a `__STDC_NO_*` macro, and C11 counts an implementation
that says so as conforming without them.

What genuinely stands between here and freestanding C11, each row live above:

- **the `#line "file"` half** — the line half landed 2026-08-14, but the file operand is still
  dropped; it wants `__FILE__` to stop being one name per TU first. (The `#if` evaluator landed
  the same day, below.)
- **block-scope `struct` tags** — the last of the syntax rows, and the one that is not a
  widening: `stag` is one flat table, so scoping it wants a mechanism, not a lane.
- **the diagnostic obligation**: a non-constant `_Static_assert` is let by today, which is a
  constraint violation passing in silence — the one class §4 names outright. The *channel* is
  no longer missing: `blame` (parse.l) files a sentence beside the watermark and `pfail`
  reports it in place of the generic near-token line, so a refusal can name the program's
  fault rather than the compiler's position.

**Landed 2026-08-16** (test/cc/142-syntax.c and 138-ucn.c hold them to gcc; the refusals sit in
test/gate/moon.sh), and the deliberate readings in them:

- **`_Thread_local`** (and gcc's `__thread`) is an ignorable specifier: no TLS, no threads
  (`__STDC_NO_THREADS__`), so a thread-local *is* the one static object — observationally
  right, and C11 gives the storage class no opt-out macro to refuse it under. ⚠ it is the row
  that would read wrong, in silence, the day threads arrive.
- **universal character names in an identifier**, the remaining half of a C99-mandatory row.
  A UCN names a code point, so `Å` and a raw utf-8 `Å` intern as one name and export the
  same symbol bytes gcc does. ⚠ a byte past 127 is now an identifier char everywhere, so a
  stray one outside a literal interns instead of scaring; Annex D's ranges are not enforced,
  which accepts more than C11 spells rather than less.
- **`switch (x) case 0: ;`** — `parm`, the brace-less if/while arms' own door, was already the
  shape a body of one wants.
- **`int f(int), a;`** — `one`/`more` hoisted out of the dispatch's inner scope, so the
  function-first list reaches the object lane mproto cannot take.
- **bare `typeof`**, and an attribute run **before** a struct/union tag.
- **an integer where a pointer is owed** — the §4 row that took `host/main.c`'s `return 1` in
  silence and handed back address 1. `return <non-zero literal>` from a `T *` now refuses and
  says so; a cast still passes, because a cast says the program means it.
- **`_Generic` over QUALIFIED types** (test/cc/143-genericqual.c) — the row below, and the last
  program in c-testsuite that compiled clean and answered wrong.

None of these is large on its own. The honest summary is that conformance here is a **ladder of
small rungs, not a rewrite** — and that the ledger below is the ladder.

---

## the syntax ledger

All of C89 passes. What remains is C99/C11/GNU.

### absent — a parse error, loudly

| construct | probe |
|---|---|
| `_Atomic` | `_Atomic int a;` — both spellings; `__STDC_NO_ATOMICS__` says so, which is C11's own door for the absence |
| statement expressions | `({ … })` |
| computed goto | `&&label`, `goto *p` |
| `asm goto` | costed below — the one refusal carrying an estimate |
| a block-scope `struct` tag | `{ struct T { int z; }; }` inside a function — tags are file-scoped here, so an inner one collides with the outer |
| designated RANGE initializers | `[1 ... 5] = 9`, gcc's extension |
| the address of a compound literal in a **static** initializer | `struct S *p = &(struct S){1,2};` — inside a function it passes |
| brace elision continuing **past** an anonymous union member | `{1,2,3,{4,5}}` over `struct { int a,b; union { int c,d; }; struct S1 s; }` — elision *into* the union is fine |
| a `##` paste that makes a macro NAME | `CAT(A,B)(x)` where `AB` is itself a macro — the pasted name is not rescanned as an invocation |
| a `##` paste with an empty operand and trailing tokens | `#define P(A,B) A ## B ; bob` |
| a register-exhausted **SSE**-class by-value argument | five float HFAs — the gp twin landed 2026-08-08 (below), this one did not |

The last seven are what `test_cts` found (doc/moon.md); `test/gate/cts.sh` names the program
each one came from.

### what passes, for contrast

The more surprising half, and all of it on every target unless the parity table below says
otherwise: designated initialisers (both `.field =` and `[i] =`), compound literals, K&R
definitions, bitfields including compound assignment, flexible array members, variadic macros,
`long long`, hex floats, anonymous unions, `restrict`, `static inline`, mixed declarations,
`for`-scoped declarations, `_Static_assert` (including `&&`/`||`/`?:` in the constant),
`_Generic` and `_Alignof` (landed 2026-08-14, below),
string-literal concatenation, self-referential structs, enum trailing commas, multidimensional
arrays, brace elision in nested initialisers, pointer-to-array declarators, functions returning
function pointers, multi-character constants (`'ab'` is 0x6162, gcc's packing, signed at four
chars), binary literals (`0b1010`, gcc's extension and C23's spelling), `__func__`, and
`__typeof__` over locals, globals, struct members, dereferences and function names.

**`_Generic` and `_Alignof` landed 2026-08-14** (test/cc/136-c11.c, held to gcc). `_Generic`
picks on the controlling expression's lvalue-converted type (`pdecay`) and lowers to the
selected arm alone, so no other arm reaches gen — a call to an undefined function in one links
clean. `_Alignof` answers `talign`, the door `playout` lays members with, so the operator
cannot drift from the layout it describes; gcc's `__alignof__` rides the same lane and keeps
its expression operand. Association matching is structural over the type, and as of
2026-08-16 that type carries **qualifiers** — the row below.

Four of them carry an edge worth knowing:

- **`_Alignas`** is honored at **file scope only**, on the one door gcc's
  `__attribute__((aligned(N)))` already used (`alignat?` → `ps 'aligns` → `cgdata`); both the
  constant and the type-name operand (`_Alignas(double)`) work, and the `.o`'s section header
  asks the linker for the same boundary. ⚠ on a **local or a struct member it is still
  skipped in silence** — the row below.

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
  gcc refuses.
- **`__extension__`** is a no-op at a declaration's head (file scope, block, member, before
  `typedef`) and as a cast-expression prefix, the typedef declarator's trailing attribute run
  skipping alongside — which is what opens `#include <pthread.h>`. gcc-refused spots like
  `int __extension__ x;` still refuse; ⚠ `sizeof(__extension__ T)` is accepted where gcc
  refuses, the one tolerance.
- **universal character names landed 2026-08-14** in every literal face
  (test/cc/138-ucn.c). ⚠ a UCN names a CODE POINT, not a byte, and that is the whole
  trap: `"\u00E4"` in a **narrow** string is the two utf-8 bytes `C3 A4`, where
  `"\xE4"` is the one byte `E4` — so `escseq` reports whether the escape was a UCN
  and the narrow lane encodes on that. Exactly 4 (or 8) hex digits: a short run refuses
  rather than taking what it found, matching gcc's *incomplete universal character name*.
  C11 6.4.3p2's **validity rule** is enforced: a UCN may not name a basic-set character
  (under `00A0`, bar `$ @ ` `), a surrogate, or anything past the last code point — so
  `\u0041` for `A` refuses. ⚠ that rule was found by the **cross** gcc (13.2), which
  refuses it where the newer host gcc takes C23's relaxation and says nothing: a
  single-oracle check would have shipped the hole. ⚠ we also refuse past-`10FFFF` where
  gcc only warns — a refusal, so it costs no right answer.
  ⚠ an identifier spelled with one still refuses — the row above.

### the directives, and which are ignored on purpose

`#pragma`, `#ident`, `#sccs`, `#assert`, `#unassert`, a bare `#` (the null directive,
C11 6.10.7) and gcc `-E`'s `# 42 "f.c"` line marker all pass and do nothing — except
**`#pragma push_macro("X")` / `pop_macro("X")`**, which save and restore the definition
(gcc's semantics: a per-name stack, a saved-undefined pops back to undefined, a pop with
nothing saved is a no-op, and the directive body reads raw so a user macro named `pop_macro`
cannot interfere — cts 00206). `#warning` says its
text and continues. **Everything else refuses** (C11 6.10p1) — the catch-all that used to ignore
an unknown directive let `#cmakedefine X 1` sail through, so an unconfigured template header
compiled clean and the name it owed was simply absent.

**`#line` MOVES the line number** as of 2026-08-14 — `__LINE__` and every later diagnostic
report the mapped line, matching gcc (test/cc/137-line.c). The delta rides `macs`, the one
state already threaded through every arm of `cppgo`, so no signature moved; it is applied
where active tokens accumulate, and again on a directive's own body, which is what makes
`#if __LINE__` right. `doinc` saves and restores it, so a header's `#line` does not follow the
return. ⚠ the **file operand is parsed and dropped**: `#line 700 "generated.y"` reports line
700 of the *real* path, where gcc says `generated.y`. `__FILE__` is the TU's name throughout
(cpp shares one macro table across includes), so the file half wants that lifted first.

⚠ `#include_next` refuses *because* it is unimplemented — ignoring it drops a header in silence,
which is worse. carries when it becomes load-bearing.

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

**C11's conditional-feature macros landed 2026-08-14** (`featdefs`, moon.l; the gate sweeps all
six targets). Saying an absence out loud is what makes it *conforming* rather than a hole, and
it lets a portable source take its other lane instead of hitting a parse error:
`__STDC_NO_ATOMICS__` and `__STDC_NO_THREADS__` everywhere, `__STDC_NO_COMPLEX__` off x64,
`__STDC_NO_VLA__` off x64/arm64 — each row tracking the parity table below, because claiming an
absence we do not have sends a consumer down a fallback for nothing. `__STDC_UTF_16__` and
`__STDC_UTF_32__` are the positive twins: `u""` is UTF-16 and `U""` UTF-32, which is exactly
what those two assert.

A user `-D` lands after the table and wins. What remains absent is the exotic tail: the
`__FLT16/32/64/128*` extended-float families, `__CHAR16/32_TYPE__`, decimal floats — nothing
in the userland ladder reads them yet.

Landing the table also made **`__LINE__` true**: the `-D` text used to skew it by its line
count (nothing compensated). `clexat` now stamps the prepended lines `1-k..0` so the TU's own
numbering starts at 1, and moon.l's `deskew` pay-back pass retired with the skew. `#line`
moves it too now (the directive section above).

### `_Generic` over qualified types — landed 2026-08-16

Two C rules pull opposite ways here, and mooncc had neither: the controlling expression is
**lvalue-converted** (DR 481), so its *top-level* const or volatile is gone before any row is
tried, while everything *inside* a pointer survives and compatibility is exact. So `const int x`
picks `int:`, and `const char *` and `char *` are two different rows. Ours dropped every
qualifier, matched the first structurally equal row, and answered whichever one was written
first — c-testsuite 00219 (`const int * const` taking the `int *` row where C takes neither and
falls to `default`) is off the roster with this.

⚠ **the type language still carries no qualifier**, and that is the point: a node for one would
reach all 56 of gen's ptr dispatch sites. The one consumer that must tell `const char *` from
`char *` keeps its **own marked copy** — `('cq mask t)` over the leaf, mask 1=const 2=volatile —
parked in `ps 'qtys` beside `'locals` and shadowed with it, staged by each declarator parse
(`ps 'qstage`) and consumed by the bind that follows. Nothing outside `_Generic` reads a marked
type, and a stage is taken only when it strips back to the type actually bound, so a mark cannot
outlive the declaration that described it. The sources of a mark are the specifier run
(`qrun`), a typedef's own (`ps 'qtdef`), a struct member's (`ps 'qmem`, which `playout` has no
slot for), and — for a cast, whose `('cast ty ..)` node keeps the bare type gen reads — a
re-read of the type-name off the tokens (`qctl`).

It costs **+0.09% of the instructions** compiling core/love.c (perf, 130.348G vs 130.231G,
stable to five figures across runs), and the `.o` is byte-identical. Two things buy that back and both are load-bearing: nothing is staged for
an unqualified declaration (the common path never touches a table), and `qrun` walks the
specifier run rather than taking a token span — a span by `tally` is O(the rest of the stream),
which would have been quadratic over a TU.

Held to gcc by test/cc/143-genericqual.c: 31 checks over locals, params, block scope, globals,
typedefs, members, array decay, `&` and `*`, and const told apart from volatile.

Two deliberate readings:

- a type-name with a **top-level** qualifier (`int * const:`) is parsed, kept, and matched
  against nothing — no lvalue-converted controlling type can be compatible with it. gcc accepts
  the row and never selects it; clang warns. We agree on the answer and say nothing.
- ⚠ only the **specifier run's** qualifier is seen. A mid-declarator one — `char * const *p`,
  where the const sits on the inner pointer — reads unqualified, so it matches *more* than C
  does, never less. `typedef char *cp; const cp x;` is read right (a const *pointer*, so the
  mark does not reach the leaf); `typedef char *cp; const cp *y;` is the shape that would not be.

⚠ one path binds a name without staging for it: a **K&R** parameter list, whose types arrive as
separate declarations. A prototype's staged mark for the same name would still be sitting there,
and it is taken if it strips back to the same type — so `int f(const char *buf);` followed by a
K&R `f(buf) char *buf;` would read `buf` as qualified. The guard makes it need a shape match as
well as a name match; nothing in the tree or the corpus reaches it.

### the `_Static_assert` quirks

- ⚠ **A failed static assert reports as `parse error near ;`.** The refusal is correct; the
  wording names the compiler's position rather than the program's fault. See.
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
each batch behind an outside package or an outside corpus
rather than behind a test we thought to write.

### an enum constant declared in a BLOCK escapes it

`enum { N = 4 };` inside a function body pins into the one flat `'enums` table and is never
taken back off at block exit, so every later function in the TU can read `N`. C11 6.2.1p7 gives
it the enclosing block's scope. Filed, not fixed: the tree has lived on the flat table since the
beginning and `shadow1` already hides a leaked constant behind any local of the same spelling, so
what is left is a name that resolves where it should not — visible, not yet a wrong answer.

⚠ it stopped being harmless once. `lvm_outer`'s `uintptr_t M = .., N = .., n = M * N, ..` read the
leaked `N` from a *different function's* `enum { N = 4 }` and compiled `n = M * 4`, so the outer
product wrote 4·M of its M·N cells and handed back a tray whose tail was uninitialized heap —
correct on the first read and garbage after the next allocation. **That was the declarator-scope
bug below, and fixing it took the wrong answer away; this row is the other half still standing.**
The two together are the lesson: a leak and a scope-point bug are each survivable, and their
product is a silent miscompile. `test/cc/146-declscope.c` holds both halves.

### a declarator was not in scope for the initializers after it — FIXED 2026-08-16

C11 6.2.1p7: a declarator's scope begins at the **end of its declarator**, so
`unsigned long M = f(), N = g(), n = M * N;` must read the local `N`. mooncc bound the
declaration's names only after the whole declaration was parsed (the block loop's `shadowdecl`),
so an earlier declarator was invisible to a later initializer and any file-scope enum constant or
typedef of the same spelling won. Now `'declaring` marks each name as its declarator finishes and
the primary rule declines to fold it; the block's `shadowdecl` still owns the durable hiding and
the restore. ⚠ found by a DIFFERENTIAL BETWEEN OUR OWN TWO BINARIES — `love` is mooncc-built and
`love0` is gcc-built, and running the same array battery under both named the one function that
differed. That instrument costs nothing and nobody had pointed it at the tray ops.

### from an outside corpus

`test_cts` holds c-testsuite's 220 programs to the output they ship (doc/moon.md). Its roster is
**refusals only** as of 2026-08-16, each loud and named — no program in the corpus compiles clean
and answers wrong on any of the three targets. 00219 was the last one (`_Generic` over a
qualifier, above); its `roster_wrong` list stays in the gate, empty, because the day one comes
back it belongs there and `wrong` is the kind that must stay loud.

⚠ **A rostered line is a claim that goes stale in silence.** Four of them (`#if ||`'s dead arm,
`int x[const *]`, a function-typed parameter, `_Generic`) had been fixed by earlier rungs and
still sat on the roster, and 00219's line said *refuses* where the truth was *answers wrong* —
which is how a live miscompile hid behind a gate that only runs on an opt-in corpus. Re-read the
roster when the corpus is in hand, not only when a gate goes red.

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

### an alignment ask on a LOCAL or a MEMBER is dropped in silence

`_Alignas(64) char buf[8];` inside a function, and `__attribute__((aligned(N)))` on a local or
a struct member, compile clean and align nothing — `alignat?` runs from `ptop` only, so it
never sees a block-scope or member declaration, and `pquals` balance-skips the tokens on the
way past. The classic use is the one that breaks: a 16-byte-aligned buffer for an SSE load.

Costing the fix: the frame side is small — `nslot` is the one cell allocator and the offsets
are its own arithmetic, so an aligned variant is a `aup` on the running high-water, and x64/
AAPCS64 hand every frame a 16-aligned base, which covers every ask up to 16. What is not small
is **threading the ask from parse to that allocator**: the align would ride the `('decl ..)`
entry, and every positional consumer of a decl entry in `gen.l` moves with it — the same shape
of cost `asm goto`'s surface row carries. Past 16 the frame must be realigned at run time, and
that should refuse rather than land wrong.

⚠ Until it lands the tree cannot use either spelling on a local, and neither can a header it
compiles. A struct **member** is a second rung: `playout` computes a member's alignment from
its type alone, and an over-aligned member also moves the tag's own alignment (`asalign`'s
16+-guard, gen.l, is written for exactly that day).

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

### an `l` suffix — landed 2026-08-16, the `f`-suffix row's twin

C11 6.4.4.1 makes an `l`/`L`-suffixed constant a **long**; ours typed it by magnitude alone, so
on the 64-bit targets `sizeof(1L)` answered 4 and `i + 2L` computed in **int** — a silent
narrowing of exactly the expression written to avoid one. `ukind` now answers a distinct `'lnum`
and pprim lowers it to `('cast long ..)`; `1LL` off t32 took the same lane, where it had been
falling through to a plain int as well. c-testsuite 00219 is what named it, two lines apart.

### an `f` suffix, and A CAST TO float — both landed 2026-08-14

Two bugs wearing one symptom, and the second was the real one. C11 6.4.4.2 makes an
`f`-suffixed constant a **float**; ours kept 53 bits, so `sizeof(1.5f)` was 8 and
`0.1f == 0.1` was **true**. The lexer now answers a distinct `'flof` kind (the suffix was
being skipped and thrown away) and parse lowers it to `('cast float ..)`.

That fixed the *type* and not the *value*, which exposed the one underneath:

⚠ **a cast to `float` never rounded.** gen keeps every float as a double in a register and
narrows only at a **store** (`fstf`), so the cast lane's `(flo? tgt)` arm passed the value
straight through — `(float)d == d` read true for an ordinary double **variable**, not just
for a literal. The cast now round-trips `cvtsd2ss`/`cvtss2sd`, which is where the rounding
becomes observable; both ops were already in the vocabulary and all six targets take it.

Held by test/cc/140-fsuffix.c. The old note here said the consumer was PDCLib's `INFINITY`
spelled `(_PDCLIB_FLT_MAX * 2)` — ⚠ that reading was wrong twice over: PDCLib is not this
tree's libc (`crew/moon/lib/nolibc/` is), and we do not define `INFINITY` at all. The real
consumer is every `float` expression in the tree.

### the `#if` evaluator — LANDED 2026-08-14, and one of its three bugs cost right answers

`#if` arithmetic is intmax_t/uintmax_t (C11 6.10.1), so a value is a 64-bit **bit pattern plus
a signedness** — `(v u)`, v in `[0,2^64)`. Love's integers are exact and unbounded, which is
why none of this fell out for free. Three separate wrongs lived here, and the first is the one
worth remembering:

- ⚠ **truth was `0 <`, where C is `!= 0`** — so `#if -1` read **false**, and so did
  `#if -1 && 1`, `#if -1 ? 1 : 0`, while `#if !(-1)` read true. Any header branching on a
  negative constant took the wrong arm in silence. This was not in the ledger; the signedness
  row is what led to it.
- **no signedness at all**, so `1UL - 2` answered -1 where C wraps it to a huge unsigned, and
  `-1 < 1U` read true where C reads false.
- **`&`, `|`, `^` answered nothing on a big.** love's bit ops stop at the fixnum and every
  pattern past 2^62 is a big, so `#if (0xffffffffffffffffUL & 0xff) == 0xff` was false.
  `cbit` splits into 32-bit limbs, operates, and recombines — arithmetic, which bigs do take.
  `<<`/`>>` had already routed around the same hole through multiply and divide.

Held to gcc by test/cc/139-ifexpr.c, seventeen conditions across truth, signedness, the
conversions, truncating division, arithmetic shift and the bitwise trio. ⚠ the one place gcc
still says more: it *warns* on signed overflow in a `#if` (`0x7fffffffffffffff + 1`); we wrap
silently and agree on the value.

⚠ **The constants live at the HEAD of cpp.l's top-level `:` and must stay there.** love0's
compiler is single-pass and folds a pure global at each definition's own compile, so one of
them bound mid-list reads as `;; missing m64` — and only in the **mooncc0** bake, which is
love0's lane. The default love takes it either way, so the edit looks clean and the build
fails two targets later.

---

## accepted in SILENCE — the constraints that get no diagnostic

C11 §4 asks for a diagnostic on every syntax-rule or constraint violation, and these get none.
They are worse than the refusals above and quieter than the wrong answers: the program is
ill-formed, we say nothing, and the damage lands somewhere else entirely. The row below, and
the int-for-pointer one that landed 2026-08-16, were both found by a **foreign cc compiling the
same file** — the only instrument that has ever caught one, and the reason a lane that builds
love with clang is worth keeping.

### a `musttail` into an incompatible prototype

An `__attribute__((musttail))` call must match its **caller's** prototype — same return type,
same parameter list. Our sibcall pass asks only whether a jump is *emittable*, so a caller
carrying an extra argument will happily jump into a callee without one:

```c
// a 5-arg caller into a 4-arg callee: mooncc emits the jump, clang refuses the compile
static struct ai *f(struct ai *g, union u *Ip, ai_word *Hp, ai_word *Sp, int extra) {
  ai_musttail return callee(g, Ip, Hp, Sp); }
```

clang: `cannot perform a tail call to function 'callee' because its signature is incompatible
with the calling function`. This is the one gap that undermines an invariant rather than a
value — `ai_musttail` is *owed*, and the whole discipline rests on a shape that cannot jump
REFUSING at compile (love.h). A check that passes the incompatible case means mooncc alone
cannot police it. Measured 2026-08-15: 58 converted `ghelp` tails, mooncc took all 58, clang
named the 5 that were extra-arg lvms.

⚠ **`make vmret` does not cover this.** It reads the shipped binary, which mooncc builds — so
it sounds mooncc's own output against mooncc's own rule. `test_front` is currently the only
gate compiling clang at `ai_tco=1`, which is what caught it.

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

**The table is generated, not maintained: `mk/tools/moon-parity.sh table` prints it and
`mk/tools/moon-parity.sh check` fails if this doc and the compiler have drifted** (`why` prints
each refusal's cause). Regenerate it rather than editing a cell by hand.

⚠ **A ✓ means the lane exists, not that it is differentiated** — the sweep compiles (`-c`) and
reads the object's symbols, and only x64/arm64/riscv64 have running gates behind them. ⚠ several of
these refusals arrive as `cannot compile 'f' (cause unnamed)` rather than a named cause —
`__int128` and every composite-argument row among them. The refusal is real either way; what is
missing is the sentence naming it.

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
lua, zlib, musl — see.
