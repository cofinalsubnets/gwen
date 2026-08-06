# moon-c-gaps — the C that mooncc doesn't accept

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
| `_Bool` | `_Bool b;` |
| `_Alignof` | `_Alignof(int)` |
| `_Generic` | `_Generic(x, int: 1, default: 0)` |
| statement expressions | `({ … })` |
| computed goto | `&&label`, `goto *p` |
| plain `typeof` | `typeof(x) y;` — ⚠ only `__typeof` / `__typeof__` are recognized |
| `asm goto` | see doc/moon-next.md |

### what passes, for contrast

The more surprising half. Variable-length arrays on **x64 and arm64** (differentiated against
gcc, including a run-time-sized one); every other target says `no lane for a variable-length
array on <tgt>`, and a VLA with an initializer refuses everywhere. `__typeof__` over locals, globals, struct members, dereferences and
function names. Designated initialisers (both `.field =` and `[i] =`), compound literals, K&R
definitions, bitfields including compound assignment, flexible array members, variadic macros,
`long long`, hex floats, wide/prefixed literals (`L`, `u`, `U`, `u8` — mooncc carries no
distinct wide type, so the prefix drops), anonymous unions, `restrict`, `static inline`, mixed
declarations, `for`-scoped declarations, `_Static_assert` itself (including `&&`/`||`/`?:` in
the constant), string-literal concatenation, self-referential structs, enum trailing commas,
multidimensional arrays, brace elision in nested initialisers, pointer-to-array declarators,
functions returning function pointers.

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

## target asymmetries

The 32-bit targets carry the live gaps. All of them are **loud scares, never silent**.

- **mixed/int-pair 8..16B composites on t32** — an aone-`int` 5..8B, or a two-eightbyte
  not-both-sse aggregate by value; register-exhausted stack HFAs (9+ double args); and
  doubles/pairs/structs across a t32 VARIADIC seam. love.c reaches none of them.
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

## external corpora — recommendation, not wired

`test/cc/` holds 114 gcc-differentiated files, so the harness exists; this is a corpus question,
not an infrastructure one.

- **c-testsuite** — ~220 tiny single-file tests with expected output, purpose-built for small
  compilers. Best first fit, and fast enough for the one-to-two-second ethos.
- **gcc.c-torture/execute** — ~1500 self-contained self-checking files (`abort()` on failure,
  `return 0` on pass). The de facto bar; tcc, chibicc, cproc and lacc all run it. Ships in the
  gcc source tarball, not installed here.
- **csmith** — random program generation for differential testing against gcc/clang; fits the
  differential-fuzz habit already in holo. Not installed.
- The commercial ANSI/ISO conformance suites (Plum Hall, Perennial ACVS, Solid Sands SuperTest)
  are not realistically obtainable. Noted so nobody goes looking twice.

Nearer real-world targets that exercise this surface without the Linux cliff: busybox, sqlite,
lua, zlib, musl — see doc/moon-userland.md.
