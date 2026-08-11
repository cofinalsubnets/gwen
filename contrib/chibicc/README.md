# contrib/chibicc — a patch for upstream, not for us

`declarator-list.patch` fixes a parse error in [chibicc](https://github.com/rui314/chibicc).
Nothing here is built, tested or shipped by this tree; it is an upstream artifact parked
where it will not rot. mooncc's own conformance ledger is doc/moon-c-gaps.md.

## the bug

**The first example under C89 3.5.4.3, "Function declarators (including prototypes)", does
not compile.** The standard picked the reproducer:

```c
int f(void), *fip(), (*pfi)();   // chibicc: "expected '{'"
```

Any declaration whose first declarator is a function and which then continues past a comma
is rejected, prototypes or not:

```c
struct S *f(void), *g(int);
int a(void), b;
```

gcc and clang accept both with **zero diagnostics** at `-pedantic-errors` under c89, c99,
c11, c17 and c23 — this is `init-declarator-list` from the original C grammar, not a
newer-standard question. It is also not on chibicc's own "does not support" list (complex
numbers, K&R prototypes, GCC inline asm, digraphs/trigraphs).

Cause: `parse()` dispatches on `is_function()`, which inspects only the first declarator,
into `function()` — and `function()` parses that one declarator and then accepts only `;`
or `{`. A `,` is unhandled.

Fix: split the symbol-registration half of `function()` into `declare_function()`, hand a
comma tail to the existing `global_variable()` loop, and let that loop dispatch on
`ty->kind == TY_FUNC` so a function declarator in the list becomes a prototype rather than
an object needing storage. 26 lines, no new machinery.

## verified

Against chibicc `main` (the tree the AUR `chibicc-git` package builds, 2026-08-11):

* `make test` — OK. `make test-stage2` (self-host) — OK.
* seven edge cases, each agreeing with gcc: `int f(void), x;` · `int x, f(void);` ·
  `extern int f(void), x;` · a `static` list with both defined later · a pointer-returning
  list of three · a plain definition still parsing · a declaration list followed by the
  definitions.

## submitting it

chibicc's README: *"I do not take pull requests in this repo. You can send me a pull
request if you find a bug, but it is very likely that I will read your patch and then
apply that to my previous commits by rewriting history."* So a PR is the documented
channel for a bug, and being closed-then-reimplemented is the expected good outcome — not
a rejection. Send one patch, one bug, and no follow-up.

⚠ keep this tree out of the PR entirely. The bug stands on its own for anyone compiling
ordinary C; the moment a downstream project appears, a bug report reads as a feature
request. No mention of love, mooncc, or what we wanted it for.

**The argument lives in the commit message, not in a PR comment.** GitHub prefills the PR
form from the single commit, so the compare link below already shows it — and since he
folds patches into history by rewriting, the commit message is the part that survives. A
PR body would be discarded. There is nothing to paste.

```sh
gh repo fork rui314/chibicc --clone
cd chibicc && git checkout -b declarator-list
patch -p1 < /path/to/contrib/chibicc/declarator-list.patch
make test && make test-stage2                     # both must print OK
git commit -a -F /path/to/the/message             # the text below
git push -u origin declarator-list
```

Then open <https://github.com/rui314/chibicc/compare/main...cofinalsubnets:chibicc:declarator-list?expand=1>
— the `...` is GitHub's compare separator, not an ellipsis — and press Create pull request.

## the citations, all verified against the text

Not from memory. `N1570` (the C11 committee draft) and `N1256` (C99 TC3) were fetched from
open-std.org and read:

| claim | checked |
|---|---|
| **C89 3.5 carries `init-declarator-list`** — the framing claim | ✅ verbatim |
| **C89 3.7.1 `function-definition` takes one declarator** | ✅ verbatim (specifiers `_opt`; C99 removed implicit int) |
| **C89 3.5.4 `direct-declarator ( parameter-type-list )`** — prototypes are C89 | ✅ verbatim, and 3.5.4.3 is titled "Function declarators (including prototypes)" |
| C11 6.7 ¶1 carries `init-declarator-list` | ✅ verbatim |
| C11 6.7.6 ¶1 `direct-declarator ( parameter-type-list )` | ✅ verbatim |
| C11 6.9.1 ¶1 `function-definition`, one declarator | ✅ verbatim |
| C99 numbers Declarators 6.7.5, and has no 6.7.6 | ✅ (C11 inserted 6.7.5 Alignment specifier) |
| the productions are identical in C99 | ✅ (C11 only adds `static_assert-declaration`) |
| gcc + clang, `-pedantic-errors`, c89–c23, zero diagnostics | ✅ 10 runs |
| the README quotes | ✅ read off the cloned tree |
| no effect on code that already compiled | ✅ 49 TUs, byte-identical .s |
| **stock rejects C89 3.5.4.3's own first example** | ✅ patched accepts, compiles and runs, agrees with gcc |
| clang's c89 complaint is `-Wstrict-prototypes`, not the list | ✅ fires on `int *fip();` alone, no comma present |

The no-effect row is the one worth reproducing: build the tree with and without the patch
and compile every TU of the compiler and `test/` with each. All 49 emit byte-identical
assembly bar two — `parse.c`, which the patch changes, and `test/macro.c`, which carries
`__TIME__` and differs between any two runs of the *same* binary a second apart.

**C89 is what decides this is a bug report and not a feature request.** The construct is in
the first standardized C, so "most of C11" was never the bar. And the reproducer's `(void)`
and `(int)` are *prototypes* — themselves a C89 addition — not the K&R identifier-list
form, so the README's exclusion of "K&R-style function prototypes" does not reach it. That
distinction is the one a skimming reader would otherwise get wrong.

⚠ ISO C90 renumbered ANSI's §3.x to §6.x; the C90 text was not checked, so the message
cites C89 (the ANSI draft, read) and C11 (N1570, read), skipping C90.
