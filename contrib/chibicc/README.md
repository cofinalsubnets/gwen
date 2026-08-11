# contrib/chibicc — a patch for upstream, not for us

`declarator-list.patch` fixes a parse error in [chibicc](https://github.com/rui314/chibicc).
Nothing here is built, tested or shipped by this tree; it is an upstream artifact parked
where it will not rot. mooncc's own conformance ledger is doc/moon-c-gaps.md.

## the bug

A declaration whose **first** declarator is a function and which then continues past a
comma is rejected:

```c
struct S;
struct S *f(void), *g(int);    // chibicc: "expected '{'"
int a(void), b;                // likewise
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

```sh
gh repo fork rui314/chibicc --clone --remote      # or fork in the web UI, then clone
cd chibicc
git checkout -b declarator-list
patch -p1 < /path/to/contrib/chibicc/declarator-list.patch
make test && make test-stage2                     # both must print OK
git commit -am "Fix parse error on a declarator list whose first declarator is a function"
git push -u origin declarator-list
gh pr create --repo rui314/chibicc --title "..." --body-file pr.md
```

`pr.md` is the body below.

---

## the PR body, ready to paste

**Title:** `Fix parse error on a declarator list whose first declarator is a function`

````markdown
`chibicc` rejects a declaration whose first declarator is a function and which then
continues past a comma:

```c
struct S;
struct S *f(void), *g(int);
int a(void), b;
int main(void) { return 0; }
```

```
$ chibicc -c t.c
t.c:2: struct S *f(void), *g(int);
                                 ^ expected '{'
```

This is plain `init-declarator-list`, unchanged since C89:

```
declaration:
        declaration-specifiers init-declarator-list_opt ;

init-declarator-list:
        init-declarator
        init-declarator-list , init-declarator
```

and a function declarator is an ordinary `direct-declarator` (C11 6.7.6):

```
direct-declarator:
        direct-declarator ( parameter-type-list )
        direct-declarator ( identifier-list_opt )
```

Nothing restricts a declarator carrying a parameter list to being the sole
init-declarator. The construct that *is* restricted to one declarator is a function
**definition** (C11 6.9.1):

```
function-definition:
        declaration-specifiers declarator declaration-list_opt compound-statement
```

— and that is exactly the distinction being conflated. `parse()` dispatches on
`is_function()`, which inspects only the first declarator, into `function()`; `function()`
parses that declarator and then accepts only `;` or `{`, so a `,` has nowhere to go. The
test is "does this declaration begin with a function declarator", where it needs to be
"is this a function definition".

gcc and clang accept the case above with zero diagnostics at `-pedantic-errors` under
`-std=` c89, c99, c11, c17 and c23. It isn't on the README's non-support list either
(complex numbers, K&R prototypes, GCC inline asm, digraphs/trigraphs), and it predates
C11 entirely, so "almost all mandatory features of C11" reads as covering it.

In fairness it is a rare spelling — I scanned 120 headers under /usr/include and found no
instance, which is presumably why it has gone unnoticed while Git and SQLite build fine.
So this is a conformance gap rather than a practical blocker.

The patch splits the symbol-registration half of `function()` into `declare_function()`,
hands a comma tail to the existing `global_variable()` loop, and has that loop dispatch on
`ty->kind == TY_FUNC` so a function declarator in the list is declared rather than given
storage. That also covers the mixed forms, `int f(void), x;` and `int x, f(void);`.

`make test` and `make test-stage2` both pass. I also checked these against gcc:
`int f(void), x;`, `int x, f(void);`, `extern int f(void), x;`, a `static` list with both
functions defined later, a pointer-returning list of three, a plain definition still
parsing, and a declaration list followed by its definitions.

I know you don't merge PRs here — please fold this into whichever commit it belongs to, or
close it and reimplement it however fits the book. No reply needed.
````
