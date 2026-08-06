# precedence — right-to-left as far as it goes, then let grip decide

How opfix decides grouping. It all lives in [`love/prel.l`](../love/prel.l)'s reader-operators
block; there is no C, so both compilers inherit it. [`test/precedence.l`](../test/precedence.l)
gates the tree, the value, short-circuit and idempotence.

## the base is flat and right-leaning

Without grips, every dyadic operator nests right-associatively at one uniform level:

```
(2 * 3 + 4)   ; would be 2*(3+4)
(10 - 2 - 3)  ; would be 10-(2-3)
```

That is the APL read, and it is *why* spec.l can write asserts infix — `(3 = 1 + 2)` folds to
`(= 3 (+ 1 2))`. But a hand trained on schoolbook math expects `2 * 3 + 4` to be 10.

## precedence is a refinement of right-associativity

Grouping and evaluation order are independent axes. Precedence and associativity decide only the
*parse tree*; runtime order is a separate matter and, in a pure core, unobservable except for
effects and which subexpression scares first (`:` stays the explicit source-order sequencer
regardless). So none of this touches evaluation — only how opfix factors the tree.

Within that parse question, right-to-left stays the default and grip intervenes *only* to pull a
tighter operator in. The whole thing is one comparison at the steal-point:

> an incoming operator steals a pending filled frame's operand **iff its grip beats the pending
> frame's grip** — at equal grip, the incomer's **hand** decides.

- **steal** — nest the incoming operator to the right.
- **don't steal** — the pending frame folds first, and the incoming (looser) operator takes the
  folded result as its left operand. This is the precedence climb.

| input | grips | result |
|---|---|---|
| `a + b + c` | equal, left-handed | fold first → `(+ (+ a b) c)` |
| `a >< b >< c` | equal, right-handed | steal → `(>< a (>< b c))` |
| `a + b * c` | `*` > `+` | `*` steals `b` → `(+ a (* b c))` |
| `a * b + c` | `+` < `*` | `+` forces `*` to fold → `(+ (* a b) c)` |

**The conservative-extension property:** when every operator shares one grip and one hand,
nothing ever folds early and the behaviour is exactly flat right-associative. Flat-right is the
degenerate case, so any file that touches only equal-grip right-handed operators is
byte-unchanged. That is the property spec.l's infix-assert law leans on.

**The hand** is associativity, and it composes with grip in one predicate:
`(? h (<= g (op-frgrip f)) (< g (op-frgrip f)))` — a RIGHT-handed incomer steals at equal grip
(its band folds right), a LEFT-handed one yields, so the pending frame folds first and its band
folds left. Arithmetic (`* / %` at 60, `+ -` at 50) is left-handed; every other band, and every
coined operator at house grip, is right.

## the table

A row is `arity`, a `grip` (higher binds tighter) and a `hand` (0 right, 1 left). `op-ent`
normalizes all five written shapes to the quad `(name arity grip . hand)`, and it is the one
place that reads the table shape, so the rest of the walk sees a uniform quad. Arity one takes
the next datum, never a left operand.

**Undeclared is infix at two, grip 95, right-handed** — above every row, so the table holds only
exceptions:

```
;   (house)        grip 95   coined operators — fresh punct, no row
;   **             grip 70   flip-apply (a ** b = (b a)), tightest DECLARED infix
;   * / %          grip 60   multiplicative, LEFT
;   + -            grip 50   additive, LEFT
;   = != < <= > >= grip 40   comparison   <- the assert-relation band
;   | & && ||      grip 30   logical
;   ><             grip 25   cons — the loosest builder
;   <- ->          grip 20   assignment aliases (pin / peep, arity 3)
;   ?              grip 10   cond (arity 3)
;   $              grip 5    weak apply — a $ b = (a b), the haskell $, loosest
```

Gaps are left so a level can slot in later.

`**` and `$` are the two **apply** operators, both self-named (the reader emits `(** a b)` /
`($ a b)`, backed by the prel globals `(: (** a b) (b a))` and `(: $ 1)` — `$` *is* the identity,
so `($ a b) = (a b)`). They bracket the range: `**` flip-applies at the tightest grip (a
pipe-like reverse apply that binds before arithmetic), `$` weak-applies at the loosest —
`f $ a + b` is `f (a + b)`, and `f $ g $ x` folds right to `f (g x)`, exactly haskell's `$`. The
glued monadic `$x` is untouched: it factors through the `monadics` table to `saturate`, a
separate valence the spaced dyadic never sees.

House sits **above every band**, as Haskell's undeclared-is-`infixl 9`. No source in the tree
rides house — opfixing every top-level form of every tracked `.l` at 27, 35 and 95 differs only
in prel.l's own literals — and lux, the one production `fixity` user, pins both its grips by
number.

`&&` and `||` are **already short-circuit macros** riding `?`/`:`, and a fresh punct symbol is
already infix-at-two, so their rows do not *add* infix — they **pin the grip below comparison**
so `(0 < x && x < 10)` groups `(&& (< 0 x) (< x 10))` rather than the flat right-fold's
`(< 0 (&& x (< x 10)))`. Infix + macro + short-circuit compose for free because **opfix and macro
expansion are separate passes in the right order**: opfix runs first and is purely structural,
factoring `a && b` → `(&& a b)` with `&&` an opaque arity-2 operator; *then* wev expands the macro
into the short-circuiting `?`. A variadic macro under binary infix is fine — `a && b && c`
factors to `(&& a (&& b c))` and the macro expands outer-then-inner to `(? a (? b c ()) ())`. A
leading operator with no left operand falls through op-steal to the plain-symbol case, so prefix
`(&& ...)` is untouched.

## the frame carries its grip

The op-fr frame is `(orig chain name need grip . got)`. ⚠ **Store grip on the frame — do not
re-probe the table.** The frame keeps two symbols and neither alone recovers grip:

- `op-fro`, the *source* symbol: a composite run — one whose leading factors are arity-one rows —
  has no row of its own, so `op-ent` gives the house grip while the operative grip is the last
  factor's.
- `op-frn`, the *resolved* name: an alias like `<-` resolves to `pin`, but the table is keyed by
  source `<-`, so probing `pin` misses.

The one value carrying the right grip in *both* cases is `en`, the last-factor entry already in
scope at the build site: for a composite it is `=`'s entry, for an alias it is `<-`'s. So grip is
captured from `en`. This is a correctness point, not a performance tradeoff.

A frame is only ever rebuilt with a new need + got; orig, chain, name and grip are fixed. The two
prefix builds take house, inert since prefix frames fold on fill and never sit filled to be
stolen from.

## op-steal is a climb

```
op-steal(g, h, out, pend):
  filled top frame F:
     steals  -> take F's last operand, re-arm to need 1
     else    -> (out',pend') = op-del(op-fold F, out, cup pend)
                op-steal(g, h, out', pend')          ; fold, retry beneath
  collecting frame -> 0
  empty pend       -> top-level last datum
```

When the incoming grip is lower, op-steal folds the pending frame and retries, possibly several
times — a stack of tighter frames all completing before the looser operator lands. Each fold is
the existing `op-del (op-fold F)` cascade, so a looser frame beneath receives the folded value as
an operand and sits filled, and the loop's next turn re-checks its grip. A small loop, not
separate folding machinery.

`op-del`'s defer-vs-fold decision — the line that makes a filled infix frame *sit* rather than
fold — is what gives the right-associative default; the hand is handled entirely in the steal
predicate.

## `fixity` — the one door onto the table

`operators` is mopped at birth, so `fixity` (whose closure captured the table) is the only
sanctioned write into it afterwards. `(fixity nm v)` pins nm's row and answers the row it
replaced *in the shape it takes*, so `(fixity nm (fixity nm new))` restores exactly, `()` both
clears a row and reports an absent one, and a non-`()` answer says someone declared that operator
before you.

`op-ent` itself is the acceptor: a shape it refuses is rolled back and scared rather than pinned,
because ⚠ **a bad row does not error — it silently demotes its operator to the fresh-punct
default**, which on a core operator is a poisoned compiler with no message.

`'(2 60 1)` is `infixl 7`, `'(2 60)` is `infixr 7`, a bare `2` is the house grip, and the nom-led
forms alias.

**It is global, deliberately (revisable).** An operator's grip is part of its meaning, so a module
that coins `<+>` wants its consumers to read `(a <+> b * c)` the way it does — Haskell exports
fixity for the same reason. The save/restore pair covers a scope-local grammar, and wiring it to
`enter`/`leave` stays available without changing this API.

## the bootstrap constraint

The reader-operators block is compiled by **c0** (the C bootstrap) before opfix exists, so the
grip machinery must stay operator-free and use only what is defined above it in the prel
(`foldl`, `L`, `link`, `?`, kind tests). No `!`/`+` sigils inside these definitions. Both
compilers call `book['opfix]`, so a change lands in one place and both inherit it — no C edit, no
second source of truth.

## what the gate holds

- The canonical cases and mixed chains, asserting the *tree* (via `show`/`op-core` on quoted
  forms) and the *value*.
- `&&`/`||` infix: `a < b && c < d` groups `(&& (< a b) (< c d))`; a short-circuit that must not
  evaluate its right arm (`(|| 1 (some-scare))` reads the left without firing the scare).
- `><` against the house default and the bands: `a + b >< c` groups `(>< (+ a b) c)`; a coined
  operator against `><` gives `(>< (~ a b) c)`.
- The hand: a same-grip arithmetic chain folds LEFT, a same-grip `><`/`$`/coined chain folds
  RIGHT.
- Idempotence: `(op-core (op-core form)) = (op-core form)`. op-core is idempotent because
  factored output carries operators only in head position; grip changes *which* tree is built,
  not that property, but the climb is checked not to reintroduce a factorable surface.
- ⚠ **Every existing assert in the corpus is a regression test for the grip bands.** The
  acceptance bar is `make test` green with zero assert edits; anything that flips is either a
  grip-band bug or a genuinely surprising precedence that must be blessed. The at-risk shapes are
  chained relations (`(!"" = 0 = $"")`), anything mixing `|`/`&`/`&&`/`||` with arithmetic or
  comparison, and unparenthesized `><`-with-band expressions.

opfix is a source→source pass upstream of analysis and codegen, so a correct re-grouping is
transparent downstream — but `make test_slow` (glaze-x86.l, arm64, kernel) is the proof, not the
assumption.

## naming

`grip` = an operator's precedence level — how tightly it holds its operands; a higher grip binds
tighter. It frames the concept in the green (what the operator *does* — grips — rather than
"precedence," which names a comparison). It is internal: absent from `(names ())`, and
mechanically swappable.
