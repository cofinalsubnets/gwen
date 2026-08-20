# precedence — right-to-left as far as it goes, then let band decide

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

Within that parse question, right-to-left stays the default and band intervenes *only* to pull a
tighter operator in. The whole thing is one comparison at the steal-point:

> an incoming operator steals a pending filled frame's operand **iff its band beats the pending
> frame's band** — at equal band, the incomer's **hand** decides.

- **steal** — nest the incoming operator to the right.
- **don't steal** — the pending frame folds first, and the incoming (looser) operator takes the
  folded result as its left operand. This is the precedence climb.

| input | grips | result |
|---|---|---|
| `a + b + c` | equal, left-handed | fold first → `(+ (+ a b) c)` |
| `a >< b >< c` | equal, right-handed | steal → `(>< a (>< b c))` |
| `a + b * c` | `*` > `+` | `*` steals `b` → `(+ a (* b c))` |
| `a * b + c` | `+` < `*` | `+` forces `*` to fold → `(+ (* a b) c)` |

**The conservative-extension property:** when every operator shares one band and one hand,
nothing ever folds early and the behaviour is exactly flat right-associative. Flat-right is the
degenerate case, so any file that touches only equal-band right-handed operators is
byte-unchanged. That is the property spec.l's infix-assert law leans on.

**The hand** is associativity, and it composes with band in one predicate:
`(? h (<= g (op-frband f)) (< g (op-frband f)))` — a RIGHT-handed incomer steals at equal band
(its band folds right), a LEFT-handed one yields, so the pending frame folds first and its band
folds left. Arithmetic (`* / %` at 60, `+ -` at 50) is left-handed; every other band, and every
coined operator at house band, is right.

## the table

A row in a spaced lane is one **signed** band: the magnitude is how tightly it holds, the sign is
the hand. Neither the arity nor the hand is a field — the arity is the **key**, so a row cannot
disagree with the lane it sits in, and the hand is the **sign**, so nobody counts to a slot.
`op-lane` normalizes the three kinds to `(name band hand . arity)` and `op-ent` asks the spaced
lanes tightest first, so that is the one place that reads the table shape and the rest of the walk
sees a uniform tuple. A spaced sigil is one whole name and never splits into factors; factoring
is the glued lane's alone — arity 0.

**The glued lane keys on a WORD, not a char**, so a row may be spelled with as many chars as it
likes — `0? 1? 2?` (nil?/one?/two?) are stock. `monofactor` splits a run **right to left**, longest
first: the sigil the reader glued to the datum matches before the ones reaching over it, so on a
run `abc` the rows `a` `bc` beat `ab` `c`. Single-char rows agree either way, which is why the stock
table alone cannot tell the two directions apart.

That order is a **preference, not a commitment** — the walk backtracks, so a run factors iff some
split exists at all. The completeness is what keeps `grip 0` composable: an unfactorable run has to
fall through to the plain application `(run d)`, since that fallback is what carries data and coined
heads past opfix, so a dead end can never be *reported*. Without the fallback, coining `?<` would
silently unfactor `2?<x` — a run with nothing to do with it. With it, `?<` is tried first, fails to
complete, and yields to `<`.

⚠ **A band must be positive.** A tablet miss answers `0`, so a zero band would read that miss as
a live row at the loosest band there is.

**Undeclared is infix at band 95, right-handed** — above every row, so the table holds only
exceptions:

```
;   (house)        band 95   coined operators — fresh punct, no row
;   **             band 70   flip-apply (a ** b = (b a)), tightest DECLARED infix
;   * / %          band 60   multiplicative, LEFT
;   + -            band 50   additive, LEFT
;   = != < <= > >= band 40   comparison   <- the assert-relation band
;   | & && ||      band 30   logical
;   ><             band 25   cons — the loosest builder
;   ?              band 10   cond — infix is the one-armed form; an else arm is prefix (? c a b)
;   $              band 5    weak apply — a $ b = (a b), the haskell $, loosest
```

Gaps are left so a level can slot in later.

⚠ **The two rows that catch a C-primed hand, both by being where C is not.**

**`<< >>` are not in the table** — they are coined punct, so they ride **house band 95**, above
every declared row. C puts the shifts *below* `+ -`; here they are above `* /`. So
`c - 192 << 6` is `c - (192 << 6)`, not `(c - 192) << 6` — which is how a UTF-8 decoder came to
compute garbage from a line that reads correctly in C. Parenthesize the arithmetic operand of a
shift, always.

**`| & && || ` are ONE band**, not four, and band 30 is right-handed like every band but
arithmetic. So they do not order against each other at all: `x & y && z` folds to
`x & (y && z)`, and `(ctbl c & 4 && c <= 55)` asks `ctbl c & (4 && c <= 55)` — a mask against a
truth bit, which read every octal digit wrong and surfaced three files away as a lex error.
C's `&` -above- `&&` ordering does not exist here; spend the parens: `((ctbl c & 4) && c <= 55)`.

Comparison (40) does bind tighter than the logical band (30), so `1 < 2 && 3 < 4` is the pair of
comparisons you meant — that one matches C, and it is the reason the other two surprise.

`**` and `$` are the two **apply** operators, both self-named (the reader emits `(** a b)` /
`($ a b)`, backed by the prel globals `(: (** a b) (b a))` and `(: $ 1)` — `$` *is* the identity,
so `($ a b) = (a b)`). They bracket the range: `**` flip-applies at the tightest band (a
pipe-like reverse apply that binds before arithmetic), `$` weak-applies at the loosest —
`f $ a + b` is `f (a + b)`, and `f $ g $ x` folds right to `f (g x)`, exactly haskell's `$`. The
glued monadic `$x` is untouched: it factors through arity 0 to `saturate`, a
separate valence the spaced dyadic never sees — one nom, two lanes, which is the valence law.

House sits **above every band**, as Haskell's undeclared-is-`infixl 9`. No source in the tree
rides house — opfixing every top-level form of every tracked `.l` at 27, 35 and 95 differs only
in prel.l's own literals — and lux, the one production `grip` user, pins both its grips by
number.

`&&` and `||` are **already short-circuit macros** riding `?`/`:`, and a fresh punct symbol is
already infix-at-two, so their rows do not *add* infix — they **pin the band below comparison**
so `(0 < x && x < 10)` groups `(&& (< 0 x) (< x 10))` rather than the flat right-fold's
`(< 0 (&& x (< x 10)))`. Infix + macro + short-circuit compose for free because **opfix and macro
expansion are separate passes in the right order**: opfix runs first and is purely structural,
factoring `a && b` → `(&& a b)` with `&&` an opaque arity-2 operator; *then* wev expands the macro
into the short-circuiting `?`. A variadic macro under binary infix is fine — `a && b && c`
factors to `(&& a (&& b c))` and the macro expands outer-then-inner to `(? a (? b c ()) ())`. A
leading operator with no left operand falls through op-steal to the plain-symbol case, so prefix
`(&& ...)` is untouched.

## the frame carries its band

The op-fr frame is `(orig name need band . got)`. ⚠ **Store band on the frame — do not re-probe
the table.** The frame keeps two symbols and neither alone recovers band: `op-frn` is the
*resolved* name, and a `grip`-declared alias like `(grip '=: '(pin 20 0))` resolves to
`pin` while the table stays keyed by source `=:`, so probing `pin` misses. The value carrying the
right band is `en`, the entry already in scope at the build site. This is a correctness point,
not a performance tradeoff.

A frame is only ever rebuilt with a new need + got; orig, name and band are fixed the moment the
operator is read.

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

When the incoming band is lower, op-steal folds the pending frame and retries, possibly several
times — a stack of tighter frames all completing before the looser operator lands. Each fold is
the existing `op-del (op-fold F)` cascade, so a looser frame beneath receives the folded value as
an operand and sits filled, and the loop's next turn re-checks its band. A small loop, not
separate folding machinery.

`op-del`'s defer-vs-fold decision — the line that makes a filled infix frame *sit* rather than
fold — is what gives the right-associative default; the hand is handled entirely in the steal
predicate.

## `grip` — the one door onto the table

`operators` is mopped at birth, so `grip` (whose closure captured the table) is the only
sanctioned write into it afterwards. `(grip ar nm v)` pins nm's row in lane `ar` and answers
the row it replaced *in the shape it takes*, so `(grip ar nm (grip ar nm new))` restores
exactly, `()` both clears a row and reports an absent one, and a non-`()` answer says someone declared that operator
before you.

The lane's own read is the acceptor: a shape it refuses is rolled back and scared rather than pinned,
because ⚠ **a bad row does not error — it silently demotes its operator to the fresh-punct
default**, which on a core operator is a poisoned compiler with no message.

A row is read **by kind**, three shapes and no positions to count: a **charm** is the band and the
sigil names itself (`-60` is `infixl 7`, `60` is `infixr 7`); a **nom** is an alias — the sigil
means that word and takes its band, one hop, so no alias can drift from its target and no cycle
can be spelled; a **`(nom band)`** pair is an alias carrying a band of its own, for a target with
no row to lend. The glued lane takes the word directly.

⚠ The glued lane accepts **anything**, not only a nom. Its row becomes the emitted head, so a
closure there evaluates perfectly well — it costs only the round trip, since opfix's output stops
being spellable for that sigil and `libra infix`/`unfix` with it. A nom is what the tree writes.

**It is global, deliberately (revisable).** An operator's band is part of its meaning, so a module
that coins `<+>` wants its consumers to read `(a <+> b * c)` the way it does — Haskell exports
grip for the same reason. The save/restore pair covers a scope-local grammar, and wiring it to
`enter`/`leave` stays available without changing this API.

## the bootstrap constraint

The reader-operators block is compiled by **c0** (the C bootstrap) before opfix exists, so the
band machinery must stay operator-free and use only what is defined above it in the prel
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
- The hand: a same-band arithmetic chain folds LEFT, a same-band `><`/`$`/coined chain folds
  RIGHT.
- Idempotence: `(op-core (op-core form)) = (op-core form)`. op-core is idempotent because
  factored output carries operators only in head position; band changes *which* tree is built,
  not that property, but the climb is checked not to reintroduce a factorable surface.
- ⚠ **Every existing assert in the corpus is a regression test for the bands.** The
  acceptance bar is `make test` green with zero assert edits; anything that flips is either a
  band bug or a genuinely surprising precedence that must be blessed. The at-risk shapes are
  anything mixing `|`/`&`/`&&`/`||` with arithmetic or comparison, and unparenthesized
  `><`-with-band expressions. Chained relations were on that list and are now their own
  feature: `(a < b < c)` CHAINS, and the two corpus sites that meant the nested reading
  (`!x == (0 = $x)`) had to say so with parens — the only two the change moved.

opfix is a source→source pass upstream of analysis and codegen, so a correct re-grouping is
transparent downstream — but `make test_slow` (glaze-x86.l, arm64, kernel) is the proof, not the
assumption.

## naming

Four words, each naming exactly one thing, and none of them borrowed:

- **grip** — how a sigil takes hold of what is around it: its lane, band and hand together. Also
  the door that declares one. It frames the concept in the green (what the operator *does*)
  rather than "fixity," which is Haskell's and has no glued case at all.
- **lane** — the arity: the operands the sigil takes from the form around it. `0` glued, `2`
  spaced, `-1` spaced with no bound, `-2` spaced and **chaining** (a same-band neighbour joins the
  run instead of nesting, so `a < b <= c` is the conjunction of its links with `b` evaluated once).
  `1` — a right operand and no left — has a key and no walk yet.
- **band** — the precedence level, a **signed** charm. The magnitude is how tightly it holds; the
  sign is the hand. Higher binds tighter, and zero can never be a band because a tablet miss
  answers `0`.
- **hand** — which way a same-band run folds. It is not a field: **a negative band is
  left-handed**, and since arithmetic is the only left-handed band in the tree, the minus marks
  the exception where it can be read.

⚠ A band being signed makes it **red** under the net, so every test on one must be by kind or by
identity — `(nil? -60)` is true, and a truth test would read every left-handed row as absent. The
door spells this out at the site.

`band` and `lane` are internal: absent from `(names ())`, and mechanically swappable. `grip` is
the one that reaches the surface, because it is the door.
