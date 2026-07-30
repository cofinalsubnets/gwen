# precedence — right-to-left as far as it goes, then let grip decide

Status: **SHIPPED 2026-07-14.** Grips live in [`love/prel.l`](../love/prel.l)'s
reader-operators block (opfix); no C, both compilers inherit it.
[`test/precedence.l`](../test/precedence.l) gates the tree + value + short-circuit
+ idempotence; `make test` is green ×3 (3439) and `test_slow` is green bar the
pre-existing qemu-arm64 `uk-jj`. The corpus audit shifted exactly THREE asserts,
all single `|`/`&` mixed with `=` (grip 30 below comparison 40): `test/spec.l:88`
and `:164` parenthesized, `test/infixop.l:27` moved to the C-ternary read.
Two calls remain gwen's (§Open): the `grip` **name** and house = 27 — both shipped
as working defaults (internal, absent from `(names ())`, mechanically swappable).
The design below is the as-built record.

## the ask

Today infix is *flat*: every dyadic operator nests right-associatively at one
uniform level. Probed on the binary:

```
(2 * 3 + 4)   ; 14   = 2*(3+4), not (2*3)+4
(10 - 2 - 3)  ; 11   = 10-(2-3), not (10-2)-3
(1 + 2 * 3)   ; 7    = 1+(2*3)
```

Elegant (it's the APL read, and it's *why* spec.l can write asserts infix —
`(3 = 1 + 2)` folds to `(= 3 (+ 1 2))`), but not what a hand trained on
schoolbook math expects: `2 * 3 + 4` should be `10`, not `14`.

The goal: let `*` bind tighter than `+`, `+` tighter than `=`, etc. — **without**
giving up the right-leaning default and **without** a C change.

## the key idea: precedence is a *refinement* of right-associativity

Grouping and evaluation order are independent axes. Precedence and
associativity decide only the *parse tree*; runtime order is a separate matter
and, in a pure core, unobservable except for effects and which subexpression
scares first (`:` stays the explicit source-order sequencer regardless). So
this change never touches evaluation — only how opfix factors the tree.

Within that parse question, the design keeps right-to-left as the default and
lets grip intervene *only* to pull a tighter operator in. The whole thing is
one comparison at the steal-point:

> an incoming operator steals a pending filled frame's operand **iff its grip ≥
> the pending frame's grip.**

- **≥ (steal)** — nest the incoming operator to the right. Equal grip steals, so
  same-level operators stay right-associative (unchanged from today).
- **< (don't steal)** — the pending frame folds first, and the incoming
  (looser) operator takes the folded result as its left operand. This is the
  precedence climb.

Traced:

| input | grips | result |
|---|---|---|
| `a + b + c` | equal | steal → `(+ a (+ b c))` — right-assoc preserved |
| `a + b * c` | `*` > `+` | `*` steals `b` → `(+ a (* b c))` |
| `a * b + c` | `+` < `*` | `+` forces `*` to fold → `(+ (* a b) c)` |

**The conservative-extension property (the reason this is safe):** when every
operator shares one grip, `≥` is always true, nothing ever folds early, and you
get back today's flat right-associative behavior *exactly*. So flat-right is the
degenerate case, and any file that touches only equal-grip operators is
byte-unchanged. This is the property the spec.l infix-assert law leans on
(see §Risks).

**Associativity landed 2026-07-25 as the HAND.** The guard above is exactly it:
an entry carries a hand alongside its grip, and `op-steal` tests
`(? h (<= g (op-frgrip f)) (< g (op-frgrip f)))` — a RIGHT-handed incomer steals
at equal grip (its band folds right), a LEFT-handed one yields, so the pending
frame folds first and its band folds left. Grip and associativity compose in one
predicate, as predicted. Arithmetic (`* / %` at 60, `+ -` at 50) is left-handed;
every other band, and every coined operator at house grip, stays right.

## what changes, precisely

Everything is in the reader-operators block of `love/prel.l`
([the table `prel.l:289`](../love/prel.l), [`op-ent` ~330](../love/prel.l),
[`op-del` ~407](../love/prel.l), [`op-steal` ~422](../love/prel.l), [`op-w` ~438](../love/prel.l)).

### 1. the table entry carries a grip

The `operators` table is `symbol -> arity` or `symbol -> (name . arity)` (an
alias). Carry a grip in a **triple** `(name arity . grip)`, chosen because it is
**accessor-compatible**: `(name arity . grip)` is `(link name (link arity grip))`,
so `cap` = name and `caup` = arity exactly as today, and `grip` is the new
`cuup`. Every existing consumer (`opfactor`, `op-long`, `mono-long`, the
`nm`/`n` reads in `op-w`) only ever reads `cap`/`caup` and is untouched; the grip
is a pure addition read as `(cuup en)`.

A bare arity or `(name . arity)` defaults to the **house grip**. Assign grips in
bands, coarse and few:

```
; higher grip binds tighter. leave gaps so a level can slot between later.
;   **             grip 70   (apply — flip-apply (a ** b = (b a)), tightest infix)
;   * / %          grip 60   (multiplicative)
;   + -            grip 50   (additive)
;   < <= > >= =    grip 40   (comparison)   <- the assert-relation band
;   | &  && ||     grip 30   (logical)
;   (house)        grip 27   (coined operators — the default, fresh punct, no row)
;   ><             grip 25   (cons — the loosest builder)
;   <- ->          grip 20   (assignment aliases)
;   ?              grip 10   (cond)
;   $              grip 5    (weak apply — a $ b = (a b), the haskell $, loosest)
```

`**` and `$` are the two **apply** operators, both self-named (the reader emits
`(** a b)` / `($ a b)`, backed by the prel globals `(: (** a b) (b a))` and
`(: $ 1)` — `$` *is* the identity, so `($ a b) = (a b)`). They bracket the
range: `**` flip-applies at the tightest grip (a pipe-like reverse apply that
binds before arithmetic), `$` weak-applies at the loosest (everything to its
right groups first, then applies — `f $ a + b` is `f (a + b)`, and `f $ g $ x`
folds right to `f (g x)`, exactly haskell's `$`). The glued monadic `$x` is
untouched — it factors through the `monadics` table to `saturate`, a separate
valence the spaced dyadic never sees.

The house default is **27**, a hair tighter than `><` (25): cons builds its pair
*last*, after everything computes, so it is deliberately the floor of the coined
range, and a generic unknown operator resolves before it (`a ~ b >< c` groups
`(a ~ b) >< c`). Both sit in the 20–30 gap, tie no named band (equal grip
steals, so a coined operator at a band's exact level would interleave-steal with
it — the gap prevents that), and yield to arithmetic / comparison / logic. This
means `><` needs its **own explicit row** now (it rode the house default before);
the table row is for grip + factoring and coexists with the define-sugar that
binds the function, exactly as `+` has both a row and a binding.

Only entries whose grip differs from the house default strictly need a table
row; the default-infix-at-two fallback (`op-w`, a fresh punct symbol with no row)
keeps grip 27, so user operators stay flat-right against each other unless they
opt in.

`op-ent` normalizes all three entry shapes to the triple `(name arity . grip)`
(charm → `(name arity . house)`, `(name . arity)` → `(name arity . house)`,
triple → verbatim), validating grip alongside the arity check it already does —
the one place that reads the table shape, so the rest of the walk sees a uniform
triple.

### 2. the pending frame remembers its grip

The op-fr frame is `(orig chain name need . got)`
([`prel.l:386`](../love/prel.l)). **Store grip on the frame — do not re-probe the
table.** The frame keeps two symbols, and neither alone recovers grip:

- `op-fro` = the *source* symbol. A composite like `!=` factors to `(! =)`; its
  source `!=` has no table row, so `op-ent` gives the house grip, but the
  operative grip is `=`'s (comparison). The source side can't see it.
- `op-frn` = the *resolved* name. An alias like `<-` resolves to `pin`; the table
  is keyed by source `<-`, so `op-ent 'pin` misses → house grip, but the
  operative grip is `<-`'s row. The resolved side can't see it.

The one value that carries the right grip in *both* cases is `en`, the last-factor
entry already in scope at the build site ([`prel.l:450`](../love/prel.l)): for a
composite it is `=`'s entry, for an alias it is `<-`'s. So **capture grip from
`en` (`(cuup en)`) into the frame** at build time. This is a correctness point,
not the perf tradeoff the frame-vs-reprobe question framed it as.

Concretely: extend `op-fr` with a grip slot + an `op-frgrip` accessor, and thread
it through the ~6 construction sites — two *preserve* (`op-del` re-arm
[`prel.l:412`](../love/prel.l), `op-steal` re-arm [`prel.l:428`](../love/prel.l) →
pass `(op-frgrip f)`), three *set* (the infix build [`prel.l:456`](../love/prel.l)
→ `(cuup en)`; the two prefix builds [`prel.l:453`](../love/prel.l),
[`prel.l:459`](../love/prel.l) → house, inert since prefix frames fold on fill and
never sit filled to be stolen from).

### 3. op-steal becomes a climb

Today [`op-steal` (`prel.l:422`)](../love/prel.l) steals from a filled top frame
unconditionally. It gains the incoming operator's grip and **one new
else-branch** — the climb reuses the existing `op-del`/`op-fold`, no new folding
machinery:

```
op-steal(g, out, pend):
  filled top frame F (op-frd F == 0):
     g >= grip(F)  -> steal F's last operand, re-arm to need 1   ; today's code, verbatim
     g <  grip(F)  -> (out',pend') = op-del(op-fold F, out, cup pend)
                      op-steal(g, out', pend')                   ; the climb: fold, retry beneath
  collecting frame -> 0                                          ; unchanged
  empty pend       -> top-level last datum                       ; unchanged (today's else)
```

The predicate: **fold when the frame grips tighter than the incomer
(`grip(F) > g`), steal otherwise (`g >= grip(F)`).** Equal grip steals, so
same-level operators stay right-associative — the conservative-extension case.

Traced:

- `a + b *` — `*`(60) ≥ `+`(50) → steal `b` → `(+ a (* b c))`.
- `a * b +` — `+`(50) < `*`(60) → fold `*` → `(* a b)` lands in `out`, retry hits
  empty pend → top-level last-datum → `(+ (* a b) c)`.
- `a + b + c` — equal(50) → steal → `(+ a (+ b c))`, right-assoc preserved.

The one subtlety versus today: when the incoming grip is *lower*, op-steal folds
the pending frame and retries, possibly several times (a stack of tighter frames
all complete before the looser operator lands) — but each fold is just the
existing `op-del (op-fold F)` cascade, so a looser frame beneath receives the
folded value as an operand and sits filled, and the loop's next turn re-checks
its grip. A small loop, not a rewrite. The caller in `op-w`'s infix branch
passes the incoming grip: `(op-steal out pend)` → `(op-steal (cuup en) out pend)`
([`prel.l:454`](../love/prel.l), `en` already bound), and already threads
`(out . pend)` back from a steal, so the shape fits.

### 4. op-del is untouched for right-assoc

`op-del`'s defer-vs-fold decision ([`prel.l:414`](../love/prel.l)) — the line that
makes a filled infix frame *sit* rather than fold — stays as-is for the
right-associative default. It only changes when we wire the left-assoc bit
(fold-on-fill), which is the deferred namespace-assignment work.

### 5. `&&` and `||` go infix — as short-circuit macros, nearly free

`&&` and `||` are **already short-circuit macros** ([`prel.l:256`](../love/prel.l),
[`prel.l:257`](../love/prel.l)): `&&` expands to nested `(? a b ())`, `||` to
`(: y a (? y y rest))`. Both ride `?`/`:` — genuine lazy special forms — so
left-to-right short-circuit is real today.

They are **also already infix** — but at the wrong grip. A fresh punct symbol
with no table row defaults to infix-at-two ([`prel.l:448`](../love/prel.l)), so
`(0 < x && x < 10)` parses today (probed: reads `1`) — as
`(< 0 (&& x (< x 10)))` under the flat right-fold, a latent mis-grouping. The two
rows (`&& ||` at grip 30, the logical band) don't *add* infix; they **pin the
grip below comparison** so the same expression groups `(&& (< 0 x) (< x 10))` —
what infix `&&` should mean. It's live but unused (zero infix `&&`/`||` in love
source — every use is prefix `(&& a b)`), so the fix changes no existing parse.

This composes with the macro for free **because opfix and macro expansion are
separate passes in the right order.** opfix runs first and is purely structural:
it factors `a && b` → `(&& a b)`, treating `&&` as an opaque arity-2 operator.
*Then* wev expands the macro → the short-circuiting `?`. Infix + macro + shortcut
fall out of the two-stage pipeline with **zero macro changes.** Three things that
make it safe:

- **Prefix uses don't break.** A leading operator with no left operand falls
  through op-steal to the plain-symbol case (the same reason prefix `(+ a b)`
  works), so the existing `(&& ...)`/`(|| ...)` calls across prel/ev are
  untouched.
- **Variadic macro, binary infix — fine.** `a && b && c` factors right-assoc to
  `(&& a (&& b c))`; the macro expands outer-then-inner to `(? a (? b c ()) ())`.
  Correct short-circuit.
- **Grip 30 (below comparison)** is the point: it makes `a < b && c < d` group
  `(a < b) && (c < d)`.

This is the same step-1 table edit — two more rows — and nothing else. It does
mean `&&`/`||` join the grip-band regression surface (§Risks 1): any assert
mixing them with comparison or arithmetic must still group as written.

## the bootstrap constraint

The reader-operators block is compiled by **c0** (the C bootstrap) before opfix
exists, so the grip machinery must stay operator-free and use only what's
defined above it in the prel (`foldl`, `L`, `link`, `?`, kind tests — the same
palette the current table build uses). No `!`/`+` sigils inside these
definitions. The table itself is already built pre-opfix this way
([`prel.l:289`](../love/prel.l)); grips are just more data in the same fold.

Both compilers (c0 and the self-hosted `feel`) call `book['opfix]`, so the change
lands in *one* place and both inherit it — no C edit, no second source of truth.

## Risks

**1. spec.l reads asserts infix.** The whole reference reads `(RESULT = EXPR)`
and folds right. The comparison band (`= < <= > >=`, grip 40) sits **below**
arithmetic (50) so `(3 = 1 + 2)` still groups `(= 3 (+ 1 2))` and `(6 = $'(1 2 3))`
still groups `(= 6 ($ ...))`. That's the natural math ordering (comparison
loosest of the relations), so it should hold — **but every existing assert is a
regression test for the grip bands.** The gate is simply: `make test` stays
green with no assert edits. Any assert that flips is either a grip-band bug or a
genuinely surprising precedence the user must bless. Enumerate the at-risk shapes
first: chained relations (`(!"" = 0 = $"")`), any assert mixing `|`/`&`/`&&`/`||`
with arithmetic or comparison, and any unparenthesized `><`-with-band expression
(blast radius tiny — `><` only landed in `602ea256`/`7ad6c4e6`).

**Audited 2026-07-14 (line-local greps over `*.l`):** the risky shapes barely
occur. Bare mixed arithmetic (`a * b + c`) — **0 sites**. Genuine infix
`&&`/`||` — **0 sites** (every love use is prefix `(&& a b)`; the raw grep's 156
hits were all C `&&`, comments, or string literals). love code parenthesizes
mixed grouping aggressively — a habit the flat rule already trained — so the
same-grip idioms above carry the weight and the precedence bands touch almost
nothing. The greps are line-local, so a multi-line infix expression could hide;
`make test` with zero assert edits remains the real backstop, but the pre-audit
predicts green.

**2. idempotence.** op-core is idempotent because factored output carries
operators only in head position. Precedence changes *which* tree we build, not
that property — but re-run op-core on the output in a test to confirm the climb
doesn't reintroduce a factorable surface.

**3. the `?` cond operator (arity 3) and the pin/peep aliases (`<-` `->`, arity
3).** Decided: `?` = grip 10 (loosest of all), `<- ->` = grip 20 (assignment-
shaped, just above cond). Set explicitly rather than falling to the house
default.

**4. glaze / native lanes.** opfix is a source→source pass upstream of analysis
and codegen; a correct re-grouping is transparent to everything downstream. No
glaze change expected — but `make test_slow` (glaze-x86.l, arm64, kernel) is the
proof, not the assumption.

## Testing / gate

- A fresh [`test/precedence.l`](../test/precedence.l) (cleaner than growing
  `test/operator.l`): the three canonical cases above plus mixed chains, each
  asserting the *tree* (via `show`/`op-core` on quoted forms) and the *value*.
- `&&`/`||` infix cases: `a < b && c < d` groups `(&& (< a b) (< c d))`; a
  short-circuit that must not evaluate its right arm (e.g. `(|| 1 (some-scare))`
  reads the left without firing the scare).
- `><` vs. the house default and the bands: `a + b >< c` groups `(>< (+ a b) c)`;
  a coined operator against `><` (`a ~ b >< c` → `(>< (~ a b) c)`).
- The hand: a same-grip arithmetic chain folds LEFT (`(opfix '(a + b + c))` =
  `'(+ (+ a b) c)`), a same-grip `><`/`$`/coined chain folds RIGHT.
- `make test` (host + love0 bootstrap, both) with **zero edits to existing
  asserts** is the acceptance bar. Then `make test_slow`.
- Idempotence assert: `(op-core (op-core form)) = (op-core form)` on the mixed
  cases.

## deferred (not this doc)

- ~~**left-associative operators.**~~ LANDED 2026-07-25 as the hand (see above).
  Namespace assignment still rides the scope-layer door
  ([`prel.l`](../love/prel.l): *"a new arity or alias waits for the scope-layer
  door"*).
- ~~user-declarable grips~~ LANDED 2026-07-25 as **`fixity`**, the one sanctioned
  write into the compile table and the only reach onto `operators` once birth mops
  the nom (the closure captures it). `(fixity nm v)` pins nm's row and answers the
  row it replaced *in the shape it takes*, so `(fixity nm (fixity nm new))`
  restores exactly, `()` both clears a row and reports an absent one, and a
  non-`()` answer says someone declared that operator before you. `op-ent` itself
  is the acceptor: a shape it refuses is rolled back and scared rather than
  pinned, because a bad row does not error — it silently demotes its operator to
  the fresh-punct default, which on a core operator is a poisoned compiler with no
  message. `'(2 60 1)` is `infixl 7`, `'(2 60)` is `infixr 7`, a bare `2` is the
  house grip, and the nom-led forms alias.

  **It is global, deliberately (revisable).** An operator's grip is part of its
  meaning, so a module that coins `<+>` wants its consumers to read
  `(a <+> b * c)` the way it does — Haskell exports fixity for the same reason.
  The save/restore pair covers a scope-local grammar, and wiring it to
  `enter`/`leave` stays available without changing this API.

## resolved (were open)

- **grip of `?` and `<- ->`.** `?` = 10, `<- ->` = 20. See §Risks 3.
- **grip on the frame vs. re-probe.** On the frame — re-probe is a *correctness*
  bug (composite `!=` and alias `<-` each defeat one of the two symbols op-ent
  would probe). Capture from `en`. See §what-changes 2.
- **`|`/`&` band, plus `&&`/`||`.** All four at grip 30, below comparison — so
  `a < b & c < d` and `a < b && c < d` group with the logical op loosest.
- **how many bands.** Six coarse levels (60/50/40/30/20/10) plus the two coined
  slots (27 house, 25 cons). No C-style 15-level ladder; gaps left to slot more.

## Open questions (genuine, for gwen)

- **`grip` the name** (see §Naming) — coins a new word under the rename freeze.
- **house = 27 vs. a distinct isolated slot.** 27 lets coined operators interact
  predictably (they yield to every band, tie none). The alternative — a level
  shared by nothing so coined ops never interact with anything — is arguably
  worse; 27 is the recommendation, revisit at the scope-layer door.

## Naming

`grip` = an operator's precedence level — how tightly it holds its operands; a
higher grip binds tighter. Frames the concept in the green (what the operator
*does* — grips — not "precedence," which names a comparison). Alternatives:
`bind`, `pull`, `tight`. Under gwen's rename freeze this coins a *new* word only,
touching no existing name; bless or swap before it ships.
