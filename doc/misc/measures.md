# net & tally — the two measures, and monadic `*` = prod

The value space's algebra: two dualities, with `net` sitting on the crossing.

## Two axes

- **Measures** — structure-preserving maps `value → scalar`, one per monoid:
  - **net** = the additive measure, the `+`-hom `(values, +) → (C, +)`. *weight* — "how much."
  - **tally** = the multiplicative measure, the semiring (rig) hom `(values, +, *) → (N, +, ·)`.
    *count* — "how many."
- **Folds** — the operation turned inward over a value's contents:
  - **net** = `foldl (+) 0`.
  - **prod** = `foldl (*) 1`.

`net` is in *both* lists because for `+` the fold **is** the hom (the free-monoid universal
property — same map, two readings). For `*` they **split**: the multiplicative fold is `prod`,
the multiplicative measure is `tally`, and they are **different maps**. So:

- monadic-`*`'s twin **by fold** is monadic-`+`: `prod ∥ net`.
- net's twin **by measure** is `tally` — **not** monadic-`*`.

`tally` is the multiplicative leg net structurally lacks.

## The laws

| form | value | reading |
|---|---|---|
| `+'(1 2 3 4)` | `10` | net — fold `+`, seed 0 |
| `*'(1 2 3 4)` | `24` | prod — fold `*`, seed 1 |
| `net(a*b)` on `'(2 3) '(4 5)` | `28` | …but `net a · net b = 45` |
| `tally(a+b)` | `4` | `= tally a + tally b` |
| `tally(a*b)` | `4` | `= tally a · tally b` (= 2·2) |
| `tally(xs*n)` on `'(2 3) 3` | `6` | `= tally xs · n` |
| `tally 7` | `0` | tally is cardinality, not weight |

$$\nu(a+b)=\nu a+\nu b,\quad \nu(a*b)\neq\nu a\cdot\nu b \qquad\quad \tau(a+b)=\tau a+\tau b,\quad \tau(a*b)=\tau a\cdot\tau b$$

So **net is a `+`-monoid hom only** (blind to `*`); **tally is the full semiring hom**. That is
the precise sense in which tally is net's twin.

## The cartesian/repeat `*` is the founded part

The value space is a genuine **semiring**: `+` = append/concat, `*` = cartesian product, the
shared unit `()` projecting to both `0` and `1`, right-distributive (`(a+b)*c = a*c + b*c`,
exact). `tally` is its rig hom — and it *unifies the lanes*: the **repeat** lane
(`tally(xs*n) = tally xs · n`, the ℕ-action / "`*` is repeated `+`") and the **cartesian** lane
(`tally(xs*ys) = tally xs · tally ys`) are **one law** under tally, since `n` is just the tally
of an n-thing. So cartesian/repeat `*` is not ad hoc — it's the product and module-action faces
of one semiring multiplication, with tally the witness. (`str*str = nil` / `sym*sym = nil` are a
closure issue — the product would type-escape the string kind.)

## The three roles

1. **Monadic `*` = `prod`, uniformly.** `*x` is `*` turned inward, the multiplicative fold:
   aggregate → product of cells, scalar → itself (vacuous, so `*5 = 5`) — the rank-uniform dual
   of `+x`, exactly as `+5 = 5`. Every glued row keeps that invariant: the sigil is its
   own dyadic op, turned monadic. (Binding `*` → `tally` would break it — `tally` is not "`*`
   folded inward" — so tally stays a named word.)
2. **`tally` is net's measure-twin** — the cardinality rig-hom, with both hom laws.
   The trinity: **net** (weight, `+`-hom) · **tally** (count, rig-hom) · **prod** (the `*`-fold,
   an operator, neither measure).
3. **`jot` is the section of tally** — `(jot n)` is the range `0..n-1`, the canonical witness
   builder for a count (`tally (jot n) = n`), a *constructor* and the right-inverse of the
   measure. A third role, distinct from both fold and measure, so it wears its own name rather
   than a sigil.
