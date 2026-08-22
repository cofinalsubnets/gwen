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

`jot` is tally's section: `tally (jot n) = n`.
