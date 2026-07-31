# clay -- C as love data

the plan for owning C the way holo owns assembly: a C AST written as love data, a shower
that renders it to C text, and one datum feeding both the generated C and its Rocq model.
the payoff sought is VERIFICATION -- doc/verify.md says love.c is "near its floor" for
shrinking and the remaining lever is verifying pieces against references. its LINE count is
near its floor; its TRUSTED surface is not, and that is where clay pays.

the map, the ranked slate, and the honest costs. **rungs 0, 1 and 2 have landed**
(`crew/moon/clay.l`, `make test_clay`, `tools/mx.l` + `mx.h`); the rest is unbuilt.

what rungs 0-1 actually found, and it is the single most useful fact in this file:
**cparse's AST is not a complete C representation.** a top-level DECLARATION mostly
does not survive it -- a typedef, a struct definition and a `_Static_assert` all land
as the empty marker `(tdef)`, a prototype keeps only its name, a function definition
has no RETURN TYPE, and `static`/`const` are gone. that information is real; it lives
in the side tables (`stag`, `sigs`) and the parse state, which `gen.l` is HANDED and a
shower is not. so clay is a SUPERSET of cparse's output, with the missing slots
APPENDED, and G1 partitions test/cc into what it can say (62) and what it cannot
(48) and prints both. the second number is the live measure of the gap, and teaching
parse.l to FILL those markers is what shrinks it -- clay's grammar already carries
the faithful forms.

## what already exists

the representation is not the work. `crew/moon/parse.l` has produced C as s-expressions all
along:

```love
(cast "int add(int a, int b) { return a+b; }")
; (prog ((fn "add" (("a" int) ("b" int)) (blk ((ret (bin + (var "a") (var "b"))))))))
```

`crew/moon/law.l:144` already calls `cgen` on hand-shaped data. `crew/moon/lib/mksys.l`
already ships a production file that skips the C frontend entirely and hands holo literal
IR. and the coverage question is nearly settled: `host/build.mk:228` compiles **love.c
itself** with mooncc, so moon's AST spans love.c's C surface *as mooncc's own cpp leaves
it*.

⚠ that qualifier is load-bearing and it was written too strongly the first time.
`crew/moon/cpp.l:410` leaves `__SIZEOF_INT128__` **deliberately undefined** on every
target, and `love.c:62` gates its limb width on exactly that macro. so mooncc-built love.c
takes the **32-bit limb** path and gcc-built love.c takes the 64-bit one -- the same file,
two different algorithms, and `unsigned __int128` is a piece of C moon cannot say at all.
this costs nothing for most of the slate and reshapes section B; see there.

| piece | where | state |
|---|---|---|
| the AST | `crew/moon/parse.l:1688`, `(cparse-t ts tgt)` | exists, unnamed as an interface |
| the goldens | `crew/moon/law.l:80-141`, `:246-251` | exists, the de-facto spec |
| the codegen seam | `crew/moon/gen.l:5306` `(cgen-obj ast stbl sigs weaks xtra tgt)` | exists, positional, `xtra` optional |
| hand-built-data precedent | `crew/moon/law.l:144-182`, `xtra = ()` | exists |
| the lower seam | `crew/moon/lib/mksys.l` -- holo IR as literal love data | exists, in production |
| the architectural shape | `crew/holo/` -- assembly as love data | exists; clay is this one level up |

what was missing: a NAME, a SHOWER (AST -> C text; `crew/moon/fmt.l` is text->text and
shares nothing with the parser), and one derivation. the first two landed in rungs 0-1
(`crew/moon/clay.l`). the derivation is `clay-tables`: `gen.l` TRUSTS the parser for `stag`
(tag -> `(fields size align)`, offsets already laid) and `sigs` (name -> `(rettype
(paramty..) variadic?)`), and neither is derivable from the AST. it moved to rung 6, where
`cc-clay` gives it a consumer.

and the writing above understates the same gap in the OTHER direction, which is what the
shower ran into: the tables are not merely underivable from the AST, they hold information
the AST never had. see the header.

the name `clay` is free in the tree. `cast` is doubly taken -- as the test helper `(cast
"int main…")` in law.l and as the AST tag for a C cast.

## the criterion

`port/inle/mkvec.l:3` states it, about assembly:

> generated rather than transcribed: the 32 x86 stubs and the 16 aarch64 vector slots were
> `.macro`/`.rept` loops in GAS, and a love loop says the same thing without an assembler's
> macro language.

**generate the C that repeats itself because C expresses the abstraction badly.** not the
biggest block -- the most repetitive one. two constraints follow:

1. **`cparse` is lossy today.** `crew/moon/lex.l` has no comment token (kinds are `kw id num
   str p`) and `cpp.l` runs before `parse.l`, so parse->clay drops every comment and fully
   expands every macro. narrative C cannot round-trip and stay itself; table-shaped and
   repetitive C can. a fact about today, not a law -- see "comments, in two halves".
2. **love.h's macros are the VM's discipline.** `Have`/`Continue`/the tail-threading are
   macros, and `make vmret` exists because that structure is fragile. the seam sits BELOW
   the `lvm_` wrappers, in the pure helpers -- which is also where the house rule already
   put them (⚠ never put scratch in an `lvm_`). this rules out the VM loop (780 lines, 97
   threading macros, 169 register refs), the GC (522 lines -- it IS the heap), `c0`/`ev`
   (whose function SIGNATURES are macro-generated by `Cata()`/`Ana()`), and the nif registry
   (`mmap`, `__builtin___clear_cache`, hand-laid machine code).

## the slate

the "array math" instinct is HALF RIGHT and the "p0" instinct is INVERTED. worth stating
both, because both are the obvious guesses.

the galaxy block (`love.c:7661-9105`) is 1445 lines, the largest contiguous region in the
file -- but it is seven alternating sub-blocks and only ~230 lines are pure; the rest is 151
VM-macro uses, 177 `Sp[]/Ip/Hp` refs, 31 `lvm()` definitions. take the pure part, not the
section.

p0 is not a target at all: doc/reader.md:510 says that arc is CLOSED, and it closed by moving
code OUT of C. what is left is 28 lines at `love.c:4346-4373` that are almost purely
GC-threading (`gxl`, `gxr`, manual stack rollback), delegating all real lexing to
`ioread1str`/`ioread1sym`, which doc/reader.md:453 says are deliberately KEPT. no pure
computation, no repetition, no payoff.

four candidates survive, and they prove different things.

### A. dtoa -- the emitter's proving ground. `love.c:3875-3990`, 116 lines

the only section scoring zero on all three impurity meters (vm=0, regs=0, heap=0). imports
TWO functions (`ioputc`, `ioputs`) and three macro constants; exports ONE symbol
(`ai_dtoa2`, 7 call sites). zero love.h macros. C surface: loops, ints, fixed-size local
arrays, one `goto`, one compound literal + union pun.

start narrower still -- `dg_mul2`/`dg_mul5`/`dg_cmp`/`dg_expand` at **3889-3910, 22 lines,
zero imports**, the smallest possible end-to-end.

its oracle is the best in the tree: an EXHAUSTIVE sweep of all 2^32 `float` bit patterns,
printed by the original and the generated build, byte-comparing the output STRINGS.
`test/roundtrip.l`, `test/show.l`, `test/math.l` already stand.

what it does NOT give is a verification story. dtoa is an algorithm, not a table, so
`clay2coq` would mean shallow-embedding imperative loops over mutable arrays into Rocq --
a research project, not a rung.

⚠ **and its TENURE is in question, which is why it moved down the order.** `dg_*` and
`ai_dtoa2` are called only from printer paths, and the printer is itself queued to be
PULLED INTO LISP (the reader's dual: a tiny `show0` in C for the death face, the full
shower in love). if dtoa goes with it, clay-ing dtoa first is throwaway. the deciding
question is small and worth settling before spending the afternoon: **does the `show0`
floor need to print floats at all, or can the death face be integer-only?** if it needs
floats, dtoa stays in C forever and this rung is safe. the bignum helpers carry no such
question -- limbs cannot leave.

### B. the bignum magnitude helpers -- the best verification payoff. `love.c:6946-7090`, 145 lines

`love.c:6934` states the law: "all multi-limb work lives in `ai_noinline` magnitude helpers
operating on raw `ai_limb` arrays (no l pointers, no allocation)". ten file-statics, eight
consumers, every parameter `ai_limb*`/`int`/`bool*`, no love.h macros.

**this is where clay meets a proof that is already standing.** `proof/rocq/big.v` models the
bignum lane against stdlib `Z` with a proven decimal codec, extracts it, and `big_drive`
FUZZES love's limbs against it. if the limb helpers are clay, `clay2coq` can put the
IMPLEMENTATION into Rocq -- upgrading an extraction-and-fuzz bridge to a proof about the code
that ships. the largest single verification step this plan offers.

fence two things, and the fence turns out to be free. `div2by1` (7018-7030) holds the
file's only `__asm__` (`divq`), and the types are `unsigned __int128` on 64-bit -- both
behind `limb_bits == 64`, which mooncc never takes (cpp.l:410, above). so:

* the mooncc leg **never sees** the asm or the `__int128`, and needs no fence at all.
* the two legs of G3 compile **different source**, so state that differential
  BEHAVIOURALLY -- same answers over the same inputs -- never as byte-identical objects.
  the byte-exact framing works for dtoa and the matrices; it does not work here.
* and this is an argument FOR the section rather than against: the 32-bit limb path is the
  one mooncc exercises and gcc never does on this box, which is precisely the
  untested-lane shape the mooncc differentials keep finding bugs in.

`7164-7213` is asm-free and is the place to start. gates: `test/bignum.l`, `test_big`.

### C. `vbin_fill` + the lane table -- one datum, two consumers. `love.c:8393-8495`, 103 lines

four near-identical blocks (float-arith, float-cmp, int-arith, int-cmp), each a `switch` over
`enum vop` (`love.c:177`), each body written through a `#define VBF(E)` that is `#undef`'d and
REDEFINED SIX TIMES INSIDE THE FUNCTION, because C cannot abstract "the same loop nest with a
different expression":

```c
#define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { ai_flo_t av = aarr?ap[p]:sa, bv = barr?bp[p]:sb; rp[p] = (E); } } while (0)
switch (op) { case vop_add: VBF(av+bv); return; case vop_sub: VBF(av-bv); return; ... }
#undef VBF
```

`.macro`/`.rept` in GAS wearing a different hat -- the criterion's exact shape. pure:
`ai_noinline static`, raw pointers into a PRE-allocated result, no `Have`, no `Continue`, no
allocation. its neighbour, the broadcast plumbing at **8343-8392 (50 lines)**, is purer still
and is the cleanest code in the array section.

it also carries an unverified claim that wants to be a theorem: the comment at 8398 asserts
the fast path is BIT-IDENTICAL to the general odometer loop below it -- "same reads (raw
f64/i64), same op (matches vop_flo/int/cmp)" -- and nothing checks it. doc/verify.md's own
pattern: a gate that measures a shape earns a theorem that OWNS the shape.

⚠ those in-function macros mean generated C writes the loop out ~11 times, so BYTE-comparison
against the original is structurally impossible. the AST-vs-AST oracle is immune (cpp runs
first, so both sides are post-expansion) -- which is what G1/G2 use anyway.

### D. the `+`/`*` dispatch matrices -- the only rung that DELETES a trusted component. `love.c:6184-6268`, 85 lines

pure static data, built by 11 `#define`s that are `#undef`'d after use -- repetitive for the
same reason. converting it DELETES `tools/mxdump.c`, the 47-line TU that `#include`s love.c
whole to recover its own tables, and with it the "an unrecognized lane pointer prints
UNKNOWN" failure mode: in clay the lane's symbol IS the datum, so the dump step and the guess
step both vanish and `tools/mx2coq.l` reads clay directly. no runtime behaviour, so its gate
is the existing theorem (`test_mx`), not a differential -- a unit test of the emitter's
designated-initializer and fn-ptr-array handling.

what makes it first rather than last is the BRIDGE it moves. today mx.v is bridge shape 3
(differential against a dump): the C is the source of truth and the proof reaches it through
a TU that includes love.c whole. after, it is bridge shape **1** -- shared source, one text
two checkers, doc/verify.md's strongest form and the discipline `gen.v` already lives under.
that is a change in the KIND of assurance, not the amount, and it is the cheapest one
available.

### the order

**the matrices, then bignum, then dtoa, then `vbin_fill`.**

this INVERTS the order first written here, and the reason is that the first ordering ranked
by "where is the emitter's oracle strongest" while the goal is "where does the trusted
surface shrink". those disagree. love.c's LINE COUNT is near its floor and doc/verify.md
says so correctly; its TRUSTED surface is not, and clay's whole payoff is there -- turning C
from text nobody proves into a rendering of a datum the theorem is about.

so: repay the visible debt FIRST, because it is the only rung that deletes a trusted
component today and the only one that upgrades a bridge; then take the verification payoff
where a proof is already half-built and the code is certain to stay in C; then dtoa, once
its tenure question is answered; then the clean demonstration of one datum feeding both.

the emitter still gets proven early -- G1's round-trip over `test/cc/` does that on rung 1,
before any of the four, and over far more C than dtoa contains.

## the seam

the shared datum sits ONE LEVEL ABOVE the AST. clay is the RENDERING; the source of truth is
a small table, and a love loop turns it into everything. mkvec.l's shape exactly: one
scaffold, a payload per lane.

for `vbin_fill` the table is one row per (op, domain):

```love
; op        domain  result   the expression, as clay
(vop-add    'flo    'flo     (bin + (var "av") (var "bv")))
(vop-sub    'flo    'flo     (bin - (var "av") (var "bv")))
(vop-add    'int    'int     (cast long (bin + (cast ulong (var "av")) (cast ulong (var "bv")))))
(vop-lt     'flo    'mask    (cond (bin < (var "av") (var "bv")) (num 1) (num 0)))
```

```
                       the table  (love data)
                              |
              .---------------+----------------.
              |               |                |
        clay (the AST)   clay2coq         clay (the AST)
              |               |                |
         clay-show       vop_denote        cc-clay
              |            + theorems          |
           C text                            holo
              |                                |
         system cc  ------ differential ---->  .o
```

* **generation** -- the loop expands the table into clay (one loop nest per domain x cmp
  group, a `switch` arm per row), then `clay-show` renders C text.
* **verification** -- the SAME ROWS become `vop_denote : vop -> R -> R -> R` in Rocq, and the
  theorem to reach for is the one the comment already claims. both C and model regenerate
  from the table on every gate run, so they cannot drift. this is doc/verify.md's bridge 1
  (shared source: one text, two checkers) -- the discipline `gen.v` already lives under.
* **native, no C text** -- clay also goes straight to `(cgen-obj …)` (`gen.l:5306`), giving a
  third leg the tree does not have: the same clay compiled two ways must agree.

for dtoa and the bignum helpers there is no table; the AST IS the datum and the shared input
to both consumers is the clay itself. weaker, still sound -- and why `vbin_fill` earns its
place even though it is third.

**what stays trusted, honestly.** the theorem is about the table; the binary is about what a
C compiler did to clay's rendering of it. the bridge proves "the table says X" ∧ "the C is a
faithful rendering". the C compiler remains unproven -- the same trusted-base story
doc/verify.md already tells about moon. state it this way or not at all.

## the gates

* **G1 faithfulness** -- `(cparse (clay-show c)) == c`, compared STRUCTURALLY on the parsed
  AST, never as a string compare of the C text (doc/reader.md:149 -- twice now the printer
  has been the thing standing in front of the bug). run over all 110 files of `test/cc/`,
  already in the tree: that makes "expresses arbitrary C" empirical rather than claimed.
* **G2 conversion equivalence** -- for the section being replaced, `(cparse
  original-section.c) == clay` modulo a stated normalization (strip `note` nodes). this makes
  a migration CHECKABLE instead of a hand-port. runs once at conversion, kept as a law.
* **G3 differential** -- the `test/gate/ulp.sh` shape: build the generated C with the system
  cc AND with mooncc, link both into one harness, require byte-identical reports. ⚠ that is
  a claim about the two builds' ANSWERS, and it holds only where both compile the same
  source after preprocessing -- section B is the exception (`__int128`, above), where the
  two legs run different algorithms and only the behaviour can be compared. then the
  `test/gate/ccarch.sh` shape across arm64 and riscv64, because `255e8074` proved TARGETS ARE
  NOT REDUNDANT (with `40a5a2b7` reverted, arm64 caught the bug while x86-64 and riscv64 both
  answered correctly by accident). for dtoa, add the exhaustive 2^32 float sweep.
* **G4 the theorem** -- `tools/clay2coq.l`, sibling of `spec2coq.l`/`mx2coq.l`. axiom-free,
  tracked in git, regenerated every run, skips loudly without coqc.

the differentials are not decoration. five mooncc codegen bugs got past a green gate in the
two days before this was written -- `3a9ce226` (4th parameter lost; `am_sin` segfaulted for
every |x| >= 2^19), `68ee440a` (u64->double converted signed, 1609 ulp), `40a5a2b7` (uint
result not wrapping at 2^32), `f549e52d` (double->integer destination), `f9151bc0` (negative
zero) -- and every one was found by a differential, none by the corpus.

## comments, in two halves

two separable jobs with very different costs. do the first; keep the second in reserve.

**emitting -- cheap, required, rung 0.** a `(note "…")` node that `clay-show` renders as a C
comment. without it, generated love.c sections arrive stripped of the prose the tree runs on.
`note` nodes are AUTHORED in the generator, exactly as mkvec.l carries its narrative in the
love that lays the assembly. generated `.c` gets an "edit the table, not this file" banner, is
CHECKED INTO GIT, and is regenerated by a gate that fails on drift -- the discipline
`proof/rocq/gen.v` already lives under.

**capturing -- expensive, optional, one customer TODAY.** making `cparse` PRESERVE comments
means `lex.l` attaching trivia to tokens and `parse.l` threading it onto every node: the
concrete-syntax-tree problem across 1692 lines of recursive descent. the lexer half is
tractable -- token arity already varies (`('p (string c) line 1)` carries a glued flag), so
consumers tolerate a 4th slot, and trivia must ride ATTACHED TO THE NEXT TOKEN, never as a
token kind, or every parse rule has to skip it. the parser half is the cost, and `cpp.l` is
genuinely awkward: C strips comments BEFORE preprocessing, so a comment inside a macro body
has no well-defined output position.

moonfmt does not need capture as it stands, and that is a fact about today, not a verdict.
`crew/moon/fmt.l:2` -- "shares NOTHING with the parser/codegen: it reads text and writes text,
a pure line-preserving pass… leaves every deliberate line-break, continuation-alignment, macro
body and comment EXACTLY as written." `libra fmt` sits the same way for .l (doc/libra.md:160)
and moonfmt's typedef harvest is a textual scan (doc/moon-next.md:191). the text lane is what
the reindenter needed to do its one job; it is not a decision that the formatter must stay
there, and moonfmt is expected to grow.

**that is the thing to watch, because it decides the cost.** the day moonfmt wants to do more
than reindent -- respace across line breaks, rewrap a table, reflow a signature -- it needs
the structure, and a formatter on the AST needs comments IN the AST for exactly the reason
this section exists. at that point capture has two customers and the parser work is shared,
not clay's alone. so: don't build capture for clay's sake, and don't write it off either --
check where moonfmt is before pricing it.

with one customer, capture buys lossless AUTOMATIC migration of hand-written narrative C --
the thing that would make "love.c as a generated artifact" reachable. every target on the
slate is table-shaped or algorithmic code whose comments you would rewrite anyway, which is
why it stays optional here rather than blocking.

## the rungs

0. **name it and law it.** `crew/moon/clay.l`, a registered module (`(use 'clay)`). the node
   grammar as data -- top `prog fn proto gdecl xdecl tdef`; stmt `blk decl sdecl ret if while
   for do switch case dflt brk cont goto lbl expr nop asm`; expr `num flo str var bin un asn
   post cond comma call deref addr dot cast szof init dfield didx clit land lor vastart vaarg
   vaend`; types the primitives plus `(ptr t) (arr t n) (struct tag) (fn ret)`; plus the new
   `note`. `clay-ok?`, a validator, because `gen.l` currently TRUSTS its input. laws in
   `crew/moon/law.l`. honor the `gripe` protocol (`(1 …)` / `('gripe file line col msg)` /
   `()`, doc/moon-diag.md). `make test_moon` stays green; no behaviour change.
   `clay-tables` (deriving `stag` + `sigs`, REUSING `playout` at `parse.l:302-339` rather
   than reimplementing C layout rules) was written here first and MOVED TO RUNG 6: nothing
   consumes it until `cc-clay` exists, and a derivation with no consumer cannot be gated.
1. **`clay-show` and G1.** AST -> C text, plus the round-trip gate over `test/cc/`. smoke it
   on `ai_T[]` (`love.c:6422`, a five-line designated-initializer table) first. new
   `test_clay` in `test/test.mk`, added to `test_slow`.
2. **the dispatch matrices; delete `tools/mxdump.c`.** LANDED. `tools/mx.l` is the
   table; `mx.h` is laid from it through clay and `#include`d by love.c (its first
   generated region); `tools/mx2coq.l` reads the same table instead of a dump, so
   mx.v's bridge moved from shape 3 to shape 1. the dumper, its `$(CC)` step, the
   function-pointer comparison and the UNKNOWN case are all gone. net C **-54**
   lines. the migration was checked the G2 way before love.c was touched -- the love
   table reproduced all 512 cells the C had compiled -- and the drift check lives in
   `test_clay` (regenerate, `cmp`), sabotage-proven.
3. **the bignum magnitude helpers, and `clay2coq.l`.** asm-free at `7164-7213` first, then
   `6946-7090`. meet `big.v` at the seam. remember both legs run different limb widths.
4. **dtoa** -- once the `show0` float question is answered. `3889-3910` first, then
   `3875-3990`. G2 against the current text; G3 with the exhaustive float sweep.
5. **`vbin_fill` + the lane table.** the one-datum-two-consumers demo, and the theorem 8398
   has been claiming for free.
6. **the lawed injection seam.** `cc-clay` in `crew/moon/moon.l`, sibling to `cc-parse`
   (`moon.l:128-148`): clay in, `clay-ok?`, `clay-tables`, `cgen-obj`. this is what makes clay
   a frontend target OTHER MODULES can share, and it enables G3's third leg. document it in
   doc/moon.md, whose architecture section names only backend seams today.

open, not committed: the rest of the `_fill` family (`vmap1_fill` 7997, `vmap2_fill` 8546,
`cplx_fill` 8751, `cbin_fill` 8784, `cplx_pow_fill` 8843, `cplx_build_fill` 8902, `cpart_fill`
8952, `carg_fill` 9077), then `bit_slow`'s limb-wise bit ops (`love.c:6289` -- doc/reader.md:511
lists these as unfinished tower work, where negatives should SCARE rather than answer `()`, so
clay would land the fix and the generation together). "love.c as a generated artifact" stays a
conversation; comment capture is what would have to be paid for first.

## running the gates

`make test` is the DEV gate (~20s, every edit) -- host + love0 must BOTH print the zz-fin
summary, love0 exactly twice. `make test_slow` is the MERGE gate, before publishing only
(`make test_all` is gone as of `c376bfa5`). between them, the individual `test_*` covering
what you touched. `out/host/love crew/libra/libra.l <file>` on every .l -- silence is clean.
`make test_selfhost` and `make test_raw` (opt-in) still compile love.c from scratch, so
generated C must survive that path. never assert on `(show x)` as a value test. and watch the
clock: a generator that crawls is a bug announcing itself.
