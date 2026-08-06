# clay -- C as love data

owning C the way holo owns assembly: a C AST written as love data, a shower that renders
it to C text, and one datum feeding both the generated C and its Rocq model.

**the goal is full translation.** `love.c.l` is the source; `love.c` is a rendering.
nothing in the file is out of scope -- the VM loop, the GC, the heap-image codec, `c0`/`ev`
and the nif registry included. what was a shortlist of repetitive blocks is now an ORDER OF
WORK over the whole file.

two constraints earlier drafts treated as binding are lifted, and both were load-bearing:

* **comments are not a requirement of the generated C.** the prose lives in `love.c.l`;
  the generated `love.c` need not carry it. this retires the comment-capture cost centre
  entirely -- see §comments.
* **nothing is ruled out by the macro discipline.** measured, the macros are mostly not a
  limit at all, and hand-paring them would be the inverse of the criterion -- see §macros,
  which is the section to read if you read one.

the payoff sought is still VERIFICATION. `doc/verify.md` says love.c is "near its floor"
for shrinking and the remaining lever is verifying pieces against references. its LINE
count is near its floor; its TRUSTED surface is not, and that is where clay pays. a love
-> JS backend is a second consumer of the same datum rather than a second implementation,
and `cc-clay` gives a native leg with no C text at all.

**rungs 0 through 2d have landed** (`crew/moon/clay.l`, `make test_clay`, `tools/mx.l` +
`mx.h` + `kinds.h`); the rest is unbuilt.

## the state, measured

on the current tree -- `love.c` 8232 lines, `love.h` 505, `crew/moon/clay.l` 479.

`make test_clay` reads **63 round-trip, 51 inexpressible, 0 unparsed, 114 files**.

⚠ **that 51 is not clay's ceiling.** it measures `cparse`'s lossiness on the way IN, not
clay's grammar on the way OUT. a typedef, a struct definition, an enum and a
`_Static_assert` all land as the empty marker `(tdef)`; a prototype keeps only its name; a
function definition has no RETURN TYPE; `static`/`const` are gone. that information is
real -- it lives in the side tables (`stag`, `sigs`) and the parse state, which `gen.l` is
HANDED and a shower is not.

so clay is a SUPERSET of cparse's output with the missing slots APPENDED, and for
GENERATION the binding constraint is the EMIT grammar, which is broader and cheap to
widen. when a form is missing the move is to add an emit-only node, as rungs 2b-2d did
with `note`, `edef` and `sdef` -- never to teach `parse.l` to round-trip it first. adding
emit-only nodes must leave the 63/51 reading untouched; that is the check that they really
are emit-only.

## the criterion

`port/inle/mkvec.l:4-6` states it, about assembly:

> this one is generated rather than transcribed: the 32 x86 stubs and the 16 aarch64
> vector slots were `.macro`/`.rept` loops in GAS, and a love loop says the same thing
> without an assembler's macro language.

that is the REASON, not a filter. under full translation it stops selecting a shortlist
and starts deciding, per macro, where the abstraction should live.

## macros -- the question this doc used to get wrong

earlier drafts said the macro discipline "rules out the VM loop (780 lines, 97 threading
macros), the GC, `c0`/`ev`, the nif registry". measured on the current file, that is
mostly not so. `cpp` runs before `parse`, so cparse never sees a macro -- but **we are
emitting, not parsing**, and most of love.c's macros are syntactically FUNCTION CALLS,
which clay already emits today as `(expr (call (var "Have") ((num 3))))`:

| macro | sites | | macro | sites |
|---|---:|---|---|---:|
| `putcharm` / `getcharm` | 285 | | `Answer` / `Answerp` | 85 |
| `Have` / `Have1` | 127 | | `Push` | 83 |
| `Width` | 112 | | `Pack` / `Unpack` | 78 |
| `Ap` | 86 | | `Continue` | 77 |
| `countof` / `avec` / `Resume` | 39 | | `Next` / `Nextp` | 49 |
| `__builtin_*` | 31 | | **total** | **1052** |

**1,052 invocation sites need no new grammar and no change to `love.h`.** the generated
`love.c` `#include`s `love.h` and uses its macros exactly as the hand-written file does.

so the move is not "remove macros" but **move each abstraction up one level**, choosing
per macro between three routes:

* **(a) leave it call-shaped.** free, and covers the table above.
* **(b) generate it away.** the X-macros `nifs(_)` / `insts(_)` (`love.c:617-692`, 5
  expansion sites) expand differently at each site because C cannot abstract "the same
  list with a different consumer". the love loop replaces them and the macro is DELETED.
  this is the criterion's exact shape and the one place a macro genuinely goes.
* **(c) add an emit-only clay node.** the rung 2b-2d precedent, for the shapes that are
  not call-shaped.

### ⚠ hand-paring is the inverse of the criterion

`lvm(lvm_add)` expands to `ai_noinline ai_noicf struct ai *lvm_add(struct ai *restrict g,
union u *Ip, ai_word *Hp, ai_word *restrict Sp)`. writing that out at **184 definition
sites** ADDS the most repetitive text in the file -- precisely what the criterion says to
generate.

and `ai_musttail return` at **311 sites** is a discipline, not noise: `love.h:64-77` says
an opportunistic miss is one frame per dispatch and a stack overflow down some long read,
which is why `make vmret` disassembles the binary to check it. do not thin these out to
suit the shower. GENERATE them.

## what clay cannot say yet

everything, counted:

| shape | sites | route |
|---|---:|---|
| `ai_musttail return X;` | 311 | a prefix slot on `ret`, or an attribute |
| `ai_noinline` / `ai_noicf` / `ai_inline` | 234 | an attribute slot on `fn` |
| `lvm(...)` definitions | 184 | attributes + `restrict`, then emit the expansion -- or a declarator-macro node |
| `__attribute__((section/used/weak))` | 9 | the same attribute slot |
| `unsigned __int128` | 7 | likely already sayable via `(named "unsigned __int128")` |
| `restrict` | 5 | a qualifier beside the existing `(const t)` |
| flexible array member (`char bytes[]`) | 5 | `(arr t ())`, or a node |
| `_Static_assert` | 4 | one top-level node |
| `__asm__` | 1 | **already sayable** -- clay has `(asm 4 4)`, matching `parse.l:1171`'s 4-slot form |

**five node shapes cover ~750 sites.** a strikingly concentrated gap for 8,232 lines.

⚠ attributes must be emit-only BY NECESSITY, not merely by choice: `parse.l:116-121`
balance-SKIPS a trailing `__attribute__((..))` / `__asm__(..)` run rather than
representing it ("the codegen owes nothing"). so an attribute clay prints cannot come back
through a parse, exactly like `note`/`edef`/`sdef`.

### the `__int128` caveat is void, and this matters

earlier drafts said `unsigned __int128` is "a piece of C moon cannot say at all", that
`cpp.l` leaves `__SIZEOF_INT128__` undefined on every target, and therefore that the
bignum section's two build legs compile DIFFERENT SOURCE and can only be compared
behaviourally.

**all of that is now stale.** `moon.l:130` defines `__SIZEOF_INT128__=16` for x64,
`parse.l:86,93` carry `__int128` in the type specifiers with `u128`/`i128` types, and
`gen.l:1073` is the d128 lane. love.c's limb seam (`love.c:48`) gates on
`UINTPTR_MAX == UINT64_MAX && defined(__SIZEOF_INT128__)`, so **mooncc on x64 takes the
same 64-bit limb path gcc does**, `divq` `__asm__` lane included. the byte-identical
framing for G3 is back on the table for the bignum section, and the "state it behaviourally"
carve-out is withdrawn. every other target still keeps the portable 32-bit branch, which
remains the untested-lane shape the mooncc differentials keep finding bugs in.

## the preprocessor -- the one real design decision

`love.c` carries **154 `#define`s** and ~92 conditional directives (`#if` 21, `#ifdef` 7,
`#ifndef` 9, `#else` 13, `#elif` 5, `#endif` 37). two of the conditionals are
architecture, not detail:

* `#if ai_tco` -- the threaded-vs-trampoline split (`love.h:46-94`), which the wasm build
  depends on (`-Dai_tco=0`) and which `love0` is built under.
* `#ifdef __wasm__` / `#if __STDC_HOSTED__` -- the freestanding/hosted split.

`love.c` is ONE TEXT compiled for many targets, and `test_fixpoint` requires the mooncc
rebuild to be byte-identical. so:

* **(a) clay gains emit-only `#if` / `#define` nodes.** keeps one-text-many-targets, keeps
  the generator target-blind. **recommended.**
* **(b) love generates per-target C.** breaks the single-text property, multiplies the
  committed artifacts, and moves the target matrix into the generator. not recommended.

under (a) the 154 shrinks on its own: many `#define`s are constants and small accessors
(`datp`, `chainp`, `two`, `str`) that are call-shaped at every use site, so the generator
either emits them as declarations or leaves them in `love.h` untouched.

this is the gating decision for whole-file capture. nothing before it depends on the answer.

## comments

**capture is not needed.** `love.c.l` holds the prose; the generated `love.c` carries
none, and you read the `.l`. that removes the concrete-syntax-tree problem across
`parse.l`'s 1,692 lines of recursive descent, which earlier drafts correctly priced as the
expensive thing standing between per-region migration and the whole file.

`note` stays, emit-only, for the banner every generated region owes: "edit `love.c.l`, not
this file". `lex.l` has no comment token and `cpp.l` runs first, so a note is AUTHORED in
the generator -- exactly as `port/inle/mkvec.l` carries its narrative in the love that lays
the assembly.

one thing worth still watching, for its own sake rather than clay's: `crew/moon/fmt.l:2`
says moonfmt "shares NOTHING with the parser/codegen: it reads text and writes text". the
day it wants to respace across line breaks or rewrap a table it needs the structure, and a
formatter on the AST needs comments IN the AST. at that point capture has a customer of its
own. don't build it for clay, and don't write it off.

## the order of work

incremental, each rung shippable, `love.c` staying hand-written until its region converts
-- exactly how `mx.h` landed.

1. **the five node shapes** -- attributes, `restrict`, the `ret` prefix, `_Static_assert`,
   flexible array members. lawed in `test/host/clay.l`. no `love.c` change; G1 must hold at
   63/51.
2. **settle the preprocessor** -- (a) or (b) above, and add `cpp-if` / `cpp-def` if (a).
3. **the X-macro registry** -- `nifs` / `insts` (`love.c:617-692`), route (b): the first
   region where a macro is deleted and the love loop is the better abstraction.
4. **the alpha-equivalence cluster** -- `3574-3609` (partial-application introspection:
   `fn_partialp`/`fn_base`/`fn_arg`/`fn_src`), `3609-3737` (de Bruijn canonical lambda
   printing), `5979-6032` (`salpha` + `shash`), `6038-6136` (the beta bridge:
   `clo_load`/`nf_hash`/`nf_walk`/`clo_eq`). **310 contiguous lines scoring zero on every
   purity meter** -- the largest such block in the file, and absent from every earlier
   slate. pure computation, no allocation, so the generated C is BYTE-comparable;
   `test/spec.l` §comparing-functions (149-165) and §reduction (167-179) already pin every
   law, with `proof/rocq/spec.v` under them.
   ⚠ the C is deliberately CONSERVATIVE -- a captured closure vs. a source lambda stays
   unbridged (`love.c:6036`), so `(: adder (\ a (\ b (+ a b))) ((adder 5) = (\ b (+ b 5))))`
   answers 0. reproduce that bail exactly; "improving" on it reads as a love bug at G2.
5. **the bignum magnitude helpers** -- `6295-6415` (raw magnitude primitives), `6415-6488`
   (operand loading + tier conversions), `6599-6643` (resumable multiply). the banner at
   `love.c:6295` states the contract: raw magnitude primitives over little-endian limb
   arrays, callers passing normalized inputs and normalizing outputs via `ai_big_canon` --
   no love pointers, no allocation.
   **this is where clay meets a proof that is already standing.** `proof/rocq/big.v` models
   the bignum lane against stdlib `Z` with a proven decimal codec, extracts it, and
   `big_drive` FUZZES love's limbs against it. if the limb helpers are clay, `clay2coq` can
   put the IMPLEMENTATION into Rocq -- upgrading an extraction-and-fuzz bridge to a proof
   about the code that ships. the largest single verification step this plan offers.
   gates: `test/bignum.l`, `test_big`.
6. **dtoa** -- `dg_mul2`/`dg_mul5`/`dg_cmp`/`dg_expand` at `3745-3765` (21 lines, zero
   imports, the smallest possible end-to-end), then `ai_dtoa2` at `3766-3841`. its oracle
   is the best in the tree: an EXHAUSTIVE sweep of all 2^32 `float` bit patterns, printed
   by the original and the generated build, byte-comparing the output STRINGS.
   `test/roundtrip.l`, `test/show.l`, `test/math.l` already stand.
7. **`vbin_fill` + the lane table** -- `love.c:7596`, whose body redefines `#define VBF(E)`
   six times (`7611-7617`) because C cannot abstract "the same loop nest with a different
   expression". `.macro`/`.rept` in GAS wearing a different hat. the one-datum-two-consumers
   demo -- see §the seam -- and the theorem `love.c:7602` has been claiming for free
   ("mixed/bignum/broadcast falls through to the general loop; results bit-identical"),
   which nothing checks.
   ⚠ those in-function macros mean generated C writes the loop out ~11 times, so
   BYTE-comparison here is structurally impossible; the AST-vs-AST oracle is immune.
   then the rest of the family: `vmap1_fill` 7255, `vmap2_fill` 7734, `twin_fill` 7932,
   `cbin_fill` 7958, `twin_pow_fill` 8011, `twin_build_fill` 8062, `cpart_fill` 8108,
   `carg_fill` 8206. and `bit_slow` (`love.c:5453`, used 5762), where `doc/io.md` lists
   unfinished tower work -- negatives should SCARE rather than answer `()` -- so clay lands
   the fix and the generation together.
8. **the GC and the heap-image codec** -- `875-1347` (473 lines: `evac_*`/`copy_*` per kind,
   `gen_wb`, `gen_minor`, `gen_major`, `gen_grow`, `gen_please`, `gcp`) and `4916-5240`
   (323 lines: the image codec). both score ZERO on every purity meter. earlier drafts
   excluded the GC on judgment ("it IS the heap"); **that exclusion is withdrawn.** the
   meter and the judgment disagreed and the meter is the one that can be checked.
9. **the VM loop and `c0`/`ev`, last** -- `1349-1938` (`c0`, whose function SIGNATURES are
   macro-generated by `Cata()`/`Ana()`) and `1976-2941` (the VM). they need step 1's nodes
   plus a declarator-macro story, and they are where a mistake is least visible.
10. **the lawed injection seam** -- `cc-clay` in `crew/moon/moon.l`, sibling to `cc-parse`
    (`moon.l:128`): clay in, `clay-ok?`, `clay-tables`, `cgen-obj` (`gen.l:6141`). this is
    what makes clay a frontend target OTHER MODULES can share, and it enables G3's third
    leg. `clay-tables` derives `stag` + `sigs`, REUSING `playout` (`parse.l:576`) rather
    than reimplementing C layout rules. document it in `doc/moon.md`, whose architecture
    section names only backend seams today.

## the seam

the shared datum sits ONE LEVEL ABOVE the AST. clay is the RENDERING; the source of truth
is a small table, and a love loop turns it into everything. mkvec.l's shape exactly: one
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
* **verification** -- the SAME ROWS become `vop_denote : vop -> R -> R -> R` in Rocq, and
  the theorem to reach for is the one the comment already claims. both C and model
  regenerate from the table on every gate run, so they cannot drift. this is
  `doc/verify.md`'s bridge 1 (shared source: one text, two checkers) -- the discipline
  `gen.v` already lives under.
* **native, no C text** -- clay also goes straight to `(cgen-obj ..)`, giving a third leg
  the tree does not have: the same clay compiled two ways must agree.

where there is no table -- dtoa, the limb helpers, the alpha cluster -- the AST IS the
datum and the shared input to both consumers is the clay itself. weaker, still sound, and
why `vbin_fill` earns its place even though it comes later.

## the gates

* **G1 faithfulness** -- `(cparse (clay-show c)) == c`, compared STRUCTURALLY on the parsed
  AST, never as a string compare of the C text (`doc/io.md` -- twice now the printer has
  been the thing standing in front of the bug). run over all 114 files of `test/cc/`: that
  makes "expresses arbitrary C" empirical rather than claimed. currently **63 / 51 / 0**.
  ⚠ emit-only additions must not move it.
* **G2 conversion equivalence** -- for the section being replaced, `(cparse
  original-section.c) == clay` modulo a stated normalization (strip `note` nodes). this
  makes a migration CHECKABLE instead of a hand-port. runs once at conversion, kept as a
  law. rung 2 did exactly this: the love table reproduced all 512 cells the C had compiled,
  before `love.c` was touched.
* **G3 differential** -- the `test/gate/ulp.sh` shape: build the generated C with the system
  cc AND with mooncc, link both into one harness, require byte-identical reports. then the
  `test/gate/ccarch.sh` shape across arm64 and riscv64, because `255e8074` proved TARGETS
  ARE NOT REDUNDANT (with `40a5a2b7` reverted, arm64 caught the bug while x86-64 and
  riscv64 both answered correctly by accident). for dtoa, add the exhaustive 2^32 float
  sweep.
* **G4 the theorem** -- `tools/clay2coq.l`, sibling of `spec2coq.l` / `mx2coq.l`.
  axiom-free, tracked in git, regenerated every run, skips loudly without coqc. it has no
  consumer until rung 5 lands.
* **regeneration drift** -- the generated file is CHECKED INTO GIT and `cmp`'d by a gate
  that fails on drift, the discipline `mx.h`, `kinds.h` and `proof/rocq/gen.v` already live
  under. there is no chicken-and-egg: regeneration is a gate, not a build step.
* **`test_fixpoint` and `test_raw`** -- they compile `love.c` from scratch and to the byte,
  so generated C must survive both. `make vmret` on every rung touching a `lvm_`, and
  `make valg`.

the differentials are not decoration. five mooncc codegen bugs got past a green gate in the
two days before this was first written -- `3a9ce226` (4th parameter lost; `am_sin`
segfaulted for every |x| >= 2^19), `68ee440a` (u64->double converted signed, 1609 ulp),
`40a5a2b7` (uint result not wrapping at 2^32), `f549e52d` (double->integer destination),
`f9151bc0` (negative zero) -- and every one was found by a differential, none by the corpus.

## running the gates

`make test` is the DEV gate (~20s, every edit) -- host + love0 must BOTH print the zz-fin
summary, love0 exactly twice. `make test_slow` is the MERGE gate, before publishing only.
between them, the individual `test_*` covering what you touched. `out/host/love
crew/libra/libra.l <file>` on every .l -- silence is clean. never assert on `(show x)` as a
value test. and watch the clock: a generator that crawls is a bug announcing itself.

## what stays trusted, honestly

the theorem is about the datum; the binary is about what a C compiler did to clay's
rendering of it. the bridge proves "the datum says X" ∧ "the C is a faithful rendering".
the C compiler remains unproven -- the same trusted-base story `doc/verify.md` already
tells about moon. state it this way or not at all.

## what has landed

0. **name it and law it.** `crew/moon/clay.l`, a registered module (`(use 'clay)`). the
   node grammar as data -- 58 tags: top `prog fn proto gdecl xdecl tdef note edef sdef`;
   stmt `blk decl sdecl ret if while for do switch case dflt brk cont goto lbl expr nop
   asm`; expr `num flo str var bin un asn post cond comma call deref addr dot cast szof
   init dfield didx clit land lor vastart vaarg vaend`; types `ptr arr varr struct named
   const`. `clay-ok?`, a validator, because `gen.l` currently TRUSTS its input. honors the
   `gripe` protocol (`doc/moon-diag.md`).
1. **`clay-show` and G1.** AST -> C text, plus the round-trip gate over `test/cc/`.
2. **the dispatch matrices; deleted `tools/mxdump.c`.** `tools/mx.l` is the table; `mx.h` is
   laid from it through clay and `#include`d by love.c (its first generated region);
   `tools/mx2coq.l` reads the same table instead of a dump, so mx.v's bridge moved from
   shape 3 to shape 1. the dumper, its `$(CC)` step, the function-pointer comparison and the
   UNKNOWN case are all gone. net C **-54** lines.
2b. **the kind lattice the matrices are INDEXED by.** `enum q` was hand-written and
   `mx-kinds` was a transcription of it, coupled by a comment and checked by nothing. now
   `kinds.h` is laid from the same roster the grid is, and `KN` is the roster's own length
   rather than a number someone counted. the generated line came out BYTE-IDENTICAL to the
   hand-written one. cost: one new clay form, `(edef NAME (CONSTS..))`, emit-only.
   ⚠ the embedding surface is TWO files now -- `mk/install.mk` ships `kinds.h` beside
   `love.h`, and an install missing it does not compile.
2c. **the rep roster, split off the dispatch one.** `enum q` was answering two questions;
   only nine members were ever `ai_typ` answers. now `enum d` is laid from its own roster,
   so the exhaustive switches drop their defaults and a tenth data sentinel is a COMPILE
   error at every site rather than a runtime trap. `KVec` left `enum q` with it, taking 31
   unreachable cells out of each grid and 62 out of `mx.v`'s square.
2d. **the aggregate, and the struct REFERENCE rule beside it.** `(sdef TAG FIELDS
   ['union])` defines a struct; the refusal is narrowed to parse's anonymous `.anon0`
   (`parse.l:614`), which is not a C identifier. FIELDS take the shape `pmembers` already
   answers, so the day parse.l fills `(tdef)` the member list is the one it hands back.
   ⚠ a named tag does not weaken G1: a file that DEFINES the struct it names still carries
   the `(tdef)` that refuses, so a tag clay prints without defining is one the source never
   defined either. laws in `test/host/clay.l`.

## open

* **the preprocessor** (§above) -- the gating decision for whole-file capture.
* **`love.c.l`'s own shape.** one file or a directory of regions? the generated `love.c` is
  one text either way, but the `.l` side has no constraint forcing it, and the answer
  decides whether a rung's diff is readable.
* **`clay2coq.l`** -- what turns any of this into a theorem rather than a tidier build. no
  consumer until rung 5.
