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

**rungs 0 through 6 have landed** (`crew/moon/clay.l`, `make test_clay`, `mx.l` +
`mx.h` + `kinds.h`, the order of work's rung 1 -- the five node shapes -- its rung 3, the
nif registry, its rung 2, the preprocessor nodes, and four more node shapes plus
`tools/clay-g2.l`); the rest is unbuilt. one rung was landed and REVERTED, and it is the
one that reshaped this doc -- see §the criterion.

## the state, measured

on the current tree -- `love.c` 8232 lines, `love.h` 540, `crew/moon/clay.l` 601.
⚠ re-measure these rather than quoting them; the previous figure sat here stale by 84.

`make test_clay` reads **63 round-trip, 55 inexpressible, 0 unparsed, 118 files**.

⚠ **that 55 is not clay's ceiling.** it measures `cparse`'s lossiness on the way IN, not
clay's grammar on the way OUT. a typedef, a struct definition, an enum and a
`_Static_assert` all land as the empty marker `(tdef)`; a block-scope declaration that
declares no OBJECT -- a bare `struct S {..};`, or a function declaration -- lands as the
equally empty `(decl ())`; a prototype keeps only its name; a
function definition has no RETURN TYPE; `static`/`const` are gone. that information is
real -- it lives in the side tables (`stag`, `sigs`) and the parse state, which `gen.l` is
HANDED and a shower is not.

so clay is a SUPERSET of cparse's output with the missing slots APPENDED, and for
GENERATION the binding constraint is the EMIT grammar, which is broader and cheap to
widen. when a form is missing the move is to add an emit-only node, as rungs 2b-2d did
with `note`, `edef` and `sdef` -- never to teach `parse.l` to round-trip it first. adding
emit-only nodes must leave the 63/55 reading untouched; that is the check that they really
are emit-only.

## the criterion -- GENERATE, DON'T TRANSCRIBE

`port/inle/mkvec.l:4-6` states it, about assembly:

> this one is generated rather than transcribed: the 32 x86 stubs and the 16 aarch64
> vector slots were `.macro`/`.rept` loops in GAS, and a love loop says the same thing
> without an assembler's macro language.

**this is the FILTER, and earlier drafts of this doc said it was not.** they ranked the
slate by purity -- "where is the emitter's oracle strongest" -- and purity is not the
question. a region earns clay when there is something to GENERATE: a repetition C cannot
abstract, which in practice means a macro doing a job a macro should not, or a table.
where C's own abstraction has NOT run out, C is the right medium for C and a conversion is
a transcription that trades readable C for less readable love.

that mistake was made and paid for. the α-equivalence cluster -- `salpha`/`shash` and the
beta bridge, 156 lines -- was converted and REVERTED, and the revert is the finding:

* it was chosen for scoring zero on every purity meter. purity selects for EASY TO CONVERT,
  which is close enough to WORTH CONVERTING to be mistaken for it, and is not the same
  thing. the region held exactly one abstraction worth sharing (a four-site binder count)
  against 156 lines of straight transcription.
* the C was readable and the love was not. `alpha.l` came to 454 lines of nested
  constructors to lay 313 lines of C where 156 stood.
* **and the verification win was nil**, which is the part that actually settles it. see
  §the parse is enough, for verification.

so: before proposing a region, name the repetition. if there is none, the answer is no.

## the parse is enough, for verification

the plan's payoff is VERIFICATION, and for that purpose a region does not need to be
authored in clay at all -- `cparse` already hands back a faithful clay term. measured on
the α cluster: **10 of 10 functions round-trip C -> clay -> C with no authoring**, which is
G1's own criterion applied to a region rather than to `test/cc/`.

the objection that killed the authored version is that a parse arrives POST-CPP: `long`
for `word`, `92` for `'\\'`, `((struct ai_chain *) a)->a` for `A(a)`, and `#if ai_tco`
folded away per configuration. every one of those is disqualifying for GENERATION -- love.c
is one text for many seats and `test_fixpoint` wants the rebuild byte-identical -- and
none of them is disqualifying for a MODEL. post-cpp is what the compiler actually compiles,
which is what a theorem should be about, and `cparse-t` takes the arch, so a model can be
derived per seat instead of pinned to one.

**that asymmetry is the whole rule.** parse-derived clay: fine for verification, fatal for
generation. authored clay: required for generation, unnecessary for verification. so
authoring is a GENERATION cost, and a region with nothing to generate should never pay it.

`clay2coq` therefore wants `(cparse ..)` in front of it, not a `.l` per region -- and it
can have the α cluster, the limb helpers and dtoa today, with no rung spent on any of them.

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
* **(b) generate it away.** the X-macros `nifs(_)` / `insts(_)` expanded differently at
  each site because C cannot abstract "the same list with a different consumer". the love
  loop replaces them and the macros are DELETED. this is the criterion's exact shape and
  the one place a macro genuinely goes -- **landed**, rung 4 in §what has landed.
* **(c) add an emit-only clay node.** the rung 2b-2d precedent, for the shapes that are
  not call-shaped.

### ⚠ hand-paring is the inverse of the criterion

`lvm(lvm_add)` expands to `ai_noinline ai_noicf struct ai *lvm_add(struct ai *restrict g,
union u *Ip, ai_word *Hp, ai_word *restrict Sp)`. writing that out at **184 definition
sites** ADDS the most repetitive text in the file -- precisely what the criterion says to
generate.

and `ai_musttail return` at **311 sites** is a discipline, not noise: `love.h` says an
opportunistic miss is one frame per dispatch and a stack overflow down some long read.
mooncc now takes the attribute and refuses any compile it cannot spell as the tail jump,
and `make vmret` disassembles the binary as the cross-check. do not thin these out to
suit the shower. GENERATE them.

## what clay cannot say yet

the five node shapes landed (rung 3 below), so the counted table this section carried is
gone -- every row is sayable, `__asm__` and `unsigned __int128` were already, and the
`lvm(...)` definitions took the emit-the-expansion route. the preprocessor followed (§below,
rung 5), and four more landed with the α-cluster attempt (§rung 6): a `proto` with a
signature, an `edef` whose constant names its value and whose tag may be absent, the index
sugar reaching through `dot`, and a character literal. so what remains is the one
alternative deliberately not built: a declarator-MACRO node that would print `lvm(lvm_add)`
itself rather than its expansion -- now the order of work's rung 5, and owed rather than
optional, since printing the expansion at 184 sites is the criterion pointing backwards.

## the preprocessor -- the one real design decision

`love.c` carries **154 `#define`s** and ~92 conditional directives (`#if` 21, `#ifdef` 7,
`#ifndef` 9, `#else` 13, `#elif` 5, `#endif` 37). two of the conditionals are
architecture, not detail:

* `#if ai_tco` -- the threaded-vs-trampoline split (`love.h:46-94`), which the wasm build
  depends on (`-Dai_tco=0`) and which `love0` is built under.
* `#ifdef __wasm__` / `#if __STDC_HOSTED__` -- the freestanding/hosted split.

`love.c` is ONE TEXT compiled for many targets, and `test_fixpoint` requires the mooncc
rebuild to be byte-identical -- so a generator cannot fold the conditionals away by
emitting per seat; it has to emit the `#if` itself. clay gained emit-only `#if` /
`#define` nodes: rung 5 in §what has landed.

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
-- exactly how `mx.h` landed. **re-ranked** against §the criterion after the α cluster was
converted and reverted: the question is no longer "how pure is it" but "what is there to
generate". a region with no repetition C cannot abstract is not on this list at all.

⚠ **the NUMBERS moved with the re-rank, and git log did not.** commits written before it
say "rung 4" for the α-equivalence cluster, which is struck below; this list's rung 4 is
`vbin_fill`. read a rung by its NAME, never by its number, and do not renumber again --
name the region in a commit message instead.

1. **the five node shapes** -- attributes, `restrict`, the `ret` prefix, `_Static_assert`,
   flexible array members. **landed** -- rung 3 in §what has landed.
2. **add preprocessor nodes** -- `cpp-if` / `cpp-def`. **landed** -- rung 5 in §what has
   landed.
3. **the X-macro registry** -- `nifs` / `insts`, route (b): the first region where a macro
   is deleted and the love loop is the better abstraction. **landed** -- rung 4 in §what
   has landed.
4. **`vbin_fill` + the lane table** -- `love.c:7596`, whose body redefines `#define VBF(E)`
   **six times** (`7611-7617`) because C cannot abstract "the same loop nest with a
   different expression". `.macro`/`.rept` in GAS wearing a different hat, and after the
   nif registry the clearest remaining case in the file. the one-datum-two-consumers demo
   -- see §the seam -- and the theorem `love.c:7602` has been claiming for free
   ("mixed/bignum/broadcast falls through to the general loop; results bit-identical"),
   which nothing checks.
   ⚠ those in-function macros mean generated C writes the loop out ~11 times, so
   BYTE-comparison here is structurally impossible; the AST-vs-AST oracle (`tools/clay-g2.l`)
   is immune, and this is the rung it was built for.
   then the rest of the family, all the same shape: `vmap1_fill` 7255, `vmap2_fill` 7734,
   `twin_fill` 7932, `cbin_fill` 7958, `twin_pow_fill` 8011, `twin_build_fill` 8062,
   `cpart_fill` 8108, `carg_fill` 8206. and `bit_slow` (`love.c:5453`, used 5762), where
   `doc/io.md` lists unfinished tower work -- negatives should SCARE rather than answer
   `()` -- so clay lands the fix and the generation together.
5. **the `lvm(..)` declarator, at 184 definition sites** -- `lvm(lvm_add)` expands to
   `ai_noinline ai_noicf struct ai *lvm_add(struct ai *restrict g, union u *Ip, ai_word *Hp,
   ai_word *restrict Sp)`, and 184 of them is the most repetitive text in the file. rung 3
   made the expansion sayable; what is not decided is whether clay should print `lvm(..)`
   ITSELF -- a declarator-MACRO node -- or its expansion. printing the expansion is a
   generated region 184 signatures longer than the one it replaces, which is the criterion
   pointing the other way, so the node is probably owed. settle that before rung 7.
6. **`c0`'s `Cata()`/`Ana()` signatures** -- `love.c:1349-1938`, where the function
   SIGNATURES are macro-generated. same shape as rung 5 and the same open question, on a
   smaller surface; a good place to answer it.
7. **the VM loop, last** -- `1976-2941`. 97 threading macros, but §macros measured them:
   they are mostly CALL-SHAPED and need no grammar, so most of the loop is transcription
   and does not qualify. what qualifies is whatever survives rungs 5-6, and the honest
   answer today is "not obviously anything". it is also where a mistake is least visible.
8. **the lawed injection seam** -- `cc-clay` in `crew/moon/moon.l`, sibling to `cc-parse`
   (`moon.l:128`): clay in, `clay-ok?`, `clay-tables`, `cgen-obj` (`gen.l:6141`). this is
   what makes clay a frontend target OTHER MODULES can share, and it enables G3's third
   leg. `clay-tables` derives `stag` + `sigs`, REUSING `playout` (`parse.l:576`) rather
   than reimplementing C layout rules. document it in `doc/moon.md`, whose architecture
   section names only backend seams today.
   ⚠ this is independent of every rung above it: it is about clay as an INPUT, and nothing
   in it asks a region to be authored.

### struck from the slate, and why

these were ranked by purity and are transcription targets. §the parse is enough shows
`clay2coq` can have all of them TODAY through `(cparse ..)`, with no rung spent:

* **the α-equivalence cluster** -- `salpha`/`shash` and the beta bridge. converted, then
  reverted; the record is in §the criterion. `test/spec.l` §comparing-functions and
  §reduction pin its laws, `proof/rocq/spec.v` sits under them, and the parse hands
  `clay2coq` the term.
* **the bignum magnitude helpers** -- the raw limb primitives, operand loading, resumable
  multiply. `proof/rocq/big.v` already models the lane against stdlib `Z` and `big_drive`
  FUZZES love's limbs against it, and putting the IMPLEMENTATION into Rocq is still the
  largest single verification step this plan offers -- but that step is `clay2coq` over
  the PARSE, not a `.l` transcription. ⚠ and it was never byte-exact anyway:
  `crew/moon/cpp.l` leaves `__SIZEOF_INT128__` undefined, so mooncc-built love.c takes the
  32-bit limb path and gcc-built takes the 64-bit one.
* ~~**dtoa**~~ -- SETTLED the other way: the printer moved into lisp (`love/post.l`), and
  `dg_*` + `ai_dtoa2` went with it. love has bignums, so the digit arrays and their x2/x5
  carry walks are just exact integer arithmetic there. nothing left to convert.
* **the GC and the heap-image codec** -- 796 lines scoring zero on the purity meter, which
  is now known to be the wrong meter. the earlier judgement that excluded them ("it IS the
  heap") happened to reach the right answer for the wrong reason.

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
* **G2 conversion equivalence** -- `tools/clay-g2.l`: parse the original translation unit
  and the converted one WHOLE, and compare the named top-level definitions as TREES.
  parsing whole is what makes it exact -- both sides meet the same cpp, typedefs and macro
  expansions, so a surviving difference is a difference in MEANING, which is the comparison
  a byte diff cannot make when the generator writes `A(l)`, prints `A(l)`, and cpp turns
  both into the same tree. runs once at a conversion, against a copy of the pre-conversion
  file; afterwards the regeneration-drift `cmp` in test_clay is what stands.
  ⚠ it is a GENERATION gate. rung 7 is what it was built for -- where the six `VBF(E)`
  redefinitions make byte comparison structurally impossible -- and it has no job on a
  region that is merely being transcribed, because such a region should not be converted.
  rung 2 did the table version of this: all 512 cells reproduced before `love.c` was
  touched.
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
   node grammar as data (`clay-tags`, the live roster): top `prog fn proto gdecl xdecl tdef
   note edef sdef`; stmt `blk decl sdecl ret if while for do switch case dflt brk cont goto
   lbl expr nop asm`; expr `num flo str var bin un asn rmw post cond comma call deref addr
   dot cast szof init dfield didx clit land lor vastart vaarg vaend`; types `ptr arr varr
   struct named const`. `rmw` is `lv op= rhs` KEPT WHOLE -- the desugar to
   `(asn lv (bin op lv rhs))` would evaluate `lv` twice (doc/moon.md, `calm?`). `clay-ok?`, a validator, because `gen.l` currently TRUSTS its input. honors the
   `gripe` protocol (`doc/moon-diag.md`).
1. **`clay-show` and G1.** AST -> C text, plus the round-trip gate over `test/cc/`.
2. **the dispatch matrices; deleted `tools/mxdump.c`.** `mx.l` is the table; `mx.h` is
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
3. **the five node shapes, ~750 sites.** attribute SPELLINGS on `fn`'s appended 5th slot
   (`("ai_noinline" "ai_noicf")` -- love.h's macros used call-shaped, never expanded);
   `(restrict t)` beside `(const t)`, pointers only, anything else refuses; a prefix slot
   on `ret` (`(ret e "ai_musttail")` -- the 311-site tail-call discipline, generated, never
   thinned); `(sassert e "msg")` for `_Static_assert`. the fifth shape cost nothing:
   `(arr t 0)` already printed the flexible member's `[]`, and `unsigned __int128` was
   `(named ..)` all along -- both now lawed. together they say a whole `lvm(..)` definition,
   which `test/host/clay.l` proves against `lvm_add`'s exact expansion. emit-only BY
   NECESSITY -- `parse.l:116-121` balance-skips a trailing attribute run ("the codegen owes
   nothing"), so none can come back through a parse, exactly like `note`/`edef`/`sdef` --
   except the `ret` prefix, which parses now: a statement-position `musttail` lands in the
   PRE slot (spelled `"__attribute__((musttail))"`), gen marks the call, and sibcall spells
   the jump or refuses. G1 held at 63/51/0 for the rest.
4. **the nif + instruction registry, and the first DELETED macros.** `nifs.l` is the
   roster -- 126 nif rows `(ARRAY name arity IMPL)` and 38 instruction names -- and `nifs.h`
   is laid from it: ONE `union u` table, a nif's little stream being a RUN inside it, then
   `def1`, the name -> value table `ai_defn` reads into the book, carrying each run's
   address. nine macros stop existing (`s1`..`s5`, `nifs`, `insts`, `niff`, `i_entry`);
   love.c takes an `#include` at the one site the two expansions stood. route (b) exactly:
   the five `sN` were arity wearing a macro, and the X-macro pair existed only because C
   cannot hand one list to two readers.
   the one table is what a generator buys that a macro could not: the run OFFSETS are a
   running sum over the roster, so 126 separate array symbols collapse into one object and
   no hand has to keep the offsets straight.
   **it cost no new clay node.** `gdecl` + `(static)`, nested `init`, `dfield` for the `.x`
   designator and `cast` for `(intptr_t)` were all already there, which is the useful
   measurement: a whole region of love.c converted on the grammar rung 3 left behind, and
   G1 held at 63/51/0 untouched.
   G2 came in two pieces, both exact. the flat table: rebuild the 336-cell sequence from
   the roster and compare it to the emitted cells -- identical, with every `def1` offset
   matching the roster's running sum and no slack at the end. and, against the macro it
   replaced: preprocess the old and new `love.c` with the same cc, tokenize, compare
   top-level declarations as multisets -- **equal** at the point the per-nif arrays were
   still named, which is what pins the cell contents to what the macros expanded to.
   net C **-84** lines.
   ⚠ 506 generated lines stand where 85 dense macro lines did, and that is the criterion
   working, not failing -- the `.l` roster is 126 rows and the repetition went where
   repetition belongs. a braced union cell is an aggregate, so `cinit`'s one layout rule
   gives it its own line; that rule is not a knob, and a compaction wanting a new clay node
   is not worth a new clay node.

5. **the preprocessor, and the data slot layout as its first consumer.** `(cpp-def NAME
   [BODY])`, `(cpp-undef NAME)`, `(cpp-if ARMS [ELSE])`, emit-only more firmly than `note`
   is -- cpp runs before the lexer, so a directive can never reach a parse. `#ifdef X`
   needs no node (`defined` wears call syntax in C's own grammar, so it is a plain
   `call`), and an `#else` out of place has no spelling because ELSE is a slot rather than
   an arm. two rules earn their keep: a `#define` body that is a clay expression renders
   at the TIGHTEST context, so `countof(_)` parenthesizes itself -- macro hygiene falling
   out of the precedence table -- and a spelling body rides through verbatim, the standing
   `fn`'s attributes already had. a gripe from inside an arm comes OUT rather than being
   concatenated into the text.
   the consumer is `ai_data_section` / `ai_data_stride` / `ai_data_n`, moved out of
   `love.h` into the generated `kinds.h`: they are a layout OF the `enum d` roster, and
   `mx.l` already laid both that roster and the six linker scripts that tile the same
   slots. one row of `mx-strides` now carries a seat's stride for the C AND for `ld`, so
   the last hand-kept crossing in the scheme -- a script tiling tighter than its seat
   believes, which answers slot 0 for every value in silence -- has no way left to be
   written down. `love.h`'s `_Static_assert` on the slot count goes with it: one roster
   lays both sides, so it had nothing left to catch.
   G1 held at 63/51/0, as it must -- an emit-only node adds nothing to round-trip.

6. **four more node shapes, `tools/clay-g2.l`, and a rung landed then REVERTED.** the
   α-equivalence cluster -- `salpha`/`shash` and the beta bridge, 156 lines -- was
   converted to `alpha.l` + `alpha.h` and then taken back out. the code is gone; what it
   bought is this doc's criterion (§the criterion, §the parse is enough) and four shapes
   that stayed:
   * a `proto` kept only its NAME (all cparse keeps) and printed `int f();`. it now takes
     `fn`'s three appended slots, laid by the same `csig`, so a forward declaration cannot
     drift from the definition it announces.
   * an `edef`'s constants took the positions C gives them and could not name a value, so
     `enum { nf_maxcap = 64 }` -- C's spelling for a lone compile-time constant -- was
     unsayable. ⚠ and a valued constant did not REFUSE: it printed the list's bytes as a
     name and emitted mojibake. that was a live bug in a committed generator's path.
   * `dot` spent its arrow on the bracket, so `k[0].ap` printed `(k + 0)->ap`.
   * there was no character literal, so a function about the backslash symbol compared
     against `92`.
   ⚠ **all four are READABILITY, not capability** -- clay could already SAY every one, and
   that is exactly what made the rung look cheap. it is not a reason to convert a region;
   it is the tax on converting one that should not be. they are kept because a GENERATION
   rung still owes readable output, and rungs 4-6 of the order of work will spend them.
   G1 held at 63/51/0 throughout, as an emit-only widening must.
   `tools/clay-g2.l` is the durable half: it proved the reverted conversion exact (10/10
   definitions tree-identical) and caught two real slips on the way -- a stray list
   constructor, and a `(blk ..)` around statements that stood in the branch directly, a
   scope the original had no brace for and a C compiler would never have noticed.


## open

* **whole-file capture** -- rung 5 built the preprocessor NODES; capturing love.c's own
  154 `#define`s and ~92 conditionals into a table is the separate question.
* **`love.c.l`'s own shape.** one file or a directory of regions? the generated `love.c` is
  one text either way, but the `.l` side has no constraint forcing it, and the answer
  decides whether a rung's diff is readable.
* **`clay2coq.l`** -- what turns any of this into a theorem rather than a tidier build, and
  after §the parse is enough it is the piece with the most standing value in the plan: it
  wants `(cparse ..)` in front of it, not a `.l` per region, and it can have the α cluster,
  the limb helpers and dtoa the day it exists. `proof/rocq/big.v` is the readiest
  customer -- it already models the bignum lane against stdlib `Z` and fuzzes love's limbs
  against the extraction, and `clay2coq` is what upgrades that to a proof about the code
  that ships.
