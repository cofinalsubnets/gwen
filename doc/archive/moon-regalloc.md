# moon-regalloc — the register story, and the catalog of the gap

How mooncc decides what lives in a register, what that costs against gcc/clang -O2, and
which levers remain — the **catalog** the differentials built, promoted from session
memory 2026-08-10. Companions: doc/moon-diff.md (the running three-compiler ledger),
doc/moon-c-gaps.md (conformance), doc/moon-diag.md (refusals), doc/hom.md (the
design the destination-die migration wears). Anchors below are into
crew/moon/gen.l; verify before leaning on one — this file states shape and physics, the
code states truth.

## the register story as it stands

The generator's baseline is an accumulator protocol: every expression computes into r0
and carries its type; locals ride the r4 frame. On top of that, residency, in layers —
each one priced, gated, and landed separately:

* **the operand pool** (`opool`, six caller-saved regs) — statement-scoped staging;
  `ralloc`/`rfree`, reset at every statement. A pool value never crosses a call.
* **the vmap** — the cross-statement leg: a universe local's value, once written through
  a pool register, maps name → reg → **class** so re-reads are zero forms. Write-through: the
  slot stays the single source of truth, so a flush forgets, never spills. Flushes at every
  label emission; a CALL kills the pool class only (the class is what says so). if/?: join
  labels survive by pair-INTERSECTION over arriving edges (the forward-JOIN meet).
* **homes and rides** — a param's positional seat (`homefold`), the self-assign ride
  license (a param whose only arrival-defs are self-updates rides its arrival register
  end-to-end), int/uint locals and params admitted under canonical extension (`lhomable?`,
  every def re-narrows via cvt so the home always holds the extended value).
* **the cs pool** (IR r11-r14 → machine r12-r15) — callee-saved homes that ride through
  calls unwrapped, priced by the depth≥2/tc≥3 in-loop license; d128 locals take PAIR
  seats (`'regw`). The whole grant is minted at one door: `seats` answers `(lh ih lp cs)`.
* **splice binds** — cbinl/cginl alias a pinned/homed caller register into an inlinee
  (`alsafe?` the staging law), park or seat other args only when priced (`paid?`:
  loop-weighted reads ≥ 8; `deep?`: two pool seats remain after the take; regen attempts
  only — the `rgon` gate).
* **recovery passes** — addrfold (address arith into addressing modes, ldx/stx/si),
  deadcell (the spush-cell reservation sweep), stld/stldw (store-load residency),
  copyprop (the forward copy walk; coal is its backward twin),
  cmpfuse (`dieb?`, the branching death proof), unframe + jr0/unhome (frame elision and
  home→arrival renames), laylax (branch relaxation, in holo).
* **the armor** — cskeep (the callee-saved contract checked at emission, path-aware,
  dark ops refused), stage.l's typed pipeline sigs, the g-pin record (`fnreset`). A
  homing rung is gated by construction now; disassembly probes are for perf reads.

What there is NOT: liveness-driven global allocation. No live intervals, no coloring or
linear scan, no spill decisions — the frame slot is always live, which is why every def
still stores and why values die at joins the meet can't cross and at every call.

## the gap, measured

| date | vs | wall/cycles | insns | note |
|---|---|---|---|---|
| 2026-07-16 | clang | 1.84× | 3.6× | first systematic differential; IPC 3.5 vs 2.0 |
| 2026-07-17 | clang | 1.52× | 2.57× | six frame/cell levers in two days |
| 2026-07-18 | clang | ~1.32× | — | indexed loads, encoding shave, relaxation. ⚠ corpus+box changed after; not comparable forward |
| 2026-08-02 | gcc | 1.52×→1.34× | 2.78×→ | fresh 3-way re-base, then fleet-slots in one rung |
| 2026-08-03 | gcc | **1.30×** | 1.96× | int128 limbs, inlining honored, non-leaf slotting, cs pool, int homing; IPC 3.07 vs 2.17 |
| 2026-08-10 | clang | **1.18× cycles** | 1.71× | doc/moon-diff.md first fill; IPC 2.98 vs 2.05; shared-.text 1.75× |
| 2026-08-11 | clang | 1.24× cycles | 1.63× | the re-base fill (moon-alloc's step 0): static both-emit **1.56×**, 141,221 B — the static and dynamic ratios came APART, repack being a size lever |
| 2026-08-11 | clang | 1.21× cycles | **1.629×** | step 0 re-run after the spush rung: static both-emit **1.47×**, 118,531 B; binary 1.618×. The dynamic row is UNMOVED across three fills while static walked 1.63→1.47 — every landed lever has been a size lever |

The remaining excess is not idiom-shaped (that catalog closed 2026-08-02: cmp-$0, neg
dances, load-then-cmp all at parity). It is structural: slot traffic. At the fifth
differential ~22% of all static insns were rsp slot movs; only ~15% of reloads were
straight-line (peephole-reachable) — the rest cross branches and calls, which is exactly
the boundary the write-through vmap cannot cross.

## the splice client — what the gap costs a JIT

2026-08-11, `bench/vmsplice/`. A second client for lever 2, pricing it from a direction
the corpus differential cannot reach: a **template JIT over the tail-threaded VM** —
decode a thread into its named handlers (`def1` + `image_ap_index` already do this; the
image codec depends on it), splice their bodies into one function with the dispatch
deleted, compile, install through `nif`.

**It works.** A hand-composed body of 64 spliced op bodies, compiled by mooncc, lifted
out of the `.o` with holo's own `ld-read` and installed with `(from 'glaze 'nif)`,
agrees with its interp twin and runs. mooncc honors `ai_musttail` here, so the emitted
tail is exactly loopepi's shape (`st Sp[0]; ld Ip[2]; Ip += 16; jmp`) — the body drops
into the VM's convention with no shim.

**The tier is real and it is ~4×.** Same 64-op body, ⚠ `LOVE_NO_GLAZE=1` (without it the
"interpreted" twin is silently glaze-compiled and the baseline reads ~9× too fast):

| lane | ns/call (64 ops) | ns/op |
|---|---|---|
| interpreted thread | 248.5 | 3.88 |
| **mooncc-composed native** | **62.8** | **0.98** |
| glaze (auto.l's recognizer) | 26.3 | 0.41 |

So a splice JIT is a *universal* baseline tier and the glaze is a further ~2.4× on the
shapes it recognizes — they compose, they do not compete.

**And the gap is the whole distance between those last two rows.** The same probe source
through both compilers, on a chain walk where nothing folds:

| | dispatched | composed | |
|---|---|---|---|
| cc | 1.59 ns/op | 0.83 | **1.91×** |
| mooncc | 1.84 ns/op | 2.21 | **0.83× — slower than dispatch** |

Insn counts are close (11.1 vs 13.0 per op), so this is not density. It is the op-to-op
seam, one line of disassembly each:

```
cc:      mov %rsi,(%rcx)  /  mov %rsi,%rdx     ; store, and KEEP it in a register
mooncc:  mov %rax,(%rcx)  /  mov (%r11),%rax   ; store, then RELOAD the slot just written
```

Sixty-four store→load round trips on the dependent path. This is lever 2's ~22% bucket
seen from the client side: a splice's every op boundary is exactly the write-through the
allocator leg exists to remove, and closing it is worth ~2.5× **on the spliced body** —
about the distance to the glaze.

### the seam's root — ONE POINTER UNDER TWO NAMES (2026-08-13)

⚠ Read this before touching the seam: four fixes were built against it that day and all four
missed, three of them because the diagnosis was read off pre-peephole IR (see *how to measure
honestly*). What survives is short.

**`Sp` is carried in two registers, and the store and the load name different ones.**

```
mov %rcx,%r11         ; the copy, at entry
...
mov %rax,(%rcx)       ; store Sp[0]  — base %rcx, the ARRIVAL
mov (%r11),%rax       ; load  Sp[0]  — base %r11, ADJACENT, same address, other name
```

`stldp` folds an adjacent store/load pair **by base-register name**, so it is handed one
pointer under two names and never fires. Hence a reload per seam, hence a 64-deep
store-to-load-forward chain, hence 0.84×. Note what this is *not*: mooncc emits 837
instructions to cc's 713 for the same body (1.17×) against 2.6× in time — **the seam is a
dependency chain, not a density problem**, and any instrument that counts instructions will
say this body is nearly fine.

The copy exists because **`Sp` does not ride**. `ride` for `composed` is `(2)` — `Hp` only.
`rst-defok` rejects `r1` on a foreign def, so `Sp` spills, and in a frameless function the
spill collapses into that entry copy. The chain, end to end: *ride denied → second register
→ two names → no fold → reload per seam → serialized forwards.*

**So the fix is upstream: make `Sp` ride.** One register for one value, and the pair folds
itself. That is the ride analysis's business, not a peephole's — which is why all four
peephole-level attempts below failed. Untested as of 2026-08-13.

Two things the probe settled that the design had worried about:

* **`Have1` is free.** A safepoint in every spliced body cost nothing on either compiler
  (cc 0.825 vs 0.830, mooncc 2.188 vs 2.214 ns/op) — the heap-limit branch predicts.
* **splicing lets the optimizer see ACROSS ops.** cc folded the 64-bump arithmetic body
  to **30 bytes** — one `add` — against mooncc's 1299. Real extra value on top of
  dispatch elimination, and the reason the probe's first version read 60×: the whole
  body had folded into its own answer (see the README's traps).

⚠ what a real splice JIT owes beyond this: the lifted body was relocation-free only
because it touches nothing external. A body that touches chains or can collect pulls in
`lvm_chain`, `lvm_sym`, `ai_please` — references that must bind to the LIVE process's
addresses. `lift.l` refuses such a body rather than lifting one that would jump wherever
it happened to be mapped; binding them is a linking step, and holo's `ld-read` (in the
image since the linker half landed) is the tool for it.

⚠ **and since 2026-08-13 the JIT is a DOOR IN THE IMAGE, not a pipeline of scripts:
`lib/splice.l`, `(use 'splice)` then `(jit f)` — compose, compile, bind, nif, all in one
process, gated by `make test_splice` (four samples jitted and differentialled against their
own twins, the fifth declining by name).** It needs an image carrying the compiler
(`love wake out/host/mooncc.image`), which the dist artifact does.

⚠ **and the source of truth is the binary itself: `mooncc -fir=PREFIX` writes the machine-form
IR of every matching function into `.rodata` under `ai_ir_<tu>`, as one readable datum, and
`nifs.l` lays the book-name→`lvm_` bridge (`ai_nif_lvm`) beside it — the registry is the only
thing that holds both names, which is the whole argument for that file.** The default `love` is built with `-fir=lvm_` and carries **91 op handlers, 28 KB**, which
a plain `love` reads back out of `/proc/self/exe` with no compiler, no source tree and no
disassembler — `(use 'splice)` then `(jit-irtab ())`, gated by `test_splice`. This is the answer
to the question the C route was asking wrong: an op's body was being scraped out of **love.c's
text**, which a shipped love does not carry, and the alternative — disassembling our own
`lvm_` functions — would hand back bytes the splicer could not reason about. holo assembles
these forms as they stand, because that is what they are.
⚠ **the cap is the splice budget and is read off the curve**, not chosen: over love.c's 195
handlers, 64 forms takes 93 of them for 29 KB where 128 takes 124 for 67 KB and the whole set
weighs 821 KB. A handler over it is absent, and the JIT declines an op it has no IR for by name.
⚠ **two flags now mirror between `host/build.mk` and `test/gate/fixpoint.sh`** — the gate's own
comment predicted this one: a flag on love.c in make's rule and not in the fixpoint's rebuild is
a byte difference that reads as a broken compiler.

⚠ **THE SPLICER LANDED 2026-08-13 and the C route is gone.** `(jit f)` now reads its op rows,
takes each op's IR out of the record, splices, assembles with holo and nifs it — **no compiler,
no source tree, no object file**, and `test_splice` runs on a plain `love`. What made it
mechanical is the VM's own convention: `g=r6, Ip=r5, Hp=r2, Sp=r1`, every op takes its argument
from `Sp[0]` and leaves its answer there, so two handler bodies laid end to end already agree
about everything. The load family is *said* (three forms), the fld family is **unfused** into a
push and an op — that fusion is the VM's, not the meaning's — and everything else is spliced.

⚠ **the splice condition is CHECKED, not assumed**: no frame, no `Ip` read, no leaving. The last
one is the sharp edge and it is the trap this section predicted: a handler carries its own room
guard, that guard jumps to `lvm_gc`, and gc **resumes at `Ip`**, which in a spliced body is the
nif cell — so a collection halfway through would re-run the ops that already ran. One hoisted
guard leads instead, ahead of any `Sp` motion, deopting to the twin. A per-op guard left in place
is a silent double-apply, not a crash.

⚠ **and no relocations at all, because a JIT knows the answers**: the one external reference a
clean handler carries is `(la rX sym)`, and at splice time that symbol's live address is a
*number* — so it becomes `(li rX addr)`, a movabs, and the body is position-independent.

**THE REFLOW is where the seam finally comes out, and it is two lines.** A handler delivers with
`(st r1 0 rX)` and the next opens by reading it straight back with `(ld rY r1 0)`; laid end to end
those are adjacent — one base, one name, nothing between. That is `stldp`'s law with none of its
aliasing question, because both sides are ours. **Measured: ~1.2× on short accessor chains and
1.6× on a 32-op body, where a mooncc-compiled composed body of the same closure also reads 1.6×**
— the splicer matches the compiler's own output while needing none of it. The remaining distance
to the probe's ~4× is now the splicer's own business: keep `Sp[0]` in a register across a whole
segment instead of storing and reloading at every op. That is the destination die (step 7) at the
splice level, and it is no longer waiting on the allocator.

⚠ **three traps paid on the way, each cheap to re-learn the hard way.** A leading dot does not
survive the reader — `.e5044` reads back as the two-element list `(. e5044)`, since `.` is an
ordinary punct symbol and there are no dotted pairs — so the serializer respells labels `jl…`.
`two?` on a nom is false (it tests cons pairs), which turned a covered op into a silent decline.
And **operand 0 is the commonest operand there is**, so a bare `(nil? i)` presence test read
`arg 0` as absence and declined every closure in silence; presence rides the `(1 x)` wrapper.

**The automation landed first (rungs 1+2, now folded into the module above).** The probe was a
hand-written body; the pipeline runs on live closures. `dis` (love/ev.l, the emission
interface's dual — a reflection primitive built pre-egg from `peek` + the book, like
`feels`, so it survives the birth mop) reads a compiled thread back to `(op-nom operand..)`
rows; the splicer maps each row to its op's own **machine-form IR**, read out of the running
binary's `.rodata` where `mooncc -fir=lvm_` wrote it, with the nif→nom bridge through
`nifs.l`'s third derivation; holo assembles it; and there is no relocation step at all,
because a JIT knows the answers — the one external reference a clean handler carries,
`(la rX sym)`, becomes `(li rX addr)` against `/proc/self/exe`'s own symtab plus the load
bias from the exe's `/proc/self/maps` line. In-process by construction: a separate process
has a different ASLR base, so the bytes are valid only in the `love` that made them.
Spliced bodies agree with their interp twins on every input (the differential is `sl-cross`
one level down — one denotation, two presentations) and beat them ~1.2× on short accessor
chains, **1.6× on a 32-op body — where a mooncc-compiled composed body of the same closure
also reads 1.6×**, so the splicer matches the compiler's output while needing none of it.
The ceiling is the ~4× the probe measured, and the gap to it is this bucket.

### the auto lane, and the coverage census that ranks everything left (2026-08-13)

The splicer is wired to fire by itself: `love/glaze/hook.l`'s `natjit` gets one more arm, **last,
under amble**, reading `(from 'splice)` at fire time — so `(use 'splice)` is the entire switch and
the lane is dormant in every love that does not load the module. It sits under amble on purpose.
Amble is *already* the glaze's universal per-op native tier, and it translates from the **source**,
declining a written list of shapes (strings, gems, `//` `%` `&` `|` `^`, inner `\`, effects, a
global read in operand position). The splicer translates from the **compiled thread** and says
whatever the compiler emitted. So what it adds is exactly amble's leavings, and putting it first
would only take work away from a lane that already does it.

⚠ **`(use 'splice)` must be its own top-level form.** A `:` builds its lambda bindings before its
body runs, so a `use` sharing a form with the closures it means to cover lights the lane after they
are already made — which reads exactly like a lane that is off, and did for an hour.

⚠ **the splicer re-enters itself.** Everything in `lib/splice.l` is love, so splicing *builds*
closures — dis's walk, holo's assembler, the tablets — and each creation re-enters `ala`, the hook,
and the door. A busy cell makes the splicer invisible to itself. It is not an optimisation: without
it the runtime cannot build a closure without recursing.

**The whole corpus, lane on** (`test_host`'s 18.5k lines, one process): **4289 tests pass** — the
differential holds at corpus scale, which is the result that matters most — **8 closures spliced,
1969 declined**, and the run costs **+25% wall clock** (4.65 s against 3.71 s). That is the honest
before-and-after today and it is *negative*: the tier buys eight closures for a quarter of the run.
The census says why, and it is one number:

| declines | reason | what it is |
|---|---|---|
| **1770** | `multi-arg` | arity ≥ 2 — refused at `lvm_cur` before a single row is read |
| 175 | `operand` | the **fused two-operand loads**: `qq` 41, `qqp` 35, `qa` 25, `qap` 25, `aq` 15, `aqp` 7, `aa` 2 — plus the branch ops `argtwocond` 13, `argcond` 5, `cond` 4, `argtap` 2 |
| 16 | `no-ir` | `add` `mul` `lt` `le` `eq` `string` `snip` `tally` `nilp` `litp` `rem` `shape` `hush` `wait` `scoop` `tablet` — **the `-fir` 64-form cap**, not a splice failure |
| 7 | `no-tail` | `ap` `sleep` `tap` `mint` `charmp` `tally` — a handler that does not end in the dispatch triple |
| 2 | `no-bridge` | `sat` — `$` has no `lvm_` row in `nifs.l` |
| **0** | `unclean` | — |

**Two findings, and the second refutes what this doc predicted.** First, 90% of the loss is
*arity*, decided before any op is looked at: whatever the splicer can or cannot say about op bodies
is untested against nine tenths of the corpus. Second, **not one handler was rejected by the splice
condition** — no frame, no call, no `Ip` read, no gc jump, in the 199 closures that got past arity.
The gc-resume trap was worth building the hoisted guard for, and the guard is right, but the wall
here is the operand and arity plumbing, not the condition. ⚠ read the `no-ir` counts as *first
blocker* counts, never frequencies: a closure declines at its first bad row, so an op that always
sits behind `lvm_cur` reads as 1.

### the two rungs the census asked for, and what they moved (2026-08-13)

**Arity — LANDED, and it cost almost nothing**, because love.c had already arranged for it.
`dis` now carries the count on its `cur` row (cell 1 is the saturation number `lvm_cur` reads;
a reader that drops it leaves the closure's arity with no door), the splicer strips that row and
hands the arity to `nif` — and *nothing else changed*. `lvm_arg i` indexes `Sp[i]` whatever the
frame's width; the collapse `Sp[net] = Sp[0]; Sp += net` restores the entry `Sp` and so is
arity-blind; and `nif`'s arity ≥ 2 cell puts `lvm_ret` and the interp twin **at the same offsets
as the arity-1 cell** — love.c's own stated law — so the dispatch tail and the guard's deopt need
no arity of their own. ⚠ the gate's multi-arg rows read the *second* parameter on purpose: an
off-by-one frame still answers correctly for every arity-1 sample.

**The push-pair family — LANDED**: `aa`/`aq`/`qa`/`qq` are pure push-push fusions (`frun2`), so
they are said as two pushes, the fld family's trick a second time. The operands ride in source
order off a *moving* `Sp`, so two pushes laid in order come out right with no arithmetic — love.c
says this over `PushA`/`PushQ` and `jit-push-a` inherits it. ⚠ only a **charm** quote: `dis`
answers `'x` for a heap quote or a raw index, which is the roster refusing to hand a code word
out, and those decline.

**Corpus, after both: 1969 → 626 declines, 8 → 11 spliced, +25% → +18% wall clock.** The declines
collapsed and the firings barely moved, which is the finding:

| declines | reason |
|---|---|
| **508** | the **apply** variants — `qap` 231, `qqp` 204, `aap` 25, `aqp` 24, `apn` 17, `argap` 7 |
| 59 | the **branch** ops — `argtwocond` 28, `argcond` 20, `cond` 7, `argtap` 4 |
| 16 | `no-bridge` — `sat` 6, `two?` 5, `><` 3, `quit` 2: **prel ALIASES**, `(: two? link? … sat saturate)` |
| 12 | `operand lvm_index` |
| ~31 | `no-ir` / `no-tail`, one apiece across ~25 handlers |

**90% of everything left is a call or a branch, and that is one statement: a spliced body is a
straight line, and the corpus's closures are not.** `frun2p` (the `p` in `qap`/`qqp`) is a
push-push *with the apply on the second load* — it enters the callee's thread with a return
address — and the `cond` family reads a **thread offset** as its operand, which a splice does not
have. Both want the same thing amble already built for itself: a stackless drive out and a resume
label. Until that lands the splicer covers whole closures only when they are pure accessor chains,
and the corpus has few of those *as whole closures*.

⚠ **REFUSED the same day: raising the `-fir` cap.** The `no-ir` row looked like a cheap rung — one
number in `gen.l`. At 128 it takes 118 handlers instead of 93 and costs **+40 KB** of binary, and
the corpus reads **629 declines and 11 firings against 626 and 11 at 64**: worse by noise, zero
firings bought. The reason is legible once measured — generic `+` is 0 forms at *either* cap
(`lvm_add`, `lvm_mul`, `lvm_eq` are over 128; `lvm_lt` is 89), because the NxN kind-dispatch table
is what makes them big. **The splicer is never going to say generic arithmetic**, and it does not
need to: the glaze's own arithmetic lanes are for exactly that, and they run first. Do not rebuild
this rung on the strength of the `no-ir` names.

### branches: the thread already carries its whole graph (2026-08-13)

The line above — "a spliced body is a straight line, and the corpus's closures are not" — was
right about closures and **wrong about threads**, and the difference is the whole rung. *A thread's
control flow is entirely internal to it.* `lvm_jump`, `lvm_cond`, `lvm_argcond` and
`lvm_argtwocond` all take `Ip[k].m`, a **same-thread cell pointer**, so a closure carries its
entire CFG and nothing about reading one asks the VM to change — in particular **`lvm_jump` does
not need to become relative**. What `dis` refuses to hand out is the *pointer*; the *index* is a
charm and perfectly safe, and `seek` (the address **of** a cell) is the door between them. So a
target is relativized at **read** time, where the base is known.

**`disg` (love/ev.l, `dis`'s sibling in the same scope) — LANDED.** The rows a closure can reach,
index-sorted, each `[idx nom operand..]`, every branch target a cell index:

```
(f x) (? (two? x) (cup x) 0)
dis   ((lvm_argtwocond 0 x) (lvm_argcup 0) (lvm_ret))                      ; the linear prefix
disg  ((0 lvm_argtwocond 0 7) (3 lvm_argcup 0) (5 lvm_ret) (7 lvm_quo0) (8 lvm_ret))
```

⚠ **both sides are re-read at every step of the scan.** A collection between two comparisons moves
the thread, and a held target address would then quietly match a *neighbour* rather than fail —
a well-formed answer naming the wrong cell. ⚠ `'torn` anywhere is `'torn` everywhere: a graph read
is all of it or none, since laying out an arm you could not read means laying out a jump into
nothing. ⚠ and `dis` keeps its own contract, unchanged and independently gated: a graph walk that
dies on one unreadable arm must not cost the linear reader the prefix it can still deliver.
The law that matters is **closure** — every target names a row that IS there (`test/dis.l`).

Two gate lessons, both paid on the way in and both about *where a law can be said*. The closure
law was first written with `cuap` (`cup∘cap`) for a row's nom, which takes the cup of the **index**
— so no target ever matched, closure was never tested, and the gate read green. It reads green on
the host either way, because the glaze natives those closures and both readers correctly answer
`()`. **love0 has no glaze**, real rows reached the law, and it failed there — which is the whole
reason that differential is kept. The mirror-image mistake came first: the *reach* law (a branching
closure's graph is strictly bigger than its linear prefix) is a **positive** claim and cannot live
in a glaze-invariant gate at all, where both readers answer `()` and `(< 0 0)` is false. It lives
in `test/gate/splice.l`, under `LOVE_NO_GLAZE`. ⚠ and `dis`/`disg` come out of **one** top-level
leak, taken apart inside it: a second top-level form looks equivalent and is not — love0 surfaces
ev.l's late leak differently, and the split bound both names to `()` there, which reads as PRESENT
and broken rather than absent.

**And the real obstacle turns out not to be the branch at all — it is `Ip`.** `lvm_argtwocond`'s
own IR is 33 forms with no call and no frame: it reads its operand at `(ld r0 r5 8)`, tests tags
against `lvm_chain`/`lvm_sym`/`lvm_nom` (`la` refs, which the splicer already turns absolute), and
picks between `(add r8 r5 24)` — `Ip+3`, the fallthrough — and `(ld r0 r5 16)` — `Ip[2]`, the
target. **Every one of those is a compile-time-known cell**, because `disg` just handed us the
whole thread. So the lever is not "say the branch ops"; it is **constant-fold `Ip`** — and the
same fold dissolves the *entire* `operand` bucket at once, including the hand-written push family
already spent on `aa`/`aq`/`qa`/`qq`, and `lvm_index`, and the `p` variants' operand halves. It
wants a small symbolic domain over the handler IR (`{known word, cell j, unknown}`) with a join at
labels, and `(mov r5 rX)` + `(jmpr r0)` becoming `jmp` to the label for cell j. ⚠ `lvm_jump` is
the free case and needs no analysis at all: 3 forms, target known, one `jmp`.

⚠ the family splits by its *predicate*, not by its shape: `lvm_argtwocond` is inline tag tests and
is in the IR table; **`lvm_cond` and `lvm_argcond` are absent from it**, because both call
`ai_nilp` and a call needs the frame that puts them over the cap. Folding `Ip` gets `argtwocond`
(28 declines) and `jump` for free and leaves `cond`/`argcond` (27) wanting a callable predicate.

### ⚠ the blocker-SET census, which refutes the ranking above (2026-08-13)

**A first-blocker table cannot say whether removing a rung moves a closure to native or merely to
its next wall**, and this doc twice ranked rungs off one anyway. `jit-blockers` walks the whole
graph and tallies *every* op the splicer cannot say; `jit-class` then asks the only question that
matters — what would have to be built for THIS closure to splice. Over the corpus, 2939 closures
reaching the last lane:

| class | count | share | what it means |
|---|---|---|---|
| **call** | **2557** | **87%** | contains an apply. No branch or operand work reaches it. |
| other | 233 | 8% | no-IR ops only: `=` 440, `><` 198, `peep` 137, `nil?` 135, `sat` 28 … |
| branch-plus | 99 | 3.4% | a branch *and* something else |
| none | 30 | 1% | blocked by nothing — and only 11 fire (see below) |
| **branch-only** | **20** | **0.7%** | **the entire payoff of folding `Ip`** |

**So `Ip` was about to be built for twenty closures.** The rung was ranked #1 an hour earlier off
`qap` 231 + `qqp` 204 + the cond family, and every one of those first-blocker counts was a closure
that would have hit a call two rows later. The Ip fold is still the right *mechanism* — the
analysis in the section above holds — but it is a 0.7% rung and must not be built next.

⚠ **calls are not one rung among several; they are 87% of the entire question.** Nothing else in
this table is worth building first, and a splice tier that cannot cross an apply cannot be fast on
this corpus at any coverage of anything else. The shape is known and is amble's: a stackless drive
out, a resume label, deopt by restart. That is the next real piece of work, and it is a large one.

⚠ the `none` 30 against `fired` 11 is the one cheap gap left, and most of it is **a quote of a
non-charm**: `dis` answers `'x` for a heap quote, which the blocker scan counts sayable and the
splicer correctly refuses — the word is a heap pointer and moves under the collector. Honest, and
worth about nineteen closures.

### keeping `-fir` — chosen (revisable), and what would change it (2026-08-13)

**Price, measured, not estimated:** `love.o` is 447,502 bytes with `-fir=lvm_` and 417,733 without
— **29,769 bytes**, matching the cap-64 curve exactly (93 handlers / 29 KB), and **0.45%** of the
6.57 MB binary. ⚠ **only `host/build.mk` carries the flag.** The freestanding targets — the
kernel's `$(KCC)` lane, wasm, the device ports — do not, so the seats where bytes actually matter
pay nothing. `dist` re-bakes the host binary, so the download door carries it.

**⚠ and the splicer is not the only reader — which is the argument that actually settles it.** The
compiler's IR sitting beside the code it emitted is a **provenance record**, useful to anyone
opening the binary, and the moment it was read *as* one it found a bug in itself. `make vmret`
checks the VM's central invariant — every `lvm_` ap tail-jumps, never returns — by shelling out to
**objdump**, picking a return mnemonic per `e_machine`, calling itself "a first-pass heuristic" in
its own header, and **skipping silently when no disassembler is installed**. In a tree whose boast
is that the link, the assembly and the compiler are all ours, that one instrument borrowed
binutils. The record states the same law with no tool and no arch knowledge at all — and when the
two readings were put side by side they **disagreed**: objdump said all 310 functions were
ret-free, the record said `lvm_scare` had a `ret`.

**objdump was right and the record was wrong.** `irblob` broke its slice only on an *exported*
name, so a file-scope **static**'s body rode into the previous row: `lvm_scare` carried
`missing_tag`, `lvm_eval` carried `ap_next`, each row ending in a stranger's `ret`. Fixed by
ending a function at any label that is not dot-prefixed (dot-prefixed ones are gen.l's internal
labels), which pays twice — the two bad records are correct now, and **91 → 105 handlers**, because
fourteen had been inflated past the form cap by a neighbour they had swallowed, for +4 KB. ⚠ **the
splicer had survived this by luck**: its "ends in the dispatch tail" check happened to reject both,
and a swallowed neighbour that ended the right way would have spliced foreign code in whole. Both
laws — one record, one function; no record contains a `ret` — are now `test/gate/splice.l`, a
second reading of `vmret` that needs no tool. ⚠ it covers the 105 under the cap where vmret covers
all 310, so it does not replace vmret; they are two presentations and are meant to agree.

**Kept, and the price argument is secondary to that one.** The IR table and the **symbol table** are a
matched pair: the IR says what an op does, the symtab binds its one `la` reference to a live
address, and either alone is useless to a splicer. The tree has *already* made this exact call for
the other half — `mk/install.mk` ships **unstripped on purpose**, paying ~2% (four times this) to
keep the table holo lays. Dropping `-fir` would be paying that 2% for nothing. And it is the whole
design: the alternatives were scraping `love.c`'s text, which a shipped love does not carry, and
disassembling our own machine code, which hands back bytes the splicer cannot reason about. A love
that can read what it is made of is of a piece with `dis`/`disg` — reflection doors the mop leaves
open deliberately.

⚠ **what would change it:** the splicer's payoff, not the table's price. If the call rung lands and
the corpus number is *still* negative, `-fir`, the splice lane and `lib/splice.l` go together — one
decision, not three. Until then the 29 KB is the cost of the tier being possible at all. ⚠ and do
not re-open this as "shrink the cap": that was refused on its own evidence (128 costs +40 KB and
buys zero firings) and the `other` 8% is the table to re-price a cap *raise* on, if ever.

### the ev inliner is already on, and it is the same rung as calls (2026-08-13)

`feel`'s `cprop` (love/ev.l) is a real beta-reduction inliner and it runs on **everything** —
`(wx (cprop x () 64 0 0) 0)`, fuel 64, no switch. So the 87% above was measured *with* it. Three
threads say where its boundary is:

```
same-scope, pure body:   ((lvm_aa 0 1) (*) (lvm_aa 1 2) (*) (*) (lvm_ret))    ; inlined, no call
global callee:           ((lvm_quote x) (lvm_qap x 2) (lvm_tap))              ; a call
same-scope, impure body: ((lvm_quote x) (lvm_quote x) (lvm_argap 2) (lvm_tap)); a call
```

**Where it fires it produces exactly the thread the splicer wants** — `sq (sq y)` becomes
straight-line arithmetic with no call at all. ⚠ but the lever is **admission, not fuel**: 64 → 4096
moves the census by one closure (call 2557 → 2558, everything else identical) and was reverted.
Admission has two halves:

* **global callees are never attempted.** `cprop` resolves a head only through `se`, the local
  static env, which carries `:`-scope bindings. In a corpus nearly every call is to a global — and
  `lvm_qap` (push quote, push arg, apply: a **statically known** callee) is the largest single
  blocker at 765.
* **the purity gate is ~50 primitives**, and `pbody?` needs every call head in the body inside it.
  ⚠ do not widen it casually: `tally` is absent for the same stated reason `peep` is — the
  container doors are pure-or-not depending on what they are handed, and `tally` on a tablet reads
  mutable state. That exclusion is correct.

**So the inliner question and the call rung are ONE rung.** Inlining a global callee inside `feel`
would make every compile redefinition-stale, which changes the language and breaks the repl. But
the glaze's callout lanes already resolve their callee via `gv` at compile time and are documented
as *"redefinition-stale like any baked global"*, and the splicer installs natives with a bytecode
twin to deopt into. **A splice-time inline of a quoted callee is the same bet in the one place that
already makes it** — and `lvm_qap`'s 765 says a large share of the 87% is the statically-known end,
which is the easy half of that rung and does not need a stackless drive at all.

**So the rungs, ranked by the blocker-set census — which is the one to trust:**

1. **calls** — 87%, and nothing else comes close. ⚠ and it splits, which is the useful part: a
   **quoted callee** (`qap` 765, `qqp`) is statically known, so the splicer can INLINE its thread
   the way `cprop` inlines a local — no drive, no resume label, just more rows — on the same
   redefinition bet the glaze's callout lanes already take and with the bytecode twin already
   there to deopt into. A **dynamic callee** (`ap` 585, `tapn` 527, `argap`) is the harder half
   and wants amble's shape: a stackless drive out, a resume label, restart deopt. Do the quoted
   half first: it is bigger, cheaper, and reuses a bet the tree has already priced.
2. **the no-IR ops behind `other`** — 8%, and `=`/`><`/`peep`/`nil?` are ordinary generic ops
   whose handlers are over the `-fir` cap. ⚠ note this is NOT the cap rung refused above: that one
   was priced on first-blocker counts and bought zero firings. Price it on this table instead.
3. **fold `Ip`** — the mechanism is right and the payoff is 0.7%. Build it when calls are done and
   it is the thing in the way, not before.
4. **the `no-bridge` sixteen, and it is not the one-liner it looks like.** `two?`, `sat`, `><`,
   `quit` are prel **aliases** — `(: two? link? … sat saturate)`, one value under two spellings,
   `id?`-identical at runtime. `dis` names by book key and gets the alias; `ai_nif_lvm` is derived
   from `nif-rows` and only knows the original. So the fix belongs where the alias is made, not in
   a lookup table inside the JIT — and a written-down `two? → link?` row in `lib/splice.l` is
   exactly the duplication `nifs.l` exists to prevent. ⚠ this is why the gate's `sm-pred` sample
   still declines: it is the only sample whose op is an alias.
5. everything else in the table is one or two closures apiece and is not worth a rung.

⚠ **the lane must decline CHEAPLY, and today it does the opposite**: the first thing `jit` does is
`dis`, the most expensive thing it does, and 1770 of the 1969 declines then throw that walk away
over an arity the hook already knew and could have passed. Rung 1 dissolves this particular case by
covering it, but the ordering law stands for every rung after: a tier that fires on every closure
the runtime builds pays its decline cost on all of them.

**What is cached, and what that cost.** Three memos, all load-bearing rather than tidy: the live
symbol table (one build per process — it slurps a 6 MB `/proc/self/exe` to get there — plus a memo
per name, since the symtab walk is linear and string-compares every entry), the checked clean body
per handler nom (the checking is per handler, the splicing per copy), and **the blob, keyed on the
op rows themselves** — the same rows denote the same bytes, so a second closure of a shape already
spliced pays a tablet lookup and a `nif`, not an assemble. That is the glaze's own arrangement
(compile per source site, creation reuses the blob) and it is what makes a creation-time tier
affordable at all: **31 ms → 216 µs** for a repeat shape, 438 µs for a fresh one. ⚠ the bytes carry
live addresses, so the cache is as in-process as they are — a field of this love, never written
out, and nothing here may ride into a baked image.

## the splice JIT and the moon arc — one boundary, aligned

⚠ **the splice JIT is not a second consumer of this arc's output; it is the same problem one
abstraction level up, and `doc/hom.md` already says so.** The VM's threaded code is
Hom(−,R) with R = `Sp[0]`: every op body ends by delivering its value into the stack cell and the
next op reads it back. That is exactly gen.l's own historical protocol — every expression delivers
into r0 and the consumer relocates — which the destination die exists to retire. The composed body
pays the representative-object bridge at every op seam for the same reason gen.l paid it at every
expression seam. `sl-cross` (test/uuspllaw.l) already proves the two machines are one design; this
is that theorem's engineering face.

**Where the information passes.** The composed body (`lib/splice.l`'s composer) is ONE C function
over one base with constant offsets, and love.h already declares that base non-aliasing:

```c
#define _lvm(n, ...) struct ai *n(struct ai *restrict g, union u *Ip, ai_word *Hp, ai_word *restrict Sp, ...)
```

⚠ so the promise the JIT would want to make is **already in the source, and mooncc throws it
away**: `parse.l`'s `pquals` skips `restrict` at the token level ("cc keeps no linkage/qualifier
state"), so `gen.l` never learns it. That is the boundary — not a new channel to build, an
existing one that is closed.

**The measured seam, reproduced in ten lines** (2026-08-13). Four `Sp[0] = f(Sp[0])` statements:

| | instructions | the dependent path |
|---|---|---|
| mooncc before | 13 | `mov %rax,(%r11)` / `mov (%r11),%rax` — **four full round trips** |
| cc -O2 | 7 | one load, one store, everything else in registers |
| **mooncc after (below)** | **11** | **register-to-register: add, imul, add, imul** |

That is the `0.83×` row in "the splice client" above — the composed body running SLOWER than
dispatch where cc gets 1.91× — isolated to a file you can read in one breath.

### rung S-1 — `stldp`, the adjacent pair on any base — LANDED 2026-08-13

`stld` folds a store followed by a load of the same slot, and its own comment already carries the
law: *"only the pair with nothing between is aliasing-proof"*. ⚠ **it needed no aliasing story and
was asking for one anyway** — both lanes gate the base on `r4`, the frame. A store to an address
immediately followed by a load from that same address yields the stored value whatever else may
alias it, because nothing runs in between. `stldp` is that pass over any base BUT `r4` (r4 stays
with the existing lanes, so the slot map, `stldkeep` and the epilogue anchors cannot move).

Twelve lines. love.c `.text` −20 B, corpus **−1,509 dynamic instructions** (five runs, range
disjoint from the baseline's), `test_fixpoint` byte-identical, `test_moon` green including its own
`stld` residency law, `test_slow` seven zz-fin lines. The client it was built for gets its whole
interior reload chain back.

⚠ **and it is an x64 win ONLY, which the size columns hid.** arm64 and riscv64 `.text` did not
move — not because the pass is neutral there but because **it never sees an `st` form at all**: a
print at `stldp`'s store test fires on x64 and is silent on arm64 for the same probe, whose arm64
emission still carries every `str`/`ldr` round trip. So the seam the splice JIT pays is untouched
on three of four targets. ⚠ this is the tree's own "a guard that hides a target also hides the bugs
that target would have caught" wearing a quieter face — there is no guard here, the pass simply
finds nothing, and a size delta of zero reads identically to a pass that ran and had no work.
**Rung S-1b: find where the arm/riscv pipeline puts those pairs and reach them.** Do not quote
S-1's number as a cross-target result.

### the ladder they share

⚠ merged into **THE LADDER** in the convergence plan below — one list for both levels. The
steps this section used to carry are numbers 1, 2, 4, 6 and 7 there.

### what moon gets back

* **a gauge the corpus cannot give.** A composed body is a pure dependent chain of store→load
  seams — it isolates lever 2 with nothing else in it, where `spec.l` averages the effect away to
  four digits. `bench/vmsplice/auto.l` already times it against its interp twin.
* **a second SHAPE of C.** love.c is hand-written; composed bodies are machine-generated,
  straight-line, one base, no locals. ⚠ this is the A-2 lesson as an instrument — the op census
  that was short by one was read off love.c alone, and a corpus with different physics is what
  catches that class.
* **a client whose win is WALL CLOCK at flat-ish insn counts**, which is the reading the a64 loop
  borrow verdict is still owed an instrument for.

## the physics — what prices a lever here

Learned by measuring, several times each; check a new lever against these before building:

* **This OoO desktop runs mooncc's code at IPC ~3.** An insn-count lever off the
  dependent path buys slack, not time: lea fusion, store-side addrfold copy-prop, and
  cmp-mem fusion all measured non-wins or regressions and were reverted. What pays:
  removed WORK on dependent chains (loads, frames, cells), fetch density with no
  critical-path cost (imm8 encoding, branch relaxation), and killed branches.
* **cmp-mem serializes load→cmp→branch** — the split form lets the OoO engine hoist the
  load. It's a density lever only; this is why real compilers keep them split too.
* **Alignment aliases the BTB.** align-16 measured −4% and was reverted; dense unaligned
  packing wins on this frontend. Corollary: **single-layout wall/cycle deltas under ~4%
  are unresolvable** — bench pad variants (an inert `ai_layout_pad` fn, sweep its size,
  median per side). Five landed rungs had their cycle read overturned by the pad probe.
  Even dyn-insn counts wiggle (±0.05% seen); measure the pad spread before invoking it.
* **The ship gate is "pays somewhere, regresses nowhere."** The host hides removed work
  as slack; mooncc's in-order targets (teensy, rp2040) cannot. A wall-neutral insn cut
  with clean semantics lands.

## the open levers, ranked

1. **Array slots** — a fixed-size local array with constant indices is a register file
   wearing a bad disguise; an element gets no vmap pin, so every touch is load+store.
   Measured 23× on chacha20 vs 1.77× on scalar poly1305 (bench/ccbench.sh carries both
   rows); love.c's z-tray elementwise lane 4.8×. Worth more than every landed rung
   combined on its shape. Watch: chacha and poly should close TOGETHER toward poly's
   ratio. FIRST RUNG LANDED 2026-08-10 (the vmap's array leg, the rung ledger below):
   element pins with write residency — chacha −19% wall, poly −10%, moving together as
   the diagnosis demanded. SECOND RUNG LANDED 2026-08-10 (frame-direct, the ledger):
   the clval fold — a constant index is a static slot, the lea/add/park dance is gone,
   deadst elides write-only element stores in call-free fns, and element fns stopped
   pinning fesc (sibcall/ride/homing open). The store-elision half CLOSED 2026-08-11
   from lever 2's side (the exit meet, and the element arm before it: element pins now
   ride calls on seats and their write-through stores sweep). What remains of this
   lever is float/pair elements riding the old walk (afd's whitelist is gp+ptr) —
   the ranking above is the shape's, not the residue's.
2. **Registers as the source of truth** — MEASURED 2026-08-11, twice. Frame-relative movs
   are **120,007 B** of mooncc's love.o against gcc's 27,328 and clang's 9,762, so the
   excess is **92,679 B, 78.2% of the 118,531 B codegen gap** — this lever is more than
   three quarters of what is left, and its share ROSE as the gap shrank. The ~22%
   static-insn figure below reads 28.5%. ⚠ the first reading of this lever said the arc's
   early rungs could not reach it — repack's scan barred 77.1% of that traffic on an
   UNCLAIMED touch, the lane being the **spush cell** (spush reserves 16 bytes with its own
   sub and never nslots; spmerge folds the sub into the prologue's, so the frame grows and
   the map does not). That rung landed the same day and **the bar is now empty**: 403 of
   640 fns pack, carrying 92.9% of the frame traffic, and every one of the ten remaining
   refusals is a raw inline-asm splice repack skips by design. Zero loose, zero unclaimed.
   doc/moon-alloc.md's rung 2 entry carries both censuses.
   The allocator leg proper: liveness over ir1,
   values surviving labels and calls, spill placement instead of write-through. Kills
   both the def-stores and the post-flush reloads (the ~22% bucket). The vmap, the JOIN
   meet, the cs pool and cskeep are its substrate; this is the step the peepholes cannot
   take (their reach measured exhausted 2026-08-04). THE ARC IS PLANNED: doc/moon-alloc.md
   — two phases (slots-become-intervals post-choice, then vreg emission), seven rungs,
   and the retirement schedule for the five parallel residency mechanisms. Its emission-side half — passing
   the CONSUMER down as a destination die instead of delivering everything to r0 — is
   modeled runnable in doc/proto/dest.l (ev.l's continuation-taking emitter shape worn
   by a register machine; gated by test_doc). All four lanes migrated 2026-08-10 —
   asn/decl, cbranch-compare, arg-seat, bin-value (the rungs below); what remains of
   the die rides the allocator leg (callish sides via cs-borrow parks, deeper arg seats).
   ⚠ this lever has a SECOND client with its own price: a splice JIT over the VM, where
   every op boundary is a write-through and the gap costs ~2.5× on the spliced body —
   "the splice client", above.
   FIRST BOUNDARY CROSSED 2026-08-10 (loop-head survival, the ledger): the map keeps
   across loop heads under optimism-with-verification, writes re-establish in place,
   and chacha20 dropped 74% of its wall — call-free hot loops now run register-resident.
   SECOND BOUNDARY CROSSED 2026-08-10 (the cs borrow, the ledger): a callish loop's
   scalar keeps migrate onto free callee-saved seats and ride THROUGH the calls —
   the call-loop shape drops ~20% wall at flat insns (the slot's store→load chain
   breaks). THE CALLISH BAR LIFTED 2026-08-10 (the aim hold, the ledger): the why was
   the free-list/pin split, and with it fixed a callish rhs aims and pins — `s +=
   f(..)` accumulators now ride seats through their own calls. SEAT EXHAUSTION
   ANSWERED 2026-08-10 (spill-around, the ledger): the surplus pins keep on their
   pool regs and reload per call, priced reads-vs-calls. KEEP DEPTH ANSWERED
   2026-08-11 (entry seeding, the ledger): a keep no longer only preserves — a
   loop's hot unpinned scalars pin by one seed load before the head, so inner
   loops rediscover what an outer keep dropped, and stld+deadst routinely erase
   the seed's memory touch entirely. SPLICE-CROSSING KEEPS ANSWERED 2026-08-11
   (the end-label meet, the ledger): a splice's end label is a forward join, not
   a clobber, so a loop keep now rides an inlined call -- straight-line, branchy
   and early-return inlinee bodies cost the pins nothing, and a callish inlinee's
   real call is spared by the same vmcflush the extern call gets. ELEMENT PINS
   ANSWERED 2026-08-11 (the element arm, the ledger): a constant-indexed element
   of a non-escaping frame array is a static slot, so lomig seats and rosters
   its pin exactly like a scalar's, and a written element re-establishes IN its
   seat. STORE ELISION ANSWERED 2026-08-11 (the exit meet, the ledger): the loop
   exit label is the THIRD join (if-joins, the splice end, now ld) -- kept pins
   ride out of the loop over the meet of the cond edge (kp-verified pairs only)
   and every break edge's snap, post-loop reads become zero forms, and with the
   slots load-free deadst sweeps the write-through stores and entry spills whole.
   What remains of the def-store bucket: the pre-call park pair and rostered
   pins' true reload stores (both priced, both earning their keep). PARAMS
   SEATED, pmin-GATED 2026-08-11 (the callish cs homes, the ledger): a priced
   callish fn whose EVERY path calls homes its params on callee-saved seats,
   wrap-free. SHRINK-WRAP BUILT AND SCOPED 2026-08-11 (the ledger): the wrap
   machinery landed (statement-level split, dual-epilogue sibs, pminp), and
   its A/B ladder settled the fleet question -- pp's per-call wraps are
   COLD-PATH wraps, dynamically right for the early-out profile; the static
   8 KB they represent is already-paid-for at runtime, and beating them needs
   a frequency signal (PGO), not a better static model.
   SHRINK-WRAP RETIRED 2026-08-12 (rung 3's first half, the ledger): it was worth
   48 bytes and one grant, against 113 lines and a per-form cost in `sibs`.
   STRAIGHT-LINE CALL CROSSINGS ANSWERED **NO** 2026-08-12 (5.1b iv-b, built and
   reverted; the ledger): keeping a pool pin across a call costs +545 insns, +498 of
   it frame traffic — the lever aimed at this bucket and grew it. 630 of 1,088
   crossings are never read again, the flush cannot tell them from the 454 that are,
   and a seat bought mid-function is a copy. The loop crossings above are the whole
   prize; the rest waits on a value BORN callee-saved, which needs the vmap retired,
   not extended. Do not rebuild it. THE CENSUS THAT PRICES THE REPLACEMENT ran the
   same day (the ledger): ~97% of loop-weighted reads are on call-crossing names, the
   callee-saved file is idle in every function of every target, and arm64 holds every
   crossing name at once in 93% of fns against x64's 62% — so iv's assignment gets
   built against **arm64 first**. doc/moon-vreg.md carries the four-phase plan.
3. **Compare staging want-hints** — HELD for the allocator. Landed for the call-free
   side 2026-08-10: cbranch's left aims at its park before evaluating, so member loads
   deliver and the bridge mov dies. What remains is the callish side (the sp cell across
   a call — a cs-borrow park would need the wrap pricing), and doc/moon-alloc.md's rung 2
   subsumes it: a call-crossing interval on a cs seat is the same value by another name.
   Do not build it separately.
4. Recorded small residues: leaf sp-fn stldw coverage; a non-positional fallback home
   (mag_cmp's g1 loses its seat and stays slotted); cs-borrow park elision (params on
   cs regs landed pmin-gated 2026-08-11, the ledger; the fleet's share went with
   shrink-wrap, retired 2026-08-12 — it needs a frequency signal, not more machinery);
   arm64 x19+ cspool (empty there today); register-binding splice
   depth (map_probe's &-decline); d128 params ("a wide arg: not carried"); variable
   index±k rebase; narrow cmp fusion (632 sites, needs cc-aware licensing); the
   3-address dance emission (encoder territory — the reverted lea-fusion physics, only
   density-neutral shapes need apply).
5. **Bytes that are not codegen** — CLIMBED 2026-08-11, and named here so the headline
   ratio is never read as this catalog's score. Two moves, both in doc/moon-diff.md's
   fills: the dead-static sweep (mooncc laid the out-of-line body of every static it had
   already spliced — 149 bodies, 32,768 B; love's own unreachable text 6.4% → 0.6%,
   under gcc's own 2.6%) and nolibc's split to 185 per-function members (libc 64,110 →
   31,662 B, dead 55.1% → 5.4%, under musl's 6.5% floor — archive granularity, no
   linker gc). Whole binary 1.99× → 1.779×. ⚠ **the corpus did not move** (40.74 → 40.72 G):
   every byte was unreachable, so none of it is the gap levers 1-4 measure. The two
   ratios answer different questions and only the both-emit codegen one prices a rung
   here. Residue: 12 libc functions sharing a file with a live sibling, ~3 KB
   unreachable left in love's own C, and the inlining question the sweep re-opened —
   mooncc's remaining symbol surplus over gcc is now a real inlining difference rather
   than bookkeeping, and has never been sized.

## the convergence plan — gen.l and the splice JIT, as one plan

⚠ **START HERE.** This section and THE LADDER inside it are the arc's only live plan, covering
both levels: gen.l's residency machinery and the splice JIT over the threaded VM. They are one
problem — see "the splice JIT and the moon arc" above for why — and were two plans until
2026-08-13. Read, in order: the criterion, the flush census, the root, THE LADDER.

Recon verdict, 2026-08-13: **incremental, and a from-scratch rewrite is refused.** Three findings
settle it.

**The defect is 14.5% of the file.** The machinery this plan touches is 1,211 lines — the
vmap/pins/keeps/`loscan`/`lomig`/`loseed` region (390), `lpick`/`ntouch`/the regen driver (400),
`alive`+`rgset` (350), `rdsp` (71). The other **7,131 lines are target lowering, ABI, type layout
and four backends**, which have no design problem and carry every hard-won cross-target fact.

**And a rewrite cannot inherit the safety net.** `test_fixpoint` rebuilds `love` BYTE-IDENTICALLY,
`test_ccarm64`/`test_ccriscv` differential 129/128 programs against gcc on two arches,
`test_raw_arm64`/`test_raw_riscv` run ~4,150 tests each. Byte-identity is the anchor every rung on
this arc has steered by, and a new `gen.l` has none — while still needing the old one to build it,
since mooncc is self-hosting.

**One constraint does not dissolve either way.** The keep decision must PRECEDE the emission that
determines whether it is valid, so optimism-with-verification is a reasonable answer to a real
phase-order problem, not accumulated cruft. ⚠ `rdsp` is a MACHINE-form transfer function (`li`,
`mov`, `push`, the alu roster) — it cannot supply the parse-tree clobber fact `loscan` needs
before emission. Any plan that assumes it can is wrong; this one did, and was corrected.

### the criterion — what this arc is actually for

⚠ **the goal is that mooncc becomes intrinsically a better program, with more of a sense of what
it is doing** — its representations should carry the facts its decisions depend on, rather than
those decisions being guessed, re-derived, or discovered by being wrong. Performance is NOT the
objective here.

⚠ but neither is "stop measuring". This tree measures because plausible stories were wrong
repeatedly, including three times this week. The shift is that **measurement is demoted from
objective to FALSIFIER**: a number's job is to catch you being wrong about what the program knows,
not to authorize a change. The census below did exactly that — it killed the hypothesis this
section was first written around.

So the ship gate for this leg is: **did the program stop guessing something? did a mechanism come
out? is performance not badly regressed** (a floor, not a payment). That is already latent in the
arc's own rules — `gen.l` 8,125 → 8,012 was recorded as a milestone in its own right, and the plan
already says no rung ships without a mechanism coming out — it was the perf gate that overrode it.

### the flush census — 29 sites, and what actually kills a live pin

Every `vmflush` call site numbered and counted over love.c on four targets plus the 133-file
`test/cc` corpus (2026-08-13, temporary instrument, reverted). **16 of 29 sites never kill a live
pin anywhere measured** — the expression-level lowerings (`dtoul`, the t32 pair shifts, `clz`/`ctz`
on rv, `va_arg`'s walk, the VLA lane) sit on targets or paths where the vmap is empty. ⚠ thumb2
fires ZERO of them: its `pool0` is empty, so it has no residency to lose at all.

Of the 514 fires that DO kill a live map on love.c/x64:

| what fires | fires | share |
|---|---|---|
| **`vmcflush` "degenerating"** — a call with `csbor` and `saro` both empty | **223** | **43.4%** |
| **the loop machinery flushing ITSELF** — head/step/cond with an empty keep | **116** | **22.6%** |
| foreign-edge statements — `case`, the switch end join | 88 | 17.1% |
| lifecycle — `rgreset`/`rgset`, per-fn entry and regen teardown | 69 | 13.4% |
| `fcb`'s leading flush — a ROLLBACK, not a clobber | 10 | 1.9% |
| loop exit / splice end with no arriving edges | 8 | 1.6% |

⚠ **this FALSIFIED the plan's first draft.** That draft said the root was a knowledge gap at
expression grain — `loscan` enumerating seven statement constructs while the flushes live in
lowerings it has never heard of. The static reading supported it; the dynamic count does not.
Exactly ONE expression-level lowering ever kills a live pin (`clz` on riscv), and `fcb`'s is a
rollback. **The two-drifting-lists story is real but nearly worthless**, and an effect table built
to fix it would have been correct and bought almost nothing.

### the root: the map has no notion of CLASS

A pin's residency class — pool, cs seat, rostered, home — is known at pin time and **is not
recorded on the entry**. The information exists (`pool0`, `csbor`, `saro`, `homes` all hold it);
it just is not attached to the thing it describes. So the honest sentence *"pool residency ends
here"* is unsayable, and 29 sites each decide what to kill with the only verb available.

That is what the two big rows above are:

* `vmcflush` does not "degenerate" — it correctly kills the pool class at a call. It reaches for
  `vmflush` only because with no cs seats and no roster there is nothing to *spare*, and sparing
  is the only way it can express the class. Given a class, there is no special case at all.
* a loop head whose own loop-scoped keep is empty flushes **every** class, including
  function-scoped cs seats it has no business touching. It is the residency mechanism flushing
  itself for want of a way to say which scope ended.

**Together, 339 of 514 pin kills — 66% — are one missing field.**

⚠ **the second bullet was wrong, and step 1 building it is what said so** (2026-08-13). Those 116
fires are `(? (two? kp) (vmset g kp) (vmflush g))` — a loop head **installing its keep**, and the
flush arm is the install of an empty one, not a kill site with a vocabulary problem. Sparing a
class there is unsound: `lokeep`/`lochk` verify the head's arriving edges only for the entries in
`kp`, so a pin spared outside it rides a back edge nothing checked — a body that re-pins the name
elsewhere then makes a top-of-body read wrong on the first arrival. The flush is that join's
correctness. **Step 1's honest reach is the 223-fire row alone, 43.4%**, and the conditional
retires because `vmset` of an empty map already IS the flush.

### the gate census — the seven never disagree, and that is the finding

⚠ run 2026-08-13 to aim step 5, and it **falsified the premise it was run on**. The plan said one
pricing would resolve contradictions between the gates. Over love.c/x64 — 4,934 verdicts from the
seven gates, deduped to each function's last regen attempt — there are no contradictions:

| | |
|---|---|
| `lpick` **grants**, a loop gate **refuses** | **0** |
| `lpick` **refuses**, a loop gate **grants** | **143** |
| `lomig` vs `loseed`, either direction | 0 / 4 |

The relation is one-directional and structural. What reads as disagreement is the gates pricing
**different classes for the same value** with nothing in the program saying so, and the honest
sentence — *"does not pay as a pool home; does pay as a borrowed seat"* — was unsayable. So the
table does not arbitrate. It makes the class the axis, and that sentence sayable.

⚠ **two instrument artifacts nearly buried this, and the raw census read 61% disagreement.**
`loseed`'s `nocand` is not a refusal — it is declining to CONSIDER a name that already holds a
register (a homed local is `regv`, not `loc`), and all 311 "lpick grants, loseed refuses" rows were
exactly that. `paid?` records under the caller's function but prices the INLINEE's parameter
namespace, so its rows fake collisions. Neither is visible without asking what a verdict means.

And the census named the axis that actually decides. Of `lpick`'s 1,036 refusals:

| why | |
|---|---|
| `tc ≥ 2` but the function CALLS — the `free?` bar | **776** |
| `tc ≤ 1` — too cold for any class | 151 |
| `tc ≥ 2`, call-free, lost the seat cap | 109 |

**Three quarters of all refusals are the crossing bar, which is not a price at all** — a structural
veto that pre-empts pricing. In the table it becomes one cell: the pool row costs `'never` across a
call. ⚠ and the ~633 warm crossing values the loop gates do NOT rescue are not step 5's to win:
5.1b iv-b already measured that granting them costs +545 insns. Saying why they are refused, once,
is the whole prize.

### THE LADDER — one list, both levels

⚠ **this is the arc's only live plan.** It supersedes `doc/moon-vreg.md`'s phase 1–4 (whose
censuses stay as evidence) and `doc/moon-alloc.md`'s phase I/II stance. Two ladders used to live
here, one per level; they were the same ladder and are now merged. A step's **serves** column says
which level asks for it — most are asked by both, which is the point.

**The floor already under it:** step 1 (the entry carries its class) · step 3 (`fcb` rolls back) ·
S-1 `stldp` · iv phase 1 steps 1–2 (spans, the admission repricing) · rungs A-0/A-1/A-2 (the cs
file real on arm64, riscv, thumb2) · 5.0/5.1a/5.1b i–iii.

| # | step | serves | what the program gets to SAY | gate |
|---|---|---|---|---|
| 1 | **class the vmap entry** — LANDED 2026-08-13 | both | *pool residency ends here* — instead of 29 sites each reaching for flush-everything | byte-identity where the class verb provably equals the flush it replaces; dynamic floor where not |
| 2 | **`restrict` survives the parser** — phase A LANDED 2026-08-13; ⚠ **phase B BUILT AND REFUSED the same day** | splice first | *this base is unaliased* — the promise `love.h` already makes on `Sp` and `pquals` discards | phase A: the roster exists, byte-identity. ⚠ phase B's gate was a PROXY: the probe loses its stores and nothing else does — see the refusals |
| 3 | **`fcb` gets rollback** — LANDED 2026-08-13 | moon | *discard the emission, keep what predates it* — a transaction, not a clobber | misses 81→69 reproduced; text delta owned by step 5, not by this verb |
| 4 | **S-1b — reach the arm/riscv pipeline** | both | that `stldp` has *work* on three targets where it silently finds none | a store print that fires on all four targets; the seam probe folds on each |
| 5 | **residency priced as extent × class × reload** — phase A LANDED 2026-08-13 | both | *why* a value lives where it lives, once, instead of seven gate stacks with stale proxies | phase A: byte-identity, the cost side in one table. phase B: corpus dynamic, mechanism count DOWN |
| 6 | **location keys — (base, offset, width)** | both | one key space: frame slots, array elements and restrict-base cells stop being three mechanisms | the array leg folds; `aeoff` stops parsing digits out of `"x[3]"` |
| 7 | **the die reaches the seam** | splice | *deliver where the consumer wants it* — an interior op boundary emits nothing at all | `make test_splice` against the interp twins; the ~4× ceiling the probe measured |
| 8 | **a module boundary for residency** | moon | which pass may ask what — the 14.5% visible AS the 14.5% | it compiles; the surface is declared |

⚠ **step 9 is a standing decision, not a rung: spend nothing on packing.** The span census says
ranges are near-whole-function and interval SHARING buys +4%. Use the simplest assignment that
works, and put the complexity budget in steps 1–6.

**Dependency notes.** 1 before 5 (class is the axis pricing is a function of). ⚠ **2 before 7 is
DEAD** — it read "an interior store cannot be dropped without the alias promise", and 7 does not
want an interior store dropped: `composed` reads every one of them back. 7 is unblocked by 2. 6 makes 1/2/5 sayable rather than
special-cased, but does not block them. 3 and 4 are independent and can go any time. 8 last, or
whenever the churn is low.

**Where to pick up (as of 2026-08-13).** Three doors, smallest first:

1. **step 5 phase B, the tie.** `rpays?` refuses a tie, `rclears?` accepts one, four gates use
   the first and two the second, and which gate got which reads as history rather than a
   decision — gen.l's own comment says *"an inconsistency, visible here and unsettled."* It is
   now measured, not just suspected: `pcs` refuses the `withcall` reproducer at `cost 2 gain 2`,
   and inspection says the gate is **right** — one home at one call is a genuine wash (`push`
   +`pop`+entry mov against a spill, a reload and a mov). ⚠ so settling this **will not move a
   benchmark, by construction**; what it buys is a tie-break rule that is stated. Do it for the
   criterion, not for a number, and do not let a flat result read as a failure.
2. **step 7's real blocker: make `Sp` ride.** See *the seam's root* — one pointer under two
   names, and four peephole fixes already refused. This is the ride analysis's business.
   Worth ~2.5× on a spliced body if it lands.
3. **steps 4 and 6**, both independent and untouched.

⚠ and a standing caution earned the hard way that week: **this arc's numbers are dependency
chains as often as they are counts.** 837 instructions against 713 explained none of a 2.6×.
Reach for `perf stat` cycles-vs-instructions before believing any insn-count story here.

### what stays, and why

`lochk`/`lomiss`/`lobar` and the regen retries STAY. The miss census priced the optimism as
load-bearing (the keeps are +184,000 dynamic) and found it discovering genuine conflicts.
⚠ the constraint under it is phase ordering — **the keep decision must PRECEDE the emission that
determines whether it is valid** — which no rewrite dissolves. ⚠ `rdsp` is a MACHINE-form transfer
function and cannot supply a parse-tree clobber fact before emission; any plan assuming it can is
wrong, and this one did.

But the SHAPE is still owed better: "emit, learn the clobber set, decide" is a program that knows
what it is doing; four retry attempts under progressively weaker assumptions is the same two
passes without the self-knowledge. Same cost, different program. **Write the fixpoint down as a
fixpoint even if it stays** — that is a step 5 deliverable, not a separate rung.

⚠ **the naming tell, the arc's own progress bar**: `lokeep`/`loseed`/`lomig`/`lochk`/`lomiss`/
`lobar`/`lonone` is seven names for the phases of one mechanism's UNCERTAINTY. Names that exist
only to describe failure modes say the mechanism should not have those failure modes. If the
ladder is working, most of those names disappear; if they survive, it is not.

## refusals — priced, closed, do not rebuild

⚠ this section exists because chronology buried one of them and it was built a SECOND time off a
census table that could not see the verdict. Read this list before proposing a mechanism.

⚠ **every entry here was priced under the old gate — "pays somewhere, regresses nowhere" in bytes
or instructions.** Under the criterion in the convergence plan above, a refusal on those grounds
is not automatically a refusal: a mechanism that lets the program STATE something it was guessing
can be worth a small regression. Two have already been re-opened on that basis (`fcb`'s rewind,
ladder step 3, landed; and `pcs`, whose 1,004 bytes is a weak reason to keep 57 lines the program
cannot explain). The rest stand — they were refused for physics, not for bytes.

* **cs seats for the call-crossing class, retrofitted onto `repack`** — refused twice (2026-08-12,
  then rebuilt from the frame-bucket census and re-refused to the sign; threshold sweep M=2 +715
  insns, M=3 +269, M=5 +102). **A cs seat can never be the store's source — sources are
  caller-saved** — so the rewrite is a frame touch turned into a reg-reg mov one for one, plus the
  save/restore pair. The `lvgp` class pays precisely because its seat CAN be the source. No pricing
  gate repairs it: a gate tight enough to be safe admits nothing. ⚠ **but the refusal is a property
  of RETROFITTING, not of cs seats** — under vreg emission the store's source is a vreg the
  allocator assigns, so it can BE the cs register and no mov exists to drop. Reachable by rung 5
  proper, by no patch to `repack`.
* **deleting `pcs`** (2026-08-12) — ablates to +1,004 B of .text and ZERO corpus instructions
  (39.466 G both ways against a ±5 M instrument floor). Regresses size, pays nowhere. Holds until
  promotion covers those bytes — and promotion structurally cannot (see the ledger).
* **iv-b, call-crossing optimism at `vmcflush`** (2026-08-12) — built whole, +545 insns, 56
  functions worse and 2 better. The census is the verdict: 1,088 crossings, **630 die unread**, 454
  want a reload, 4 found a seat. Optimism trades loads for pressure and pressure wins. ⚠ the flush
  cannot tell the 454 from the 630 — the read count is in the AST, the crossing is found in the IR
  — so the signal that would price the decision is exactly the one the site lacks. **The vmap must
  retire, not be extended.**
* **the splice seam, four peephole-level fixes** (2026-08-13) — all built, all measured, all
  missed, and each one is cheap to re-imagine, so: **the seam is not reachable from a peephole.**
  Its root is one pointer under two register names (see *the seam's root*), and every pass named
  here compares bases by name.
  1. **`restrict` → interior-store elision.** The promise is real and now survives the parser
     (ladder step 2 phase A, landed), but aliasing is not what the seam pays. Not refuted as a
     lever — refuted as *this* lever. ⚠ **BUILT ANYWAY on 2026-08-13 as ladder step 2 phase B,
     and the emission says the same thing from the client's side.** `stst` — a store the same
     cell overwrites with nothing touching memory between — closes the ten-line probe exactly
     as the ladder's gate named (13 insns → 11 after S-1 → **7**, three dead stores gone) and
     finds **ZERO work anywhere else**: love.c on four targets, the 133-file `test/cc` corpus,
     `body.c`, `splice.c`, `host/main.c`, `host/posix.c` — every `.text` byte-identical.
     **`composed`'s `Sp[0]` stores are not dead; every one is READ by the next op** through the
     other name (`mov %rax,(%rcx)` / `mov (%r11),%rax`, adjacent). No dead-store pass reaches
     that at any tier, and the roster is not even the consumer: with nothing between, the tier
     that closes the probe needs no aliasing story — **S-1's own lesson a second time**.
     ⚠ and what a rung here MUST carry first: **`volatile` does not survive `pquals` either**,
     so the compiler has no notion of it (only `asm volatile` parses) — every store-touching
     sweep today is safe only because `deadst` is r4-only and `stld`/`stldp` are adjacency-only.
     A pass over a non-frame store without that word is a silent MMIO miscompile in
     `free/blk.c` and the port mains, which `KCC ?= mooncc` compiles. The answer is twelve
     lines and it is TU-WIDE, not per-function: the inliner splices `static inline` device
     accessors into unmarked callers, and with no LTO the TU is the real edge.
  2. **the store-address park past a spliced call** — `callish?` answers on the **pre-splice
     AST**, so a call node that inlines away still refuses the park. Making the park optimistic
     (take it, then read the emission and hand it back if a call survived) is *sound* and gated
     green — 134-program battery, `test_clay`, `test_fixpoint` — and on love.c moves 8 `lvm_*`
     functions, 3 shrinking by 7 and **5 growing by 2**. Net −11 insns. Fails *pays somewhere,
     regresses nowhere*; kept at `scratchpad/park-v2.diff`, not landed. ⚠ It does **not** help
     `composed`: it only swaps which side of the seam uses the copy. Three sub-traps found
     inside it, each worth a rebuild if forgotten: a park must ride **`rpin`** (a splice body's
     `psreset` returns an unheld register to the pool and its own `ralloc` takes it); the
     decision must be made **before** `ralloc`, because a park handed back has already advanced
     the `%vN` mint; and **the mint is never rolled back** — a *nested* store that declines
     would rename registers its own rhs already emitted under the old names (`badreg %v1`).
  3. **`(mov A B)` + `(ld A A o)` → `(ld A B o)` before `stld`** — byte-identical output.
     `addrfold` already performs exactly this fusion later in the chain.
  4. **reconciling the ride with `stldkeep`** — `stldw`'s keep arm displaces an existing slot
     binding instead of joining it, which is a real defect and worth fixing on its own merits;
     it is **not** this one. `composed` is **frameless** — no prologue, no slot — so the pass
     is not involved at all. ⚠ general lesson: check whether the function even *has* a frame
     before reasoning about its slots.
* **ablating the loop keeps** (2026-08-13, phase 1 step 3) — static said delete (−300/−275/−391 B,
  thumb2 byte-identical); dynamic said **+184,000 insns**. Text size is structurally blind to a
  mechanism whose whole output is loads removed from loop BODIES.
* **subsuming the keeps' optimism by a coverage rule** (2026-08-13, phase 2) — the census aiming it
  was an instrument artifact; corrected, only 12 of 34 misses are the coverage case and 9 of the 14
  head flushes that kill a live keep are inner loops `loscan` correctly REFUSED.
* **`fcb`'s pre-lane rewind** — ⚠ **RE-OPENED and then LANDED 2026-08-13 as ladder step 3.** It had
  been refused the same week on +8/+12/+8 bytes x64/arm64/riscv64 at dynamic and compile-time
  neutral — a PRICING answer to a VOCABULARY question. The flush was a rollback wearing a
  clobber's clothes, and the bytes come from the preserved pin holding a register out of a
  four-wide pool, which is the pricing ladder's business and not the verb's. Every number
  reproduced on the re-build (misses 81 → 69; +8/+12/+8, thumb2 +0 — it has no pool to preserve
  into). ⚠ what stays refused is the register argument for DELETING the flush ("pins can never
  live in r0–r3") — true about registers, wrong about the flush, which exists for the DISCARDED
  emission.
* **the loop borrow on a64** — a net loss there (−187 alone, dragging both-on to −236). Verdict on
  insns only; the x64 win was WALL CLOCK at flat insns and there is no cross-target wall
  instrument. Owed before this is settled.
* **`rasg`: argument-register targets, and the bridge direction** — each bought with a regression.
  A mint moved onto an arrival drops rides; a death-copy moved onto r0 robs stld's forwarding.

## traps paid for

**Instruments.** ⚠ a "last X before Y" record is an attribution only if something CLEARS X when
the thing is re-established — otherwise it reports staleness with a straight face, and plausibly
enough to be written into a doc (the 2026-08-13 miss census, seeded per function while the regen
re-runs each function four times). ⚠ `make out/host/mooncc.image` relinks love through the CURRENT
gen.l, so a reproducer can crash in the instrument rather than the subject — hand-bake on a
saved-healthy binary. ⚠ the make lane compiles through `mooncc0.image`, love0-baked from the
TREE's gen.l, so a variant test leaning on `make` is testing the tree, not the variant. ⚠ an
image skews against a rebuilt `love` — symptoms are a segfault or `;; missing moon-main`, not a
diagnostic. ⚠ **diff the artifacts you already have before you instrument**: ablation binaries
answer "which functions and how much" for free.

⚠ **`say err` from inside gen.l prints NOTHING during a compile** (2026-08-13, an hour). A probe
written that way reports "this path is never taken" across 536 compiles while being silent itself
— the failure mode the doc already warns about, wearing a new face, and the probe text WAS in the
image (`strings` confirmed it). **Use `quit <code>` and read the exit status**: it cannot be
swallowed, `make` surfaces it as `Error <code>` on the very TU that hit it, and a sweep is one
`[ $? = 7 ]`. And validate any zero by **firing the probe on the COMPLEMENT** — if the negated
condition does not fire either, the instrument is dead, not the path.

**Measurement.** ⚠ a codegen rung owes a COMPILE-TIME A/B, not only a codegen one — the first ship
of copy propagation cost **78% of the compiler's speed** (13.2 → 23.4 s) and every gate stayed
green, because gates ask whether the output is right and none asks what it cost to produce.
⚠ `build` runs TWICE per fn (ir1 and the regen), so a fixpoint in the build tail is paid four
times. ⚠ measure cycles interleaved and in rounds, read the MINIMA beside the median: two builds
10 bytes apart read 0.3% apart on a corpus at IPC 2.7. ⚠ two agreeing samples are not a control
(clang's chacha read 198.1 twice by coincidence, then 169.9/182.7). ⚠ the mcobj cache is not paid
once per tree — a rung that changes the compiler changes every member hash.

**Positional and name-keyed reasoning.** ⚠ `unframe` reads the prologue BY POSITION (`sv3` asks
whether form 4 is exactly `(st r4 -8 r3)`), so a save spliced at index 4 displaces it and kills the
caller's rbx — presenting as a segfault in `main` AFTER the corpus printed `tests pass`. ⚠ a
name-keyed bisect over one TU's functions is not a bisect when the switch matches across every TU.
⚠ a law spelling a frame OFFSET keeps PASSING while counting the wrong thing — `(= 1 (ldsp wlkf
8))` counted a cs restore in place of the park it named. Anchor laws on SHAPE, and verify in both
worlds: a law that passes pre-rung and post-rung describes residency rather than layout. ⚠ a law
page owes a run per SIGNATURE change, not per behavior change (six one-arg `csdefs` asserts
under-applied to a truthy closure and test_moon was red unnoticed).

**Targets and coverage.** ⚠ **a guard that hides a target also hides the bugs that target would
have caught** — `coal`'s x64 bail hid that `lvout` silently UNDER-approximates on any target whose
exit is not `(ret)`/`(jmpr)`; v6m's exit is POP-PC, so the function had no exit the kit could see,
and an exit nothing can see is one where nothing is live. ⚠ a latent unsoundness only shows when
something raises the stakes (the fold rode unguarded for two rungs by luck). ⚠ a floor cannot be
read off an x64 shape — deriving it from `pro4?` handed arm and riscv a floor of 0. ⚠ enumerate
op censuses over the CORPUS, not one program: A-1's eleven ops were twelve, because love.c never
converts a double to an unsigned.

**FIXED 2026-08-13 — the rp2040 lane, and where the diagnosis kept being wrong.** `test_embed`
had been red for several commits: `cc: internal error: lea-far-hi r12`, one command to reproduce
(`mooncc -t thumb1 -c crew/moon/lib/math/am.c`). ⚠ **the good error message did most of the work**
— it named the constraint, and one extra operand in the scare made it decisive: `lea r12, r4,
896`, a far frame-relative address compute into a HIGH register, where v6-M's `t1-li` is
low-register-only.

⚠ **but three plausible origins were wrong, each refuted by a probe rather than by argument**: not
`cgexpr`'s two `rT` array-decay lea sites (instrumented — neither fires), not anything visible at
`cskeep` (instrumented — silent for the failing function), not present at `t16slots`'s entry
(instrumented — silent). It is not emitted by `gen.l` at all. **holo synthesizes it**, and the
answer was already in the file: `t1-mem-far` has a borrowed-low-scratch lane for exactly this
shape — `push s; li s,off; add s,base; …; pop s` — and its comment even names `ld r12,r4,slot` as
a classic customer. `t1-lea` never got that lane and gave up instead. Nine lines give it one,
reusing the same helpers; `sp` base stays barred (the push would shift it). ⚠ `t1-li` sets flags,
as it already did in the low-dest lane, so a lea between a cmp and its branch would breach on
both — inherited, not introduced, and now written down. rp2040 links; `test_thumb1` (qemu
Cortex-M0, am.c bit-exact) and `test_thumb2` (Cortex-M7) green.

**Open, not mine, and diagnosed (2026-08-13).** `test_embed`'s rp2040 lane has been red for
several commits: `cc: internal error: lea-far-hi r12`, reproducible in one command —
`mooncc -t thumb1 -c crew/moon/lib/math/am.c`. The operands are **`lea r12, r4, 896`**: a far
address compute off the frame base into a HIGH register, and v6-M's `t1-li` is low-register-only,
so `t1-lea`'s far lane scares. ⚠ **the fix is upstream, not in the encoder** — holo has no scratch
to invent, and inventing one there is how a roster becomes a miscompile. `pkr` answers `r12` on
t32 (`gen.l:779`) and v6m already special-cases r12 away in two other lanes
(`(? (v6m? g) 'r0 'r12)`), so the shape of the answer is a v6m register choice. Confirmed
independent of `stldp` by reproducing with that pass neutered.

**Process.** ⚠ the shell's cwd silently reset to the POST tree mid-session and a falsification
flip quietly read the pre-rung sources and answered plausibly — absolute paths for every gate in a
worktree session, and treat a flip that agrees too easily as a tree check first.

## the laws a new rung must hold

* **unframe is correctness, not optimization** — the glaze ABI rides Ip in rbp, so a
  framed fn that calls into the VM faults. Any op that can base on r4 joins a4base AND
  uoff (and deadst's scans); a raw op bars unframe at every splice site (emit named ops).
* **A new op names itself everywhere**: holo's x64-emitters row + ir-arity row, its ISA
  family in gen.l's tables, and its cs effect (cskeep refuses a dark op).
* **alsafe?** — an alias lives only where staging never writes: x64 all-but-r0-r3;
  a64/t32/rv only pool0 ∪ cspool. The ride license covers defs, not reads.
* **vmap hygiene** — pins are born only from pool regs; no pin is born during call-arg
  staging (the argseal) or inside a splice body; any lane that DISCARDS a cgexpr
  emission must vmflush (cgexpr is not pure under pins).
* **rgon-gate any mid-compile register consumer** — an ir1-visible bind flips
  lpick/payoff rankings unpriced (a fn can lose its tail jump); regen attempts only.
* **pears stops at the first body label** — a park's per-iteration reset wears the
  entry-home mov shape; renaming past a label erases it (the strpbrk miscompile;
  test_libc was the only catch).
* **cu0 stays in assignment order** (cs save offsets ride it); d128 stays out of
  homable?/vmap (the pair rides `'regw` seats only).
* a64 raw splices scratch r2/r3/r9/r15 ONLY — r10-r14 are homes.

## how to measure honestly

⚠ **and read the convergence plan's criterion first: on this leg a number is a FALSIFIER, not an
objective.** Every instrument below stays; what changed is what a good reading authorizes. A win
does not license a mechanism the program cannot explain, and a small loss does not veto one that
lets it state something it was guessing.

* Build twins from the repo root (the seat walk; a scratchpad mooncc can't link), same
  TU set, ccache off. Bench under LOVE_NO_IMAGE=1 on both sides, corpus as a FILE, or
  bake both — never mixed (a fresh worktree's cold image once read as −23% insns).
* After a gen.l edit: `make out/host/mooncc.image` FIRST (make test does not rebake it),
  then rebuild the moon objects; a bake that crashes leaves a poisoned image — restore
  love from a known-good copy and bake by hand.
* Interleave slot-alternating rounds; byte-cmp the binaries before believing a delta on
  a semantic no-op (⚠ a bake is never byte-stable and the embedded love-version hash
  differs across HEADs — compare offset sets or the .o). Real text is `objdump -h`
  .text, never size(1).
* ⚠ **a microbenchmark whose ops are constant on their input measures folding, not the
  op.** bench/vmsplice's first version read 60× at 0.023 ns/op — under a cycle, the body
  gone. Two tells, both cheap: a sub-cycle per-op cost, and a `.text` far smaller than
  the source implies (30 bytes for 64 ops). Reach every operand through a `volatile`
  seed and real heap data, and read the emitted size before the timing.
* Probe the verdict chain before theorizing: the sed-probe on a gen.l copy + the mooncc
  cat answers in minutes what disassembly matrices can't; the framed-set diff (objdump
  push-rbp per symbol, comm vs a twin) finds frame regressions; dump pre-fold IR via the
  in-process cc-parse/cgen-obj recipe. A probe's INPUT gets validated before its verdict
  is believed.
* ⚠ **A PROBE INSIDE A PASS READS THAT PASS'S INPUT, NOT THE OUTPUT.** The single most
  expensive mistake of 2026-08-13, and it looks exactly like a finding. An instrument
  placed in `stld` dumped `composed`'s seam as `(ld r0 r4 -40)` / `(st r4 -56 r0)` /
  `(ld r1 r4 -56)` — four frame ops per seam, an obvious residency bug, and a whole day
  of work aimed at it. **`stldw` erases all four before emission.** The shipped binary
  never had them, which the disassembly says in one command:
  `objdump -d x.o | awk '/<fn>:/{p=1} p{print} p&&/^$/{exit}'`.
  The rule: an intermediate representation is evidence about the pass you are standing
  in and nothing further downstream. **A claim about what the program COSTS is a claim
  about the emitted bytes, so read the emitted bytes.** Same shape as the `say err`
  trap below — both are instruments answering a different question than the one asked.
* ⚠ **a gate that names a PROBE is not a gate on the client, and this ladder wrote one.** Step 2
  phase B's gate said *the ten-line seam probe loses its dead interior stores* — it does, and the
  client shares none of that shape: the probe's four statements have no control flow, so its seam
  is an adjacency (`stldp`) with a dead store behind it, while every op body in `composed` carries
  a branch, so its store and the next op's load are adjacent but its stores are LIVE. **Read the
  client's own emission before building to the reduction's shape** — one `objdump` of the function
  the rung is for, first, not after.
* And the instrument that finally worked was the plain one: **compile the same TU with
  both compilers and diff the disassembly per symbol.** Sizes first
  (`awk` the insn count per symbol, `join` the two lists, print the rows that differ) —
  it names the handful of functions a change actually touched, out of hundreds, and it
  is the only reading that cannot be wrong about what shipped. `-fno-inline` (landed
  2026-08-13) exists to make that diff possible at all: a spliced function has no symbol
  to compare.

## the rungs, dated (git log is the full story; these are the shas)

⚠ condensed 2026-08-13, 1,431 → ~780 lines. Landed work collapses to its sha and the number that
moved; what a future session would ACT on — the priced refusals and the traps — was pulled OUT of
chronology into the two sections above, because burial in this ledger is what let a refused
mechanism get built a second time.

2026-07-16 · vmap+addrfold pair 3110edcd (the pair is the lever; each alone loses) ·
branch fusion 1ea516b3 · leaf-frame elision 4ea1ba3c.
2026-07-17 · reservation-orphan sweep bc6da67d (the arc's biggest: cycles −8.5%) · park
fold 7153eaad · live-park conversion a84f5976 · call-riding elision 3c20777e · store-park
eaa142b0 · float branch fusion c6af55ce · indexed loads f83b9d7c · dest≠base widening
d3b5eddd · tbr lane 9b2e0621 · jr0+unhome 81f74713 · rbp-free frames 2bb44f5d · encoding
shave 20105c14.
2026-07-18 · branch relaxation 97dcd369 (+ the wake-shim restructure 38e7f8ad).
2026-08-02 · fleet-slots e2d5989b (self −11.4%) · ride license b025327f · member-store
fold 38f51cff · cmp load-op fusion ae392a47 · negative disps e1a6bf99 · __int128 limbs
6ea9223d · always_inline honored ae6a611a.
2026-08-03 · non-leaf slotting 4e05fcc3 · cs pool e339995b (sort −41% cycles) · musttail
sweep 33360636 · slot-load elision c6dad445 · d128 pairs a5b5bb21 · leaf cs pool 26515d3b ·
canonical int homing 62c179f5 · d128 cs pairs 74707fce · shuttle skip af72212d.
2026-08-04 · JOIN meets + splice binds 4266830e 5aa36096 (+ the wrs? fix 3226863a) ·
mov-husk fold 6793a12b · alu read-through 3d11770c · rename sandwich d836b230 · priced
cginl park 591593fc (payload: the pears miscompile fix) · body-decl binds 1317b30e ·
priced int-param homes 33af3413 · cskeep b1ac6f03 · stage sigs ba5af92c · seat table
6c6cbcb9 · g-pin record bdc7e886.
2026-08-10 · the DESTINATION DIE, four lanes (asn/decl, compare, arg-seat, bin value): a
destination pre-aims at its park before the value evaluates, so the bridge mov never exists.
dyn insns −0.72% / −1.50% / −0.07% / −0.04%; a call on either side bars every lane. ONE aim per
spine — the first bin build aimed at every level and drained the pool.
2026-08-10 · ARRAY SLOTS, rungs 1+2 (the vmap's ARRAY LEG, and the frame-direct element lvalue):
element pins under minted content-compared keys ("x[3]"), then a constant-indexed element read as
a STATIC slot (off + k·elsize) so three lanes stop computing addresses. chacha −19% wall, poly
−10%; rung 2 byte-identical on love.c (the pay is escaped arrays, callish-rhs stores, no-param
fns). ⚠ the ESCAPE gate is the whole soundness argument — any bare `x` is decay and excludes the
array, so no pointer analysis exists; the mem trio is licensed BY NAME.
2026-08-10 · LOOP-HEAD SURVIVAL (the vmap's first boundary) — optimism with verification: the
pinned map keeps at a loop head, every arriving edge checks map ⊇ keep, a miss bars and the regen
retries, so only a zero-miss build ships. chacha20 −24.5% dyn insns, −74% wall.
2026-08-10 · THE CS BORROW (the second boundary — values across CALLS): a callish loop migrates
scalar pins onto free callee-saved seats at entry (`lomig`), and `vmcflush` spares them. Gauge
−19.6% wall at FLAT insns — the win is the broken store→load chain, not count.
2026-08-10 · THE AIM HOLD (the callish bar lifts): the ana_d miscompile was the FREE-LIST/PIN
SPLIT — `pool` and `vpin` agree only at psreset boundaries and the callish aim opened a window
between them. Three-fold fix: the hint rides `rpin`; pin doors evict from `pool` outright;
`ralloc` SCARES on handing out a vpin/rpin member.
2026-08-10 · SPILL-AROUND (the keep past seat exhaustion): a surplus pin stays on its pool
register, rostered (`saro`), and every call reloads it — priced at reads ≥ calls (a nested loop's
calls ×8). −5.0% insns on the seat-exhausted shape; the one-read shape byte-identical.
2026-08-11 · ENTRY SEEDING (the keep learns to CREATE): a hot scalar arriving unpinned takes one
slot load laid beside the seat movs. −10.3% insns on the seat-covered call loop. ⚠ the trap was
the CAP'S CONTRACT — `lonone` "cannot miss", but a seed that ignored it could, so no bar ever
accumulated and the attempt loop spun forever.
2026-08-11 · SPLICE-CROSSING KEEPS (the end-label meet): `cginl`'s unconditional end flush was
over-conservative — the end label is a forward join whose edges are all known. −21.7% insns on
the callish-inlinee loop. Excavation beside it: the weak crt0 tail passed the RAW SP to an
arg-taking main in a libc-free link (fdbd7aba).
2026-08-11 · ELEMENT PINS ACROSS CALLS: an element's slot is a static frame offset recoverable
from the minted key (`aeoff`), so the element arm seats and rosters like a scalar. −9.5% insns
and −14.5% CYCLES.
2026-08-11 · STORE ELISION ACROSS CALLS (the exit meet, the third join): `ld` meets its arriving
edges, so post-loop reads are zero forms and deadst sweeps the write-through stores. −12.5%
insns / −16.6% cycles. TWO SOUNDNESS HOLES closed: the cond edge must carry kp-VERIFIED pairs
only; and `pex`, the &-taken prepass, never descended into an element in HEAD position — an init
list's first element is an expression there, so `&lam` as a compound literal's first initializer
escaped the scan and any kept register went STALE across a GC-ing call (test/cc/132-clitaddr.c).
2026-08-11 · PARAMS ON CS SEATS, pmin-gated: a priced callish fn's homable params take seats
instead of wrapped hregs. ⚠ THE DYNAMIC VERDICT REVERSED THE STATIC HEADLINE — −1,574 static
insns / −8 KB read as a win, and the corpus measured **+2.7% dynamic**: the save/reload is
PER-INVOCATION and the wraps were PER-CALL, so every early-out fast path pays the prologue and
earns nothing. `pmin` (every path through the fn calls) is what survived: 13 symbols, −1,020 B,
corpus exact.
2026-08-11 · SHRINK-WRAP (retired 2026-08-12, below): saves at the callish region's head rather
than the prologue. ⚠ every static model tried (dirty loads, wrap-benefit, region-1 read debit)
approved grants the corpus refuted — a cold-path wrap is dynamically already paid for, and only
a frequency signal (PGO) beats one.
2026-08-11 · SLOT REPACK + object-precise deadst: `nslot` registers every cell in `g 'slots`,
deadst's lea arm marks the OBJECT live, and repack packs objects whose live windows never meet.
.text −24.6 KB (−4.2%), median frame 72→40 B. ⚠ PLACEMENT was the payload lesson: the first build
ran repack inside the per-attempt pipeline and the rankers price param slots BY BUILD OFFSET, so
a packed ir1 misprices every grant. Post-choice, the winner packs once.
2026-08-11 · THE spush CELL JOINS THE SLOT MAP: deadcell converted a live spush cell into a frame
slot and never told `g 'slots`, so repack's scan barred whole on one unclaimed r4 touch — **215
of 478 fns holding 77.1% of love.c's frame traffic**. One foldl closes it: .text −22,381 B
(−6.2%) at FLAT insns — the win is ENCODING, disp8 replacing disp32.
2026-08-11 · THE DEAD-STATIC SWEEP a1e12f40 (bytes, lever 5): a mark from roots over the emitted
forms sweeps statics nothing reaches. 149 bodies, 32,768 B; love's unreachable 6.4%→0.6% against
gcc's 2.6% and clang's 1.7%. ⚠ the ROOTS are the whole safety argument — every exported fn, alias
target, section-named fn, and every nom the DATA lane names (kind-indexed tables reach the
collector by ADDRESS, never by call).
2026-08-11 · STEP 0 RE-RUN (measurement only, HEAD a055d279): both-emit codegen 1.56×→1.47×,
binary 1.702→1.618×. ⚠ THE READING THAT OUTLIVES IT: dynamic held at 1.63/1.625/1.629 across
three fills while static walked 1.63→1.47. **Every lever the tree had landed to that point was a
size lever, and the executed stream had not moved.** The allocator leg is the first rung owed a
corpus A/B as its headline.
2026-08-12 · RUNGS 1+2, THE LIVENESS KIT AND SLOT PROMOTION 4dd9bc41: `rdsp` was already the
per-form transfer function and the missing half was the GRAPH — per form, not per block, so there
are no blocks to build and the fixpoint is repack's own widen shape. On it, promotion: an object
whose every touch is a full-word ld/st at its own base takes a caller-saved register across its
widened window. .text −6,315 B; frame movs −11.3%. ⚠ static insns move by TWENTY-NINE — the trade
is a memory mov for a register mov at equal count, so this is the arc's first rung whose case is
DYNAMIC: corpus insns −0.73%, cycles −0.93%. ⚠ **a liveness universe must cover the SEATS, not
just the traffic** — the universe was built from what `rdsp` names while a seat is chosen from the
caller-saved file, so a register the fn never mentions was absent from a call's clobber set and
read as free ACROSS the call (ai_sleep promoted into rdx; it survived cskeep, the laws and every
gate but running). Two more, each now a comment: a cs save is `(st r4 slot r9)` and its restore
`(ld r9 r4 slot)` — the exact shape of a promotable temporary; and a sibcall is a jmp to no LOCAL
label, so read as a plain transfer it has no successor and liveness called the whole argument file
dead where it is most alive.
2026-08-12 · STEP 0 AGAINST RUNGS 1+2 (measurement only): codegen 1.47→1.44×, and THE ROW THAT
MATTERS — corpus insns 42.291→41.984 G with both natives flat, so the dynamic ratio moved
1.629→1.617×, the first movement in that row after three fills of size levers. poly1305 PASSED
gcc while chacha held at 3.5×, the pair's designed reading firing exactly as specified.
2026-08-12 · COALESCING (rung 4, pulled AHEAD of rung 3): a copy whose source was defined by the
form before it and dies at the copy is a def that named the wrong register. The liveness kit's
first consumer to ask "does it die HERE" rather than "does it die soon". +38 lines. **corpus insns
−1.76%**, .text −1.83%, reg-reg movs 12,353→10,391 — the largest single move on this arc. ⚠ the
plan's ordering inverted with it: rung 4 comes BEFORE rung 3, because nothing that turns memory
into copies pays until the copies can go.
2026-08-12 · COPY PROPAGATION (the forward half; `dehusk` retired): `dehusk`'s five hand-cut
windows onto one law are one pass — a forward walk carrying `reg -> source`, killed at each def
and control edge. The renamable slots are PROBED off `rdsp` rather than whitelisted, so the roster
cannot fall out of step with the table it models. corpus insns −0.68%, cycles −0.49%, .text
−0.67%. ⚠⚠ **THE TWO DIRECTIONS COMPOSE, and that is where most of the win is**: forward
propagation ALONE regresses the corpus (+0.19%) while shrinking .text, because renaming
`(add r1 r1 imm)`'s source breaks two-address fusion. Protecting the fusion is 914 B WORSE; the
fix is to let the break happen and hand the wreck to `coal`, whose backward fold rebuilds it on
the right register. ⚠ AND PLACEMENT IS A REAL CHOICE — four were built. The three post-choice
placements all read BETTER on paper (−0.94%/−1.38% .text) and all LOSE on the clock (+0.27%,
+0.48%, +1.4% cycles against −0.37% for the build tail). A mov the CPU rename-eliminates costs no
cycles, so deleting one LATE buys instruction count and nothing else, while deleting it EARLY
feeds addrfold/cmpfuse/deaddef a cleaner input — that is where the clock moves. ⚠ AND THE GUARDS
ARE PROVED, NOT ARGUED: the rename relation is FINITE (75 op shapes, ≤2 read positions, 15
registers), so law.l exhausts it — 390 forward renames and 570 backward folds through the real
carriers, encoded by the real holo-bytes, with a coverage assert reading the op ROSTERS so an op
joining one without a shape goes red by name. Falsified three ways. **Reach for exhaustion before
reaching for search** when the carrier is small.
2026-08-12 · SHRINK-WRAP RETIRED (rung 3, first half): ablation priced the arc's heaviest
mechanism at **48 bytes** and nothing on the corpus. Gone with it: `pminp`, `cgitemx`, `swre`, the
statement-level region split, four `g` slots, and the DUAL-EPILOGUE flavor of `sibs` — two
parameters threaded through six recursive calls and recomputed at every form of every function on
every target, for one function's benefit. **gen.l 8,125 → 8,012, the arc's first negative-LOC
milestone.** ⚠ the deletion is byte-identical to the guard-off ablation, which is the check that
says the mechanism came out whole. ⚠ **the two halves differ by 42× per line**: `swcs` 113 lines
for 48 B (0.42 B/line); `pcs` ~57 lines for 1,004 B (17.6 B/line). A rung named for a mechanism
CLASS hid that spread — **price the members, not the class.**
2026-08-12 · WHY PROMOTION CANNOT SUBSUME `pcs` (the probe behind that refusal). The class is 15
functions of ~640 and the benefit is TWO. It is structural and gen.l says so in its own comment:
promotion's seat roster is `lvgp`, the CALLER-saved file, and `lvtx` has a call define everything
outside `csregs`, so **promotion is caller-saved-only BY CONSTRUCTION** while `pcs` seats params
callee-saved for exactly the reason promotion cannot. ⚠ `pmin` is the tell — it gates the grant on
EVERY PATH CONTAINING A CALL, precisely the class promotion is structurally unable to serve. **A
mechanism whose entry condition is another's exclusion condition was never going to be subsumed by
it.** ⚠⚠ THE ARC DEPENDENCY IS INVERTED: `pcs` can only be retired by the rung that hands out
callee-saved seats — it is not rung 3's second half, it is rung 5's.
2026-08-12 · WHERE THE FRAME BUCKET IS STUCK — the promotion-rejection census (5,216 objects,
14,768 touches): noseat-call 44.5%, shape 25.1%, prom 16.4%, noseat-plain 13.6%, wide/esc 0.4%.
⚠⚠ **AND THAT BUCKET IS A TRAP — see the refusals section; it was rediscovered and the pass built
a SECOND time off this very table**, reproducing the original verdict to the sign. ⚠ **why the
census over-reads, stated so a third attempt does not happen**: it counts where the traffic IS and
cannot count what a lever BANKS. The other three buckets each name a different rung — `shape` is a
touch-shape question no register file reaches, `noseat-plain` is honest pressure (greedy losing to
a real scan), and wide/esc at 0.4% proves neither address-taking nor multi-word objects are worth
a rung.
2026-08-12 · RUNG 5.0 — THE LIVENESS KIT LEARNS ITS MACHINE: `csregs`, `lvgp` and `lvret` answer
per target the way `(argr g)` and `(cspool g)` already did. ⚠ each roster is holo's OWN TABLE read
back rather than the ABI document, because a wrong one is a miscompile and not a missed
optimization. Ships byte-identical on x64 — the gate a foundation rung wants is not "the tests
pass" but "the compiler did not change its mind". Then `coal`'s x64 bail goes: arm64 .text
−2.25%, riscv64 −2.02%, the first allocator pass to reach those targets.
2026-08-12 · RUNG 5.1a — THE VREG SHADOW: `ralloc` answers a minted `%vN` mapped to the physical
the pool picked, and `vrfix` substitutes at build's innermost tail. Byte-identical on all four
targets. ⚠ the finding that outlived the diff: **the old discipline compares PHYSICALS, and its
accidents are load-bearing** — an unheld hint returns to the pool at a splice psreset, re-mints,
and the value comes back "honoring" the hint by coincidence of register, and that coincidence
decides an rfree. `rpeq?` now carries every hint-honor and two-address alias test. ⚠ when
destinations BIND, the accidental-honor lane disappears — every place `rpeq?` sits is a place the
assignment must make deliberate.
2026-08-12 · RUNG 5.1b i–iii — THE INTERVAL VERDICT AND THE COALESCE ENGINE (doc/moon-vreg.md
carries the ladder). The checker resolved every live set over love.c + the corpus on four targets:
**zero conflicts, zero entry-live mints, zero mint-mint collisions — the shadow discipline is
interval-sound.** Three findings priced the rest: every full-liveness-droppable mov has a
STRAIGHT-LINE span, so the assignment needs no fixpoint in the build tail (a real one measured
+92% compile — the +78% ghost, dodged by measurement this time); coalescing SUBSUMES
wnt-threading, dissolving a feared ~200-site refactor; and the pinned 4,103 are the vmap class
whole, which only step iv frees. `rasg` landed the ADOPTION direction: −12 insns, .text −30/−12/−8
B. Two refusals bought with regressions are load-bearing — **no argument-register targets** and
**no bridge direction** (a death-copy moved onto r0 robs stld's forwarding; the bridge mov is the
HANDOFF, not dead weight). iv-a (the vmap holds mints) landed on the branch: byte-identical but
+6 insns from three adoptions refusing over honestly-longer spans.
2026-08-12 · THE CALL-CROSSING CENSUS (love.c, four targets). Demand: **~97% of loop-weighted
reads sit on names that cross at least one call**, on every target — not a niche class, THE class.
Supply: the callee-saved file was entirely idle, identically in every function (4 usable on x64,
10 arm64, 11 riscv64, 7 thumb2). ⚠ SUPERSEDED 2026-08-13 by the span census: the idle file is what
made arm64 first, but the spans then said the file is not the binding constraint there at all, so
what arm64 is first FOR is the admission rule, not the scan. ⚠ riscv64/thumb2 read low only
because `nhome` is 0 there, so their demand is understated by exactly the parameters.
2026-08-12 · IV RUNG A-0 — THE CS FILE BECOMES REAL ON ARM64. `cspool` was () for every arm and
riscv target, the a64 prologue never spliced `cssv`, and `cskeep` bailed on `arm? g` — so three
landed, priced mechanisms had never run where the file is 10 registers wide. ⚠ two latent bugs the
file's absence had hidden: `sibjmp` read the `epi-a64` CONSTANT instead of the passed-in `ejx`, so
a tail call jumped with the callee's seats dirty (18 functions, and `cskeep` caught every one the
moment it was allowed to look); and `rdsp` did not model `adds`/`subs`, so it answered 'bar and
every analysis silently declined those functions. love.c/arm64 −941 insns, −3,758 B. ⚠ **the
file's three consumers do NOT transfer their x64 pricing** — lpick's overflow is the prize
(≈ −755), and the loop borrow is a net LOSS on a64, so a64 does not take it. That is a verdict on
insns, not on the mechanism: the borrow's x64 win was measured in WALL CLOCK at flat insns, and
there is no cross-target wall instrument. **Owed: an arm/riscv wall instrument.**
2026-08-13 · IV RUNG A-1 — RISCV JOINS, AND THE CS OVERFLOW LEARNS ITS EXITS. riscv landed at −5
insns with `vbin_fill` alone giving back +232, which forced the pricing question A-0 had recorded:
**`lpick`'s cs overflow had no per-invocation term at all** — invisible while x64 offered four
seats. The fix is `pcs`'s own accounting worn by the locals: a seat costs one save plus one reload
per EXIT, so its touches must clear `1 + nx`. Four variants measured; per-item ships (−798 arm64 /
−179 riscv64). ⚠ the term is NOT target-gated and x64 moves too (−43) — the accounting was
missing, not arm-specific.
**2026-08-13 — iv phase 1 step 1: alive answers a per-name live SPAN, and the span census
refuses the packing argument.** `rec` recorded only call-bearing statements; `spn` now folds
every statement's live set into a `[lo hi]` tick extent (the control-only lanes record too, so
the extent is a superset of the true range — a `goto` reads the whole universe). Byte-identical
`.text`/`.data` on all four targets, compile time flat. What the table then said stopped the
scan: mean max-overlap is 4.3 on x64/arm64 and 1.7 on riscv/thumb2, so **96% of arm64 functions
could seat their whole universe in the ten-register file** — and letting disjoint spans share a
register buys only +4% more names seated (+7% on x64's four). The reason is that **the median
universe name is live across 95% of its function**, p75 the whole body: a param is live from
entry by definition and a C local is declared at the top of its scope, so at the statement
grain there are no short ranges to pack. ⚠ therefore the file is not the binding constraint on
arm64 — `lpick`'s ADMISSION rule is — and the census that ranked arm64 first ranked it on
supply the allocator does not need. Price the admission gates before building the scan.

**2026-08-13 — iv phase 1 step 2: the nested-loop licence retires.** The admission census (tag
every rejection point, count over love.c) said **69% of names reach neither candidate list** on
arm64/riscv — all barred from the pool lane by `crossing`, which is correct physics, so the cs
lane is the only door — and that `lpick`'s `ln < 1` nested-loop licence shuts it on 49% of them
(the `1 + nx9` term 31%, `tc <= 2` 13%, `lea` 4%). That licence was a PROXY for "the save/restore
pair amortizes", from before A-1 added the real per-invocation accounting; with the actual cost
charged it charges twice. Dropping it and ranking candidates by total slot touches (rather than
by `ln`, which degenerates to 0 for most candidates once the gate is gone): **arm64 −1,185,
riscv64 −1,109, x64 −1,509 insns — −2.0% of love.c's x64 .text** — and the dynamic gate, which is
the one that matters when the rule's whole provenance is static counts lying, reads **271.87M vs
272.08M instructions retired on the corpus, −206,000 against ±500 noise.** ⚠ 31 shape anchors in
law.l broke; across all 172 law snippets the change is −17 insns, sp-loads flat, sp-stores −2, so
they were re-anchorings — loops shorten because a counter gains a cs home (`h3` 18 → 14
insns/iteration, `nrg` 21 → 19, `lo8` 13 → 10). Two anchors were rewritten rather than renamed:
lo8's law asserted the post-loop call bars the homes (the cs lane does not care — the callee
preserves the seat), and nrg's `(= 1 (ldsp nrgf 56))` was PASSING while counting a cs restore
instead of the slot it was written for — the offset-anchor accident law.l:263 warns about.

**2026-08-13 — iv rung A-2: thumb2's file opens, and the op census was short by one.** Twelve
ops modelled in `rdsp` (the carry family joins `flagops` — adcs/sbcs/ors set the flags, adc/sbc
READ the carry a preceding adds/subs left, so none may be lifted; `(umull dl dh a b)` is the
first form defining TWO registers; `udivll`/`uremll` are the first NULLARY ones, the whole
64-step expansion being the form, owning the r0..r3 quad while r4..r7 are pushed and popped
inside it), plus a cspool row for t32 (r4..r10) and one for **v6m starting at r5** — it keeps
r4 as a bottom frame base, pushed after the sub. **−14,728 bytes on love.c/thumb2, 80 fns
better and 55 worse (worst +256, lvm_hush); x64, arm64 and riscv64 all BYTE-IDENTICAL**, so
modelling clz and the carry family — which the other targets do emit — changed nothing there.
lvm_aprod alone goes 4,634 → 4,201 insns, and the byte delta is exactly 4x the insn delta, so
every form removed was a 32-bit wide one.

⚠ **the eleven-op census was wrong, and the method is the lesson**: it was enumerated over
love.c, which never emits `cvttsd2ui`, so the twelfth op only surfaced when test/thumb2/libd.c
hit the live gate. Re-swept with cskeep printing instead of scaring over 133 test/cc files plus
the thumb corpus on all four targets — clean. A census over one program describes that program.

Also: **cskeep stopped naming targets.** Its gate was `arm? g && !(a64? g || rv? g)`, a list
that grew once per rung; it now asks `!(two? (cspool g))` — a target with no file has nothing
to breach — so a8 bails for the true reason and the next backend to get a file is covered
before anyone writes it.

**2026-08-13 — iv phase 1 step 3 REFUSED: the loop keeps are load-bearing, and static codegen
said they were not.** With the cs file real everywhere and lpick's admission widened, ablate the
keeps (`lonone`, the existing keep-nothing knob) and see whether the homes absorbed them. Static
said yes on every target: **x64 −300 insns, arm64 −275, riscv64 −391, thumb2 byte-identical**
(already inert there). **Dynamic said no by 600x: +184,000 instructions retired** (272,056,500
vs 271,872,500; baseline reproduced to ±400 either side of the experiment). ⚠ the physics is
that the keeps remove loads from LOOP BODIES — a load out of a thousand-iteration loop is one
byte of text and a thousand executed instructions, so text size is structurally blind to the
mechanism's whole output, not merely noisy about it. +184,000 is comparable to the −206,000
that the same day's admission fix bought. ⚠ **therefore the retirement cannot be a deletion**:
delete-and-measure looks free on every static instrument and is wrong every time. It must be
SUBSUMPTION — a loop-kept name taking a real interval seat from the assignment, so the keep has
no customer — which reorders phase 3 as written.

**2026-08-13 — the miss census CORRECTED, and the subsumption refused.** The 2026-08-12 reading
("all 34 misses trace to `vmcflush` degenerating to a whole-map flush") was an INSTRUMENT
ARTIFACT. ⚠ the killer tablet recorded the last flush that ever dropped a name, but it is seeded
per function while the regen re-runs each function up to four times — so the site it named was
routinely a flush from an earlier attempt, before the pin that actually missed was even
established. Clear-on-pin (every pin door plus `vmset` wipes the name's entry) is what makes the
record mean what it says, and under it `vmcflush` accounts for **zero** misses and `case` for
zero. The real distribution is four causes: a `for` head 13, a selective non-flush drop 12,
`fcb` 8, a `while`/`do` head 5. ⚠ the lesson generalizes past this arc: a "last X before Y"
instrument is only an attribution if something clears X when the thing it describes is
re-established — otherwise it reports staleness with a straight face, and the story it tells is
plausible enough to write into a doc.

The correction kills the rung it was aiming. Only 14 head-flush events kill a live enclosing
keep, and 9 are inner loops whose licence `loscan` REFUSED — flushing there is correct. So
"coverage, not optimism" covers 12 of 34, not 34 of 34, and `lochk`/`lomiss`/`lobar` stay.
The `fcb` narrowing that came out of the census was sound (snapshot the map before the discarded
int-flavored emission, restore it rather than flush to nothing — misses 81 → 69) and still lost:
+8/+12/+8 bytes on x64/arm64/riscv64, dynamically and compile-time neutral. Capacity, not
correctness — a preserved pin holds a register out of the pool for the rest of the loop.

**2026-08-13 — rung S-1: `stldp`, and the splice JIT's seam.** `stld` folds a store followed by a
load of the same slot and its own comment already carried the law — *"only the pair with nothing
between is aliasing-proof"* — while both lanes gated the base on `r4`. ⚠ **the adjacent pair needs
no aliasing story and was asking for one anyway.** `stldp` runs it over any base but `r4` (r4 stays
with the existing lanes, so the slot map, `stldkeep` and the epilogue anchors cannot move). Twelve
lines. love.c `.text` −20 B, corpus **−1,509 dynamic insns** (five runs, range disjoint from
baseline), arm64/riscv64/thumb2 unmoved, test_fixpoint byte-identical, test_moon green including
its own `stld` law. The client is the splice JIT: a composed body writes `Sp[0]` at every op
boundary and the next op reads it, and that chain was why the composed body ran at 0.83× — SLOWER
than dispatch — where cc gets 1.91×. Reproduced in a ten-line probe; the interior reloads are now
gone and the dependent path is register-to-register. ⚠ what `stldp` cannot reach is the dead
interior STORES, and those need the alias promise love.h already makes (`ai_word *restrict Sp`)
and `parse.l` discards at the token level. That is the shared ladder's one frontend step.

**2026-08-13 — THE LADDER step 1: the vmap entry carries its CLASS.** An entry is `(nm reg class)`
now, the class read off the grants in force (`vpcls`: `csbor` → 'bor, else `pool0` and the roster
→ 'ros or 'pool, else `()` — **no residency, so the class doubles as the eligibility**). `vmset`
is the one stamp point, which is what makes a scope-end sayable: a map outlives the grant it was
pinned under, so the exit meet rides a loop's rostered pins out and they arrive unrostered instead
of claiming a reload that no longer exists. `vmcflush` then says *pool residency ends here* and
the `(!(two? bs) && !(two? sr))` degenerate branch is gone — 223 of 514 pin kills stop being a
special case. **Byte-identical love.o on x64/arm64/riscv64/thumb2, `test_fixpoint`, `test_moon`,
`test_ccarm64`, `test_ccriscv`, `test_slow`.** The naming tell moved the right way: five pin doors
(`vmpin`/`vmbpin`/`vmrepin`/`vapin`/`vaepin`) became one body under three gates, because "where
does this pin live" was the question the three-way dispatch was asking without a word for it.
⚠ what did NOT come out is the census's second row — see the falsification under the flush census.

**2026-08-13 — and the element doors squish to one.** `vapin` (pool-only) and `vaepin` (seat-aware)
differed in one gate, so "may an element take a cs seat?" had two answers depending on which door
a site happened to call. The pool gate looked load-bearing — one-reg-one-name means an element
taking a seat EVICTS whatever scalar holds it, and a seated scalar's loop keep would then miss,
bar and regen. It is not: **the seat-class element pin is unreached by the tree's own C and by
539 corpus compiles across four targets, and on a synthetic shape that does reach it (an element
read whose want is a loop-seated register) the object is byte-identical** — the register an
element takes is never one another name is claiming, so the gate defended an eviction that does
not occur. Probed with `quit`, and the zero validated by firing the complement. One door.
⚠ gen.l now opens `(use 'pat)`: the entry's shape is stated in three pattern-headed accessors
(`vnm`/`vrg`/`vcls`) and every reader destructures, so a wrong-arity entry answers `()` rather
than a silently shifted field. love0's build-tool boot does not splice `pat`, hence the file's own
`use` — the module is already in `libs0`, so no frontend changed.

**2026-08-13 — THE LADDER step 3: `fcb` rolls back.** The float-compare lane discards its
int-flavored emission and re-evaluates both sides, and it opened with a full `vmflush` for a
reason its own comment stated: a pin born in the dropped forms would survive with its establishing
load gone. ⚠ **that invariant is narrower than the flush that served it** — a pin from BEFORE the
lane keeps both its load and its truth. So the lane snapshots at entry (`m9`, before any `cgexpr`)
and `fcb` `vmset`s it back: births die, predecessors live, and the re-evaluation's own drops and
pins land on the rolled-back map exactly as they would have on a fresh entry. **Misses 81 → 69**
on love.c/x64, reproduced to the figure. Priced: `.text` +8/+12/+8 x64/arm64/riscv64 and **+0 on
thumb2, which has no pool to preserve into** — the tell that the bytes ARE the preserved pin
holding a register out of a four-wide pool, and that is step 5's to price, not this verb's.
Dynamic neutral: three interleaved corpus-minus-boot rounds put every delta (−0.06M, +2.2M,
+1.1M insns on 33.3G) inside the baseline's own 3.9M spread.

**2026-08-13 — THE LADDER step 5, phase A: what a register COSTS is one table.** Seven gates each
carried their own inline cost arithmetic — `2·nc·nh`, `1+nx`, `nc`, a bare `7`, and a structural
veto — for the same handful of physical facts. `rcost` states them once, per class: a roster
reloads at every call in scope (`n`), a seat pays one save and a reload per exit (`1+n`), a
caller-saved home pays the wrap PAIR per call (`2n`), a splice bind's park costs a measured 7, and
**a pool home across a call is `'never` — not expensive, unavailable**, which is where the census's
776-refusal `free?` bar went. `rprice` answers the margin; `rpays?` and `rclears?` test it.
**Byte-identical on x64/arm64/riscv64/thumb2 — no verdict moved.** `test_slow`, `test_fixpoint`,
both cross differentials.

⚠ **the extents deliberately stayed put**, one per gate: `ntouch` reads ir1 (machine traffic),
`rdw` reads the AST (source use, loop-weighted), `nrac` counts only the reloads `stld` cannot
remat — that last one knows what a LATER pass will erase, and no generic counter carries it.
Unifying extents would be the program knowing less. What unified is the cost, which is one physics.

⚠ **and the shape gave up a claim it could not keep.** The plan was to key the table by class ×
FRAME (per invocation vs per iteration) so a mismatched pair could not be written — the error this
arc paid for twice. It cannot: both numbers arrive from the one caller, so a frame argument would
be decoration that checks nothing. Each call site names its clock in a comment instead; enforcing
it needs the extents to carry their own frame, which is a phase-B question.

⚠ **one inconsistency surfaced and is left standing, deliberately**: the two loop gates grant at a
TIE (`nc <= reads`), the four others demand a strict win. Phase A preserves both — hence two
testers where there should be one — and names it. Settling it moves verdicts, so it is phase B's.

**2026-08-13 — THE LADDER step 2, phase A: the promise survives the parser.** `pquals` drops the
qualifier run at the token level, so `love.h`'s `ai_word *restrict Sp` reached the codegen as a
plain pointer. It now rides out of band: fn name → its restrict param names, scraped where the run
is dropped and filed under the function. **243 of love.c's functions carry one; `lvm_add` records
`("g" "Sp")`, exactly what `love.h` promises.** Byte-identical ×4 — nothing reads it yet.

⚠ **the fact deliberately does NOT go in the type.** clay already has a `(restrict t)` node and a
law that it qualifies only pointers, so that looked like the obvious home — but clay's own header
says `(const t)` is a form *with no cparse counterpart*: **the compiler's type tree has never
carried a qualifier node at all.** Introducing one would put an unfamiliar shape in front of 56
`'ptr`/`ptr?` dispatch sites in gen.l and break clay's G1 round-trip. `weaks` is the precedent end
to end (scraped in parse.l, carried on `ps`, handed to `cgen`), and `pxtra` was already a bag of
eight tablets, so a ninth cost no signature change — gen's unpack is length-guarded, so callers
passing eight still work.

⚠ **keyed per FUNCTION, which is the one place the `weaks` precedent does not transfer**: weaks is
global, restrict is not. A bare name set would promise no-alias about a `p` that is restrict in one
function and plain in the next. ⚠ and only a qualifier run FOLLOWING a star counts — restrict
qualifies the pointer, not the pointee.

⚠ **left unresolved, named at the site, and phase B's to settle before it consumes**: block-scope
shadowing. Two blocks in one function may declare the same name, one restrict and one not, and the
roster would over-promise. Phase A cannot be wrong about it; a consumer can.

2026-08-13 · **`-fno-inline` fc429bf8** — the instrument, and the day's only unambiguous
payer. mooncc swallowed the whole `-f` family by the advisory rule; the flag is now real and bars
the splice table TU-wide (the door `__attribute__((noinline))` opens one name at a time), riding
the gen's option bag at `pxtra`'s tail. It **outranks `always_inline`**: an instrument a source
attribute could override leaves the reader with no way to say *read this function as written*.
`test_fixpoint` holds the default path byte-identical. Gated both halves in `test_moon` — that it
bites (7 functions emitted, 11 with it) and that the answer is unchanged.
⚠ **why it matters beyond the flag:** the day's four dead ends were all diagnosed off intermediate
IR, and what finally settled the question was diffing two finished binaries per symbol — which is
impossible for a spliced function, because it has no symbol. The reason the arc lacked that
reading for a year was a missing flag.

Reverted with verdicts worth keeping: lea fusion c618c3d9, fn alignment 4e8bb80c, E5
read-establishment 132a9599, store-side addrfold copy-prop, cmp-mem (the first build) — each a
physics lesson in the sections above. The priced refusals are pulled out under **refusals**, which
is where to look before proposing a mechanism; the chronology is not.
