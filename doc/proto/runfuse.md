# run fusion — specializing on the SHAPE of a load run, not on an operand's value

An experiment on branch `nfuse`. The threaded VM already has operand-value-specialized
loads (`lvm_arg0..3`, `lvm_quo0..3/m1/m2` — one word, no operand fetch) and ap-fused
loads (`lvm_argap`, `lvm_quoteap`, `lvm_argtap`). Both still emit **one instruction per
load**. The question: fuse a whole RUN of consecutive loads into one op named for its
shape — `lvm_qa` (quote, arg), `lvm_qaa`, `lvm_qap` (quote, argap) — with the operands
riding after it in source order.

An `argap`/`quoteap` can only be the LAST element: an apply hands control away and
resumes at a fixed `Ip`, so a run has no dispatchable point in its middle.

## what the code actually looks like

`love.c` grew a temporary `AI_VMPROF` profiler: every load op notes the shape of the
maximal run it starts (`prof_end` skips mid-run entries, so it is one note per run
EXECUTION, not per instruction). Corpus = the dev gate's own workload
(`make test_host`, `LOVE_NO_IMAGE`, compile + run).

    run length   runs           share    load dispatches
    1            582.8M         67.1%    582.8M   (47.9% of all loads)
    2            237.9M         27.4%    475.9M   (39.1%)
    3             39.3M          4.5%    117.9M    (9.7%)
    4              4.8M          0.6%     19.3M
    5+             3.6M          0.4%     20.5M

    top shapes:  a 445.3M | qa 132.0M | A 91.0M | qA 57.3M | aa 28.2M | q 26.7M
                 Q 17.2M | qaa 15.6M | qqa 11.3M | aA 7.5M | qQ 6.7M | aq 4.2M
                 (a = arg, q = quote, A = argap, Q = quoteap, T = argtap)

**Two thirds of runs are a single load, and the mean run is 1.40 loads long.** That is
the ceiling on the whole idea, and it was worth measuring before writing any opcodes.
`qa` is `(- n 1)` and every other inlined dyadic with a constant; `qA` is an l2r call
on a global.

## what was built

⚠ READ THE LADDER AT THE BOTTOM FIRST. This section and the two measurement sections
after it are the experiment in the order it happened, and the order was wrong: run
fusion is the WEAKEST of the three axes it turned up. The branch ends at 13 opcodes --
8 run pairs, 4 load+consumer, 1 load+predicate+cond -- and the 16 length-3 run ops
described below were built, measured and deleted.

24 run ops to begin with, macro-generated (`frun2`/`frun2p`/`frun3`/`frun3p` in love.c;
the `frun3` pair is gone now). The name spells the run in source order; a trailing `p`
means the last load carries the apply. Operands ride in source order and the pushes run
left to right off a moving `Sp`, so an index written against its own load's frame comes
out right with no arithmetic.

`argtap` was left out of the fused set: `T` is 2.5M, 0.2%.

The emitter (`love/ev.l`, `fuse`) needs **no lookahead** — code builds backward, so a
load's successor is already in place at `tail`. Growing is the move the ap-fusion
already makes: overwrite the successor's OP cell with our operand and prepend the fused
op, which leaves the successor's own operands where they sat (backpatch sites included).
A successor that was operand-specialized carries no operand cell, so its implicit value
is written out first — the only case that spends the second reserved word.

`mem` is a one-entry cache of "op word -> how to grow it", a raw `spin` span (NOT a
tablet: this is written on every load emitted, and `pin` is the heavyweight polymorphic
nif the spa/spq note already warns about; the first cut used five `pin`s per instruction
and cost more than the dispatches it bought back). It needs no invalidation, because the
shape of an instruction is a function of its op word alone — so the `(id? h (peek 0 mem))`
guard is exact, and a stale entry either fails the compare or names the very op at `tail`.

## measurements

Three binaries with **byte-identical machine code** — A (no run fusion), B2 (pairs), B3
(up to 3) — differing only in the baked image. This matters: merely adding the 24 dead
ops to `love.c` and never emitting them moved every bench 1.5–4.5%, which is code-layout
noise **larger than the effect being measured**. Comparing A/B2/B3 cancels it.

corpus (`perf stat -r 5`, LOVE_NO_IMAGE — compile + run):

                insn        cycles      branches    branch-misses
    A          86.965G     25.473G      8.607G       88.45M
    B2         85.475G     24.683G      8.360G       80.20M
                -1.71%      -3.10%      -2.87%       -9.3%
    B3         85.160G     24.485G      8.320G       77.83M
                -2.07%      -3.88%      -3.33%       -12.0%

corpus wall clock, 9 interleaved samples, median: A 7747ms, B2 7539 (**-2.68%**),
B3 7504 (**-3.14%**).

egg boot (compile-dominated — nothing runs twice): B2 insn +2.81% cyc +2.21%,
B3 insn +2.61% cyc +1.85%. Wall +1.16% / +0.75%. The peephole costs more than it
saves when the emitted code is executed once.

steady-state benches (tak, closure, sum, bintrees, fib, primes — self-timed inner
loops, so compile and boot are excluded): **no change**. perf on the woken image
confirms it is real and not noise — `sum` -0.1% insn, `bintrees` -0.7%, `closure`
-0.05%, cycles flat. These are glaze / allocator / apply bound, not load-dispatch bound.

baked binary (image included): A 6,353,176 → B2 6,323,224 (-29K) → B3 6,309,848 (-43K).
Fused code is SMALLER: a pair of plain loads is 4 words, the fused op is 3.

## what it says

The mechanism is exactly what it should be — fewer branches, and disproportionately
fewer MISSES (-12%): merging N indirect dispatches into one removes N-1 hard-to-predict
indirect jumps. The branch delta (-247M at B2, -287M at B3) agrees with the arithmetic
the run-length table predicts: greedy right-to-left chunking of the measured
distribution removes 296M dispatches at cap 2 and 336M at cap 3, and the fused ops hand
a few branches back (the `p` variants keep the fixnum-operator test).

Coverage, re-measured with the hook on all 24 fused ops and the same instrument driving
the same command for all three:

    load dispatches   base 1216.5M   cap2 841.9M (-30.8%)   cap3 796.6M (-34.5%)

⚠ the FIRST cut of that number was wrong twice over and neither error was the branch's.
`Prof()` was missing from the fused ops, so a run STARTING with one went unnoted and the
ratio was undercounted on both sides ("-45%"). And `make` does not track flag changes,
so a `GCDBG=-D AI_VMPROF=1` build silently reuses a `love.o` with no profiler in it and
writes an EMPTY profile from a run that otherwise passes: the harness now touches
love.c and greps the binary for the instrument before trusting a single number out of
it. The perf counters above never depended on any of this.

### how the two peephole axes share the work

The three builds all carry the FULL existing peephole set — the baseline is unmodified
HEAD — and run fusion never displaces it by rule: `fuse`'s ap/tap clauses are tested
before the growth clause, and spa/spq is the fallthrough. But the OUTCOME is not
"untouched", and the profile is what says so. Executions, millions:

                arg   arg0-3 | quote   quo* | argap quoteap argtap | run2 run3 run2p run3p
    base      150.2   573.3  | 265.5   34.2 | 162.5    27.8    3.0 |    -    -     -     -
    cap2       57.6   360.1  | 129.6   12.6 |  57.3     9.7    2.9 | 145.2   -  66.9     -
    cap3       55.3   358.1  | 116.6    9.0 |  57.2     9.7    2.9 | 108.0 19.8  52.4   7.8

* the one-load AP-FUSION is not replaced, it is ABSORBED: `argap` drops 162.5M -> 57.3M
  because two thirds of its executions are now the tail of a fused run (`run2p`/`run3p`).
  `argtap` is flat at ~2.9M, which is just the fused set declining to cover it.
* the OPERAND-VALUE specialization is genuinely cannibalised, and this is the one place
  the two axes compete: `arg0-3` loses 213M executions (573.3 -> 360.1), `quo*` 63%.
  Every specialized load a run grows onto has its implicit operand written back out, so
  that pair costs a word it used not to. It still nets SMALLER overall (-29K/-43K of
  binary) because the plain-load pairs it also swallows each save a word.
* cap3 barely moves the single-load columns; it almost entirely converts run2 -> run3
  and run2p -> run3p. Which is the same story the wall clock tells about length three.

### so is the value axis still earning its keep? ABLATED, and yes

The cannibalisation above reads like a case for dropping spa/spq — 213M of its
executions gone, and it is a compare chain on every emit. So it was ablated: the same
four-cell 2x2, all four binaries again byte-identical in machine code.

                                   corpus cyc   corpus wall   binary
    A   spec  + no fusion             25.335G       7808ms   6,353,176
    C   NO spec + no fusion           25.502G       7852ms   6,409,936
    B3  spec  + fusion (cap 3)        24.408G       7609ms   6,309,848
    D   NO spec + fusion (cap 3)      24.850G       7690ms   6,335,272

Dropping it LOSES in every configuration: +0.66% cycles alone, **+1.81% on top of run
fusion**, and 25-57K of image. And it does not buy back the compile time it was supposed
to cost — egg boot is flat (4.469 -> 4.487G cycles without fusion, 4.559 -> 4.582G with),
with instructions UP, so the chain pays for itself even where nothing runs twice.

The surprise is the direction: specialization matters MORE after fusion, not less. The
two axes turn out to PARTITION the load population rather than compete for it. Fusion
takes the runs; specialization takes the singletons — and the singletons are the bigger
half (in cap3, `arg0-3` alone is 358M of 797M dispatches, and 87% of the surviving
single-arg loads are specialized). Cannibalising a third of its executions left
specialization sitting on exactly the population fusion can never reach, which is why
removing it hurts more once the fusable loads are gone.

Ablate before you optimise: the profile's "-37% arg0-3" was a true fact that pointed the
wrong way.

## the OTHER axis: load + the op that consumes it

Runs are only half the picture. Something EATS the loads, and the residue after run
fusion is 48% lone loads that no run length can reach. So the profiler was extended to
record what each run runs INTO -- the joint (shape, consumer) distribution, with the
consumer resolved to its SOURCE NAME off def1 rather than guessed from a fixed char set.

    consumers, all shapes:  cup 133.7M | ? 128.8M | cap 80.7M | two? 77.5M
                            = 47.1M | id? 38.9M | nom? 10.2M | charm? 9.9M | + 5.5M

**Arithmetic is 1.4%.** The `arg;add` I proposed as the motivating example above was
wrong about this workload: it is compiler and list code, so the consumers are accessors,
predicates and branches. The top four alone eat 64% of every load dispatch.

Four ops: `lvm_argcap`, `lvm_argcup`, `lvm_argtwo` (2 words -- exactly what a specialized
arg0..3 plus a 1-word op already cost, for one dispatch instead of two) and `lvm_argcond`,
which never touches the stack at all. The emitter side is one more clause in `fuse`,
using the same overwrite-the-op-cell move as everything else here. The tree already
fuses from the OTHER side at RUNTIME (cmp_lt peeks Ip[1] for lvm_cond); this is the
compile-time twin, and it reaches ops that have no such peek.

corpus, three binaries with byte-identical machine code:

                    insn      cycles   branches  bmiss    maxrss   corpus self-time
    N0 none        86.955G   25.371G    8.605G   87.65M   1444MB     6.485s
    N1 runs        85.140G   24.722G    8.309G   78.63M   1464MB     6.157s  (-5.1%)
    N2 runs+cons   84.556G   23.642G    8.251G   73.66M   1094MB     5.643s (-13.0%)

**Consumer fusion is worth more than run fusion, for 4 opcodes against 24**: -4.37%
cycles ON TOP of run fusion, where all 24 run ops bought -2.56%. Egg boot pays +3.0%
(vs +1.7% for runs alone) -- same shape as before, the peephole costs where nothing runs
twice. `argcap/argcup/argtwo` fire 321.9M times; `argcond` only 7.2M.

⚠ THE MECHANISM IS NOT FULLY ACCOUNTED FOR, and the numbers should be read with that in
mind. ~329M fused dispatches ought to remove ~329M branches; perf measures -58M. What
moves instead is MEMORY: peak RSS -24% (1444 -> 1094 MB) and minor faults -35%. The
plausible story is that a slightly smaller live set drops the two-space pool below a
growth step, and pool growth is stepwise, so a small change shows up as a big
discontinuity -- but that is inference, not measurement, and it is worth pinning down
before anyone leans on the -13% number.

`argcond` barely firing is the interesting negative: `?` is the second-biggest consumer
at 128.8M, but almost none of it is a BARE LOCAL as the test. `(? (two? b) ..)` compiles
to argtwo-then-cond, and the cond consumes a predicate RESULT, which is not a load. The
high-value cond fusion is therefore PREDICATE + cond, not load + cond -- the next rung.

## the rung above: load + predicate + cond, in ONE op

Splitting the fused ops in the profile sizes it exactly: of the 86.1M conds eating a
fused load+accessor, `argtwo` -> cond is **64.4M**, argcap -> cond 15.0M, argcup -> cond
6.7M. So ONE opcode takes 75% of it.

`lvm_argtwocond i else` tests `two?` of a local and branches. No push, no pop, no
`ai_nilp` call -- the entire `(? (two? b) ..)` is one dispatch where it was three. The
emit consumes the predicate's op cell AND the cond's, so it spends no new word.

It fires **64.4M** times, essentially every site: `W:?` in the profile falls from 64.4M
to 10.7K.

⚠ argcap/argcup twins were built, measured, and DELETED. At runtime an argcap is followed
by a cond 21.7M times -- but at EMIT time cap/cup sit before a cond in 8 sites out of
2497. Those runtime adjacencies come from branch joins and let bodies, where the cell
after the accessor was not the cond when karg looked. Two opcodes that fire ~never are
worse than none, and the runtime-vs-emit-time gap is the thing to remember: a hot dynamic
adjacency is NOT evidence that a compile-time peephole can catch it.

## length 3 was ABLATED and is GONE

With the other two axes in place, the sixteen length-3 run ops were measured out. First
in isolation -- cap 3 against cap 2, identical machine code, the ops merely unused:

    corpus   cap 3: insn 79.899G  cyc 22.174G  br 7.733G  bmiss 71.63M
             cap 2: insn 80.147G  cyc 22.331G  br 7.764G  bmiss 74.77M   (+0.71% cyc)

Then actually deleted, ops and emitter arms both. `size` says why that is free:

    with the 16:  text 609,993   data 5,655,087
    without:      text 603,289   data 5,662,143     (-6.7K of VM, +7.1K of image)

and the end-to-end cycle count lands at 22.141G against 22.174G WITH them. The VM text
they cost is worth about what the fusion they bought was worth, so removing them is a
wash on time and a wash on size -- while retiring 16 opcodes. Gone.

A longer run just chunks into pairs, which is what the run-length table said would be
fine: lengths 4+ are 1.0% of runs, and length 3 is 4.5%.

## the ladder, end to end

corpus, four binaries with byte-identical machine code. THIRTEEN new opcodes total:
8 run pairs, 4 load+consumer, 1 load+predicate+cond.

                       insn     cycles  branches  bmiss  corpus self-time   binary
    R0 nothing       86.961G   25.238G   8.606G  90.93M      6.766s        6,344,640
    R1 pairs (8)     85.571G   24.321G   8.370G  80.19M      6.574s        6,317,224
    R2 +consumer(4)  84.702G   23.051G   8.264G  74.55M      5.559s        6,316,536
    R3 +pred/cond(1) 80.105G   22.141G   7.762G  72.86M      5.275s        6,313,968

    cycles vs R0:      -3.63%   -8.66%  -12.27%
    egg boot:  4.450G -> 4.556G (+2.4%) -> 4.609G (+3.6%) -> 4.521G (+1.6%)

Read the opcode budget against that: 8 pair ops buy -3.6% cycles, 4 consumer ops buy
another -5.0, and ONE predicate/cond op buys another -3.6. **The value per opcode runs
the opposite way to the order this was built in** -- and the axis that started the whole
experiment is the weakest of the three. Ship it backwards if it ships in pieces.

R3 also gets its compile-time cost back (+1.6% at boot, against +3.6% for R2) -- the
compiler is itself full of `(? (two? b) ..)`, so it speeds up its own emitter.

⚠ the RSS column is REAL but not stable: the same P2 emitter measured 1094MB in one
build and 828MB here. Two-space pool growth is stepwise, so a small change in live set
shows up as a large discontinuity, and the number should not be quoted as a headline.
The instruction and cycle columns are the reliable ones.

⚠ the profiler's RUN count is not comparable across variants and should not be quoted:
the forward walk caps at 8 elements, so a long baseline run is split into several
counted runs where its fused form fits in one. The dispatch total is unaffected --
every element is counted exactly once either way -- and is the figure above.

But the payoff is bounded by that table, and it is small:

* **Length 2 is where the value is.** 8 opcodes, and they survive to the final build.
* **Length 3 was deleted.** 16 more opcodes for +0.71% cycles in isolation, and nothing
  at all once their VM text is counted -- see the ablation below.
* **Nothing above 3 is worth asking about**: lengths 4+ are 1.0% of runs.
* The idea is invisible on steady-state loops and slightly negative on compile-only
  work.

**The residue is the interesting part.** Of every load dispatch in the BASELINE, 48% is
a run of one — a lone `arg` or `quote` with no load neighbour, unreachable from this
axis at any run length.
Reaching it needs a different fusion: load + the CONSUMING OPERATOR (`arg;add`,
`arg;cond`), not load + load. That is where the next look should go.

Two smaller leaks, both worth a look and neither chased here:

* ~43M `qa` runs survive fusion even on a woken image, so they come from threads `ev.l`
  never emitted — `prel`'s hand-assembled `numfn` n-fold text (`[quote f][arg n][ap]*n`)
  is the prime suspect, and it could be laid as `[qa][f][n]` for free.
* `ava`'s closure references go out through `em2 lvm_quote` rather than `kim`, so they
  never reach the peephole at all (`qA` is still 9.9M on the image lane).

## status

`make test` green (host + love0 twice, vmret, waits). `make test_slow` hit two
failures, **both pre-existing on unmodified HEAD** and neither this branch's:

* `test_front` — `;; missing sent`. Reproduced by stashing the branch and rebuilding.
* `test_hue` — `vim/syntax.vim is stale`. The no-fusion baseline binary regenerates a
  syntax.vim byte-identical to the fused one, and both differ from the committed file:
  a book name moved and the generated file was not refreshed.

The generated `proof/rocq/gen.v` also comes out at 713 asserts against a committed 715 —
again identical from all three binaries, so `test/spec.l` moved without a regenerate.

The `AI_VMPROF` block in love.c is the instrument, not the product: `#ifdef`-guarded,
off in every build, and it should come out before any merge.
