# plan: a wasm backend for mooncc

Drop emcc — the last foreign tool in a product path. What emcc actually supplies
today is small and known: clang→wasm codegen, a libc (malloc, memcpy, clock,
exit-as-throw), and the JS glue (`Module`, ccall/cwrap, heap views). The build is
three TUs (`src/love.c`, `am.c`, `wasm/host.c`) with no FS, no asyncify, no
threads, a five-verb export API, and `-Dai_tco=0` — a lane that already exists
and is already gated. The 32-bit port ledger (`wasm/32bit-findings.md`) is paid.
The core already declines the JIT on `__wasm__`. So the *runtime* is ready; what
does not exist is the compiler half.

## why this is not a sixth `defbackend`

Every holo backend is a `{lower, patch}` pair — a byte emitter under a two-pass
address-assigning driver. Wasm breaks each assumption that driver rests on:

- **no addresses.** No fixups, no branch relaxation; a module of index spaces
  and LEB128 varints (nothing in the tree emits varints — the primitives are
  byte/le16/le32/le64). obj.l/link.l/elf.l (~2,100 lines) generalize to none of
  it.
- **no jumps.** The IR is flags-and-jumps and gen.l emits unrestricted labels
  (C `goto`, case labels at depth); wasm demands structured control flow. A
  relooper — or a dispatch-loop fallback — has no analogue anywhere in the tree.
- **no registers.** Wasm has typed locals; the accumulator protocol and the whole
  residency layer (the one build's seats -- upar/ulloc off alive -- the pool,
  and the cs grants) model a 16-register
  file with a callee-saved contract that stops meaning anything. And locals are
  not addressable, so the r4 frame decision inverts into an escape analysis
  (love.c takes addresses of locals via alloca 42 times). ⚠ the residency half of
  this bullet is the cheapest of the four and is now PRICED: rung -1 ran the layer
  out on x64 and the compiler still reproduces itself byte for byte. What stands is
  the rest — the frame, the convention, the pins.
- **mandatory validation, typed instructions.** holo's IR deliberately forgets
  width past the op name; a module must know i32 from i64 per value.

Scale anchor: riscv — a fifth register-machine ELF target riding arm64's lanes —
cost ~1,160 lines. Wasm shares neither property; budget a low multiple of that.

## the ladder

- **rung -1 — the empty pool, on hardware that debugs. ✅ RUN.** gen.l's shuttle
  (`tor0` + `spush2` + `tor0` + `spop2`) IS a stack machine spelled through memory,
  and `ralloc` answering `()` — dry, the caller keeps the shuttle — is a
  configuration every `cgbin` path already handles, because callish/shift/constant
  sides force it today. t32 goes further and ships with no operand pool at all
  (`tpool`, gen.l — AAPCS32 leaves nothing caller-saved to pool), gated by
  test_thumb1/2. Two ablations on x64, one binding each:
  - **A, the accumulator protocol** — `ralloc` dry always: `.text` 896,896 →
    921,472 (**+2.7%**), +4.2% corpus cycles, 80 of 866 laws fail.
  - **B, the pool and the locals homes** — `tpool ()`, and `pools` filters from it
    so the homes go with the pool: `.text` → 937,856 (**+4.6%**), +4.9% cycles,
    106 laws fail.

  ⚠ B is NOT the whole residency layer — `cspool` is a separate file and survives
  it. Ablating that too (`cspool ()`, and both together) costs +6.6% and **+12.5%**
  corpus cycles; the full split and its economics are doc/misc/moon-gauge.md. What
  matters here is that **wasm pays neither half**: a wasm local is unbounded and
  engine-allocated (so the pool and the homes have nothing to buy) and it survives
  a call by construction (so the cs seats have nothing to buy). The 12.5% is the
  price of the ablation on x64, not the price of the wasm lane.

  Every configuration holds `test_cts` at 207/220 with the same 12 clean refusals
  and the same 1 wrong answer, and every one holds the fixpoint: the ablated mooncc
  compiles every TU, links love1, and love1 bakes its own image and rebuilds itself
  byte-identically.
  ⚠ every failing law is a register identity or a residency count
  (`(member? '(add r9 r1 r14) epwf)`, `(= 0 (stc lp1f))`, wraps/splds/movs/ldc9);
  not one is a value, a type, or a control shape. The laws pin the LAYER, not the
  meaning — a lane change churns them, and that is not breakage.

  So the residency layer is removable — the compiler still reproduces itself exactly
  without it — and the wasm lane's register story is the empty arm of a binding that
  already has one. Untouched and still rung 4's: the pinned lanes, the SysV
  convention wasm replaces outright, and the r4 frame's address-taken escape
  analysis.
- **rung 0 — the module writer.** LEB128, the section vocabulary, the type table,
  a whole-program emitter (mooncc already compiles love in one drive — skip the
  `.o`/linker story entirely, no `linking` custom sections). Proven on hand-built
  IR against a validator before gen.l is touched.
- **rung 1 — control flow by dispatch loop.** Lower every function as one
  `loop` + `br_table` over a label variable: trivially correct for arbitrary
  labels, no reducibility analysis, and it makes rung 2 testable. A real relooper
  is a later optimisation rung, not a prerequisite.
- **rung 2 — the type law.** Chosen (revisable): target memory64 and keep holo's
  own law — the ALU stays 64-bit (every value an i64), widths bite only at
  memory. This makes typing a non-problem, keeps the fixnum width, and skips the
  thumb-shaped 32-bit gap column whole. Cost: memory64 support in engines is
  recent; the existing wasm32 lane's findings stop applying. If memory64
  disappoints, the fallback is wasm32 + i64 ALU with address wrapping.
- **rung 3 — the environment.** A hand-written JS shim replacing emcc's glue:
  the Module factory, the five verbs, string marshalling, memory views — ~100
  lines against a fixed import set (clock, exit). malloc comes from nolibc over
  `memory.grow`; no WASI needed, the browser frontend already lives without FS,
  env, subprocess, signals. `index.html` keeps its API.
- **rung 4 — gen.l's wasm lane.** The tgt predicate, routing around the residency
  layer (a neutered configuration, not a rewrite — the five real targets must not
  feel it), the shadow stack for address-taken locals, `callr` via
  `call_indirect`. `stage.l` grows the dice the lane needs.
- **rung 5 — the gate.** `test_ccwasm` beside ccarm64/ccriscv, the law corpus and
  `wasm/test.mjs`'s bao ride through our module. Verification instrument: a
  foreign validator/engine at gate time only (node already sits there and already
  skips when absent) — same standing as qemu-user in dist_cross. The product
  path drops emcc; the gate may still borrow eyes.
- **rung 6 — a splicer in the browser.** Off the AOT path, after the artifact
  ships. Wasm forbids the native JIT by construction (`src/love.c` declines on
  `__wasm__`: a jump to a data address traps), so the browser love has no tier at
  all. A template splicer is the shape that works with no writable-executable page,
  because it builds a MODULE instead of patching code: read a thread back, take each
  row's own IR, strip the dispatch, lay the bodies end to end. ⚠ the tree had exactly
  this for native (`lib/splice.l`, over a `mooncc -fir` record) and it was **cut** —
  `doc/misc/moon-gauge.md`'s census is why: 95% of closures carry a call, which no amount
  of branch or operand work reaches. Whoever revives the idea here inherits that
  ceiling and must say what changes it. Three substitutions the native one wanted:
  holo becomes rung 0's module writer, `nif` becomes a table index reached by
  `call_indirect` (rung 4 builds it for `callr` anyway), and the symtab + load-bias +
  movabs apparatus deletes whole, since a module's external references are imports
  resolved by name — which also drops the in-process-only constraint. Open: the
  per-splice Module+Instance cost, and synchronous instantiation being size-capped on
  the main thread. Prior art for the skeleton: Mono's jiterpreter.

## choices (revisable)

- whole-program module, no wasm `.o`/linker — one consumer (the love build)
  doesn't pay for a relocatable story.
- dispatch loop before relooper — correctness first, shape later; the tree's own
  rule (ablate before you optimise).
- `-Dai_tco=0` stays; `return_call` is an optimisation rung once engines earn it.
- `wasm/love.js` (313 KB committed) gets rebuilt by our emitter behind the same
  `make wasm` door, and the emcc Makefile stays until the module passes the same
  gate — pays somewhere, regresses nowhere.

## difficulty

High. Three genuinely new pieces (container, control-flow reconstruction, the
lane through gen.l that bypasses the register story) and the verification
instrument the other backends leaned on — differential fuzzing against llvm-mc —
has no clean analogue. Bounded, though: the runtime side is done, the API is
five verbs, and every decision stays inside our own toolchain.
