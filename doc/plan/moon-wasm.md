# plan: a wasm backend for mooncc

Drop emcc — the last foreign tool in a product path. What emcc actually supplies
today is small and known: clang→wasm codegen, a libc (malloc, memcpy, clock,
exit-as-throw), and the JS glue (`Module`, ccall/cwrap, heap views). The build is
three TUs (`core/love.c`, `am.c`, `wasm/host.c`) with no FS, no asyncify, no
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
  residency layer (vmap, homes, cs pool, the recovery passes) model a 16-register
  file with a callee-saved contract that stops meaning anything. And locals are
  not addressable, so the r4 frame decision inverts into an escape analysis
  (love.c takes addresses of locals via alloca 42 times).
- **mandatory validation, typed instructions.** holo's IR deliberately forgets
  width past the op name; a module must know i32 from i64 per value.

Scale anchor: riscv — a fifth register-machine ELF target riding arm64's lanes —
cost ~1,160 lines. Wasm shares neither property; budget a low multiple of that.

## the ladder

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
