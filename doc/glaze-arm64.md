# the aarch64 glaze — the ABI and the register-role map

The glaze (`love/glaze/emit.l` + `auto.l`) emits **neutral holo IR** and is parameterized by
target: every codegen helper takes a `tgt` (`'x64` or the arm64 target), and `crew/holo/x64.l` /
`crew/holo/arm64.l` carry the encodings. The raw-byte x86 lane is retired — there is one lane,
label-resolved, and arm64 falls out of it.

On x86 the full glaze runs. On arm64 the **integer lanes** (leaf, n-ary, closure, counted loops,
the mutually-recursive arith group) and the **scalar float-result leaf** (`jitfr`) are live,
validated under qemu by `make test_arm64`. Only the **float-GRID lane** (`autonat`/`jitfgridn`,
packed SSE2) and `castbuild`'s cask-fill stay x86-only; on arm64 they fall to the interpreter,
and `autogroup` carries every source rewrite that lowers to the integer group, so fib/tak/primes
still glaze.

⚠ `make test_arm64` builds `tco=1` to match the native ABI. A trampoline build (`love-tco 0`)
stays the pure interpreter — glazed code Continues by tail-jump, which only the threaded VM
honors — though the module still installs for probes.

## How native code is reached (why the ABI is what it is)

The register convention is **not** register-pinning — it is the **platform C calling
convention**. A `nat`/native closure's body is entered as an ordinary indirect *tail call*
through a function pointer whose C type is
`lvm_t = struct ai *(*)(struct ai *g, union u *Ip, ai_word *Hp, ai_word *Sp)` (love.h). Dispatch
is `Continue() = Ip->ap(g, Ip, Hp, Sp)`; each op is a separate `ai_noinline noipa` function
ending in a tail `Continue()`, so ops chain by sibling-jump and **g/Ip/Hp/Sp live in the ABI's
first four argument registers across the whole chain**. Apply: `lvm_ap` sets `Ip` to the closure
value cell and `Continue()`s straight into the emitted bytes; install + cell layout
`[code,src,code,interp,lvm_ret,n]` in `lvm_nif`. The body may scratch other registers but **must
preserve g/Ip/Hp/Sp** for the next op and for the deopt `interp`.

Consequence — same mechanism, no arch-specific VM code (dispatch, install and the W^X mmap are
plain C under `#if __STDC_HOSTED__`, never `#if __aarch64__`):

| logical | x86-64 (SysV) | aarch64 (AAPCS64) |
|---|---|---|
| g  | rdi | x0 |
| Ip | rsi | x1 |
| Hp | rdx | x2 |
| Sp | rcx | x3 |
| result slot | `Sp[0]` (stored, then Continue) | `Sp[0]` (same) |

The result is **stored to `Sp[0]` and tail-threaded**, never returned in a register per-op (only
the final `lvm_ret` returns to C). So the accumulator need not be the ABI return register.

## The register-role map

`crew/holo/x64.l`'s abstract file is tuned to SysV: g/Ip/Hp/Sp = abstract **r6/r5/r2/r1**, acc =
r0. `crew/holo/arm64.l` is an **identity** file (rN→xN), so the *same* abstract numbers would
land g/Ip/Hp/Sp in x6/x5/x2/x1 — wrong. AAPCS needs x0/x1/x2/x3. So the two targets use
different role→register maps, and `greg` (emit.l) is the one place that knows which:

| role | x86-64 (abstract) | aarch64 (abstract) | note |
|---|---|---|---|
| g  | rdi (r6) | x0 (r0) | unused by the integer lane; kept intact |
| Ip | rsi (r5) | x1 (r1) | deopt / Continue |
| Hp | rdx (r2) | x2 (r2) | cons / room guard |
| Sp | rcx (r1) | x3 (r3) | arg slots, result store |
| acc | rax (r0) | x4 (r4) | the accumulator (on arm64 **not** r0) |
| tmp | r8 (r7) | x5 (r5) | popped operand |
| anchor | r9 (r8) | x9 (r9) | post-prologue SP snapshot |
| scratch | rbp (r4) | x10 (r10) | clobbered; ≠ operands |
| loop 0/1/2 | r9/r10/r11 (r8/r9/r10) | x6/x7/x8 (r6/r7/r8) | `loopregs`: caller-saved on both, so they survive the body's acc/tmp/scratch; arm64 picks x6–x8 to dodge scratch and anchor |
| args | rbx/r13/r14/r15 | x19–x22 | **callee-saved**; each group `H` saves/restores |

x16/x17 are IP0/IP1 scratch (the indexed stores borrow x16) and x18 is platform-reserved, so
neither is in the file. The callee-saved bank x19–x28 plus `fp`(x29)/`lr`(x30) is exposed.

## aarch64 codegen gotchas

- **`mov` to/from `sp` must be `lea`, not `mov`.** The ORR-based `mov` reads/writes XZR for
  register 31, never sp — `(mov sp r23)` silently assembles to `mov xzr,x23`. Use `(lea r23 sp 0)`
  to snapshot sp into the anchor and `(lea sp r23 0)` for the deopt frame-abandon (x86's
  `mov rsp,r12`).
- **Save `lr` (x30) around nested calls.** `call`=`BL` clobbers x30; a group `H` that calls a
  sibling must `(push lr)` in its prologue and `(pop lr)` before `(ret)` (x86 gets this free from
  CALL/RET's implicit stack return address). `lr`/`fp` are arm64-only names — `(push lr)` raises
  `badreg` on x64, which is correct: it is emitted only for arm64.
- **Overflow is split**: `adds`/`subs` + `(br vs …)` for add/sub; a fused `mulo` for multiply.
  There is no flag-setting multiply on aarch64, so `mulo` is
  `SMULH t,a,b ; MUL d,a,b ; CMP t, d ASR#63 ; B.NE` (overflow ⇔ high half ≠ sign-extension of
  low half) — hence the explicit scratch `t`.
- **Indexed addressing is synthesized** (no `[base+ix*scale+disp]` single insn): `ldx`/`leax`
  borrow the destination as the address temp; `stx` uses x16.
- **bignum `li`** extracts MOVZ/MOVK lanes with `//`/`%` (not `&`/`>>`, which drop a bignum's
  high limb) — needed for the map hash constant `0x9e3779b97f4a7c15`.
- **The I-cache is flushed in C.** aarch64's I-cache is not coherent with freshly written code,
  so love.c calls `__builtin___clear_cache(base, base+len)` after writing, on both the hosted
  (W^X) and freestanding (RWX) paths, before the first jump in. (Guarded off for wasm, whose
  clang has no such intrinsic and which declines the glaze anyway.)

## Working in emit.l

The shared scaffold: `greg` (the role→reg map above), `cgir` (leaf/n-ary expr→IR),
`coreir`/`jitcore` (prologue / body / putfix-epilogue+Continue / deopt + install), `deoptir`.
The lanes are `jitir` (arity-1 leaf), `jitnir` (arity ≥2, `Sp[i]` param fetch via `fxir`),
`jitgroupir` (mutual recursion — `cggir`/`gargsir`/`mkhir`/`mkouterir`, calls as
`(call <name>)`+`(label <name>)` with holo resolving the relative offsets), `loopcode` /
`loopcode-n` (counted loops), and `jitfr` (float-result leaf).

⚠ IR chunks concatenate FLAT via `asm` (emit.l's `foldl-(+)`; on lists of IR forms `+` appends)
— NOT nested `(cat (cat …))`, which bites paren-counting.

⚠ Each `(L 'op …)` builds a form; a quoted `'(op A …)` embeds the literal symbol `A`, not the
register value. Only quote all-literal forms (`'(br eq OVF)`); use `(L …)` when a role-reg is an
operand.

**How to add a lane** (the proven loop): prototype in scratch against
`cat emit.l proto.l | love` (x86 behavioral == interp); disassemble the arm64 output with
`llvm-mc --disassemble --triple=aarch64`; integrate into `emit.l` additively; add
`test/glaze-x86.l` coverage; `make test_glaze`; then the full `make test`. The glaze is
corpus-independent (unbound under `LOVE_NO_IMAGE`; `emit.l` is a host-only bake, not in
`love0`), so only `test_glaze` and `test_arm64` exercise it.

## Remaining

- **the float/grid lane on arm64** — `jitfgridn` and its packed-SSE2 register file need a NEON
  float file in `crew/holo/`; the integer IR does not model float registers.
- **`castbuild`** (the cask-fill string-accumulator rewrite) is x86-only for the same reason.
