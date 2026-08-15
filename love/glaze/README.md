# jit — the codegen backend (`ev` but faster)

> **What this is.** The aim is **`ev` but faster**: a closure compiled to native
> machine code that is *indistinguishable from the interpreted closure* — applied by
> juxtaposition (`(f x)`, no verb), `=`/`show`-identical to its source, and **deopting
> to the interpreter on overflow** so it is never wrong, only faster. The bricks:
>
> - **`nat`** (nif, `love.c`) — the install seam: emitted bytes → a TRANSPARENT
>   applicable native closure. Cell `[code, src, code, interp, lvm_ret, 0]`, value at
>   the 3rd word, so `value[-1]`=src (`fn_src`/printer/`salpha` → `=`/`show` see the
>   source) and `value[1]`=interp (the deopt fallback). W^X arena with a finalizer.
> - **`love/glaze/emit.l`** — a love-level **x86-64 emitter**: compiles `(\ x E)` arithmetic
>   and a counted-sum loop `(\ n Σ_{i<n} body)` to native, with a `jno`+inline-deopt
>   guard on every `+`/`-`/`*` and on `putfix` (its `add rax,rax` overflow flag is
>   exactly the 62-bit fixnum boundary). x86-64 only; load with `-l love/glaze/emit.l`.
>
> This realizes the law the earlier experiment found — *a glaze wins only when it owns
> the loop* — concretely: the counted-loop emitter owns the iteration end to end
> (~tens-of-× on in-range arithmetic loops; deopts out of range). It is the
> **generalization of the baked array kernels** (`asum`/`aprod`/…) to arbitrary
> scalar/control-flow loops that have no array to bake. **WIP** toward the *auto
> hook*: `ev` installing native for hot regions transparently, after which `nat` goes
> internal (mopped like `boxfix`/`wev`) and there is no user-facing verb at all.
>
> **The leaf substrate is gone.** `eat`/`toast` (a word→word trampoline over an opaque
> executable handle) and `love/glaze/probe.l` predated `nat` and were superseded by it;
> they were deleted once the production glaze had run on `nif`/`nifx` for good. The
> earlier scalar/array/fold kernels and the `opjit` hook are **gone** too; their one
> fixed-code win (reduction reassociation) lives **baked in the C builtins**. The
> kernel-RWX finding they established is recorded below. (`git log` for the arc.)

## The finding: on the kernel, emitted bytes just run

A retired probe (`love/glaze/probe.l`, deleted with `eat`/`toast`) built a buf holding
six AMD64 bytes —

```
B8 2A 00 00 00   mov eax, 42      ; imm32 little-endian
C3               ret
```

— copied them to the heap and jumped in. Run on the **kernel** target under qemu it
returned the immediate exactly (verified at 42 and at 12345). So Limine maps the HHDM —
which backs the kernel heap — **without the NX bit**: kernel data memory is already
executable. No page-table work, no `mprotect`; a love glaze on the kernel is just love
emitting bytes and calling them where they land.

The **host** is the opposite: Linux maps the malloc heap no-execute, so raw heap bytes
can't be run. That is what the W^X arena (`code_maplen` + `nat_unmap` in `love.c`) is
for, and `nif`/`nifx` route every hosted install through it. The corpus test
(`test/glaze.l`) stays architecture-neutral — x86_64 opcodes would crash an aarch64 or
wasm host — so it covers the install guards (non-byte / empty code → nothing) plus the
*decline* laws, which read as plain arithmetic and answer the same everywhere by design;
the executing tests are `test/glaze-x86.l`.

Those decline laws are the ones worth understanding before touching a lane. A lane is a
**recognizer + codegen pair**, and `cggir`'s dispatch ends in a silent `()`: an operand it
cannot emit compiles to *no code at all* and leaves whatever the accumulator last held. So
a recognizer that admits one shape too many does not crash — it answers a plausible number,
forever. `make test_glazefuzz` (`love/glaze/fuzz.l`) is the standing guard: 3000 random
closures, one shape per lane, run glazed and again under `LOVE_NO_GLAZE=1`, required to
agree byte for byte. Its leaf pools carry what the integer lanes *cannot* hold — strings,
noms, lists, gems — because the interesting behaviour is declining, not compiling.

Bringing up a **new** freestanding target re-opens the same question. Emit the probe
sequence for that target with holo rather than reaching for a hardcoded x86-64 buf.

## What the experiment found, and where it went

The full version generated x86_64/SSE in love and ran it through the leaf trampoline:
a scalar `(\ p <arith>)` kernel, an automatic `ev`/`opfix` hook to apply it
transparently, and array kernels (`amap`/`areduce`/…) over `z`/`r`/`c` arrays. The
transparency was made exact (`=`-preserving via `respec`, de-Bruijn `show` intact).
Then the benchmark settled it:

| 5M ops | glaze | interpreter | |
|---|---|---|---|
| `x*x+1` (scalar hook) | ~290 ms | ~230 ms | glaze **~25% slower** |
| `sum x*x` (array fold) | ~10 ms | ~450 ms | glaze **~45× faster** |

A native *function* called from the interpreted loop pays a call-boundary tax
(`putfix` marshal, the trampoline nif, the result decode) heavier than interpreting a
small arithmetic body — there's no case where the scalar hook wins. **The glaze only
pays off when it owns the loop.** And the loops worth owning are the reductions —
whose speedup, once reassociation was recognized as sound (`*`/`+` are commutative
monoids), is just a multi-accumulator C loop the compiler schedules. So that win was
**baked into the builtins** (`asum`/`aprod`/`amax`/`amin`, ~3× and portable), and the
glaze scaffold — kernels, folds, the `opjit` hook, ~1200 lines — was retracted, and the
leaf trampoline under it retired once `nif`/`nifx` carried the production path.

## Re-running a bringup probe on the kernel

The recipe below is how the finding above was reproduced; it needs a probe corpus that
jumps into emitted bytes and prints the result, ending in `(exit 0)` (which the kernel
routes to qemu's isa-debug-exit). Write one with holo for whichever target you are
bringing up.

```sh
make host                                  # builds love0 + the bake tools
cp <your-probe>.l out/lib/ktests.l         # make the probe the whole K_TEST corpus
out/host/love0 -l love/prel.l mk/tools/lcatv.l out/lib/ktests.l > out/lib/ktests.h
touch out/lib/ktests.l out/lib/ktests.h
make -s K_TEST=1 out/free/love-x86_64-test.iso
qemu-system-x86_64 -m 256M -M q35 -serial stdio -display none -no-reboot \
  -drive if=pflash,unit=0,format=raw,file=dl/edk2-ovmf/ovmf-code-x86_64.fd,readonly=on \
  -cdrom out/free/love-x86_64-test.iso \
  -device isa-debug-exit,iobase=0xf4,iosize=0x04
# then restore the real corpus:  make out/lib/ktests.h   (or rm it; the next build re-bakes)
```

## Caveats / TODO

- **AArch64 cache.** the install seam omits the I-cache flush AArch64 needs after the
  arena takes fresh code (`__builtin___clear_cache(base, base+len)` before the first
  entry). The flush belongs where the bytes are written, not where they are jumped to.
- **The contract at the boundary.** A value crossing into native code arrives as its
  *raw tagged* love word (`putfix A`) and the result is re-tagged (`putfix r`), so a
  codegen must untag at the boundary. Keep what crosses to unboxed machine words: no
  allocation, no heap pointers held inside — that is what keeps it GC-safe.
- **No verification.** This is the raw trampoline plus a safe loader — no semantics,
  no proof that the bytes mean the love they claim to. A *verified* glaze is a separate,
  much larger effort (and the place love's in-tree prover could eventually earn its
  keep).
