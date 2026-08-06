# moon-next — asm goto

An assessment of what `asm goto` would cost mooncc. Not built. It is close to kernel-only —
worth doing when something you actually want to compile demands it, and not before. (The two
items that used to sit above it here — the 32-bit pointer-width layout hole and `typeof` — have
landed; `__typeof__` works, plain `typeof` does not, which doc/moon-c-gaps.md carries.)

The refs below were accurate when written; re-check them at the point of edit rather than
trusting them.

## the allocator is not the problem

The obvious fear — that a terminator with multiple successors would break the tuned register
allocator — **does not apply.** `hasasm` already disables register homing for any function
containing asm, and the vmap flushes at every label, so nothing is expected to live in a
register across an asm statement. `alive` already returns the whole universe for both `goto`
and `asm`. Keep homing off under `hasasm` and the allocator needs no change at all.

## the real blockers

**a. The raw blob cannot name an outer label.** `cgasm` assembles the body immediately via
`holo-bytes` with an empty pre-bound label table, so any label not defined inside the template
hits `(scare 'undef-label ..)`. There is a clean hook, though: raw is lowered verbatim by every
backend (`x64.l`, `arm64.l`, `thumb2.l`, `thumb1.l`), and `chunk-len`/`resolve` already handle
inline `('fix w kind label aux)` items anywhere in the stream. A raw carrying an unresolved fix
would lay out and resolve against the **outer** function's label table for free. What is missing
is a holo entry point — a variant of `assemble-at` — that assembles while leaving a whitelist of
external labels as fix placeholders instead of scaring. `laylax` would need to treat such a fix
as its widest form.

**b. `cfoldir`'s pend merge is the one correctness hazard.** A label with no recorded pending
state that is linearly live inherits the fall-through state verbatim. An invisible in-edge — a
branch out of an opaque raw blob into a C label — makes that join unsound: constants assumed at
L would not hold on the asm edge. The minimum viable fix is to collect the asm-goto target
labels per function and add them to the `backs` table so they take the existing "assume nothing"
path. Cheap, and it mirrors back-edge handling exactly — which exists for precisely this reason.

**c. Surface.** `pasm` hardcodes three colons as `s1`/`s2`/`s3`; a fourth (GotoLabels) needs an
`s4` and a fifth field on the `('asm ..)` node, which ripples to every positional consumer in
`gen.l` and to the goldens in `law.l`. `asmsub` must learn `%lN` — currently `'bad` — and
substitute the *mangled* label `fn.NAME`, sharing the mangling with the label emitter.
`asm goto` is implicitly volatile and (pre-GCC-14) takes no outputs.

Everything else already refuses or resets on raw: `unframe` bails, `deadcell` dirties, `deaddef`
treats it as a barrier.

**Assessment:** about a week, touching parse, one gen pass, and one new holo entry point — and
not the allocator.

## what this does not buy

`asm goto` does not bring Linux into range on its own. The kernel additionally wants
`__label__`, computed goto, `_Generic`, and attribute semantics that change codegen.
