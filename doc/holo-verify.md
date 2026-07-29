# holo encoder verification -- the ladder, and where it goes next

holo (crew/holo/, the x86-64 + aarch64 assembler) is the last untrusted stage of the
toolchain. we own everything above it -- the surface syntax, both compilers, the self-hosting
core -- and moon compiles love.c while holo links it, so the source-to-machine-code path no longer
routes through gcc/ld. that makes an *end-to-end verified* chain possible for the first time;
holo's encoder, being small and table-shaped, is the natural place to start closing it.

before this work holo's only check was the frozen goldens in holotest.l / astest.l: a few dozen
forms, each validated once by hand ("emit the bytes, `objdump -d -M intel`, confirm the
mnemonic"). the ladder automates and then *proves* that round-trip.

## the two rungs, and how they compose

**rung 1 -- fuzz** (crew/holo/fuzz/, `make test_holofuzz`). generate random neutral-IR forms,
encode via holo, disassemble the bytes, check the decode matches intent. it is a *decode* oracle,
not byte-exact: x86 encoding is non-unique (holo legally picks the 32-bit zero-extend mov, minimal
disp, redundant rex; arm64 lsl#0 == lsr#0), so registers compare by abstract identity, immediates
by value, memory structurally. x64 disassembles with objdump (cross-checked by llvm-mc); arm64
and riscv64 with llvm-mc (host objdump has no aarch64), where every insn is 4 bytes so the
instruction count is a free consumption check. the riscv lane fuzzes the FUSED shapes -- no flags
register there, so cmp+br / cmp+set / ucomisd+br pairs check whole sequences (inverted hop + jal,
slt/feq predicates). wide surface (25 x64 classes + 16 arm64 + 27 riscv), ~32.5k + ~27k + ~27k
samples, zero discrepancies. the disassemblers are *trusted* -- this rung grounds holo's decode
against the real ISA.

the SYSTEM lane rides the same rung with the oracle turned around (`sysdiff.py`, same gate). a
privileged instruction has exactly ONE encoding and a small enumerable operand space, so the
sharp check is the assemble side: write the intended text, hand it to llvm-mc, demand the same
bytes holo produced -- byte-exact, no non-uniqueness to tolerate. the arm64 op tables are read
out of `crew/holo/arm64.l` itself, so a row added to holo is checked with no edit to the
harness; x86 enumerates its register file instead. 910 encodings, zero discrepancies.

**rung 2 -- prove** (proof/rocq/enc*.v, `make test_encver`). a machine-checked *reference encoder*
in Rocq for a slice of the ISA, with `<slice>_roundtrip_ok` proving `decode (encode i) = Some i`
over the slice's domain by `vm_compute` -- the domain is finite, so the exhaustive check IS the
proof (the gen.v style), axiom-free (`Print Assumptions` stays "Closed under the global context").
the reference `encode` extracts to OCaml (the extract.v pattern), and a driver emits a love program
asserting holo's `holo-hex` is BYTE-IDENTICAL to the proven reference. byte-exact is valid here
because each slice picks a domain where holo's encoding is the canonical one. narrow surface, no
trusted disassembler -- this rung checks holo against a machine-checked oracle.

the rungs are complementary. rung 1 grounds the *decode model* against silicon (via objdump/llvm)
over a wide surface; rung 2 proves holo's bytes equal a reference whose encode/decode round-trip is
machine-checked, over a narrow surface. a slice covered by both is: correct against the real ISA
(rung 1) AND exactly the machine-checked-canonical encoding (rung 2).

what rung 2 proves, precisely: the encode/decode *pair* is internally consistent (invertible), and
holo matches the encoder. it does not, alone, prove the decode model matches silicon -- that is
what rung 1's objdump/llvm cross-check supplies. the honest claim is the composition.

## what has landed (2026-07-17, branch `post`)

| rung | files | domain | check | commit |
|------|-------|--------|-------|--------|
| 1 fuzz x64+arm64+riscv | crew/holo/fuzz/ | 25 + 16 + 27 classes | decode vs objdump/llvm-mc | `75761e5e` |
| 2a reg-direct | proof/rocq/enc.v | mov + reg-reg ALU, 16x16 x 7 | byte-exact, 1792 | `8ceb2d91` |
| 2b memory | proof/rocq/encmem.v | ld/st base+disp, ModRM+SIB | byte-exact, 6144 | `c9281f4d` |
| 2c immediate | proof/rocq/encli.v | `li` 3-way form choice | byte-exact, 320 | `969335e2` |

each 2x slice models the fiddly part of its corner: 2a the REX/ModRM bit-packing (which register
bit lands in REX.R vs REX.B); 2b the memory quirks (rsp/r12 forcing a SIB byte, rbp/r13 forcing a
displacement, disp sizing); 2c the shortest-form choice by value range (b8 imm32 / C7 imm32 /
movabs imm64). `test_encver` runs all three (~13s), guarded on coqc + ocamlopt.

## how to add a prove slice

the loop, ~an afternoon per slice:

1. **probe holo** for the exact bytes on the tricky cases (`echo '(...)' | ... | out/host/love`,
   then disassemble). the reference must match holo's *choices*, so read them off the binary --
   never guess. the quirk cases are the ones to nail (SIB escapes, forced disp, form boundaries).
2. **model in Rocq** (proof/rocq/enc<slice>.v): an `encode` matching holo, a small auditable
   `decode`, and `<slice>_roundtrips : bool` = `forallb` over the finite domain checking
   `decode (encode ...)` recovers the operands. `Theorem ... : ... = true. Proof. vm_compute.
   reflexivity. Qed.` then `Print Assumptions` to confirm axiom-free.
3. **extract** `encode` to OCaml (`Extraction "enc<slice>_ref.ml" encode`).
4. **differential driver** (enc<slice>_drive.ml): enumerate the domain, compute reference hex from
   the extracted encoder, emit a love program asserting holo's `holo-hex` is byte-identical; print
   `<name>-oracle: P / T PASS`.
5. **wire** into the `test_encver` recipe (another coqc + ocamlopt + drive + grep), and negative-
   test it (corrupt one expected byte, confirm the grep would fail).

the reg <-> hardware map (holo abstract regs are NOT the x86 hardware numbers -- r4=rbp=5,
r7=r8=8, sp=rsp=4, ...) lives in every driver's `regs` list; it was probed once in
crew/holo/fuzz/regmap.py.

### extraction caveats (both real, both bit us)

- **nat literals** extract as nested `succ` -- verbose generated .ml but runs fine (enc.v uses the
  extract.v nat->int mapping). fine for opcode constants.
- **Z -> OCaml int is 63-bit** (`ExtrOcamlZInt`), and its Euclidean mod/div OVERFLOWS at min_int
  (-2^62). so a differential driver over signed values must cap well inside 63 bits (encli caps at
  +-2^60). the *proof* is over unbounded Z and is unaffected; only the extracted differential is
  limited. state the cap in the driver.

## where it goes next

**remaining x64 encode slices** (each a prove rung on the pattern above):
- **indexed addressing** -- `ldx`/`leax`/`stx` with a SIB index register + scale (1/2/4/8). the
  last piece of the addressing-mode encoding; the SIB index field + REX.X. note the rung-1 finding
  that `sp` can't be a SIB index (encodes as no-index) -- the reference should exclude it, matching
  the fuzzer's `irand`.
- **sized loads/stores** -- the movsx/movsxd/movzx families (ld1/ld2/ld4/ldu*, st1/st2/st4) with
  their 0F-prefixed opcodes and operand-size prefixes.
- **shifts, setcc, unary, push/pop, the SSE double lane** -- smaller, mostly uniform; cheap slices.

**tightening the existing slices:**
- a general **disp-serialization lemma** (`decode_le n (encode_le n v) = v` for v in range), so the
  memory round-trip is proven for ALL offsets rather than the sampled boundary set. the selection
  logic is already exhaustively covered; this closes the disp-value dimension.
- likewise a signed-immediate serialization lemma to lift encli off its sampled imm set.

**arm64 prove rung** -- holo's other backend. fixed-width 32-bit instructions make the encoder
*cleaner* to model than x86 (no ModRM/SIB/variable length), but the movz/movk `li` chain and the
condition codes need care. the fuzz rung (rung 1) already covers arm64 wide; a prove rung would
give it the same machine-checked floor as x64. reuse the driver shape with an aarch64 reference.

**deeper, and off the encoder** -- the same ownership-buys-verification arc has two neighbours
already spec'd but unstarted: **moon translation-validation** (per-compile certificates that moon's
asm refines the C it compiled), and the **GC / scheduler refinement** proofs -- gc.v's
`barrier_sound` and doc/proto/sched.l's 11 checks are proven as *models*; what's missing is the
proof that the actual C refines them (doc/gengc.md). closing holo's encoder is the
first, smallest instance of that general move: prove the model, then bind the real artifact to it.

## running

```
make test_holofuzz     # rung 1: fuzz both backends (needs python3 + objdump / llvm-mc)
make test_encver       # rung 2: the three prove slices (needs coqc + ocamlopt)
python3 crew/holo/fuzz/fuzz.py --arch arm64 -n 500 --seed 7   # a bigger fuzz campaign by hand
python3 crew/holo/fuzz/sysdiff.py -v                          # the system lane, row by row
```

both gates skip gracefully when their toolchain is absent, and both live in `make test_all`.
