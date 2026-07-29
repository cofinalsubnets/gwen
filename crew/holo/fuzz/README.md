# holo encoder fuzz — the fuzz-first rung of the holo verification ladder

> The full ladder (this fuzz rung + the machine-checked prove rung in
> `proof/rocq/enc*.v`) and the roadmap for further slices live in
> [`doc/holo-verify.md`](../../../doc/holo-verify.md).

The verification frontier stops at holo today: `crew/holo/` (the x86-64 + aarch64 assembler)
has no formal proof, only the frozen goldens in `holotest.l`/`astest.l`. Those goldens were
each validated by hand — "emit the bytes, `objdump -d -M intel`, confirm the mnemonic" — over a
few dozen forms. This harness **automates that exact round-trip and runs it over tens of
thousands of randomly generated forms**, so the encoder is exercised far past the goldens
before we invest in a proof.

It is the first rung of a ladder (the shape borrowed from `proof/rocq/extract.v`'s
differential oracle, which fuzzes a Coq-extracted normalizer against the live `ev`):

1. **fuzz** (this) — random IR form → holo bytes → disassemble → check the decode matches intent.
2. **prove** — a verified encoder: instruction → bytes, checked against the same decode relation
   the fuzzer uses as its oracle. The fuzzer's per-class checkers are the informal spec the
   proof will formalize.

## How the oracle works

For each generated IR form we already know the intended instruction. We ask holo to encode it,
then **disassemble the bytes with `objdump`** (cross-checked by `llvm-mc`) and verify the decoded
mnemonic + operands match intent. This is a *decode* oracle, not a byte-exact one, because x86
encoding is non-unique: holo legally picks the 32-bit zero-extending `mov` for small positive
immediates, minimal displacement sizes, redundant-but-legal REX prefixes, etc. A byte-exact
compare against GNU `as` would false-alarm on every such choice. So:

- **registers** compare by *abstract identity* (width-agnostic — `eax` and `rax` both read back as
  holo `r0`). The abstract→x86 map is probed directly from the encoder by `regmap.py`, never
  guessed.
- **immediates** compare by numeric value mod 2^64 (so a sign-extended `-1` and its `0xff..ff`
  decode agree).
- **memory** operands compare base / index / scale / displacement structurally.

For x64, `objdump` gives the primary semantic check and `llvm-mc` is a second, independent
decoder required to also decode the bytes without falling back to `.byte`. **arm64** is
disassembled by `llvm-mc --triple=aarch64` (the host `objdump` is not built with aarch64); since
every aarch64 instruction is exactly 4 bytes, the decoded-instruction count must equal
`len(bytes)/4`, which doubles as a full-consumption check.

## Running

```
python3 crew/holo/fuzz/regmap.py                            # verify the abstract-reg -> x86 map
python3 crew/holo/fuzz/fuzz.py --arch x64  -n 300 --seed 7  # x64, both decoders
python3 crew/holo/fuzz/fuzz.py --arch arm64 -n 300 --seed 7 # arm64, via llvm-mc
python3 crew/holo/fuzz/fuzz.py --arch riscv -n 300 --seed 7 # riscv64, via llvm-mc
python3 crew/holo/fuzz/fuzz.py --arch x64 -n 250 --seed 3 --no-llvm    # faster, objdump only
python3 crew/holo/fuzz/fuzz.py --arch arm64 --classes ld,st,li -n 500  # a subset
```

Deterministic per seed. Needs `out/host/love` built, plus `objdump` (x64) / `llvm-mc` (arm64, and
x64 unless `--no-llvm`). Exit code is nonzero iff any sample fails.

## Coverage

**x64 — 25 classes**: moves/immediates (`mov_rr`, `li`), ALU reg+imm (`alu_rr`, `alu_imm`,
`flagalu`), `cmp`, memory base+disp loads/stores incl. the rsp(SIB)/rbp(forced-disp) quirks
(`ld`, `st`, `ld_sized`, `st_sized`), scaled-index addressing (`ldx`, `leax`), `lea`, shifts by
immediate and by CL (`shift`, `shiftv`), `unary`, `push`/`pop`, `setcc` (the setcc+movzx pair),
indirect `jmpr`/`callr`, and the SSE2 double lane (`ssealu`, `cvt`, `movqxr`, `ldsd`, `stsd`).

**arm64 — 16 classes**: `mov_rr`, `li` (the movz/movk chain, 1–4 insns, reconstructed and
compared by value), three-address `alu_rr` (add/sub/and/orr/eor/mul) and 12-bit `alu_imm`,
`cmp`, `ld`/`st` and the sized family (`ldrsb`/`ldursw`/`strb`/… with x-vs-w width checks),
`pushpop` (pre/post-indexed sp), `shift` (lsl/lsr/asr), `unary`, `jmpr`/`callr` (br/blr),
`setcc` (single-insn `cset`), `sx` (sxtb/sxth/sxtw). The two backends share generators where the
neutral IR maps cleanly; the checkers are arch-specific (arm64 is three-address, stores put the
source operand first, ALU immediates are 12-bit, and logical/mul immediates *raise* rather than
emit — so those are not fuzzed there).

**riscv64 — 27 classes**: `mov_rr`, `li` (the lui/addiw/slli/addi chain, reconstructed and
compared by value incl. the 32-bit addiw wraparound), three-address `alu_rr` and 12-bit
`alu_imm`, the **fused flag classes** (riscv has no flags register, so `cmp`/`ucomisd` remember
their operands and the consumer fuses: `cmpbr`/`cmpbr_imm` check the whole inverted-hop+`jal`
shape, `setcc` the `slt`/`sltu`/`seqz` forms, `fcmpbr` the `feq`/`flt`/`fle` predicates),
`ld`/`st` and the sized family, the far-displacement lui+add lane (`ld_far`), synthesized
index addressing (`ldx`, `stx`), shifts/rotates (`rot` checks the srli/slli/or triple),
`shiftv`, `unary`, `sxzx`, `divrem`, `jmpr`/`callr`, `pushpop`, `mulo` (the 5-insn
mulh-vs-sign-fill shape), and the D lane (`fp3`, `fmov`, `fcvt` incl. the `rtz` rounding mode,
`fldst`). riscv's `sp` (x2) is a general register in every encoding, so it needs no
out-of-contract barring at all.

Status as of 2026-07-17: **x64 ~32,500 samples / 5 seeds and arm64 ~27,000 / 5 seeds, zero
encoder discrepancies on either backend.** 2026-07-27: **riscv64 27,000 samples / 5 seeds,
zero discrepancies.**

## Known out-of-contract inputs (not reachable bugs)

The stack pointer has architecture-specific restrictions, so the generators bar it from the
positions where the two backends would diverge — the fuzz analogue of respecting holo's contract:

- **x64**: `rsp` (holo `sp`) cannot be a scaled SIB *index* — slot `100b` means "no index". holo
  silently encodes `sp`-as-index as no-index (dropping the scaled term). Generators bar `sp` from
  index slots (`irand`).
- **arm64**: encoding `31` is SP only in load/store-base and add/sub-immediate contexts; as a
  general data-processing or value operand it is XZR (the zero register). holo maps `sp`→31 and
  uses it *only* as SP (per `crew/holo/arm64.l`), so `(cmp x sp)`, `(jmpr sp)`, a loaded/stored
  value register of `sp`, etc. silently encode the zero register — diverging from x64 where
  `rsp` is a general operand. Generators use `nrand` (no-sp) for those positions and keep `sp`
  only for load/store base and add/sub-immediate (the reachable stack cases, verified).

Neither is reachable from real codegen. If holo ever grows a caller that could hit these, the
backend should hard-reject rather than silently mis-encode — which is what `mov` now does: a
kernel moves SP by name (`mov x9, sp` around an `msr spsel`), and `mov` is ORR against XZR, so
`(mov r9 sp)` would have answered zero. It **scares** instead and points at `(lea d s 0)`,
add-#0, which is the instruction the assembler writes for that move.

## The system lane — `sysdiff.py`

`sysdiff.py` checks the privileged instructions with a different, sharper oracle. Each system op
has exactly one encoding and a small enumerable operand space (a system register by name, a
barrier domain, a cache operation), so instead of disassembling holo's bytes it **assembles the
intended text with `llvm-mc` and demands the same bytes** — byte-exact, in both directions of
the encode/decode pair.

It reads the op tables straight out of `crew/holo/arm64.l` (`arm64-sysregs`, `arm64-pstate`,
`arm64-barrier-opts`, and the four SYS tables), so **a row added to holo is checked on the next
run with no edit here** — the uncovered row is the one that ships wrong. The x86 side has no
name tables (its operand space *is* the register file), so it enumerates: cr0–cr8 × every GPR ×
read/write, `lgdt`/`lidt`/`invlpg` over every base including the rsp-SIB and rbp-forced-disp
quirks, `ltr`, all 256 `int` vectors, and the nullaries.

```
python3 crew/holo/fuzz/sysdiff.py             # both arches (in test_holofuzz)
python3 crew/holo/fuzz/sysdiff.py --arch arm64 -v
```

Two rejections by `llvm-mc` are counted as skips, not failures, because holo is deliberately the
more permissive of the two: writing a **read-only** system register (holo does not model
read-only-ness — a kernel that writes one takes a fault, not wrong bytes), and the wrong-arity
spelling of a SYS operation (`tlbi vae1` with no register — holo encodes Rt=31, XZR, which is a
legal operand it simply does not police). Each SYS operation offers both spellings and the run
fails if `llvm-mc` takes neither, so a table typo cannot hide behind a skip.

Status as of 2026-07-28: **910 system encodings, zero discrepancies** (743 x64, 167 arm64). The
one deliberate divergence is `int 3`: `llvm-mc` folds it to the one-byte `CC`, holo keeps `CD 03`
(see `crew/holo/x64.l` — `trap` is the `CC` form).

## Extending

Add a `g_<class>(rng) -> (ir_string, checker)` generator and register it in `GENS`. The checker
receives the parsed objdump instruction list and raises `Fail(msg)` on any mismatch. Probe what
holo actually emits first (`echo "(...)" | ... | out/host/love`, then `objdump` the bytes) so the
checker matches reality rather than assumption — several classes lower to more than one machine
instruction (e.g. `setcc` → setcc+movzx, three-address ALU with distinct dest → mov+op).
