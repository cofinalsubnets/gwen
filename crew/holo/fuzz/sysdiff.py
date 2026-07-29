#!/usr/bin/env python3
"""holo SYSTEM-lane differential -- every privileged encoding, byte-exact against llvm-mc.

The rest of fuzz.py is a DECODE oracle: emit bytes, disassemble, check the meaning came back.
That is the right shape for the ALU/memory surface, where holo may legally pick among several
encodings of one instruction. The system lane is the opposite: each op has exactly one encoding
and a small, enumerable operand space (a system register by name, a barrier domain, a cache
operation), so the sharp oracle is an ASSEMBLE-side one -- write the instruction llvm-mc's way,
assemble it, and demand the same bytes holo produced.

The op tables are read STRAIGHT OUT of crew/holo/arm64.l rather than restated here, so a row
added to holo is checked the next run with no edit to this file -- which is the point: the
uncovered row is the one that ships wrong. The x86 side has no name tables (the operand space
IS the register file), so it enumerates instead.

  crew/holo/fuzz/sysdiff.py            # both arches
  crew/holo/fuzz/sysdiff.py --arch x64 -v
"""
import os, re, subprocess, sys, argparse

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import fuzz                                         # run_holo reads fuzz.TARGET_SYM for the backend
from fuzz import run_holo, R2X                      # the holo driver + the x86 register map

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
A64 = f"{ROOT}/crew/holo/arm64.l"

# A64 system registers llvm-mc refuses to WRITE. Not a holo limitation -- holo does not model
# read-only-ness, and a kernel that writes one gets a fault, not wrong bytes. Skipping the msr
# half keeps the mrs half checked; any OTHER llvm-mc rejection is a real failure.
RO_REJECT = "expected writable system register"


def table(name):
    """the ('key >< ..) rows of a `name (tabof ...)` form in arm64.l, in source order."""
    src = open(A64).read()
    i = src.index(f"\n   {name} (tabof")
    depth, j = 0, src.index("(tabof", i)
    for j in range(j, len(src)):
        if src[j] == "(": depth += 1
        elif src[j] == ")":
            depth -= 1
            if depth == 0: break
    return re.findall(r"\('([A-Za-z0-9_]+) ><", src[i:j])


def cases_a64():
    """(holo IR, llvm-mc assembler text) for every row of every arm64 system table."""
    out = []
    for r in table("arm64-sysregs"):
        out.append((f"(mrs r5 {r})", f"mrs x5, {r}"))
        out.append((f"(msr {r} r5)", f"msr {r}, x5"))
    for o in table("arm64-barrier-opts"):
        out.append((f"(dsb {o})", f"dsb {o}"))
        out.append((f"(dmb {o})", f"dmb {o}"))
    for f in table("arm64-pstate"):
        for i in range(16):
            out.append((f"(msri {f} {i})", f"msr {f}, #{i}"))
    # the SYS family. llvm-mc knows which operations take a register and refuses the other
    # spelling; holo encodes both (Rt = 31 is XZR, a legal operand it simply does not police).
    # so offer BOTH and let the assembler choose -- `pair` demands that exactly the accepted
    # one matched, which catches a table typo that made llvm-mc refuse everything.
    for tab, ir in (("arm64-tlbi-ops", "tlbi"), ("arm64-ic-ops", "ic"),
                    ("arm64-dc-ops", "dc"), ("arm64-at-ops", "at")):
        for o in table(tab):
            out.append((f"({ir} {o} r7)", f"{ir} {o}, x7", f"{ir} {o}"))
            out.append((f"({ir} {o} zr)", f"{ir} {o}", f"{ir} {o}"))
    for imm in (0, 1, 2, 15, 255, 4660, 61440, 65535):
        out.append((f"(brk {imm})", f"brk #{imm}"))
        out.append((f"(dbrk {imm})", f"hlt #{imm}"))       # A64's HLT: the DEBUGGER halt
        out.append((f"(hvc {imm})", f"hvc #{imm}"))
        out.append((f"(smc {imm})", f"smc #{imm}"))
        out.append((f"(udf {imm})", f"udf #{imm}"))
    for n in ("isb", "wfi", "wfe", "eret"):
        out.append((f"({n})", n))
    out.append(("(lea r9 sp 0)", "mov x9, sp"))            # the SP move `mov` cannot make
    out.append(("(lea sp r9 0)", "mov sp, x9"))
    return out


X64_W = {'rax': 'ax', 'rcx': 'cx', 'rdx': 'dx', 'rbx': 'bx', 'rsp': 'sp', 'rbp': 'bp',
         'rsi': 'si', 'rdi': 'di', **{f"r{n}": f"r{n}w" for n in range(8, 16)}}


def cases_x64():
    out = []
    gp = [r for r in R2X if r != 'sp']                     # rsp is not a CR-move operand
    for n in range(9):                                     # cr0..cr8 -- REX.R is what reaches cr8
        for r in gp:
            out.append((f"(ldcr {r} {n})", f"mov {R2X[r]}, cr{n}"))
            out.append((f"(stcr {n} {r})", f"mov cr{n}, {R2X[r]}"))
    for r in R2X:                                          # every base, rsp/rbp quirks included
        for off in (0, 8, 16, 200):
            out.append((f"(lgdt {r} {off})", f"lgdt [{R2X[r]} + {off}]"))
            out.append((f"(lidt {r} {off})", f"lidt [{R2X[r]} + {off}]"))
            out.append((f"(invlpg {r} {off})", f"invlpg [{R2X[r]} + {off}]"))
        out.append((f"(ltr {r})", f"ltr {X64_W[R2X[r]]}"))
    for n in range(256):
        if n == 3: continue    # llvm-mc folds `int 3` to the one-byte CC; holo keeps CD 03
        out.append((f"(int {n})", f"int {n}"))
    for n in ("cli", "sti", "hlt", "ud2", "iretq", "swapgs",
              "rdmsr", "wrmsr", "cpuid", "rdtsc"):
        out.append((f"({n})", n))
    return out


def llvm(arch, lines):
    """assemble each line; answer a list of hex strings, None where llvm-mc refused."""
    triple = "aarch64" if arch == "arm64" else "x86_64"
    head = [] if arch == "arm64" else [".intel_syntax noprefix"]

    def run(body):
        p = subprocess.run(["llvm-mc", f"-triple={triple}", "--show-encoding"],
                           input="\n".join(head + ["\t" + b for b in body]) + "\n",
                           capture_output=True, text=True)
        return p.stdout, p.stderr

    # pass 1: find the lines llvm-mc rejects (its encodings come out unnumbered, so an error
    # would slide every later line by one). pass 2 assembles only what it accepted.
    _, err = run(lines)
    bad = {}
    for m in re.finditer(r"<stdin>:(\d+):\d+: error: (.*)", err):
        bad[int(m.group(1)) - 1 - len(head)] = m.group(2)
    good = [b for i, b in enumerate(lines) if i not in bad]
    out, _ = run(good)
    enc = [re.sub(r"0x|,", "", h) for h in re.findall(r"encoding: \[([^\]]*)\]", out)]
    if len(enc) != len(good):
        sys.exit(f"sysdiff: llvm-mc gave {len(enc)} encodings for {len(good)} lines")
    it = iter(enc)
    return [(None, bad[i]) if i in bad else (next(it), None) for i in range(len(lines))]


def sweep(arch, verbose):
    cases = [c if len(c) == 3 else (*c, None)
             for c in (cases_a64() if arch == "arm64" else cases_x64())]
    fuzz.TARGET_SYM = arch
    hexes = run_holo([(str(i), ir) for i, (ir, _, _) in enumerate(cases)])
    ref = llvm(arch, [asm for _, asm, _ in cases])
    npass = nskip = 0
    fails, seen = [], {}
    for i, (ir, asm, pair) in enumerate(cases):
        if pair is not None: seen.setdefault(pair, 0)
        want, why = ref[i]
        if want is None:
            # the rejections that are not failures: a read-only system register on the WRITE
            # half, and the spelling of a SYS operation llvm-mc does not take (its twin carries
            # the check). anything else means holo said something llvm-mc cannot read.
            if RO_REJECT in why and asm.startswith("msr "): nskip += 1
            elif pair is not None: nskip += 1
            else: fails.append((ir, asm, hexes.get(str(i)), f"llvm-mc reject: {why}"))
            continue
        got = hexes.get(str(i))
        if got == want:
            npass += 1
            if pair is not None: seen[pair] += 1
            if verbose: print(f"  ok  {asm:34} {got}")
        else:
            fails.append((ir, asm, got, f"want {want}"))
    for pair, n in seen.items():
        if n == 0: fails.append((pair, pair, None, "llvm-mc took NEITHER spelling"))
    print(f"\n=== holo {arch} system-lane differential vs llvm-mc ===")
    print(f"  {npass} match  {len(fails)} fail  {nskip} skipped (read-only / wrong-arity spelling)")
    for ir, asm, got, why in fails[:40]:
        print(f"  {ir}\n      as {asm!r} got={got} :: {why}")
    return 1 if fails else 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--arch", default="all", choices=["all", "x64", "arm64"])
    ap.add_argument("-v", action="store_true")
    args = ap.parse_args()
    arches = ["x64", "arm64"] if args.arch == "all" else [args.arch]
    return max(sweep(a, args.v) for a in arches)


if __name__ == "__main__":
    sys.exit(main())
