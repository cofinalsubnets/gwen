#!/usr/bin/env python3
"""Derive holo's abstract-register -> x86-64 mapping directly from the encoder.
Emit (mov r0 rK) for every abstract reg, disassemble, read the source register."""
import subprocess, tempfile, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
AI = f"{ROOT}/out/host/love"
HOLO = [f"{ROOT}/crew/holo/holo.l", f"{ROOT}/crew/holo/x64.l"]

REGS = [f"r{i}" for i in range(15)] + ["sp"]

def run_holo(programs):
    """programs: list of (tag, ir-string). Returns dict tag->hex."""
    helper = '(: (emit t p) (: _ (puts t) _ (puts " ") _ (puts (holo-hex (quote x64) p)) _ (puts "\\n") _ (flush out)))\n'
    body = "".join(f'(emit "{tag}" (quote {ir}))\n' for tag, ir in programs)
    prog = helper + body
    src = "".join(open(f).read() for f in HOLO) + "\n" + prog
    with tempfile.NamedTemporaryFile("w", suffix=".l", delete=False) as f:
        f.write(src); path = f.name
    try:
        out = subprocess.run([AI], stdin=open(path), capture_output=True, text=True, timeout=60)
    finally:
        os.unlink(path)
    res = {}
    for line in out.stdout.splitlines():
        parts = line.split()
        if len(parts) == 2:
            res[parts[0]] = parts[1]
    if out.returncode != 0 or out.stderr.strip():
        sys.stderr.write("HOLO STDERR:\n" + out.stderr + "\n")
    return res

def objdump(hexstr):
    """Disassemble raw hex bytes as x86-64. Return list of (mnemonic, operands-str)."""
    b = bytes.fromhex(hexstr)
    with tempfile.NamedTemporaryFile("wb", suffix=".bin", delete=False) as f:
        f.write(b); path = f.name
    try:
        out = subprocess.run(
            ["objdump","-D","-b","binary","-m","i386:x86-64","-M","intel", path],
            capture_output=True, text=True)
    finally:
        os.unlink(path)
    insns = []
    for line in out.stdout.splitlines():
        m = re.match(r'\s+[0-9a-f]+:\s+([0-9a-f ]+?)\s{2,}([a-z][a-z0-9.]*)\s*(.*)', line)
        if m:
            insns.append((m.group(2), m.group(3).strip()))
    return insns

if __name__ == "__main__":
    progs = [(r, f"(({ 'mov' } r0 {r}))") for r in REGS]
    res = run_holo(progs)
    print(f"{'abstract':8} {'hex':14} {'disasm'}")
    r2x = {}
    for r in REGS:
        h = res.get(r, "")
        ins = objdump(h) if h else []
        d = ins[0] if ins else ("?", "?")
        # mov dest,src  -> src is the abstract reg's x86 name
        ops = d[1].split(",")
        src = ops[1].strip() if len(ops) == 2 else "?"
        r2x[r] = src
        print(f"{r:8} {h:14} {d[0]} {d[1]}   -> {r}={src}")
    print("\nR2X =", r2x)
