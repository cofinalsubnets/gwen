#!/usr/bin/env python3
"""vmret.py -- flag lvm_* VM-instruction handlers that contain a ret.

The ll VM is threaded: every `lvm_*` opcode handler is supposed to be
tail-call optimised into a `jmp` to the next handler, never a normal
`ret`. A handler that the compiler failed to TCO emits a real `ret` in
its epilogue, which silently breaks the threaded dispatch. This tool
disassembles an ELF and flags every `lvm_*` function whose body still
contains a `ret` instruction, so the broken-TCO suspects can be triaged.

It is deliberately a first-pass heuristic: it reports *any* ret in a
matching function, without reasoning about whether that ret is reachable
or whether the handler legitimately returns (a few do -- host I/O nifs,
yield, etc.). Expect false positives; the point is to narrow the search.

The kernel is built for two arches (x86_64, aarch64), so the tool is
cross-arch: it reads the ELF's e_machine, picks a return mnemonic for
that target, and -- since the system objdump is usually x86-only --
prefers the multi-target llvm-objdump for any non-x86 image.

Usage:
    vmret.py ELF [--prefix PREFIX] [--objdump PATH]

    --prefix   symbol-name prefix to check (default: lvm_)
    --objdump  disassembler to use; otherwise $OBJDUMP, then (for x86)
               objdump else llvm-objdump first. llvm-objdump auto-detects
               the target from the ELF, so no triple is needed.

Exit status: 0 if no matching function contains a ret, 1 if any do,
2 on a usage or tool error -- so it can gate a build.
"""
import os, re, shutil, struct, subprocess, sys

EM_X86_64    = 62                                 # ELF e_machine values
EM_AARCH64   = 183

# A disassembly line is a function header `<addr> <name>:` or an
# instruction `<addr>: <text>`. Both GNU objdump and llvm-objdump emit
# this shape (with --no-show-raw-insn the raw bytes are gone).
HEADER_RE = re.compile(r"^\s*([0-9a-fA-F]+)\s+<(.+)>:\s*$")   # 1 = start addr, 2 = name
# llvm-objdump also prints intra-function *local labels* as headers of the
# same shape (e.g. `<.L0 >`, `<.Ltmp3>`), sometimes thousands of them.
# They are NOT function boundaries; a ret in the segment
# after one still belongs to the enclosing function. Treating them as
# headers would end the watched function early and silently miss its tail
# rets, so we recognize and skip them.
LABEL_RE  = re.compile(r"^\.")
# Instruction lines: <addr>: <text>. Leading whitespace is optional -- some
# objdump builds indent it, others (e.g. GNU objdump on a high-half kernel
# image) print the address flush-left. Requiring the indent silently skipped
# every instruction and made the tool report all functions ret-free.
INSN_RE   = re.compile(r"^\s*([0-9a-fA-F]+):\s+(.*)$")

# Instruction prefixes that can sit in front of `ret` (e.g. `rep ret`,
# the old AMD branch-target padding). Skipped when extracting the mnemonic.
PREFIXES = {"rep", "repz", "repe", "repnz", "repne", "lock", "bnd",
            "notrack", "data16"}

# Per-arch ordinary-return mnemonic. llvm-objdump with its default flags
# (pseudo-instructions on) spells the function return as `ret` on every
# target we build for; x86 also uses retq/retf/lret. We deliberately do
# NOT match the privileged/exception returns (iret, eret) -- those are
# ISR/trap epilogues, not threaded-VM handlers -- nor the
# register-indirect tail jumps (jmp/br), which are *correct* TCO.
RET_RES = {
    EM_X86_64:    re.compile(r"^l?ret[a-z]*$"),   # ret, retq, retf, lret; not iret
    EM_AARCH64:   re.compile(r"^ret(aa|ab)?$"),   # ret + PAC variants; not eret
}
RET_DEFAULT = RET_RES[EM_X86_64]                  # fallback for an unknown arch


def fail(msg):
    sys.stderr.write("vmret: " + msg + "\n"); sys.exit(2)


def elf_machine(path):
    """Return (e_machine, name) from the ELF header, or fail."""
    try:
        with open(path, "rb") as f:
            hdr = f.read(20)
    except OSError as e:
        fail("%s: %s" % (path, e.strerror))
    if hdr[:4] != b"\x7fELF":
        fail(path + ": not an ELF file")
    if hdr[4] != 2 or hdr[5] != 1:
        fail(path + ": not ELF64 little-endian")
    (machine,) = struct.unpack_from("<H", hdr, 18)
    names = {EM_X86_64: "x86-64", EM_AARCH64: "aarch64"}
    return machine, names.get(machine, "0x%x" % machine)


def func_sizes(path):
    """Map {st_value: st_size} for sized STT_FUNC symbols, read from .symtab.
    The text scan charges any `ret` in inter-function padding to the nearest
    preceding <name>: header (e.g. a stray aligned `ret` after a clean tail-
    jump). Bounding each function to [start, start+size) drops those orphans.
    Empty if stripped (-> scan falls back to its old unbounded behaviour)."""
    with open(path, "rb") as f:
        data = f.read()
    (e_shoff,) = struct.unpack_from("<Q", data, 0x28)
    e_shentsize, e_shnum = struct.unpack_from("<HH", data, 0x3a)
    symtab = None
    for k in range(e_shnum):
        sh = e_shoff + k * e_shentsize
        (sh_type,) = struct.unpack_from("<I", data, sh + 4)
        if sh_type == 2:                                  # SHT_SYMTAB
            symtab = struct.unpack_from("<QQ", data, sh + 24)   # (sh_offset, sh_size)
            break
    sizes = {}
    if symtab:
        off, size = symtab
        for p in range(off, off + size, 24):              # Elf64_Sym is 24 bytes
            st_info = data[p + 4]
            st_value, st_size = struct.unpack_from("<QQ", data, p + 8)
            if (st_info & 0xf) == 2 and st_size:          # STT_FUNC, sized
                sizes[st_value] = st_size
    return sizes


def find_objdump(explicit, machine):
    # The system `objdump` is typically built for the host arch only (x86-64
    # here), so it can't disassemble the kernel's other-arch ELFs --
    # llvm-objdump is multi-target. Prefer GNU objdump only for x86, and
    # llvm-objdump for everything else. An explicit --objdump / $OBJDUMP
    # (e.g. a GNU cross-objdump) always wins.
    env = os.environ.get("OBJDUMP")
    if machine == EM_X86_64:
        order = (explicit, env, "objdump", "llvm-objdump")
    else:
        order = (explicit, env, "llvm-objdump", "objdump")
    for cand in order:
        if cand and shutil.which(cand):
            return cand
    fail("no disassembler found (tried llvm-objdump, objdump); pass --objdump")


def disassemble(objdump, path):
    cmd = [objdump, "-d", "--no-show-raw-insn", path]
    try:
        p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    except OSError as e:
        fail("could not run %s: %s" % (objdump, e.strerror))
    if p.returncode != 0:
        fail("%s failed: %s" % (objdump, p.stderr.decode("utf-8", "replace").strip()))
    return p.stdout.decode("utf-8", "replace").splitlines()


def mnemonic(insn_text):
    """First non-prefix token of an instruction's text."""
    for tok in insn_text.split():
        if tok in PREFIXES:
            continue
        return tok
    return ""


def scan(lines, prefix, ret_re, sizes):
    """Walk the disassembly, grouping by function header. Return
    [(name, [ret_addr, ...]), ...] for matching functions, and the total
    number of matching functions seen (flagged or not). `sizes` maps a
    function start address to its symtab byte size; a ret at or past
    start+size is padding (not the function's) and is ignored -- unsized
    symbols stay unbounded (old behaviour)."""
    flagged, total = [], 0
    name, watching, rets = None, False, []
    lim = 1 << 64                       # current function's extent end (unbounded until a header)

    def close():
        if watching and rets:
            flagged.append((name, rets))

    for line in lines:
        h = HEADER_RE.match(line)
        if h:
            hname = h.group(2).strip()
            if LABEL_RE.match(hname):
                continue                # local label: stay inside this function
            close()
            name = hname
            watching = name.startswith(prefix)
            rets = []
            start = int(h.group(1), 16)
            sz = sizes.get(start, 0)
            lim = start + sz if sz else (1 << 64)   # bound to the symtab extent; unsized -> unbounded
            if watching:
                total += 1
            continue
        if not watching:
            continue
        m = INSN_RE.match(line)
        if m and int(m.group(1), 16) < lim and ret_re.match(mnemonic(m.group(2))):
            rets.append(m.group(1))
    close()
    return flagged, total


def main():
    argv = sys.argv[1:]
    path = prefix = objdump = None
    prefix = "lvm_"
    i = 0
    while i < len(argv):
        a = argv[i]
        if a == "--prefix" and i + 1 < len(argv):
            prefix = argv[i + 1]; i += 2
        elif a == "--objdump" and i + 1 < len(argv):
            objdump = argv[i + 1]; i += 2
        elif a in ("-h", "--help"):
            sys.stdout.write(__doc__); sys.exit(0)
        elif a.startswith("-"):
            fail("unknown option: " + a)
        elif path is None:
            path = a; i += 1
        else:
            fail("unexpected argument: " + a)
    if path is None:
        fail("usage: vmret.py ELF [--prefix PREFIX] [--objdump PATH]")

    machine, mname = elf_machine(path)
    ret_re = RET_RES.get(machine)
    if ret_re is None:
        sys.stderr.write("vmret: note: %s is an unrecognized arch (%s); "
                         "falling back to the generic `ret` matcher\n"
                         % (os.path.basename(path), mname))
        ret_re = RET_DEFAULT

    od = find_objdump(objdump, machine)
    flagged, total = scan(disassemble(od, path), prefix, ret_re, func_sizes(path))

    base = os.path.basename(path)
    if total == 0:
        sys.stderr.write("vmret: no %s* functions found in %s "
                         "(stripped binary?)\n" % (prefix, base))
        sys.exit(2)
    if not flagged:
        print("%s: all %d %s* functions are ret-free" % (base, total, prefix))
        sys.exit(0)

    print("%s: %d of %d %s* functions contain a ret (TCO suspects):"
          % (base, len(flagged), total, prefix))
    width = max(len(n) for n, _ in flagged)
    for name, rets in sorted(flagged):
        addrs = ", ".join("0x" + a for a in rets)
        print("  %-*s  ret @ %s" % (width, name, addrs))
    sys.exit(1)


if __name__ == "__main__":
    main()
