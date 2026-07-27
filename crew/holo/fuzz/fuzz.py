#!/usr/bin/env python3
"""holo encoder differential fuzzer -- x86-64 + aarch64 + riscv64 (fuzz-first rung of the ladder).

For each generated IR form we know the intended instruction. We ask holo to encode it for the
selected --arch, then DISASSEMBLE the bytes (x64: objdump, cross-checked by llvm-mc; arm64 and
riscv: llvm-mc) and verify the decoded instruction matches intent -- automating exactly the manual
round-trip the goldens document ("emit the bytes, disassemble, confirm the mnemonic"), extended
to operands and run over many forms.

Tolerates encoding non-uniqueness: registers compare by ABSTRACT identity (width-agnostic,
so eax/rax both read as holo r0), immediates by numeric value mod 2^64. A byte-exact oracle
would false-alarm on holo's legal 32-bit-mov-zero-extend and disp-size choices; a decode
oracle checks meaning, not layout.
"""
import subprocess, tempfile, os, re, sys, random, argparse

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
AI = f"{ROOT}/out/host/love"
HOLO = [f"{ROOT}/crew/holo/holo.l", f"{ROOT}/crew/holo/x64.l", f"{ROOT}/crew/holo/arm64.l",
        f"{ROOT}/crew/holo/riscv.l"]
MASK = (1 << 64) - 1

# --- holo abstract reg <-> x86 (probed by regmap.py; canonical 64-bit names) ---
R2X = {'r0':'rax','r1':'rcx','r2':'rdx','r3':'rbx','r4':'rbp','r5':'rsi','r6':'rdi',
       'r7':'r8','r8':'r9','r9':'r10','r10':'r11','r11':'r12','r12':'r13','r13':'r14',
       'r14':'r15','sp':'rsp'}
GPREGS = list(R2X.keys())

# width families: every x86 gp name -> canonical 64-bit -> abstract
_FAMILIES = {
 'rax':['rax','eax','ax','al'], 'rcx':['rcx','ecx','cx','cl'], 'rdx':['rdx','edx','dx','dl'],
 'rbx':['rbx','ebx','bx','bl'], 'rsp':['rsp','esp','sp','spl'], 'rbp':['rbp','ebp','bp','bpl'],
 'rsi':['rsi','esi','si','sil'], 'rdi':['rdi','edi','di','dil'],
}
for n in range(8,16):
    _FAMILIES[f'r{n}'] = [f'r{n}', f'r{n}d', f'r{n}w', f'r{n}b']
X86_TO_64 = {name: canon for canon, names in _FAMILIES.items() for name in names}
X64_TO_ABS = {v: k for k, v in R2X.items()}

def name_to_abs(x86name):
    canon = X86_TO_64.get(x86name)
    return X64_TO_ABS.get(canon)

# --- arm64 (holo target 'arm64): r0..r15 -> x0..x15 (identity), sp -> 31.
# Neutral fuzzing uses the shared r0..r14+sp set. w<N>/x<N> both read as r<N>. ---
def arm_name_to_abs(name):
    name = name.strip()
    if name in ('sp', 'wsp'): return 'sp'
    if name in ('xzr', 'wzr'): return 'zr'
    m = re.fullmatch(r'[xw](\d+)', name)
    return f"r{int(m.group(1))}" if m else None

# arch-dispatch globals -- main() points these at the selected backend so the
# shared checkers/parsers stay arch-agnostic.
NAME_TO_ABS = name_to_abs          # x86 name / arm name -> abstract reg
TARGET_SYM  = 'x64'                # holo-hex target symbol

# ---------------------------------------------------------------- holo driver
def run_holo(programs, timeout=120):
    helper = ('(: (emit t p) (: _ (puts t) _ (puts " ") '
              f'_ (puts (holo-hex (quote {TARGET_SYM}) p)) _ (puts "\\n") _ (flush out)))\n')
    body = "".join(f'(emit "{tag}" (quote ({ir})))\n' for tag, ir in programs)
    src = "".join(open(f).read() for f in HOLO) + "\n" + helper + body
    with tempfile.NamedTemporaryFile("w", suffix=".l", delete=False) as f:
        f.write(src); path = f.name
    try:
        out = subprocess.run([AI], stdin=open(path), capture_output=True, text=True, timeout=timeout)
    finally:
        os.unlink(path)
    res = {}
    for line in out.stdout.splitlines():
        p = line.split()
        if len(p) == 2 and re.fullmatch(r'[0-9a-f]*', p[1]):
            res[p[0]] = p[1]
    if out.returncode != 0:
        sys.stderr.write(f"[holo exit {out.returncode}] {out.stderr[:400]}\n")
    return res

# ---------------------------------------------------------------- disassembly
def objdump(hexstr):
    """-> list of dicts {mnem, ops(str), nbytes, bad}"""
    b = bytes.fromhex(hexstr)
    with tempfile.NamedTemporaryFile("wb", suffix=".bin", delete=False) as f:
        f.write(b); path = f.name
    try:
        out = subprocess.run(["objdump","-D","-b","binary","-m","i386:x86-64","-M","intel", path],
                             capture_output=True, text=True)
    finally:
        os.unlink(path)
    insns = []
    for line in out.stdout.splitlines():
        m = re.match(r'\s+[0-9a-f]+:\s+([0-9a-f ]+?)\s{2,}(.*)', line)
        if m:
            nb = len(m.group(1).split())
            mnem, ops = strip_prefixes(m.group(2).strip())
            insns.append({'mnem': mnem, 'ops': ops,
                          'nbytes': nb, 'bad': '(bad)' in line})
    return insns

# objdump renders a redundant/mandatory prefix as a leading pseudo-mnemonic
# (e.g. "rex mov ...", "data16 rex mov ...") -- fold it off to reach the real op.
_PREFIX = re.compile(r'^(rex(\.[wrxb]+)?|data16|data32|addr32|lock|rep|repz|repnz|'
                     r'bnd|notrack|cs|ss|ds|es|fs|gs)$')
def strip_prefixes(text):
    toks = text.split()
    while toks and _PREFIX.match(toks[0]):
        toks.pop(0)
    if not toks: return ("", "")
    return toks[0], " ".join(toks[1:])

def llvm_ok(hexstr):
    """secondary x86 decoder: llvm-mc must decode without emitting .byte/error."""
    spaced = " ".join(f"0x{hexstr[i:i+2]}" for i in range(0, len(hexstr), 2))
    p = subprocess.run(["llvm-mc","--disassemble","--triple=x86_64"],
                       input=spaced, capture_output=True, text=True)
    if p.returncode != 0: return False, p.stderr.strip()[:120]
    if '.byte' in p.stdout: return False, "llvm .byte (undecodable)"
    return True, p.stdout.strip()

def disasm_arm(hexstr):
    """aarch64 via llvm-mc (host objdump lacks aarch64). Every insn is 4 bytes,
    so len(insns) must equal len(bytes)/4 -- a built-in full-consumption check."""
    spaced = " ".join(f"0x{hexstr[i:i+2]}" for i in range(0, len(hexstr), 2))
    p = subprocess.run(["llvm-mc","--disassemble","--triple=aarch64"],
                       input=spaced, capture_output=True, text=True)
    insns = []
    for line in p.stdout.splitlines():
        line = line.split("//")[0].strip()          # drop the =0x.. value comment
        if not line: continue
        if line.startswith(".byte"):
            insns.append({'mnem': '.byte', 'ops': '', 'nbytes': 4, 'bad': True}); continue
        parts = line.split(None, 1)
        insns.append({'mnem': parts[0], 'ops': (parts[1].strip() if len(parts) > 1 else ''),
                      'nbytes': 4, 'bad': False})
    want = len(hexstr) // 8
    if len(insns) != want:                            # undecodable/misaligned bytes
        insns.append({'mnem': f'<{len(insns)}!={want} insns>', 'ops': '', 'nbytes': 4, 'bad': True})
    return insns

# ---------------------------------------------------------------- operand parse
def parse_ops_x64(opstr):
    """split top-level comma operands (x86 memory has no commas inside our forms)."""
    return [o.strip() for o in opstr.split(",")] if opstr else []

def parse_ops_arm(opstr):
    """comma-split respecting [...] brackets: 'x3, [sp, #-16]!' -> ['x3','[sp, #-16]!']"""
    out, depth, cur = [], 0, ""
    for ch in opstr:
        if ch == '[': depth += 1
        elif ch == ']': depth -= 1
        if ch == ',' and depth == 0:
            out.append(cur.strip()); cur = ""
        else: cur += ch
    if cur.strip(): out.append(cur.strip())
    return out

def parse_mem_x64(op):
    """'QWORD PTR [rbp-0x8]' / '[rax+rcx*4+0x10]' -> dict or None"""
    m = re.search(r'\[([^\]]+)\]', op)
    if not m: return None
    inner = m.group(1)
    base = idx = None; scale = 1; disp = 0
    mi = re.search(r'([a-z0-9]+)\*([1248])', inner)
    if mi:
        idx = mi.group(1); scale = int(mi.group(2))
        inner = inner.replace(mi.group(0), "")
    md = re.search(r'([+-])(0x[0-9a-f]+|\d+)\s*$', inner)
    if md:
        v = int(md.group(2), 0); disp = v if md.group(1) == '+' else -v
        inner = inner[:md.start()]
    toks = [t for t in re.split(r'[+]', inner) if t.strip()]
    if toks: base = toks[0].strip()
    return {'base': base, 'index': idx, 'scale': scale, 'disp': disp}

def parse_mem_arm(op):
    """'[x6, #8]' / '[x6]' / '[sp, #-16]!' / '[x0, x1, lsl #3]' -> dict or None"""
    m = re.search(r'\[([^\]]+)\]', op)
    if not m: return None
    parts = [p.strip() for p in m.group(1).split(",")]
    base = parts[0]; idx = None; scale = 1; disp = 0
    for p in parts[1:]:
        if p.startswith('#'):
            disp = imm_val_arm(p)
        elif p.startswith(('x','w')) and re.fullmatch(r'[xw]\d+', p):
            idx = p
        elif p.startswith('lsl'):
            mm = re.search(r'#(\d+)', p); scale = (1 << int(mm.group(1))) if mm else 1
    return {'base': base, 'index': idx, 'scale': scale, 'disp': disp}

def imm_val(op):        # x86 intel immediates (0x.. / decimal)
    op = op.strip()
    try: return int(op, 0) & MASK
    except ValueError: return None

def imm_val_arm(op):    # '#5' / '#-8' / '#0x3c'
    op = op.strip().lstrip('#')
    try: return int(op, 0)
    except ValueError: return None

# arch dispatch -- main() rebinds these to the selected backend
DISASM    = objdump
PARSE_OPS = parse_ops_x64
PARSE_MEM = parse_mem_x64
IMM_VAL   = imm_val

def parse_ops(s): return PARSE_OPS(s)
def parse_mem(op): return PARSE_MEM(op)
def reg_abs(op): return NAME_TO_ABS(op.strip())

# ---------------------------------------------------------------- checkers
class Fail(Exception): pass

def expect_single(insns):
    if not insns: raise Fail("no instruction decoded")
    if any(i['bad'] for i in insns): raise Fail("undecodable/bad byte")
    if len(insns) != 1: raise Fail(f"{len(insns)} insns, expected 1: " +
                                   "; ".join(f"{i['mnem']} {i['ops']}" for i in insns))
    return insns[0]

def expect_n(insns, n):
    if any(i['bad'] for i in insns): raise Fail("undecodable/bad byte")
    if len(insns) != n: raise Fail(f"{len(insns)} insns, expected {n}: " +
                                   "; ".join(f"{i['mnem']} {i['ops']}" for i in insns))
    return insns

def chk_mnem(ins, allowed):
    if ins['mnem'] not in allowed:
        raise Fail(f"mnem {ins['mnem']} not in {allowed} (ops {ins['ops']})")

def chk_reg(op, want_abs):
    got = reg_abs(op)
    if got != want_abs: raise Fail(f"reg {op}->{got} != {want_abs}")

def chk_mem(op, base, disp, index=None, scale=1):
    m = parse_mem(op)
    if not m: raise Fail(f"not a mem operand: {op}")
    if reg_abs(m['base']) != base: raise Fail(f"mem base {m['base']}->{reg_abs(m['base'])} != {base}")
    if m['disp'] != disp: raise Fail(f"mem disp {m['disp']} != {disp}")
    if index is not None:
        if reg_abs(m['index'] or '') != index: raise Fail(f"mem index {m['index']} != {index}")
        if m['scale'] != scale: raise Fail(f"mem scale {m['scale']} != {scale}")

# ---------------------------------------------------------------- generators
def rrand(rng): return rng.choice(GPREGS)
# rsp (holo 'sp') cannot be a scaled SIB index on x86-64 -- the index slot 100b
# means "no index". Real codegen never scales the stack pointer; bar it here so
# the index-addressing classes stay in-contract. (holo silently encodes sp-as-
# index as no-index rather than rejecting -- a latent out-of-contract weakness.)
def irand(rng): return rng.choice([r for r in GPREGS if r != 'sp'])

def imm_small(rng): return rng.randint(0, 127)
def imm_s32(rng): return rng.randint(-(1<<31), (1<<31)-1)
def imm_u32(rng): return rng.randint(0, (1<<32)-1)
def imm_big(rng): return rng.randint(1<<32, (1<<63)-1)
def disp_any(rng): return rng.choice([0, rng.randint(-128,127), rng.randint(-(1<<31),(1<<31)-1)])

def g_mov_rr(rng):
    d, s = rrand(rng), rrand(rng)
    def chk(ins):
        i = expect_single(ins); chk_mnem(i, {'mov'}); o = parse_ops(i['ops'])
        chk_reg(o[0], d); chk_reg(o[1], s)
    return f"(mov {d} {s})", chk

def g_li(rng):
    d = rrand(rng); imm = rng.choice([imm_small,imm_s32,imm_u32,imm_big])(rng)
    def chk(ins):
        i = expect_single(ins); chk_mnem(i, {'mov','movabs'}); o = parse_ops(i['ops'])
        chk_reg(o[0], d)
        if imm_val(o[1]) != (imm & MASK): raise Fail(f"imm {imm_val(o[1])} != {imm & MASK}")
    return f"(li {d} {imm})", chk

def g_alu_rr(rng):
    op = rng.choice(['add','sub','and','or','xor','imul']); d, b = rrand(rng), rrand(rng)
    def chk(ins):
        i = expect_single(ins); chk_mnem(i, {op}); o = parse_ops(i['ops'])
        chk_reg(o[0], d); chk_reg(o[1], b)
    return f"({op} {d} {d} {b})", chk

def g_alu_imm(rng):
    op = rng.choice(['add','sub','and','or','xor']); d = rrand(rng); imm = imm_s32(rng)
    def chk(ins):
        i = expect_single(ins); chk_mnem(i, {op}); o = parse_ops(i['ops'])
        chk_reg(o[0], d)
        if imm_val(o[1]) != (imm & MASK): raise Fail(f"imm {imm_val(o[1])} != {imm & MASK}")
    return f"({op} {d} {d} {imm})", chk

def g_cmp(rng):
    a = rrand(rng)
    if rng.random() < 0.5:
        b = rrand(rng)
        def chk(ins):
            i = expect_single(ins); chk_mnem(i,{'cmp'}); o=parse_ops(i['ops'])
            chk_reg(o[0],a); chk_reg(o[1],b)
        return f"(cmp {a} {b})", chk
    imm = imm_s32(rng)
    def chk(ins):
        i = expect_single(ins); chk_mnem(i,{'cmp'}); o=parse_ops(i['ops'])
        chk_reg(o[0],a)
        if imm_val(o[1]) != (imm & MASK): raise Fail(f"imm {imm_val(o[1])} != {imm&MASK}")
    return f"(cmp {a} {imm})", chk

def g_ld(rng):
    d, base, off = rrand(rng), rrand(rng), disp_any(rng)
    def chk(ins):
        i = expect_single(ins); chk_mnem(i,{'mov'}); o=parse_ops(i['ops'])
        chk_reg(o[0], d); chk_mem(o[1], base, off)
    return f"(ld {d} {base} {off})", chk

def g_st(rng):
    base, off, s = rrand(rng), disp_any(rng), rrand(rng)
    def chk(ins):
        i = expect_single(ins); chk_mnem(i,{'mov'}); o=parse_ops(i['ops'])
        chk_mem(o[0], base, off); chk_reg(o[1], s)
    return f"(st {base} {off} {s})", chk

SIZED_LD = {'ld1':({'movsx'},), 'ld2':({'movsx'},), 'ld4':({'movsxd'},),
            'ldu1':({'movzx'},), 'ldu2':({'movzx'},), 'ldu4':({'mov'},)}
def g_ld_sized(rng):
    op = rng.choice(list(SIZED_LD)); d, base, off = rrand(rng), rrand(rng), disp_any(rng)
    allowed = SIZED_LD[op][0]
    def chk(ins):
        i = expect_single(ins); chk_mnem(i, allowed); o=parse_ops(i['ops'])
        chk_reg(o[0], d); chk_mem(o[1], base, off)
    return f"({op} {d} {base} {off})", chk

def g_st_sized(rng):
    op = rng.choice(['st1','st2','st4']); base, off, s = rrand(rng), disp_any(rng), rrand(rng)
    def chk(ins):
        i = expect_single(ins); chk_mnem(i,{'mov'}); o=parse_ops(i['ops'])
        chk_mem(o[0], base, off); chk_reg(o[1], s)
    return f"({op} {base} {off} {s})", chk

def g_pushpop(rng):
    if rng.random() < 0.5:
        s = rrand(rng)
        def chk(ins):
            i=expect_single(ins); chk_mnem(i,{'push'}); chk_reg(parse_ops(i['ops'])[0], s)
        return f"(push {s})", chk
    d = nrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'pop'}); chk_reg(parse_ops(i['ops'])[0], d)
    return f"(pop {d})", chk

def g_lea(rng):
    d, base, off = rrand(rng), rrand(rng), disp_any(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'lea'}); o=parse_ops(i['ops'])
        chk_reg(o[0], d); chk_mem(o[1], base, off)
    return f"(lea {d} {base} {off})", chk

def g_shift(rng):
    op = rng.choice(['shl','shr','sar','rol','ror']); d = rrand(rng); c = rng.randint(0,63)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{op}); o=parse_ops(i['ops'])
        chk_reg(o[0], d)
        if imm_val(o[1]) != c: raise Fail(f"shift cnt {imm_val(o[1])} != {c}")
    return f"({op} {d} {c})", chk

def g_unary(rng):
    op = rng.choice(['neg','not','inc','dec']); d = rrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{op}); chk_reg(parse_ops(i['ops'])[0], d)
    return f"({op} {d})", chk

def g_ldx(rng):
    d, base, ix = rrand(rng), rrand(rng), irand(rng); sc = rng.choice([1,2,4,8]); dp = disp_any(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'mov'}); o=parse_ops(i['ops'])
        chk_reg(o[0], d); chk_mem(o[1], base, dp, index=ix, scale=sc)
    return f"(ldx {d} {base} {ix} {sc} {dp})", chk

def g_leax(rng):
    d, base, ix = rrand(rng), rrand(rng), irand(rng); sc = rng.choice([1,2,4,8]); dp = disp_any(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'lea'}); o=parse_ops(i['ops'])
        chk_reg(o[0], d); chk_mem(o[1], base, dp, index=ix, scale=sc)
    return f"(leax {d} {base} {ix} {sc} {dp})", chk

def g_shiftv(rng):
    op = rng.choice(['shlv','shrv','sarv']); mn = {'shlv':'shl','shrv':'shr','sarv':'sar'}[op]
    d = rng.choice([r for r in GPREGS if r != 'r1'])   # count pinned to r1=cl; d must differ
    def chk(ins):
        i = expect_single(ins); chk_mnem(i,{mn}); o=parse_ops(i['ops'])
        chk_reg(o[0], d)
        if o[1] != 'cl': raise Fail(f"shiftv count {o[1]} != cl")
    return f"({op} {d} r1)", chk

def g_flagalu(rng):
    op = rng.choice(['adds','subs']); mn = op[:-1]; d = rrand(rng)
    if rng.random() < 0.5:
        b = rrand(rng)
        def chk(ins):
            i=expect_single(ins); chk_mnem(i,{mn}); o=parse_ops(i['ops'])
            chk_reg(o[0],d); chk_reg(o[1],b)
        return f"({op} {d} {d} {b})", chk
    imm = imm_s32(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{mn}); o=parse_ops(i['ops'])
        chk_reg(o[0],d)
        if imm_val(o[1]) != (imm & MASK): raise Fail(f"imm {imm_val(o[1])} != {imm&MASK}")
    return f"({op} {d} {d} {imm})", chk

SETCC = {'eq':'sete','ne':'setne','lt':'setl','le':'setle','gt':'setg','ge':'setge',
         'below':'setb','above':'seta','ae':'setae','be':'setbe','s':'sets','ns':'setns',
         'vs':'seto','vc':'setno'}
def g_setcc(rng):
    cond = rng.choice(list(SETCC)); d = rrand(rng); mn = SETCC[cond]
    def chk(insns):
        if any(i['bad'] for i in insns): raise Fail("objdump (bad)")
        if len(insns) != 2: raise Fail(f"{len(insns)} insns, want setcc+movzx: " +
                                       "; ".join(f"{i['mnem']} {i['ops']}" for i in insns))
        a, b = insns
        chk_mnem(a, {mn}); chk_reg(parse_ops(a['ops'])[0], d)   # setcc dl (low byte of d)
        chk_mnem(b, {'movzx'}); chk_reg(parse_ops(b['ops'])[0], d)
    return f"(set {cond} {d})", chk

def g_jmpr(rng):
    d = rrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'jmp'}); chk_reg(parse_ops(i['ops'])[0], d)
    return f"(jmpr {d})", chk

def g_callr(rng):
    d = rrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'call'}); chk_reg(parse_ops(i['ops'])[0], d)
    return f"(callr {d})", chk

# --- SSE2 double lane: f0..f15 <-> xmm0..xmm15 ---
FREGS = [f"f{i}" for i in range(16)]
def frand(rng): return rng.choice(FREGS)
def xmm_idx(op):
    m = re.fullmatch(r'xmm(\d+)', op.strip())
    return int(m.group(1)) if m else None
def chk_xmm(op, fN):
    want = int(fN[1:])
    if xmm_idx(op) != want: raise Fail(f"xmm {op} != xmm{want}")

def g_ssealu(rng):
    op = rng.choice(['addsd','subsd','mulsd','divsd']); d, s = frand(rng), frand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{op}); o=parse_ops(i['ops'])
        chk_xmm(o[0], d); chk_xmm(o[1], s)
    return f"({op} {d} {s})", chk

def g_cvt(rng):
    d, g = frand(rng), rrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'cvtsi2sd'}); o=parse_ops(i['ops'])
        chk_xmm(o[0], d); chk_reg(o[1], g)
    return f"(cvtsi2sd {d} {g})", chk

def g_movqxr(rng):
    d, g = frand(rng), rrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'movq'}); o=parse_ops(i['ops'])
        chk_xmm(o[0], d); chk_reg(o[1], g)
    return f"(movqxr {d} {g})", chk

def g_ldsd(rng):
    d, base, off = frand(rng), rrand(rng), disp_any(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'movsd'}); o=parse_ops(i['ops'])
        chk_xmm(o[0], d); chk_mem(o[1], base, off)
    return f"(ldsd {d} {base} {off})", chk

def g_stsd(rng):
    base, off, s = rrand(rng), disp_any(rng), frand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'movsd'}); o=parse_ops(i['ops'])
        chk_mem(o[0], base, off); chk_xmm(o[1], s)
    return f"(stsd {base} {off} {s})", chk

GENS_X64 = {
 'mov_rr': g_mov_rr, 'li': g_li, 'alu_rr': g_alu_rr, 'alu_imm': g_alu_imm, 'cmp': g_cmp,
 'ld': g_ld, 'st': g_st, 'ld_sized': g_ld_sized, 'st_sized': g_st_sized,
 'pushpop': g_pushpop, 'lea': g_lea, 'shift': g_shift, 'unary': g_unary,
 'ldx': g_ldx, 'leax': g_leax,
 'shiftv': g_shiftv, 'flagalu': g_flagalu, 'setcc': g_setcc, 'jmpr': g_jmpr, 'callr': g_callr,
 'ssealu': g_ssealu, 'cvt': g_cvt, 'movqxr': g_movqxr, 'ldsd': g_ldsd, 'stsd': g_stsd,
}

# ================================================================ arm64 lane
# holo target 'arm64: native three-address, fixed 32-bit insns, r0..r14+sp shared.
# Disassembled by llvm-mc (host objdump lacks aarch64). Mnemonics differ (or->orr,
# xor->eor, imul->mul, shifts->lsl/lsr/asr, sized->ldrsb/ldursw.., set->cset), store
# operand order is SRC-first, ALU immediates are 12-bit, logical/mul imm RAISE, and
# `li` is a movz/movk chain (1..4 insns) -- so these checkers are arm-specific.
def arm_width(op):
    op = op.strip()
    if op in ('sp','wsp'): return 'sp'
    return op[0] if op and op[0] in 'xw' else '?'

def chk_width(op, w):
    if arm_width(op) != w: raise Fail(f"reg {op} width {arm_width(op)} != {w}")

def recon_li(insns, dabs):
    """reconstruct the 64-bit value from holo's movz/movk(/movn/mov-alias) chain."""
    val = 0
    for i in insns:
        if i['bad']: raise Fail("undecodable in li chain")
        o = parse_ops_arm(i['ops'])
        if reg_abs(o[0]) != dabs: raise Fail(f"li dest {o[0]} != {dabs}")
        sh = re.search(r'lsl #(\d+)', i['ops']); shift = int(sh.group(1)) if sh else 0
        imm = imm_val_arm(o[1]) if len(o) > 1 else 0
        m = i['mnem']
        if m in ('mov','movz'):   val = (imm << shift) & MASK
        elif m == 'movk':         val = (val & ~(0xffff << shift) | ((imm & 0xffff) << shift)) & MASK
        elif m == 'movn':         val = (~((imm & 0xffff) << shift)) & MASK
        elif m == 'orr':          val = imm & MASK
        else: raise Fail(f"li unexpected mnem {m} ({i['mnem']} {i['ops']})")
    return val & MASK

def arm_disp(rng):
    return rng.choice([0, rng.randint(-256, 255), 8 * rng.randint(0, 511)])  # ldur/ldr single-insn

# aarch64 reg 31 is SP only in load/store-base and add/sub-imm contexts; in a
# data-processing or value position it is XZR (the zero register). holo maps
# 'sp'->31 and uses it ONLY as SP (per crew/holo/arm64.l), so 'sp' is out-of-
# contract as a general operand -- bar it from those slots (cf. x64's irand).
def nrand(rng): return rng.choice([r for r in GPREGS if r != 'sp'])

def a_mov_rr(rng):
    d, s = nrand(rng), nrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'mov'}); o=parse_ops_arm(i['ops'])
        chk_reg(o[0], d); chk_reg(o[1], s)
    return f"(mov {d} {s})", chk

def a_li(rng):
    d = nrand(rng); imm = rng.choice([imm_small, lambda r: r.randint(0,0xffff),
                                      imm_s32, imm_u32, imm_big,
                                      lambda r: -r.randint(1,0xffff)])(rng)
    def chk(ins):
        if not ins: raise Fail("no insns")
        if reg_abs(parse_ops_arm(ins[0]['ops'])[0]) is None: raise Fail("li dest unparsed")
        got = recon_li(ins, reg_abs(parse_ops_arm(ins[0]['ops'])[0]))
        # dest already checked == some reg; ensure it is d and value matches
        if reg_abs(parse_ops_arm(ins[0]['ops'])[0]) != d: raise Fail("li dest != d")
        if got != (imm & MASK): raise Fail(f"li value {hex(got)} != {hex(imm & MASK)}")
    return f"(li {d} {imm})", chk

ARM_ALU = {'add':'add','sub':'sub','and':'and','or':'orr','xor':'eor','imul':'mul'}
def a_alu_rr(rng):
    op = rng.choice(list(ARM_ALU)); mn = ARM_ALU[op]; d,a,b = nrand(rng),nrand(rng),nrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{mn}); o=parse_ops_arm(i['ops'])
        chk_reg(o[0],d); chk_reg(o[1],a); chk_reg(o[2],b)
    return f"({op} {d} {a} {b})", chk

def a_alu_imm(rng):
    op = rng.choice(['add','sub']); d,a = rrand(rng),rrand(rng); imm = rng.randint(0,4095)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{op}); o=parse_ops_arm(i['ops'])
        chk_reg(o[0],d); chk_reg(o[1],a)
        if imm_val_arm(o[2]) != imm: raise Fail(f"imm {imm_val_arm(o[2])} != {imm}")
    return f"({op} {d} {a} {imm})", chk

def a_cmp(rng):
    a = nrand(rng)
    if rng.random() < 0.5:
        b = nrand(rng)
        def chk(ins):
            i=expect_single(ins); chk_mnem(i,{'cmp'}); o=parse_ops_arm(i['ops'])
            chk_reg(o[0],a); chk_reg(o[1],b)
        return f"(cmp {a} {b})", chk
    imm = rng.randint(0,4095)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'cmp'}); o=parse_ops_arm(i['ops'])
        chk_reg(o[0],a)
        if imm_val_arm(o[1]) != imm: raise Fail(f"imm {imm_val_arm(o[1])} != {imm}")
    return f"(cmp {a} {imm})", chk

def a_ld(rng):
    d, base, off = nrand(rng), rrand(rng), arm_disp(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'ldr','ldur'}); o=parse_ops_arm(i['ops'])
        chk_reg(o[0], d); chk_width(o[0],'x'); chk_mem(o[1], base, off)
    return f"(ld {d} {base} {off})", chk

def a_st(rng):
    base, off, s = rrand(rng), arm_disp(rng), nrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'str','stur'}); o=parse_ops_arm(i['ops'])
        chk_reg(o[0], s); chk_width(o[0],'x'); chk_mem(o[1], base, off)   # src FIRST on arm
    return f"(st {base} {off} {s})", chk

ARM_LD_SIZED = {'ld1':({'ldrsb','ldursb'},'x'), 'ld2':({'ldrsh','ldursh'},'x'),
                'ld4':({'ldrsw','ldursw'},'x'), 'ldu1':({'ldrb','ldurb'},'w'),
                'ldu2':({'ldrh','ldurh'},'w'), 'ldu4':({'ldr','ldur'},'w')}
def a_ld_sized(rng):
    op = rng.choice(list(ARM_LD_SIZED)); mns,w = ARM_LD_SIZED[op]
    d, base, off = nrand(rng), rrand(rng), arm_disp(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,mns); o=parse_ops_arm(i['ops'])
        chk_reg(o[0], d); chk_width(o[0], w); chk_mem(o[1], base, off)
    return f"({op} {d} {base} {off})", chk

ARM_ST_SIZED = {'st1':{'strb','sturb'}, 'st2':{'strh','sturh'}, 'st4':{'str','stur'}}
def a_st_sized(rng):
    op = rng.choice(list(ARM_ST_SIZED)); mns = ARM_ST_SIZED[op]
    base, off, s = rrand(rng), arm_disp(rng), nrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,mns); o=parse_ops_arm(i['ops'])
        chk_reg(o[0], s); chk_width(o[0],'w'); chk_mem(o[1], base, off)
    return f"({op} {base} {off} {s})", chk

def a_pushpop(rng):
    if rng.random() < 0.5:
        s = nrand(rng)
        def chk(ins):
            i=expect_single(ins); chk_mnem(i,{'str'}); o=parse_ops_arm(i['ops'])
            chk_reg(o[0], s); m=parse_mem_arm(o[1])
            if reg_abs(m['base']) != 'sp': raise Fail(f"push base {m['base']} != sp")
        return f"(push {s})", chk
    d = nrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'ldr'}); o=parse_ops_arm(i['ops'])
        chk_reg(o[0], d)
        if reg_abs(parse_mem_arm(o[1])['base']) != 'sp': raise Fail("pop base != sp")
    return f"(pop {d})", chk

ARM_SHIFT = {'shl':'lsl','shr':'lsr','sar':'asr'}
def a_shift(rng):
    op = rng.choice(list(ARM_SHIFT)); mn = ARM_SHIFT[op]; d = nrand(rng); c = rng.randint(0,63)
    # lsl #0 and lsr #0 share the ubfm #0,#63 encoding (both no-ops); llvm prints
    # the lsr alias, so accept any shift mnemonic at count 0.
    allowed = {'lsl','lsr','asr'} if c == 0 else {mn}
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,allowed); o=parse_ops_arm(i['ops'])
        chk_reg(o[0], d); chk_reg(o[1], d)
        if imm_val_arm(o[2]) != c: raise Fail(f"shift {imm_val_arm(o[2])} != {c}")
    return f"({op} {d} {c})", chk

def a_unary(rng):
    op = rng.choice(['neg','not','inc','dec']); d = nrand(rng)
    if op in ('neg','not'):
        mn = {'neg':'neg','not':'mvn'}[op]
        def chk(ins):
            i=expect_single(ins); chk_mnem(i,{mn}); o=parse_ops_arm(i['ops'])
            chk_reg(o[0], d); chk_reg(o[1], d)
        return f"({op} {d})", chk
    mn, one = ('add','inc') if op=='inc' else ('sub','dec')
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{mn}); o=parse_ops_arm(i['ops'])
        chk_reg(o[0], d); chk_reg(o[1], d)
        if imm_val_arm(o[2]) != 1: raise Fail(f"{op} imm {imm_val_arm(o[2])} != 1")
    return f"({op} {d})", chk

def a_jmpr(rng):
    d = nrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'br'}); chk_reg(parse_ops_arm(i['ops'])[0], d)
    return f"(jmpr {d})", chk

def a_callr(rng):
    d = nrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'blr'}); chk_reg(parse_ops_arm(i['ops'])[0], d)
    return f"(callr {d})", chk

ARM_CC = {'eq':'eq','ne':'ne','lt':'lt','le':'le','gt':'gt','ge':'ge','below':'lo',
          'above':'hi','ae':'hs','be':'ls','s':'mi','ns':'pl','vs':'vs','vc':'vc'}
def a_setcc(rng):
    cond = rng.choice(list(ARM_CC)); d = nrand(rng); cc = ARM_CC[cond]
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{'cset'}); o=parse_ops_arm(i['ops'])
        chk_reg(o[0], d)
        if o[1] != cc: raise Fail(f"cset cond {o[1]} != {cc} (holo {cond})")
    return f"(set {cond} {d})", chk

ARM_SX = {'sx1':'sxtb','sx2':'sxth','sx4':'sxtw'}
def a_sx(rng):
    op = rng.choice(list(ARM_SX)); mn = ARM_SX[op]; d = nrand(rng)
    def chk(ins):
        i=expect_single(ins); chk_mnem(i,{mn}); o=parse_ops_arm(i['ops'])
        chk_reg(o[0], d); chk_width(o[0],'x'); chk_reg(o[1], d); chk_width(o[1],'w')
    return f"({op} {d})", chk

GENS_ARM = {
 'mov_rr': a_mov_rr, 'li': a_li, 'alu_rr': a_alu_rr, 'alu_imm': a_alu_imm, 'cmp': a_cmp,
 'ld': a_ld, 'st': a_st, 'ld_sized': a_ld_sized, 'st_sized': a_st_sized,
 'pushpop': a_pushpop, 'shift': a_shift, 'unary': a_unary, 'jmpr': a_jmpr, 'callr': a_callr,
 'setcc': a_setcc, 'sx': a_sx,
}

# ================================================================ riscv lane
# holo target 'riscv64: RV64IMFD, fixed 32-bit insns, three-address, NO FLAGS -- cmp/test/ucomisd
# remember their operands and br/set fuse them, so the flag classes fuzz cmp+CONSUMER pairs and
# check the whole fused shape (inverted hop + jal for br, slt/sltu/feq/flt/fle for set).
# Disassembled by llvm-mc --triple=riscv64. Registers print as ABI names (a0/t0/s2/fa0..).
RV_R2ABI = {'r0':'a0','r1':'a1','r2':'a2','r3':'a3','r4':'a4','r5':'a5','r6':'a6','r7':'a7',
            'r8':'t0','r9':'t1','r10':'t2','r11':'t3','r12':'t4',
            'r13':'s2','r14':'s3','r15':'s1','sp':'sp','fp':'s0','lr':'ra'}
RV_ABI2R = {v: k for k, v in RV_R2ABI.items()}
RV_ABI2R.update({'zero':'zero','t5':'t5','t6':'t6','gp':'gp','tp':'tp'})
for _i in range(4, 12):
    RV_ABI2R[f's{_i}'] = f'r{15+_i}'                     # s4..s11 -> r19..r26 (the bank)
for _i in range(8):
    RV_ABI2R[f'fa{_i}'] = f'f{_i}'                       # fa0..fa7 -> f0..f7
    RV_ABI2R[f'ft{_i}'] = f'f{8+_i}'                     # ft0..ft7 -> f8..f15
for _i in range(8, 12):
    RV_ABI2R[f'ft{_i}'] = f'f{8+_i}'                     # ft8..ft11 -> f16..f19

def rv_name_to_abs(name):
    return RV_ABI2R.get(name.strip())

def disasm_rv(hexstr):
    """riscv64 via llvm-mc; every emitted insn is 4 bytes (no compressed), so
    len(insns) == len(bytes)/4 is the free consumption check (cf. arm64)."""
    spaced = " ".join(f"0x{hexstr[i:i+2]}" for i in range(0, len(hexstr), 2))
    p = subprocess.run(["llvm-mc","--disassemble","--triple=riscv64","--mattr=+m,+f,+d"],
                       input=spaced, capture_output=True, text=True)
    insns = []
    for line in p.stdout.splitlines():
        line = line.split("#")[0].strip()
        if not line or line.startswith('.text'): continue
        if line.startswith(".byte"):
            insns.append({'mnem': '.byte', 'ops': '', 'nbytes': 4, 'bad': True}); continue
        parts = line.split(None, 1)
        insns.append({'mnem': parts[0], 'ops': (parts[1].strip() if len(parts) > 1 else ''),
                      'nbytes': 4, 'bad': False})
    want = len(hexstr) // 8
    if len(insns) != want:
        insns.append({'mnem': f'<{len(insns)}!={want} insns>', 'ops': '', 'nbytes': 4, 'bad': True})
    return insns

def parse_mem_rv(op):
    """'8(a1)' / '-300(t6)' -> dict or None"""
    m = re.fullmatch(r'\s*(-?(?:0x[0-9a-f]+|\d+))?\((\w+)\)\s*', op)
    if not m: return None
    return {'base': m.group(2), 'index': None, 'scale': 1,
            'disp': int(m.group(1), 0) if m.group(1) else 0}

def imm_val_rv(op):
    try: return int(op.strip(), 0)
    except ValueError: return None

RV_GPREGS = list(RV_R2ABI.keys())
RV_FREGS = [f"f{i}" for i in range(16)]
def rv_rand(rng): return rng.choice(RV_GPREGS)
def rv_frand(rng): return rng.choice(RV_FREGS)
def rv_imm12(rng): return rng.randint(-2048, 2047)

def rv_chk_seq(insns, want):
    """want: list of (mnem-set, [operand checks]) where a check is ('r', abs) /
    ('i', val) / ('m', base, disp). Anchors the whole decode."""
    ins = expect_n(insns, len(want))
    for i, (mns, opchecks) in zip(ins, want):
        chk_mnem(i, mns)
        o = parse_ops(i['ops'])
        if len(o) != len(opchecks): raise Fail(f"{i['mnem']}: {len(o)} ops != {len(opchecks)} ({i['ops']})")
        for got, want_c in zip(o, opchecks):
            if want_c[0] == 'r': chk_reg(got, want_c[1])
            elif want_c[0] == 'i':
                if IMM_VAL(got) != want_c[1]: raise Fail(f"imm {got} != {want_c[1]}")
            elif want_c[0] == 'm': chk_mem(got, want_c[1], want_c[2])

def rv_mov_rr(rng):
    d, s = rv_rand(rng), rv_rand(rng)
    def chk(ins): rv_chk_seq(ins, [({'mv'}, [('r',d),('r',s)])])
    return f"(mov {d} {s})", chk

def rv_li(rng):
    d = rv_rand(rng)
    imm = rng.choice([rv_imm12, imm_s32, imm_u32, imm_big,
                      lambda r: -r.randint(1<<32, (1<<62))])(rng)
    def chk(insns):
        if not insns or any(i['bad'] for i in insns): raise Fail("bad li decode")
        val = 0
        for i in insns:
            o = parse_ops(i['ops']); m = i['mnem']
            if reg_abs(o[0]) != d: raise Fail(f"li dest {o[0]} != {d}")
            if m == 'li':      val = IMM_VAL(o[1])
            elif m == 'lui':   v = IMM_VAL(o[1]) << 12; val = v - (1<<32) if v >= (1<<31) else v
            elif m == 'addiw': val = ((val + IMM_VAL(o[2])) & 0xffffffff); val -= (1<<32) if val >= (1<<31) else 0
            elif m == 'addi':  val = val + IMM_VAL(o[2])
            elif m == 'slli':  val = val << IMM_VAL(o[2])
            else: raise Fail(f"li unexpected mnem {m}")
        if (val & MASK) != (imm & MASK): raise Fail(f"li value {hex(val & MASK)} != {hex(imm & MASK)}")
    return f"(li {d} {imm})", chk

RV_ALU = {'add':'add','sub':'sub','and':'and','or':'or','xor':'xor','imul':'mul'}
def rv_alu_rr(rng):
    op = rng.choice(list(RV_ALU)); d,a,b = rv_rand(rng), rv_rand(rng), rv_rand(rng)
    def chk(ins): rv_chk_seq(ins, [({RV_ALU[op]}, [('r',d),('r',a),('r',b)])])
    return f"({op} {d} {a} {b})", chk

def rv_alu_imm(rng):
    # imm 1..2046 avoids the alias minefield (mv at 0, zext.b at and 255, not at xor -1)
    op = rng.choice(['add','sub','and','or','xor']); d,a = rv_rand(rng), rv_rand(rng)
    imm = rng.randint(1, 2046)
    if op == 'and' and imm == 255: imm = 254
    mn = {'add':'addi','sub':'addi','and':'andi','or':'ori','xor':'xori'}[op]
    want = -imm if op == 'sub' else imm
    def chk(ins): rv_chk_seq(ins, [({mn}, [('r',d),('r',a),('i',want)])])
    return f"({op} {d} {a} {imm})", chk

# cond -> the INVERTED hop mnemonic + whether operands swap (taken-form gt/le/above/be swap)
RV_HOP = {'eq':('bne',0),'ne':('beq',0),'lt':('bge',0),'ge':('blt',0),
          'gt':('bge',1),'le':('blt',1),'below':('bgeu',0),'ae':('bltu',0),
          'above':('bgeu',1),'be':('bltu',1)}
def rv_cmpbr(rng):
    cond = rng.choice(list(RV_HOP)); a, b = rv_rand(rng), rv_rand(rng)
    mn, sw = RV_HOP[cond]
    x, y = (b, a) if sw else (a, b)
    def chk(ins): rv_chk_seq(ins, [({mn}, [('r',x),('r',y),('i',8)]),
                                   ({'j'}, [('i',4)])])
    return f"(cmp {a} {b}) (br {cond} l) (label l)", chk

def rv_cmpbr_imm(rng):
    cond = rng.choice(list(RV_HOP)); a = rv_rand(rng); imm = rng.randint(1, 2047)
    mn, sw = RV_HOP[cond]
    x, y = ('t5', a) if sw else (a, 't5')
    def chk(ins): rv_chk_seq(ins, [({'li'}, [('r','t5'),('i',imm)]),
                                   ({mn}, [('r',x),('r',y),('i',8)]),
                                   ({'j'}, [('i',4)])])
    return f"(cmp {a} {imm}) (br {cond} l) (label l)", chk

def rv_setcc(rng):
    cond = rng.choice(['eq','ne','lt','ge','gt','le','below','above','ae','be','s','ns'])
    a, b, d = rv_rand(rng), rv_rand(rng), rv_rand(rng)
    XORI1 = ({'xori'}, [('r',d),('r',d),('i',1)])
    shapes = {
     'eq':    [({'xor'}, [('r','t6'),('r',a),('r',b)]), ({'seqz'}, [('r',d),('r','t6')])],
     'ne':    [({'xor'}, [('r','t6'),('r',a),('r',b)]), ({'snez'}, [('r',d),('r','t6')])],
     'lt':    [({'slt'}, [('r',d),('r',a),('r',b)])],
     'ge':    [({'slt'}, [('r',d),('r',a),('r',b)]), XORI1],
     'gt':    [({'slt'}, [('r',d),('r',b),('r',a)])],
     'le':    [({'slt'}, [('r',d),('r',b),('r',a)]), XORI1],
     'below': [({'sltu'}, [('r',d),('r',a),('r',b)])],
     'above': [({'sltu'}, [('r',d),('r',b),('r',a)])],
     'ae':    [({'sltu'}, [('r',d),('r',a),('r',b)]), XORI1],
     'be':    [({'sltu'}, [('r',d),('r',b),('r',a)]), XORI1],
     's':     [({'sub'}, [('r','t6'),('r',a),('r',b)]), ({'sltz'}, [('r',d),('r','t6')])],
     'ns':    [({'sub'}, [('r','t6'),('r',a),('r',b)]), ({'sltz'}, [('r',d),('r','t6')]), XORI1],
    }
    def chk(ins): rv_chk_seq(ins, shapes[cond])
    return f"(cmp {a} {b}) (set {cond} {d})", chk

RV_LD = {'ld':'ld','ld1':'lb','ld2':'lh','ld4':'lw','ldu1':'lbu','ldu2':'lhu','ldu4':'lwu'}
RV_ST = {'st':'sd','st1':'sb','st2':'sh','st4':'sw'}
def rv_ld(rng):
    op = rng.choice(list(RV_LD)); d, base, off = rv_rand(rng), rv_rand(rng), rv_imm12(rng)
    def chk(ins): rv_chk_seq(ins, [({RV_LD[op]}, [('r',d),('m',base,off)])])
    return f"({op} {d} {base} {off})", chk

def rv_st(rng):
    op = rng.choice(list(RV_ST)); base, off, s = rv_rand(rng), rv_imm12(rng), rv_rand(rng)
    def chk(ins): rv_chk_seq(ins, [({RV_ST[op]}, [('r',s),('m',base,off)])])
    return f"({op} {base} {off} {s})", chk

def rv_ld_far(rng):
    d, base = rv_rand(rng), rv_rand(rng)
    off = rng.choice([1,-1]) * rng.randint(2048, (1<<31) - 4096)
    lo = off & 4095; lo = lo - 4096 if lo > 2047 else lo
    hi = ((off - lo) >> 12) & 0xfffff
    def chk(ins): rv_chk_seq(ins, [({'lui'}, [('r','t6'),('i',hi)]),
                                   ({'add'}, [('r','t6'),('r','t6'),('r',base)]),
                                   ({'ld'}, [('r',d),('m','t6',lo)])])
    return f"(ld {d} {base} {off})", chk

def rv_ldx(rng):
    d, base, ix = rv_rand(rng), rv_rand(rng), rv_rand(rng)
    sc = rng.choice([2,4,8]); dp = rv_imm12(rng)
    amt = {2:1,4:2,8:3}[sc]
    def chk(ins): rv_chk_seq(ins, [({'slli'}, [('r','t6'),('r',ix),('i',amt)]),
                                   ({'add'}, [('r','t6'),('r','t6'),('r',base)]),
                                   ({'ld'}, [('r',d),('m','t6',dp)])])
    return f"(ldx {d} {base} {ix} {sc} {dp})", chk

def rv_stx(rng):
    base, ix, s = rv_rand(rng), rv_rand(rng), rv_rand(rng)
    sc = rng.choice([2,4,8]); dp = rv_imm12(rng)
    amt = {2:1,4:2,8:3}[sc]
    def chk(ins): rv_chk_seq(ins, [({'slli'}, [('r','t6'),('r',ix),('i',amt)]),
                                   ({'add'}, [('r','t6'),('r','t6'),('r',base)]),
                                   ({'sd'}, [('r',s),('m','t6',dp)])])
    return f"(stx {base} {ix} {sc} {dp} {s})", chk

def rv_shift(rng):
    op = rng.choice(['shl','shr','sar']); mn = {'shl':'slli','shr':'srli','sar':'srai'}[op]
    d = rv_rand(rng); c = rng.randint(1, 63)
    def chk(ins): rv_chk_seq(ins, [({mn}, [('r',d),('r',d),('i',c)])])
    return f"({op} {d} {c})", chk

def rv_rot(rng):
    op = rng.choice(['rol','ror']); d = rv_rand(rng); c = rng.randint(1, 63)
    k = c if op == 'ror' else (64 - c) & 63
    def chk(ins):
        rv_chk_seq(ins, [({'srli'}, [('r','t6'),('r',d),('i',k)]),
                         ({'slli'}, [('r',d),('r',d),('i',(64-k)&63)]),
                         ({'or'}, [('r',d),('r',d),('r','t6')])])
    return f"({op} {d} {c})", chk

def rv_shiftv(rng):
    op = rng.choice(['shlv','shrv','sarv']); mn = {'shlv':'sll','shrv':'srl','sarv':'sra'}[op]
    d = rng.choice([r for r in RV_GPREGS if r != 'r1'])
    def chk(ins): rv_chk_seq(ins, [({mn}, [('r',d),('r',d),('r','r1')])])
    return f"({op} {d} r1)", chk

def rv_unary(rng):
    op = rng.choice(['neg','not','inc','dec']); d = rv_rand(rng)
    shapes = {'neg': [({'neg'}, [('r',d),('r',d)])],
              'not': [({'not'}, [('r',d),('r',d)])],
              'inc': [({'addi'}, [('r',d),('r',d),('i',1)])],
              'dec': [({'addi'}, [('r',d),('r',d),('i',-1)])]}
    def chk(ins): rv_chk_seq(ins, shapes[op])
    return f"({op} {d})", chk

RV_SXZX = {'sx1': [({'slli'},56),({'srai'},56)], 'sx2': [({'slli'},48),({'srai'},48)],
           'zx2': [({'slli'},48),({'srli'},48)], 'zx4': [({'slli'},32),({'srli'},32)]}
def rv_sxzx(rng):
    op = rng.choice(list(RV_SXZX)); d = rv_rand(rng)
    def chk(ins): rv_chk_seq(ins, [(mns, [('r',d),('r',d),('i',c)]) for mns,c in RV_SXZX[op]])
    return f"({op} {d})", chk

def rv_divrem(rng):
    op = rng.choice(['div','udiv','rem','urem']); mn = {'div':'div','udiv':'divu','rem':'rem','urem':'remu'}[op]
    d,a,b,t = rv_rand(rng), rv_rand(rng), rv_rand(rng), rv_rand(rng)
    ir = f"({op} {d} {a} {b})" if op in ('div','udiv') else f"({op} {d} {a} {b} {t})"
    def chk(ins): rv_chk_seq(ins, [({mn}, [('r',d),('r',a),('r',b)])])
    return ir, chk

def rv_jmpr(rng):
    d = rv_rand(rng)
    def chk(ins):
        if d == 'lr':                       # jalr x0, 0(ra) prints as the `ret` alias
            i = expect_single(ins); chk_mnem(i, {'ret'})
        else:
            rv_chk_seq(ins, [({'jr'}, [('r',d)])])
    return f"(jmpr {d})", chk

def rv_callr(rng):
    d = rv_rand(rng)
    def chk(ins): rv_chk_seq(ins, [({'jalr'}, [('r',d)])])
    return f"(callr {d})", chk

RV_FP3 = {'addsd':'fadd.d','subsd':'fsub.d','mulsd':'fmul.d','divsd':'fdiv.d'}
def rv_fp3(rng):
    op = rng.choice(list(RV_FP3)); d, s = rv_frand(rng), rv_frand(rng)
    def chk(ins): rv_chk_seq(ins, [({RV_FP3[op]}, [('r',d),('r',d),('r',s)])])
    return f"({op} {d} {s})", chk

def rv_fmov(rng):
    d, s = rv_frand(rng), rv_frand(rng)
    def chk(ins): rv_chk_seq(ins, [({'fmv.d'}, [('r',d),('r',s)])])
    return f"(movsd {d} {s})", chk

def rv_fcvt(rng):
    which = rng.choice(['cvtsi2sd','cvttsd2si','movqxr','movqrx'])
    f, g = rv_frand(rng), rv_rand(rng)
    shapes = {'cvtsi2sd': (f"(cvtsi2sd {f} {g})", [({'fcvt.d.l'}, [('r',f),('r',g)])]),
              'cvttsd2si': (f"(cvttsd2si {g} {f})", [({'fcvt.l.d'}, [('r',g),('r',f),('i',None)])]),
              'movqxr': (f"(movqxr {f} {g})", [({'fmv.d.x'}, [('r',f),('r',g)])]),
              'movqrx': (f"(movqrx {g} {f})", [({'fmv.x.d'}, [('r',g),('r',f)])])}
    ir, want = shapes[which]
    def chk(ins):
        if which == 'cvttsd2si':   # trailing rtz rounding-mode token
            i = expect_single(ins); chk_mnem(i, {'fcvt.l.d'}); o = parse_ops(i['ops'])
            chk_reg(o[0], g); chk_reg(o[1], f)
            if len(o) != 3 or o[2] != 'rtz': raise Fail(f"fcvt.l.d rm {o[2:]} != rtz")
        else:
            rv_chk_seq(ins, want)
    return ir, chk

def rv_fldst(rng):
    which = rng.choice(['ldsd','stsd','ldss','stss'])
    f, base, off = rv_frand(rng), rv_rand(rng), rv_imm12(rng)
    if which == 'ldsd':
        ir, want = f"(ldsd {f} {base} {off})", [({'fld'}, [('r',f),('m',base,off)])]
    elif which == 'stsd':
        ir, want = f"(stsd {base} {off} {f})", [({'fsd'}, [('r',f),('m',base,off)])]
    elif which == 'ldss':
        ir, want = f"(ldss {f} {base} {off})", [({'flw'}, [('r',f),('m',base,off)])]
    else:
        ir, want = f"(stss {base} {off} {f})", [({'fsw'}, [('r',f),('m',base,off)])]
    def chk(ins): rv_chk_seq(ins, want)
    return ir, chk

RV_FHOP = {'above': ('flt.d', 1, 'beqz'), 'be': ('flt.d', 1, 'bnez'),
           'ae': ('fle.d', 1, 'beqz'), 'below': ('fle.d', 1, 'bnez'),
           'eq': ('feq.d', 0, 'beqz'), 'ne': ('feq.d', 0, 'bnez')}
def rv_fcmpbr(rng):
    cond = rng.choice(list(RV_FHOP)); a, b = rv_frand(rng), rv_frand(rng)
    mn, sw, hop = RV_FHOP[cond]
    x, y = (b, a) if sw else (a, b)
    def chk(ins): rv_chk_seq(ins, [({mn}, [('r','t6'),('r',x),('r',y)]),
                                   ({hop}, [('r','t6'),('i',8)]),
                                   ({'j'}, [('i',4)])])
    return f"(ucomisd {a} {b}) (br {cond} l) (label l)", chk

def rv_mulo(rng):
    regs = rng.sample(RV_GPREGS, 4); d, a, b, t = regs
    def chk(ins): rv_chk_seq(ins, [({'mulh'}, [('r',t),('r',a),('r',b)]),
                                   ({'mul'}, [('r',d),('r',a),('r',b)]),
                                   ({'srai'}, [('r','t6'),('r',d),('i',63)]),
                                   ({'beq'}, [('r',t),('r','t6'),('i',8)]),
                                   ({'j'}, [('i',4)])])
    return f"(mulo {d} {a} {b} {t} l) (label l)", chk

def rv_pushpop(rng):
    if rng.random() < 0.5:
        s = rv_rand(rng)
        def chk(ins): rv_chk_seq(ins, [({'addi'}, [('r','sp'),('r','sp'),('i',-16)]),
                                       ({'sd'}, [('r',s),('m','sp',0)])])
        return f"(push {s})", chk
    d = rv_rand(rng)
    def chk(ins): rv_chk_seq(ins, [({'ld'}, [('r',d),('m','sp',0)]),
                                   ({'addi'}, [('r','sp'),('r','sp'),('i',16)])])
    return f"(pop {d})", chk

GENS_RV = {
 'mov_rr': rv_mov_rr, 'li': rv_li, 'alu_rr': rv_alu_rr, 'alu_imm': rv_alu_imm,
 'cmpbr': rv_cmpbr, 'cmpbr_imm': rv_cmpbr_imm, 'setcc': rv_setcc,
 'ld': rv_ld, 'st': rv_st, 'ld_far': rv_ld_far, 'ldx': rv_ldx, 'stx': rv_stx,
 'shift': rv_shift, 'rot': rv_rot, 'shiftv': rv_shiftv, 'unary': rv_unary, 'sxzx': rv_sxzx,
 'divrem': rv_divrem, 'jmpr': rv_jmpr, 'callr': rv_callr, 'pushpop': rv_pushpop,
 'fp3': rv_fp3, 'fmov': rv_fmov, 'fcvt': rv_fcvt, 'fldst': rv_fldst,
 'fcmpbr': rv_fcmpbr, 'mulo': rv_mulo,
}

ARCHES = {
 'x64':  {'target':'x64',   'gens':GENS_X64, 'name_to_abs':name_to_abs,
          'disasm':objdump,  'parse_ops':parse_ops_x64, 'parse_mem':parse_mem_x64, 'llvm':True},
 'arm64':{'target':'arm64', 'gens':GENS_ARM, 'name_to_abs':arm_name_to_abs,
          'disasm':disasm_arm,'parse_ops':parse_ops_arm,'parse_mem':parse_mem_arm,'llvm':False},
 'riscv':{'target':'riscv64','gens':GENS_RV, 'name_to_abs':rv_name_to_abs,
          'disasm':disasm_rv,'parse_ops':parse_ops_x64,'parse_mem':parse_mem_rv,'llvm':False,
          'imm_val':imm_val_rv},
}

# ---------------------------------------------------------------- driver
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("-n", type=int, default=200, help="samples per class")
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--arch", default="x64", choices=list(ARCHES))
    ap.add_argument("--classes", default="all")
    ap.add_argument("--no-llvm", action="store_true")
    ap.add_argument("-v", action="store_true", help="show every failure detail")
    args = ap.parse_args()
    rng = random.Random(args.seed)

    global NAME_TO_ABS, TARGET_SYM, DISASM, PARSE_OPS, PARSE_MEM, IMM_VAL
    A = ARCHES[args.arch]
    NAME_TO_ABS, TARGET_SYM = A['name_to_abs'], A['target']
    DISASM, PARSE_OPS, PARSE_MEM = A['disasm'], A['parse_ops'], A['parse_mem']
    IMM_VAL = A.get('imm_val', imm_val)
    gens = A['gens']; use_llvm2 = A['llvm'] and not args.no_llvm
    classes = list(gens) if args.classes == "all" else args.classes.split(",")

    samples = []  # (tag, ir, chk, cls)
    for cls in classes:
        for k in range(args.n):
            ir, chk = gens[cls](rng)
            samples.append((f"{cls}#{k}", ir, chk, cls))

    hexes = run_holo([(t, ir) for t, ir, _, _ in samples])

    from collections import defaultdict
    npass = defaultdict(int); nfail = defaultdict(int); fails = []
    for tag, ir, chk, cls in samples:
        h = hexes.get(tag)
        if h is None:
            nfail[cls]+=1; fails.append((tag, ir, "?", "holo produced no output")); continue
        try:
            ins = DISASM(h)
            chk(ins)
            if use_llvm2:
                ok, info = llvm_ok(h)
                if not ok: raise Fail(f"llvm-mc reject: {info}")
            npass[cls]+=1
        except Fail as e:
            nfail[cls]+=1; fails.append((tag, ir, h, str(e)))

    print(f"\n=== holo {args.arch} encoder fuzz: seed {args.seed}, {args.n}/class ===")
    tot_p = tot_f = 0
    for cls in classes:
        p, f = npass[cls], nfail[cls]; tot_p+=p; tot_f+=f
        flag = "" if f == 0 else "  <-- FAIL"
        print(f"  {cls:10} {p:5} pass  {f:4} fail{flag}")
    print(f"  {'TOTAL':10} {tot_p:5} pass  {tot_f:4} fail")
    if fails:
        print(f"\n--- {len(fails)} failures (showing {min(len(fails), 40)}) ---")
        for tag, ir, h, msg in fails[:40]:
            print(f"  [{tag}] {ir}\n      hex={h}  :: {msg}")
    return 1 if tot_f else 0

if __name__ == "__main__":
    sys.exit(main())
