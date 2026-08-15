#!/usr/bin/env python3
"""ccdead.py -- how much of each lane's libc the binary can never reach.

ccsize.sh answers how many libc bytes a lane ships. This answers how many
it can call, and the gap between them is what granularity buys: a libc that
arrives as one object arrives whole, where one compiled ~a function (musl)
or ~an area (crew/moon/lib/nolibc/, pulled member by need) sheds what the
link never asks for. Run it on any lane that grows a libc to see whether
its shape still earns its size.

Method: walk call/jmp/lea targets out of `objdump -d`, seeded from _start
and from every function address found in the data sections (vtables and
jump tables are reached no other way). Anything unvisited is unreachable.

The error is one-directional -- an indirect call the scan misses marks a
live function dead, never the reverse -- so the reachable column is a
LOWER bound and the dead column an UPPER one. Read the musl lane as the
control: its ~6.5% is mostly REAL dead code (crt, link-time alternates,
a public entry riding along with the object-mate that is used), not scan
error, so it is the floor a libc built for static linking still cannot
get under. A lane well clear of it is reporting something.

⚠ this reports the LIBC column only, but love's OWN C is worth asking about
too, and the natives are the control that makes the answer readable: mooncc
leaves 6.4% of its own text unreachable against gcc's 2.6% and clang's 1.7%.
The excess is not this scan missing a dispatch table -- all three lanes run
the same tables -- it is that MOONCC INLINES WITHOUT DROPPING THE BODY. Its
own `static ai_inline` copy_data is spliced into every call site and the
out-of-line copy still ships, referenced by nothing. Read the natives' figure
as the floor here exactly as in the libc column.

Usage: ./ccdead.py            (after ./ccbench.sh, or `make ccbench`)
       ./ccdead.py ELF ...    (⚠ no own-object split, so every text symbol is
                               reported as libc -- read with the caveat above)
"""
import collections, os, re, subprocess, sys

R = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
W = os.path.join(R, "out/test/bench/cc")
CANON = re.compile(r"\.(isra|part|constprop|cold|lto_priv|localalias|llvm)[._0-9]*$")
DATA = (".data", ".rodata", ".data.rel.ro", ".init_array")


def run(*a):
    return subprocess.run(a, capture_output=True, text=True).stdout


def textsyms(elf):
    """{name: bytes}, address-gap derived -- mooncc's ELF carries no st_size."""
    rows = []
    for ln in run("nm", "-n", "--defined-only", elf).splitlines():
        p = ln.split()
        if len(p) == 3 and p[1].upper() == "T":
            rows.append((int(p[0], 16), CANON.sub("", CANON.sub("", p[2]))))
    out = collections.Counter()
    for i, (ad, n) in enumerate(rows):
        out[n] += rows[i + 1][0] - ad if i + 1 < len(rows) else 0
    return out, {ad: n for ad, n in rows}


def reachable(elf, byaddr):
    edges, cur = collections.defaultdict(set), None
    hdr = re.compile(r"^[0-9a-f]+ <(.+)>:$")
    tgt = re.compile(r"<([^>+ ]+)(?:\+0x[0-9a-f]+)?>")
    for ln in run("objdump", "-d", elf).splitlines():
        m = hdr.match(ln)
        if m:
            cur = m.group(1)
            edges.setdefault(cur, set())
        elif cur and "\t" in ln:
            edges[cur].update(tgt.findall(ln))
    seed = {n for n in ("_start", "__ai_start", "main") if n in edges}
    for sec in DATA:  # a function pointer in data is a root: nothing calls it by name
        raw = subprocess.run(
            ["objcopy", "-O", "binary", "--only-section", sec, elf, "/dev/stdout"],
            capture_output=True).stdout
        for i in range(0, max(0, len(raw) - 7)):  # unaligned too: a packed table is legal
            v = int.from_bytes(raw[i:i + 8], "little")
            if v in byaddr:
                seed.add(byaddr[v])
    seen, q = set(), list(seed)
    while q:
        n = q.pop()
        if n not in seen:
            seen.add(n)
            q.extend(edges.get(n, ()))
    return seen


def objsyms(*objs):
    names = set()
    for ln in run("nm", "--defined-only", *objs).splitlines():
        p = ln.split()
        if len(p) == 3 and p[1].upper() == "T":
            names.add(CANON.sub("", CANON.sub("", p[2])))
    return names


def ownof(lane):
    """the lane's love C, by ITS OWN objects -- never by what the other binary lacks."""
    if lane == "mooncc":
        d = os.path.join(W, "mooncc")
        if not os.path.isdir(d):
            return None
        skip = re.compile(r"^(love|nolibc|sys|m_[a-z0-9]+)\.o$")
        o = [os.path.join(d, f) for f in sorted(os.listdir(d))
             if f.endswith(".o") and not skip.match(f)]
        o += [os.path.join(d, "love.o"), os.path.join(d, "m_am.o")]
    else:
        d = os.path.join(W, "o-love-" + lane)
        if not os.path.isdir(d):
            return None
        o = [os.path.join(d, "love.o"), os.path.join(d, "am.o")]
        o += [os.path.join(d, "host", f) for f in sorted(os.listdir(os.path.join(d, "host")))]
    o = [p for p in o if os.path.exists(p)]
    return objsyms(*o) if o else None


def main():
    if len(sys.argv) > 1:
        lanes = [(os.path.basename(e), e, None) for e in sys.argv[1:]]
    else:
        lanes = []
        for l in ("mooncc", "gcc-musl", "clang-musl", "gcc", "clang"):
            e = os.path.join(W, "love-" + l)
            if os.path.exists(e):
                lanes.append((l, e, ownof(l)))
        if not lanes:
            sys.exit("ccdead: no lane binaries under %s -- run ./ccbench.sh first" % W)

    print("libc .text, shipped against reachable  (musl is the control: its dead set is the floor)")
    print("%-12s %6s %10s %6s %10s %6s %10s" %
          ("lane", "syms", "shipped", "live", "bytes", "dead", "bytes"))
    for lane, elf, own in lanes:
        sz, byaddr = textsyms(elf)
        live = reachable(elf, byaddr)
        libc = [n for n in sz if own is None or n not in own]
        d = [n for n in libc if n not in live]
        tot, dead = sum(sz[n] for n in libc), sum(sz[n] for n in d)
        print("%-12s %6d %10d %6d %10d %6d %10d  %4.1f%%" %
              (lane, len(libc), tot, len(libc) - len(d), tot - dead, len(d), dead,
               100.0 * dead / tot if tot else 0))
        if os.environ.get("CCDEAD_V"):
            for n in sorted(d, key=lambda x: -sz[x])[:20]:
                print("      %-28s %7d" % (n, sz[n]))


main()
