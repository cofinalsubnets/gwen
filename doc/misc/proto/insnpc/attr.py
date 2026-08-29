# per-symbol guest insn attribution: pc.<world>.out + nm of the matching binary
import sys, subprocess, bisect
def load(binpath, pcout):
    ent_elf = None
    for line in subprocess.run(['readelf','-h',binpath],capture_output=True,text=True).stdout.splitlines():
        if 'Entry point' in line: ent_elf = int(line.split()[-1], 16)
    syms = []
    for line in subprocess.run(['nm','-n',binpath],capture_output=True,text=True).stdout.splitlines():
        p = line.split()
        if len(p) == 3 and p[1] in 'TtWw':
            syms.append((int(p[0],16), p[2]))
    addrs = [a for a,_ in syms]
    counts, bias, total = {}, None, 0
    for line in open(pcout):
        if line.startswith('#'):
            if line.startswith('# entry'):
                bias = int(line.split()[2],16) - ent_elf
            continue
        pc, n, c = line.split()
        pc = int(pc,16) - bias; w = int(c) * int(n); total += w
        i = bisect.bisect_right(addrs, pc) - 1
        name = syms[i][1] if i >= 0 else '?%x' % pc
        counts[name] = counts.get(name,0) + w
    return counts, total
a, ta = load(sys.argv[1], sys.argv[2])
b, tb = load(sys.argv[3], sys.argv[4])
print('total %s %d  %s %d  diff %+d (%+.2f%%)' % (sys.argv[2],ta,sys.argv[4],tb,tb-ta,100*(tb-ta)/ta))
rows = [(b.get(k,0)-a.get(k,0), k, a.get(k,0), b.get(k,0)) for k in set(a)|set(b)]
rows.sort()
print('%-28s %16s %16s %14s' % ('symbol','dance','uni','diff'))
for d,k,va,vb in rows[:25]: print('%-28s %16d %16d %+14d' % (k,va,vb,d))
print('...')
for d,k,va,vb in rows[-25:]: print('%-28s %16d %16d %+14d' % (k,va,vb,d))
