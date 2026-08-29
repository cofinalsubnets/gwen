# ccattr.py -- where mooncc's corpus gap against another compiler sits, per symbol.
# usage: python3 ccattr.py sym.A.txt sym.B.txt   (perf report --sort symbol -F sample,symbol
# output, several runs concatenated; A = mooncc's lane, B = the other). see README.md.
import sys, re, collections
def load(p):
    c=collections.Counter()
    for l in open(p):
        m=re.match(r'\s*(\d+)\s+\[\.\]\s+(\S+)', l)
        if m: c[m.group(2)]+=int(m.group(1))
    return c
a=load(sys.argv[1]); b=load(sys.argv[2]); ta=sum(a.values()); tb=sum(b.values())
print('samples A %d B %d ratio %.3f'%(ta,tb,ta/tb))
rows=sorted(((a.get(s,0)-b.get(s,0), s, a.get(s,0), b.get(s,0)) for s in set(a)|set(b)), reverse=True)
print('%-28s %9s %9s %9s %6s'%('symbol','A','B','excess','ratio'))
for r in rows[:25]+[None]+rows[-8:]:
    if r is None: print('...'); continue
    d,s,x,y=r
    print('%-28s %9d %9d %+9d %6s'%(s[:28],x,y,d,('%.2f'%(x/y) if y else '--')))
oa=sum(v for s,v in a.items() if s not in b); ob=sum(v for s,v in b.items() if s not in a)
print('A-only symbols %d (%.1f%%), B-only %d (%.1f%%): inlining differences, the ratios are sound past that'%(oa,100*oa/ta,ob,100*ob/tb))
print('top-10 excess covers %+d of %+d'%(sum(r[0] for r in rows[:10]), ta-tb))
