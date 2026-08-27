import re, sys, collections

# ---------- s-expression parsing ----------
def tokenize(s):
    return re.findall(r'\(|\)|[^\s()]+', s)
def parse(toks, i=0):
    out=[]
    while i < len(toks):
        t=toks[i]
        if t=='(':
            sub,i=parse(toks,i+1); out.append(sub)
        elif t==')':
            return out,i+1
        else:
            try: out.append(int(t))
            except ValueError: out.append(t)
            i+=1
    return out,i

def load(path):
    tus={}
    cur=None
    for line in open(path):
        if line.startswith('== '):
            cur=line[3:].strip()
        elif line.startswith('!!'):
            cur=None
        elif cur and line.startswith('(('):
            forms,_=parse(tokenize(line))
            tus[cur]=forms[0]
            cur=None
    return tus

# ---------- split into functions ----------
def split_fns(forms):
    fns=[]; name=None; body=[]
    for f in forms:
        if isinstance(f,list) and f and f[0]=='label' and isinstance(f[1],str) and not f[1].startswith('.'):
            if name: fns.append((name,body))
            name=f[1]; body=[]
        elif isinstance(f,list) and f and f[0]=='align':
            continue
        else:
            if name is not None: body.append(f)
    if name: fns.append((name,body))
    return fns

GPREGS={'r0','r1','r2','r3','r5','r6','r7','r8','r9','r10','r11','r12','r13','r14','r15','r4'}
BASES={'sp','r4'}
LOADS={'ld':8,'ld4':4,'ldu4':4,'ld2':2,'ldu2':2,'ld1':1,'ldu1':1}
SIGNED_LD={'ld4','ld2','ld1'}   # sign-extending narrow loads
STORES={'st':8,'st4':4,'st2':2,'st1':1}
SIS={'si':8,'si4':4,'si2':2,'si1':1}
ALU3={'add','sub','and','or','xor','imul'}
SH2={'shl','shr','sar','ror4'}
EXT={'sx4':(4,True),'zx4':(4,False),'sx2':(2,True),'zx2':(2,False),'sx1':(1,True),'zx1':(1,False)}
TERMINAL={'ret','jmp','jmpr','ud2'}
M=(1<<64)-1
def s64(v): v&=M; return v-(1<<64) if v>=(1<<63) else v

def isreg(x): return isinstance(x,str) and x in GPREGS
def mentions_base(f):
    return any((x in BASES) for x in f[1:] if isinstance(x,str))

def escape_scan(body):
    # spaces whose address leaks: lea/lean/leax on the base, or any unmodeled op naming it
    esc=set()
    for f in body:
        if not isinstance(f,list) or not f: continue
        op=f[0]
        if op in ('lea','lean','leax'):
            for x in f[1:]:
                if isinstance(x,str) and x in BASES: esc.add(x)
        elif op in LOADS or op in STORES or op in SIS or op in ('ldsd','stsd','cmpm','cmprm','ldss','stss'):
            continue
        elif op in ('cvtsi2sd','movqxr','movqrx'):
            continue
        else:
            for x in f[1:]:
                if isinstance(x,str) and x in BASES and not (op in ALU3 and f[1]=='sp'):
                    esc.add(x)
    return esc

def loop_spans(body):
    lab={}
    for i,f in enumerate(body):
        if isinstance(f,list) and f and f[0]=='label': lab[f[1]]=i
    spans=[]
    for i,f in enumerate(body):
        if isinstance(f,list) and f and f[0] in ('jmp','br'):
            tgt=f[1] if f[0]=='jmp' else f[2]
            j=lab.get(tgt,-1)
            if 0<=j<i: spans.append((j,i))
    return spans
def depth_at(spans,i):
    return sum(1 for a,b in spans if a<=i<=b)
def weight(spans,i):
    return 8**min(depth_at(spans,i),3)

# ---------- abstract constant analysis ----------
# state: dict reg->('k',v)|('nac'); slots: dict (space,absoff)->('k',v,width); cursor int; flags
NAC=('nac',)
class St:
    __slots__=('regs','slots','cur','flags','stack')
    def __init__(s): s.regs={}; s.slots={}; s.cur=0; s.flags=None; s.stack=[]
    def copy(s):
        t=St(); t.regs=dict(s.regs); t.slots=dict(s.slots); t.cur=s.cur
        t.flags=s.flags; t.stack=list(s.stack); return t
def join(a,b):
    if a is None: return b.copy()
    if b is None: return a.copy()
    t=St()
    if a.cur==b.cur:
        t.cur=a.cur
        t.slots={k:v for k,v in a.slots.items() if b.slots.get(k)==v}
    else:
        t.cur=a.cur  # cursor disagreement: drop cells
    t.regs={k:v for k,v in a.regs.items() if b.regs.get(k)==v}
    t.flags=a.flags if a.flags==b.flags else None
    t.stack=a.stack if a.stack==b.stack else []
    return t
def steq(a,b):
    return a.regs==b.regs and a.slots==b.slots and a.cur==b.cur and a.flags==b.flags and a.stack==b.stack

def kv(st,x):
    if isinstance(x,int): return x
    if isreg(x):
        v=st.regs.get(x)
        if v and v[0]=='k': return v[1]
    return None

def analyze_const(name,body,esc,linear=False,kcap=None):
    """returns list of findings (idx, kind, detail)"""
    spans=loop_spans(body)
    lab={f[1]:i for i,f in enumerate(body) if isinstance(f,list) and f and f[0]=='label'}
    backlabels={body[a][1] for a,b in spans}
    findings=[]
    # worklist over form indices; state per label
    instate={0:St()}
    # for linear mode: single pass in order with pend joins, back-edge labels reset
    pend={}
    def cap_ok(v):
        return True if kcap is None else (0<=v<kcap)
    def setreg(st,r,v):
        if v is None or not cap_ok(v): st.regs[r]=NAC
        else: st.regs[r]=('k',s64(v))
    order=list(range(len(body)))
    # iterate to fixpoint (or once for linear)
    labin={}   # label idx -> state
    changed=True
    passes=0
    maxpass=1 if linear else 40
    reported=set()
    final=linear            # linear mode records on its single pass
    while (changed and passes<maxpass) or (not linear and final and passes<maxpass+1):
        if not changed: pass
        changed=False; passes+=1
        st=St()
        live=True
        for i in order:
            f=body[i]
            if not (isinstance(f,list) and f):
                continue
            op=f[0]
            if op=='label':
                l=f[1]
                if linear:
                    if l in backlabels:
                        st=St(); live=True
                    else:
                        pd=pend.get(l)
                        if not live: st=pd.copy() if pd else St()
                        elif pd is not None: st=join(st,pd)
                        live=True
                else:
                    prev=labin.get(i)
                    inc=pend.get(l)
                    if live and inc is not None: cand=join(st,inc)
                    elif live: cand=st
                    elif inc is not None: cand=inc.copy()
                    else: cand=St()
                    if prev is None or not steq(join(prev,cand),prev):
                        labin[i]=join(prev,cand) if prev is not None else cand.copy()
                        changed=True
                    st=labin[i].copy(); live=True
                continue
            if not live: continue
            def record(kind,detail=''):
                if not final: return
                key=(i,kind)
                if key not in reported:
                    reported.add(key)
                    findings.append((i,kind,detail,weight(spans,i)))
            # transfer
            if op=='li':
                setreg(st,f[1],f[2] if isinstance(f[2],int) else None)
            elif op=='mov':
                d,s0=f[1],f[2]
                v=kv(st,s0)
                if v is not None: record('mov-of-known',f'{d}<-{v}')
                st.regs[d]=st.regs.get(s0,NAC) if isreg(s0) else NAC
            elif op in LOADS:
                d,b,o=f[1],f[2],f[3]
                if isinstance(b,str) and b in BASES and isinstance(o,int):
                    slot=st.slots.get((b,st.cur+o))
                    if slot and slot[0]=='k' and slot[2]==LOADS[op]:
                        v=slot[1]
                        w=LOADS[op]
                        if w<8:
                            v&=(1<<(8*w))-1
                            if op in SIGNED_LD and v>=(1<<(8*w-1)): v-=(1<<(8*w))
                        record('const-reload',f'{d}<-[{b}+{o}]={v}')
                        setreg(st,d,v)
                    else:
                        st.regs[d]=NAC
                else:
                    st.regs[d]=NAC
            elif op in STORES:
                b,o,r=f[1],f[2],f[3]
                if not (isinstance(b,str) and b in BASES):
                    if esc: st.slots={k:v for k,v in st.slots.items() if k[0] not in esc}
                elif isinstance(o,int):
                    v=st.regs.get(r) if isreg(r) else None
                    key=(b,st.cur+o)
                    # clobber overlapping keys
                    for k in [k for k in st.slots if k[0]==b and abs(k[1]-key[1])<8]:
                        del st.slots[k]
                    if v and v[0]=='k' and cap_ok(v[1]):
                        st.slots[key]=('k',v[1],STORES[op])
            elif op in SIS:
                b,o,k0=f[1],f[2],f[3]
                if isinstance(b,str) and b in BASES and isinstance(o,int) and isinstance(k0,int):
                    key=(b,st.cur+o)
                    for k in [k for k in st.slots if k[0]==b and abs(k[1]-key[1])<8]:
                        del st.slots[k]
                    if cap_ok(k0): st.slots[key]=('k',s64(k0),SIS[op])
            elif op in ALU3:
                d,a,b2=f[1],f[2],f[3]
                if d=='sp':
                    if a=='sp' and isinstance(b2,int):
                        st.cur += (b2 if op=='add' else -b2)
                    else:
                        st.slots={k:v for k,v in st.slots.items() if k[0]!='sp'}
                    continue
                va,vb=kv(st,a),kv(st,b2)
                if va is not None and vb is not None:
                    r={'add':va+vb,'sub':va-vb,'and':va&vb,'or':va|vb,'xor':va^vb,'imul':va*vb}[op]
                    record('foldable-alu',f'{op} {va} {vb}')
                    setreg(st,d,r)
                else:
                    st.regs[d]=NAC
            elif op in SH2:
                d=f[1]; amt=f[2] if len(f)>2 else None
                v=kv(st,d); k=amt if isinstance(amt,int) else None
                if v is not None and k is not None:
                    if op=='shl': r=v<<k
                    elif op=='shr': r=(v&M)>>k
                    elif op=='sar': r=v>>k
                    else: r=((v&0xffffffff)>>k)|((v<<(32-k))&0xffffffff)
                    record('foldable-alu',f'{op} {v} {k}')
                    setreg(st,d,r)
                else: st.regs[d]=NAC
            elif op in EXT:
                d=f[1]; w,sg=EXT[op]; v=kv(st,d)
                if v is not None:
                    v&=(1<<(8*w))-1
                    if sg and v>=(1<<(8*w-1)): v-=(1<<(8*w))
                    setreg(st,d,v)
                else: st.regs[d]=NAC
            elif op in ('not','neg'):
                d=f[1]; v=kv(st,d)
                setreg(st,d,(~v if op=='not' else -v) if v is not None else None)
            elif op in ('cmp','test'):
                a,b2=f[1],f[2]
                va,vb=kv(st,a),kv(st,b2)
                st.flags=(op,va,vb) if (va is not None and vb is not None) else None
            elif op in ('cmpm','cmprm'):
                st.flags=None
            elif op=='set':
                st.regs[f[-1] if isreg(f[-1]) else f[1]]=NAC
            elif op=='br':
                cc,l=f[1],f[2]
                if st.flags and st.flags[1] is not None:
                    record('decidable-branch',f'{cc} {st.flags[1]} {st.flags[2]}')
                pd=pend.get(l)
                pend[l]=join(pd,st) if pd is not None else st.copy()
                if not linear and pd is not None and not steq(pend[l],pd): changed=True
                if linear and l in backlabels: pass
            elif op=='jmp':
                l=f[1]
                pd=pend.get(l)
                pend[l]=join(pd,st) if pd is not None else st.copy()
                if not linear and pd is not None and not steq(pend[l],pd): changed=True
                live=False
            elif op in ('call','callr'):
                st.regs={}
                st.flags=None
                st.stack=[]
                if esc: st.slots={}
            elif op=='push':
                v=st.regs.get(f[1],NAC) if isreg(f[1]) else NAC
                st.stack.append(v); st.cur-=8
            elif op=='pop':
                st.cur+=8
                v=st.stack.pop() if st.stack else NAC
                if isreg(f[1]): st.regs[f[1]]=v
            elif op in ('ret','jmpr','ud2'):
                live=False
            elif op in ('sys','raw'):
                st.regs={}; st.flags=None; st.stack=[]
                if esc or op=='raw': st.slots={}
            elif op=='la':
                st.regs[f[1]]=NAC
            elif op in ('ldsd','stsd','ldss','stss','movsd','addsd','subsd','mulsd','divsd','ucomisd','cvtsi2sd','sqrtsd','xorps','minsd','maxsd'):
                if op=='stsd' and isinstance(f[1],str) and f[1] in BASES and isinstance(f[2],int):
                    key=(f[1],st.cur+f[2])
                    for k in [k for k in st.slots if k[0]==f[1] and abs(k[1]-key[1])<8]:
                        del st.slots[k]
                st.flags=None if op=='ucomisd' else st.flags
            elif op in ('movqxr',):
                pass
            elif op in ('movqrx','cvttsd2si'):
                if isreg(f[1]): st.regs[f[1]]=NAC
            else:
                # unknown: invalidate everything it names; if it names a base, drop that space;
                # a memory-writing op (stx and kin) drops every escaped space's cells
                for x in f[1:]:
                    if isreg(x): st.regs[x]=NAC
                if mentions_base(f):
                    st.slots={}
                elif op in ('stx','stxb') or op.startswith('st'):
                    if esc: st.slots={k:v for k,v in st.slots.items() if k[0] not in esc}
                st.flags=None
                if op in ('div','udiv','urem'):
                    for r in ('r0','r2'): st.regs[r]=NAC
        if linear: break
        if not changed and not final:
            final=True; changed=True   # one more pass, recording, states already converged
        elif final and not changed:
            break
    return findings

# ---------- dead store analysis (backward, byte-granular) ----------
def analyze_dead(name,body,esc):
    spans=loop_spans(body)
    lab={f[1]:i for i,f in enumerate(body) if isinstance(f,list) and f and f[0]=='label'}
    n=len(body)
    succ=[[] for _ in range(n)]
    for i,f in enumerate(body):
        if not (isinstance(f,list) and f): 
            if i+1<n: succ[i].append(i+1)
            continue
        op=f[0]
        if op=='jmp': succ[i]=[lab[f[1]]] if f[1] in lab else []
        elif op=='br':
            succ[i]=([lab[f[2]]] if f[2] in lab else [])+([i+1] if i+1<n else [])
        elif op in ('ret','jmpr','ud2'): succ[i]=[]
        else:
            if i+1<n: succ[i].append(i+1)
    # forward sp-cursor per index (needed for absolute offsets)
    cur=[None]*n
    work=[(0,0)]
    while work:
        i,c=work.pop()
        if i>=n or cur[i] is not None: continue
        cur[i]=c
        f=body[i]; c2=c
        if isinstance(f,list) and f:
            if f[0] in ALU3 and f[1]=='sp' and f[2]=='sp' and isinstance(f[3],int):
                c2=c+(f[3] if f[0]=='add' else -f[3])
            elif f[0]=='push': c2=c-8
            elif f[0]=='pop': c2=c+8
        for j in succ[i]: work.append((j,c2))
    labset=set(lab)
    # backward liveness of (space,byte)
    livein=[None]*n
    def reads_writes(i):
        f=body[i]; rd=set(); wr=set(); allr=False
        if not (isinstance(f,list) and f): return rd,wr,allr
        op=f[0]; c=cur[i] or 0
        if op in LOADS and isinstance(f[2],str) and f[2] in BASES and isinstance(f[3],int):
            rd={(f[2],c+f[3]+k) for k in range(LOADS[op])}
        elif op in ('ldsd','ldss') and isinstance(f[2],str) and f[2] in BASES and isinstance(f[3],int):
            rd={(f[2],c+f[3]+k) for k in range(8)}
        elif op in STORES and isinstance(f[1],str) and f[1] in BASES and isinstance(f[2],int):
            wr={(f[1],c+f[2]+k) for k in range(STORES[op])}
        elif op in SIS and isinstance(f[1],str) and f[1] in BASES and isinstance(f[2],int):
            wr={(f[1],c+f[2]+k) for k in range(SIS[op])}
        elif op in ('stsd','stss') and isinstance(f[1],str) and f[1] in BASES and isinstance(f[2],int):
            wr={(f[1],c+f[2]+k) for k in range(8)}
        elif op in ('cmpm','cmprm'):
            allr=True
        elif op in ('call','callr','sys','raw','jmpr'):
            allr=True
        elif op=='jmp':
            allr = f[1] not in labset       # a tail jump out of the fn hands the frame on
        elif op in ('ret','br','label','li','mov','cmp','test','push','pop','set','la','ud2') or op in ALU3 or op in SH2 or op in EXT or op in ('not','neg'):
            pass
        else:
            allr=True   # unknown op: reads everything
        return rd,wr,allr
    ALL=('ALL',)
    changed=True
    while changed:
        changed=False
        for i in range(n-1,-1,-1):
            f=body[i]
            out=set()
            isterm = isinstance(f,list) and f and f[0] in ('ret','jmpr','sys','ud2')
            if isterm:
                out = {ALL} if esc else set()
            for j in succ[i]:
                s=livein[j]
                if s is None: continue
                if ALL in s or ALL in out: out={ALL}
                else: out|=s
            rd,wr,allr=reads_writes(i)
            if allr or ALL in out:
                newin={ALL}
            else:
                newin=(out-wr)|rd
            if livein[i] is None or newin!=livein[i]:
                livein[i]=newin; changed=True
    dead=[]
    for i in range(n):
        f=body[i]
        if not (isinstance(f,list) and f): continue
        op=f[0]
        rd,wr,_=reads_writes(i)
        if not wr: continue
        out=set()
        for j in succ[i]:
            s=livein[j] or set()
            if ALL in s: out={ALL}; break
            out|=s
        if ALL in out: continue
        if not (wr & out):
            near_call = any(isinstance(body[k],list) and body[k] and body[k][0] in ('call','callr')
                            for k in range(i+1,min(i+6,n)))
            dead.append((i,op,f,weight(spans,i),near_call))
    return dead

# ---------- mem2reg census ----------
def analyze_promo(name,body,esc):
    spans=loop_spans(body)
    touches=collections.defaultdict(list)  # (space,off) -> [(idx,op)]
    bad=set()
    cur=0
    for i,f in enumerate(body):
        if not (isinstance(f,list) and f): continue
        op=f[0]
        if op in ALU3 and f[1]=='sp' and f[2]=='sp' and isinstance(f[3],int):
            cur += (f[3] if op=='add' else -f[3]); continue
        if op=='push': cur-=8; continue
        if op=='pop': cur+=8; continue
        if op in LOADS and isinstance(f[2],str) and f[2] in BASES and isinstance(f[3],int):
            touches[(f[2],cur+f[3],LOADS[op])].append((i,op))
        elif op in STORES and isinstance(f[1],str) and f[1] in BASES and isinstance(f[2],int):
            touches[(f[1],cur+f[2],STORES[op])].append((i,op))
        elif op in SIS and isinstance(f[1],str) and f[1] in BASES and isinstance(f[2],int):
            touches[(f[1],cur+f[2],SIS[op])].append((i,op))
        elif op in ('ldsd','stsd','ldss','stss'):
            if isinstance(f[2] if op[0]=='l' else f[1],str):
                b=f[2] if op[0]=='l' else f[1]
                o=f[3] if op[0]=='l' else f[2]
                if b in BASES and isinstance(o,int): bad.add((b,cur+o))
        elif mentions_base(f) and op not in ('label','jmp','br','ret','call','li','mov','cmp','test','la'):
            for x in f[1:]:
                if isinstance(x,str) and x in BASES:
                    return []   # unmodeled base use: skip fn conservatively
    # group by (space, off8) 8-aligned cell
    cells=collections.defaultdict(list)
    for (sp0,off,w),ts in touches.items():
        cells[(sp0,off- (off%8) if off>=0 else off-(off%8))].append((off,w,ts))
    callidx=[i for i,f in enumerate(body) if isinstance(f,list) and f and f[0] in ('call','callr','sys')]
    out=[]
    for (sp0,base),lst in cells.items():
        if sp0 in esc: continue
        if any((sp0,o) in bad for o,w,ts in lst): continue
        allts=sorted(t for o,w,ts in lst for t,_ in ts)
        widths={w for o,w,ts in lst}
        offs={o for o,w,ts in lst}
        sifed=any(op in SIS for o,w,ts in lst for _,op in ts)
        fullword = widths=={8} and len(offs)==1 and not sifed
        narrow = len(offs)==1 and len(widths)>=1 and not fullword and max(widths)<=8 and len(widths)==1
        multi = len(offs)>1
        lo,hi=allts[0],allts[-1]
        across=any(lo<c<hi for c in callidx)
        wsum=sum(weight(spans,i) for o,w,ts in lst for i,_ in ts)
        out.append((base,fullword,across,sifed,narrow,multi,len(allts),wsum))
    return out

def main():
    tus=load(sys.argv[1])
    G=collections.Counter(); GW=collections.Counter()
    L=collections.Counter(); LW=collections.Counter()
    LN=collections.Counter(); LNW=collections.Counter()
    dead_n=0; dead_w=0; dead_wrap=0
    promo_full=0; promo_mixed=0; promo_full_w=0; promo_mixed_w=0; promo_touch=0; promo_spill=0; promo_spill_w=0; promo_si=0; promo_nar=0; promo_nar_w=0; promo_multi=0; promo_multi_w=0
    top=collections.Counter()
    pertu=collections.defaultdict(collections.Counter)
    nfns=0; totforms=0; totld=0; totalu=0
    for tu,forms in tus.items():
        for name,body in split_fns(forms):
            nfns+=1
            totforms+=len(body)
            totld+=sum(1 for f in body if isinstance(f,list) and f and f[0] in LOADS)
            totalu+=sum(1 for f in body if isinstance(f,list) and f and (f[0] in ALU3 or f[0] in SH2))
            esc=escape_scan(body)
            g=analyze_const(name,body,esc,linear=False)
            l=analyze_const(name,body,esc,linear=True,kcap=1<<30)
            lw=analyze_const(name,body,esc,linear=True)
            for i,kind,detail,w in g:
                G[kind]+=1; GW[kind]+=w; top[(tu,name,kind)]+=1
                pertu[tu][kind]+=1
            for i,kind,detail,w in l:
                L[kind]+=1; LW[kind]+=w
            for i,kind,detail,w in lw:
                LN[kind]+=1; LNW[kind]+=w
            for i,op,f,w,near in analyze_dead(name,body,esc):
                dead_n+=1; dead_w+=w
                if near: dead_wrap+=1
                pertu[tu]['dead-store']+=1
                top[(tu,name,'dead-store')]+=1
            for base,fullword,across,sifed,narrow,multi,nt,wsum in analyze_promo(name,body,esc):
                promo_touch+=nt
                if across: promo_spill+=1; promo_spill_w+=wsum
                elif fullword: promo_full+=1; promo_full_w+=wsum
                elif narrow or sifed and not multi: promo_nar+=1; promo_nar_w+=wsum
                elif multi: promo_multi+=1; promo_multi_w+=wsum
                else:
                    promo_mixed+=1; promo_mixed_w+=wsum
    print(f"functions analyzed: {nfns}  (TUs: {len(tus)}); total forms {totforms}, of them {totld} loads, {totalu} alu")
    print("\n== GLOBAL (SSA-grade) residual facts in FINAL forms ==")
    print(f"{'category':24s} {'static':>8s} {'loop-wt':>10s}")
    for k in sorted(G): print(f"{k:24s} {G[k]:8d} {GW[k]:10d}")
    print(f"{'dead-store':24s} {dead_n:8d} {dead_w:10d}   ({dead_wrap} adjacent to a call = wrap class)")
    print("\n== of which visible LINEAR in cfoldir's own domain (kcap 2^30, back-edges reset) — recognizer gaps ==")
    for k in sorted(L): print(f"{k:24s} {L[k]:8d} {LW[k]:10d}")
    print("\n== visible LINEAR with 64-bit constants (still no fixpoint) — + the kmax gap ==")
    for k in sorted(LN): print(f"{k:24s} {LN[k]:8d} {LNW[k]:10d}")
    print("\n== mem2reg census: un-promoted direct-touch frame cells (non-escaped fns) ==")
    print(f"live across a call (spill class, not headroom): {promo_spill:6d} cells, loop-wt touches {promo_spill_w}")
    print(f"full-word single-cell, call-free span         : {promo_full:6d} cells, loop-wt touches {promo_full_w}")
    print(f"narrow/si single-offset, call-free span       : {promo_nar:6d} cells, loop-wt touches {promo_nar_w}   (SSA-promotable with width handling)")
    print(f"multi-offset (array/struct piece), call-free  : {promo_multi:6d} cells, loop-wt touches {promo_multi_w}   (SROA territory, not plain SSA)")
    print(f"other mixed, call-free span                   : {promo_mixed:6d} cells, loop-wt touches {promo_mixed_w}")
    print("\n== top sites (global facts) ==")
    for (tu,name,kind),n in top.most_common(25):
        print(f"{n:5d}  {kind:18s} {name}  ({tu})")
    print("\n== per-TU (global facts) ==")
    for tu in sorted(pertu, key=lambda t:-sum(pertu[t].values()))[:15]:
        c=pertu[tu]
        print(f"{sum(c.values()):6d}  {tu}  {dict(c)}")

main()
