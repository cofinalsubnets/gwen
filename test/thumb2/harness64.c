typedef unsigned long long u64;
typedef long long s64;
extern u64 acc64;
u64 add64(u64,u64); u64 sub64(u64,u64); u64 mul64(u64,u64); u64 div64(u64,u64); u64 rem64(u64,u64);
u64 shl64(u64,int); u64 shr64(u64,int); s64 sar64(s64,int);
u64 shlc(u64); u64 shrc(u64); s64 sarc(s64);
int lt64u(u64,u64); int lt64s(s64,s64); int ge64s(s64,s64); int eq64(u64,u64); int gt64u(u64,u64);
u64 widenu(unsigned); s64 widens(int); unsigned low32(u64); int clz64(u64);
u64 neg64(u64); u64 not64(u64); int nz64(u64); u64 five(u64,int,u64,int,u64); u64 sel(int,u64);
u64 bump(u64); u64 tow(int);
u64 asum(int); int csum(int);
int ltlu(long, unsigned); long divlu(long, unsigned); long remlu(long, unsigned);
static volatile u64 A = 0x123456789abcdef0ULL, B = 0x0fedcba987654321ULL;
static volatile u64 T = 9007199254740993ULL, U = 9007199254740992ULL;  /* 2^53+1 vs 2^53: the tie */
static volatile s64 N = -1234567890123LL;
static volatile int c17 = 17, c47 = 47, c32 = 32;
int run(void){
 int ok = 0;
#define CK(x) do{ ok++; if(!(x)) return 100+ok; }while(0)
 CK(add64(A,B) == A+B);
 ok += add64(0xffffffffULL, 1) == 0x100000000ULL;      /* the carry */
 ok += sub64(B,A) == B-A;                              /* the borrow (wraps) */
 CK(mul64(A,B) == A*B);
 CK(div64(A,7) == A/7);
 CK(div64(A,B) == A/B);
 CK(rem64(A,B) == A%B);
 ok += rem64(A, 0x100000001ULL) == A % 0x100000001ULL; /* divisor crossing 32 bits */
 CK(shl64(A,c17) == A<<c17);
 CK(shl64(A,c47) == A<<c47);
 ok += shl64(A,c32) == A<<c32;                         /* the boundary, variable */
 CK(shr64(A,c17) == A>>c17);
 CK(shr64(A,c47) == A>>c47);
 CK(sar64(N,c17) == N>>c17);
 CK(sar64(N,c47) == N>>c47);
 CK(shlc(A) == ((A<<5)^(A<<32)^(A<<37)));
 CK(shrc(A) == ((A>>5)^(A>>32)^(A>>37)));
 CK(sarc(N) == ((N>>31)^(N>>32)^(N>>47)));
 ok += lt64u(U,T) == 1;                                /* the 2^53 tie, exact */
 CK(lt64u(T,U) == 0);
 ok += lt64s(N,1) == 1;                                /* signed: hi words differ */
 CK(lt64s(1,N) == 0);
 CK(ge64s(N,N) == 1);
 CK(eq64(T,U) == 0);
 CK(eq64(T,T) == 1);
 CK(gt64u(T,U) == 1);
 ok += lt64u(add64(A,0), add64(A,1)) == 1;             /* lo-only difference */
 ok += widenu(0x80000001u) == 0x80000001ULL;           /* zx: no sign smear */
 ok += widens(-5) == -5LL;                             /* sx */
 CK(low32(A) == 0x9abcdef0u);
 CK(clz64(A) == 3);
 CK(clz64(1) == 63);
 CK(neg64(A) == (u64)0-A);
 CK(not64(A) == ~A);
 CK(nz64(0) == 7);
 CK(nz64(A) == 3);
 ok += five(A,3,B,4,T) == A+3+B+4+T;                   /* pair args: regs + 8-aligned stack */
 CK(sel(1,A) == A);
 ok += sel(0,A) == 0;                                  /* the pair/0 ternary arms */
 ok += bump(A) == 5+A+1;                               /* global pair RMW + pair ++ */
 CK(acc64 == 5+A+1);
 ok += tow(33) == ((u64)1<<33);                        /* (dlimb)1 << s, merged */
 CK(tow(5) == (((u64)1<<5)|((u64)1<<33)));
 CK(asum(5) == 1+4+7+10+13);
 CK(csum(4) == 97+98+99+100);
 CK(ltlu(-6, 5u) == (-6L < 5u));                       /* ILP32: unsigned compare (gcc computes the rhs) */
 CK(divlu(-6, 5u) == (long)(-6L / 5u));
 CK(remlu(-6, 5u) == (long)(-6L % 5u));
 return 48;                                            /* 46 checks */
}
