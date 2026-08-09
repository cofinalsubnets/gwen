typedef unsigned long long u64;
typedef long long s64;
u64 acc64 = 5;
u64 add64(u64 a, u64 b){ return a + b; }
u64 sub64(u64 a, u64 b){ return a - b; }
u64 mul64(u64 a, u64 b){ return a * b; }
u64 div64(u64 a, u64 b){ return a / b; }
u64 rem64(u64 a, u64 b){ return a % b; }
u64 shl64(u64 a, int c){ return a << c; }
u64 shr64(u64 a, int c){ return a >> c; }
s64 sar64(s64 a, int c){ return a >> c; }
u64 shlc(u64 a){ return (a << 5) ^ (a << 32) ^ (a << 37); }
u64 shrc(u64 a){ return (a >> 5) ^ (a >> 32) ^ (a >> 37); }
s64 sarc(s64 a){ return (a >> 31) ^ (a >> 32) ^ (a >> 47); }
int lt64u(u64 a, u64 b){ return a < b; }
int lt64s(s64 a, s64 b){ return a < b; }
int ge64s(s64 a, s64 b){ return a >= b; }
int eq64(u64 a, u64 b){ return a == b; }
int gt64u(u64 a, u64 b){ return a > b; }
u64 widenu(unsigned x){ return (u64)x; }
s64 widens(int x){ return (s64)x; }
unsigned low32(u64 a){ return (unsigned)a; }
int clz64(u64 a){ return __builtin_clzll(a); }
u64 neg64(u64 a){ return 0 - a; }
u64 not64(u64 a){ return ~a; }
int nz64(u64 a){ return a ? 3 : 7; }
u64 five(u64 a, int b, u64 c, int d, u64 e){ return a + b + c + d + e; }
u64 sel(int c, u64 a){ return c ? a : 0; }
u64 bump(u64 x){ acc64 += x; acc64++; return acc64; }
u64 tow(int s){ u64 v = (u64)1 << s; return v | ((u64)1 << 33); }
unsigned garr[8];
char cbuf[8];
u64 asum(int n){ int i; u64 s = 0; for (i = 0; i < n; i++) garr[i] = i*3+1; for (i = 0; i < n; i++) s += garr[i]; return s; }
int csum(int n){ int i, s = 0; for (i = 0; i < n; i++) cbuf[i] = 'a'+i; for (i = 0; i < n; i++) s += cbuf[i]; return s; }
/* ILP32: long meets uint at ULONG (C11 6.3.1.8) -- compare and divide go UNSIGNED
 * here where the LP64 targets go signed (long holds every uint there) */
int ltlu(long a, unsigned b){ return a < b; }
long divlu(long a, unsigned b){ return a / b; }
long remlu(long a, unsigned b){ return a % b; }
