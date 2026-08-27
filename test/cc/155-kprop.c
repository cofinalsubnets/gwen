/* constant propagation of write-once locals: each shape that must fold beside
 * the one that must decline -- shadowing, a taken address, unsigned laws, narrow
 * types, faces, and the chain -- summed against the same source under gcc. */
int use(long *p) { return (int)*p; }
long g;

long chain(void) { long a = 8; long b = a; long c = a + b; return c * b; }
long faces(void) { long m = -1; long n = ~0; return m + n; }
unsigned udiv(long x) { unsigned int u = 0xFFFFFFFFu; return (unsigned)(x / (long)u) + (u / 3u) + (u % 7u); }
long ucmp(void)  { unsigned int u = 0xFFFFFFFFu; long s = -1; return (s < (long)u) + ((unsigned long)u >> 3); }
int narrow(void) { char c = (char)200; short h = (short)70000; return c + h; }
long shad(void)  { long d = 1; { long dd; dd = d; d = d; } return d; }
long shad2(void) { long d = 1; if (d) { long e = 2; return d + e; } return 9; }
long taken(void) { long p = 5; use(&p); return p; }
long globshadow(void) { g = 3; long r0 = g; long g2 = 7; return r0 + g2; }
long boolv(void) { _Bool b = 3; return b + 1; }
long shifts(void) { long s = 61; unsigned long x = 0x8000000000000001ul; return (long)(x >> s) + (long)((long)x >> s); }
long szf(void)   { short w = 4; return sizeof(w) + sizeof w + w; }
long wloop(long n) { long w = 8; long t = 0; while (n) { t += w; n--; } return t; }
long forloop(void) { long t = 0; for (long i = 0; i < 5; i++) { long k = 3; t += k * i; } return t; }
long condy(void)  { long k = 0; return k ? 10 : 20; }
long lateasn(void)  { long u; long v = 2; u = v; return u + v; }

int main(void) {
  long acc = 0;
  acc += chain() + faces() + (long)udiv(100) + ucmp() + narrow() + shad() + shad2();
  acc += taken() + globshadow() + boolv() + shifts() + szf() + wloop(7) + forloop();
  acc += condy() + lateasn();
  return (int)(acc & 0x7f);
}
