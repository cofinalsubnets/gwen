/* v6-M composites, mooncc side: the MEMORY-return lane (>4B composite fills
   the caller temp whose address rides the implicit first arg -- gcc's exact
   base-ABI shape) and the position-0 16B r0-r3 quad (love.c's zn shapes:
   zn()/ai_net() return by value, zn_nonpos() takes one by value). dd is the
   8B blob (an even-odd gp pair in, MEMORY out); ii the <=4B int one (r0). */
struct zn { double re, im; };
struct dd { double d; };
struct ii { short a, b; };

struct zn zmake(double re, double im) { struct zn z; z.re = re; z.im = im; return z; }
int znonpos(struct zn z) { return z.im == 0.0 && z.re <= 0.0; }
double znorm(struct zn a) { return a.re*a.re + a.im*a.im; }
struct zn zscale(double s) { return zmake(s * 2.0, 0.0 - s); }   /* sret chained through sret */
double zchain(double x) {                 /* all-internal: result member reads off sret temps */
  struct zn a = zmake(x, 2.0*x);
  struct zn b = zscale(a.re + a.im);
  return znorm(a) + b.re - b.im + (double)znonpos(zmake(-1.0, 0.0)); }

struct dd dmake(double d) { struct dd r; r.d = d; return r; }
double dget(struct dd v) { return v.d * 2.0; }
struct ii imake(int a, int b) { struct ii r; r.a = (short)a; r.b = (short)b; return r; }
int isum(struct ii v) { return v.a + v.b; }
