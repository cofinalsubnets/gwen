/* v6-M composites, gcc side (-O2 -mcpu=cortex-m0): the base-ABI memory return
   and the position-0 16B quad cross the gcc<->mooncc boundary both ways. */
struct zn { double re, im; };
struct dd { double d; };
struct ii { short a, b; };
struct zn zmake(double, double); int znonpos(struct zn);
double znorm(struct zn); struct zn zscale(double); double zchain(double);
struct dd dmake(double); double dget(struct dd);
struct ii imake(int, int); int isum(struct ii);

static volatile double X = 2.5, Y = -1.25;
int run(void){
 int ok = 0;
 struct zn a, b;
#define CK(x) do{ ok++; if(!(x)) return 100+ok; }while(0)
 a = zmake(X, Y);                       /* memory return crosses mooncc->gcc */
 CK(a.re == X && a.im == Y);
 CK(znonpos(a) == 0);                   /* the 16B quad crosses gcc->mooncc */
 b = zmake(-3.0, 0.0);
 CK(znonpos(b) == 1);
 CK(znorm(a) == X*X + Y*Y);
 b = zscale(X);
 CK(b.re == X*2.0 && b.im == 0.0 - X);
 { double x = 1.5;                      /* zchain vs gcc's own arithmetic */
   struct zn ga; double gb_re, gb_im;
   ga.re = x; ga.im = 2.0*x;
   gb_re = (ga.re + ga.im)*2.0; gb_im = 0.0 - (ga.re + ga.im);
   CK(zchain(x) == ga.re*ga.re + ga.im*ga.im + gb_re - gb_im + 1.0); }
 { struct dd d = dmake(X); CK(d.d == X); CK(dget(d) == X*2.0); }
 { struct ii v = imake(-7, 1000); CK(isum(v) == 993); }
 return 9;
}
