/* v6-M bare floats, gcc side (-O2 -mcpu=cortex-m0): every seam word-for-word
   against gcc's own soft-float base ABI, twins for the arithmetic. */
float fadd(float, float); float fmix(int, float, float, int);
float fchain(float); double f2dbl(float); float dbl2f(double);
int fcmp(float, float); float fovf(float, float, float, float, float);
static float t_fadd(float a, float b){ return a + b; }
static float t_fmix(int i, float a, float b, int j){ return a * (float)i + b - (float)j; }
static float t_fchain(float x){ return t_fadd(t_fadd(x, 1.5f), t_fmix(2, x, 0.25f, 1)); }
static volatile float X = 2.5f, Y = -1.25f;
int run(void){
 int ok = 0;
#define CK(x) do{ ok++; if(!(x)) return 100+ok; }while(0)
 CK(fadd(X, Y) == t_fadd(X, Y));
 CK(fmix(3, X, Y, 2) == t_fmix(3, X, Y, 2));
 CK(fchain(X) == t_fchain(X));
 CK(f2dbl(X) == 5.0);
 CK(dbl2f(2.0) == 2.5f);
 CK(fcmp(Y, X) == 3 && fcmp(X, X) == 5 && fcmp(X, Y) == 7);
 CK(fovf(1.0f, 2.0f, 3.0f, 4.0f, 5.0f) == 1.0f + 4.0f + 12.0f + 32.0f + 80.0f);
 return 7;
}
