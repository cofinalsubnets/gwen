/* v6-M bare floats, mooncc side: ai_flo_t IS float on a 32-bit love, so bare
   float args/params/returns are EVERYWHERE -- one WORD each (binary32, the
   gcc base-ABI shape; the widened-pair mismatch here was the bug that kept
   the egg from hatching). */
float fadd(float a, float b){ return a + b; }
float fmix(int i, float a, float b, int j){ return a * (float)i + b - (float)j; }
float fchain(float x){ return fadd(fadd(x, 1.5f), fmix(2, x, 0.25f, 1)); }
double f2dbl(float x){ return (double)x * 2.0; }
float dbl2f(double d){ return (float)(d + 0.5); }
int fcmp(float a, float b){ return a < b ? 3 : a == b ? 5 : 7; }
float fovf(float a, float b, float c, float d, float e){ return a + b*2.0f + c*4.0f + d*8.0f + e*16.0f; }
