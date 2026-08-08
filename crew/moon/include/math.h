#ifndef _AI_MATH_H
#define _AI_MATH_H
double sin(double), cos(double), tan(double);
double asin(double), acos(double), atan(double);
double sinh(double), cosh(double), tanh(double);
double exp(double), log(double), log2(double), log10(double);
double sqrt(double), fabs(double), floor(double), ceil(double);
double atan2(double, double), pow(double, double), fmod(double, double);
double frexp(double, int*), ldexp(double, int);
/* the C99 float twins -- thin narrowings of the doubles above (nolibc.c's faces),
 * declared because ordinary sources reach for them: st asks ceilf for a cell box. */
float sinf(float), cosf(float), tanf(float);
float asinf(float), acosf(float), atanf(float);
float expf(float), logf(float), log2f(float), log10f(float);
float sqrtf(float), fabsf(float), floorf(float), ceilf(float);
float atan2f(float, float), powf(float, float), fmodf(float, float);
#define HUGE_VAL 1e999   /* overflows to +inf in the lexer (ieee-inf; fbits images it) */
#define HUGE_VALF 1e999f
#endif
