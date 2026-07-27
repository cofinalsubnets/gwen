#ifndef _AI_MATH_H
#define _AI_MATH_H
double sin(double), cos(double), tan(double);
double asin(double), acos(double), atan(double);
double sinh(double), cosh(double), tanh(double);
double exp(double), log(double), log2(double), log10(double);
double sqrt(double), fabs(double), floor(double), ceil(double);
double atan2(double, double), pow(double, double), fmod(double, double);
double frexp(double, int*), ldexp(double, int);
#define HUGE_VAL 1e999   /* overflows to +inf in the lexer (ieee-inf; fbits images it) */
#endif
