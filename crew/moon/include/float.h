#ifndef _AI_FLOAT_H
#define _AI_FLOAT_H
/* freestanding <float.h> for cc: the IEEE-754 characteristics for this ABI.
 * float = binary32, double = binary64, and long double IS double -- cc gives it
 * 8 bytes and a 53-bit significand, which is what __LDBL_MANT_DIG__ says and what
 * codegen does, so the LDBL_ rungs below restate the DBL ones. an 80-bit x87
 * story here hands out an LDBL_MAX that overflows to infinity. */
#define FLT_RADIX        2
#define FLT_ROUNDS       1     /* round to nearest */
#define FLT_EVAL_METHOD  0     /* evaluate each type at its own width */
#define DECIMAL_DIG      17    /* long double -> decimal round-trip */

#define FLT_MANT_DIG     24
#define DBL_MANT_DIG     53
#define LDBL_MANT_DIG    53

#define FLT_DIG          6
#define DBL_DIG          15
#define LDBL_DIG         15

#define FLT_MIN_EXP      (-125)
#define DBL_MIN_EXP      (-1021)
#define LDBL_MIN_EXP     (-1021)

#define FLT_MAX_EXP      128
#define DBL_MAX_EXP      1024
#define LDBL_MAX_EXP     1024

#define FLT_MIN_10_EXP   (-37)
#define DBL_MIN_10_EXP   (-307)
#define LDBL_MIN_10_EXP  (-307)

#define FLT_MAX_10_EXP   38
#define DBL_MAX_10_EXP   308
#define LDBL_MAX_10_EXP  308

#define FLT_MAX          3.40282346638528859812e+38F
#define DBL_MAX          1.79769313486231570815e+308
#define LDBL_MAX         1.79769313486231570815e+308L

#define FLT_MIN          1.17549435082228750797e-38F
#define DBL_MIN          2.22507385850720138309e-308
#define LDBL_MIN         2.22507385850720138309e-308L

#define FLT_EPSILON      1.19209289550781250000e-7F
#define DBL_EPSILON      2.22044604925031308085e-16
#define LDBL_EPSILON     2.22044604925031308085e-16L
#endif
