/* The syntax rungs of doc/moon-c-gaps.md, held to gcc: a brace-less switch body, a
 * declarator list opening with a FUNCTION, an attribute run BEFORE a struct/union
 * tag, _Thread_local, bare typeof. The battery compares EXIT CODES, so a failing
 * check's number comes back. ⚠ _Thread_local is plain static storage here, so this
 * is an oracle only for the single-threaded reading gcc shares.
 */

/* the substatement under the label belongs to the switch: no case, no run */
static int sw(int x)  { int r = 0; switch (x) case 2: r = 5; return r; }
static int swd(int x) { int r = 1; switch (x) default: r = 9; return r; }
static int swb(int x) { int r = 0; switch (x) { case 2: r = 5; } return r; }

/* function FIRST: not mproto's all-prototype shape, so the object lane takes it */
int add(int, int), counter;
static int mul(int a, int b), scale;
int wide(double, short), g2(int), widened;   /* three declarators, two of them functions */

int add(int a, int b) { return a + b; }
static int mul(int a, int b) { return a * b; }
int wide(double d, short s) { return (int)d + s; }
int g2(int x) { return x; }

/* the attribute run before the tag, where only after the body used to parse */
struct __attribute__((packed)) P { char a; int b; };
union  __attribute__((packed)) Q { char a; int b; };
struct __attribute__((packed)) R { char a; int b; } rv;

typeof(counter) same_as_counter;         /* gcc's gnu-mode spelling, C23's own */
_Thread_local int tls  = 7;
__thread      int tls2 = 8;

int main(void)
{
    if (sw(2) != 5)  return 1;
    if (sw(3) != 0)  return 2;           /* the labelled statement stays INSIDE */
    if (swd(0) != 9) return 3;
    if (swb(2) != 5) return 4;
    if (swb(3) != 0) return 5;

    counter = 3; scale = 4;
    if (add(counter, 1) != 4) return 6;
    if (mul(scale, 2) != 8)   return 7;
    /* the sig that survives must carry PARAM TYPES, or the call ABI is wrong */
    widened = wide(2.5, 3);
    if (widened != 5)     return 15;
    if (g2(9) != 9)       return 16;

    if (sizeof(struct P) != 5) return 8;
    if (sizeof(union Q) != 4)  return 9;
    if (sizeof(rv) != 5)       return 10;

    same_as_counter = 5;
    if (same_as_counter != 5)                     return 11;
    if (sizeof(same_as_counter) != sizeof(int))   return 12;

    if (tls + tls2 != 15) return 13;
    tls = 1;
    if (tls + tls2 != 9)  return 14;

    return 0;
}
