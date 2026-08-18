/* #line moves the reported line (C11 6.10.4) -- it had never moved anything, so
 * a generated .c reported positions in the generator's output rather than in the
 * source it came from. Plus the two declarator rows that landed beside it: `[*]`
 * as a prototype's unspecified VLA bound, and a function-typed parameter carrying
 * a non-empty parameter list.
 *
 * ⚠ `#line N` makes the NEXT line N, so the line after it is N and not N+1.
 * ⚠ the file operand is accepted and DROPPED -- __FILE__ stays the TU's name, so
 * nothing here may test it.
 */

/* the operands for the two macro-expanded `#line`s below: a plain object macro, and
   one that only becomes a digit sequence after a second round of expansion */
#define five_hundred 500
#define SIX 600
#define PLUS SIX

/* C11 6.7.6.2p4: a parameter declared [*] decays to a pointer like [] does */
static int sum_star(int n, int v[*]);
static int sum_star(int n, int v[])
{
    int i, t = 0;
    for (i = 0; i < n; i++) t += v[i];
    return t;
}

/* a function-typed parameter with a parameter list -- it adjusts to a pointer */
static int apply(int f(int), int x) { return f(x); }
static int twice(int x) { return x + x; }

int main(void)
{
    int v[3];
    v[0] = 1; v[1] = 2; v[2] = 4;
    if (sum_star(3, v) != 7) return 1;
    if (apply(twice, 21) != 42) return 2;

#line 100
    if (__LINE__ != 100) return 3;
    if (__LINE__ != 101) return 4;

#line 200 "elsewhere.c"
    if (__LINE__ != 200) return 5;

    /* a second one resets outright rather than composing */
#line 300
    if (__LINE__ != 300) return 6;

    /* ..and the DIRECTIVE lane sees it too. ⚠ keep this pair ADJACENT: `#line N`
       makes the very next line N, so the check cannot drift when the file grows. */
#line 400
#if __LINE__ != 400
#error the directive lane must see the mapped line too
#endif

    /* C11 6.10.4p3: an operand that is not already a digit sequence is macro-expanded
       and then has to be one. ⚠ keep each `#line` ADJACENT to its check. */
#line five_hundred
    if (__LINE__ != 500) return 7;

#line PLUS
    if (__LINE__ != 600) return 8;

    return 0;
}
