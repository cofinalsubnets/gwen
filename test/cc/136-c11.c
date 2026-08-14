/* C11's three type-directed operators, held to gcc: _Generic selects on the
 * controlling expression's LVALUE-CONVERTED type (so a string literal picks
 * char*, never char[4]) and evaluates no other arm; _Alignof answers talign,
 * the door playout lays members with; _Alignas is honored at FILE SCOPE.
 *
 * ⚠ the battery compares EXIT CODES, so every check folds into the status:
 * 0 is agreement, and the first failing check's number comes back instead.
 */
#include <stdalign.h>
#include <iso646.h>

alignas(64) char over = 1;
char after = 2;

static int gen_i(int x)      { return _Generic(x, int: 1, long: 2, default: 9); }
static int gen_p(char *x)    { return _Generic(x, char *: 1, char: 2, default: 9); }
static int gen_d(double x)   { return _Generic(x, float: 1, double: 2, default: 9); }
static int gen_no(short x)   { return _Generic(x, int: 1, default: 9); }

/* an unselected arm is not evaluated -- nor even emitted, so a call to an
 * undefined function in one links clean. */
int never_defined(void);
static int gen_lazy(void)    { return _Generic(1, int: 7, default: never_defined()); }

struct pad { char a; double b; };

int main(void)
{
    if (gen_i(0) != 1)  return 1;
    if (gen_p(0) != 1)  return 2;
    if (gen_d(0) != 2)  return 3;
    if (gen_no(0) != 9) return 4;
    if (gen_lazy() != 7) return 5;

    /* the controlling expression decays, and promotion applies to (c + 0) */
    if (_Generic("lit", char *: 1, default: 9) != 1) return 6;
    if (_Generic((char)1 + 0, int: 1, char: 2, default: 9) != 1) return 7;

    if (_Alignof(char) != 1) return 8;
    if (_Alignof(int) != _Alignof(unsigned)) return 9;
    if (_Alignof(double) != 8) return 10;
    if (_Alignof(struct pad) != 8) return 11;
    if (alignof(long) != sizeof(long)) return 12;

    if ((unsigned long)&over % 64u) return 13;
    if (&after == &over) return 14;

    if (not (1 and 2)) return 15;
    if ((5 bitand 3) != 1 or (5 bitor 2) != 7) return 16;

    return 0;
}
