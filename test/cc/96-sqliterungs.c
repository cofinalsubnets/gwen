/* the sqlite rungs, gcc-differential and freestanding: sizeof over a GLOBAL
   settles at parse (an array bound), the null-pointer offsetof idiom folds
   ((int)((char*)&((T*)0)->F) -- sqlite carries its own macro), the
   int-to-pointer constant ((void*)&((char*)0)[X]) images -- stride-scaled
   for non-char pointees -- BLOCK-scope typedefs bind and shadow with C
   scope, a string literal initializes a char-array MEMBER without starting
   an elision run, and C's tentative-definition rule (6.9.2) lays ONE object
   however the tentative and initialized forms are ordered. */

static const unsigned char magic[] = { 0xd9, 0xd5, 0x05, 0xf9, 0x20, 0xa1, 0x63, 0xd7 };
struct parse { char zErr[11]; int rc; long mark; double r; };
#define off(T,F) ((int)((char*)&((T*)0)->F))
#define I2P(X) ((void*)&((char*)0)[X])

static void *hint = I2P(42);
static int *ihint = &((int*)0)[3];
static const struct { unsigned char n; char z[7]; double lim; } xform[] = {
  { 6, "second", 4.6427e14 },
  { 6, "minute", 7.7379e12 },
};

int tent;                /* tentative, then initialized: one object, value 5 */
int tent = 5;
int tent2 = 7;           /* initialized, then tentative: still 7 */
int tent2;

int main(void)
{
  char hdr[sizeof(magic) + 4];                 /* 12 -- a global's sizeof as a bound */
  char tail[sizeof(struct parse) - off(struct parse, mark)];   /* 32-16 = 16 */
  int r = 0;

  typedef void (*logfn)(void*, int);           /* a block-scope typedef */
  logfn lf = 0;
  {
    typedef short logfn;                       /* shadowed in an inner block */
    logfn inner = 3;
    r += (int) sizeof(logfn) + inner;          /* 2 + 3 */
  }
  r += lf == 0 ? 1 : 0;                        /* 1 -- the outer typedef restored */
  r += (int) sizeof(logfn);                    /* 8 */

  hdr[0] = 1; tail[0] = 2;
  r += (int) sizeof hdr + hdr[0];              /* 13 */
  r += (int) sizeof tail + tail[0];            /* 18 */
  r += off(struct parse, rc);                  /* 12 */
  r += (int) (long) hint;                      /* 42 */
  r += (int) (long) ihint;                     /* 12 -- int stride scales */
  r += xform[1].z[0];                          /* 'm' = 109 */
  r += (int) (xform[1].lim > 7e12 ? 5 : 0);    /* 5 -- the double member imaged */
  r += tent + tent2;                           /* 12 */

  return r;                                    /* 5+1+8+13+18+12+42+12+109+5+12 = 237 */
}
