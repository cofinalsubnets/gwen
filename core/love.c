#include "love.h"
// the build's version string, generated into out/lib/love_version.h and surfaced
// as `love-version`. -DAiVersion wins (love0 pins "bootstrap" so a new commit never
// relinks the bootstrap); -DAiHaveVersionH says the header exists -- mooncc has
// no __has_include, so the probe alone is not enough.
#ifndef AiVersion
# if defined(AiHaveVersionH) || (defined(__has_include) && __has_include("love_version.h"))
#  include "love_version.h"
# endif
#endif
#ifndef AiVersion
#define AiVersion "unknown"
#endif

// --- kernel-internal declarations ---

// the math floor is ours on every frontend: crew/moon/lib/math/am.c (fdlibm and
// -lm both retired); the 32-bit lane computes in binary64 and narrows.
double am_sin(double), am_cos(double), am_atan2(double, double),
       am_sqrt(double), am_exp(double), am_log(double), am_pow(double, double),
       am_strtod(char const*, char**);   // correctly rounded read: the printer's twin
#if UINTPTR_MAX == UINT64_MAX
#define Bits 64
typedef double ai_flo_t;
#define ai_sin   am_sin
#define ai_cos   am_cos
#define ai_atan2 am_atan2
#define ai_sqrt  am_sqrt
#define ai_exp   am_exp
#define ai_log   am_log
#define ai_pow   am_pow
#elif UINTPTR_MAX == UINT32_MAX
#define Bits 32
typedef float ai_flo_t;
float am_sinf(float), am_cosf(float), am_atan2f(float, float), am_sqrtf(float),
      am_expf(float), am_logf(float), am_powf(float, float);
#define ai_sin   am_sinf
#define ai_cos   am_cosf
#define ai_atan2 am_atan2f
#define ai_sqrt  am_sqrtf
#define ai_exp   am_expf
#define ai_log   am_logf
#define ai_pow   am_powf
#endif

// bignum limbs: native-width wherever a double-width product type exists (64-bit
// limbs do a quarter the limb-ops); the 32-bit wasm shim (no __int128) keeps 32-bit
// limbs with a u64 accumulator. ai_dlimb/ai_sdlimb are the double-limbs.
#if UINTPTR_MAX == UINT64_MAX && defined(__SIZEOF_INT128__)
typedef uint64_t ai_limb;
typedef unsigned __int128 ai_dlimb;
typedef __int128 ai_sdlimb;
#define limb_bits 64
#else
typedef uint32_t ai_limb;
typedef uint64_t ai_dlimb;
typedef int64_t ai_sdlimb;
#define limb_bits 32
#endif
#define limb_clz(x) (__builtin_clzll((unsigned long long) (x)) - (8 * (int) sizeof(unsigned long long) - limb_bits))  // leading zeros of a nonzero limb, at limb width
#define wlimbs (Bits / limb_bits)   // limbs to hold one machine word: 1 (native-width limbs) or 2 (32-bit limbs on a 64-bit word)
// decimal digits a limb spans: floor(limb_bits * log10 2), 30103 = round(1e5 log10 2).
// the reader packs chunk digits per mul-add pass.
#define limb_dec_chunk  (limb_bits * 30103 / 100000)
// the binary-radix twins: the most digits whose product still fits a limb
#define limb_hex_chunk  ((limb_bits - 1) / 4)
#define limb_oct_chunk  ((limb_bits - 1) / 3)

#define Bytes (Bits>>3)
_Static_assert(Bytes == sizeof(uintptr_t), "word size sanity check");

#include <stdarg.h>
_Static_assert(sizeof(union u) == sizeof(intptr_t), "cell size equals word size");

// remembered-set capacity in words; g->alloc'd, so the collector stays freestanding
#define AiRemCap (1u << 16)
// initial pool sizes, words per half; both grow on demand (a tiny device overrides
// with -Dai_minor0/-Dai_major0 and accepts more collections)
#ifndef ai_minor0
# define ai_minor0 (1u << 14)   // ~128 KB minor (the main pool); the dev host boots fast
#endif
#ifndef ai_major0
# define ai_major0 (1u << 16)      // ~512 KB major-pool half; grows much less often than the minor pool
#endif
// the nursery's copy-overhead setpoint (gen_please): resize to keep copied/allocated
// inside [1/(4*ratio), 1/ratio]. larger ratio = lower overhead, more RAM.
#ifndef ai_gc_ratio
# define ai_gc_ratio 24   // the knee of the GC-reduction curve; past it RAM doubles for a flat curve
#endif
// total memory budget in words (2*minor + 2*major); 0 = unbounded. a device sets its
// RAM (-Dai_budget=131072 for a 1 MB Teensy); the nursery then sizes by appel's rule (gen_please).
#ifndef ai_budget
# define ai_budget 0
#endif
_Static_assert(-1 >> 1 == -1, "sign extended shift");
// structural test for the charm ZERO -- an identity, not a measure. ⚠ distinct
// from ai_nilp, the language's falsy predicate (all-zero tray, unit, red net).
#define zerop(_) (word(_)==zero)
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define ptr(_) ((word*)(_))
#define datp(_) in_data(cell(_)->ap)
#define avec(g, y, ...) (mm(g,&(y)),(__VA_ARGS__),um(g))
#define mm(g,r) ((ai_core_of(g)->root=&((struct ai_r){(word*)(r),ai_core_of(g)->root})))
#define um(g) (ai_core_of(g)->root=ai_core_of(g)->root->n)


#if UINTPTR_MAX > 0xffffffffu
#define mix ((uintptr_t) 0x9e3779b97f4a7c15) // round(2^64 / phi)
#else
#define mix ((uintptr_t) 0x9e3779b9) // round(2^32 / phi)
#endif

#define typ(_) ai_typ(cell(_))

#if ai_tco
#define ai_status_yield ai_status_ok
#else
#define ai_status_yield ai_status_eof
#endif
#define str_type_width (Width(struct ai_str))
#define op1(nom, i, x) lvm(nom) { Sp[0] = (x); Ip += i; ai_musttail return Continue(); }
#define op11(nom, x) op1(nom, 1, x)

#define pop1 ai_pop1


// one word per element: ints fold to Z, floats to R; C is the rank-0 complex pair
// (a tray rejects it at rank>=1). ordered Z < R < C, so `>= ai_R` is the float test.
// O slots hold live l words -- the one tray type the GC traces per element
// (evac_tray); O elements route through the promoting scalar dispatch (lvm_obin),
// which is what makes a bignum array add exactly instead of wrapping.
enum ai_tray_type { ai_Z, ai_R, ai_C, ai_O, };
// elementwise dyadic opcodes for lvm_vbin. codes >= vop_lt produce a 0/1 mask
// (vop_eq is table-shared machinery only -- tray_eq answers `=` as a boolean).
// vop_quot is `/` (true division), vop_fquot `//` (truncating); both in the arith group.
enum vop { vop_add, vop_sub, vop_mul, vop_quot, vop_rem, vop_fquot,
           vop_band, vop_bor, vop_bxor, vop_bsl, vop_bsr,
           vop_lt, vop_le, vop_gt, vop_ge, vop_eq, };
// the bitwise codes ride the WORD LANE (spec.l's width law): defined only where
// the cells are machine words; other operands take the whole op to the zero point
#define vop_bitp(op) ((op) >= vop_band && (op) <= vop_bsr)
// mask the shift count: C leaves count >= width undefined, and an accident is not a law
#define shmask(n) ((uintptr_t)(n) & (8 * sizeof(intptr_t) - 1))
word intern_checked(struct ai*, struct ai_str*);
uintptr_t intern_reserve(struct ai*);
uintptr_t hash(struct ai*, intptr_t);
static ai_inline union u *map_fill_back(union u*, uintptr_t);
lvm_t lvm_kcall,
 lvm_chain, lvm_tray, lvm_sym, lvm_nom, lvm_str, lvm_big, lvm_gembox, // the data sentinels; each tail-jumps to its apply handler
 lvm_putn, lvm_gauge, lvm_tune, lvm_clock, lvm_nclock, lvm_please, lvm_apof, lvm_seal, lvm_heard, lvm_worn, lvm_myself, lvm_books, lvm_setbooks, lvm_mods, lvm_lib,
 lvm_nilp,  lvm_putc, lvm_mint, lvm_nomctor, lvm_intern, lvm_chainp,
 lvm_saturate, lvm_ceil, lvm_peep, lvm_lamsrc, lvm_nifnom, lvm_cask, lvm_casknew, lvm_bcopy,
 lvm_coin, lvm_coinmk, lvm_load, lvm_dieof, lvm_coinp, lvm_add_coin, lvm_mul_coin, lvm_sub_coin, lvm_quot_coin,   // newtypes: a coin (die + payload), a typed hot riding KHot
 lvm_charmp,  lvm_nomp,   lvm_namep,  lvm_mintp,  lvm_strp,   lvm_tabp, lvm_band,   lvm_bor,  lvm_gem,  lvm_gemp,
 lvm_sin, lvm_cos, lvm_log, lvm_pow,   // sqrt/exp/tan/atan/atan2 are derived (numeral/complex forms), not nifs
 lvm_twin, lvm_twinp, lvm_re, lvm_im, lvm_conj, lvm_abs, lvm_carg,   // complex; lvm_twin_bin declared apart below
 lvm_bxor,  lvm_bsr,    lvm_bsl,    lvm_snip,
 lvm_link,   lvm_cap,  lvm_cup,    lvm_puts,
 lvm_string, lvm_lt,     lvm_le,   lvm_eq,     lvm_same, lvm_gt,  lvm_ge,
 lvm_sort,  lvm_tally, lvm_longp,
 lvm_pin, lvm_pull, lvm_tablet,   lvm_keys,  lvm_dig,
 lvm_unc, lvm_poke, lvm_peek,
 lvm_seek,  lvm_trim,   lvm_spin,   lvm_add,
 lvm_sub,   lvm_mul,    lvm_quot,   lvm_fquot, lvm_rem,  lvm_arg,
 lvm_bmul_start, lvm_bmul,   // resumable (yieldable) bignum multiply (chunked schoolbook)
 lvm_kmul,                   // resumable (yieldable) subquadratic Karatsuba (loop body)
 lvm_bdiv,                   // resumable (yieldable) bignum long division (loop body)
 lvm_quote, lvm_index,  lvm_eval,   lvm_cond, lvm_jump,   lvm_defglob,
 lvm_ap,    lvm_tap,    lvm_apn,    lvm_tapn, lvm_ret,
 lvm_argap, lvm_quoteap, lvm_argtap,
 lvm_arg0, lvm_arg1, lvm_arg2, lvm_arg3,
 lvm_quo0, lvm_quo1, lvm_quo2, lvm_quo3, lvm_quom1, lvm_quom2,
 // run fusion: a whole run of loads in one op, named for its shape (see below)
 lvm_aa, lvm_aq, lvm_qa, lvm_qq,
 lvm_aap, lvm_aqp, lvm_qap, lvm_qqp,
 // load + consumer: arg fused with the op that eats it
 lvm_argcap, lvm_argcup, lvm_argtwo, lvm_argcond,
 lvm_argtwocond,                                  // load + predicate + cond
 lvm_callk, lvm_scare, lvm_yield_sw, lvm_yield_nif, lvm_task_exit, lvm_spawn, lvm_wait,
 lvm_sleep, lvm_donep, lvm_scoop, lvm_hush, lvm_key,
 lvm_await,
 lvm_fgetc, lvm_fungetc, lvm_chug, lvm_unchug, lvm_inhand, lvm_fputc, lvm_fputs, lvm_fflush,
 lvm_fputbn, lvm_sound0,
 lvm_trayctor, lvm_iota, lvm_rank, lvm_alen, lvm_shape, lvm_atype,   // typed multi-rank arrays
 lvm_asum, lvm_aprod, lvm_max, lvm_min, lvm_aall, lvm_inner, lvm_outer,
 lvm_packp, lvm_bigp, lvm_sunp, lvm_setp, lvm_intf, lvm_litp, lvm_hotp,
 lvm_nif,         // CODEGEN BACKEND: emitted bytes -> applicable native value (1-arg / multi-arg)
 lvm_nifx,        // ... with an EXTRAS word (value[3]+8 = Ip+32): refs a native needs beyond the twin (the callout's clos, amble's ()/globals) ride a GC-walked cell slot, so value[1] stays the PLAIN twin and the image revert (img_nif_interp) never dereferences a pack
 lvm_resume,      // the WALKABLE call-out resume: jump blob-base + untag(offset) after delivering the result -- the frame carries an odd charm + an out-of-pool code address, so a GC (gen_grow included) with a call-out pending walks it clean (the retB stack-interior pointer is retired)
 lvm_calloutdrive, lvm_calloutresume;   // the drive addresses as fixnums (the glaze emitter bakes them as `li Ip` immediates)
// these carry extra operands, so they are declared apart from the plain lvm_t list
lvm(lvm_vbin, int);   // the elementwise/broadcast dyadic engine (vop selects the op)
lvm(lvm_bdiv_start, int);   // resumable long-division entry; the int is vop (vop_fquot / vop_rem)
lvm(lvm_vmap1, ai_flo_t (*)(ai_flo_t));            // monadic math fn elementwise, e.g. (sin a-tray)
lvm(lvm_vmap2, ai_flo_t (*)(ai_flo_t, ai_flo_t));  // ..dyadic with broadcast, e.g. (pow a-tray a-tray)
lvm(lvm_twin_bin, int);   // complex scalar lane; vop selects add/sub/mul/quot (rem -> zero)
lvm(lvm_cbin, int);   // complex-array lane: vbin's broadcast in the complex domain; `=` -> mask, ordering/% -> zero
lvm(lvm_obin, int);   // object-array lane: each element op runs the promoting scalar dispatch
// the data sentinels: each is the first word (ap) of its rep's heap objects and
// tail-jumps straight to its apply handler -- the sentinel IS the rep (enum d).
// bodies are byte-identical, kept distinct by address (ai_noicf).
static lvm(data_num_apply); static lvm(data_string_apply);
static lvm(data_sym_apply); static lvm(data_pair_apply);
#if ai_data_section
// ⚠ THE SLOT IS THE KIND. each sentinel lays in its own input section love_data.N,
// N its enum d value, on a grain of ai_data_stride -- so a run of one-fn sections
// tiles at exactly that, and ai_typ is one divide on (ap - lvm_sym) with in_data one
// unsigned compare (love.h). ld is told the tiling outright, in scripts mx.l lays
// from the same roster enum d comes from; holo needs no telling -- it lays each
// section on the grain the object declares, which is the same thing.
#define DSENT(nn, name, handler) \
 __attribute__((section("love_data." #nn), used, aligned(ai_data_stride))) \
 lvm(name) { ai_musttail return Ap(handler, g); }
DSENT(0,  lvm_sym,     data_sym_apply)
DSENT(1,  lvm_nom,     data_sym_apply)
DSENT(2,  lvm_sunbox,  data_num_apply)
DSENT(3,  lvm_gembox,  data_num_apply)
DSENT(4,  lvm_twinbox, data_num_apply)
DSENT(5,  lvm_big,     data_num_apply)
DSENT(6,  lvm_tray,    data_num_apply)
DSENT(7,  lvm_str,     data_string_apply)
DSENT(8,  lvm_chain,   data_pair_apply)
#else
lvm(lvm_tray)   { ai_musttail return Ap(data_num_apply, g); }
lvm(lvm_big)   { ai_musttail return Ap(data_num_apply, g); }
lvm(lvm_str)   { ai_musttail return Ap(data_string_apply, g); }
lvm(lvm_sym)   { ai_musttail return Ap(data_sym_apply, g); }
lvm(lvm_nom)   { ai_musttail return Ap(data_sym_apply, g); }
lvm(lvm_chain) { ai_musttail return Ap(data_pair_apply, g); }
lvm(lvm_gembox)   { ai_musttail return Ap(data_num_apply, g); }
lvm(lvm_sunbox)  { ai_musttail return Ap(data_num_apply, g); }
lvm(lvm_twinbox)  { ai_musttail return Ap(data_num_apply, g); }
#endif
char const *ai_nif_name(intptr_t);
#define tray(_) ((struct ai_tray*)(_))
#define charmp oddp
#define sym(_) ((struct ai_mint*)(_))
#define nom(_) ((struct ai_nom*)(_))
#define big(_) ((struct ai_big*)(_))
#define cask(_) ((struct ai_cask*)(_))
static ai_inline bool mintp(word _) { return lamp(_) && cell(_)->ap == lvm_sym; }
static ai_inline bool namep(word _) { return lamp(_) && cell(_)->ap == lvm_nom; }
static ai_inline bool packp(word _) { return lamp(_) && cell(_)->ap == lvm_tray; }
static ai_inline bool strp(word _) { return lamp(_) && cell(_)->ap == lvm_str; }
static ai_inline bool nomp(word x) { return lamp(x) && (cell(x)->ap == lvm_sym || cell(x)->ap == lvm_nom); }
// mutable flat byte string. NOT a data kind: the head is the behaves-as-0 lvm_cask,
// so the GC walks a cask as a plain length-2 thread and forwards the embedded ai_str
// free. earned by the build tools that back-patch an image in place.
static ai_inline bool caskp(word _) { return lamp(_) && cell(_)->ap == lvm_cask; }
// a map is a lookup-lambda with stable identity across growth: a fixed HEADER
// [lvm_map_lookup, backing, <tag>] callers hold, and an open-addressed BACKING
// [lvm_map_data, len, cap, k0,v0, .., <tag>] -- growth swaps header[1], so aliased
// references (ev's scopes) see later inserts. both are plain threads, no bespoke
// GC. empty slots hold map_gap, a unique out-of-pool address. (m k) -> value, () absent.
static lvm_t lvm_map_lookup, lvm_map_data;
static ai_inline bool tabp(word _) { return lamp(_) && cell(_)->ap == lvm_map_lookup; }
static const word ai_map_gap_cell = 0;
#define map_gap ((word) &ai_map_gap_cell)
#define map_min_cap 4
#define map_hint_max (1u << 24)        // the `(tablet n)` size hint saturates to this bounded green charm
static ai_inline word map_back(word m) { return cell(m)[1].x; }
static ai_inline word *map_slots(word m) { return &cell(map_back(m))[3].x; }
static ai_inline uintptr_t map_len(word m) { return getcharm(cell(map_back(m))[1].x); }
static ai_inline uintptr_t map_cap(word m) { return getcharm(cell(map_back(m))[2].x); }
word ai_mapget(struct ai*, word, word, word);
static word bookget(struct ai*, word, word);   // the layered global read: walks g->book (a CHAIN of books) head-first
static word macroget(struct ai*, word);        // the layered macro read: each layer's table rides its [zero] slot
static struct ai *ai_mapput(struct ai*), *map_new(struct ai*);
// the byte ops read from a string or a cask; both resolve to a ai_str of bytes.
static ai_inline struct ai_str *bytes_of(word x) { return caskp(x) ? cask(x)->str : str(x); }
// a COIN: a newtype value, a typed hot [lvm_coin, die, payload] -- a plain thread,
// no bespoke evac. ai_kind reads KHot, so +/* route every coin combination to
// lvm_addh/mulh, where a coin operand is intercepted. the DIE (a map keyed by the
// slot fixnums below) is the type descriptor; every coin of a type is struck from one die.
struct ai_coin { lvm_t *ap; word die; word payload; };
static ai_inline bool coinp(word _) { return lamp(_) && cell(_)->ap == lvm_coin; }
static ai_inline word coin_die(word x) { return ((struct ai_coin*) x)->die; }
static ai_inline word coin_load(word x) { return ((struct ai_coin*) x)->payload; }
// die slots (fixnum keys). ADD/MUL/APPLY are closures run inside the VM; net/=/<
// /show/tally default over the payload in pure C. HOT truthy = the die's coins are
// lit? (references); absent = fresh data. NET is a MODE fixnum, never a closure --
// ai_net is pure C under every truth test and must not re-enter the VM.
enum { DieName = 0, DieAdd = 1, DieMul = 2, DieApply = 3, DieHot = 4, DieSub = 5,
       DieNet = 6,    // net MODE, a fixnum: absent/0 = net of payload; 1 = net by TALLY (the
                       // count); 2 = RATIO (an (n d)-of-reals payload nets n/d, sign exact)
       DieStar = 7,   // truthy = the die's coins are NUMERIC: numeral application powers them
                       // through their own * (prel num-ap reads this slot; C never does)
       DieDiv = 8 };  // `/` -- like `-` it has no kind matrix, so lvm_quot intercepts coins itself
// read a die slot, or () if absent / the die is not a map.
static ai_inline word die_get(struct ai *g, word die, intptr_t slot) {
 return tabp(die) ? ai_mapget(g, zero, putcharm(slot), die) : zero; }
// arbitrary-precision integer, its own sentinel kind: flat raw limbs, moved by
// memcpy (a thread sound would misread even-and-in-pool limb words). slen = signed
// limb count, little-endian, top limb nonzero; zero always demotes, so slen is
// never 0. canonical demotion keeps charmp/sunp/bigp mutually exclusive.
struct ai_big { lvm_t *ap; intptr_t slen; ai_limb limb[]; };
static ai_inline bool bigp(word _) { return lamp(_) && cell(_)->ap == lvm_big; }
static ai_inline struct ai_big *ini_big(struct ai_big *b, intptr_t slen) {
 return b->ap = lvm_big, b->slen = slen, b; }
uintptr_t ai_big_bytes(struct ai_big*);
// canonicalize a magnitude into the smallest tier: fixnum, sun box, bignum
// (bumps *hp when it boxes); one sink shared by the reader and the arith slow paths
word ai_big_canon(ai_word **hp, ai_limb const *limb, int n, bool neg);
ai_flo_t ai_big_to_flo(word);                 // bignum -> double (used by toflo)
intptr_t ai_big_low(word);                   // bignum value mod 2^W (low machine word)
int ai_big_cmp(word, word);                  // -1/0/1 over two integer operands
bool ai_ratio_exact(struct ai*, word);  // int/ceil/saturate's exact-ratio domain: a net-mode-2 coin over integer (n d)
struct ai
 *ai_ratio_rung(struct ai*, int),     // ..and the lane: long-divide the parts (0 int, 1 ceil, 2 saturate), packed
 *ai_big_binop(struct ai*, int vop),  // vop_add..vop_rem, packed; pops one operand
 *ai_big_quot_true(struct ai*),       // `/` bignum lane: exact quotient when b | a, else a float box
 *ai_big_read_dec(struct ai*),        // sp[0] [+-]?digits token -> canonical value
 *ai_big_read_hex(struct ai*),        // ..and its [+-]?0x<hexdigits> twin
 *ai_big_read_oct(struct ai*);        // ..and [+-]?0<octdigits>, the third

// a boxed scalar float: a lean {ap, payload} box, two words
static ai_inline bool gemp(word _) { return lamp(_) && cell(_)->ap == lvm_gembox; }
static ai_inline bool sunp(word _) { return lamp(_) && cell(_)->ap == lvm_sunbox; }
static ai_inline bool twinp(word _) { return lamp(_) && cell(_)->ap == lvm_twinbox; }
static ai_inline bool trayp(word _) { return packp(_) && tray(_)->rank >= 1; }
static ai_inline bool galaxyp(word _) { return trayp(_) && tray(_)->type != ai_O; }

// FIXME uh, there's a max rank? that's not on purpose
#define maxrank 8   // bounds the stack index/stride arrays in the broadcast loop
extern size_t const
 ai_vt_[],                 // element byte size by ai_tray_type
 ai_T[];                   // element byte size by ai_tray_type (used pre-definition by lvm_gauge)
// Element payload: laid out row-major just past the shape words.
static ai_inline void *tray_data(struct ai_tray *v) { return (void*) (v->shape + v->rank); }
// Total element count = product of the dimensions (1 for a rank-0 scalar box).
static ai_inline uintptr_t tray_nelem(struct ai_tray *v) {
 uintptr_t n = 1;
 for (uintptr_t i = 0; i < v->rank; i++) n *= v->shape[i];
 return n; }
static ai_inline struct ai_tray *ini_tray(struct ai_tray *v, enum ai_tray_type t, uintptr_t rank) {
 return v->ap = lvm_tray, v->type = t, v->rank = rank, v; }
// Read element i of v as a double / as an integer (sign-extending the narrow
// integer types; truncating a float toward zero for the int reader). The int
// reader is only used on integer-typed arrays in practice.
static ai_inline ai_flo_t tray_get_flo(struct ai_tray *v, uintptr_t i) {
 void *p = tray_data(v);
 return v->type == ai_R ? ((ai_flo_t*) p)[i] : (ai_flo_t) ((intptr_t*) p)[i]; }
static ai_inline intptr_t tray_get_int(struct ai_tray *v, uintptr_t i) {
 void *p = tray_data(v);
 return v->type == ai_R ? (intptr_t) ((ai_flo_t*) p)[i] : ((intptr_t*) p)[i]; }
// Write element i of v, converting to v's element kind.
static ai_inline void tray_put_int(struct ai_tray *v, uintptr_t i, intptr_t x) {
 void *p = tray_data(v);
 if (v->type == ai_R) ((ai_flo_t*) p)[i] = (ai_flo_t) x; else ((intptr_t*) p)[i] = x; }
static ai_inline void tray_put_flo(struct ai_tray *v, uintptr_t i, ai_flo_t x) {
 void *p = tray_data(v);
 if (v->type == ai_R) ((ai_flo_t*) p)[i] = x; else ((intptr_t*) p)[i] = (intptr_t) x; }
// Read/write element i of a ai_O array as a raw tagged l word (the GC traces
// these; see evac_tray). No conversion -- the slot IS a value.
static ai_inline word tray_get_obj(struct ai_tray *v, uintptr_t i) {
 return ((word*) tray_data(v))[i]; }
static ai_inline void tray_put_obj(struct ai_tray *v, uintptr_t i, word x) {
 ((word*) tray_data(v))[i] = x; }

// truth: x is false iff (= 0 ($ x)). the net's codomain is COMPLEX (ai_net): a
// complex scalar nets itself, every other scalar nets real, aggregates SUM -- so
// the net is additive exactly. common kinds short-circuit with no walk; a sum
// cannot (a later negative cancels). lockstep with ai_saturate ($): same zero conditions.
static ai_inline struct ai_str *nom_str(struct ai *g, word x);   // a named sym -> its name string, else 0
struct ai_zn { ai_flo_t re, im; };                     // the net: a complex value
static ai_inline struct ai_zn zn(ai_flo_t re, ai_flo_t im) {
  struct ai_zn z = {re, im}; return z; }
// THE TRUTH GATE, not the total order -- the one place the two part: a net is
// nothing unless its REAL part is positive, so a pure phase is BLUE (truth cannot
// depend on which root of x^2+1 we named `i`). the lexicographic order stays as
// it was -- sorting needs totality. doc/measures.md.
// a macro, not a fn: a by-value ai_zn argument stages through push/pop, which
// bars unframe in every fn ai_nilp splices into (the hot truth-test fleet)
// FIXME open code this as ai_net(g, x).re <= 0
#define zn_false(z) ((z).re <= 0)
static struct ai_zn ai_net(struct ai *, word);         // fwd: aggregates sum their elements
static intptr_t ai_count(struct ai *, word);           // fwd: tally's C body (net-mode 1 reads it)
// ⚠ only the two lanes that answer with no load and no call earn a line here: they
// carry the corpus (a charm truth test, `()`), and ai_net is never inlined, so a
// third lane costs more than the walk it skips. every other kind's shape is ai_net's.
static ai_inline bool ai_nilp(struct ai *g, word x) {
  if (charmp(x)) return getcharm(x) <= 0;            // a charm is its own net
  if (mintp(x)) return true;                         // a bare point nets nothing
  return ai_net(g, x).re <= 0; }

// truncation toward zero / float remainder; pure and freestanding-safe (no libm)
static ai_inline ai_flo_t ai_trunc(ai_flo_t x) {
 if (x != x) return x;
 ai_flo_t m = x < 0 ? -x : x;
 if (m > (ai_flo_t) 9.22e18) return x;
 return (ai_flo_t) (int64_t) x; }
static ai_inline ai_flo_t ai_fmod(ai_flo_t a, ai_flo_t b) {
 return a - ai_trunc(a / b) * b; }

// --- numeric tower helpers ---
#define isnum(x) (charmp(x) || gemp(x) || sunp(x) || bigp(x))
// integer value of a fixnum-or-box operand (callers exclude floats AND bignums)
#define toint(x) (charmp(x) ? (intptr_t) getcharm(x) : sun_get(x))
// double value of any numeric operand (a bignum widens via ai_big_to_flo)
#define toflo(x) (charmp(x) ? (ai_flo_t) getcharm(x) : gemp(x) ? gem_get(x) : sunp(x) ? (ai_flo_t) sun_get(x) : ai_big_to_flo(x))
#define twin_req Width(struct ai_twin)
// the tagged fixnum range: putcharm spends one bit
#define mincharm (INTPTR_MIN >> 1)
#define maxcharm (INTPTR_MAX >> 1)
// emit an integer/double result into `_res`, demoting to a fixnum when it fits.
// caller holds Have(box_req); takes no &local, so the caller keeps its tail call.
#define emit_int(r, R) do { intptr_t _r = (R); \
 if (_r >= mincharm && _r <= maxcharm) r = putcharm(_r); \
 else r = mk_sun(&Hp, _r); } while (0)
#define emit_gem(r, R) do { r = mk_gem(&Hp, (R)); } while (0)

// RNG: state is a rank-1 i64 tray of length 4 (xoshiro256++), its payload raw
// bytes moved by memcpy -- tray_get/put_int would truncate the limbs on 32-bit
// ports. fixed 8-byte limbs make a seed reproduce on every target.
#define rng_state_len 4
#define rng_payload_bytes (rng_state_len * 8)
#define rng_tray_bytes (sizeof(struct ai_tray) + sizeof(uintptr_t) + rng_payload_bytes)
#define rng_tray_req (b2w(rng_tray_bytes))
// whichever element kind is 8 bytes wide, so ai_tray_bytes sees the full payload
#define rng_vt (Bytes == 4 ? ai_C : ai_Z)
void ai_rng_seed(struct ai_tray*, uint64_t);   // shape an i64 state tray + seed it (SplitMix64)
lvm_t lvm_wheel, lvm_turn, lvm_turnf;
int memcmp(void const*, void const*, size_t);
void *malloc(size_t), free(void*),
 *memcpy(void*restrict, void const*restrict, size_t),
 *memmove(void*restrict, void const*restrict, size_t),
 *memset(void*, int, size_t);
size_t strlen(char const*);

// the lean scalar boxes: {ap, payload} GC leaves, copied like bignums
struct ai_gem { lvm_t *ap; ai_word w; };
#define gem_req Width(struct ai_gem)
struct ai_sun { lvm_t *ap; intptr_t w; };    // raw intptr_t payload, no bit pun
#define sun_req Width(struct ai_sun)
#define box_req (gem_req > sun_req ? gem_req : sun_req)     // what emit_int/emit_gem reserve
struct ai_twin { lvm_t *ap; ai_word re, im; };   // two punned-double payload words
// pun through a union, NOT memcpy(&local,..): the memcpy form escapes a stack
// local, and clang -Os then refuses the sibling call out of any inlining VM ap --
// silently breaking threaded dispatch (mk/tools/vmret.l).
_Static_assert(sizeof(ai_flo_t) == sizeof(uintptr_t), "float box assumes ai_flo_t is pointer-width");
typedef union { uintptr_t u; ai_flo_t d; } ai_flo_pun;
static ai_inline ai_flo_t gem_get(word x) {
 return ((ai_flo_pun){ .u = ((struct ai_gem*) x)->w }).d; }
// allocate a float box at *hpp (caller holds Have(gem_req)); no &local, so the caller keeps its tail call.
// ⚠ THE LAW, the one real-float box-write: NaN collapses to 0 so the order stays total and
// !x == (0 = $x) holds. inf rides through. glaze's jit lanes emit the same collapse.
static ai_inline word mk_gem(ai_word **hpp, ai_flo_t v) {
 struct ai_gem *f = (struct ai_gem*) *hpp;
 *hpp += gem_req;
 f->ap = lvm_gembox;
 if (v != v) v = 0; // nothing is unequal to itself, come on IEEE, give me a break
 f->w = ((ai_flo_pun){.d = v}).u;
 return word(f); }

static ai_inline ai_flo_t twin_re(word x) {
 return ((ai_flo_pun){ .u = ((struct ai_twin*) x)->re }).d; }

static ai_inline ai_flo_t twin_im(word x) {
 return ((ai_flo_pun){ .u = ((struct ai_twin*) x)->im }).d; }

static ai_inline ai_flo_t twin_mod(word x) {   // |z|
 ai_flo_t re = twin_re(x), im = twin_im(x);
 return ai_sqrt(re * re + im * im); }

// mk_twin allocates at *hpp (caller holds Have(twin_req)); no &local taken
static ai_inline void twin_set(struct ai_twin *v, ai_flo_t re, ai_flo_t im) {
 v->re = ((ai_flo_pun){ .d = re }).u;
 v->im = ((ai_flo_pun){ .d = im }).u; }

static ai_inline word mk_twin(ai_word **hpp, ai_flo_t re, ai_flo_t im) {
 struct ai_twin *v = (struct ai_twin*) *hpp;
 *hpp += twin_req;
 v->ap = lvm_twinbox;
 twin_set(v, re, im);
 return word(v); }

static ai_inline intptr_t sun_get(word x) { return ((struct ai_sun*) x)->w; }

// allocate a sun box at *hpp (caller holds Have(sun_req)); no &local taken
static ai_inline word mk_sun(ai_word **hpp, intptr_t v) {
 struct ai_sun *w = (struct ai_sun*) *hpp; *hpp += sun_req;
 w->ap = lvm_sunbox; w->w = v; return word(w); }

// a tray key -> a row-major element offset: a fixnum on a rank-1 tray, else a
// shape-list of `rank` fixnums. -1 = wrong rank or out of bounds (the caller's miss
// lane). peep reads through it, pin writes through it: one index law. ⚠ answers by
// VALUE, never through an out-param: an escaping &local costs its caller the tail jump.
static intptr_t tray_off(struct ai_tray *v, word k) {
 if (v->rank == 1 && charmp(k)) {
  intptr_t ix = getcharm(k);
  return ix >= 0 && ix < (intptr_t) v->shape[0] ? ix : -1; }
 if (!chainp(k)) return -1;
 uintptr_t a = 0, o = 0;
 for (word l = k;; l = B(l)) {
  if (!chainp(l)) return a == v->rank ? (intptr_t) o : -1;
  word ki = A(l);
  if (a >= v->rank || !charmp(ki)) return -1;
  intptr_t ix = getcharm(ki);
  if (ix < 0 || ix >= (intptr_t) v->shape[a]) return -1;
  o = o * v->shape[a] + (uintptr_t) ix, a++; } }

// store x at element i, converting to v's tier: O takes any value verbatim, C packs
// (re,im) (a real rides in as (r,0)), R/Z take a number. false = a non-number into a
// numeric tray, which leaves the slot alone. ⚠ v is the CALLER's fresh tray -- an
// object slot gaining a young word needs no barrier only because nothing old is written.
static bool tray_put(struct ai_tray *v, uintptr_t i, word x) {
 if (v->type == ai_O) return tray_put_obj(v, i, x), true;
 if (v->type == ai_C) {
  ai_flo_t *fp = tray_data(v);
  if (twinp(x)) fp[2*i] = twin_re(x), fp[2*i+1] = twin_im(x);
  else if (isnum(x)) fp[2*i] = toflo(x), fp[2*i+1] = 0;
  else return false;
  return true; }
 if (!isnum(x)) return false;
 if (v->type >= ai_R) tray_put_flo(v, i, toflo(x));
 else tray_put_int(v, i, charmp(x) ? (intptr_t) getcharm(x)
                      : gemp(x) ? (intptr_t) gem_get(x) : sun_get(x));
 return true; }

// equality comparisons inline the fast identity check
ai_noinline bool eqv(struct ai*, word, word); // this is for checking equality of non-identical values
static bool eqv_at(struct ai*, word, word, word*); // eqv with an explicit worklist base (for re-entrant calls from the beta bridge)
// eqv has no value-equality for distinct charms or distinct noms -- identity is
// their whole equality -- so eql settles both inline and skips the noinline call
static ai_inline bool eql(struct ai *g, word a, word b) {
 return a == b ? true : (a & b & 1) || (nomp(a) && nomp(b)) ? false : eqv(g, a, b); }

// threads (and every sounded heap object) end with one tag word: the object's own
// head pointer with bit 1 set. the terminator test is the tag bits AND the payload
// pointing back into the pool -- an embedded external pointer can carry (x&3)==2
// but never points into the pool.
#define ai_thread_tag 2
static ai_inline bool tagp(word x, word const *lo, word const *hi) {
 word const *p = (word const*) (x & ~(word) 3);
 return (x & 3) == ai_thread_tag && p >= lo && p < hi; }
// GC scans run with DIFFERENT [lo,hi), so a terminator must be recognized by which
// LIVE pool its head lands in, not the caller's single range -- else a young-pointing
// terminator under the major range is gcp'd as a field and followed off the heap.
static ai_inline bool in_live_pool(struct ai *g, word const *p) {
 if (p >= ptr(g) && p < ptr(g) + g->len) return true;             // minor / main pool
 if (g->gc_to_lo && p >= g->gc_to_lo && p < g->gc_to_hi) return true;   // current to-space
 if (g->major_pool && p >= g->major_pool && p < g->major_pool + 2 * g->major_len) return true;   // both major halves
 return false; }
static ai_inline bool tagl(struct ai *g, word x) {                  // range-independent terminator test
 return (x & 3) == ai_thread_tag && in_live_pool(g, (word const*) (x & ~(word) 3)); }
static ai_inline union u *tagthread(union u *h, uintptr_t len) {
  return h[len].x = word(h) | ai_thread_tag, h; }
#define topof(g) ((word*)g+g->len)
static ai_inline struct ai_tag { union u *head; union u end[]; } *ttag(struct ai*g, union u *k) {
 // scan k forward to its terminator; a tenured object lives in the major pool, so pick the range
 word *lo, *hi;
 if (ptr(k) >= g->major_base && ptr(k) < g->major_hp) lo = g->major_base, hi = g->major_hp;
 else lo = ptr(g), hi = topof(g);
 while (!tagp(k->x, lo, hi)) k++;
 return (struct ai_tag*) k; }
static ai_inline union u *tag_head(struct ai_tag *t) {
 return cell(word(t->head) & ~(word) 3); }

static ai_inline union u *clip(struct ai *g, union u *k) {
 return tagthread(k, cell(ttag(g, k)) - k); }



static ai_inline struct ai_mint *ini_missing(struct ai_mint *y, uintptr_t code) {
 return y->ap = lvm_sym, y->code = code, y; }

// the spelling hash a fresh nom caches in its `dig` slot (same fnv walk as the
// KString lane in hash(), so a nom and its name string hash alike)
static ai_inline uintptr_t nom_dig(uintptr_t name) {
 uintptr_t n = len(name), h = mix;
 char const *bs = txt(name);
 while (n--) h ^= (uint8_t) *bs++, h *= mix;
 return h; }

static ai_inline struct ai_nom *ini_nom(struct ai_nom *y, uintptr_t name, uintptr_t code, uintptr_t dig) {
 return y->ap = lvm_nom, y->name = name, y->code = code, y->dig = dig, y; }

static ai_inline struct ai_str *ini_str(struct ai_str *s, uintptr_t len) {
 return s->ap = lvm_str, s->len = len, s; }

// the unique empty string: data-segment, immortal (gcp's out-of-pool
// short-circuit); a zero-length string is never heap-allocated.
const struct ai_str ai_str_empty = { .ap = lvm_str, .len = 0 };
// () -- the one serial-0 mint, shared by every core (serial 0 is never drawn, so
// it is unique + least in the order). See the ZeroPoint macro in love.h.
const struct ai_mint ai_mint_zero = { .ap = lvm_sym, .code = 0 };


static ai_inline uintptr_t rot(uintptr_t x) {
  int const s = sizeof(uintptr_t) * 4; // shift bits = word bits / 2 = sizeof(word) * 4
  return (x << s) | (x >> s); }

// the four doors that are not a device; spelled out beside their readn/writen
extern struct ai_port_vt const ai_ti_vt, ai_to_vt, ai_closed_vt, ai_ci_vt;

static ai_inline void *off_pool(struct ai *g) {
 return g == g->pool ? (word*) g->pool + g->len : (word*) g->pool; }
static ai_inline struct ai *pushq(struct ai*g) { return intern(ai_strof(g, "\\")); }
static ai_inline struct ai *push0(struct ai*g) { return ai_push(g, 1, zero); }
static ai_inline size_t llen(word l) {
 size_t n = 0;
 while (chainp(l)) n++, l = B(l);
 return n; }
static ai_inline struct ai*ai_pop(struct ai*g, uintptr_t n) {
 return ai_core_of(g)->sp += n, g; }

// ============================================================================
// macros (hoisted from all merged units; see section banners below)
// ============================================================================






#define min(p,q) ((p)<(q)?(p):(q))
#define max(p,q) ((p)>(q)?(p):(q))




#define limb_base ((ai_dlimb) 1 << limb_bits)

#define yield_interval 64
// fairness yields between parked-ring sweeps, a separate counter from yield_interval:
// a yield walks the short run ring, a sweep is a syscall over the parked ring, and one
// knob cannot price both. ⚠ this bounds only how long a ready parked task waits behind
// a peer that NEVER blocks -- an i/o task sweeps on its own blocking path first.
#define sweep_interval 16
// a fairness yield clears any stale one-shot park intention first: lvm_fgetc never
// clears the fd on a successful read, and a periodic yield that inherited it would
// park this task on that fd for good. ⚠ g->parked joins the guard: a server whose
// every client is blocked leaves a self-ring, and a tasks-only test stops firing.
#define YieldCheck() \
  if ((g->tasks->m != g->tasks || g->parked) && ++g->yield_ctr >= yield_interval) \
    { g->next_wait_fd = -1; g->next_wake_at = 0; ai_musttail return Ap(lvm_yield_sw, g); }
#define argn(nom, i) lvm(nom) { Have1(); Sp[-1] = Sp[i]; Sp -= 1; Ip += 1; ai_musttail return Continue(); }
#define quon(nom, v) lvm(nom) { Have1(); Sp -= 1; Sp[0] = putcharm(v); Ip += 1; ai_musttail return Continue(); }

#define Ana(n, ...) struct ai *n(struct ai *g, struct env **c, intptr_t x, ##__VA_ARGS__)
#define Cata(n, ...) struct ai *n(struct ai *g, struct env **c, ##__VA_ARGS__)
#define incl(e, n) ((e)->len += ((n)<<1))
#define Kp (g->ip)
#define cata1(n, ...) static Cata(n) { return __VA_ARGS__, pull(g, c); }
#define forget() (ai_core_of(g)->root=(mm0),g)

#define fs0(g) (ai_core_of(g)->sp[0])


// ============================================================================
// g
// ============================================================================
enum ai_status ai_fin(struct ai *g) {
 enum ai_status s = ai_code_of(g);
 if ((g = ai_core_of(g))) {
   for (struct ai_fz *fz = g->fz; fz; fz->fn(fz->p), fz = fz->next); // run finalizers
   // ⚠ the rem set and the major pool are ai_ini_0's OWN g->alloc calls, not room inside
   // the nursery -- a frontend that exits never misses them, one that fins to make room
   // for the next runtime gets nothing back without this.
   if (g->rem) g->alloc(g, g->rem, 0);
   if (g->major_pool) g->alloc(g, g->major_pool, 0);
   g->alloc(g, g->pool, 0); }                 // ..the pool IS g, so it goes last
 return s; }

// the module lane's target: find-or-make mod's tablet on the registry (g->mods,
// the same lazy singleton lvm_mods answers -- the drain runs at boot, before
// prel, so both are creatable here, and again over a woken image, where the
// found tablet takes the re-pin) and push it where the book map would sit.
static struct ai *ai_modtab(struct ai *g, char const *mod) {
 if (!ai_ok(g)) return g;
 struct ai *c = ai_core_of(g);
 if (c->mods == zero) {
  if (!ai_ok(g = map_new(g))) return g;
  c = ai_core_of(g), c->mods = c->sp[0], c->sp++; }
 if (!ai_ok(g = intern(ai_strof(g, mod)))) return g;   // [modnom ..]
 c = ai_core_of(g);
 word m = ai_mapget(c, zero, c->sp[0], c->mods);
 if (m != zero) { c->sp[0] = m; return g; }             // [tablet ..]
 if (!ai_ok(g = map_new(g))) return g;                  // a fresh module: [tablet modnom ..]
 c = ai_core_of(g);
 g = ai_push(g, 3, c->sp[1], c->sp[0], c->mods);        // (key val coll) for mapput
 if (!ai_ok(g = ai_mapput(g))) return g;                // [mods tablet modnom ..]
 c = ai_core_of(g);
 c->sp[2] = c->sp[1], c->sp += 2;                       // [tablet ..]
 return g; }

// ⚠ every .x here must be IMMORTAL -- a nif address, a fixnum, an out-of-pool
// constant. C cannot re-root what it holds in an array, and no ordering fixes it;
// a value that MOVES arrives on the stack instead (ai_defv).
// mod non-NULL binds the whole table under that module instead of the book.
struct ai *ai_defn(struct ai*g, struct ai_def const*defs, uintptr_t n, char const *mod) {
 for (g = mod ? ai_modtab(g, mod) : ai_push(g, 1, A(ai_core_of(g)->book)); n--;
  g = ai_mapput(intern(ai_strof(ai_push(g, 1, defs[n].x), defs[n].n))));
 ai_core_of(g)->sp++;
 return g; }

// ai_defn's twin for a value that MOVES: it rides g->sp[0], where the collector
// updates it, and is LEFT there (a second name binds the same one; callers pop).
// the sp[1] re-read happens AFTER the book push, so a collection inside it is accounted for.
struct ai *ai_defv(struct ai *g, char const *nm) {
 if (!ai_ok(g)) return g;
 g = ai_push(g, 1, A(ai_core_of(g)->book));           // [book, value, ..]
 if (!ai_ok(g)) return g;
 g = ai_mapput(intern(ai_strof(ai_push(g, 1, ai_core_of(g)->sp[1]), nm)));
 if (ai_ok(g)) ai_core_of(g)->sp++;                   // [value, ..]
 return g; }

// the nif + instruction registry: one `union u` table, a nif's little stream being a
// RUN inside it, then def1 -- the name -> value table ai_defn reads into the book,
// carrying each run's address. both are laid from the one roster in nifs.l -- EDIT
// THAT, not nifs.h; `make test_clay` fails on drift.
#include "nifs.h"

static lvm(_lvm_yield_c) { return Pack(g), g; }
static union u const yield_c[] = { {_lvm_yield_c} };

// lvm_help: the default help ap -- re-encode the raised status, yield to C.
// _lvm_help_scare sits outside lvm_* on purpose: the one designed `ret`
// (vmret sounds lvm_* only), reached by tail call.
static lvm(_lvm_help_scare, enum ai_status s) { return Pack(g), encode(g, s); }
lvm(lvm_help) {
 enum ai_status s = ai_code_of(g);
 return Ap(_lvm_help_scare, ai_core_of(g), s); }

// reverse-lookup a nif value -> its source name or NULL (the printer renders nifs by name)
char const *ai_nif_name(intptr_t x) {
 for (uintptr_t i = 0; i < countof(def1); i++) if (def1[i].x == x) return def1[i].n;
 return 0; }

static struct ai *ai_ini_0(struct ai*g, uintptr_t len0, void *(*al)(struct ai*, void*, size_t)) {
 memset(g, 0, sizeof(struct ai));      // the core needs no leading ap: () is the const ZeroPoint, never (word)g
 g->len = len0, g->pool = (void*) g, g->alloc = al;
 g->scare_a = g->scare_b = zero;        // v0..end is GC-walked: raw 0 is not a value
 g->hot_read = g->hot_numap = g->hot_stack = g->hot_compose = g->hot_opfix = g->hot_help = g->hot_show = zero;   // unsealed: hot_hook traps until (seal-hook) fills them; help zero = nobody listening
 g->hot_io = zero;                     // the task's stdio: zero is the console, the steady state
 g->mods = zero;                       // the module registry: lazily created by the first (mods _) read
 g->hp = g->end, g->sp = (word*) g + len0, g->ip = (union u*) yield_c, g->t0 = ai_clock();
 g->minor = g->end;                  // generational watermark: nothing tenured yet (the first collection sets it)
 // the rem set + major pool ride g->alloc: a frontend that cannot supply them cannot run
 g->major_len = ai_major0;
 g->rem = g->alloc(g, NULL, AiRemCap * sizeof(word));
 g->major_pool = g->rem ? g->alloc(g, NULL, 2 * g->major_len * sizeof(word)) : NULL;
 if (!g->major_pool) { if (g->rem) g->alloc(g, g->rem, 0); return encode(g, ai_status_scare); }
 g->major_base = g->major_hp = g->major_pool, g->rem_cap = AiRemCap, g->budget = ai_budget;
 g->minor0 = ai_minor0, g->major0 = ai_major0, g->ratio = ai_gc_ratio;   // the live knobs; `tune` moves them
 g->next_wait_events = ai_wait_in;
 // book + macro maps (lookup-lambdas) then the main task thread.
 if (ai_ok(g = map_new(g)) && ai_ok(g = map_new(g)) && ai_ok(g = ai_have(g, 9))) {
  union u *M = bump(g, 9);            // sp[0]=macro, sp[1]=book (no GC since ai_have)
  M[0].m = M;
  M[1].x = zero;   // sentinel; replaced on first yield
  M[2].x = zero;   // main pid
  M[3].x = zero;   // wake_at: zero means "always runnable"
  M[4].x = putcharm(-1);  // wait_fd: -1 = not waiting on I/O (slot value -1, non-zero)
  M[5].x = putcharm(ai_wait_in);   // wait_events: the read direction, the default
  M[6].x = zero;   // help: helpless until the first (hear f)
  M[7].x = zero;   // stdio: the console until the first (wear l)
  g->tasks = tagthread(M, 8);
  g->parked = NULL;   // nothing is fd-parked before the first task ever parks
  // book[zero] = macro (the macro table -- no separate field). Both are on the
  // stack; push the zero key so (sp2,sp1,sp0)=(book,macro,zero) for ai_mapput.
  g = ai_push(g, 1, zero);
  g = ai_mapput(g);                     // -> sp[0] = book
  g->book = g->sp[0];                  // henceforth GC-forwarded via the v0..end loop
  // the ABYSS: g->book holds a CHAIN of books, walked head-first (bookget) --
  // one link today (orth, the boot book); a later layer prepends and shadows.
  // The l-level `book` global stays the orth MAP (def0 pins A(g->book)).
  if (ai_ok(g = ai_have(g, Width(struct ai_chain)))) {
   struct ai_chain *ly = (void*) bump(g, Width(struct ai_chain));
   ini_chain(ly, g->sp[0], ZeroPoint);
   g->book = (word) ly; }
  g = ai_pop(g, 1);
  // the WEAK intern map (string -> the canonical atom), created before the
  // first intern (the def tables just below). it lives OUTSIDE the traced
  // v0 region: a collection clones it untraced and sweeps it at the fixpoint.
  g = map_new(g);
  if (ai_ok(g)) g->symbols = ai_pop1(g);
  struct ai_def def0[] = {
   {"book", A(g->book)},   // the l-level book = the orth MAP (the chain stays C-side; `books` reads it)
   {"in", (word) &ai_stdin},
   {"out", (word) &ai_stdout},
   {"err", (word) &ai_stderr},
   // the two doors prel BUILDS (tap and jug), so it can stamp the kind it means;
   // mopped at birth like every other raw pointer the compiler folds (love/egg.l)
   {"ci-vt", (word) &ai_ci_vt},
   {"to-vt", (word) &ai_to_vt},
   // max-charm/min-charm: this build's fixnum bounds, exposed so width-specific
   // tests gate on the real boundary (it differs on 32- vs 64-bit ports).
   {"max-charm", putcharm((ai_word)((uintptr_t)-1 >> 2))},
   {"min-charm", putcharm(-(ai_word)((uintptr_t)-1 >> 2) - 1)},
   // love-tco: glazed code continues by tail-jump, which only the threaded build
   // honors -- auto.l reads this and keeps the interpreter on a trampoline build
   {"love-tco", putcharm(ai_tco)}, };
  g = ai_defn(g, def0, countof(def0), 0);
  g = ai_defn(g, def1, countof(def1), 0);
  if (ai_ok(g = ai_strof(g, AiVersion)))            // a live string: off the STACK, never an ai_def
   g = ai_pop(ai_defv(g, "love-version"), 1);
  // `love-arch`: the host CPU the glaze emits for. auto-ev interns it as the assembler
  // target ('x64 / 'arm64) and gates the still-x86-only lanes (float / loops).
#if defined(__x86_64__)
  #define AiArch "x64"
#elif defined(__aarch64__)
  #define AiArch "arm64"
#elif defined(__riscv)
  #define AiArch "riscv64"
#else
  #define AiArch "other"
#endif
  if (ai_ok(g = ai_strof(g, AiArch)))
   g = ai_pop(ai_defv(g, "love-arch"), 1);
  // the 'missing tag needs nothing here (the raise sites mint it); the reader owns
  // no operator tables -- book['operators] is seeded by the prel and factored at compile time
 }
 return g; }

struct ai *ai_ini_m(void *(*al)(struct ai*, void*, size_t)) {
 uintptr_t const len0 = ai_minor0;   // initial minor pool; grows on demand (gen_grow)
 struct ai *g = al(NULL, NULL, 2 * len0 * sizeof(word));
 return g == NULL ? encode(g, ai_status_scare) : ai_ini_0(g, len0, al); }

static void *ai_libc_alloc(struct ai*g, void *p, size_t n) { (void) g; return n ? malloc(n) : (free(p), NULL); }
struct ai *ai_ini(void) { return ai_ini_m(ai_libc_alloc); }

// ============================================================================
// stack
// ============================================================================
static struct ai *ai_pushr(struct ai *g, uintptr_t m, uintptr_t n, va_list xs) {
 if (n == m) return ai_please(g, m);
 word x = va_arg(xs, word);
 mm(g, &x);
 g = ai_pushr(g, m, n + 1, xs);
 um(g);
 if (ai_ok(g)) *--g->sp = x;
 return g; }

struct ai *ai_push(struct ai *g, uintptr_t m, ...) {
 if (!ai_ok(g)) return g;
 va_list xs;
 va_start(xs, m);
 uintptr_t n = 0;
 if (avail(g) < m) g = ai_pushr(g, m, n, xs);
 else for (g->sp -= m; n < m; g->sp[n++] = va_arg(xs, word));
 va_end(xs);
 return g; }

struct ai *gxl(struct ai *g) {
 if (ai_ok(g = ai_have(g, Width(struct ai_chain)))) {
  struct ai_chain *p = bump(g, Width(struct ai_chain));
  ini_chain(p, g->sp[0], g->sp[1]);
  *++g->sp = (word) p; }
 return g; }

struct ai *gxr(struct ai *g) {
 if (ai_ok(g = ai_have(g, Width(struct ai_chain)))) {
  struct ai_chain *p = bump(g, Width(struct ai_chain));
  ini_chain(p, g->sp[1], g->sp[0]);
  *++g->sp = (word) p; }
 return g; }

// ============================================================================
// gc
// ============================================================================
lvm(lvm_gc, uintptr_t n) {
 Pack(g);
 if (!ai_ok(g = ai_please(g, n))) return Ap(_lvm_ghelp, g);
 return Resume(); }

static word gcp(struct ai*, word, word const *, word const *);

static ai_inline void evac_chain(struct ai*g, word const*const p0, word const*const t0) {
 struct ai_chain *w = (struct ai_chain*) g->cp;
 g->cp += Width(struct ai_chain);
 w->a = gcp(g, w->a, p0, t0);
 w->b = gcp(g, w->b, p0, t0); }

static ai_inline void evac_tray(struct ai*g, word const*const p0, word const*const t0) {
 struct ai_tray *v = tray(g->cp);
 g->cp += b2w(ai_tray_bytes(v));
 if (v->type != ai_O) return;                 // numeric trays are GC leaves (flat payload)
 word *e = (word*) tray_data(v);              // object tray: forward each live element word
 uintptr_t n = tray_nelem(v);
 while (n--) e[n] = gcp(g, e[n], p0, t0); }

static ai_inline void evac_str(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += b2w(sizeof(struct ai_str) + str(g->cp)->len); }

static ai_inline void evac_big(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += b2w(ai_big_bytes(big(g->cp))); }

// the lean boxes are flat GC leaves
static ai_inline void evac_gem(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += gem_req; }

static ai_inline void evac_sun(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += sun_req; }

static ai_inline void evac_twin(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += twin_req; }

static ai_inline void evac_sym(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += Width(struct ai_mint); }              // uniform 2 words; copy_sym forwards the serial

static ai_inline void evac_nom(struct ai*g, word const*const p0, word const*const t0) {
 struct ai_nom *w = (struct ai_nom*) g->cp;
 g->cp += Width(struct ai_nom);                 // 4 words; forward the name string (serial + dig are scalars)
 w->name = gcp(g, w->name, p0, t0); }

static ai_inline void evac_thread(struct ai *g, word const *const p0, word const*const t0) {
  // tagl ends the thread regardless of scan space, so a young-pointing terminator is never gcp'd as a field
  for (g->cp += 1; !tagl(g, g->cp[-1]); g->cp[-1] = gcp(g, g->cp[-1], p0, t0), g->cp++);
  // a pinned thread's terminator names its own head, which moved with the block.
  // copy_thread re-tags what it copies; a memcpy'd one keeps a from-space self-pointer,
  // and the loop above ends on it without handing it to gcp.
  if (g->froze_lo) {
   word *h = (word*)(g->cp[-1] & ~(word) 3);
   if (h >= g->froze_lo && h < g->froze_hi)
    g->cp[-1] = (word)(g->gc_to_lo + (h - g->froze_lo)) | ai_thread_tag; } }

static ai_inline void evac_data(struct ai *g, word const *const p0, word const*const t0) {
  switch (typ(g->cp)) {
   case DTray: return evac_tray(g, p0, t0);
   case DMint: return evac_sym(g, p0, t0);
   case DNom: return evac_nom(g, p0, t0);
   case DChain: return evac_chain(g, p0, t0);
   case DString: return evac_str(g, p0, t0);
   case DBig: return evac_big(g, p0, t0);
   case DGem: return evac_gem(g, p0, t0);
   case DSun: return evac_sun(g, p0, t0);
   case DTwin: return evac_twin(g, p0, t0); }
  __builtin_trap(); }                            // a hot outside enum d: the object is not what its ap says

// ===== generational write barrier =====
// a minor scavenges only [minor, hp) and finds old->young edges through the REM
// SET: every edge execution mints (a map pin, a store) goes through gen_wb, so a
// minor under a complete set is sound (test/proof/rocq/gc.v barrier_sound).
// the one escape is overflow (rem_miss): a dropped entry forces the next collection
// MAJOR, which traces from roots and needs no rem set.
// young?: the ADDRESS is the generation (no age bits) -- in [minor, hp).
static ai_inline bool ai_young(struct ai *g, word p) {
 return lamp(p) && ptr(p) >= g->minor && ptr(p) < g->hp; }
static bool gen_remembered(struct ai *g, word obj) {
 for (uintptr_t i = 0; i < g->rem_n; i++) if (g->rem[i] == obj) return true;
 return false; }
static void gen_remember(struct ai *g, word obj) {
 if (g->rem_n && g->rem[g->rem_n - 1] == obj) return;          // hot path: same map as last pin
 if (gen_remembered(g, obj)) return;                           // deduped: the set stays small (book + a few)
 if (g->rem_n < g->rem_cap) g->rem[g->rem_n++] = obj;          // full: the miss forces a MAJOR (roots-only trace, no rem set), so a dropped entry can't orphan a young edge
 else g->rem_miss++;
 if (g->rem_n > g->rem_hi) g->rem_hi = g->rem_n; }
// an old `src` gains a young `p` -> remember src. maps and reader spines are the
// only in-place mutations, so this is the whole hot-path barrier.
static ai_inline void gen_wb(struct ai *g, word src, word p) {
 if (lamp(src) && ai_young(g, p) && !ai_young(g, src)) gen_remember(g, src); }
static ai_inline bool ai_major_cell(struct ai *g, word *c) {       // a tenured cell: inside the major pool
 return (ai_word*) c >= g->major_base && (ai_word*) c < g->major_hp; }
// the cell barrier (c0's stores, ev's poke): remember the smallest scannable unit
// around a young-into-tenured store. gen_wb_cell: a cell in a TAGGED span, never a
// chain's field (data has no terminator). gen_wb_two: a CONS mutation -- ⚠ NOT A
// DOOR: patching a cons in place is off-road; the five callers left each patch a
// list they consed moments earlier and nobody else holds. both mask g.
static ai_inline void gen_wb_cell(struct ai *g, void *cl, word v) {
 g = ai_core_of(g);
 if (ai_young(g, v) && ai_major_cell(g, cl)) gen_remember(g, (word) cl); }
static ai_inline void gen_wb_two(struct ai *g, word two, word v) {
 g = ai_core_of(g);
 if (ai_young(g, v) && ai_major_cell(g, ptr(two))) gen_remember(g, two); }
// gen_scan_inplace: a tenured object pointing into the young set stays put, but its
// young fields must promote -- gcp each outgoing pointer IN PLACE. evac_* without
// the relocation; a thread's terminator sits in the major to-space.
static void gen_scan_inplace(struct ai *g, word obj, word const *p0, word const *t0) {
 union u *p = cell(obj);
 if (datp(obj)) switch (typ(obj)) {
  case DChain: { struct ai_chain *w = two(obj);
                 w->a = gcp(g, w->a, p0, t0), w->b = gcp(g, w->b, p0, t0); break; }
  case DTray:   { struct ai_tray *v = tray(p); if (v->type == ai_O) { word *e = (word*) tray_data(v);
                 for (uintptr_t i = 0, ne = tray_nelem(v); i < ne; i++) e[i] = gcp(g, e[i], p0, t0); } break; }
  case DNom:   { nom(p)->name = gcp(g, nom(p)->name, p0, t0); break; }
  default: break;                                  // DMint/DString/DBig/DGem/DSun/DTwin: pointer-free leaves
 } else { for (union u *q = p; !tagl(g, q->x); q++) q->x = gcp(g, q->x, p0, t0); } }   // a thread: every word to the tag terminator (tagl: head in any live pool)
          // INCLUDING word0 -- a normal thread's ap is out-of-pool (gcp no-op) but a task-ring node's
          // word0 is its `next` pointer, the very old->young edge the rem set exists to chase.

// relocate finalizer nodes out of the dead minor into the major. a minor never
// RUNS a finalizer; that waits for a major's compact.
static void gen_fz_relocate(struct ai *g) {
 struct ai_fz **link = &g->fz;
 for (struct ai_fz *fz = *link; fz; ) {
  struct ai_fz *next = fz->next;
  if ((word*) fz >= (word*) g->end && (word*) fz < g->hp) {   // node was in the minor -> relocate
   struct ai_fz *nn = bump(g, Width(struct ai_fz));           // gc_gen set -> major
   nn->p = fz->p, nn->fn = fz->fn, nn->next = next;
   *link = nn, link = &nn->next;
  } else link = &fz->next;
  fz = next; } }

// the weak-table sweep + finalizer pass of a MAJOR's compact: symbols_rebuild /
// run_finalizers, but bumping into the major to-space and testing survival against gc_to_{lo,hi}
static word major_symbols_rebuild(struct ai *g, word om) {
 if (!om) return 0;
 uintptr_t cap = map_cap(om), mask = cap - 1, n = 0;
 union u *b = map_fill_back(bump(g, 4 + 2 * cap), cap), *hd = bump(g, 3);
 hd[0].ap = lvm_map_lookup, hd[1].x = (word) b, tagthread(hd, 2);
 word *os = map_slots(om), *ns = &b[3].x;
 word const *lo = g->gc_to_lo, *hi = g->gc_to_hi;
 for (uintptr_t j = 0; j < cap; j++) {
  word k = os[2 * j];
  if (k == map_gap) continue;
  word e = os[2 * j + 1], fwd;
  // a pinned atom survives without a forward: the prefix is not copied, so word0 is
  // untouched and the test below would read every frozen symbol as dead.
  if (g->froze_lo && ptr(e) >= g->froze_lo && ptr(e) < g->froze_hi)
   fwd = (word)(g->gc_to_lo + (ptr(e) - g->froze_lo));
  else {
   fwd = cell(e)->x;                            // the atom's first word: its forward, if it survived
   if (!(lamp(fwd) && lo <= ptr(fwd) && ptr(fwd) < hi)) continue; }
  word nk = nom(fwd)->name;
  uintptr_t i = hash(g, nk) & mask;
  while (ns[2 * i] != map_gap) i = (i + 1) & mask;
  ns[2 * i] = nk, ns[2 * i + 1] = fwd, n++; }
 b[1].x = putcharm(n);
 return (word) hd; }
static void major_run_finalizers(struct ai *g) {
 struct ai_fz *new_fz = NULL;
 for (struct ai_fz *fz = g->fz; fz; fz = fz->next) {
  word fwd = fz->p->x;
  if (g->froze_lo && (word*) fz->p >= g->froze_lo && (word*) fz->p < g->froze_hi)
   fwd = (word)(g->gc_to_lo + ((word*) fz->p - g->froze_lo));   // pinned: alive, and it did not move (symbols_rebuild's rule)
  if (lamp(fwd) && g->gc_to_lo <= ptr(fwd) && ptr(fwd) < g->gc_to_hi) {
   struct ai_fz *nn = bump(g, Width(struct ai_fz));
   nn->p = cell(fwd), nn->fn = fz->fn, nn->next = new_fz, new_fz = nn;
  } else fz->fn(fz->p); }
 g->fz = new_fz; }

// AiGcStress's two numbers: an EVEN poison, so a stale read faults at an address
// a backtrace can name; and how often a forced collection is a MAJOR (gen_please).
#define ai_gc_poison ((word) 0xd0d0d0d0d0d0d0d0ULL)
#define ai_gc_stress_major 32

// the MINOR: evacuate [end, hp) into the major active half, reset hp = end. the
// cheney scan starts at the append point, walking only fresh survivors; gc_fwd
// tells a forward made THIS collection from a pointer to a pre-existing major object.
static void gen_minor(struct ai *g) {
 ai_image_note(0x33);
 word const *p0 = (word const*) g->end, *t0 = g->hp;          // minor from-range
 g->gc_gen = 1, g->gc_f2lo = 0;
 g->gc_to_lo = g->major_base, g->gc_to_hi = g->major_base + g->major_len;
 g->gc_fwd = g->major_hp;
 g->cp = g->major_hp;
 g->ip = cell(gcp(g, word(g->ip), p0, t0));
 g->tasks = cell(gcp(g, word(g->tasks), p0, t0));
 if (g->parked) g->parked = cell(gcp(g, word(g->parked), p0, t0));   // the parked ring is its own root
 for (word i = 0; i < g->end - &g->v0; i++) (&g->v0)[i] = gcp(g, (&g->v0)[i], p0, t0);   // core vars
 for (word *s = g->sp; s < topof(g); s++) *s = gcp(g, *s, p0, t0);                       // stack
 for (struct ai_r *r = g->root; r; r = r->n) *r->x = gcp(g, *r->x, p0, t0);              // C roots
 // the weak intern map is its own field, not a root: promote its STRUCTURE by hand
 // (entries stay weak -- a major drops dead atoms). young header: gcp it; tenured:
 // scan its possibly-young backing in place.
 if (g->symbols) {
  if (ai_young(g, g->symbols)) g->symbols = gcp(g, g->symbols, p0, t0);
  else gen_scan_inplace(g, g->symbols, p0, t0), gen_scan_inplace(g, map_back(g->symbols), p0, t0);
 }
 for (uintptr_t i = 0; i < g->rem_n; i++) gen_scan_inplace(g, g->rem[i], p0, t0);        // major->young edges
 for (struct ai_fz *fz = g->fz; fz; fz = fz->next) fz->p = cell(gcp(g, word(fz->p), p0, t0));
 while (g->cp < g->major_hp) (datp(g->cp) ? evac_data : evac_thread)(g, p0, t0);
#ifdef AiGcCheck
 // the fixpoint IS a fixpoint (gc.v drain_second_pass_copies_nothing): re-drive the
 // whole scan; every gcp must be an identity. if major_hp moves, the first pass LOST
 // a reachable object -- trap at the collection that lost it. (make test_gcheck)
 { word *hp1 = g->major_hp;
  g->cp = (word*) g->gc_fwd;
  g->ip = cell(gcp(g, word(g->ip), p0, t0));
  g->tasks = cell(gcp(g, word(g->tasks), p0, t0));
  if (g->parked) g->parked = cell(gcp(g, word(g->parked), p0, t0));
  for (word i = 0; i < g->end - &g->v0; i++) (&g->v0)[i] = gcp(g, (&g->v0)[i], p0, t0);
  for (word *s = g->sp; s < topof(g); s++) *s = gcp(g, *s, p0, t0);
  for (struct ai_r *r = g->root; r; r = r->n) *r->x = gcp(g, *r->x, p0, t0);
  if (g->symbols) {
   if (ai_young(g, g->symbols)) g->symbols = gcp(g, g->symbols, p0, t0);
   else gen_scan_inplace(g, g->symbols, p0, t0), gen_scan_inplace(g, map_back(g->symbols), p0, t0);
  }
  for (uintptr_t i = 0; i < g->rem_n; i++) gen_scan_inplace(g, g->rem[i], p0, t0);
  for (struct ai_fz *fz = g->fz; fz; fz = fz->next) fz->p = cell(gcp(g, word(fz->p), p0, t0));
  while (g->cp < g->major_hp) (datp(g->cp) ? evac_data : evac_thread)(g, p0, t0);
  if (g->major_hp != hp1) __builtin_trap(); }
#endif
 if (g->fz) gen_fz_relocate(g);
 g->hp = g->end;                                              // minor emptied
#ifdef AiGcStress
 // ⚠ poison the vacated nursery, or the stress build is half a detector: a stale
 // local otherwise reads a forwarding pointer that still looks live. last thing
 // here -- gen_fz_relocate is the from-space's last reader.
 for (word *p = (word*) p0; p < (word*) t0; p++) *p = ai_gc_poison;
#endif
 g->gc_gen = 0; }

// the MAJOR: one cheney pass from the real roots over BOTH from-spaces into the
// spare half -- reachability, never a linear sweep, which is why a rem-set overflow
// forces one. then rebuild the intern map, run finalizers, flip, reset the minor.
static struct ai *gen_major(struct ai *g) {
 ai_image_note(0x34);
 word const *p0 = g->major_base, *t0 = g->major_hp;              // from-range 1: major active
 // size the to-space for the worst case: all of major-active AND all of the minor survive
 uintptr_t used = (uintptr_t)(g->major_hp - g->major_base), young = (uintptr_t)(g->hp - (word*) g->end);
 uintptr_t need = used + young;
 // grow/shrink by a whole STEP (= ai_major0), need + 25% headroom: one step at a
 // time prevents thrash, and snapping DOWN reclaims floated dead promotions.
 uintptr_t step = g->major0, want = need + (need >> 2) + 16;
 uintptr_t to_len = ((want + step - 1) / step) * step;
 if (to_len < step) to_len = step;
 uintptr_t need_step = ((need + step - 1) / step) * step;       // the TIGHT size: smallest step-multiple holding `need`
 if (need_step < step) need_step = step;
 // budget cap: keep the major pair within its share, but NEVER below need_step (the
 // to-space must hold the worst-case promotion); too small falls through to the OOM path
 if (g->budget) {
  uintptr_t cap = g->budget > 2 * (uintptr_t) g->len ? (g->budget - 2 * (uintptr_t) g->len) / 2 : 0;
  if (to_len > cap) to_len = cap > need_step ? (cap / step) * step : need_step; }
 word *spare = (g->major_base == g->major_pool) ? g->major_pool + g->major_len : g->major_pool;  // the same-size other half
 word *to, *resized = 0;
 if (to_len != g->major_len) {                                 // a different-size pair: alloc it, free the old
  resized = g->alloc(g, NULL, 2 * to_len * sizeof(word));
  if (!resized && to_len > need_step)                          // the headroom alloc failed: retry at the TIGHT size
   to_len = need_step, resized = (need_step == g->major_len) ? 0 : g->alloc(g, NULL, 2 * need_step * sizeof(word));
  if (resized) to = resized;
  else if (need <= g->major_len) to_len = g->major_len, to = spare;   // alloc failed, but the existing spare half holds the live set
  else return g->gc_gen = 0, encode(g, ai_status_scare);             // true OOM: compacting would overflow the spare -> clean scare, no corruption
 } else to = spare;
 g->gc_gen = 1;
 // the pinned prefix rides across verbatim at the same offsets. the scan starts below it
 // so its words are still rewritten in place -- a frozen object may point at something
 // new -- and gc_fwd starts past it, since nothing in it is copied or forwarded.
 uintptr_t froze = g->froze;
 if (froze) {
  memcpy(to, g->major_base, froze * sizeof(word));
  g->froze_lo = g->major_base, g->froze_hi = g->major_base + froze;
  // a live finalizer node is three raw words with no header, so the scan below cannot
  // stride it. nothing reaches the copies -- run_finalizers bumps a fresh list past the
  // block -- so forge each into a dead chain of the same width.
  for (struct ai_fz *z = g->fz; z; z = z->next)
   if ((word*) z >= g->froze_lo && (word*) z < g->froze_hi) {
    word *c = to + ((word*) z - g->froze_lo);
    c[0] = (word) lvm_chain, c[1] = c[2] = ZeroPoint; }
  // the intern map's pinned copy is ballast the moment the rebuild below re-homes it,
  // holding whatever the last intern wrote -- GC timing, not program state. leave the
  // husk canonically EMPTY before the scan can trace it: the bytes ride every later
  // image verbatim, and dead entries must not pin their atoms.
  if (g->symbols) {
   word *hc = (word*) cell(g->symbols), *bc = (word*) cell(map_back(g->symbols));
   if (hc >= g->froze_lo && hc < g->froze_hi) (to + (hc - g->froze_lo))[1] = ZeroPoint;
   if (bc >= g->froze_lo && bc < g->froze_hi) {
    word *c = to + (bc - g->froze_lo);
    uintptr_t bcap = getcharm(c[2]);
    c[1] = putcharm(0);
    for (uintptr_t j = 0; j < bcap; j++) c[3 + 2 * j] = map_gap, c[4 + 2 * j] = zero; } } }
 g->major_hp = to + froze, g->cp = to;
 g->gc_to_lo = to, g->gc_to_hi = to + to_len, g->gc_fwd = to + froze;   // fresh to-space: every copy is a forward
 g->gc_f2lo = (word*) g->end, g->gc_f2hi = g->hp;            // from-range 2: the minor (promote young in the same pass)
 g->ip = cell(gcp(g, word(g->ip), p0, t0));
 g->tasks = cell(gcp(g, word(g->tasks), p0, t0));
 if (g->parked) g->parked = cell(gcp(g, word(g->parked), p0, t0));   // the parked ring is its own root
 for (word i = 0; i < g->end - &g->v0; i++) (&g->v0)[i] = gcp(g, (&g->v0)[i], p0, t0);
 for (word *s = g->sp; s < topof(g); s++) *s = gcp(g, *s, p0, t0);
 for (struct ai_r *r = g->root; r; r = r->n) *r->x = gcp(g, *r->x, p0, t0);
 word om = g->symbols; g->symbols = 0;                       // weak: rebuilt after the fixpoint
 while (g->cp < g->major_hp) (datp(g->cp) ? evac_data : evac_thread)(g, p0, t0);
 g->symbols = major_symbols_rebuild(g, om);
 major_run_finalizers(g);
 g->gc_f2lo = g->gc_f2hi = 0;                                // the minor range is consumed
 if (resized) g->alloc(g, g->major_pool, 0), g->major_pool = resized, g->major_len = to_len;
 g->major_base = to;                                           // flip: active = the to-space
 g->hp = g->end;                                             // the minor's young was promoted: reset it
#ifdef AiGcStress
 // poison the promoted young, like the minor. ⚠ the old major half does NOT -- a
 // measurement (ten minutes on a 24-second lane), and the least needed: a cheney
 // copy left a forwarding pointer in word0 (the ap), which faults on dispatch.
 for (word *p = (word*) g->end; p < (word*) g->end + young; p++) *p = ai_gc_poison;
#endif
 g->froze_lo = g->froze_hi = NULL;                              // the window is one major's; a minor must not see it
 // the rem set dies with the half it named: a major promotes every survivor, so there is
 // no old->young edge left to remember and every address in it points into a half about
 // to be reused. cleared here rather than in gen_please alone, because a major can be
 // called directly -- the image dump compacts before it serializes.
 g->rem_n = 0, g->rem_miss = 0;
 return g->gc_gen = 0, g; }

// resize the MINOR pool, decoupled from the major. called right after a collection,
// so the minor is EMPTY: only the core + stack move; the major + intern map ride
// through untouched (() is ZeroPoint, so nothing points at the moving core).
static struct ai *gen_grow(struct ai *g, uintptr_t len1) {
 struct ai *h = g->alloc(g, NULL, len1 * 2 * sizeof(word));
 if (!h) return encode(g, ai_status_scare);
 memcpy(h, g, sizeof(struct ai));
 h->pool = (void*) h, h->len = len1;
 word const *p0 = ptr(g), *t0 = ptr(g) + g->len, *sp0 = g->sp;
 word sh = t0 - sp0;
 h->sp = ptr(h) + len1 - sh;
 h->hp = h->cp = h->end;                     // core moves to h; no (word)g root to forward (() is the const ZeroPoint)
 h->gc_gen = 0, h->gc_to_lo = ptr(h), h->gc_to_hi = ptr(h) + len1, h->gc_fwd = ptr(h), h->gc_f2lo = 0;
 h->ip = cell(gcp(h, word(h->ip), p0, t0));
 h->tasks = cell(gcp(h, word(h->tasks), p0, t0));
 if (h->parked) h->parked = cell(gcp(h, word(h->parked), p0, t0));
 // h->symbols + the major were memcpy'd and live outside [p0,t0): untouched, NOT rebuilt
 for (word i = 0; i < h->end - &h->v0; i++) (&h->v0)[i] = gcp(h, (&h->v0)[i], p0, t0);   // core vars
 for (word n = 0; n < sh; n++) h->sp[n] = gcp(h, sp0[n], p0, t0);                        // stack
 for (struct ai_r *s = h->root; s; s = s->n) *s->x = gcp(h, *s->x, p0, t0);              // C roots
 while (h->cp < h->hp) (datp(h->cp) ? evac_data : evac_thread)(h, p0, t0);               // heap empty -> ~nothing
 h->minor = h->end;
 h->n_resize += 1;
 if (h->len > h->max_len) h->max_len = h->len;
 g->alloc(g, g->pool, 0);                    // free the old main pool
 return h; }

// the GC entry: a MINOR unless the rem set overflowed or the major lacks headroom --
// then a MAJOR. afterwards size the minor by appel's rule against the budget.
static struct ai *gen_please(struct ai *g, uintptr_t req0) {
 ai_image_note(0x32);
 uintptr_t seen_young = (uintptr_t)(g->hp - g->end);
 uintptr_t major_free = (uintptr_t)((g->major_base + g->major_len) - g->major_hp);
 g->since_major += seen_young;                                  // young allocated (∝ scanned) since the last major
 // a MAJOR: forced by rem-set overflow, by the major lacking room for a worst-case
 // promotion, or by the amortization rule -- live set + 4 minor-pools allocated since
 // the last one -- so floating dead tenured objects sweep and the pool can shrink.
 bool major = g->rem_miss
   || major_free < (uintptr_t) g->len + req0 + 16
   || g->since_major > g->major_live0 + 4 * (uintptr_t) g->len;
#ifdef AiGcStress
 // ⚠ a minor is not enough: stress-collecting tenures everything almost at once,
 // and a minor never moves the tenured -- the detector answered green on its own
 // control. every-collection-major cost a 458 s boot, so a major rides every Nth
 // collection instead: deterministic, off g->n_gc. coverage is the stated trade.
 major = major || g->n_gc % ai_gc_stress_major == 0;
#endif
 word *before = g->major_hp;
 if (major) {
  if (!ai_ok(g = gen_major(g))) return g;     // a true OOM mid-major (compacting would overflow the spare): propagate the scare
  g->n_gc += 1;
  g->since_major = 0, g->major_live0 = (uintptr_t)(g->major_hp - g->major_base);   // reset the amortization window
 } else gen_minor(g), g->n_gc += 1, g->n_minor += 1;
 uintptr_t copied = major ? (uintptr_t)(g->major_hp - g->major_base) : (uintptr_t)(g->major_hp - before);
 g->n_seen += seen_young;
 g->n_evac += copied;
 if (major) { if (copied > g->major_hi) g->major_hi = copied; }
 else if (copied > g->minor_hi) g->minor_hi = copied;
 g->rem_n = 0, g->rem_miss = 0;
 { uintptr_t e = (uintptr_t)(g->major_hp - g->major_base); if (e > g->max_heap) g->max_heap = e; }
 // MINOR resize, deterministic (words copied / words allocated -- no wall clock, so
 // the schedule is reproducible): keep the copy overhead inside a band, accumulated
 // over a sliding window; ai_budget caps the footprint by appel's rule.
 uintptr_t const ratio = g->ratio;              // target band: grow above 1/ratio overhead, shrink below 1/(4*ratio)
#ifdef AiGcStress
 // ⚠ the band is meaningless on a forced schedule (`allocated` ~0 -> the nursery
 // doubles every collection, a 256 MB oom); the HARD FLOOR stays -- it guarantees
 // the pending allocation fits, and skipping it reads like a runtime bug.
 { uintptr_t used0 = g->len - avail(g), req = req0 + used0 + (used0 >> 2);
   return req <= (uintptr_t) g->len ? g : gen_grow(g, req); }
#endif
 g->win_alloc += seen_young, g->win_copied += copied;
 uintptr_t used = g->len - avail(g), req = req0 + used + (used >> 2), len1 = g->len, arena = len1;
 // resize STICKINESS: act only on two consecutive same-way windows (lean tracks the
 // streak; in-band ends it). a resize is the costliest single act (fresh pool, full
 // copy, every page refaulted), and a hair-trigger flapped workloads near a band
 // edge. a hint holds the size and KEEPS ACCUMULATING: a spike self-corrects, a real
 // ramp confirms next collection. (measured out, do not revisit: first-verdict-with-
 // reset taxes ramps ~4%; excluding majors from the window storms them, +70% wall.)
 if (g->win_copied * ratio > g->win_alloc) {                   // overhead > 1/ratio: nursery too small
  if ((g->lean = g->lean > 0 ? g->lean + 1 : 1) >= 2) {        // confirmed: grow
   uintptr_t wa = g->win_alloc | 1;                            // grow until the PROJECTED overhead lands in band (| 1: guarantee progress)
   while (g->win_copied * ratio > wa) arena <<= 1, wa <<= 1;    // (doubling the pool ~doubles alloc-between-GCs)
   g->lean = 0, g->win_alloc = g->win_copied = 0; }
 } else if (g->win_copied * (ratio * 4) < g->win_alloc) {       // overhead < 1/(4*ratio): oversized
  if ((g->lean = g->lean < 0 ? g->lean - 1 : -1) <= -2)
   arena = len1 >> 1, g->lean = 0, g->win_alloc = g->win_copied = 0;   // shrink ONE step (gentle -- multi-step collapses on a lucky GC)
 } else if (g->win_alloc > 8 * len1) g->win_alloc = g->win_copied = 0, g->lean = 0;   // in band: cap the window; the streak dies
 if (g->budget) {
  // appel cap, reserving room for the major that must hold the worst-case promotion
  // (live + this whole nursery): the nursery gets ~(budget - 2*live)/4
  uintptr_t lv = 2 * g->major_live0, room = g->budget > lv ? (g->budget - lv) / 4 : 0;
  if (arena > room) arena = room; }
 if (arena < g->minor0) arena = g->minor0;                     // floor
 if (arena < req) arena = req;                                 // hard floor: hold the pending allocation
 return arena == len1 ? g : gen_grow(g, arena); }

ai_noinline struct ai *ai_please(struct ai *g, uintptr_t req0) {
 return gen_please(g, req0); }   // generational ONLY: a minor (or major) into the major pool that ai_ini_0 guarantees

static ai_inline word copy_chain(struct ai*g, struct ai_chain *src, word const *const p0, word const *const t0) {
 struct ai_chain *dst = bump(g, Width(struct ai_chain));
 ini_chain(dst, src->a, src->b);
 src->ap = (lvm_t*) dst;
 return word(dst); }

static ai_inline word copy_tray(struct ai*g, struct ai_tray *src, word const *const p0, word const*const t0) {
 uintptr_t bytes = ai_tray_bytes(src);
 struct ai_tray *dst = bump(g, b2w(bytes));
 src->ap = memcpy(dst, src, bytes);
 return word(dst); }

static ai_inline word copy_str(struct ai*g, struct ai_str *src, word const *const p0, word const*const t0) {
 uintptr_t bytes = sizeof(struct ai_str) + src->len;
 struct ai_str *dst = bump(g, b2w(bytes));
 src->ap = memcpy(dst, src, bytes);
 return word(dst); }

// bignums and the lean boxes are flat: one memcpy, like strings
static ai_inline word copy_big(struct ai*g, struct ai_big *src, word const *const p0, word const*const t0) {
 uintptr_t bytes = ai_big_bytes(src);
 struct ai_big *dst = bump(g, b2w(bytes));
 src->ap = memcpy(dst, src, bytes);
 return word(dst); }

static ai_inline word copy_gem(struct ai*g, struct ai_gem *src, word const *const p0, word const*const t0) {
 struct ai_gem *dst = bump(g, gem_req);
 src->ap = memcpy(dst, src, sizeof(struct ai_gem));
 return word(dst); }

static ai_inline word copy_sun(struct ai*g, struct ai_sun *src, word const *const p0, word const*const t0) {
 struct ai_sun *dst = bump(g, sun_req);
 src->ap = memcpy(dst, src, sizeof(struct ai_sun));
 return word(dst); }

static ai_inline word copy_twin(struct ai*g, struct ai_twin *src, word const *const p0, word const*const t0) {
 struct ai_twin *dst = bump(g, twin_req);
 src->ap = memcpy(dst, src, sizeof(struct ai_twin));
 return word(dst); }

// atoms copy like any object; interning maintenance is the post-fixpoint table sweep's
static ai_inline word copy_sym(struct ai*g, struct ai_mint *src, word const *const p0, word const*const t0) {
 struct ai_mint *dst = bump(g, Width(struct ai_mint));
 (void) p0, (void) t0;                            // a mint carries no name to forward now
 ini_missing(dst, src->code);                     // just the serial rides
 return word(src->ap = (lvm_t*) dst); }

static ai_inline word copy_nom(struct ai*g, struct ai_nom *src, word const *const p0, word const*const t0) {
 struct ai_nom *dst = bump(g, Width(struct ai_nom));
 (void) p0, (void) t0;                            // shallow: evac_nom forwards the name later (Cheney)
 ini_nom(dst, src->name, src->code, src->dig);    // name copied raw, serial + dig ride
 return word(src->ap = (lvm_t*) dst); }

static ai_inline word copy_data(struct ai *g, union u *src, word const *const p0, word const *const t0) {
 switch (typ(src)) {
  case DChain: return copy_chain(g, two(src), p0, t0);
  case DTray: return copy_tray(g, tray(src), p0, t0);
  case DMint: return copy_sym(g, sym(src), p0, t0);
  case DNom: return copy_nom(g, nom(src), p0, t0);
  case DString: return copy_str(g, str(src), p0, t0);
  case DBig: return copy_big(g, big(src), p0, t0);
  case DGem: return copy_gem(g, (struct ai_gem*) src, p0, t0);
  case DSun: return copy_sun(g, (struct ai_sun*) src, p0, t0);
  case DTwin: return copy_twin(g, (struct ai_twin*) src, p0, t0); }
 __builtin_trap(); }

static ai_inline struct ai_tag *ttag2(struct ai *g, union u *k) {
 while (!tagl(g, k->x)) k++;                                 // tagl: terminator head in any live pool
 return (struct ai_tag*) k; }

static ai_inline word copy_thread(struct ai *g, union u *src, word const *const p0, word const *const t0) {
 // it's a thread, find the end to find the head
 struct ai_tag *t = ttag2(g, src);
 union u *ini = tag_head(t), *d = bump(g, t->end - ini), *dst = d;
 // copy each content word to dest and leave a forwarding pointer behind,
 // stopping at the terminator; then rewrite it as the new tagged head
 for (union u *s = ini; !tagl(g, s->x); s->x = (word) d, d++, s++) d->x = s->x;
 return (word) (tagthread(dst, d - dst) + (src - ini)); }

static ai_noinline intptr_t gcp(struct ai *g, word x, word const *p0, word const *t0) {
 // a number stays; else find which from-space range holds x (a major traces two),
 // so copy_thread's terminator scan uses x's own home
 if (charmp(x)) return x;
 // the pinned prefix is not traced: it is memcpy'd to the head of the to-space, so its
 // answer is arithmetic and leaves no forwarding pointer. froze_lo is 0 otherwise.
 if (g->froze_lo && ptr(x) >= g->froze_lo && ptr(x) < g->froze_hi)
  return (word)(g->gc_to_lo + (ptr(x) - g->froze_lo));
 word const *lo = p0, *hi = t0;
 if (!(ptr(x) >= lo && ptr(x) < hi)) {
  if (g->gc_f2lo && ptr(x) >= g->gc_f2lo && ptr(x) < g->gc_f2hi) lo = g->gc_f2lo, hi = g->gc_f2hi;
  else return x;
 }
 union u *src = cell(x);
 x = src->x; // get its contents
 // if it contains a pointer to the new space then return the pointer (already forwarded)
 word const *flo = g->gc_fwd, *fhi = g->gc_to_hi;   // forwarding window of THIS collection (major/spare/new pool)
 return lamp(x) && flo <= ptr(x) && ptr(x) < fhi ? x :
        in_data((void*) x) ? copy_data(g, src, lo, hi) :
                                copy_thread(g, src, lo, hi); }

// ============================================================================
// ev
// ============================================================================
static ai_inline struct ai *pushl(struct ai*g) { return intern(ai_strof(g, "\\")); }
static struct ai *c0(struct ai *g, lvm_t *y);
static struct ai *ai_eval(struct ai *g);

// function state using this type
struct env {
 struct env *par; // enclosing scope
 word args, imps, // positional and closure variables
  stack, // computed arguments and let bindings on stack
  lams, // lambdas defined in a local let form
  len,  // thread length accumulator
  branches, // stack for conditional alternate branch addresses
  exits,
  sites, // recursive-fn ref backpatch: list of (lams-entry . operand-cell)
  src,  // a lambda's source \-expr, stashed at the thread head for printing (zero = none)
  fars, // a let's binding NAMES, pinned before its lambdas compile: the shadow set
  end[]; }; // stach for conditional exit addresses

typedef Ana(ana);
typedef Cata(cata);
static ana analyze, ana_d, ana_c, ana_l, ana_q, ana_ap;
static Ana(ana_2, word, word);
static cata c1_i, c1_ix, c1_var, c1_yield, c1_ret, c1, c1_recv;
static ai_inline Cata(pull) { return ai_ok(g) ? ((cata*) pop1(g))(g, c) : g; }

// generic instruction ana aps
static ai_inline struct ai *c0_ix(struct ai *g, struct env **c, lvm_t *i, word x) {
 return incl(*c, 2), ai_push(g, 3, c1_ix, i, x); }

static ai_inline struct ai *c0_i(struct ai *g, struct env **c, lvm_t *i) {
 return incl(*c, 1), ai_push(g, 2, c1_i, i); }

static struct ai *enscope(struct ai *g, struct env *par, word args, word imps) {
 uintptr_t const n = Width(struct env) + Width(struct ai_tag);
 g = ai_push(g, 3, args, imps, par);
 if (ai_ok(g = ai_have(g, n))) {
  struct env *c = bump(g, n);
  c->stack = c->branches = c->exits = c->lams = c->len = c->sites = c->src = c->fars = zero;
  c->args = g->sp[0], c->imps = g->sp[1], c->par = (struct env*) g->sp[2];
  *(g->sp += 2) = (word) tagthread((union u*)c, Width(struct env)); }
 return g; }

static word memq(struct ai *g, word l, word k) {
 for (; chainp(l); l = B(l)) if (eql(g, k, A(l))) return l;
 return 0; }

static word assq(struct ai *g, word l, word k) {
 for (; chainp(l); l = B(l)) if (eql(g, k, AA(l))) return A(l);
 return 0; }

static struct ai *append(struct ai *g) {
 uintptr_t i = 0;
 for (word l; ai_ok(g) && chainp(g->sp[0]); i++)
  l = B(g->sp[0]),
  g->sp[0] = A(g->sp[0]),
  g = ai_push(g, 1, l);
 if (!ai_ok(g)) return g;
 if (i == 0) return g->sp++, g;
 for (g->sp[0] = g->sp[i + 1]; i--; g = gxr(g));
 if (ai_ok(g)) g->sp[1] = g->sp[0], g->sp++;
 return g; }

// don't inline this so callers can tail call optimize
static ai_noinline struct ai *c0(struct ai *g, lvm_t *y) {
 // every in-place store below is precisely barriered (gen_wb_cell/two), so a
 // mid-compile collection stays MINOR. the opfix prepass runs first; a chain whose
 // head is already a top is a constructed direct application (never readable
 // source): skipped, which also terminates the recursion through ai_eval.
 { word x0 = g->sp[0];
   if (chainp(x0) && (!lamp(A(x0)) || datp(A(x0)))) {
    word of = ai_core_of(g)->hot_opfix;          // sealed: a book rebind can't reach this lane;
    if (lamp(of)) {                              // pre-seal (mid-prel bootstrap) it is zero and
                                                 // the pass skips -- everything there is prefix
     g = ai_eval(gxr(gxl(gxl(pushq(gxl(ai_push(g, 4, x0, zero, zero, of)))))));
     if (!ai_ok(g)) return g;
     g->sp[1] = g->sp[0], g->sp += 1; } } }
 if (!ai_ok(g = enscope(g, (struct env*) zero, zero, zero))) return g;
 struct env *c = (void*) ptr(pop1(g));
 word x = g->sp[0];
 g->sp[0] = (word) c1_yield;
 mm(g, &c); mm(g, &x);
 if (ai_ok(g = analyze(g, &c, x)))
   g = c1(c0_ix(g, &c, y, word(g->ip)), &c);
 um(g), um(g);
 return g; }

static Cata(c1) {
 uintptr_t l = getcharm((*c)->len);
 // a lambda carries its source \-expr: reserve one extra leading word for it so
 // it sits at value[-1] (the printer's discriminator) and rides inside the thread
 // span (head = src word) for free GC tracing. top-level/aux threads have no src.
 uintptr_t extra = zerop((*c)->src) ? 0 : 1;
 g = ai_have(g, l + extra + Width(struct ai_tag));
 if (ai_ok(g)) {
  union u *k = bump(g, l + extra + Width(struct ai_tag));
  memset(k, -1, (l + extra) * sizeof(word));
  Kp = tagthread(k, l + extra) + l + extra;
  if (ai_ok(g = pull(g, c))) {           // pull emits l words (may GC); Kp now = entry
   // read src AFTER all allocation: ai_have/pull can GC and relocate the env's src.
   if (extra) Kp[-1].x = (*c)->src,     // value[-1] = source \-expr
              gen_wb_cell(g, Kp - 1, Kp[-1].x),
              clip(g, Kp - 1);          // tag head spans [src .. body]; value stays Kp
   else clip(g, Kp); } }
 return g; }

static Cata(c1_yield) { return g; }

static Cata(c1_cond_pop_exit) { return
 (*c)->exits = B((*c)->exits), // pops cond expression exit address off env stack exits
 gen_wb_cell(g, &(*c)->exits, (*c)->exits),
 pull(g, c); }

static Cata(c1_apn) {
 word arity = pop1(g);
 if (arity == putcharm(1)) {
  if (Kp[0].ap == lvm_ret) Kp[0].ap = lvm_tap;
  else Kp -= 1, Kp[0].ap = lvm_ap; }
 else {
  if (Kp[0].ap == lvm_ret) Kp -= 1, Kp[0].ap = lvm_tapn, Kp[1].x = arity;
  else Kp -= 2, Kp[0].ap = lvm_apn, Kp[1].x = arity; }
 return pull(g, c); }


static Cata(c1_i) {
 lvm_t *i = (void*) pop1(g);
 Kp -= 1;
 Kp[0].ap = i;
 return pull(g, c); }

static Cata(c1_ix) {
 lvm_t *i = (void*) pop1(g);
 word x = pop1(g);
 Kp -= 2;
 Kp[0].ap = i;
 Kp[1].x = x;
 gen_wb_cell(g, Kp + 1, x);
 return pull(g, c); }

// Emit a recursive-function ref: bake `quote AB(y)` if the closure is final, else
// `quote zero` + stash the operand cell in the site for ana_d to backpatch.
static Cata(c1_recv) {
 word y = pop1(g), site = pop1(g);
 Kp -= 2;
 Kp[0].ap = lvm_quote;
 if (zerop(site)) Kp[1].x = AB(y), gen_wb_cell(g, Kp + 1, Kp[1].x);
 else Kp[1].x = zero, B(site) = (word) &Kp[1], gen_wb_two(g, site, B(site));
 return pull(g, c); }

static Cata(c1_ar, lvm_t *i, word ar) { return
 Kp -= 2,
 Kp[0].ap = i,
 Kp[1].x = putcharm(ar),
 pull(g, c); }

static Cata(c1_cur) {
 struct env *e = (void*) pop1(g);
 uintptr_t ar = llen(e->args) + llen(e->imps);
 return ar == 1 ? pull(g, c) : c1_ar(g, c, lvm_cur, ar); }

static Cata(c1_ret) {
 struct env *e = (struct env*) pop1(g);
 uintptr_t ar = llen(e->args) + llen(e->imps);
 return c1_ar(g, c, lvm_ret, ar); }

cata1(c1_cond_push_branch, g = gxl(ai_push(g, 2, Kp, (*c)->branches)), (*c)->branches = ai_ok(g) ? pop1(g) : zero, gen_wb_cell(g, &(*c)->branches, (*c)->branches))
cata1(c1_cond_push_exit, g = gxl(ai_push(g, 2, Kp, (*c)->exits)), (*c)->exits = ai_ok(g) ? pop1(g) : zero, gen_wb_cell(g, &(*c)->exits, (*c)->exits))
cata1(c1_cond_pop_branch, Kp -= 2, Kp[0].ap = lvm_cond, Kp[1].x = A((*c)->branches),   // Kp[1] = a same-thread address: no cross-gen edge
      (*c)->branches = B((*c)->branches), gen_wb_cell(g, &(*c)->branches, (*c)->branches))

static Cata(c1_cond_exit) {
 union u *a = cell(A((*c)->exits));
 if (a->ap == lvm_ret || a->ap == lvm_tap)
  Kp = memcpy(Kp - 2, a, 2 * sizeof(*Kp));
 else if (a->ap == lvm_tapn)
  Kp = memcpy(Kp - 3, a, 3 * sizeof(*Kp));
 else
  Kp -= 2, Kp[0].ap = lvm_jump, Kp[1].x = (word) a;
 return pull(g, c); }

static lvm(_lvm_yieldk) { return
 Ip = Ip[1].m,
 Pack(g),
 encode(g, ai_status_yield); }


// a hardware fault is a CRASH on every target: no handler, no recovery -- a fault
// means an invariant is already broken, and the immediate core dump names the site.
// (a barrier here once turned that class into a silent per-call siglongjmp storm.)
static struct ai *ai_eval(struct ai *g) {
 if (!ai_ok(g)) return g;                        // ⚠ c0 reads g->sp[0] before any guard of its own
 g = c0(g, _lvm_yieldk);
#if ai_tco
 if (ai_ok(g)) g = g->ip->ap(g, g->ip, g->hp, g->sp);
 return g;
#else
 while (ai_ok(g)) g = g->ip->ap(g);
 if (ai_code_of(g) == ai_status_eof) g = ai_core_of(g);
 return g;
#endif
}

static word lidx(struct ai*g, word x, word l) {
 word i = 0;
 for (; chainp(l); i++, l = B(l)) if (eql(g, x, A(l))) return i;
 return -1; }

static Ana(ana_v) {
 word y;
 if (!ai_ok(g)) return g;
 for (struct env *d = *c;; d = d->par) {
  if (zerop(d)) {
   if ((y = bookget(g, 0, x))) return ana_q(g, c, y);
   // undefined global: resolved by lvm_index at run time. record it as a captured
   // free variable only when nested -- at top level imps would alias an
   // uninitialized arg slot. re-read x from the imps hook: the push above can GC.
   if (!zerop((*c)->par))
    g = gxl(ai_push(g, 2, x, (*c)->imps)),
    x = ai_ok(g) ? A((*c)->imps = pop1(g)) : zero,
    gen_wb_cell(g, &(*c)->imps, (*c)->imps);
   return c0_ix(g, c, lvm_index, x); }
  // lambda definition of local let form?
  if ((y = assq(g, d->lams, x))) {
   // recursive-fn ref: record a backpatch site on d (the lams-owning scope) when
   // the closure isn't built yet, then apply the captured imports.
   word site = zero;
   if (zerop(AB(y))) {
    mm(g, &d), mm(g, &y);
    g = gxl(ai_push(g, 2, y, zero)); // site = (y . zero)
    if (ai_ok(g)) {
     g = gxl(ai_push(g, 2, g->sp[0], d->sites)); // (site . d->sites)
     if (ai_ok(g)) d->sites = pop1(g), gen_wb_cell(g, &d->sites, d->sites), site = pop1(g); }
    um(g), um(g); }
   incl(*c, 2);
   if (ai_ok(g = ai_push(g, 3, c1_recv, y, site)))
    g = ana_ap(g, c, BB(g->sp[1]));
   return g; }
  // let binding in the *current* scope -> a direct stack slot.
  if (d == *c && memq(g, d->stack, x)) return
    c0_ix(g, c, lvm_arg, putcharm(lidx(g, x, d->stack)));
  // the shadow guard: d's let BINDS x (fars) but x is not yet a lams entry or a
  // slot -- the nom is this let's, so the walk must not escape to an enclosing
  // binding of the same spelling. import it; the rebuild resolves it through lams.
  if (!zerop(d->fars) && memq(g, d->fars, x) &&
      !(!zerop(d->par) && memq(g, d->par->stack, x))) {
   if (!zerop((*c)->par))
    g = gxl(ai_push(g, 2, x, (*c)->imps)),
    x = ai_ok(g) ? A((*c)->imps = pop1(g)) : zero,
    gen_wb_cell(g, &(*c)->imps, (*c)->imps);
   return c0_ix(g, c, lvm_index, x); }
  // a let binding, closure var, or lambda arg. if from an enclosing scope, import
  // it into this scope's imps so the offset c1_var emits is valid in THIS frame.
  if (memq(g, d->stack, x) || memq(g, d->imps, x) || memq(g, d->args, x)) {
   incl(*c, 2);
   if (d != *c) // found in an enclosing scope -> import (capture) it
    g = gxl(ai_push(g, 2, x, (*c)->imps)),
    x = ai_ok(g) ? A((*c)->imps = pop1(g)) : zero,
    gen_wb_cell(g, &(*c)->imps, (*c)->imps);
   return ai_push(g, 3, c1_var, x, (*c)->stack); } } }


static Cata(c1_var) {
 word v = pop1(g), i = llen(pop1(g)); // stack inset
 for (word l = (*c)->imps; !zerop(l); l = B(l), i++)
  if (eql(g, v, A(l))) goto out;
 for (word l = (*c)->args; !zerop(l); l = B(l), i++)
  if (eql(g, v, A(l))) break;
out:
 return Kp -= 2,
        Kp[0].ap = lvm_arg,
        Kp[1].x = putcharm(i),
        pull(g, c); }

static ai_noinline Ana(analyze) {
 if (nomp(x) && x != ZeroPoint) return ana_v(g, c, x); // lookup symbol as variable
 if (!chainp(x)) return ana_q(g, c, x); // non-chains are self quoting
 word a = A(x), b = B(x);                        // it must be a chain
 // if it is a special form then do that
 struct ai_str *nm;                             // a special form is headed by a 1-char NAMED symbol (\ : ?)
 if (chainp(b) && (nm = nom_str(g, a)) && len(nm) == 1)  // ⚠ chainp: (\) (:) (?) hold no operand to
                                                // consume, so an EMPTY form is not a special form at
                                                // all -- it falls to (f) == f like every other head.
                                                // nom_str is 0 for a bare mint / the core / a non-sym
  switch (*txt(nm)) {
   case '\\': return ana_l(g, c, b);
   case ':': return ana_d(g, c, b);
   case '?': return ana_c(g, c, b); }
 return ana_2(g, c, x, a, b); }


static struct ai *c0_lambda(struct ai *g, struct env **c, intptr_t imps, intptr_t exp) {
 union u *k, *ip;
 word ops = exp;             // the full operand list (params… body) for the stored src
 struct env *d = NULL;
 mm(g, &d); mm(g, &exp); mm(g, &ops);
 g = enscope(g, *c, exp, imps);

 if (ai_ok(g)) {
  d = (struct env*) pop1(g);
  exp = d->args;
  int n = 0; // push exp args onto stack
  for (; chainp(B(exp)); exp = B(exp), n++) g = ai_push(g, 1, A(exp));
  for (g = push0(g); n--; g = gxr(g));
  exp = A(exp); }

 if (ai_ok(g)) {
  d->args = g->sp[0];
  gen_wb_cell(g, &d->args, d->args);
  g->sp[0] = (word) c1_yield;
  incl(d, 4);
  g = ai_push(g, 2, c1_cur, d);
  g = analyze(g, &d, exp);
  // stash the source \-expr for the printer AFTER analyze (imps now known),
  // prepending the imports as leading params so a closure round-trips
  if (ai_ok(g)) {
   word l = d->imps; int ni = 0;
   mm(g, &l);
   for (; chainp(l); l = B(l), ni++) g = ai_push(g, 1, A(l));  // push imp1..impN
   um(g);
   g = ai_push(g, 1, ops);                                   // tail = (params… body)
   while (ni-- > 0) g = gxr(g);                             // fold: imps ++ ops
   g = gxl(pushl(g));                                       // link '\ onto the front
   if (ai_ok(g)) d->src = pop1(g), gen_wb_cell(g, &d->src, d->src); }
  if (ai_ok(g = ai_push(g, 2, c1_ret, d)))
    ip = g->ip,
    avec(g, ip, g = c1(g, &d)); }

 if (ai_ok(g)) k = g->ip, g->ip = ip, g = gxl(ai_push(g, 2, k, d->imps));

 return um(g), um(g), um(g), g; }

static Ana(c0_cond_exit) { return
 incl(*c, 3),
 ai_push(analyze(g, c, x), 1, c1_cond_exit); }

static Ana(c0_cond_r) { return
 !chainp(x) ? c0_cond_exit(g, c, ZeroPoint) :   // clauses ran out: implicit else -> () (zero-ontology: the same () the reader terminates lists with)
 !chainp(B(x)) ? c0_cond_exit(g, c, A(x)) :
 (avec(g, x,
  incl(*c, 2),
  g = analyze(g, c, A(x)),
  g = ai_push(g, 1, c1_cond_pop_branch),
  g = c0_cond_exit(g, c, AB(x)),
  g = ai_push(g, 1, c1_cond_push_branch),
  g = c0_cond_r(g, c, BB(x))), g); }


static struct ai *ana_ap_r2l(struct ai *g, struct env **c, word x);
static struct ai *ana_ap(struct ai *g, struct env **c, intptr_t x) {
 if (!ai_ok(g)) return g;
 bool imfp =
  g->sp[0] == (word) c1_ix &&
  g->sp[1] == (word) lvm_quote &&
  lamp(g->sp[2]);
 intptr_t
  ca = llen(x),
  va =
   imfp && cell(g->sp[2])->ap == lvm_cur ?
    getcharm(cell(g->sp[2])[1].x) :
    1;
 bool b1p = ca == 1 && imfp && cell(g->sp[2])[1].ap == lvm_ret0,
      anp = va == ca && ca > 1,
      bnp = anp && cell(g->sp[2])[3].ap == lvm_ret0;

 if (b1p) { // inline an instruction
  lvm_t *i = cell(g->sp[2])->ap;
  g->sp += 3;
  g = c0_i(analyze(g, c, A(x)), c, i);
  return g; }

 if (bnp) { // inline a curried instruction
  lvm_t *i = cell(g->sp[2])[2].ap;
  g->sp += 3;
  g = c0_i(ana_ap_r2l(g, c, x), c, i); // r2l arg eval
  if (ai_ok(g)) { while (ca--) (*c)->stack = B((*c)->stack); gen_wb_cell(g, &(*c)->stack, (*c)->stack); }
  return g; }

 if (ai_ok(g = gxl(ai_push(g, 3, zero, (*c)->stack, x)))) {
  (*c)->stack = pop1(g), gen_wb_cell(g, &(*c)->stack, (*c)->stack), x = pop1(g), mm(g, &x);
  if (anp) { // r2l 1 n-ary ap
   g = ana_ap_r2l(g, c, x),
   incl(*c, 2),
   g = ai_push(g, 2, c1_apn, putcharm(ca));
   if (ai_ok(g)) { while (ca--) (*c)->stack = B((*c)->stack); gen_wb_cell(g, &(*c)->stack, (*c)->stack); } }
  else while (chainp(x)) // l2r n 1-ary ap
   g = analyze(g, c, A(x)),
   incl(*c, 2),
   g = ai_push(g, 2, c1_apn, putcharm(1)),
   x = B(x);
  um(g), (*c)->stack = B((*c)->stack), gen_wb_cell(g, &(*c)->stack, (*c)->stack); }

 return g; }


static struct ai *ana_ap_r2l(struct ai *g, struct env **c, word x) {
 if (chainp(x)) {
  word y = A(x);
  avec(g, y, g = ana_ap_r2l(g, c, B(x)));
  g = analyze(g, c, y);
  g = gxl(ai_push(g, 2, zero, (*c)->stack));
  if (ai_ok(g)) (*c)->stack = pop1(g), gen_wb_cell(g, &(*c)->stack, (*c)->stack); }
 return g; }

static ai_inline bool lambp(struct ai *g, word x) {
 struct ai_str *n;                                      // headed by the named symbol \ (nom_str 0 for a bare mint / non-sym)
 return chainp(x) && chainp(B(x)) && chainp(B(B(x))) &&
  (n = nom_str(g, A(x))) && len(n) == 1 && txt(n)[0] == '\\'; }

static ai_inline word rev(struct ai *g, word l) {
 word m, n = zero;   // reversal points each cons at its (younger) predecessor: barrier it
 while (chainp(l)) m = l, l = B(l), B(m) = n, gen_wb_two(g, m, n), n = m;
 return n; }

static word ldels(struct ai *g, word lam, word l);

// a lexically bound nom SHADOWS a macro of the same spelling (ev.l's wx/cprop
// carry the twin guard). binder rosters only -- imps may record undefined globals.
static bool lexbound(struct ai *g, struct env *d, word x) {
 for (; !zerop(d); d = d->par)
  if (memq(g, d->args, x) || memq(g, d->stack, x) ||
      memq(g, d->fars, x) || assq(g, d->lams, x)) return true;
 return false; }

static ai_inline Ana(ana_2, word a, word b) {
 if ((x = macroget(ai_core_of(g), a)) && !lexbound(g, *c, a))   // macro table = each layer's [zero] slot, walked; the scope walk only on a macro HIT
  return g = ai_eval(gxr(gxl(gxl(pushq(gxl(ai_push(g, 4, b, zero, zero, x))))))),
         analyze(g, c, ai_ok(g) ? pop1(g) : 0);
 if (!chainp(b)) return analyze(g, c, a);  // (f) == f -- BELOW the macro lane, which has no value to be
 return avec(g, b, g = analyze(g, c, a)),
        ana_ap(g, c, b); }

static ai_inline Ana(ana_q) { return c0_ix(g, c, lvm_quote, x); }
static ai_inline Ana(ana_l) {
  if (!chainp(B(x))) return ana_q(g, c, A(x)); // one operand, no params: quote
  return g = c0_lambda(g, c, zero, x),
         analyze(g, c, ai_ok(g) ? pop1(g) : 0); }
static Ana(c0_cond_r);
static ai_inline Ana(ana_c) {
 return !chainp(B(x)) ? analyze(g, c, A(x)) :
    (g = ai_push(g, 2, x, c1_cond_pop_exit),
     g = c0_cond_r(g, c, ai_ok(g) ? pop1(g) : zero),
     ai_push(g, 1, c1_cond_push_exit)); }
// this is the longest C function :(
// it handles the let special form in a way to support sequential and recursive binding.
static ai_inline struct ai *ana_d(struct ai *g, struct env **b, word exp) {
 if (!chainp(B(exp))) return analyze(g, b, A(exp));
 struct ai_r *mm0 = ai_core_of(g)->root;
 mm(g, &exp);
 // delegate the letrec*-value rewrite to the l `boxfix` prepass once that global
 // exists: forward-referenced bindings indirect through nom-keyed cells (prel.l).
 // ev.l runs the same pass in feel, so both lanes share one boxfix.
 if (ai_ok(g = intern(ai_strof(g, "boxfix")))) {
  word bf = bookget(g, 0, pop1(g));
  if (bf && lamp(bf)) {
   g = ai_eval(gxr(gxl(gxl(pushq(gxl(ai_push(g, 4, exp, zero, zero, bf)))))));
   if (ai_ok(g)) exp = pop1(g); } }
 g = enscope(g, *b, (*b)->args, (*b)->imps);
 if (!ai_ok(g)) return forget();
 struct env *q = (struct env*) pop1(g), **c = &q;
 // lots of variables :(
 word nom = zero, def = zero, lam = zero,
      v = zero, d = zero, e = zero, os = zero;
 mm(g, &nom), mm(g, &def), mm(g, &lam);
 mm(g, &d); mm(g, &e); mm(g, &v); mm(g, &q); mm(g, &os);

 // pin the let's binding names on q BEFORE any lambda compiles (the shadow set):
 // the walk must see an inner-bound nom as bound HERE while lams is still zero,
 // or it resolves to an enclosing sibling and under-applies (cf. ev.l avb's 'far guard)
 for (d = exp; chainp(d) && chainp(B(d)); d = BB(d)) {
  for (e = A(d); chainp(e) && !nomp(e); e = A(e)); // unroll (f x..) define-sugar to the name
  g = gxl(ai_push(g, 2, e, q->fars));
  if (!ai_ok(g)) return forget();
  q->fars = pop1(g), gen_wb_cell(g, &q->fars, q->fars); }

 // collect vars and defs into two lists, exposing the preceding bindings on the
 // enclosing stack so a sibling ref captures as a free variable instead of a
 // same-named global; the stack is restored before any code is emitted.
 os = (*b)->stack;
 while (chainp(exp) && chainp(B(exp))) {
  for (d = A(exp), e = AB(exp); chainp(d) && !nomp(d); e = pop1(g), d = A(d)) {  // a NAMED sym is a chain now: stop the (f x) define-sugar unroll at the name
   g = gxl(ai_push(g, 2, e, zero));
   g = append(gxl(pushl(ai_push(g, 1, B(d)))));
   if (!ai_ok(g)) return forget(); }
  g = gxl(ai_push(g, 2, d, nom));
  g = gxl(ai_push(g, 2, e, def));
  if (!ai_ok(g)) return forget();
  def = pop1(g), nom = pop1(g);
  // if it's a lambda compile it and record in lam list
  if (lambp(g, e)) {
   g = ai_push(g, 2, d, lam);
   g = gxl(gxr(c0_lambda(g, c, zero, B(e))));
   if (!ai_ok(g)) return forget();
   lam = pop1(g); }
  g = gxl(ai_push(g, 2, d, (*b)->stack)); // expose this binding to later siblings
  (*b)->stack = ai_ok(g) ? pop1(g) : zero;
  gen_wb_cell(g, &(*b)->stack, (*b)->stack);
  exp = BB(exp); }
 (*b)->stack = os, gen_wb_cell(g, &(*b)->stack, os); // restore: emission below rebuilds the real frame

 intptr_t l = llen(nom);
 bool oddp = chainp(exp),
      globp = !oddp && zerop((*b)->args); // we check this again later to make global bindings at top level
 if (!oddp) { // if there's no body then evaluate the name of the last definition
  g = gxl(ai_push(g, 2, A(nom), zero));
  if (!ai_ok(g)) return forget();
  exp = pop1(g); }

 // find closures: for each pair of bound functions, if e needs d then e needs d's variables
 word j, vars, var;
 do for (j = 0, d = lam; chainp(d); d = B(d)) // for each bound function variable
  for (e = lam; chainp(e); e = B(e)) if (d != e) // for each other bound function variable
   if (memq(g, BB(A(e)), AA(d))) // if you need this function
    for (v = BB(A(d)); chainp(v); v = B(v)) // then you need its variables
     if (!memq(g, vars = BB(A(e)), var = A(v))) // only add if it's not already there
      j++,
      g = gxl(ai_push(g, 2, var, vars)),
      BB(A(e)) = ai_ok(g) ? pop1(g) : zero,
      gen_wb_two(g, B(A(e)), BB(A(e)));
 while (j);

 // now delete defined functions from the closure variable lists
 // they will be bound lazily when the function runs
 for (e = lam; chainp(e); BB(A(e)) = ldels(g, lam, BB(A(e))), gen_wb_two(g, B(A(e)), BB(A(e))), e = B(e));

 (*c)->lams = lam, gen_wb_cell(g, &(*c)->lams, lam);
 g = append(gxl(pushl(ai_push(g, 2, nom, exp))));

 if (!ai_ok(g)) return forget();
 exp = pop1(g);

 //
 // all the code emissions are below here (??)
 //

 // clear each function's provisional closure so a ref hit mid-rebuild defers to a
 // backpatch site rather than baking the stale closure; keep the import sets (BB).
 for (d = lam; chainp(d); d = B(d)) AB(A(d)) = zero;

 for (e = nom, v = def; chainp(e); e = B(e), v = B(v))
  if (lambp(g, A(v))) {
   d = assq(g, lam, A(e));
   size_t nb = llen(BB(d)); // the import row is FROZEN here: sites already applied it
   g = c0_lambda(g, c, BB(d), BA(v));
   if (!ai_ok(g)) return forget();
   A(v) = B(d) = pop1(g), gen_wb_two(g, v, A(v)), gen_wb_two(g, d, A(v));
   if (llen(BB(d)) != nb) __builtin_trap(); } // growth = those sites under-apply (cf. ev.l weave's 'imports-grew scare)

 // closures final -> backpatch each recorded recursive-fn ref with its thread.
 for (d = (*c)->sites; chainp(d); d = B(d))
  cell(B(A(d)))->x = AB(A(A(d))), gen_wb_cell(g, cell(B(A(d))), AB(A(A(d))));
 (*c)->sites = zero;

 nom = rev(g, nom); // put in literal order
 g = analyze(g, b, exp);
 g = gxl(ai_push(g, 2, zero, e = (*b)->stack)); // push function stack rep
 (*b)->stack = ai_ok(g) ? pop1(g) : zero;
 gen_wb_cell(g, &(*b)->stack, (*b)->stack);
 for (def = rev(g, def); chainp(nom); nom = B(nom), def = B(def))
  g = analyze(g, b, A(def)),
  g = globp ? c0_ix(g, b, lvm_defglob, A(nom)) : g,
  g = gxl(ai_push(g, 2, A(nom), (*b)->stack)),
  (*b)->stack = ai_ok(g) ? pop1(g) : zero,
  gen_wb_cell(g, &(*b)->stack, (*b)->stack);
 return
  (*b)->stack = e, gen_wb_cell(g, &(*b)->stack, e),
  incl(*b, 2),
  g = ai_push(g, 2, c1_apn, putcharm(l)),
  forget(); }

static word ldels(struct ai *g, word lam, word l) {
 if (!chainp(l)) return zero;
 word m = ldels(g, lam, B(l));
 if (!assq(g, lam, A(l))) B(l) = m, m = l;
 return m; }

lvm(lvm_defglob) {
 Have(3);
 Sp -= 3;
 word k = Ip[1].x, v = Sp[3];
 Sp[0] = k, Sp[1] = v, Sp[2] = A(g->book), Pack(g);          // a pin lands in the HEAD layer
 if (!ai_ok(g = ai_mapput(g))) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g), Sp += 1, Ip += 2;
 ai_musttail return Continue(); }

// lvm_index (the late-bound global read) is defined below lvm_scare: its
// miss path is the missing condition and borrows the whole help apparatus.

lvm(lvm_eval) { Ip++; Pack(g);
 if (!ai_ok(g = c0(g, lvm_jump))) ai_musttail return Ap(_lvm_ghelp, g);
 ai_musttail return Resume(); }

// ai_evals_ lives with the boot stitch it shares its machinery with, at the
// foot of the reader section.

// ============================================================================
// vm
// ============================================================================
// the hooks (love.h): lisp the C lanes reach by slot, handed over by (seal-hook n f).
// hot_hook traps on an unsealed slot -- a clean failure, never a wild read.
static ai_inline ai_word hot_hook(ai_word h) { if (!lamp(h)) __builtin_trap(); return h; }

// `+`/`*` of two functions build a new function (church add / composition) from
// hooks 2 and 3; the C aps reuse numap_drive to compute the partial.
// fixnum application dispatches to (num-ap n x): numeric x -> x**n, function x ->
// x iterated n times. the drive is [ap, ap_next, ret0] over [n, num-ap, x, ret];
// lvm_numap is the non-tail form, lvm_numtap the tail form; the fused arg/quote
// variants push their argument and bump Ip so the layout lines up, then divert.
// ap_next applies the partial to the next argument: swap the result into operator
// position, then ap -- one ap_next cell in a drive = one more curried argument.
static lvm(ap_next) {
 word t = Sp[0]; Sp[0] = Sp[1], Sp[1] = t;
 ai_musttail return Ap(lvm_ap, g); }
union u const numap_drive[] = { {lvm_ap}, {.ap = ap_next}, {.ap = lvm_ret0} };

// --- the stackless call-out bridge (the glaze call-out arc) ---
// a native blob applies clos to arg the way the VM does: build [arg, clos, RET] on
// Sp, point Ip at callout_drive, tail-jump. RET is the blob's own native resume
// point, so control returns entirely through Continue() tail-jumps -- no C frame is
// pinned across the sub-run, so a deep callee grows the VM stack, never the C stack.
static union u const callout_drive[] = { {lvm_ap}, {.ap = lvm_ret0} };
// (calloutdrive x) -> the drive's address as a fixnum: a data-segment const, so the
// glaze emitter's baked `li Ip` immediate survives an image reload (unlike a W^X pointer)
lvm(lvm_calloutdrive) { ai_musttail return Answer(putcharm((intptr_t) callout_drive)); }
// the WALKABLE resume: v1's RET was a stack-interior pointer, which a collection
// with a call-out pending fed to gcp. here the frame is [arg, clos, tag(bb - entry),
// entry] -- the resume rides as an odd charm offset (the walk skips it) plus the
// blob's raw out-of-pool base, and lvm_resume jumps base+offset. relocation-safe.
static union u const callout_resume[] = { {lvm_ap}, {.ap = lvm_resume} };
lvm(lvm_calloutresume) { ai_musttail return Answer(putcharm((intptr_t) callout_resume)); }

// ============================================================================
// the lisp help calling convention
// ============================================================================
// an installed help makes a raise the call (help a b) through help_drive
// (numap_drive's 2-arg twin) into a per-class epilogue: help_ret_more delivers the
// help's result to the raise site's resume text -- ⚠ despite the name, the
// DELIBERATE-scare lane, what makes (scare a b) and `missing` resumable; a bare
// scare is observed, then takes the default escape to C.
// ⚠ the epilogue's arithmetic is the RAISE SITE's 3-word frame [resume a b], not
// the help frame the drive consumes, so the two sizes move apart.
static lvm(help_ret_more) {   // [result resume a b ..] -> resume sees result
 Ip = cell(Sp[1]);
 Sp[3] = Sp[0];
 Sp += 3;
 ai_musttail return Continue(); }
static lvm(help_ret_scare) {  // result ignored: scares are not (yet) resumable
 return Pack(g), encode(g, ai_status_scare); }
static union u const help_more_k[] = { {help_ret_more} };
static union u const help_scare_k[] = { {help_ret_scare} };
static union u const help_drive[] =
 { {lvm_ap}, {.ap = ap_next}, {.ap = lvm_ret0} };

// raise a scare with data a/b at the heard help as (help a b); helpless (or
// still too tight after a collect) hand the scare-encoded core back to C.
// callers Pack first (ip stays at the raise site); a/b survive the collect in
// the scare_a/b stash, so the raise buys its own frame and never allocates.
static struct ai *ai_raise(struct ai *c, word a, word b, union u const *K) {
 c->scare_a = a, c->scare_b = b;  // for the exit face
 word h = c->hot_help;
 if (!ai_nilp(c, h) && avail(c) < 4) {
  struct ai *p = ai_please(c, 4);
  if (!ai_ok(p)) return encode(ai_core_of(p), ai_status_scare);
  c = ai_core_of(p);                            // moved: re-derive every pointer
  a = c->scare_a, b = c->scare_b;
  h = c->hot_help; }
 if (!ai_nilp(c, h) && avail(c) >= 4) {
  word *sp = c->sp -= 4;          // [a h b K | raise site data ..]
  sp[0] = a, sp[1] = h;
  sp[2] = b;
  sp[3] = word(K);
  c->ip = (union u*) help_drive;
#if ai_tco
  return c->ip->ap(c, c->ip, c->hp, c->sp);
#else
  return c;                       // ok-g: the trampoline dispatches help_drive
#endif
 }
 return encode(c, ai_status_scare);
}
// re-raise a failed op's scare: bare data, observe-then-terminal.
lvm(_lvm_ghelp) { return ai_raise(ai_core_of(g), zero, zero, help_scare_k); }
// (scare a b): the deliberate raise. the raise point is a clean boundary, so the
// help's result is delivered back as the value via the more continuation; helpless
// it is terminal.
lvm(lvm_scare) {
 Have1();                          // the resume push only: ai_raise buys its own frame
 word a = Sp[0], b = Sp[1];
 *--Sp = word(Ip + 1);             // [resume a b ..]: help_more_k's layout
 return Pack(g), ai_raise(g, a, b, help_more_k); }
// the missing miss sentinel: a private static address no book value can equal,
// so a name bound to zero stays distinct from no entry at all.
static union u const no_entry[1];
// the GC-free C-data emitters (defined below), forward-declared for lvm_index's helpless-miss face.
static struct ai *ioputs(struct ai*, char const*),
                 *ioputc(struct ai*, int);
static ai_inline struct ai *zflush(struct ai*);
// a helpless missing read answers ZeroPoint: absence is a POINT, not a quantity --
// a number would exponentiate under a numeral, a unit absorbs (what keeps
// (i love you) = 1). distinct from 0 and "".
// the 'missing tag, minted WHERE IT IS USED: both callers are cold, so a
// short-lived string beats a core slot or a book binding. ⚠ MAY COLLECT: Pack
// first, hold no heap local across it -- the OOM lane raises a bare scare and
// never comes through here. answers the tag, or 0 if the intern failed.
static ai_noinline word missing_tag(struct ai *g) {
 struct ai *h = intern(ai_strof(g, "missing"));
 return ai_ok(h) ? ai_pop1(h) : 0; }

// a read of the LIVE book by name -- the global twin of boxfix's (missing cell
// 'nom). a miss raises (help 'missing nom); helpless it reads the zero point.
// the site never self-patches: a later define is seen, a rebind honoured.
lvm(lvm_index) {
 Have1();                          // room for the push first (may GC; no live local held yet)
 word v = bookget(g, word(no_entry), Ip[1].x);
 if (v != word(no_entry)) return
  *--Sp = v,                       // present: push the live value, no quote patch
  Ip += 2,
  Continue();
 word h = g->hot_help;
 if (ai_nilp(g, h)) {
#if __STDC_HOSTED__
  // helpless (file mode): the zero point is silent, so surface ";; missing <nom>"
  // on err and still answer it. missing-specific -- a deliberate scare stays
  // terminal. nom_str + ioput* hold no heap operand -> no GC, so Sp/Ip survive.
  struct ai_str *nm = nom_str(g, Ip[1].x);
  if (nm) { struct ai_io *sv = g->io; g->io = &ai_stderr.io;
            struct ai *w = ioputs(g, ";; missing ");
            for (uintptr_t i = 0; ai_ok(w) && i < nm->len; i++) w = ioputc(w, nm->bytes[i]);
            if (ai_ok(w)) w = ioputc(w, '\n');
            if (ai_ok(w)) zflush(w);
            g->io = sv; }
#endif
  *--Sp = ZeroPoint; ai_musttail return Next(2); }
 Pack(g);                          // the tag is minted only on the lane that carries it
 word a = missing_tag(g);          // may collect
 if (!a) ai_musttail return Ap(_lvm_ghelp, g);   // no tag to be had: the bare scare, still packed
 Unpack(g);
 Have(3);                          // AFTER the intern: a collect here re-dispatches the
                                   // whole op, so `a` is either untouched or never read
 word b = Ip[1].x;
 Sp -= 3;
 Sp[0] = word(Ip + 2), Sp[1] = a, Sp[2] = b;   // help_more_k's layout
 return Pack(g), ai_raise(g, a, b, help_more_k); }
// the fused aps bump Ip so it points at an operand, not a re-runnable instruction --
// a plain Have() would re-dispatch into it. gc by hand and re-Ap (idempotent up to here).
#define NumapHave(self) if (Sp < Hp + 2) { \
 Pack(g); g = ai_please(g, 2); if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g); \
 Unpack(g); ai_musttail return Ap(self, g); }
static lvm(lvm_numap) {
 NumapHave(lvm_numap);
 word h = hot_hook(g->hot_numap);
 word n = Sp[1], x = Sp[0], *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = n, dst[1] = h, dst[2] = x, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; ai_musttail return Continue(); }
static lvm(lvm_numtap) {
 NumapHave(lvm_numtap);
 word h = hot_hook(g->hot_numap);
 word fs = getcharm(Ip[1].x), n = Sp[1], x = Sp[0], *dst = &Sp[fs + 2] - 3, ret = Sp[fs + 2];
 dst[0] = n, dst[1] = h, dst[2] = x, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; ai_musttail return Continue(); }

// (seal-hook n f): install f as core hook n (0 read, 1 num-ap, 2 stack, 3
// compose, 4 opfix, 5 the help, 6 the task's stdio -- the two DYNAMIC slots, which
// alone skip the lambda gate: 5 takes a help or (), 6 a 3-chain or () -- 7 show). the CALLER
// hands the function over, which makes every ordering
// contract lexical; a non-lambda or unknown slot traps. a switch, not a table:
// a slot[] would be an address-taken local (the lvm scratch rule).
lvm(lvm_seal) {
 if (getcharm(Sp[0]) != 5 && getcharm(Sp[0]) != 6 && !lamp(Sp[1])) __builtin_trap();   // the two DYNAMIC slots alone skip the gate
 switch (getcharm(Sp[0])) {
  case 0: g->hot_read = Sp[1]; break;
  case 1: g->hot_numap = Sp[1]; break;
  case 2: g->hot_stack = Sp[1]; break;
  case 3: g->hot_compose = Sp[1]; break;
  case 4: g->hot_opfix = Sp[1]; break;
  case 5: g->hot_help = Sp[1]; break;
  case 6: g->hot_io = chainp(Sp[1]) ? Sp[1] : zero; break;   // anything but a chain hands the console back
  case 7: g->hot_show = Sp[1]; break;
  default: __builtin_trap(); }
 Sp += 1, Sp[0] = zero, Ip += 1;
 ai_musttail return Continue(); }
// (heard x) -> the installed help (x ignored): the live read of hook 5, what
// prel's cellread and bao's launcher ask before choosing to raise or install.
op11(lvm_heard, (intptr_t) g->hot_help)
// (worn x) -> the stdio this task wears (x ignored): the live read of hook 6, the
// zero point when it wears the console. what a caller saves before re-seating.
op11(lvm_worn, (intptr_t) g->hot_io)
// (myself x) -> the running task's own id (x ignored): the charm `twirl` answered for it,
// and the zero point for the task nobody twirled. the run ring's head IS the running
// task, so this is a read of its pid slot. what a per-task escape compares against
// before it jumps -- a help is INHERITED at spawn, so a child can hold a continuation
// captured in its parent's stack, and landing there tears both.
op11(lvm_myself, (intptr_t) g->tasks[2].x)

// `+`/`*` over a lambda operand: build the combinator partial (stack/compose g g)
// through numap_drive. Ip is at the re-runnable +/* opcode, so a plain Have is
// safe; the slots are read AFTER it (v0..end is what the GC updates).
static lvm(lvm_addh) {
 if (coinp(Sp[0]) || coinp(Sp[1])) ai_musttail return Ap(lvm_add_coin, g);
 Have(2);
 word h = hot_hook(g->hot_stack);
 word fa = Sp[0], ga = Sp[1], *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = fa, dst[1] = h, dst[2] = ga, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; ai_musttail return Continue(); }
static lvm(lvm_mulh) {
 if (coinp(Sp[0]) || coinp(Sp[1])) ai_musttail return Ap(lvm_mul_coin, g);
 Have(2);
 word h = hot_hook(g->hot_compose);
 word fa = Sp[0], ga = Sp[1], *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = fa, dst[1] = h, dst[2] = ga, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; ai_musttail return Continue(); }

// coin +/*/-//: run the coin's die method over the RAW operands via numap_drive.
// two DISTINCT dies have no canonical combination -> zero (the method never sees
// a foreign payload); a missing method is zero too. the ()-identity never
// reaches here -- the dispatchers hoist the mint case. Ip is still the opcode
// (Ap preserves it), so word(Ip + 1) is the true return.
static lvm(lvm_coin_op, intptr_t slot) {
 word a = Sp[0], b = Sp[1];
 if (coinp(a) && coinp(b) && coin_die(a) != coin_die(b))
  return Push(ZeroPoint);             // two distinct newtypes: no canonical +/*
 word f = die_get(g, coinp(a) ? coin_die(a) : coin_die(b), slot);
 if (ai_nilp(g, f)) return Push(ZeroPoint);   // no method -> zero
 Have(2);
 a = Sp[0], b = Sp[1];                              // re-read post-GC
 f = die_get(g, coinp(a) ? coin_die(a) : coin_die(b), slot);
 word *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = a, dst[1] = f, dst[2] = b, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; return Continue(); }
lvm(lvm_add_coin) { return Ap(lvm_coin_op, g, DieAdd); }
lvm(lvm_mul_coin) { return Ap(lvm_coin_op, g, DieMul); }
// `-` and `/` have no kind matrix; lvm_sub/lvm_quot intercept coins themselves and land here.
lvm(lvm_sub_coin) { return Ap(lvm_coin_op, g, DieSub); }
lvm(lvm_quot_coin) { return Ap(lvm_coin_op, g, DieDiv); }

// applying a coin: run the die's APPLY closure as `((f self) arg)`; absent, a coin
// is an opaque handle (const-1), like a cask/port. self is the value at Ip (the apply
// trampoline sets Ip = the applied object); arg/ret are on the stack.
lvm(lvm_coin) {
 if (ai_nilp(g, die_get(g, coin_die(word(Ip)), DieApply))) {   // default opaque-apply: const-1
  Ip = cell(*++Sp); *Sp = putcharm(1); ai_musttail return Continue(); }
 Have(2);
 word self = word(Ip), f = die_get(g, coin_die(self), DieApply);
 word arg = Sp[0], ret = Sp[1], *dst = Sp - 2;
 dst[0] = self, dst[1] = f, dst[2] = arg, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; ai_musttail return Continue(); }

// (coin die payload) -> a fresh coin struck from the die over the payload.
lvm(lvm_coinmk) {
 Have(Width(struct ai_coin) + Width(struct ai_tag));
 union u *k = (union u*) Hp;
 Hp += Width(struct ai_coin) + Width(struct ai_tag);
 ((struct ai_coin*) k)->ap = lvm_coin;
 ((struct ai_coin*) k)->die = Sp[0];
 ((struct ai_coin*) k)->payload = Sp[1];
 tagthread(k, Width(struct ai_coin));
 ai_musttail return Push(word(k)); }
// (load x) -> the payload of a coin, else x itself (a plain value loads as itself).
lvm(lvm_load) {
 Sp[0] = coinp(Sp[0]) ? coin_load(Sp[0]) : Sp[0];
 ai_musttail return Next(1); }
op11(lvm_dieof, coinp(Sp[0]) ? coin_die(Sp[0]) : zero)   // (die-of x): a coin's die, else ()
op11(lvm_coinp, coinp(Sp[0]) ? putcharm(1) : zero)   // (coin? x)

// apply function to one argument
lvm(lvm_ap) {
 union u *k;
 if (oddp(Sp[1])) ai_musttail return Ap(lvm_numap, g);
 k = cell(Sp[1]), Sp[1] = word(Ip + 1), Ip = k;
 YieldCheck();
 ai_musttail return Continue(); }

// tail call
lvm(lvm_tap) {
 if (oddp(Sp[1])) ai_musttail return Ap(lvm_numtap, g);         // fixnum operator -> num-ap, deliver to caller
 intptr_t x = Sp[0], j = Sp[1];
 Sp += getcharm(Ip[1].x) + 1;
 Ip = cell(j), Sp[0] = x;
 YieldCheck();
 ai_musttail return Continue(); }

// apply to multiple arguments
lvm(lvm_apn) {
 size_t n = getcharm(Ip[1].x);
 union u *r = Ip + 2; // return address
 // this instruction is only emitted when the callee is known to be a function
 // so putting a value off the stack into Ip is safe. the +2 is cause we leave
 // the currying instruction in there... should be skipped in compiler instead FIXME
 Ip = cell(Sp[n]) + 2;
 Sp[n] = word(r); // store return address
 YieldCheck();
 ai_musttail return Continue(); }

// tail call
lvm(lvm_tapn) {
 size_t n = getcharm(Ip[1].x),
        r = getcharm(Ip[2].x);
 Ip = cell(Sp[n]) + 2;
 word *o = Sp;
 for (Sp += r + 1; n--; Sp[n] = o[n]);
 YieldCheck();
 ai_musttail return Continue(); }

// return
lvm(lvm_ret) {
 word n = getcharm(Ip[1].x) + 1;
 Ip = cell(Sp[n]); Sp[n] = Sp[0]; Sp += n; ai_musttail return Continue(); }

lvm(lvm_ret0) { return
 Ip = cell(Sp[1]),
 Sp[1] = Sp[0],
 Sp += 1,
 Continue(); }
// the walkable call-out resume (see callout_resume above): Sp[0]=result, Sp[1]=tag(bb - entry),
// Sp[2]=entry (the blob's W^X base). deliver the result where the blob expects it (Sp[0] on entry,
// two frame words consumed -- the same landing as the retired retB path) and tail-jump the blob's
// resume label. Ip is dead across a call-out (the blobs cache their twin in a Sp slot), so it rides
// through unchanged.
lvm(lvm_resume) {
 lvm_t *t = (lvm_t*) (Sp[2] + ((word) Sp[1] >> 1));
 Sp[2] = Sp[0];
 Sp += 2;
 ai_musttail return Ap(t, g); }

// kcall : x = Sp[0], k = Ip[1] -> Ip = k, Sp[0] = x
lvm(lvm_kcall) {
 word x = Sp[0];
 union u *stack = Ip + 2, *end = (union u*) ttag(g, stack);
 uintptr_t height = end - stack;
 Have(height);
 *(Sp = memmove(topof(g) - height, stack, height * sizeof(word))) = x;
 Ip = Ip[1].m;
 ai_musttail return Continue(); }

// callk : i = Sp[0], k = Ip + 1 -> Ip = i, Sp[0] = k
lvm(lvm_callk) {
 word f_val = Sp[0];                         // g, the call_cc arg
 if (oddp(f_val)) ai_musttail return Next(1);
 word height = topof(g) - Sp;
 uintptr_t n = 2 + height;                   // lvm_kcall + (ip + 1) + stack = thread_contents
 Have(n + Width(struct ai_tag) + 1);          // thread_contents + thread_tag + 1 stack = _mem_req
 union u *k = (union u*) Hp;
 Hp += n + Width(struct ai_tag);              // thread_contents + thread_tag = _heap_alloc
 k[0].ap = lvm_kcall;                       // 
 k[1].m  = Ip + 1;                           // resume at next instruction
 memcpy(k + 2, Sp, height * sizeof(word));
 Sp -= 1;
 Sp[0] = word(tagthread(k, n));
 Sp[1] = f_val;
 ai_musttail return Ap(lvm_ap, g); }

// lvm_yield_sw_mono can't call ai_wait_fds directly with a stack record
static ai_noinline void wait_one(int fd, int events, uintptr_t ms) {
  struct ai_wait_fd w = { .fd = fd, .events = (short) events };
  ai_wait_fds(&w, 1, ms); }

// monotask fast path
static lvm(lvm_yield_sw_mono) { uintptr_t my_wake = g->next_wake_at;
 int my_wait_fd = g->next_wait_fd, my_events = g->next_wait_events;
 g->next_wake_at = 0;
 g->next_wait_fd = -1;
 g->next_wait_events = ai_wait_in;
 g->yield_ctr = 0;
 if (my_wake) for (uintptr_t now; my_wake > (now = ai_clock());)
  my_wait_fd >= 0 ? wait_one(my_wait_fd, my_events, my_wake - now) : ai_sleep(my_wake - now);
 else if (my_wait_fd >= 0)
  while (!ai_ready(my_wait_fd, my_events)) wait_one(my_wait_fd, my_events, 0);
 ai_musttail return Continue(); }

// the parked ring by pid; the predecessor comes back too (singly linked, and an
// unsplice cannot go looking for it twice)
static ai_inline union u *parked_find(struct ai *g, intptr_t pid, union u **prevp) {
 union u *head = g->parked;
 if (!head) return NULL;
 union u *prev = head;
 do { union u *n = prev->m;
      if (getcharm(n[2].x) == pid) return *prevp = prev, n;
      prev = n; } while (prev != head);
 return NULL; }

// Take `n` off the parked ring. ⚠ CALLED WITH g PACKED: gen_wb reads g->hp to tell
// young from old, and the live Hp runs ahead of the last Pack.
static ai_inline void parked_drop(struct ai *g, union u *prev, union u *n) {
 if (prev == n) return (void) (g->parked = NULL);   // it was the whole ring
 prev->m = n->m;
 gen_wb(g, (word) prev, (word) prev->m);            // an old node now links to a (maybe young) successor
 if (g->parked == n) g->parked = prev; }

// ...and onto the run ring behind `tail`, so a park-and-wake task queues behind its
// peers. ⚠ answers the new tail, so a many-task wake walks the run ring ONCE
// (finding the tail inside made a 200-client wake quadratic). ⚠ the wait_fd is
// cleared on the way in -- the run ring's whole invariant: nothing there is fd-parked.
static ai_inline union u *run_splice_at(struct ai *g, union u *tail, union u *n) {
 n[0].m = g->tasks;
 n[4].x = putcharm(-1);
 gen_wb(g, (word) n, (word) n[0].m);
 tail->m = n;
 gen_wb(g, (word) tail, (word) tail->m);
 return n; }

// is the task named by pid still live? ⚠ the ring head is the RUNNING task, whose
// saved ip is stale -- me_live carries its own yield's answer. a pid with no node
// is GONE, not live: a catcher must not wait on a ghost.
static ai_inline int task_live(struct ai *g, union u *head, intptr_t pid, int me_live) {
 if (getcharm(head[2].x) == pid) return me_live;
 for (union u *n = head->m; n != head; n = n->m)
  if (getcharm(n[2].x) == pid) return n[1].m->ap != lvm_task_exit;
 union u *prev;   // ⚠ AND THE PARKED RING: a caught task blocked on an fd is LIVE, and
 union u *p = parked_find(g, pid, &prev);   // a catcher told otherwise stops waiting.
 return p ? p[1].m->ap != lvm_task_exit : 0; }

// is this parked task sitting on a port already holding bytes? bytes live in the
// PORT, not the fd; a reader parks with Ip unadvanced, so its port is the top of
// its saved stack. ⚠ the ap guard is what makes reading n[8] legal: only these two
// ops park with a port at Sp[0]; every other parker answers false first.
static ai_inline bool wait_buffered(struct ai*, lvm_t*, word, int);

// readiness the wait already answered: poll(2) reports every ready fd in its set.
// -> 1 ready, 0 not, -1 don't know (no block, or fd not in it). ⚠ match on the
// (fd, events) PAIR -- two tasks can park on one fd in opposite directions. ⚠ ANY
// nonzero revents is ready: a hung-up fd wants waking to read the end. the cursor
// is speed, not correctness.
static ai_inline int polled_ready(struct ai_wait_fd const *fds, int nfds, int *cur, int fd, int ev) {
 for (int i = 0; i < nfds; i++) {
  int j = *cur + i < nfds ? *cur + i : *cur + i - nfds;
  if (fds[j].fd == fd && fds[j].events == (short) ev)
   return *cur = j + 1 < nfds ? j + 1 : 0, fds[j].revents != 0; }
 return -1; }

// first runnable peer, RUN ring only: nothing here is fd-parked, so the walk
// issues no syscall. the catch clause matters -- without it a catcher is always
// runnable and the scheduler never reaches its wait (the catch park carries no
// state: Ip is unadvanced, so the saved ip IS the catch, the pid its stack top).
// ⚠ the wait_fd arm is a floor, not a path: a slipped invariant costs a re-park.
static ai_inline union u *find_runnable(struct ai *g, union u *head, uintptr_t now, int me_live) {
 for (union u *n = head->m; n != head; n = n->m)
  if (n[1].m->ap != lvm_task_exit && (uintptr_t) getcharm(n[3].x) <= now) {
   if (n[1].m->ap == lvm_wait && task_live(g, head, getcharm(n[8].x), me_live)) continue;
   int wf = (int) getcharm(n[4].x);
   if (wf < 0 || wait_buffered(g, n[1].m->ap, n[8].x, wf)
       || ai_ready(wf, (int) getcharm(n[5].x))) return n; }
 return NULL; }

// can this parked task run again? deadline come, port holding bytes, or fd ready
// (off the filled block, else ask). ⚠ `ask` is whether the kernel MAY be asked:
// with it clear only the syscall-free terms count -- the pass yield_sw_wait makes
// BEFORE it builds a wait.
static ai_inline int parked_ready(struct ai *g, union u *n, uintptr_t now,
                                  struct ai_wait_fd const *fds, int nfds, int *cur, int ask) {
 if (n[1].m->ap == lvm_task_exit || (uintptr_t) getcharm(n[3].x) > now) return 0;
 int wf = (int) getcharm(n[4].x), ev = (int) getcharm(n[5].x);
 if (wf < 0 || wait_buffered(g, n[1].m->ap, n[8].x, wf)) return 1;
 int pr = polled_ready(fds, nfds, cur, wf, ev);
 return pr < 0 ? (ask && ai_ready(wf, ev)) : pr; }

// THE WAKE PASS: every parked task that can run again moves to the run ring; answers
// how many. ⚠ walked by COUNT -- a waking task is unspliced under the cursor, and the
// head is as free to leave as anyone. ⚠ called with g PACKED (every relink barriers).
static ai_noinline int wake_parked(struct ai *g, uintptr_t now,
                                   struct ai_wait_fd const *fds, int nfds, int ask) {
 if (!g->parked) return 0;
 int n = 1, cur = 0, woke = 0;
 for (union u *q = g->parked->m; q != g->parked; q = q->m) n++;
 union u *prev = g->parked, *tail = NULL;
 for (int i = 0; i < n && g->parked; i++) {
  union u *t = prev->m;
  if (!parked_ready(g, t, now, fds, nfds, &cur, ask)) { prev = t; continue; }
  if (!tail) for (tail = g->tasks; tail->m != g->tasks; tail = tail->m);   // once, on the first wake
  parked_drop(g, prev, t);
  tail = run_splice_at(g, tail, t);
  woke++; }
 return woke; }

// the fairness path's ask: ONE sweep of every parked fd, then the wake. the block
// rides the [hp, sp) gap (⚠ called with g PACKED). ⚠ the block is AUTHORITATIVE
// here, unlike the wait's: all-zero means "none ready", never "nobody said".
static ai_noinline int poll_parked(struct ai *g, uintptr_t now) {
 int n = 1;
 for (union u *q = g->parked->m; q != g->parked; q = q->m) n++;
 struct ai_wait_fd *fds = (struct ai_wait_fd*) g->hp;
 // no gap to lay them in (the heap at its fullest, a collection pending): ask the old
 // way rather than skip the sweep, which would leave a ready peer parked.
 if (avail(g) < b2w((uintptr_t) n * sizeof *fds)) return wake_parked(g, now, NULL, 0, 1);
 int k = 0;
 union u *q = g->parked;
 do { int wf = (int) getcharm(q[4].x);
      if (wf >= 0) fds[k].fd = wf, fds[k].revents = 0, fds[k++].events = (short) getcharm(q[5].x);
      q = q->m; } while (q != g->parked);
 ai_ready_fds(fds, k);
 return wake_parked(g, now, fds, k, 0); }

// ⚠ the fd set is sized by the COUNT, never a constant (kiosko parks a task per
// client); the block rides the uncommitted heap gap, so counting first retires the
// cap by construction. ⚠ called with g PACKED. ⚠ BOTH rings are walked: the run
// ring holds the sleepers, the parked ring the fds -- one ring's terms alone
// oversleep the other's.
static ai_noinline union u *yield_sw_wait(struct ai *g, uintptr_t my_wake, int my_wait_fd, int my_events, int me_live) {
 // ⚠ the syscall-free wakes FIRST, load-bearing: a parked task whose port already
 // holds bytes is runnable over an fd with nothing left to say -- a wait built
 // while it is parked never returns and `catch` hangs (test/host/parked.l, law 2).
 if (wake_parked(g, ai_clock(), NULL, 0, 0)) {
  union u *n = find_runnable(g, g->tasks, ai_clock(), me_live);
  if (n) return n; }
 uintptr_t min_wake = my_wake;
 int nfds = my_wait_fd >= 0;
 for (union u *n = g->tasks->m; n != g->tasks; n = n->m)
  if (n[1].m->ap != lvm_task_exit) {
   uintptr_t wa = (uintptr_t) getcharm(n[3].x);
   if (wa && (!min_wake || wa < min_wake)) min_wake = wa; }
 if (g->parked) {
  union u *q = g->parked;
  do { uintptr_t wa = (uintptr_t) getcharm(q[3].x);
       if (wa && (!min_wake || wa < min_wake)) min_wake = wa;
       if (getcharm(q[4].x) >= 0) nfds++;
       q = q->m; } while (q != g->parked); }
 if (!min_wake && !nfds) return NULL;
 uintptr_t now = ai_clock(), ticks = min_wake ? min_wake - now : 0;
 // the filled block, once the wait answers it; stays NULL unless some entry came
 // back nonzero, so a frontend that fills nothing keeps working the old way
 struct ai_wait_fd const *pol = NULL;
 int npol = 0;
 if (!min_wake || min_wake > now) {
  struct ai_wait_fd *fds = (struct ai_wait_fd*) g->hp;
  // no gap to lay them in (the heap at its fullest, a collection pending): wait
  // on the CLOCK alone and come straight back, rather than on a set we already
  // know is short -- the one thing this rung exists to stop.
  if (avail(g) < b2w((uintptr_t) nfds * sizeof *fds)) ai_wait_fds(NULL, 0, ticks ? ticks : 1);
  else {
   int k = 0;
   // ⚠ revents is ZEROED here and nowhere else. The block is raw heap gap, so an
   // unwritten slot would otherwise read as whatever the last allocation left, and
   // "ready" is exactly the wrong way to guess.
   if (my_wait_fd >= 0)
    fds[k].fd = my_wait_fd, fds[k].revents = 0, fds[k++].events = (short) my_events;
   if (g->parked) {
    union u *q = g->parked;
    do { int wf = (int) getcharm(q[4].x);
         if (wf >= 0)
          fds[k].fd = wf, fds[k].revents = 0, fds[k++].events = (short) getcharm(q[5].x);
         q = q->m; } while (q != g->parked); }
   ai_wait_fds(fds, k, ticks);
   for (int i = 0; i < k; i++) if (fds[i].revents) { pol = fds, npol = k; break; } }
  now = ai_clock(); }
 if (my_wait_fd >= 0) {
  int cur = 0, pr = polled_ready(pol, npol, &cur, my_wait_fd, my_events);
  if (pr < 0 ? ai_ready(my_wait_fd, my_events) : pr) return NULL; }
 wake_parked(g, now, pol, npol, 1);   // the wait answered the whole parked ring: collect it
 return find_runnable(g, g->tasks, now, me_live); }

lvm(lvm_yield_sw) {
 // ⚠ THE MONOTASK DOOR NEEDS BOTH RINGS EMPTY. A lone runnable task with parked peers
 // reads as a self-ring now, and the mono path waits on ITS OWN fd only -- the peers
 // would sleep through every wake they were owed.
 if (g->tasks->m == g->tasks && !g->parked) ai_musttail return Ap(lvm_yield_sw_mono, g);
 // a task on its way out is not live, and its own node cannot say so yet -- the
 // snapshot that records the exit is written at the foot of this op.
 int me_live = Ip->ap != lvm_task_exit;
 uintptr_t now = ai_clock();
 uintptr_t my_wake = g->next_wake_at;
 int my_wait_fd = g->next_wait_fd, my_events = g->next_wait_events;
 // a FAIRNESS yield never reaches yield_sw_wait, so this counter is the only thing
 // asking on its behalf whether a parked peer woke; sweeping is a syscall, so it
 // rides sweep_interval. ⚠ it must fire even with a runnable peer to hand the cpu
 // to -- two compute tasks trading turns would starve every parked peer for good.
 int fair = !my_wake && my_wait_fd < 0 && Ip->ap != lvm_wait;
 if (fair && g->parked && ++g->sweep_ctr >= sweep_interval) {
  g->sweep_ctr = 0;
  Pack(g);                     // the sweep lays its fd block in the [hp, sp) gap
  poll_parked(g, now);
  Unpack(g); }                 // nothing allocated, so these come back unchanged
 union u *next = find_runnable(g, g->tasks, now, me_live);
 if (!next) {
  // a fairness yield with no runnable peer just keeps running: falling into
  // yield_sw_wait would throttle compute to the slowest sleeping peer's period.
  // a blocked task still waits below. ⚠ a catcher takes this arm only over its
  // own dead body: Ip still points at the catch, so it would spin.
  if (fair) { g->yield_ctr = 0; ai_musttail return Continue(); }
  Pack(g);                     // the wait lays its fd block in the [hp, sp) gap
  next = yield_sw_wait(g, my_wake, my_wait_fd, my_events, me_live);
  Unpack(g);                   // nothing allocated, so these come back unchanged
  if (!next) {
   g->next_wake_at = 0;
   g->next_wait_fd = -1;
   g->next_wait_events = ai_wait_in;
   if (g->yield_ctr >= yield_interval) g->yield_ctr = 0;
   ai_musttail return Continue(); } }
 word my_height = topof(g) - Sp;
 union u *next_stack = next + 8,
       *end = (union u*) ttag(g, next_stack);
 uintptr_t restore_h = end - next_stack,
           need = my_height + restore_h + 9;
 if (Sp < Hp + need) {
  Pack(g);
  if (!ai_ok(g = ai_please(ai_push(g, 1, next), need))) ai_musttail return Ap(_lvm_ghelp, g);
  next = cell(pop1(g));
  Unpack(g);
  next_stack = next + 8; }   // recompute: next was forwarded by gc
 g->next_wake_at = 0;
 g->next_wait_fd = -1;
 g->next_wait_events = ai_wait_in;
 union u *prev = next;
 while (prev->m != g->tasks) prev = prev->m;
 union u *N = (union u*) Hp;
 Hp += need - restore_h;
 // THE SNAPSHOT'S RING IS DECIDED BY ITS WAIT_FD. A task giving up its turn for an fd
 // is not runnable and must not be walked as though it were -- it leaves the run ring
 // here, which is the whole rung, and comes back through wake_parked.
 int parking = my_wait_fd >= 0;
 N[0].m = parking ? (g->parked ? g->parked->m : N) : g->tasks->m;
 N[1].m = Ip;
 N[2].x = g->tasks[2].x;
 N[3].x = putcharm((intptr_t) my_wake);
 N[4].x = putcharm(my_wait_fd);
 N[5].x = putcharm(my_events);
 N[6].x = g->hot_help;            // the help is the TASK's: saved here, restored below
 N[7].x = g->hot_io;              // ...and so is the stdio it wears
 memcpy(N + 8, Sp, my_height * sizeof(word));
 tagthread(N, 8 + my_height);
 // the run ring closes over the departing head either way: onto the snapshot when it
 // stays, or over it entirely when it parks.
 prev->m = parking ? g->tasks->m : N;
 // Pack FIRST: ai_young reads g->hp, and the live Hp runs ahead of the last Pack --
 // against a stale g->hp the fresh node reads as OLD, the barrier drops the edge, and
 // the next minor eats the ring (berth+ink froze in seconds on exactly this).
 Pack(g);
 gen_wb(g, (word) prev, (word) prev->m);   // task ring: an old node now links to the fresh (young) yield snapshot
 if (parking) {
  // ⚠ N ALREADY POINTS INTO THE PARKED RING (or at itself): only the ring's own link
  // in is left, and only that one is an old->young edge worth a barrier.
  if (g->parked) { g->parked->m = N; gen_wb(g, (word) g->parked, (word) g->parked->m); }
  else g->parked = N; }
 g->yield_ctr = 0;
 g->tasks = next;
 g->hot_help = next[6].x;
 g->hot_io = next[7].x;
 Sp = memmove(topof(g) - restore_h, next_stack, restore_h * sizeof(word));
 Ip = next[1].m;
 ai_musttail return Continue(); }

lvm(lvm_yield_nif) { Ip++; ai_musttail return Ap(lvm_yield_sw, g); }
lvm(lvm_task_exit) { ai_musttail return Ap(lvm_yield_sw, g); }
static union u const spawn_body[] = { {lvm_ap}, {.ap = lvm_task_exit} };
lvm(lvm_spawn) {
 Have(11);
 // New task node N: [next, saved_ip=spawn_body, pid, wake_at, wait_fd, wait_events, help, stdio, stack[0..1]=x,fn, tag]
 union u *N = (union u*) Hp;
 Hp += 11;
 word fn = Sp[0], x = Sp[1];
 uintptr_t pid = ++g->next_serial;   // a pid is a fresh identity: drawn from the mint stream
 N[0].m = g->tasks->m;
 N[1].m = (union u*) spawn_body;
 N[2].x = Sp[1] = putcharm(pid);
 N[3].x = zero;         // wake_at: sentinel for "always runnable"
 N[4].x = putcharm(-1);  // wait_fd: -1 = not waiting on I/O
 N[5].x = putcharm(ai_wait_in);   // wait_events: the read direction, the default
 N[6].x = g->hot_help;   // INHERITED: a child starts under its parent's help, never helpless
 N[7].x = g->hot_io;     // ...and under its parent's stdio, the console until it wears its own
 N[8].x = x;
 N[9].x = fn;
 g->tasks->m = tagthread(N, 10);
 Pack(g);   // sync: ai_young reads g->hp (see lvm_yield_sw)
 gen_wb(g, (word) g->tasks, (word) g->tasks->m);   // task ring: an old node now links to the fresh (young) spawned task
 ai_musttail return Nextp(1, 1); }

lvm(lvm_wait) {
 word pid_arg = Sp[0], ret = zero;
 intptr_t target = getcharm(pid_arg);
 for (union u *node = g->tasks->m; node != g->tasks; node = node->m) {
  if (getcharm(node[2].x) != target) continue;
  if (node[1].m->ap == lvm_task_exit) {
   // dormant: dormant task's stack is just [retval] at node[8]
   ret = node[8].x;
   union u *prev = node;
   while (prev->m != node) prev = prev->m;
   prev->m = node->m;
   Pack(g);   // sync: ai_young reads g->hp (see lvm_yield_sw)
   gen_wb(g, (word) prev, (word) prev->m);   // task ring: unsplicing relinks an old node to a (maybe young) successor
   break; }
   // still running: yield without advancing Ip -- BOTH halves of the park (the
   // re-entry on resume, and the record: Ip here says "parked in catch", Sp[0]
   // names the peer). clear both wait intentions: a stale fd would gate the park.
   g->next_wake_at = 0;
   g->next_wait_fd = -1;
  ai_musttail return Ap(lvm_yield_sw, g); }
 // ⚠ and the parked ring, or catching a task merely blocked on an fd answers the
 // zero point at once; it is live, so park exactly as above.
 { union u *prev, *p = parked_find(g, target, &prev);
   if (p) { g->next_wake_at = 0; g->next_wait_fd = -1; ai_musttail return Ap(lvm_yield_sw, g); } }
 ai_musttail return Answer(ret); }

lvm(lvm_donep) {
 word pid_arg = Sp[0], result = putcharm(1);
 intptr_t target = getcharm(pid_arg);
 for (union u *node = g->tasks->m; node != g->tasks; node = node->m)
  if (getcharm(node[2].x) == target) {
   if (node[1].m->ap != lvm_task_exit) result = zero;
   Sp[0] = result, Ip += 1;
   ai_musttail return Continue(); }
 // an unfound pid reads LANDED, so a task merely parked on an fd would report finished --
 // a collector would drop a live session's handle mid-request.
 { union u *prev;
   if (parked_find(g, target, &prev)) result = zero; }
 Sp[0] = result;
 Ip += 1;
 ai_musttail return Continue(); }

// (scoop _) -> (pid . retval) of ONE finished task, or () when none have -- the
// task-side twin of `glean` (host/posix.c). ⚠ presence rides the PAIR, never the
// net: a retval is legitimately (), so `two?` is the test and ZeroPoint the empty
// answer. only the RUN ring is walked (parked = blocked = unfinished); the arg is
// a dummy, so a bare (scoop) curries -- call it (scoop 0).
lvm(lvm_scoop) {
 Have(Width(struct ai_chain));
 for (union u *prev = g->tasks, *node = prev->m; node != g->tasks; prev = node, node = node->m) {
  if (node[1].m->ap != lvm_task_exit) continue;
  word pid = node[2].x, ret = node[8].x;   // dormant: the stack is just [retval] at node[8]
  struct ai_chain *p = (struct ai_chain*) Hp;
  Hp += Width(struct ai_chain);
  ini_chain(p, pid, ret);
  prev->m = node->m;
  Pack(g);   // sync: ai_young reads g->hp (see lvm_yield_sw)
  gen_wb(g, (word) prev, (word) prev->m);   // task ring: unsplicing relinks an old node to a (maybe young) successor
  Sp[0] = (word) p, Ip += 1;
  ai_musttail return Continue(); }
 Sp[0] = ZeroPoint, Ip += 1;
 ai_musttail return Continue(); }

lvm(lvm_hush) {
 word pid_arg = Sp[0], result = zero;
 intptr_t target = getcharm(pid_arg);
 union u *prev = g->tasks;
 for (union u *node = prev->m; node != g->tasks; prev = node, node = node->m)
  if (getcharm(node[2].x) == target) {
   prev->m = node->m;
   Pack(g);   // sync: ai_young reads g->hp (see lvm_yield_sw)
   gen_wb(g, (word) prev, (word) prev->m);   // unsplice relinks an old node to a (maybe young) successor
   Sp[0] = putcharm(1), Ip += 1;
   ai_musttail return Continue(); }
 // freeze reaches the parked ring too -- a task blocked on a quiet fd is exactly the one
 // a caller most wants to be able to stop.
 { union u *pp, *p = parked_find(g, target, &pp);
   if (p) { Pack(g); parked_drop(g, pp, p); result = putcharm(1); } }
 Sp[0] = result;
 Ip += 1;
 ai_musttail return Continue(); }

lvm(lvm_sleep) {
 word n = Sp[0];
 Sp[0] = zero;
 Ip += 1;
 // rest waits on the CLOCK alone: a lingering next_wait_fd would gate the timer on
 // that fd firing (a painter slept forever on a quiet port)
 g->next_wait_fd = -1;
 if (!charmp(n) || getcharm(n) <= 0) { g->next_wake_at = 0; ai_musttail return Ap(lvm_yield_sw, g); }
 g->next_wake_at = (uintptr_t) ai_clock() + getcharm(n);
 ai_musttail return Ap(lvm_yield_sw, g); }


lvm(lvm_jump) { Ip = Ip[1].m; ai_musttail return Continue(); }
// The only compiled truthiness branch (`?`, and the `&&`/`||` macros). Uses the
// language falsy predicate so an all-zero tray (boxed 0.0, zero int box,
// all-zero array) takes the false arm, lifting "0 is the only false scalar".
lvm(lvm_cond) { Ip = ai_nilp(g, *Sp++) ? Ip[1].m : Ip + 2; ai_musttail return Continue(); }
lvm(lvm_unc) {
 Have1();
 *--Sp = Ip[1].x;
 Ip = Ip[2].m;
 ai_musttail return Continue(); }

lvm(lvm_cur) {
 size_t const S = 3 + Width(struct ai_tag);
 Have(S + 2);
 union u *k = (union u*) Hp, *j = k;
 Hp += S;
 size_t n = getcharm(Ip[1].x);
 // FIXME this does not always need to be a runtime check
 if (n > 2) Hp += 2,
            j += 2,
            k[0].ap = lvm_cur,
            k[1].x = putcharm(n - 1);
 return
  j[0].ap = lvm_unc,
  j[1].x = *Sp++,
  j[2].m = Ip + 2,
  Ip = cell(*Sp),
  Sp[0] = (word) tagthread(k, j + 3 - k),
  Continue(); }

// load instructions
//
lvm(lvm_quote) {
 Have1();
 Sp -= 1;
 Sp[0] = Ip[1].x;
 Ip += 2;
 ai_musttail return Continue(); }

// A port has no function meaning either: applying it behaves as 0 (yields 1), like
// a cask (byte-identical body, kept distinct by ai_noicf -- see lvm_cask).
lvm(lvm_port_io) {
  Ip = cell(*++Sp);
  *Sp = putcharm(1);
  ai_musttail return Continue(); }

// push a value from the stack
lvm(lvm_arg) {
 Have1();
 Sp[-1] = Sp[getcharm(Ip[1].x)];
 Sp -= 1;
 Ip += 2;
 ai_musttail return Continue(); }

// fused (arg <idx> ; ap): the dominant "call a function on a local" shape, one
// dispatch saved; resume is Ip+2 (2-word op)
lvm(lvm_argap) {
 if (oddp(Sp[0])) {                                  // fixnum operator -> num-ap, resume at Ip+2
  Have1();
  Sp[-1] = Sp[getcharm(Ip[1].x)], Sp -= 1, Ip += 1;   // push local under operator; resume now Ip+2
  ai_musttail return Ap(lvm_numap, g); }
 Have1();
 Sp[-1] = Sp[getcharm(Ip[1].x)];
 Sp -= 1;
 union u *k = cell(Sp[1]); Sp[1] = word(Ip + 2), Ip = k;
 YieldCheck();
 ai_musttail return Continue(); }

// fused (quote <v> ; ap): a call with a constant arg; resume Ip+2
lvm(lvm_quoteap) {
 if (oddp(Sp[0])) {                                  // fixnum operator -> num-ap, resume at Ip+2
  Have1();
  Sp[-1] = Ip[1].x, Sp -= 1, Ip += 1;               // push const under operator; resume now Ip+2
  ai_musttail return Ap(lvm_numap, g); }
 Have1();
 Sp -= 1;
 Sp[0] = Ip[1].x;
 union u *k = cell(Sp[1]); Sp[1] = word(Ip + 2), Ip = k;
 YieldCheck();
 ai_musttail return Continue(); }

// fused (arg <idx> ; tap <fs>): the single-arg tail-call shape, e.g. a tail (loop x)
lvm(lvm_argtap) {
 if (oddp(Sp[0])) {                                  // fixnum operator -> num-ap, deliver to caller
  Have1();
  Sp[-1] = Sp[getcharm(Ip[1].x)], Sp -= 1, Ip += 1;   // push local under operator; fs operand now Ip[1]
  ai_musttail return Ap(lvm_numtap, g); }
 Have1();
 Sp[-1] = Sp[getcharm(Ip[1].x)];
 Sp -= 1;
 intptr_t x = Sp[0], j = Sp[1];
 Sp += getcharm(Ip[2].x) + 1;
 Ip = cell(j), Sp[0] = x;
 YieldCheck();
 ai_musttail return Continue(); }

// operand-specialized arg/quote: 1-word ops for the hottest indices/constants
argn(lvm_arg0, 0) argn(lvm_arg1, 1) argn(lvm_arg2, 2) argn(lvm_arg3, 3)
quon(lvm_quo0, 0) quon(lvm_quo1, 1) quon(lvm_quo2, 2) quon(lvm_quo3, 3)
quon(lvm_quom1, -1) quon(lvm_quom2, -2)

// RUN FUSION: one op for a whole RUN of consecutive loads, specialized on the
// SHAPE of the run rather than on an operand's value. The name spells the run in
// source order -- `a` an arg (its index the operand), `q` a quote (its constant
// the operand) -- and a trailing `p` says the LAST load carries the apply (it was
// argap/quoteap). The apply can only sit at the END: an ap hands control away and
// resumes at a fixed Ip, and a run has no dispatchable point in its middle.
//
// The operands ride in source order, so an index is written as the compiler saw
// it -- relative to the Sp of ITS OWN load. Pushing left to right off a moving Sp
// makes that come out right with no arithmetic: by the time PushA(2) runs, Sp has
// already dropped past the first push, which is exactly the frame the second load
// was compiled against.
#define PushA(k) (Sp[-1] = Sp[getcharm(Ip[k].x)], Sp -= 1)
#define PushQ(k) (Sp[-1] = Ip[k].x, Sp -= 1)
// pure run, 2 loads: op + 2 operands = 3 words.
#define frun2(nom, p1, p2) lvm(nom) { Have(2); p1(1); p2(2); Ip += 3; ai_musttail return Continue(); }
// ... with the apply on the second load. The operator is what the FIRST load
// pushed (cf. lvm_argap, which reads it at Sp[0] before its own push), so the
// fixnum test sits between the two. The numap lane bumps Ip to leave numap's
// `ret = Ip+1` landing past the whole op.
#define frun2p(nom, p1, p2) lvm(nom) { \
 Have(2); p1(1); \
 if (oddp(Sp[0])) { p2(2); Ip += 2; ai_musttail return Ap(lvm_numap, g); } \
 p2(2); \
 union u *k = cell(Sp[1]); Sp[1] = word(Ip + 3), Ip = k; \
 YieldCheck(); \
 ai_musttail return Continue(); }
frun2(lvm_aa, PushA, PushA) frun2(lvm_aq, PushA, PushQ)
frun2(lvm_qa, PushQ, PushA) frun2(lvm_qq, PushQ, PushQ)
frun2p(lvm_aap, PushA, PushA) frun2p(lvm_aqp, PushA, PushQ)
frun2p(lvm_qap, PushQ, PushA) frun2p(lvm_qqp, PushQ, PushQ)
// LOAD + CONSUMER fusion -- the other axis. A run's loads are only half the story:
// something EATS them, and measured on the corpus that consumer is overwhelmingly an
// accessor, a predicate or a branch, NOT arithmetic (cup 133.7M, `?` 128.8M, cap 80.7M,
// two? 77.5M vs + at 5.5M -- 64% of every load dispatch goes into the first four).
// So these fuse `arg` with the op that consumes it: 2 words, exactly what the
// operand-specialized arg0..3 plus a 1-word op already cost, for one dispatch instead
// of two. The tree already fuses from the OTHER side at runtime (cmp_lt peeks Ip[1]
// for lvm_cond); this is the compile-time twin, and it reaches ops with no such peek.
// ⚠ the parameter is NOT named `x`: the body says Ip[1].x, and a macro parameter of
// that name substitutes into the MEMBER access.
#define fld(nom, val) lvm(nom) { Have1(); word v = Sp[getcharm(Ip[1].x)]; Sp[-1] = (val); Sp -= 1; Ip += 2; ai_musttail return Continue(); }
fld(lvm_argcap, chainp(v) ? A(v) : v)
fld(lvm_argcup, chainp(v) ? B(v) : ZeroPoint)
fld(lvm_argtwo, (chainp(v) && !nomp(v)) ? putcharm(1) : zero)
// arg + cond: the test never reaches the stack at all -- no push, no pop, one op.
// Layout [Ip]=argcond [Ip+1]=idx [Ip+2]=else-addr [Ip+3]=then, matching lvm_cond's
// own targets shifted by our operand (cf. the cmp_lt note).
lvm(lvm_argcond) { Ip = ai_nilp(g, Sp[getcharm(Ip[1].x)]) ? Ip[2].m : Ip + 3; ai_musttail return Continue(); }
// ... and the RUNG ABOVE: load + predicate + cond, all three in one op. Measured, this
// is where `?` actually lives: only 7.1M conds test a bare local, while 86.1M test the
// result of a fused load+accessor -- `(? (two? b) ..)` is the shape, 64.4M of it. The
// whole test then costs one dispatch and NO stack traffic at all: nothing is pushed to
// be immediately popped by the branch. Layout [Ip]=op [Ip+1]=idx [Ip+2]=else, so the
// emit consumes the predicate's op cell AND the cond's, spending no new word.
#define fldc(nom, test) lvm(nom) { word v = Sp[getcharm(Ip[1].x)]; \
 Ip = (test) ? Ip + 3 : Ip[2].m; ai_musttail return Continue(); }
fldc(lvm_argtwocond, chainp(v) && !nomp(v))            // two? answers a charm: no ai_nilp needed

lvm(lvm_trim) { return
 clip(g, cell(Sp[0])), Ip++, Continue(); }

lvm(lvm_seek) { return
 Sp[1] = word(cell(Sp[1]) + getcharm(Sp[0])),
 Sp++, Ip++, Continue(); }

lvm(lvm_peek) { return
 Sp[1] = (cell(Sp[1]) + getcharm(Sp[0]))->x,
 Sp++, Ip++, Continue(); }

lvm(lvm_poke) {
 union u *c = cell(Sp[2]) + getcharm(Sp[0]);
 Pack(g);                    // ai_young reads g->hp -- the live Hp may be ahead (the lvm-context law)
 gen_wb_cell(g, c, Sp[1]);   // poke's CONTRACT: the target cell sits in a tagged span (a spin
                             // thread, an env) -- never a chain's field (ev boxes those; a chain
                             // has no terminator for the remembered cell-walk).
 c->x = Sp[1]; *(Sp += 2) = word(c); ai_musttail return Next(1); }

lvm(lvm_spin) {
 size_t n = getcharm(Sp[0]);
 Have(n + Width(struct ai_tag));
 union u *k = (union u*) Hp;
 Hp += n + Width(struct ai_tag);
 Sp[0] = word(memset(tagthread(k, n), -1, n * sizeof(word)));
 ai_musttail return Next(1); }

// THE NET: the complex-valued measure. a complex scalar nets ITSELF (additivity
// needs phase, so the codomain is C and the order retraction happens ONCE, in the
// observers); every other scalar nets real; a chain or rank>=1 array nets the SUM
// of its elements' nets -- recursive, unclamped, SPINE only -- so negatives cancel
// and opposite phases annihilate by vector cancellation. net(asum v) = net(v).
static struct ai_zn ai_net(struct ai *g, word x) {
  if (charmp(x)) return zn((ai_flo_t) getcharm(x), 0);               // fixnum: its value
  if (caskp(x)) { struct ai_str *b = cask(x)->str; ai_flo_t t = 0; // hot chars: Σ charms, like a string
    for (uintptr_t i = 0; i < b->len; i++) t += (uint8_t) b->bytes[i];
    return zn(t, 0); }
  if (tabp(x)) return zn((ai_flo_t) map_len(x), 0);              // table: key count
  if (coinp(x)) {                                              // a coin nets its payload (the monoid hom), unless
    word mode = die_get(g, coin_die(x), DieNet);              // its die pins a net MODE.
    if (mode == putcharm(1))                                   // mode 1: net by TALLY, the COUNT -- never negative,
      return zn((ai_flo_t) ai_count(g, coin_load(x)), 0);      // so truth is "has any"
    if (mode == putcharm(2)) {                                 // mode 2: RATIO -- an (n d)-of-reals payload nets
      word p = coin_load(x);                                   // n/d, the SIGN exact (value truth for rationals):
      if (chainp(p) && chainp(B(p))) {                         // the division's sign is IEEE-true, and the two
        struct ai_zn n = ai_net(g, A(p)), d = ai_net(g, A(B(p)));  // loss lanes below restore it from the
        if (n.im == 0 && d.im == 0 && d.re != 0) {             // components' own exact signs.
          ai_flo_t s = (n.re < 0) != (d.re < 0) ? -1.0 : 1.0;
          ai_flo_t v = n.re / d.re;
          if (v != v) v = s;                                   // inf/inf (two giant bignums): sign carries
          else if (v == 0 && n.re != 0)                        // underflow: a live sign never reads as the floor
            v = s * (ai_flo_t) (Bits == 64 ? 1e-300 : 1e-37);  // (the rescue must be nonzero at ai_flo_t's width)
          return zn(v, 0); } } }                               // a malformed payload falls through to the hom
    return ai_net(g, coin_load(x)); }
  if (!datp(x)) return zn(1, 0);                                // opaque but present (fn / port): truthy
  switch (typ(x)) {
    case DString: { ai_flo_t t = 0;                                 // a string is PACKED CHARS: Σ charms
      for (uintptr_t i = 0; i < len(x); i++) t += (uint8_t) txt(x)[i];
      return zn(t, 0); }                                           // (the count moved to tally)
    case DChain: { struct ai_zn s = zn(0, 0); word p = x;           // chain: sum the SPINE's nets --
      do { struct ai_zn e = ai_net(g, A(p));                       // complex sums, so negatives cancel,
           s.re += e.re, s.im += e.im;                           // phases cancel, and a chain of
           p = B(p); } while (chainp(p));                          // nothings nets to nothing
      return s; }
    case DBig: return zn(ai_big_to_flo(x), 0);                   // bignum: full magnitude, sign intact
    case DGem: return zn(gem_get(x), 0);                         // a boxed float nets its value
    case DSun: return zn((ai_flo_t) sun_get(x), 0);             // a sun nets its value
    case DTwin: return zn(twin_re(x), twin_im(x));               // a complex nets ITSELF (phase intact)
    case DMint: return zn(0, 0);                                 // a bare point nets nothing (the distinct nothing)
    case DNom: { ai_flo_t t = 0; struct ai_str *s = str(nom(x)->name);  // a named point nets its SPELLING's charms
      for (uintptr_t i = 0; i < s->len; i++) t += (uint8_t) txt(s)[i];
      return zn(t, 0); }
    case DTray: { struct ai_tray *v = tray(x);                 // a rank>=1 tray (the scalar stars are DGem/DSun/DTwin)
      uintptr_t i, n = tray_nelem(v);
      struct ai_zn s = zn(0, 0);                                  // rank>=1 array -> Σ elem
      if (v->type == ai_C) { ai_flo_t *d = tray_data(v);
        for (i = 0; i < n; i++) s.re += d[2*i], s.im += d[2*i+1];
        return s; }
      if (v->type == ai_O)
        for (i = 0; i < n; i++) { struct ai_zn e = ai_net(g, tray_get_obj(v, i));
          s.re += e.re, s.im += e.im; }
      else for (i = 0; i < n; i++) s.re += tray_get_flo(v, i);
      return s; } }
  return zn(1, 0); }
// $: the net observed once -- max(0, ceil) of its order-signed magnitude (a
// phaseful net takes |z|, gated by zn_false). lockstep with ai_nilp.
static intptr_t ai_saturate(struct ai *g, word x) {
  // ⚠ the charm lane is EXACTNESS, not speed: the net is a double, so above 2^53 a
  // charm comes back rounded -- and $ is the identity on every green charm (spec.l).
  if (charmp(x)) { intptr_t n = getcharm(x); return n <= 0 ? 0 : n; }
  ai_flo_t re = ai_net(g, x).re;
  if (re <= 0) return 0;
  if (re >= (ai_flo_t) maxcharm) return maxcharm;
  intptr_t i = (intptr_t) re;
  return i + (re > (ai_flo_t) i ? 1 : 0); }
lvm(lvm_saturate) {
 if (ai_ratio_exact(g, Sp[0])) { Pack(g); g = ai_ratio_rung(g, 2);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 Sp[0] = putcharm(ai_saturate(g, Sp[0])); Ip += 1; ai_musttail return Continue(); }
// THE TOWER'S THIRD RUNG: ceil(re(net x)) -- the measure retracted onto the integers, where
// saturate is this one with its floor raised to 0 and bit is it with the ceiling lowered to 1.
// ⚠ it SATURATES at the charm bounds like every rung below it: a charm is the codomain, so a
// measure that will not fit lands on the edge rather than wrapping or widening.
static intptr_t ai_ceilnet(struct ai *g, word x) {
  if (charmp(x)) return getcharm(x);
  ai_flo_t re = ai_net(g, x).re;
  if (re >= (ai_flo_t) maxcharm) return maxcharm;
  if (re <= (ai_flo_t) mincharm) return mincharm;
  intptr_t i = (intptr_t) re;
  return i + (re > (ai_flo_t) i ? 1 : 0); }
lvm(lvm_ceil) {
 if (ai_ratio_exact(g, Sp[0])) { Pack(g); g = ai_ratio_rung(g, 1);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 Sp[0] = putcharm(ai_ceilnet(g, Sp[0])); Ip += 1; ai_musttail return Continue(); }

// ============================================================================
// io
// ============================================================================
// THE ATOMIC-EDGE CONTRACT: every write can grow a backing (a GC), so an io op
// spanning more than one write must PARK its heap operand (ai_push -> g->sp) and
// RE-READ it across each one -- never hold a raw pointer over an edge. the whole
// io_* family holds it without exception; the lam_* helpers are pure (no io, no
// alloc) and cannot open an edge.
static ai_inline bool iop(word x) { return lamp(x) && cell(x)->ap == lvm_port_io; }
// THE PORT AN OP ACTS ON. in/out/err are three NAMES, not three devices: a task that
// wears its own stdio (hook 6, the chain (i o e); a non-port element keeps the
// console) reaches them through here, and every op routes its operand IN PLACE so the
// re-read across a GC edge finds the same port the op started on. ⚠ the CALL SITES ask
// the steady-state question first, so an op that is not routing pays a load and a
// branch and never the store back.
// ⚠ OP-LEVEL ONLY -- id?, peek, hot? and the image all still answer the static. prel's
// tap/jug read the port head by index, so a routed peek would lie to them.
static ai_inline word io_route(struct ai *g, word x) {
 if (g->hot_io == zero) return x;
 word l = g->hot_io, s;
 if (x == (word) &ai_stdin)       s = A(l);
 else if (x == (word) &ai_stdout) s = chainp(B(l)) ? A(B(l)) : zero;
 else if (x == (word) &ai_stderr) s = chainp(B(l)) && chainp(BB(l)) ? A(BB(l)) : zero;
 else return x;
 return iop(s) ? s : x; }
// THE DESCRIPTOR, and the only way to it: the vt says whether there is one, so a
// port whose door is not a device answers -1 and no cast is ever taken on faith.
intptr_t ai_io_fd(struct ai_io const *i) {
 return i->vt == &ai_fd_port_vt ? getcharm(((struct ai_fio const*) i)->fd) : -1; }

// --- the buffered lanes (generic, above the vt) ---
// a HEAP fd port is an ai_bio (love.h), dressed lazily; bio_of is the ONE guard
// (heap AND fd-backed), and nothing reads past the head without it. zgetc serves
// ungetc -> the pending run -> one readn gulp, and THAT ORDER IS THE PARK LAW: a
// port holding bytes is readable however quiet its fd is; a dry gulp answers
// IoWouldBlock and the caller parks. a read drains pending writes first (the
// request/response crossover).
static ai_inline struct ai_bio *bio_of(struct ai *g, struct ai_io *i) {
 return i->vt == &ai_fd_port_vt && in_live_pool(ai_core_of(g), (word const*) i)
      ? (struct ai_bio*) i : NULL; }
static ai_inline bool bio_rpending(struct ai_bio *b) {
 return b && b->rbuf && !(b->rbuf & 1) && getcharm(b->rpos) < getcharm(b->rlen); }
static ai_inline bool bio_wpending(struct ai_bio *b) {
 return b && b->wbuf && !(b->wbuf & 1) && getcharm(b->wlen) > 0; }
// the scheduler's half of the park law above, declared up by find_runnable.
static ai_inline bool wait_buffered(struct ai *g, lvm_t *ap, word x, int fd) {
 return (ap == lvm_fgetc || ap == lvm_await) && iop(x)
     && ai_io_fd((struct ai_io*) x) == fd
     && bio_rpending(bio_of(g, (struct ai_io*) x)); }
// the write run outgrew its backing: double it, pending bytes and all (only
// reachable when a device took less than the whole run)
static struct ai *bio_wgrow(struct ai *g) {
 struct ai_bio *b = (struct ai_bio*) ai_core_of(g)->io;
 uintptr_t n = getcharm(b->wlen), cap = len(str(b->wbuf));
 if (!ai_ok(g = str0(g, cap ? cap * 2 : ai_iobuf))) return g;
 struct ai *fc = ai_core_of(g);
 b = (struct ai_bio*) fc->io;
 struct ai_str *nb = str(fc->sp[0]);
 memcpy(txt(nb), txt(str(b->wbuf)), n);
 b->wbuf = word(nb);
 gen_wb(fc, word(b), b->wbuf);
 fc->sp += 1;
 return g; }
// ⚠ what did not land stays pending: the run slides down to the front and the next
// drain carries it (zeroing wlen up front once dropped the tail on a mid-buffer EPIPE)
static struct ai *io_wdrain(struct ai *g, struct ai_io *i) {
 if (!ai_ok(g) || !bio_wpending(bio_of(g, i))) return g;
 struct ai_port_vt const *vt = i->vt;
 if (!vt->writen) return g;                     // no write door: the run waits for one
 for (;;) {
  struct ai_bio *b = (struct ai_bio*) i;
  uintptr_t n = getcharm(b->wlen);
  if (!n) return g;
  intptr_t k;
  avec(g, i, k = vt->writen(&g, (unsigned char*) txt(str(b->wbuf)), n));
  if (!ai_ok(g)) return g;
  b = (struct ai_bio*) i;                       // writen may allocate: re-derive
  // ⚠ the device is GONE: drop the run -- keeping it parks a task forever
  // (close and seal wait for an empty run)
  if (k < 0) return b->wlen = putcharm(0), g;
  if (!k) return g;
  char *w = txt(str(b->wbuf));
  if ((uintptr_t) k < n) memmove(w, w + k, n - (uintptr_t) k);
  b->wlen = putcharm(n - (uintptr_t) k); } }
// io_refill's third answer, beside a byte and EOF: the device has nothing right
// now. distinct on purpose; never escapes lvm_fgetc.
#define IoWouldBlock ((uintptr_t) -2)
// the three answers for every port. no read method = END; no buffer = ask for one byte.
// WHICH BIO OWNS THIS PORT'S READ RUN: its own, or -- for the static input port on a seat
// that lent it one -- the BORROWED one in `inport`. A static cannot own a heap buffer, so a
// frontend that can put fd 0 back where its reader stopped parks a heap bio there instead;
// same fd and same vt, so every lane below reads it verbatim.
// ⚠ `in` KEEPS ITS IDENTITY AND ITS ONE-BYTE FACE: chug still finds nothing in hand, flow
// still drips, (id? p in) still holds -- rebinding `in` to the bio instead would break that
// last one, since bao's `reads` folded its own `in` at egg-compile time. Only the DEVICE
// reads in gulps, which is why nothing above this line can tell, and why the fd offset it
// runs ahead of is the frontend's to rewind before anyone inherits it.
static ai_inline struct ai_bio *rbio_of(struct ai *g, struct ai_io *i) {
 struct ai_bio *b = bio_of(g, i);
 return b ? b : i == &ai_stdin.io ? (struct ai_bio*) ai_core_of(g)->inport : NULL; }
static struct ai *io_refill(struct ai *g) {
 struct ai *fc = ai_core_of(g);
 struct ai_bio *b = rbio_of(g, fc->io);
 struct ai_port_vt const *vt = fc->io->vt;
 if (!vt->readn) return fc->b = EOF, g;
 if (!b) {                                       // no buffer: the same lane at n = 1
  unsigned char c;
  intptr_t k = vt->readn(g, &c, 1);
  if (k > 0) fc->b = c;
  else if (k < 0) fc->b = EOF;
  else fc->b = IoWouldBlock;
  return g; }
 if (bio_wpending(b)) {                          // the crossover: our unsent ask goes first
  if (!ai_ok(g = io_wdrain(g, fc->io))) return g;
  fc = ai_core_of(g), b = rbio_of(g, fc->io); }
 if (!b->rbuf || (b->rbuf & 1)) {                // first buffered read: dress the backing
  if (!ai_ok(g = str0(g, ai_iobuf))) return g;
  fc = ai_core_of(g), b = rbio_of(g, fc->io);    // the GC may have moved the port
  b->rbuf = fc->sp[0];
  b->rpos = b->rlen = putcharm(0);
  gen_wb(fc, (word) b, b->rbuf);                 // a tenured port takes a young backing
  fc->sp += 1; }
 struct ai_str *r = str(b->rbuf);
 intptr_t k = vt->readn(g, (unsigned char*) txt(r), r->len);
 if (k > 0) {
  b->rlen = putcharm(k), b->rpos = putcharm(1);
  fc->b = (unsigned char) txt(r)[0];
  return g; }
 if (k < 0) return fc->b = EOF, g;
 // k == 0 is "would block", the ordinary answer. ⚠ NEVER WAIT HERE: this runs
 // under lvm_fgetc, one op -- a blocking poll stops the whole VM, not the reading
 // task. hand it back and let the caller park.
 return fc->b = IoWouldBlock, g; }
static ai_inline struct ai *zgetc(struct ai*g) {
 if (!ai_ok(g)) return g;
 struct ai *fc = ai_core_of(g);
 struct ai_io *i = fc->io;
 if (getcharm(i->ungetc_buf) != EOF) {
  fc->b = getcharm(i->ungetc_buf);
  i->ungetc_buf = putcharm(EOF);
  return g; }
 struct ai_bio *b = rbio_of(g, i);
 if (bio_rpending(b)) {
  uintptr_t p = getcharm(b->rpos);
  fc->b = (unsigned char) txt(str(b->rbuf))[p];
  b->rpos = putcharm(p + 1);
  return g; }
 return io_refill(g); }
// the pushback is the PORT's, not the device's: one head word for every kind of port
static ai_inline struct ai *zungetc(struct ai*g, int c) {
 if (!ai_ok(g)) return g;
 struct ai *fc = ai_core_of(g);
 struct ai_io *i = fc->io;
 i->ungetc_buf = putcharm(c);
 return fc->b = c, g; }
static struct ai *ioputc(struct ai*g, int c) {
 if (!ai_ok(g)) return g;
 struct ai *fc = ai_core_of(g);
 struct ai_bio *b = bio_of(g, fc->io);
 struct ai_port_vt const *vt = fc->io->vt;
 if (!vt->writen) return g;                      // no write door: the byte goes nowhere
 if (!b) {                                       // no buffer: the same lane at n = 1.
  unsigned char x = (unsigned char) c;           // ⚠ src is a C LOCAL, so a sink that
  if (!vt->writen(&g, &x, 1) && ai_ok(g))        // grows on the first ask lands it on
   vt->writen(&g, &x, 1);                        // the second -- the growth made room.
  return g; }
 if (!b->wbuf || (b->wbuf & 1)) {                // dress the write backing
  if (!ai_ok(g = str0(g, ai_iobuf))) return g;
  fc = ai_core_of(g), b = (struct ai_bio*) fc->io;
  b->wbuf = fc->sp[0];
  b->wlen = putcharm(0);
  gen_wb(fc, (word) b, b->wbuf);
  fc->sp += 1; }
 uintptr_t n = getcharm(b->wlen);
 if (n >= len(str(b->wbuf))) {       // a drain the device short-changed left
  if (!ai_ok(g = bio_wgrow(g))) return g;        // no room: the residue keeps its place
  fc = ai_core_of(g), b = (struct ai_bio*) fc->io; }
 struct ai_str *w = str(b->wbuf);
 txt(w)[n] = (char) c;
 b->wlen = putcharm(n + 1);
 return n + 1 >= w->len ? io_wdrain(g, fc->io) : g; }
// ⚠ flush means TRY, never wait: what the device would not take stays in the write
// run and lands at the next write, at close, or through the finalizer's drain
static struct ai *zflush(struct ai*g) {
 if (!ai_ok(g)) return g;
 g = io_wdrain(g, ai_core_of(g)->io);
 return ai_ok(g) ? ai_core_of(g)->io->vt->flush(g) : g; }
// the exported faces (love.h): a host nif consults/drains the read run without
// knowing the bio shape -- swig's first course rides these.
uintptr_t ai_io_pending(struct ai *g, struct ai_io *i) {
 struct ai_bio *b = rbio_of(g, i);
 return bio_rpending(b) ? (uintptr_t)(getcharm(b->rlen) - getcharm(b->rpos)) : 0; }
uintptr_t ai_io_read_drain(struct ai *g, struct ai_io *i, unsigned char *dst, uintptr_t n) {
 struct ai_bio *b = rbio_of(g, i);
 if (!bio_rpending(b)) return 0;
 uintptr_t p = getcharm(b->rpos), l = getcharm(b->rlen), k = l - p < n ? l - p : n;
 memcpy(dst, txt(str(b->rbuf)) + p, k);
 b->rpos = putcharm(p + k);
 return k; }
// `unsee` OVER A COUNT: move this port's position inside the run it holds, and answer how many
// bytes moved -- a short answer IS the refusal, and the caller's only check. n > 0 un-reads
// (gives back), n < 0 re-reads (takes). It moves the POSITION, so what a give-back returns is
// whatever the run last gave, not a remembered chug.
// ⚠ SIGNED BECAUSE RELATIVE DOES NOT COMPOSE. A caller that gave back and then walked on is
// BEHIND its own position and must step forward again; give-back-only makes that second step a
// rewind to the run's start, and the reader re-reads the whole stream (bao's `reads`).
// ⚠ rbio_of, not bio_of: the run BORROWED under a static counts, and that is the whole point
// -- stdin's seek-back is ai_io_pending, so this is what puts bytes back inside it.
// ⚠ REACHES ONLY THE CURRENT RUN: a refill replaces rbuf and resets rpos, so the clamp to
// [0, rlen] is what makes a stale ask answer what is really there instead of trusting n.
// ⚠ THE RUN ONLY. The pushback byte chug lays in FRONT of it is `unsee`'s to restore.
uintptr_t ai_io_unread(struct ai *g, struct ai_io *i, intptr_t n) {
 struct ai_bio *b = rbio_of(g, i);
 if (!b || !b->rbuf || (b->rbuf & 1)) return 0;
 uintptr_t p = getcharm(b->rpos), l = getcharm(b->rlen);
 if (n >= 0) { uintptr_t k = p < (uintptr_t) n ? p : (uintptr_t) n;
               b->rpos = putcharm(p - k); return k; }
 uintptr_t want = (uintptr_t) -n, room = l > p ? l - p : 0, k = room < want ? room : want;
 b->rpos = putcharm(p + k);
 return k; }
// (chug port): everything ALREADY readable, as ONE exact-length text -- the
// pushback byte if there is one, then the run. it never touches the device and
// never parks, so the gulp is: draw the first byte with `see` (which refills, and
// parks if it must), unsee it, then chug the run whole.
// ⚠ "" IS THE ORDINARY ANSWER, not a failure: a port with no run answers it (the
// statics, which bio_of refuses and whose text is not in memory), so a caller must
// draw with `see` rather than spin here. the length is known BEFORE the string is
// minted, which is the whole point -- no over-allocate, no trim.
ai_noinline static struct ai *chug_str(struct ai *g, struct ai_io *i) {
 uintptr_t u = getcharm(i->ungetc_buf) != EOF ? 1 : 0;
 struct ai_port_vt const *vt = i->vt;
 g->io = i;                                   // athand reads it, as readn does
 uintptr_t n = u + (rbio_of(g, i) ? ai_io_pending(g, i)
                    : vt->athand ? vt->athand(g, ai_iobuf) : 0);
 if (!ai_ok(g = str0(g, n))) return g;
 i = ai_core_of(g)->io;                       // str0 collects: the port may have moved
 if (n) {
  char *d = txt(g->sp[0]);
  if (u) *d = (char) getcharm(i->ungetc_buf), i->ungetc_buf = putcharm(EOF);
  // the fill splits where the count did: a bio drains its buffer, an at-hand source
  // reads its own text. never a device -- for one, athand answered 0.
  if (n - u) {
   if (rbio_of(g, i)) ai_io_read_drain(g, i, (unsigned char*) d + u, n - u);
   else vt->readn(g, (unsigned char*) d + u, n - u); } }
 return g->sp[1] = g->sp[0], g->sp += 1, g; }

lvm(lvm_chug) {
 if (g->hot_io != zero) Sp[0] = io_route(g, Sp[0]);
 if (!iop(Sp[0])) { Sp[0] = EmptyString; ai_musttail return Next(1); }
 Pack(g); g = chug_str(g, (struct ai_io*) Sp[0]);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

// (inhand port): how many bytes this port holds ready -- the count `chug` would hand over.
// The borrowed run counts, so a reader can ask whether anyone ELSE has drawn on the port
// since it last looked, which is the only way to know its own charlist is still the port's.
lvm(lvm_inhand) {
 if (g->hot_io != zero) Sp[0] = io_route(g, Sp[0]);
 Sp[0] = putcharm(iop(Sp[0]) ? (ai_word) ai_io_pending(g, (struct ai_io*) Sp[0]) : 0);
 ai_musttail return Next(1); }

// (unchug port n): hand back up to n bytes of the run this port already gave out, so a
// caller that chugged more than it used leaves the rest where the port's position sees it.
// Answers how many went back -- a short answer IS the refusal (ai_io_unread's ⚠ notes).
lvm(lvm_unchug) {
 if (g->hot_io != zero) Sp[0] = io_route(g, Sp[0]);
 Sp[1] = putcharm(iop(Sp[0]) && charmp(Sp[1]) && getcharm(Sp[1]) != 0
                  ? (ai_word) ai_io_unread(g, (struct ai_io*) Sp[0],
                                           (intptr_t) getcharm(Sp[1])) : 0);
 ai_musttail return Nextp(1, 1); }

struct ai *ai_io_wflush(struct ai *g, struct ai_io *i) { return io_wdrain(g, i); }

uintptr_t ai_io_wpending(struct ai *g, struct ai_io *i) {
 struct ai_bio *b = bio_of(g, i);
 return bio_wpending(b) ? (uintptr_t) getcharm(b->wlen) : 0; }

// GC-context finalizer hook: weak no-op; the host overrides with write(2).
__attribute__((weak)) void ai_fd_drain(int fd, void const *p, uintptr_t n) {
 (void) fd; (void) p; (void) n; }

struct ci { struct ai_io io; ai_word head; }; // charlist input
// ⚠ `t` IS A C POINTER RIDING A THREAD WORD, and that is sound for one reason: gcp
// forwards only what lies inside a from-space, so a .rodata address passes through
// every collection untouched. It also means the text must OUTLIVE the port -- only
// immortal strings here (the baked library, love.h's struct ai_lib).
struct ti { struct ai_io io; ai_word t; ai_word i; }; // C string input
struct to { struct ai_io io; struct ai_str *buf; ai_word i; }; // lisp string output
static struct ai *noop_flush(struct ai *g) { return g; }

// the charlist source's read door: walks the spine, never blocks, so a spent list
// is the END. a buffer is NOT the answer here -- no syscall on this row to
// amortize, and the spine IS the run, which is what athand counts.
// ⚠ a charm outside 0..255 lands as its LOW BYTE: the raw charm once forged the
// end of the stream (test/io.l's tap section).
static uintptr_t ci_athand(struct ai *g, uintptr_t n) {
 word h = ((struct ci*) g->io)->head;
 uintptr_t k = 0;
 while (k < n && chainp(h)) k++, h = B(h);
 return k; }
static intptr_t ci_readn(struct ai *g, unsigned char *dst, uintptr_t n) {
 struct ci *i = (struct ci*) g->io;
 uintptr_t k = 0;
 while (k < n && chainp(i->head))
  dst[k++] = (unsigned char) getcharm(A(i->head)), i->head = B(i->head);
 return k ? (intptr_t) k : -1; }

// the C string source's read door: NUL ends it, so the text needs no length beside
// it. no buffer, and none wanted: the text is already in memory, so athand scans
// ahead for the run and the source never becomes a love value.
static uintptr_t ti_athand(struct ai *g, uintptr_t n) {
 struct ti *i = (struct ti*) g->io;
 char const *t = (char const*) i->t + (uintptr_t) getcharm(i->i);
 uintptr_t k = 0;
 while (k < n && t[k]) k++;
 return k; }
static intptr_t ti_readn(struct ai *g, unsigned char *dst, uintptr_t n) {
 struct ti *i = (struct ti*) g->io;
 char const *t = (char const*) i->t;
 uintptr_t p = (uintptr_t) getcharm(i->i), k = 0;
 while (k < n && t[p]) dst[k++] = (unsigned char) t[p++];
 i->i = putcharm((intptr_t) p);
 return k ? (intptr_t) k : -1; }

// the string sink's write door: land what fits, else DOUBLE and answer 0 having
// landed nothing. ⚠ the grow and the copy cannot share a call: str0 collects, and
// src may be the very string being printed -- the caller re-derives and comes back.
static intptr_t to_writen(struct ai **fp, unsigned char const *src, uintptr_t n) {
 struct ai *g = *fp;
 struct to *o = (struct to*) g->io;
 uintptr_t i = getcharm(o->i), cap = len(o->buf);
 if (i < cap) {
  uintptr_t k = cap - i < n ? cap - i : n;
  memcpy(txt(o->buf) + i, src, k);
  o->i = putcharm(i + k);
  return (intptr_t) k; }
 if (!ai_ok(*fp = g = str0(g, cap ? cap * 2 : ai_iobuf))) return 0;
 o = (struct to*) g->io;                  // GC may have moved it; g->io is GC-traced
 struct ai_str *nb = str(g->sp[0]);
 memcpy(txt(nb), txt(o->buf), i);
 o->buf = nb;
 gen_wb(g, (word) o, (word) nb);   // a tenured string-sink takes a fresh young backing -> remember it
 g->sp++;
 return 0; }

struct ai_port_vt const
 ai_ti_vt     = { noop_flush, NULL,      ti_readn, ti_athand },  // a C string: the baked library's door (lvm_lib)
 ai_to_vt     = { noop_flush, to_writen, NULL,     NULL },       // a string sink: prel's `jug`
 ai_closed_vt = { noop_flush, NULL,      NULL,     NULL },       // what `close` leaves behind
 ai_ci_vt     = { noop_flush, NULL,      ci_readn, ci_athand };  // a charlist: prel's `tap`

// (fputc port byte) — write byte to port; return byte.
lvm(lvm_fputc) {
 if (g->hot_io != zero) Sp[0] = io_route(g, Sp[0]);
 if (iop(Sp[0])) {
  g->io = (struct ai_io*) Sp[0];
  Pack(g);
  // backpressure, as in lvm_fputs -- but the drain is BEHIND the test: draining
  // every put would turn a put loop into one write(2) per byte
  if (ai_io_wpending(g, (struct ai_io*) g->sp[0]) >= ai_iobuf) {
   g = io_wdrain(g, (struct ai_io*) g->sp[0]);
   if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
   if (ai_io_wpending(g, (struct ai_io*) g->sp[0]) >= ai_iobuf) {
    Unpack(g);
    g->next_wake_at = ai_clock() + 1;
    ai_musttail return Ap(lvm_yield_sw, g); } }
  if (!ai_ok(g = ioputc(g, getcharm(g->sp[1])))) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g); }
 ai_musttail return Nextp(1, 1); }

// (fflush port): FLUSH MEANS DELIVER -- a short-answering device parks the TASK
// and the op re-runs (safe: a flush consumes nothing)
lvm(lvm_fflush) {
 if (g->hot_io != zero) Sp[0] = io_route(g, Sp[0]);
 if (iop(Sp[0])) {
  g->io = (struct ai_io*) Sp[0];
  Pack(g);
  if (!ai_ok(g = zflush(g))) ai_musttail return Ap(_lvm_ghelp, g);
  if (ai_io_wpending(g, (struct ai_io*) g->sp[0])) {
   Unpack(g);
   g->next_wake_at = ai_clock() + 1;      // the write residue's poll -- see io_wdrain
   ai_musttail return Ap(lvm_yield_sw, g); }
  Unpack(g); }
 ai_musttail return Next(1); }

// (fputs port s) — write every byte of string-or-cask s; no-op on misuse. bytes_of
// re-reads each iteration so GC inside ioputc can forward it.
lvm(lvm_fputs) {
 if (g->hot_io != zero) Sp[0] = io_route(g, Sp[0]);
 if (iop(Sp[0]) && (strp(Sp[1]) || caskp(Sp[1]))) {
  g->io = (struct ai_io*) Sp[0];
  uintptr_t i = 0, l = len(bytes_of(Sp[1]));
  // the bulk lane when the port has one; a 0 makes one byte of progress through
  // ioputc (its C-local src is the one shape that can grow and land in one breath).
  // ⚠ the direct stroke is only for an EMPTY buffer: going direct past a pending
  // run would overtake it and the stream comes out shuffled.
  intptr_t (*wn)(struct ai**, unsigned char const*, uintptr_t) = g->io->vt->writen;
  Pack(g);
  g = io_wdrain(g, (struct ai_io*) g->sp[0]);   // buffered puts land before the bulk stroke
  // ⚠ backpressure: the write run is a buffer, not a queue -- an op that would push
  // it past its own size waits for the device (nothing consumed, the re-run free).
  // the bound is one buffer plus one say, never an accumulation across ops.
  if (ai_ok(g) && ai_io_wpending(g, (struct ai_io*) g->sp[0]) >= ai_iobuf) {
   Unpack(g);
   g->next_wake_at = ai_clock() + 1;            // the write residue's poll -- see io_wdrain
   ai_musttail return Ap(lvm_yield_sw, g); }
  while (ai_ok(g) && i < l) {
   struct ai *w = g;       // the frame BY ADDRESS, off the restrict-qualified param
   intptr_t k = wn && !bio_wpending(bio_of(g, (struct ai_io*) g->sp[0]))
              ? wn(&w, (unsigned char const*) txt(bytes_of(w->sp[1])) + i, l - i) : 0;
   g = w;
   if (k > 0) i += (uintptr_t) k;
   else g = ioputc(g, txt(bytes_of(g->sp[1]))[i++]); }
  if (!ai_ok(g = zflush(g))) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g); }
 ai_musttail return Nextp(1, 1); }

static struct ai*gfputbn(struct ai *g, intptr_t n, uint8_t b, struct ai_io *o);
lvm(lvm_fputbn) {
 if (g->hot_io != zero) Sp[0] = io_route(g, Sp[0]);
 if (iop(Sp[0])) {
   Pack(g);
   g = gfputbn(g, getcharm(Sp[1]), getcharm(Sp[2]), (struct ai_io*) Sp[0]);
   if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
   Unpack(g);
   Sp[2] = Sp[1]; }
 ai_musttail return Nextp(1, 2); }

static struct ai*ioputs(struct ai*g, char const *s) {
 while (*s) g = ioputc(g, *s++);
 return g; }

static struct ai*ioputn(struct ai *g, intptr_t n, uint8_t b) {
 uintptr_t
  m = n >= 0 || b != 10 ? (uintptr_t) n : (g = ioputc(g, '-'), -(uintptr_t) n),
  q = m / b,
  r = m % b;
 if (q) g = ioputn(g, q, b);
 return ioputc(g, ai_digits[r]); }

// the terminal scare face's FLOOR: the printer proper lives in love (post.l), and
// by here the VM has stopped, so there is nobody left to run it. this spells the
// shapes a condition actually wears -- name, text, number, list -- and hands every
// other kind its address. the full face is bao's shell-help, which is love and runs
// while the machine still does. no allocation: safe on an exhausted heap.
static struct ai *facex(struct ai *g, word x, int d) {
 if (charmp(x)) return ioputn(g, getcharm(x), 10);
 if (x == ZeroPoint) return ioputs(g, "()");
 struct ai_str *nm = nom_str(g, x);
 if (!nm && datp(x) && typ(x) == DNom) nm = str(nom(x)->name);
 if (nm) { for (uintptr_t i = 0; ai_ok(g) && i < len(nm); i++) g = ioputc(g, txt(nm)[i]);
           return g; }
 if (datp(x) && typ(x) == DString) {
  g = ioputc(g, '"');
  for (uintptr_t i = 0, n = len(x); ai_ok(g) && i < n; i++) g = ioputc(g, txt(x)[i]);
  return ioputc(g, '"'); }
 if (chainp(x) && d < 4) {                        // bounded: a cyclic condition must not spin
  for (g = ioputc(g, '(');; g = ioputc(g, ' '), x = B(x)) {
   g = facex(g, A(x), d + 1);
   if (!chainp(B(x))) return ioputc(g, ')'); } }
 return ioputn(ioputc(g, '\\'), (intptr_t) x, 36); }

// the terminal scare face (love.h): stashed condition data prints ";; a b" on err;
// the bare scare (oom) prints ";; oom@len=N". best-effort.
void ai_scare_face_(struct ai *g) {
 if (!(g = ai_core_of(g))) return;
 g->io = &ai_stderr.io;
 if (zerop(g->scare_a) && zerop(g->scare_b)) {
  g = ioputs(g, ";; oom@len=");
  if (ai_ok(g)) g = ioputn(g, (intptr_t) ai_core_of(g)->len, 10); }
 else {
  g = ioputs(g, ";; ");
  if (ai_ok(g)) g = facex(g, ai_core_of(g)->scare_a, 0);
  if (ai_ok(g)) g = ioputc(g, ' ');
  if (ai_ok(g)) g = facex(g, ai_core_of(g)->scare_b, 0); }
 if (ai_ok(g)) g = ioputc(g, '\n');
 if (ai_ok(g)) zflush(g); }

static ai_inline struct ai*gfputbn(struct ai *g, intptr_t n, uint8_t b, struct ai_io *o) {
 return g->io = o, ioputn(g, n, b); }

// --- partial-application introspection ---
// a partial-app closure is a thread headed lvm_unc (or [lvm_cur n][lvm_unc …]);
// each unc cell holds a captured arg at [1] and a link at [2], so the base value
// is terminal_link-2 and the args are the chain of [1] fields, newest first.
static bool fn_partialp(union u *k) {
 return k[0].ap == lvm_unc || (k[0].ap == lvm_cur && k[2].ap == lvm_unc); }
static ai_inline union u *fn_unc0(union u *k) {
 return k[0].ap == lvm_cur ? k + 2 : k; }       // first unc cell
static union u *fn_base(union u *k, int *nargs) { // base value + captured-arg count
 union u *u = fn_unc0(k), *link;
 int n = 0;
 for (;;) { link = u[2].m; n++; if (link[0].ap != lvm_unc) break; u = link; }
 return *nargs = n, link - 2; }
static word fn_arg(union u *k, int i, int nargs) { // i-th arg in application order
 union u *u = fn_unc0(k);
 for (int w = nargs - 1 - i; w > 0; w--) u = u[2].m;
 return u[1].x; }

// the source \-expr stashed at value[-1] by a compiled lambda, or 0. only an
// ala/k0s lambda reserves that leading cell, so probe the TAG (which records the
// true start) instead of reading value[-1] -- a wrap/partial/continuation puts its
// value AT the start, and value[-1] there reads the neighbouring object.
// in_heap: the main pool OR the major pool (tenured objects live there).
static ai_inline bool in_heap(struct ai *c, word x) {
 return (ptr(x) >= ptr(c) && ptr(x) < ptr(c) + c->len) || (ptr(x) >= c->major_base && ptr(x) < c->major_hp); }
static word fn_src(struct ai *c, union u *k, word x) {
 // the two pools are independent mallocs (major may sit above or below): test each range
 bool xin = (ptr(x) > ptr(c) && ptr(x) < ptr(c) + c->len) || (ptr(x) >= c->major_base && ptr(x) < c->major_hp);
 if (!xin || fn_partialp(k)) return 0;
 if (k == tag_head(ttag(c, k))) return 0;       // value at allocation start: no leading src cell
 word s = k[-1].x;
 return lamp(s) && in_heap(c, s) && chainp(s) ? s : 0; }
// (lamsrc f): that source, or () -- the one heap-layout question the printer in
// love cannot ask for itself (reading value[-1] unguarded walks a neighbour).
lvm(lvm_lamsrc) {
 word x = Sp[0], s = lamp(x) && !datp(x) ? fn_src(g, cell(x), x) : 0;
 Sp[0] = s ? s : ZeroPoint;
 ai_musttail return Next(1); }

// (nifnom f): a nif's roster spelling, or (). the book cannot answer this: two
// names can share one nif value (link and ><, peep and ->), and def1 is which of
// them is the NAME. the printer's other C-only question.
lvm(lvm_nifnom) {
 char const *nm = ai_nif_name(Sp[0]);
 if (!nm) ai_musttail return Answer(ZeroPoint);
 uintptr_t n = strlen(nm);
 Have(str_type_width + b2w(n));
 struct ai_str *s = ini_str(str(Hp), n); Hp += str_type_width + b2w(n);
 memcpy(txt(s), nm, n);
 ai_musttail return Answer(word(s)); }

static ai_inline bool lam_head(struct ai *g, word a) {        // is a the symbol \ ?
 struct ai_str *nm;                                          // a named sym (name . mint); nom_str is 0 for a bare mint / the core
 return (nm = nom_str(g, a)) && len(nm) == 1 && txt(nm)[0] == '\\'; }
static ai_inline bool lam_isp(struct ai *g, word x) {         // (\ b.. body): >=2 operands
 return chainp(x) && lam_head(g, A(x)) && chainp(B(x)) && chainp(BB(x)); }
// (fgetc port): a non-port reads as an already-empty stream (EOF), so a
// read-until-(-1) loop over a misused port is bounded
lvm(lvm_fgetc) {
 if (g->hot_io != zero) Sp[0] = io_route(g, Sp[0]);
 if (iop(Sp[0])) {
  struct ai_io *i = (struct ai_io*) Sp[0];
  struct ai_bio *bb = bio_of(g, i);
  if (bio_wpending(bb)) {                 // our unsent ask goes out before we wait for the answer
   g->io = i;
   Pack(g);
   if (!ai_ok(g = io_wdrain(g, i))) ai_musttail return Ap(_lvm_ghelp, g);
   Unpack(g); }
  // ⚠ no readiness pre-guard: zgetc already makes that test, and asking first
  // lied on the kernel (reading an output fd parked forever where it now reads
  // the END). cue?/await still ask -- they have no read to answer them.
  Pack(g);
  g->io = i;
  if (!ai_ok(g = zgetc(g))) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g);
  if (g->b == IoWouldBlock) {          // the refill raced and lost -- park, don't spin
   g->next_wait_fd = ai_io_fd((struct ai_io*) Sp[0]);   // re-read: the gc may have moved it
   ai_musttail return Ap(lvm_yield_sw, g); }
  Sp[0] = putcharm(g->b); }
 else Sp[0] = putcharm(EOF);
 ai_musttail return Next(1); }

// (await port): cooperatively PARK until the port's fd is readable, then return
// the port (so it chains into a read) -- for fds you can't drain a byte at a time
// (signalfd, timerfd). Ip is unadvanced, so the task re-checks on reschedule.
lvm(lvm_await) {
 if (g->hot_io != zero) Sp[0] = io_route(g, Sp[0]);   // and the ROUTED port is what it answers -- the read that chains off it lands there too
 if (iop(Sp[0])) {
  intptr_t fd = ai_io_fd((struct ai_io*) Sp[0]);
  // ⚠ the buffer counts: a port holding bytes is readable however quiet its fd is
  if (fd >= 0 && !bio_rpending(bio_of(g, (struct ai_io*) Sp[0])) && !ai_ready(fd, ai_wait_in)) {
   g->next_wait_fd = fd;
   ai_musttail return Ap(lvm_yield_sw, g); } }
 ai_musttail return Next(1); }

// (fungetc port byte) — push back one byte, return the byte.
lvm(lvm_fungetc) {
 if (g->hot_io != zero) Sp[0] = io_route(g, Sp[0]);
 if (iop(Sp[0])) {
  struct ai_io *i = (struct ai_io*) Sp[0];
  Pack(g);
  g->io = i;
  if (!ai_ok(g = zungetc(g, getcharm(g->sp[1])))) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g); }
 ai_musttail return Nextp(1, 1); }

// heap-port finalizer: runs inside GC (from-space readable); fd < 0 means
// already closed or a non-OS fd
static void io_close(void *p) {
 struct ai_bio *b = p;                         // every finalized port is a bio (ai_io_alloc made it)
 intptr_t fd = ai_io_fd(&b->f.io);
 if (fd < 0) return;
 if (b->wbuf && !(b->wbuf & 1) && getcharm(b->wlen) > 0)   // unflushed bytes ride out raw --
  ai_fd_drain((int) fd, txt(str(b->wbuf)), (uintptr_t) getcharm(b->wlen));   // from-space is readable here
 ai_fd_close(fd); }

// heap-allocate a stream port for an OS fd: push it on Sp[0], register io_close
struct ai *ai_io_alloc(struct ai *g, int fd) {
 uintptr_t const n = Width(struct ai_bio);     // a heap fd port carries the buffer lanes (love.h)
 if (ai_ok(g = ai_have(g, n + Width(struct ai_tag) + Width(struct ai_fz) + 1))) {
  union u *k = bump(g, n + Width(struct ai_tag));
  struct ai_bio *io = (struct ai_bio*) k;
  io->f.io.ap = lvm_port_io;
  io->f.io.vt = &ai_fd_port_vt;
  io->f.io.ungetc_buf = putcharm(EOF);
  io->f.fd = putcharm(fd);
  io->rbuf = io->wbuf = 0;                     // never dressed (io_refill/ioputc dress lazily)
  io->rpos = io->rlen = io->wlen = putcharm(0);
  *--g->sp = (word) tagthread(k, n);            // stack slot reserved by the +1 in have()
  struct ai_fz *z = bump(g, Width(struct ai_fz));
  z->p = k, z->fn = io_close, z->next = g->fz, g->fz = z; }
 return g; }

// A token is a plain decimal integer iff it is [+-]?[0-9]+ with no leading-zero
// prefix (a leading zero is octal's prefix; bare "0" parses as decimal).
static ai_inline bool is_dec_int(char const *s, uintptr_t n) {
 uintptr_t i = (n && (s[0] == '-' || s[0] == '+')) ? 1 : 0;
 if (i >= n) return false;                       // a lone sign is a symbol
 if (s[i] == '0' && n - i > 1) return false;     // leading zero -> octal's, below
 for (; i < n; i++) if (s[i] < '0' || s[i] > '9') return false;
 return true; }

// ..a hex integer iff it is [+-]?0[xX][0-9a-fA-F]+ -- at least one digit, so a
// bare "0x" stays an honest symbol..
static ai_inline bool is_hex_int(char const *s, uintptr_t n) {
 uintptr_t i = (n && (s[0] == '-' || s[0] == '+')) ? 1 : 0;
 if (n - i < 3 || s[i] != '0' || (s[i+1] | 32) != 'x') return false;
 for (i += 2; i < n; i++)
  if (!((s[i] >= '0' && s[i] <= '9') || ((s[i] | 32) >= 'a' && (s[i] | 32) <= 'f'))) return false;
 return true; }

// ..and octal iff [+-]?0[0-7]+ ("08" keeps the strtod -> intern path). ALL THREE
// READ AT FULL PRECISION through ai_big_read_*: an integer literal is fixnum /
// box / bignum by its VALUE (strtol overflowed differently per libc, so the same
// source once read as three different numbers).
static ai_inline bool is_oct_int(char const *s, uintptr_t n) {
 uintptr_t i = (n && (s[0] == '-' || s[0] == '+')) ? 1 : 0;
 if (n - i < 2 || s[i] != '0') return false;
 for (i += 1; i < n; i++) if (s[i] < '0' || s[i] > '7') return false;
 return true; }

static ai_inline struct ai *ioread1sym(struct ai*g, uintptr_t d, int c), *ioread1str(struct ai*g, uintptr_t d);

struct ai *grbufg(struct ai *g, uintptr_t len) {
 if (ai_ok(g = str0(g, 2 * len)))
  memcpy(txt(g->sp[0]), txt(g->sp[1]), len),
  g->sp[1] = g->sp[0],
  g->sp++;
 return g; }

static ai_noinline double strtod_wrap(struct ai*g, word x) {
 struct ai_str *s = str(x);
 if (!strp(x) || !s->len) return NAN;
 char *e, *b = off_pool(g);
 memcpy(b, s->bytes, s->len);
 b[s->len] = 0;
 double r = am_strtod(b, &e);
 return e != b && *e == 0 ? (ai_flo_t) r : (ai_flo_t) NAN; }

// (flo s): parse a string as a decimal float -> a box if the whole string parses,
// else zero (the l-side reader's twin of the C cascade)
lvm(lvm_gem) {
 word x = Sp[0];
 double d = strtod_wrap(g, x);
 if (d != d) ai_musttail return Answer(zero);
 Have(gem_req);
 Sp[0] = mk_gem(&Hp, (ai_flo_t) d);
 ai_musttail return Next(1); }

// (string x): a charlist -> the string of those bytes; a named symbol -> its
// name string; a fixnum -> the one-byte string of its low byte. Identity on any
// other type (strings, anonymous syms, zero, ...).
lvm(lvm_string) {
 word x = Sp[0];
 if (charmp(x)) {                                     // fixnum -> one-byte string
  uintptr_t req = str_type_width + b2w(1);
  Have(req);
  struct ai_str *s = (void*) Hp;
  Hp += req;
  ini_str(s, 1);
  txt(s)[0] = (char) getcharm(x);
  ai_musttail return Answer(word(s)); }
 if (nomp(x)) {                                      // a named symbol (name . mint) -> its name string; a bare point -> identity
  struct ai_str *nm = nom_str(g, x);
  Sp[0] = nm ? word(nm) : word(EmptyString);
  ai_musttail return Next(1); }
 if (chainp(x)) {                                      // charlist -> string
  uintptr_t n = llen(x), req = str_type_width + b2w(n);
  Have(req);
  struct ai_str *s = (void*) Hp;
  Hp += req;
  ini_str(s, n);
  for (uintptr_t i = 0; n--; x = B(x)) txt(s)[i++] = (char) getcharm(A(x));
  ai_musttail return Answer(word(s)); }
 if (caskp(x)) {                                      // a cask -> a fresh string copy of its bytes
  uintptr_t n = len(cask(x)->str), req = str_type_width + b2w(n);
  Have(req);
  struct ai_str *src = cask(Sp[0])->str;               // re-read post-Have (a GC may have moved the cask)
  struct ai_str *s = (void*) Hp;
  Hp += req;
  ini_str(s, n);
  memcpy(txt(s), txt(src), n);
  ai_musttail return Answer(word(s)); }
 // `string` ANSWERS A STRING: a string is the only identity, every other kind coerces
 // through hook 7. ⚠ px reaches `string` on chains and noms only, so show cannot recur.
 if (x == ZeroPoint) { Sp[0] = word(EmptyString); ai_musttail return Next(1); }   // the empty charlist
 if (strp(x) || !lamp(g->hot_show)) ai_musttail return Next(1);   // ..or the boot window, where identity stands
 Have(2);                                               // the drive grows Sp by two
 { word *dst = Sp - 2;                                  // [x show ret] -- callout_drive's 1-arg shape
   dst[0] = Sp[0], dst[1] = g->hot_show, dst[2] = word(Ip + 1);
   Sp = dst; Ip = (union u*) callout_drive; }
 ai_musttail return Continue(); }

////
/// " the parser "
//
// p0's input is a CHARLIST and its position IS the list: the cursor is one love
// value on the l stack, named by its DEPTH (a collection moves the stack, never
// a depth); p0reads piles datums above it. ⚠ a lookahead needs no pushback --
// `unget` is simply not advancing.
static ai_inline word *p0cur(struct ai *g, uintptr_t d) { return topof(ai_core_of(g)) - d; }
static ai_inline int p0peek(struct ai *g, uintptr_t d) {
 word h = *p0cur(g, d);
 return chainp(h) ? (int) getcharm(A(h)) : EOF; }
static ai_inline int p0peek2(struct ai *g, uintptr_t d) {
 word h = *p0cur(g, d);
 return chainp(h) && chainp(B(h)) ? (int) getcharm(A(B(h))) : EOF; }
static ai_inline void p0pop(struct ai *g, uintptr_t d) {
 word *c = p0cur(g, d);
 if (chainp(*c)) *c = B(*c); }
static ai_inline int p0getc(struct ai *g, uintptr_t d) {
 int c = p0peek(g, d);
 return p0pop(g, d), c; }

// the next SIGNIFICANT char, the cursor left AT it: whitespace stepped over,
// `;` and `#!` (shebang) running to end of line. a bare `#` is significant (the
// len reader macro), as is any other non-whitespace char.
static int p0skip(struct ai *g, uintptr_t d) {
 for (int c; (c = p0peek(g, d)) != EOF;) {
  if (c == ';' || (c == '#' && p0peek2(g, d) == '!'))
   while ((c = p0getc(g, d)) != EOF && c != '\n' && c != '\r');
  else if (c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\f' || !c) p0pop(g, d);
  else return c; }
 return EOF; }

static ai_inline struct ai *ioread1str(struct ai*g, uintptr_t d) {
 int c;
 size_t n = 0, lim = sizeof(word);
 for (g = str0(g, lim); ai_ok(g); g = grbufg(g, lim), lim *= 2)
  for (; n < lim; txt(g->sp[0])[n++] = c) {
   if ((c = p0getc(g, d)) == '"')                    // close quote; "" -> the empty
    return n ? (len(g->sp[0]) = n, g)                // (truthy) singleton, never allocated
             : (g->sp[0] = EmptyString, g);
   else if (c == EOF) return encode(g, ai_status_more);
   else if (c == '\\') {                             // escape: take next char
    if ((c = p0getc(g, d)) == EOF) return encode(g, ai_status_more);
    else if (c == 'n') c = '\n';
    else if (c == 't') c = '\t';
    else if (c == 'r') c = '\r';
    else if (c == 'e') c = 27;                    // \e: ESC, the terminal's own letter
    else if (c == '0') c = '\0';
    else if (c == 'x') {                          // \xHH: two hex digits
     int h1 = p0getc(g, d), h2 = p0getc(g, d);
     if (h1 == EOF || h2 == EOF) return encode(g, ai_status_more);
     int v1 = h1 <= '9' ? h1 - '0' : (h1 | 0x20) - 'a' + 10;
     int v2 = h2 <= '9' ? h2 - '0' : (h2 | 0x20) - 'a' + 10;
     c = ((v1 & 0xf) << 4) | (v2 & 0xf); } } }
 return g; }



static ai_inline struct ai *ioread1sym(struct ai*g, uintptr_t d, int c) {
 uintptr_t n = 1, lim = sizeof(intptr_t);
 if (ai_ok(g = str0(g, sizeof(word))))
  for (txt(str(g->sp[0]))[0] = c; ai_ok(g); g = grbufg(g, lim), lim *= 2)
   for (; n < lim; txt(g->sp[0])[n++] = c) {
    switch (c = p0peek(g, d)) {
     default: p0pop(g, d); continue;
     case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
     case '(': case ')': case '[': case ']': case '{': case '}':
     // note: '\'' is NOT here -- a name keeps a trailing/internal prime (x', n'',
     // the prover idiom). A LEADING ' is still quote: p0read1 dispatches it as a
     // wrap before this sounder ever runs, so only a continuation ' reaches here.
     case '"': case '`': case ',': case 0 : case EOF: {   // the cursor stays ON the terminator
      struct ai_str *s = str(g->sp[0]);
      txt(s)[len(s) = n] = 0; // zero terminate for am_strtod ; n < lim so this is safe
      // the three predicates are exhaustive over what a base-0 strtol would have
      // accepted whole, which is why the reader no longer calls it
      if (is_dec_int(txt(s), n)) return ai_big_read_dec(g);
      if (is_hex_int(txt(s), n)) return ai_big_read_hex(g);
      if (is_oct_int(txt(s), n)) return ai_big_read_oct(g);
      char *e;
      // the IEEE specials read by their own names; everything else strtod
      // would take by spelling (inf, infinity, nan) stays a symbol: a float
      // token leads with a digit (a sign or dot may front it).
      char *tx = txt(s);
      double dv;
      if (n == 8 && !memcmp(tx, "ieee-inf", 8)) dv = __builtin_inf();
      else if (n == 9 && !memcmp(tx, "-ieee-inf", 9)) dv = -__builtin_inf();
      // ⚠ no ieee-nan twin: mk_gem collapses NaN to 0, so there is no NaN value to
      // name -- "ieee-nan" stays an honest symbol, free for binding.
      else {
       char c0 = *tx == '+' || *tx == '-' ? tx[1] : *tx;
       if (!(c0 >= '0' && c0 <= '9') && c0 != '.') return intern(g);
       dv = am_strtod(tx, &e);
       if (e == tx || *e != 0) return intern(g); }
      if (ai_ok(g = ai_have(g, gem_req)))
       g->sp[0] = mk_gem(&g->hp, dv);
      return g; } } }
 return g; }

////
/// " p0 -- the bootstrap reader "  (doc/io.md rung 5)
//
// the PURE LISP SUBSET and nothing else: delimiters, comments, strings, atoms,
// ' quote -- the sigil surface is p1's, and p1.l + egg.l are held to this subset
// so p0 can read them. control flow on the C stack, VALUES on g->sp: datums pile on the
// l stack and fold at the close, so no love value sits in a C local across an
// allocation. ⚠ a reader of a SUBSET, not a validator -- enforcement is the
// differential (test/host/rdiff.l). ⚠ it must read what lcat PRINTS (minified
// against `sound`'s grammar). ⚠ nesting rides the C stack, so p0 is depth-bounded
// (~100k on the host; the tree's deepest form is 38; rdiff.l pins 20000).
static struct ai *p0read1(struct ai *g, uintptr_t d);

// a list: read datums until `)`, then fold n of them off the stack. the tail is
// ZeroPoint, NOT zero -- reader lists are ()-terminated (the zero-ontology), and
// zero is the fixnum 0, which the printer shows the same way.
static struct ai *p0reads(struct ai *g, uintptr_t d) {
 uintptr_t n = 0;
 for (int c; ai_ok(g); n++) {
  if ((c = p0skip(g, d)) == ')') { p0pop(g, d); break; }
  if (c == EOF) return encode(ai_core_of(g), ai_status_more);   // unclosed list
  g = p0read1(g, d); }
 if (!ai_ok(g)) return g;
 for (g = ai_push(g, 1, ZeroPoint); ai_ok(g) && n--; g = gxr(g));
 return g; }                                            // () folds zero times -> ZeroPoint

static struct ai *p0read1(struct ai *g, uintptr_t d) {
 int c = p0skip(g, d);
 p0pop(g, d);
 switch (c) {
  case '(': return p0reads(g, d);
  case ')': case EOF: return encode(ai_core_of(g), ai_status_eof);  // stray ) / no datum
  case '"': return ioread1str(g, d);
  case '\'':                                            // quote: 'x = (\ x)
   g = p0read1(g, d);
   if (ai_code_of(g) == ai_status_eof)                  // quote with no operand
    g = encode(ai_core_of(g), ai_status_more);
   if (!ai_ok(g)) return g;
   g = gxr(ai_push(g, 1, ZeroPoint));                   // (d . ())
   if (ai_ok(g)) g = intern(ai_strof(g, "\\"));
   return gxl(g);                                       // (\ . (d))
  case '\\': return intern(ai_strof(g, "\\"));          // lambda/quote: NEVER fuses (form space)
  default: return ioread1sym(g, d, c); } }              // name / number

// (sound0 text): sound's bootstrap twin over p0's grammar, for the differential.
// THE TEXT SLOT IS THE CURSOR: sp[0] comes in as the charlist and goes out as the
// answer; what is left in between IS the residue. ⚠ the body stays in an
// ai_noinline helper: a frame in the lvm_ would force the tail Continue() into a
// ret (make vmret).
ai_noinline static struct ai *p0text(struct ai *g) {
 uintptr_t const d = topof(g) - g->sp;                // the cursor's depth, and the rollback point
 g = p0read1(g, d);
 if (ai_ok(g)) return gxl(g);                         // (datum . residue), over the text slot
 enum ai_status const st = ai_code_of(g);             // no datum: which nothing?
 if (st != ai_status_eof && st != ai_status_more) return g;   // a real failure (oom) propagates
 // ⚠ the rollback is not optional: a torn parse leaves p0reads's pile behind and
 // the text slot is no longer sp[0] -- drop back to the entry depth
 g = ai_core_of(g), g->sp = topof(g) - d;
 if (st == ai_status_eof) return g->sp[0] = ZeroPoint, g;     // a clean end, over the text slot
 if (!ai_ok(g = intern(ai_strof(g, "torn")))) return g;
 return g->sp[1] = g->sp[0], g->sp++, g; }

lvm(lvm_sound0) {
 Pack(g);
 if (!ai_ok(g = p0text(g))) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g); ai_musttail return Next(1); }

////
/// " the boot stitch "  (doc/io.md rung 6b)
//
// the egg's corpus is STITCHED: p0 reads the halves it owns (p1.l, prel.l,
// egg.l) and p1, the reader in love, reads ev.l -- the egg expression never
// learns. the circularity resolves by reading p1.l TWICE: once evaluated on the
// spot so p1 is callable, once into the corpus so it recompiles like everything
// else. ⚠ AT THE HEAD, never the tail: sit answers the LAST form's value, which
// is what gets pinned as ev.

// the boot's text is a C string, so cons it: one Have for the whole run, then a
// backward walk that needs no root. the peak is one text at a time (~424KB transient).
static struct ai *p0chars(struct ai *g, char const *s) {
 uintptr_t n = 0;
 while (s[n]) n++;
 g = ai_push(g, 1, ZeroPoint);                                     // the cursor's slot first,
 if (!ai_ok(g = ai_have(g, n * Width(struct ai_chain)))) return g; // then the whole run at once
 word l = ZeroPoint;
 for (uintptr_t i = n; i--;) {
  struct ai_chain *p = bump(g, Width(struct ai_chain));
  ini_chain(p, putcharm((unsigned char) s[i]), l);
  l = (word) p; }
 return g->sp[0] = l, g; }

// read every top-level datum of a C string with p0 and CONS them, in source
// order, onto the list already on top of the stack. reading the corpus RIGHT TO
// LEFT then stitches its halves with no append and no copy.
static struct ai *p0onto(struct ai *g, char const *s) {
 if (!ai_ok(g = p0chars(g, s))) return g;
 uintptr_t const d = topof(g) - g->sp;               // the cursor, pushed under the datums
 uintptr_t n = 0;
 for (;; n++) {
  g = p0read1(g, d);
  if (ai_ok(g)) continue;
  if (ai_code_of(g) != ai_status_eof) return g;      // more: an unfinished shape
  g = ai_core_of(g);
  break; }
 if (!ai_ok(g = ai_push(g, 1, zero))) return g;       // reserve first, THEN copy the
 g->sp[0] = g->sp[n + 2];                            // tail up: a push can gc and move it
 for (; ai_ok(g) && n--; g = gxr(g));                //   (+2: the datums sit over the cursor)
 return ai_ok(g) ? (g->sp[2] = g->sp[0], g->sp += 2, g) : g; }

// the corpus, read by the reader in love: an ordinary call of hook 0 on the whole text
static struct ai *p1text(struct ai *g, char const *s) {
 g = ai_strof(g, s);
 g = gxr(push0(g));                                  // ("<text>")
 if (!ai_ok(g = ai_push(g, 1, zero))) return g;       // reserve FIRST, then read the slot:
 g->sp[0] = ai_core_of(g)->hot_read;                 //   a push can gc, and the gc is what
 if (!ai_ok(g = ai_eval(gxl(g)))) return g;          //   moves hot_read. (<reader> "<text>")
 // p1 answers `torn` for an unfinished shape; the egg would fold over it as an
 // EMPTY corpus and silently pin ev to 0, so refuse it here (chainp AND NOT nomp)
 word r = g->sp[0];
 return (chainp(r) && !nomp(r)) || r == ZeroPoint ? g
      : encode(ai_core_of(g), ai_status_more); }

// a text -> the list of its forms, pushed: p1 reads it once sealed, p0 until then
// (the sealed slot IS the test)
static struct ai *readtext(struct ai *g, char const *s) {
 if (lamp(ai_core_of(g)->hot_read)) return p1text(g, s);
 return p0onto(push0(g), s); }

static struct ai *qtop(struct ai *g) {                // x on top -> 'x
 return gxl(pushq(gxr(push0(g)))); }                 // (x), then (\ x)

// apply a ONE-FORM driver text (pure lisp, p0-read) to the quoted list on top of
// the stack: (<driver> '(list))
static struct ai *applyq(struct ai *g, char const *driver) {
 if (!ai_ok(g)) return g;                            // ⚠ ai_pop bumps sp unguarded
 g = p0onto(gxr(push0(qtop(g))), driver);            // ('(list)), then (driver '(list))
 return ai_pop(ai_eval(g), 1); }

// the plain eval fold: run a list of forms in order, answer the last one's
// value. `ev` is read late so one text drives both of love0's passes.
static char const evfold[] = "((:(e a b)(? b(e(ev 'ev(cap b))(cup b))a)e)0)";

// every top-level form of a text, evaluated in order -- the frontends' door for
// a boot tail, a CLI driver, a corpus runner.
ai_noinline struct ai *ai_evals_(struct ai *g, char const *s) {
 ai_image_note(0x20);
 g = readtext(g, s);
 ai_image_note(0x22);
 return applyq(g, evfold); }

// the egg takes TWO corpora: `corpus` is sat twice (ev compiles itself), `post`
// once, after the hatch and before the mop -- the seat for love that needs the
// runtime-internal noms (peek/seek) the mop is about to take off the book.
ai_noinline struct ai *ai_egg_(struct ai *g, char const *egg, char const *p1,
                               char const *corpus, char const *post) {
 g = p0onto(ai_push(g, 1, ZeroPoint), p1);           // p1's forms, by p0 ..
 g = applyq(g, evfold);                              // .. and c0 evals them: p1 is live
 g = gxr(push0(qtop(p1text(g, post))));              // ('post), parked under the corpus
 g = p1text(g, corpus);                              // prel + ev, through the reader in love
 g = p0onto(g, p1);                                  // and p1 at the HEAD of the corpus
 g = p0onto(gxl(qtop(g)), egg);                      // (egg 'corpus 'post)
 return ai_pop(ai_eval(g), 1); }

// ============================================================================
// sys
// ============================================================================
op11(lvm_clock, putcharm(ai_clock() - (charmp(Sp[0]) ? getcharm(Sp[0]) : 0)))

// the fine clock: monotonic ns for DIFFERENCES ((nclock t) is ns minus t); clock
// stays at ms, the scheduler's scale (ns wraps 32 bits every 4.3s). weak default
// degrades to ms*1e6; hosts override with a real ns source.
__attribute__((weak)) intptr_t ai_nclock(void) {
 return (intptr_t) (ai_clock() * 1000000u); }
op11(lvm_nclock, putcharm(ai_nclock() - ((Sp[0] & 1) ? getcharm(Sp[0]) : 0)))

// (please x): a collection on demand -- () a minor, a positive charm a major;
// answers the new n_gc (the real-time lever). a forced collection OBSERVES and
// never STEERS: the resize window is zeroed for the call and put back, so a probe
// forcing minors can't talk the nursery into doubling (the pause gauge's first
// draft ran the pool to oom@8GB through exactly that feedback).
lvm(lvm_please) {
 word n = Sp[0];
 Pack(g);
 if ((n & 1) && getcharm(n) > 0)
  g->since_major = g->major_live0 + 4 * (uintptr_t) g->len + 1;
 uintptr_t wa = g->win_alloc, wc = g->win_copied;
 intptr_t ln = g->lean;
 g->win_alloc = g->win_copied = 0, g->lean = 0;
 if (!ai_ok(g = ai_please(g, 0))) ai_musttail return Ap(_lvm_ghelp, g);
 g->win_alloc = wa, g->win_copied = wc, g->lean = ln;
 Unpack(g);
 Sp[0] = putcharm((intptr_t) g->n_gc);
 Ip += 1; ai_musttail return Continue(); }

// (gauge 0) -> a rank-1 Z array of VM stats (full machine words, not 62-bit fixnums):
//   [0] len       pool size (words)
//   [1] heap      words used from base (core + live heap)
//   [2] stack     stack height (words)
//   [3] n_gc      collections so far
//   [4] max_len   peak pool size (words)
//   [5] max_heap  peak live heap after a collection (words)
//   [6] n_seen    Σ heap occupancy entering each collection (scanned = live + dead, words)
//   [7] n_evac    Σ heap survivors copied out each collection (live, words)
//   [8] old       the tenured set: words live in the major pool, else [end, minor)
//   [9] rem_miss  rem-set entries dropped on overflow since the last collection (a miss forces a major; ~always 0)
//  [10] rem_hi    peak remembered-set size (distinct old objects with a young field)
//  [11] n_minor   MINOR collections so far (majors = n_gc - n_minor)
//  [12] major_cap the major pool's reserved footprint: 2*major_len words (both halves), 0 if non-gen
//  [13] n_resize  pool reallocations so far (the pool-cliff tell)
//  [14] minor_hi  peak words one MINOR copied -- the pause gauge (a copying
//  [15] major_hi  peak words one MAJOR copied    collection's pause is its copy volume)
// derive: mortality = (n_seen - n_evac)/n_seen ; copy-amp = n_evac/max_heap
lvm(lvm_gauge) {
 enum { N = 16 };
 uintptr_t const bytes = sizeof(struct ai_tray) + 1 * sizeof(word) + N * ai_T[ai_Z];
 Have(b2w(bytes));
 struct ai_tray *v = (struct ai_tray*) Hp;
 Hp += b2w(bytes);
 ini_tray(v, ai_Z, 1);
 v->shape[0] = N;
 tray_put_int(v, 0, (intptr_t) g->len);
 tray_put_int(v, 1, (intptr_t) (Hp - ptr(g)));
 tray_put_int(v, 2, (intptr_t) (ptr(g) + g->len - Sp));
 tray_put_int(v, 3, (intptr_t) g->n_gc);
 tray_put_int(v, 4, (intptr_t) g->max_len);
 tray_put_int(v, 5, (intptr_t) g->max_heap);
 tray_put_int(v, 6, (intptr_t) g->n_seen);
 tray_put_int(v, 7, (intptr_t) g->n_evac);
 tray_put_int(v, 9, (intptr_t) g->rem_miss);
 tray_put_int(v, 10, (intptr_t) g->rem_hi);
 tray_put_int(v, 11, (intptr_t) g->n_minor);
 tray_put_int(v, 8, (intptr_t) (g->major_pool ? g->major_hp - g->major_base : g->minor - (word*) g->end));  // major live (gen), else [end,minor)
 tray_put_int(v, 12, (intptr_t) (g->major_pool ? 2 * g->major_len : 0));  // major pool capacity (both halves), words
 tray_put_int(v, 13, (intptr_t) g->n_resize);
 tray_put_int(v, 14, (intptr_t) g->minor_hi);
 tray_put_int(v, 15, (intptr_t) g->major_hi);
 ai_musttail return Answer(word(v)); }

// (tune v) -> the four live GC knobs as a rank-1 Z array, in WORDS:
//   [0] budget  total footprint cap (2*minor + 2*major); 0 = unbounded (appel's rule)
//   [1] minor0  the nursery FLOOR every resize clamps up to
//   [2] major0  the major pool's grow/shrink STEP (never 0: it divides)
//   [3] ratio   copy-overhead setpoint -- hold copied/allocated inside [1/(4*ratio), 1/ratio]
// (tune ()) reads; a rank-1 4-array WRITES and answers what it REPLACED, so a probe
// can put the knobs back. seeded at ai_ini from ai_minor0/ai_major0/ai_gc_ratio.
// a knob lands at the NEXT collection -- tightening budget frees nothing until then,
// so pair it with (please 1). a wrong shape is a silent no-op answering the current
// knobs (pin's misuse convention). ⚠ these are untraced scalars ahead of v0, so a bake
// does NOT carry them: a woken image tunes again (host's LOVE_BUDGET_MB does exactly that).
lvm(lvm_tune) {
 enum { N = 4 };
 uintptr_t const bytes = sizeof(struct ai_tray) + 1 * sizeof(word) + N * ai_T[ai_Z];
 Have(b2w(bytes));
 word x = Sp[0];                             // read POST-Have: a collection forwards the operand
 struct ai_tray *v = (struct ai_tray*) Hp;
 Hp += b2w(bytes);
 ini_tray(v, ai_Z, 1);
 v->shape[0] = N;
 tray_put_int(v, 0, (intptr_t) g->budget);
 tray_put_int(v, 1, (intptr_t) g->minor0);
 tray_put_int(v, 2, (intptr_t) g->major0);
 tray_put_int(v, 3, (intptr_t) g->ratio);
 if (galaxyp(x) && tray(x)->rank == 1 && tray(x)->shape[0] == N) {
  struct ai_tray *w = tray(x);
  intptr_t b = tray_get_int(w, 0), mi = tray_get_int(w, 1),
           ma = tray_get_int(w, 2), ra = tray_get_int(w, 3);
  g->budget = b > 0 ? (uintptr_t) b : 0;     // <= 0 is the unbounded spelling, not a refusal
  if (mi > 0) g->minor0 = (uintptr_t) mi;    // a 0 floor would let the nursery vanish
  if (ma > 0) g->major0 = (uintptr_t) ma;    // the step divides
  if (ra > 0) g->ratio = (uintptr_t) ra; }   // 0 would never grow and always shrink
 ai_musttail return Answer(word(v)); }

// (apof x): x's kind pointer (cell[0]) as a fixnum, 0 for a fixnum/immediate. The string-lane glaze
// reads the kind of a reference string at codegen time and emits a `cmp [s], kind; jne deopt` type guard.
lvm(lvm_apof) {
 word x = Sp[0];
 Sp[0] = putcharm(lamp(x) ? (uintptr_t) cell(x)->ap : 0);
 Ip += 1;
 ai_musttail return Continue(); }

// default fd-keyed waits, conservative (all fds always-ready; multi-source wait
// collapses to sleep) so non-multitasking frontends link without impls
__attribute__((weak)) bool ai_ready(int fd, int events) { (void) fd, (void) events; return true; }
__attribute__((weak)) void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ticks) {
  (void) fds; (void) n; ai_sleep(ticks); }
// the default AUTHORITATIVE readiness sweep: ask one at a time but fill every
// slot, so "none ready" never reads as "nobody answered"; hosts replace the loop
// with one poll(2)
__attribute__((weak)) void ai_ready_fds(struct ai_wait_fd *fds, int n) {
  for (int i = 0; i < n; i++)
    fds[i].revents = ai_ready(fds[i].fd, fds[i].events) ? fds[i].events : 0; }

__attribute__((weak)) void ai_fd_close(int fd) { (void) fd; }   // host overrides with close(2)
// default sleep is busy wait
__attribute__((weak)) ai_noinline void ai_sleep(uintptr_t ticks) {
  for (ticks += ai_clock(); ai_clock() < ticks;); }

// (cue? p): would `see` answer WITHOUT PARKING? the dual of the park law -- all
// three terms (pushback, buffered run, fd), or a port with bytes in hand reads
// "not ready". ⚠ it asks WILL YOU ANSWER, not IS THERE DATA: a hung-up fd reads
// ready and the see answers -1. a non-port asks about stdin (the bare (cue? 0)).
lvm(lvm_key) {
 Sp[0] = io_route(g, iop(Sp[0]) ? Sp[0] : (word) &ai_stdin);   // the bare (cue? 0) asks about stdin, so it routes too
 struct ai_io *i = (struct ai_io*) Sp[0];
 Sp[0] = (getcharm(i->ungetc_buf) != EOF || bio_rpending(bio_of(g, i))
          || ai_ready((int) ai_io_fd(i), ai_wait_in)) ? putcharm(1) : zero;
 Ip += 1;
 ai_musttail return Continue(); }

// ============================================================================
// map (lookup-lambda backed by an open-addressed thread; see tabp comment)
// ============================================================================
// backing is internal -- only ever reached from a header[1], never applied as a
// l value; its ap behaves-as-1 like lvm_cask should it ever be (it won't).
static lvm(lvm_map_data) {
 Ip = cell(*++Sp); *Sp = putcharm(1); ai_musttail return Continue(); }

// the backing slot of k, or -- if absent -- the first empty slot on its probe
// chain. load is kept < 3/4 so an empty slot always terminates the sound.
static ai_inline uintptr_t map_probe(struct ai *g, word m, word k, bool *found) {
 uintptr_t mask = map_cap(m) - 1, i = hash(g, k) & mask;
 word *s = map_slots(m);
 for (;; i = (i + 1) & mask) {
  word sk = s[2 * i];
  if (sk == map_gap) return *found = false, i;
  if (eql(g, k, sk)) return *found = true, i; } }

word ai_mapget(struct ai *g, word dflt, word k, word m) {
 bool found; uintptr_t i = map_probe(g, m, k, &found);
 return found ? map_slots(m)[2 * i + 1] : dflt; }

// the layered global read: g->book is a CHAIN of books walked head-first. a
// per-layer miss needs its own sentinel -- a stored () must SHADOW, never fall
// through. the l twin is ev.l's gv; keep them in step.
static word bookget(struct ai *g, word dflt, word k) {
 static union u const miss[1];
 for (word c = g->book; chainp(c); c = B(c)) {
  word v = ai_mapget(g, word(miss), k, A(c));
  if (v != word(miss)) return v; }
 return dflt; }

// the layered macro read: each layer's macro table rides its [zero] slot; miss
// answers 0, the no-macro convention
static word macroget(struct ai *g, word k) {
 static union u const miss[1];
 for (word c = g->book; chainp(c); c = B(c)) {
  word mt = ai_mapget(g, word(miss), zero, A(c));
  if (mt == word(miss)) continue;
  word v = ai_mapget(g, word(miss), k, mt);
  if (v != word(miss)) return v; }
 return 0; }

// fill an empty cap-slot backing at b (cap a power of two); caller reserves it.
static ai_inline union u *map_fill_back(union u *b, uintptr_t cap) {
 b[0].ap = lvm_map_data, b[1].x = putcharm(0), b[2].x = putcharm(cap);
 for (uintptr_t i = 0; i < cap; i++) b[3 + 2 * i].x = map_gap, b[4 + 2 * i].x = zero;
 return tagthread(b, 3 + 2 * cap); }

// double the backing of the map at sp[2], rehash, swap into header[1]; the
// header never moves, so aliased references stay valid
static ai_noinline struct ai *map_grow(struct ai *g) {
 uintptr_t ncap = 2 * map_cap(g->sp[2]);
 if (!ai_ok(g = ai_have(g, 4 + 2 * ncap))) return g;
 word m = g->sp[2];                                 // re-fetch header after GC
 union u *nb = map_fill_back((union u*) g->hp, ncap);
 g->hp += 4 + 2 * ncap;
 word *os = map_slots(m), *ns = &nb[3].x;
 uintptr_t ocap = map_cap(m), nlen = 0, nmask = ncap - 1;
 for (uintptr_t j = 0; j < ocap; j++) {
  word k = os[2 * j];
  if (k == map_gap) continue;
  uintptr_t i = hash(g, k) & nmask;
  while (ns[2 * i] != map_gap) i = (i + 1) & nmask;
  ns[2 * i] = k, ns[2 * i + 1] = os[2 * j + 1], nlen++; }
 nb[1].x = putcharm(nlen);
 cell(m)[1].x = (word) nb;                         // swap backing; header identity stable
 gen_wb(g, m, (word) nb);                          // barrier: old header now points at the fresh (young) backing
 return g; }

// (put k v map): mutate in place; grow (may GC) on a new key past the load
// factor, re-reading k/v from the stack afterwards. Leaves the map at sp[2].
static ai_noinline struct ai *ai_mapput(struct ai *g) {
 if (!ai_ok(g)) return g;
 bool found; uintptr_t i = map_probe(g, g->sp[2], g->sp[0], &found);
 if (found) {
  gen_wb(g, map_back(g->sp[2]), g->sp[1]);         // barrier: a young value into an old backing
  return map_slots(g->sp[2])[2 * i + 1] = g->sp[1], g->sp += 2, g; }
 if ((map_len(g->sp[2]) + 1) * 4 >= map_cap(g->sp[2]) * 3) {
  if (!ai_ok(g = map_grow(g))) return g;
  i = map_probe(g, g->sp[2], g->sp[0], &found); }   // re-probe larger backing
 word *s = map_slots(g->sp[2]);
 s[2 * i] = g->sp[0], s[2 * i + 1] = g->sp[1];
 gen_wb(g, map_back(g->sp[2]), g->sp[0]);          // barrier: a young key ...
 gen_wb(g, map_back(g->sp[2]), g->sp[1]);          // ... or young value into an old backing
 cell(map_back(g->sp[2]))[1].x = putcharm(map_len(g->sp[2]) + 1);
 return g->sp += 2, g; }

// ai_mapdel: delete k, backward-shift the probe chain so no tombstone is
// needed; v is the not-found result. No allocation. Leaves the map at sp[2].
static ai_noinline word ai_mapdel(struct ai *g, word m, word k, word dflt) {
 bool found; uintptr_t i = map_probe(g, m, k, &found);
 if (!found) return dflt;
 word *s = map_slots(m); uintptr_t mask = map_cap(m) - 1;
 for (uintptr_t j = i;;) {
  j = (j + 1) & mask;
  if (s[2 * j] == map_gap) break;
  uintptr_t h = hash(g, s[2 * j]) & mask;            // ideal slot of the probed key
  bool gap = i <= j ? (h <= i || h > j) : (h <= i && h > j);   // h not in (i, j]
  if (gap) {
   s[2 * i] = s[2 * j], s[2 * i + 1] = s[2 * j + 1];
   gen_wb(g, map_back(m), s[2 * i]), gen_wb(g, map_back(m), s[2 * i + 1]);  // delete shifts a (maybe young) k/v within an old backing
   i = j; } }
 s[2 * i] = map_gap, s[2 * i + 1] = zero;
 cell(map_back(m))[1].x = putcharm(map_len(m) - 1);
 return m; }

// C-callable fresh empty map, pushed on sp[0]. Same shape as lvm_tablet.
static struct ai *map_new(struct ai *g) {
 uintptr_t cap = map_min_cap, nb = 4 + 2 * cap;
 if (!ai_ok(g = ai_have(g, nb + 3))) return g;
 union u *b = map_fill_back((union u*) g->hp, cap), *h = (union u*) (g->hp + nb);
 h[0].ap = lvm_map_lookup, h[1].x = (word) b, tagthread(h, 2);
 g->hp += nb + 3;
 return ai_push(g, 1, (word) h); }

// (tablet n): a fresh empty map; n is a SIZE HINT (presized below the 0.75 load
// factor, so inserting n known keys never rehashes). n<=0 keeps the min capacity.
lvm(lvm_tablet) {
 intptr_t raw = charmp(Sp[0]) ? getcharm(Sp[0]) : 0;          // saturate to a bounded green charm first
 uintptr_t hint = raw <= 0 ? 0 : (uintptr_t) raw > map_hint_max ? map_hint_max : (uintptr_t) raw;
 uintptr_t cap = map_min_cap;
 while (cap * 3 <= hint * 4) cap *= 2;                        // grow to hold `hint` below the 0.75 load factor
 uintptr_t nb = 4 + 2 * cap;
 Have(nb + 3);
 union u *b = map_fill_back((union u*) Hp, cap);
 union u *h = (union u*) (Hp + nb);
 h[0].ap = lvm_map_lookup, h[1].x = (word) b, tagthread(h, 2);
 Sp[0] = (word) h;
 Hp += nb + 3; ai_musttail return Next(1); }

// (m k): map application is lookup, () if absent; unwinds like self-quote
static lvm(lvm_map_lookup) {
 word v = ai_mapget(g, ZeroPoint, Sp[0], (word) Ip);   // a map miss answers () (the zero point), not the number 0
 Ip = cell(*++Sp); *Sp = v; ai_musttail return Continue(); }

op11(lvm_tabp, tabp(Sp[0]) ? putcharm(1) : zero)
// (lit? x): the upper segment of the lattice, ai_kind >= KTablet -- tablets and the
// tops above (closures, nifs, cask/port), never the fresh value-data below. a
// COIN's die decides (DieHot truthy = lit): lit? is the lattice cut, not storage.
lvm(lvm_litp) {
 word x = Sp[0];
 bool lit = coinp(x) ? !ai_nilp(g, die_get(g, coin_die(x), DieHot))   // a coin: its die decides
                     : ai_kind(x) >= KTablet;                             // else the lattice cut
 Sp[0] = lit ? putcharm(1) : zero;
 ai_musttail return Next(1); }
// (hot? x): an opaque hot handle -- a cask or a port (a task is a fixnum id, not a handle)
op11(lvm_hotp, (caskp(Sp[0]) || iop(Sp[0])) ? putcharm(1) : zero)

// (hash x) -- the general hashing method exposed to l as a fixnum.
op11(lvm_dig, putcharm(hash(g, Sp[0])))

lvm(lvm_peep) {                                // (peep coll key default): collection-first
 word x = Sp[0], k = Sp[1], z = Sp[2], n;
 if (caskp(x)) {                                 // mutable byte string: byte index
  struct ai_str *s = cask(x)->str;
  if (charmp(k) && (n = getcharm(k)) >= 0 && n < (word) len(s))
   z = putcharm((unsigned char) txt(s)[n]); }
 else if (tabp(x)) z = ai_mapget(g, z, k, x);     // map lookup (not a data sentinel)
 else if (lamp(x) && datp(x)) switch (typ(x)) {
  default: break;                               // a bare mint (DMint) is not indexable
  case DGem:                                    // a rank-0 scalar float: a zero key derefs to itself
  case DSun:                                   // ... same for a sun
  case DTwin:                                   // ... and a complex scalar
   if (zerop(k)) z = x;
   break;
  case DTray: {
   // array index: a fixnum (rank-1) or a row-major shape-list (rank-N);
   // out-of-bounds or wrong rank falls through to the default
   struct ai_tray *v = tray(x);
   intptr_t o = tray_off(v, k); uintptr_t off = (uintptr_t) o; bool ok = o >= 0;
   if (ok && v->type == ai_O) z = tray_get_obj(v, off);   // object: the slot IS the value
   else if (ok && v->type == ai_C) {                       // packed complex -> a (re,im) box
    Have(twin_req); v = tray(Sp[0]);                      // re-read coll (Sp[0]) post-Have
    ai_flo_t *fp = tray_data(v);
    z = mk_twin(&Hp, fp[2*off], fp[2*off+1]); }
   else if (ok) { word _res; Have(box_req); v = tray(Sp[0]);
    if (v->type >= ai_R) emit_gem(_res, tray_get_flo(v, off));
    else emit_int(_res, tray_get_int(v, off));
    z = _res; }
   break; }
  case DString:
   // byte as its unsigned value 0..255 (txt is signed char[]: cast, or a high byte sign-extends)
   if (charmp(k) && (n = getcharm(k)) >= 0 && n < (word) len(x))
    z = putcharm((unsigned char) txt(x)[n]);
   break;
  case DChain:
   if (charmp(k) && (n = getcharm(k)) >= 0) {
    while (n-- && chainp(x = B(x)));
    if (chainp(x)) z = A(x); } }
 ai_musttail return Answerp(2, z); }

// (pin coll key val): a map or a cask has a cell, so the write is in place and the SAME
// collection answers; text, a chain and a tray have none, so a FRESH one carrying the pin
// answers -- the functional update. (peep (pin c k v) k d) = v wherever the pin lands;
// a rank-0 scalar is the one kind peep reads that pin does not write (there is no cell to
// replace, only the value itself). out-of-range/wrong-kind is a silent no-op answering
// coll, the byte ops' misuse convention.
lvm(lvm_pin) {
 word x = Sp[0], n;                              // coll
 if (tabp(x)) {
  Sp[0] = Sp[1], Sp[1] = Sp[2], Sp[2] = x;       // ai_mapput wants (sp0,sp1,sp2)=(key,val,coll)
  Pack(g);
  if (!ai_ok(g = ai_mapput(g))) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g);
  ai_musttail return Next(1); }
 if (caskp(x)) {
  if (charmp(Sp[1]) && charmp(Sp[2]) && (n = getcharm(Sp[1])) >= 0 && n < (word) len(cask(x)->str))
   txt(cask(x)->str)[n] = (char) getcharm(Sp[2]);    // index = key = Sp[1], val = Sp[2]
  ai_musttail return Answerp(2, x); }
 if (lamp(x) && datp(x)) switch (typ(x)) {
  default: break;                                // a mint, a scalar: nothing to pin into
  case DString: {                                // one byte replaced in a fresh text
   if (!charmp(Sp[1]) || !charmp(Sp[2])) break;
   if ((n = getcharm(Sp[1])) < 0 || n >= (word) len(x)) break;
   uintptr_t sz = len(x), req = str_type_width + b2w(sz);
   Have(req);
   struct ai_str *s = ini_str(str(Hp), sz); Hp += req;
   memcpy(s->bytes, txt(Sp[0]), sz);             // re-read coll: the Have may have moved it
   s->bytes[n] = (char) getcharm(Sp[2]);
   ai_musttail return Answerp(2, word(s)); }
  case DChain: {                                 // the prefix copied, the tail SHARED
   if (!charmp(Sp[1]) || (n = getcharm(Sp[1])) < 0 || n >= (word) llen(x)) break;
   Have((uintptr_t) (n + 1) * Width(struct ai_chain));
   struct ai_chain *w = (struct ai_chain*) Hp, *base = w;
   Hp += (uintptr_t) (n + 1) * Width(struct ai_chain);
   word l = Sp[0];                               // re-read coll post-Have
   for (word i = 0; i < n; i++, w++, l = B(l)) ini_chain(w, A(l), word(w + 1));
   ini_chain(w, Sp[2], B(l));                    // the pinned cell, then the old tail
   ai_musttail return Answerp(2, word(base)); }
  case DTray: {                                  // the whole payload copied, one slot stored
   intptr_t o = tray_off(tray(x), Sp[1]);
   if (o < 0) break;
   uintptr_t req = b2w(ai_tray_bytes(tray(x)));
   Have(req);
   struct ai_tray *v = (struct ai_tray*) Hp; Hp += req;
   memcpy(v, tray(Sp[0]), ai_tray_bytes(tray(Sp[0])));   // re-read coll post-Have
   if (!tray_put(v, (uintptr_t) o, Sp[2])) { Hp -= req; break; }   // a non-number into a numeric tray
   ai_musttail return Answerp(2, word(v)); } }
 ai_musttail return Answerp(2, x); }

// (pull coll key default): remove key from a map, answering its value or default
// (symmetry with peep); a non-map coll yields default
lvm(lvm_pull) {
 word coll = Sp[0], v = Sp[2];                   // default
 if (tabp(coll)) {
  v = ai_mapget(g, Sp[2], Sp[1], coll);           // value, or default if absent
  ai_mapdel(g, coll, Sp[1], Sp[2]); }             // remove in place (no-op if absent)
 ai_musttail return Answerp(2, v); }

lvm(lvm_keys) {
 intptr_t list = ZeroPoint;                         // () terminator / empty-map result (zero-ontology)
 if (tabp(Sp[0])) {
  uintptr_t cap = map_cap(Sp[0]), n = map_len(Sp[0]);
  Have(n * Width(struct ai_chain));
  struct ai_chain *chains = (struct ai_chain*) Hp;
  Hp += n * Width(struct ai_chain);
  word *s = map_slots(Sp[0]);                    // re-read after Have (GC may move the map)
  for (uintptr_t i = cap; i;)
   if (s[2 * --i] != map_gap)
    ini_chain(chains, s[2 * i], list), list = (intptr_t) chains, chains++; }
 Sp[0] = list;
 Ip += 1;
 ai_musttail return Continue(); }

static ai_noinline uintptr_t hash_two(struct ai *g, word x) {
 word *base = off_pool(g), *top = base + g->len, *w = base;
 for (uintptr_t h = mix;; x = *--w) {
  while (chainp(x)) {
   if (w == top) __builtin_trap();       // worklist overflow: a cycle
   h = (h ^ mix) * mix;                  // mark a chain node
   *w++ = A(x), x = B(x); }
  h = (h ^ hash(g, x)) * mix;          // x is a leaf: hash won't recur
  if (w == base) return h; } }

// the anchor an out-of-pool ap hashes AGAINST: the OFFSET survives a bake/wake
// where the raw address does not (a bake-time bucket index would miss at wake and
// every nif-keyed table would silently read empty).
static const char hash_base[1] = {0};
struct arib; static uintptr_t shash(struct ai *g, word x, struct arib *env);  // α-invariant source hash
static bool clo_nfhash(struct ai *g, word x, uintptr_t *out);  // partial-app -> capture-substitution normal-form hash (the beta bridge)
uintptr_t hash(struct ai *g, intptr_t x) {
 if (charmp(x)) return rot(x*mix);
 if (!datp(x)) {
   // out-of-pool: offset from hash_base. in-pool: a sourced lambda hashes its
   // \-expr α-invariantly (agreeing with `=`), else by length. all GC-stable.
   if (!in_heap(g, x)) return rot((x - (intptr_t) hash_base) * mix);   // a tenured closure lives in the major pool, still in-heap
   union u *k = cell(x); struct ai_tag *tg = ttag(g, k);
   if (tag_head(tg) < k) return shash(g, k[-1].x, 0);   // no-capture lambda: α-invariant source hash
   uintptr_t nf;                                        // partial-app over a SOURCED base: hash its capture-substitution
   if (clo_nfhash(g, x, &nf)) return nf;                // normal form, so the beta bridge stays hash-consistent (=-equal -> same hash)
   uintptr_t r = mix;                                   // else (continuation / handle / bif-based partial-app): by object length
   for (union u *y = k; y < (union u*) tg; y++) r ^= r * mix;
   return r; }
 switch (typ(x)) {
   case DChain: return hash_two(g, x);
   case DMint: return sym(x)->code;
   case DNom: return nom(x)->dig;                  // the cached SPELLING hash -- a serial would key
                                                   // bucket order to intern history (a reproducible-
                                                   // build leak); same-spelled noms collide, `=` separates
   case DTray: {
    uintptr_t len = ai_tray_bytes(tray(x)), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case DBig: {
    uintptr_t len = ai_big_bytes(big(x)), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case DGem: {                                 // hash the lean box (ap is GC-stable, payload is the value)
    uintptr_t len = gem_req * sizeof(word), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case DSun: {                                // same: hash the lean box bytes
    uintptr_t len = sun_req * sizeof(word), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case DTwin: {                                // same: hash the lean (ap, re, im) box bytes
    uintptr_t len = twin_req * sizeof(word), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case DString: {
    uintptr_t n = len(x), h = mix;
    char const *bs = txt(x);
    while (n--) h ^= (uint8_t) *bs++, h *= mix;
    return h; } }
 __builtin_trap(); }

// ============================================================================
// str
// ============================================================================
struct ai *str0(struct ai *g, uintptr_t len) {
 if (!len) { if (ai_ok(g = ai_have(g, 1))) *--g->sp = EmptyString; return g; } // never alloc empty
 uintptr_t req = str_type_width + b2w(len);
 if (ai_ok(g = ai_have(g, req + 1)))
  *--g->sp = word(ini_str(bump(g, req), len));
 return g; }

struct ai *ai_strof(struct ai *g, char const *cs) {
 uintptr_t len = strlen(cs);
 if (ai_ok(g = str0(g, len))) memcpy(txt(g->sp[0]), cs, len);
 return g; }

op11(lvm_strp, strp(Sp[0]) ? putcharm(1) : zero)
lvm(lvm_snip) {
 if (!strp(Sp[0])) Sp[2] = zero;
 else {
  struct ai_str *s = str(Sp[0]), *t;
  intptr_t i = oddp(Sp[1]) ? getcharm(Sp[1]) : 0,
           j = oddp(Sp[2]) ? getcharm(Sp[2]) : 0;
  i = max(i, 0), i = min(i, (word) len(s));
  j = max(j, i), j = min(j, (word) len(s));
  // An empty range (i == j) answers a STRING, the closest form of nothing for this
  // kind, not the bare floor (fixnum 0) -- and THE empty string, never a fresh one.
  // ⚠ no 0-length string is ever allocated (str0 holds the same line), which is what
  // lets two empties be id?-equal wherever they were built.
  if (j == i) Sp[2] = EmptyString;
  else {
   size_t req = str_type_width + b2w(j - i);
   Have(req);
   s = str(Sp[0]);                               // re-read post-Have (GC may have moved it)
   t = str(Hp);
   Hp += req;
   ini_str(t, j - i);
   memcpy(txt(t), txt(s) + i, j - i);
   Sp[2] = (word) t; } }
 ai_musttail return Nextp(1, 2); }


// applying a cask behaves as 0 (yields 1); byte-identical to lvm_port_io, kept
// distinct by ai_noicf so caskp and iop never collide
lvm(lvm_cask) {
 Ip = cell(*++Sp); *Sp = putcharm(1); ai_musttail return Continue(); }
// (cask n) — a zeroed n-byte mutable cask; (cask charlist) — one holding those
// bytes (the bulk way in). n<=0 -> EmptyString, so NO empty cask object exists.
// two heap objects under one Have, so no GC sees a half-built cask.
lvm(lvm_casknew) {
 bool listp = chainp(Sp[0]);
 intptr_t n = charmp(Sp[0]) ? getcharm(Sp[0]) : listp ? (intptr_t) llen(Sp[0]) : 0;
 if (n <= 0) ai_musttail return Answer(EmptyString);   // no empty cask: it is ""
 uintptr_t sreq = str_type_width + b2w(n),
           breq = Width(struct ai_cask) + Width(struct ai_tag);
 Have(sreq + breq);
 struct ai_str *s = ini_str(str(Hp), n);
 Hp += sreq;
 if (listp) {                                                // the charlist lane, mirroring lvm_string's
  word y = Sp[0];                                            // re-read post-Have, like the cask lane there
  for (uintptr_t i = 0; i < (uintptr_t) n; y = B(y)) txt(s)[i++] = (char) getcharm(A(y)); }
 else memset(txt(s), 0, n);
 union u *k = (union u*) Hp;
 Hp += breq;
 cask(k)->ap = lvm_cask;
 cask(k)->str = s;
 tagthread(k, Width(struct ai_cask));
 ai_musttail return Answer(word(k)); }

// THE W^X CODE ARENA (hosted only): the malloc heap is NX, so `nat` copies
// emitted bytes into a W^X mapping -- mmap RW, write, mprotect R+X, never write
// again. the code address lives outside the GC pool; nat_unmap frees it when the
// native closure dies. the kernel's HHDM is executable, so it needs none of this.
#if __STDC_HOSTED__
#include <sys/mman.h>
#include <unistd.h>
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif
static ai_inline size_t code_maplen(size_t codelen) {   // round the arena up to a page; glibc's sysconf is a cached auxv load, not a syscall
 long q = sysconf(_SC_PAGESIZE); size_t ps = q > 0 ? (size_t) q : 4096, need = sizeof(struct ai_str) + codelen;
 return (need + ps - 1) & ~(ps - 1); }
#endif

// ============================================================================
// CODEGEN BACKEND brick 1 -- the native-install seam (provisional; -> `ev`)
// ============================================================================
// the W^X arena finalizer: recover the ai_str base from the code address and
// munmap (a dead native's header is the out-of-pool code addr, a live one's a forward)
#if __STDC_HOSTED__
static void nat_unmap(void *p) {
 char *code = (char*) ((union u*) p)[0].ap;            // header == the W^X code address
 struct ai_str *base = str(code - sizeof(struct ai_str));   // code == s->bytes
 munmap(base, code_maplen(base->len)); }
#endif

// (nif code interp src arity): emitted bytes -> a TRANSPARENT applicable native
// closure (the lvm ABI: g=rdi Ip=rsi Hp=rdx Sp=rcx). arity 1: a 6-word cell
// entering the native body directly; arity>=2: an 8-word lvm_cur cell (curry to
// saturation). value[-1]=src (=/show-identical to the source), value[1]=interp
// (the deopt fallback, so native is never wrong), lvm_ret at the same offset in
// both, so the emitted body is layout-blind. cell[0] duplicates the code addr:
// run_finalizers' dead/live discriminator. internal: the egg mops it.
lvm(lvm_nif) {
 word codebuf = Sp[0];                        // Sp[0]=code Sp[1]=interp Sp[2]=src Sp[3]=arity
 intptr_t ar = oddp(Sp[3]) ? getcharm(Sp[3]) : 0;
 if (!(strp(codebuf) || caskp(codebuf)) || ar < 1) ai_musttail return Answerp(3, zero);
 uintptr_t n = len(bytes_of(codebuf));
 if (n == 0) ai_musttail return Answerp(3, zero);
#ifdef __wasm__                                // wasm has NO executable code pages: a jump to a data address traps.
 ai_musttail return Answerp(3, zero); //  decline unconditionally -> the interp twin runs (emscripten's mprotect
#endif                                         //  is a no-op returning 0, so the mprotect guard below does NOT catch this).
#if __STDC_HOSTED__
 Have(9 + Width(struct ai_fz));               // 9 covers both cells (6/8 words) + tag + fz
 size_t maplen = code_maplen(n);
 void *base = mmap(0, maplen, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
 if (base == MAP_FAILED) ai_musttail return Answerp(3, zero);
 struct ai_str *s = ini_str(str(base), n);
 memcpy(txt(s), txt(bytes_of(Sp[0])), n);     // reload codebuf: a GC in Have may have moved it
 if (mprotect(base, maplen, PROT_READ | PROT_EXEC))
  { munmap(base, maplen); ai_musttail return Answerp(3, zero); }
#ifndef __wasm__                               // guarded: emscripten's clang has no clear_cache intrinsic (dead here anyway -- wasm early-declined above)
 __builtin___clear_cache(txt(s), txt(s) + n);  // AArch64: the I-cache is NOT coherent with the freshly
#endif                                         // written D-cache -- flush or it runs stale bytes (no-op on x86)
#else
 Have(str_type_width + b2w(n) + 9);           // freestanding: HHDM is RWX, a heap copy runs
 struct ai_str *s = ini_str(str(Hp), n); Hp += str_type_width + b2w(n);
 memcpy(txt(s), txt(bytes_of(Sp[0])), n);
 __builtin___clear_cache(txt(s), txt(s) + n);  // same I-cache flush on the freestanding (RWX) path
#endif
 union u *k = (union u*) Hp;
 if (ar == 1) {                               // 6-word direct-entry cell (the old nat)
  Hp += 7;
  k[0].ap = (lvm_t*) txt(s);                  // header (== code, out-of-pool): finalizer dead-detect
  k[1].x  = Sp[2];                            // src   (value[-1], for =/show)
  k[2].ap = (lvm_t*) txt(s);                  // code  (value[0]): the emitted body, the entry
  k[3].x  = Sp[1];                            // interp(value[1]): deopt fallback
  k[4].ap = lvm_ret;                          // value[2]: fast-path return
  k[5].x  = putcharm(0);                      // ret n=1
  tagthread(k, 6);
 } else {                                     // 8-word lvm_cur cell (the old natn)
  Hp += 9;
  k[0].ap = (lvm_t*) txt(s);                  // header (out-of-pool): finalizer dead-detect
  k[1].x  = Sp[2];                            // src (value[-1])
  k[2].ap = lvm_cur;                          // value[0]: curry to saturation
  k[3].x  = putcharm(ar);
  k[4].ap = (lvm_t*) txt(s);                  // native body (lvm_cur resume Ip+2)
  k[5].x  = Sp[1];                            // interp: deopt fallback
  k[6].ap = lvm_ret;
  k[7].x  = putcharm(ar - 1);                 // ret pops n=arity
  tagthread(k, 8);
 }
#if __STDC_HOSTED__
 struct ai_fz *z = (struct ai_fz*) Hp; Hp += Width(struct ai_fz);
 z->p = k, z->fn = nat_unmap, z->next = g->fz, g->fz = z;
#endif
 ai_musttail return Answerp(3, word(k + 2)); }

lvm(lvm_nifx) {
 word codebuf = Sp[0];                        // Sp[0]=code Sp[1]=interp Sp[2]=src Sp[3]=arity Sp[4]=extras
 intptr_t ar = oddp(Sp[3]) ? getcharm(Sp[3]) : 0;
 if (!(strp(codebuf) || caskp(codebuf)) || ar < 1) ai_musttail return Answerp(4, zero);
 uintptr_t n = len(bytes_of(codebuf));
 if (n == 0) ai_musttail return Answerp(4, zero);
#ifdef __wasm__                                // wasm has NO executable code pages: a jump to a data address traps.
 ai_musttail return Answerp(4, zero); //  decline unconditionally -> the interp twin runs (emscripten's mprotect
#endif                                         //  is a no-op returning 0, so the mprotect guard below does NOT catch this).
#if __STDC_HOSTED__
 Have(11 + Width(struct ai_fz));              // 11 covers both cells (7/9 words) + tag + fz
 size_t maplen = code_maplen(n);
 void *base = mmap(0, maplen, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
 if (base == MAP_FAILED) ai_musttail return Answerp(4, zero);
 struct ai_str *s = ini_str(str(base), n);
 memcpy(txt(s), txt(bytes_of(Sp[0])), n);     // reload codebuf: a GC in Have may have moved it
 if (mprotect(base, maplen, PROT_READ | PROT_EXEC))
  { munmap(base, maplen); ai_musttail return Answerp(4, zero); }
#ifndef __wasm__                               // guarded: emscripten's clang has no clear_cache intrinsic (dead here anyway -- wasm early-declined above)
 __builtin___clear_cache(txt(s), txt(s) + n);  // AArch64: the I-cache is NOT coherent with the freshly
#endif                                         // written D-cache -- flush or it runs stale bytes (no-op on x86)
#else
 Have(str_type_width + b2w(n) + 11);          // freestanding: HHDM is RWX, a heap copy runs
 struct ai_str *s = ini_str(str(Hp), n);
 Hp += str_type_width + b2w(n);
 memcpy(txt(s), txt(bytes_of(Sp[0])), n);
 __builtin___clear_cache(txt(s), txt(s) + n);  // same I-cache flush on the freestanding (RWX) path
#endif
 union u *k = (union u*) Hp;
 if (ar == 1) {                               // 7-word direct-entry cell: nif + the extras slot
  Hp += 8;
  k[0].ap = (lvm_t*) txt(s);                  // header (== code, out-of-pool): finalizer dead-detect
  k[1].x  = Sp[2];                            // src   (value[-1], for =/show)
  k[2].ap = (lvm_t*) txt(s);                  // code  (value[0]): the emitted body, the entry
  k[3].x  = Sp[1];                            // interp(value[1]): deopt fallback
  k[4].ap = lvm_ret;                          // value[2]: fast-path return
  k[5].x  = putcharm(0);                      // ret n=1
  k[6].x  = Sp[4];                            // extras (value[3]+8 = Ip+32): GC-walked, image-encoded
  tagthread(k, 7);
 } else {                                     // 9-word lvm_cur cell: natn + the extras slot
  Hp += 10;
  k[0].ap = (lvm_t*) txt(s);                  // header (out-of-pool): finalizer dead-detect
  k[1].x  = Sp[2];                            // src (value[-1])
  k[2].ap = lvm_cur;                          // value[0]: curry to saturation
  k[3].x  = putcharm(ar);
  k[4].ap = (lvm_t*) txt(s);                  // native body (lvm_cur resume Ip+2)
  k[5].x  = Sp[1];                            // interp: deopt fallback
  k[6].ap = lvm_ret;
  k[7].x  = putcharm(ar - 1);                 // ret pops n=arity
  k[8].x  = Sp[4];                            // extras at Ip+32 from the body entry (same offset as arity-1)
  tagthread(k, 9);
 }
#if __STDC_HOSTED__
 struct ai_fz *z = (struct ai_fz*) Hp; Hp += Width(struct ai_fz);
 z->p = k, z->fn = nat_unmap, z->next = g->fz, g->fz = z;
#endif
 ai_musttail return Answerp(4, word(k + 2)); }


// (pour dst doff src soff n): copy n bytes of string-or-cask src into cask dst,
// clamped to both backings (an out-of-range ask copies less, never tramples); answers dst
lvm(lvm_bcopy) {
 word dst = Sp[0], src = Sp[2];
 if (caskp(dst) && (strp(src) || caskp(src))) {
  struct ai_str *d = cask(dst)->str, *s = bytes_of(src);
  intptr_t doff = getcharm(Sp[1]), soff = getcharm(Sp[3]), n = getcharm(Sp[4]),
           dl = len(d), sl = len(s);
  if (n < 0) n = 0;
  if (doff < 0) doff = 0;
  if (soff < 0) soff = 0;
  if (doff + n > dl) n = dl - doff;
  if (soff + n > sl) n = sl - soff;
  if (n > 0) memmove(txt(d) + doff, txt(s) + soff, n); }
 ai_musttail return Answerp(4, dst); }

// public predicate for frontends that need to check string args
bool ai_strp(ai_word x) { return strp(x); }

// ============================================================================
// the heap-image snapshot (doc/snapshot.md): serialize the compacted live heap
// with every pointer-bearing word range-encoded in place, so a fresh process
// reconstructs by re-walking. the core owns the BUFFER codec; the host wraps file io.
// ============================================================================
// lvm_* that appear as an object's ap but are NOT in def1[]
static lvm_t *const image_extra_aps[] = {
 lvm_chain, lvm_tray, lvm_sym, lvm_nom, lvm_str, lvm_big, lvm_gembox, lvm_sunbox, lvm_twinbox,  // data sentinels
 lvm_map_lookup, lvm_map_data, lvm_cask, lvm_coin, lvm_port_io,                       // thread aps
 lvm_cur, lvm_help, lvm_ret0, lvm_ap, lvm_ret,                                         // dispatchers
 // instruction fns a compiled thread embeds directly (no def1 cell); odd on
 // thumb, so they would otherwise escape as "fixnums" -- raw baker addresses
 lvm_callk, lvm_kcall, lvm_jump, lvm_scare, lvm_unc,
 lvm_fputbn, lvm_yield_sw, lvm_yield_nif, lvm_task_exit,
 _lvm_yieldk };   // the yield continuation: c0'd, so a task parked mid-yield carries it
// size (words) of a data object, the same per-kind logic as the GC. d carries the
// kind, s the raw length words -- two homes only during a fused image load, where
// the decoded ap lands in the pool while the payload still sits in the source blob.
static uintptr_t image_datasize(union u *d, void const *s) {
 switch (ai_typ(d)) {
  case DChain: return Width(struct ai_chain);
  case DMint:  return Width(struct ai_mint);
  case DNom:   return Width(struct ai_nom);
  case DGem:   return Width(struct ai_gem);
  case DSun:  return Width(struct ai_sun);
  case DTwin:  return Width(struct ai_twin);
  case DString:return b2w(sizeof(struct ai_str) + ((struct ai_str const*) s)->len);
  case DBig:   return b2w(ai_big_bytes((struct ai_big*)(word) s));
  case DTray:  return b2w(ai_tray_bytes((struct ai_tray*)(word) s)); }
 return 0; }                                                     // unreachable: ai_typ covers the 9
static uintptr_t image_objsize(struct ai *g, union u *p) {
 if (in_data(p->ap)) return image_datasize(p, p);
 word *term = (word*) ttag(g, p);                                // thread: scan to terminator (production)
 return (uintptr_t)(term - (word*) p) + 1; }
// the HOST nif slice: [__start_ai_nifs, __stop_ai_nifs) is a link-order table whose length
// is a RUNTIME quantity, and the token layout wants a compile-time one -- so the index space
// reserves a fixed slice and only the occupied prefix is ever spelled. A host nif's value is
// the bare fn (AiNif stores it raw), so without this lane every app nif rode as an absolute.
// ⚠ the slice is INDEXED BY POSITION, so this table's order is part of the image's contract.
// Nothing checks it by name: the ANCHOR does the whole job, since a binary whose nif set
// differs is a different binary and its symbol gap says so.
#define ImageNHost 256u
static ai_inline uintptr_t image_nhost(void) {
 uintptr_t n = (uintptr_t)(__stop_ai_nifs - __start_ai_nifs);
 return n < ImageNHost ? n : ImageNHost; }
// bidirectional lvm_* table: index <-> address. supplemental table 0..E-1, def1 E.., then
// the host slice last so existing indices keep their meaning.
static intptr_t image_ap_index(intptr_t ap) {
 for (uintptr_t i = 0; i < countof(image_extra_aps); i++)
  if ((intptr_t) image_extra_aps[i] == ap) return (intptr_t) i;
 for (uintptr_t j = 0; j < countof(def1); j++)
  if (def1[j].x == ap) return (intptr_t)(countof(image_extra_aps) + j);
 for (uintptr_t k = 0, n = image_nhost(); k < n; k++)
  if (__start_ai_nifs[k].x == ap)
   return (intptr_t)(countof(image_extra_aps) + countof(def1) + k);
 return -1; }
static ai_inline intptr_t image_ap_resolve(intptr_t idx) {
 uintptr_t e = countof(image_extra_aps), d = countof(def1);
 if (idx < (intptr_t) e) return (intptr_t) image_extra_aps[idx];
 if (idx < (intptr_t)(e + d)) return def1[idx - e].x;
 { uintptr_t k = (uintptr_t) idx - e - d;                    // the host slice; a short roster reads 0
   return k < image_nhost() ? __start_ai_nifs[k].x : 0; } }
// the BARE-FN lane: a compiled thread embeds a nif's fn directly; it is reachable
// symbolically as the code slot of its def1 cell (cell[0], or cell[2] under lvm_cur)
static intptr_t image_fn_slot(word const *cell) {
 return (intptr_t) (cell[0] == (word) lvm_cur ? cell[2] : cell[0]); }
static intptr_t image_fn_index(intptr_t v) {
 for (uintptr_t j = 0; j < countof(def1); j++) {
  word const *c = (word const*) def1[j].x;
  if (image_fn_slot(c) == v) return (intptr_t) j; }
 for (uintptr_t k = 0, n = image_nhost(); k < n; k++) {
  word const *c = (word const*) __start_ai_nifs[k].x;
  if (image_fn_slot(c) == v) return (intptr_t)(countof(def1) + k); }
 return -1; }
static intptr_t image_fn_resolve(intptr_t j) {
 uintptr_t d = countof(def1);
 if (j < (intptr_t) d) return image_fn_slot((word const*) def1[j].x);
 { uintptr_t k = (uintptr_t) j - d;                          // the host slice; a short roster reads 0
   return k < image_nhost() ? image_fn_slot((word const*) __start_ai_nifs[k].x) : 0; } }
// the out-of-pool IMMORTALS: (), "", the std ports, NULL (a mid-eval dump meets it
// in an undressed rbuf/wbuf), map_gap appended LAST so existing indices stay stable
// ⚠ EVERY PORT VTABLE BELONGS HERE: a port's head carries its vt, so an imaged
// port holds a binary address that only an index survives the trip.
static const word image_immortals[] = { ZeroPoint, EmptyString, (word) &ai_stdin, (word) &ai_stdout, (word) &ai_stderr, 0, map_gap,
 (word) &ai_fd_port_vt, (word) &ai_ti_vt, (word) &ai_to_vt, (word) &ai_closed_vt, (word) &ai_ci_vt,
 (word) yield_c };   // g->ip's parked value: a root holds this binary address, so only an index survives
static intptr_t image_imm_index(word v) {
 for (uintptr_t i = 0; i < countof(image_immortals); i++) if (image_immortals[i] == v) return (intptr_t) i;
 return -1; }
// ai_image_save / ai_image_load, the BUFFER codec: save compacts g and serializes
// {header, dictionary, token stream}; load validates, expands, decodes in place.
// a mismatched buffer -> NULL, so the caller boots normally -- never wrong.
#define ImageMagic 0x34304f4e53494119ULL   /* bump if the wire format changes ("..04": the header carries its encode base) */
#if defined(__x86_64__)
#define ImageArch 1
#elif defined(__aarch64__)
#define ImageArch 2
#elif defined(__riscv)
#define ImageArch 3
#else
#define ImageArch 0
#endif
// the image is binary-SPECIFIC: its indices and kept absolutes mean anything only
// in the binary that dumped it. two guards reject a mismatch -> NULL -> normal
// boot: `arch`, and `anchor` -- the GAP between two of the binary's own symbols,
// which a cross-arch or stale build lays out differently.
struct image_hdr {
 uint64_t magic, wordsize, nwords, arch, anchor, nroot, rsv1, nstream, next_serial;
 uint64_t root_tag[24], root_val[24];        /* symbols, tasks, then the entire v0..end region walked
                                                GENERICALLY -- a new v0 field rides with no codec change */
};
// the ABSOLUTE-POINTER GUARD: a kept absolute is only wakeable inside the
// binary's own load segments -- a W^X pointer dies with the bake process. the
// host hands the bounds check in (NULL = audit off); a bad absolute fails the dump.
// the walk's whole state, threaded: a dump owns no globals, so it is re-entrant and
// nothing survives it. base/hp are the compacted live half, hb the blob's bytes.
struct img_ctx {
 word *base, *hp;
 uintptr_t nabs;                  // kept absolutes -- 0 means the image needs no base delta
 uintptr_t cur_off, cur_ap;       // the object being encoded: offset + its hot -- what the guard is told
 int fail, suppress;              // fail is STICKY (any refusal ends the dump); suppress: inside a reverted husk
 struct ai_image_guard const *guard; };
// a native cell cannot wake, but it carries its bytecode twin (interp), so the
// DUMP reverts it: references encode as interp, the husk rides as inert ballast.
// wx-bad WITHOUT the nif signature stays a refusal: better no bake than a storm.
static int img_wxp(struct img_ctx *x, word v) {         // an un-wakeable absolute? (every legit lane excluded FIRST:
 if (!v || oddp(v)) return 0;                           //  a heap pointer is outside the binary's segments too)
 if ((word*) v >= x->base && (word*) v < x->hp) return 0;  // in-pool: a value, not code
 if (image_ap_index((intptr_t) v) >= 0) return 0;       // lvm table
 if (image_imm_index(v) >= 0) return 0;                 // immortal
 return x->guard && !x->guard->ok(x->guard->ctx, (uintptr_t) v, x->cur_off, x->cur_ap); }
static word img_nif_interp(struct img_ctx *x, word v) {   // v -> a cell VALUE; its bytecode twin | 0
 word *base = x->base, *hp = x->hp;
 word *c = (word*) v; word e = 0;
 // match every fixed word AND c[-2]: [code, interp, lvm_ret, n] is ALSO what
 // value+2 of an arity>=2 cell reads as (a partial-app's terminal unc LINK points
 // there, fn_base = link-2), and redirecting a LINK to the twin's VALUE shears the
 // -2 contract -- apply enters two words early, re-currying forever.
 if (c + 4 <= hp && c - 2 >= base && c[-2] == c[0] && img_wxp(x, c[0])
     && c[2] == (word) lvm_ret && oddp(c[3])) e = c[1];
 else if (c + 6 <= hp && c[0] == (word) lvm_cur && oddp(c[1])
     && img_wxp(x, c[2]) && c[4] == (word) lvm_ret && oddp(c[5])) e = c[3];
 else if (c + 4 <= hp && c - 2 >= base && c[-2] == (word) lvm_cur && oddp(c[-1])
     && img_wxp(x, c[0]) && c[2] == (word) lvm_ret && oddp(c[3])
     && c[1]) e = c[1] + 2 * sizeof(word);            // a LINK (value+2): twin's value+2, contract kept
 return e; }
// encode a live value (post-compaction) -> portable (tag,payload):
//  0 FIX raw | 1 PTR word-offset into the blob | 2 LVM table index | 3 IMM immortal index
static void image_root_enc(struct img_ctx *x, word v, uint64_t *tag, uint64_t *val) {
 intptr_t li = image_ap_index((intptr_t) v); if (li >= 0) { *tag = 2, *val = (uint64_t) li; return; }  // ap table FIRST: thumb aps are ODD (see img_encode)
 if (oddp(v)) { *tag = 0, *val = (uint64_t) v; return; }
 if ((word*) v >= x->base && (word*) v < x->hp) {
  word e = img_nif_interp(x, v);                 // a root holding a dead-native cell: its twin rides instead
  if (e) v = e;
  *tag = 1, *val = (uint64_t)((word*) v - x->base); return; }
 intptr_t ii = image_imm_index(v); if (ii >= 0) { *tag = 3, *val = (uint64_t) ii; return; }
 *tag = 0, *val = (uint64_t) v; }            // out-of-pool non-immortal root (unexpected): keep absolute
static word image_root_dec(uint64_t tag, uint64_t val, word *base) {
 return tag == 1 ? (word)(base + val) : tag == 2 ? (word) image_ap_resolve((intptr_t) val)
      : tag == 3 ? image_immortals[val] : (word) val; }
// the SELF-DESCRIBING blob (no reloc tables; the load re-derives by re-walking):
//   heap pointer  -> its BYTE offset       [0, IdxBase)
//   lvm_* ap      -> IdxBase + 2*index     [IdxBase, IdxBase+2*NLVM)
//   immortal      -> IdxBase + 2*NLVM+2*ii [.., TBOUND)
//   binary ptr    -> kept ABSOLUTE (>= TBOUND), base-delta-shifted on load
// the lanes start at a constant, not at the blob's own length, so the encoding is a pure
// function of the heap and one blob can begin with another (doc/plan/image-chain.md).
// a floor is the only way to get that: a string's payload rides raw and can be any even
// value, so no rule downstream of the encoder can tell a lane from a byte.
// fixnums (odd) pass through; every encoded pointer is EVEN (indices doubled), so
// parity discriminates. a binary pointer below TBOUND would alias -> dump refuses.
#define ImageNLvm ((uintptr_t)(countof(image_extra_aps) + countof(def1) + ImageNHost))
#define ImageNImm ((uintptr_t) countof(image_immortals))
#define ImageCellW 16u   /* max nif-cell span (words) an interior link can sit in */
// the BARE-FN lane's width: one slot per nif CELL whose code slot a thread can embed --
// def1's, then the host slice's (AiNif registers a cell too: host/main.c's nif_exit[]).
#define ImageNFn ((uintptr_t)(countof(def1) + ImageNHost))
// the lane floor: above any heap this codec encodes (1 TB on 64-bit, 128 MB on 32-bit;
// a dump past it is refused rather than aliased) and below the absolute lane.
#define ImageIdxBase ((uintptr_t) 1 << (sizeof(uintptr_t) == 8 ? 40 : 27))
// TBOUND: the top of the index region. Every rung above encodes below it, so anything
// at or over it is a binary pointer -- which is why one spelling, not three.
#define ImageTBound (ImageIdxBase + 2 * (ImageNLvm + ImageNImm) \
                                  + 2 * ImageNLvm * ImageCellW + 2 * ImageNFn)
// ⚠ A KEPT ABSOLUTE IS STORED RELATIVE TO THE ANCHOR, and that is what makes a bake
// REPRODUCIBLE. It used to ride as the raw address and get +delta'd on load: correct
// either way, but the stored bytes then moved with ASLR, so two bakes of one tree
// differed in every one of them (7050 words of a host image, 22501 of the artifact's)
// and no release could be checked by its hash. The offset from image_immortals is the
// same number on every run. The BIAS re-centres it: the offset is signed (rodata sits
// either side of the anchor) and the encoding is unsigned and must land above TBOUND,
// so half a bias of headroom each way, and a pointer farther than that refuses the
// dump rather than aliasing an index. Parity still discriminates: TBOUND, the bias and
// the offset are all even, so an encoded absolute is never mistaken for a fixnum.
#define ImageAbsBias ((uintptr_t) 1 << (sizeof(uintptr_t) == 8 ? 40 : 26))
static intptr_t img_encode(struct img_ctx *x, intptr_t v) {
 uintptr_t const hb = ImageIdxBase;
 // the ap table FIRST, before parity: on thumb every fn address is ODD and would
 // ride raw as a "fixnum", valid only at the baker's base (the qemu-twins trap
 // that walled the teensy wake). a colliding fixnum: ~300 values out of 2^31.
 intptr_t idx = image_ap_index(v);
 if (idx >= 0) return (intptr_t)(hb + 2 * (uintptr_t) idx);                      // lvm_* ap (odd on thumb, even on x64 -- both land here)
 if (oddp(v)) {
  intptr_t fj = image_fn_index(v);                                               // a bare nif fn embedded as an instruction word
  if (fj >= 0) return (intptr_t)(hb + 2 * (ImageNLvm + ImageNImm)
                                    + 2 * ImageNLvm * ImageCellW
                                    + 2 * (uintptr_t) fj);
  return v; }                                                                    // fixnum
 if (v >= (intptr_t) x->base && v < (intptr_t) x->hp) {                          // in-pool heap pointer -> byte offset
  word e = img_nif_interp(x, (word) v);                                          //   ... unless it aims at a dead-native cell:
  if (e) return img_encode(x, (intptr_t) e);                                     //   redirect to its bytecode twin (interp)
  return v - (intptr_t) x->base; }
 intptr_t ii = image_imm_index((word) v);
 if (ii >= 0) return (intptr_t)(hb + 2 * ImageNLvm + 2 * (uintptr_t) ii);       // out-of-pool immortal
 // the BARE-FN lane again, for an EVEN-pointer arch. A compiled thread embeds a nif's
 // code slot directly; on thumb the value is ODD and the parity branch above catches it
 // (it must -- it would otherwise ride as a fixnum). x64/arm64 pointers are even, so the
 // same words reached the kept-absolute tail instead and made every image binary-specific.
 { intptr_t fj = image_fn_index(v);
   if (fj >= 0) return (intptr_t)(hb + 2 * (ImageNLvm + ImageNImm)
                                     + 2 * ImageNLvm * ImageCellW
                                     + 2 * (uintptr_t) fj); }
 // an INTERIOR pointer into a def1 nif cell (a baked partial's curry link):
 // encode (cell index, word offset); the owning cell is the GREATEST base <= v
 { intptr_t bj = -1; uintptr_t boff = 0;
   for (uintptr_t j = 0; j < countof(def1); j++) {
    uintptr_t x = (uintptr_t) def1[j].x, d = (uintptr_t) v - x;
    if ((uintptr_t) v > x && d < ImageCellW * sizeof(word) && !(d % sizeof(word))
        && (bj < 0 || x > (uintptr_t) def1[bj].x)) bj = (intptr_t) j, boff = d / sizeof(word); }
   if (bj >= 0) return (intptr_t)(hb + 2 * (ImageNLvm + ImageNImm)
                                     + 2 * (((uintptr_t)(countof(image_extra_aps) + (uintptr_t) bj)) * ImageCellW + boff)); }
 if ((uintptr_t) v < ImageTBound)
  x->fail = 1;                                                                   // a binary ptr in the index range: unencodable
 if (img_wxp(x, (word) v)) {
  if (!x->suppress) x->fail = 1;                                                 // un-wakeable absolute (JIT/W^X/mmap)
  // ⚠ A REVERTED HUSK'S DEAD JIT ADDRESS BAKES AS A CONSTANT. The husk is ballast --
  // every reference was redirected to the interp twin, so nothing reaches this word
  // after a wake -- and it pointed into the glaze's W^X mmap, whose distance from the
  // binary is ASLR-randomized. Written through, it was the LAST thing making a bake
  // unreproducible (1110 words of the artifact, 5.5 GB from the anchor and moving by
  // up to 933 MB a run). It is no less correct than it was: the old lane stored the
  // address and added the BINARY's delta at wake, which never named the JIT page
  // either. Now it is deterministic garbage instead of random garbage.
  else return 1; }                                                               // tagged 0: decodes as a fixnum, executes never
 { uintptr_t r = (uintptr_t) v - (uintptr_t) image_immortals + ImageAbsBias;   // wraps below the anchor; the bias re-centres
   if (r >= 2 * ImageAbsBias) { x->fail = 1; return v; }                       // farther from the anchor than the bias carries
   x->nabs++;                                                                    // kept absolute: the image is now binary-specific
   return (intptr_t)(ImageTBound + r); } }                                  // binary (host nif/.rodata), anchor-relative
// the decode ladder, split hot/cold by the rung-0 census: odd,
// heap offset, lvm index and immortal are 98.7% of decodes; the cold tail keeps
// the nif-cell interior, bare-fn and kept-absolute rungs out of the walk's way.
static ai_noinline intptr_t img_decode_cold(intptr_t v, intptr_t delta) {
 uintptr_t const hb = ImageIdxBase;
 uintptr_t uv = (uintptr_t) v;
 if (uv < hb + 2 * (ImageNLvm + ImageNImm) + 2 * ImageNLvm * ImageCellW) {   // nif-cell interior: base + word offset
  uintptr_t k = (uv - hb - 2 * (ImageNLvm + ImageNImm)) / 2;
  return image_ap_resolve((intptr_t)(k / ImageCellW)) + (k % ImageCellW) * sizeof(word); }
 if (uv < hb + 2 * (ImageNLvm + ImageNImm) + 2 * ImageNLvm * ImageCellW
         + 2 * ImageNFn)                                                        // bare-fn lane: the cell's code slot
  return image_fn_resolve((intptr_t)((uv - hb - 2 * (ImageNLvm + ImageNImm)
                                         - 2 * ImageNLvm * ImageCellW) / 2));
 // the kept absolute, rebuilt against THIS run's anchor -- so the stored bytes never
 // held an address and `delta` has no part in it (the anchor check upstream is the
 // only thing left that reads one).
 (void) delta;
 return (intptr_t)((uintptr_t) image_immortals + uv - ImageTBound - ImageAbsBias); }
static ai_inline intptr_t img_decode(intptr_t v, word *base, intptr_t delta) {
 uintptr_t const hb = ImageIdxBase;
 if (oddp(v)) return v;
 uintptr_t uv = (uintptr_t) v;
 if (uv < hb) return (intptr_t)((char*) base + uv);                              // byte offset -> live pointer
 if (uv < hb + 2 * ImageNLvm) return image_ap_resolve((intptr_t)((uv - hb) / 2));
 if (uv < hb + 2 * (ImageNLvm + ImageNImm)) return (intptr_t) image_immortals[(uv - hb - 2 * ImageNLvm) / 2];
 return img_decode_cold(v, delta); }
// ============================================================================
// the TOKEN STREAM: an encoded word rides as one byte when it is one of the 248
// commonest words in the image, else as an escape naming its own width. Half an
// image is 25 distinct words and the single commonest -- lvm_chain's index, the ap
// every pair wears -- is 23% of it, so the stream lands near a quarter of the blob.
// ⚠ CHOSEN BY COUNT, NOT BY LANE. A fixed token budget per lane is the obvious design, the
// encoding being a lane ladder already -- it is also 3.63x against this 3.82x, and tuned to
// whichever image was measured; counts follow a kernel's or an artifact's wherever those go.
// ⚠ ALWAYS ImageNDict WORDS -- a short image repeats its commonest into the spare seats. That
// costs nothing, spares the wake a bound test per word, and lets the loader read the dictionary
// WHERE IT LIES: the header is a whole number of words and a shebang is padded to one.
#define ImageNDict 248u   /* tokens 0..247 name a dictionary word, 248..255 a 1..8-byte literal */
#define ImageDHash 512u   /* the encoder's value -> token map (open-addressed, 0xff = free) */
// ⚠ the encoder's tables ride the ALLOCATOR, never the frame: together they are kilobytes, and
// an arm32 load has 12 bits of displacement -- port/mps2 refused to compile them onto the stack.
struct img_dic { word dict[ImageNDict], key[ImageDHash]; unsigned char tk[ImageDHash]; };
static uintptr_t img_hash(word v) {
 uintptr_t h = (uintptr_t) v; h ^= h >> 17; h *= 0x9e3779b1u; h ^= h >> 13; return h; }
// one heapsort for the codec's three orders (dictionary words, intern pairs, serial
// ranks): the arrays differ in shape, so lt and stride are the caller's and the heap
// walk is shared. no recursion, no scratch, no worst case.
struct img_ord { int (*lt)(struct img_ord const*, uintptr_t, uintptr_t);
                 word *a; uintptr_t stride; word const *blob; uintptr_t const *nm; };
static void img_ord_swap(struct img_ord const *o, uintptr_t i, uintptr_t j) {
 for (uintptr_t k = 0; k < o->stride; k++) {
  word t = o->a[o->stride * i + k];
  o->a[o->stride * i + k] = o->a[o->stride * j + k], o->a[o->stride * j + k] = t; } }
static void img_ord_sift(struct img_ord const *o, uintptr_t i, uintptr_t n) {
 for (uintptr_t c; (c = 2 * i + 1) < n; i = c) {
  if (c + 1 < n && o->lt(o, c, c + 1)) c++;
  if (!o->lt(o, i, c)) break;
  img_ord_swap(o, i, c); } }
static void img_sort(struct img_ord const *o, uintptr_t n) {
 for (uintptr_t i = n / 2; i-- > 0; ) img_ord_sift(o, i, n);
 for (uintptr_t k = n; k > 1; ) { img_ord_swap(o, 0, --k); img_ord_sift(o, 0, k); } }
static int img_lt_word(struct img_ord const *o, uintptr_t i, uintptr_t j) {
 return o->a[i] < o->a[j]; }
// the commonest words of the blob, most frequent first. ⚠ EXACT, and the tie-break is
// total: two machines baking one tree must choose the SAME 248 words or the images differ
// in every token (test_bakerep). A sorted copy costs a pass and answers exactly; the
// approximate counters that would save it have a tie order, which is the thing to avoid.
static uintptr_t img_dict(word *sorted, uintptr_t nw, word *dict) {
 uintptr_t cnt[ImageNDict], nd = 0;
 struct img_ord o = { img_lt_word, sorted, 1, NULL, NULL };
 img_sort(&o, nw);
 for (uintptr_t i = 0; i < nw; ) {
  uintptr_t j = i; while (j < nw && sorted[j] == sorted[i]) j++;
  uintptr_t n = j - i;
  if (nd < ImageNDict || n > cnt[nd - 1]) {                               // beats the weakest seat
   uintptr_t k = nd < ImageNDict ? nd++ : ImageNDict - 1;
   for (; k && cnt[k - 1] < n; k--) dict[k] = dict[k - 1], cnt[k] = cnt[k - 1];
   dict[k] = sorted[i], cnt[k] = n; }
  i = j; }
 return nd; }
static int img_tok(word const *key, unsigned char const *tk, word v) {
 for (uintptr_t h = img_hash(v) & (ImageDHash - 1); tk[h] != 0xff; h = (h + 1) & (ImageDHash - 1))
  if (key[h] == v) return tk[h];
 return -1; }
// one token per blob word. out == NULL sizes the stream instead of writing it, so the
// buffer is allocated at its true length rather than at a worst case nine times the blob.
static uintptr_t img_stream(unsigned char *out, word const *blob, uintptr_t nw,
                            word const *key, unsigned char const *tk) {
 uintptr_t n = 0;
 for (uintptr_t i = 0; i < nw; i++) {
  int t = img_tok(key, tk, blob[i]);
  if (t >= 0) { if (out) out[n] = (unsigned char) t; n++; continue; }
  uintptr_t uv = (uintptr_t) blob[i], q = uv; unsigned wd = 0;
  do wd++, q >>= 8; while (q);                             // ⚠ unsigned: word is signed, and a
  if (out) { out[n] = (unsigned char)(ImageNDict + wd - 1);   // negative one would shift forever
   for (unsigned k = 0; k < wd; k++) out[n + 1 + k] = (unsigned char)(uv >> (8 * k)); }
  n += 1 + wd; }
 return n; }
// ..and back, into the pool. answers where the stream stopped, or NULL if it ran short --
// a foreign buffer, so the caller boots normally. it does not require the whole stream: a
// derived image is the first nw words of a longer one, and only the caller knows whether
// a leftover tail is a prefix or a corruption.
static unsigned char const *img_expand(word *out, uintptr_t nw, unsigned char const *p,
                                       unsigned char const *end, word const *dict) {
 for (uintptr_t i = 0; i < nw; i++) {
  if (p >= end) return NULL;
  unsigned t = *p++;
  if (t < ImageNDict) { out[i] = dict[t]; continue; }
  unsigned wd = t - ImageNDict + 1;
  if ((uintptr_t)(end - p) < wd) return NULL;
  uintptr_t v = 0;
  if (wd == 3) v = (uintptr_t) p[0] | (uintptr_t) p[1] << 8 | (uintptr_t) p[2] << 16;  // 4 escapes in 5 are a
  else for (unsigned k = 0; k < wd; k++) v |= (uintptr_t) p[k] << (8 * k);             // heap offset, three wide
  p += wd, out[i] = (word) v; }
 return p; }
// the intern map's slot order is its insertion HISTORY: linear probing settles a
// collision by arrival, and each major re-arrives in old slot order, so the layout
// carries when the session's collections fired -- which the GC budget moves. the dump
// re-inserts the live pairs in SPELLING order instead: one layout per key set,
// whatever the session lived through. in place, over the backing the compact just
// bumped, so the session keeps the canonical map too.
static int img_nom_before(word a, word b) {          // spelling order: bytes, then length
 struct ai_str *x = (struct ai_str*) a, *y = (struct ai_str*) b;
 uintptr_t n = x->len < y->len ? x->len : y->len;
 int c = memcmp(x->bytes, y->bytes, n);
 return c < 0 || (c == 0 && x->len < y->len); }
static int img_lt_pair(struct img_ord const *o, uintptr_t i, uintptr_t j) {
 return img_nom_before(o->a[2 * i], o->a[2 * j]); }
static struct ai *img_canon_symbols(struct ai *g) {
 word m = g->symbols;
 if (!m) return g;
 uintptr_t cap = map_cap(m), mask = cap - 1, n = 0;
 word *s = map_slots(m);
 word *pairs = g->alloc(g, NULL, 2 * cap * sizeof(word));
 if (!pairs) return encode(g, ai_status_scare);
 for (uintptr_t j = 0; j < cap; j++)
  if (s[2 * j] != map_gap) pairs[2 * n] = s[2 * j], pairs[2 * n + 1] = s[2 * j + 1], n++;
 { struct img_ord o = { img_lt_pair, pairs, 2, NULL, NULL };
   img_sort(&o, n); }
 for (uintptr_t j = 0; j < cap; j++) s[2 * j] = map_gap, s[2 * j + 1] = zero;
 for (uintptr_t k = 0; k < n; k++) {
  uintptr_t i = hash(g, pairs[2 * k]) & mask;
  while (s[2 * i] != map_gap) i = (i + 1) & mask;
  s[2 * i] = pairs[2 * k], s[2 * i + 1] = pairs[2 * k + 1]; }
 g->alloc(g, pairs, 0);
 return g; }
// canonical serial ORDER: mints keep session order; named noms order by SPELLING
// (ties by session order). session order alone is not canonical -- a weak drop plus a
// re-intern hands a name a fresh serial at a GC-chosen moment -- and `code` is only an
// ORDER key behind the name, so a rank that sorts names by spelling preserves every
// comparison while the bytes stop caring when the session's collections fired.
// nm[serial] is the name string's blob byte offset, 0 for the nameless. a blob string
// wears the ai_str shape (the encode touches only the ap word), so spelling order is
// img_nom_before either side of the encode.
static int img_lt_rank(struct img_ord const *o, uintptr_t i, uintptr_t j) {
 uintptr_t a = (uintptr_t) o->a[i], b = (uintptr_t) o->a[j], na = o->nm[a], nb = o->nm[b];
 if (!na || !nb) return na == nb ? a < b : !na;
 { word x = (word)((char const*) o->blob + na), y = (word)((char const*) o->blob + nb);
   return img_nom_before(x, y) ? 1 : img_nom_before(y, x) ? 0 : a < b; } }
// assign ranks 1..k to the marked serials; answers k, or -1 on OOM. slots are
// (word-offset << 1 | named); a named slot's word -1 is the encoded name.
static uintptr_t img_rank_assign(struct ai *g, word const *blob, uintptr_t const *slots,
                                 uintptr_t nslot, word *rank, uintptr_t nser) {
 uintptr_t *nm = g->alloc(g, NULL, nser * sizeof(uintptr_t));
 uintptr_t *live = g->alloc(g, NULL, nser * sizeof(uintptr_t));
 uintptr_t n = 0, k;
 if (!nm || !live) { g->alloc(g, nm, 0); g->alloc(g, live, 0); return (uintptr_t) -1; }
 memset(nm, 0, nser * sizeof(uintptr_t));
 for (uintptr_t i = 0; i < nslot; i++) {
  uintptr_t v = (uintptr_t) blob[slots[i] >> 1];
  if (v < nser && (slots[i] & 1)) nm[v] = (uintptr_t) blob[(slots[i] >> 1) - 1]; }
 for (uintptr_t i = 1; i < nser; i++) if (rank[i]) live[n++] = i;
 { struct img_ord o = { img_lt_rank, (word*) live, 1, blob, nm };
   img_sort(&o, n); }
 for (k = 0; k < n; k++) rank[live[k]] = k + 1;
 g->alloc(g, nm, 0), g->alloc(g, live, 0);
 return n; }
// compact g and encode its live half into a fresh g->alloc'd blob, filling *Ho; NULL on
// failure. the blob is words, not the wire: img_wire tokenizes it for a file and the
// layered bake diffs two of them. dumps wherever it is called -- a mid-eval dump's
// continuation rides as wake-unreachable ballast -- and the guarded entry keeps the boot
// path honest.
static word *img_build(struct ai *g, struct image_hdr *Ho, struct ai_image_guard const *guard,
                       uintptr_t *outnw) {
 g->image_why = 1;
 if (!g->major_pool) return NULL;                        // needs the major pool (it holds the compacted live half)
 ai_core_of(g)->io = NULL;                               // clear the non-deterministic fd before the bake
 g->image_why = 2;
 if (!ai_ok(gen_major(g))) return NULL;                  // COMPACT: live half -> [major_base, major_hp) (OOM -> no image)
 if (!ai_ok(g = img_canon_symbols(g))) return NULL;      // canonical intern layout (OOM -> no image)
 g->image_why = 3;
 word *base = g->major_base, *hp = g->major_hp;
 uintptr_t nw = (uintptr_t)(hp - base), bytes = nw * sizeof(word);
 // the heap must fit under the lane floor, or a byte offset collides with an index and
 // decodes as an ap.
 g->image_why = 8;
 if (bytes >= ImageIdxBase) return NULL;
 g->image_why = 3;
 word *blob = g->alloc(g, NULL, bytes);                  // the encoded words: scratch, not the file
 if (!blob) return NULL;
 memcpy(blob, base, bytes);
 // canonical serials (blob-side only): the mint stream's live members rename
 // monotone to 1..k below, and the header counter drops to k -- a dead mint (a
 // stray task's pid, a scratch gensym) leaves neither its number nor a +1
 // ripple through every nom minted after it, so one live heap answers one byte
 // string whatever the session's history. `code` is an ORDER key (a name tie
 // compares noms by it), so rank-order assignment preserves every comparison;
 // serial 0 stays the immortal ()'s. the SESSION keeps its own serials -- the
 // rename touches the blob alone, so a mid-eval bake's continuation is
 // unharmed and the woken twin starts canonical. ⚠ a pid charm COPIED into
 // user data is unfindable and keeps its old number across a bake -- a session
 // boundary a pid was never promised to cross.
 uintptr_t nslot = 0, *slots = g->alloc(g, NULL, (nw / 2 + 1) * sizeof(uintptr_t));
 if (!slots) { g->alloc(g, blob, 0); return NULL; }
 struct img_ctx X = { base, hp, 0, 0, 0, 0, 0, guard }, *x = &X;
 for (union u *p = (union u*) base; (word*) p < hp; ) {   // walk the LIVE heap (ttag works on it), encode into blob
  uintptr_t off = (uintptr_t)((word*) p - base);
  // a LIVE finalizer node sits raw in the heap (three words, no header), so no
  // walk can stride it: forge its blob copy into a dead chain of the same width.
  // the fz head lives outside the root window, so a woken session has no finalizables.
  struct ai_fz *z = g->fz;
  while (z && (union u*) z != p) z = z->next;
  if (z) {
   blob[off] = img_encode(x, (intptr_t) lvm_chain);
   blob[off + 1] = blob[off + 2] = img_encode(x, (intptr_t) ZeroPoint);
   p = (union u*) ((word*) p + Width(struct ai_fz));
   continue; }
  uintptr_t sz = image_objsize(g, p);
  x->cur_off = off, x->cur_ap = ((word*) p)[0];
  // a native cell's allocation head: header duplicates the code (arity-1) or fronts an
  // lvm_cur curry cell (arity>=2). references were REDIRECTED to interp, so the husk is
  // wake-unreachable ballast -- suppress the guard across it. (p+2 is the cell VALUE.)
  x->suppress = ((word*) p + 6 <= hp)
    && img_wxp(x, ((word*) p)[0])
    && ((((word*) p)[2] == ((word*) p)[0] && ((word*) p)[4] == (word) lvm_ret)
        || (((word*) p)[2] == (word) lvm_cur && (word*) p + 8 <= hp && ((word*) p)[6] == (word) lvm_ret));
  blob[off] = img_encode(x, ((word*) p)[0]);                                    // word0: the ap
  if (in_data(p->ap)) switch (ai_typ(p)) {
   case DChain: blob[off + 1] = img_encode(x, ((struct ai_chain*) p)->a);
                blob[off + 2] = img_encode(x, ((struct ai_chain*) p)->b); break;
   case DNom:   blob[off + 1] = img_encode(x, (intptr_t) nom(p)->name);
                slots[nslot++] = (off + 2) << 1 | 1; break;   // the serial word, canonicalized below (tagged: named)
   case DMint:  slots[nslot++] = (off + 1) << 1; break;  // mints AND missings (one shape, one ap)
   case DTray:   if (tray(p)->type == ai_O) {
                 word *e = (word*) tray_data(tray(p)); uintptr_t ne = tray_nelem(tray(p)), eo = (uintptr_t)(e - (word*) p);
                 for (uintptr_t i = 0; i < ne; i++) blob[off + eo + i] = img_encode(x, e[i]); }
                break;
   // ⚠ the tail padding is uninitialized heap -- a stale POINTER FRAGMENT, ASLR-varying
   case DString: { uintptr_t n = ((struct ai_str*) p)->len, w = b2w(n);
                   if (w) memset((char*)(blob + off + str_type_width) + n, 0,
                                 w * sizeof(word) - n);
                   break; }
   default: break; }                                     // DMint/DBig/DGem/DSun/DTwin: flat leaves
  else for (uintptr_t i = 1; i < sz; i++) blob[off + i] = img_encode(x, ((word*) p)[i]);   // thread interior + terminator
  x->suppress = 0;
  p = (union u*) ((word*) p + sz); }
 g->image_why = 4;
 if (x->fail) { g->alloc(g, slots, 0); g->alloc(g, blob, 0); return NULL; }   // a binary pointer landed in the index range -> refuse (caller boots normally)
 // the rename: mark live serials (the collected nom/mint slots read RAW off the
 // blob -- scalars rode the memcpy -- plus the pids of both task rings), rank
 // them 1..k in img_rank_assign's canonical order, rewrite in place. rings walk
 // the LIVE post-compaction nodes; their pid word sits at [2] as a charm.
 uintptr_t nser = g->next_serial + 1, kser = 0;
 word *rank = g->alloc(g, NULL, nser * sizeof(word));
 if (!rank) { g->alloc(g, slots, 0); g->alloc(g, blob, 0); return NULL; }
 memset(rank, 0, nser * sizeof(word));
 for (uintptr_t i = 0; i < nslot; i++)
  if ((uintptr_t) blob[slots[i] >> 1] < nser) rank[blob[slots[i] >> 1]] = 1;
 for (union u *n = g->tasks, *st = n; n; n = n->m == st ? NULL : n->m) {
  uintptr_t pid = getcharm(n[2].x);
  if (pid < nser) rank[pid] = 1; }
 if (g->parked)
  for (union u *n = g->parked, *st = n; n; n = n->m == st ? NULL : n->m) {
   uintptr_t pid = getcharm(n[2].x);
   if (pid < nser) rank[pid] = 1; }
 rank[0] = 0;                                            // the immortal ()'s, never drawn, never moved
 kser = img_rank_assign(g, blob, slots, nslot, rank, nser);
 if (kser == (uintptr_t) -1) { g->alloc(g, rank, 0); g->alloc(g, slots, 0); g->alloc(g, blob, 0); return NULL; }
 for (uintptr_t i = 0; i < nslot; i++)
  if ((uintptr_t) blob[slots[i] >> 1] < nser) blob[slots[i] >> 1] = rank[blob[slots[i] >> 1]];
 for (union u *n = g->tasks, *st = n; n; n = n->m == st ? NULL : n->m) {
  uintptr_t off = (uintptr_t)((word*) n - base), pid = getcharm(n[2].x);
  if ((word*) n >= base && (word*) n < hp && pid < nser) blob[off + 2] = putcharm(rank[pid]); }
 if (g->parked)
  for (union u *n = g->parked, *st = n; n; n = n->m == st ? NULL : n->m) {
   uintptr_t off = (uintptr_t)((word*) n - base), pid = getcharm(n[2].x);
   if ((word*) n >= base && (word*) n < hp && pid < nser) blob[off + 2] = putcharm(rank[pid]); }
 g->alloc(g, slots, 0);
 // rsv1 carries the kept-absolute count, ODD-tagged ((n<<1)|1) so a pre-field image
 // (rsv1 == 0) never reads as "zero absolutes" -- those keep the strict anchor check.
 // ⚠ `anchor` is the GAP between the two symbols, not either address. Addresses would
 // write this run's ASLR base into the header, which is the whole of what a
 // reproducible bake must not carry.
 // the counter drops to the live count: the woken twin's first mint lands
 // above every renamed 1..kser, and the bytes carry no dead mints.
 struct image_hdr H = { ImageMagic, sizeof(word), nw, ImageArch, (uint64_t)((word) &ai_image_save - (word) image_immortals), 0, (uint64_t)(x->nabs << 1) | 1u, 0, kser, {0}, {0} };
 g->alloc(g, rank, 0);
 // roots = symbols + tasks (live OUTSIDE v0), then the whole GC-traced v0..end block, GENERICALLY: any
 // field added to struct ai's v0 region is serialized automatically, no codec edit (cf. the GC's v0..end loop).
 uintptr_t nv = (word*) g->end - (word*) &g->v0, nr = 2 + nv;
 g->image_why = 5;
 if (nr > countof(H.root_tag)) { g->alloc(g, blob, 0); return NULL; }    // grew past the header table -> bump root_tag[]
 image_root_enc(x, g->symbols,      &H.root_tag[0], &H.root_val[0]);
 image_root_enc(x, (word) g->tasks, &H.root_tag[1], &H.root_val[1]);
 for (uintptr_t i = 0; i < nv; i++) image_root_enc(x, ((word*) &g->v0)[i], &H.root_tag[2 + i], &H.root_val[2 + i]);
 g->image_why = 6;
 if (x->fail) { g->alloc(g, blob, 0); return NULL; }     // ..a ROOT refused: the walk's own check is behind us
 H.nroot = nr;
 return g->image_why = 0, *Ho = H, *outnw = nw, blob; }
// ..and the wire: {header, dictionary, token stream}, g->alloc'd. fills H.nstream.
static void *img_wire(struct ai *g, struct image_hdr *H, word const *blob, uintptr_t nw, uintptr_t *outlen) {
 uintptr_t bytes = nw * sizeof(word);
 // the dictionary wants a sorted copy and the copy is the blob's size again -- transient,
 // and bake-time, which is the side of this trade nobody waits on.
 struct img_dic *d = g->alloc(g, NULL, sizeof *d);
 if (!d) return NULL;
 word *sorted = g->alloc(g, NULL, bytes);
 if (!sorted) { g->alloc(g, d, 0); return NULL; }
 memcpy(sorted, blob, bytes);
 uintptr_t nd = img_dict(sorted, nw, d->dict);
 g->alloc(g, sorted, 0);
 memset(d->tk, 0xff, sizeof d->tk);
 for (uintptr_t i = 0; i < nd; i++) {
  uintptr_t h = img_hash(d->dict[i]) & (ImageDHash - 1);
  while (d->tk[h] != 0xff) h = (h + 1) & (ImageDHash - 1);
  d->key[h] = d->dict[i], d->tk[h] = (unsigned char) i; }
 for (uintptr_t i = nd; i < ImageNDict; i++) d->dict[i] = nd ? d->dict[0] : 0;   // the spare seats
 uintptr_t ns = img_stream(NULL, blob, nw, d->key, d->tk), db = ImageNDict * sizeof(word);
 H->nstream = ns;
 uintptr_t total = sizeof *H + db + ns;
 char *buf = g->alloc(g, NULL, total);
 if (!buf) { g->alloc(g, d, 0); return NULL; }
 memcpy(buf, H, sizeof *H);
 memcpy(buf + sizeof *H, d->dict, db);
 img_stream((unsigned char*)(buf + sizeof *H + db), blob, nw, d->key, d->tk);
 g->alloc(g, d, 0);
 return *outlen = total, buf; }
void *ai_image_save_(struct ai *g, uintptr_t *outlen, struct ai_image_guard const *guard) {
 struct image_hdr H;
 uintptr_t nw = 0;
 word *blob = img_build(g, &H, guard, &nw);
 if (!blob) return NULL;
 void *buf = img_wire(g, &H, blob, nw, outlen);
 return g->alloc(g, blob, 0), buf; }
void *ai_image_save(struct ai *g, uintptr_t *outlen, struct ai_image_guard const *guard) {
 if ((word*) g->sp != topof(g)) return NULL;             // quiescent: an empty AI stack at the dump point
 return ai_image_save_(g, outlen, guard); }
// ============================================================================
// the layered bake (doc/plan/image-chain.md): one process, images in inclusion order.
// each layer freezes what it dumped, so the next layer's blob begins with this one's and
// the small image stores its parent's prefix plus the words that changed.
// ============================================================================
// the first half: compact, pin, and answer this layer as {header, raw blob}. untokenized,
// since its only reader is ai_image_save_over below. g->alloc'd; the caller owns it.
void *ai_image_freeze(struct ai *g, uintptr_t *outlen, struct ai_image_guard const *guard) {
 struct image_hdr H;
 uintptr_t nw = 0;
 if ((word*) g->sp != topof(g)) return g->image_why = 7, NULL;   // quiescent, like ai_image_save
 word *blob = img_build(g, &H, guard, &nw);              // ..which compacts under the PREVIOUS pin, if any
 if (!blob) return NULL;
 uintptr_t total = sizeof H + nw * sizeof(word);
 char *rec = g->alloc(g, NULL, total);
 if (!rec) return g->alloc(g, blob, 0), NULL;
 memcpy(rec, &H, sizeof H), memcpy(rec + sizeof H, blob, nw * sizeof(word));
 g->alloc(g, blob, 0);
 g->froze = nw;                                          // ..and from here nothing in it moves again
 return *outlen = total, rec; }
// the derived record for one frozen baseline against the blob just built: the baseline's
// own header, then every prefix word changed since the freeze. the prefix itself is never
// stored -- the loader reads it out of the parent's stream.
static void *img_derive(struct ai *g, void const *base, uintptr_t blen,
                        struct image_hdr const *H, word const *blob, uintptr_t nw, uintptr_t *outlen) {
 struct image_hdr B;
 if (blen < sizeof B) return NULL;
 memcpy(&B, base, sizeof B);
 uintptr_t bn = B.nwords;
 if (B.magic != ImageMagic || bn > nw || blen < sizeof B + bn * sizeof(word)) return NULL;
 word const *bb = (word const*)((char const*) base + sizeof B);
 // a plain comparison: the encoding is a pure function of the heap, so a pinned object
 // keeps its offset and every lane is a constant plus an index. nothing to re-seat.
 uintptr_t np = 0;
 for (uintptr_t i = 0; i < bn; i++) if (bb[i] != blob[i]) np++;
 uintptr_t total = sizeof B + (1 + 2 * np) * sizeof(uint64_t);
 char *rec = g->alloc(g, NULL, total);
 if (!rec) return NULL;
 memcpy(rec, &B, sizeof B);
 { struct image_hdr *D = (struct image_hdr*)(void*) rec;
   D->nstream = 0; }                                      // ..and it carries no stream of its own
 uint64_t *w = (uint64_t*)(void*)(rec + sizeof B);
 *w++ = (uint64_t) np;
 for (uintptr_t i = 0; i < bn; i++)
  if (bb[i] != blob[i]) *w++ = (uint64_t) i, *w++ = (uint64_t) bb[i];
 return *outlen = total, rec; }
// the second half: the full image, and beside it the derived record for each frozen
// baseline -- subout[i] for bases[i], NULL where it did not fit (a foreign record, or a
// prefix longer than this blob). a NULL is not an error: the caller lays that layer
// whole, which is only bigger.
void *ai_image_save_over(struct ai *g, uintptr_t *outlen, struct ai_image_guard const *guard,
                         void *const *bases, uintptr_t const *blens, uintptr_t nbase,
                         void **subout, uintptr_t *sublens) {
 struct image_hdr H;
 uintptr_t nw = 0;
 if ((word*) g->sp != topof(g)) return g->image_why = 7, NULL;   // quiescent, like ai_image_save
 word *blob = img_build(g, &H, guard, &nw);
 if (!blob) return NULL;
 for (uintptr_t i = 0; i < nbase; i++)
  subout[i] = img_derive(g, bases[i], blens[i], &H, blob, nw, &sublens[i]);
 void *buf = img_wire(g, &H, blob, nw, outlen);
 return g->alloc(g, blob, 0), buf; }
// the image-wake progress hook: weak no-op, overridden by a port bringing the
// wake up on new metal (a crashed wake with no debugger is otherwise invisible).
// stages: 1 header, 2 pool, 3 blob, 4 the token stream expanded, 0x100+k walk (per 64K words), 5 walk, 6 roots.
__attribute__((weak)) void ai_image_note(uintptr_t stage) { (void) stage; }
// the wake, over a stream that may carry more than this image: `buf` holds the header,
// dictionary and token stream to read, and `Hw` is the header to wake with -- the same one
// for a plain image, the derived record's for a prefix of it. `patch` names the prefix
// words the deriving session changed.
static struct ai *img_wake(void const *buf, uintptr_t len, struct image_hdr const *Hw,
                           uint64_t const *patch, uintptr_t npatch,
                           void *(*al)(struct ai*, void*, size_t)) {
 struct image_hdr S, H = *Hw;                            // S: the STREAM's own header
 if (len < sizeof S) return NULL;
 memcpy(&S, buf, sizeof S);
 if (S.magic != ImageMagic || S.wordsize != sizeof(word) || S.arch != ImageArch) return NULL;
 if (H.magic != ImageMagic || H.wordsize != sizeof(word) || H.arch != ImageArch) return NULL;
 if (H.nwords > S.nwords) return NULL;                   // a derived image is a PREFIX, never longer
 uintptr_t nw = H.nwords, db = ImageNDict * sizeof(word), ns = S.nstream;
 // ⚠ the stream's length is the HEADER's, never the buffer's: a baked image arrives inside a
 // reserved section and a file may carry a shebang, so "the rest of what you handed me" is
 // the one reading that would make a good image look foreign and fall silently back to the egg.
 if (len < sizeof S + db + ns) return NULL;                       // truncated buffer
 ai_image_note(1);
 struct ai *g = ai_ini_m(al);
 if (!g) return NULL;
 if (nw > g->major_len) {                                // grow the major pool to fit the image
  g->alloc(g, g->major_pool, 0);
  // ⚠ the slack is what the NURSERY ramps into: a minor is forced to a major once the
  // pool has less free than a whole nursery (gen_please's worst-case promotion test),
  // and the nursery doubles toward its overhead setpoint, so slack at 25% of live is
  // outgrown within a few doublings -- and the major it then forces copies the whole
  // woken image. hold a floor instead; the pages stay untouched until the ramp wants them.
  g->major_len = nw + (nw >> 2 > (1u << 19) ? nw >> 2 : 1u << 19);
  g->major_pool = g->major_base = g->alloc(g, NULL, 2 * g->major_len * sizeof(word));
  if (!g->major_pool) return NULL;
 }
 word *base = g->major_base;
 if (!base) return NULL;
 ai_image_note(2);
 g->major_hp = base + nw;
 ai_image_note(3);
 // ⚠ THE CHECK IS A DISTANCE, NEVER TWO ADDRESSES, and that is the last thing between a
 // bake and a hash anyone can check: the two symbols shift together under ASLR, so storing
 // where they LANDED wrote this run's mmap base into the header and two bakes of one tree
 // differed there and nowhere else. The gap between them is the same number every run and
 // discriminates exactly as well -- it is what the old pair was compared FOR (the deltas
 // agreeing IS the gap being preserved), and a stale or cross-arch binary moves one symbol
 // without the other. delta is 0 now in every lane: absolutes are stored anchor-relative,
 // so nothing on the decode side wants a shift at all.
 intptr_t delta = 0;
 // ⚠ THE ANCHOR IS UNCONDITIONAL, symbolic image or not. It used to be skipped once nothing
 // was left to relocate -- true of relocation, and the wrong question: an index still MEANS
 // whatever this binary's tables say, so a foreign build reads the same words as other
 // functions. The gap between two of our own symbols answers that for free and moves on
 // ANY layout change, which is more than a roster of the tables could promise.
 if ((intptr_t)((word) &ai_image_save - (word) image_immortals) != (intptr_t) H.anchor)
  return NULL;                                                                   // a DIFFERENT binary -> normal boot
 // EXPAND the token stream into the pool, then decode it there IN PLACE. The two passes
 // read and write one word at a time at the same index, so src and base are the same array
 // -- and a payload word arrives already seated, which is why the flat-leaf memcpys are gone.
 { unsigned char const *p0 = (unsigned char const*) buf + sizeof S + db,
                       *q = img_expand(base, nw, p0, p0 + ns, (word const*)((char const*) buf + sizeof S));
   // a whole image consumes its stream exactly; a prefix stops where its words end and
   // the tail is the parent's business.
   if (!q || (nw == S.nwords && q != p0 + ns)) return NULL; }
 // ..then the words this layer changed since it was frozen; every other word of the
 // prefix is the parent's, byte for byte.
 for (uintptr_t i = 0; i < npatch; i++) {
  uintptr_t ix = (uintptr_t) patch[2 * i];
  if (ix >= nw) return NULL;
  base[ix] = (word) patch[2 * i + 1]; }
 ai_image_note(4);
 word const *src = base;
 for (uintptr_t off = 0; off < nw; ) {
  uintptr_t sz;
  union u *p = (union u*)(base + off);
  word const *s = src + off;
  base[off] = (word) img_decode((intptr_t) s[0], base, delta);                // word0 first: the ap (kinding needs it real)
  if (in_data(p->ap)) { sz = image_datasize(p, s);                                // data kinds: size by ai_typ + the SOURCE's raw length words
   switch (ai_typ(p)) {
    case DChain: base[off + 1] = (word) img_decode((intptr_t) s[1], base, delta);
                 base[off + 2] = (word) img_decode((intptr_t) s[2], base, delta); break;
    case DNom:   base[off + 1] = (word) img_decode((intptr_t) s[1], base, delta); break;   // code + dig ride raw
    case DTray:  if (tray(p)->type == ai_O) { word *e = (word*) tray_data(tray(p)); uintptr_t ne = tray_nelem(tray(p));
                  for (uintptr_t i = 0; i < ne; i++) e[i] = img_decode(e[i], base, delta); }
                 break;
    default:     break; }                                                         // flat leaves: payload is already seated
  } else {                                                                        // thread: the ENCODED terminator is its head's byte offset | tag
   word term = (word)(off * sizeof(word) + ai_thread_tag); uintptr_t k = 1;
   uintptr_t kmax = nw - off;                                                     // BOUND the walk: a mis-decoded word0 must refuse
   for (;; k++) {                                                                 // ONE pass, decoding to the terminator (rung 2):
    if (k >= kmax) return NULL;                                                   // the load, never march off the pool (on metal the                                               // the load, never march off the pool (on metal the
    if (s[k] == term) break;                                                      // pool's edge is a dead bus, and a dead bus is MUTE)
    base[off + k] = (word) img_decode((intptr_t) s[k], base, delta); }
   base[off + k] = (word) p + ai_thread_tag;                                      // the terminator, decoded by hand: its head went live
   sz = k + 1; }
  off += sz; }
 ai_image_note(5);
 uintptr_t nv = (word*) g->end - (word*) &g->v0;                         // same struct/binary (anchor-checked) -> same layout
 if (H.nroot != 2 + nv) return NULL;                                     // root count mismatch -> stale/foreign image -> normal boot
 g->symbols = image_root_dec(H.root_tag[0], H.root_val[0], base);
 g->tasks   = (union u*) image_root_dec(H.root_tag[1], H.root_val[1], base);
 // ⚠ the parked ring is NOT in the image: an fd means nothing in a new process,
 // and a baker is single-tasked -- a woken runtime starts empty
 g->parked  = NULL;
 for (uintptr_t i = 0; i < nv; i++) ((word*) &g->v0)[i] = image_root_dec(H.root_tag[2 + i], H.root_val[2 + i], base);
 g->next_serial = H.next_serial;
 g->hot_io = zero;   // ⚠ a worn port names an fd, which means nothing in a new process -- a woken task wears the console (the parked ring's rule)
 ai_image_note(6);
 // sp stays at ai_ini's topof(g) (empty AI stack); the dispatch re-establishes ip
 g->major_live0 = nw, g->since_major = 0;
 // seed the nursery against the live set the image arrives with: the resize controller
 // otherwise ramps from the bare floor a doubling -- and a collection -- at a time,
 // and a woken runtime already knows how much it will be scanning past.
 { uintptr_t want = nw >> 1;
   if (want > (uintptr_t) g->len) { struct ai *h = gen_grow(g, want); if (ai_ok(h)) g = h; } }
 return g; }
struct ai *ai_image_load_m(void const *buf, uintptr_t len, void *(*al)(struct ai*, void*, size_t)) {
 struct image_hdr H;
 if (len < sizeof H) return NULL;
 memcpy(&H, buf, sizeof H);
 return img_wake(buf, len, &H, NULL, 0, al); }
struct ai *ai_image_load(void const *buf, uintptr_t len) { return ai_image_load_m(buf, len, ai_libc_alloc); }
// ..and a derived image: the first H.nwords words of the parent's stream, patched. `sub`
// is what ai_image_save_over answered; a torn one answers NULL and the caller boots the
// egg. the record must be word-aligned -- the container lays entries at 8.
struct ai *ai_image_load_over(void const *parent, uintptr_t plen, void const *sub, uintptr_t slen) {
 struct image_hdr D;
 if (slen < sizeof D + sizeof(uint64_t)) return NULL;
 memcpy(&D, sub, sizeof D);
 uint64_t const *w = (uint64_t const*)(void const*)((char const*) sub + sizeof D);
 uint64_t np = w[0];
 if (np > (uint64_t) D.nwords
     || slen < sizeof D + (1 + 2 * (uintptr_t) np) * sizeof(uint64_t)) return NULL;
 return img_wake(parent, plen, &D, w + 1, (uintptr_t) np, ai_libc_alloc); }

// ============================================================================
// sym
// ============================================================================
// (intern s) -> the interned symbol named by string s; identity on any other arg.
// the empty spelling names nothing: (intern "") is ().
lvm(lvm_intern) {
 if (strp(Sp[0])) {
  if (Sp[0] == EmptyString) ai_musttail return Answer(ZeroPoint);  // (intern "") -> () (zero-ontology: the empty spelling is the zero point)
  word y;
  Have(intern_reserve(g));
  Pack(g), y = intern_checked(g, str(g->sp[0])), Unpack(g);
  Sp[0] = y; }
 ai_musttail return Next(1); }

// (mint _) -> a fresh nameless POINT, identity its only property (the arg is
// ignored). `code` gets the mint serial: its hash and its order key, GC-stable.
// mints answer nomp, so they bind as gensyms.
lvm(lvm_mint) {
 Have(Width(struct ai_mint));
 struct ai_mint *y = (struct ai_mint*) Hp;
 Hp += Width(struct ai_mint);                   // mints are uniform: ap, code
 ini_missing(y, ++g->next_serial);
 return
  Sp[0] = word(y),
  Ip += 1,
  Continue(); }

// (nom n) -> a FRESH, uninterned named point: a string names it, a symbol lends
// its spelling, anything else falls to a bare mint. two (nom 'x) are distinct --
// the gensym-with-a-name.
lvm(lvm_nomctor) {
 Have(Width(struct ai_nom));                    // >= Width(struct ai_mint), so the bare-mint fallback fits too
 word n = Sp[0];                                // re-read post-GC (the stack is rooted)
 struct ai_str *nm = strp(n) ? str(n) : nom_str(g, n);   // a string is the name; a sym lends its spelling
 if (!nm) ai_musttail return Ap(lvm_mint, g);               // no name -> a bare mint
 struct ai_nom *y = (struct ai_nom*) Hp; Hp += Width(struct ai_nom);
 ini_nom(y, word(nm), ++g->next_serial, nom_dig(word(nm)));
 ai_musttail return Answer(word(y)); }

struct ai *intern(struct ai*g) {
 if (!ai_ok(g)) return g;                        // ⚠ intern_reserve READS g, and ai_have's guard is
                                                 // too late (it is an ARGUMENT): a caller's scare
                                                 // was dereferenced rather than propagated
 if (ai_ok(g = ai_have(g, intern_reserve(g))))   // atom + (at the load factor) the doubled backing
  g->sp[0] = intern_checked(g, str(g->sp[0]));
 return g; }

// what a fresh intern may bump: the atom, plus (at the load factor) the doubled
// backing. callers reserve this BEFORE intern_checked, so the insert never allocates.
uintptr_t intern_reserve(struct ai *g) {
 word m = g->symbols;
 uintptr_t extra = m && (map_len(m) + 1) * 4 >= map_cap(m) * 3 ? 4 + 4 * map_cap(m) : 0;
 return Width(struct ai_nom) + extra; }   // a named symbol is one flat KNom (name + serial)

// probe the WEAK intern map by string content; a miss mints the canonical KNom
// and inserts it. one canonical nom per spelling. bump-only in here (see intern_reserve).
ai_noinline word intern_checked(struct ai *g, struct ai_str *b) {
 word m = g->symbols;
 bool found; uintptr_t i = map_probe(g, m, word(b), &found);
 if (found) return map_slots(m)[2 * i + 1];
 if ((map_len(m) + 1) * 4 >= map_cap(m) * 3) {           // at load: rehash into a doubled backing
  uintptr_t ncap = 2 * map_cap(m), nmask = ncap - 1;
  union u *nb = map_fill_back(bump(g, 4 + 2 * ncap), ncap);
  word *os = map_slots(m), *ns = &nb[3].x;
  uintptr_t ocap = map_cap(m), nlen = 0;
  for (uintptr_t j = 0; j < ocap; j++) {
   word k = os[2 * j];
   if (k == map_gap) continue;
   uintptr_t x = hash(g, k) & nmask;
   while (ns[2 * x] != map_gap) x = (x + 1) & nmask;
   ns[2 * x] = k, ns[2 * x + 1] = os[2 * j + 1], nlen++; }
  nb[1].x = putcharm(nlen);
  cell(m)[1].x = (word) nb;                              // swap backing; header identity stable
  i = map_probe(g, m, word(b), &found); }
 struct ai_nom *y = ini_nom(bump(g, Width(struct ai_nom)), word(b), ++g->next_serial, nom_dig(word(b)));  // the canonical KNom: name + serial + cached spelling hash
 word *slots = map_slots(m);
 slots[2 * i] = word(b), slots[2 * i + 1] = word(y);
 cell(map_back(m))[1].x = putcharm(map_len(m) + 1);
 return word(y); }

// (nom? x): a REAL point -- a non-() mint or a named nom; () is the one point
// that is NOT nom?
op11(lvm_nomp, (nomp(Sp[0]) && Sp[0] != ZeroPoint) ? putcharm(1) : zero)
// (name? x): a NAMED point only (KNom) -- a nom with a spelling. name? => nom?; the gap
// nom? \ name? is the anonymous-but-real mints (gensyms).
op11(lvm_namep, namep(Sp[0]) ? putcharm(1) : zero)
// (mint? x): that gap, asked directly -- a BARE point, the gensym `nom` hands back.
// mint? and name? PARTITION nom?, and () is in neither. the only way to ask, since
// `string` answers text for every point alike and a mint's spelling is "".
op11(lvm_mintp, (mintp(Sp[0]) && Sp[0] != ZeroPoint) ? putcharm(1) : zero)
op11(lvm_packp, (packp(Sp[0]) || gemp(Sp[0]) || sunp(Sp[0]) || twinp(Sp[0])) ? putcharm(1) : zero)  // the pack family: arrays + the lean gem/sun/twin scalar boxes
op11(lvm_bigp, bigp(Sp[0]) ? putcharm(1) : zero)
op11(lvm_sunp, sunp(Sp[0]) ? putcharm(1) : zero)
op11(lvm_setp, trayp(Sp[0]) ? putcharm(1) : zero)
// (int x): truncate a float scalar to a fixnum; other numbers pass through. Used by
// num-ap to get an integer composition count from a non-integer numeral operator.
// int: a gem truncates toward zero, SATURATING at the charm bounds like the other
// rungs (the bare cast wrapped above 2^62 -- UB read as 0); an exact-ratio coin
// truncates by long division; everything else passes through.
lvm(lvm_intf) {
 if (ai_ratio_exact(g, Sp[0])) { Pack(g); g = ai_ratio_rung(g, 0);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 if (gemp(Sp[0])) { ai_flo_t v = gem_get(Sp[0]);
  Sp[0] = putcharm(v >= (ai_flo_t) maxcharm ? maxcharm
                 : v <= (ai_flo_t) mincharm ? mincharm
                 : v != v ? 0 : (intptr_t) v); }
 Ip += 1; ai_musttail return Continue(); }

// ============================================================================
// chain
// ============================================================================
op11(lvm_cap, chainp(Sp[0]) ? A(Sp[0]) : Sp[0])
op11(lvm_cup, chainp(Sp[0]) ? B(Sp[0]) : ZeroPoint)   // cup of an atom -> the const () (ZeroPoint), NOT the moving core (which had serial g->ip, not 0)
op11(lvm_books, g->book)   // the live layer chain (the abyss) -- runtime-internal, mopped at birth; ev.l's gv walks it
op11(lvm_setbooks, (g->book = Sp[0], zero))   // SET the layer chain: the scope-layer door (open/use/close ride it); runtime-internal, mopped at birth
// (mods _): the MODULE REGISTRY book (g->mods), a lazy singleton so both
// bootstrap prel runs capture the SAME tablet; runtime-internal, mopped at birth
lvm(lvm_mods) {
 if (g->mods == zero) {
  uintptr_t cap = map_min_cap, nb = 4 + 2 * cap;
  Have(nb + 3);
  union u *b = map_fill_back((union u*) Hp, cap), *h = (union u*) (Hp + nb);
  h[0].ap = lvm_map_lookup, h[1].x = (word) b, tagthread(h, 2);
  Hp += nb + 3;
  g->mods = (word) h; }
 ai_musttail return Answer(g->mods); }
// a frontend bakes no sources unless it says so (love.h)
__attribute__((weak)) struct ai_lib const *ai_libs(void) { return NULL; }
// (lib nm): the SOURCE LIBRARY -- the frontend's static table (love.h), answering nm's
// baked .l text as a READ PORT over the C string itself, or nothing on a miss. The text
// is never copied: no source is a love value, none is traced by a collection, and none
// reaches an image. A miss falls through to `use`'s filesystem walk (love/prel.l).
lvm(lvm_lib) {
 struct ai_lib const *t = ai_libs();
 struct ai_str *nm = nomp(Sp[0]) ? nom_str(g, Sp[0]) : NULL;
 if (t && nm) for (; t->nom; t++) {
  if (strlen(t->nom) != len(nm) || memcmp(t->nom, txt(nm), len(nm))) continue;
  Have(Width(struct ti) + Width(struct ai_tag));   // ⚠ nm dies here; the re-run re-finds the row
  struct ti *p = (struct ti*) Hp;
  Hp += Width(struct ti) + Width(struct ai_tag);
  p->io.ap = lvm_port_io;
  p->io.vt = &ai_ti_vt;
  p->io.ungetc_buf = putcharm(EOF);
  p->t = (ai_word) t->src, p->i = putcharm(0);
  tagthread((union u*) p, Width(struct ti));
  ai_musttail return Answer(word(p)); }
 ai_musttail return Answer(zero); }
// push a fresh writable LAYER at the head of the book chain -- the runtime's
// enter: the session's scope, every defglob's target
struct ai *ai_layer_(struct ai *g) {
 if (!ai_ok(g)) return g;
 if (!ai_ok(g = map_new(g))) return g;                 // sp[0] = the fresh layer map
 g = gxr(ai_push(g, 1, ai_core_of(g)->book));          // (layer . chain)
 if (!ai_ok(g)) return g;
 ai_core_of(g)->book = *ai_core_of(g)->sp;
 return ai_pop(g, 1); }
// drop the link just below the head -- the runtime's bare leave, the inverse of
// one `use`; nothing below the head is a no-op
struct ai *ai_unsplice_(struct ai *g) {
 if (!ai_ok(g)) return g;
 word bk = ai_core_of(g)->book;
 if (!chainp(B(bk))) return g;
 g = gxl(ai_push(g, 2, A(bk), B(B(bk))));              // (head . below-the-neighbour)
 if (!ai_ok(g)) return g;
 ai_core_of(g)->book = *ai_core_of(g)->sp;
 return ai_pop(g, 1); }
op11(lvm_chainp, (chainp(Sp[0]) && !nomp(Sp[0])) ? putcharm(1) : zero)  // the SURFACE chain?: a real compound list. a named symbol reads (name . mint) but counts as an atom
lvm(lvm_link) {
 Have(Width(struct ai_chain));
 struct ai_chain *w = (struct ai_chain*) Hp;
 Hp += Width(struct ai_chain);
 ini_chain(w, Sp[0], Sp[1]);
 *++Sp = word(w);
 Ip++;
 ai_musttail return Continue(); }

#define avm_slow(op, vop, ovf, fexpr) static lvm(lvm_##op##n) { \
 word a = Sp[0], b = Sp[1]; \
 if (trayp(a) || trayp(b)) return Ap(lvm_vbin, g, vop); \
 if (twinp(a) || twinp(b)) return Ap(lvm_twin_bin, g, vop); \
 if (!isnum(a) || !isnum(b)) ai_musttail return Push(ZeroPoint); \
 if (gemp(a) || gemp(b)) { word _res; Have(box_req); \
  ai_flo_t ad = toflo(a), bd = toflo(b); \
  emit_gem(_res, fexpr); \
  ai_musttail return Push(_res); } \
 if (!bigp(a) && !bigp(b)) { intptr_t av = toint(a), bv = toint(b), t; \
  if (!ovf(av, bv, &t)) { word _res; Have(box_req); emit_int(_res, t); \
   ai_musttail return Push(_res); } } \
 if ((vop) == vop_mul) ai_musttail return Ap(lvm_bmul_start, g); /* O(n^2): run yieldable */ \
 Pack(g); g = ai_big_binop(g, vop); \
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g); \
 ai_musttail return Resume(); }
#define avm_slowdiv(op, vop, c_op, fexpr) static lvm(lvm_##op##n) { \
 word a = Sp[0], b = Sp[1]; \
 if (trayp(a) || trayp(b)) return Ap(lvm_vbin, g, vop); \
 if (twinp(a) || twinp(b)) return Ap(lvm_twin_bin, g, vop); \
 if (!isnum(a) || !isnum(b)) ai_musttail return Push(ZeroPoint); \
 if (gemp(a) || gemp(b) || b == zero) { word _res; Have(box_req); \
  ai_flo_t ad = toflo(a), bd = toflo(b); \
  emit_gem(_res, fexpr); \
  ai_musttail return Push(_res); } \
 if (!bigp(a) && !bigp(b)) { intptr_t av = toint(a), bv = toint(b); \
  if (!(av == INTPTR_MIN && bv == -1)) { word _res; Have(box_req); emit_int(_res, av c_op bv); \
   ai_musttail return Push(_res); } } \
 return Ap(lvm_bdiv_start, g, vop); }   /* big // and % run yieldable (resumable long division) */
// a bare mint (() too) RIDES THROUGH every dyadic arithmetic lane, either side:
// the unit is the do-nothing operand, so x - () = () - x = x, likewise / // % &
// | ^ << >>. comparisons and `=` stay strict.
#define avm_unit(a, b) \
 if (mintp(a)) ai_musttail return Push(b); \
 if (mintp(b)) return *++Sp = a, Ip++, Continue()
#define avm_div(op, c_op) lvm(lvm_##op) { \
 word a = Sp[0], b = Sp[1]; \
 if (charmp(a) && charmp(b)) { \
  intptr_t av = getcharm(a), bv = getcharm(b); \
  if (bv != 0 && !(av == INTPTR_MIN && bv == -1)) { \
   intptr_t t = av c_op bv; \
   if (t >= mincharm && t <= maxcharm) \
    ai_musttail return Push(putcharm(t)); } } \
 avm_unit(a, b); \
 ai_musttail return Ap(lvm_##op##n, g); }
// the ordered comparisons (< <= > >=) and their total order over all values are
// defined after vcmp_int/vcmp_flo (the per-op helpers they reuse), by lvm_vbin.
#define bit_slow(n, c_op) static lvm(lvm_##n##_slow) {               \
 word a = Sp[0], b = Sp[1], _res;                                     \
 if (!(charmp(a) || sunp(a)) || !(charmp(b) || sunp(b)))                  \
  ai_musttail return Push(ZeroPoint);                               \
 Have(box_req);                                                       \
 emit_int(_res, toint(a) c_op toint(b));                                    \
 ai_musttail return Push(_res); }
#define mvm1(n) lvm(lvm_##n) { return Ap(lvm_math1, g, ai_##n); }
#define m1(_) _(sin) _(cos)   // sqrt/exp/tan/atan derived; sin/cos/log are the kept transcendentals (log has its own ap)


avm_slow(add, vop_add, __builtin_add_overflow, ad + bd)
avm_slow(sub, vop_sub, __builtin_sub_overflow, ad - bd)
avm_slow(mul, vop_mul, __builtin_mul_overflow, ad * bd)

avm_slowdiv(fquot, vop_fquot, /, ai_trunc(ad / bd))  // `//` truncating: float operand floors toward zero
avm_slowdiv(rem, vop_rem, %, ai_fmod(ad, bd))    // NaN on bd == 0

// `/` true division: exact integer when b divides a, a float box otherwise
// (the truncating quotient is `//`)
static lvm(lvm_quotn) {
 word a = Sp[0], b = Sp[1];
 if (trayp(a) || trayp(b)) return Ap(lvm_vbin, g, vop_quot);
 if (twinp(a) || twinp(b)) return Ap(lvm_twin_bin, g, vop_quot);
 if (!isnum(a) || !isnum(b)) ai_musttail return Push(ZeroPoint);
 if (gemp(a) || gemp(b) || b == zero) { word _res; Have(box_req);   // ±inf/NaN on ÷0
  ai_flo_t ad = toflo(a), bd = toflo(b);
  emit_gem(_res, ad / bd);
  ai_musttail return Push(_res); }
 if (!bigp(a) && !bigp(b)) { intptr_t av = toint(a), bv = toint(b);  // bv != 0 (b != zero)
  if (!(av == INTPTR_MIN && bv == -1)) {                            // INT_MIN/-1 is exact but overflows -> bignum lane
   if (av % bv == 0) { word _res; Have(box_req); emit_int(_res, av / bv);
    ai_musttail return Push(_res); }
   word _res; Have(box_req);                                        // inexact -> promote to float
   emit_gem(_res, (ai_flo_t) av / (ai_flo_t) bv);
   ai_musttail return Push(_res); } }
 Pack(g); g = ai_big_quot_true(g);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 ai_musttail return Resume(); }

// `-`: fixnum fast path, the () unit, then coins (`-` has no kind matrix, so the
// interception lives here), then the numeric slow lane
lvm(lvm_sub) {
 word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) { intptr_t t;
  if (!__builtin_sub_overflow((intptr_t) getcharm(a), (intptr_t) getcharm(b), &t) &&
      t >= mincharm && t <= maxcharm)
   ai_musttail return Push(putcharm(t)); }
 avm_unit(a, b);
 if (coinp(a) || coinp(b)) ai_musttail return Ap(lvm_sub_coin, g);
 ai_musttail return Ap(lvm_subn, g); }
// lvm_mul + its kind matrix live after the `+` string lane (they reuse nom_str /
// stringrank for the symbol-repetition case), below.

// `+` on sequences is order-preserving concatenation, a scalar lifting into the
// sequence on the side it appears:
//   str + str  -> byte concat          list + list -> spine append
//   str + list -> (link str list)      list + str  -> (append list (list str))
//   num + str  -> byte at front        num + list  -> (link num list)  (etc.)
// ai_add_lr selects the ordered reading.
// FIXME if we always want to allow commutative reading should this be false?
static const bool ai_add_lr = true;
// THE BYTE LAW: text + number is one byte, strictly an exact integer 0..255
// (rep-blind: 66.0 is 66); anything else zero. answers the byte or -1.
static ai_inline intptr_t seq_byte(word x) {
 if (charmp(x)) { intptr_t v = getcharm(x); return v < 0 || v > 255 ? -1 : v; }
 if (gemp(x)) { ai_flo_t f = gem_get(x);
  if (!(f >= 0 && f <= 255)) return -1;                 // range first (nan fails); cast below is safe
  return f != (ai_flo_t) (intptr_t) f ? -1 : (intptr_t) f; }
 return -1; }
// LIST lane: at least one operand is a chain (the matrix only routes list-involved
// chains here). list+list -> spine append; elt<->list -> the non-list operand joins
// as a scalar element (front if it is on the left, else appended at the tail).
static lvm(lvm_add_seq) {
 // a named symbol is an ATOM for + (an element to adjoin), so the list tests use
 // chainp; sym + sym/str/num falls through to zero (no symbol string algebra)
 word a = Sp[0], b = Sp[1];
 if (chainp(a) && chainp(b)) {                         // list + list -> append a..b
  uintptr_t n = llen(a); Have(n * Width(struct ai_chain));
  a = Sp[0], b = Sp[1];
  struct ai_chain *base = (struct ai_chain*) Hp, *w = base;
  Hp += n * Width(struct ai_chain);
  for (word l = a; chainp(l); l = B(l), w++) ini_chain(w, A(l), word(w + 1));
  (w - 1)->b = b;                                // last cdr -> b
  ai_musttail return Push(word(base)); }
 if (chainp(a) || chainp(b)) {                          // elt <-> list (a bare mint never
  bool front = !ai_add_lr || chainp(b);               // reaches here -- lvm_add's identity early-out caught it)
  word lst = chainp(a) ? a : b, elt = chainp(a) ? b : a;
  if (front) { Sp[0] = elt, Sp[1] = lst; ai_musttail return Ap(lvm_link, g); }  // (link elt list)
  uintptr_t n = llen(lst) + 1; Have(n * Width(struct ai_chain));        // append elt at tail
  lst = chainp(Sp[0]) ? Sp[0] : Sp[1], elt = chainp(Sp[0]) ? Sp[1] : Sp[0];
  struct ai_chain *base = (struct ai_chain*) Hp, *w = base;
  Hp += n * Width(struct ai_chain);
  for (word l = lst; chainp(l); l = B(l), w++) ini_chain(w, A(l), word(w + 1));
  ini_chain(w, elt, ZeroPoint);                     // trailing (elt . ()) -- list terminator (zero-ontology)
  ai_musttail return Push(word(base)); }
 ai_musttail return Push(ZeroPoint); }          // neither is a real list (e.g. sym + sym/str/num): no algebra -> zero

// --- TEXT lane: strings + symbols ---
// the string tower is STRING (0) < UNINTERNED-SYM (1) < NAMED-SYM|NUM (2); mixing
// demotes to the lower rank (min keeps the partner's type). the concat is built
// as one string in operand order, then returned per rank: as-is / fresh mint / interned.
static ai_inline struct ai_str *nom_str(struct ai *g, word x) {   // symbol -> name string, or 0 (a bare mint / the zero point / a non-symbol)
 return namep(x) ? str(nom(x)->name) : 0; }  // a named point (KNom) carries its name; a bare mint is nameless
static ai_inline int stringrank(struct ai *g, word x) {    // STR 0 / mint 1 / NAMED-sym|NUM 2
 if (strp(x)) return 0;
 if (namep(x)) return 2;          // a NAMED symbol: result re-interns (the min pulls a string operand to 0 -> demote)
 if (mintp(x)) return 1;          // a bare mint / the zero point: an uninterned (fresh) symbol
 return 2; }                      // a number contributes one byte (rank 2)
static ai_inline uintptr_t stringlen(struct ai *g, word x) {  // bytes x contributes to a concat
 if (strp(x)) return len(x);
 if (nomp(x)) { struct ai_str *n = nom_str(g, x); return n ? n->len : 0; }
 return 1; }                                            // number -> one byte
static ai_inline char *add_emit(struct ai *g, char *w, word x) {  // append x's bytes; return advanced w
 if (strp(x)) return (void) memcpy(w, txt(x), len(x)), w + len(x);
 if (nomp(x)) { struct ai_str *n = nom_str(g, x);
  return n ? ((void) memcpy(w, txt(n), n->len), w + n->len) : w; }
 return *w = (char) seq_byte(x), w + 1; }               // number -> one byte (unreachable from + since the
                                                        // degenerate lane; symbol paths never land here)
static lvm(lvm_add_string) {
 word a = Sp[0], b = Sp[1];
 if (trayp(a) || trayp(b)) ai_musttail return Push(ZeroPoint); // array <-> string: undefined
 if (!strp(a) && !nomp(a) && seq_byte(a) < 0) ai_musttail return Push(ZeroPoint);  // the byte law
 if (!strp(b) && !nomp(b) && seq_byte(b) < 0) ai_musttail return Push(ZeroPoint);
 int rank = min(stringrank(g, a), stringrank(g, b));
 uintptr_t n = stringlen(g, a) + stringlen(g, b);
 if (!n) ai_musttail return Push(rank ? zero : EmptyString);
 uintptr_t req = str_type_width + b2w(n);
 Have(req);
 a = Sp[0], b = Sp[1];                                  // re-read post-GC
 struct ai_str *z = ini_str(str(Hp), n);
 Hp += req;
 add_emit(g, add_emit(g, txt(z), a), b);                      // a's bytes then b's, in order
 *++Sp = word(z);
 return rank == 0 ? (Ip++, Continue())                  // string
      : rank == 1 ? Ap(lvm_mint, g)                  // uninterned symbol (fresh)
                  : Ap(lvm_intern, g); }               // interned symbol
static lvm(lvm_0) {                             // unsupported mix (array <-> string)
 ai_musttail return Push(ZeroPoint); }
// the UNIT lane: a bare mint rides through +/*. the dispatchers early-out a mint
// first, so these cells are belt and braces -- but they say the TRUE thing, so
// the matrix stands correct on its own (mx.v checks the whole square).
static lvm(lvm_bin_unit) {
 word a = Sp[0], b = Sp[1];
 ai_musttail return Push(mintp(a) ? b : a); }
// the DEGENERATE lane: a mixed pair with no lawful crossing answers the higher
// band's operand whole -- the foreigner arrives as that band's unit, since the
// only hom a group has into a free monoid is trivial. this is what restores +
// associativity (the byte law and the element-adjoin law could not associate).
static lvm(lvm_bin_a) { word a = Sp[0]; ai_musttail return Push(a); }
static lvm(lvm_bin_b) { word b = Sp[1]; ai_musttail return Push(b); }

// ============================================================================
// generic-op lane aps, the dispatch matrices, then the `+`/`*` dispatchers
// ============================================================================

// `*` REPEAT lane: a sequence times a scalar count n is n copies joined ("repeated
// +"). THE COUNT LAW (the associativity arc): a count acts by |count| when it is an
// EXACT integer -- magnitude is the one multiplicative hom that survives the sign
// crossing ((-1)*(-2) re-enters the positives) -- and any INEXACT count (gem, twin,
// tray) answers the absorbing () (those classes are closed under *, so the refusal
// composes: (x * 2.5) * 2 and x * (2.5 * 2 = 5.0) both land ()).
static lvm(lvm_mul_rep) {
 word a = Sp[0], b = Sp[1];
 bool aseq = strp(a) || chainp(a) || namep(a);       // a string / list / NAMED symbol repeats
 word seq = aseq ? a : b, cnt = aseq ? b : a;
 if ((!strp(seq) && !chainp(seq) && !namep(seq)) || (!charmp(cnt) && !bigp(cnt)))
  ai_musttail return Push(ZeroPoint);             // seq not a sequence/symbol, or count not exact
 uintptr_t n;
 if (charmp(cnt)) { intptr_t v = getcharm(cnt); n = (uintptr_t) (v < 0 ? -v : v); }
 else n = (uintptr_t) maxcharm;                      // |big|: past addressable, dies in Have()
 if (chainp(seq)) {                                   // list -> n copies of the spine
  if (!n) ai_musttail return Push(ZeroPoint);   // 0 copies -> the empty list () (zero-ontology)
  uintptr_t m = llen(seq), total = m * n;
  Have(total * Width(struct ai_chain));
  seq = chainp(Sp[0]) ? Sp[0] : Sp[1];                // re-read post-GC
  struct ai_chain *base = (struct ai_chain*) Hp, *w = base;
  Hp += total * Width(struct ai_chain);
  for (uintptr_t i = 0; i < n; i++)
   for (word l = seq; chainp(l); l = B(l), w++) ini_chain(w, A(l), word(w + 1));
  (w - 1)->b = ZeroPoint;                            // list terminator () (zero-ontology)
  ai_musttail return Push(word(base)); }
 // string / symbol spelling -> repeat the bytes; a symbol RE-INTERNS the result
 bool sym = namep(seq);
 struct ai_str *src = sym ? str(nom(seq)->name) : str(seq);
 uintptr_t sl = src->len, total = sl * n;
 if (!total) ai_musttail return Push(sym ? ZeroPoint : EmptyString);  // 0 copies: () for a sym, "" for a string
 uintptr_t req = str_type_width + b2w(total);
 Have(req);
 word sw = sym ? (namep(Sp[0]) ? Sp[0] : Sp[1]) : (strp(Sp[0]) ? Sp[0] : Sp[1]);  // re-read post-GC
 src = sym ? str(nom(sw)->name) : str(sw);
 struct ai_str *z = ini_str(str(Hp), total);
 Hp += req;
 for (uintptr_t i = 0; i < n; i++) memcpy(txt(z) + i * sl, txt(src), sl);
 *++Sp = word(z);
 return sym ? Ap(lvm_intern, g) : (Ip++, Continue()); }

// `*` CARTESIAN lane: chain * chain -> the ordered cartesian product (tally is
// the homomorphism; the outer loop ranges the LEFT operand so right-
// distributivity holds on the nose). 3*pairs chains total, one Have.
static lvm(lvm_mul_cart) {
 word a = Sp[0], b = Sp[1];
 if (!chainp(a) || !chainp(b)) ai_musttail return Push(ZeroPoint);   // chain*chain only
 uintptr_t m = llen(a), n = llen(b), pairs = m * n;
 if (!pairs) ai_musttail return Push(ZeroPoint);             // empty operand annihilates
 Have(3 * pairs * Width(struct ai_chain));
 a = Sp[0], b = Sp[1];                                               // re-read post-GC
 struct ai_chain *spine = (struct ai_chain*) Hp, *pc = spine + pairs;
 Hp += 3 * pairs * Width(struct ai_chain);
 uintptr_t idx = 0;
 for (word la = a; chainp(la); la = B(la)) {
  word av = A(la);
  for (word lb = b; chainp(lb); lb = B(lb), idx++) {
   struct ai_chain *p0 = pc + 2 * idx, *p1 = p0 + 1;
   ini_chain(p1, A(lb), ZeroPoint);                                  // (bj)
   ini_chain(p0, av, word(p1));                                      // (ai bj)
   ini_chain(spine + idx, word(p0), idx + 1 < pairs ? word(spine + idx + 1) : ZeroPoint); } }
 ai_musttail return Push(word(spine)); }

// --- apply lane (the data-value `(g x)` aps) ---
// an applied data value's sentinel tail-jumps straight to its handler -- no table.
// the sequences INDEX and JUXTAPOSE -- text by byte, a chain by element -- numbers are
// church numerals; opaque handles behave as 0 via their own sentinels. const-1 is the
// DEFAULT action: what an operand gets when its kind has no lane of its own (mx.l).

// (s k): index the string -- the unsigned byte at k, negatives from the end, else 1
// (matches "" == 0: a numeric ("" k) is k**0 == 1). a TEXT k JUXTAPOSES instead: (s t)
// is s then t, C's adjacent-literal law with the literal restriction lifted -- and it
// curries, so ("a" "b" "c") joins three. agrees with (+ s t) on every text pair.
static lvm(data_string_apply) {
 if (strp(Sp[0])) {
  uintptr_t m = len(Ip), n = len(Sp[0]), req = str_type_width + b2w(m + n);
  if (!(m + n)) { Ip = cell(*++Sp); *Sp = EmptyString; ai_musttail return Continue(); }  // no empty string is ever allocated
  Have(req);
  struct ai_str *z = ini_str(str(Hp), m + n);
  Hp += req;
  memcpy(txt(z), txt(Ip), m);
  memcpy(txt(z) + m, txt(Sp[0]), n);
  Ip = cell(*++Sp); *Sp = word(z); ai_musttail return Continue(); }
 word v = putcharm(1);
 if (oddp(Sp[0])) {
  word n = getcharm(Sp[0]);
  if (n < 0) n += (word) len(Ip);                       // -1 is the last byte
  if (n >= 0 && n < (word) len(Ip)) v = putcharm((unsigned char) txt(Ip)[n]); }
 Ip = cell(*++Sp); *Sp = v; ai_musttail return Continue(); }

// applying a symbol: a point applies as every unit does -- const-1
static lvm(data_sym_apply) {
 Ip = cell(*++Sp); *Sp = putcharm(1); ai_musttail return Continue(); }

// (n x): church-numeral application for the boxed tower -- the same
// [n, num-ap, x, ret] frame as lvm_numap
static lvm(data_num_apply) {
 Have(2);
 word h = hot_hook(g->hot_numap);
 word n = word(Ip), x = Sp[0], ret = Sp[1], *dst = Sp - 2;
 dst[0] = n, dst[1] = h, dst[2] = x, dst[3] = ret;
 Sp = dst; Ip = (union u*) numap_drive; ai_musttail return Continue(); }

// (l k): index the spine -- the kth element, negatives from the end, out of range the
// unit 1. (l m): a chain operand JUXTAPOSES -- the append, agreeing with (+ l m) on
// the nose (add_seq's list+list lane, spelled here). the text law, one lattice rung up:
// a chain indexes elements where text indexes bytes. every other operand is const-1.
static lvm(data_pair_apply) {
 if (chainp(Sp[0])) {
  uintptr_t n = llen(word(Ip));
  Have(n * Width(struct ai_chain));
  struct ai_chain *base = (struct ai_chain*) Hp, *w = base;
  Hp += n * Width(struct ai_chain);
  for (word l = word(Ip); chainp(l); l = B(l), w++) ini_chain(w, A(l), word(w + 1));
  (w - 1)->b = Sp[0];                        // last cdr -> the operand (a chain is never empty)
  Ip = cell(*++Sp); *Sp = word(base); ai_musttail return Continue(); }
 word v = putcharm(1);
 if (oddp(Sp[0])) {
  word k = getcharm(Sp[0]), l = word(Ip);
  if (k < 0) k += (word) llen(l);            // -1 is the last element
  if (k >= 0) { while (k-- > 0 && chainp(l)) l = B(l);
                if (chainp(l)) v = A(l); } }
 Ip = cell(*++Sp); *Sp = v; ai_musttail return Continue(); }

// === the two generic-op dispatch matrices (+ and *), indexed by ai_kind =====
// lanes: *n numeric/broadcast (every star and tray kind routes identically), add_seq
// a list anywhere, add_string strings (+ a number as one byte -- the byte law),
// mul_rep sequence * count, *l a lambda-or-map operand (church add / compose),
// lvm_0 undefined -> zero. precedence: lambda > tablet > chain > text > number.
// the tables are GENERATED: one datum (mx.l) feeds this header AND the rocq
// model mx.v, so theorem and code cannot drift. EDIT mx.l, not mx.h;
// `make test_clay` regenerates and fails on drift.
#include "mx.h"

// any value -> the kind it dispatches as (enum q, love.h): fixnum -> KCharm,
// non-data heap pointer -> KTablet/KHot, else the rep's kind. a TRAY is the one rep
// that dispatches four ways, by element tier. exported so the apply sentinels share
// it; it sits under mx.h for ai_kind_of_d, the rep -> kind crossing.
enum q ai_kind(word x) {
 if (charmp(x)) return KCharm;
 if (!datp(x)) return tabp(x) ? KTablet : KHot;
 enum d r = typ(x);
 if (r == DTray) return (enum q) (KTrayZ + tray(x)->type);
 return ai_kind_of_d[r]; }

// === the `+`/`*` dispatchers (fixnum fast path, then the matrix) ============
lvm(lvm_add) {
 word a = Sp[0], b = Sp[1]; intptr_t t;
 if (charmp(a) && charmp(b)
     && !__builtin_add_overflow((intptr_t) getcharm(a), (intptr_t) getcharm(b), &t)
     && t >= mincharm && t <= maxcharm)
  ai_musttail return Push(putcharm(t));
 // a bare mint is +'s identity in every lane; the matrix says the same thing
 // (lvm_bin_unit), so this is the fast path, never load-bearing
 if (mintp(a)) ai_musttail return Push(b);
 if (mintp(b)) ai_musttail return Push(a);
 ai_musttail return Ap(ai_add_mx[ai_kind(a)][ai_kind(b)], g); }
lvm(lvm_mul) {
 word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) { intptr_t t;
  if (!__builtin_mul_overflow((intptr_t) getcharm(a), (intptr_t) getcharm(b), &t)
      && t >= mincharm && t <= maxcharm)
   ai_musttail return Push(putcharm(t)); }
 // a bare mint is the ZERO, and the zero ANNIHILATES under * (the semiring law:
 // 0*x = 0; the identity is 1, which is already the identity function). the
 // matrix says the same thing (lvm_0), so this stays a fast path.
 if (mintp(a) || mintp(b)) ai_musttail return Push(ZeroPoint);
 ai_musttail return Ap(ai_mul_mx[ai_kind(a)][ai_kind(b)], g); }

avm_div(fquot, /)                               // `//` fixnum fast path: truncating quotient
avm_div(rem, %)
// `/` fixnum fast path: stay exact only when b divides a; otherwise the slow lane
// promotes to a float box. The INT_MIN/-1 guard precedes the `%` (it would be UB).
lvm(lvm_quot) {
 word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) { intptr_t av = getcharm(a), bv = getcharm(b);
  if (bv != 0 && !(av == INTPTR_MIN && bv == -1) && av % bv == 0) {
   intptr_t t = av / bv;
   if (t >= mincharm && t <= maxcharm) ai_musttail return Push(putcharm(t)); } }
 avm_unit(a, b);
 if (coinp(a) || coinp(b)) ai_musttail return Ap(lvm_quot_coin, g);   // the die's DIV method, slot 8
 ai_musttail return Ap(lvm_quotn, g); }

// The ordered comparisons (lvm_lt/le/gt/ge) and their total order are defined
// after vcmp_int/vcmp_flo (the per-op trichotomy helpers), near lvm_vbin.

// bitwise and/or/xor: the both-fixnum tag trick (two odds stay odd under & and |;
// ^ clears the tag, re-set it). integer-only: any other operand yields zero.
bit_slow(band, &) bit_slow(bor, |) bit_slow(bxor, ^)
lvm(lvm_band) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) ai_musttail return Push((a & b) | 1);
 avm_unit(a, b);
 if (trayp(a) || trayp(b)) return Ap(lvm_vbin, g, vop_band);
 ai_musttail return Ap(lvm_band_slow, g); }
lvm(lvm_bor) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) ai_musttail return Push((a | b) | 1);
 avm_unit(a, b);
 if (trayp(a) || trayp(b)) return Ap(lvm_vbin, g, vop_bor);
 ai_musttail return Ap(lvm_bor_slow, g); }
lvm(lvm_bxor) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) ai_musttail return Push((a ^ b) | 1);
 avm_unit(a, b);
 if (trayp(a) || trayp(b)) return Ap(lvm_vbin, g, vop_bxor);
 ai_musttail return Ap(lvm_bxor_slow, g); }
// (bitwise complement is `(^ x -1)`; logical not is the `!` reader sigil / `zerop`.)

// >> : arithmetic right shift; a fixnum only shrinks, so the fast path never allocates
static lvm(lvm_bsr_slow) { word a = Sp[0], b = Sp[1], _res;
 if (!(charmp(a) || sunp(a)) || !charmp(b)) ai_musttail return Push(ZeroPoint);
 Have(box_req);
 emit_int(_res, toint(a) >> shmask(getcharm(b)));
 ai_musttail return Push(_res); }
lvm(lvm_bsr) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b))
  ai_musttail return Push(putcharm(getcharm(a) >> shmask(getcharm(b))));
 avm_unit(a, b);
 if (trayp(a) || trayp(b)) return Ap(lvm_vbin, g, vop_bsr);
 ai_musttail return Ap(lvm_bsr_slow, g); }

// << : can overflow the tag, so it always runs the box/demote path; the shift is
// done in uintptr_t for well-defined overflow
lvm(lvm_bsl) { word a = Sp[0], b = Sp[1], _res;
 avm_unit(a, b);
 if (trayp(a) || trayp(b)) return Ap(lvm_vbin, g, vop_bsl);
 if (!(charmp(a) || sunp(a)) || !charmp(b)) ai_musttail return Push(ZeroPoint);
 Have(box_req);
 emit_int(_res, (intptr_t)((uintptr_t) toint(a) << shmask(getcharm(b))));
 ai_musttail return Push(_res); }

op(lvm_charmp, 1, oddp(Sp[0]) ? putcharm(1) : zero)   // (charm? x): a fixnum -- a charm, the tagged odd word
// (nil? x): the falsy predicate, (= 0 ($ x)) without the clamp. the single
// truthiness oracle: `?`, zerop and aall all consult ai_nilp, so the feel pass
// can drop a zerop wrapper.
op11(lvm_nilp, ai_nilp(g, Sp[0]) ? putcharm(1) : zero)

// Unary math nif: numeric arg → double, call fn, box the rank-0 f64 result.
// Non-numeric arg → zero. TCO-clean (no & escapes).
static lvm(lvm_math1, ai_flo_t (*fn)(ai_flo_t)) {
 word a = Sp[0];
 if (trayp(a)) {                               // (sin a-tray) etc. -> gem tray; a twin tray is undefined
  if (tray(a)->type == ai_C) return Answer(ZeroPoint);
  return Ap(lvm_vmap1, g, fn); }
 if (!isnum(a)) return Answer(ZeroPoint);
 ai_flo_t ad = toflo(a), rd = fn(ad);
 Have(gem_req);
 Sp[0] = mk_gem(&Hp, rd); return Next(1); }

static lvm(lvm_math2, ai_flo_t (*fn)(ai_flo_t, ai_flo_t)) {
 word a = Sp[0], b = Sp[1];
 if (trayp(a) || trayp(b)) {                               // (pow arr ..) etc. -> float array
  if ((trayp(a) && tray(a)->type == ai_C) || (trayp(b) && tray(b)->type == ai_C))
   return Push(ZeroPoint);                 // complex array undefined here
  return Ap(lvm_vmap2, g, fn); }
 if (!isnum(a) || !isnum(b)) return Push(ZeroPoint);
 ai_flo_t ad = toflo(a), bd = toflo(b), rd = fn(ad, bd);
 Have(gem_req);
 *++Sp = mk_gem(&Hp, rd); return Next(1); }


m1(mvm1)

// (log x): a positive real stays float; a negative real or complex widens to the
// complex principal value ~((log |z|) (arg z)) -- so (log -1) = (* i pi), euler in
// the exact direction. arrays stay elementwise float.
lvm(lvm_log) {
 word a = Sp[0];
 ai_flo_t m, th;
 if (twinp(a)) m = ai_log(twin_mod(a)), th = ai_atan2(twin_im(a), twin_re(a));
 else if (isnum(a) && toflo(a) < 0) { ai_flo_t ad = toflo(a);
  m = ai_log(-ad), th = ai_atan2(0, ad); }
 else return Ap(lvm_math1, g, ai_log);
 Have(twin_req);
 Sp[0] = mk_twin(&Hp, m, th); ai_musttail return Next(1); }

op11(lvm_gemp, gemp(Sp[0]) ? putcharm(1) : zero)

// ============================================================================
// tray
// ============================================================================
size_t const ai_T[] = {
 [ai_Z] = Bytes,
 [ai_R] = Bytes,
 [ai_C] = 2 * Bytes,      // complex scalar: (re, im)
 [ai_O] = Bytes, };       // object: one tagged l word per element

uintptr_t ai_tray_bytes(struct ai_tray *v) {
 return sizeof(struct ai_tray) + v->rank * sizeof(word) + ai_T[v->type] * tray_nelem(v); }

// ============================================================================
// rng
// ============================================================================
// xoshiro256++ seeded by SplitMix64. C holds no RNG state and never draws: the
// primitives are wheel (fresh state) and the functional steps turn/turnf, which
// copy the state and answer (value . new-state) -- the input is never mutated.
// the global rand/randf stream is prel lisp over the same steps. not a CSPRNG.

static ai_inline uint64_t rotl64(uint64_t x, int k) {
 return (x << k) | (x >> (64 - k)); }

// the uint64_t scratch lives in these ai_noinline helpers, moved via memcpy:
// taking &s in a VM ap defeats the sibcall, and memcpy is alignment-safe.

// advance the 4-word state at `payload` and return one 64-bit draw
static ai_noinline uint64_t rng_step(void *payload) {
 uint64_t s[4];
 memcpy(s, payload, sizeof s);
 uint64_t const result = rotl64(s[0] + s[3], 23) + s[0];
 uint64_t const t = s[1] << 17;
 s[2] ^= s[0]; s[3] ^= s[1]; s[1] ^= s[2]; s[0] ^= s[3];
 s[2] ^= t; s[3] = rotl64(s[3], 45);
 memcpy(payload, s, sizeof s);
 return result; }

// fill the state from a seed via SplitMix64; the all-zero state is xoshiro's
// fixed point, so substitute a nonzero word
static ai_noinline void rng_seed_into(void *payload, uint64_t seed) {
 uint64_t s[4], x = seed;
 for (int i = 0; i < rng_state_len; i++) {
  uint64_t z = (x += (uint64_t) 0x9e3779b97f4a7c15);
  z = (z ^ (z >> 30)) * (uint64_t) 0xbf58476d1ce4e5b9;
  z = (z ^ (z >> 27)) * (uint64_t) 0x94d049bb133111eb;
  s[i] = z ^ (z >> 31); }
 if (!(s[0] | s[1] | s[2] | s[3])) s[0] = 1;
 memcpy(payload, s, sizeof s); }

// Map a 64-bit draw to a float in [0,1): keep the high mantissa bits and scale.
static ai_inline ai_flo_t u64_to_unit(uint64_t u) {
#if Bits >= 64
 return (ai_flo_t) (u >> 11) * (ai_flo_t) 0x1.0p-53;
#else
 return (ai_flo_t) (uint32_t) (u >> 40) * (ai_flo_t) 0x1.0p-24f;
#endif
}

// shape v as a state tray and seed it; no &local, so an inlining caller keeps its tail call
void ai_rng_seed(struct ai_tray *v, uint64_t seed) {
 ini_tray(v, rng_vt, 1);
 v->shape[0] = rng_state_len;
 rng_seed_into(tray_data(v), seed); }

// Is x a well-formed state tray (rank-1 i64, length 4)?
static ai_inline bool rng_state_p(word x) {
 return packp(x) && tray(x)->rank == 1 && tray(x)->type == rng_vt
        && tray(x)->shape[0] == rng_state_len; }

// a fresh state tray at Hp copying src's limbs; caller holds Have(rng_tray_req)
static ai_inline struct ai_tray *rng_copy(ai_word **hp, struct ai_tray *src) {
 struct ai_tray *v = (struct ai_tray*) *hp;
 *hp += rng_tray_req;
 ini_tray(v, rng_vt, 1);
 v->shape[0] = rng_state_len;
 memcpy(tray_data(v), tray_data(src), rng_payload_bytes);
 return v; }

// canonicalize a 62-bit draw to the smallest integer tier. out-of-line so the
// limb[] scratch never forces a frame in lvm_turn (make vmret); bump-only.
static ai_noinline word rng_canon(struct ai *g, uint64_t r) {
 ai_limb limb[64 / limb_bits]; int nl = 0;               // split the 64-bit draw into native limbs (1 or 2)
 for (int i = 0; (size_t) i * limb_bits < 64; i++) limb[i] = (ai_limb) (r >> (i * limb_bits)), nl = i + 1;
 return ai_big_canon(&g->hp, limb, nl, false); }

// (wheel n): a fresh state tray deterministically seeded from fixnum n. A
// non-fixnum seeds from 0.
lvm(lvm_wheel) {
 word n = Sp[0];
 uint64_t seed = charmp(n) ? (uint64_t) (intptr_t) getcharm(n) : 0;
 Have(rng_tray_req);
 struct ai_tray *v = (struct ai_tray*) Hp; Hp += rng_tray_req;
 ai_rng_seed(v, seed);
 ai_musttail return Answer(word(v)); }

// (turn st): functional draw -> (value . st'), value a fixed 62 bits so a seed
// yields the IDENTICAL integer on every target; st is copied, never mutated
#define rng_draw_mask (((uint64_t) 1 << 62) - 1)              // 62 bits = 64-bit maxcharm
#define rng_draw_req  (Width(struct ai_big) + b2w((64 / limb_bits) * sizeof(ai_limb)))  // worst case: the 62-bit draw split into native limbs
lvm(lvm_turn) {
 word st = Sp[0];
 if (!rng_state_p(st)) ai_musttail return Answer(ZeroPoint);
 Have(rng_tray_req + rng_draw_req + Width(struct ai_chain));
 st = Sp[0];                                 // re-read post-Have
 struct ai_tray *v = rng_copy(&Hp, tray(st));
 uint64_t r = rng_step(tray_data(v)) & rng_draw_mask;
 Pack(g);
 word val = rng_canon(g, r);
 Unpack(g);
 struct ai_chain *p = (struct ai_chain*) Hp; Hp += Width(struct ai_chain);
 ini_chain(p, val, word(v));
 ai_musttail return Answer(word(p)); }

// (turnf st): functional draw -> (float . st'), float in [0,1).
lvm(lvm_turnf) {
 word st = Sp[0], _res;
 if (!rng_state_p(st)) ai_musttail return Answer(ZeroPoint);
 Have(rng_tray_req + box_req + Width(struct ai_chain));
 st = Sp[0];                                 // re-read post-Have
 struct ai_tray *v = rng_copy(&Hp, tray(st));
 uint64_t r = rng_step(tray_data(v));
 ai_flo_t u = u64_to_unit(r);
 emit_gem(_res, u);                                // box at Hp, into _res
 struct ai_chain *p = (struct ai_chain*) Hp; Hp += Width(struct ai_chain);
 ini_chain(p, _res, word(v));
 ai_musttail return Answer(word(p)); }

// ============================================================================
// eq
// ============================================================================
// α-equivalence of two stored lambda sources: bound variables match by binder
// position, free by symbol. `:` binders are not tracked (sound, conservative);
// a one-operand \ is quote, compared as data.
struct arib { word la, lb; int na, nb; struct arib *up; };  // binder rib: (p…body) lists + param counts
static int arib_pos(word s, word l, int n) {                // index of s among the first n of l, else -1
 for (int i = 0; i < n && chainp(l); i++, l = B(l)) if (A(l) == s) return i;
 return -1; }
static bool ai_isbs(struct ai *g, word h) {                  // h is the `\` symbol?
 struct ai_str *n; return (n = nom_str(g, h)) && n->len == 1 && n->bytes[0] == '\\'; }
static bool salpha(struct ai *g, word a, word b, struct arib *env) {
 if (nomp(a) || nomp(b)) {
  if (!nomp(a) || !nomp(b)) return false;
  for (struct arib *r = env; r; r = r->up) {
   int ia = arib_pos(a, r->la, r->na), ib = arib_pos(b, r->lb, r->nb);
   if (ia >= 0 || ib >= 0) return ia == ib; }               // bound at this rib: positions agree
  return a == b; }                                          // both free: same symbol
 if (!chainp(a) || !chainp(b)) return eqv(g, a, b);             // numbers / strings / atoms
 if (ai_isbs(g, A(a)) && ai_isbs(g, A(b))) {                        // both `\`-headed
  word pa = B(a), pb = B(b);
  if (!chainp(B(pa)) || !chainp(B(pb))) return eqv(g, a, b);    // one-operand \ = quote: data
  int na = 0, nb = 0;                                       // (\ p1..pn body): params = init, body = last
  word t = pa;
  for (; chainp(B(t)); t = B(t)) na++;
  word ba = A(t);
  for (t = pb; chainp(B(t)); t = B(t)) nb++;
  word bb = A(t);
  if (na != nb) return false;
  struct arib r = { pa, pb, na, nb, env };
  return salpha(g, ba, bb, &r); }
 return salpha(g, A(a), A(b), env) && salpha(g, B(a), B(b), env); }  // structural: app / ? / :
// α-invariant hash of a source \-expr, parallel to salpha: a bound variable hashes by its
// binder coordinate (rib depth, position), a free variable by its symbol code, so α-equal
// lambdas hash equal and the total order (cmp3, by repr hash) agrees with `=`.
static uintptr_t shash(struct ai *g, word x, struct arib *env) {
 if (nomp(x)) {
  int d = 0;
  for (struct arib *r = env; r; r = r->up, d++) {
   int i = arib_pos(x, r->la, r->na);
   if (i >= 0) return rot((uintptr_t) (d * 131 + i + 1) * mix); }
  return hash(g, x); }                  // a free variable: its stable identity hash (mint serial / interned name . mint)
 if (!chainp(x)) return hash(g, x);
 if (ai_isbs(g, A(x))) {
  word p = B(x);
  if (!chainp(B(p))) return hash(g, x);                       // one-operand \ = quote: data
  int n = 0;
  word t = p;
  for (; chainp(B(t)); t = B(t)) n++;
  word body = A(t);
  struct arib r = { p, p, n, n, env };
  return (mix * (uintptr_t) (n + 7)) ^ (shash(g, body, &r) * mix); }
 return (mix ^ (shash(g, A(x), env) * mix)) ^ (shash(g, B(x), env) * mix); }

// --- the beta bridge: a closure VALUE compares up to the capture-substitution
// ev already performed -- (adder 5) = (\ x (+ x 5)). done WITHOUT allocating: the
// base source is walked virtually, its leading binders split FILLED (resolve to
// the captured value) and REMAINING (post-substitution de Bruijn coordinates).
// sound by construction; a captured closure vs a source lambda stays unbridged
// (conservative, but nf_hash mirrors shash so =-equal closures always hash equal).
enum { nf_maxcap = 64 };                                  // cap the captured-arg count we bridge; deeper -> fall back
struct clonf { word body, rem, fsyms; int nr, fn; word fv[nf_maxcap]; };  // residual: body, remaining-binder list (nr), filled-binder list (fn) + values
// Load a closure value's capture-substitution residual. A partial-app over a sourced base, or a
// no-capture lambda (fn = 0). Returns false for a source-less base (a bif) or a quote -- caller falls back.
static bool clo_load(struct ai *c, word v, struct clonf *o) {
 if (!lamp(v) || datp(v) || !in_heap(c, v)) return false;
 union u *k = cell(v);
 word s; int na = 0;
 if (fn_partialp(k)) {
  union u *bk = fn_base(k, &na);
  if (na < 0 || na > nf_maxcap) return false;
  word base = (word) bk;
  s = fn_src(c, cell(base), base);
  for (int i = 0; i < na; i++) o->fv[i] = fn_arg(k, i, na);
 } else s = fn_src(c, k, v);
 if (!s || !lam_isp(c, s)) return false;                  // source-less base / quote: not bridged here
 word p = B(s);                                           // (b0 b1 .. body): binder list then body
 int nb = 0; word t = p;
 for (; chainp(B(t)); t = B(t)) nb++;
 if (na >= nb) return false;                              // captures consume the whole group (shouldn't for a partial-app): bail safe
 word rem = p;
 for (int i = 0; i < na; i++) rem = B(rem);               // remaining binders start past the filled ones
 o->body = A(t); o->rem = rem; o->nr = nb - na; o->fsyms = p; o->fn = na;
 return true; }
// α-invariant hash of a residual's body, mirroring shash: a genuine binder by
// coordinate, a FILLED binder by its captured value's hash, a free var by symbol
static uintptr_t nf_hash(struct ai *g, word x, struct arib *env, word fs, int fn, word *fv) {
 if (nomp(x)) {
  int d = 0;
  for (struct arib *r = env; r; r = r->up, d++) {
   int i = arib_pos(x, r->la, r->na);
   if (i >= 0) return rot((uintptr_t) (d * 131 + i + 1) * mix); }   // genuine binder
  int j = arib_pos(x, fs, fn);
  if (j >= 0) return hash(g, fv[j]);                      // filled binder: the captured value as a literal
  return hash(g, x); }                                    // free var
 if (!chainp(x)) return hash(g, x);
 if (ai_isbs(g, A(x))) {
  word p = B(x);
  if (!chainp(B(p))) return hash(g, x);                   // quote: data
  int n = 0; word t = p;
  for (; chainp(B(t)); t = B(t)) n++;
  word body = A(t);
  struct arib r = { p, p, n, n, env };
  return (mix * (uintptr_t) (n + 7)) ^ (nf_hash(g, body, &r, fs, fn, fv) * mix); }
 return (mix ^ (nf_hash(g, A(x), env, fs, fn, fv) * mix)) ^ (nf_hash(g, B(x), env, fs, fn, fv) * mix); }
static bool clo_nfhash(struct ai *g, word x, uintptr_t *out) {
 struct clonf o;
 if (!clo_load(ai_core_of(g), x, &o) || !o.fn) return false;   // o.fn == 0: a no-capture lambda, already hashed via shash upstream
 struct arib r = { o.rem, o.rem, o.nr, o.nr, 0 };
 *out = (mix * (uintptr_t) (o.nr + 7)) ^ (nf_hash(g, o.body, &r, o.fsyms, o.fn, o.fv) * mix);
 return true; }
// does runtime value V equal the meaning of source term b? filled binder ->
// compare captures; literal atom -> compare; anything else conservative false.
static bool val_vs_src(struct ai *g, word V, word b, struct arib *rb, struct clonf *cb, word *scratch) {
 if (nomp(b)) {
  for (struct arib *r = rb; r; r = r->up) if (arib_pos(b, r->la, r->na) >= 0) return false;  // a remaining param
  int j = arib_pos(b, cb->fsyms, cb->fn);
  return j >= 0 ? eqv_at(g, V, cb->fv[j], scratch) : false; }   // filled: both values | free: conservative false
 if (!chainp(b)) return eqv_at(g, V, b, scratch);               // literal atom (number / string)
 return false; }                                               // compound source (app / lambda): conservative
// α + value equality of two residual bodies in lockstep: a nom classifies BOUND
// (by coordinate), FILLED (a captured value), FREE (by symbol), or NOTNOM
static bool nf_walk(struct ai *g, word a, struct arib *ra, struct clonf *ca,
                                  word b, struct arib *rb, struct clonf *cb, word *scratch) {
 if (nomp(a) || nomp(b)) {
  int ka = 3; intptr_t ac = 0; word av = 0;              // 0 BOUND, 1 FILLED, 2 FREE, 3 NOTNOM
  if (nomp(a)) {
   int d = 0; ka = 2;
   for (struct arib *r = ra; r; r = r->up, d++) { int i = arib_pos(a, r->la, r->na); if (i >= 0) { ka = 0; ac = (intptr_t) d * 4096 + i; break; } }
   if (ka == 2) { int j = arib_pos(a, ca->fsyms, ca->fn); if (j >= 0) { ka = 1; av = ca->fv[j]; } } }
  int kb = 3; intptr_t bc = 0; word bv = 0;
  if (nomp(b)) {
   int d = 0; kb = 2;
   for (struct arib *r = rb; r; r = r->up, d++) { int i = arib_pos(b, r->la, r->na); if (i >= 0) { kb = 0; bc = (intptr_t) d * 4096 + i; break; } }
   if (kb == 2) { int j = arib_pos(b, cb->fsyms, cb->fn); if (j >= 0) { kb = 1; bv = cb->fv[j]; } } }
  if (ka == 0 || kb == 0) return ka == 0 && kb == 0 && ac == bc;   // a bound var matches only the same-coordinate bound var
  if (ka == 1 && kb == 1) return eqv_at(g, av, bv, scratch);       // two captured values
  if (ka == 1) return val_vs_src(g, av, b, rb, cb, scratch);
  if (kb == 1) return val_vs_src(g, bv, a, ra, ca, scratch);
  if (ka == 2 && kb == 2) return a == b;                           // two free vars
  return false; }                                                  // FREE vs NOTNOM
 if (!chainp(a) || !chainp(b)) return eqv_at(g, a, b, scratch);
 if (ai_isbs(g, A(a)) && ai_isbs(g, A(b))) {
  word pa = B(a), pb = B(b);
  if (!chainp(B(pa)) || !chainp(B(pb))) return eqv_at(g, a, b, scratch);   // quote: data
  int na = 0, nb = 0; word t = pa;
  for (; chainp(B(t)); t = B(t)) na++;
  word ba = A(t);
  for (t = pb; chainp(B(t)); t = B(t)) nb++;
  word bb = A(t);
  if (na != nb) return false;
  struct arib rA = { pa, pa, na, na, ra }, rB = { pb, pb, nb, nb, rb };
  return nf_walk(g, ba, &rA, ca, bb, &rB, cb, scratch); }
 return nf_walk(g, A(a), ra, ca, A(b), rb, cb, scratch) && nf_walk(g, B(a), ra, ca, B(b), rb, cb, scratch); }
static bool clo_eq(struct ai *g, struct clonf *ca, struct clonf *cb, word *scratch) {  // residual α+value equality
 if (ca->nr != cb->nr) return false;                                   // different residual arity
 struct arib rA = { ca->rem, ca->rem, ca->nr, ca->nr, 0 }, rB = { cb->rem, cb->rem, cb->nr, cb->nr, 0 };
 return nf_walk(g, ca->body, &rA, ca, cb->body, &rB, cb, scratch); }
// `base` is where this frame's worklist starts: the public eqv passes off_pool; a
// re-entrant beta-bridge call passes the caller's live top, so nested scratch sits
// ABOVE the pending pairs instead of clobbering them.
static bool eqv_at(struct ai *g, word a, word b, word *base) {
 word *top = off_pool(g) + g->len, *w = base;
 struct ai *c = ai_core_of(g);
 for (;;) {
  if (a != b) {
   // coins: equal iff same die and eqv payloads
   if (coinp(a) || coinp(b)) {
    if (coinp(a) && coinp(b) && coin_die(a) == coin_die(b)) {
     a = coin_load(a), b = coin_load(b); continue; }
    return false; }
   // function values: equality up to the beta the runtime already ran (the
   // bridge). a source-less base (a bif partial like (+ 1)) can't residualize:
   // fall back to base + captures pairwise. maps/ports/mixed fall to identity.
   if (lamp(a) && lamp(b) && !datp(a) && !datp(b)) {
    union u *ka = cell(a), *kb = cell(b);
    bool pa = fn_partialp(ka), pb = fn_partialp(kb);
    if (!pa && !pb) {                                      // common case: two no-capture lambdas -> α-compare sources
     word sa = fn_src(c, ka, a), sb = fn_src(c, kb, b);
     if (sa && sb) { if (!salpha(g, sa, sb, 0)) return false; a = b; continue; }
     return false; }                                      // a source-less function value -> identity (already failed)
    struct clonf ra_, rb_;                                // a partial-app is in play: bridge via the capture-substitution residual
    if (clo_load(c, a, &ra_) && clo_load(c, b, &rb_)) {
     if (!clo_eq(g, &ra_, &rb_, w)) return false;         // w = the live worklist top: the bridge's re-entrant eqv scratches above it
     a = b; continue; }                                   // residuals equal -> drain worklist
    if (pa && pb) {                                        // source-less base (a bif): compare base + captures pairwise
     int na, nb; union u *ba = fn_base(ka, &na), *bb = fn_base(kb, &nb);
     if (na != nb) return false;
     if (top - w < 2 * (na + 1)) __builtin_trap();        // worklist overflow / cycle
     for (int i = 0; i < na; i++) *w++ = fn_arg(ka, i, na), *w++ = fn_arg(kb, i, nb);
     a = (word) ba, b = (word) bb; continue; }
    return false; }
   // a number never equals a closure: bridging 0/1 to their church lambdas would
   // break congruence, the order, and tower transitivity
   if (((a | b) & 1) || !datp(a) || !datp(b) || typ(a) != typ(b)) return false;
   switch (typ(a)) {
    default: return false;
    case DChain:
     if (top - w < 2) __builtin_trap();     // worklist overflow: a cycle
     *w++ = B(a), *w++ = B(b), a = A(a), b = A(b);
     continue;
    case DTray: {
     size_t la = ai_tray_bytes(tray(a)), lb = ai_tray_bytes(tray(b));
     if (la != lb || memcmp(tray(a), tray(b), la)) return false;
     break; }
    case DGem:
     if (gem_get(a) != gem_get(b)) return false;       // two float boxes: compare the payload (parallels = / cmp)
     break;
    case DSun:
     if (sun_get(a) != sun_get(b)) return false;       // two suns: compare the payload
     break;
    case DTwin:
     if (twin_re(a) != twin_re(b) || twin_im(a) != twin_im(b)) return false;  // re AND im
     break;
    case DBig: {
     struct ai_big *x = big(a), *y = big(b);
     if (x->slen != y->slen) return false;
     size_t nb = (size_t) (x->slen < 0 ? -x->slen : x->slen) * sizeof(ai_limb);
     if (memcmp(x->limb, y->limb, nb)) return false;
     break; }
    case DString:
     if (len(a) != len(b) || memcmp(txt(a), txt(b), len(a))) return false;
     break; } }
  if (w == base) return true;              // worklist drained: all equal
  b = *--w, a = *--w; } }
ai_noinline bool eqv(struct ai *g, word a, word b) { return eqv_at(g, a, b, off_pool(g)); }

// whole-array `=`: a boolean like every other kind (shapes match, every cell
// equal), NOT the elementwise mask -- `<` and `>` are the mask makers. cells
// compare ACROSS TIERS (a z-tray equals a gem-tray of the same values); object
// cells go through eqv; an object tray never equals a numeric one.
static ai_noinline bool tray_eq(struct ai *g, word a, word b) {
 if (!trayp(a) || !trayp(b)) return false;            // an array is never a scalar
 struct ai_tray *va = tray(a), *vb = tray(b);
 if (va->rank != vb->rank) return false;
 for (uintptr_t k = 0; k < va->rank; k++)
  if (va->shape[k] != vb->shape[k]) return false;   // same shape, not merely conformant
 uintptr_t n = tray_nelem(va);
 bool oa = va->type == ai_O, ob = vb->type == ai_O;
 if (oa || ob) {
  if (oa != ob) return false;
  for (uintptr_t i = 0; i < n; i++)
   if (!eqv(g, tray_get_obj(va, i), tray_get_obj(vb, i))) return false;
  return true; }
 if (va->type == ai_C || vb->type == ai_C) {        // (re,im) per cell; a real reads as (r,0)
  ai_flo_t const *pa = tray_data(va), *pb = tray_data(vb);
  for (uintptr_t i = 0; i < n; i++) {
   ai_flo_t are = va->type == ai_C ? pa[2*i] : tray_get_flo(va, i);
   ai_flo_t aim = va->type == ai_C ? pa[2*i+1] : 0;
   ai_flo_t bre = vb->type == ai_C ? pb[2*i] : tray_get_flo(vb, i);
   ai_flo_t bim = vb->type == ai_C ? pb[2*i+1] : 0;
   if (are != bre || aim != bim) return false; }
  return true; }
 if (va->type == ai_Z && vb->type == ai_Z) {        // exact: no double round-trip
  for (uintptr_t i = 0; i < n; i++)
   if (tray_get_int(va, i) != tray_get_int(vb, i)) return false;
  return true; }
 for (uintptr_t i = 0; i < n; i++)                  // a float on either side: as doubles
  if (tray_get_flo(va, i) != tray_get_flo(vb, i)) return false;
 return true; }

// (= a b): value-equality with numeric promotion across the tower; falls through
// to eql for non-numeric operands. strictly looser than eqv, which still rejects
// mixed-type chains (table keys 3 and 3.0 stay distinct).
lvm(lvm_eq) {
 word a = Sp[0], b = Sp[1];
 // the common case: identity settles two charms, and a point against ANYTHING
 // (a point equals only itself). both skip the dispatch below and fuse a
 // following `?` directly (then -> Ip+3, else -> Ip[2].m).
 if (__builtin_expect((charmp(a) && charmp(b)) || nomp(a) || nomp(b), 1)) {
  bool r = a == b;
  if (Ip[1].ap == lvm_cond) { Sp += 2; Ip = r ? Ip + 3 : Ip[2].m; ai_musttail return Continue(); }
  ai_musttail return Answerp(1, r ? putcharm(1) : zero); }
 if (trayp(a) || trayp(b)) {   // whole-array equality -> a boolean; the mask lives on < and >
  bool r = tray_eq(g, a, b);
  Sp[1] = r ? putcharm(1) : zero;
  ai_musttail return Nextp(1, 1); }
 // complex: equal iff re AND im match, a real reading as (r, 0); before the
 // float lane so a complex never reaches toflo
 if (twinp(a) || twinp(b)) {
  bool r = (twinp(a) || isnum(a)) && (twinp(b) || isnum(b))
        && (twinp(a) ? twin_re(a) : toflo(a)) == (twinp(b) ? twin_re(b) : toflo(b))
        && (twinp(a) ? twin_im(a) : 0) == (twinp(b) ? twin_im(b) : 0);
  Sp[1] = r ? putcharm(1) : zero;
  ai_musttail return Nextp(1, 1); }
 bool r;
 // a float operand compares as doubles across the whole tower (a bignum loses
 // precision past 2^53, the documented caveat); otherwise eql
 if (gemp(a) || gemp(b)) r = isnum(a) && isnum(b) && (toflo(a) == toflo(b));
 else r = eql(g, a, b);
 Sp[1] = r ? putcharm(1) : zero;
 ai_musttail return Nextp(1, 1); }

// (id? a b): pointer/word identity, no structural recursion
lvm(lvm_same) {
 Sp[1] = Sp[0] == Sp[1] ? putcharm(1) : zero;
 ai_musttail return Nextp(1, 1); }

// ============================================================================
// big
// ============================================================================
// bignums close the tower fixnum -> sun box -> bignum. all multi-limb work
// lives in ai_noinline magnitude helpers over raw limb arrays (no allocation),
// so the VM entries keep their tail calls and the GC never sees a half-built
// object. products/divides stay hardware (mulq/divq via div2by1) -- never
// __multi3/__udivti3, which the -nostdlib freestanding port cannot supply.


// |slen| of a heap bignum.
static ai_inline int big_nlimbs(word x) {
 intptr_t s = big(x)->slen;
 return (int) (s < 0 ? -s : s); }

uintptr_t ai_big_bytes(struct ai_big *b) {
 intptr_t n = b->slen < 0 ? -b->slen : b->slen;
 return sizeof(struct ai_big) + (uintptr_t) n * sizeof(ai_limb); }

// --- raw magnitude primitives (little-endian limb arrays); callers pass
// normalized inputs and normalize outputs via ai_big_canon

static int mag_copy(ai_limb *dst, ai_limb const *src, int n) {
 for (int i = 0; i < n; i++) dst[i] = src[i];
 return n; }

// Compare magnitudes: -1 if a<b, 0 if equal, 1 if a>b.
static ai_noinline int mag_cmp(ai_limb const *a, int na, ai_limb const *b, int nb) {
 while (na > 0 && a[na-1] == 0) na--;
 while (nb > 0 && b[nb-1] == 0) nb--;
 if (na != nb) return na < nb ? -1 : 1;
 for (int i = na - 1; i >= 0; i--) if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
 return 0; }

// r = a + b. r distinct from a,b; capacity >= max(na,nb)+1. Returns limb count.
static ai_noinline int mag_add(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb) {
 if (na < nb) { ai_limb const *t = a; a = b; b = t; int u = na; na = nb; nb = u; }
 ai_dlimb c = 0; int i = 0;
 for (; i < nb; i++) { ai_dlimb s = (ai_dlimb) a[i] + b[i] + c; r[i] = (ai_limb) s; c = s >> limb_bits; }
 for (; i < na; i++) { ai_dlimb s = (ai_dlimb) a[i] + c;        r[i] = (ai_limb) s; c = s >> limb_bits; }
 if (c) r[i++] = (ai_limb) c;
 return i; }

// r = a - b, requires a >= b (magnitudes). r distinct from a,b. Returns na
// (caller normalizes away any high zero limbs the subtraction produced).
static ai_noinline int mag_sub(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb) {
 ai_sdlimb borrow = 0; int i = 0;
 for (; i < nb; i++) {
  ai_sdlimb d = (ai_sdlimb) a[i] - b[i] - borrow;
  if (d < 0) d += (ai_sdlimb) limb_base, borrow = 1; else borrow = 0;
  r[i] = (ai_limb) d; }
 for (; i < na; i++) {
  ai_sdlimb d = (ai_sdlimb) a[i] - borrow;
  if (d < 0) d += (ai_sdlimb) limb_base, borrow = 1; else borrow = 0;
  r[i] = (ai_limb) d; }
 return na; }

// r = a * b (schoolbook). r must be distinct from a,b; capacity >= na+nb. Used
// one-shot by ai_big_binop (the object-array elementwise lane); the scalar `*`
// path instead drives a chunked, yieldable copy of this loop in lvm_bmul.
static ai_noinline void mag_mul(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb) {
 for (int i = 0; i < na + nb; i++) r[i] = 0;
 for (int i = 0; i < na; i++) {
  ai_dlimb carry = 0; ai_limb ai = a[i];            // ai stays a limb so ai*b[j] is the hardware 64x64->128 (not a 128x128 __multi3)
  for (int j = 0; j < nb; j++) {
   ai_dlimb s = (ai_dlimb) ai * b[j] + r[i+j] + carry;
   r[i+j] = (ai_limb) s; carry = s >> limb_bits; }
  r[i+nb] = (ai_limb) carry; } }

// a = a*mul + add, in place (mul,add < 2^limb_bits). a capacity must allow one
// carry limb at a[n]. Returns the new limb count. Used by the decimal reader.
static ai_noinline int mag_mul_add_small(ai_limb *a, int n, ai_limb mul, ai_limb add) {
 ai_dlimb c = add;
 for (int i = 0; i < n; i++) { ai_dlimb s = (ai_dlimb) a[i] * mul + c; a[i] = (ai_limb) s; c = s >> limb_bits; }
 if (c) a[n++] = (ai_limb) c;
 return n; }

// 128/64 -> quotient + remainder, caller guarantees the quotient fits a limb
// (hi < d): the hardware divq on x86-64, never __udivti3
static ai_inline ai_limb div2by1(ai_limb hi, ai_limb lo, ai_limb d, ai_limb *rem) {
#if defined(__x86_64__) && limb_bits == 64 && defined(__GNUC__)
 // gcc/clang take the one-divq asm; mooncc compiles the C face below natively
 // (its u128/u64 divide IS the same two-step divq dance, emitted whole)
 __asm__("divq %2" : "+a"(lo), "+d"(hi) : "r"(d));
 return *rem = hi, lo;
#else
 ai_dlimb num = ((ai_dlimb) hi << limb_bits) | lo;
 return *rem = (ai_limb) (num % d), (ai_limb) (num / d);
#endif
}
// 128/64 -> FULL quotient + remainder for the q-hat step, as two divq-safe steps
static ai_inline ai_dlimb div128by64(ai_limb hi, ai_limb lo, ai_limb d, ai_limb *rem) {
 ai_limb qhi = hi / d, r1 = hi % d;
 ai_limb qlo = div2by1(r1, lo, d, rem);
 return ((ai_dlimb) qhi << limb_bits) | qlo; }

// Knuth Algorithm D long division (Hacker's Delight divmnu): u (m limbs) / v (n
// limbs, m >= n) -> q (m-n+1 limbs), r (n limbs); un/vn are normalization scratch.
static ai_noinline void mag_divmod(ai_limb *q, ai_limb *r,
  ai_limb const *u, int m, ai_limb const *v, int n, ai_limb *un, ai_limb *vn) {
 ai_dlimb const B = limb_base;
 if (n == 1) {                                  // single-limb divisor: simple
  ai_limb rem = 0;
  for (int j = m - 1; j >= 0; j--) { ai_limb rr; q[j] = div2by1(rem, u[j], v[0], &rr); rem = rr; }
  r[0] = rem; return; }
 int s = limb_clz(v[n-1]);                       // normalize so v[n-1] has its top bit set
 for (int i = n - 1; i > 0; i--) vn[i] = (v[i] << s) | (s ? (ai_dlimb) v[i-1] >> (limb_bits - s) : 0);
 vn[0] = v[0] << s;
 un[m] = s ? (ai_dlimb) u[m-1] >> (limb_bits - s) : 0;
 for (int i = m - 1; i > 0; i--) un[i] = (u[i] << s) | (s ? (ai_dlimb) u[i-1] >> (limb_bits - s) : 0);
 un[0] = u[0] << s;
 for (int j = m - n; j >= 0; j--) {
  ai_limb rr;                                   // 128/64 q-hat: divq-safe two-step, no __udivti3
  ai_dlimb qhat = div128by64(un[j+n], un[j+n-1], vn[n-1], &rr), rhat = rr;
  while (qhat >= B || qhat * vn[n-2] > ((rhat << limb_bits) | un[j+n-2])) {
   qhat--; rhat += vn[n-1];
   if (rhat >= B) break; }
  ai_sdlimb borrow = 0;                          // multiply and subtract qhat*v
  for (int i = 0; i < n; i++) {
   ai_dlimb p = (ai_dlimb) (ai_limb) qhat * vn[i];   // qhat < B here: a limb, so 64x64->128 not 128x128
   ai_sdlimb sub = (ai_sdlimb) un[i+j] - borrow - (ai_sdlimb) (ai_limb) p;
   un[i+j] = (ai_limb) sub;
   borrow = (ai_sdlimb) (p >> limb_bits) - (sub >> limb_bits); }
  ai_sdlimb sub = (ai_sdlimb) un[j+n] - borrow;
  un[j+n] = (ai_limb) sub;
  q[j] = (ai_limb) qhat;
  if (sub < 0) {                                // qhat was one too big: add back
   q[j]--;
   ai_dlimb carry = 0;
   for (int i = 0; i < n; i++) { ai_dlimb t = (ai_dlimb) un[i+j] + vn[i] + carry; un[i+j] = (ai_limb) t; carry = t >> limb_bits; }
   un[j+n] = (ai_limb) (un[j+n] + carry); } }
 for (int i = 0; i < n; i++) r[i] = s ? (un[i] >> s) | ((ai_dlimb) un[i+1] << (limb_bits - s)) : un[i]; }

// --- operand loading + tier conversions -------------------------------------

// Load integer operand x (fixnum / sun box / bignum -- never a float) as a
// magnitude. A fixnum/box fills `scratch` (wlimbs limbs: 1 with native-width
// limbs, 2 with 32-bit limbs on a 64-bit word) and points *out at it; a bignum
// points *out into its heap limbs (stable only while no GC runs). Sets *neg and
// returns the limb count (0 for the value zero). wlimbs = limbs to hold one word.
static int load_int_mag(word x, ai_limb scratch[wlimbs], ai_limb const **out, bool *neg) {
 if (bigp(x)) { struct ai_big *b = big(x); intptr_t s = b->slen;
  *neg = s < 0, *out = b->limb; return (int) (s < 0 ? -s : s); }
 intptr_t v = charmp(x) ? (intptr_t) getcharm(x) : sun_get(x);
 *neg = v < 0;
 uintptr_t u = *neg ? (uintptr_t) 0 - (uintptr_t) v : (uintptr_t) v;
 int k = 0;
 for (int i = 0; i < wlimbs; i++) { scratch[i] = (ai_limb) (u >> (limb_bits * i)); if (scratch[i]) k = i + 1; }
 *out = scratch;
 return k; }

ai_flo_t ai_big_to_flo(word x) {
 struct ai_big *b = big(x);
 intptr_t sl = b->slen;
 bool neg = sl < 0;
 int n = (int) (neg ? -sl : sl);
 double r = 0;
 for (int i = n - 1; i >= 0; i--) r = r * (double) limb_base + (double) b->limb[i];
 return (ai_flo_t) (neg ? -r : r); }

// The bignum's two's-complement value mod 2^W (its low machine word). Used when
// an integer-array elementwise op must broadcast a bignum scalar down to one
// machine-int element ("arrays win; demote the bignum by its low bits").
intptr_t ai_big_low(word x) {
 struct ai_big *b = big(x);
 intptr_t sl = b->slen;
 bool neg = sl < 0;
 int n = (int) (neg ? -sl : sl);
 uintptr_t u = 0;
 for (int i = 0; i < n && i < wlimbs; i++) u |= (uintptr_t) b->limb[i] << (limb_bits * i);
 return (intptr_t) (neg ? (uintptr_t) 0 - u : u); }

int ai_big_cmp(word a, word b) {
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool na, nb;
 int nla = load_int_mag(a, sa, &la, &na), nlb = load_int_mag(b, sb, &lb, &nb);
 bool aneg = na && nla > 0, bneg = nb && nlb > 0;   // zero is non-negative
 if (aneg != bneg) return aneg ? -1 : 1;
 int c = mag_cmp(la, nla, lb, nlb);
 return aneg ? -c : c; }

// demote a magnitude to the smallest tier: fixnum, sun box, bignum -- the single
// sink that keeps the tiers disjoint, so eqv / table keys stay well defined
word ai_big_canon(ai_word **hp, ai_limb const *limb, int n, bool neg) {
 while (n > 0 && limb[n-1] == 0) n--;
 if (n == 0) return zero;
 if (n <= wlimbs) {
  uintptr_t u = 0;
  for (int i = 0; i < n; i++) u |= (uintptr_t) limb[i] << (limb_bits * i);   // combine limbs into a word
  uintptr_t const fixmag = (uintptr_t) 1 << (Bits - 2);   // |mincharm|  = 2^(W-2)
  uintptr_t const boxmag = (uintptr_t) 1 << (Bits - 1);   // |INT_MIN|  = 2^(W-1)
  intptr_t val;
  if (!neg) {
   if (u <= fixmag - 1) return putcharm((intptr_t) u);       // maxcharm = 2^(W-2)-1
   if (u > boxmag - 1) goto big;                            // > INTPTR_MAX -> bignum
   val = (intptr_t) u; }
  else {
   if (u <= fixmag) return putcharm((intptr_t) ((uintptr_t) 0 - u));   // incl mincharm
   if (u > boxmag) goto big;                                          // < INTPTR_MIN -> bignum
   val = (intptr_t) ((uintptr_t) 0 - u); }                            // incl INTPTR_MIN
  return mk_sun(hp, val); }
big: ;                                   // C11 wants a statement before a declaration
 struct ai_big *b = ini_big(big(*hp), neg ? -n : n);
 for (int i = 0; i < n; i++) b->limb[i] = limb[i];
 *hp += b2w(sizeof(struct ai_big) + (size_t) n * sizeof(ai_limb));
 return word(b); }

// --- arithmetic (sign-magnitude over the loaded operands) -------------------

// r = a +/- b (subtract flips b's sign), result magnitude + sign.
static void big_addsub(ai_limb *r, int *rn, bool *rneg,
  ai_limb const *a, int na, bool nega, ai_limb const *b, int nb, bool negb, bool subtract) {
 bool sb = subtract ? !negb : negb;             // effective sign of the b operand
 if (nega == sb) { *rn = mag_add(r, a, na, b, nb); *rneg = nega; }
 else { int c = mag_cmp(a, na, b, nb);
  if (c == 0) { *rn = 0; *rneg = false; }
  else if (c > 0) { *rn = mag_sub(r, a, na, b, nb); *rneg = nega; }
  else { *rn = mag_sub(r, b, nb, a, na); *rneg = sb; } } }

// Add magnitude s (sn limbs) into r at limb offset off, carrying up. r is sized
// for the full result, so the carry settles within it.
static void mag_add_off(ai_limb *r, int rn, ai_limb const *s, int sn, int off) {
 ai_dlimb c = 0; int i = 0;
 for (; i < sn; i++)            { ai_dlimb t = (ai_dlimb) r[off+i] + s[i] + c; r[off+i] = (ai_limb) t; c = t >> limb_bits; }
 for (; c && off + i < rn; i++) { ai_dlimb t = (ai_dlimb) r[off+i] + c;        r[off+i] = (ai_limb) t; c = t >> limb_bits; } }

// karatsuba for EQUAL-length operands: three half-size products in place of one
// full one; below kara_cutoff schoolbook's lower constant wins. t is scratch.
#define kara_cutoff 40   // limbs/operand above which Karatsuba beats schoolbook (measured crossover)
static void mag_mul_kara(ai_limb *r, ai_limb const *a, ai_limb const *b, int n, ai_limb *t) {
 if (n < kara_cutoff) { mag_mul(r, a, n, b, n); return; }
 int m = n / 2, h = n - m;                           // low m limbs, high h (m or m+1) limbs
 mag_mul_kara(r,       a,     b,     m, t);           // z0 -> r[0..2m)
 mag_mul_kara(r + 2*m, a + m, b + m, h, t);           // z2 -> r[2m..2n)
 int z0n = 2*m;  while (z0n > 0 && r[z0n-1] == 0) z0n--;
 int z2n = 2*h;  while (z2n > 0 && r[2*m + z2n-1] == 0) z2n--;
 ai_limb *sa = t, *sb = sa + (h+1), *z1 = sb + (h+1);
 int nsa = mag_add(sa, a, m, a + m, h), nsb = mag_add(sb, b, m, b + m, h);
 mag_mul(z1, sa, nsa, sb, nsb);                       // z1 = (a0+a1)(b0+b1) on the half-size sums
 int nz1 = nsa + nsb;                       while (nz1 > 0 && z1[nz1-1] == 0) nz1--;
 nz1 = mag_sub(z1, z1, nz1, r,       z0n);  while (nz1 > 0 && z1[nz1-1] == 0) nz1--;   // z1 -= z0
 nz1 = mag_sub(z1, z1, nz1, r + 2*m, z2n);  while (nz1 > 0 && z1[nz1-1] == 0) nz1--;   // z1 -= z2
 mag_add_off(r, 2*n, z1, nz1, m); }                  // r += z1 * B^m

static int big_mul_mag(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb, ai_limb *t) {
 if (na == nb) mag_mul_kara(r, a, b, na, t); else mag_mul(r, a, na, b, nb);
 int n = na + nb; while (n > 0 && r[n-1] == 0) n--;
 return n; }

// the packed multi-precision lane for + - * / % (zero divisor screened by the
// caller): computes a (vop) b, leaves the result at g->sp[1], pops one, advances
// ip -- so the caller is just Pack; binop; Unpack; Continue.
struct ai *ai_big_binop(struct ai *g, int vop) {
 word a = g->sp[0], b = g->sp[1];
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2;
 int bound = na + nb + 2;                        // result magnitude upper bound
 int work = 4 * (na + nb) + 16;                  // divmod scratch upper bound
 uintptr_t res_area = Width(struct ai_big) + b2w((size_t) bound * sizeof(ai_limb)),
           ws_words = b2w((size_t) (bound + work) * sizeof(ai_limb));
 if (!ai_ok(g = ai_have(g, res_area + ws_words))) return g;
 a = g->sp[0], b = g->sp[1];                     // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 int nla = load_int_mag(a, sa, &la, &nega), nlb = load_int_mag(b, sb, &lb, &negb);
 ai_limb *rmag = (ai_limb*) (g->hp + res_area), *scr = rmag + bound;
 int rn = 0; bool rneg = false;
 switch (vop) {
  case vop_add: big_addsub(rmag, &rn, &rneg, la, nla, nega, lb, nlb, negb, false); break;
  case vop_sub: big_addsub(rmag, &rn, &rneg, la, nla, nega, lb, nlb, negb, true); break;
  case vop_mul: rn = big_mul_mag(rmag, la, nla, lb, nlb, scr); rneg = nega != negb; break;
  default: {                                     // vop_quot / vop_rem (truncated)
   int c = mag_cmp(la, nla, lb, nlb);
   if (c < 0) {                                  // |a| < |b|: q = 0, r = a
    if (vop == vop_rem) rn = mag_copy(rmag, la, nla), rneg = nega; }
   else {
    ai_limb *q = scr, *rem = q + (nla - nlb + 1), *un = rem + nlb, *vn = un + (nla + 1);
    mag_divmod(q, rem, la, nla, lb, nlb, un, vn);
    if (vop != vop_rem) {                          // vop_quot / vop_fquot: truncated quotient
     int qn = nla - nlb + 1; while (qn > 0 && q[qn-1] == 0) qn--;
     rn = mag_copy(rmag, q, qn), rneg = nega != negb; }
    else {
     int rr = nlb; while (rr > 0 && rem[rr-1] == 0) rr--;
     rn = mag_copy(rmag, rem, rr), rneg = nega; } } } }
 g->sp[1] = ai_big_canon(&g->hp, rmag, rn, rneg);
 g->sp++;
 g->ip = (union u*) g->ip + 1;
 return g; }

// the integer rungs' EXACT LANE (int / ceil / saturate) for a ratio coin: above
// 2^53 the float net rounds, so a rung riding it lands on the wrong integer.
// domain: a net-mode-2 coin over (n d), both exact integers, d nonzero (a zero
// divisor keeps the float lane's inf/sign story).
bool ai_ratio_exact(struct ai *g, word x) {
 if (!coinp(x) || die_get(g, coin_die(x), DieNet) != putcharm(2)) return false;
 word p = coin_load(x);
 if (!chainp(p) || !chainp(B(p))) return false;
 word n = A(p), d = A(B(p));
 if (!(charmp(n) || sunp(n) || bigp(n)) || !(charmp(d) || sunp(d) || bigp(d))) return false;
 return charmp(d) ? d != putcharm(0) : sunp(d) ? sun_get(d) != 0 : true; }
// ..the lane: trunc(n/d) by long division, clamped to the charm bounds like every
// rung (the codomain law), then the rung's own adjustment -- ceil rounds a dropped
// remainder up, saturate is ceil with its floor raised to 0. the operand rides
// g->sp[0] across ai_have's GC edge; scratch sits above hp and is never committed.
struct ai *ai_ratio_rung(struct ai *g, int rung) {
 word x = g->sp[0], p = coin_load(x), a = A(p), b = A(B(p));
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2;
 if (!ai_ok(g = ai_have(g, b2w((size_t) (4 * (na + nb) + 16) * sizeof(ai_limb))))) return g;
 x = g->sp[0], p = coin_load(x), a = A(p), b = A(B(p));       // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 int nla = load_int_mag(a, sa, &la, &nega), nlb = load_int_mag(b, sb, &lb, &negb);
 bool rneg = nega != negb, rnz = false, sat = false;
 uintptr_t uq = 0;
 if (nla == 0) ;                                              // 0/d: q 0, r 0
 else if (mag_cmp(la, nla, lb, nlb) < 0) rnz = true;          // |a| < |b|: q 0, r a
 else {
  ai_limb *q = (ai_limb*) g->hp, *rem = q + (nla - nlb + 1), *un = rem + nlb, *vn = un + (nla + 1);
  mag_divmod(q, rem, la, nla, lb, nlb, un, vn);
  int qn = nla - nlb + 1; while (qn > 0 && q[qn-1] == 0) qn--;
  for (int i = 0; i < nlb; i++) if (rem[i]) { rnz = true; break; }
  if (qn > wlimbs) sat = true;
  else { for (int i = 0; i < qn; i++) uq |= (uintptr_t) q[i] << (limb_bits * i);
         if (uq > (uintptr_t) maxcharm + (rneg ? 1 : 0)) sat = true; } }
 intptr_t t = sat ? (rneg ? mincharm : maxcharm)
                  : rneg ? -(intptr_t) uq : (intptr_t) uq;
 if (rung >= 1 && !sat && rnz && !rneg && t < maxcharm) t++;  // ceil: a dropped remainder rounds up
 if (rung == 2 && t < 0) t = 0;                               // saturate: the floor rises to 0
 g->sp[0] = putcharm(t);
 g->ip = (union u*) g->ip + 1;
 return g; }

// `/` over the bignum lane: like ai_big_binop's truncated quotient, but the result
// stays an exact integer ONLY when b divides a; a nonzero remainder promotes to a
// float box of a/b (the bignum analogue of the scalar `/` int promotion). Operands
// at g->sp[0..1] are integers; a zero divisor is screened off by the caller.
struct ai *ai_big_quot_true(struct ai *g) {
 word a = g->sp[0], b = g->sp[1];
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2;
 int bound = na + nb + 2, work = 4 * (na + nb) + 16;
 uintptr_t res_area = Width(struct ai_big) + b2w((size_t) bound * sizeof(ai_limb)),
           ws_words = b2w((size_t) (bound + work) * sizeof(ai_limb));
 if (!ai_ok(g = ai_have(g, res_area + ws_words + box_req))) return g;
 a = g->sp[0], b = g->sp[1];                     // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 int nla = load_int_mag(a, sa, &la, &nega), nlb = load_int_mag(b, sb, &lb, &negb);
 ai_limb *rmag = (ai_limb*) (g->hp + res_area), *scr = rmag + bound;
 int rn = 0; bool rneg = false, exact;
 int c = mag_cmp(la, nla, lb, nlb);
 if (c < 0) exact = (nla == 0);                  // |a| < |b|: q = 0, exact iff a == 0
 else {
  ai_limb *q = scr, *rem = q + (nla - nlb + 1), *un = rem + nlb, *vn = un + (nla + 1);
  mag_divmod(q, rem, la, nla, lb, nlb, un, vn);
  int rr = nlb; while (rr > 0 && rem[rr-1] == 0) rr--;
  exact = (rr == 0);
  int qn = nla - nlb + 1; while (qn > 0 && q[qn-1] == 0) qn--;
  rn = mag_copy(rmag, q, qn), rneg = nega != negb; }
 if (exact) g->sp[1] = ai_big_canon(&g->hp, rmag, rn, rneg);
 else g->sp[1] = mk_gem(&g->hp, toflo(a) / toflo(b));  // a,b still valid: no GC since the re-fetch, and toflo is alloc-free
 g->sp++;
 g->ip = (union u*) g->ip + 1;
 return g; }

// --- resumable (yieldable) multiply ---
// schoolbook run as one C call never yields, so a huge product would block every
// peer task. drive it as a self-looping VM instruction instead: the partial
// product lives in a cask, each dispatch folds ~bmul_chunk limb-mults, and the
// work state rides the l stack [i, r, ret_ip, a, b]. operands stay heap bignums
// so the loop reads stable limb pointers (no &scratch -- the sibcall law).
#define bmul_chunk (1 << 14)
static union u const bmul_loop[1] = { { .ap = lvm_bmul } };

// materialize integer x as a heap ai_big (a bignum returns in place)
static union u *as_big(ai_word **hp, word x) {
 if (bigp(x)) return cell(x);
 intptr_t v = toint(x);
 bool neg = v < 0;
 uintptr_t u = neg ? (uintptr_t) 0 - (uintptr_t) v : (uintptr_t) v;
 ai_limb tmp[wlimbs]; int n = 0;                                  // a machine word is wlimbs limbs
 for (int i = 0; i < wlimbs; i++) { tmp[i] = (ai_limb) (u >> (limb_bits * i)); if (tmp[i]) n = i + 1; }
 struct ai_big *b = ini_big(big(*hp), neg ? -n : n);
 for (int i = 0; i < n; i++) b->limb[i] = tmp[i];
 *hp += b2w(sizeof(struct ai_big) + (size_t) n * sizeof(ai_limb));
 return cell((word) b); }

// promote both operands, allocate the zeroed result cask, lay out the work frame;
// one ai_have so no half-built state is ever seen
static struct ai *ai_bmul_setup(struct ai *g) {
 word a = g->sp[0], b = g->sp[1];
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2;
 uintptr_t rbytes = (uintptr_t) (na + nb) * sizeof(ai_limb),
           sreq = str_type_width + b2w(rbytes),
           breq = Width(struct ai_cask) + Width(struct ai_tag),
           bigmax = Width(struct ai_big) + b2w((size_t) wlimbs * sizeof(ai_limb));
 if (!ai_ok(g = ai_have(g, 2 * bigmax + sreq + breq + 3))) return g;
 a = g->sp[0], b = g->sp[1];                       // re-fetch (ai_have may have GC'd)
 union u *abig = as_big(&g->hp, a), *bbig = as_big(&g->hp, b), *ret = g->ip + 1;
 struct ai_str *s = ini_str(str(g->hp), rbytes);
 g->hp += sreq; memset(txt(s), 0, rbytes);
 union u *k = (union u*) g->hp; g->hp += breq;
 cask(k)->ap = lvm_cask;
 cask(k)->str = s;
 tagthread(k, Width(struct ai_cask));
 g->sp -= 3;                                       // [i, r, ret_ip, abig, bbig]
 g->sp[0] = putcharm(0), g->sp[1] = word(k), g->sp[2] = word(ret);
 g->sp[3] = word(abig), g->sp[4] = word(bbig);
 g->ip = (union u*) bmul_loop;
 return g; }

// --- resumable Karatsuba multiply: a TRUE recursive karatsuba (O(n^1.585)) as a
// yieldable VM instruction. the whole computation lives in ONE pinned cask
// [hdr | job stack | A(n) | B(n) | R(2n) | scratch], re-read by offset each
// dispatch. a JOB either SPLITS (push the three half-size children, LIFO) or
// COMBINES (z1 -= z0; z1 -= z2; r += z1<<m). only na==nb routes here.
#define kmul_chunk (1 << 14)   // leaf limb-mults folded per dispatch before a yield check
#define KmulHdr 8             // ws header limbs: [0]=n [1]=top (stack ptr) [2]=sign [3]=r_off
#define KmulJw  6             // job record limbs: ar, br, n, rr, sr, state
static union u const kmul_loop[1] = { { .ap = lvm_kmul } };

static struct ai *ai_kmul_setup(struct ai *g) {
 word a = g->sp[0], b = g->sp[1];
 // caller contract: bigp(a)&&bigp(b), na==nb==n, n>=kara_cutoff
 int n = big_nlimbs(a), d = 0;
 for (int t = n; t >= kara_cutoff; t = (t + 1) / 2) d++;   // Karatsuba depth
 uintptr_t njob = (uintptr_t) 8 * d + 32,                 // job-stack capacity (~3d live, generous)
           scrn = (uintptr_t) 6 * n + 16 * (uintptr_t) d + 256,      // O(n) scratch, with margin
           jobs_off = KmulHdr,
           a_off = jobs_off + njob * KmulJw,
           b_off = a_off + n,
           r_off = b_off + n,
           scr_off = r_off + 2 * (uintptr_t) n,
           wslimbs = scr_off + scrn,
           sreq = str_type_width + b2w(wslimbs * sizeof(ai_limb)),
           breq = Width(struct ai_cask) + Width(struct ai_tag);
 if (!ai_ok(g = ai_have(g, sreq + breq + 3))) return g;
 a = g->sp[0], b = g->sp[1];                              // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 (void) load_int_mag(a, sa, &la, &nega);
 (void) load_int_mag(b, sb, &lb, &negb);
 struct ai_str *ws_s = ini_str(str(g->hp), wslimbs * sizeof(ai_limb));
 g->hp += sreq;
 ai_limb *ws = (ai_limb*) txt(ws_s);
 for (uintptr_t i = 0; i < wslimbs; i++) ws[i] = 0;       // zero everything: clean result + scratch slots
 for (int i = 0; i < n; i++) ws[a_off + i] = la[i];
 for (int i = 0; i < n; i++) ws[b_off + i] = lb[i];
 ws[0] = (ai_limb) n, ws[1] = 1, ws[2] = (ai_limb) (nega != negb), ws[3] = r_off;   // n, top=1, sign, r_off
 ai_limb *j0 = ws + jobs_off;                             // the root job: multiply A x B -> R
 j0[0] = a_off, j0[1] = b_off, j0[2] = (ai_limb) n, j0[3] = r_off, j0[4] = scr_off, j0[5] = 0;
 union u *k = (union u*) g->hp; g->hp += breq;
 cask(k)->ap = lvm_cask;
 cask(k)->str = ws_s;
 tagthread(k, Width(struct ai_cask));
 union u *ret = g->ip + 1;
 g->sp[0] = word(k), g->sp[1] = word(ret);                // frame [ws_buf, ret_ip]  (was [a, b])
 g->ip = (union u*) kmul_loop;
 return g; }

// FIXME can we choose different types to reduce the amount of explicit casting in this function?
lvm(lvm_kmul) {
 ai_limb *ws = (ai_limb*) txt(cask(Sp[0])->str);
 int n = (int) ws[0], top = (int) ws[1];
 ai_limb *jobs = ws + KmulHdr;
 long budget = kmul_chunk;
 while (top > 0 && budget > 0) {
  ws[1] = (ai_limb) top; YieldCheck();             // persist top, then a PER-JOB yield check:
                                                   // a once-per-dispatch check yields too rarely here
  ai_limb *J = jobs + (uintptr_t) (top - 1) * KmulJw;
  uintptr_t ar = J[0], br = J[1];
  int jn = (int) J[2];
  uintptr_t rr = J[3], sr = J[4];
  int st = (int) J[5];
  if (jn < kara_cutoff) {                                 // leaf: schoolbook jn x jn -> ws[rr..rr+2jn)
   mag_mul(ws + rr, ws + ar, jn, ws + br, jn);
   budget -= (long) jn * jn; top--; continue; }
  int m = jn / 2, h = jn - m;                             // low m limbs, high h (m or m+1)
  if (st == 0) {                                          // SPLIT
   uintptr_t saO = sr, sbO = sr + (uintptr_t) (h + 1),
             z1O = sr + 2 * (uintptr_t) (h + 1), csr = sr + 4 * (uintptr_t) (h + 1);
   int ns = mag_add(ws + saO, ws + ar, m, ws + ar + m, h);            // sa = a_lo + a_hi
   for (int i = ns; i < h + 1; i++) ws[saO + i] = 0;                  // zero-extend to exactly h+1
   int nt = mag_add(ws + sbO, ws + br, m, ws + br + m, h);            // sb = b_lo + b_hi
   for (int i = nt; i < h + 1; i++) ws[sbO + i] = 0;
   for (int i = 0; i < 2 * (h + 1); i++) ws[z1O + i] = 0;            // clear z1's output slot
   J[5] = 1;                                                          // this job COMBINES when it returns
   ai_limb *z1J = jobs + (uintptr_t) top       * KmulJw;            // push z1 = sa*sb (pops first)
   z1J[0] = saO, z1J[1] = sbO, z1J[2] = (ai_limb) (h + 1), z1J[3] = z1O, z1J[4] = csr, z1J[5] = 0;
   ai_limb *z2J = jobs + (uintptr_t) (top + 1) * KmulJw;            // push z2 = a_hi*b_hi -> r[2m..]
   z2J[0] = ar + m, z2J[1] = br + m, z2J[2] = (ai_limb) h, z2J[3] = rr + 2 * (uintptr_t) m, z2J[4] = csr, z2J[5] = 0;
   ai_limb *z0J = jobs + (uintptr_t) (top + 2) * KmulJw;            // push z0 = a_lo*b_lo -> r[0..] (pops last)
   z0J[0] = ar, z0J[1] = br, z0J[2] = (ai_limb) m, z0J[3] = rr, z0J[4] = csr, z0J[5] = 0;
   top += 3; budget -= jn;                                            // pop order z0,z2,z1 then this (combine)
  } else {                                                // COMBINE (st == 1)
   uintptr_t z1O = sr + 2 * (uintptr_t) (h + 1);
   int z0n = 2 * m;
   while (z0n > 0 && ws[rr + (uintptr_t) z0n - 1] == 0) z0n--;
   int z2n = 2 * h;
   while (z2n > 0 && ws[rr + 2 * (uintptr_t) m + (uintptr_t) z2n - 1] == 0) z2n--;
   int nz1 = 2 * (h + 1);
   while (nz1 > 0 && ws[z1O + (uintptr_t) nz1 - 1] == 0) nz1--;
   nz1 = mag_sub(ws + z1O, ws + z1O, nz1, ws + rr, z0n);                       // z1 -= z0
   while (nz1 > 0 && ws[z1O + (uintptr_t) nz1 - 1] == 0) nz1--;
   nz1 = mag_sub(ws + z1O, ws + z1O, nz1, ws + rr + 2 * (uintptr_t) m, z2n);   // z1 -= z2
   while (nz1 > 0 && ws[z1O + (uintptr_t) nz1 - 1] == 0) nz1--;
   mag_add_off(ws + rr, 2 * jn, ws + z1O, nz1, m);                             // r += z1 * B^m
   budget -= jn; top--; }
 }
 ws[1] = (ai_limb) top;                                   // persist the stack pointer before any yield
 if (top > 0) { YieldCheck(); ai_musttail return Continue(); }
 bool neg = ws[2]; uintptr_t r_off = ws[3];               // done: ws[r_off..r_off+2n) is the product
 Have(Width(struct ai_big) + b2w(((size_t) 2 * (size_t) n + 1) * sizeof(ai_limb)));
 ws = (ai_limb*) txt(cask(Sp[0])->str);                    // re-fetch (Have may have GC'd)
 n = (int) ws[0], r_off = ws[3];
 word ret = Sp[1], res;
 Pack(g);
 res = ai_big_canon(&g->hp, ws + r_off, 2 * n, neg);
 Unpack(g);
 Sp += 1; Sp[0] = res; Ip = cell(ret); ai_musttail return Continue(); }

lvm(lvm_bmul_start) {
 // small-product fast path: a product that fits ONE chunk never yields, so the
 // resumable setup is pure overhead -- and that is the common case. one-shot it
 // through ai_big_binop. (na <= chunk/nb keeps na*nb from overflowing a 32-bit int.)
 word a = Sp[0], b = Sp[1];
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2;
 if (na <= bmul_chunk / nb) {
  Pack(g); g = ai_big_binop(g, vop_mul);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 if (bigp(a) && bigp(b) && na == nb) {           // equal-length large: subquadratic Karatsuba
  Pack(g); g = ai_kmul_setup(g);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 Pack(g); g = ai_bmul_setup(g);                  // unequal-length large: chunked schoolbook
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 ai_musttail return Resume(); }

lvm(lvm_bmul) {
 int i = (int) getcharm(Sp[0]);
 struct ai_big *A = big(Sp[3]), *B = big(Sp[4]);
 intptr_t sla = A->slen, slb = B->slen;
 int na = sla < 0 ? -sla : sla, nb = slb < 0 ? -slb : slb;
 if (!na || !nb) {                                // a zero operand: product is 0
  word ret = Sp[2]; Sp += 4; Sp[0] = zero; Ip = cell(ret); ai_musttail return Continue(); }
 ai_limb *la = A->limb, *lb = B->limb, *rl = (ai_limb*) txt(cask(Sp[1])->str);
 int end = min(i + max(1, bmul_chunk / nb), na);
 for (; i < end; i++) {                           // schoolbook outer loop, one chunk of rows
  ai_dlimb carry = 0; ai_limb ai = la[i];           // limb-typed so ai*lb[j] is the hardware 64x64->128, not a 128x128 multiply
  for (int j = 0; j < nb; j++) {
   ai_dlimb t = (ai_dlimb) ai * lb[j] + rl[i+j] + carry;
   rl[i+j] = (ai_limb) t, carry = t >> limb_bits; }
  rl[i+nb] = (ai_limb) carry; }
 Sp[0] = putcharm(i);                               // persist progress before any yield/GC
 if (i < na) { YieldCheck(); ai_musttail return Continue(); }
 bool neg = (sla < 0) != (slb < 0); word ret;     // done: canonicalize the product
 Have(Width(struct ai_big) + b2w((size_t) (na + nb) * sizeof(ai_limb)));
 ret = Sp[2];
 ai_limb *rmag = (ai_limb*) txt(cask(Sp[1])->str);  // re-fetch (Have may have GC'd)
 Pack(g);                                          // canon needs the synced g->hp (not &Hp: stack-local escapes block the sibcall)
 word res = ai_big_canon(&g->hp, rmag, na + nb, neg);
 Unpack(g);
 Sp += 4; Sp[0] = res; Ip = cell(ret); ai_musttail return Continue(); }

// --- resumable long division (lvm_bmul's divmod twin): normalize once into a
// pinned workspace [hdr | vn | un | q], grind the Knuth-D loop in chunks,
// persisting the index j. every entry re-reads the base from Sp[1] (a GC moves
// it). cheap divides one-shot through ai_big_binop. which: 0 = //, 1 = %.
#define bdiv_chunk (1 << 14)
#define BdivHdr 6           // ws header limbs: m, n, s(shift), which, nega, negb
static union u const bdiv_loop[1] = { { .ap = lvm_bdiv } };

static struct ai *ai_bdiv_setup(struct ai *g, int which) {
 word a = g->sp[0], b = g->sp[1];
 int mub = bigp(a) ? big_nlimbs(a) : wlimbs, nub = bigp(b) ? big_nlimbs(b) : wlimbs;
 uintptr_t wslimbs = BdivHdr + (uintptr_t) nub + (mub + 1) + (mub - nub + 1);
 uintptr_t sreq = str_type_width + b2w(wslimbs * sizeof(ai_limb));
 uintptr_t breq = Width(struct ai_cask) + Width(struct ai_tag);
 if (!ai_ok(g = ai_have(g, sreq + breq + 3))) return g;
 a = g->sp[0], b = g->sp[1];                          // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 int m = load_int_mag(a, sa, &la, &nega), n = load_int_mag(b, sb, &lb, &negb);
 struct ai_str *ws_s = ini_str(str(g->hp), wslimbs * sizeof(ai_limb));
 g->hp += sreq;
 ai_limb *ws = (ai_limb*) txt(ws_s);
 ai_limb *vn = ws + BdivHdr, *un = vn + n, *q = un + (m + 1);
 int s = limb_clz(lb[n-1]);                           // normalize so v[n-1]'s top bit is set
 for (int i = n-1; i > 0; i--) vn[i] = (lb[i] << s) | (s ? (ai_dlimb) lb[i-1] >> (limb_bits - s) : 0);
 vn[0] = lb[0] << s;
 un[m] = s ? (ai_dlimb) la[m-1] >> (limb_bits - s) : 0;
 for (int i = m-1; i > 0; i--) un[i] = (la[i] << s) | (s ? (ai_dlimb) la[i-1] >> (limb_bits - s) : 0);
 un[0] = la[0] << s;
 for (int i = 0; i < m - n + 1; i++) q[i] = 0;
 ws[0] = (ai_limb) m, ws[1] = (ai_limb) n, ws[2] = (ai_limb) s, ws[3] = (ai_limb) which;
 ws[4] = (ai_limb) nega, ws[5] = (ai_limb) negb;
 union u *k = (union u*) g->hp; g->hp += breq;
 cask(k)->ap = lvm_cask;
 cask(k)->str = ws_s;
 tagthread(k, Width(struct ai_cask));
 union u *ret = g->ip + 1;
 g->sp -= 1;                                          // [j, ws_buf, ret_ip]  (was [a, b])
 g->sp[0] = putcharm(m - n), g->sp[1] = word(k), g->sp[2] = word(ret);
 g->ip = (union u*) bdiv_loop;
 return g; }

lvm(lvm_bdiv_start, int vop) {
 word a = Sp[0], b = Sp[1];
 int m = bigp(a) ? big_nlimbs(a) : wlimbs, n = bigp(b) ? big_nlimbs(b) : wlimbs;
 // one-shot the cheap cases: |a|<|b| (q=0), single-limb divisor, or a short quotient.
 if (m < n || n < 2 || m - n < (int) (bdiv_chunk / (uintptr_t) n)) {
  Pack(g); g = ai_big_binop(g, vop);
  if (!ai_ok(g)) return Ap(_lvm_ghelp, g);
  return Resume(); }
 Pack(g); g = ai_bdiv_setup(g, vop == vop_rem);
 if (!ai_ok(g)) return Ap(_lvm_ghelp, g);
 return Resume(); }

lvm(lvm_bdiv) {
 ai_limb *ws = (ai_limb*) txt(cask(Sp[1])->str);
 int m = (int) ws[0], n = (int) ws[1], s = (int) ws[2], which = (int) ws[3];
 bool nega = ws[4], negb = ws[5];
 ai_limb *vn = ws + BdivHdr, *un = vn + n, *q = un + (m + 1);
 int j = (int) getcharm(Sp[0]);
 ai_dlimb const B = limb_base;
 int steps = max(1, (int) (bdiv_chunk / (uintptr_t) n));
 for (int c = 0; c < steps && j >= 0; c++, j--) {      // one Knuth-D quotient limb per iteration
  ai_limb rr;
  ai_dlimb qhat = div128by64(un[j+n], un[j+n-1], vn[n-1], &rr), rhat = rr;
  while (qhat >= B || qhat * vn[n-2] > ((rhat << limb_bits) | un[j+n-2])) {
   qhat--; rhat += vn[n-1]; if (rhat >= B) break; }
  ai_sdlimb borrow = 0;                                // multiply and subtract qhat*v
  for (int i = 0; i < n; i++) {
   ai_dlimb p = (ai_dlimb) (ai_limb) qhat * vn[i];
   ai_sdlimb sub = (ai_sdlimb) un[i+j] - borrow - (ai_sdlimb) (ai_limb) p;
   un[i+j] = (ai_limb) sub;
   borrow = (ai_sdlimb) (p >> limb_bits) - (sub >> limb_bits); }
  ai_sdlimb sub = (ai_sdlimb) un[j+n] - borrow;
  un[j+n] = (ai_limb) sub;
  q[j] = (ai_limb) qhat;
  if (sub < 0) {                                       // qhat one too big: add back
   q[j]--;
   ai_dlimb carry = 0;
   for (int i = 0; i < n; i++) { ai_dlimb t = (ai_dlimb) un[i+j] + vn[i] + carry; un[i+j] = (ai_limb) t; carry = t >> limb_bits; }
   un[j+n] = (ai_limb) (un[j+n] + carry); } }
 if (j >= 0) { Sp[0] = putcharm(j); YieldCheck(); ai_musttail return Continue(); }
 // done: canonicalize the requested output. denormalize the remainder into vn (now
 // dead), NOT in place, so a GC-retry of this tail stays idempotent. persist j=-1
 // first so a retry skips the loop.
 Sp[0] = putcharm(-1);
 int outn = which ? n : (m - n + 1);
 Have(Width(struct ai_big) + b2w((size_t) (outn + 1) * sizeof(ai_limb)));
 ws = (ai_limb*) txt(cask(Sp[1])->str);                 // re-fetch (Have may have GC'd)
 vn = ws + BdivHdr, un = vn + n, q = un + (m + 1);
 bool rneg = which ? nega : (nega != negb);
 word ret = Sp[2], res;
 Pack(g);
 if (which) {
  for (int i = 0; i < n; i++) vn[i] = s ? (un[i] >> s) | ((ai_dlimb) un[i+1] << (limb_bits - s)) : un[i];
  res = ai_big_canon(&g->hp, vn, n, rneg); }
 else res = ai_big_canon(&g->hp, q, m - n + 1, rneg);
 Unpack(g);
 Sp += 2; Sp[0] = res; Ip = cell(ret); ai_musttail return Continue(); }

// --- reader / printer -------------------------------------------------------

// One digit, either radix -- decimal digits sort below 'a', so the same fold
// reads both and hex takes either case.
static ai_inline ai_limb rdigit(char c) {
 return (ai_limb) (c <= '9' ? c - '0' : (c | 32) - 'a' + 10); }

// g->sp[0] is a [+-]?<pfx><digits> token; replace it with the canonical value.
// accumulates `chunk` digits per mul-add pass (radix**chunk fits one limb).
static struct ai *big_read_radix(struct ai *g, ai_limb radix, int chunk, uintptr_t pfx) {
 struct ai_str *tok = str(g->sp[0]);
 uintptr_t n = tok->len;
 char const *s = tok->bytes;
 bool neg = n && s[0] == '-';
 uintptr_t i = ((n && (s[0] == '-' || s[0] == '+')) ? 1 : 0) + pfx, ndig = n - i;
 int cap = (int) (ndig / (uintptr_t) chunk) + 3;  // upper-bound magnitude limbs (>= ndig/digits-per-limb)
 uintptr_t res_area = Width(struct ai_big) + b2w((size_t) cap * sizeof(ai_limb));
 if (!ai_ok(g = ai_have(g, res_area + b2w((size_t) cap * sizeof(ai_limb))))) return g;
 tok = str(g->sp[0]), s = tok->bytes;            // re-fetch post-GC
 ai_limb *mag = (ai_limb*) (g->hp + res_area);
 int m = 0;
 while (i < n) {
  ai_limb acc = 0, pw = 1; int k = 0;
  for (; i < n && k < chunk; i++, k++) acc = acc * radix + rdigit(s[i]), pw *= radix;
  m = mag_mul_add_small(mag, m, pw, acc); }
 g->sp[0] = ai_big_canon(&g->hp, mag, m, neg);
 return g; }
struct ai *ai_big_read_dec(struct ai *g) { return big_read_radix(g, 10, limb_dec_chunk, 0); }
struct ai *ai_big_read_hex(struct ai *g) { return big_read_radix(g, 16, limb_hex_chunk, 2); }
struct ai *ai_big_read_oct(struct ai *g) { return big_read_radix(g,  8, limb_oct_chunk, 1); }

// --- (tray witness shape-list vals): THE typed array constructor (mopped; the
// prel's *-tray wrap it). the witness names its tier by example (0/0.0/~(0 0)/()
// -> z/r/c/o); vals fills row-major (missing stays 0, extras ignored). bad
// witness / negative dim / over-rank -> zero.
lvm(lvm_trayctor) {
 word t = Sp[0], shp = Sp[1];                  // t = a WITNESS GEM (names its tier), vals = Sp[2]
 // the type is read off the witness's KIND -- a value inhabiting the tier: 0 -> Z,
 // 0.0 -> R, ~(0 0) -> C, and anything else (canonically (), the O floor) -> O.
 intptr_t ty = twinp(t) ? ai_C : gemp(t) ? ai_R
             : (charmp(t) || sunp(t) || bigp(t)) ? ai_Z : ai_O;
 uintptr_t rank = 0, nelem = 1;
 for (word l = shp; chainp(l); l = B(l)) {
  word d = A(l);
  if (!charmp(d) || getcharm(d) < 0) ai_musttail return Answerp(2, zero);
  rank++, nelem *= (uintptr_t) getcharm(d); }
 if (rank > maxrank) ai_musttail return Answerp(2, zero);
 uintptr_t bytes = sizeof(struct ai_tray) + rank * sizeof(word) + nelem * ai_T[ty];
 Have(b2w(bytes));
 struct ai_tray *v = (struct ai_tray*) Hp;
 Hp += b2w(bytes);
 ini_tray(v, ty, rank);
 uintptr_t i = 0;                              // re-walk the (possibly moved) lists
 for (word l = Sp[1]; chainp(l); l = B(l)) v->shape[i++] = (uintptr_t) getcharm(A(l));
 if (ty == ai_O) for (i = 0; i < nelem; i++) tray_put_obj(v, i, ZeroPoint);  // O floor is () not 0
 else memset(tray_data(v), 0, nelem * ai_T[ty]);
 i = 0;                                        // no alloc below, so v/Sp[2] stay put
 for (word l = Sp[2]; chainp(l) && i < nelem; l = B(l), i++) tray_put(v, i, A(l));
 // only a RANK-0 point (empty shape) demotes to its lone scalar gem; a
 // rank-1-len-1 STAYS an array (@(5) is a one-cell array, not 5 -- collapsing it
 // left the surface discontinuous). root the built tray: the box alloc can GC.
 Sp[2] = word(v);
 if (rank == 0) {
  if (ty == ai_O) ai_musttail return Answerp(2, tray_get_obj(v, 0));
  if (ty == ai_C) { Have(twin_req); v = tray(Sp[2]); ai_flo_t *fp = tray_data(v);
   Sp[2] = mk_twin(&Hp, fp[0], fp[1]); ai_musttail return Nextp(1, 2); }
  word _res; Have(box_req); v = tray(Sp[2]);
  if (ty >= ai_R) emit_gem(_res, tray_get_flo(v, 0));
  else emit_int(_res, tray_get_int(v, 0));
  ai_musttail return Answerp(2, _res); }
 ai_musttail return Answerp(2, word(v)); }

// (iota n): a z-array of 0..n-1, the array twin of `jot`; n<0 or non-fixnum -> zero
lvm(lvm_iota) {
 word nx = Sp[0];
 if (!charmp(nx) || getcharm(nx) < 0) ai_musttail return Answer(ZeroPoint);
 uintptr_t n = (uintptr_t) getcharm(nx);
 uintptr_t bytes = sizeof(struct ai_tray) + 1 * sizeof(word) + n * ai_T[ai_Z];
 Have(b2w(bytes));
 struct ai_tray *v = (struct ai_tray*) Hp;
 Hp += b2w(bytes);
 ini_tray(v, ai_Z, 1);
 v->shape[0] = n;
 for (uintptr_t i = 0; i < n; i++) tray_put_int(v, i, (intptr_t) i);
 ai_musttail return Answer(word(v)); }

// --- accessors -------------------------------------------------------------
// rank / element-type code as fixnums; zero for a non-tray. Both 0 for a scalar box.
op11(lvm_rank, packp(Sp[0]) ? putcharm(tray(Sp[0])->rank) : ZeroPoint)
op11(lvm_atype, packp(Sp[0]) ? putcharm(tray(Sp[0])->type) : ZeroPoint)

// total element count (1 for a scalar box), zero for a non-tray.
lvm(lvm_alen) {
 word x = Sp[0];
 if (!packp(x)) ai_musttail return Answer(ZeroPoint);
 ai_musttail return Answer(putcharm(tray_nelem(tray(x)))); }

// dimensions as a list (allocates rank link cells), zero for a non-tray.
lvm(lvm_shape) {
 word x = Sp[0];
 if (!packp(x)) ai_musttail return Answer(ZeroPoint);
 uintptr_t r = tray(x)->rank;
 Have(r * Width(struct ai_chain));
 struct ai_tray *v = tray(Sp[0]);                 // re-read post-Have
 struct ai_chain *p = (struct ai_chain*) Hp;
 Hp += r * Width(struct ai_chain);
 word list = ZeroPoint;                             // () terminator (zero-ontology)
 for (uintptr_t i = r; i--; )
  ini_chain(p, putcharm(v->shape[i]), list), list = word(p), p++;
 ai_musttail return Answer(list); }


// ai_O reductions (sum/prod/max/min) fold through the promoting scalar op, so an
// object array reduces *exactly*. Defined after the object lane (below); the
// numeric reductions divert here when their operand is a ai_O array.
static struct ai *ored(struct ai *g, int kind);   // kind: 0 sum, 1 prod, 2 max, 3 min

// --- reductions: array -> scalar; identity on a scalar, which makes
// (aall (< a b)) rank-agnostic
lvm(lvm_asum) {
 word x = Sp[0];
 if (!packp(x)) ai_musttail return Next(1);        // scalar: (asum 5) = 5
 if (tray(x)->type == ai_O) {
  Pack(g); g = ored(g, 0);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 if (tray(x)->type == ai_C) {                   // complex sum -> a complex box
  struct ai_tray *v = tray(x); uintptr_t n = tray_nelem(v);  // K=4 accumulators (see aprod)
  ai_flo_t *fp = tray_data(v);                   // read all parts before Have (no alloc here)
  ai_flo_t a0=0,b0=0, a1=0,b1=0, a2=0,b2=0, a3=0,b3=0; uintptr_t j = 0;
  for (; j + 4 <= n; j += 4) {
   a0 += fp[2*j];   b0 += fp[2*j+1]; a1 += fp[2*j+2]; b1 += fp[2*j+3];
   a2 += fp[2*j+4]; b2 += fp[2*j+5]; a3 += fp[2*j+6]; b3 += fp[2*j+7]; }
  for (; j < n; j++) a0 += fp[2*j], b0 += fp[2*j+1];
  ai_flo_t sr = (a0+a1)+(a2+a3), si = (b0+b1)+(b2+b3);
  Have(twin_req);
  Sp[0] = mk_twin(&Hp, sr, si); ai_musttail return Next(1); }
 struct ai_tray *v = tray(x);
 uintptr_t n = tray_nelem(v);
 bool fdom = v->type >= ai_R; word _res;
 Have(box_req);
 v = tray(Sp[0]);
 if (fdom) {                                    // K=4 accumulators (see aprod complex)
  ai_flo_t a0=0,a1=0,a2=0,a3=0; uintptr_t i = 0;
  for (; i + 4 <= n; i += 4) a0+=tray_get_flo(v,i), a1+=tray_get_flo(v,i+1), a2+=tray_get_flo(v,i+2), a3+=tray_get_flo(v,i+3);
  for (; i < n; i++) a0 += tray_get_flo(v, i);
  emit_gem(_res, (a0+a1)+(a2+a3)); }
 else {                                         // K=4 (modular, Z/2^64 is a commutative ring -> assoc+exact)
  uintptr_t a0=0,a1=0,a2=0,a3=0, i = 0;
  for (; i + 4 <= n; i += 4) a0+=(uintptr_t)tray_get_int(v,i), a1+=(uintptr_t)tray_get_int(v,i+1), a2+=(uintptr_t)tray_get_int(v,i+2), a3+=(uintptr_t)tray_get_int(v,i+3);
  for (; i < n; i++) a0 += (uintptr_t) tray_get_int(v, i);
  emit_int(_res, (intptr_t) ((a0+a1)+(a2+a3))); }
 ai_musttail return Answer(_res); }

lvm(lvm_aprod) {
 word x = Sp[0];
 if (!packp(x)) ai_musttail return Next(1);
 if (tray(x)->type == ai_O) {
  Pack(g); g = ored(g, 1);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 if (tray(x)->type == ai_C) {                   // complex product -> a complex box
  // K=4 independent accumulators break the multiply latency chain (~3x);
  // reassociation is sound -- fp differs only in last-bit rounding per grouping
  struct ai_tray *v = tray(x); uintptr_t n = tray_nelem(v);
  ai_flo_t *fp = tray_data(v);
  ai_flo_t r0=1,i0=0, r1=1,i1=0, r2=1,i2=0, r3=1,i3=0, t; uintptr_t j = 0;
  for (; j + 4 <= n; j += 4) {
   t = r0*fp[2*j]  -i0*fp[2*j+1]; i0 = r0*fp[2*j+1]+i0*fp[2*j];   r0 = t;
   t = r1*fp[2*j+2]-i1*fp[2*j+3]; i1 = r1*fp[2*j+3]+i1*fp[2*j+2]; r1 = t;
   t = r2*fp[2*j+4]-i2*fp[2*j+5]; i2 = r2*fp[2*j+5]+i2*fp[2*j+4]; r2 = t;
   t = r3*fp[2*j+6]-i3*fp[2*j+7]; i3 = r3*fp[2*j+7]+i3*fp[2*j+6]; r3 = t; }
  for (; j < n; j++) { t = r0*fp[2*j]-i0*fp[2*j+1]; i0 = r0*fp[2*j+1]+i0*fp[2*j]; r0 = t; }
  ai_flo_t ra = r0*r1-i0*i1, ia = r0*i1+i0*r1, rb = r2*r3-i2*i3, ib = r2*i3+i2*r3;
  ai_flo_t pr = ra*rb-ia*ib, pi = ra*ib+ia*rb;
  Have(twin_req);
  Sp[0] = mk_twin(&Hp, pr, pi); ai_musttail return Next(1); }
 struct ai_tray *v = tray(x);
 uintptr_t n = tray_nelem(v);
 bool fdom = v->type >= ai_R; word _res;
 Have(box_req); v = tray(Sp[0]);
 if (fdom) {                                    // K=4 accumulators (see complex above)
  ai_flo_t a0=1,a1=1,a2=1,a3=1; uintptr_t i = 0;
  for (; i + 4 <= n; i += 4) a0*=tray_get_flo(v,i), a1*=tray_get_flo(v,i+1), a2*=tray_get_flo(v,i+2), a3*=tray_get_flo(v,i+3);
  for (; i < n; i++) a0 *= tray_get_flo(v, i);
  emit_gem(_res, (a0*a1)*(a2*a3)); }
 else {                                         // K=4 (modular product; imul is latency-bound, ~3x)
  uintptr_t a0=1,a1=1,a2=1,a3=1, i = 0;
  for (; i + 4 <= n; i += 4) a0*=(uintptr_t)tray_get_int(v,i), a1*=(uintptr_t)tray_get_int(v,i+1), a2*=(uintptr_t)tray_get_int(v,i+2), a3*=(uintptr_t)tray_get_int(v,i+3);
  for (; i < n; i++) a0 *= (uintptr_t) tray_get_int(v, i);
  emit_int(_res, (intptr_t) ((a0*a1)*(a2*a3))); }
 ai_musttail return Answer(_res); }

// max / min over a non-empty array (kind 2 = max, 3 = min, matching ored);
// empty -> zero; scalar -> identity. The kind selects the comparison sense.
static lvm(lvm_aextreme, int kind) {
 word x = Sp[0];
 if (!packp(x)) return Next(1);
 if (tray(x)->type == ai_O) {
  Pack(g); g = ored(g, kind);
  if (!ai_ok(g)) return Ap(_lvm_ghelp, g);
  return Resume(); }
 if (tray(x)->type == ai_C) return Answer(ZeroPoint);   // complex: unordered
 struct ai_tray *v = tray(x);
 uintptr_t n = tray_nelem(v);
 if (!n) return Answer(ZeroPoint);
 bool fdom = v->type >= ai_R, ismax = kind == 2; word _res;
 Have(box_req); v = tray(Sp[0]);
 // K=4 running extremes break the latency chain; EXACT (selects an existing element)
 if (fdom) { ai_flo_t m0 = tray_get_flo(v, 0), m1=m0, m2=m0, m3=m0, e; uintptr_t i = 1;
  for (; i + 4 <= n; i += 4) {
   e = tray_get_flo(v,i);   if (ismax?e>m0:e<m0) m0=e;
   e = tray_get_flo(v,i+1); if (ismax?e>m1:e<m1) m1=e;
   e = tray_get_flo(v,i+2); if (ismax?e>m2:e<m2) m2=e;
   e = tray_get_flo(v,i+3); if (ismax?e>m3:e<m3) m3=e; }
  for (; i < n; i++) { e = tray_get_flo(v,i); if (ismax?e>m0:e<m0) m0=e; }
  if (ismax?m1>m0:m1<m0) m0=m1;
  if (ismax?m2>m0:m2<m0) m0=m2;
  if (ismax?m3>m0:m3<m0) m0=m3;
  emit_gem(_res, m0); }
 else { intptr_t m0 = tray_get_int(v, 0), m1=m0, m2=m0, m3=m0, e; uintptr_t i = 1;
  for (; i + 4 <= n; i += 4) {
   e = tray_get_int(v,i);   if (ismax?e>m0:e<m0) m0=e;
   e = tray_get_int(v,i+1); if (ismax?e>m1:e<m1) m1=e;
   e = tray_get_int(v,i+2); if (ismax?e>m2:e<m2) m2=e;
   e = tray_get_int(v,i+3); if (ismax?e>m3:e<m3) m3=e; }
  for (; i < n; i++) { e = tray_get_int(v,i); if (ismax?e>m0:e<m0) m0=e; }
  if (ismax?m1>m0:m1<m0) m0=m1;
  if (ismax?m2>m0:m2<m0) m0=m2;
  if (ismax?m3>m0:m3<m0) m0=m3;
  emit_int(_res, m0); }
 return Answer(_res); }
lvm(lvm_max) { return Ap(lvm_aextreme, g, 2); }
lvm(lvm_min) { return Ap(lvm_aextreme, g, 3); }

// aall: the bool conjunction reduction ("no zero element"; empty -> vacuously
// true; scalar -> identity). the disjunction is just `len`.
lvm(lvm_aall) {
 word x = Sp[0];
 if (!packp(x)) ai_musttail return Next(1);
 struct ai_tray *v = tray(x);
 uintptr_t n = tray_nelem(v);
 if (v->type == ai_O) {                         // object: a falsy element fails the conjunction
  for (uintptr_t i = 0; i < n; i++)
   if (ai_nilp(g, tray_get_obj(v, i))) ai_musttail return Answer(zero);
  ai_musttail return Answer(putcharm(1)); }
 if (v->type == ai_C) {                         // complex: a 0+0i element fails the conjunction
  ai_flo_t *fp = tray_data(v);
  for (uintptr_t i = 0; i < n; i++)
   if (fp[2*i] == 0 && fp[2*i+1] == 0) ai_musttail return Answer(zero);
  ai_musttail return Answer(putcharm(1)); }
 // a short-circuit sound, NOT an accumulator chain -- already load-bound (the
 // compiler vectorizes it), so multi-accumulating buys nothing; left as is.
 bool fdom = v->type >= ai_R;
 for (uintptr_t i = 0; i < n; i++)
  if (fdom ? tray_get_flo(v, i) == 0 : tray_get_int(v, i) == 0)
   ai_musttail return Answer(zero);
 ai_musttail return Answer(putcharm(1)); }

// (outer a b): result[I,J] = a[I] * b[J], shape a.shape ++ b.shape; complex/
// object or over-rank -> zero
lvm(lvm_outer) {
 word a = Sp[0], b = Sp[1];
 if (!(trayp(a) && trayp(b))) ai_musttail return Push(ZeroPoint);
 struct ai_tray *va = tray(a), *vb = tray(b);
 if (va->type > ai_R || vb->type > ai_R) ai_musttail return Push(ZeroPoint);
 uintptr_t M = tray_nelem(va), N = tray_nelem(vb), n = M * N, rank = va->rank + vb->rank;
 if (rank > maxrank) ai_musttail return Push(ZeroPoint);
 bool fdom = va->type == ai_R || vb->type == ai_R;
 enum ai_tray_type rt = fdom ? ai_R : ai_Z;
 uintptr_t bytes = sizeof(struct ai_tray) + rank * sizeof(word) + ai_T[rt] * n;
 Have(b2w(bytes));
 va = tray(Sp[0]), vb = tray(Sp[1]);          // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, rt, rank);
 for (uintptr_t i = 0; i < va->rank; i++) r->shape[i] = va->shape[i];
 for (uintptr_t i = 0; i < vb->rank; i++) r->shape[va->rank + i] = vb->shape[i];
 if (fdom) { ai_flo_t *rp = tray_data(r);
  for (uintptr_t i = 0; i < M; i++) { ai_flo_t av = tray_get_flo(va, i);
   for (uintptr_t j = 0; j < N; j++) rp[i*N+j] = av * tray_get_flo(vb, j); } }
 else { intptr_t *rp = tray_data(r);
  for (uintptr_t i = 0; i < M; i++) { intptr_t av = tray_get_int(va, i);
   for (uintptr_t j = 0; j < N; j++) rp[i*N+j] = (intptr_t)((uintptr_t)av * (uintptr_t)tray_get_int(vb, j)); } }
 ai_musttail return Push(word(r)); }   // arity 2

// (inner a b): +.× -- contract a's LAST axis with b's FIRST (1D·1D = dot, 2D·2D =
// matmul); mismatch/complex/object/over-rank -> zero. ikj order so the inner j
// loop vectorizes.
lvm(lvm_inner) {
 word a = Sp[0], b = Sp[1];
 if (!(trayp(a) && trayp(b))) ai_musttail return Push(ZeroPoint);
 struct ai_tray *va = tray(a), *vb = tray(b);
 if (va->type > ai_R || vb->type > ai_R || va->rank < 1 || vb->rank < 1)
  ai_musttail return Push(ZeroPoint);
 uintptr_t K = va->shape[va->rank - 1];
 if (K != vb->shape[0]) ai_musttail return Push(ZeroPoint);   // contracted axes must agree
 uintptr_t M = 1, N = 1;
 for (uintptr_t i = 0; i + 1 < va->rank; i++) M *= va->shape[i];
 for (uintptr_t i = 1; i < vb->rank; i++) N *= vb->shape[i];
 uintptr_t rank = (va->rank - 1) + (vb->rank - 1), n = M * N;
 if (rank > maxrank) ai_musttail return Push(ZeroPoint);
 bool fdom = va->type == ai_R || vb->type == ai_R, ar = va->type == ai_R, br = vb->type == ai_R;
 if (n == 1) {                                  // 1D·1D dot, or any 1-cell contraction -> scalar (invariant)
  word _res;
  if (fdom) {
   ai_flo_t *Ad = tray_data(va), *Bd = tray_data(vb);
   intptr_t *Ai = tray_data(va), *Bi = tray_data(vb); ai_flo_t acc = 0;
   for (uintptr_t l = 0; l < K; l++) acc += (ar ? Ad[l] : (ai_flo_t) Ai[l]) * (br ? Bd[l] : (ai_flo_t) Bi[l]);
   Have(box_req); emit_gem(_res, acc); }
  else {
   intptr_t *A = tray_data(va), *B = tray_data(vb);
   uintptr_t acc = 0;
   for (uintptr_t l = 0; l < K; l++) acc += (uintptr_t) A[l] * (uintptr_t) B[l];
   Have(box_req);
   emit_int(_res, (intptr_t) acc); }
  ai_musttail return Push(_res); }
 enum ai_tray_type rt = fdom ? ai_R : ai_Z;
 uintptr_t bytes = sizeof(struct ai_tray) + rank * sizeof(word) + ai_T[rt] * n;
 Have(b2w(bytes));
 va = tray(Sp[0]), vb = tray(Sp[1]);
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, rt, rank);
 { uintptr_t s = 0;
   for (uintptr_t i = 0; i + 1 < va->rank; i++) r->shape[s++] = va->shape[i];
   for (uintptr_t i = 1; i < vb->rank; i++) r->shape[s++] = vb->shape[i]; }
 if (fdom) { ai_flo_t *C = tray_data(r);
  ai_flo_t *Ad = tray_data(va), *Bd = tray_data(vb);
  intptr_t *Ai = tray_data(va), *Bi = tray_data(vb);
  for (uintptr_t p = 0; p < n; p++) C[p] = 0;
  for (uintptr_t i = 0; i < M; i++)
   for (uintptr_t l = 0; l < K; l++) { ai_flo_t av = ar ? Ad[i*K+l] : (ai_flo_t) Ai[i*K+l];
    for (uintptr_t j = 0; j < N; j++) C[i*N+j] += av * (br ? Bd[l*N+j] : (ai_flo_t) Bi[l*N+j]); } }
 else { intptr_t *C = tray_data(r), *A = tray_data(va), *B = tray_data(vb);
  for (uintptr_t p = 0; p < n; p++) C[p] = 0;
  for (uintptr_t i = 0; i < M; i++)
   for (uintptr_t l = 0; l < K; l++) { intptr_t av = A[i*K+l];
    for (uintptr_t j = 0; j < N; j++) C[i*N+j] = (intptr_t)((uintptr_t)C[i*N+j] + (uintptr_t)av * (uintptr_t)B[l*N+j]); } }
 ai_musttail return Push(word(r)); }   // arity 2

// --- elementwise monadic math over an array -> a float array of the same shape;
// the fill loop takes no &local, so the lvm wrapper keeps its tail call
static ai_noinline void vmap1_fill(struct ai_tray *r, struct ai_tray *a, ai_flo_t (*fn)(ai_flo_t)) {
 uintptr_t n = tray_nelem(r);
 for (uintptr_t i = 0; i < n; i++) tray_put_flo(r, i, fn(tray_get_flo(a, i))); }

lvm(lvm_vmap1, ai_flo_t (*fn)(ai_flo_t)) {
 struct ai_tray *a = tray(Sp[0]);
 uintptr_t rank = a->rank, n = tray_nelem(a);
 uintptr_t bytes = sizeof(struct ai_tray) + rank * sizeof(word) + n * ai_T[ai_R];
 Have(b2w(bytes));
 a = tray(Sp[0]);                               // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp;
 Hp += b2w(bytes);
 ini_tray(r, ai_R, rank);
 for (uintptr_t i = 0; i < rank; i++) r->shape[i] = a->shape[i];
 vmap1_fill(r, a, fn);
 return Answer(word(r)); }

// --- elementwise dyadic engine with broadcasting. integer division guards /0
// and INT_MIN/-1 -> 0 (one element can't change the whole result's domain).
static ai_flo_t vop_flo(int op, ai_flo_t a, ai_flo_t b) {
 switch (op) {
  case vop_sub: return a - b; case vop_mul: return a * b;
  case vop_quot: return a / b; case vop_fquot: return ai_trunc(a / b);
  case vop_rem: return ai_fmod(a, b);
  default: return a + b; } }                   // vop_add
static intptr_t vop_int(int op, intptr_t a, intptr_t b) {
 switch (op) {
  case vop_sub: return (intptr_t)((uintptr_t) a - (uintptr_t) b);
  case vop_mul: return (intptr_t)((uintptr_t) a * (uintptr_t) b);
  case vop_quot: case vop_fquot: return (b == 0 || (a == INTPTR_MIN && b == -1)) ? 0 : a / b;
  case vop_rem:  return (b == 0 || (a == INTPTR_MIN && b == -1)) ? 0 : a % b;
  case vop_band: return a & b;
  case vop_bor:  return a | b;
  case vop_bxor: return a ^ b;
  case vop_bsl:  return (intptr_t)((uintptr_t) a << shmask(b));
  case vop_bsr:  return a >> shmask(b);
  default: return (intptr_t)((uintptr_t) a + (uintptr_t) b); } } // vop_add
static intptr_t vcmp_flo(int op, ai_flo_t a, ai_flo_t b) {
 switch (op) {
  case vop_lt: return a < b; case vop_le: return a <= b;
  case vop_gt: return a > b; case vop_ge: return a >= b;
  default: return a == b; } }                   // vop_eq
static intptr_t vcmp_int(int op, intptr_t a, intptr_t b) {
 switch (op) {
  case vop_lt: return a < b; case vop_le: return a <= b;
  case vop_gt: return a > b; case vop_ge: return a >= b;
  default: return a == b; } }                   // vop_eq

// === ordered comparison: the true-blue total order over ALL values ===========
// low -> high: () < mint < string < number < tray < chain < map < hot (an array
// operand compares ELEMENTWISE via lvm_vbin instead -- the mask). within a band:
// numbers by value across the tower (complex lexicographic by (re, im), NaN
// unordered), strings lex, symbols by name then serial, chains lex recursively,
// lambdas/maps by repr hash (GC-stable). only < and <= are implemented; > and >=
// REVERSE the operands (right for NaN: swap, never negate). a total preorder:
// hash-colliding lambdas compare equal but are not =. the compare order is
// DECOUPLED from the enum dispatch order -- cmp_rank remaps, the matrices untouched.
static ai_inline int cmp_rank(struct ai *g, word x) {
 if (nomp(x)) return 0;                            // mint/symbol -- the floor (a named sym is a (name . mint) chain)
 enum q k = ai_kind(x);
 if (k == KString) return 1;                       // string: above mint, below number
 if (isnum(x) || twinp(x)) return 2;               // the number band, by value (charm bridges up from string)
 if (k == KTrayZ || k == KTrayR || k == KTrayC) return 2;  // a GALAXY folds into the number band, ordered by its net
 if (k == KTrayO) return 3;                         // object tray: above the numbers, BELOW chain
 if (k == KChain) return 4;                        // chain: the grammar substrate -- HIGH, just under book (only book's mutability seats it above)
 if (k == KTablet) return 5;                          // tablet: above chain
 if (coinp(x) && die_get(g, coin_die(x), DieNet) == putcharm(2))
  return 2;                                        // a RATIO coin seats IN the number band, by its value
 return 6; }                                       // KHot -- thread/function, the ceiling (the only kind left)
static ai_inline intptr_t bytes_cmp(const char *pa, uintptr_t la, const char *pb, uintptr_t lb) {
 uintptr_t n = la < lb ? la : lb;
 int c = n ? memcmp(pa, pb, n) : 0;
 return c ? (c < 0 ? -1 : 1) : la < lb ? -1 : la > lb ? 1 : 0; }
// the floor band (cmp_rank 0): () < bare mints (KMint, by serial) < named points
// (KNom, by name lex then serial). () is the serial-0 point, seated least of all by
// an identity guard; a named point outranks every bare mint (the na/nb split below).
static ai_inline intptr_t mint_cmp(struct ai *g, word a, word b) {
 if (a == b) return 0;
 word core = ZeroPoint;                       // () is the nameless serial-0 point: least of all
 if (a == core) return -1;                               // (a != b, so b is some other mint above it)
 if (b == core) return 1;                                // -- guarded by identity, its atom slots are never read
 // a named point is a KNom; a bare mint is the nameless atom. bare mints rank below
 // every named symbol; within a band, named by (name lex, then serial), bare by serial.
 bool na = namep(a), nb = namep(b);
 if (na != nb) return na ? 1 : -1;                       // bare mint < named symbol
 if (na) {                                               // both named (KNom): name first, then the serial
  struct ai_str *sa = str(nom(a)->name), *sb = str(nom(b)->name);
  intptr_t c = bytes_cmp(txt(sa), len(sa), txt(sb), len(sb));
  if (c) return c;
  uintptr_t ma = nom(a)->code, mb = nom(b)->code;
  return ma < mb ? -1 : ma > mb ? 1 : 0; }
 uintptr_t ca = sym(a)->code, cb = sym(b)->code;         // both bare: by serial
 return ca < cb ? -1 : ca > cb ? 1 : 0; }
// Two galaxies of EQUAL net: a strict tiebreak so cmp3 stays antisymmetric --
// shape lexicographically (rank, then dims), then cell content (re, then im),
// row-major. Reached only from the number band below, both operands galaxies.
static ai_inline struct ai_zn tray_cell_zn(struct ai_tray *v, uintptr_t i) {
 if (v->type == ai_C) { ai_flo_t *d = tray_data(v); return zn(d[2*i], d[2*i+1]); }
 return zn(tray_get_flo(v, i), 0); }
static intptr_t galaxy_tie(struct ai_tray *va, struct ai_tray *vb) {
 if (va->rank != vb->rank) return va->rank < vb->rank ? -1 : 1;
 for (uintptr_t i = 0; i < va->rank; i++)
  if (va->shape[i] != vb->shape[i]) return va->shape[i] < vb->shape[i] ? -1 : 1;
 uintptr_t n = tray_nelem(va);                             // same shape -> same nelem
 for (uintptr_t i = 0; i < n; i++) {
  struct ai_zn ea = tray_cell_zn(va, i), eb = tray_cell_zn(vb, i);
  if (ea.re != eb.re) return ea.re < eb.re ? -1 : 1;
  if (ea.im != eb.im) return ea.im < eb.im ? -1 : 1; }
 return 0; }
// a RATIO coin orders by its VALUE: int64-fitting components cross-multiply
// EXACTLY (near-equal rationals order right where the float quotient ties);
// anything wider falls to the sign-exact net quotients.
static ai_inline bool ratio_ifit(word x, int64_t *v) {
 if (charmp(x) || sunp(x)) return *v = toint(x), true;
 if (!bigp(x)) return false;
 struct ai_big *b = big(x);
 int n = big_nlimbs(x);
 if (n * limb_bits > 64) return false;
 uint64_t m = b->limb[n-1];
 for (int i = n - 2; i >= 0; i--) m = (m << (limb_bits - 1) << 1) | b->limb[i];
 bool neg = b->slen < 0;
 if (m > (uint64_t) INT64_MAX + neg) return false;
 return *v = (int64_t) (neg ? 0 - m : m), true; }
static ai_inline bool ratio_iview(word x, int64_t *n, int64_t *d) {
 if (coinp(x)) { word p = coin_load(x);
  if (!chainp(p) || !chainp(B(p))) return false;
  return ratio_ifit(A(p), n) && ratio_ifit(A(B(p)), d) && *d != 0; }
 return ratio_ifit(x, n) && (*d = 1, true); }
#if !defined(__SIZEOF_INT128__)
// u64 x u64 -> 128-bit magnitude product from 32-bit half-word partials -- the exact
// cross-multiply for builds without __int128 (mooncc's love-raw; the thumb ports).
static ai_inline void ratio_mag_mul(uint64_t a, uint64_t b, uint64_t *hi, uint64_t *lo) {
 uint64_t mask = 0xFFFFFFFFu;
 uint64_t a0 = a & mask, a1 = a >> 32, b0 = b & mask, b1 = b >> 32;
 uint64_t p00 = a0 * b0, p01 = a0 * b1, p10 = a1 * b0, p11 = a1 * b1;
 uint64_t mid = (p00 >> 32) + (p01 & mask) + (p10 & mask);
 *lo = (p00 & mask) | (mid << 32);
 *hi = p11 + (p01 >> 32) + (p10 >> 32) + (mid >> 32); }
#endif
static ai_inline bool ratio_xcmp(int64_t n1, int64_t d1, int64_t n2, int64_t d2, intptr_t *c) {
 intptr_t s = (d1 < 0) != (d2 < 0) ? -1 : 1;
#if defined(__SIZEOF_INT128__)
 __int128 l = (__int128) n1 * d2, r = (__int128) n2 * d1;
 return *c = l == r ? 0 : (l < r ? -s : s), true;
#else
 // exact sign + magnitude: |n1|*|d2| vs |n2|*|d1| as double-word pairs, signs on top.
 uint64_t la = n1 < 0 ? (uint64_t) 0 - (uint64_t) n1 : (uint64_t) n1;
 uint64_t lb = d2 < 0 ? (uint64_t) 0 - (uint64_t) d2 : (uint64_t) d2;
 uint64_t ra = n2 < 0 ? (uint64_t) 0 - (uint64_t) n2 : (uint64_t) n2;
 uint64_t rb = d1 < 0 ? (uint64_t) 0 - (uint64_t) d1 : (uint64_t) d1;
 uint64_t lhi, llo, rhi, rlo;
 ratio_mag_mul(la, lb, &lhi, &llo);
 ratio_mag_mul(ra, rb, &rhi, &rlo);
 bool zl = !(lhi | llo), zr = !(rhi | rlo);
 bool sl = !zl && ((n1 < 0) != (d2 < 0)), sr = !zr && ((n2 < 0) != (d1 < 0));
 intptr_t cl;                                     // -1/0/1 of l - r, signs first then magnitudes
 if (sl != sr) cl = sl ? -1 : 1;
 else { intptr_t cm = lhi != rhi ? (lhi < rhi ? -1 : 1) : llo != rlo ? (llo < rlo ? -1 : 1) : 0;
        cl = sl ? -cm : cm; }
 return *c = cl == 0 ? 0 : (cl < 0 ? -s : s), true;
#endif
}
// 3-way total-order comparator (-1/0/1); the recursive engine for the chain case.
// Floats collapse NaN to "equal" here (a structural total order can't carry IEEE
// unorderedness); the scalar lane below keeps NaN unordered at the top level. hash
// is alloc-free + GC-stable, so the lambda case is safe to call mid-comparison.
static intptr_t cmp3(struct ai *g, word a, word b) {
 int ra = cmp_rank(g, a), rb = cmp_rank(g, b);
 if (ra != rb) return ra < rb ? -1 : 1;                    // cross-kind: the true-blue lattice (cmp_rank)
 // same band -- dispatch by the actual kind (NOT the synthetic cmp_rank, which remaps mint/
 // string/tray/chain off their enum ordinal). symbols first: a named sym IS a chain, so the chain
 // recursion below would otherwise grab it.
 if (nomp(a)) return mint_cmp(g, a, b);                    // mint band: () < bare mints < named syms
 if (ra == 2) {                                            // number band: stars + galaxies, ordered by net
  if (coinp(a) || coinp(b)) {                              // a RATIO coin in the band (cmp_rank read its
   int64_t n1, d1, n2, d2; intptr_t c;                     // mode-2 die): int64-fitting components -> EXACT
   if (ratio_iview(a, &n1, &d1) && ratio_iview(b, &n2, &d2)
       && ratio_xcmp(n1, d1, n2, d2, &c)) return c;
   struct ai_zn za = ai_net(g, a), zb = ai_net(g, b);      // else the sign-exact quotients
   if (za.re != zb.re) return za.re < zb.re ? -1 : 1;
   if (za.im != zb.im) return za.im < zb.im ? -1 : 1;
   if (galaxyp(a) != galaxyp(b)) return galaxyp(a) ? 1 : -1;  // net tie vs a galaxy: the star seats below
   return 0; }
  if (galaxyp(a) || galaxyp(b)) {                          // a galaxy in play -> by net (re, im), then star<galaxy, then shape/content
   bool ga = galaxyp(a), gb = galaxyp(b);
   struct ai_zn na = ai_net(g, a), nb = ai_net(g, b);
   if (na.re != nb.re) return na.re < nb.re ? -1 : 1;
   if (na.im != nb.im) return na.im < nb.im ? -1 : 1;
   if (ga != gb) return ga ? 1 : -1;                       // net tie: a star seats below a galaxy
   return galaxy_tie(tray(a), tray(b)); }                    // both galaxies, equal net: shape then content
  if (twinp(a) || twinp(b)) {                              // both scalars -- complex: (re, im) lexicographic
   ai_flo_t ar = twinp(a) ? twin_re(a) : toflo(a), br = twinp(b) ? twin_re(b) : toflo(b);
   if (ar != br) return ar < br ? -1 : 1;
   ai_flo_t ai = twinp(a) ? twin_im(a) : 0, bi = twinp(b) ? twin_im(b) : 0;
   return ai < bi ? -1 : ai > bi ? 1 : 0; }
  if (gemp(a) || gemp(b)) { ai_flo_t av = toflo(a), bv = toflo(b); return av < bv ? -1 : av > bv ? 1 : 0; }
  return ai_big_cmp(a, b); }                                // exact fix/box/big tower
 if (strp(a)) return bytes_cmp(txt(a), len(a), txt(b), len(b));
 if (chainp(a)) { intptr_t c = cmp3(g, A(a), A(b)); return c ? c : cmp3(g, B(a), B(b)); }  // chain: car, then cdr
 if (coinp(a) && coinp(b) && coin_die(a) == coin_die(b))  // same die: order by payload
  return cmp3(g, coin_load(a), coin_load(b));
 uintptr_t ha = hash(g, a), hb = hash(g, b);               // lambda/map/port/cask: by repr hash
 return ha < hb ? -1 : ha > hb ? 1 : 0; }

// (sort l): stable ascending merge by cmp3 -- one reservation up front (n result
// chains + 2n scratch in the uncommitted gap), and cmp3 is alloc-free, so nothing
// moves between reservation and fill. prel's sort dispatches (<)/(>) here.
// (tally x): THE COUNT -- how many, never how much: a string/cask its charms, a
// list its spine, an array its cells, a map its keys, a symbol its spelling.
static intptr_t ai_count(struct ai *g, word l) {
 while (coinp(l)) l = coin_load(l);                  // a coin tallies its payload
 if (strp(l)) return (intptr_t) len(l);
 if (caskp(l)) return (intptr_t) len(cask(l)->str);
 if (tabp(l)) return (intptr_t) map_len(l);
 if (trayp(l)) return (intptr_t) tray_nelem(tray(l));
 if (nomp(l)) { struct ai_str *nm = nom_str(g, l); return nm ? (intptr_t) len(nm) : 0; }  // a sym counts its spelling; a bare mint / the core: 0
 intptr_t n = 0;
 while (chainp(l)) n++, l = B(l);
 return n; }
lvm(lvm_tally) {
 Sp[0] = putcharm(ai_count(g, Sp[0]));
 ai_musttail return Next(1); }

// (long? n l): is l a chain at least n links deep -- the ARITY question, asked once.
// `two?` per step is the spelling that reads, and it costs a cup, a load and a
// dispatch EACH; a destructuring pattern asks it n times to reach n fields
// (love/pat.l). n <= 0 is true of anything: no link is claimed.
lvm(lvm_longp) { word l = Sp[1];
 if (!charmp(Sp[0])) ai_musttail return Push(zero);
 for (intptr_t k = getcharm(Sp[0]); k > 0; k--) {
  if (!chainp(l) || nomp(l)) ai_musttail return Push(zero);
  l = B(l); }
 ai_musttail return Push(putcharm(1)); }

lvm(lvm_sort) {
 word l = Sp[0];
 if (!chainp(l) || !chainp(B(l))) ai_musttail return Next(1);
 uintptr_t n = 0;
 for (word p = l; chainp(p); p = B(p)) n++;
 uintptr_t req = n * Width(struct ai_chain) + 2 * n;
 Have(req);
 l = Sp[0];                                        // re-read post-GC
 struct ai_chain *spine = (struct ai_chain*) Hp;
 Hp += n * Width(struct ai_chain);                   // commit the spine only
 word *a = (word*) Hp, *b = a + n;                 // scratch: the uncommitted gap
 uintptr_t i = 0;
 for (word p = l; chainp(p); p = B(p)) a[i++] = A(p);
 for (i = 0; i < n; i++) if (!charmp(a[i])) break;   // all-fixnum FAST PATH: a tagged fixnum (v<<1|1)
 bool allfix = i == n;                               // orders as a signed word, so skip the generic cmp3
 for (uintptr_t w = 1; w < n; w *= 2) {            // bottom-up stable merge
  for (uintptr_t lo = 0; lo < n; lo += 2 * w) {
   uintptr_t m = min(lo + w, n), hi = min(lo + 2 * w, n), x = lo, y = m, o = lo;
   if (allfix) while (x < m && y < hi) b[o++] = (intptr_t) a[y] < (intptr_t) a[x] ? a[y++] : a[x++];   // branch ONCE per segment, not per compare
   else        while (x < m && y < hi) b[o++] = cmp3(g, a[y], a[x]) < 0 ? a[y++] : a[x++];
   while (x < m) b[o++] = a[x++];
   while (y < hi) b[o++] = a[y++]; }
  word *t = a; a = b; b = t; }
 for (i = 0; i < n; i++) ini_chain(spine + i, a[i], word(spine + i + 1));
 spine[n - 1].b = ZeroPoint;                        // () terminator (zero-ontology)
 ai_musttail return Answer(word(spine)); }

// the `<` / `<=` lane (op is vop_lt or vop_le). An array operand -> elementwise
// mask (lvm_vbin); a top-level float/complex chain is IEEE-faithful (NaN ->
// unordered -> false), so e.g. (<= nan nan) is zero.
static lvm(lvm_cmp_ord, int op) {
 word a = Sp[0], b = Sp[1]; intptr_t r;
 if (trayp(a) || trayp(b)) return Ap(lvm_vbin, g, op);      // array -> elementwise
 int ra = cmp_rank(g, a), rb = cmp_rank(g, b);
 if (ra != rb) r = vcmp_int(op, ra, rb);                   // cross-kind: the true-blue lattice (cmp_rank)
 else if (!(isnum(a) || twinp(a)) || coinp(b)) r = vcmp_int(op, cmp3(g, a, b), 0);  // same non-number band, or a ratio coin either side (a coin as `a` fails isnum; as `b` this catches it): via cmp3
 else if (twinp(a) || twinp(b)) {                          // complex: lexicographic, per op
  ai_flo_t ar = twinp(a) ? twin_re(a) : toflo(a), br = twinp(b) ? twin_re(b) : toflo(b);
  r = ar != br ? vcmp_flo(op, ar, br)
              : vcmp_flo(op, twinp(a) ? twin_im(a) : 0, twinp(b) ? twin_im(b) : 0); }
 else if (gemp(a) || gemp(b)) r = vcmp_flo(op, toflo(a), toflo(b));
 else if (bigp(a) || bigp(b)) r = vcmp_int(op, ai_big_cmp(a, b), 0);
 else r = vcmp_int(op, toint(a), toint(b));
 return Push(r ? putcharm(1) : zero); }
// `<` `<=` are the implemented side (both-fixnum fast path: tagged order is
// monotonic); `>` `>=` reverse the operands. COND FUSION: when the fast path sees
// lvm_cond next it branches DIRECTLY (true -> Ip+3, false -> Ip[2].m) instead of
// materializing a boolean and paying a second dispatch; the slow path falls
// through to the retained lvm_cond. the gt/ge reversers fuse for free.
#define cmp_lt(nom, vop) lvm(nom) { \
 word a = Sp[0], b = Sp[1]; \
 if (__builtin_expect(charmp(a) && charmp(b), 1)) { \
  intptr_t r = vcmp_int(vop, a, b); \
  if (Ip[1].ap == lvm_cond) { Sp += 2; Ip = r ? Ip + 3 : Ip[2].m; ai_musttail return Continue(); } \
  ai_musttail return Push(r ? putcharm(1) : zero); } \
 return Ap(lvm_cmp_ord, g, vop); }
cmp_lt(lvm_lt, vop_lt) cmp_lt(lvm_le, vop_le)
#undef cmp_lt
lvm(lvm_gt) { word t = Sp[0]; Sp[0] = Sp[1], Sp[1] = t; ai_musttail return Ap(lvm_lt, g); }  // a > b == b < a
lvm(lvm_ge) { word t = Sp[0]; Sp[0] = Sp[1], Sp[1] = t; ai_musttail return Ap(lvm_le, g); }  // a >= b == b <= a

// comparison from a 3-way sign: a bignum is always out of machine-int range, so
// it orders against any int element by its sign alone -- exactly
static intptr_t vcmp_sign(int op, int s) {
 switch (op) {
  case vop_lt: return s < 0; case vop_le: return s <= 0;
  case vop_gt: return s > 0; case vop_ge: return s >= 0;
  default: return s == 0; } }                   // vop_eq

// the broadcast dim: a size-1 axis takes the OTHER size -- including 0, so an
// empty axis stays empty (a max would fill one element out of an empty operand)
static ai_inline uintptr_t bdim(uintptr_t da, uintptr_t db) {
 return da == 1 ? db : db == 1 ? da : da; }

// the broadcast plumbing shared by every elementwise lane; the shape walk runs
// twice (gate before Have, fill after), re-deriving each time (operands move).
// bshape_n: the broadcast element count, or (uintptr_t) -1 on non-conformance.
static uintptr_t bshape_n(word a, word b) {
 bool atray = trayp(a), btray = trayp(b);
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0;
 uintptr_t R = ra > rb ? ra : rb, n = 1;
 for (uintptr_t k = 0; k < R; k++) {
  uintptr_t da = (atray && k < ra) ? tray(a)->shape[ra - 1 - k] : 1;
  uintptr_t db = (btray && k < rb) ? tray(b)->shape[rb - 1 - k] : 1;
  if (da != db && da != 1 && db != 1) return (uintptr_t) -1;
  n *= bdim(da, db); }
 return n; }

// Fill shape[0..R) with the broadcast shape of a and b (conformance already
// gated by bshape_n).
static void bshape_put(uintptr_t *shape, uintptr_t R, word a, word b) {
 bool atray = trayp(a), btray = trayp(b);
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0;
 for (uintptr_t k = 0; k < R; k++) {
  uintptr_t da = (atray && k < ra) ? tray(a)->shape[ra - 1 - k] : 1;
  uintptr_t db = (btray && k < rb) ? tray(b)->shape[rb - 1 - k] : 1;
  shape[R - 1 - k] = bdim(da, db); } }

// c[j]: the operand's flat-offset contribution of result axis j (0 when that
// axis is absent in the operand or is a size-1 broadcast axis); v == 0 reads
// a scalar operand (all zeros).
static void bstride(struct ai_tray *v, uintptr_t R, intptr_t *c) {
 for (uintptr_t j = 0; j < R; j++) c[j] = 0;
 if (!v) return;
 intptr_t s = 1;
 for (intptr_t o = (intptr_t) v->rank - 1; o >= 0; o--) {
  intptr_t j = o + (intptr_t) R - (intptr_t) v->rank;
  c[j] = v->shape[o] == 1 ? 0 : s, s *= (intptr_t) v->shape[o]; } }

// One odometer tick over shape[0..R), rightmost axis fastest.
static ai_inline void odo_step(intptr_t *idx, uintptr_t R, uintptr_t const *shape) {
 for (intptr_t j = (intptr_t) R - 1; j >= 0; j--) {
  if (++idx[j] < (intptr_t) shape[j]) break;
  idx[j] = 0; } }

// Fill the (already-shaped) result r with a `op` b, broadcasting. All the
// &-taking stack arrays (strides, odometer) live here so the lvm wrapper stays
// TCO-clean. No allocation inside, so operand pointers can't move under us.
static ai_noinline void vbin_fill(struct ai_tray *r, word a, word b, int op, bool fdom) {
 uintptr_t R = r->rank, n = tray_nelem(r);
 bool atray = trayp(a), btray = trayp(b);
 struct ai_tray *va = atray ? tray(a) : 0, *vb = btray ? tray(b) : 0;
 // CONTIGUOUS MONOTYPE FAST PATH: no broadcasting, so the odometer and dispatch
 // vanish -- raw pointers, the op hoisted once, a body the compiler vectorizes.
 // mixed/bignum/broadcast falls through to the general loop; results bit-identical.
 { bool cmpf = op >= vop_lt;
   bool aok = !atray || tray_nelem(va) == n, bok = !btray || tray_nelem(vb) == n;
   bool nobig = !((!atray && bigp(a)) || (!btray && bigp(b)));
   if (aok && bok && nobig) {
    if (fdom && (!atray || va->type == ai_R) && (!btray || vb->type == ai_R)) {
     ai_flo_t sa = atray ? 0 : toflo(a), sb = btray ? 0 : toflo(b);
     ai_flo_t *ap = atray ? (ai_flo_t*) tray_data(va) : 0, *bp = btray ? (ai_flo_t*) tray_data(vb) : 0;
     if (cmpf) { intptr_t *rp = (intptr_t*) tray_data(r);
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { ai_flo_t av = atray?ap[p]:sa, bv = btray?bp[p]:sb; rp[p] = (E)?1:0; } } while (0)
      switch (op) { case vop_lt: VBF(av<bv); return; case vop_le: VBF(av<=bv); return;
        case vop_gt: VBF(av>bv); return; case vop_ge: VBF(av>=bv); return; case vop_eq: VBF(av==bv); return; }
      #undef VBF
     } else { ai_flo_t *rp = (ai_flo_t*) tray_data(r);
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { ai_flo_t av = atray?ap[p]:sa, bv = btray?bp[p]:sb; rp[p] = (E); } } while (0)
      switch (op) { case vop_add: VBF(av+bv); return; case vop_sub: VBF(av-bv); return;
        case vop_mul: VBF(av*bv); return; case vop_quot: VBF(av/bv); return;
        case vop_fquot: VBF(ai_trunc(av/bv)); return; case vop_rem: VBF(ai_fmod(av,bv)); return; }
      #undef VBF
     }
    } else if (!fdom && (!atray || va->type == ai_Z) && (!btray || vb->type == ai_Z)) {
     intptr_t sia = atray ? 0 : (charmp(a) ? (intptr_t) getcharm(a) : sun_get(a));
     intptr_t sib = btray ? 0 : (charmp(b) ? (intptr_t) getcharm(b) : sun_get(b));
     intptr_t *ap = atray ? (intptr_t*) tray_data(va) : 0, *bp = btray ? (intptr_t*) tray_data(vb) : 0;
     intptr_t *rp = (intptr_t*) tray_data(r);   // r is ai_Z for both int-arith and the mask
     if (cmpf) {
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { intptr_t av = atray?ap[p]:sia, bv = btray?bp[p]:sib; rp[p] = (E)?1:0; } } while (0)
      switch (op) { case vop_lt: VBF(av<bv); return; case vop_le: VBF(av<=bv); return;
        case vop_gt: VBF(av>bv); return; case vop_ge: VBF(av>=bv); return; case vop_eq: VBF(av==bv); return; }
      #undef VBF
     } else {
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { intptr_t av = atray?ap[p]:sia, bv = btray?bp[p]:sib; rp[p] = (E); } } while (0)
      switch (op) {
        case vop_add: VBF((intptr_t)((uintptr_t)av+(uintptr_t)bv)); return;
        case vop_sub: VBF((intptr_t)((uintptr_t)av-(uintptr_t)bv)); return;
        case vop_mul: VBF((intptr_t)((uintptr_t)av*(uintptr_t)bv)); return;
        case vop_quot: case vop_fquot: VBF((bv==0||(av==INTPTR_MIN&&bv==-1))?0:av/bv); return;
        case vop_rem: VBF((bv==0||(av==INTPTR_MIN&&bv==-1))?0:av%bv); return; }
      #undef VBF
     } } } }
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 bool cmp = op >= vop_lt;
 // the int domain demotes a bignum scalar by low bits for arithmetic, but a
 // COMPARISON against one is decided exactly by its sign below
 ai_flo_t sa = atray ? 0 : toflo(a), sb = btray ? 0 : toflo(b);
 intptr_t ia = atray ? 0 : charmp(a) ? getcharm(a) : bigp(a) ? ai_big_low(a) : sun_get(a),
          ib = btray ? 0 : charmp(b) ? getcharm(b) : bigp(b) ? ai_big_low(b) : sun_get(b);
 bool abig = !atray && bigp(a), bbig = !btray && bigp(b);   // at most one (the other is an array)
 int asign = abig ? (big(a)->slen < 0 ? -1 : 1) : 0,
     bsign = bbig ? (big(b)->slen < 0 ? -1 : 1) : 0;
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  if (fdom) {
   ai_flo_t av = atray ? tray_get_flo(va, oa) : sa, bv = btray ? tray_get_flo(vb, ob) : sb;
   if (cmp) tray_put_int(r, p, vcmp_flo(op, av, bv) ? 1 : 0);
   else tray_put_flo(r, p, vop_flo(op, av, bv)); }
  else {
   intptr_t av = atray ? tray_get_int(va, oa) : ia, bv = btray ? tray_get_int(vb, ob) : ib;
   if (cmp) {                                    // bignum side (if any) sorts by sign: a-b ~ asign, or -bsign
    intptr_t t = (abig || bbig) ? vcmp_sign(op, abig ? asign : -bsign) : vcmp_int(op, av, bv);
    tray_put_int(r, p, t ? 1 : 0); }
   else tray_put_int(r, p, vop_int(op, av, bv)); }
  odo_step(idx, R, r->shape); } }

// `/` over the integer domain: true if some element divides inexactly, so the
// whole result promotes to f64 (a bignum scalar forces the float lane). called
// only after conformance is checked.
static ai_noinline bool vquot_needs_float(word a, word b) {
 bool atray = trayp(a), btray = trayp(b);
 if ((!atray && bigp(a)) || (!btray && bigp(b))) return true;
 struct ai_tray *va = atray ? tray(a) : 0, *vb = btray ? tray(b) : 0;
 uintptr_t ra = atray ? va->rank : 0,
           rb = btray ? vb->rank : 0,
           R = ra > rb ? ra : rb,
           n = bshape_n(a, b), shp[maxrank];
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 bshape_put(shp, R, a, b);
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 intptr_t ia = atray ? 0 : toint(a), ib = btray ? 0 : toint(b);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  intptr_t av = atray ? tray_get_int(va, oa) : ia, bv = btray ? tray_get_int(vb, ob) : ib;
  if (bv == 0 || av % bv != 0) return true;
  odo_step(idx, R, shp); }
 return false; }

lvm(lvm_vbin, int op) {
 word a = Sp[0], b = Sp[1];
 bool atray = trayp(a), btray = trayp(b);
 // complex lane first (a complex scalar isn't isnum, so it must divert before
 // the gate below); mixing ai_C with ai_O is unsupported -- the ai_O lane wins
 if (((atray && tray(a)->type == ai_C) || (btray && tray(b)->type == ai_C) || twinp(a) || twinp(b))
     && !(atray && tray(a)->type == ai_O) && !(btray && tray(b)->type == ai_O)) {
  if (vop_bitp(op)) return Push(ZeroPoint);   // no bits on a complex
  return Ap(lvm_cbin, g, op); }
 if (!(atray || isnum(a)) || !(btray || isnum(b)))   // each operand: array or scalar
  return Push(op == vop_eq ? zero : ZeroPoint);   // `=` is boolean: undefined face -> 0, not ()
 if ((atray && tray(a)->type == ai_O) || (btray && tray(b)->type == ai_O)) {
  // boxed cells are NOT the word lane: a big refuses the bits on a star, so the
  // object tray refuses them whole rather than answering per-element zero.
  if (vop_bitp(op)) return Push(ZeroPoint);
  return Ap(lvm_obin, g, op); }                   // object array -> promoting lane
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0;
 uintptr_t R = ra > rb ? ra : rb;
 // compute-type = max element type; a scalar int contributes the lowest type
 // (i8) so it never widens an int array, a scalar float forces the float lane.
 int ta = atray ? (int) tray(a)->type : gemp(a) ? (int) ai_R : (int) ai_Z;
 int tb = btray ? (int) tray(b)->type : gemp(b) ? (int) ai_R : (int) ai_Z;
 int ct = ta > tb ? ta : tb;
 bool fdom = ct >= ai_R, cmp = op >= vop_lt;
 if (vop_bitp(op) && fdom) return Push(ZeroPoint);   // no bits on a gem
 uintptr_t n = bshape_n(a, b);                     // conformance + result size
 if (n == (uintptr_t) -1) return Push(op == vop_eq ? zero : ZeroPoint);   // non-conformant `=` -> 0
 // `/` over an all-integer broadcast promotes the whole result to f64 the moment
 // any element divides inexactly (matching the scalar `/`); `//` (vop_fquot) stays
 // integer. Sound only after conformance is known good (offsets are then in range).
 if (op == vop_quot && !fdom && !cmp && vquot_needs_float(a, b)) fdom = true, ct = ai_R;
 enum ai_tray_type rt = cmp ? ai_Z : (enum ai_tray_type) ct;   // compare -> 0/1 Z mask
 uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[rt];
 Have(b2w(bytes));
 a = Sp[0], b = Sp[1];                                       // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, rt, R);
 bshape_put(r->shape, R, a, b);
 vbin_fill(r, a, b, op, fdom);
 return Push(word(r)); }

// --- dyadic math map with broadcasting (pow / atan2 over arrays): lvm_vbin's
// float-domain twin -- the result is always a float array, each element fn(av, bv)
static ai_noinline void vmap2_fill(struct ai_tray *r, word a, word b, ai_flo_t (*fn)(ai_flo_t, ai_flo_t)) {
 uintptr_t R = r->rank, n = tray_nelem(r);
 bool atray = trayp(a), btray = trayp(b);
 struct ai_tray *va = atray ? tray(a) : 0, *vb = btray ? tray(b) : 0;
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 ai_flo_t sa = atray ? 0 : toflo(a), sb = btray ? 0 : toflo(b);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  ai_flo_t av = atray ? tray_get_flo(va, oa) : sa, bv = btray ? tray_get_flo(vb, ob) : sb;
  tray_put_flo(r, p, fn(av, bv));
  odo_step(idx, R, r->shape); } }

lvm(lvm_vmap2, ai_flo_t (*fn)(ai_flo_t, ai_flo_t)) {
 word a = Sp[0], b = Sp[1];
 bool atray = trayp(a), btray = trayp(b);
 if (!(atray || isnum(a)) || !(btray || isnum(b)))   // each operand: array or scalar
  return Push(ZeroPoint);
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0;
 uintptr_t R = ra > rb ? ra : rb, n = bshape_n(a, b);
 if (n == (uintptr_t) -1) return Push(ZeroPoint);
 uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[ai_R];
 Have(b2w(bytes));
 a = Sp[0], b = Sp[1];                                       // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, ai_R, R);
 bshape_put(r->shape, R, a, b);
 vmap2_fill(r, a, b, fn);
 return Push(word(r)); }

// ============================================================================
// obin -- object-array elementwise lane (ai_O)
// ============================================================================
// the typed lanes wrap on overflow; the object lane routes every element through
// the promoting scalar dispatch, so a ai_O array adds/multiplies EXACTLY. the
// inner loop allocates, so it runs Pack'd and re-fetches every live pointer.

// one element op, allocating via *fp; zero for a non-numeric/complex operand
static word obin_elem(struct ai **fp, int op, word a, word b) {
 if (op >= vop_lt) {                            // comparison -> 1 / zero, no allocation
  if (!isnum(a) || !isnum(b)) return zero;       // twinp not in isnum -> unordered -> zero
  intptr_t t = (gemp(a) || gemp(b)) ? vcmp_flo(op, toflo(a), toflo(b))
             : (bigp(a) || bigp(b)) ? vcmp_int(op, ai_big_cmp(a, b), 0)
                                    : vcmp_int(op, toint(a), toint(b));
  return t ? putcharm(1) : zero; }
 if (!isnum(a) || !isnum(b)) return zero;
 struct ai *g = *fp;
 if (gemp(a) || gemp(b)) {                      // float domain -> float box
  ai_flo_t r = vop_flo(op, toflo(a), toflo(b));  // ⚠ BOTH OPERANDS READ FIRST: a/b are raw words
  if (!ai_ok(g = ai_have(g, gem_req))) return *fp = g, zero;   // and a float box is a heap object, so
  *fp = g;                                                    // toflo after the have reads a moved one
  return mk_gem(&g->hp, r); }
 if (!bigp(a) && !bigp(b)) {                    // machine-int fast path, overflow-checked
  intptr_t av = toint(a), bv = toint(b), t; bool of;
  switch (op) {
   case vop_quot: case vop_fquot:                         // object (ai_O) arrays truncate under both / and //
                  if (bv == 0) return putcharm(0);          // array convention: int /0 -> 0
                  of = (av == INTPTR_MIN && bv == -1); t = of ? 0 : av / bv; break;
   case vop_rem:  if (bv == 0) return putcharm(0);
                  of = (av == INTPTR_MIN && bv == -1); t = of ? 0 : av % bv; break;
   case vop_sub:  of = __builtin_sub_overflow(av, bv, &t); break;
   case vop_mul:  of = __builtin_mul_overflow(av, bv, &t); break;
   default:       of = __builtin_add_overflow(av, bv, &t); break; }   // vop_add
  if (!of) {                                    // demote-or-box the result
   if (t >= mincharm && t <= maxcharm) return putcharm(t);
   if (!ai_ok(g = ai_have(g, sun_req))) return *fp = g, zero;
   *fp = g;
   return mk_sun(&g->hp, t); } }
 // bignum lane: ai_big_binop computes sp[0] (op) sp[1], leaves it at sp[1],
 // pops one, and advances ip -- so save/restore ip and pop the net result.
 if (!ai_ok(g = ai_push(g, 2, a, b))) return *fp = g, zero;
 union u *ip0 = g->ip;
 avec(g, ip0, g = ai_big_binop(g, op));
 if (!ai_ok(g)) return *fp = g, zero;
 g->ip = ip0;
 word r = g->sp[0]; g->sp++;
 return *fp = g, r; }

// widen the numeric array at g->sp[slot] to a ai_O copy (box each element);
// allocates per element, everything re-fetched after every box
static struct ai *tray_to_obj(struct ai *g, int slot) {
 struct ai_tray *src = tray(g->sp[slot]);
 uintptr_t R = src->rank, n = 1;
 for (uintptr_t i = 0; i < R; i++) n *= src->shape[i];
 uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[ai_O];
 if (!ai_ok(g = ai_have(g, b2w(bytes)))) return g;
 src = tray(g->sp[slot]);
 struct ai_tray *dst = (struct ai_tray*) g->hp; g->hp += b2w(bytes);
 ini_tray(dst, ai_O, R);
 for (uintptr_t i = 0; i < R; i++) dst->shape[i] = src->shape[i];
 for (uintptr_t i = 0; i < n; i++) tray_put_obj(dst, i, zero);   // safe pre-fill (GC may see it)
 if (!ai_ok(g = ai_push(g, 1, word(dst)))) return g;             // sp[0]=dst, src now at slot+1
 for (uintptr_t i = 0; i < n; i++) {
  struct ai_tray *s = tray(g->sp[slot + 1]);
  word v;
  if (s->type >= ai_R) {                                        // float -> float box
   ai_flo_t e = tray_get_flo(s, i);
   if (!ai_ok(g = ai_have(g, gem_req))) return g;
   v = mk_gem(&g->hp, e); }
  else {                                                       // int -> fixnum or sun box
   intptr_t e = tray_get_int(s, i);
   if (e >= mincharm && e <= maxcharm) v = putcharm(e);
   else { if (!ai_ok(g = ai_have(g, sun_req))) return g;
    v = mk_sun(&g->hp, e); } }
  tray_put_obj(tray(g->sp[0]), i, v);                            // re-fetch dst post-box
  gen_wb(g, g->sp[0], v); }                                    // ... and BARRIER it: see obin_run
 word d = g->sp[0]; g->sp++; g->sp[slot] = d;                  // install copy, drop the parked root
 return g; }

// Pack'd body of lvm_obin (operands at g->sp[0..1], >=1 is a ai_O array).
static struct ai *obin_run(struct ai *g, int op) {
 word a = g->sp[0], b = g->sp[1];
 bool atray = trayp(a), btray = trayp(b);
 if (atray && tray(a)->type != ai_O) { if (!ai_ok(g = tray_to_obj(g, 0))) return g; }
 if (btray && tray(b)->type != ai_O) { if (!ai_ok(g = tray_to_obj(g, 1))) return g; }
 a = g->sp[0], b = g->sp[1], atray = trayp(a), btray = trayp(b);
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0;
 uintptr_t R = ra > rb ? ra : rb, n = bshape_n(a, b), shp[maxrank];
 if (n == (uintptr_t) -1) {                                    // non-conforming -> zero
  g->sp[1] = zero, g->sp++, g->ip = (union u*) g->ip + 1; return g; }
 bshape_put(shp, R, a, b);
 uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[ai_O];
 if (!ai_ok(g = ai_have(g, b2w(bytes)))) return g;
 struct ai_tray *r = (struct ai_tray*) g->hp; g->hp += b2w(bytes);
 ini_tray(r, ai_O, R);
 for (uintptr_t k = 0; k < R; k++) r->shape[k] = shp[k];
 for (uintptr_t p = 0; p < n; p++) tray_put_obj(r, p, zero);     // zero-fill before any GC
 if (!ai_ok(g = ai_push(g, 1, word(r)))) return g;               // sp: [0]=r [1]=a [2]=b
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(atray ? tray(g->sp[1]) : 0, R, ca), bstride(btray ? tray(g->sp[2]) : 0, R, cb);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  word ae = atray ? tray_get_obj(tray(g->sp[1]), oa) : g->sp[1];  // scalar operand re-read each step
  word be = btray ? tray_get_obj(tray(g->sp[2]), ob) : g->sp[2];
  word res = obin_elem(&g, op, ae, be);
  if (!ai_ok(g)) return g;
  tray_put_obj(tray(g->sp[0]), p, res);                          // re-fetch result post-alloc
  // ⚠ and BARRIER it: a minor mid-loop promotes the result array while its
  // elements stay young -- an edge the rem set must carry, or the next minor
  // frees an element still in the array
  gen_wb(g, g->sp[0], res);
  odo_step(idx, R, shp); }
 word result = g->sp[0];                                       // collapse [r,a,b] -> r, advance ip
 g->sp += 2, g->sp[0] = result, g->ip = (union u*) g->ip + 1;
 return g; }

lvm(lvm_obin, int op) {
 Pack(g);
 g = obin_run(g, op);
 if (!ai_ok(g)) return Ap(_lvm_ghelp, g);
 return Resume(); }

// ai_O reduction body (kind: 0 sum, 1 prod, 2 max, 3 min). g->sp[0] is the array.
static struct ai *ored(struct ai *g, int kind) {
 struct ai_tray *v = tray(g->sp[0]);
 uintptr_t n = 1; for (uintptr_t i = 0; i < v->rank; i++) n *= v->shape[i];
 if (kind >= 2) {                                              // max/min: pick an element, no alloc
  if (!n) { g->sp[0] = zero, g->ip = (union u*) g->ip + 1; return g; }
  word acc = tray_get_obj(tray(g->sp[0]), 0);
  int cop = kind == 2 ? vop_gt : vop_lt;
  for (uintptr_t i = 1; i < n; i++) {
   word e = tray_get_obj(tray(g->sp[0]), i);
   if (obin_elem(&g, cop, e, acc) == putcharm(1)) acc = e; }
  g->sp[0] = acc, g->ip = (union u*) g->ip + 1; return g; }
 word init = kind == 0 ? putcharm(0) : putcharm(1);               // sum/prod: fold with allocation
 int aop = kind == 0 ? vop_add : vop_mul;
 if (!ai_ok(g = ai_push(g, 1, init))) return g;                 // sp[0]=acc, sp[1]=array
 for (uintptr_t i = 0; i < n; i++) {
  word e = tray_get_obj(tray(g->sp[1]), i);
  word acc = obin_elem(&g, aop, g->sp[0], e);
  if (!ai_ok(g)) return g;
  g->sp[0] = acc; }
 word result = g->sp[0]; g->sp++, g->sp[0] = result;          // collapse acc into the array slot
 g->ip = (union u*) g->ip + 1;
 return g; }

// (re, im) of an operand: a complex its parts, a real (value, 0); caller
// guarantees twinp or isnum
static ai_inline void twin_parts(word x, ai_flo_t *re, ai_flo_t *im) {
 if (twinp(x)) *re = twin_re(x), *im = twin_im(x);
 else *re = toflo(x), *im = 0; }

// (ar,ai) `vop` (br,bi) in components: the one set of complex formulas, shared
// by the scalar lane (twin_fill) and the packed array lane (cbin_fill).
static ai_inline void twin_op(int vop, ai_flo_t ar, ai_flo_t ai, ai_flo_t br, ai_flo_t bi,
                             ai_flo_t *re, ai_flo_t *im) {
 switch (vop) {
  case vop_sub: *re = ar - br; *im = ai - bi; break;
  case vop_mul: *re = ar * br - ai * bi; *im = ar * bi + ai * br; break;
  case vop_quot: { ai_flo_t d = br * br + bi * bi;   // (ac+bd)/(c^2+d^2) + ...
   *re = (ar * br + ai * bi) / d; *im = (ai * br - ar * bi) / d; break; }
  default: *re = ar + br; *im = ai + bi; } }          // vop_add

// fill the complex box with a `vop` b; the &-taking lives here (the wrapper's tail call)
static ai_noinline void twin_fill(struct ai_twin *v, word a, word b, int vop) {
 ai_flo_t ar, ai, br, bi, re, im;
 twin_parts(a, &ar, &ai); twin_parts(b, &br, &bi);
 twin_op(vop, ar, ai, br, bi, &re, &im);
 twin_set(v, re, im); }

// the complex arithmetic lane: a real operand promotes to (r, 0); non-numeric,
// or % (undefined on complex), yields zero
lvm(lvm_twin_bin, int vop) {
 word a = Sp[0], b = Sp[1];
 if (!(twinp(a) || isnum(a)) || !(twinp(b) || isnum(b)) || vop > vop_quot)
  return Push(ZeroPoint);
 Have(twin_req);
 a = Sp[0], b = Sp[1];                              // re-read post-Have
 struct ai_twin *v = (struct ai_twin*) Hp; v->ap = lvm_twinbox; Hp += twin_req;
 twin_fill(v, a, b, vop);
 return Push(word(v)); }

// --- complex-array elementwise lane (ai_C): lvm_vbin's complex twin -- packed
// (re,im) broadcast, a real element promoting to (v, 0)
static ai_inline void cbin_part(bool istray, struct ai_tray *v, ai_flo_t sre, ai_flo_t sim,
                               uintptr_t o, ai_flo_t *re, ai_flo_t *im) {
 if (!istray) { *re = sre; *im = sim; return; }
 if (v->type == ai_C) { ai_flo_t *fp = tray_data(v); *re = fp[2*o]; *im = fp[2*o+1]; }
 else { *re = tray_get_flo(v, o); *im = 0; } }

static ai_noinline void cbin_fill(struct ai_tray *r, word a, word b, int op, bool cmp) {
 uintptr_t R = r->rank, n = tray_nelem(r);
 bool atray = trayp(a), btray = trayp(b);
 struct ai_tray *va = atray ? tray(a) : 0, *vb = btray ? tray(b) : 0;
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 ai_flo_t sar = 0, sai = 0, sbr = 0, sbi = 0;
 if (!atray) { if (twinp(a)) sar = twin_re(a), sai = twin_im(a); else sar = toflo(a); }
 if (!btray) { if (twinp(b)) sbr = twin_re(b), sbi = twin_im(b); else sbr = toflo(b); }
 ai_flo_t *rf = cmp ? 0 : tray_data(r);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  ai_flo_t ar, ai, br, bi, re, im;
  cbin_part(atray, va, sar, sai, oa, &ar, &ai);
  cbin_part(btray, vb, sbr, sbi, ob, &br, &bi);
  if (cmp) {                                   // (re,im) LEXICOGRAPHIC -- the same order
   int t;                                      // cmp3's complex arm gives a scalar pair
   if (op == vop_eq) t = ar == br && ai == bi;     // kept exact (a NaN is equal to nothing)
   else {
    int c = ar < br ? -1 : ar > br ? 1 : ai < bi ? -1 : ai > bi ? 1 : 0;
    t = op == vop_lt ? c < 0 : op == vop_le ? c <= 0
      : op == vop_gt ? c > 0 : c >= 0; }        // vop_ge
   tray_put_int(r, p, t ? 1 : 0); }
  else {
   twin_op(op, ar, ai, br, bi, &re, &im);
   rf[2*p] = re; rf[2*p+1] = im; }
  odo_step(idx, R, r->shape); } }

lvm(lvm_cbin, int op) {
 word a = Sp[0], b = Sp[1];
 bool atray = trayp(a), btray = trayp(b);
 // % and // stay undefined on complex, but the ORDERINGS hold ((re,im)
 // lexicographic): a tray follows its scalar
 if (!(atray || twinp(a) || isnum(a)) || !(btray || twinp(b) || isnum(b))
     || op == vop_rem || op == vop_fquot)
  return Push(op == vop_eq ? zero : ZeroPoint);   // `=` is boolean: undefined face -> 0, not ()
 bool cmp = op >= vop_lt;
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0;
 uintptr_t R = ra > rb ? ra : rb, n = bshape_n(a, b);
 if (n == (uintptr_t) -1) return Push(op == vop_eq ? zero : ZeroPoint);   // non-conformant `=` -> 0
 enum ai_tray_type rt = cmp ? ai_Z : ai_C;              // compare -> i64 mask, else packed complex
 uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[rt];
 Have(b2w(bytes));
 a = Sp[0], b = Sp[1];                                 // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, rt, R);
 bshape_put(r->shape, R, a, b);
 cbin_fill(r, a, b, op, cmp);
 return Push(word(r)); }

// w ** z via the principal branch: exp(z * Log w); w == 0 falls out as the IEEE limit
static ai_noinline void twin_pow_fill(struct ai_twin *v, word wbase, word zexp) {
 ai_flo_t wr, wi, zr, zi;
 twin_parts(wbase, &wr, &wi); twin_parts(zexp, &zr, &zi);
 ai_flo_t lr = (ai_flo_t) 0.5 * ai_log(wr * wr + wi * wi),    // ln|w|
         li = ai_atan2(wi, wr);                             // arg w
 ai_flo_t pr = zr * lr - zi * li, pi = zr * li + zi * lr,   // z * Log w
         e = ai_exp(pr);
 twin_set(v, e * ai_cos(pi), e * ai_sin(pi)); }

// sin/cos of pi*x, the angle reduced BEFORE multiplying by pi so a half-integer
// lands exactly on the axis -- what makes ((/ 1 2) -1) = i bit-exact
static ai_flo_t ai_sinpi(ai_flo_t x) {
 intptr_t n = (intptr_t) x; ai_flo_t r = x - (ai_flo_t) n;
 if (r < 0) r += 1, n--;                              // x = n + r, r in (0,1)
 ai_flo_t s = r == (ai_flo_t) 0.5 ? 1
   : ai_sin((ai_flo_t) 3.141592653589793 * (r < (ai_flo_t) 0.5 ? r : 1 - r));
 return n & 1 ? -s : s; }
static ai_flo_t ai_cospi(ai_flo_t x) {
 intptr_t n = (intptr_t) x; ai_flo_t r = x - (ai_flo_t) n;
 if (r < 0) r += 1, n--;
 ai_flo_t c = r == (ai_flo_t) 0.5 ? 0
   : r < (ai_flo_t) 0.5 ? ai_cos((ai_flo_t) 3.141592653589793 * r)
   : -ai_cos((ai_flo_t) 3.141592653589793 * (1 - r));
 return n & 1 ? -c : c; }
// finite non-integer? everything at/past 2^mantissa is an integer; nan/inf out.
static ai_inline bool flo_fracp(ai_flo_t x) {
 ai_flo_t lim = (ai_flo_t) (1ull << (Bits == 64 ? 53 : 24));
 return x > -lim && x < lim && (ai_flo_t) (intptr_t) x != x; }

// (power b e): complex operands take the complex lane; a finite negative real
// base to a non-integer power widens to its principal root instead of nan (pow
// climbs tiers like log). everything else keeps the IEEE real lanes.
lvm(lvm_pow) {
 word a = Sp[0], b = Sp[1];
 if (twinp(a) || twinp(b)) {
  if (!(twinp(a) || isnum(a)) || !(twinp(b) || isnum(b)))
   ai_musttail return Push(ZeroPoint);
  Have(twin_req);
  a = Sp[0], b = Sp[1];                              // re-read post-Have
  struct ai_twin *v = (struct ai_twin*) Hp;
  Hp += twin_req;
  v->ap = lvm_twinbox;
  twin_pow_fill(v, a, b);
  ai_musttail return Push(word(v)); }
 if (isnum(a) && isnum(b)) {
  ai_flo_t ad = toflo(a), bd = toflo(b);
  if (ad < 0 && !__builtin_isinf(ad) && flo_fracp(bd)) {
   ai_flo_t m = ai_pow(-ad, bd), re = m * ai_cospi(bd), im = m * ai_sinpi(bd);
   Have(twin_req);
   *++Sp = mk_twin(&Hp, re, im); ai_musttail return Next(1); } }
 return Ap(lvm_math2, g, ai_pow); }

// fill a packed ai_C array with (re = a-element, im = b-element) under broadcast
static ai_noinline void twin_build_fill(struct ai_tray *r, word a, word b) {
 uintptr_t R = r->rank, n = tray_nelem(r);
 bool atray = trayp(a), btray = trayp(b);
 struct ai_tray *va = atray ? tray(a) : 0, *vb = btray ? tray(b) : 0;
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 ai_flo_t sa = atray ? 0 : toflo(a), sb = btray ? 0 : toflo(b),
          *rf = tray_data(r);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  rf[2*p]   = atray ? tray_get_flo(va, oa) : sa;
  rf[2*p+1] = btray ? tray_get_flo(vb, ob) : sb;
  odo_step(idx, R, r->shape); } }

// (twin re im): scalars -> a complex box; a real array operand -> a packed ai_C
// array (so arg stays elementwise); complex/object array or non-numeric -> zero
lvm(lvm_twin) {
 word a = Sp[0], b = Sp[1];
 bool atray = trayp(a), btray = trayp(b);
 if (atray || btray) {
  if ((atray && tray(a)->type >= ai_C) || (btray && tray(b)->type >= ai_C)
      || (!atray && !isnum(a)) || (!btray && !isnum(b)))
   ai_musttail return Push(ZeroPoint);
  uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0,
            R = ra > rb ? ra : rb, n = bshape_n(a, b);
  if (n == (uintptr_t) -1) ai_musttail return Push(ZeroPoint);
  uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[ai_C];
  Have(b2w(bytes));
  a = Sp[0], b = Sp[1];                                     // re-read post-Have
  struct ai_tray *r = (struct ai_tray*) Hp;
  Hp += b2w(bytes);
  ini_tray(r, ai_C, R);
  bshape_put(r->shape, R, a, b);
  twin_build_fill(r, a, b);
  ai_musttail return Push(word(r)); }
 if (!isnum(a) || !isnum(b)) ai_musttail return Push(ZeroPoint);
 ai_flo_t re = toflo(a), im = toflo(b);             // values extracted before alloc
 Have(twin_req);
 *++Sp = mk_twin(&Hp, re, im); ai_musttail return Next(1); }

// (twinp x): is x a complex scalar?
op11(lvm_twinp, twinp(Sp[0]) ? putcharm(1) : zero)

// fill r with component `off` (0 = re, 1 = im) of each element; off < 0 is the
// (im realarr) lane -- all zeros
static ai_noinline void cpart_fill(struct ai_tray *r, struct ai_tray *v, int off) {
 uintptr_t n = tray_nelem(r);
 if (off < 0) {
  intptr_t *zp = tray_data(r);
  for (uintptr_t p = 0; p < n; p++) zp[p] = 0;
  return; }
 ai_flo_t *rf = tray_data(r), *fp = tray_data(v);
 for (uintptr_t p = 0; p < n; p++) rf[p] = fp[2*p + off]; }

// the array lane of re/im: result carries the operand's shape
static lvm(lvm_cpart, int off) {
 struct ai_tray *v = tray(Sp[0]);
 enum ai_tray_type rt = off < 0 ? ai_Z : ai_R;
 uintptr_t R = v->rank, n = tray_nelem(v);
 uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[rt];
 Have(b2w(bytes));
 v = tray(Sp[0]);                                           // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, rt, R);
 for (uintptr_t i = 0; i < R; i++) r->shape[i] = v->shape[i];
 cpart_fill(r, v, off);
 return Answer(word(r)); }

// (re z) / (im z): the parts, elementwise over an array (a real array IS its own
// real part; im of one is fresh zeros); object array or non-number -> zero
lvm(lvm_re) {
 word a = Sp[0], _res;
 if (twinp(a)) {
  ai_flo_t re = twin_re(a);
  Have(box_req);
  emit_gem(_res, re);
  ai_musttail return Answer(_res); }
 if (trayp(a)) {
  enum ai_tray_type t = tray(a)->type;
  if (t == ai_O) ai_musttail return Answer(ZeroPoint);   // a tray is not a number
  if (t != ai_C) ai_musttail return Next(1);          // a real array is its own real part
  return Ap(lvm_cpart, g, 0); }
 if (isnum(a)) ai_musttail return Next(1);            // re of a real is itself
 ai_musttail return Answer(ZeroPoint); }

lvm(lvm_im) {
 word a = Sp[0], _res;
 if (twinp(a)) {
  ai_flo_t im = twin_im(a);
  Have(box_req);
  emit_gem(_res, im);
  ai_musttail return Answer(_res); }
 if (trayp(a)) {
  enum ai_tray_type t = tray(a)->type;
  if (t == ai_O) ai_musttail return Answer(ZeroPoint);
  return Ap(lvm_cpart, g, t == ai_C ? 1 : -1); }   // real array -> zeros of its shape
 if (isnum(a)) ai_musttail return Answer(putcharm(0));   // im of a real is 0
 ai_musttail return Answer(ZeroPoint); }

// (conj z): complex conjugate. conj LIFTS -- a real r becomes ~(r 0), so it
// always lands in C (the monadic `~`).
lvm(lvm_conj) {
 word a = Sp[0];
 if (twinp(a)) {
  ai_flo_t re = twin_re(a), im = twin_im(a);
  Have(twin_req);
  Sp[0] = mk_twin(&Hp, re, -im);
  ai_musttail return Next(1); }
 if (isnum(a)) {
  ai_flo_t re = toflo(a);            // lift a real to ~(r 0)
  Have(twin_req);
  Sp[0] = mk_twin(&Hp, re, 0);
  ai_musttail return Next(1); }
 ai_musttail return Answer(ZeroPoint); }

// (abs z): magnitude in its own tier; |INTPTR_MIN| promotes to a bignum (the one
// magnitude the box can't hold), its limb scratch out of line per the lvm scratch rule.
static ai_noinline word abs_wmin(struct ai *g) {
 uintptr_t u = (uintptr_t) 1 << (Bits - 1);
 ai_limb lb[wlimbs];
 for (int i = 0; i < wlimbs; i++) lb[i] = (ai_limb) (u >> (limb_bits * i));
 return ai_big_canon(&g->hp, lb, wlimbs, false); }
lvm(lvm_abs) {
 word a = Sp[0], _res;
 if (charmp(a)) {
  intptr_t n = getcharm(a);
  Have(box_req);
  emit_int(_res, n < 0 ? (intptr_t) (0 - (uintptr_t) n) : n);
  ai_musttail return Answer(_res); }
 if (twinp(a)) {
  ai_flo_t m = twin_mod(a);
  Have(box_req);
  emit_gem(_res, m);
  ai_musttail return Answer(_res); }
 if (gemp(a)) {
  ai_flo_t v = gem_get(a); if (v < 0) v = -v;
  Have(box_req);
  emit_gem(_res, v);
  ai_musttail return Answer(_res); }
 if (sunp(a)) { intptr_t n = sun_get(a);
  if (n == INTPTR_MIN) {                              // |INTPTR_MIN| = 2^(W-1): the bignum lane
   Have(b2w(sizeof(struct ai_big) + wlimbs * sizeof(ai_limb)));
   ai_musttail return Answer(abs_wmin(g)); }
  Have(box_req); emit_int(_res, n < 0 ? (intptr_t) (0 - (uintptr_t) n) : n);
  ai_musttail return Answer(_res); }
 if (bigp(a)) {
  struct ai_big *x = big(a);
  if (x->slen > 0) ai_musttail return Next(1);         // already non-negative
  uintptr_t bytes = ai_big_bytes(x); Have(b2w(bytes));
  x = big(Sp[0]);                         // re-read post-Have
  struct ai_big *y = big(Hp);
  Hp += b2w(bytes);
  memcpy(y, x, bytes); y->slen = -x->slen;           // flip the sign
  ai_musttail return Answer(word(y)); }
 if (trayp(a)) {                                       // vector -> scalar: the Euclidean (L2) norm
  struct ai_tray *v = tray(a); uintptr_t i, n = tray_nelem(v);   // sqrt(sum of squares); abs of a
  ai_flo_t s = 0;                                      // complex elem is its 2-vector modulus; ai_C sums 2n floats
  if (v->type == ai_C) { ai_flo_t *fp = tray_data(v); for (i = 0; i < 2*n; i++) s += fp[i] * fp[i]; }
  else for (i = 0; i < n; i++) { ai_flo_t e = tray_get_flo(v, i); s += e * e; }
  Have(box_req);
  emit_gem(_res, ai_sqrt(s));
  ai_musttail return Answer(_res); }
 if (tabp(a)) {                                       // table: its key count (so (int (abs t)) == (len t))
  Have(box_req);
  emit_int(_res, (intptr_t) map_len(a));
  ai_musttail return Answer(_res); }
 ai_musttail return Answer(ZeroPoint); }

// fill f64 array r with arg of each element of v
static ai_noinline void carg_fill(struct ai_tray *r, struct ai_tray *v) {
 uintptr_t n = tray_nelem(v);
 ai_flo_t *rf = tray_data(r);
 if (v->type == ai_C) { ai_flo_t *fp = tray_data(v);
  for (uintptr_t p = 0; p < n; p++) rf[p] = ai_atan2(fp[2*p+1], fp[2*p]); }
 else for (uintptr_t p = 0; p < n; p++) rf[p] = ai_atan2(0, tray_get_flo(v, p)); }

// (arg z): phase angle atan2(im, re); elementwise over an array, zero on a non-number
lvm(lvm_carg) {
 word a = Sp[0], _res;
 if (twinp(a)) {
  ai_flo_t r = ai_atan2(twin_im(a), twin_re(a));
  Have(box_req);
  emit_gem(_res, r);
  ai_musttail return Answer(_res); }
 if (trayp(a)) {
  struct ai_tray *v = tray(a);
  if (v->type == ai_O) ai_musttail return Answer(ZeroPoint);   // object array -> zero
  uintptr_t R = v->rank, n = 1;
  for (uintptr_t i = 0; i < R; i++) n *= v->shape[i];
  uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[ai_R];
  Have(b2w(bytes));
  v = tray(Sp[0]);                                           // re-read post-Have
  struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
  ini_tray(r, ai_R, R);
  for (uintptr_t i = 0; i < R; i++) r->shape[i] = v->shape[i];
  carg_fill(r, v);
  ai_musttail return Answer(word(r)); }
 if (isnum(a)) {
  ai_flo_t r = ai_atan2(0, toflo(a));
  Have(box_req);
  emit_gem(_res, r);
  ai_musttail return Answer(_res); }
 ai_musttail return Answer(ZeroPoint); }
