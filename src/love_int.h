// love_int.h -- the runtime's private header: the object layouts, the predicates and
// accessors over them, and the seam between src/love*.c's translation units. NOT the
// public surface -- that is love.h, which this includes. a nif writer wants love.h.
#ifndef AI_LOVE_INT_H
#define AI_LOVE_INT_H
#include "love.h"
#include <sys/mman.h>
#include <unistd.h>

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
#define AiRemCap (1u << 12)
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
// structural test for the charm zero -- an identity, not a measure. distinct
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
// a string's whole footprint in words: header + bytes + the NUL behind bytes[len]
#define str_width(n) (str_type_width + b2w((n) + 1))
#define op1(nom, i, x) lvm(nom) { Sp[0] = (x); Ip += i; ai_musttail return Continue(); }
#define op11(nom, x) op1(nom, 1, x)

#define pop1 ai_pop1


// one word per element: ints fold to Z, floats to R; C is the rank-0 complex pair
// (a tray rejects it at rank>=1). ordered Z < R < C, so `>= ai_R` is the float test.
// O slots hold live l words -- the one tray type the GC traces per element
// (evac_tray); O elements route through the promoting scalar dispatch (lvm_obin),
// which is what makes a bignum array add exactly instead of wrapping.
enum ai_tray_type { ai_Z, ai_R, ai_C, ai_O, };
// the math lvms' function argument, carried in g->b like every other extra arg
typedef ai_flo_t (*ai_flo1)(ai_flo_t);
typedef ai_flo_t (*ai_flo2)(ai_flo_t, ai_flo_t);
// elementwise dyadic opcodes for lvm_vbin. codes >= vop_lt produce a 0/1 mask
// (vop_eq is table-shared machinery only -- tray_eq answers `=` as a boolean).
// vop_quot is `/` (true division), vop_fquot `//` (truncating); both in the arith group.
enum vop { vop_add, vop_sub, vop_mul, vop_quot, vop_rem, vop_fquot,
           vop_band, vop_bor, vop_bxor, vop_bsl, vop_bsr,
           vop_lt, vop_le, vop_gt, vop_ge, vop_eq, };
// the bitwise codes ride the word lane (spec.l's width law): defined only where
// the cells are machine words; other operands take the whole op to the zero point
#define vop_bitp(op) ((op) >= vop_band && (op) <= vop_bsr)
// mask the shift count: C leaves count >= width undefined, and an accident is not a law
#define shmask(n) ((uintptr_t)(n) & (8 * sizeof(intptr_t) - 1))
word intern_checked(struct ai*, struct ai_str*);
uintptr_t intern_reserve(struct ai*),
          hash(struct ai*, intptr_t);
union u *map_fill_back(union u*, uintptr_t);
lvm_t lvm_kcall,
 lvm_chain, lvm_tray, lvm_sym, lvm_nom, lvm_str, lvm_big, lvm_gembox, // the data sentinels; each tail-jumps to its apply handler
 lvm_putn, lvm_seal, lvm_heard, lvm_worn, lvm_myself,
 lvm_nilp, lvm_putc, lvm_intern,
 lvm_saturate, lvm_ceil, lvm_peep, lvm_lamsrc, lvm_nifnom, lvm_cask, lvm_bcopy,
 lvm_coin, lvm_coinmk, lvm_load, lvm_dieof, lvm_coinp, lvm_sub_coin, lvm_quot_coin,   // newtypes: a coin (die + payload), a typed hot riding KHot
 lvm_charmp, lvm_tabp, lvm_band, lvm_bor, lvm_gem, lvm_gemp,
 lvm_sin, lvm_cos, lvm_log, lvm_pow,   // sqrt/exp/tan/atan/atan2 are derived (numeral/complex forms), not nifs
 lvm_twin, lvm_twinp, lvm_re, lvm_im, lvm_conj, lvm_abs, lvm_carg,   // complex; lvm_twin_bin declared apart below
 lvm_bxor, lvm_bsr, lvm_bsl, lvm_puts,
 lvm_string, lvm_lt,     lvm_le,   lvm_eq,     lvm_same, lvm_gt,  lvm_ge,
 lvm_sort,  lvm_tally, lvm_longp,
 lvm_pin, lvm_pull, lvm_tablet,   lvm_keys,  lvm_dig,
 lvm_unc, lvm_poke, lvm_peek,
 lvm_seek,  lvm_trim,   lvm_spin,   lvm_add,
 lvm_mul,    lvm_quot,   lvm_fquot, lvm_rem,  lvm_arg,
 lvm_bmul_start,             // the resumable bignum multiply's entry; its loop bodies are num.c's
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
 lvm_sleep, lvm_donep, lvm_scoop, lvm_hush,
 lvm_await,
 lvm_fgetc, lvm_fungetc, lvm_chug, lvm_unchug, lvm_inhand, lvm_fputc, lvm_fputs, lvm_fflush,
 lvm_fputbn, lvm_sound0,
 lvm_trayctor, lvm_iota, lvm_rank, lvm_alen, lvm_shape, lvm_atype,   // typed multi-rank arrays
 lvm_asum, lvm_aprod, lvm_max, lvm_min, lvm_aall, lvm_inner, lvm_outer,
 lvm_litp, lvm_hotp,
 lvm_nif,         // codegen backend: emitted bytes -> applicable native value (1-arg / multi-arg)
 lvm_nifx,        // ... with an extras word (value[3]+8 = Ip+32): refs a native needs beyond the twin (the callout's clos, amble's ()/globals) ride a GC-walked cell slot, so value[1] stays the plain twin and the image revert (img_nif_interp) never dereferences a pack
 lvm_calloutdrive, lvm_calloutresume,   // the drive addresses as fixnums (probes; a native reads them off g->jk)
 lvm_jkoff,       // (jkoff x): g->jk's byte offset, what the emitter's `jk` law loads from
 lvm_natp;        // (nat? f): is f a native closure -- its code in the arena
// ⚠ THE ATTRIBUTES ARE THE DECLARATION: `lvm(n)` is `ai_noinline ai_noicf _lvm(n)`, so these
// cannot fold into the plain lvm_t list above without shedding both. ai_noicf is noipa, and the
// data sentinels below are what it is for -- see their note.
ai_noinline ai_noicf lvm_t
 lvm_0, lvm_add_seq, lvm_add_string, lvm_addh, lvm_arg0,
 lvm_atype, lvm_bin_a, lvm_bin_b, lvm_bin_unit, lvm_coinp,
 lvm_dieof, lvm_dig, lvm_gemp, lvm_heard, lvm_hotp,
 lvm_mulh, lvm_myself, lvm_nilp, lvm_quo0, lvm_quom1,
 lvm_quotn, lvm_rank, lvm_tabp, lvm_twinp, lvm_worn,
 // these carry extra operands, so they are declared apart from the plain lvm_t list
 lvm_vbin, lvm_bdiv_start, lvm_vmap1, lvm_vmap2, lvm_twin_bin, lvm_cbin, lvm_obin,
 // the data sentinels: each is the first word (ap) of its rep's heap objects and
 // tail-jumps straight to its apply handler -- the sentinel is the rep (enum d).
 // bodies are byte-identical, kept distinct by address (ai_noicf).
 data_num_apply, data_sym_apply, data_string_apply, data_pair_apply,
 lvm_sym, lvm_nom, lvm_sunbox, lvm_gembox, lvm_twinbox,
 lvm_big, lvm_tray, lvm_str, lvm_chain,
 lvm_addn, lvm_fquotn, lvm_muln,
 lvm_remn, _lvm_yieldk;
char const *ai_nif_name(intptr_t);
#define tray(_) ((struct ai_tray*)(_))
#define sym(_) ((struct ai_mint*)(_))
#define nom(_) ((struct ai_nom*)(_))
#define big(_) ((struct ai_big*)(_))
#define cask(_) ((struct ai_cask*)(_))
static ai_inline bool mintp(word _) { return lamp(_) && cell(_)->ap == lvm_sym; }
static ai_inline bool namep(word _) { return lamp(_) && cell(_)->ap == lvm_nom; }
static ai_inline bool packp(word _) { return lamp(_) && cell(_)->ap == lvm_tray; }
static ai_inline bool strp(word _) { return lamp(_) && cell(_)->ap == lvm_str; }
static ai_inline bool nomp(word x) { return lamp(x) && (cell(x)->ap == lvm_sym || cell(x)->ap == lvm_nom); }
// mutable flat byte string. not a data kind: the head is the behaves-as-0 lvm_cask,
// so the GC walks a cask as a plain length-2 thread and forwards the embedded ai_str
// free. earned by the build tools that back-patch an image in place.
static ai_inline bool caskp(word _) { return lamp(_) && cell(_)->ap == lvm_cask; }
// a map is a lookup-lambda with stable identity across growth: a fixed header
// [lvm_map_lookup, backing, <tag>] callers hold, and an open-addressed backing
// [lvm_map_data, len, cap, k0,v0, .., <tag>] -- growth swaps header[1], so aliased
// references (ev's scopes) see later inserts. both are plain threads, no bespoke
// GC. empty slots hold map_gap, a unique out-of-pool address. (m k) -> value, () absent.
lvm_t lvm_map_lookup, lvm_map_data;
static ai_inline bool tabp(word _) { return lamp(_) && cell(_)->ap == lvm_map_lookup; }
extern const word ai_map_gap_cell;   // one definition: map_gap is its ADDRESS
#define map_gap ((word) &ai_map_gap_cell)
#define map_min_cap 4
#define map_hint_max (1u << 24)        // the `(tablet n)` size hint saturates to this bounded green charm
static ai_inline word map_back(word m) { return cell(m)[1].x; }
static ai_inline word *map_slots(word m) { return &cell(map_back(m))[3].x; }
static ai_inline uintptr_t map_len(word m) { return getcharm(cell(map_back(m))[1].x); }
static ai_inline uintptr_t map_cap(word m) { return getcharm(cell(map_back(m))[2].x); }
word
 ai_mapget(struct ai*, word, word, word),
 bookget(struct ai*, word, word),   // the layered global read: walks g->book (a chain of books) head-first
 macroget(struct ai*, word);        // the layered macro read: each layer's table rides its [zero] slot
struct ai *ai_mapput(struct ai*), *map_new(struct ai*);
// the byte ops read from a string or a cask; both resolve to a ai_str of bytes.
static ai_inline struct ai_str *bytes_of(word x) { return caskp(x) ? cask(x)->str : str(x); }
// a coin: a newtype value, a typed hot [lvm_coin, die, payload] -- a plain thread,
// no bespoke evac. ai_kind reads KHot, so +/* route every coin combination to
// lvm_addh/mulh, where a coin operand is intercepted. the die (a map keyed by the
// slot fixnums below) is the type descriptor; every coin of a type is struck from one die.
struct ai_coin { lvm_t *ap; word die; word payload; };
static ai_inline bool coinp(word _) { return lamp(_) && cell(_)->ap == lvm_coin; }
static ai_inline word coin_die(word x) { return ((struct ai_coin*) x)->die; }
static ai_inline word coin_load(word x) { return ((struct ai_coin*) x)->payload; }
// die slots (fixnum keys). add/mul/apply are closures run inside the VM; net/=/<
// /show/tally default over the payload in pure C. hot truthy = the die's coins are
// lit? (references); absent = fresh data. net is a mode fixnum, never a closure --
// ai_net is pure C under every truth test and must not re-enter the VM.
enum { DieName = 0, DieAdd = 1, DieMul = 2, DieApply = 3, DieHot = 4, DieSub = 5,
       DieNet = 6,    // net mode, a fixnum: absent/0 = net of payload; 1 = net by tally (the
                       // count); 2 = ratio (an (n d)-of-reals payload nets n/d, sign exact)
       DieStar = 7,   // truthy = the die's coins are numeric: numeral application powers them
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
// element payload: laid out row-major just past the shape words.
static ai_inline void *tray_data(struct ai_tray *v) { return (void*) (v->shape + v->rank); }
// total element count = product of the dimensions (1 for a rank-0 scalar box).
static ai_inline uintptr_t tray_nelem(struct ai_tray *v) {
 uintptr_t n = 1;
 for (uintptr_t i = 0; i < v->rank; i++) n *= v->shape[i];
 return n; }
static ai_inline struct ai_tray *ini_tray(struct ai_tray *v, enum ai_tray_type t, uintptr_t rank) {
 return v->ap = lvm_tray, v->type = t, v->rank = rank, v; }
// read element i of v as a double / as an integer (sign-extending the narrow
// integer types; truncating a float toward zero for the int reader). the int
// reader is only used on integer-typed arrays in practice.
static ai_inline ai_flo_t tray_get_flo(struct ai_tray *v, uintptr_t i) {
 void *p = tray_data(v);
 return v->type == ai_R ? ((ai_flo_t*) p)[i] : (ai_flo_t) ((intptr_t*) p)[i]; }
static ai_inline intptr_t tray_get_int(struct ai_tray *v, uintptr_t i) {
 void *p = tray_data(v);
 return v->type == ai_R ? (intptr_t) ((ai_flo_t*) p)[i] : ((intptr_t*) p)[i]; }
// write element i of v, converting to v's element kind.
static ai_inline void tray_put_int(struct ai_tray *v, uintptr_t i, intptr_t x) {
 void *p = tray_data(v);
 if (v->type == ai_R) ((ai_flo_t*) p)[i] = (ai_flo_t) x; else ((intptr_t*) p)[i] = x; }
static ai_inline void tray_put_flo(struct ai_tray *v, uintptr_t i, ai_flo_t x) {
 void *p = tray_data(v);
 if (v->type == ai_R) ((ai_flo_t*) p)[i] = x; else ((intptr_t*) p)[i] = (intptr_t) x; }
// read/write element i of a ai_O array as a raw tagged l word (the GC traces
// these; see evac_tray). no conversion -- the slot is a value.
static ai_inline word tray_get_obj(struct ai_tray *v, uintptr_t i) {
 return ((word*) tray_data(v))[i]; }
static ai_inline void tray_put_obj(struct ai_tray *v, uintptr_t i, word x) {
 ((word*) tray_data(v))[i] = x; }

// truth: x is false iff (= 0 ($ x)). the net's codomain is complex (ai_net): a
// complex scalar nets itself, every other scalar nets real, aggregates sum -- so
// the net is additive exactly. common kinds short-circuit with no walk; a sum
// cannot (a later negative cancels). lockstep with ai_saturate ($): same zero conditions.
struct ai_str *nom_str(struct ai *g, word x);   // a named sym -> its name string, else 0
struct ai_zn { ai_flo_t re, im; };                     // the net: a complex value
static ai_inline struct ai_zn zn(ai_flo_t re, ai_flo_t im) {
  struct ai_zn z = {re, im}; return z; }
// the truth gate, not the total order -- the one place the two part: a net is
// nothing unless its real part is positive, so a pure phase is blue (truth cannot
// depend on which root of x^2+1 we named `i`). the lexicographic order stays as
// it was -- sorting needs totality.
// a macro, not a fn: a by-value ai_zn argument stages through push/pop, which
// bars unframe in every fn ai_nilp splices into (the hot truth-test fleet)
struct ai_zn ai_net(struct ai *, word);         // fwd: aggregates sum their elements
intptr_t ai_count(struct ai *, word);           // fwd: tally's C body (net-mode 1 reads it)
// only the two lanes that answer with no load and no call earn a line here: they
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
// integer value of a fixnum-or-box operand (callers exclude floats and bignums)
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
// pun through a union, not memcpy(&local,..): the memcpy form escapes a stack
// local, and clang -Os then refuses the sibling call out of any inlining VM ap --
// silently breaking threaded dispatch (tools/vmret.l).
_Static_assert(sizeof(ai_flo_t) == sizeof(uintptr_t), "float box assumes ai_flo_t is pointer-width");
typedef union { uintptr_t u; ai_flo_t d; } ai_flo_pun;
static ai_inline ai_flo_t gem_get(word x) {
 return ((ai_flo_pun){ .u = ((struct ai_gem*) x)->w }).d; }
// allocate a float box at *hpp (caller holds Have(gem_req)); no &local, so the caller keeps its tail call.
// the law, the one real-float box-write: NaN collapses to 0 so the order stays total and
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
// lane). peep reads through it, pin writes through it: one index law. answers by
// value, never through an out-param: an escaping &local costs its caller the tail jump.
static ai_inline intptr_t tray_off(struct ai_tray *v, word k) {
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
// numeric tray, which leaves the slot alone. v is the caller's fresh tray -- an
// object slot gaining a young word needs no barrier only because nothing old is written.
static ai_inline bool tray_put(struct ai_tray *v, uintptr_t i, word x) {
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

// equality comparisons inline the fast identity check. ⚠ eqv is declared HERE, not with the
// other bools at the tail: eql below calls it, and a caller cannot precede its declaration.
ai_noinline bool eqv(struct ai*, word, word); // this is for checking equality of non-identical values
// eqv has no value-equality for distinct charms or distinct noms -- identity is
// their whole equality -- so eql settles both inline and skips the noinline call
static ai_inline bool eql(struct ai *g, word a, word b) {
 return a == b ? true : (a & b & 1) || (nomp(a) && nomp(b)) ? false : eqv(g, a, b); }

// threads (and every sounded heap object) end with one tag word: the object's own
// head pointer with bit 1 set. the terminator test is the tag bits and the payload
// pointing back into the pool -- an embedded external pointer can carry (x&3)==2
// but never points into the pool.
#define ai_thread_tag 2
static ai_inline bool tagp(word x, word const *lo, word const *hi) {
 word const *p = (word const*) (x & ~(word) 3);
 return (x & 3) == ai_thread_tag && p >= lo && p < hi; }
// the collection: the state that means something only for the span of one pass, held
// on the C stack of whoever drives it. it is not in the core because between two
// collections there is no answer for any of it, and a stale range is exactly how a
// walk leaves the heap. every function below that can be reached from gcp takes it.
struct ai_gcx {
 word const *p0, *t0;        // the from-space under trace
 word const *f2lo, *f2hi;    // a second from-space (0 = unused); a major traces {major ∪ minor} in one pass
 word *to_lo, *to_hi;        // where survivors land: the tagp range [to_lo, to_hi)
 word *fwd;                  // the forwarding floor: word0 in [fwd, to_hi) = a copy made this pass
 word *cp; };                // the cheney scan cursor
// the pools a heap pointer may live in between collections -- the question bio_of asks
// of a port (heap bio, or the static it cannot own a buffer for).
static ai_inline bool in_live_pool(struct ai *g, word const *p) {
 if (p >= ptr(g) && p < ptr(g) + g->len) return true;             // minor / main pool
 return g->major_pool && p >= g->major_pool && p < g->major_pool + 2 * g->major_len; }   // both major halves
// GC scans run with different [lo,hi), so a terminator must be recognized by which
// live pool its head lands in, not the caller's single range -- else a young-pointing
// terminator under the major range is gcp'd as a field and followed off the heap.
// mid-pass the to-space is a third range: a fresh pair gen_major has not flipped to
// yet, or the new pool gen_grow is copying into -- neither is a pool of g's yet.
static ai_inline bool tagl(struct ai *g, struct ai_gcx *X, word x) {   // range-independent terminator test
 if ((x & 3) != ai_thread_tag) return false;
 word const *p = (word const*) (x & ~(word) 3);
 return (X->to_lo && p >= X->to_lo && p < X->to_hi) || in_live_pool(g, p); }
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
 s->ap = lvm_str, s->len = len;
 ((word*) s->bytes)[b2w(len + 1) - 1] = 0;   // tail word first: pad + NUL stay zero under the fill
 return s; }

// the unique empty string: data-segment, immortal (gcp's out-of-pool
// short-circuit); a zero-length string is never heap-allocated.
// () -- the one serial-0 mint, shared by every core (serial 0 is never drawn, so
// it is unique + least in the order). see the ZeroPoint macro in love.h.


static ai_inline uintptr_t rot(uintptr_t x) {
  int const s = sizeof(uintptr_t) * 4; // shift bits = word bits / 2 = sizeof(word) * 4
  return (x << s) | (x >> s); }

// the four doors that are not a device; spelled out beside their readn/writen
extern struct ai_port_vt const ai_to_vt, ai_closed_vt, ai_ci_vt;

// the pool's spare half: the core sits at the base of the active one, so the scratch
// a walk borrows starts one pool length up
static ai_inline void *off_pool(struct ai *g) { return (word*) g + g->len; }
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
// knob cannot price both. this bounds only how long a ready parked task waits behind
// a peer that never blocks -- an i/o task sweeps on its own blocking path first.
#define sweep_interval 16
// a fairness yield clears any stale one-shot park intention first: lvm_fgetc never
// clears the fd on a successful read, and a periodic yield that inherited it would
// park this task on that fd for good. g->parked joins the guard: a server whose
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


#define avm_unit(a, b) \
 if (mintp(a) || mintp(b)) ai_musttail return Push(ZeroPoint)
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
#define bit_slow(n, c_op) lvm(lvm_##n##_slow) {               \
 word a = Sp[0], b = Sp[1], _res;                                     \
 if (!(charmp(a) || sunp(a)) || !(charmp(b) || sunp(b)))                  \
  ai_musttail return Push(ZeroPoint);                               \
 Have(box_req);                                                       \
 emit_int(_res, toint(a) c_op toint(b));                                    \
 ai_musttail return Push(_res); }
#define mvm1(n) lvm(lvm_##n) { { g->b = (ai_word) (uintptr_t) (ai_##n); ai_musttail return Ap(lvm_math1, g); } }
#define m1(_) _(sin) _(cos)   // sqrt/exp/tan/atan derived; sin/cos/log are the kept transcendentals (log has its own ap)
#define cmp_lt(nom, vop) lvm(nom) { \
 word a = Sp[0], b = Sp[1]; \
 if (__builtin_expect(charmp(a) && charmp(b), 1)) { \
  intptr_t r = vcmp_int(vop, a, b); \
  if (Ip[1].ap == lvm_cond) { Sp += 2; Ip = r ? Ip + 3 : Ip[2].m; ai_musttail return Continue(); } \
  ai_musttail return Push(r ? putcharm(1) : zero); } \
 { g->b = (ai_word) (vop); ai_musttail return Ap(lvm_cmp_ord, g); } }

// --------------------------------------------------------------------------
// THE TU SEAM. src/love*.c is one runtime cut into translation units so no single
// one is the whole build's critical path; everything not named here stays static to
// its own file. an always_inline that crosses loses the attribute: a call to one
// cannot cross a TU, and hoisting the body here drags its callees along too.
// --------------------------------------------------------------------------
struct ai; struct ai_bio; struct ai_cask; struct ai_def; struct ai_io; struct ai_mint; struct ai_nom; struct ai_port_vt; struct ai_r; struct ai_str; struct ai_tray; struct arib; struct clonf; struct env; struct img_ctx;
// nifs.h lands in love.c, so def1 reaches the image codec through these. a count,
// because countof wants the array's complete type and an extern one has none.
extern struct ai_def const *const ai_def1;
extern uintptr_t const ai_def1_n;
extern union u const callout_drive[];
extern union u const yield_c[];
struct ai_bio *bio_of(struct ai *g, struct ai_io *i);
char *code_install(struct ai *g, char const *src, size_t n), *code_adopt(struct ai *g, char const *src, size_t n);
void code_free(struct ai *g, char *code), jk_ini(struct ai *g);
int code_in(struct ai *g, uintptr_t v);
size_t code_len(char *code);
// the jk slots (g->jk): what a native reads off g -- the emitter's `jk` law names them the same
enum { JkChain, JkStr, JkMap, JkNom, JkMint, JkGem, JkCask, JkDrive, JkResume, JkCur };
union u *fn_base(union u *k, int *nargs);
struct ai
 *ai_eval_(struct ai *g),
 *ioputc(struct ai*g, int c),
 *ioputs(struct ai*g, char const *s),
 *gen_grow(struct ai *g, uintptr_t len1),
 *gen_major(struct ai *g, uintptr_t req0, bool *tight),
 *ored(struct ai *g, int kind), *zflush(struct ai*g);
uintptr_t
 bshape_n(word a, word b),
 shash(struct ai *g, word x, struct arib *env, word *base),
 hash_at(struct ai *g, intptr_t x, word *base),
 map_probe(struct ai *g, word m, word k, bool *found);
struct ai_str *seq_cat(struct ai *g, void *w, word a, word b);
intptr_t
 fn_arg(union u *k, int i, int nargs),
 *task_io(struct ai *g),
 vcmp_flo(int op, ai_flo_t a, ai_flo_t b),
 vcmp_int(int op, intptr_t a, intptr_t b),
 io_route(struct ai *g, word x),
 hot_hook(word h),
 fn_src(struct ai *c, union u *k, word x);
void
 *ai_libc_alloc(struct ai*g, void *p, size_t n),
 bshape_put(uintptr_t *shape, uintptr_t R, word a, word b),
 bstride(struct ai_tray *v, uintptr_t R, intptr_t *c),
 odo_step(intptr_t *idx, uintptr_t R, uintptr_t const *shape),
 gen_wb(struct ai *g, word src, word p),
 gen_wb_cell(struct ai *g, void *cl, word v);
ai_flo_t vop_flo(int op, ai_flo_t a, ai_flo_t b);
bool
 bio_rpending(struct ai_bio *b),
 wait_buffered(struct ai *g, lvm_t *ap, word x, int fd),
 clo_nfhash(struct ai *g, word x, uintptr_t *out, word *base),
 fn_partialp(union u *k),
 in_heap(struct ai *c, word x),
 iop(word x),
 lam_isp(struct ai *g, word x);
#endif
