#include "love.h"
// The build's version string (the version-control id), generated into out/lib/love_version.h by
// the Makefile and surfaced in the runtime as the `love-version` global (ai_ini_0).
// Optional include so a standalone/unwired compile still builds; falls back to "unknown".
// A lane that HAS the header says so with -DAI_HAVE_VERSION_H, and __has_include is only the
// fallback probe: MOONCC DOES NOT IMPLEMENT IT, and mooncc builds the default host binary --
// so on the shipped `love` the probe read false and `love --version` answered "unknown".
// A -DAI_VERSION on the command line wins over both: love0 pins "bootstrap" that way
// (host/build.mk's gl0_cc), which keeps a new commit's id from relinking the bootstrap --
// and every lcat header, and every object, behind it.
#ifndef AI_VERSION
# if defined(AI_HAVE_VERSION_H) || (defined(__has_include) && __has_include("love_version.h"))
#  include "love_version.h"
# endif
#endif
#ifndef AI_VERSION
#define AI_VERSION "unknown"
#endif

// ============================================================================
// kernel-internal declarations (private; merged from former i.h)
// ============================================================================

// the math floor is OURS on every frontend: crew/moon/lib/math/am.c (the seven
// transcendentals; fdlibm and -lm both retired). the 32-bit lane computes in
// binary64 and narrows -- correct within a float ulp for free.
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

// Bignum limbs are native-word-width where a double-width integer is available for
// the limb products and the Knuth divmod q-hat step -- the host and the 64-bit
// kernels: 64-bit limbs do a quarter the limb-ops of 32-bit limbs on the same
// number (schoolbook mul/div are O(limbs^2)), the difference between love and a
// 64-bit-digit BigInt. The 32-bit wasm shim (no __int128) keeps 32-bit limbs with
// a uint64_t accumulator. ai_dlimb is the unsigned double-limb (products, carries),
// ai_sdlimb the signed one (subtract / divmod borrows). limb_clz counts a limb's
// leading zeros at the chosen width.
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
// decimal digits a limb spans: floor(limb_bits * log10 2). The reader packs this
// many digits per mul-add pass (10^chunk fits a limb); the printer bounds its byte
// count by chunk+1 (a limb prints in < that many digits). 30103 = round(1e5 log10 2).
#define limb_dec_chunk  (limb_bits * 30103 / 100000)
#define limb_dec_digits (limb_dec_chunk + 1)
// the binary-radix twins, exact at 4 and 3 bits a digit. a pass multiplies by
// radix^chunk, so chunk is the most digits whose product still FITS a limb: one
// bit shy of the full limb that 16^(limb_bits/4) would be, exactly.
#define limb_hex_chunk  ((limb_bits - 1) / 4)
#define limb_oct_chunk  ((limb_bits - 1) / 3)

#define Bytes (Bits>>3)
_Static_assert(Bytes == sizeof(uintptr_t), "word size sanity check");

#include <stdarg.h>
_Static_assert(sizeof(union u) == sizeof(intptr_t), "cell size equals word size");

// remembered-set capacity in words (old objects holding a young pointer). g->alloc'd, never raw malloc,
// so the generational collector is freestanding -- it rides whatever allocator the frontend supplies.
#define AI_REM_CAP (1u << 16)
// INITIAL pool sizes, in words per half. CONFIGURABLE like the custom allocator (g->alloc): both
// pools GROW on demand, so these are only a starting point -- a tiny device overrides them small
// (-Dai_minor0=... -Dai_major0=...) and accepts more collections; a host leaves them roomy and boots
// in few.
#ifndef ai_minor0
# define ai_minor0 (1u << 14)   // ~128 KB minor (the main pool); the dev host boots fast
#endif
#ifndef ai_major0
# define ai_major0 (1u << 16)      // ~512 KB major-pool half; grows much less often than the minor pool
#endif
// ai_gc_ratio: the generational nursery's COPY-OVERHEAD setpoint (gen_please). The minor pool is
// resized to hold words-copied/words-allocated inside the band [1/(4*ratio), 1/ratio] -- a LARGER
// ratio targets LOWER overhead, so the nursery grows bigger and collections fire less often (faster,
// more RAM); a smaller ratio keeps it lean. This is the self-dampening differential resizer's setpoint.
#ifndef ai_gc_ratio
# define ai_gc_ratio 24   // tuned at the knee of the GC-reduction curve: ~3x fewer collections than 8,
#endif                    // same 128MB nursery high-water as 16 (the controller doubles, so both land there),
                          // emergent (a light program stays near the 8KB seed). The allocation-heavy benches
                          // (tree/hash/sort) gain ~10%; past ~24 the curve flattens and RAM doubles (-> 64 = 256MB).
// ai_budget: the TOTAL memory budget, in words -- the cap on what the runtime reserves (2*minor + 2*major,
// both pools two-space). 0 = UNBOUNDED (a host: the budget grows on demand). A small device sets it to
// its RAM (-Dai_budget=131072 for a 1 MB Teensy). The minor pool is then sized by APPEL'S RULE -- the
// nursery gets the free budget after the major pool -- so one knob governs the whole footprint, and the
// schedule is deterministic (a function of the live set + budget, never the wall clock). See gen_please.
#ifndef ai_budget
# define ai_budget 0
#endif
_Static_assert(-1 >> 1 == -1, "sign extended shift");
// nilp: structural test for the nil word (the only false scalar). Distinct from
// ai_nilp below (the language falsy predicate, which also counts an all-zero vec);
// the l `nilp` nif maps to ai_nilp, not this macro.
#define nilp(_) (word(_)==nil)
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
#define op1(nom, i, x) lvm(nom) { Sp[0] = (x); Ip += i; return Continue(); }
#define op11(nom, x) op1(nom, 1, x)

#define pop1 ai_pop1

// One word per element: every integer width folds to Z (intptr_t), every float
// width to R (ai_flo_t), and C is the rank-0 complex scalar (two ai_flo_t). Ordered
// Z < R < C so `>= ai_R` is the float-domain test and C is the widest *numeric* tier.
// arr rejects ty == C, so C never appears as a rank>=1 array element -- complex
// only ever shows up as a rank-0 scalar (Cp), handled by explicit cplx branches.
// O (object) is the odd tier out: its slots hold live l words (any value --
// fixnum, bignum, box, complex, string, chain...), so it is the ONE vec type the
// copying GC must trace element-by-element (evac_vec). It sits outside the numeric
// order; the typed fast lanes gate on `type <= ai_C`, the arith lane on `type == ai_O`
// (lvm_obin), so O elements always route through the promoting scalar dispatch --
// that is what makes a bignum array add/multiply exactly instead of wrapping.
enum ai_vec_type { ai_Z, ai_R, ai_C, ai_O, };
// Elementwise dyadic opcodes for lvm_vbin (kernel/arr.c). The five arith codes
// match the arith slow aps; the five compare codes (>= vop_lt) produce a
// 0/1 bool array. `<` and `>` are the MASK MAKERS; vop_eq is NOT reached from `=`
// any more (arr_eq answers that as a boolean -- see the note there), and its arms
// below are kept only because the opcode table is shared machinery.
// vop_quot is `/` (true division: float when a element divides inexactly);
// vop_fquot is `//` (truncating integer division). Both stay in the arith group
// (< vop_lt) so `op >= vop_lt` still selects the compare codes.
enum vop { vop_add, vop_sub, vop_mul, vop_quot, vop_rem, vop_fquot,
           vop_band, vop_bor, vop_bxor, vop_bsl, vop_bsr,
           vop_lt, vop_le, vop_gt, vop_ge, vop_eq, };
// The bitwise codes ride the WORD LANE (test/spec.l's width law): they are defined
// only where the cells ARE machine words, so a float/complex/object operand takes
// the whole op to the zero point rather than mapping elementwise.
#define vop_bitp(op) ((op) >= vop_band && (op) <= vop_bsr)
// A shift count masks to the word. C leaves a count >= width UNDEFINED and every
// target we ride happens to mask, but an accident is not a law -- so we mask here
// and the spec can state it.
#define shmask(n) ((uintptr_t)(n) & (8 * sizeof(intptr_t) - 1))
word intern_checked(struct ai*, struct ai_str*);
uintptr_t intern_reserve(struct ai*);
uintptr_t hash(struct ai*, intptr_t);
static ai_inline union u *map_fill_back(union u*, uintptr_t);
lvm_t lvm_kcall,
 lvm_chain, lvm_vec, lvm_sym, lvm_nom, lvm_str, lvm_big, lvm_flo, // data sentinels (enum q order); each tail-jumps to its apply handler
 lvm_putn, lvm_gauge,    lvm_clock, lvm_nclock, lvm_please, lvm_apof, lvm_seal, lvm_books, lvm_setbooks, lvm_mods, lvm_lib,
 lvm_nilp,  lvm_putc, lvm_mint, lvm_nomctor, lvm_intern, lvm_chainp,
 lvm_pin, lvm_peep, lvm_fputx, lvm_buf, lvm_bufnew, lvm_bcopy,
 lvm_coin, lvm_coinmk, lvm_load, lvm_dieof, lvm_coinp, lvm_add_coin, lvm_mul_coin, lvm_sub_coin, lvm_quot_coin,   // newtypes: a coin (die + payload), a typed hot riding KHot
 lvm_charmp,  lvm_nomp,   lvm_namep,  lvm_strp,   lvm_tabp, lvm_band,   lvm_bor,  lvm_real,  lvm_flop,
 lvm_sin, lvm_cos, lvm_log, lvm_pow,   // sqrt/exp/tan/atan/atan2 are derived (numeral/complex forms), not nifs
 // Step 7 -- complex (kernel/cplx.c). lvm_cplx_bin (declared apart, below) is
 // the arithmetic lane the scalar arith slow paths divert into.
 lvm_cplx, lvm_Cp, lvm_re, lvm_im, lvm_conj, lvm_abs, lvm_carg,
 lvm_bxor,  lvm_bsr,    lvm_bsl,    lvm_snip,
 lvm_link,   lvm_cap,  lvm_cup,    lvm_puts,
 lvm_string, lvm_lt,     lvm_le,   lvm_eq,     lvm_same, lvm_gt,  lvm_ge,
 lvm_sort,  lvm_tally,
 lvm_put, lvm_pull, lvm_tablet,   lvm_keys,  lvm_dig,
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
 lvm_callk, lvm_scare, lvm_missing, lvm_yield_sw, lvm_yield_nif, lvm_task_exit, lvm_spawn, lvm_wait,
 lvm_sleep, lvm_donep, lvm_hush, lvm_key,
 lvm_await,
 lvm_fgetc, lvm_fungetc, lvm_fputc, lvm_fputs, lvm_fflush,
 lvm_fputbn, lvm_sound0, lvm_dot,
 // Step 5a -- typed multi-rank arrays (kernel/arr.c). lvm_vbin is the shared
 // elementwise/broadcast engine the arith/compare slow lanes divert into.
 lvm_tray, lvm_iota, lvm_rank, lvm_alen, lvm_shape, lvm_atype,
 lvm_asum, lvm_aprod, lvm_max, lvm_min, lvm_aall, lvm_inner, lvm_outer,
 lvm_packp, lvm_bigp, lvm_widep, lvm_setp, lvm_intf, lvm_litp, lvm_hotp,
 lvm_nif,         // CODEGEN BACKEND: emitted bytes -> applicable native value (1-arg / multi-arg)
 lvm_nifx,        // ... with an EXTRAS word (value[3]+8 = Ip+32): refs a native needs beyond the twin (the callout's clos, amble's ()/globals) ride a GC-walked cell slot, so value[1] stays the PLAIN twin and the image revert (img_nif_interp) never dereferences a pack
 lvm_resume,      // the WALKABLE call-out resume: jump blob-base + untag(offset) after delivering the result -- the frame carries an odd charm + an out-of-pool code address, so a GC (gen_grow included) with a call-out pending walks it clean (the retB stack-interior pointer is retired)
 lvm_calloutdrive, lvm_calloutresume;   // the drive addresses as fixnums (the glaze emitter bakes them as `li Ip` immediates)
// Carry extra operands, so (like lvm_gc) they are declared apart from the
// plain lvm_t list, which fixes the 4-argument ap signature. lvm_vbin
// is the elementwise/broadcast dyadic engine (vop selects the op); lvm_vmap1
// applies a monadic math fn elementwise to an array (e.g. (sin arr)); lvm_vmap2
// is the dyadic analogue with broadcasting (e.g. (pow arr arr), (atan2 ...)).
lvm(lvm_vbin, int);
lvm(lvm_bdiv_start, int);   // resumable long-division entry; the int is vop (vop_fquot / vop_rem)
lvm(lvm_vmap1, ai_flo_t (*)(ai_flo_t));
lvm(lvm_vmap2, ai_flo_t (*)(ai_flo_t, ai_flo_t));
// Complex arithmetic lane (kernel/cplx.c): the scalar arith slow paths divert
// here when either operand is complex; vop selects add/sub/mul/quot (rem -> nil).
lvm(lvm_cplx_bin, int);
// Complex-array elementwise lane (ai_C): lvm_vbin diverts here when an operand is
// a packed (re,im) ai_C array (or a complex scalar paired with an array). Same
// numpy broadcast as vbin, complex domain; `=` -> mask, ordering/% -> nil.
lvm(lvm_cbin, int);
// Object-array elementwise lane (ai_O): the broadcast engine lvm_vbin diverts
// here when an operand is a ai_O array, so each element op runs the promoting
// scalar dispatch (exact bignum results) instead of the typed raw-C lanes.
lvm(lvm_obin, int);
// data-kind recovery (datp/typ): ai_typ/in_data now live in love.h as a direct
// compare against the sentinel addresses -- no generated header, no section.
//
// The data-kind sentinels: each is the first word (ap) of a data kind's heap
// objects. Each tail-jumps STRAIGHT to its apply handler -- the sentinel already
// IS the kind, so there is no dispatch table and no ai_typ/ai_kind recovery on
// the apply path (the apply was uniform in the argument kind anyway: a handler
// that cares re-inspects the arg itself, e.g. data_string_apply's index test).
// Bodies within a group are byte-identical tail calls, kept distinct only by
// address (ai_noicf, in the lvm macro) so ai_typ's compare elsewhere still works.
// (The handlers live far below, near the generic-op dispatchers; declared here.)
static lvm(data_num_apply); static lvm(data_string_apply);
static lvm(data_sym_apply); static lvm(data_pair_apply);
lvm(lvm_vec)   { return Ap(data_num_apply, g); }
lvm(lvm_big)   { return Ap(data_num_apply, g); }
lvm(lvm_str)   { return Ap(data_string_apply, g); }
lvm(lvm_sym)   { return Ap(data_sym_apply, g); }
lvm(lvm_nom)   { return Ap(data_sym_apply, g); }
lvm(lvm_chain) { return Ap(data_pair_apply, g); }
lvm(lvm_flo)   { return Ap(data_num_apply, g); }
lvm(lvm_wide)  { return Ap(data_num_apply, g); }
lvm(lvm_cbox)  { return Ap(data_num_apply, g); }
char const *ai_nif_name(intptr_t);
#define vec(_) ((struct ai_vec*)(_))
#define charmp oddp
#define sym(_) ((struct ai_mint*)(_))
#define nom(_) ((struct ai_nom*)(_))
// mintp: a bare POINT -- the KMint object (the nameless atoms: () and the fresh mints).
// namep: a NAMED point -- the KNom object (a name string + a serial, its own kind now,
// not a chain). nomp recognizes EITHER -- a bare point or a named one -- so "symbol-ish"
// holds for gensyms (mints) and named syms alike (CLAUDE.md: mints answer nomp). NEITHER
// is a chain anymore, so a chain is ALWAYS a real compound (formp == chainp): the
// (name . mint)-chain masquerade -- and its special-casing everywhere -- is gone.
static ai_inline bool mintp(word _) { return lamp(_) && cell(_)->ap == lvm_sym; }
static ai_inline bool namep(word _) { return lamp(_) && cell(_)->ap == lvm_nom; }
static ai_inline bool packp(word _) { return lamp(_) && cell(_)->ap == lvm_vec; }
static ai_inline bool strp(word _) { return lamp(_) && cell(_)->ap == lvm_str; }
static ai_inline bool nomp(word _) { return mintp(_) || namep(_); }
// formp: a REAL compound list. A nom is its own kind (not a chain), so this is now exactly
// chainp -- what the surface `two?` nif means and what list-vs-atom logic wants.
static ai_inline bool formp(word _) { return chainp(_); }
// Mutable flat byte string. NOT a data kind: its head word is the
// behaves-as-0 lvm_buf (like lvm_port_io for ports), so the GC walks a buf
// as a plain length-2 thread -- [lvm_buf, backing ai_str, terminator] -- and
// the generic thread sound forwards the embedded string pointer for free; no
// bespoke evac/copy rule, and the data-sentinel mechanism stays reserved for
// kinds that need one. The bytes live in an ordinary ai_str we mutate in place
// (cf. the `to` output port). Earned by the build tools that back-patch a
// binary image in place. Recognized by ap, like iop() for ports.
// (struct ai_buf -- the 2-word wrapper -- now lives in love.h, the buf's public face.)
static ai_inline bool bufp(word _) { return lamp(_) && cell(_)->ap == lvm_buf; }
// A map is a lookup-lambda with stable identity across growth, like the hash it
// replaces (whose struct stayed put while its bucket array reallocated). Two
// threads: a fixed 2-word HEADER [lvm_map_lookup, backing, <tag>] that callers
// hold, and a BACKING [lvm_map_data, putcharm(len), putcharm(cap), k0,v0, … , <tag>]
// it points at -- open-addressed, linear-probed, cap a power of two. Growth
// allocates a new backing and swaps header[1]; the header never moves, so an
// aliased reference (ev's scopes) sees later inserts. Both are plain threads:
// len/cap are fixnums and keys/vals l words, so evac_thread traces them with no
// bespoke GC, like ai_buf. Empty slots hold map_gap, a unique word-aligned
// out-of-pool address gcp leaves untouched, never a legal key and never read as
// a terminator. (m k) looks k up (() if absent) through lvm_map_lookup.
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
static word macroget(struct ai*, word);        // the layered macro read: each layer's table rides its [nil] slot
static struct ai *ai_mapput(struct ai*), *map_new(struct ai*);
static ai_inline struct ai_str *buf_str(word x) { return ((struct ai_buf*) x)->str; }
// the byte ops read from a string or a buf; both resolve to a ai_str of bytes.
static ai_inline struct ai_str *bytes_of(word x) { return bufp(x) ? buf_str(x) : str(x); }
// a COIN: a newtype value, a typed hot. Like a buf, NOT a data kind -- its head word
// is the behaves-as-0 lvm_coin, so GC walks it as a plain length-3 thread [lvm_coin,
// die, payload, terminator] and forwards the two embedded words for free; no
// bespoke evac. ai_kind reads KHot (it is !datp and not a map), so the +/* matrix
// already routes every coin combination to the KHot lane (lvm_addh/lvm_mulh), where a
// coin operand is intercepted. The DIE is the type descriptor (a map keyed by the
// slot fixnums below -- a monoid/ring); every coin of a type is struck from one die.
// The PAYLOAD is the backing value. The ()-identity of +/* is inherited free -- the
// mint-identity hoist sits above the matrix.
struct ai_coin { lvm_t *ap; word die; word payload; };
static ai_inline bool coinp(word _) { return lamp(_) && cell(_)->ap == lvm_coin; }
static ai_inline word coin_die(word x) { return ((struct ai_coin*) x)->die; }
static ai_inline word coin_load(word x) { return ((struct ai_coin*) x)->payload; }
// die slots (fixnum keys into the descriptor map). NAME is a symbol for show;
// ADD/MUL/APPLY are closures run INSIDE the VM (+/* and apply are lvm handlers, so
// they can call love code). net/=/</show/tally default over the payload in pure C. HOT,
// if truthy, makes the die's coins lit? (a reference value); absent -> the coin is
// fresh DATA (not lit?), like a rational -- a value, not a reference. NET is a MODE
// fixnum, not a closure (ai_net is pure C under every truth test and must never
// re-enter the VM): mode 1 nets by the COUNT, so truth reads "has any" even when
// the payload's content sums red (a signed ledger, a polynomial's coefficients);
// mode 2 nets an (n d)-of-reals payload as the RATIO n/d with the sign exact, so
// a rational's truth is its VALUE's sign, not its representation's.
enum { DIE_NAME = 0, DIE_ADD = 1, DIE_MUL = 2, DIE_APPLY = 3, DIE_HOT = 4, DIE_SUB = 5,
       DIE_NET = 6,    // net MODE, a fixnum: absent/0 = net of payload; 1 = net by TALLY (the
                       // count); 2 = RATIO (an (n d)-of-reals payload nets n/d, sign exact)
       DIE_STAR = 7,   // truthy = the die's coins are NUMERIC: numeral application powers them
                       // through their own * (prel num-ap reads this slot; C never does)
       DIE_DIV = 8 };  // `/` -- like `-` it has no kind matrix, so lvm_quot intercepts coins itself
// read a die slot, or () if absent / the die is not a map.
static ai_inline word die_get(struct ai *g, word die, intptr_t slot) {
 return tabp(die) ? ai_mapget(g, nil, putcharm(slot), die) : nil; }
// Arbitrary-precision integer (Step 6). Own data-sentinel kind KBig: a flat,
// GC-trivial object (raw limbs, no embedded l pointers) the copying GC moves
// by memcpy. A generic thread sound can't hold inline limb words (a limb that's
// even-and-in-pool would be spuriously forwarded, one matching ai_thread_tag would
// truncate the object), so a flat bignum needs its own copy/evac rule -- like
// KString strings -- which is exactly what the sentinel buys. slen = signed limb
// count (negative => negative value); |slen| 32-bit limbs little-endian
// (limb[0] least significant), top limb nonzero (normalized). Zero is never a
// bignum (it demotes to the fixnum nil), so slen is never 0 and the sign is
// unambiguous. Canonical demotion keeps the tiers disjoint: a value in fixnum
// range is a fixnum, one in intptr_t range a wide-int box, only wider values a
// bignum -- so charmp/widep/bigp are mutually exclusive and =/eqv stay well defined.
struct ai_big { lvm_t *ap; intptr_t slen; ai_limb limb[]; };
static ai_inline bool bigp(word _) { return lamp(_) && cell(_)->ap == lvm_big; }
static ai_inline struct ai_big *ini_big(struct ai_big *b, intptr_t slen) {
 return b->ap = lvm_big, b->slen = slen, b; }
uintptr_t ai_big_bytes(struct ai_big*);
// Canonicalize a magnitude (limb[0..n), sign neg) into the smallest tier:
// fixnum, else wide-int box, else bignum; bumps *hp when it boxes/bignums. One
// sink shared by the reader and the arithmetic slow paths.
word ai_big_canon(ai_word **hp, ai_limb const *limb, int n, bool neg);
ai_flo_t ai_big_to_flo(word);                 // bignum -> double (used by toflo)
intptr_t ai_big_low(word);                   // bignum value mod 2^W (low machine word)
int ai_big_cmp(word, word);                  // -1/0/1 over two integer operands
struct ai *ai_big_binop(struct ai*, int vop);  // vop_add..vop_rem, packed; pops one operand
struct ai *ai_big_quot_true(struct ai*);       // `/` bignum lane: exact quotient when b | a, else a float box
struct ai *ai_big_dec(struct ai*);             // sp[0] bignum -> decimal string
struct ai *ai_big_read_dec(struct ai*);        // sp[0] [+-]?digits token -> canonical value
struct ai *ai_big_read_hex(struct ai*);        // ..and its [+-]?0x<hexdigits> twin
struct ai *ai_big_read_oct(struct ai*);        // ..and [+-]?0<octdigits>, the third

// A boxed scalar float: its own data sentinel (lvm_flo) and a lean {ap, payload}
// box (struct ai_flo, below) -- two words, vs the four a rank-0 ai_R vec spent.
// Stage 2 of the scalar-gem split: float left the vec machinery, so flop is an
// ap check (no rank/type fields to read), and ai_typ recovers KFlo directly.
static ai_inline bool flop(word _) {
  return lamp(_) && cell(_)->ap == lvm_flo; }
// Wide-integer box: its own data sentinel (lvm_wide) and a lean {ap, payload}
// box (struct ai_wide, below) -- like the float box, two words vs the four a
// rank-0 ai_Z vec spent. Arises only from transparent fixnum overflow
// (kernel/math.c) / bignum demotion; never holds a value that fits the fixnum
// tag (canonical demotion keeps box and fixnum ranges disjoint), so widep and
// charmp never both hold for the same number.
static ai_inline bool widep(word _) {
  return lamp(_) && cell(_)->ap == lvm_wide; }
// A complex scalar: its own data sentinel (lvm_cbox) and a lean three-word
// {ap, re, im} box (struct ai_cplx, below) vs the five a rank-0 ai_C vec spent.
// Deliberately NOT folded into isnum -- the real-tower macros (toflo/toint)
// would misread its two-word payload, so the arith/eq paths handle complex via
// explicit Cp branches placed before the real lanes (complex > float > int/big).
static ai_inline bool Cp(word _) {
  return lamp(_) && cell(_)->ap == lvm_cbox; }
// A typed array. The invariant is RANK, not count: a vec exists at rank>=1 and any
// tally, and only a rank-0 point demotes at the build seam -- so this guard is
// LOAD-BEARING, not the documented assertion it used to be when arrp coincided with
// packp. flop/widep/Cp catch the scalar gems.
// The elementwise arith/compare lanes divert to lvm_vbin when either operand arrp.
static ai_inline bool arrp(word _) { return packp(_) && vec(_)->rank >= 1; }
// A GALAXY: a numeric array (a set of stars). Bands into the number band by its net
// (cmp_rank/cmp3); a tray (ai_O) stays below chain. type != ai_O catches Z/R/C.
static ai_inline bool galaxyp(word _) { return arrp(_) && vec(_)->type != ai_O; }

// Max array rank (bounds the stack index/stride arrays in the broadcast loop).
#define maxrank 8
extern size_t const ai_vt_[];                 // element byte size by ai_vec_type
extern size_t const ai_T[];                   // element byte size by ai_vec_type (used pre-definition by lvm_gauge)
// Element payload: laid out row-major just past the shape words.
static ai_inline void *vec_data(struct ai_vec *v) { return (void*) (v->shape + v->rank); }
// Total element count = product of the dimensions (1 for a rank-0 scalar box).
static ai_inline uintptr_t vec_nelem(struct ai_vec *v) {
 uintptr_t n = 1;
 for (uintptr_t i = 0; i < v->rank; i++) n *= v->shape[i];
 return n; }
static ai_inline struct ai_vec *ini_vec(struct ai_vec *v, enum ai_vec_type t, uintptr_t rank) {
 return v->ap = lvm_vec, v->type = t, v->rank = rank, v; }
// Read element i of v as a double / as an integer (sign-extending the narrow
// integer types; truncating a float toward zero for the int reader). The int
// reader is only used on integer-typed arrays in practice.
static ai_inline ai_flo_t vec_get_flo(struct ai_vec *v, uintptr_t i) {
 void *p = vec_data(v);
 return v->type == ai_R ? ((ai_flo_t*) p)[i] : (ai_flo_t) ((intptr_t*) p)[i]; }
static ai_inline intptr_t vec_get_int(struct ai_vec *v, uintptr_t i) {
 void *p = vec_data(v);
 return v->type == ai_R ? (intptr_t) ((ai_flo_t*) p)[i] : ((intptr_t*) p)[i]; }
// Write element i of v, converting to v's element kind.
static ai_inline void vec_put_int(struct ai_vec *v, uintptr_t i, intptr_t x) {
 void *p = vec_data(v);
 if (v->type == ai_R) ((ai_flo_t*) p)[i] = (ai_flo_t) x; else ((intptr_t*) p)[i] = x; }
static ai_inline void vec_put_flo(struct ai_vec *v, uintptr_t i, ai_flo_t x) {
 void *p = vec_data(v);
 if (v->type == ai_R) ((ai_flo_t*) p)[i] = x; else ((intptr_t*) p)[i] = (intptr_t) x; }
// Read/write element i of a ai_O array as a raw tagged l word (the GC traces
// these; see evac_vec). No conversion -- the slot IS a value.
static ai_inline word vec_get_obj(struct ai_vec *v, uintptr_t i) {
 return ((word*) vec_data(v))[i]; }
static ai_inline void vec_put_obj(struct ai_vec *v, uintptr_t i, word x) {
 ((word*) vec_data(v))[i] = x; }

// Truthiness: x is false iff (= 0 ($ x)) -- the net's sign. NOT read in the total
// order: a twin gem gates on its REAL part, so a pure phase is blue (see zn_false).
// THE NET'S CODOMAIN IS COMPLEX (ai_net below): a complex scalar nets ITSELF, phase
// intact; every other scalar nets on the real line; aggregates SUM as complex
// numbers -- + is total there, so the net is additive EXACTLY, at every rank and
// phase. Truth and $ each observe the net through ONE retraction at the boundary:
// ai_nilp reads the lex sign (re, then im -- the flagged order choice, applied once,
// never per element), ai_pin the order-signed magnitude. The common kinds short-
// circuit with NO walk: nil (the 0 word), EmptyString, fixnum sign, table key count,
// bignum sign, symbol name; chains, vecs, strings and bufs compute their net
// (content measures: an all-NUL text or zeroed buf is nothing, and a sum cannot
// short-circuit -- a later negative can cancel an early positive). Lockstep with
// ai_pin (lvm_pin): same zero conditions.
// a symbol's net, shared by ai_net ($) and ai_nilp (!): the SPELLING's charm
// sum (a symbol nets what its nom chain nets -- chars measure by content now).
// every MINT (the nameless fresh point: materially empty, a DISTINCT NOTHING)
// nets 0 -> falsy; so does an all-NUL spelling: a string of nothings is
// nothing, in or out of a symbol.
static ai_inline struct ai_str *add_name(struct ai *g, word x);   // a named sym (name . mint) -> its name string, else 0
struct ai_zn { ai_flo_t re, im; };                     // the net: a complex value
static ai_inline struct ai_zn zn(ai_flo_t re, ai_flo_t im) {
  struct ai_zn z = {re, im}; return z; }
// THE TRUTH GATE, and it is NOT the total order -- the one place the two part.
// A net is nothing unless its REAL part is positive. For a real net that is the
// order exactly (im == 0 makes the old lexicographic test collapse to re <= 0),
// so only the IMAGINARY AXIS moves: ~(0 1) was true and ~(0 -1) false, which made
// truth depend on which root of x^2+1 we named `i` -- conjugation is a field
// automorphism, so nothing intrinsic separates them, and C admits no order
// compatible with its arithmetic at all. Phase has no sign. So a pure phase is
// BLUE, the zero floor: it has magnitude (|z| is still what $ measures once the
// gate passes) but it is not positively anything. The lexicographic order stays
// exactly as it was -- sorting needs totality, and `i` and `-i` must still be
// distinguishable there; truth is a measure question and does not.
static ai_inline bool zn_false(struct ai_zn z) { return z.re <= 0; }
static struct ai_zn ai_net(struct ai *, word);         // fwd: aggregates sum their elements
static intptr_t ai_count(struct ai *, word);           // fwd: tally's C body (net-mode 1 reads it)
static ai_inline bool ai_nilp(struct ai *g, word x) {
  if (x == nil || x == EmptyString) return true;
  if (charmp(x)) return getcharm(x) < 0;                 // 0 is nil (caught above); negatives false
  if (tabp(x)) return map_len(x) == 0;
  if (bigp(x)) return ((struct ai_big*) x)->slen < 0; // a negative bignum is false
  if (mintp(x)) return true;                         // a bare point (a mint / the zero point) nets 0 -> nil
  if (coinp(x)) return zn_false(ai_net(g, x));      // a coin's truth is its payload's net (lockstep with ai_net/$)
  if (chainp(x) || namep(x) || packp(x) || flop(x) || widep(x) || Cp(x) || strp(x) || bufp(x))
    return zn_false(ai_net(g, x));                   // content measures (a nom by its spelling): the net's real part <= 0
  return false; }                                    // fn / port: present

// Truncation toward zero / float remainder. Pure, freestanding-safe (no libm):
// 1/0 lowers to an FPU divide that yields +-inf or NaN per IEEE, and inf*0=NaN
// propagates through ai_fmod's subtraction. Shared by the scalar arith slow
// paths (math.c) and the elementwise array lane (arr.c).
static ai_inline ai_flo_t ai_trunc(ai_flo_t x) {
 if (x != x) return x;
 ai_flo_t m = x < 0 ? -x : x;
 if (m > (ai_flo_t) 9.22e18) return x;
 return (ai_flo_t) (int64_t) x; }
static ai_inline ai_flo_t ai_fmod(ai_flo_t a, ai_flo_t b) {
 return a - ai_trunc(a / b) * b; }

// --- numeric tower helpers (shared by math.c, arr.c, hash.c) ----------------
// Numeric scalar = a fixnum, a boxed float (flop), or a boxed wide int (widep).
#define isnum(x) (charmp(x) || flop(x) || widep(x) || bigp(x))
// Integer value of a fixnum-or-box operand (callers must exclude floats AND
// bignums -- a bignum doesn't fit an intptr_t; integer lanes guard on !bigp).
#define toint(x) (charmp(x) ? (intptr_t) getcharm(x) : box_get(x))
// Double value of any numeric operand (a bignum widens via ai_big_to_flo).
#define toflo(x) (charmp(x) ? (ai_flo_t) getcharm(x) : flop(x) ? flo_get(x) : widep(x) ? (ai_flo_t) box_get(x) : ai_big_to_flo(x))
// box_req (the Have for emit_int/emit_flo) is defined with the lean box structs
// below -- a float/wide box is two words, no longer the rank-0 ai_R/ai_Z vec it was.
// Heap words for one lean complex box (struct ai_cplx, defined below): ap + re + im.
#define cplx_req Width(struct ai_cplx)
// The tagged fixnum range: putcharm spends one bit, so |value| <= 2^(Bits-2).
#define fix_min (INTPTR_MIN >> 1)
#define fix_max (INTPTR_MAX >> 1)
// Emit an integer result R into `_res`: demote to a fixnum when it fits the
// tag, else box it as a rank-0 ai_Z scalar (bumping Hp). The caller must
// already hold Have(box_req). Takes no &local, so a ap that uses it keeps
// its trailing tail call.
#define emit_int(R) do { intptr_t _r = (R); \
 if (_r >= fix_min && _r <= fix_max) _res = putcharm(_r); \
 else _res = mk_wide(&Hp, _r); } while (0)
// Emit a double result R into `_res` as a rank-0 ai_R box. Same Have(box_req)
// precondition and TCO discipline as emit_int.
#define emit_flo(R) do { _res = mk_flo(&Hp, (R)); } while (0)

// Step 8 -- RNG (kernel/rng.c). State is a rank-1 i64 vec of length 4 (256 bits,
// xoshiro256++). It rides the existing vec machinery (no data sentinel) but its
// payload is treated as raw bytes -- moved by memcpy, never via vec_get/put_int,
// which would truncate the 64-bit limbs to intptr_t on 32-bit ports. The fixed
// 8-byte limbs make a seed reproduce the same sequence on every target.
#define rng_state_len 4
#define rng_payload_bytes (rng_state_len * 8)
#define rng_vec_bytes (sizeof(struct ai_vec) + sizeof(uintptr_t) + rng_payload_bytes)
#define rng_vec_req (b2w(rng_vec_bytes))
// State element kind: pick whichever kind is 8 bytes wide so ai_vec_bytes (GC) sees
// the full 256-bit payload -- Z (one word) on 64-bit, C (two words) on 32-bit.
#define rng_vt (Bytes == 4 ? ai_C : ai_Z)
void ai_rng_seed(struct ai_vec*, uint64_t);   // shape an i64 state vec + seed it (SplitMix64)
lvm_t lvm_wheel, lvm_turn, lvm_turnf;
int memcmp(void const*, void const*, size_t);
void *malloc(size_t), free(void*),
 *memcpy(void*restrict, void const*restrict, size_t),
 *memmove(void*restrict, void const*restrict, size_t),
 *memset(void*, int, size_t);
size_t strlen(char const*);

// The lean scalar-float box: ap (lvm_flo) then one payload word holding the
// punned double -- two words, half the rank-0 ai_R vec it replaced. A GC leaf
// (flat payload, no embedded l pointers), copied/evac'd like a bignum.
struct ai_flo { lvm_t *ap; ai_word w; };
#define flo_req Width(struct ai_flo)
// The lean wide-int box: ap (lvm_wide) then one raw intptr_t payload (no bit
// reinterpretation, unlike the float box). Same shape/size as ai_flo, distinct ap.
struct ai_wide { lvm_t *ap; intptr_t w; };
#define wide_req Width(struct ai_wide)
// Heap words emit_int/emit_flo reserve: a lean float OR wide-int box (both two
// words -- same shape, distinct ap). Was Width(ai_vec)+1 when these emitted a
// rank-0 vec; the Stage-2 split made the box lean and collapsing every RANK-0 array
// at the build seam retired the rank-0 vec for good. (A rank-1-len-1 stays an array
// and never was the thing this rested on -- the saving is rank-0's alone.)
#define box_req (flo_req > wide_req ? flo_req : wide_req)
// The lean complex box: ap (lvm_cbox) then two punned-double payload words
// (re, im) -- three words vs the five a rank-0 ai_C vec spent. Also a flat GC leaf.
struct ai_cplx { lvm_t *ap; ai_word re, im; };
// Boxed scalar float access. The payload occupies one uintptr_t-wide
// word (ai_flo_t is f64 on 64-bit ports, f32 on 32-bit -- always
// the width of uintptr_t). Pun through a union rather than
// memcpy(&local, ...): both are strict-aliasing clean, but the memcpy form
// takes the address of a stack local, which clang -Os treats as an escape
// and then refuses to sibling-call the trailing Continue() out of any VM
// ap that inlines this -- silently breaking threaded dispatch (a
// `call`+`ret` where there must be a `jmp`; see tools/vmret.l). GCC proves
// the local dead and TCOs either way; the union keeps the value in a
// register so clang does too.
_Static_assert(sizeof(ai_flo_t) == sizeof(uintptr_t), "float box assumes ai_flo_t is pointer-width");
typedef union { uintptr_t u; ai_flo_t d; } ai_flo_pun;
static ai_inline ai_flo_t flo_get(word x) {
 return ((ai_flo_pun){ .u = ((struct ai_flo*) x)->w }).d; }
static ai_inline void flo_put(void *p, ai_flo_t v) {
 // NaN collapses to 0.0: love's "undefined is nothing" convention (a type error
 // is nil) reaching the floats. This is the single real-float box-write, so 0/0
 // and every other indeterminate land on 0 -- the value NaN's own net ($ = 0)
 // already claimed. Restores the total order (NaN was its only incomparable
 // point) and the !x == (0 = $x) coherence (NaN alone netted 0 yet read truthy).
 // `inf` is untouched (it is comparable); complex NaN rides cplx_put, not here.
 if (v != v) v = 0;                              // v != v iff v is NaN
 *(uintptr_t*) p = ((ai_flo_pun){ .d = v }).u; }
// Allocate a lean float box at *hpp (caller holds Have(flo_req)) and return it.
// Takes no &local, so a VM ap that uses it keeps its trailing tail call.
static ai_inline word mk_flo(ai_word **hpp, ai_flo_t v) {
 struct ai_flo *f = (struct ai_flo*) *hpp; *hpp += flo_req;
 f->ap = lvm_flo; flo_put(&f->w, v); return word(f); }

// Boxed complex access: the lean box's two punned-double payload words. Same
// union-pun discipline as flo_get so an inlining VM ap keeps its tail call.
static ai_inline ai_flo_t cplx_re(word x) {
 return ((ai_flo_pun){ .u = ((struct ai_cplx*) x)->re }).d; }
static ai_inline ai_flo_t cplx_im(word x) {
 return ((ai_flo_pun){ .u = ((struct ai_cplx*) x)->im }).d; }
// |z| of a boxed complex scalar: the L2 norm of (re, im).
static ai_inline ai_flo_t cplx_mod(word x) {
 ai_flo_t re = cplx_re(x), im = cplx_im(x);
 return ai_sqrt(re * re + im * im); }
// cplx_set writes both components of an already-shaped box; mk_cplx allocates one
// at *hpp (caller holds Have(cplx_req)) and fills it. Neither takes a &local, so
// a VM ap that uses them keeps its trailing tail call.
static ai_inline void cplx_set(struct ai_cplx *v, ai_flo_t re, ai_flo_t im) {
 v->re = ((ai_flo_pun){ .d = re }).u;
 v->im = ((ai_flo_pun){ .d = im }).u; }
static ai_inline word mk_cplx(ai_word **hpp, ai_flo_t re, ai_flo_t im) {
 struct ai_cplx *v = (struct ai_cplx*) *hpp; *hpp += cplx_req;
 v->ap = lvm_cbox; cplx_set(v, re, im); return word(v); }

// Boxed wide-int read: the lean box's raw intptr_t payload (no bit
// reinterpretation, unlike the float box). Writes go through mk_wide below.
static ai_inline intptr_t box_get(word x) { return ((struct ai_wide*) x)->w; }
// Allocate a lean wide-int box at *hpp (caller holds Have(wide_req)) and return
// it. Takes no &local, so a VM ap that uses it keeps its trailing tail call.
static ai_inline word mk_wide(ai_word **hpp, intptr_t v) {
 struct ai_wide *w = (struct ai_wide*) *hpp; *hpp += wide_req;
 w->ap = lvm_wide; w->w = v; return word(w); }

// equality comparisons inline the fast identity check
ai_noinline bool eqv(struct ai*, word, word); // this is for checking equality of non-identical values
static bool eqv_at(struct ai*, word, word, word*); // eqv with an explicit worklist base (for re-entrant calls from the beta bridge)
// eqv has no value-equality for two distinct fixnums (odd; its only cross-value
// bridge, 0/1 <-> (\ _ 1)/(\ x x), needs a heap lambda) nor for two distinct POINTS
// (mints/noms -- identity is their whole equality: interned syms collapse per
// spelling, distinct noms are distinct keys; eqv's switch has no KMint/KNom arm).
// Both answer false by identity alone, so eql settles them inline and skips the
// noinline call -- the hot path under map-key and scope-variable lookup, which
// compare nom/symbol keys by the thousand while compiling. lamp is evenp (no
// deref); () is ZeroPoint, an immortal const mint, so pointp's ap read is always safe.
static ai_inline bool pointp(word x) {
 lvm_t *p; return lamp(x) && ((p = cell(x)->ap) == lvm_sym || p == lvm_nom); }
static ai_inline bool eql(struct ai *g, word a, word b) {
 return a == b ? true : (a & b & 1) || (pointp(a) && pointp(b)) ? false : eqv(g, a, b); }

// Threads -- and every other variable-length heap object the GC copies by
// sounding (continuations, task nodes, env scopes, ports) -- end with a single
// tag word: the object's own head pointer with bit 1 set (ai_thread_tag), saving a
// word over a separate NULL marker + head. Small ints are odd and l heap
// pointers are word-aligned, so the only other word that can carry (x & 3) == 2
// is an embedded *external* pointer (host data/function) that happens to land
// on a 2-byte boundary. So the terminator test is not just the tag bits: the
// payload must also point back into [lo, hi), the pool the object lives in --
// which a stray external pointer never does.
#define ai_thread_tag 2
static ai_inline bool tagp(word x, word const *lo, word const *hi) {
 word const *p = (word const*) (x & ~(word) 3);
 return (x & 3) == ai_thread_tag && p >= lo && p < hi; }
// A terminator is tag-2 with its payload (the object head) in a LIVE heap region. GC scans
// run with DIFFERENT [lo,hi) -- a minor's copy uses the young from-range, but evac/scan-in-place
// use the major to-range -- so a terminator must be recognized by which pool its head lands in,
// NOT the single range the caller happens to scan. Else a young-pointing terminator met under
// the major range is missed, gcp'd as a field, and followed off the heap (the kernel tco=1 #PF).
// A stray 2-byte-aligned EXTERNAL pointer lands in NO pool, so it is still rejected. Live
// regions: the minor/main pool, the current to-space (a major's resized half), both major halves.
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
 // scan k forward to its terminator (which points back into the SAME pool the object lives in). A
 // tenured object sits in the major pool, outside the main pool -- generationally graduated, so check.
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

// The unique empty string and empty (anonymous) symbol. Both live in the data
// segment, so the Cheney forwarder leaves any pointer to them untouched (gcp's
// out-of-pool short-circuit, like ai_stdin/stdout/stderr) -- immortal, never copied
// or freed, so `const` is safe. Strings are immutable, so a single empty string
// suffices and we NEVER heap-allocate a zero-length one (str0/strin/the reader and
// the `+` string lane all hand back ai_str_empty). Predicates read `ap`, so it
// behaves as a normal string value; the FAM `bytes[]` is simply absent (len 0).
// External linkage (declared in love.h with the EmptyString macro) so the
// frontends can return it too (e.g. host_run's empty-output capture). (the
// empty SYMBOL died in the one-nothing round: () reads as 0.)
const struct ai_str ai_str_empty = { .ap = lvm_str, .len = 0 };
// () -- the one serial-0 mint, shared by every core (serial 0 is never drawn, so
// it is unique + least in the order). See the ZeroPoint macro in love.h.
const struct ai_mint ai_mint_zero = { .ap = lvm_sym, .code = 0 };


static ai_inline uintptr_t rot(uintptr_t x) {
  int const s = sizeof(uintptr_t) * 4; // shift bits = word bits / 2 = sizeof(word) * 4
  return (x << s) | (x >> s); }

extern struct ai_port_vt const synth[];

static ai_inline void *off_pool(struct ai *g) {
 return g == g->pool ? (word*) g->pool + g->len : (word*) g->pool; }
static ai_inline struct ai *pushq(struct ai*g) { return intern(ai_strof(g, "\\")); }
static ai_inline struct ai *push0(struct ai*g) { return ai_push(g, 1, nil); }
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
// A *fairness* yield (deep in compute, not an explicit I/O/timer wait): clear any
// stale next_wait_fd/next_wake_at first. Those are one-shot intentions set right
// before an explicit yield (lvm_fgetc/lvm_sleep), and lvm_fgetc never clears the fd
// on a successful read -- so after e.g. `(slurp nic)` it lingers at the nic fd. If a
// periodic yield then inherits it, yield_sw saves this task as parked on that fd and
// find_runnable never reschedules it until the fd happens to fire again (deadlock).
// Same hazard the lvm_wait path already guards against.
#define YieldCheck() \
  if (g->tasks->m != g->tasks && ++g->yield_ctr >= yield_interval) \
    { g->next_wait_fd = -1; g->next_wake_at = 0; return Ap(lvm_yield_sw, g); }
#define argn(nom, i) lvm(nom) { Have1(); Sp[-1] = Sp[i]; Sp -= 1; Ip += 1; return Continue(); }
#define quon(nom, v) lvm(nom) { Have1(); Sp -= 1; Sp[0] = putcharm(v); Ip += 1; return Continue(); }

#define Ana(n, ...) struct ai *n(struct ai *g, struct env **c, intptr_t x, ##__VA_ARGS__)
#define Cata(n, ...) struct ai *n(struct ai *g, struct env **c, ##__VA_ARGS__)
#define incl(e, n) ((e)->len += ((n)<<1))
#define Kp (g->ip)
#define cata1(n, ...) static Cata(n) { return __VA_ARGS__, pull(g, c); }
#define forget() (ai_core_of(g)->root=(mm0),g)

#define fs0(g) (ai_core_of(g)->sp[0])
#if UINTPTR_MAX > 0xffffffffu
// the infinity test is > the LARGEST FINITE double, so the whole finite range
// prints digits -- a 1e308 threshold sent (1e308, DBL_MAX] to "ieee-inf", a
// finite number wearing infinity's face (the 32-bit branch always had it right)
#define dtoa_inf    __DBL_MAX__
#define dtoa_sci_hi 1e16
#define dtoa_sci_lo 1e-4
#else
#define dtoa_inf    __FLT_MAX__
#define dtoa_sci_hi 1e16f
#define dtoa_sci_lo 1e-4f
#endif

#define s1(i) {{i}, {lvm_ret0}}
#define s2(i) {{lvm_cur},{.x=putcharm(2)},{i}, {lvm_ret0}}
#define s3(i) {{lvm_cur},{.x=putcharm(3)},{i}, {lvm_ret0}}
#define s4(i) {{lvm_cur},{.x=putcharm(4)},{i}, {lvm_ret0}}
#define s5(i) {{lvm_cur},{.x=putcharm(5)},{i}, {lvm_ret0}}
// `+` and `*` OF TWO FUNCTIONS are the prel's `stack` and `compose` -- two lines of
// love, reached from lvm_addh/lvm_mulh through g->hot_stack / g->hot_compose. They used
// to be hand-transcribed compiler output baked as immortal C threads here, on the
// grounds that being closure-free they "are not lisp"; but the hook mechanism costs a
// slot, the definitions ARE the Church laws (love/prel.l states them), and a lisp
// closure prints as itself where a bare thread does not.
#define nifs(_) \
 _(nif_clock, "clock", s1(lvm_clock)) _(nif_nclock, "nclock", s1(lvm_nclock)) _(nif_please, "please", s1(lvm_please))\
 _(nif_gauge, "gauge", s1(lvm_gauge)) _(nif_apof, "apof", s1(lvm_apof))\
 _(nif_seal, "seal-hook", s2(lvm_seal)) _(nif_books, "books", s1(lvm_books)) _(nif_setbooks, "setbooks", s1(lvm_setbooks)) _(nif_mods, "mods", s1(lvm_mods)) _(nif_lib, "lib", s1(lvm_lib))\
 _(nif_add, "+", s2(lvm_add)) _(nif_sub, "-", s2(lvm_sub)) _(nif_mul, "*", s2(lvm_mul))\
 _(nif_quot, "/", s2(lvm_quot)) _(nif_fquot, "//", s2(lvm_fquot)) _(nif_rem, "%", s2(lvm_rem)) \
 _(nif_lt, "<", s2(lvm_lt))  _(nif_le, "<=", s2(lvm_le)) _(nif_eq, "=", s2(lvm_eq))\
 _(nif_ge, ">=", s2(lvm_ge))  _(nif_gt, ">", s2(lvm_gt)) \
 _(nif_same, "id?", s2(lvm_same)) \
 _(nif_bsl, "<<", s2(lvm_bsl)) _(nif_bsr, ">>", s2(lvm_bsr))\
 _(nif_band, "&", s2(lvm_band)) _(nif_bor, "|", s2(lvm_bor)) _(nif_bxor, "^", s2(lvm_bxor))\
 _(nif_link, "link", s2(lvm_link)) _(nif_car, "cap", s1(lvm_cap)) _(nif_cdr, "cup", s1(lvm_cup))\
 _(nif_sort, "sort", s1(lvm_sort)) _(nif_tally, "tally", s1(lvm_tally)) \
 _(nif_snip, "snip", s3(lvm_snip)) \
 _(nif_sound0, "sound0", s1(lvm_sound0))\
 _(nif_string, "string", s1(lvm_string))\
 _(nif_intern, "intern", s1(lvm_intern)) _(nif_mint, "mint", s1(lvm_mint))\
 _(nif_nomctor, "nom", s1(lvm_nomctor))\
 _(nif_spin, "spin", s1(lvm_spin))\
 _(nif_peek, "peek", s2(lvm_peek)) _(nif_poke, "poke", s3(lvm_poke)) _(nif_trim, "trim", s1(lvm_trim))\
 _(nif_seek, "seek", s2(lvm_seek)) _(nif_pin, "saturate", s1(lvm_pin)) _(nif_peep, "peep", s3(lvm_peep))\
 _(nif_put, "pin", s3(lvm_put)) _(nif_pull, "pull", s3(lvm_pull))\
 _(nif_table, "tablet", s1(lvm_tablet)) _(nif_keys, "keys", s1(lvm_keys))\
 _(nif_dig, "dig", s1(lvm_dig))\
 _(nif_bufnew, "cask", s1(lvm_bufnew)) _(nif_bcopy, "pour", s5(lvm_bcopy))\
 _(nif_chainp, "two?", s1(lvm_chainp)) _(nif_strp, "string?", s1(lvm_strp))\
 _(nif_real, "gem", s1(lvm_real)) _(nif_flop, "gem?", s1(lvm_flop))\
 _(nif_sin, "sine", s1(lvm_sin)) _(nif_cos, "cosine", s1(lvm_cos))\
 _(nif_log, "log", s1(lvm_log)) _(nif_pow, "power", s2(lvm_pow))\
 _(nif_cplx, "twin", s2(lvm_cplx)) _(nif_Cp, "twin?", s1(lvm_Cp))\
 _(nif_re, "re", s1(lvm_re)) _(nif_im, "im", s1(lvm_im)) _(nif_conj, "conj", s1(lvm_conj))\
 _(nif_abs, "abs", s1(lvm_abs)) _(nif_arg, "arg", s1(lvm_carg))\
 _(nif_tray, "tray", s3(lvm_tray))\
 _(nif_iota, "iota", s1(lvm_iota))\
 _(nif_nif, "nif", s4(lvm_nif))\
_(nif_nifx, "nifx", s5(lvm_nifx))\
 _(nif_rank, "rank", s1(lvm_rank))\
 _(nif_alen, "alen", s1(lvm_alen)) _(nif_shape, "shape", s1(lvm_shape))\
 _(nif_atype, "atype", s1(lvm_atype))\
 _(nif_asum, "asum", s1(lvm_asum)) _(nif_aprod, "aprod", s1(lvm_aprod))\
 _(nif_max, "max", s1(lvm_max)) _(nif_min, "min", s1(lvm_min))\
 _(nif_aall, "aall", s1(lvm_aall)) _(nif_inner, "inner", s2(lvm_inner)) _(nif_outer, "outer", s2(lvm_outer))\
 _(nif_packp, "packp", s1(lvm_packp)) _(nif_bigp, "big?", s1(lvm_bigp)) _(nif_widep, "sun?", s1(lvm_widep))\
 _(nif_setp, "tray?", s1(lvm_setp)) _(nif_intf, "int", s1(lvm_intf))\
 _(nif_nomp, "nom?", s1(lvm_nomp)) _(nif_namep, "name?", s1(lvm_namep)) _(nif_tabp, "tablet?", s1(lvm_tabp)) _(nif_charmp, "charm?", s1(lvm_charmp))\
 _(nif_litp, "lit?", s1(lvm_litp)) _(nif_hotp, "hot?", s1(lvm_hotp))\
 _(nif_nilp, "nil?", s1(lvm_nilp)) _(nif_ev, "ev", s1(lvm_eval))\
 _(nif_callk, "call-cc", s1(lvm_callk)) _(nif_scare, "scare", s2(lvm_scare))\
 _(nif_missing, "missing", s2(lvm_missing)) _(nif_yield, "yield", s1(lvm_yield_nif)) \
 _(nif_spawn, "twirl", s2(lvm_spawn)) _(nif_wait, "catch", s1(lvm_wait)) \
 _(nif_sleep, "rest", s1(lvm_sleep)) _(nif_donep, "back?", s1(lvm_donep)) \
 _(nif_hush, "freeze", s1(lvm_hush)) \
 _(nif_key, "cue?", s1(lvm_key)) \
 _(nif_fputbn, "putbn", s3(lvm_fputbn))\
 _(nif_fputx, "print", s2(lvm_fputx))\
 _(nif_await, "await", s1(lvm_await))\
 _(nif_fgetc, "see", s1(lvm_fgetc)) _(nif_fungetc, "unsee", s2(lvm_fungetc))\
 _(nif_fputc, "put", s2(lvm_fputc)) _(nif_fputs, "say", s2(lvm_fputs))  _(nif_fflush, "flush", s1(lvm_fflush))\
 _(nif_dot, "dot", s1(lvm_dot))\
 _(nif_wheel, "wheel", s1(lvm_wheel))\
 _(nif_turn, "turn", s1(lvm_turn)) _(nif_turnf, "turnf", s1(lvm_turnf))\
 _(nif_coinmk, "coin", s2(lvm_coinmk)) _(nif_load, "load", s1(lvm_load))\
 _(nif_dieof, "die-of", s1(lvm_dieof)) _(nif_coinp, "coin?", s1(lvm_coinp))\
 _(nif_calloutdrive, "calloutdrive", s1(lvm_calloutdrive)) _(nif_calloutresume, "calloutresume", s1(lvm_calloutresume))
#define native_implemented_function(n, _, d) static union u const n[] = d;
#define insts(_) _(lvm_unc) _(lvm_index) _(lvm_ret) _(lvm_ap) _(lvm_tap) _(lvm_apn) _(lvm_tapn)\
  _(lvm_jump) _(lvm_cond) _(lvm_arg) _(lvm_quote) _(lvm_defglob)\
  _(lvm_argap) _(lvm_quoteap) _(lvm_argtap)\
  _(lvm_arg0) _(lvm_arg1) _(lvm_arg2) _(lvm_arg3)\
  _(lvm_quo0) _(lvm_quo1) _(lvm_quo2) _(lvm_quo3) _(lvm_quom1) _(lvm_quom2)
#define niff(b, n, _) {n, (intptr_t) b},
#define i_entry(i) {#i, (intptr_t) i},

// ============================================================================
// g
// ============================================================================
enum ai_status ai_fin(struct ai *g) {
 enum ai_status s = ai_code_of(g);
 if ((g = ai_core_of(g))) {
   for (struct ai_fz *fz = g->fz; fz; fz->fn(fz->p), fz = fz->next); // run finalizers
   g->alloc(g, g->pool, 0); }
 return s; }

struct ai *ai_defn(struct ai*g, struct ai_def const*defs, uintptr_t n) {
 for (g = ai_push(g, 1, A(ai_core_of(g)->book)); n--;
  g = ai_mapput(intern(ai_strof(ai_push(g, 1, defs[n].x), defs[n].n))));
 ai_core_of(g)->sp++;
 return g; }

nifs(native_implemented_function);

static lvm(_lvm_yield_c) { return Pack(g), g; }
static union u const yield_c[] = { {_lvm_yield_c} };

// lvm_help: the default help ap, a first-class vm ap (declared in love.h with
// ret0/cur/port_io). A raise enters it with the raised status encoded into g
// (see ghelp2 below). A scare re-encodes and yields to C. (The MORE bit used to
// arrive here too, as read control flow; the reader answers its own nothings as
// values now, so the scare lane is the whole of it.) Define a global
// `help` function to land raises in l instead.
// the scare exit door: re-encode and yield to C. Lives OUTSIDE the lvm_*
// namespace (the underscore convention, like _lvm_yield_c above) because it
// is the one designed return -- the vmret gate's no-ret invariant sounds
// lvm_*, and lvm_help reaches here by tail call (a jmp under ai_tco).
static lvm(_lvm_help_scare, enum ai_status s) { return Pack(g), encode(g, s); }
lvm(lvm_help) {
 enum ai_status s = ai_code_of(g);
 return Ap(_lvm_help_scare, ai_core_of(g), s); }
static union u const raise_c[] = { {lvm_help} };

// ghelp2/ghelp are defined after numap_drive (the help call frame runs
// through its 3-arg twin); declared in love.h.

static struct ai_def const def1[] = { nifs(niff) insts(i_entry)};

// reverse-lookup a function value against the builtin table -> its source name,
// or NULL. Used by the printer to render nifs (e.g. `+`) by name.
char const *ai_nif_name(intptr_t x) {
 for (uintptr_t i = 0; i < countof(def1); i++) if (def1[i].x == x) return def1[i].n;
 return 0; }

static struct ai *ai_ini_0(struct ai*g, uintptr_t len0, void *(*al)(struct ai*, void*, size_t)) {
 memset(g, 0, sizeof(struct ai));      // the core needs no leading ap: () is the const ZeroPoint, never (word)g
 g->len = len0, g->pool = (void*) g, g->alloc = al;
 g->scare_a = g->scare_b = nil;        // v0..end is GC-walked: raw 0 is not a value
 g->hot_read = g->hot_numap = g->hot_stack = g->hot_compose = g->hot_opfix = nil;   // unsealed: hot_hook traps until (seal-hook) fills them
 g->mods = nil;                       // the module registry: lazily created by the first (mods _) read
 g->lib = nil;                        // the source library: same lazy shape ((lib _) / ai_lib_)
 g->hp = g->end, g->sp = (word*) g + len0, g->ip = (union u*) yield_c, g->t0 = ai_clock();
 g->minor = g->end;                  // generational watermark: nothing tenured yet (the first collection sets it)
 // generational setup: a remembered set (old objects holding a young pointer) + the MAJOR pool (a
 // separate two-space for tenured objects). Both via g->alloc -- the collector rides the frontend's
 // heap, so a frontend that cannot supply them cannot run (the generational collector is the only one).
 g->major_len = ai_major0;                                         // initial major half (configurable); grows in gen_major, much less often than the minor
 g->rem = g->alloc(g, NULL, AI_REM_CAP * sizeof(word));
 g->major_pool = g->rem ? g->alloc(g, NULL, 2 * g->major_len * sizeof(word)) : NULL;
 if (!g->major_pool) { if (g->rem) g->alloc(g, g->rem, 0); return encode(g, ai_status_scare); }
 g->major_base = g->major_hp = g->major_pool, g->rem_cap = AI_REM_CAP, g->budget = ai_budget;
 // book + macro maps (lookup-lambdas) then the main task thread.
 if (ai_ok(g = map_new(g)) && ai_ok(g = map_new(g)) && ai_ok(g = ai_have(g, 6))) {
  union u *M = bump(g, 6);            // sp[0]=macro, sp[1]=book (no GC since ai_have)
  M[0].m = M;
  M[1].x = nil;   // sentinel; replaced on first yield
  M[2].x = nil;   // main pid
  M[3].x = nil;   // wake_at: nil means "always runnable"
  M[4].x = putcharm(-1);  // wait_fd: -1 = not waiting on I/O (slot value -1, non-zero)
  g->tasks = tagthread(M, 5);
  // book[nil] = macro (the macro table -- no separate field). Both are on the
  // stack; push the nil key so (sp2,sp1,sp0)=(book,macro,nil) for ai_mapput.
  g = ai_push(g, 1, nil);
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
   // max-charm/min-charm: this build's fixnum bounds, exposed so width-specific
   // tests gate on the real boundary (it differs on 32- vs 64-bit ports).
   {"max-charm", putcharm((ai_word)((uintptr_t)-1 >> 2))},
   {"min-charm", putcharm(-(ai_word)((uintptr_t)-1 >> 2) - 1)},
   // love-tco: 1 = the tail-threaded VM, 0 = the trampoline. glazed code CONTINUES
   // by tail-jump (registers carry Ip/Hp/Sp), which only the threaded build
   // honors -- auto.l reads this and keeps the pure interpreter on a trampoline
   // build (a tco=0 host with the glaze installed segfaulted on its first hot loop).
   {"love-tco", putcharm(ai_tco)}, };
  g = ai_defn(g, def0, countof(def0));
  g = ai_defn(g, def1, countof(def1));
  // `love-version`: the build's version-control id (love_version.h), surfaced on init so the user
  // can read the running version. A non-fixnum global, harmlessly skipped by ev.l's pureset.
  if (ai_ok(g = ai_strof(g, AI_VERSION))) {
   struct ai_def vd[] = {{"love-version", ai_pop1(g)}};
   g = ai_defn(g, vd, countof(vd)); }
  // `love-arch`: the host CPU the glaze emits for. auto-ev interns it as the assembler
  // target ('x64 / 'arm64) and gates the still-x86-only lanes (float / loops).
#if defined(__x86_64__)
  #define AI_ARCH "x64"
#elif defined(__aarch64__)
  #define AI_ARCH "arm64"
#elif defined(__riscv)
  #define AI_ARCH "riscv64"
#else
  #define AI_ARCH "other"
#endif
  if (ai_ok(g = ai_strof(g, AI_ARCH))) {
   struct ai_def ad[] = {{"love-arch", ai_pop1(g)}};
   g = ai_defn(g, ad, countof(ad)); }
  // the 'missing condition tag needs no pre-intern: it is the `missing` nif's
  // name, so installing that nif interns it and the book roots it; the raise
  // path reads it back alloc-free via sym_probe (lvm_index/lvm_missing).
  // the reader owns no operator tables: book['operators] (the ONE table,
  // symbol -> arity | (name . arity)) is seeded by the prel, and the
  // opfix source pass (prel.l, hooked by both compilers at c0 and feel)
  // factors sigil tokens against it at compile time. data reading is
  // purely structural.
 }
 return g; }

struct ai *ai_ini_m(void *(*al)(struct ai*, void*, size_t)) {
 uintptr_t const len0 = ai_minor0;   // initial MINOR pool (the main pool); GROWS on demand (gen_grow), so a
                                       // tiny device overrides ai_minor0 small and scales up only as needed
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
 if (!ai_ok(g = ai_please(g, n))) return ghelp(g);
 return Unpack(g), Continue(); }

static word gcp(struct ai*, word, word const *, word const *);

static ai_inline void evac_chain(struct ai*g, word const*const p0, word const*const t0) {
 struct ai_chain *w = (struct ai_chain*) g->cp;
 g->cp += Width(struct ai_chain);
 w->a = gcp(g, w->a, p0, t0);
 w->b = gcp(g, w->b, p0, t0); }

static ai_inline void evac_vec(struct ai*g, word const*const p0, word const*const t0) {
 struct ai_vec *v = vec(g->cp);
 g->cp += b2w(ai_vec_bytes(v));
 if (v->type != ai_O) return;                 // numeric vecs are GC leaves (flat payload)
 word *e = (word*) vec_data(v);              // object vec: forward each live element word
 uintptr_t n = vec_nelem(v);
 while (n--) e[n] = gcp(g, e[n], p0, t0); }

static ai_inline void evac_str(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += b2w(sizeof(struct ai_str) + str(g->cp)->len); }

static ai_inline void evac_big(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += b2w(ai_big_bytes((struct ai_big*) g->cp)); }

// the lean float box is a flat GC leaf (ap + payload): advance past its two words.
static ai_inline void evac_flo(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += flo_req; }

// the lean wide-int box is the same flat GC leaf shape.
static ai_inline void evac_wide(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += wide_req; }

// the lean complex box: a flat three-word GC leaf.
static ai_inline void evac_cplx(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += cplx_req; }

static ai_inline void evac_sym(struct ai*g, word const*const p0, word const*const t0) {
 g->cp += Width(struct ai_mint); }              // uniform 2 words; copy_sym forwards the serial

static ai_inline void evac_nom(struct ai*g, word const*const p0, word const*const t0) {
 struct ai_nom *w = (struct ai_nom*) g->cp;
 g->cp += Width(struct ai_nom);                 // 4 words; forward the name string (serial + dig are scalars)
 w->name = gcp(g, w->name, p0, t0); }

static ai_inline void evac_thread(struct ai *g, word const *const p0, word const*const t0) {
  // a terminator (head in ANY live pool, tagl) ends the thread -- recognized regardless of which
  // space the scan runs over, so a young-pointing terminator here isn't gcp'd as a field and
  // followed off the heap (the kernel tco=1 #PF). A stray external content word is still rejected.
  for (g->cp += 1; !tagl(g, g->cp[-1]); g->cp[-1] = gcp(g, g->cp[-1], p0, t0), g->cp++); }

static ai_inline void evac_data(struct ai *g, word const *const p0, word const*const t0) {
  switch (typ(g->cp)) {
   default: __builtin_trap();
   case KVec: return evac_vec(g, p0, t0);
   case KMint: return evac_sym(g, p0, t0);
   case KNom: return evac_nom(g, p0, t0);
   case KChain: return evac_chain(g, p0, t0);
   case KString: return evac_str(g, p0, t0);
   case KBig: return evac_big(g, p0, t0);
   case KFlo: return evac_flo(g, p0, t0);
   case KWide: return evac_wide(g, p0, t0);
   case KCplx: return evac_cplx(g, p0, t0); } }

// ===== generational write barrier (stage 2) =====================================
// A MINOR collection scavenges only [minor, hp); it finds the young objects an OLD
// object still points at through the REMEMBERED SET -- gen_wb records any old `src`
// that takes a young `p`, so the minor rescans src. Every old->young edge an
// EXECUTION mints -- a map pin, a store -- goes through gen_wb, so a minor
// under a complete rem set is sound -- exactly the barrier proof/rocq/gc.v's `barrier_sound`
// proves (rem_complete => the minor reaches every young object the mutator can). See
// doc/gengc.md.
//
// The rem set is the WHOLE barrier: every old-cell mutation is remembered precisely --
// maps/rings via gen_wb, c0's stores and ev's poke via gen_wb_cell/gen_wb_two (the
// exact mutated cell of a TAGGED span, or the mutated cons -- both shapes
// gen_scan_inplace re-walks soundly; poke's tagged-span contract at lvm_poke). The
// one escape is rem-set OVERFLOW (rem_miss): a dropped entry forces the next
// collection to a MAJOR, which traces from roots with no rem set and so is
// unconditionally sound.
//
// young?: a heap pointer allocated since the last collection -- in [minor, hp).
// The ADDRESS is the generation (love objects carry no age bit).
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
// the write barrier: an old `src` gains a young key/value/backing/tail `p` -> remember
// src so a minor rescans it. Old maps (a pin) and reader spines (set-tail) are the
// structures execution/the reader mutate in place; everything else is immutable, so
// this is the whole hot-path barrier.
static ai_inline void gen_wb(struct ai *g, word src, word p) {
 if (lamp(src) && ai_young(g, p) && !ai_young(g, src)) gen_remember(g, src); }
static ai_inline bool ai_major_cell(struct ai *g, word *c) {       // a tenured cell: inside the major pool
 return (ai_word*) c >= g->major_base && (ai_word*) c < g->major_hp; }
// the cell barrier (c0's stores, ev's poke): a store of a young value into a tenured
// cell, remembered by the smallest scannable unit around it so the next minor
// re-promotes what the store planted.
//   gen_wb_cell -- the cell sits in a TAGGED span (a thread under construction, a
//     struct env): gen_scan_inplace runs [cell, terminator), covering the store.
//     Never a chain's field: data has no terminator, the scan would walk off the object.
//   gen_wb_two -- a CONS mutation: remember the cons (the KChain lane rescans a+b).
//     ⚠ THIS IS NOT A DOOR. patching a cons in place is OFF-ROAD here -- build the
//     new structure and publish it, or mutate a TABLET, whose `pin` barriers
//     itself. the five callers left all sit inside the letrec analyzer (ana_d,
//     c1_recv) and each patches a list that function consed itself moments
//     earlier and nobody else holds; that confinement is the whole reason they
//     are survivable, and it is not a precedent.
// Both mask g: a call site may hold a scared g mid-chain.
static ai_inline void gen_wb_cell(struct ai *g, void *cl, word v) {
 g = ai_core_of(g);
 if (ai_young(g, v) && ai_major_cell(g, cl)) gen_remember(g, (word) cl); }
static ai_inline void gen_wb_two(struct ai *g, word two, word v) {
 g = ai_core_of(g);
 if (ai_young(g, v) && ai_major_cell(g, ptr(two))) gen_remember(g, two); }
// (Stage 2's reachability AUDIT -- which proved the rem set complete for a hypothetical
// minor -- is retired here: its old-region was [end, minor), which the two-pool layout
// replaces, and the live MINOR below is itself verified end-to-end by the differential
// oracle (the whole corpus runs byte-identical with minors firing). rem_miss stays 0.)
//
// gen_scan_inplace: a MAJOR-pool object that points into the young set (a remembered map
// backing/header, a task-ring node, or the weak intern map) is NOT moved by a minor --
// it stays tenured. But its young fields must be promoted, so gcp each outgoing pointer
// IN PLACE, rewriting the field to its promoted address. This is evac_* without the
// relocation; a thread's terminator sits in the major (the to-space, gc_to_{lo,hi}).
static void gen_scan_inplace(struct ai *g, word obj, word const *p0, word const *t0) {
 union u *p = cell(obj);
 if (datp(obj)) switch (typ(obj)) {
  case KChain: { struct ai_chain *w = two(obj);
                 w->a = gcp(g, w->a, p0, t0), w->b = gcp(g, w->b, p0, t0); break; }
  case KVec:   { struct ai_vec *v = vec(p); if (v->type == ai_O) { word *e = (word*) vec_data(v);
                 for (uintptr_t i = 0, ne = vec_nelem(v); i < ne; i++) e[i] = gcp(g, e[i], p0, t0); } break; }
  case KNom:   { nom(p)->name = gcp(g, nom(p)->name, p0, t0); break; }
  default: break;                                  // KMint/KString/KBig/KFlo/KWide/KCplx: pointer-free leaves
 } else { for (union u *q = p; !tagl(g, q->x); q++) q->x = gcp(g, q->x, p0, t0); } }   // a thread: every word to the tag terminator (tagl: head in any live pool)
          // INCLUDING word0 -- a normal thread's ap is out-of-pool (gcp no-op) but a task-ring node's
          // word0 is its `next` pointer, the very old->young edge the rem set exists to chase.

// gen_fz_relocate: after a drain's cheney fixpoint, any finalizer NODE that lived in the
// (now-dead) minor is copied to the major (bump -> major_hp, past the scanned frontier,
// like run_finalizers bumps post-fixpoint). Its `p` target was already promoted in the
// root phase. A minor/drain never RUNS a finalizer; that waits for a major's compact.
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

// major_symbols_rebuild / major_run_finalizers: the weak-table sweep and finalizer pass of
// a MAJOR's compact -- exactly symbols_rebuild / run_finalizers, but bumping into the major
// to-space (gc_gen redirects bump -> major_hp) and testing survival against gc_to_{lo,hi}
// (the spare/new half) instead of the main pool. Run after the compact's cheney fixpoint.
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
  word fwd = cell(os[2 * j + 1])->x;            // the atom's first word: its forward, if it survived
  if (!(lamp(fwd) && lo <= ptr(fwd) && ptr(fwd) < hi)) continue;
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
  if (lamp(fwd) && g->gc_to_lo <= ptr(fwd) && ptr(fwd) < g->gc_to_hi) {
   struct ai_fz *nn = bump(g, Width(struct ai_fz));
   nn->p = cell(fwd), nn->fn = fz->fn, nn->next = new_fz, new_fz = nn;
  } else fz->fn(fz->p); }
 g->fz = new_fz; }

// gen_minor: the MINOR. Evacuate the minor [end, hp) into the major ACTIVE half (append at
// major_hp), then reset hp = end. The cheney scan starts at the append point (major_hp), so it walks
// only the freshly-promoted survivors -- never the existing major -- and relies on the rem set
// (+ a strong scan of the weak intern map) for the major->young edges. gc_fwd = the pre-drain
// major_hp, so a forwarded young object (word0 >= gc_fwd) is told from a pointer to a pre-existing
// major object (< gc_fwd, left in place). No linear walk of the major: a minor never reads a dead
// or non-object word.
static void gen_minor(struct ai *g) {
 ai_image_note(0x33);
 word const *p0 = (word const*) g->end, *t0 = g->hp;          // minor from-range
 g->gc_gen = 1, g->gc_f2lo = 0;
 g->gc_to_lo = g->major_base, g->gc_to_hi = g->major_base + g->major_len;
 g->gc_fwd = g->major_hp;
 g->cp = g->major_hp;
 g->ip = cell(gcp(g, word(g->ip), p0, t0));
 g->tasks = cell(gcp(g, word(g->tasks), p0, t0));
 for (word i = 0; i < g->end - &g->v0; i++) (&g->v0)[i] = gcp(g, (&g->v0)[i], p0, t0);   // core vars
 for (word *s = g->sp; s < topof(g); s++) *s = gcp(g, *s, p0, t0);                       // stack
 for (struct ai_r *r = g->root; r; r = r->n) *r->x = gcp(g, *r->x, p0, t0);              // C roots
 // The weak intern map is NOT a normal root (it is its own field), so promote its STRUCTURE by
 // hand or it is lost. Its ENTRIES stay weak: a major rebuilds the table, dropping dead atoms.
 // While the header is itself young (the first collections) gcp it and let the cheney scan chase
 // the backing + atoms; once it is tenured, scan its (possibly young) backing in place.
 if (g->symbols) {
  if (ai_young(g, g->symbols)) g->symbols = gcp(g, g->symbols, p0, t0);
  else gen_scan_inplace(g, g->symbols, p0, t0), gen_scan_inplace(g, map_back(g->symbols), p0, t0);
 }
 for (uintptr_t i = 0; i < g->rem_n; i++) gen_scan_inplace(g, g->rem[i], p0, t0);        // major->young edges
 for (struct ai_fz *fz = g->fz; fz; fz = fz->next) fz->p = cell(gcp(g, word(fz->p), p0, t0));
 while (g->cp < g->major_hp) (datp(g->cp) ? evac_data : evac_thread)(g, p0, t0);
#ifdef AI_GC_CHECK
 // the fixpoint IS a fixpoint (gc.v's drain_second_pass_copies_nothing,
 // instanced on every minor): re-drive the WHOLE scan -- roots, rem set, the
 // promoted window -- and every gcp must be an identity. If major_hp moves,
 // the first pass LOST an object the mutator can still reach (a scan-window
 // or walker bug), and we trap right at the collection that lost it instead
 // of corrupting silently. Debug builds only (make test_gcheck).
 { word *hp1 = g->major_hp;
  g->cp = (word*) g->gc_fwd;
  g->ip = cell(gcp(g, word(g->ip), p0, t0));
  g->tasks = cell(gcp(g, word(g->tasks), p0, t0));
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
 g->gc_gen = 0; }

// gen_major: a full collection, by REACHABILITY (never a linear sweep, so dead objects and their
// stale pointers are simply ignored). One Cheney pass from the real roots over BOTH from-spaces --
// the major ACTIVE half (p0/t0) and the minor (gc_f2{lo,hi}) -- copying every reachable object,
// young or old, into the spare half (or a fresh pair twice the size when the major is over half
// full). This is why a rem-set overflow forces a major: tracing from roots finds every LIVE
// old->young edge with no rem set. Then rebuild the weak intern map + run finalizers into the to-space, flip, and reset the
// (now fully promoted) minor. The core stays put in the main pool, so there is no core-flop: nil is
// ZeroPoint (an out-of-pool const), not the core.
static struct ai *gen_major(struct ai *g) {
 ai_image_note(0x34);
 word const *p0 = g->major_base, *t0 = g->major_hp;              // from-range 1: major active
 // size the to-space for the WORST CASE: all of major-active AND all of the minor survive (the
 // major promotes both -- the minor can dwarf the major half, so size for both, never just double).
 uintptr_t used = (uintptr_t)(g->major_hp - g->major_base), young = (uintptr_t)(g->hp - (word*) g->end);
 uintptr_t need = used + young;
 // grow/shrink by a whole STEP (= ai_major0): snap NEED + 25% headroom up to a step multiple. One step
 // at a time prevents thrash; snapping DOWN when `need` collapses keeps the pool small -- a churn
 // workload that floated dead promotions between majors reclaims them here, rather than doubling forever.
 uintptr_t step = ai_major0, want = need + (need >> 2) + 16;
 uintptr_t to_len = ((want + step - 1) / step) * step;
 if (to_len < step) to_len = step;
 uintptr_t need_step = ((need + step - 1) / step) * step;       // the TIGHT size: smallest step-multiple holding `need`
 if (need_step < step) need_step = step;
 // BUDGET CAP: keep the major pair within its share of the bound so the whole footprint (2*major +
 // 2*minor) stays bounded on a constrained device -- but NEVER below the tight `need_step`, or the
 // to-space can't hold the worst-case promotion. A budget too small for the live set falls through to
 // the OOM path below. (Unbounded host: g->budget == 0, so this is skipped and the sizing is unchanged.)
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
 g->major_hp = to, g->cp = to;
 g->gc_to_lo = to, g->gc_to_hi = to + to_len, g->gc_fwd = to;   // fresh to-space: every copy is a forward
 g->gc_f2lo = (word*) g->end, g->gc_f2hi = g->hp;            // from-range 2: the minor (promote young in the same pass)
 g->ip = cell(gcp(g, word(g->ip), p0, t0));
 g->tasks = cell(gcp(g, word(g->tasks), p0, t0));
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
 return g->gc_gen = 0, g; }

// gen_grow: resize the MINOR pool (the main pool) to len1 -- its own copy/growth, decoupled from the
// major. Called right after a collection, so the minor heap is EMPTY: only the core + stack move,
// to a fresh pool. The major + the weak intern map ride through untouched (they live outside the
// from-space [ptr(g), ptr(g)+len)). Safe because () is ZeroPoint (an immortal const), NOT the core:
// nothing in the major points at the moving core, so no fix-up is needed -- the core-flop here only
// catches a (vestigial) root that is literally (word)g.
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

// gen_please: the generational GC entry. A MINOR (cheap, frequent) unless the rem set
// overflowed or the major lacks headroom for a worst-case drain -- then a MAJOR (drain +
// compact). The minor ends empty; then size it by Appel's rule against the memory budget (below),
// DECOUPLED from the major (which grows in gen_major and far less often).
static struct ai *gen_please(struct ai *g, uintptr_t req0) {
 ai_image_note(0x32);
 uintptr_t seen_young = (uintptr_t)(g->hp - g->end);
 uintptr_t major_free = (uintptr_t)((g->major_base + g->major_len) - g->major_hp);
 g->since_major += seen_young;                                  // young allocated (∝ scanned) since the last major
 // a MAJOR (else a minor): forced by a rem-set overflow (rem_miss -- a dropped entry would make a minor unsound); or the major can't hold a worst-case
 // promotion; or we've allocated the post-major live set + 4 minor-pools' worth since the last major --
 // the amortization rule that periodically traces, so floating DEAD tenured objects (which never grow
 // occupancy -- they die in place, invisible to a minor) get swept and the pool can shrink back. The
 // 4*minor-pool floor keeps majors the minority (>= 4 minors between them) even when little is tenured;
 // the major_live0 term amortizes major cost against allocation as the tenured set grows.
 bool major = g->rem_miss
   || major_free < (uintptr_t) g->len + req0 + 16
   || g->since_major > g->major_live0 + 4 * (uintptr_t) g->len;
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
 // MINOR resize -- DETERMINISTIC, no wall clock. Keep the GC's COPY OVERHEAD (words copied / words
 // allocated) inside a band, the word-for-word analogue of the old time ratio -- so the schedule is
 // reproducible, immune to machine load, and a slow major can't feed back into the size. The ratio is
 // accumulated over a sliding window (reset on a resize), which smooths the per-collection spikes a live
 // multiple would chase. ai_budget (0 = unbounded) caps the whole footprint -- Appel's rule: the nursery
 // gets the free budget after the major pool. One knob bounds a small device.
 enum { ratio = ai_gc_ratio };                  // target band: grow above 1/ratio overhead, shrink below 1/(4*ratio)
 g->win_alloc += seen_young, g->win_copied += copied;
 uintptr_t used = g->len - avail(g), req = req0 + used + (used >> 2), len1 = g->len, arena = len1;
 // resize STICKINESS: one out-of-band window is a hint, not a verdict -- act only when TWO
 // CONSECUTIVE windows lean the same way (lean tracks the streak; an in-band window ends it).
 // a resize is the runtime's costliest single act (a fresh pool, a full core+stack copy, every
 // page refaulted), and the hair-trigger FLAPPED workloads sitting near a band edge between
 // pool sizes (~150 remaps in one mooncc compile; a few-MB live-set nudge swung the pool 2x
 // and ~0.5s of soft faults). still deterministic: no wall clock, just one more window.
 // one out-of-band verdict is a HINT, not a mandate: hold the size, keep the streak (lean),
 // and KEEP ACCUMULATING -- no window reset on a hint. a transient spike (a major collection's
 // copied volume landing in the window is the big one) self-corrects: the ratio dilutes back
 // into band as the window grows and the streak dies. a real ramp stays out of band and
 // confirms at the very next collection, over a LONGER (smoother) window than the fresh spiky
 // one a reset would give it. the hair-trigger acted on the first verdict, so one spike window
 // bought a PERMANENT doubling (in-band at 2x: a one-way ratchet) -- workloads sitting near a
 // band edge swung a full pool doubling (~0.5s of soft faults) on a few-MB live-set nudge.
 // (measured out, do not revisit: acting on the first verdict with a fresh-window reset saves
 // more RSS on ratchet-y workloads but taxes ramp-y ones ~4%; EXCLUDING majors from the window
 // collapses RSS but the nursery then never amortizes them -- majors storm, +70% wall.)
 if (g->win_copied * ratio > g->win_alloc) {                   // overhead > 1/ratio: nursery too small
  if ((g->lean = g->lean > 0 ? g->lean + 1 : 1) >= 2) {        // confirmed: grow
   uintptr_t wa = g->win_alloc | 1;                            // grow until the PROJECTED overhead lands in band (| 1: guarantee progress)
   while (g->win_copied * ratio > wa) arena <<= 1, wa <<= 1;    // (doubling the pool ~doubles alloc-between-GCs)
   g->lean = 0, g->win_alloc = g->win_copied = 0; }
 } else if (g->win_copied * (ratio * 4) < g->win_alloc) {       // overhead < 1/(4*ratio): oversized
  if ((g->lean = g->lean < 0 ? g->lean - 1 : -1) <= -2)
   arena = len1 >> 1, g->lean = 0, g->win_alloc = g->win_copied = 0;   // shrink ONE step (gentle -- multi-step collapses on a lucky GC)
 } else if (g->win_alloc > 8 * len1) g->win_alloc = g->win_copied = 0, g->lean = 0;   // in band: cap the window; the streak dies
 if (g->budget) {                                              // Appel cap, sized so the MAJOR (which must hold the
  // worst-case promotion = the live tenured set PLUS this whole nursery) also fits the budget. With 2*major +
  // 2*nursery <= budget and major ~ live + nursery, the nursery gets ~(budget - 2*live)/4 -- reserve for the
  // major that holds it, or it balloons past what gen_major can allocate (the freestanding OOM that motivated this).
  uintptr_t lv = 2 * g->major_live0, room = g->budget > lv ? (g->budget - lv) / 4 : 0;
  if (arena > room) arena = room; }
 if (arena < (uintptr_t) ai_minor0) arena = ai_minor0;         // floor
 if (arena < req) arena = req;                                 // hard floor: hold the pending allocation
 return arena == len1 ? g : gen_grow(g, arena); }

ai_noinline struct ai *ai_please(struct ai *g, uintptr_t req0) {
 return gen_please(g, req0); }   // generational ONLY: a minor (or major) into the major pool that ai_ini_0 guarantees

static ai_inline word copy_chain(struct ai*g, struct ai_chain *src, word const *const p0, word const *const t0) {
 struct ai_chain *dst = bump(g, Width(struct ai_chain));
 ini_chain(dst, src->a, src->b);
 src->ap = (lvm_t*) dst;
 return word(dst); }

static ai_inline word copy_vec(struct ai*g, struct ai_vec *src, word const *const p0, word const*const t0) {
 uintptr_t bytes = ai_vec_bytes(src);
 struct ai_vec *dst = bump(g, b2w(bytes));
 src->ap = memcpy(dst, src, bytes);
 return word(dst); }

static ai_inline word copy_str(struct ai*g, struct ai_str *src, word const *const p0, word const*const t0) {
 uintptr_t bytes = sizeof(struct ai_str) + src->len;
 struct ai_str *dst = bump(g, b2w(bytes));
 src->ap = memcpy(dst, src, bytes);
 return word(dst); }

// Bignums are flat (raw limbs, no embedded l pointers), so they copy by a
// single memcpy and evac by advancing past their bytes -- exactly like strings.
static ai_inline word copy_big(struct ai*g, struct ai_big *src, word const *const p0, word const*const t0) {
 uintptr_t bytes = ai_big_bytes(src);
 struct ai_big *dst = bump(g, b2w(bytes));
 src->ap = memcpy(dst, src, bytes);
 return word(dst); }

// the lean float box is flat (ap + payload, no embedded l pointers) -- copy by
// one memcpy and evac by advancing past its words, exactly like a bignum.
static ai_inline word copy_flo(struct ai*g, struct ai_flo *src, word const *const p0, word const*const t0) {
 struct ai_flo *dst = bump(g, flo_req);
 src->ap = memcpy(dst, src, sizeof(struct ai_flo));
 return word(dst); }

static ai_inline word copy_wide(struct ai*g, struct ai_wide *src, word const *const p0, word const*const t0) {
 struct ai_wide *dst = bump(g, wide_req);
 src->ap = memcpy(dst, src, sizeof(struct ai_wide));
 return word(dst); }

static ai_inline word copy_cplx(struct ai*g, struct ai_cplx *src, word const *const p0, word const*const t0) {
 struct ai_cplx *dst = bump(g, cplx_req);
 src->ap = memcpy(dst, src, sizeof(struct ai_cplx));
 return word(dst); }

// atoms copy like any object (the nom string forwards normally); interning
// maintenance moved WHOLLY to the post-fixpoint table sweep (symbols_sweep).
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
  default: __builtin_trap();
  case KChain: return copy_chain(g, two(src), p0, t0);
  case KVec: return copy_vec(g, vec(src), p0, t0);
  case KMint: return copy_sym(g, sym(src), p0, t0);
  case KNom: return copy_nom(g, nom(src), p0, t0);
  case KString: return copy_str(g, str(src), p0, t0);
  case KBig: return copy_big(g, (struct ai_big*) src, p0, t0);
  case KFlo: return copy_flo(g, (struct ai_flo*) src, p0, t0);
  case KWide: return copy_wide(g, (struct ai_wide*) src, p0, t0);
  case KCplx: return copy_cplx(g, (struct ai_cplx*) src, p0, t0); } }

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
 // if it's a number it stays; else find which FROM-space range holds it (a major traces two:
 // the major-active half AND the minor -- gc_f2{lo,hi} -- in one reachability pass). lo/hi end up
 // the range that CONTAINS x, so copy_thread's terminator scan uses x's own home.
 if (charmp(x)) return x;
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
static struct ai_mint *sym_probe(struct ai *g, char const *nm, uintptr_t n);

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
  src,  // a lambda's source \-expr, stashed at the thread head for printing (nil = none)
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
  c->stack = c->branches = c->exits = c->lams = c->len = c->sites = c->src = c->fars = nil;
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
 // every in-place store below (env accumulators, cons surgery, thread emission) is
 // precisely barriered -- gen_wb_cell / gen_wb_two -- so a mid-compile collection
 // stays MINOR. (a blanket forced-major here once cost a woken image a full-heap
 // copy on every invocation.)
 // the operator factor pass: c0 delegates the sigil surface -> core source
 // rewrite to the l `opfix` prepass (prel.l) -- evaluated like a macro,
 // once that global exists (i.e. for everything after its own definition
 // partway through the prel) -- so both compilers see factored forms.
 // A chain whose head is already a top (a non-data heap value -- C lamp is
 // just the heap test) is a constructed direct application ((f 'x) calls
 // built by this hook, boxfix's, or ana_2's -- never readable source):
 // skipped, which also terminates the recursion through ai_eval.
 { word x0 = g->sp[0];
   if (chainp(x0) && (!lamp(A(x0)) || datp(A(x0)))) {
    word of = ai_core_of(g)->hot_opfix;          // sealed: a book rebind can't reach this lane
    if (!lamp(of)) {                             // pre-seal (mid-prel bootstrap): probe by nom
     struct ai_mint *os = sym_probe(ai_core_of(g), "opfix", 5);
     of = os ? bookget(ai_core_of(g), 0, word(os)) : 0; }
    if (of && lamp(of)) {
     g = ai_eval(gxr(gxl(gxl(pushq(gxl(ai_push(g, 4, x0, nil, nil, of)))))));
     if (!ai_ok(g)) return g;
     g->sp[1] = g->sp[0], g->sp += 1; } } }
 if (!ai_ok(g = enscope(g, (struct env*) nil, nil, nil))) return g;
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
 uintptr_t extra = nilp((*c)->src) ? 0 : 1;
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
// `quote nil` + stash the operand cell in the site for ana_d to backpatch.
static Cata(c1_recv) {
 word y = pop1(g), site = pop1(g);
 Kp -= 2;
 Kp[0].ap = lvm_quote;
 if (nilp(site)) Kp[1].x = AB(y), gen_wb_cell(g, Kp + 1, Kp[1].x);
 else Kp[1].x = nil, B(site) = (word) &Kp[1], gen_wb_two(g, site, B(site));
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

cata1(c1_cond_push_branch, g = gxl(ai_push(g, 2, Kp, (*c)->branches)), (*c)->branches = ai_ok(g) ? pop1(g) : nil, gen_wb_cell(g, &(*c)->branches, (*c)->branches))
cata1(c1_cond_push_exit, g = gxl(ai_push(g, 2, Kp, (*c)->exits)), (*c)->exits = ai_ok(g) ? pop1(g) : nil, gen_wb_cell(g, &(*c)->exits, (*c)->exits))
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


// A hardware fault (SIGSEGV/SIGILL/SIGBUS/SIGFPE) is a CRASH on every target: no signal
// handler, no recovery. A fault means an invariant is already broken -- a dangling native,
// a torn heap, a bad emit -- and the loud, immediate core dump names the site while the
// evidence is still on the stack. (A barrier here once turned exactly that class of bug
// into a silent per-call siglongjmp storm; the fix belongs at the source, e.g. the bake's
// dead-native revert in img_nif_interp.)
static struct ai *ai_eval(struct ai *g) {
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
  if (nilp(d)) {
   if ((y = bookget(g, 0, x))) return ana_q(g, c, y);
   // undefined global: resolved by lvm_index via the book at run time.
   // Only record it as a captured free variable when this scope is nested
   // (cf. ev.l avb: `(? (get 0 'par c) (push 'imp x))`). At top level there
   // is no enclosing frame to capture from, so adding x to imps would make a
   // second reference resolve via memq(imps) to an uninitialized arg slot.
   // re-read x from the imps hook: the gxl/ai_push above can GC and relocate
   // the symbol, leaving the local x dangling (cf. the same A((*c)->imps)
   // pattern in the capture path below). c0_ix then emits the live pointer.
   if (!nilp((*c)->par))
    g = gxl(ai_push(g, 2, x, (*c)->imps)),
    x = ai_ok(g) ? A((*c)->imps = pop1(g)) : nil,
    gen_wb_cell(g, &(*c)->imps, (*c)->imps);
   return c0_ix(g, c, lvm_index, x); }
  // lambda definition of local let form?
  if ((y = assq(g, d->lams, x))) {
   // recursive-fn ref: record a backpatch site on d (the lams-owning scope) when
   // the closure isn't built yet, then apply the captured imports.
   word site = nil;
   if (nilp(AB(y))) {
    mm(g, &d), mm(g, &y);
    g = gxl(ai_push(g, 2, y, nil)); // site = (y . nil)
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
  // the shadow guard: d's let BINDS x (fars) but x is neither a lams entry yet
  // (a later/self lambda sibling, pre-rebuild) nor one of the let's slots
  // (those sit on the PARENT's stack) -- the nom is this let's, so the walk
  // must not escape to an enclosing binding of the same spelling. import it,
  // as the book-miss below would; the rebuild resolves it through lams.
  if (!nilp(d->fars) && memq(g, d->fars, x) &&
      !(!nilp(d->par) && memq(g, d->par->stack, x))) {
   if (!nilp((*c)->par))
    g = gxl(ai_push(g, 2, x, (*c)->imps)),
    x = ai_ok(g) ? A((*c)->imps = pop1(g)) : nil,
    gen_wb_cell(g, &(*c)->imps, (*c)->imps);
   return c0_ix(g, c, lvm_index, x); }
  // a let binding, closure var, or lambda arg -- possibly from an enclosing
  // scope. If enclosing, import it into this scope's free-variable (imps) list
  // so the offset c1_var emits is valid in *this* frame, not the defining one;
  // otherwise a captured let binding aliases whatever sits at the same offset
  // in the closure's frame (see the boot.l compiler's ava fix, commit 8e3acf0).
  if (memq(g, d->stack, x) || memq(g, d->imps, x) || memq(g, d->args, x)) {
   incl(*c, 2);
   if (d != *c) // found in an enclosing scope -> import (capture) it
    g = gxl(ai_push(g, 2, x, (*c)->imps)),
    x = ai_ok(g) ? A((*c)->imps = pop1(g)) : nil,
    gen_wb_cell(g, &(*c)->imps, (*c)->imps);
   return ai_push(g, 3, c1_var, x, (*c)->stack); } } }


static Cata(c1_var) {
 word v = pop1(g), i = llen(pop1(g)); // stack inset
 for (word l = (*c)->imps; !nilp(l); l = B(l), i++)
  if (eql(g, v, A(l))) goto out;
 for (word l = (*c)->args; !nilp(l); l = B(l), i++)
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
 if (!chainp(b)) return analyze(g, c, a); // singleton list has value of element
 // if it is a special form then do that
 struct ai_str *nm;                             // a special form is headed by a 1-char NAMED symbol (\ : ?)
 if ((nm = add_name(g, a)) && len(nm) == 1)     // add_name is 0 for a bare mint / the core / a non-sym
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
  // stash the source \-expr for the printer (ioput_fn), built AFTER analyze so the
  // captured imports (d->imps) are known. ops is (params… body); prepend the
  // imports as leading params (the frame layout is [imps, args]) so a closure
  // prints as `(\ imps… params… body)` applied to its captures and round-trips.
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
 !chainp(x) ? c0_cond_exit(g, c, ZeroPoint) :   // clauses ran out: implicit else -> () (nil-ontology: the same () the reader terminates lists with)
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

 if (ai_ok(g = gxl(ai_push(g, 3, nil, (*c)->stack, x)))) {
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
  g = gxl(ai_push(g, 2, nil, (*c)->stack));
  if (ai_ok(g)) (*c)->stack = pop1(g), gen_wb_cell(g, &(*c)->stack, (*c)->stack); }
 return g; }

static ai_inline bool lambp(struct ai *g, word x) {
 struct ai_str *n;                                      // headed by the named symbol \ (add_name 0 for a bare mint / non-sym)
 return chainp(x) && chainp(B(x)) && chainp(B(B(x))) &&
  (n = add_name(g, A(x))) && len(n) == 1 && txt(n)[0] == '\\'; }

static ai_inline word rev(struct ai *g, word l) {
 word m, n = nil;   // reversal points each cons at its (younger) predecessor: barrier it
 while (chainp(l)) m = l, l = B(l), B(m) = n, gen_wb_two(g, m, n), n = m;
 return n; }

static word ldels(struct ai *g, word lam, word l);

// a nom lexically bound anywhere up the scope chain SHADOWS a macro of the same
// spelling: expansion must not fire (ev.l's wx/cprop carry the twin guard, so both
// compilers agree). binder rosters only -- imps are derived captures and may record
// undefined globals, so they don't count as binders.
static bool lexbound(struct ai *g, struct env *d, word x) {
 for (; !nilp(d); d = d->par)
  if (memq(g, d->args, x) || memq(g, d->stack, x) ||
      memq(g, d->fars, x) || assq(g, d->lams, x)) return true;
 return false; }

static ai_inline Ana(ana_2, word a, word b) {
 if ((x = macroget(ai_core_of(g), a)) && !lexbound(g, *c, a))   // macro table = each layer's [nil] slot, walked; the scope walk only on a macro HIT
  return g = ai_eval(gxr(gxl(gxl(pushq(gxl(ai_push(g, 4, b, nil, nil, x))))))),
         analyze(g, c, ai_ok(g) ? pop1(g) : 0);
 return avec(g, b, g = analyze(g, c, a)),
        ana_ap(g, c, b); }

static ai_inline Ana(ana_q) { return c0_ix(g, c, lvm_quote, x); }
static ai_inline Ana(ana_l) {
  if (!chainp(B(x))) return ana_q(g, c, A(x)); // one operand, no params: quote
  return g = c0_lambda(g, c, nil, x),
         analyze(g, c, ai_ok(g) ? pop1(g) : 0); }
static Ana(c0_cond_r);
static ai_inline Ana(ana_c) {
 return !chainp(B(x)) ? analyze(g, c, A(x)) :
    (g = ai_push(g, 2, x, c1_cond_pop_exit),
     g = c0_cond_r(g, c, ai_ok(g) ? pop1(g) : nil),
     ai_push(g, 1, c1_cond_push_exit)); }
// this is the longest C function :(
// it handles the let special form in a way to support sequential and recursive binding.
static ai_inline struct ai *ana_d(struct ai *g, struct env **b, word exp) {
 if (!chainp(B(exp))) return analyze(g, b, A(exp));
 struct ai_r *mm0 = ai_core_of(g)->root;
 mm(g, &exp);
 // recursive-value boxing: c0 is the bootstrap compiler, so it delegates the
 // letrec*-value rewrite to the l `boxfix` prepass (prel.l) -- evaluated
 // like a macro -- once that global exists (i.e. for everything after its own
 // definition partway through the prel). It indirects forward-referenced
 // bindings through nom-keyed cells -- one scope; see prel.l. The runtime compiler
 // (ev.l) runs the same pass in feel, so both lanes share one boxfix. exp is
 // rooted across the alloc.
 if (ai_ok(g = intern(ai_strof(g, "boxfix")))) {
  word bf = bookget(g, 0, pop1(g));
  if (bf && lamp(bf)) {
   g = ai_eval(gxr(gxl(gxl(pushq(gxl(ai_push(g, 4, exp, nil, nil, bf)))))));
   if (ai_ok(g)) exp = pop1(g); } }
 g = enscope(g, *b, (*b)->args, (*b)->imps);
 if (!ai_ok(g)) return forget();
 struct env *q = (struct env*) pop1(g), **c = &q;
 // lots of variables :(
 word nom = nil, def = nil, lam = nil,
      v = nil, d = nil, e = nil, os = nil;
 mm(g, &nom), mm(g, &def), mm(g, &lam);
 mm(g, &d); mm(g, &e); mm(g, &v); mm(g, &q); mm(g, &os);

 // pin the let's binding names on q BEFORE any lambda compiles (the shadow
 // set): the variable walk must see an inner-bound nom as bound HERE even
 // while lams is still nil, or a nested loop named like a later sibling of
 // an enclosing let resolves to that sibling and applies its import row
 // mid-rebuild -- past the closure fixpoint, so the recursion's self-sites
 // under-apply (a truthy partial, the silent no-op). cf. ev.l avb's 'far guard.
 for (d = exp; chainp(d) && chainp(B(d)); d = BB(d)) {
  for (e = A(d); chainp(e) && !nomp(e); e = A(e)); // unroll (f x..) define-sugar to the name
  g = gxl(ai_push(g, 2, e, q->fars));
  if (!ai_ok(g)) return forget();
  q->fars = pop1(g), gen_wb_cell(g, &q->fars, q->fars); }

 // collect vars and defs into two lists.
 // While finding each bound lambda's closure (the c0_lambda below) we expose
 // the preceding bindings on the enclosing scope's stack, so a let-bound
 // lambda that refers to a sibling binding captures it as a free variable
 // instead of falling through to a same-named global (cf. ev.l l2/jj's
 // `_ (push 'stk (car n))`). The original stack is restored after the loop,
 // before any code is emitted, so the run-time frame layout is unchanged.
 os = (*b)->stack;
 while (chainp(exp) && chainp(B(exp))) {
  for (d = A(exp), e = AB(exp); chainp(d) && !nomp(d); e = pop1(g), d = A(d)) {  // a NAMED sym is a chain now: stop the (f x) define-sugar unroll at the name
   g = gxl(ai_push(g, 2, e, nil));
   g = append(gxl(pushl(ai_push(g, 1, B(d)))));
   if (!ai_ok(g)) return forget(); }
  g = gxl(ai_push(g, 2, d, nom));
  g = gxl(ai_push(g, 2, e, def));
  if (!ai_ok(g)) return forget();
  def = pop1(g), nom = pop1(g);
  // if it's a lambda compile it and record in lam list
  if (lambp(g, e)) {
   g = ai_push(g, 2, d, lam);
   g = gxl(gxr(c0_lambda(g, c, nil, B(e))));
   if (!ai_ok(g)) return forget();
   lam = pop1(g); }
  g = gxl(ai_push(g, 2, d, (*b)->stack)); // expose this binding to later siblings
  (*b)->stack = ai_ok(g) ? pop1(g) : nil;
  gen_wb_cell(g, &(*b)->stack, (*b)->stack);
  exp = BB(exp); }
 (*b)->stack = os, gen_wb_cell(g, &(*b)->stack, os); // restore: emission below rebuilds the real frame

 intptr_t l = llen(nom);
 bool oddp = chainp(exp),
      globp = !oddp && nilp((*b)->args); // we check this again later to make global bindings at top level
 if (!oddp) { // if there's no body then evaluate the name of the last definition
  g = gxl(ai_push(g, 2, A(nom), nil));
  if (!ai_ok(g)) return forget();
  exp = pop1(g); }

 // find closures
 // for each function g with closure C(g)
 // for each function g with closure C(g)
 // if g in C(g) then C(g) include C(g)
 word j, vars, var;
 do for (j = 0, d = lam; chainp(d); d = B(d)) // for each bound function variable
  for (e = lam; chainp(e); e = B(e)) if (d != e) // for each other bound function variable
   if (memq(g, BB(A(e)), AA(d))) // if you need this function
    for (v = BB(A(d)); chainp(v); v = B(v)) // then you need its variables
     if (!memq(g, vars = BB(A(e)), var = A(v))) // only add if it's not already there
      j++,
      g = gxl(ai_push(g, 2, var, vars)),
      BB(A(e)) = ai_ok(g) ? pop1(g) : nil,
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
 for (d = lam; chainp(d); d = B(d)) AB(A(d)) = nil;

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
 (*c)->sites = nil;

 nom = rev(g, nom); // put in literal order
 g = analyze(g, b, exp);
 g = gxl(ai_push(g, 2, nil, e = (*b)->stack)); // push function stack rep
 (*b)->stack = ai_ok(g) ? pop1(g) : nil;
 gen_wb_cell(g, &(*b)->stack, (*b)->stack);
 for (def = rev(g, def); chainp(nom); nom = B(nom), def = B(def))
  g = analyze(g, b, A(def)),
  g = globp ? c0_ix(g, b, lvm_defglob, A(nom)) : g,
  g = gxl(ai_push(g, 2, A(nom), (*b)->stack)),
  (*b)->stack = ai_ok(g) ? pop1(g) : nil,
  gen_wb_cell(g, &(*b)->stack, (*b)->stack);
 return
  (*b)->stack = e, gen_wb_cell(g, &(*b)->stack, e),
  incl(*b, 2),
  g = ai_push(g, 2, c1_apn, putcharm(l)),
  forget(); }

static word ldels(struct ai *g, word lam, word l) {
 if (!chainp(l)) return nil;
 word m = ldels(g, lam, B(l));
 if (!assq(g, lam, A(l))) B(l) = m, m = l;
 return m; }

lvm(lvm_defglob) {
 Have(3);
 Sp -= 3;
 word k = Ip[1].x, v = Sp[3];
 return Sp[0] = k, Sp[1] = v, Sp[2] = A(g->book), Pack(g),   // a pin lands in the HEAD layer
  !ai_ok(g = ai_mapput(g)) ? ghelp(g) : (Unpack(g), Sp += 1, Ip += 2, Continue()); }

// lvm_index (the late-bound global read) is defined below lvm_scare: its
// miss path is the missing condition and borrows the whole help apparatus.

lvm(lvm_eval) { return Ip++, Pack(g),
 !ai_ok(g = c0(g, lvm_jump)) ? ghelp(g) : (Unpack(g), Continue()); }

// ai_evals_ lives with the boot stitch it shares its machinery with, at the
// foot of the reader section.

// ============================================================================
// vm
// ============================================================================
// Probe the symbol map for a C-string name, materializing the lookup with a
// phantom ai_str on the C stack -- same content hash, same walk as
// intern_checked, but never interning or allocating. A miss means the name was
// never read, so no global by that name was ever defined.
uintptr_t hash(struct ai*, intptr_t);
static struct ai_mint *sym_probe(struct ai *g, char const *nm, uintptr_t n) {
 word m = g->symbols;
 if (!m) return 0;
 uintptr_t h = mix;                            // the KString content hash, over the C bytes
 for (uintptr_t j = 0; j < n; j++) h ^= (uint8_t) nm[j], h *= mix;
 uintptr_t mask = map_cap(m) - 1, i = h & mask;
 word *s = map_slots(m);
 for (;; i = (i + 1) & mask) {
  word k = s[2 * i];
  if (k == map_gap) return 0;
  if (len(k) == n && !memcmp(txt(k), nm, n)) return sym(s[2 * i + 1]); } }

// The five hooks (love.h) are lisp the C apply/compile lanes must reach. (seal-hook n f)
// hands each one over as it comes into being; the lanes then read the slot directly -- no
// per-call sym_probe + book lookup, and no later rebind of the nom can reach them.
// hot_hook traps if a slot is unsealed (nil) or somehow not a lambda: a clean failure, never
// a wild read. g->hot_* is GC-traced (v0..end) and rides the egg image, so a loaded image
// is sealed.
static ai_inline ai_word hot_hook(ai_word h) { if (!lamp(h)) __builtin_trap(); return h; }

// The function combinators for `+` and `*` (hooks 2 and 3, the prel's `stack`/`compose`).
// A thread operand takes precedence over every other type, so `+`/`*` of a function build a new
// function -- the README's Church arithmetic, agreeing with numerals: `+` is Church add
// ((+ g g) a x = g a (g a x)), `*` is composition. stack is the 4-arg add combinator, compose the
// 3-arg; the C aps reuse numap_drive to compute the partial (stack g g) / (compose g g) -- itself
// the new function -- and leave it as the result, resuming at Ip+1.

// Fixnum-as-function application. A fixnum operator n applied to x is dispatched
// to the l ap at book['num-ap] as (num-ap n x): numeric x -> x**n, a
// function x -> x iterated n times (Church numerals).
//
// The driver mirrors the chain driver: with the stack laid out [n, num-ap, x, ret]
// it applies num-ap to n (a partial), swaps so that partial becomes the operator,
// applies it to x, and ret0s the result to ret. The five apply sites divert here as
// a tail jump (no extra args -> stays a sibcall, cf. vmret): lvm_numap is the
// non-tail form (frame below Sp, resume at Ip+1), lvm_numtap the tail form (frame
// in the popped region, deliver to the caller's ret at Sp[fs+2]). The fused arg/quote
// variants first push their argument under the operator and bump Ip by one word so
// the canonical [.. x n] layout and resume/frame-size operand line up, then divert.
static lvm(numap_swap) {
 word t = Sp[0]; Sp[0] = Sp[1], Sp[1] = t;
 return Ap(lvm_ap, g); }
union u const numap_drive[] = { {lvm_ap}, {.ap = numap_swap}, {.ap = lvm_ret0} };

// --- callout_drive: the STACKLESS call-out bridge (the glaze call-out arc) -------------------------
// A native glaze blob calls a closure the way the VM itself does -- NOT by re-entering C, but by
// threading through the VM stack. To apply `clos` to `arg` and resume in native code, the blob builds
// a [arg, clos, RET] frame on Sp, points Ip at callout_drive, and TAIL-jumps (Continue): lvm_ap
// applies clos to arg, lvm_ret0 delivers the result to RET. RET is the blob's own native resume
// point (its address materialized position-independently via the `la` IR op, stored in a Sp slot the
// callee cannot reach), so control comes back to native code -- with the result in Sp[0] -- entirely
// through Continue() tail-jumps. This is the SAME shape as numap_drive (church application) and
// help_drive (the condition system): NO C stack frame is pinned across the sub-run, so a deep or
// re-entrant callee grows the heap VM stack (Sp) -- growable, degrading to a scare -- exactly like the
// interpreter, and NEVER the C stack. (The retired ai_call1 was a re-entrant C driver used only to
// measure the arithmetic tax; it pinned one C frame per call-out nesting level -- the one construct
// that could overflow the C stack -- so it is gone.) The loop's live state is spilled to Sp (a GC
// root) around each call, so a collection inside the callee relocates it correctly for free.
static union u const callout_drive[] = { {lvm_ap}, {.ap = lvm_ret0} };
// (calloutdrive x) -> the address of callout_drive as a fixnum (x ignored) -- the glaze emitter reads
// it once and bakes it as the `li Ip` immediate. A data-segment const, so the address is stable
// across an image reload (unlike a W^X JIT pointer).
lvm(lvm_calloutdrive) { return Sp[0] = putcharm((intptr_t) callout_drive), Ip++, Continue(); }
// --- callout_resume: the WALKABLE resume (v2 of the bridge) -----------------------------------------
// callout_drive's RET was the ADDRESS OF A STACK SLOT holding the blob's resume point -- a stack-
// INTERIOR in-pool pointer, which violates the stack-walk invariant (slots hold values or out-of-pool
// code addresses only): a collection with a call-out pending (gen_grow under deep recursion) fed it
// to gcp, which trapped. Here the frame is [arg, clos, tag(bb - entry), entry]:
// the resume rides as an ODD CHARM offset (the walk skips it) plus the blob's raw W^X base (out of
// every pool bound -- gcp returns it unharmed), and lvm_resume jumps base+offset after delivering the
// result. NO stack-interior pointers remain, and a stack RELOCATION is equally safe: the offset is a
// value, the base is immortal. Same stackless shape as callout_drive otherwise.
static union u const callout_resume[] = { {lvm_ap}, {.ap = lvm_resume} };
// (calloutresume x) -> the address of callout_resume as a fixnum (x ignored).
lvm(lvm_calloutresume) { return Sp[0] = putcharm((intptr_t) callout_resume), Ip++, Continue(); }

// ============================================================================
// the lisp help calling convention
// ============================================================================
// With a global `help` function installed, a raise becomes the call
// (help s a b): s = the status word (prel's `scare?` reads it -- one bit now,
// the reader having stopped raising the other), a/b = the condition data, nil
// nil for a scare (oom is bare; future scares define their shapes). The
// frame runs through help_drive (numap_drive's 3-arg twin) into a per-class
// epilogue: help_ret_more delivers the ap's result to the raise site's resume
// text -- ⚠ despite the name that is the DELIBERATE-scare lane, what makes
// (scare a b) and `missing` resumable; a bare scare is observed, then takes the
// default escape to C.
static lvm(help_ret_more) {   // [result resume port sentinel ..] -> resume sees result
 Ip = cell(Sp[1]);
 Sp[3] = Sp[0];
 Sp += 3;
 return Continue(); }
static lvm(help_ret_scare) {  // result ignored: scares are not (yet) resumable
 return Pack(g), encode(g, ai_status_scare); }
static union u const help_more_k[] = { {help_ret_more} };
static union u const help_scare_k[] = { {help_ret_scare} };
static union u const help_drive[] =
 { {lvm_ap}, {.ap = numap_swap}, {.ap = numap_swap}, {.ap = lvm_ret0} };

// Raise status s with condition data a/b to the help continuation. With a
// global `help` function -- present the way everything is present: by its
// net -- and 5 words of stack headroom (the raise path never
// allocates), build the (help s a b) frame and run it; else the C default
// raise_c, which resumes the eof protocol raw. Pre-book raises (ai_ini_0)
// always take the default.
static struct ai *ai_raise(struct ai *c, enum ai_status s, word a, word b,
                         union u const *K) {
 if (s == ai_status_scare) c->scare_a = a, c->scare_b = b; // for the exit face
 if (c->book) {
  struct ai_mint *ts = sym_probe(c, "help", 4);
  word h = ts ? bookget(c, nil, word(ts)) : nil;
  if (!ai_nilp(c, h) && avail(c) >= 5) {
   word *sp = c->sp -= 5;          // [s h a b K | raise site data ..]
   sp[0] = putcharm(s), sp[1] = h;
   sp[2] = a, sp[3] = b;
   sp[4] = word(K);
   c->ip = (union u*) help_drive;
#if ai_tco
   return c->ip->ap(c, c->ip, c->hp, c->sp);
#else
   return c;                       // ok-g: the trampoline dispatches help_drive
#endif
  } }
 union u *t = (union u*) raise_c;
 c->ip = t;
#if ai_tco
 return t->ap(encode(c, s), t, c->hp, c->sp);
#else
 return t->ap(encode(c, s));
#endif
}
struct ai *ghelp2(struct ai *g, enum ai_status s) {
 // nothing raises the MORE bit through help any more -- the reader answers its
 // own nothings as values (doc/io.md rung 6c) -- so this is the scare lane
 // alone. help_more_k lives on regardless: it is what makes a DELIBERATE scare
 // resumable, and `scare`/`missing` reach for it directly below.
 return ai_raise(ai_core_of(g), s, nil, nil, help_scare_k); }
// Raise on an already-tagged g: re-raise its own status.
struct ai *ghelp(struct ai *g) { return ghelp2(ai_core_of(g), ai_code_of(g)); }
// (scare a b): the deliberate raise -- the user scares, the scare bit is set
// unconditionally and the global help hears (help 1 a b). Unlike a C scare
// (oom), the raise point here is a clean boundary, so the help's result is
// delivered back as (scare a b)'s value via the more continuation -- the
// resume is this nif's own ret. With no help installed the C default
// applies and the scare is terminal.
lvm(lvm_scare) {
 Have(6);                          // [resume] + ai_raise's 5 words (a, b are the
                                   // two args, already on the stack). Under-
                                   // provisioning here lets ai_raise's avail>=5
                                   // guard fail and SILENTLY drop the help call,
                                   // so a deliberate scare can miss its handler
                                   // on a tight heap (cf. lvm_index/lvm_missing,
                                   // which Have(8) for the same reason).
 word a = Sp[0], b = Sp[1];
 *--Sp = word(Ip + 1);             // [resume a b ..]: help_more_k's layout
 return Pack(g), ai_raise(g, ai_status_scare, a, b, help_more_k); }
// the missing miss sentinel: a private static address no book value can equal,
// so a name bound to nil stays distinct from no entry at all.
static union u const no_entry[1];
// the GC-free C-data emitters (defined below), forward-declared for lvm_index's helpless-miss face.
static struct ai *ioputs(struct ai*, char const*);
static struct ai *ioputc(struct ai*, int);
static ai_inline struct ai *zflush(struct ai*);
// THE NOTHING IS THE CORE: () = ZeroPoint, what a helpless missing read
// answers and what () reads as. The core's head is a nameless serial-0 mint (the one
// serial never drawn), nameless, $0, falsy, applying const-1 like every unit. absence
// is a POINT, not a quantity: a number would exponentiate under a numeral ((i love) =
// 0**i is honest nan), a unit absorbs -- which is what keeps (i love you) = 1. It prints
// () -- the face of absence.
// No named constant; the nothing is ZeroPoint (ai_mint_zero). DISTINCT from 0 (fixnum) and "" (string).
// A read of the LIVE book (the outermost cell) by name -- the missing-name law
// at the global scope, the twin of boxfix's local (missing cell 'nom). A hit
// pushes the current value; a miss is a MISSING name -- a nom not in the book,
// a call for help: with a global help installed the read raises (help 1 'missing
// nom) and the help's result is the value here; helpless it reads the zero point.
// The site NEVER self-patches (it stays a live read, so a define that lands later
// is seen, a rebind is honoured) and the name is NOT a frame import.
lvm(lvm_index) {
 Have1();                          // room for the push first (may GC; no live local held yet)
 word v = bookget(g, word(no_entry), Ip[1].x);
 if (v != word(no_entry)) return
  *--Sp = v,                       // present: push the live value, no quote patch
  Ip += 2,
  Continue();
 Have(8);                          // [resume a b] + ai_raise's 5 words
 struct ai_mint *ts = sym_probe(g, "help", 4);
 word h = ts ? bookget(g, nil, word(ts)) : nil;
 if (ai_nilp(g, h)) {
#if __STDC_HOSTED__
  // helpless (file mode): the zero point is otherwise SILENT, so an unbound name reads
  // ()=ZeroPoint and (() x)=const-1=1 -- a silent miscompile that fakes a result. So ALWAYS
  // SURFACE it on err: ";; missing <nom>", then still answer the zero point. NON-terminal and
  // MISSING-SPECIFIC -- a deliberate (scare ..) keeps the terminal helpless law, so an assert
  // failure still stops the run. add_name + ioput* hold no heap operand -> no GC, so Sp/Ip
  // survive; best-effort, a print error just stops.
  struct ai_str *nm = add_name(g, Ip[1].x);
  if (nm) { struct ai_io *sv = g->io; g->io = &ai_stderr;
            struct ai *w = ioputs(g, ";; missing ");
            for (uintptr_t i = 0; ai_ok(w) && i < nm->len; i++) w = ioputc(w, nm->bytes[i]);
            if (ai_ok(w)) w = ioputc(w, '\n');
            if (ai_ok(w)) zflush(w);
            g->io = sv; }
#endif
  return *--Sp = ZeroPoint, Ip += 2, Continue(); }
 word a = word(sym_probe(g, "missing", 7)), b = Ip[1].x;   // 'missing: the nif's name, rooted by the book
 Sp -= 3;
 Sp[0] = word(Ip + 2), Sp[1] = a, Sp[2] = b;   // help_more_k's layout
 return Pack(g), ai_raise(g, ai_status_scare, a, b, help_more_k); }
// (missing t k): the book read as a value -- lvm_index's law with the book as an
// argument (a tablet is a little book; the book is just the outermost one).
// k present in map t answers the value; a miss is the MISSING CONDITION: with a
// global help installed the read raises (help 1 'missing k) and the help's result
// is the value, helpless it reads the zero point. boxfix's letrec cells read this way --
// pre-fill is a miss, the binding-site nom the payload. distinct from peep,
// whose caller names what absence means.
lvm(lvm_missing) {
 word v = tabp(Sp[0]) ? ai_mapget(g, word(no_entry), Sp[1], Sp[0]) : word(no_entry);
 if (v != word(no_entry)) return
  Sp[1] = v,
  Sp++,
  Ip++,
  Continue();
 Have(8);                          // [resume a b] + ai_raise's 5 words
 struct ai_mint *ts = sym_probe(g, "help", 4);
 word h = ts ? bookget(g, nil, word(ts)) : nil;
 if (ai_nilp(g, h)) return
  Sp[1] = ZeroPoint,
  Sp++,
  Ip++,
  Continue();
 word a = word(sym_probe(g, "missing", 7));   // 'missing: the nif's name, rooted by the book
 Sp -= 1;                          // [resume a b]: b = the key, already in place at Sp[2]
 Sp[0] = word(Ip + 1), Sp[1] = a;
 return Pack(g), ai_raise(g, ai_status_scare, a, Sp[2], help_more_k); }
// numap/numtap are tail-called (Ap) from the fused arg/quote aps, which bump
// Ip by one word so its `ret = Ip+1` math lines up -- leaving Ip pointing at an
// operand, NOT a re-runnable instruction. So a plain Have() here is unsafe: lvm_gc
// re-dispatches via Continue() -> cell(Ip)->ap, which would jump into that operand.
// Instead gc by hand and re-Ap ourselves (we read but don't mutate before this, so
// re-entry is idempotent); the dispatch never has to trust Ip.
#define NumapHave(self) if (Sp < Hp + 2) { \
 Pack(g); g = ai_please(g, 2); if (!ai_ok(g)) return ghelp(g); \
 return Unpack(g), Ap(self, g); }
static lvm(lvm_numap) {
 NumapHave(lvm_numap);
 word h = hot_hook(g->hot_numap);
 word n = Sp[1], x = Sp[0], *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = n, dst[1] = h, dst[2] = x, dst[3] = ret;
 return Sp = dst, Ip = (union u*) numap_drive, Continue(); }
static lvm(lvm_numtap) {
 NumapHave(lvm_numtap);
 word h = hot_hook(g->hot_numap);
 word fs = getcharm(Ip[1].x), n = Sp[1], x = Sp[0], *dst = &Sp[fs + 2] - 3, ret = Sp[fs + 2];
 dst[0] = n, dst[1] = h, dst[2] = x, dst[3] = ret;
 return Sp = dst, Ip = (union u*) numap_drive, Continue(); }

// (seal-hook n f): install f as core hook n -- 0 read, 1 num-ap, 2 stack, 3 compose,
// 4 opfix -- numbered in the order the boot seals them (p1, then the prel twice over).
// The CALLER hands the function over rather than the nif reading it back off the book,
// which is what lets p1 seal the reader with no name anywhere and makes every ordering
// contract LEXICAL: a seal written above its subject reads a missing name, where the old
// probe trapped from inside C with nothing to point at. A non-lambda or an unknown slot
// traps, so hot_hook never meets a wild read. The prel runs in every warm and g->hot_*
// rides the egg image, so every runtime ends up sealed.
// A switch, not a table: an lvm_ body must stay frame-free (the sibcall law; see the lvm_
// scratch rule), and a slot[] would be an address-taken local.
lvm(lvm_seal) {
 if (!lamp(Sp[1])) __builtin_trap();
 switch (getcharm(Sp[0])) {
  case 0: g->hot_read = Sp[1]; break;
  case 1: g->hot_numap = Sp[1]; break;
  case 2: g->hot_stack = Sp[1]; break;
  case 3: g->hot_compose = Sp[1]; break;
  case 4: g->hot_opfix = Sp[1]; break;
  default: __builtin_trap(); }
 Sp += 1, Sp[0] = nil, Ip += 1;
 return Continue(); }

// `+`/`*` over a lambda operand: build the combinator partial (stack/compose g g)
// and leave it as the result. Mirrors lvm_numap's frame -- [g, comb, g, ret=Ip+1]
// run through numap_drive -- but the combinator (4-arg stack / 3-arg compose) applied
// to 2 args yields a closure (the new function) instead of a value. Ip is at the +/*
// opcode (a re-runnable instruction), so a plain Have is safe; operands re-read after.
// The combinators are the prel's `stack`/`compose`, read from their slots AFTER the Have
// so a collection there cannot strand the pointer (v0..end is what the GC updates).
static lvm(lvm_addh) {
 if (coinp(Sp[0]) || coinp(Sp[1])) return Ap(lvm_add_coin, g);
 Have(2);
 word h = hot_hook(g->hot_stack);
 word fa = Sp[0], ga = Sp[1], *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = fa, dst[1] = h, dst[2] = ga, dst[3] = ret;
 return Sp = dst, Ip = (union u*) numap_drive, Continue(); }
static lvm(lvm_mulh) {
 if (coinp(Sp[0]) || coinp(Sp[1])) return Ap(lvm_mul_coin, g);
 Have(2);
 word h = hot_hook(g->hot_compose);
 word fa = Sp[0], ga = Sp[1], *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = fa, dst[1] = h, dst[2] = ga, dst[3] = ret;
 return Sp = dst, Ip = (union u*) numap_drive, Continue(); }

// --- coin +/*/-// : run the die's ADD/MUL/SUB/DIV closure over the two operands ----
// Reached from the KHot lane -- and from lvm_sub's/lvm_quot's own coin checks, `-`
// and `/` having no kind matrix -- when either operand is a coin (so coin + number, coin +
// chain, coin + coin all land here). One operand is a coin: ITS die's method runs,
// `(\ a b ...)`, computing the result via numap_drive -- the same frame lvm_addh uses
// for church-add. The method gets the RAW operands, so a coin + a non-coin is the
// method's call (the default `(load b)` of a non-coin is b itself). But two DISTINCT
// newtypes have no canonical combination, so coin + coin of different dies is nil
// (like string*string) -- the method never sees a foreign payload. A missing method
// (e.g. a monoid has no `*`) is nil too, like the undefined matrix cells. The
// ()-identity never reaches here -- lvm_add/lvm_mul hoist the mint case above the matrix.
// Ip is still the +/* opcode here (lvm_add/lvm_addh/this are all reached via Ap,
// which preserves Ip), so word(Ip + 1) is the true return -- the same continuation
// the church-add path in lvm_addh uses.
static lvm(lvm_coin_op, intptr_t slot) {
 word a = Sp[0], b = Sp[1];
 if (coinp(a) && coinp(b) && coin_die(a) != coin_die(b))
  return *++Sp = ZeroPoint, Ip++, Continue();             // two distinct newtypes: no canonical +/*
 word f = die_get(g, coinp(a) ? coin_die(a) : coin_die(b), slot);
 if (ai_nilp(g, f)) return *++Sp = ZeroPoint, Ip++, Continue();   // no method -> nil
 Have(2);
 a = Sp[0], b = Sp[1];                              // re-read post-GC
 f = die_get(g, coinp(a) ? coin_die(a) : coin_die(b), slot);
 word *dst = Sp - 2, ret = word(Ip + 1);
 dst[0] = a, dst[1] = f, dst[2] = b, dst[3] = ret;
 return Sp = dst, Ip = (union u*) numap_drive, Continue(); }
lvm(lvm_add_coin) { return Ap(lvm_coin_op, g, DIE_ADD); }
lvm(lvm_mul_coin) { return Ap(lvm_coin_op, g, DIE_MUL); }
// `-` and `/` have no kind matrix; lvm_sub/lvm_quot intercept coins themselves and land here.
lvm(lvm_sub_coin) { return Ap(lvm_coin_op, g, DIE_SUB); }
lvm(lvm_quot_coin) { return Ap(lvm_coin_op, g, DIE_DIV); }

// applying a coin: run the die's APPLY closure as `((f self) arg)`; absent, a coin
// is an opaque handle (const-1), like a cask/port. self is the value at Ip (the apply
// trampoline sets Ip = the applied object); arg/ret are on the stack.
lvm(lvm_coin) {
 if (ai_nilp(g, die_get(g, coin_die(word(Ip)), DIE_APPLY)))
  return Ip = cell(*++Sp), *Sp = putcharm(1), Continue();   // default opaque-apply: const-1
 Have(2);
 word self = word(Ip), f = die_get(g, coin_die(self), DIE_APPLY);
 word arg = Sp[0], ret = Sp[1], *dst = Sp - 2;
 dst[0] = self, dst[1] = f, dst[2] = arg, dst[3] = ret;
 return Sp = dst, Ip = (union u*) numap_drive, Continue(); }

// (coin die payload) -> a fresh coin struck from the die over the payload.
lvm(lvm_coinmk) {
 Have(Width(struct ai_coin) + Width(struct ai_tag));
 union u *k = (union u*) Hp;
 Hp += Width(struct ai_coin) + Width(struct ai_tag);
 ((struct ai_coin*) k)->ap = lvm_coin;
 ((struct ai_coin*) k)->die = Sp[0];
 ((struct ai_coin*) k)->payload = Sp[1];
 tagthread(k, Width(struct ai_coin));
 return *++Sp = word(k), Ip++, Continue(); }
// (load x) -> the payload of a coin, else x itself (a plain value loads as itself).
lvm(lvm_load) {
 Sp[0] = coinp(Sp[0]) ? coin_load(Sp[0]) : Sp[0];
 return Ip++, Continue(); }
op11(lvm_dieof, coinp(Sp[0]) ? coin_die(Sp[0]) : nil)   // (die-of x): a coin's die, else ()
op11(lvm_coinp, coinp(Sp[0]) ? putcharm(1) : nil)   // (coin? x)

// apply function to one argument
lvm(lvm_ap) {
 union u *k;
 if (oddp(Sp[1])) return Ap(lvm_numap, g);
 k = cell(Sp[1]), Sp[1] = word(Ip + 1), Ip = k;
 YieldCheck();
 return Continue(); }

// tail call
lvm(lvm_tap) {
 if (oddp(Sp[1])) return Ap(lvm_numtap, g);         // fixnum operator -> num-ap, deliver to caller
 intptr_t x = Sp[0], j = Sp[1];
 Sp += getcharm(Ip[1].x) + 1;
 Ip = cell(j), Sp[0] = x;
 YieldCheck();
 return Continue(); }

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
 return Continue(); }

// tail call
lvm(lvm_tapn) {
 size_t n = getcharm(Ip[1].x),
        r = getcharm(Ip[2].x);
 Ip = cell(Sp[n]) + 2;
 word *o = Sp;
 for (Sp += r + 1; n--; Sp[n] = o[n]);
 YieldCheck();
 return Continue(); }

// return
lvm(lvm_ret) {
 word n = getcharm(Ip[1].x) + 1;
 return Ip = cell(Sp[n]), Sp[n] = Sp[0], Sp += n, Continue(); }

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
 return Ap(t, g); }

// kcall : x = Sp[0], k = Ip[1] -> Ip = k, Sp[0] = x
lvm(lvm_kcall) {
 word x = Sp[0];
 union u *stack = Ip + 2, *end = (union u*) ttag(g, stack);
 uintptr_t height = end - stack;
 Have(height);
 *(Sp = memmove(topof(g) - height, stack, height * sizeof(word))) = x;
 Ip = Ip[1].m;
 return Continue(); }

// callk : i = Sp[0], k = Ip + 1 -> Ip = i, Sp[0] = k
lvm(lvm_callk) {
 word f_val = Sp[0];                         // g, the call_cc arg
 if (oddp(f_val)) return Ip += 1, Continue();
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
 return Ap(lvm_ap, g); }

// lvm_yield_sw_mono can't call ai_wait_fds directly with a stack pointer
static ai_noinline void ai_wait_fd(int const fd, int n, uintptr_t ms) {
  ai_wait_fds(&fd, n, ms); }

// monotask fast path
static lvm(lvm_yield_sw_mono) { uintptr_t my_wake = g->next_wake_at;
 int my_wait_fd = g->next_wait_fd;
 g->next_wake_at = 0;
 g->next_wait_fd = -1;
 g->yield_ctr = 0;
 if (my_wake) for (uintptr_t now; my_wake > (now = ai_clock());)
  my_wait_fd >= 0 ? ai_wait_fd(my_wait_fd, 1, my_wake - now) : ai_sleep(my_wake - now);
 else if (my_wait_fd >= 0)
  while (!ai_ready(my_wait_fd)) ai_wait_fd(my_wait_fd, 1, 0);
 return Continue(); }

// First non-dormant peer in the ring whose wake_at <= now and whose
// wait_fd is either unset or actually ready. Without the wait_fd check
// a task parked on stdin would be scheduled immediately, busy-looping
// through yield_sw and filling the heap with stale task nodes.
static ai_inline union u *find_runnable(union u *head, uintptr_t now) {
 for (union u *n = head->m; n != head; n = n->m)
  if (n[1].m->ap != lvm_task_exit && (uintptr_t) getcharm(n[3].x) <= now) {
   int wf = (int) getcharm(n[4].x);
   if (wf < 0 || ai_ready(wf)) return n; }
 return NULL; }

static ai_noinline union u *yield_sw_wait(struct ai *g, uintptr_t my_wake, int my_wait_fd) {
 uintptr_t min_wake = my_wake;
 int fds[ai_wait_fds_max], nfds = 0;
 if (my_wait_fd >= 0) fds[nfds++] = my_wait_fd;
 for (union u *n = g->tasks->m; n != g->tasks; n = n->m)
  if (n[1].m->ap != lvm_task_exit) {
   uintptr_t wa = (uintptr_t) getcharm(n[3].x);
   if (wa && (!min_wake || wa < min_wake)) min_wake = wa;
   int wf = (int) getcharm(n[4].x);
   if (wf >= 0 && nfds < ai_wait_fds_max) fds[nfds++] = wf; }
 if (!min_wake && !nfds) return NULL;
 uintptr_t now = ai_clock();
 if (!min_wake) ai_wait_fds(fds, nfds, 0);
 else if (min_wake > now) ai_wait_fds(fds, nfds, min_wake - now);
 now = ai_clock();
 if (my_wait_fd >= 0 && ai_ready(my_wait_fd)) return NULL;
 return find_runnable(g->tasks, now); }

lvm(lvm_yield_sw) {
 if (g->tasks->m == g->tasks) return Ap(lvm_yield_sw_mono, g);
 union u *next = find_runnable(g->tasks, ai_clock());
 uintptr_t my_wake = g->next_wake_at;
 int my_wait_fd = g->next_wait_fd;
 if (!next) {
  // A *fairness* yield (this task is still runnable: no wake deadline, no I/O
  // wait) with no runnable peer -- just keep running this task. Crucially do NOT
  // fall into yield_sw_wait, which sleeps until the nearest peer's wake_at: that
  // would throttle a compute-heavy task to the slowest sleeping peer's period
  // (a 3 s heartbeat would crawl an `ev` to 64 ap-cycles per 3 s). A blocked task
  // (my_wake set, or my_wait_fd >= 0) still waits properly below; sleeping peers
  // are picked up by a later YieldCheck once their wake_at passes.
  if (!my_wake && my_wait_fd < 0) { g->yield_ctr = 0; return Continue(); }
  next = yield_sw_wait(g, my_wake, my_wait_fd);
  if (!next) {
   g->next_wake_at = 0;
   g->next_wait_fd = -1;
   if (g->yield_ctr >= yield_interval) g->yield_ctr = 0;
   return Continue(); } }
 word my_height = topof(g) - Sp;
 union u *next_stack = next + 5,
       *end = (union u*) ttag(g, next_stack);
 uintptr_t restore_h = end - next_stack,
           need = my_height + restore_h + 6;
 if (Sp < Hp + need) {
  Pack(g);
  if (!ai_ok(g = ai_please(ai_push(g, 1, next), need))) return ghelp(g);
  next = cell(pop1(g));
  Unpack(g);
  next_stack = next + 5; }   // recompute: next was forwarded by gc
 g->next_wake_at = 0;
 g->next_wait_fd = -1;
 union u *prev = next;
 while (prev->m != g->tasks) prev = prev->m;
 union u *N = (union u*) Hp;
 Hp += need - restore_h;
 N[0].m = g->tasks->m;
 N[1].m = Ip;
 N[2].x = g->tasks[2].x;
 N[3].x = putcharm((intptr_t) my_wake);
 N[4].x = putcharm(my_wait_fd);
 memcpy(N + 5, Sp, my_height * sizeof(word));
 prev->m = tagthread(N, 5 + my_height);
 // Pack FIRST: ai_young reads g->hp, and the live Hp runs ahead of the last Pack --
 // against a stale g->hp the fresh node reads as OLD, the barrier drops the edge, and
 // the next minor eats the ring (berth+ink froze in seconds on exactly this).
 Pack(g);
 gen_wb(g, (word) prev, (word) prev->m);   // task ring: an old node now links to the fresh (young) yield snapshot
 g->yield_ctr = 0;
 g->tasks = next;
 Sp = memmove(topof(g) - restore_h, next_stack, restore_h * sizeof(word));
 Ip = next[1].m;
 return Continue(); }

lvm(lvm_yield_nif) { return Ip++, Ap(lvm_yield_sw, g); }
lvm(lvm_task_exit) { return Ap(lvm_yield_sw, g); }
static union u const spawn_body[] = { {lvm_ap}, {.ap = lvm_task_exit} };
lvm(lvm_spawn) {
 Have(8);
 // New task node N: [next, saved_ip=spawn_body, pid, wake_at=0, wait_io=0, stack[0..1]=x,fn, tag]
 union u *N = (union u*) Hp;
 Hp += 8;
 word fn = Sp[0], x = Sp[1];
 uintptr_t pid = ++g->next_serial;   // a pid is a fresh identity: drawn from the mint stream
 N[0].m = g->tasks->m;
 N[1].m = (union u*) spawn_body;
 N[2].x = Sp[1] = putcharm(pid);
 N[3].x = nil;         // wake_at: sentinel for "always runnable"
 N[4].x = putcharm(-1);  // wait_fd: -1 = not waiting on I/O
 N[5].x = x;
 N[6].x = fn;
 g->tasks->m = tagthread(N, 7);
 Pack(g);   // sync: ai_young reads g->hp (see lvm_yield_sw)
 gen_wb(g, (word) g->tasks, (word) g->tasks->m);   // task ring: an old node now links to the fresh (young) spawned task
 return Sp++, Ip++, Continue(); }

lvm(lvm_wait) {
 word pid_arg = Sp[0], ret = nil;
 intptr_t target = getcharm(pid_arg);
 for (union u *node = g->tasks->m; node != g->tasks; node = node->m) {
  if (getcharm(node[2].x) != target) continue;
  if (node[1].m->ap == lvm_task_exit) {
   // dormant: dormant task's stack is just [retval] at node[5]
   ret = node[5].x;
   union u *prev = node;
   while (prev->m != node) prev = prev->m;
   prev->m = node->m;
   Pack(g);   // sync: ai_young reads g->hp (see lvm_yield_sw)
   gen_wb(g, (word) prev, (word) prev->m);   // task ring: unsplicing relinks an old node to a (maybe young) successor
   break; }
   // still running: yield without advancing Ip (re-enter wait on resume).
   // A task blocked in `wait` is polling a peer, NOT blocked on I/O or a timer,
   // so clear any stale next_wait_fd/next_wake_at (e.g. a serial-read fd left set
   // by the kernel's cooperative input) -- otherwise yield_sw saves this task as
   // parked on an unready fd and find_runnable never reschedules it (deadlock).
   g->next_wake_at = 0;
   g->next_wait_fd = -1;
  return Ap(lvm_yield_sw, g); }
 return *Sp = ret, Ip++, Continue(); }

lvm(lvm_donep) {
 word pid_arg = Sp[0], result = putcharm(1);
 intptr_t target = getcharm(pid_arg);
 for (union u *node = g->tasks->m; node != g->tasks; node = node->m)
  if (getcharm(node[2].x) == target) {
   if (node[1].m->ap != lvm_task_exit) result = nil;
   break; }
 Sp[0] = result;
 Ip += 1;
 return Continue(); }

lvm(lvm_hush) {
 word pid_arg = Sp[0], result = nil;
 intptr_t target = getcharm(pid_arg);
 union u *prev = g->tasks;
 for (union u *node = prev->m; node != g->tasks; prev = node, node = node->m)
  if (getcharm(node[2].x) == target) {
   prev->m = node->m;
   Pack(g);   // sync: ai_young reads g->hp (see lvm_yield_sw)
   gen_wb(g, (word) prev, (word) prev->m);   // unsplice relinks an old node to a (maybe young) successor
   result = putcharm(1);
   break; }
 Sp[0] = result;
 Ip += 1;
 return Continue(); }

lvm(lvm_sleep) {
 word n = Sp[0];
 Sp[0] = nil;
 Ip += 1;
 // rest waits on the CLOCK alone: a lingering next_wait_fd (the one-shot read
 // intention -- see YieldCheck) would ride into the park and find_runnable then
 // gates the timer on that fd firing -- a periodic task ticks only when stray
 // traffic arrives and sleeps forever on a quiet port (haven's painter did).
 g->next_wait_fd = -1;
 if (!charmp(n) || getcharm(n) <= 0) { g->next_wake_at = 0; return Ap(lvm_yield_sw, g); }
 g->next_wake_at = (uintptr_t) ai_clock() + getcharm(n);
 return Ap(lvm_yield_sw, g); }


lvm(lvm_jump) { return Ip = Ip[1].m, Continue(); }
// The only compiled truthiness branch (`?`, and the `&&`/`||` macros). Uses the
// language falsy predicate so an all-zero vec (boxed 0.0, zero int box,
// all-zero array) takes the false arm, lifting "0 is the only false scalar".
lvm(lvm_cond) { return Ip = ai_nilp(g, *Sp++) ? Ip[1].m : Ip + 2, Continue(); }
lvm(lvm_unc) {
 Have1();
 *--Sp = Ip[1].x;
 Ip = Ip[2].m;
 return Continue(); }

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
 return Continue(); }

// A port has no function meaning either: applying it behaves as 0 (yields 1), like
// a buf (byte-identical body, kept distinct by ai_noicf -- see lvm_buf).
lvm(lvm_port_io) {
  Ip = cell(*++Sp);
  *Sp = putcharm(1);
  return Continue(); }

// push a value from the stack
lvm(lvm_arg) {
 Have1();
 Sp[-1] = Sp[getcharm(Ip[1].x)];
 Sp -= 1;
 Ip += 2;
 return Continue(); }

// fused (lvm_arg <idx> ; lvm_ap): push local at <idx>, then apply. A 2-word op
// emitted by the compiler's `karg` when an arg ref is immediately followed by a
// non-tail ap (the dominant "call a function on a local" shape). Saves one
// dispatch + the standalone ap word vs. the unfused chain. The post-pattern
// resume address is Ip+2 (cf. lvm_ap's Ip+1, since the op is one word longer).
lvm(lvm_argap) {
 if (oddp(Sp[0])) {                                  // fixnum operator -> num-ap, resume at Ip+2
  Have1();
  Sp[-1] = Sp[getcharm(Ip[1].x)], Sp -= 1, Ip += 1;   // push local under operator; resume now Ip+2
  return Ap(lvm_numap, g); }
 Have1();
 Sp[-1] = Sp[getcharm(Ip[1].x)];
 Sp -= 1;
 union u *k = cell(Sp[1]); Sp[1] = word(Ip + 2), Ip = k;
 YieldCheck();
 return Continue(); }

// fused (lvm_quote <v> ; lvm_ap): push constant <v>, then apply. Emitted by
// kim when a quote is immediately followed by a non-tail ap (a call with a
// constant arg, e.g. (k 0)). Resume at Ip+2 (2-word op), cf. lvm_argap.
lvm(lvm_quoteap) {
 if (oddp(Sp[0])) {                                  // fixnum operator -> num-ap, resume at Ip+2
  Have1();
  Sp[-1] = Ip[1].x, Sp -= 1, Ip += 1;               // push const under operator; resume now Ip+2
  return Ap(lvm_numap, g); }
 Have1();
 Sp -= 1;
 Sp[0] = Ip[1].x;
 union u *k = cell(Sp[1]); Sp[1] = word(Ip + 2), Ip = k;
 YieldCheck();
 return Continue(); }

// fused (lvm_arg <idx> ; lvm_tap <fs>): push local <idx>, then tail-call,
// popping frame size <fs> at Ip[2] (tap's operand, kept in place by the fused
// emit). The single-arg tail-call shape, e.g. a tail (loop x) or cont (k v).
lvm(lvm_argtap) {
 if (oddp(Sp[0])) {                                  // fixnum operator -> num-ap, deliver to caller
  Have1();
  Sp[-1] = Sp[getcharm(Ip[1].x)], Sp -= 1, Ip += 1;   // push local under operator; fs operand now Ip[1]
  return Ap(lvm_numtap, g); }
 Have1();
 Sp[-1] = Sp[getcharm(Ip[1].x)];
 Sp -= 1;
 intptr_t x = Sp[0], j = Sp[1];
 Sp += getcharm(Ip[2].x) + 1;
 Ip = cell(j), Sp[0] = x;
 YieldCheck();
 return Continue(); }

// operand-value-specialized arg/quote: 1-word ops with the index/constant baked
// into the ap (no Ip[1] operand fetch). Emitted by the compiler's spa/spq for
// the hottest indices {0..3} / constants {0,1,2,3,-1,-2}.
argn(lvm_arg0, 0) argn(lvm_arg1, 1) argn(lvm_arg2, 2) argn(lvm_arg3, 3)
quon(lvm_quo0, 0) quon(lvm_quo1, 1) quon(lvm_quo2, 2) quon(lvm_quo3, 3)
quon(lvm_quom1, -1) quon(lvm_quom2, -2)

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
 return c->x = Sp[1], *(Sp += 2) = word(c), Ip++, Continue(); }

lvm(lvm_spin) {
 size_t n = getcharm(Sp[0]);
 Have(n + Width(struct ai_tag));
 union u *k = (union u*) Hp;
 Hp += n + Width(struct ai_tag);
 Sp[0] = word(memset(tagthread(k, n), -1, n * sizeof(word)));
 return Ip++, Continue(); }

// ceil a positive measure into a fixnum, saturating at fix_max. ceil (not floor)
// so the result is 0 *only* when m is exactly 0 -- $ then doubles as a zero test:
// (= 0 ($ x)) iff x is nothing. Never overflows putcharm's tag.
static ai_inline intptr_t len_sat(ai_flo_t m) {
  if (m >= (ai_flo_t) fix_max) return fix_max;
  intptr_t i = (intptr_t) m;                    // trunc toward 0 (m >= 0)
  return i + (m > (ai_flo_t) i ? 1 : 0); }       // bump for any fractional part -> ceil
// The NET: the COMPLEX-VALUED measure of any value (the net->C round: the spec's
// additivity law -- net(a + b) = net a + net b wherever + is total -- holds over
// complex operands only if the measure keeps phase, so the codomain is C and the
// order retraction happens ONCE, in the observers). A complex scalar nets ITSELF;
// every other scalar nets on the real line (a number its value, text/spellings
// their charm sums, a table its key count, a fn/port 1); a chain or rank>=1
// array nets the SUM of its elements' nets -- a TRUE complex sum, recursive and
// UNCLAMPED, the SPINE only (a dotted tail is not an element) -- so a list of
// negatives nets negative, a chain of nothings nets to nothing, and opposite
// phases annihilate by VECTOR cancellation, not by the order's tiebreak. The
// arrangement does not matter: a packed ai_C array and the o-list of the same
// values net the same sum, and net(asum v) = net(v) by construction.
//   ai_C  packed (re,im) float chains at vec_data -> componentwise sum
//   ai_O  object words -> each element's own ai_net (recursive; depth bounded by nesting)
//   ai_Z/ai_R  the element values directly (vec_get_flo), imaginary part 0
static struct ai_zn ai_net(struct ai *g, word x) {
  if (charmp(x)) return zn((ai_flo_t) getcharm(x), 0);               // fixnum: its value
  if (bufp(x)) { struct ai_str *b = buf_str(x); ai_flo_t t = 0;   // hot chars: Σ charms, like a string
    for (uintptr_t i = 0; i < b->len; i++) t += (uint8_t) b->bytes[i];
    return zn(t, 0); }
  if (tabp(x)) return zn((ai_flo_t) map_len(x), 0);              // table: key count
  if (coinp(x)) {                                              // a coin nets its payload (the monoid hom), unless
    word mode = die_get(g, coin_die(x), DIE_NET);              // its die pins a net MODE.
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
    default: return zn(1, 0);                                   // unknown present data kind -> truthy
    case KString: { ai_flo_t t = 0;                                 // a string is PACKED CHARS: Σ charms
      for (uintptr_t i = 0; i < len(x); i++) t += (uint8_t) txt(x)[i];
      return zn(t, 0); }                                           // (the count moved to tally)
    case KChain: { struct ai_zn s = zn(0, 0); word p = x;           // chain: sum the SPINE's nets --
      do { struct ai_zn e = ai_net(g, A(p));                       // complex sums, so negatives cancel,
           s.re += e.re, s.im += e.im;                           // phases cancel, and a chain of
           p = B(p); } while (chainp(p));                          // nothings nets to nothing
      return s; }
    case KBig: return zn(ai_big_to_flo(x), 0);                   // bignum: full magnitude, sign intact
    case KFlo: return zn(flo_get(x), 0);                         // a boxed float nets its value
    case KWide: return zn((ai_flo_t) box_get(x), 0);             // a boxed wide int nets its value
    case KCplx: return zn(cplx_re(x), cplx_im(x));               // a complex nets ITSELF (phase intact)
    case KMint: return zn(0, 0);                                 // a bare point nets nothing (the distinct nothing)
    case KNom: { ai_flo_t t = 0; struct ai_str *s = str(nom(x)->name);  // a named point nets its SPELLING's charms
      for (uintptr_t i = 0; i < s->len; i++) t += (uint8_t) txt(s)[i];
      return zn(t, 0); }
    case KVec: { struct ai_vec *v = vec(x);                 // a rank>=1 array (scalar gems: KFlo/KWide/KCplx)
      uintptr_t i, n = vec_nelem(v);
      struct ai_zn s = zn(0, 0);                                  // rank>=1 array -> Σ elem
      if (v->type == ai_C) { ai_flo_t *d = vec_data(v);
        for (i = 0; i < n; i++) s.re += d[2*i], s.im += d[2*i+1];
        return s; }
      if (v->type == ai_O)
        for (i = 0; i < n; i++) { struct ai_zn e = ai_net(g, vec_get_obj(v, i));
          s.re += e.re, s.im += e.im; }
      else for (i = 0; i < n; i++) s.re += vec_get_flo(v, i);
      return s; } } }
// The $ operator: the net observed once -- max(0, ceil) of its ORDER-SIGNED
// MAGNITUDE (a real net is its own magnitude, exactly; a phaseful net takes
// |z|, gated by zn_false) -- so (nilp x) == (= 0 ($ x)) at every kind and
// rank: a negative real, a non-positive complex, a list or array whose net
// with a non-positive REAL part all measure 0. Lockstep with ai_nilp.
static intptr_t ai_pin(struct ai *g, word x) {
  if (charmp(x)) { intptr_t n = getcharm(x); return n <= 0 ? 0 : n; }   // <= 0 -> 0 (0 is nil), exact
  struct ai_zn z = ai_net(g, x);
  if (zn_false(z)) return 0;
  return len_sat(z.im == 0 ? z.re : ai_sqrt(z.re * z.re + z.im * z.im)); }
lvm(lvm_pin) { Sp[0] = putcharm(ai_pin(g, Sp[0])); Ip += 1; return Continue(); }

// ============================================================================
// io
// ============================================================================
// THE ATOMIC-EDGE CONTRACT (an io_* function owns the io buffer slot).
// A `to` string sink (struct to) GROWS its backing ai_str via str0 -> a GC, and
// so does a bio port whose device short-changed a drain (bio_wgrow), so EVERY
// write is a relocation point. The contract for the whole io_* family: an io op
// that spans more than one write must PARK its heap operand (ai_push -> g->sp)
// and RE-READ it across each one -- never hold a raw pointer over an edge.
// Audited 2026-06-16, extended to the bulk lane 2026-07-31 when writen took the
// allocating half of the vt; the family holds it without exception:
//   * structural printers park + re-read: ioput_str/_sym/_two/_map/_big/_arr
//     (e.g. ioput_chain re-reads A/B(g->sp[0]) after every byte; ioput_map snapshots
//     k/v into a fresh list under the seen-list cycle guard).
//   * to_writen re-derives o = g->io (GC-traced) and copy-then-swaps the grown
//     buffer, so the swap is atomic across its own str0 GC -- and it lands NOTHING
//     on the growing call, because its src may be the very string being printed.
//   * io_wdrain roots the port itself (avec) across the stroke and re-derives the
//     backing after it, since writen is allowed to allocate now.
//   * scalar decompositions (re/im, float) are read to C locals BEFORE any zputc
//     (ioput_vec_scalar_complex, ioput_carr_elem).
//   * pure C-data emitters hold no heap operand, so need no park: ioputcs/ioputn/
//     ioprintf (integer + char formats only -- no %s).
//   * lvm edges Pack/Unpack and re-read bytes each step (lvm_dot/_fputs/_fputc),
//     zflush at the close; a scare mid-print stops cleanly (ai_ok gates).
// The lam_* lambda-canonicalization helpers are PURE (no io, no buffer): they
// cannot open an edge, and lam_canon reserves its cells up front (alloc-free
// rebuild), so even ioput_fn_body -> lam_canon keeps the surrounding op atomic.
static ai_inline bool iop(word x) { return lamp(x) && cell(x)->ap == lvm_port_io; }
static ai_inline struct ai_port_vt const *port_vt(word fd_tagged) {
 intptr_t fd = getcharm(fd_tagged);
 return fd >= 0 ? &ai_fd_port_vt : &synth[-(fd + 1)]; }

// --- the buffered lanes (generic, above the vt) ------------------------------
// a HEAP fd port is an ai_bio (love.h): both buffer lanes ride behind the bare
// ai_io head, dressed lazily and invisible everywhere else. bio_of is the ONE
// guard -- heap (traced, so a backing survives GC) AND fd >= 0 (synth ports
// overlay their own fields past the head; statics are untraced) -- nothing
// reads past the head without it. zgetc serves ungetc -> the pending run ->
// one readn gulp, and THAT ORDER IS THE PARK LAW: a port holding bytes is
// readable however quiet its fd is, so nothing above needs to ask twice. a dry
// gulp hands back IO_WOULDBLOCK and the caller parks. zputc lands bytes in
// wbuf and drains by whole writen strokes; a read on the same port drains
// writes first (the request/response crossover), as do flush and say's bulk
// lane; the finalizer drains a dying port through ai_fd_drain.
static ai_inline struct ai_bio *bio_of(struct ai *g, struct ai_io *i) {
 return getcharm(i->fd) >= 0 && in_live_pool(ai_core_of(g), (word const*) i)
      ? (struct ai_bio*) i : NULL; }
static ai_inline bool bio_rpending(struct ai_bio *b) {
 return b && b->rbuf && !(b->rbuf & 1) && getcharm(b->rpos) < getcharm(b->rlen); }
static ai_inline bool bio_wpending(struct ai_bio *b) {
 return b && b->wbuf && !(b->wbuf & 1) && getcharm(b->wlen) > 0; }
// the write run outgrew its backing: double it, pending bytes and all. only
// reachable when a device took LESS than the whole run -- a port that drains to
// empty never sees it -- so the growth is the residue's, not the buffer size's.
static struct ai *bio_wgrow(struct ai *g) {
 struct ai_bio *b = (struct ai_bio*) ai_core_of(g)->io;
 uintptr_t n = getcharm(b->wlen), cap = len((struct ai_str*) b->wbuf);
 if (!ai_ok(g = str0(g, cap ? cap * 2 : ai_iobuf))) return g;
 struct ai *fc = ai_core_of(g);
 b = (struct ai_bio*) fc->io;
 struct ai_str *nb = (struct ai_str*) fc->sp[0];
 memcpy(txt(nb), txt((struct ai_str*) b->wbuf), n);
 b->wbuf = (word) nb;
 gen_wb(fc, (word) b, b->wbuf);
 fc->sp += 1;
 return g; }
// ⚠ WHAT DID NOT LAND STAYS PENDING. this used to zero wlen BEFORE the stroke and
// throw writen's count away, so a mid-buffer EPIPE or ENOSPC dropped the tail in
// silence -- the port reported a clean write of bytes the fd never took. the run
// slides down to the front instead, and the next drain carries it.
static struct ai *io_wdrain(struct ai *g, struct ai_io *i) {
 if (!ai_ok(g) || !bio_wpending(bio_of(g, i))) return g;
 struct ai_port_vt const *vt = port_vt(i->fd);
 if (!vt->writen) return g;                     // no write door: the run waits for one
 for (;;) {
  struct ai_bio *b = (struct ai_bio*) i;
  uintptr_t n = getcharm(b->wlen);
  if (!n) return g;
  intptr_t k;
  avec(g, i, k = vt->writen(&g, (unsigned char*) txt((struct ai_str*) b->wbuf), n));
  if (!ai_ok(g) || k <= 0) return g;
  b = (struct ai_bio*) i;                       // writen may allocate: re-derive
  char *w = txt((struct ai_str*) b->wbuf);
  if ((uintptr_t) k < n) memmove(w, w + k, n - (uintptr_t) k);
  b->wlen = putcharm(n - (uintptr_t) k); } }
// io_refill's third answer, beside a byte and EOF: the device has nothing right
// now. distinct from EOF (-1) and from any byte -- the one sentinel/two meanings
// bug this arc kept finding, spelled apart on purpose. it never escapes lvm_fgetc.
#define IO_WOULDBLOCK ((uintptr_t) -2)
// the three answers, in one place for every port there is. a port with no read
// method is at END (that is what a NULL slot means); a port with no BUFFER asks
// for exactly one byte and is otherwise identical.
// ⚠ THE UNBUFFERED LANE STAYS UNBUFFERED. bio_of refuses the statics (they sit
// outside the live pool), so `in` reads one byte per readn -- and it must, because
// `trickle` and every interactive reader stand on stdin not gulping past what was
// asked for. a buffer here would make the repl swallow the line after the one it
// is reading, and re-open the ownership question doc/io.md part III closed.
static struct ai *io_refill(struct ai *g) {
 struct ai *fc = ai_core_of(g);
 struct ai_bio *b = bio_of(g, fc->io);
 struct ai_port_vt const *vt = port_vt(fc->io->fd);
 if (!vt->readn) return fc->b = EOF, g;
 if (!b) {                                       // no buffer: the same lane at n = 1
  unsigned char c;
  intptr_t k = vt->readn(g, &c, 1);
  if (k > 0) fc->b = c;
  else if (k < 0) fc->b = EOF;
  else fc->b = IO_WOULDBLOCK;
  return g; }
 if (bio_wpending(b)) {                          // the crossover: our unsent ask goes first
  if (!ai_ok(g = io_wdrain(g, fc->io))) return g;
  fc = ai_core_of(g), b = (struct ai_bio*) fc->io; }
 if (!b->rbuf || (b->rbuf & 1)) {                // first buffered read: dress the backing
  if (!ai_ok(g = str0(g, ai_iobuf))) return g;
  fc = ai_core_of(g), b = (struct ai_bio*) fc->io;   // the GC may have moved the port
  b->rbuf = fc->sp[0];
  b->rpos = b->rlen = putcharm(0);
  gen_wb(fc, (word) b, b->rbuf);                 // a tenured port takes a young backing
  fc->sp += 1; }
 struct ai_str *r = (struct ai_str*) b->rbuf;
 intptr_t k = vt->readn(g, (unsigned char*) txt(r), r->len);
 if (k > 0) {
  b->rlen = putcharm(k), b->rpos = putcharm(1);
  fc->b = (unsigned char) txt(r)[0];
  return g; }
 if (k < 0) return fc->b = EOF, g;
 // k == 0 is "would block", and it is the ORDINARY answer now: every quiet device
 // on every frontend comes through here, so every test that reads anything walks
 // it (test/front/io.l gates the sharp case, where the fd said ready and then said
 // no). ⚠ NEVER WAIT HERE. this runs under lvm_fgetc, which is ONE op -- a
 // blocking poll stops the whole VM, not the reading task, so every other task
 // starves on one quiet fd. hand it back and let the caller park.
 return fc->b = IO_WOULDBLOCK, g; }
static ai_inline struct ai *zgetc(struct ai*g) {
 if (!ai_ok(g)) return g;
 struct ai *fc = ai_core_of(g);
 struct ai_io *i = fc->io;
 if (getcharm(i->ungetc_buf) != EOF) {
  fc->b = getcharm(i->ungetc_buf);
  i->ungetc_buf = putcharm(EOF);
  return g; }
 struct ai_bio *b = bio_of(g, i);
 if (bio_rpending(b)) {
  uintptr_t p = getcharm(b->rpos);
  fc->b = (unsigned char) txt((struct ai_str*) b->rbuf)[p];
  b->rpos = putcharm(p + 1);
  return g; }
 return io_refill(g); }
// the pushback is the PORT's, not the device's: one head word, set here for every
// kind of port there is. it was a vt slot with nine identical implementations, and
// the synthetic rows answered a no-op -- so `unsee` on a jug threw the byte away
// while zgetc above would happily have served it back.
static ai_inline struct ai *zungetc(struct ai*g, int c) {
 if (!ai_ok(g)) return g;
 struct ai *fc = ai_core_of(g);
 struct ai_io *i = fc->io;
 i->ungetc_buf = putcharm(c);
 return fc->b = c, g; }
static struct ai *zputc(struct ai*g, int c) {
 if (!ai_ok(g)) return g;
 struct ai *fc = ai_core_of(g);
 struct ai_bio *b = bio_of(g, fc->io);
 struct ai_port_vt const *vt = port_vt(fc->io->fd);
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
 if (n >= len((struct ai_str*) b->wbuf)) {       // a drain the device short-changed left
  if (!ai_ok(g = bio_wgrow(g))) return g;        // no room: the residue keeps its place
  fc = ai_core_of(g), b = (struct ai_bio*) fc->io; }
 struct ai_str *w = (struct ai_str*) b->wbuf;
 txt(w)[n] = (char) c;
 b->wlen = putcharm(n + 1);
 return n + 1 >= w->len ? io_wdrain(g, fc->io) : g; }
static struct ai *zflush(struct ai*g) {
 if (!ai_ok(g)) return g;
 g = io_wdrain(g, ai_core_of(g)->io);
 return ai_ok(g) ? port_vt(ai_core_of(g)->io->fd)->flush(g) : g; }
// the exported faces (love.h): a host nif consults/drains the read run without
// knowing the bio shape -- swig's first course rides these.
uintptr_t ai_io_pending(struct ai *g, struct ai_io *i) {
 struct ai_bio *b = bio_of(g, i);
 return bio_rpending(b) ? (uintptr_t)(getcharm(b->rlen) - getcharm(b->rpos)) : 0; }
uintptr_t ai_io_read_drain(struct ai *g, struct ai_io *i, unsigned char *dst, uintptr_t n) {
 struct ai_bio *b = bio_of(g, i);
 if (!bio_rpending(b)) return 0;
 uintptr_t p = getcharm(b->rpos), l = getcharm(b->rlen), k = l - p < n ? l - p : n;
 memcpy(dst, txt((struct ai_str*) b->rbuf) + p, k);
 b->rpos = putcharm(p + k);
 return k; }
struct ai *ai_io_wflush(struct ai *g, struct ai_io *i) { return io_wdrain(g, i); }
// GC-context finalizer hook: weak no-op; the host overrides with write(2).
__attribute__((weak)) void ai_fd_drain(int fd, void const *p, uintptr_t n) {
 (void) fd; (void) p; (void) n; }
struct ci { struct ai_io io; ai_word head; }; // charlist input
struct to { struct ai_io io; struct ai_str *buf; ai_word i; }; // lisp string output
static struct ai *ai_dtoa2(struct ai*, ai_flo_t);
static struct ai *gfputx(struct ai *g, struct ai_io *o, intptr_t x);

static struct ai *noop_flush(struct ai *g) { return g; }

// the charlist source's read door. it walks the spine and never blocks, so a
// spent list is the END -- there is no "quiet" answer a colist could give.
// ⚠ it is asked for ONE byte at a time and always will be: fd -4 fails bio_of,
// so there is no backing to gulp into. written as the bulk lane anyway, because
// the contract is the contract and the loop costs nothing.
// ⚠ a charm outside 0..255 lands as its LOW BYTE, which ci_getc did not do -- it
// handed the raw charm straight to g->b, so a -1 in the list FORGED the end of
// the stream. probed against both binaries: `(flow (tap '(-1 65)))` came back
// EMPTY before and is 2 long now, so it was sound/reads/slurp that lost the whole
// list to one element, not `see` (which answered -1 and carried on -- worse, since
// nothing looked wrong). a port is a byte stream on every other row; this was the
// exception. the law is in test/io.l's tap section.
static intptr_t ci_readn(struct ai *g, unsigned char *dst, uintptr_t n) {
 struct ci *i = (struct ci*) g->io;
 uintptr_t k = 0;
 while (k < n && chainp(i->head))
  dst[k++] = (unsigned char) getcharm(A(i->head)), i->head = B(i->head);
 return k ? (intptr_t) k : -1; }

static struct ai *to_flush(struct ai *g) { return g; }

// the string sink's write door (the writen contract lives in love.h). it lands
// what fits in the CURRENT backing; when the backing is spent it DOUBLES and
// answers 0, having landed nothing.
// ⚠ THE GROW AND THE COPY CANNOT SHARE A CALL. str0 collects, and src may be the
// heap string the caller is printing FROM -- copying after the grow reads a moved
// address. so the grow is its own answer: the caller re-derives its source and
// comes back to a sink with room. zputc's one-byte lane holds src in a C local,
// which the GC never moves, so its second ask always lands.
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
 struct ai_str *nb = (struct ai_str*) g->sp[0];
 memcpy(txt(nb), txt(o->buf), i);
 o->buf = nb;
 gen_wb(g, (word) o, (word) nb);   // a tenured string-sink takes a fresh young backing -> remember it
 g->sp++;
 return 0; }
struct ai_port_vt const synth[] = {
 /* fd = -1, ti: a read-only C-string source. NOTHING CONSTRUCTS ONE -- p0onto
    was its only maker and the boot cursor is a charlist now (rung 8) -- but the
    fd is a PROTOCOL number that prel pokes by hand, so the row stays a hole
    rather than renumbering its neighbours. */
 { noop_flush, NULL,      NULL },
 /* fd = -2, to: write-only vec sink   */
 { to_flush,   to_writen, NULL },
 /* fd = -3, closed port (post-close)  */
 { noop_flush, NULL,      NULL },
 /* fd = -4, ci: read-only charlist source -- prel's `tap` builds one by poke. */
 { noop_flush, NULL,      ci_readn }, };

// (fputc port byte) — write byte to port; return byte.
lvm(lvm_fputc) {
 if (iop(Sp[0])) {
  g->io = (struct ai_io*) Sp[0];
  Pack(g);
  if (!ai_ok(g = zputc(g, getcharm(g->sp[1])))) return ghelp(g);
  Unpack(g); }
 return Sp++, Ip++, Continue(); }

// (fflush port) — flush; return the port.
lvm(lvm_fflush) {
 if (iop(Sp[0])) {
  g->io = (struct ai_io*) Sp[0];
  Pack(g);
  if (!ai_ok(g = zflush(g))) return ghelp(g);
  Unpack(g); }
 return Ip++, Continue(); }

// (fputs port s) — write every byte of string-or-buf s through port; return
// the port. No-op when args are misused (non-port, or neither string nor
// buf). bytes_of resolves either to the ai_str holding the bytes, re-read each
// iteration so GC inside zputc (e.g., growing a sink buffer) can forward it
// safely (for a buf, GC may move both the wrapper and its backing string).
lvm(lvm_fputs) {
 if (iop(Sp[0]) && (strp(Sp[1]) || bufp(Sp[1]))) {
  g->io = (struct ai_io*) Sp[0];
  uintptr_t i = 0, l = len(bytes_of(Sp[1]));
  // the bulk lane when the port has one (writen: fd ports write(2) the run,
  // sinks memcpy what fits); a 0 makes one byte of progress through zputc, whose
  // one-byte src is a C local -- the only shape in which a sink may grow and land
  // in the same breath. src re-derives from g->sp[1] every pass, so a
  // GC-forwarded string is safe.
  // ⚠ THE DIRECT STROKE IS ONLY FOR AN EMPTY BUFFER. zputc parks its byte in the
  // port's write run, so a stroke that went direct while something was pending
  // would OVERTAKE it and the stream would come out shuffled. Once the device
  // short-changes us, everything rides the buffer until a drain empties it again.
  intptr_t (*wn)(struct ai**, unsigned char const*, uintptr_t) = port_vt(g->io->fd)->writen;
  Pack(g);
  g = io_wdrain(g, (struct ai_io*) g->sp[0]);   // buffered puts land before the bulk stroke
  while (ai_ok(g) && i < l) {
   struct ai *w = g;       // the frame BY ADDRESS, off the restrict-qualified param
   intptr_t k = wn && !bio_wpending(bio_of(g, (struct ai_io*) g->sp[0]))
              ? wn(&w, (unsigned char const*) txt(bytes_of(w->sp[1])) + i, l - i) : 0;
   g = w;
   if (k > 0) i += (uintptr_t) k;
   else g = zputc(g, txt(bytes_of(g->sp[1]))[i++]); }
  if (!ai_ok(g = zflush(g))) return ghelp(g);
  Unpack(g); }
 return Sp++, Ip++, Continue(); }

lvm(lvm_fputx) {
 if (iop(Sp[0])) {
  Pack(g);
  if (!ai_ok(g = gfputx(g, (struct ai_io*) Sp[0], Sp[1]))) return ghelp(g);
  Unpack(g); }
 return Sp++, Ip++, Continue(); }

// (dot x) -> print x to `out` and return x. A string/buf is written verbatim
// (puts discipline -- raw bytes); any other value in external form (putx
// discipline -- the inspect form). The `.` reader sigil expands to (dot x). The
// string bytes are re-read from g->sp[0] each iteration (GC inside zputc may
// forward them); gfputx is self-GC-safe over its value arg, as in lvm_fputx.
lvm(lvm_dot) {
 word x = Sp[0];
 g->io = &ai_stdout;
 Pack(g);
 if (strp(x) || bufp(x)) {
  uintptr_t i = 0, l = len(bytes_of(x));
  while (ai_ok(g) && i < l) g = zputc(g, txt(bytes_of(g->sp[0]))[i++]);
  if (ai_ok(g)) g = zflush(g); }
 else g = gfputx(g, &ai_stdout, x);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 return Ip++, Continue(); }

static struct ai*gfputbn(struct ai *g, intptr_t n, uint8_t b, struct ai_io *o);
lvm(lvm_fputbn) {
 if (iop(Sp[0])) {
   Pack(g);
   if (!ai_ok(g = gfputbn(g, getcharm(Sp[1]), getcharm(Sp[2]), (struct ai_io*) Sp[0]))) return ghelp(g);
   Unpack(g);
   Sp[2] = Sp[1]; }
 return Sp += 2, Ip++, Continue(); }

static struct ai*ioputc(struct ai*g, int c) { return zputc(g, c); }
static struct ai*ioputs(struct ai*g, char const *s) {
 while (*s) g = ioputc(g, *s++);
 return g; }

// the terminal scare face (declared in love.h): an helpless scare's stashed
// condition data prints as ";; a b" -- the shell help's face -- to the err
// port; the bare scare (nil nil) is oom, which has no data: answer 0 and let
// the frontend report it raw. best-effort: a failure mid-print just stops.
int ai_scare_face_(struct ai *g) {
 struct ai *c = ai_core_of(g);
 if (!c || (nilp(c->scare_a) && nilp(c->scare_b))) return 0;
 c->io = &ai_stderr;
 struct ai *h = ioputs(c, ";; ");  // gfputx may GC and MOVE the core: re-derive
 if (ai_ok(h)) h = gfputx(h, &ai_stderr, ai_core_of(h)->scare_a);
 if (ai_ok(h)) h = ioputc(h, ' ');
 if (ai_ok(h)) h = gfputx(h, &ai_stderr, ai_core_of(h)->scare_b);
 if (ai_ok(h)) h = ioputc(h, '\n');
 if (ai_ok(h)) zflush(h);
 return 1; }

static struct ai*ioputn(struct ai *g, intptr_t n, uint8_t b) {
 uintptr_t
  m = n >= 0 || b != 10 ? (uintptr_t) n : (g = ioputc(g, '-'), -(uintptr_t) n),
  q = m / b,
  r = m % b;
 if (q) g = ioputn(g, q, b);
 return ioputc(g, ai_digits[r]); }

static ai_inline struct ai*gfputbn(struct ai *g, intptr_t n, uint8_t b, struct ai_io *o) {
 return g->io = o, ioputn(g, n, b); }

static struct ai*gvzprintf(struct ai*g, char const *fmt, va_list xs) {
 for (int c; (c = *fmt++);) {
  if (c != '%') g = ioputc(g, c);
  else pass: switch ((c = *fmt++)) {
   case 0: return g;
   case 'l': goto pass;
   case 'b': g = ioputn(g, va_arg(xs, uintptr_t), 2); continue;
   case 'n': g = ioputn(g, va_arg(xs, uintptr_t), 6); continue;
   case 'o': g = ioputn(g, va_arg(xs, uintptr_t), 8); continue;
   case 'd': g = ioputn(g, va_arg(xs, uintptr_t), 10); continue;
   case 'u': g = ioputn(g, va_arg(xs, uintptr_t), 12); continue;
   case 'x': g = ioputn(g, va_arg(xs, uintptr_t), 16); continue;
   case 'z': g = ioputn(g, va_arg(xs, uintptr_t), 36); continue;
   case '%': g = ioputc(g, '%'); continue;             // %% -> literal %
   default: g = ioputc(g, c); } }
 return g; }

static struct ai *ioprintf(struct ai *g, char const *fmt, ...) {
 va_list xs;
 va_start(xs, fmt);
 g = gvzprintf(g, fmt, xs);
 va_end(xs);
 return g; }

static struct ai *ioputx(struct ai *g, intptr_t x, uintptr_t off);
static struct ai *ioputcs(struct ai *g, char const *s);

// --- print cycle detection (tables only) --------------------------------------
// A "seen" list of the tables on the current print path lives in a single stack
// slot at the bottom of the print region (established by gfputx). It moves with
// the stack on GC, so callers locate it by its offset from the stack top (`off`),
// which GC preserves; the offset is threaded down the recursion as an ordinary
// integer (no struct-g state). A table is consed on as we descend into it and
// dropped as we ascend, so the list is exactly the ancestor path of tables. When
// printing finishes gfputx restores the original stack height, discarding it.
static word *seen_slot(struct ai *g, uintptr_t off) {
 return topof(ai_core_of(g)) - off; }
static bool seen_member(struct ai *g, uintptr_t off, word x) {
 for (word l = *seen_slot(g, off); chainp(l); l = B(l)) if (A(l) == x) return true;
 return false; }
static struct ai *seen_push(struct ai *g, uintptr_t off, word x) {   // link x onto seen
 if (!ai_ok(g = ai_push(g, 1, x))) return g;                         // protect x across GC
 if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return ai_pop(g, 1);
 struct ai_chain *p = bump(g, Width(struct ai_chain));
 word *slot = seen_slot(g, off);                                   // re-read: GC may move it
 ini_chain(p, g->sp[0], *slot);
 *slot = (word) p;
 return ai_pop(g, 1); }
static void seen_pop(struct ai *g, uintptr_t off) {                 // drop the newest entry
 word *slot = seen_slot(g, off);
 *slot = B(*slot); }

static ai_inline struct ai*ioput_chain(struct ai*g, word _, uintptr_t off) {
 { struct ai_str *nm = add_name(g, _);                 // a NAMED symbol is (name . mint): print its bare name
   if (nm) { if (!ai_ok(g = ai_push(g, 1, word(nm)))) return g;
             for (uintptr_t l = len(g->sp[0]), i = 0; ai_ok(g) && i < l;)
               g = ioputc(g, txt(g->sp[0])[i++]);
             return ai_pop(g, 1); } }
 if (!ai_ok(g = ai_push(g, 1, _))) return g;
 struct ai_str *n;
 // a one-operand `\` chain (`(\ x)`) is quote -> print as 'x; ≥2 operands is a lambda.
 if ((n = add_name(g, A(g->sp[0]))) && len(n) == 1 && txt(n)[0] == '\\'
     && chainp(B(g->sp[0])) && !chainp(BB(g->sp[0]))) {
  g = ioputc(g, '\'');                          // GC here may relocate sp[0]; read AB after
  g = ioputx(g, AB(g->sp[0]), off); }
 // a `(mono (run datum))` chain is a GLUED MONADIC -> print the source `run`+`datum`
 // (the reverse of opfix's fusion: *5, +(-3), $$0). a reader-built mono always reparses:
 // the reader only fuses where it round-trips (* to a bare datum, +/- to ( ' " @ ~ # `).
 else if ((n = add_name(g, A(g->sp[0]))) && len(n) == 4 && !memcmp(txt(n), "mono", 4)
          && chainp(B(g->sp[0])) && !chainp(BB(g->sp[0]))                                 // (mono X)
          && chainp(AB(g->sp[0])) && chainp(B(AB(g->sp[0]))) && !chainp(BB(AB(g->sp[0])))) {  // X = (run datum)
  g = ioputx(g, A(AB(g->sp[0])), off);          // run -- the operator symbol
  g = ioputx(g, AB(AB(g->sp[0])), off); }       // the operand, glued (re-read fresh after run)
 else for (g = ioputc(g, '(');; g = ioputc(g, ' '), g->sp[0] = B(g->sp[0])) {
  g = ioputx(g, A(g->sp[0]), off);            // off threaded so nested tables are still tracked
  if (!chainp(B(g->sp[0]))) { g = ioputc(g, ')'); break; } }
 return ai_pop(g, 1); }


// Print element i of the array parked at g->sp[0] as a bare number (float ->
// ai_dtoa, integer -> base 10). The element value is read before any ioputc, so
// a GC during printing (string-port growth) that relocates the array is safe;
// callers re-fetch vec(g->sp[0]) each call for the same reason.
static struct ai *ioput_vec_elem(struct ai *g, uintptr_t i) {
 struct ai_vec *v = vec(g->sp[0]);
 if (v->type >= ai_R)
  return ai_dtoa2(g, vec_get_flo(v, i));
 return ioputn(g, vec_get_int(v, i), 10); }

// Print element i of a packed ai_C array (at g->sp[0]) as ~(re im) -- the same
// read-back form as a complex scalar (the `~` reader macro splices into (twin …)).
// re/im are copied to C locals before any ioputc/ai_dtoa2 (which may grow a string
// port and relocate the array).
static struct ai *ioput_carr_elem(struct ai *g, uintptr_t i) {
 ai_flo_t *fp = vec_data(vec(g->sp[0]));
 ai_flo_t re = fp[2*i], im = fp[2*i+1];
 g = ioprintf(g, "~("); g = ai_dtoa2(g, re); g = ioputc(g, ' ');
 g = ai_dtoa2(g, im); return ioputc(g, ')'); }

// Print a rank>=1 array (g->sp[0]) as a constructor expression that reads back to
// the same array -- always a SURFACE form, never the typed `arr` call. A rank-1
// array uses the terse `@(a b …)` sugar (the `@` reader macro splices into
// `(vec a b …)`); rank>=2 uses `(array '(shape) elem …)`. Either way a-type
// re-infers the element type from the printed elements: bare numbers -> z/r,
// ~(re im) -> c, anything else -> o, with symbol/chain elements quoted so eval
// reconstructs them. (An o array of self-evaluating scalars re-reads at its
// natural tier -- the type is inferred, not pinned.) The array may move on a GC
// during printing, so shape/elements are re-fetched from g->sp[0] each step.
static ai_noinline struct ai *ioputx(struct ai *g, intptr_t x, uintptr_t off);

static struct ai *ioput_arr_elem(struct ai *g, uintptr_t i, uintptr_t type, uintptr_t off) {
 if (type == ai_C) return ioput_carr_elem(g, i);
 if (type != ai_O) return ioput_vec_elem(g, i);
 word e = vec_get_obj(vec(g->sp[0]), i);           // kind test only; re-fetched below
 if (nomp(e) || chainp(e)) g = ioputc(g, '\'');          // quote, so eval rebuilds the element
 return ioputx(g, vec_get_obj(vec(g->sp[0]), i), off); }

static struct ai *ioput_arr(struct ai *g, uintptr_t off) {
 struct ai_vec *v = vec(g->sp[0]);
 uintptr_t rank = v->rank, type = v->type, nelem = vec_nelem(v);
 if (rank == 1) {                                      // terse rank-1: @(…), empty -> @()
  g = ioputc(g, '@'); g = ioputc(g, '(');
  for (uintptr_t i = 0; ai_ok(g) && i < nelem; i++) {
   if (i) g = ioputc(g, ' ');
   g = ioput_arr_elem(g, i, type, off); }
  return ai_ok(g) ? ioputc(g, ')') : g; }
 // rank>=2: (array '(shape) elem …) -- @ has no shape spelling yet (a-type of the
 // printed elements re-infers the element type, the loss every surface form accepts).
 g = ioprintf(g, "(array '(");                         // (array '(shape) elem …)
 for (uintptr_t i = 0; ai_ok(g) && i < rank; i++) {
  if (i) g = ioputc(g, ' ');
  g = ioputn(g, vec(g->sp[0])->shape[i], 10); }
 g = ioputc(g, ')');
 for (uintptr_t i = 0; ai_ok(g) && i < nelem; i++) {
  g = ioputc(g, ' '); g = ioput_arr_elem(g, i, type, off); }
 return ai_ok(g) ? ioputc(g, ')') : g; }

// complex -> ~(re im); round-trips by re-evaluation (the `~` reader macro splices
// into (twin re im), and twin is a nif). re/im are read into C locals up front so a
// GC during ai_dtoa2 can't strand the operand.
static ai_inline struct ai*ioput_vec_scalar_complex(struct ai*g) {
 ai_flo_t re = cplx_re(g->sp[0]), im = cplx_im(g->sp[0]);
 g = ioprintf(g, "~(");
 g = ai_dtoa2(g, re);
 g = ioputc(g, ' ');
 g = ai_dtoa2(g, im);
 return ioputc(g, ')'); }

// A vec is always a rank>=1 array now (the scalar gems print via their own
// KFlo/KWide/KCplx printer cases); ioput_arr does the surface @(…)/(array …) form.
static ai_inline struct ai*ioput_vec(struct ai*g, word _, uintptr_t off) {
 if (!ai_ok(g = ai_push(g, 1, _))) return g;
 return ai_pop(ioput_arr(g, off), 1); }

static ai_inline struct ai*ioput_str(struct ai*g, word _) {
 uintptr_t slen = len(_);
 g = ioputc(ai_push(g, 1, _), '"');
 for (uintptr_t i = 0; ai_ok(g) && i < slen; i++) {
  char c = txt(g->sp[0])[i];
  if (c == '\\' || c == '"') g = ioputc(g, '\\');
  else if (c == '\n') g = ioputc(g, '\\'), c = 'n';
  else if (c == '\t') g = ioputc(g, '\\'), c = 't';
  else if (c == '\r') g = ioputc(g, '\\'), c = 'r';
  else if (c == 27)   g = ioputc(g, '\\'), c = 'e';
  else if (c == '\0') g = ioputc(g, '\\'), c = '0';
  else if ((unsigned char) c < 32)
   g = ioputc(ioputc(ioputc(g, '\\'), 'x'), ai_digits[(c >> 4) & 0xf]),
   c = ai_digits[c & 0xf];
  g = ioputc(g, c); }
 return ai_pop(ioputc(g, '"'), 1); }

// A bare mint (KMint) is nameless: () is ZeroPoint (the face of absence), and
// every other point prints `(mint <serial>)` -- the serial in decimal, the mint's
// GC-stable identity AND its `<` order key, so distinct mints wear distinct faces
// that sort like the values. The face is DIAGNOSTIC, not a reparse: a mint cannot
// round-trip (identity is its whole being, no spelling carries it; `(mint N)` re-
// reads to a FRESH point, a new serial -- mint ignores its arg), so the face only
// has to DISTINGUISH, which the serial does. A NAMED symbol is no longer here --
// it is the (name . mint) chain, printed bare by ioput_chain. Notes:
//  - non-reparse is not a regression: `show` is diagnostic, not serialization;
//    nothing rebuilds a mint from its printed form.
//  - stable within a run: the serial rides the GC copy (ini_missing forwards
//    src->code), so (show m) = (show m) for a live mint across any collection.
//  - never asserted as a literal: the serial counts mints stream-wide (non-
//    deterministic across runs/corpus order), so tests assert the SHAPE only
//    (leading "(mint ", stable, distinct-mints-distinct). serial 0 is (), the const,
//    printed () by the guard above, so the absence face never collides with a mint.
static ai_inline struct ai*ioput_sym(struct ai*g, word _) {
 if (_ == ZeroPoint) return ioputcs(g, "()");  // the face of absence
 return ioputcs(ioputn(ioputcs(g, "(mint "), (intptr_t) sym(_)->code, 10), ")"); }   // (mint <serial>): the serial is the mint's identity/order key (diagnostic, not an exact reparse)
// a named point prints its bare name (the spelling), no sigil -- it reparses to itself.
// Park the nom: ioputc may GC and MOVE both the nom and its name, so re-derive each step.
static ai_inline struct ai*ioput_nom(struct ai*g, word _) {
 uintptr_t n = len(nom(_)->name);
 g = ai_push(g, 1, _);
 for (uintptr_t i = 0; ai_ok(g) && i < n; i++)
  g = ioputc(g, (uint8_t) txt(str(nom(g->sp[0])->name))[i]);
 return ai_ok(g) ? ai_pop(g, 1) : g; }


// Maps print as #(k v …), the empty map as #(); both round-trip (the reader's # empty exception).
// A map is mutable and can hold itself, so guard the recursion with the seen
// list. Snapshot k/v into a list first (printing may GC and move the map).
static ai_inline struct ai*ioput_map(struct ai*g, word x, uintptr_t off) {
 if (seen_member(g, off, x)) return ioputcs(g, "<cycle>");
 if (!ai_ok(g = seen_push(g, off, x))) return g;        // sp[0] = seen list head (= x)
 x = A(*seen_slot(g, off));                             // reload x: seen_push may have GC'd
 if (!ai_ok(g = ai_push(g, 1, x))) return seen_pop(g, off), g;   // sp[0] = map
 uintptr_t cap = map_cap(g->sp[0]), n = map_len(g->sp[0]);
 if (!ai_ok(g = ai_have(g, n * 2 * Width(struct ai_chain)))) return seen_pop(ai_pop(g, 1), off), g;
 word *s = map_slots(g->sp[0]);                         // re-fetch after possible GC
 struct ai_chain *p = bump(g, n * 2 * Width(struct ai_chain));
 word list = ZeroPoint;                                 // () terminator (nil-ontology)
 for (uintptr_t i = cap; i;)
  if (s[2 * --i] != map_gap) {
   struct ai_chain *kv = p++;
   ini_chain(kv, s[2 * i], s[2 * i + 1]);                 // (k . v)
   ini_chain(p, (word) kv, list), list = (word) p++; }    // link onto the snapshot
 fs0(g) = list;
 if (!chainp(fs0(g))) g = ioputcs(g, "#()");                    // the empty map prints #(), which reads straight back to it
 else {
  if (ai_ok(g = ioprintf(g, "#("))) for (bool sp = false;;) {
   if (sp) g = ioputc(g, ' ');
   sp = true;
   g = ioputx(g, AA(ai_core_of(g)->sp[0]), off);
   g = ioputc(g, ' '); g = ioputx(g, BA(ai_core_of(g)->sp[0]), off);
   ai_core_of(g)->sp[0] = B(ai_core_of(g)->sp[0]);
   if (!ai_ok(g) || !chainp(g->sp[0])) break; }
  g = ai_ok(g) ? ioputc(g, ')') : g; }
 g = ai_pop(g, 1);
 return seen_pop(g, off), g; }

// A bignum prints in base 10 (with sign). ai_big_dec renders it to a fresh
// string (repeated divide-by-10 of a heap-local copy); we then emit the bytes,
// re-fetching sp[0] each step since ioputc may grow a string port and GC.
static ai_inline struct ai*ioput_big(struct ai*g, word x) {
 if (!ai_ok(g = ai_push(g, 1, x))) return g;
 g = ai_big_dec(g);
 for (uintptr_t i = 0, n = ai_ok(g) ? len(g->sp[0]) : 0; ai_ok(g) && i < n; i++)
  g = ioputc(g, txt(g->sp[0])[i]);
 return ai_pop(g, 1); }

// emit a C string literal byte-for-byte.
static struct ai *ioputcs(struct ai *g, char const *s) {
 for (; ai_ok(g) && *s; s++) g = ioputc(g, *s);
 return g; }

// --- partial-application introspection (mirrors kernel/vm.c lvm_cur/lvm_unc) ---
// A partial-app closure is a thread whose head is lvm_unc (one more arg wanted)
// or [lvm_cur n][lvm_unc …] (more wanted). Each lvm_unc cell holds a captured
// arg at [1] and a link at [2] that points either to the next (older) closure's
// unc cell or, for the last one, two cells into the underlying function's body --
// so the base function value is terminal_link-2 and the captured args are the
// chain of [1] fields, newest first. The chain survives GC (interior pointers are
// relocated), so callers re-walk from the parked, possibly-moved closure.
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

static struct ai *ioput_fn_body(struct ai *g, word x, uintptr_t off);

// the in-pool source \-expr stashed at value[-1] by a compiled lambda, or 0.
// Only an ala/k0s lambda reserves that leading cell, so its value pointer sits one past
// the allocation start. A k0 top-level wrap, a partial-app (lvm_cur/unc), a continuation,
// or an opaque handle puts its value AT the start -- no leading cell -- so reading value[-1]
// there reads the neighbouring object: uninitialised/foreign (flaky to valgrind, and garbage
// that looked like an in-pool chain would spuriously read back as a source). Probe the tag,
// which records the true start, instead of reading value[-1]: ttag sounds only defined thread
// cells. value > start <=> a reserved leading cell exists. (fn_partialp is a cheap fast
// reject so the common curried-closure case skips the tag sound.)
// in_heap: ptr p is a live heap object -- the main pool OR the major pool (a tenured object lives there
// once the generational minor graduates; major_base/major_hp are 0 in a non-gen build, so the second
// disjunct is dead there). The print/introspect paths use this to reject foreign/out-of-pool pointers.
static ai_inline bool in_heap(struct ai *c, word x) {
 return (ptr(x) >= ptr(c) && ptr(x) < ptr(c) + c->len) || (ptr(x) >= c->major_base && ptr(x) < c->major_hp); }
static word fn_src(struct ai *c, union u *k, word x) {
 // x must be a real heap object (main pool above the core base, or the major pool -- the two pools are
 // independent mallocs, so the major pool may sit ABOVE or BELOW the main one; test each range).
 bool xin = (ptr(x) > ptr(c) && ptr(x) < ptr(c) + c->len) || (ptr(x) >= c->major_base && ptr(x) < c->major_hp);
 if (!xin || fn_partialp(k)) return 0;
 if (k == tag_head(ttag(c, k))) return 0;       // value at allocation start: no leading src cell
 word s = k[-1].x;
 return lamp(s) && in_heap(c, s) && chainp(s) ? s : 0; }

// --- de Bruijn canonical printing of a lambda's source ---------------------
// A \-bound variable prints as $<level> where the level (de Bruijn LEVEL:
// outermost binder 0, counting inward, left-to-right within a \-group) is
// rendered in a-z shortlex ($a,$b,…,$z,$aa,…), so a-equivalent lambdas print
// identically -- (\ x x) and (\ y y) both -> (\ $a $a) -- and inspect agrees
// with = (same binder convention as salpha). Free/global/:-bound vars keep
// their names; quoted data (one-operand \) is shared verbatim, never renamed.
// lam_canon rebuilds the source with bound syms replaced: it pre-interns the
// d<lvl> names and pre-reserves the cell count, so the rebuild itself allocates
// nothing and cannot GC -- pointers into the parked source stay stable. The
// names are interned plain symbols (d0,d1,…) so the printed form round-trips.
struct lam_bv { word sym; uintptr_t lev; struct lam_bv *up; };  // a \-binder in scope
static ai_inline bool lam_head(struct ai *g, word a) {        // is a the symbol \ ?
 struct ai_str *nm;                                          // a named sym (name . mint); add_name is 0 for a bare mint / the core
 return (nm = add_name(g, a)) && len(nm) == 1 && txt(nm)[0] == '\\'; }
static ai_inline bool lam_isp(struct ai *g, word x) {         // (\ b.. body): >=2 operands
 return chainp(x) && lam_head(g, A(x)) && chainp(B(x)) && chainp(BB(x)); }
static ai_inline bool lam_quotep(struct ai *g, word x) {       // (\ datum): exactly 1 operand
 return chainp(x) && lam_head(g, A(x)) && chainp(B(x)) && !chainp(BB(x)); }
static uintptr_t lam_cells(struct ai *g, word x) {             // chains the rebuild will bump
 return !formp(x) || lam_quotep(g, x) ? 0 : 1 + lam_cells(g, A(x)) + lam_cells(g, B(x)); }  // a symbol is an atom (formp), not a compound to descend
static uintptr_t lam_depth(struct ai *g, word x, uintptr_t d) {  // max binder level + 1 (= # d-syms)
 if (!formp(x) || lam_quotep(g, x)) return d;
 if (lam_isp(g, x)) {
  word o = B(x); uintptr_t nb = 0;
  while (chainp(B(o))) nb++, o = B(o);                          // every operand but the last = a binder
  uintptr_t here = d + nb, body = lam_depth(g, A(o), here);
  return here > body ? here : body; }
 uintptr_t a = lam_depth(g, A(x), d), b = lam_depth(g, B(x), d);
 return a > b ? a : b; }
static word lam_build_ops(struct ai *g, word o, struct lam_bv *sc, uintptr_t d, uintptr_t D);
static word lam_build(struct ai *g, word x, struct lam_bv *sc, uintptr_t d, uintptr_t D) {
 if (!formp(x)) {                                              // atom (incl. a symbol): a bound sym -> d<lev>, else as-is
  if (nomp(x)) for (struct lam_bv *p = sc; p; p = p->up) if (p->sym == x) return g->sp[D - 1 - p->lev];
  return x; }
 if (lam_quotep(g, x)) return x;                              // quoted data: share, do not descend
 word a, b;
 if (lam_isp(g, x)) a = A(x), b = lam_build_ops(g, B(x), sc, d, D);  // share \, rename the operand spine
 else a = lam_build(g, A(x), sc, d, D), b = lam_build(g, B(x), sc, d, D);
 struct ai_chain *p = bump(g, Width(struct ai_chain));
 return ini_chain(p, a, b), (word) p; }
static word lam_build_ops(struct ai *g, word o, struct lam_bv *sc, uintptr_t d, uintptr_t D) {
 word car, rest;
 if (!chainp(B(o))) car = lam_build(g, A(o), sc, d, D), rest = nil;  // last operand = the body
 else { struct lam_bv fr = { A(o), d, sc };                        // a binder, level d, in scope for the rest
        car = g->sp[D - 1 - d], rest = lam_build_ops(g, B(o), &fr, d + 1, D); }
 struct ai_chain *p = bump(g, Width(struct ai_chain));
 return ini_chain(p, car, rest), (word) p; }
// sp[0] = a lambda's source \-expr; replace it with the de Bruijn-renamed copy.
static struct ai *lam_canon(struct ai *g) {
 word s = g->sp[0];
 if (!lam_isp(g, s)) return g;                               // not a lambda -> leave as-is
 uintptr_t P = lam_cells(g, s), D = lam_depth(g, s, 0);
 for (uintptr_t i = 0; i < D; i++) {                          // push d0,d1,… (de Bruijn level); src parked below
  char b[24], *e = b + sizeof b; uintptr_t n = i;            // interned d<lvl> -> reads back as the same sym
  *--e = 0;
  do { *--e = '0' + n % 10; } while (n /= 10);
  *--e = 'd';
  if (!ai_ok(g = intern(ai_strof(g, e)))) return g; }
 if (!ai_ok(g = ai_have(g, P * Width(struct ai_chain)))) return g;  // reserve cells: the last possible GC
 word r = lam_build(g, g->sp[D], 0, 0, D);                     // alloc-free, GC-free; g->sp stays put
 return g->sp[D] = r, g->sp += D, g; }

// Print a function value as a bare, re-parsable form that reconstructs under eval
// (like @(…)/~(…)/#(…)): (base arg…) for a partial application / closure, the bare
// name for a builtin (\+ for an operator builtin), (\ …) for a compiled lambda (its
// stored source). An opaque thread (continuation, top-level wrap) has no constructor
// form, so it prints as the opaque, re-parsable token \<addr>.
static struct ai *ioput_fn(struct ai *g, word x, uintptr_t off) {
 union u *k = cell(x);
 bool reprp = fn_partialp(k) || ai_nif_name(x) || fn_src(ai_core_of(g), k, x);
 return reprp ? ioput_fn_body(g, x, off) : ioprintf(g, "\\%z", x); }

// Render a function as a bare constructor expression. Detection
// order matters: a bare multi-arg lambda and a partial-app both have a lvm_cur
// head, and a nif's value[-1] is undefined static data. The partial-app base
// recurses here (not ioput_fn) so it renders inline.
static struct ai *ioput_fn_body(struct ai *g, word x, uintptr_t off) {
 struct ai *c = ai_core_of(g);
 union u *k = cell(x);
 if (fn_partialp(k)) {                              // (base arg…)
  if (!ai_ok(g = ai_push(g, 1, x))) return g;         // park: GC relocates the closure
  int na; fn_base(cell(g->sp[0]), &na);
  g = ioputc(g, '(');
  { union u *bk = cell(g->sp[0]); int n2;           // base re-derived after each ioputc
    g = ioput_fn_body(g, (word) fn_base(bk, &n2), off); }
  for (int i = 0; ai_ok(g) && i < na; i++) {
   g = ioputc(g, ' ');                              // separate stmt: re-read arg after GC
   g = ioputx(g, fn_arg(cell(g->sp[0]), i, na), off); }
  return ai_pop(ai_ok(g) ? ioputc(g, ')') : g, 1); }
 char const *nm = ai_nif_name(x);                    // builtin -> name
 if (nm) return ioputcs(g, nm);
 word s = fn_src(c, k, x);                          // compiled lambda -> source \-expr
 if (!s) return ioprintf(g, "\\%z", x);
 if (!ai_ok(g = ai_push(g, 1, s))) return g;          // park source across lam_canon's allocs
 g = lam_canon(g);                                   // sp[0] := de Bruijn-renamed copy
 if (ai_ok(g)) g = ioputx(g, g->sp[0], off);
 return ai_ok(g) ? ai_pop(g, 1) : g; }

// a coin shows as `(name payload)` -- reparsable when the die's NAME is the symbol
// of a bound constructor (the common case: `(z5 3)`). Nameless -> `(coin payload)`.
static struct ai *ioput_coin(struct ai *g, word x, uintptr_t off) {
 word nm = die_get(g, coin_die(x), DIE_NAME);
 g = ioputc(g, '(');
 g = ai_nilp(g, nm) ? ioputcs(g, "coin") : ioputx(g, nm, off);
 g = ioputc(g, ' ');
 g = ioputx(g, coin_load(x), off);
 return ioputc(g, ')'); }
static ai_noinline struct ai *ioputx(struct ai *g, intptr_t x, uintptr_t off) {
 if (charmp(x)) return ioprintf(g, "%d", getcharm(x));
 if (coinp(x)) return ioput_coin(g, x, off);
 if (!datp(x)) return tabp(x) ? ioput_map(g, x, off) : ioput_fn(g, x, off);
 // Maps are the only mutable/self-referential value, and ioput_map guards its
 // own recursion (the seen list); the data kinds below are acyclic.
 switch (typ(x)) {
   default: __builtin_trap();
   case KChain:  return ioput_chain(g, x, off);
   case KVec:  return ioput_vec(g, x, off);
   case KFlo:  return ai_dtoa2(g, flo_get(x));
   case KWide: return ioputn(g, box_get(x), 10);
   case KCplx: if (!ai_ok(g = ai_push(g, 1, x))) return g;  // ~(re im); the helper reads sp[0]
               return ai_pop(ioput_vec_scalar_complex(g), 1);
   case KMint:  return ioput_sym(g, x);
   case KNom:   return ioput_nom(g, x);
   case KString: return ioput_str(g, x);
   case KBig:  return ioput_big(g, x); } }

// Establish a fresh seen-list slot at the bottom of the print region, print, then
// restore the original stack height (discarding the slot and the whole list).
static ai_inline struct ai *gfputx(struct ai *g, struct ai_io *o, intptr_t x) {
 struct ai *c = ai_core_of(g);
 c->io = o;
 uintptr_t base = topof(c) - c->sp;                 // original height (GC-invariant)
 if (!ai_ok(g = ai_push(g, 1, nil))) return g;        // the seen-list slot
 c = ai_core_of(g);
 g = ioputx(g, x, topof(c) - c->sp);                // offset of the slot from the top
 c = ai_core_of(g);
 return c->sp = topof(c) - base, g; }               // restore original stack height

// --- the SHORTEST-ROUNDTRIP float printer (exact Steele & White) -----------
// a gem prints the fewest digits that read back to exactly its bits. the
// machinery is EXACT decimal integers on the stack: a float is m * 2^e with a
// small integer m, so 4m and the two binary-rounding midpoints 4m+-2 (4m-1 at
// a binade floor, where the gap below halves) at 2^(e-2) are integers whose
// decimal expansions are finite -- digit arrays under x2/x5 carry walks, no
// libm, no strtod, no allocation, the same digits on every target. the answer
// is the least N such that rounding v's digits to N lands strictly inside
// (lo, hi) -- interval closed when the mantissa is even, since round-to-
// nearest-even reads a midpoint back to v. the old printer's 15-digit chop
// conflated neighbors (0.1 + 0.2 printed "0.3") and drifted in its divide-
// down loop (1.5e308 printed "1.4999..."); exactness retires both.

// digit arrays: base-10, LSB first, no leading zeros; value = digits * 10^scale
enum { dgmax = sizeof(ai_flo_t) == 4 ? 128 : 800 };   // worst expansion: every x5 step adds < 1 digit
static void dg_mul2(unsigned char *d, int *n) {
 int c = 0;
 for (int i = 0; i < *n; i++) { int t = d[i] * 2 + c; d[i] = t % 10; c = t / 10; }
 if (c) d[(*n)++] = c; }
static void dg_mul5(unsigned char *d, int *n) {
 int c = 0;
 for (int i = 0; i < *n; i++) { int t = d[i] * 5 + c; d[i] = t % 10; c = t / 10; }
 if (c) d[(*n)++] = c; }
static int dg_cmp(unsigned char const *a, int na, unsigned char const *b, int nb) {
 if (na != nb) return na < nb ? -1 : 1;
 for (int i = na - 1; i >= 0; i--) if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
 return 0; }
// expand m * 2^e2 into d; every caller shares one e2, so the scales align and
// the arrays compare directly as integers
static void dg_expand(unsigned char *d, int *n, uintptr_t m, int e2) {
 *n = 0;
 if (!m) d[(*n)++] = 0;
 while (m) d[(*n)++] = m % 10, m /= 10;
 for (; e2 > 0; e2--) dg_mul2(d, n);
 for (; e2 < 0; e2++) dg_mul5(d, n); }

static struct ai* ai_dtoa2(struct ai*g, ai_flo_t v) {
 if (v != v) return ioputs(g, "ieee-nan");
 // ⚠ the SIGN BIT, not v < 0 -- because -0.0 < 0 is FALSE, so a comparison
 // drops the sign of a zero and this printed "0.0" for it. the reader then read
 // that back as +0.0, so the roundtrip law failed on exactly one value: love
 // has a negative zero ((1.0 / -0.0) is -ieee-inf), the printer just could not
 // say it. -0.0 is a legitimate answer of the arithmetic, not a display quirk.
 if (((ai_flo_pun){ .d = v }).u >> (8 * sizeof(ai_flo_t) - 1)) g = ioputc(g, '-'), v = -v;
 if (v > dtoa_inf) return ioputs(g, "ieee-inf");
 if (v == 0) return ioputs(g, "0.0");
 int const mbits = sizeof(ai_flo_t) == 4 ? 23 : 52;
 int const ebits = sizeof(ai_flo_t) == 4 ? 8 : 11;
 int const bias  = sizeof(ai_flo_t) == 4 ? 127 : 1023;
 uintptr_t bits = ((ai_flo_pun){ .d = v }).u;
 uintptr_t mant = bits & (((uintptr_t) 1 << mbits) - 1);
 int be = (int) ((bits >> mbits) & (((uintptr_t) 1 << ebits) - 1));
 uintptr_t m = be ? mant | ((uintptr_t) 1 << mbits) : mant;   // denormal: no implicit bit
 int e2 = (be ? be : 1) - bias - mbits;
 // v, lo, hi at one shared exponent e2-2: lo halves its distance at a binade
 // floor (mantissa field 0, and a normal above the lowest -- there the gap
 // below already equals the denormal spacing)
 bool floorb = be > 1 && mant == 0;
 unsigned char dv[dgmax], dl[dgmax], dh[dgmax], dc[dgmax];
 int nv, nl, nh, nc = 0;
 dg_expand(dv, &nv, 4 * m, e2 - 2);
 dg_expand(dl, &nl, 4 * m - (floorb ? 1 : 2), e2 - 2);
 dg_expand(dh, &nh, 4 * m + 2, e2 - 2);
 bool even = !(m & 1);                       // round-to-nearest-even: midpoints read back to v
 // the least N whose rounded form sits inside the interval: at each N try the
 // truncation and the bump (the only N-digit values adjacent to v), nearer one
 // first; at N = all-of-v's-digits the truncation IS v, strictly inside, so
 // this terminates
 for (int N = 1; N <= nv; N++) {
  int k = nv - N;                            // low digits dropped
  int first = k > 0 && dv[k - 1] >= 5;       // the nearer candidate leads (a display nicety)
  for (int t = 0; t < 2; t++) {
   int up = t ? !first : first;
   for (int i = 0; i < nv; i++) dc[i] = i < k ? 0 : dv[i];
   nc = nv;
   if (up) {                                 // bump: + 10^k, with carry
    int c = 1;
    for (int i = k; c && i < nc; i++) { int s = dc[i] + c; dc[i] = s % 10; c = s / 10; }
    if (c) dc[nc++] = 1; }
   int cl = dg_cmp(dc, nc, dl, nl), ch = dg_cmp(dc, nc, dh, nh);
   if ((cl > 0 || (even && cl == 0)) && (ch < 0 || (even && ch == 0))) goto chosen; } }
chosen:;
 // dc holds the winner; strip its trailing zeros into significant digits
 int lead = nc;                              // total digits, before stripping
 int drop = 0;
 while (drop < nc - 1 && dc[drop] == 0) drop++;
 int sig = nc - drop;
 // decimal exponent: value = dc * 10^scale, scale = the x5 steps taken
 int scale = e2 - 2 < 0 ? e2 - 2 : 0;
 int dxp = lead + scale;                     // value = 0.<digits> * 10^dxp
 char ob[48]; int on = 0;                    // sig <= 17 and |dxp| <= 350: fits
 bool sci = v >= dtoa_sci_hi || v < dtoa_sci_lo;
 if (sci) {
  ob[on++] = '0' + dc[nc - 1];
  if (sig > 1) { ob[on++] = '.';
   for (int i = nc - 2; i >= drop; i--) ob[on++] = '0' + dc[i]; }
  ob[on++] = 'e';
  int e = dxp - 1;
  if (e < 0) ob[on++] = '-', e = -e;
  char eb[8]; int en = 0;
  if (!e) eb[en++] = '0';
  while (e) eb[en++] = '0' + e % 10, e /= 10;
  while (en) ob[on++] = eb[--en]; }
 else if (dxp > 0) {                         // 1 <= v < 1e16: point inside or after
  for (int i = 0; i < dxp; i++) ob[on++] = i < sig ? '0' + dc[nc - 1 - i] : '0';
  ob[on++] = '.';
  if (sig > dxp) for (int i = dxp; i < sig; i++) ob[on++] = '0' + dc[nc - 1 - i];
  else ob[on++] = '0'; }                     // integral: force "X.0", never a fixnum face
 else {                                      // 1e-4 <= v < 1: leading "0."
  ob[on++] = '0'; ob[on++] = '.';
  for (int i = 0; i < -dxp; i++) ob[on++] = '0';
  for (int i = 0; i < sig; i++) ob[on++] = '0' + dc[nc - 1 - i]; }
 for (int i = 0; i < on; i++) g = ioputc(g, ob[i]);
 return g; }

// (fgetc port) — like (getc _) but on an explicit port. Cooperative wait
// uses the port's own fd. A non-port reads as an already-empty stream: EOF
// (-1), not the echoed argument, so a (fgetc ...)-until-(-1) loop over a
// misused or absent port (e.g. `open` on a host with no filesystem) is
// bounded instead of spinning forever on a value that never equals -1.
lvm(lvm_fgetc) {
 if (iop(Sp[0])) {
  struct ai_io *i = (struct ai_io*) Sp[0];
  struct ai_bio *bb = bio_of(g, i);
  if (bio_wpending(bb)) {                 // our unsent ask goes out before we wait for the answer
   g->io = i;
   Pack(g);
   if (!ai_ok(g = io_wdrain(g, i))) return ghelp(g);
   Unpack(g); }
  // ⚠ NO READINESS PRE-GUARD. There was one -- pushback, then buffered run, then
  // ai_ready -- and it was the third copy of a test zgetc already makes by reading
  // those same three things in that same order. The device ANSWERS now: a byte, an
  // end, or IO_WOULDBLOCK. Asking first was also a lie on the kernel, where
  // k_sources[1].ready is NULL: reading an output fd parked forever on a wait
  // nothing could satisfy, where it now reads the END the dispatcher promises.
  // (cue? and await still ask -- they have no read to answer them.)
  Pack(g);
  g->io = i;
  if (!ai_ok(g = zgetc(g))) return ghelp(g);
  Unpack(g);
  if (g->b == IO_WOULDBLOCK) {          // the refill raced and lost -- park, don't spin
   g->next_wait_fd = getcharm(((struct ai_io*) Sp[0])->fd);   // re-read: the gc may have moved it
   return Ap(lvm_yield_sw, g); }
  Sp[0] = putcharm(g->b); }
 else Sp[0] = putcharm(EOF);
 return Ip++, Continue(); }

// (await port) — cooperatively PARK the running task until `port`'s fd is readable,
// then return the port (so it chains into a read). lvm_fgetc buried this park inside
// getc; pulled out, it serves the fds you CAN'T drain a byte at a time -- a signalfd,
// a timerfd -- where a kind-specific nif reads the record AFTER the wait. The
// scheduler folds every parked task's fd + the nearest timer into one ai_wait_fds, so
// two tasks awaiting {signalfd, clock} ARE the {nic, clock} merge. A non-port (or a
// synthetic fd<0) is vacuously ready -- returns the arg. Ip is unadvanced on the park,
// so the task re-runs this ap on reschedule and re-checks readiness.
lvm(lvm_await) {
 if (iop(Sp[0])) {
  intptr_t fd = getcharm(((struct ai_io*) Sp[0])->fd);
  if (fd >= 0 && !ai_ready(fd)) {
   g->next_wait_fd = fd;
   return Ap(lvm_yield_sw, g); } }
 return Ip++, Continue(); }

// (fungetc port byte) — push back one byte, return the byte.
lvm(lvm_fungetc) {
 if (iop(Sp[0])) {
  struct ai_io *i = (struct ai_io*) Sp[0];
  Pack(g);
  g->io = i;
  if (!ai_ok(g = zungetc(g, getcharm(g->sp[1])))) return ghelp(g);
  Unpack(g); }
 return Sp++, Ip++, Continue(); }

// Finalizer for heap stream ports: extract the fd and ask the frontend to
// close it. Runs inside GC (run_finalizers); fz->p still points at the
// from-space port so its fields are readable. Skip if fd < 0 — that means
// either the port was already closed explicitly (fd mutated to a synth
// sentinel) or the caller wrapped a non-OS fd.
static void io_close(void *p) {
 struct ai_bio *b = p;                         // every finalized port is a bio (ai_io_alloc made it)
 intptr_t fd = getcharm(b->io.fd);
 if (fd < 0) return;
 if (b->wbuf && !(b->wbuf & 1) && getcharm(b->wlen) > 0)   // unflushed bytes ride out raw --
  ai_fd_drain((int) fd, txt((struct ai_str*) b->wbuf), (uintptr_t) getcharm(b->wlen));   // from-space is readable here
 ai_fd_close(fd); }

// Heap-allocate a stream port for the given OS fd. Pushes the port pointer
// on Sp[0] and registers io_close as its finalizer. The fd >= 0 path of
// the dispatcher routes through ai_fd_port_vt, so the host's read/write
// methods see this port like any other.
struct ai *ai_io_alloc(struct ai *g, int fd) {
 uintptr_t const n = Width(struct ai_bio);     // a heap fd port carries the buffer lanes (love.h)
 if (ai_ok(g = ai_have(g, n + Width(struct ai_tag) + Width(struct ai_fz) + 1))) {
  union u *k = bump(g, n + Width(struct ai_tag));
  struct ai_bio *io = (struct ai_bio*) k;
  io->io.ap = lvm_port_io;
  io->io.fd = putcharm(fd);
  io->io.ungetc_buf = putcharm(EOF);
  io->rbuf = io->wbuf = 0;                     // never dressed (io_refill/zputc dress lazily)
  io->rpos = io->rlen = io->wlen = putcharm(0);
  *--g->sp = (word) tagthread(k, n);            // stack slot reserved by the +1 in have()
  struct ai_fz *z = bump(g, Width(struct ai_fz));
  z->p = k, z->fn = io_close, z->next = g->fz, g->fz = z; }
 return g; }

static struct ai *grbufg(struct ai *g, uintptr_t len);

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

// ..and an octal integer iff it is [+-]?0[0-7]+, the leading zero being the whole
// prefix. "08" matches none of the three and keeps the strtod -> intern path it
// has always had.
//
// ALL THREE READ AT FULL PRECISION, through ai_big_read_dec / _hex / _oct, and
// that is the law: an integer literal is a fixnum / box / bignum by its VALUE.
// hex and octal used to go by strtol, which is where the numeric tower had its
// one hole -- a literal at or above 2^63 came back as whatever that build's
// strtol did with an overflow, so the SAME SOURCE read as three different
// numbers (nolibc wrapped to the negative twin, glibc saturated, and wasm's
// 32-bit long did neither). port/inle/klink.l spells kernel addresses exactly
// this way, and it survived on the wrap alone.
static ai_inline bool is_oct_int(char const *s, uintptr_t n) {
 uintptr_t i = (n && (s[0] == '-' || s[0] == '+')) ? 1 : 0;
 if (n - i < 2 || s[i] != '0') return false;
 for (i += 1; i < n; i++) if (s[i] < '0' || s[i] > '7') return false;
 return true; }

static ai_inline struct ai *ioread1sym(struct ai*g, uintptr_t d, int c), *ioread1str(struct ai*g, uintptr_t d);

static struct ai *grbufg(struct ai *g, uintptr_t len) {
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

// (flo s) — parse a l string as a decimal float. Returns a rank-0
// f64 box if the entire string parses, else nil. Used by the l-side
// reader in repl.l to match the C reader's strtod → intern cascade on
// float-shaped tokens.
lvm(lvm_real) {
 word x = Sp[0];
 double d = strtod_wrap(g, x);
 if (d != d) return Sp[0] = nil, Ip += 1, Continue();
 Have(flo_req);
 Sp[0] = mk_flo(&Hp, (ai_flo_t) d);
 return Ip++, Continue(); }

// (string x): a charlist -> the string of those bytes; a named symbol -> its
// name string; a fixnum -> the one-byte string of its low byte. Identity on any
// other type (strings, anonymous syms, nil, ...).
lvm(lvm_string) {
 word x = Sp[0];
 if (x == nil) return Ip++, Continue();             // nil is the empty string (0)
 if (charmp(x)) {                                     // fixnum -> one-byte string
  uintptr_t req = str_type_width + b2w(1);
  Have(req);
  struct ai_str *s = (void*) Hp;
  Hp += req;
  ini_str(s, 1);
  txt(s)[0] = (char) getcharm(x);
  return Sp[0] = word(s), Ip++, Continue(); }
 if (nomp(x)) {                                      // a named symbol (name . mint) -> its name string; a bare point -> identity
  struct ai_str *nm = add_name(g, x);
  if (nm) Sp[0] = word(nm);                          // the car is the name; a bare mint / the core is nameless
  return Ip++, Continue(); }
 if (chainp(x)) {                                      // charlist -> string
  uintptr_t n = llen(x), req = str_type_width + b2w(n);
  Have(req);
  struct ai_str *s = (void*) Hp;
  Hp += req;
  ini_str(s, n);
  for (uintptr_t i = 0; n--; x = B(x)) txt(s)[i++] = (char) getcharm(A(x));
  return Sp[0] = word(s), Ip++, Continue(); }
 if (bufp(x)) {                                       // a cask/buf -> a fresh string copy of its bytes
  uintptr_t n = len(buf_str(x)), req = str_type_width + b2w(n);
  Have(req);
  struct ai_str *src = buf_str(Sp[0]);                // re-read post-Have (a GC may have moved the buf)
  struct ai_str *s = (void*) Hp;
  Hp += req;
  ini_str(s, n);
  memcpy(txt(s), txt(src), n);
  return Sp[0] = word(s), Ip++, Continue(); }
 return Ip++, Continue(); }                          // any other type: identity

////
/// " the parser "
//
// p0's input is a CHARLIST and its position IS the list, so the cursor is ONE
// love value on the l stack -- named by its DEPTH, never by an address: a
// collection moves the stack, but nothing moves a value's depth in it. Every
// leaf both reads and updates it, and p0reads piles datums ABOVE it, which is
// why the depth and not sp[0] is the name.
//
// ⚠ A LOOKAHEAD NEEDS NO PUSHBACK when the input is a list -- `unget` is simply
// NOT ADVANCING, and the char after the next is one more B away. That is what
// took the ungetc buffer out of every reader here but the port's own.
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
  for (txt((struct ai_str*) g->sp[0])[0] = c; ai_ok(g); g = grbufg(g, lim), lim *= 2)
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
      // An integer in any of the three bases reads at full precision (fixnum /
      // box / bignum); everything left is a float or a symbol. THE THREE
      // PREDICATES ARE EXHAUSTIVE over what a base-0 strtol would have accepted
      // whole, which is why the reader no longer calls it: a token that is not
      // one of them (0x with no digits, 08, 1e5, abc) is one strtol would have
      // left mid-way anyway. checked over the corpus and a 44-token sweep.
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
      // no ieee-nan literal: NaN collapses to 0 (flo_put), so there is no NaN
      // value to name -- "ieee-nan" reads as an honest symbol, free for binding.
      else {
       char c0 = *tx == '+' || *tx == '-' ? tx[1] : *tx;
       if (!(c0 >= '0' && c0 <= '9') && c0 != '.') return intern(g);
       dv = am_strtod(tx, &e);
       if (e == tx || *e != 0) return intern(g); }
      if (ai_ok(g = ai_have(g, flo_req)))
       g->sp[0] = mk_flo(&g->hp, dv);
      return g; } } }
 return g; }

////
/// " p0 -- the bootstrap reader "  (doc/io.md rung 5)
//
// The PURE LISP SUBSET and nothing else: delimiters, `;` and `#!` comments,
// `"…"` with escapes, atoms, `'` quote. None of the sigil surface -- no operator
// run, no mono wrap, no ` # @ ~ constructor wraps, no comma, no [ ] { } synonyms.
// Those are p1's, the reader written in love that will ride on top of this one.
// prel.l is held to this subset (rung 4) so p0 can read it; ev.l is not.
//
// ported forward from 2efbaa51^:g/g.c:1286-1400, the last C reader before the
// sigil surface arrived -- and it answers the GC question by DEMONSTRATION.
// CONTROL FLOW ON THE C STACK, VALUES ON g->sp: p0reads lets datums pile up on
// the l stack and folds them with gxr at the close, and the input cursor is one
// more slot down there, so no love value ever sits in a C local across an
// allocation and p0 needs no heap frame stack of its own.
//
// Its leaf lexers are p0skip (comments), ioread1str (escapes) and ioread1sym
// (the atom lane -- three integer bases at full precision, the float fallback,
// the ieee-inf literals). rung 2 made that number tower the tree's one answer
// for what a literal means, and a second copy in C is exactly the divergence it
// deleted -- p1 reaches this one through sound0's differential, not by forking.
//
// ⚠ p0 is a READER OF A SUBSET, not a validator. A char outside the subset
// reaches the atom lane and comes back a plain symbol -- `#(a b)` reads as `#`
// then `(a b)` where the structural reader gives `(hash a b)`. The enforcement
// is the DIFFERENTIAL (test/host/rdiff.l), where that divergence is loud, not a
// check here that would have to be kept in step with a grammar it does not own.
//
// ⚠ IT MUST READ WHAT lcat PRINTS, not only what the tree types. the boot
// embeds the lcat'd headers -- canonical `show` output, MINIFIED against the
// STRUCTURAL reader's grammar (tools/lcat.l drops a space wherever `sound`
// parses the same with and without it). so p0's subset has to agree with
// `sound` on every join lcat is willing to make, and the two lexers meeting
// only in the leaves is what keeps that cheap. `\` is a case down in p0read1
// for the grammar's own sake: it leads an operator run that never fuses, so
// `(\x x)` is a lambda wherever it is typed, not a name.

// ⚠ NESTING RIDES THE C STACK, which is the trade that buys p0 its size -- so p0
// is DEPTH-BOUNDED where the structural reader, stackless by construction, is
// not. measured on the host: 100k deep reads, 200k faults. the deepest form in
// the tree is 38 (test/uupatch.l) and p0's inputs are the bootstrap files the
// tree owns, so the bound sits three orders of magnitude clear of its use;
// test/host/rdiff.l pins it at 20000.
static struct ai *p0read1(struct ai *g, uintptr_t d);

// a list: read datums until `)`, then fold n of them off the stack. the tail is
// ZeroPoint, NOT nil -- reader lists are ()-terminated (the nil-ontology), and
// nil is the fixnum 0, which the printer shows the same way.
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

// (sound0 text): sound's bootstrap twin -- the same protocol over p0's grammar,
// so the differential (test/host/rdiff.l) compares the two readers like with
// like. answers the datum consed onto what is left, () at a clean end, or the
// symbol `torn` where the text ran out inside a shape.
//
// THE TEXT SLOT IS THE CURSOR: sp[0] comes in as the charlist to read and goes
// out as the answer, and what is left of it in between IS the residue -- so
// there is no port to dress, nothing to recover, and the reader's whole state is
// one traced stack slot. ⚠ it stays in an ai_noinline helper for the OTHER
// reason: a frame in the lvm_ body would force the tail Continue() into a ret
// and grow the stack every step (`make vmret`).
ai_noinline static struct ai *p0text(struct ai *g) {
 uintptr_t const d = topof(g) - g->sp;                // the cursor's depth, and the rollback point
 g = p0read1(g, d);
 if (ai_ok(g)) return gxl(g);                         // (datum . residue), over the text slot
 enum ai_status const st = ai_code_of(g);             // no datum: which nothing?
 if (st != ai_status_eof && st != ai_status_more) return g;   // a real failure (oom) propagates
 // ⚠ THE ROLLBACK IS NOT OPTIONAL. p0reads lets a list's datums pile up on the
 // l stack and folds them only at the close, so a TORN parse leaves that pile
 // behind -- and the text slot is no longer sp[0]. drop back to the depth we
 // came in at, which is exactly what the old parse nif's transaction did.
 g = ai_core_of(g), g->sp = topof(g) - d;
 if (st == ai_status_eof) return g->sp[0] = ZeroPoint, g;     // a clean end, over the text slot
 if (!ai_ok(g = intern(ai_strof(g, "torn")))) return g;
 return g->sp[1] = g->sp[0], g->sp++, g; }

lvm(lvm_sound0) {
 Pack(g);
 if (!ai_ok(g = p0text(g))) return ghelp(g);
 return Unpack(g), Ip++, Continue(); }

////
/// " the boot stitch "  (doc/io.md rung 6b)
//
// The egg's argument used to be ONE READ over one juxtaposed string --
// "(" egg.h " '(" prel.h ev.h "))" -- so every frontend needed the whole C
// reader before any love existed. It is STITCHED now: p0 reads the halves it
// owns (p1.l, prel.l, egg.l, each held to the pure lisp subset) and P1, THE
// READER IN LOVE, reads ev.l. The egg expression is unchanged and its internals
// never learn anything happened -- (sit (sit ev 0 egg) 0 egg) still folds over
// the COMPLETE corpus as one unevaluated list, which is the thing that makes
// prel ev1-compiled rather than left as c0 output.
//
// The circularity resolves by reading p1.l TWICE. The first read is evaluated
// on the spot by c0, purely so p1 is callable for ev.l; the second puts p1's
// forms in the corpus so the double sit recompiles them, ev1-compiled like
// everything else. ⚠ AT THE HEAD, never the tail: `sit` answers the LAST form's
// value and that value is what gets pinned as `ev`, so p1 at the tail would
// install the reader as the evaluator.

// p0's cursor is a charlist and the boot's text is a C string, so the boot
// CONSES it: one Have for the whole run, then a backward walk that needs no root
// (nothing allocates once the space is reserved). the peak is one text at a time
// -- the p0onto calls are sequential -- and the largest is prel.h, ~18KB, so
// ~424KB of transient cells on 64-bit against the 8MB the smallest baker has.
static struct ai *p0chars(struct ai *g, char const *s) {
 uintptr_t n = 0;
 while (s[n]) n++;
 if (!ai_ok(g = ai_push(g, 1, ZeroPoint))) return g;               // the cursor's slot first,
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
 if (!ai_ok(g = ai_push(g, 1, nil))) return g;       // reserve first, THEN copy the
 g->sp[0] = g->sp[n + 2];                            // tail up: a push can gc and move it
 for (; ai_ok(g) && n--; g = gxr(g));                //   (+2: the datums sit over the cursor)
 return ai_ok(g) ? (g->sp[2] = g->sp[0], g->sp += 2, g) : g; }

// ev's half, read by the reader in love: an ordinary call of hook 0 on the text,
// answering the file's forms. p1's own door takes the WHOLE TEXT because the boot
// has the whole text and there is nothing to resume.
static struct ai *p1text(struct ai *g, char const *s) {
 if (!ai_ok(g = ai_strof(g, s))) return g;
 if (!ai_ok(g = gxr(push0(g)))) return g;            // ("<text>")
 if (!ai_ok(g = ai_push(g, 1, nil))) return g;       // reserve FIRST, then read the slot:
 g->sp[0] = ai_core_of(g)->hot_read;                 //   a push can gc, and the gc is what
 if (!ai_ok(g = ai_eval(gxl(g)))) return g;          //   moves hot_read. (<reader> "<text>")
 // p1 answers `torn` -- a NAMED symbol -- for a shape that did not finish. the
 // egg would fold over it as an EMPTY corpus and pin `ev` to 0, silently, so the
 // one shape that is not a corpus is refused here. ⚠ the test is chainp AND NOT
 // nomp, love's own `two?`: a named symbol is a (name . mint) chain, so a bare
 // chainp calls `torn` a corpus.
 word r = g->sp[0];
 return (chainp(r) && !nomp(r)) || r == ZeroPoint ? g
      : encode(ai_core_of(g), ai_status_more); }

// a text -> the list of its forms, pushed. P1 READS IT once p1.l has been
// evaluated, p0 until then -- the boot stitch below makes p1 live before it
// reads anything else, and love0's build-tool lane evals p1.l up front for the
// same reason, so the p0 lane serves only the forms that bring p1 into being.
// the sealed slot IS the test: unsealed means p1 does not exist yet.
static struct ai *readtext(struct ai *g, char const *s) {
 if (lamp(ai_core_of(g)->hot_read)) return p1text(g, s);
 return ai_ok(g = push0(g)) ? p0onto(g, s) : g; }

// apply a ONE-FORM driver text to the list on top of the stack, quoted:
// (<driver> '(list)). the driver reads ONTO its own quoted operand, so the
// application is itself a stitch. the egg driver and the plain eval fold are
// both pure lisp one-formers, so p0 reads them.
static struct ai *applyq(struct ai *g, char const *driver) {
 if (!ai_ok(g = gxr(push0(g)))) return g;            // (list)
 if (!ai_ok(g = gxl(pushq(g)))) return g;            // '(list)
 if (!ai_ok(g = gxr(push0(g)))) return g;            // ('(list))
 if (!ai_ok(g = p0onto(g, driver))) return g;        // (driver '(list))
 return ai_pop(ai_eval(g), 1); }

// the plain eval fold: run a list of forms in order, answer the last one's
// value. `ev` is read late so one text drives both of love0's passes.
static char const evfold[] = "((:(e a b)(? b(e(ev 'ev(cap b))(cup b))a)e)0)";

// every top-level form of a text, evaluated in order -- the frontends' door for
// a boot tail, a CLI driver, a corpus runner.
ai_noinline struct ai *ai_evals_(struct ai *g, char const *s) {
 ai_image_note(0x20);
 if (!ai_ok(g = readtext(g, s))) return g;
 ai_image_note(0x22);
 return applyq(g, evfold); }

ai_noinline struct ai *ai_egg_(struct ai *g, char const *egg, char const *p1,
                               char const *prel, char const *ev) {
 if (!ai_ok(g = ai_push(g, 1, ZeroPoint))) return g;
 if (!ai_ok(g = p0onto(g, p1))) return g;            // p1's forms, by p0 ..
 if (!ai_ok(g = applyq(g, evfold))) return g;        // .. and c0 evals them: p1 is live
 if (!ai_ok(g = p1text(g, ev))) return g;            // ev's half, through the reader in love
 if (!ai_ok(g = p0onto(g, prel))) return g;          // prel's half: rung 4 holds it to the subset
 if (!ai_ok(g = p0onto(g, p1))) return g;            // and p1 at the HEAD of the corpus
 return applyq(g, egg); }

// ============================================================================
// sys
// ============================================================================
op11(lvm_clock, putcharm(ai_clock() - getcharm(Sp[0])))

// the fine clock: monotonic nanoseconds, `clock`'s interval twin. clock stays
// at ms -- the scheduler's scale, safe in a 32-bit word (ns wraps 32 bits every
// 4.3s, so it can never be the deadline unit) -- and nclock answers for
// DIFFERENCES: (nclock t) is ns minus t, so time a region with two calls. the
// weak default degrades to ms*1e6 (every frontend links today); a frontend
// with a real ns source overrides (host/main.c: CLOCK_MONOTONIC; an MCU's
// cycle counter or the TSC are later rungs). on a 32-bit target the honest
// window is the charm's ns range -- an interval clock, not a calendar.
__attribute__((weak)) intptr_t ai_nclock(void) {
 return (intptr_t) (ai_clock() * 1000000u); }
op11(lvm_nclock, putcharm(ai_nclock() - ((Sp[0] & 1) ? getcharm(Sp[0]) : 0)))

// (please x): a collection ON DEMAND -- () asks for a minor, a positive charm
// for a major (gen_please's amortization rule made due, so the forced path IS
// the real one); answers the new n_gc. the real-time lever: a loop that knows
// its idle moment schedules its own pauses there. a forced collection
// OBSERVES the heap and never STEERS the schedule: the resize window
// (win_alloc/win_copied/lean) is zeroed for the call -- one out-of-band
// verdict never confirms a resize, lean needs two in a row -- and put back
// after, so a probe forcing minors under high-survival churn can't talk the
// nursery into doubling itself (the pause gauge's first draft ran the pool to
// oom@8GB through exactly that feedback).
lvm(lvm_please) {
 word n = Sp[0];
 Pack(g);
 if ((n & 1) && getcharm(n) > 0)
  g->since_major = g->major_live0 + 4 * (uintptr_t) g->len + 1;
 uintptr_t wa = g->win_alloc, wc = g->win_copied;
 intptr_t ln = g->lean;
 g->win_alloc = g->win_copied = 0, g->lean = 0;
 if (!ai_ok(g = ai_please(g, 0))) return ghelp(g);
 g->win_alloc = wa, g->win_copied = wc, g->lean = ln;
 Unpack(g);
 Sp[0] = putcharm((intptr_t) g->n_gc);
 Ip += 1; return Continue(); }

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
//  [13] n_resize  pool reallocations so far (band resizes + forced grows) -- the pool-cliff tell:
//                 a bench that prints its delta catches remap flapping cold
//  [14] minor_hi  peak words a single MINOR copied -- the pause gauge: a copying collection's
//  [15] major_hi  peak words a single MAJOR copied    pause is its copy volume, so these bound the
//                 worst pause of each kind in words (deterministic; ns = words * the copy rate,
//                 which test/host/gcpause.l measures on the host)
// derive: mortality = (n_seen - n_evac)/n_seen ; copy-amp = n_evac/max_heap ; young = heap - core.
// Indices [3..7],[9..12] are the generational instrumentation ([8] is always live). An array (not a list):
// every field is a charm, so a flat numeric tray is the natural rep -- compact, net/max apply directly.
lvm(lvm_gauge) {
 enum { N = 16 };
 uintptr_t const bytes = sizeof(struct ai_vec) + 1 * sizeof(word) + N * ai_T[ai_Z];
 Have(b2w(bytes));
 struct ai_vec *v = (struct ai_vec*) Hp;
 Hp += b2w(bytes);
 ini_vec(v, ai_Z, 1);
 v->shape[0] = N;
 vec_put_int(v, 0, (intptr_t) g->len);
 vec_put_int(v, 1, (intptr_t) (Hp - ptr(g)));
 vec_put_int(v, 2, (intptr_t) (ptr(g) + g->len - Sp));
 vec_put_int(v, 3, (intptr_t) g->n_gc);
 vec_put_int(v, 4, (intptr_t) g->max_len);
 vec_put_int(v, 5, (intptr_t) g->max_heap);
 vec_put_int(v, 6, (intptr_t) g->n_seen);
 vec_put_int(v, 7, (intptr_t) g->n_evac);
 vec_put_int(v, 9, (intptr_t) g->rem_miss);
 vec_put_int(v, 10, (intptr_t) g->rem_hi);
 vec_put_int(v, 11, (intptr_t) g->n_minor);
 vec_put_int(v, 8, (intptr_t) (g->major_pool ? g->major_hp - g->major_base : g->minor - (word*) g->end));  // major live (gen), else [end,minor)
 vec_put_int(v, 12, (intptr_t) (g->major_pool ? 2 * g->major_len : 0));  // major pool capacity (both halves), words
 vec_put_int(v, 13, (intptr_t) g->n_resize);
 vec_put_int(v, 14, (intptr_t) g->minor_hi);
 vec_put_int(v, 15, (intptr_t) g->major_hi);
 return Sp[0] = word(v), Ip++, Continue(); }

// (apof x): x's kind pointer (cell[0]) as a fixnum, 0 for a fixnum/immediate. The string-lane glaze
// reads the kind of a reference string at codegen time and emits a `cmp [s], kind; jne deopt` type guard.
lvm(lvm_apof) {
 word x = Sp[0];
 Sp[0] = putcharm(lamp(x) ? (uintptr_t) cell(x)->ap : 0);
 Ip += 1;
 return Continue(); }

// Default fd-keyed waits. Frontends override; defaults are conservative
// (all fds always-ready; multi-source wait collapses to plain sleep) so
// frontends that don't multitask (lcat, pd) link without providing impls.
__attribute__((weak)) bool ai_ready(int fd) { (void) fd; return true; }
__attribute__((weak)) void ai_wait_fds(int const *fds, int n, uintptr_t ticks) {
  (void) fds; (void) n; ai_sleep(ticks); }

// Default fd close is a no-op. The host overrides with close(2); kernel
// and pd don't have real OS fds to release, so the no-op is correct.
__attribute__((weak)) void ai_fd_close(int fd) { (void) fd; }
// default sleep is busy wait
__attribute__((weak)) ai_noinline void ai_sleep(uintptr_t ticks) {
  for (ticks += ai_clock(); ai_clock() < ticks;); }

// (cue? p): would `see` answer WITHOUT PARKING? the exact dual of the park law,
// so it tests the same three terms lvm_fgetc's guard does -- a pushed-back byte,
// a pending buffered run, then the fd. asking any less than all three calls a
// port with bytes already in hand "not ready", which is the one answer that
// makes a drain loop stall on text it is holding.
// ⚠ it asks WILL YOU ANSWER, not IS THERE DATA: a hung-up fd reads ready and the
// see that follows answers -1. that is what a refill loop wants (drain, then let
// the -1 end it) and it is why there is no separate eof question here.
// a NON-PORT asks about stdin -- the bare `(cue? 0)` the repl poll, rove,
// manifest and ink all write, from before the question could name a port.
lvm(lvm_key) {
 struct ai_io *i = iop(Sp[0]) ? (struct ai_io*) Sp[0] : &ai_stdin;
 Sp[0] = (getcharm(i->ungetc_buf) != EOF || bio_rpending(bio_of(g, i))
          || ai_ready(getcharm(i->fd))) ? putcharm(1) : nil;
 Ip += 1;
 return Continue(); }

// ============================================================================
// map (lookup-lambda backed by an open-addressed thread; see tabp comment)
// ============================================================================
// backing is internal -- only ever reached from a header[1], never applied as a
// l value; its ap behaves-as-1 like lvm_buf should it ever be (it won't).
static lvm(lvm_map_data) {
 return Ip = cell(*++Sp), *Sp = putcharm(1), Continue(); }

// the backing slot of k, or -- if absent -- the first empty slot on its probe
// chain. load is kept < 3/4 so an empty slot always terminates the sound.
static ai_inline uintptr_t map_probe(struct ai *g, word m, word k, bool *found) {
 uintptr_t mask = map_cap(m) - 1, i = hash(g, k) & mask;
 word *s = map_slots(m);
 for (;; i = (i + 1) & mask) {
  word sk = s[2 * i];
  if (sk == map_gap) return *found = false, i;
  if (eql(g, k, sk)) return *found = true, i; } }

word ai_mapget(struct ai *g, word zero, word k, word m) {
 bool found; uintptr_t i = map_probe(g, m, k, &found);
 return found ? map_slots(m)[2 * i + 1] : zero; }

// the layered global read: g->book is a CHAIN of books (the abyss -- one link
// today, orth), walked head-first so an upper layer shadows a lower; zero is
// the total-miss answer. A per-layer miss needs its own sentinel (a stored ()
// must SHADOW, never fall through) -- the private static's address can be no
// value. The l twin is ev.l's gv; keep them in step.
static word bookget(struct ai *g, word zero, word k) {
 static union u const miss[1];
 for (word c = g->book; chainp(c); c = B(c)) {
  word v = ai_mapget(g, word(miss), k, A(c));
  if (v != word(miss)) return v; }
 return zero; }

// the layered macro read: each layer's macro table rides its [nil] slot (orth's
// is made at boot; a layer without one is skipped). Miss answers 0, the macro
// hook's no-macro convention.
static word macroget(struct ai *g, word k) {
 static union u const miss[1];
 for (word c = g->book; chainp(c); c = B(c)) {
  word mt = ai_mapget(g, word(miss), nil, A(c));
  if (mt == word(miss)) continue;
  word v = ai_mapget(g, word(miss), k, mt);
  if (v != word(miss)) return v; }
 return 0; }

// fill an empty cap-slot backing at b (cap a power of two); caller reserves it.
static ai_inline union u *map_fill_back(union u *b, uintptr_t cap) {
 b[0].ap = lvm_map_data, b[1].x = putcharm(0), b[2].x = putcharm(cap);
 for (uintptr_t i = 0; i < cap; i++) b[3 + 2 * i].x = map_gap, b[4 + 2 * i].x = nil;
 return tagthread(b, 3 + 2 * cap); }

// double the backing of the map at sp[2] and rehash into it, then swap it into
// header[1]; the header never moves, so aliased references stay valid. The
// rehash inserts distinct keys into a backing with room to spare, so it never
// allocates and the fresh backing can't move under it.
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
static ai_noinline word ai_mapdel(struct ai *g, word m, word k, word zero) {
 bool found; uintptr_t i = map_probe(g, m, k, &found);
 if (!found) return zero;
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
 s[2 * i] = map_gap, s[2 * i + 1] = nil;
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

// (tablet n): a fresh EMPTY map -- header [lvm_map_lookup, backing] + backing.
// n is a SIZE HINT: the backing is presized to hold n entries below the 0.75 load
// factor (ai_mapput grows when (len+1)*4 >= cap*3), so a loop that inserts n known
// keys never rehashes. n<=0 (incl. the bare `(tablet 0)`) keeps the min capacity.
// The map is empty either way (len 0); the hint is a pure throughput optimization.
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
 return Hp += nb + 3, Ip++, Continue(); }

// (m k): map application is lookup, nil if absent (the map is its own lookup fn,
// so (m k) == (get 0 k m)). No alloc, unwinds like self-quote: drop the arg,
// jump to the return address at Sp[1], leave the result on top.
static lvm(lvm_map_lookup) {
 word v = ai_mapget(g, ZeroPoint, Sp[0], (word) Ip);   // a map miss answers () (the zero point), not the number 0
 return Ip = cell(*++Sp), *Sp = v, Continue(); }

op11(lvm_tabp, tabp(Sp[0]) ? putcharm(1) : nil)
// (litp x): is x LIT -- a top-region reference value? lit? names the upper segment of
// the lattice, ai_kind >= KMap: a map (mutable) and the tops above it (closures, nifs,
// and the opaque hots cask/port -- so `hot? ⟹ lit?`). NOT the fresh value-data
// below (numbers, strings, points, chains, trays), so a missing nom's () is not lit?.
// (the raw heap-pointer test is `lamp`, C-internal; lit? is the lattice cut, not storage.)
// A COIN dispatches via KHot but is NOT intrinsically a reference: its DIE decides --
// DIE_HOT truthy -> lit (a mutable-backed newtype), absent -> fresh DATA (a rational is
// a value like a number, not lit?). So lit? reads the die, not the storage kind.
lvm(lvm_litp) {
 word x = Sp[0];
 bool lit = coinp(x) ? !ai_nilp(g, die_get(g, coin_die(x), DIE_HOT))   // a coin: its die decides
                     : ai_kind(x) >= KMap;                             // else the lattice cut
 Sp[0] = lit ? putcharm(1) : nil;
 return Ip++, Continue(); }
// (hotp x): is x an opaque hot handle -- a buf or a port? (a task is referenced
// by a fixnum id, not a handle object.) a hot also answers lamp (it acts as
// a constant function); hotp is the refinement that names the zoo.
op11(lvm_hotp, (bufp(Sp[0]) || iop(Sp[0])) ? putcharm(1) : nil)

// (hash x) -- the general hashing method exposed to l as a fixnum.
op11(lvm_dig, putcharm(hash(g, Sp[0])))

lvm(lvm_peep) {                                // (peep coll key default): collection-first
 word x = Sp[0], k = Sp[1], z = Sp[2], n;
 if (bufp(x)) {                                  // mutable byte string: byte index
  struct ai_str *s = buf_str(x);
  if (charmp(k) && (n = getcharm(k)) >= 0 && n < (word) len(s))
   z = putcharm((unsigned char) txt(s)[n]); }
 else if (tabp(x)) z = ai_mapget(g, z, k, x);     // map lookup (not a data sentinel)
 else if (lamp(x) && datp(x)) switch (typ(x)) {
  default: break;                               // a bare mint (KMint) is not indexable
  case KFlo:                                    // a rank-0 scalar float: a nil key derefs to itself
  case KWide:                                   // ... same for a wide-int scalar
  case KCplx:                                   // ... and a complex scalar
   if (nilp(k)) z = x;
   break;
  case KVec: {
   // Array index: a fixnum for a rank-1 array, or a shape-list (row-major) for
   // rank-N. Out-of-bounds or a wrong-rank key falls through to the default `z`.
   // Integer elements keep integer type (emit_int demotes-or-boxes); float
   // elements box an f64. (A vec is always rank>=1 now; scalar gems deref above.)
   struct ai_vec *v = vec(x);
   uintptr_t R = v->rank, off = 0; bool ok = false;
   if (R == 1 && charmp(k)) {
    intptr_t ix = getcharm(k);
    if (ix >= 0 && ix < (intptr_t) v->shape[0]) off = ix, ok = true; }
   else if (chainp(k)) {
    uintptr_t a = 0; ok = true;
    for (word l = k;; l = B(l)) {
     if (!chainp(l)) { ok = a == R; break; }
     word ki = A(l);
     if (a >= R || !charmp(ki)) { ok = false; break; }
     intptr_t ix = getcharm(ki);
     if (ix < 0 || ix >= (intptr_t) v->shape[a]) { ok = false; break; }
     off = off * v->shape[a] + ix, a++; } }
   if (ok && v->type == ai_O) z = vec_get_obj(v, off);   // object: the slot IS the value
   else if (ok && v->type == ai_C) {                       // packed complex -> a (re,im) box
    Have(cplx_req); v = vec(Sp[0]);                      // re-read coll (Sp[0]) post-Have
    ai_flo_t *fp = vec_data(v);
    z = mk_cplx(&Hp, fp[2*off], fp[2*off+1]); }
   else if (ok) { word _res; Have(box_req); v = vec(Sp[0]);
    if (v->type >= ai_R) emit_flo(vec_get_flo(v, off));
    else emit_int(vec_get_int(v, off));
    z = _res; }
   break; }
  case KString:
   // Byte as its unsigned value 0..255 -- bytes are data, signedness is the
   // operator's job. txt is signed char[], so cast to avoid sign-extending a
   // high byte (e.g. 0xff -> -1) when binary data is indexed.
   if (charmp(k) && (n = getcharm(k)) >= 0 && n < (word) len(x))
    z = putcharm((unsigned char) txt(x)[n]);
   break;
  case KChain:
   if (charmp(k) && (n = getcharm(k)) >= 0) {
    while (n-- && chainp(x = B(x)));
    if (chainp(x)) z = A(x); } }
 return Sp[2] = z, Sp += 2, Ip += 1, Continue(); }

// (pin coll key val): collection-first map insert, or -- when coll is a buf --
// store the byte val at index key. Both leave coll on the stack as the result.
// A buf store needs no allocation, so no GC dance; out-of-range/non-numeric is a
// silent no-op, matching the misuse convention of the other byte ops.
lvm(lvm_put) {
 word x = Sp[0], n;                              // coll
 if (tabp(x)) {
  Sp[0] = Sp[1], Sp[1] = Sp[2], Sp[2] = x;       // ai_mapput wants (sp0,sp1,sp2)=(key,val,coll)
  Pack(g);
  if (!ai_ok(g = ai_mapput(g))) return ghelp(g);
  Unpack(g); }
 else {
  if (bufp(x) && charmp(Sp[1]) && (n = getcharm(Sp[1])) >= 0 && n < (word) len(buf_str(x)))
   txt(buf_str(x))[n] = (char) getcharm(Sp[2]);     // index = key = Sp[1], val = Sp[2]
  Sp[2] = x, Sp += 2; }                           // leave coll as the result
 return Ip += 1, Continue(); }

// (pull coll key default): remove key from a map, returning its value -- or
// default if absent (symmetry with peep) -- and mutating coll in place.
// ai_mapget reads the value, ai_mapdel removes it; neither allocates, so no GC
// dance. A non-map coll yields default (silent misuse).
lvm(lvm_pull) {
 word coll = Sp[0], v = Sp[2];                   // default
 if (tabp(coll)) {
  v = ai_mapget(g, Sp[2], Sp[1], coll);           // value, or default if absent
  ai_mapdel(g, coll, Sp[1], Sp[2]); }             // remove in place (no-op if absent)
 return Sp[2] = v, Sp += 2, Ip += 1, Continue(); }

lvm(lvm_keys) {
 intptr_t list = ZeroPoint;                         // () terminator / empty-map result (nil-ontology)
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
 return Continue(); }

static ai_noinline uintptr_t hash_two(struct ai *g, word x) {
 word *base = off_pool(g), *top = base + g->len, *w = base;
 for (uintptr_t h = mix;; x = *--w) {
  while (chainp(x)) {
   if (w == top) __builtin_trap();       // worklist overflow: a cycle
   h = (h ^ mix) * mix;                  // mark a chain node
   *w++ = A(x), x = B(x); }
  h = (h ^ hash(g, x)) * mix;          // x is a leaf: hash won't recur
  if (w == base) return h; } }

// general hashing method...
// The anchor an out-of-pool ap hashes AGAINST. Its address moves with every other
// main-program address (one load-base delta shifts them together), so the OFFSET is
// what stays put. Hashing the raw address instead would be stable within a run but
// not across a bake/wake: a bucket index computed at bake time would miss at wake,
// and every nif-keyed table (feels, pureset) would silently read empty -- the woken
// compiler folding no arithmetic and inlining no nif-bodied lambda, while `=` on the
// same nifs kept answering true. The offset also makes the index deterministic per build.
static const char hash_base[1] = {0};
struct arib; static uintptr_t shash(struct ai *g, word x, struct arib *env);  // α-invariant source hash
static bool clo_nfhash(struct ai *g, word x, uintptr_t *out);  // partial-app -> capture-substitution normal-form hash (the beta bridge)
uintptr_t hash(struct ai *g, intptr_t x) {
 if (charmp(x)) return rot(x*mix);
 if (!datp(x)) {
   // out-of-pool (static nif): its distinct offset from hash_base (see there -- the
   // address alone would not survive a wake). in-pool: a compiled lambda
   // parks its source \-expr one cell before the entry (the tag head points there) and
   // hashes it α-invariantly (so the order agrees with `=`'s α-equivalence); else by
   // length. All GC-stable (buckets survive copy).
   if (!in_heap(g, x)) return rot((x - (intptr_t) hash_base) * mix);   // a tenured closure lives in the major pool, still in-heap
   union u *k = cell(x); struct ai_tag *tg = ttag(g, k);
   if (tag_head(tg) < k) return shash(g, k[-1].x, 0);   // no-capture lambda: α-invariant source hash
   uintptr_t nf;                                        // partial-app over a SOURCED base: hash its capture-substitution
   if (clo_nfhash(g, x, &nf)) return nf;                // normal form, so the beta bridge stays hash-consistent (=-equal -> same hash)
   uintptr_t r = mix;                                   // else (continuation / handle / bif-based partial-app): by object length
   for (union u *y = k; y < (union u*) tg; y++) r ^= r * mix;
   return r; }
 switch (typ(x)) {
   default: __builtin_trap();
   case KChain: return hash_two(g, x);
   case KMint: return sym(x)->code;
   case KNom: return nom(x)->dig;                  // the cached SPELLING hash -- content, like every
                                                   // other data kind. a serial would be cheaper to mint
                                                   // but is a session-history key: it orders every
                                                   // tablet's buckets by INTERN ORDER, so an unrelated
                                                   // load reshuffles every keys walk (a reproducible-
                                                   // build leak). same-spelled DISTINCT noms collide;
                                                   // the bucket probe's `=` separates them.
   case KVec: {
    uintptr_t len = ai_vec_bytes(vec(x)), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case KBig: {
    uintptr_t len = ai_big_bytes((struct ai_big*) x), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case KFlo: {                                 // hash the lean box (ap is GC-stable, payload is the value)
    uintptr_t len = flo_req * sizeof(word), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case KWide: {                                // same: hash the lean box bytes
    uintptr_t len = wide_req * sizeof(word), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case KCplx: {                                // same: hash the lean (ap, re, im) box bytes
    uintptr_t len = cplx_req * sizeof(word), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case KString: {
    uintptr_t n = len(x), h = mix;
    char const *bs = txt(x);
    while (n--) h ^= (uint8_t) *bs++, h *= mix;
    return h; } } }

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

op11(lvm_strp, strp(Sp[0]) ? putcharm(1) : nil)
lvm(lvm_snip) {
 if (!strp(Sp[0])) Sp[2] = nil;
 else {
  struct ai_str *s = str(Sp[0]), *t;
  intptr_t i = oddp(Sp[1]) ? getcharm(Sp[1]) : 0,
           j = oddp(Sp[2]) ? getcharm(Sp[2]) : 0;
  i = max(i, 0), i = min(i, (word) len(s));
  j = max(j, i), j = min(j, (word) len(s));
  // An empty range (i == j) builds a 0-length string "" -- a string snip returns a
  // STRING, the closest form of nothing for this kind, not the bare floor (fixnum 0).
  size_t req = str_type_width + b2w(j - i);
  Have(req);
  s = str(Sp[0]);                                // re-read post-Have (GC may have moved it)
  t = (struct ai_str*) Hp;
  Hp += req;
  ini_str(t, j - i);
  memcpy(txt(t), txt(s) + i, j - i);
  Sp[2] = (word) t; }
 return Ip += 1, Sp += 2, Continue(); }


// A buf has no function meaning, so applying it behaves as 0 (yields 1, the const-1
// identity numeral) -- like every structureless value. Its address is still the
// kind tag: ai_noicf (on every lvm) keeps this byte-identical to lvm_port_io so
// bufp and iop never collide. NOT a data sentinel, so the GC copies a buf via the
// generic thread path and the cheney sound forwards its backing-string pointer.
lvm(lvm_buf) {
 return Ip = cell(*++Sp), *Sp = putcharm(1), Continue(); }
// (buf n) — allocate a zeroed n-byte mutable buf. (buf charlist) — allocate one HOLDING
// those bytes, the lane `string` has always had for a charlist: it is the only BULK way
// into a cask, and without it the sole bulk byte path in the system ran through a string
// (filling by pin costs ~20x). n<=0 / non-numeric -> the empty
// string singleton EmptyString, so NO empty buf object ever exists (an un-writable 0-byte
// buf IS ""); this lets ai_nilp drop its buf branch (every real buf has len>=1, truthy).
// Two heap objects under one Have (so no GC sees a half-built buf): the backing ai_str
// holding the bytes, and the length-2 wrapper thread [lvm_buf, str, terminator].
lvm(lvm_bufnew) {
 bool listp = chainp(Sp[0]);
 intptr_t n = charmp(Sp[0]) ? getcharm(Sp[0]) : listp ? (intptr_t) llen(Sp[0]) : 0;
 if (n <= 0) return Sp[0] = EmptyString, Ip++, Continue();   // no empty buf: it is ""
 uintptr_t sreq = str_type_width + b2w(n),
           breq = Width(struct ai_buf) + Width(struct ai_tag);
 Have(sreq + breq);
 struct ai_str *s = ini_str((struct ai_str*) Hp, n);
 Hp += sreq;
 if (listp) {                                                // the charlist lane, mirroring lvm_string's
  word y = Sp[0];                                            // re-read post-Have, like the buf lane there
  for (uintptr_t i = 0; i < (uintptr_t) n; y = B(y)) txt(s)[i++] = (char) getcharm(A(y)); }
 else memset(txt(s), 0, n);
 union u *k = (union u*) Hp;
 Hp += breq;
 ((struct ai_buf*) k)->ap = lvm_buf;
 ((struct ai_buf*) k)->str = s;
 tagthread(k, Width(struct ai_buf));
 return Sp[0] = word(k), Ip++, Continue(); }

// THE W^X CODE ARENA (hosted builds only). The Linux malloc heap is NX, so emitted
// bytes cannot be run where they land -- the jump faults. `nat` copies them into a
// W^X mapping instead: mmap RW, write, mprotect to R+X, and never write again
// (writable XOR executable, honored, so hardened systems that forbid RWX still run
// it). The mapping is page-rounded and holds an ai_str header [len, bytes..]; the
// code address lives outside the GC pool -- gcp's out-of-pool short-circuit leaves
// it untouched (like the immortal data-segment constants) -- and nat_unmap munmaps
// it when the native closure dies. The freestanding kernel needs none of this: its
// HHDM is mapped executable, so a plain heap copy already runs (love/glaze/README.md).
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
// the W^X arena finalizer: recover the ai_str base from the code address (cell[0],
// the header) and munmap it. Fires only on death (run_finalizers' survival test
// reads z->p->x: the header is the out-of-pool code addr when dead, an in-pool
// forward when alive -- so a dead native closure is correctly reclaimed).
#if __STDC_HOSTED__
static void nat_unmap(void *p) {
 char *code = (char*) ((union u*) p)[0].ap;            // header == the W^X code address
 struct ai_str *base = (struct ai_str*) (code - sizeof(struct ai_str));   // code == s->bytes
 munmap(base, code_maplen(base->len)); }
#endif

// (nat codebuf interp src) installs codebuf's machine code and returns a TRANSPARENT
// applicable native closure. Cell: [code, src, code, interp, lvm_ret, putcharm(0)] with
// the VALUE pointing at the 3rd word (the second `code`), so:
//   value[0]  = code      -- lvm_ap dispatches straight into the emitted body
//   value[-1] = src       -- the source \-expr, where fn_src/the printer/salpha look,
//                            so the native closure is =/show-IDENTICAL to its source
//   value[1]  = interp    -- the deopt fallback (rsi+8): an overflow guard tail-applies
//                            it to the original arg (Sp[0]) -> native is never wrong
//   value[2]  = lvm_ret   -- the fast-path return (emitted: add rsi,16; Continue)
// cell[0] is a HEADER duplicate of the code addr at the allocation start: out-of-pool,
// so run_finalizers distinguishes a dead closure (header) from a live one (forward).
// The code follows the lvm ABI (g=rdi Ip=rsi Hp=rdx Sp=rcx). Applied by JUXTAPOSITION
// -- no run-verb; plumbing the compiler calls to emit native for a hot closure,
// transparently. Internal: the egg mops it like boxfix/feel.
// (nif code interp src arity) -- emitted bytes -> applicable native value; the merge
// of the old nat (arity 1) and natn (arity>=2). ARITY 1: a 6-word cell whose ENTRY is
// the native body directly (value[-1]=src, value[1]=interp deopt, value[2]=lvm_ret),
// ret pops n=1. ARITY>=2: an 8-word cell whose entry is lvm_cur (curry to saturation),
// the body at cell+2 (lvm_cur's Ip+2 resume), ret pops n=arity. interp@rsi+8 and
// lvm_ret@rsi+16 sit at the SAME offsets in both, so the emitted body is layout-blind;
// DEOPT jmps to interp's body. value[-1]=src (=/show). Internal: the egg mops it.
lvm(lvm_nif) {
 word codebuf = Sp[0];                        // Sp[0]=code Sp[1]=interp Sp[2]=src Sp[3]=arity
 intptr_t ar = oddp(Sp[3]) ? getcharm(Sp[3]) : 0;
 if (!(strp(codebuf) || bufp(codebuf)) || ar < 1) return Sp[3] = nil, Sp += 3, Ip++, Continue();
 uintptr_t n = len(bytes_of(codebuf));
 if (n == 0) return Sp[3] = nil, Sp += 3, Ip++, Continue();
#ifdef __wasm__                                // wasm has NO executable code pages: a jump to a data address traps.
 return Sp[3] = nil, Sp += 3, Ip++, Continue(); //  decline unconditionally -> the interp twin runs (emscripten's mprotect
#endif                                         //  is a no-op returning 0, so the mprotect guard below does NOT catch this).
#if __STDC_HOSTED__
 Have(9 + Width(struct ai_fz));               // 9 covers both cells (6/8 words) + tag + fz
 size_t maplen = code_maplen(n);
 void *base = mmap(0, maplen, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
 if (base == MAP_FAILED) return Sp[3] = nil, Sp += 3, Ip++, Continue();
 struct ai_str *s = ini_str((struct ai_str*) base, n);
 memcpy(txt(s), txt(bytes_of(Sp[0])), n);     // reload codebuf: a GC in Have may have moved it
 if (mprotect(base, maplen, PROT_READ | PROT_EXEC))
  return munmap(base, maplen), Sp[3] = nil, Sp += 3, Ip++, Continue();
#ifndef __wasm__                               // guarded: emscripten's clang has no clear_cache intrinsic (dead here anyway -- wasm early-declined above)
 __builtin___clear_cache(txt(s), txt(s) + n);  // AArch64: the I-cache is NOT coherent with the freshly
#endif                                         // written D-cache -- flush or it runs stale bytes (no-op on x86)
#else
 Have(str_type_width + b2w(n) + 9);           // freestanding: HHDM is RWX, a heap copy runs
 struct ai_str *s = ini_str((struct ai_str*) Hp, n); Hp += str_type_width + b2w(n);
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
 return Sp[3] = word(k + 2), Sp += 3, Ip++, Continue(); }

lvm(lvm_nifx) {
 word codebuf = Sp[0];                        // Sp[0]=code Sp[1]=interp Sp[2]=src Sp[3]=arity Sp[4]=extras
 intptr_t ar = oddp(Sp[3]) ? getcharm(Sp[3]) : 0;
 if (!(strp(codebuf) || bufp(codebuf)) || ar < 1) return Sp[4] = nil, Sp += 4, Ip++, Continue();
 uintptr_t n = len(bytes_of(codebuf));
 if (n == 0) return Sp[4] = nil, Sp += 4, Ip++, Continue();
#ifdef __wasm__                                // wasm has NO executable code pages: a jump to a data address traps.
 return Sp[4] = nil, Sp += 4, Ip++, Continue(); //  decline unconditionally -> the interp twin runs (emscripten's mprotect
#endif                                         //  is a no-op returning 0, so the mprotect guard below does NOT catch this).
#if __STDC_HOSTED__
 Have(11 + Width(struct ai_fz));              // 11 covers both cells (7/9 words) + tag + fz
 size_t maplen = code_maplen(n);
 void *base = mmap(0, maplen, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
 if (base == MAP_FAILED) return Sp[4] = nil, Sp += 4, Ip++, Continue();
 struct ai_str *s = ini_str((struct ai_str*) base, n);
 memcpy(txt(s), txt(bytes_of(Sp[0])), n);     // reload codebuf: a GC in Have may have moved it
 if (mprotect(base, maplen, PROT_READ | PROT_EXEC))
  return munmap(base, maplen), Sp[4] = nil, Sp += 4, Ip++, Continue();
#ifndef __wasm__                               // guarded: emscripten's clang has no clear_cache intrinsic (dead here anyway -- wasm early-declined above)
 __builtin___clear_cache(txt(s), txt(s) + n);  // AArch64: the I-cache is NOT coherent with the freshly
#endif                                         // written D-cache -- flush or it runs stale bytes (no-op on x86)
#else
 Have(str_type_width + b2w(n) + 11);          // freestanding: HHDM is RWX, a heap copy runs
 struct ai_str *s = ini_str((struct ai_str*) Hp, n); Hp += str_type_width + b2w(n);
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
 return Sp[4] = word(k + 2), Sp += 4, Ip++, Continue(); }


// (bcopy dst doff src soff n) — copy n bytes from src[soff..] into buf dst at
// doff. src may be a string or buf; dst must be a buf. Ranges are clamped to
// both backing stores -- a safety net (the caller sizes dst to fit), so an
// out-of-range request copies less rather than trampling the heap. Returns
// dst. No allocation, so no GC dance and the trailing tail call is preserved.
lvm(lvm_bcopy) {
 word dst = Sp[0], src = Sp[2];
 if (bufp(dst) && (strp(src) || bufp(src))) {
  struct ai_str *d = buf_str(dst), *s = bytes_of(src);
  intptr_t doff = getcharm(Sp[1]), soff = getcharm(Sp[3]), n = getcharm(Sp[4]),
           dl = len(d), sl = len(s);
  if (n < 0) n = 0;
  if (doff < 0) doff = 0;
  if (soff < 0) soff = 0;
  if (doff + n > dl) n = dl - doff;
  if (soff + n > sl) n = sl - soff;
  if (n > 0) memmove(txt(d) + doff, txt(s) + soff, n); }
 return Sp[4] = dst, Sp += 4, Ip += 1, Continue(); }

// public predicate for frontends that need to check string args
bool ai_strp(ai_word x) { return strp(x); }

// ============================================================================
// the heap-image snapshot (doc/snapshot.md): serialize the compacted live heap to a
// flat blob with every pointer-bearing word range-encoded in place, so a fresh process
// reconstructs the runtime by re-walking -- skipping the ~230 ms egg eval. The core
// owns the BUFFER codec (ai_image_save/ai_image_load, no stdio); the host wraps it with
// file I/O. The one genuinely-new risk (the copying GC round-trips heap pointers but
// never maps C-function pointers) is the lvm_* symbolic table below.
// ============================================================================
// lvm_* that appear as an object's ap but are NOT in def1[] (data sentinels +
// thread aps/dispatchers). def1[] (via ai_nif_name) covers the nifs + insts.
static lvm_t *const image_extra_aps[] = {
 lvm_chain, lvm_vec, lvm_sym, lvm_nom, lvm_str, lvm_big, lvm_flo, lvm_wide, lvm_cbox,  // data sentinels
 lvm_map_lookup, lvm_map_data, lvm_buf, lvm_coin, lvm_port_io,                        // thread aps
 lvm_cur, lvm_help, lvm_ret0, lvm_ap, lvm_ret,                                         // dispatchers
 // instruction fns a compiled thread embeds directly (no def1 cell to reach
 // them through). On thumb these are ODD and would otherwise escape as
 // "fixnums" -- raw baker addresses, the cross-binary poison.
 lvm_callk, lvm_kcall, lvm_jump, lvm_scare, lvm_missing, lvm_unc, lvm_dot,
 lvm_fputbn, lvm_yield_sw, lvm_yield_nif, lvm_task_exit };
// size (words) of the object at p, using the SAME per-kind logic as the GC:
// data kinds by their copy_* sizes; threads via the production terminator scan
// (ttag, which picks the right pool bounds -- the hand-rolled [lo,hi) scan drifted).
static uintptr_t image_objsize(struct ai *g, union u *p) {
 if (in_data(p->ap)) switch (ai_typ(p)) {
  case KChain: return Width(struct ai_chain);
  case KMint:  return Width(struct ai_mint);
  case KNom:   return Width(struct ai_nom);
  case KFlo:   return Width(struct ai_flo);
  case KWide:  return Width(struct ai_wide);
  case KCplx:  return Width(struct ai_cplx);
  case KString:return b2w(sizeof(struct ai_str) + str(p)->len);
  case KBig:   return b2w(ai_big_bytes((struct ai_big*) p));
  default:     return b2w(ai_vec_bytes((struct ai_vec*) p)); }   // KVec
 word *term = (word*) ttag(g, p);                                // thread: scan to terminator (production)
 return (uintptr_t)(term - (word*) p) + 1; }
// bidirectional lvm_* table: index <-> address. supplemental table 0..E-1, then def1 E..
static intptr_t image_ap_index(intptr_t ap) {
 for (uintptr_t i = 0; i < countof(image_extra_aps); i++)
  if ((intptr_t) image_extra_aps[i] == ap) return (intptr_t) i;
 for (uintptr_t j = 0; j < countof(def1); j++)
  if (def1[j].x == ap) return (intptr_t)(countof(image_extra_aps) + j);
 return -1; }
static intptr_t image_ap_resolve(intptr_t idx) {
 return idx < (intptr_t) countof(image_extra_aps)
   ? (intptr_t) image_extra_aps[idx]
   : def1[idx - countof(image_extra_aps)].x; }
// the BARE-FN lane: a compiled thread embeds a nif's lvm function directly as
// an instruction word; the fn is reachable symbolically as the CODE SLOT of
// its def1 cell (cell[0], or cell[2] under a lvm_cur curry header). On thumb
// fn addresses are odd, so without this lane they ride raw (binary-specific).
static intptr_t image_fn_slot(word const *cell) {
 return (intptr_t) (cell[0] == (word) lvm_cur ? cell[2] : cell[0]); }
static intptr_t image_fn_index(intptr_t v) {
 for (uintptr_t j = 0; j < countof(def1); j++) {
  word const *c = (word const*) def1[j].x;
  if (image_fn_slot(c) == v) return (intptr_t) j; }
 return -1; }
static intptr_t image_fn_resolve(intptr_t j) {
 return image_fn_slot((word const*) def1[j].x); }
// the out-of-pool IMMORTALS (a small symbolic table like the lvm_* one, for cross-process):
// ZeroPoint (()), EmptyString (""), the three std ports -- and NULL, which a MID-EVAL dump
// (the bake nif) meets in a live bio port's undressed rbuf/wbuf; a raw zero is even and
// below the index bound, so without its own slot it read as unencodable.
// Serialize -> index, load -> re-resolve.
// (map_gap appended LAST: existing images' indices stay stable. it was the one
// out-of-pool sentinel riding the absolute lane -- a portable image needs it here.)
static const word image_immortals[] = { ZeroPoint, EmptyString, (word) &ai_stdin, (word) &ai_stdout, (word) &ai_stderr, 0, map_gap };
static intptr_t image_imm_index(word v) {
 for (uintptr_t i = 0; i < countof(image_immortals); i++) if (image_immortals[i] == v) return (intptr_t) i;
 return -1; }
// ============================================================================
// ai_image_save / ai_image_load -- the BUFFER codec (no stdio: the host wraps these
// with file I/O). save compacts g and serializes {header, blob} into a g->alloc'd
// buffer; load validates the header, reconstructs a fresh g, and decodes the blob in
// place. A mismatched/bad buffer -> NULL, so the caller boots normally -- never wrong.
// ============================================================================
#define IMAGE_MAGIC 0x31304f4e53494119ULL   /* bump if the wire format changes */
#if defined(__x86_64__)
#define IMAGE_ARCH 1
#elif defined(__aarch64__)
#define IMAGE_ARCH 2
#elif defined(__riscv)
#define IMAGE_ARCH 3
#else
#define IMAGE_ARCH 0
#endif
// The image is binary-SPECIFIC: its lvm-table indices and binary pointers only mean anything in the
// exact binary that dumped it. magic+wordsize don't distinguish x86 from aarch64 (same source ->
// same magic, both wordsize 8), so a cross-arch (or stale-rebuild) load would mis-resolve and crash.
// Two guards reject a mismatch -> image_load NULL -> normal boot: `arch` (a compile-time arch tag)
// and `anchor` (the dump-time address of image_dump itself). Under ASLR the WHOLE binary shifts by one
// base delta, so the refsym (image_immortals) delta must equal the anchor (image_dump) delta -- a
// different binary lays its symbols out differently, so the two deltas disagree and the load is refused.
struct image_hdr {
 uint64_t magic, wordsize, nwords, arch, anchor, nroot, rsv1, refsym, next_serial;
 uint64_t root_tag[24], root_val[24];        /* symbols, tasks, then the ENTIRE v0..end region (book,
                                                scare_a/b, the church hooks, x/io) walked GENERICALLY --
                                                same words the GC traces, so a new v0 field rides along
                                                with no codec change. nroot = how many were written. */
};
// the ABSOLUTE-POINTER GUARD: a kept-absolute word is only wakeable
// if it aims INSIDE the binary's own load segments (.text/.rodata shift by one base
// delta) -- a JIT/W^X pointer dies with the bake process and every use post-wake is a
// hardware fault the barrier eats PER CALL (the storm). The core cannot tell the two
// apart (no phdr here); the host installs the bounds check. NULL guard = audit off.
// A bad absolute fails the dump (a refused bake beats a storming binary) and the first
// few offenders land in ai_image_bad[] (heap word offset, value) for the host to name.
uintptr_t (*ai_image_absguard)(uintptr_t) = 0;
uintptr_t ai_image_bad[8];                       // quads: obj-off, val, obj-word0, rsv -- first 2 offenders
uintptr_t ai_image_nbad = 0;
// a native (nif) cell CANNOT wake -- its entry aims into this process's W^X mmap -- but
// it carries its own bytecode twin (interp, the deopt fallback), so the DUMP reverts it:
// every reference to the cell encodes as a reference to interp instead, and the husk
// (now wake-unreachable) rides as inert ballast with the guard suppressed. Anything
// wx-bad WITHOUT the nif signature stays a refusal: better no bake than a storm.
static int img_wxp(word v, word *base, word *hp) {   // an un-wakeable absolute? (every legit lane excluded FIRST:
 if (!v || oddp(v)) return 0;                        //  a heap pointer is outside the binary's segments too)
 if ((word*) v >= base && (word*) v < hp) return 0;  // in-pool: a value, not code
 if (image_ap_index((intptr_t) v) >= 0) return 0;    // lvm table
 if (image_imm_index(v) >= 0) return 0;              // immortal
 return ai_image_absguard && !ai_image_absguard((uintptr_t) v); }
static word img_nif_interp(word v, word *base, word *hp) {   // v -> a cell VALUE; its bytecode twin | 0
 word *c = (word*) v; word e = 0;
 // the FULL cell shapes (lvm_nif): arity-1 value = [code, interp, lvm_ret, n(odd)];
 // arity>=2 value = [lvm_cur, ar(odd), code, interp, lvm_ret, n(odd)] -- match every
 // fixed word AND the word BEFORE the value (the allocation header duplicates the code
 // for arity-1, fronts lvm_cur for arity>=2), because [code, interp, lvm_ret, n] is ALSO
 // what value+2 of an arity>=2 cell reads as: a partial-app's terminal unc LINK points
 // exactly there (fn_base = link-2), and redirecting a LINK to the twin's VALUE shears
 // the -2 contract -- the woken base lands on the twin's src cell and apply enters two
 // words early, re-currying forever. c[-2] tells the three apart.
 if (c + 4 <= hp && c - 2 >= base && c[-2] == c[0] && img_wxp(c[0], base, hp)
     && c[2] == (word) lvm_ret && oddp(c[3])) e = c[1];
 else if (c + 6 <= hp && c[0] == (word) lvm_cur && oddp(c[1])
     && img_wxp(c[2], base, hp) && c[4] == (word) lvm_ret && oddp(c[5])) e = c[3];
 else if (c + 4 <= hp && c - 2 >= base && c[-2] == (word) lvm_cur && oddp(c[-1])
     && img_wxp(c[0], base, hp) && c[2] == (word) lvm_ret && oddp(c[3])
     && c[1]) e = c[1] + 2 * sizeof(word);            // a LINK (value+2): twin's value+2, contract kept
 return e; }
static int img_suppress = 0;                     // inside a reverted husk: absolutes are inert ballast
static uintptr_t img_nabs = 0;                   // kept-absolute count this dump: 0 -> the image is
                                                 // binary-PORTABLE (nothing needs the base delta), and
                                                 // the load may skip the same-binary anchor check
// encode a live value (post-compaction) -> portable (tag,payload):
//  0 FIX raw | 1 PTR word-offset into the blob | 2 LVM table index | 3 IMM immortal index
static void image_root_enc(word v, word *base, word *hp, uint64_t *tag, uint64_t *val) {
 intptr_t li = image_ap_index((intptr_t) v); if (li >= 0) { *tag = 2, *val = (uint64_t) li; return; }  // ap table FIRST: thumb aps are ODD (see img_encode_)
 if (oddp(v)) { *tag = 0, *val = (uint64_t) v; return; }
 if ((word*) v >= base && (word*) v < hp) {
  word e = img_nif_interp(v, base, hp);          // a root holding a dead-native cell: its twin rides instead
  if (e) v = e;
  *tag = 1, *val = (uint64_t)((word*) v - base); return; }
 intptr_t ii = image_imm_index(v); if (ii >= 0) { *tag = 3, *val = (uint64_t) ii; return; }
 *tag = 0, *val = (uint64_t) v; }            // out-of-pool non-immortal root (unexpected): keep absolute
static word image_root_dec(uint64_t tag, uint64_t val, word *base) {
 return tag == 1 ? (word)(base + val) : tag == 2 ? (word) image_ap_resolve((intptr_t) val)
      : tag == 3 ? image_immortals[val] : (word) val; }
// --- the SELF-DESCRIBING blob: range-encode every pointer-bearing word IN PLACE, so the load
// re-derives the relocation by re-walking the heap (no stored reloc tables -> the file is just
// header + heap). hb = nw*sizeof(word) (the heap byte size). A live word maps to:
//   heap pointer  -> its BYTE offset            [0, hb)            (the thread terminator head|2 -> off+2 rides here)
//   lvm_* ap      -> hb + 2*index               [hb, hb+2*NLVM)
//   immortal      -> hb + 2*NLVM + 2*ii         [hb+2*NLVM, TBOUND)
//   binary ptr    -> kept ABSOLUTE (>= TBOUND, host .text/.rodata) -> base-delta-shifted on load
// fixnums (odd) pass through; EVERY encoded pointer is even, so the load tells pointer from fixnum
// by parity and the category by value range. Indices are doubled to stay even (parity is the
// pointer/fixnum discriminator). A binary pointer below TBOUND would alias an index -> dump refuses.
#define IMAGE_NLVM ((uintptr_t)(countof(image_extra_aps) + countof(def1)))
#define IMAGE_NIMM ((uintptr_t) countof(image_immortals))
#define IMAGE_CELLW 16u   /* max nif-cell span (words) an interior link can sit in */
static intptr_t img_encode_(intptr_t v, word *base, word *hp, uintptr_t hb, int *fail) {
 // the ap table FIRST, before parity: on thumb every function address wears
 // the interworking bit (|1, ODD), so an ap would otherwise pass as a
 // "fixnum" and ride RAW -- valid only where the baking binary's text lands
 // at the same base (the qemu-twins trap that walled the teensy wake: the
 // woken word0s pointed at the BAKER's addresses, in_data matched nothing,
 // and the terminator scan marched off the pool into the dead bus). a
 // fixnum colliding with a tabled ap's exact odd value would misencode --
 // ~300 privileged values out of 2^31, none reachable from a corpus bake.
 intptr_t idx = image_ap_index(v);
 if (idx >= 0) return (intptr_t)(hb + 2 * (uintptr_t) idx);                      // lvm_* ap (odd on thumb, even on x64 -- both land here)
 if (oddp(v)) {
  intptr_t fj = image_fn_index(v);                                               // a bare nif fn embedded as an instruction word
  if (fj >= 0) return (intptr_t)(hb + 2 * (IMAGE_NLVM + IMAGE_NIMM)
                                    + 2 * IMAGE_NLVM * IMAGE_CELLW
                                    + 2 * (uintptr_t) fj);
  return v; }                                                                    // fixnum
 if (v >= (intptr_t) base && v < (intptr_t) hp) {                                // in-pool heap pointer -> byte offset
  word e = img_nif_interp((word) v, base, hp);                                   //   ... unless it aims at a dead-native cell:
  if (e) return img_encode_((intptr_t) e, base, hp, hb, fail);                   //   redirect to its bytecode twin (interp)
  return v - (intptr_t) base; }
 intptr_t ii = image_imm_index((word) v);
 if (ii >= 0) return (intptr_t)(hb + 2 * IMAGE_NLVM + 2 * (uintptr_t) ii);       // out-of-pool immortal
 // an INTERIOR pointer into a def1 nif cell -- a baked partial application's
 // curry link aims at cell_base + off (fn_base = link-2, the unc contract).
 // encode (cell index, word offset): the owning cell is the one with the
 // GREATEST base <= v (cells pack densely, a fixed window would alias).
 { intptr_t bj = -1; uintptr_t boff = 0;
   for (uintptr_t j = 0; j < countof(def1); j++) {
    uintptr_t x = (uintptr_t) def1[j].x, d = (uintptr_t) v - x;
    if ((uintptr_t) v > x && d < IMAGE_CELLW * sizeof(word) && !(d % sizeof(word))
        && (bj < 0 || x > (uintptr_t) def1[bj].x)) bj = (intptr_t) j, boff = d / sizeof(word); }
   if (bj >= 0) return (intptr_t)(hb + 2 * (IMAGE_NLVM + IMAGE_NIMM)
                                     + 2 * (((uintptr_t)(countof(image_extra_aps) + (uintptr_t) bj)) * IMAGE_CELLW + boff)); }
 if ((uintptr_t) v < hb + 2 * (IMAGE_NLVM + IMAGE_NIMM) + 2 * IMAGE_NLVM * IMAGE_CELLW
                   + 2 * countof(def1))
  *fail = 1;                                                                     // a binary ptr in the index range: unencodable
 if (!img_suppress && img_wxp((word) v, base, hp)) *fail = 2;                    // un-wakeable absolute (JIT/W^X/mmap)
 img_nabs++;                                                                     // kept absolute: the image is now binary-specific
 return v; }                                                                     // binary (host nif/.rodata): absolute, +delta on load
static uintptr_t img_cur_off, img_cur_ap;        // the object being encoded: offset + its word0 (the hot)
static intptr_t img_encode(intptr_t v, word *base, word *hp, uintptr_t hb, int *fail) {
 int f0 = *fail; *fail = 0;
 intptr_t r = img_encode_(v, base, hp, hb, fail);
 if (*fail == 2 && ai_image_nbad < countof(ai_image_bad) / 4)
  ai_image_bad[4 * ai_image_nbad] = img_cur_off, ai_image_bad[4 * ai_image_nbad + 1] = (uintptr_t) v,
  ai_image_bad[4 * ai_image_nbad + 2] = img_cur_ap, ai_image_bad[4 * ai_image_nbad + 3] = 0, ai_image_nbad++;
 if (f0) *fail = f0 > *fail ? f0 : *fail;
 return r; }
static intptr_t img_decode(intptr_t v, word *base, uintptr_t hb, intptr_t delta) {
 if (oddp(v)) return v;
 uintptr_t uv = (uintptr_t) v;
 if (uv < hb) return (intptr_t)((char*) base + uv);                              // byte offset -> live pointer
 if (uv < hb + 2 * IMAGE_NLVM) return image_ap_resolve((intptr_t)((uv - hb) / 2));
 if (uv < hb + 2 * (IMAGE_NLVM + IMAGE_NIMM)) return (intptr_t) image_immortals[(uv - hb - 2 * IMAGE_NLVM) / 2];
 if (uv < hb + 2 * (IMAGE_NLVM + IMAGE_NIMM) + 2 * IMAGE_NLVM * IMAGE_CELLW) {   // nif-cell interior: base + word offset
  uintptr_t k = (uv - hb - 2 * (IMAGE_NLVM + IMAGE_NIMM)) / 2;
  return image_ap_resolve((intptr_t)(k / IMAGE_CELLW)) + (k % IMAGE_CELLW) * sizeof(word); }
 if (uv < hb + 2 * (IMAGE_NLVM + IMAGE_NIMM) + 2 * IMAGE_NLVM * IMAGE_CELLW
         + 2 * countof(def1))                                                    // bare-fn lane: the cell's code slot
  return image_fn_resolve((intptr_t)((uv - hb - 2 * (IMAGE_NLVM + IMAGE_NIMM)
                                         - 2 * IMAGE_NLVM * IMAGE_CELLW) / 2));
 return v + delta; }
// dump g to PATH (compacts g -- call at a quiescent point). 0 ok, <0 error. The file is just
// {header, blob}: the blob is the compacted heap with every pointer-bearing word range-encoded in
// place (img_encode), so the load re-derives the relocation by re-walking -- NO stored reloc tables.
// serialize g -> a fresh g->alloc'd {header, blob} buffer; *outlen = its byte length; NULL on failure.
// the worker dumps WHEREVER it's called: a mid-eval dump (the bake nif) traces the live
// stack, so the running continuation's objects ride into the blob as wake-unreachable
// ballast -- harmless, the load side resets sp and re-establishes ip regardless. the
// guarded entry below keeps the boot path honest (a non-quiescent boot dump is a bug).
void *ai_image_save_(struct ai *g, uintptr_t *outlen) {
 if (!g->major_pool) return NULL;                        // needs the major pool (it holds the compacted live half)
 if (!ai_ok(gen_major(g))) return NULL;                  // COMPACT: live half -> [major_base, major_hp) (OOM -> no image)
 word *base = g->major_base, *hp = g->major_hp;
 uintptr_t nw = (uintptr_t)(hp - base), bytes = nw * sizeof(word), hb = bytes, total = sizeof(struct image_hdr) + bytes;
 char *buf = g->alloc(g, NULL, total);
 if (!buf) return NULL;
 word *blob = (word*) (buf + sizeof(struct image_hdr));  // the blob follows the header in one buffer
 memcpy(blob, base, bytes);
 int fail = 0;
 img_nabs = 0;
 for (union u *p = (union u*) base; (word*) p < hp; ) {   // walk the LIVE heap (ttag works on it), encode into blob
  uintptr_t off = (uintptr_t)((word*) p - base);
  // a LIVE finalizer node (an open port's close, a nat's unmap) sits RAW in the
  // heap -- three words, no object header -- so neither this walk nor the load's
  // blind re-walk can stride it. forge its blob copy into a dead chain (the same
  // width): wake-unreachable ballast, and the fz head lives OUTSIDE the v0..end
  // root window, so a woken session starts with no finalizables -- the dump-time
  // fds meant nothing in the new process anyway. (a boot-path dump has no live
  // fz: ports are closed and gen_major just consumed the dead ones.)
  struct ai_fz *z = g->fz;
  while (z && (union u*) z != p) z = z->next;
  if (z) {
   blob[off] = img_encode((intptr_t) lvm_chain, base, hp, hb, &fail);
   blob[off + 1] = blob[off + 2] = img_encode((intptr_t) ZeroPoint, base, hp, hb, &fail);
   p = (union u*) ((word*) p + Width(struct ai_fz));
   continue; }
  uintptr_t sz = image_objsize(g, p);
  img_cur_off = off, img_cur_ap = ((word*) p)[0];
  // a native cell's allocation head: header duplicates the code (arity-1) or fronts an
  // lvm_cur curry cell (arity>=2). references were REDIRECTED to interp, so the husk is
  // wake-unreachable ballast -- suppress the guard across it. (p+2 is the cell VALUE.)
  img_suppress = ((word*) p + 6 <= hp)
    && img_wxp(((word*) p)[0], base, hp)
    && ((((word*) p)[2] == ((word*) p)[0] && ((word*) p)[4] == (word) lvm_ret)
        || (((word*) p)[2] == (word) lvm_cur && (word*) p + 8 <= hp && ((word*) p)[6] == (word) lvm_ret));
  blob[off] = img_encode(((word*) p)[0], base, hp, hb, &fail);                                    // word0: the ap
  if (in_data(p->ap)) switch (ai_typ(p)) {
   case KChain: blob[off + 1] = img_encode(((struct ai_chain*) p)->a, base, hp, hb, &fail);
                blob[off + 2] = img_encode(((struct ai_chain*) p)->b, base, hp, hb, &fail); break;
   case KNom:   blob[off + 1] = img_encode((intptr_t) nom(p)->name, base, hp, hb, &fail); break;
   case KVec:   if (vec(p)->type == ai_O) {
                 word *e = (word*) vec_data(vec(p)); uintptr_t ne = vec_nelem(vec(p)), eo = (uintptr_t)(e - (word*) p);
                 for (uintptr_t i = 0; i < ne; i++) blob[off + eo + i] = img_encode(e[i], base, hp, hb, &fail); }
                break;
   default: break; }                                     // KMint/KStr/KBig/KFlo/KWide/KCplx: flat leaves
  else for (uintptr_t i = 1; i < sz; i++) blob[off + i] = img_encode(((word*) p)[i], base, hp, hb, &fail);   // thread interior + terminator
  img_suppress = 0;
  p = (union u*) ((word*) p + sz); }
 if (fail) { g->alloc(g, buf, 0); return NULL; }         // a binary pointer landed in the index range -> refuse (caller boots normally)
 // rsv1 carries the kept-absolute count, ODD-tagged ((n<<1)|1) so a pre-field image
 // (rsv1 == 0) never reads as "zero absolutes" -- those keep the strict anchor check.
 struct image_hdr H = { IMAGE_MAGIC, sizeof(word), nw, IMAGE_ARCH, (uint64_t)(word) &ai_image_save, 0, (uint64_t)(img_nabs << 1) | 1u, (uint64_t)(word) image_immortals, g->next_serial, {0}, {0} };
 // roots = symbols + tasks (live OUTSIDE v0), then the whole GC-traced v0..end block, GENERICALLY: any
 // field added to struct ai's v0 region is serialized automatically, no codec edit (cf. the GC's v0..end loop).
 uintptr_t nv = (word*) g->end - (word*) &g->v0, nr = 2 + nv;
 if (nr > countof(H.root_tag)) { g->alloc(g, buf, 0); return NULL; }     // grew past the header table -> bump root_tag[]
 image_root_enc(g->symbols,        base, hp, &H.root_tag[0], &H.root_val[0]);
 image_root_enc((word) g->tasks,   base, hp, &H.root_tag[1], &H.root_val[1]);
 for (uintptr_t i = 0; i < nv; i++) image_root_enc(((word*) &g->v0)[i], base, hp, &H.root_tag[2 + i], &H.root_val[2 + i]);
 H.nroot = nr;
 memcpy(buf, &H, sizeof H);
 return *outlen = total, buf; }
void *ai_image_save(struct ai *g, uintptr_t *outlen) {
 if ((word*) g->sp != topof(g)) return NULL;             // quiescent: an empty AI stack at the dump point
 return ai_image_save_(g, outlen); }
// reconstruct a fresh g from a SAVE buffer ({header, blob} of byte length len); NULL on any problem.
// image-wake progress hook: a no-op everywhere, overridable by a port
// bringing the wake up on new metal (a crashed wake with no debugger is
// otherwise invisible -- the teensy lesson). Stages: 1 header ok, 2 pool
// ready, 3 blob in pool, 0x100+k walk progress (per 64K words), 5 walk
// done, 6 roots done.
__attribute__((weak)) void ai_image_note(uintptr_t stage) { (void) stage; }
struct ai *ai_image_load_m(void const *buf, uintptr_t len, void *(*al)(struct ai*, void*, size_t)) {
 struct image_hdr H;
 if (len < sizeof H) return NULL;
 memcpy(&H, buf, sizeof H);
 if (H.magic != IMAGE_MAGIC || H.wordsize != sizeof(word) || H.arch != IMAGE_ARCH) return NULL;
 uintptr_t nw = H.nwords, bytes = nw * sizeof(word);
 if (len < sizeof H + bytes) return NULL;                // truncated buffer
 ai_image_note(1);
 struct ai *g = ai_ini_m(al);
 if (!g) return NULL;
 if (nw > g->major_len) {                                // grow the major pool to fit the image
  g->alloc(g, g->major_pool, 0);
  g->major_len = nw + (nw >> 2);
  g->major_pool = g->major_base = g->alloc(g, NULL, 2 * g->major_len * sizeof(word));
  if (!g->major_pool) return NULL;
 }
 word *base = g->major_base;
 if (!base) return NULL;
 ai_image_note(2);
 memcpy(base, (char const*) buf + sizeof H, bytes);      // the blob -> the major pool
 g->major_hp = base + nw;
 ai_image_note(3);
 uintptr_t hb = bytes;
 intptr_t delta = (intptr_t)(word) image_immortals - (intptr_t) H.refsym;        // the ASLR shift dump->load (same binary, one PIE base)
 if ((H.rsv1 & 1) && !(H.rsv1 >> 1)) delta = 0;                                  // ZERO kept absolutes: fully symbolic image (offsets +
                                                                                 // table indices only) -- binary-PORTABLE, no delta to check
 else if ((intptr_t)((word) &ai_image_save - (intptr_t) H.anchor) != delta) return NULL; // anchor delta != refsym delta -> a DIFFERENT binary (cross-arch/stale) -> normal boot
 for (union u *p = (union u*) base; (word*) p < base + nw; ) {                    // re-walk the blob, decode in place (no tables)
  uintptr_t off = (uintptr_t)((word*) p - base), sz;
  if (!(off & 0xFFFFu)) ai_image_note(0x100 + (off >> 16));                       // walk progress, per 64K words
  ((word*) p)[0] = img_decode(((word*) p)[0], base, hb, delta);                   // word0 first: the ap (sizing needs it real)
  if (in_data(p->ap)) { sz = image_objsize(g, p);                                 // data kinds: size by ai_typ + raw length words
   switch (ai_typ(p)) {
    case KChain: ((word*) p)[1] = img_decode(((word*) p)[1], base, hb, delta);
                 ((word*) p)[2] = img_decode(((word*) p)[2], base, hb, delta); break;
    case KNom:   ((word*) p)[1] = img_decode(((word*) p)[1], base, hb, delta); break;
    case KVec:   if (vec(p)->type == ai_O) { word *e = (word*) vec_data(vec(p)); uintptr_t ne = vec_nelem(vec(p));
                  for (uintptr_t i = 0; i < ne; i++) e[i] = img_decode(e[i], base, hb, delta); }
                 break;
    default: break; }
  } else {                                                                        // thread: the ENCODED terminator is its head's byte offset | tag
   intptr_t term = (intptr_t)(off * sizeof(word) + ai_thread_tag); uintptr_t k = 1;
   uintptr_t kmax = (uintptr_t)(base + nw - (word*) p);                           // BOUND the scan: a mis-decoded word0 must refuse
   while (((word*) p)[k] != term) if (++k >= kmax) return NULL;                   // the load, never march off the pool (on metal the
   sz = k + 1;                                                                    // pool's edge is a dead bus, and a dead bus is MUTE)
   for (uintptr_t i = 1; i < sz; i++) ((word*) p)[i] = img_decode(((word*) p)[i], base, hb, delta); }
  p = (union u*) ((word*) p + sz); }
 ai_image_note(5);
 uintptr_t nv = (word*) g->end - (word*) &g->v0;                         // same struct/binary (anchor-checked) -> same layout
 if (H.nroot != 2 + nv) return NULL;                                     // root count mismatch -> stale/foreign image -> normal boot
 g->symbols = image_root_dec(H.root_tag[0], H.root_val[0], base);
 g->tasks   = (union u*) image_root_dec(H.root_tag[1], H.root_val[1], base);
 for (uintptr_t i = 0; i < nv; i++) ((word*) &g->v0)[i] = image_root_dec(H.root_tag[2 + i], H.root_val[2 + i], base);
 g->next_serial = H.next_serial;
 ai_image_note(6);
 // sp stays at ai_ini's topof(g) (empty AI stack); the dispatch re-establishes ip
 g->major_live0 = nw, g->since_major = 0;
 return g; }
struct ai *ai_image_load(void const *buf, uintptr_t len) { return ai_image_load_m(buf, len, ai_libc_alloc); }

// ============================================================================
// sym
// ============================================================================
// (intern s) -> the interned symbol named by string s; identity on any other arg.
// The empty spelling names nothing: (intern "") is the zero point () -- never an
// atom; the empty symbol IS the unit (() is +'s and *'s identity, applies const-1).
lvm(lvm_intern) {
 if (strp(Sp[0])) {
  if (Sp[0] == EmptyString) return Sp[0] = ZeroPoint, Ip += 1, Continue();  // (intern "") -> () (nil-ontology: the empty spelling is the zero point)
  word y;
  Have(intern_reserve(g));
  Pack(g), y = intern_checked(g, (struct ai_str*) g->sp[0]), Unpack(g);
  Sp[0] = y; }
 return Ip += 1, Continue(); }

// (mint _) -> a fresh POINT, adjoined to the value space: nameless, materially
// empty ($mint = 0, applies const-1 like every unit), identity its only
// property -- the arg is ignored. `code` gets THE MINT SERIAL (monotonic,
// pre-incremented) -- the point's hash and its place
// in the total order: mints order by creation, GC-stable. a named point is a KNom
// (its own kind), built by intern (canonical) or the `nom` nif (fresh/uninterned) --
// NOT a chain. mints still answer nomp (a nameless atom), so they bind as gensyms.
lvm(lvm_mint) {
 Have(Width(struct ai_mint));
 struct ai_mint *y = (struct ai_mint*) Hp;
 Hp += Width(struct ai_mint);                   // mints are uniform: ap, code
 ini_missing(y, ++g->next_serial);
 return
  Sp[0] = word(y),
  Ip += 1,
  Continue(); }

// (nom n) -> a FRESH, UNINTERNED named point (KNom): a string names it, a symbol
// lends its spelling, anything else falls to a bare mint. Distinct from intern's
// canonical noms -- two (nom 'x) are different points (distinct serials), so `nom`
// is the gensym-with-a-name. The McCarthy symbol, restored as its own atom.
lvm(lvm_nomctor) {
 Have(Width(struct ai_nom));                    // >= Width(struct ai_mint), so the bare-mint fallback fits too
 word n = Sp[0];                                // re-read post-GC (the stack is rooted)
 struct ai_str *nm = strp(n) ? str(n) : add_name(g, n);   // a string is the name; a sym lends its spelling
 if (!nm) return Ap(lvm_mint, g);               // no name -> a bare mint
 struct ai_nom *y = (struct ai_nom*) Hp; Hp += Width(struct ai_nom);
 ini_nom(y, word(nm), ++g->next_serial, nom_dig(word(nm)));
 return Sp[0] = word(y), Ip += 1, Continue(); }

struct ai *intern(struct ai*g) {
 if (ai_ok(g = ai_have(g, intern_reserve(g))))   // atom + (at the load factor) the doubled backing
  g->sp[0] = intern_checked(g, (struct ai_str*) g->sp[0]);
 return g; }

// avail must be >= Width(struct ai_mint) when this is called.
// how much a fresh intern may bump: the atom, plus -- when the table sits at
// the load factor -- the doubled backing it rehashes into. callers reserve
// this BEFORE intern_checked so the insert below never allocates (and so the
// string never moves mid-insert).
uintptr_t intern_reserve(struct ai *g) {
 word m = g->symbols;
 uintptr_t extra = m && (map_len(m) + 1) * 4 >= map_cap(m) * 3 ? 4 + 4 * map_cap(m) : 0;
 return Width(struct ai_nom) + extra; }   // a named symbol is one flat KNom (name + serial)

// intern: probe the WEAK intern map by string content (string hash + content
// equality -- the same value-keyed probe every map uses); a miss mints the
// canonical KNom -- a flat named point (name string + a fresh serial) -- and
// inserts it (keyed by the name). One canonical nom per spelling, so `idp` survives.
// avail must be >= intern_reserve(g) when this is called: bump-only in here.
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

// (nom? x): a REAL point -- a non-() mint or a named nom. () (the zero point, the
// absence a missing read returns) is the one point that is NOT nom? -- anonymous AND
// empty, it stands apart from every identity-bearing point. (name? below is narrower.)
op11(lvm_nomp, (nomp(Sp[0]) && Sp[0] != ZeroPoint) ? putcharm(1) : nil)
// (name? x): a NAMED point only (KNom) -- a nom with a spelling. name? => nom?; the gap
// nom? \ name? is the anonymous-but-real mints (gensyms).
op11(lvm_namep, namep(Sp[0]) ? putcharm(1) : nil)
op11(lvm_packp, (packp(Sp[0]) || flop(Sp[0]) || widep(Sp[0]) || Cp(Sp[0])) ? putcharm(1) : nil)  // the pack family: arrays + the lean float/wide/complex scalar boxes
op11(lvm_bigp, bigp(Sp[0]) ? putcharm(1) : nil)
op11(lvm_widep, widep(Sp[0]) ? putcharm(1) : nil)
op11(lvm_setp, arrp(Sp[0]) ? putcharm(1) : nil)
// (int x): truncate a float scalar to a fixnum; other numbers pass through. Used by
// num-ap to get an integer composition count from a non-integer numeral operator.
op11(lvm_intf, flop(Sp[0]) ? putcharm((intptr_t) flo_get(Sp[0])) : Sp[0])

// ============================================================================
// chain
// ============================================================================
op11(lvm_cap, chainp(Sp[0]) ? A(Sp[0]) : Sp[0])
op11(lvm_cup, chainp(Sp[0]) ? B(Sp[0]) : ZeroPoint)   // cup of an atom -> the const () (ZeroPoint), NOT the moving core (which had serial g->ip, not 0)
op11(lvm_books, g->book)   // the live layer chain (the abyss) -- runtime-internal, mopped at birth; ev.l's gv walks it
op11(lvm_setbooks, (g->book = Sp[0], nil))   // SET the layer chain: the scope-layer door (open/use/close ride it); runtime-internal, mopped at birth
// (mods _): the MODULE REGISTRY book (g->mods) -- name -> module-book, written by `leave` on a
// named scope, read by `use`/`from` (love/prel.l). A LAZY SINGLETON: created on the first read, so
// a bootstrap's two prel runs capture the SAME tablet and a pre-egg registration survives the
// egg warm. Runtime-internal (mopped at birth); the prel captures it pre-mop.
lvm(lvm_mods) {
 if (g->mods == nil) {
  uintptr_t cap = map_min_cap, nb = 4 + 2 * cap;
  Have(nb + 3);
  union u *b = map_fill_back((union u*) Hp, cap), *h = (union u*) (Hp + nb);
  h[0].ap = lvm_map_lookup, h[1].x = (word) b, tagthread(h, 2);
  Hp += nb + 3;
  g->mods = (word) h; }
 return Sp[0] = g->mods, Ip++, Continue(); }
// (lib _): the SOURCE LIBRARY book (g->lib) -- name -> source text, read by `use`'s miss
// lane (the loader) so a frontend can bake loadable modules with no filesystem. Filled
// from C by ai_lib_ below. The same lazy-singleton shape as mods, for the same reason.
lvm(lvm_lib) {
 if (g->lib == nil) {
  uintptr_t cap = map_min_cap, nb = 4 + 2 * cap;
  Have(nb + 3);
  union u *b = map_fill_back((union u*) Hp, cap), *h = (union u*) (Hp + nb);
  h[0].ap = lvm_map_lookup, h[1].x = (word) b, tagthread(h, 2);
  Hp += nb + 3;
  g->lib = (word) h; }
 return Sp[0] = g->lib, Ip++, Continue(); }
// register name -> source text in the library, from C (a frontend's boot). The love-side
// twin of ai_defn: push the singleton, the value (the source, as a string), the interned
// name; ai_mapput leaves the map on the stack, dropped here.
struct ai *ai_lib_(struct ai *g, char const *nm, char const *src) {
 if (!ai_ok(g)) return g;
 if (ai_core_of(g)->lib == nil) {
  if (!ai_ok(g = map_new(g))) return g;                // pushes the fresh map
  ai_core_of(g)->lib = *ai_core_of(g)->sp;
  ai_core_of(g)->sp++; }
 g = ai_push(g, 1, ai_core_of(g)->lib);
 g = ai_mapput(intern(ai_strof(ai_strof(g, src), nm)));
 if (ai_ok(g)) ai_core_of(g)->sp++;
 return g; }
// push a fresh writable LAYER as the head of the book chain -- the runtime's own
// enter, now that the nom is mopped at birth: the session's scope, every defglob's
// target. no stash and no macro slot (`::` creates one on demand); reads walk down
// as ever, so the base is read-only for the plainest reason -- never the head.
struct ai *ai_layer_(struct ai *g) {
 if (!ai_ok(g)) return g;
 if (!ai_ok(g = map_new(g))) return g;                 // sp[0] = the fresh layer map
 g = gxr(ai_push(g, 1, ai_core_of(g)->book));          // (layer . chain)
 if (!ai_ok(g)) return g;
 ai_core_of(g)->book = *ai_core_of(g)->sp;
 return ai_pop(g, 1); }
// drop the link just below the head -- the runtime's own bare leave, the exact
// inverse of one `use`: boot brackets a non-ambient module load (holo) with
// ai_evals_("(use 'x)") .. ai_unsplice_. nothing below the head is a no-op.
struct ai *ai_unsplice_(struct ai *g) {
 if (!ai_ok(g)) return g;
 word bk = ai_core_of(g)->book;
 if (!chainp(B(bk))) return g;
 g = gxl(ai_push(g, 2, A(bk), B(B(bk))));              // (head . below-the-neighbour)
 if (!ai_ok(g)) return g;
 ai_core_of(g)->book = *ai_core_of(g)->sp;
 return ai_pop(g, 1); }
op11(lvm_chainp, (chainp(Sp[0]) && !nomp(Sp[0])) ? putcharm(1) : nil)  // the SURFACE chainp = a real compound list (formp): a named symbol is (name . mint) but counts as an atom
lvm(lvm_link) {
 Have(Width(struct ai_chain));
 struct ai_chain *w = (struct ai_chain*) Hp;
 Hp += Width(struct ai_chain);
 ini_chain(w, Sp[0], Sp[1]);
 *++Sp = word(w);
 Ip++;
 return Continue(); }

#define avm_slow(op, vop, ovf, fexpr) static lvm(lvm_##op##n) { \
 word a = Sp[0], b = Sp[1]; \
 if (arrp(a) || arrp(b)) return Ap(lvm_vbin, g, vop); \
 if (Cp(a) || Cp(b)) return Ap(lvm_cplx_bin, g, vop); \
 if (!isnum(a) || !isnum(b)) return *++Sp = ZeroPoint, Ip++, Continue(); \
 if (flop(a) || flop(b)) { word _res; Have(box_req); \
  ai_flo_t ad = toflo(a), bd = toflo(b); \
  emit_flo(fexpr); \
  return *++Sp = _res, Ip++, Continue(); } \
 if (!bigp(a) && !bigp(b)) { intptr_t av = toint(a), bv = toint(b), t; \
  if (!ovf(av, bv, &t)) { word _res; Have(box_req); emit_int(t); \
   return *++Sp = _res, Ip++, Continue(); } } \
 if ((vop) == vop_mul) return Ap(lvm_bmul_start, g); /* O(n^2): run yieldable */ \
 Pack(g); g = ai_big_binop(g, vop); \
 if (!ai_ok(g)) return ghelp(g); \
 return Unpack(g), Continue(); }
#define avm_slowdiv(op, vop, c_op, fexpr) static lvm(lvm_##op##n) { \
 word a = Sp[0], b = Sp[1]; \
 if (arrp(a) || arrp(b)) return Ap(lvm_vbin, g, vop); \
 if (Cp(a) || Cp(b)) return Ap(lvm_cplx_bin, g, vop); \
 if (!isnum(a) || !isnum(b)) return *++Sp = ZeroPoint, Ip++, Continue(); \
 if (flop(a) || flop(b) || b == nil) { word _res; Have(box_req); \
  ai_flo_t ad = toflo(a), bd = toflo(b); \
  emit_flo(fexpr); \
  return *++Sp = _res, Ip++, Continue(); } \
 if (!bigp(a) && !bigp(b)) { intptr_t av = toint(a), bv = toint(b); \
  if (!(av == INTPTR_MIN && bv == -1)) { word _res; Have(box_req); emit_int(av c_op bv); \
   return *++Sp = _res, Ip++, Continue(); } } \
 return Ap(lvm_bdiv_start, g, vop); }   /* big // and % run yieldable (resumable long division) */
// A bare mint -- the zero point () too -- RIDES THROUGH every dyadic arithmetic
// lane, either side: the unit is the DO-NOTHING operand (the same law as
// lvm_add/lvm_mul's identity lanes), so x - () = () - x = x, and likewise
// / // % & | ^ << >>. Not the number 0 -- no face projection, the op just never
// happens -- so an undefined op's () vanishes when chained through ANY of these,
// exactly as it does through + and *. Comparisons and `=` stay strict: () keeps
// its floor seat in the total order.
#define avm_unit(a, b) \
 if (mintp(a)) return *++Sp = b, Ip++, Continue(); \
 if (mintp(b)) return *++Sp = a, Ip++, Continue()
#define avm_div(op, c_op) lvm(lvm_##op) { \
 word a = Sp[0], b = Sp[1]; \
 if (charmp(a) && charmp(b)) { \
  intptr_t av = getcharm(a), bv = getcharm(b); \
  if (bv != 0 && !(av == INTPTR_MIN && bv == -1)) { \
   intptr_t t = av c_op bv; \
   if (t >= fix_min && t <= fix_max) \
    return *++Sp = putcharm(t), Ip++, Continue(); } } \
 avm_unit(a, b); \
 return Ap(lvm_##op##n, g); }
// the ordered comparisons (< <= > >=) and their total order over all values are
// defined after vcmp_int/vcmp_flo (the per-op helpers they reuse), by lvm_vbin.
#define bit_slow(n, c_op) static lvm(lvm_##n##_slow) {               \
 word a = Sp[0], b = Sp[1], _res;                                     \
 if (!(charmp(a) || widep(a)) || !(charmp(b) || widep(b)))                  \
  return *++Sp = ZeroPoint, Ip++, Continue();                               \
 Have(box_req);                                                       \
 emit_int(toint(a) c_op toint(b));                                    \
 return *++Sp = _res, Ip++, Continue(); }
#define mvm1(n) lvm(lvm_##n) { return Ap(lvm_math1, g, ai_##n); }
#define m1(_) _(sin) _(cos)   // sqrt/exp/tan/atan derived; sin/cos/log are the kept transcendentals (log has its own ap)


avm_slow(add, vop_add, __builtin_add_overflow, ad + bd)
avm_slow(sub, vop_sub, __builtin_sub_overflow, ad - bd)
avm_slow(mul, vop_mul, __builtin_mul_overflow, ad * bd)

avm_slowdiv(fquot, vop_fquot, /, ai_trunc(ad / bd))  // `//` truncating: float operand floors toward zero
avm_slowdiv(rem, vop_rem, %, ai_fmod(ad, bd))    // NaN on bd == 0

// `/` true division: exact integer when b divides a, a float box otherwise (the
// truncating quotient is `//`, lvm_fquot above). Mirrors avm_slowdiv's lane order
// -- array / complex / non-num / float-or-÷0 -- then splits the integer lane on
// divisibility. The bignum lane defers to ai_big_quot_true (same exact-or-float test).
static lvm(lvm_quotn) {
 word a = Sp[0], b = Sp[1];
 if (arrp(a) || arrp(b)) return Ap(lvm_vbin, g, vop_quot);
 if (Cp(a) || Cp(b)) return Ap(lvm_cplx_bin, g, vop_quot);
 if (!isnum(a) || !isnum(b)) return *++Sp = ZeroPoint, Ip++, Continue();
 if (flop(a) || flop(b) || b == nil) { word _res; Have(box_req);   // ±inf/NaN on ÷0
  ai_flo_t ad = toflo(a), bd = toflo(b);
  emit_flo(ad / bd);
  return *++Sp = _res, Ip++, Continue(); }
 if (!bigp(a) && !bigp(b)) { intptr_t av = toint(a), bv = toint(b);  // bv != 0 (b != nil)
  if (!(av == INTPTR_MIN && bv == -1)) {                            // INT_MIN/-1 is exact but overflows -> bignum lane
   if (av % bv == 0) { word _res; Have(box_req); emit_int(av / bv);
    return *++Sp = _res, Ip++, Continue(); }
   word _res; Have(box_req);                                        // inexact -> promote to float
   emit_flo((ai_flo_t) av / (ai_flo_t) bv);
   return *++Sp = _res, Ip++, Continue(); } }
 Pack(g); g = ai_big_quot_true(g);
 if (!ai_ok(g)) return ghelp(g);
 return Unpack(g), Continue(); }

// `-`: fixnum fast path, the () unit, then coins (the die's SUB method, slot 5 --
// `-` has no kind matrix, so the coin interception the +/* matrices route through
// lvm_addh/lvm_mulh lives right here), then the numeric slow lane.
lvm(lvm_sub) {
 word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) { intptr_t t;
  if (!__builtin_sub_overflow((intptr_t) getcharm(a), (intptr_t) getcharm(b), &t) &&
      t >= fix_min && t <= fix_max)
   return *++Sp = putcharm(t), Ip++, Continue(); }
 avm_unit(a, b);
 if (coinp(a) || coinp(b)) return Ap(lvm_sub_coin, g);
 return Ap(lvm_subn, g); }
// lvm_mul + its kind matrix live after the `+` string lane (they reuse add_name /
// stringrank for the symbol-repetition case), below.

// `+` is overloaded: still arithmetic add, but generic over strings and lists,
// where it is order-preserving concatenation -- a precedes b in the result, with a
// scalar lifted into the sequence on the side it appears (left -> front, right ->
// back). This is deliberately noncommutative (unlike numeric add): the written
// order survives, since concatenation is inherently ordered. (nil counts as the
// number 0 here, not the empty list.)
//   str + str   -> byte concat            list + list -> spine append
//   str + list  -> (link str list)        list + str  -> (append list (list str))
//   num + str   -> byte at front          str + num   -> byte at back
//   num + list  -> (link num list)        list + num  -> (append list (list num))
// RUNTIME FLAG: ai_add_lr selects order-preserving (left->front, right->back); set
// it false for the commutative reading (smaller operand always joins the front, so
// a+b == b+a like numeric add). A plain mutable global -> toggleable at runtime.
// FIXME we always want commutative reading so make this false then remove
static const bool ai_add_lr = true;
// THE BYTE LAW: text + number is one byte, STRICTLY -- the value must be an exact
// integer 0..255 (rep-blind, like `=`: 66.0 is 66; a wide box/bignum is never in
// range), anything else nil, like `-` on strings. Returns the byte or -1.
static ai_inline intptr_t seq_byte(word x) {
 if (charmp(x)) { intptr_t v = getcharm(x); return v < 0 || v > 255 ? -1 : v; }
 if (flop(x)) { ai_flo_t f = flo_get(x);
  if (!(f >= 0 && f <= 255)) return -1;                 // range first (nan fails); cast below is safe
  return f != (ai_flo_t) (intptr_t) f ? -1 : (intptr_t) f; }
 return -1; }
// LIST lane: at least one operand is a chain (the matrix only routes list-involved
// chains here). list+list -> spine append; elt<->list -> the non-list operand joins
// as a scalar element (front if it is on the left, else appended at the tail).
static lvm(lvm_add_seq) {
 // a named symbol is a (name . mint) chain, but for + it is an ATOM (an element to adjoin,
 // like a number), NOT a list to append -- so the "is this a list" tests use formp, not the
 // raw chainp macro. sym + real-list adjoins (`'ef + '(1 2)` = `(ef 1 2)`); sym + sym/str/num
 // falls through to nil (no symbol string algebra). spine-walks stay chainp (real lists only).
 word a = Sp[0], b = Sp[1];
 if (formp(a) && formp(b)) {                         // list + list -> append a..b
  uintptr_t n = llen(a); Have(n * Width(struct ai_chain));
  a = Sp[0], b = Sp[1];
  struct ai_chain *base = (struct ai_chain*) Hp, *w = base;
  Hp += n * Width(struct ai_chain);
  for (word l = a; chainp(l); l = B(l), w++) ini_chain(w, A(l), word(w + 1));
  (w - 1)->b = b;                                // last cdr -> b
  return *++Sp = word(base), Ip++, Continue(); }
 if (formp(a) || formp(b)) {                          // elt <-> list (a bare mint never
  bool front = !ai_add_lr || formp(b);               // reaches here -- lvm_add's identity early-out caught it)
  word lst = formp(a) ? a : b, elt = formp(a) ? b : a;
  if (front) { Sp[0] = elt, Sp[1] = lst; return Ap(lvm_link, g); }  // (link elt list)
  uintptr_t n = llen(lst) + 1; Have(n * Width(struct ai_chain));        // append elt at tail
  lst = formp(Sp[0]) ? Sp[0] : Sp[1], elt = formp(Sp[0]) ? Sp[1] : Sp[0];
  struct ai_chain *base = (struct ai_chain*) Hp, *w = base;
  Hp += n * Width(struct ai_chain);
  for (word l = lst; chainp(l); l = B(l), w++) ini_chain(w, A(l), word(w + 1));
  ini_chain(w, elt, ZeroPoint);                     // trailing (elt . ()) -- list terminator (nil-ontology)
  return *++Sp = word(base), Ip++, Continue(); }
 return *++Sp = ZeroPoint, Ip++, Continue(); }          // neither is a real list (e.g. sym + sym/str/num): no algebra -> nil

// --- TEXT lane: strings + symbols, name-compatible -------------------------
// The string tower is STRING (rank 0) < UNINTERNED-SYM (1) < INTERNED-SYM (2). A
// symbol's bytes are its name (anonymous -> empty, like ""); a number contributes
// one byte (seq_byte) and sits at the top rank, so min() keeps its partner's type
// (a scalar lifts into whatever it joins). Mixing demotes to the lower rank:
// isym+usym -> usym, sym+str -> str, num+sym -> sym (lifted), num+str -> str. The
// concat is built as one string in operand order, then returned per the result
// rank: string as-is / nom'd to a fresh uninterned sym / interned. An empty
// result is the ai_str_empty singleton, or nil at symbol rank (the symbol
// lane is gated off by the dispatch matrix since the mint round anyway).
static ai_inline struct ai_str *add_name(struct ai *g, word x) {   // symbol -> name string, or 0 (a bare mint / the zero point / a non-symbol)
 return namep(x) ? str(nom(x)->name) : 0; }  // a named point (KNom) carries its name; a bare mint is nameless
static ai_inline int stringrank(struct ai *g, word x) {    // STR 0 / mint 1 / NAMED-sym|NUM 2
 if (strp(x)) return 0;
 if (namep(x)) return 2;          // a NAMED symbol: result re-interns (the min pulls a string operand to 0 -> demote)
 if (mintp(x)) return 1;          // a bare mint / the zero point: an uninterned (fresh) symbol
 return 2; }                      // a number contributes one byte (rank 2)
static ai_inline uintptr_t stringlen(struct ai *g, word x) {  // bytes x contributes to a concat
 if (strp(x)) return len(x);
 if (nomp(x)) { struct ai_str *n = add_name(g, x); return n ? n->len : 0; }
 return 1; }                                            // number -> one byte
static ai_inline char *add_emit(struct ai *g, char *w, word x) {  // append x's bytes; return advanced w
 if (strp(x)) return (void) memcpy(w, txt(x), len(x)), w + len(x);
 if (nomp(x)) { struct ai_str *n = add_name(g, x);
  return n ? ((void) memcpy(w, txt(n), n->len), w + n->len) : w; }
 return *w = (char) seq_byte(x), w + 1; }               // number -> one byte (gated >= 0 by the byte law)
static lvm(lvm_add_string) {
 word a = Sp[0], b = Sp[1];
 if (arrp(a) || arrp(b)) return *++Sp = ZeroPoint, Ip++, Continue(); // array <-> string: undefined
 if (!strp(a) && !nomp(a) && seq_byte(a) < 0) return *++Sp = ZeroPoint, Ip++, Continue();  // the byte law
 if (!strp(b) && !nomp(b) && seq_byte(b) < 0) return *++Sp = ZeroPoint, Ip++, Continue();
 int rank = min(stringrank(g, a), stringrank(g, b));
 uintptr_t n = stringlen(g, a) + stringlen(g, b);
 if (!n) return *++Sp = rank ? nil : EmptyString, Ip++, Continue();
 uintptr_t req = str_type_width + b2w(n);
 Have(req);
 a = Sp[0], b = Sp[1];                                  // re-read post-GC
 struct ai_str *z = ini_str((struct ai_str*) Hp, n); Hp += req;
 add_emit(g, add_emit(g, txt(z), a), b);                      // a's bytes then b's, in order
 *++Sp = word(z);
 return rank == 0 ? (Ip++, Continue())                  // string
      : rank == 1 ? Ap(lvm_mint, g)                  // uninterned symbol (fresh)
                  : Ap(lvm_intern, g); }               // interned symbol
static lvm(lvm_0) {                             // unsupported mix (array <-> string)
 return *++Sp = ZeroPoint, Ip++, Continue(); }
// the UNIT lane: a bare mint rides through +/* untouched (() + x = x, x * () = x).
// The dispatchers early-out a mint BEFORE indexing the matrices (the fast path),
// so these cells are belt and braces -- but they say the TRUE thing, so the
// matrix stands correct on its own and the whole square is symmetric, KMint
// included (mx.v checks it; the mint row/col was asymmetric dead cells before).
static lvm(lvm_bin_unit) {
 word a = Sp[0], b = Sp[1];
 return *++Sp = mintp(a) ? b : a, Ip++, Continue(); }

// The fundamental value kind for generic-op dispatch (enum q in love.h): a fixnum is
// the odd tag (KCharm), a non-data heap pointer is a thread/function (KHot), else ai_typ
// gives the data kind. The refinement: a vec is always a rank>=1 array now (the
// scalar gems wide/float/complex carry their own sentinels and ai_typ gives them
// KWide/KFlo/KCplx directly), so a vec expands purely by element tier to
// KArrZ..KArrO. ai_typ gives the coarse KVec; ai_kind splits it across the row.
// Exported (not inline) so data.c's apply sentinels share it.
enum q ai_kind(word x) {
 if (charmp(x)) return KCharm;
 if (!datp(x)) return tabp(x) ? KMap : KHot;
 enum q k = typ(x);
 if (k != KVec) return k;
 return (enum q) (KArrZ + vec(x)->type); }

// ============================================================================
// generic-op lane aps, then all three dispatch matrices adjacent, then the
// `+`/`*` dispatchers. The numeric slow lanes (addn/muln…) come from the AVM_*
// macros above; the `+` string lanes (add_seq/add_string) and lvm_0 just above; the
// lambda combinators (lvm_addh/lvm_mulh) near num-ap. Defined here: the `*`
// repeat lane and the apply aps -- everything the matrices reference.
// ============================================================================

// `*` REPEAT lane: the multiplicative analog of `+`'s concat. `*` is "repeated
// `+`": a sequence (string / symbol / list) times a scalar count n is n copies
// joined, just as `(* 2 3)` is 2+2+2. The count is the OTHER operand, SATURATED
// to a green charm (($ c) -- the count law, shared with numeral-apply and array shapes:
// non-positive -> 0 -> the empty singleton, a float ceils, a complex counts by
// total-order-guarded modulus); an array (or any non-number) is not a count ->
// nil. A symbol is a (name . mint) chain but has NO * (repeat) algebra -- it is neither a
// real list nor a string here, so it nils out (intern/string are the bridge).
static lvm(lvm_mul_rep) {
 word a = Sp[0], b = Sp[1];
 bool aseq = strp(a) || formp(a) || namep(a);       // a string / list / NAMED symbol repeats
 word seq = aseq ? a : b, cnt = aseq ? b : a;
 if ((!strp(seq) && !formp(seq) && !namep(seq)) || (!isnum(cnt) && !Cp(cnt)))
  return *++Sp = ZeroPoint, Ip++, Continue();             // seq not a sequence/symbol, or count not a number
 uintptr_t n = (uintptr_t) ai_pin(g, cnt);
 if (formp(seq)) {                                   // list -> n copies of the spine
  if (!n) return *++Sp = ZeroPoint, Ip++, Continue();   // 0 copies -> the empty list () (nil-ontology)
  uintptr_t m = llen(seq), total = m * n;
  Have(total * Width(struct ai_chain));
  seq = formp(Sp[0]) ? Sp[0] : Sp[1];                // re-read post-GC
  struct ai_chain *base = (struct ai_chain*) Hp, *w = base;
  Hp += total * Width(struct ai_chain);
  for (uintptr_t i = 0; i < n; i++)
   for (word l = seq; chainp(l); l = B(l), w++) ini_chain(w, A(l), word(w + 1));
  (w - 1)->b = ZeroPoint;                            // list terminator () (nil-ontology)
  return *++Sp = word(base), Ip++, Continue(); }
 // string / symbol spelling -> repeat the bytes; a symbol RE-INTERNS the result
 bool sym = namep(seq);
 struct ai_str *src = sym ? str(nom(seq)->name) : str(seq);
 uintptr_t sl = src->len, total = sl * n;
 if (!total) return *++Sp = sym ? ZeroPoint : EmptyString, Ip++, Continue();  // 0 copies: () for a sym, "" for a string
 uintptr_t req = str_type_width + b2w(total);
 Have(req);
 word sw = sym ? (namep(Sp[0]) ? Sp[0] : Sp[1]) : (strp(Sp[0]) ? Sp[0] : Sp[1]);  // re-read post-GC
 src = sym ? str(nom(sw)->name) : str(sw);
 struct ai_str *z = ini_str((struct ai_str*) Hp, total); Hp += req;
 for (uintptr_t i = 0; i < n; i++) memcpy(txt(z) + i * sl, txt(src), sl);
 *++Sp = word(z);
 return sym ? Ap(lvm_intern, g) : (Ip++, Continue()); }

// `*` CARTESIAN lane: chain * chain -> the ordered cartesian product, the semiring
// product whose `+` is `cat`. Each pair is a proper 2-list (love bj), so the product
// of an m-list and an n-list is an (m*n)-list of pairs -- tally is the homomorphism
// (tally(a*b) = tally a * tally b), and the outer loop ranges the LEFT operand so
// right-distributivity (a+b)*c = a*c + b*c holds on the nose (left-distributivity
// holds up to a permutation -- intrinsic to ordered pairs). () is the annihilator
// (an empty operand can't reach here -- it's a mint, caught by lvm_mul's identity
// early-out -- but llen 0 -> () keeps the lane total). Layout: `pairs` outer spine
// cells, then 2*pairs pair cells; 3*pairs chains total, one Have.
static lvm(lvm_mul_cart) {
 word a = Sp[0], b = Sp[1];
 if (!formp(a) || !formp(b)) return *++Sp = ZeroPoint, Ip++, Continue();   // chain*chain only
 uintptr_t m = llen(a), n = llen(b), pairs = m * n;
 if (!pairs) return *++Sp = ZeroPoint, Ip++, Continue();             // empty operand annihilates
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
   ini_chain(p0, av, word(p1));                                      // (love bj)
   ini_chain(spine + idx, word(p0), idx + 1 < pairs ? word(spine + idx + 1) : ZeroPoint); } }
 return *++Sp = word(spine), Ip++, Continue(); }

// --- apply lane (the data-value `(g x)` aps; moved here from data.c) -----
// When a data value is applied, its sentinel (a data sentinel above) tail-jumps
// STRAIGHT to one of these handlers -- the sentinel encodes the kind, so there is
// no table. Every data kind has a meaningful apply (chain = eliminator,
// string/symbol = byte index, numeric tower = Church numeral); opaque handles
// (ports, buffers) behave as 0 via their own lvm_* sentinel, not through here.
// Maps look up via lvm_map_lookup (a thread ap, not a data sentinel).

// (s k): applying a string indexes it -- k a byte offset, result the unsigned byte
// 0..255 there, 1 if k is non-numeric or out of range (matches "" == 0: a numeric
// ("" k) is the Church numeral k**0 == 1). No alloc, unwinds like self-quote.
static lvm(data_string_apply) {
 word k = Sp[0], v = putcharm(1), n;
 if (oddp(k) && (n = getcharm(k)) >= 0 && n < (word) len(Ip))
  v = putcharm((unsigned char) txt(Ip)[n]);
 return Ip = cell(*++Sp), *Sp = v, Continue(); }

// (y k): applying a symbol indexes its underlying name string, so (y k) == (nom k).
// nom encodes the kind: a string is the name (interned), a symbol is the naming
// symbol of a named-uninterned sym (follow once to its string nom), 0 is an anonymous
// nom. With no underlying string we act like 0 (absent name == "" == 0 -> 1).
// applying a symbol: the name-index is RELEASED (the string semantics left the
// symbols with the mint round) -- a symbol is a point with a spelling attribute,
// and a point applies as every unit does: const-1, like 0, (), and the hots.
static lvm(data_sym_apply) {
 return Ip = cell(*++Sp), *Sp = putcharm(1), Continue(); }

// (n x): applying a number is Church-numeral application, like a fixnum (cf.
// lvm_numap). Fixnums reach num-ap via the odd-tag check in lvm_ap; the rest of the
// tower (floats, boxes, complex, arrays -- all lvm_vec -- and bignums) are heap
// pointers, so they arrive at their data sentinel. We lay the same [n, num-ap, x, ret]
// frame and run numap_drive, handing the boxed operator n to the l num-ap ap,
// which picks exponentiate / compose / self by operand+operator kind.
static lvm(data_num_apply) {
 Have(2);
 word h = hot_hook(g->hot_numap);
 word n = word(Ip), x = Sp[0], ret = Sp[1], *dst = Sp - 2;
 dst[0] = n, dst[1] = h, dst[2] = x, dst[3] = ret;
 return Sp = dst, Ip = (union u*) numap_drive, Continue(); }

// ((a . b) g) == (g a b): a chain is its own Church eliminator (link = \a b g.g a b).
// Re-enter the apply protocol via a static driver thread: lay the stack as the two
// curried calls expect, then [ap ; swap+ap ; ret0] runs ((g a) b). pair_swap reorders
// [result, b] -> [b, result] so the second ap sees arg=b, fn=(g a). The driver lives
// in .data, so the return addresses it leaves on the stack fall outside the GC pool.
static lvm(pair_swap) {
 word t = Sp[0]; Sp[0] = Sp[1], Sp[1] = t;
 return Ap(lvm_ap, g); }
static union u const pair_drive[] = { {lvm_ap}, {.ap = pair_swap}, {.ap = lvm_ret0} };
static lvm(data_pair_apply) {
 // A chain is its OWN Church eliminator. Named syms no longer reach here -- they are
 // KNom now, applying as const-1 through their own apply row (data_sym_apply) -- so the
 // old (name . mint)-chain special-case is gone: every chain here is a real compound.
 Have(2);
 word a = A(Ip), b = B(Ip), fn = Sp[0];     // re-read after the Have guard; no alloc past here
 Sp -= 2;                                    // grow the frame to [a, fn, b, ret]
 Sp[0] = a, Sp[1] = fn, Sp[2] = b;           // Sp[3] = ret (was Sp[1]) stays put
 return Ip = (union u*) pair_drive, Continue(); }

// === the two generic-op dispatch matrices (+ and *), adjacent ==============
// All indexed by ai_kind. The kind
// order (love.h) makes each lane a contiguous block: [KCharm..KArrO] arithmetic (the
// scalar GEM tower charm/wide/float/complex/big, the vec sentinel, then the parallel
// array tower arrZ/arrR/arrC/arrO), then [KString..KChain] sequence, then KMap, then KHot.
// The rows below are NAMED-index (NUMK + the five) -- adding a kind can't shift a column.
// Lanes:
//   *n   = numeric tower & arrays (arithmetic / broadcast) -- the lane ap still
//          refines by ai_vec_type; every gem/array kind routes identically (NUMK).
//   add_seq = a list anywhere (other operand a scalar element / spine); chain wins
//   add_string = strings (+ a number as one byte -- the byte law; nils an array
//              operand internally). SYMBOLS left the string algebra with the mint
//              round: their string cells are lvm_0 (intern/string = the explicit bridge)
//   mul_rep  = sequence * scalar-count -> repetition
//   *l   = a LAMBDA-or-MAP operand (precedence: the KMap/KHot rows+cols) -- Church
//          add / compose; a map IS a lookup lambda for +/*, kept deliberately, so
//          its rung shares the lanes (the rung exists for the order)
//   lvm_0 = undefined (-> nil): sequence*sequence
// Precedence (high->low): lambda > map > chain > text > number(incl array).

// The two tables themselves are GENERATED, and love.c's only generated region:
// tools/mx.l holds them as love data and lays this header through clay (the C-as-
// love-data layer, doc/clay.md). ONE datum feeds both the C below and the Rocq
// model proof/rocq/mx.v, so the theorem and the code cannot drift -- where mx.v
// used to reach these statics through tools/mxdump.c, a translation unit that
// #include'd this file WHOLE and named each lane by comparing function pointers.
// EDIT tools/mx.l, NOT mx.h; `make test_clay` regenerates and fails on drift.
#include "mx.h"
// (apply is no longer a matrix: each data sentinel tail-jumps straight to its
// handler -- see the apply lane above. The apply was uniform in the argument kind,
// so the 2-D table was pure indirection; a handler that cares about the arg kind
// re-inspects it itself.)

// === the `+`/`*` dispatchers (fixnum fast path, then the matrix) ============
lvm(lvm_add) {
 word a = Sp[0], b = Sp[1]; intptr_t t;
 if (charmp(a) && charmp(b)
     && !__builtin_add_overflow((intptr_t) getcharm(a), (intptr_t) getcharm(b), &t)
     && t >= fix_min && t <= fix_max)
  return *++Sp = putcharm(t), Ip++, Continue();
 // a bare mint -- the zero point () too -- is +'s IDENTITY in every lane (not just on
 // lists): it nets 0, nothing adjoins nothing, so () + x = x + () = x for all x. (mintp is
 // false for a NAMED symbol, so it skips this unit lane and coerces just below.) The
 // matrix says the same thing (its mint row/col is lvm_bin_unit), so this is the fast
 // path, never load-bearing.
 if (mintp(a)) return *++Sp = b, Ip++, Continue();
 if (mintp(b)) return *++Sp = a, Ip++, Continue();
 return Ap(ai_add_mx[ai_kind(a)][ai_kind(b)], g); }
lvm(lvm_mul) {
 word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) { intptr_t t;
  if (!__builtin_mul_overflow((intptr_t) getcharm(a), (intptr_t) getcharm(b), &t)
      && t >= fix_min && t <= fix_max)
   return *++Sp = putcharm(t), Ip++, Continue(); }
 // a bare mint -- the zero point () too -- is *'s IDENTITY: a do-nothing UNIT, () * x =
 // x * () = x for all x. () is treated as the unit here (not the number 0, which would
 // ANNIHILATE), the same skip-me role it plays for + -- so it OVERRIDES the count lane:
 // `"ab" * ()` is `"ab"` (one copy), distinct from `"ab" * 0` = "".
 if (mintp(a)) return *++Sp = b, Ip++, Continue();
 if (mintp(b)) return *++Sp = a, Ip++, Continue();
 return Ap(ai_mul_mx[ai_kind(a)][ai_kind(b)], g); }

avm_div(fquot, /)                               // `//` fixnum fast path: truncating quotient
avm_div(rem, %)
// `/` fixnum fast path: stay exact only when b divides a; otherwise the slow lane
// promotes to a float box. The INT_MIN/-1 guard precedes the `%` (it would be UB).
lvm(lvm_quot) {
 word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) { intptr_t av = getcharm(a), bv = getcharm(b);
  if (bv != 0 && !(av == INTPTR_MIN && bv == -1) && av % bv == 0) {
   intptr_t t = av / bv;
   if (t >= fix_min && t <= fix_max) return *++Sp = putcharm(t), Ip++, Continue(); } }
 avm_unit(a, b);
 if (coinp(a) || coinp(b)) return Ap(lvm_quot_coin, g);   // the die's DIV method, slot 8
 return Ap(lvm_quotn, g); }

// The ordered comparisons (lvm_lt/le/gt/ge) and their total order are defined
// after vcmp_int/vcmp_flo (the per-op trichotomy helpers), near lvm_vbin.

// Bitwise and/or/xor: fast both-fixnum tag trick (two odds stay odd under &
// and |; ^ clears the tag bit so we re-set it). A box operand routes to the
// slow ap, which works at full width and demotes-or-boxes; these are
// integer-only, so a float (or any non-integer) operand yields nil.
bit_slow(band, &) bit_slow(bor, |) bit_slow(bxor, ^)
lvm(lvm_band) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) return *++Sp = (a & b) | 1, Ip++, Continue();
 avm_unit(a, b);
 if (arrp(a) || arrp(b)) return Ap(lvm_vbin, g, vop_band);
 return Ap(lvm_band_slow, g); }
lvm(lvm_bor) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) return *++Sp = (a | b) | 1, Ip++, Continue();
 avm_unit(a, b);
 if (arrp(a) || arrp(b)) return Ap(lvm_vbin, g, vop_bor);
 return Ap(lvm_bor_slow, g); }
lvm(lvm_bxor) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) return *++Sp = (a ^ b) | 1, Ip++, Continue();
 avm_unit(a, b);
 if (arrp(a) || arrp(b)) return Ap(lvm_vbin, g, vop_bxor);
 return Ap(lvm_bxor_slow, g); }
// (bitwise complement is `(^ x -1)`; logical not is the `!` reader sigil / `nilp`.)

// >> : arithmetic right shift. A fixnum value only shrinks, so it keeps a
// non-allocating fast path; a boxed value routes to the slow ap.
static lvm(lvm_bsr_slow) { word a = Sp[0], b = Sp[1], _res;
 if (!(charmp(a) || widep(a)) || !charmp(b)) return *++Sp = ZeroPoint, Ip++, Continue();
 Have(box_req);
 emit_int(toint(a) >> shmask(getcharm(b)));
 return *++Sp = _res, Ip++, Continue(); }
lvm(lvm_bsr) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b))
  return *++Sp = putcharm(getcharm(a) >> shmask(getcharm(b))), Ip++, Continue();
 avm_unit(a, b);
 if (arrp(a) || arrp(b)) return Ap(lvm_vbin, g, vop_bsr);
 return Ap(lvm_bsr_slow, g); }

// << : can overflow the tag, so it always runs through the box/demote path
// (emit_int still demotes small results — only genuinely wide values
// allocate). Shift done in uintptr_t for well-defined overflow.
lvm(lvm_bsl) { word a = Sp[0], b = Sp[1], _res;
 avm_unit(a, b);
 if (arrp(a) || arrp(b)) return Ap(lvm_vbin, g, vop_bsl);
 if (!(charmp(a) || widep(a)) || !charmp(b)) return *++Sp = ZeroPoint, Ip++, Continue();
 Have(box_req);
 emit_int((intptr_t)((uintptr_t) toint(a) << shmask(getcharm(b))));
 return *++Sp = _res, Ip++, Continue(); }

op(lvm_charmp, 1, oddp(Sp[0]) ? putcharm(1) : nil)   // (charm? x): a fixnum -- a charm, the tagged odd word
// `nilp`/`not`: the falsy predicate -- the efficient generic form of (= 0 ($ x)),
// via ai_nilp, which reads the net's sign without the clamp (a sym/string/fn is truthy
// with no walk). The single truthiness oracle: `?` (lvm_cond), nilp, and aall all
// consult ai_nilp, so `(? (nilp e) a b)` == `(? e b a)` -- the feel pass drops such a
// nilp wrapper. Use `(= x 0)` for a literal scalar-zero test.
op11(lvm_nilp, ai_nilp(g, Sp[0]) ? putcharm(1) : nil)

// Unary math nif: numeric arg → double, call fn, box the rank-0 f64 result.
// Non-numeric arg → nil. TCO-clean (no & escapes).
static lvm(lvm_math1, ai_flo_t (*fn)(ai_flo_t)) {
 word a = Sp[0];
 if (arrp(a)) {                               // (sin arr) etc. -> float array; complex array undefined
  if (vec(a)->type == ai_C) return Sp[0] = ZeroPoint, Ip++, Continue();
  return Ap(lvm_vmap1, g, fn); }
 if (!isnum(a)) return Sp[0] = ZeroPoint, Ip++, Continue();
 ai_flo_t ad = toflo(a), rd = fn(ad);
 Have(flo_req);
 return Sp[0] = mk_flo(&Hp, rd), Ip++, Continue(); }

static lvm(lvm_math2, ai_flo_t (*fn)(ai_flo_t, ai_flo_t)) {
 word a = Sp[0], b = Sp[1];
 if (arrp(a) || arrp(b)) {                               // (pow arr ..) etc. -> float array
  if ((arrp(a) && vec(a)->type == ai_C) || (arrp(b) && vec(b)->type == ai_C))
   return *++Sp = ZeroPoint, Ip++, Continue();                 // complex array undefined here
  return Ap(lvm_vmap2, g, fn); }
 if (!isnum(a) || !isnum(b)) return
  *++Sp = ZeroPoint, Ip++, Continue();
 ai_flo_t ad = toflo(a), bd = toflo(b), rd = fn(ad, bd);
 Have(flo_req);
 return *++Sp = mk_flo(&Hp, rd), Ip++, Continue(); }


// ai_sin .. ai_pow are macro aliases (g/g.h) for the C library math
// functions: libm on hosted builds, k/libc.c on the freestanding
// kernel. The op generators reference them through ai_##n, which the
// preprocessor resounds into the real names after pasting.
m1(mvm1)

// (log x): natural log, climbing the tier lattice like everything else. A
// positive real stays float (math1); a negative real or a complex scalar
// widens to the complex principal value ~((log |z|) (arg z)) -- so euler's
// identity holds in the direction floats can state it exactly: (log -1) =
// (* i pi), since atan2(0,-1) is pi by IEEE fiat and i moves it with exact
// 0/1 products. Arrays stay elementwise float (negative elements -> nan).
lvm(lvm_log) {
 word a = Sp[0];
 ai_flo_t m, th;
 if (Cp(a)) m = ai_log(cplx_mod(a)), th = ai_atan2(cplx_im(a), cplx_re(a));
 else if (isnum(a) && toflo(a) < 0) { ai_flo_t ad = toflo(a);
  m = ai_log(-ad), th = ai_atan2(0, ad); }
 else return Ap(lvm_math1, g, ai_log);
 Have(cplx_req);
 return Sp[0] = mk_cplx(&Hp, m, th), Ip++, Continue(); }

op11(lvm_flop, flop(Sp[0]) ? putcharm(1) : nil)

// ============================================================================
// vec
// ============================================================================
size_t const ai_T[] = {
 [ai_Z] = Bytes,
 [ai_R] = Bytes,
 [ai_C] = 2 * Bytes,      // complex scalar: (re, im)
 [ai_O] = Bytes, };       // object: one tagged l word per element

uintptr_t ai_vec_bytes(struct ai_vec *v) {
 return sizeof(struct ai_vec) + v->rank * sizeof(word) + ai_T[v->type] * vec_nelem(v); }

// ============================================================================
// rng
// ============================================================================
// Step 8 -- random numbers. xoshiro256++ (Blackman & Vigna, public domain)
// seeded by SplitMix64. State is a rank-1 i64 vec of length 4 (256 bits) that
// rides the existing vec machinery -- no data sentinel, no enum q or wasm-vt
// changes (cf. complex, Step 7). The payload is treated
// as raw bytes (memcpy), never via the typed vec_get/put accessors, so the
// 64-bit limbs survive on 32-bit ports and a given seed reproduces the same
// sequence on host/kernel/MCU/WASM/Playdate. C holds no RNG state and never
// draws: the only primitives are seed (fresh state from a fixnum) and the
// functional steps turn/turnf (copy the input state, step the copy,
// return (value . new-state) -- the input is never mutated). seed + random are
// the explicit-state surface; the global rand/randf stream (over book['rng-state])
// is prel lisp riding the same steps. Not a CSPRNG.

static ai_inline uint64_t rotl64(uint64_t x, int k) {
 return (x << k) | (x >> (64 - k)); }

// All the uint64_t scratch (s[4]) lives in these ai_noinline helpers that move it
// via memcpy: taking &s in a VM ap would defeat the Continue() sibcall (see
// the flo_get note in i.h), and memcpy is alignment-safe (a 32-bit port's
// vec_data is only 4-byte aligned, so a raw uint64_t* deref could fault).

// Advance the 4-word state stored at `payload` and return one 64-bit draw.
static ai_noinline uint64_t rng_step(void *payload) {
 uint64_t s[4];
 memcpy(s, payload, sizeof s);
 uint64_t const result = rotl64(s[0] + s[3], 23) + s[0];
 uint64_t const t = s[1] << 17;
 s[2] ^= s[0]; s[3] ^= s[1]; s[1] ^= s[2]; s[0] ^= s[3];
 s[2] ^= t; s[3] = rotl64(s[3], 45);
 memcpy(payload, s, sizeof s);
 return result; }

// Fill the 4-word state at `payload` from a 64-bit seed via SplitMix64. The
// all-zero state is xoshiro's fixed point, so substitute a nonzero word if it
// ever arises (SplitMix64 makes that practically impossible, but be exact).
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

// Shape v as an i64 state vec (rank 1, len 4) and seed it. ini_vec + a
// pointer write only, so an inlining caller keeps its tail call; the &s
// scratch stays inside rng_seed_into.
void ai_rng_seed(struct ai_vec *v, uint64_t seed) {
 ini_vec(v, rng_vt, 1);
 v->shape[0] = rng_state_len;
 rng_seed_into(vec_data(v), seed); }

// Is x a well-formed state vec (rank-1 i64, length 4)?
static ai_inline bool rng_state_p(word x) {
 return packp(x) && vec(x)->rank == 1 && vec(x)->type == rng_vt
        && vec(x)->shape[0] == rng_state_len; }

// Build a fresh state vec at Hp, copying the 4 limbs of `src` into it. Caller
// holds Have(rng_vec_req). Both pointers are heap pointers -> no &local escape.
static ai_inline struct ai_vec *rng_copy(ai_word **hp, struct ai_vec *src) {
 struct ai_vec *v = (struct ai_vec*) *hp;
 *hp += rng_vec_req;
 ini_vec(v, rng_vt, 1);
 v->shape[0] = rng_state_len;
 memcpy(vec_data(v), vec_data(src), rng_payload_bytes);
 return v; }

// Canonicalize a 62-bit draw to the smallest integer tier (a fixnum on a 64-bit
// word, a bignum on a 32-bit one). Out-of-line so its limb[] scratch and the
// ai_big_canon call don't force a frame in lvm_turn -- a frame there would
// turn the tail Continue() into a ret and trip `make vmret`. The caller has
// already Have'd rng_draw_req; ai_big_canon only bumps g->hp (no GC).
static ai_noinline word rng_canon(struct ai *g, uint64_t r) {
 ai_limb limb[64 / limb_bits]; int nl = 0;               // split the 64-bit draw into native limbs (1 or 2)
 for (int i = 0; (size_t) i * limb_bits < 64; i++) limb[i] = (ai_limb) (r >> (i * limb_bits)), nl = i + 1;
 return ai_big_canon(&g->hp, limb, nl, false); }

// (wheel n): a fresh state vec deterministically seeded from fixnum n. A
// non-fixnum seeds from 0.
lvm(lvm_wheel) {
 word n = Sp[0];
 uint64_t seed = charmp(n) ? (uint64_t) (intptr_t) getcharm(n) : 0;
 Have(rng_vec_req);
 struct ai_vec *v = (struct ai_vec*) Hp; Hp += rng_vec_req;
 ai_rng_seed(v, seed);
 return Sp[0] = word(v), Ip++, Continue(); }

// (turn st): functional draw -> (value . st'), value a non-negative
// integer of a fixed 62 bits -- the 64-bit host's fixnum width -- so a seed
// yields the IDENTICAL integer on every target (a fixnum on a 64-bit word, a
// bignum on a 32-bit one), not a word-width truncation. The draw is split into
// two 32-bit limbs and canonicalized; ai_big_canon picks the tier. st is copied
// (referentially transparent); st' is the stepped copy.
#define rng_draw_mask (((uint64_t) 1 << 62) - 1)              // 62 bits = 64-bit fix_max
#define rng_draw_req  (Width(struct ai_big) + b2w((64 / limb_bits) * sizeof(ai_limb)))  // worst case: the 62-bit draw split into native limbs
lvm(lvm_turn) {
 word st = Sp[0];
 if (!rng_state_p(st)) return Sp[0] = ZeroPoint, Ip++, Continue();
 Have(rng_vec_req + rng_draw_req + Width(struct ai_chain));
 st = Sp[0];                                 // re-read post-Have
 struct ai_vec *v = rng_copy(&Hp, vec(st));
 uint64_t r = rng_step(vec_data(v)) & rng_draw_mask;
 Pack(g);
 word val = rng_canon(g, r);
 Unpack(g);
 struct ai_chain *p = (struct ai_chain*) Hp; Hp += Width(struct ai_chain);
 ini_chain(p, val, word(v));
 return Sp[0] = word(p), Ip++, Continue(); }

// (turnf st): functional draw -> (float . st'), float in [0,1).
lvm(lvm_turnf) {
 word st = Sp[0], _res;
 if (!rng_state_p(st)) return Sp[0] = ZeroPoint, Ip++, Continue();
 Have(rng_vec_req + box_req + Width(struct ai_chain));
 st = Sp[0];                                 // re-read post-Have
 struct ai_vec *v = rng_copy(&Hp, vec(st));
 uint64_t r = rng_step(vec_data(v));
 ai_flo_t u = u64_to_unit(r);
 emit_flo(u);                                // box at Hp, into _res
 struct ai_chain *p = (struct ai_chain*) Hp; Hp += Width(struct ai_chain);
 ini_chain(p, _res, word(v));
 return Sp[0] = word(p), Ip++, Continue(); }

// ============================================================================
// eq
// ============================================================================
// α-equivalence of two stored lambda source \-exprs. Bound variables (a \ form's
// leading params -- imports + params, since a closure parks (\ imps… params… body))
// match by binder position, not name, so (\ x x) and (\ y y) compare equal; free
// variables match by symbol. `:` binders are not tracked (a sound, conservative
// incompleteness: a let-rebound name needs the same spelling); a one-operand \ is
// quote -- compared as data via eqv, never walked for binders.
struct arib { word la, lb; int na, nb; struct arib *up; };  // binder rib: (p…body) lists + param counts
static int arib_pos(word s, word l, int n) {                // index of s among the first n of l, else -1
 for (int i = 0; i < n && chainp(l); i++, l = B(l)) if (A(l) == s) return i;
 return -1; }
static bool ai_isbs(struct ai *g, word h) {                  // h is the `\` symbol?
 struct ai_str *n; return (n = add_name(g, h)) && n->len == 1 && n->bytes[0] == '\\'; }
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

// --- the beta bridge: a closure VALUE compares up to the capture-substitution `ev`
// already performed to build it. A partial-app (a held beta-redex: base function + the
// captured args) IS the no-capture lambda whose source is the base body with those args
// substituted for the leading binders -- so (adder 5) = (\ x (+ x 5)). `=` (salpha) and the
// α-hash (shash) deliberately stop at α+structural on SOURCE terms; this lifts the SAME
// reduction `ev` baked into the value into both, so the order/hash stay consistent.
//
// Done WITHOUT allocating (eqv/hash are GC-sensitive hot paths): we walk the base source
// virtually. The base outer \-group's leading binders split FILLED (the first `fn`, bound to
// a captured value) and REMAINING (genuine params of the residual). A filled binder resolves
// to its captured value -- which, as the substituted literal, hashes/compares exactly as that
// value -- and a remaining binder takes its POST-substitution de Bruijn position (so the
// residual's coordinates match a no-capture lambda's). Nested \-groups inside the body are
// genuine (cap-substitution touches only the outer group), so they recurse as plain shash/salpha
// with the filled env threaded through (an inner binder of the same name SHADOWS a filled one --
// the rib is consulted before the filled env). SOUND by construction (true only when genuinely
// equal); a captured closure vs a source lambda stays unbridged (conservative -- incomplete, but
// nf_hash mirrors shash so consistency is exact: =-equal closures always hash equal).
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
// α-invariant hash of a residual's body, mirroring shash exactly: a genuine binder by its
// (rib-depth, position) coordinate, a FILLED binder by its captured value's hash (= the
// substituted literal), a free var by its symbol. The (fs, fn, fv) filled env is constant
// through the structural recursion (nested \-groups push genuine ribs, consulted first).
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
// Does runtime value V equal the meaning of source term b (under b's ribs rb + filled env cb)?
// b a filled binder -> compare the two captured values; b a literal atom -> compare value to literal;
// b a param / free var / compound -> not equal (conservative: we never EVALUATE a free reference).
static bool val_vs_src(struct ai *g, word V, word b, struct arib *rb, struct clonf *cb, word *scratch) {
 if (nomp(b)) {
  for (struct arib *r = rb; r; r = r->up) if (arib_pos(b, r->la, r->na) >= 0) return false;  // a remaining param
  int j = arib_pos(b, cb->fsyms, cb->fn);
  return j >= 0 ? eqv_at(g, V, cb->fv[j], scratch) : false; }   // filled: both values | free: conservative false
 if (!chainp(b)) return eqv_at(g, V, b, scratch);               // literal atom (number / string)
 return false; }                                               // compound source (app / lambda): conservative
// α + value equality of two residual bodies, walked in lockstep (parallel to salpha). A nom on
// either side is classified BOUND (genuine binder, by coordinate), FILLED (a captured value),
// FREE (by symbol), or NOTNOM (a non-nom term). The genuine structure stays lockstep, so a BOUND
// nom's (depth, position) is comparable across sides; a FILLED nom crosses to value-vs-source.
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
// `base` is where THIS frame's worklist starts. The public eqv passes off_pool; a re-entrant call
// from the beta bridge (clo_eq -> nf_walk) passes the CALLER's live worklist top, so the nested
// comparison's scratch sits ABOVE the pending pairs instead of clobbering them. `top` stays the one
// absolute ceiling (off_pool + len) either way.
static bool eqv_at(struct ai *g, word a, word b, word *base) {
 word *top = off_pool(g) + g->len, *w = base;
 struct ai *c = ai_core_of(g);
 for (;;) {
  if (a != b) {
   // Coins (newtypes): equal iff same die and the payloads are eqv -- so
   // (z5 3) = (z5 3) by value, while a coin vs a non-coin (or a different die)
   // is false. Recurse on the payloads through the worklist, like the chain arm.
   if (coinp(a) || coinp(b)) {
    if (coinp(a) && coinp(b) && coin_die(a) == coin_die(b)) {
     a = coin_load(a), b = coin_load(b); continue; }
    return false; }
   // Function values: equality up to the beta the runtime already ran (the bridge). Each side
   // loads to its capture-substitution residual -- a no-capture lambda's source, or a partial-app's
   // base body with the captured args substituted for the leading binders -- and we α+value-compare
   // those, so (adder 5) = (\ x (+ x 5)) and (adder 5) != (adder 6) alike. A source-less base (a
   // bif-based partial-app like (+ 1)) can't residualize: fall back to the structural pairwise
   // chain (base + captured args). Maps / ports / mixed fall to identity (a==b already failed).
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
   // A number never equals a closure: representation-crossing edges stay false.
   // (Numerals ACT as their church lambdas under apply -- (1 z) = z, (0 z) = 1 --
   // but `=` doesn't say so: bridging 0/1 breaks congruence (2 * id /= 2), the
   // order (a lambda seats in the top band), and tower transitivity.)
   if (((a | b) & 1) || !datp(a) || !datp(b) || typ(a) != typ(b)) return false;
   switch (typ(a)) {
    default: return false;
    case KChain:
     if (top - w < 2) __builtin_trap();     // worklist overflow: a cycle
     *w++ = B(a), *w++ = B(b), a = A(a), b = A(b);
     continue;
    case KVec: {
     size_t la = ai_vec_bytes(vec(a)), lb = ai_vec_bytes(vec(b));
     if (la != lb || memcmp(vec(a), vec(b), la)) return false;
     break; }
    case KFlo:
     if (flo_get(a) != flo_get(b)) return false;       // two float boxes: compare the payload (parallels = / cmp)
     break;
    case KWide:
     if (box_get(a) != box_get(b)) return false;       // two wide boxes: compare the payload
     break;
    case KCplx:
     if (cplx_re(a) != cplx_re(b) || cplx_im(a) != cplx_im(b)) return false;  // re AND im
     break;
    case KBig: {
     struct ai_big *x = (struct ai_big*) a, *y = (struct ai_big*) b;
     if (x->slen != y->slen) return false;
     size_t nb = (size_t) (x->slen < 0 ? -x->slen : x->slen) * sizeof(ai_limb);
     if (memcmp(x->limb, y->limb, nb)) return false;
     break; }
    case KString:
     if (len(a) != len(b) || memcmp(txt(a), txt(b), len(a))) return false;
     break; } }
  if (w == base) return true;              // worklist drained: all equal
  b = *--w, a = *--w; } }
ai_noinline bool eqv(struct ai *g, word a, word b) { return eqv_at(g, a, b, off_pool(g)); }

// Whole-array `=`. `=` answers {0,1} on EVERY kind -- a number, a string, a list, a
// point -- and an array is no exception: equal iff the shapes match and every cell is
// equal. It is NOT the elementwise mask. `<` and `>` are the mask makers (test/spec.l's
// arrays section), and routing `=` through them made a PREDICATE silently mean "some
// cell matched": (assert (@(2 4 7) = @(2 4 6))) used to pass, and only failed when NO
// cell matched. An equality that is true of unequal things is worse than useless in the
// one place equality is load-bearing.
// Cells compare ACROSS TIERS, the way (= 5 5.0) is true, so a z-tray equals a gem-tray
// of the same values. Object cells go through eqv -- eqv's own KVec arm memcmps the raw
// words, which compares POINTERS for a boxed cell, so a tray of equal strings would read
// unequal. An object tray never equals a numeric one: boxed cells and packed limbs are
// different representations, and reading one as the other would have to allocate.
static ai_noinline bool arr_eq(struct ai *g, word a, word b) {
 if (!arrp(a) || !arrp(b)) return false;            // an array is never a scalar
 struct ai_vec *va = vec(a), *vb = vec(b);
 if (va->rank != vb->rank) return false;
 for (uintptr_t k = 0; k < va->rank; k++)
  if (va->shape[k] != vb->shape[k]) return false;   // same shape, not merely conformant
 uintptr_t n = vec_nelem(va);
 bool oa = va->type == ai_O, ob = vb->type == ai_O;
 if (oa || ob) {
  if (oa != ob) return false;
  for (uintptr_t i = 0; i < n; i++)
   if (!eqv(g, vec_get_obj(va, i), vec_get_obj(vb, i))) return false;
  return true; }
 if (va->type == ai_C || vb->type == ai_C) {        // (re,im) per cell; a real reads as (r,0)
  ai_flo_t const *pa = vec_data(va), *pb = vec_data(vb);
  for (uintptr_t i = 0; i < n; i++) {
   ai_flo_t are = va->type == ai_C ? pa[2*i] : vec_get_flo(va, i);
   ai_flo_t aim = va->type == ai_C ? pa[2*i+1] : 0;
   ai_flo_t bre = vb->type == ai_C ? pb[2*i] : vec_get_flo(vb, i);
   ai_flo_t bim = vb->type == ai_C ? pb[2*i+1] : 0;
   if (are != bre || aim != bim) return false; }
  return true; }
 if (va->type == ai_Z && vb->type == ai_Z) {        // exact: no double round-trip
  for (uintptr_t i = 0; i < n; i++)
   if (vec_get_int(va, i) != vec_get_int(vb, i)) return false;
  return true; }
 for (uintptr_t i = 0; i < n; i++)                  // a float on either side: as doubles
  if (vec_get_flo(va, i) != vec_get_flo(vb, i)) return false;
 return true; }

// (= a b) — value-equality with numeric promotion across the numeric tower
// (fixnum / boxed float / boxed wide int). With a float operand we compare as
// doubles (a box widens via box_get); otherwise eql handles it — two equal
// wide-int boxes match through eqv's vec arm (ai_vec_bytes covers the type +
// payload), while a box and a fixnum never collide since boxes hold only
// out-of-fixnum-range values. Falls through to eql for non-numeric operands so
// symbol/chain/string identity is unchanged. Strictly looser than eqv, which
// still rejects mixed-type chains (so table keys 3 and 3.0 stay distinct).
lvm(lvm_eq) {
 word a = Sp[0], b = Sp[1];
 // Two fixnums: `=` iff identical (no tower widening needed).
 // The common case (loop guards, table-key tests),
 // so skip the arr/complex/float dispatch below, and fuse a following `?` directly
 // (see the cmp_lt cond-fusion note: then -> Ip+3, else -> Ip[2].m).
 // Identity settles two charms, and a point (mint/nom) against ANYTHING (a
 // point equals only itself: no numeric bridge, and eqv has no KMint/KNom
 // arm) -- symbol-dispatch chains compare noms by the thousand while
 // compiling, so both shapes skip the arr/complex/float dispatch below and
 // fuse a following `?`. `=` answers a boolean everywhere the elementwise
 // mask doesn't apply, so a point beside an array reads 0 here (and the
 // engine's undefined eq faces below agree).
 if (__builtin_expect((charmp(a) && charmp(b)) || pointp(a) || pointp(b), 1)) {
  bool r = a == b;
  if (Ip[1].ap == lvm_cond) return Sp += 2, Ip = r ? Ip + 3 : Ip[2].m, Continue();
  return Sp[1] = r ? putcharm(1) : nil, Sp++, Ip++, Continue(); }
 // Over a rank>=1 array, `=` is WHOLE-ARRAY equality -> a boolean, like every other
 // kind (arr_eq). The mask lives on `<` and `>`. Rank-0 boxes stay scalar (below).
 if (arrp(a) || arrp(b)) {
  bool r = arr_eq(g, a, b);
  Sp[1] = r ? putcharm(1) : nil;
  return Sp++, Ip++, Continue(); }
 // Complex equality: equal iff re and im match. A real operand reads as (r, 0),
 // so the cross-real case `(= (cplx 2 0) 2)` is true (numeric widening, like
 // `(= 2 2.0)`); a non-numeric operand makes it false. Done before the float
 // lane so a complex never reaches toflo (which would misread its two words).
 if (Cp(a) || Cp(b)) {
  bool r = (Cp(a) || isnum(a)) && (Cp(b) || isnum(b))
        && (Cp(a) ? cplx_re(a) : toflo(a)) == (Cp(b) ? cplx_re(b) : toflo(b))
        && (Cp(a) ? cplx_im(a) : 0) == (Cp(b) ? cplx_im(b) : 0);
  Sp[1] = r ? putcharm(1) : nil;
  return Sp++, Ip++, Continue(); }
 bool r;
 // A float operand compares as doubles across the whole numeric tower (fixnum /
 // float box / wide-int box / bignum all widen via toflo; a bignum loses
 // precision past 2^53, the documented float caveat). Otherwise eql: two equal
 // bignums match through eqv's KBig arm, and canonical demotion keeps a bignum
 // distinct from any fixnum/box of a different value.
 if (flop(a) || flop(b)) r = isnum(a) && isnum(b) && (toflo(a) == toflo(b));
 else r = eql(g, a, b);
 Sp[1] = r ? putcharm(1) : nil;
 return Sp++, Ip++, Continue(); }

// (same a b) — pointer/word identity, no structural recursion. Distinguishes
// two distinct objects that `=` would conflate (e.g. two equal chains), so the
// compiler can find a unique marker by identity.
lvm(lvm_same) {
 Sp[1] = Sp[0] == Sp[1] ? putcharm(1) : nil;
 return Sp++, Ip++, Continue(); }

// ============================================================================
// big
// ============================================================================
// Step 6 -- arbitrary-precision integers (bignums). Closes the numeric tower
// fixnum -> wide-int box -> bignum. The representation is the KBig data
// sentinel `struct ai_big` (i.h): sign-magnitude, 32-bit base-2^32 limbs,
// little-endian, top limb nonzero, slen the signed limb count. Zero is never a
// bignum (it demotes to nil), so every bignum has |slen| >= 1 limbs.
//
// All multi-limb work lives in ai_noinline magnitude helpers operating on raw
// ai_limb arrays (no l pointers, no allocation), so the VM-facing entry points
// keep their tail calls and the GC never sees a half-built object. Limbs are
// NATIVE word width (64-bit on a 64-bit host/kernel, 32-bit on the wasm shim);
// ai_dlimb is the double-limb accumulator for products/carries (unsigned __int128
// at 64-bit, uint64_t at 32-bit). A 64x64->128 product is the hardware mulq (one
// operand stays a limb -- never __multi3), and the divmod's 2-limb/1-limb step is
// the hardware divq via div2by1 (never __udivti3) -- both soft routines the
// -nostdlib freestanding port has no compiler-rt to supply. Schoolbook mul +
// Knuth Algorithm D divmod -- Karatsuba/Toom are a later speed diff.


// |slen| of a heap bignum.
static ai_inline int big_nlimbs(word x) {
 intptr_t s = ((struct ai_big*) x)->slen;
 return (int) (s < 0 ? -s : s); }

uintptr_t ai_big_bytes(struct ai_big *b) {
 intptr_t n = b->slen < 0 ? -b->slen : b->slen;
 return sizeof(struct ai_big) + (uintptr_t) n * sizeof(ai_limb); }

// --- raw magnitude primitives (little-endian uint32_t limb arrays) ----------
// Callers pass normalized inputs (no leading zero limbs) and normalize outputs
// via ai_big_canon, which strips leading zeros itself.

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
  ai_dlimb carry = 0; ai_limb love = a[i];          // love stays a limb so love*b[j] is the hardware 64x64->128 (not a 128x128 __multi3)
  for (int j = 0; j < nb; j++) {
   ai_dlimb s = (ai_dlimb) love * b[j] + r[i+j] + carry;
   r[i+j] = (ai_limb) s; carry = s >> limb_bits; }
  r[i+nb] = (ai_limb) carry; } }

// a = a*mul + add, in place (mul,add < 2^limb_bits). a capacity must allow one
// carry limb at a[n]. Returns the new limb count. Used by the decimal reader.
static ai_noinline int mag_mul_add_small(ai_limb *a, int n, ai_limb mul, ai_limb add) {
 ai_dlimb c = add;
 for (int i = 0; i < n; i++) { ai_dlimb s = (ai_dlimb) a[i] * mul + c; a[i] = (ai_limb) s; c = s >> limb_bits; }
 if (c) a[n++] = (ai_limb) c;
 return n; }

// 128/64 -> 64-bit quotient + 64-bit remainder, where the CALLER guarantees the quotient fits a limb
// (hi < d). On x86-64 the hardware `divq` does it in one instruction -- the divide-side twin of mag_mul's
// mulq-not-__multi3 trick, AVOIDING __udivti3/__umodti3, the __int128 soft-divide the -nostdlib
// freestanding port has no compiler-rt to supply. Off x86-64 (or with 32-bit limbs) the plain double-limb
// divide is a 64/32 (wasm) or links via libgcc (a hosted non-x86 build).
static ai_inline ai_limb div2by1(ai_limb hi, ai_limb lo, ai_limb d, ai_limb *rem) {
#if defined(__x86_64__) && limb_bits == 64
 ai_limb q;
 __asm__("divq %[d]" : "=a"(q), "=d"(*rem) : [d] "rm"(d), "a"(lo), "d"(hi));
 return q;
#else
 ai_dlimb num = ((ai_dlimb) hi << limb_bits) | lo;
 return *rem = (ai_limb) (num % d), (ai_limb) (num / d);
#endif
}
// 128/64 -> FULL quotient (up to limb_bits+1 bits, a double-limb) + remainder, for the q-hat step where
// the high limb can reach the divisor. Two divq-safe steps: divide the high limb (giving the high quotient
// limb), then the (partial-remainder : low limb) pair whose high < d fits a limb. The single-limb 64/64
// divides need no __int128.
static ai_inline ai_dlimb div128by64(ai_limb hi, ai_limb lo, ai_limb d, ai_limb *rem) {
 ai_limb qhi = hi / d, r1 = hi % d;
 ai_limb qlo = div2by1(r1, lo, d, rem);
 return ((ai_dlimb) qhi << limb_bits) | qlo; }

// a /= d in place (d != 0), returning the remainder. Used by the printer.
static ai_noinline ai_limb mag_divmod_small(ai_limb *a, int n, ai_limb d) {
 ai_limb rem = 0;
 for (int i = n - 1; i >= 0; i--) { ai_limb r; a[i] = div2by1(rem, a[i], d, &r); rem = r; }
 return rem; }

// Knuth Algorithm D long division (Hacker's Delight `divmnu`). Divides u (m
// limbs) by v (n limbs, v[n-1] != 0, m >= n): q gets the m-n+1 quotient limbs,
// r the n remainder limbs. un (scratch, >= m+1) and vn (scratch, >= n) hold the
// normalized dividend/divisor. q,r,un,vn all distinct from u,v. The q-hat step
// and multiply-subtract run in the double-limb types (a 128/64 divide on 64-bit
// limbs -- the reason the wide-limb lane needs a double-width integer).
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

// Load integer operand x (fixnum / wide-int box / bignum -- never a float) as a
// magnitude. A fixnum/box fills `scratch` (wlimbs limbs: 1 with native-width
// limbs, 2 with 32-bit limbs on a 64-bit word) and points *out at it; a bignum
// points *out into its heap limbs (stable only while no GC runs). Sets *neg and
// returns the limb count (0 for the value zero). wlimbs = limbs to hold one word.
static int load_int_mag(word x, ai_limb scratch[wlimbs], ai_limb const **out, bool *neg) {
 if (bigp(x)) { struct ai_big *b = (struct ai_big*) x; intptr_t s = b->slen;
  *neg = s < 0, *out = b->limb; return (int) (s < 0 ? -s : s); }
 intptr_t v = charmp(x) ? (intptr_t) getcharm(x) : box_get(x);
 *neg = v < 0;
 uintptr_t u = *neg ? (uintptr_t) 0 - (uintptr_t) v : (uintptr_t) v;
 int k = 0;
 for (int i = 0; i < wlimbs; i++) { scratch[i] = (ai_limb) (u >> (limb_bits * i)); if (scratch[i]) k = i + 1; }
 *out = scratch;
 return k; }

ai_flo_t ai_big_to_flo(word x) {
 struct ai_big *b = (struct ai_big*) x;
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
 struct ai_big *b = (struct ai_big*) x;
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

// Demote a magnitude to the smallest tier. Strip leading zeros; a value in
// fixnum range -> a tagged fixnum; in intptr_t range -> a wide-int box; wider
// -> a fresh bignum. Bumps *hp for the box/bignum cases. The single sink that
// keeps the tiers disjoint, so eqv / table keys stay well defined. (With native-
// width limbs a machine word is one limb, so a 1-limb magnitude can still exceed
// the box range and become a bignum -- the goto handles it.)
word ai_big_canon(ai_word **hp, ai_limb const *limb, int n, bool neg) {
 while (n > 0 && limb[n-1] == 0) n--;
 if (n == 0) return nil;
 if (n <= wlimbs) {
  uintptr_t u = 0;
  for (int i = 0; i < n; i++) u |= (uintptr_t) limb[i] << (limb_bits * i);   // combine limbs into a word
  uintptr_t const fixmag = (uintptr_t) 1 << (Bits - 2);   // |fix_min|  = 2^(W-2)
  uintptr_t const boxmag = (uintptr_t) 1 << (Bits - 1);   // |INT_MIN|  = 2^(W-1)
  intptr_t val;
  if (!neg) {
   if (u <= fixmag - 1) return putcharm((intptr_t) u);       // fix_max = 2^(W-2)-1
   if (u > boxmag - 1) goto big;                            // > INTPTR_MAX -> bignum
   val = (intptr_t) u; }
  else {
   if (u <= fixmag) return putcharm((intptr_t) ((uintptr_t) 0 - u));   // incl fix_min
   if (u > boxmag) goto big;                                          // < INTPTR_MIN -> bignum
   val = (intptr_t) ((uintptr_t) 0 - u); }                            // incl INTPTR_MIN
  return mk_wide(hp, val); }
big:;
 struct ai_big *b = ini_big((struct ai_big*) *hp, neg ? -n : n);
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

// Karatsuba for EQUAL-length operands (n limbs each): split each at the low m =
// n/2 limbs, recurse the halves' products z0 = a0*b0 (-> r[0..2m)) and z2 = a1*b1
// (-> r[2m..2n)), then add z1 = (a0+a1)(b0+b1) - z0 - z2 at offset m -- three
// half-size products in place of one full one (~3/4 the limb-mults, recursively).
// Below kara_cutoff schoolbook's lower constant wins. t is scratch (the divmod
// scratch ai_big_binop already reserves covers it). Equal length is the case worth
// splitting (squaring, modpow); the unequal lane stays schoolbook (big_mul_mag).
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

// Packed multi-precision lane for + - * / %, reached from the arith slow paths
// when either operand is a bignum or a fixnum/box op overflowed a machine word.
// Operands at g->sp[0..1] are integers (fixnum/box/bignum); a zero divisor is
// screened off by the caller. Computes a (vop) b, leaves the canonical result
// at g->sp[1], pops one operand, and advances g->ip -- so the caller is just
// Pack(g); g = ai_big_binop(g, vop); Unpack(g); Continue();  (cf. lvm_gc).
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
 else g->sp[1] = mk_flo(&g->hp, toflo(a) / toflo(b));  // a,b still valid: no GC since the re-fetch, and toflo is alloc-free
 g->sp++;
 g->ip = (union u*) g->ip + 1;
 return g; }

// --- resumable (yieldable) multiply -----------------------------------------
// Schoolbook multiply is O(na*nb) and, run as one C call, never yields -- so a
// peer task (the repl's Ctrl-C poller) can't interrupt a huge product. Instead
// we drive it as a self-looping VM instruction: the partial product lives in a
// buf (flat bytes, GC-relocates safely), and each dispatch folds ~bmul_chunk
// limb-mults of rows, then re-dispatches with a YieldCheck. The work state rides
// the l stack [i, r, ret_ip, a, b] -- a yield saves/restores it, so the
// product resumes exactly where it paused. Ip parks on bmul_loop while looping;
// on completion it jumps back to ret_ip (the instruction after `*`).
// Operands a,b are kept as heap bignums so the loop reads stable limb pointers
// directly: a tail-jumping ap must NOT take the address of a stack local
// (it blocks the sibcall), which rules out load_int_mag's scratch array. Setup
// (which may use scratch) is hoisted into a plain function, ai_bmul_setup.
#define bmul_chunk (1 << 14)
static union u const bmul_loop[1] = { { .ap = lvm_bmul } };

// Materialize integer x (fixnum/box/bignum) as a heap ai_big, bumping *hp for the
// fixnum/box case; a bignum is returned in place. Plain function: scratch is fine.
static union u *as_big(ai_word **hp, word x) {
 if (bigp(x)) return cell(x);
 intptr_t v = toint(x);
 bool neg = v < 0;
 uintptr_t u = neg ? (uintptr_t) 0 - (uintptr_t) v : (uintptr_t) v;
 ai_limb tmp[wlimbs]; int n = 0;                                  // a machine word is wlimbs limbs
 for (int i = 0; i < wlimbs; i++) { tmp[i] = (ai_limb) (u >> (limb_bits * i)); if (tmp[i]) n = i + 1; }
 struct ai_big *b = ini_big((struct ai_big*) *hp, neg ? -n : n);
 for (int i = 0; i < n; i++) b->limb[i] = tmp[i];
 *hp += b2w(sizeof(struct ai_big) + (size_t) n * sizeof(ai_limb));
 return cell((word) b); }

// g->sp[0]=a g->sp[1]=b (integers whose product overflows a word). Promote both
// to bignums, allocate the zeroed result buf, and lay out the work frame; on
// return g->ip is bmul_loop. One ai_have so no half-built state is ever seen.
static struct ai *ai_bmul_setup(struct ai *g) {
 word a = g->sp[0], b = g->sp[1];
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2;
 uintptr_t rbytes = (uintptr_t) (na + nb) * sizeof(ai_limb),
           sreq = str_type_width + b2w(rbytes),
           breq = Width(struct ai_buf) + Width(struct ai_tag),
           bigmax = Width(struct ai_big) + b2w((size_t) wlimbs * sizeof(ai_limb));
 if (!ai_ok(g = ai_have(g, 2 * bigmax + sreq + breq + 3))) return g;
 a = g->sp[0], b = g->sp[1];                       // re-fetch (ai_have may have GC'd)
 union u *abig = as_big(&g->hp, a), *bbig = as_big(&g->hp, b), *ret = g->ip + 1;
 struct ai_str *s = ini_str((struct ai_str*) g->hp, rbytes);
 g->hp += sreq; memset(txt(s), 0, rbytes);
 union u *k = (union u*) g->hp; g->hp += breq;
 ((struct ai_buf*) k)->ap = lvm_buf, ((struct ai_buf*) k)->str = s, tagthread(k, Width(struct ai_buf));
 g->sp -= 3;                                       // [i, r, ret_ip, abig, bbig]
 g->sp[0] = putcharm(0), g->sp[1] = word(k), g->sp[2] = word(ret);
 g->sp[3] = word(abig), g->sp[4] = word(bbig);
 g->ip = (union u*) bmul_loop;
 return g; }

// --- resumable (yieldable) Karatsuba multiply -------------------------------
// The chunked schoolbook above is O(na*nb): fine, and it yields, but a huge
// EQUAL-length product (squaring, modpow) deserves SUBQUADRATIC work too. This
// drives a TRUE recursive Karatsuba -- z0, z2 AND the middle z1 all recurse, so
// O(n^1.585), not the O(n^2) of the tree's older half-recursive mag_mul_kara --
// as a yieldable VM instruction. The whole computation lives in ONE pinned buf:
//   [ hdr | job stack | A(n) | B(n) | R(2n) | scratch ]
// re-read by offset each dispatch (a parked yield lets GC move the buf). A JOB
// multiplies ws[ar..ar+n) x ws[br..br+n) -> ws[rr..rr+2n): state 0 SPLITS (form
// the two operand sums sa=a0+a1, sb=b0+b1, then push the three half-size child
// jobs z0,z2,z1); state 1 COMBINES (z1 -= z0; z1 -= z2; r += z1<<m). Children
// run before the parent's combine (LIFO) and share one scratch arena below the
// sums, since they run sequentially. Only na==nb routes here (the case worth
// splitting -- squaring/modpow); unequal-large stays on lvm_bmul's schoolbook.
#define kmul_chunk (1 << 14)   // leaf limb-mults folded per dispatch before a yield check
#define KMUL_HDR 8             // ws header limbs: [0]=n [1]=top (stack ptr) [2]=sign [3]=r_off
#define KMUL_JW  6             // job record limbs: ar, br, n, rr, sr, state
static union u const kmul_loop[1] = { { .ap = lvm_kmul } };

static struct ai *ai_kmul_setup(struct ai *g) {
 word a = g->sp[0], b = g->sp[1];
 int n = big_nlimbs(a);                                   // caller contract: bigp(a)&&bigp(b), na==nb==n, n>=kara_cutoff
 int d = 0; for (int t = n; t >= kara_cutoff; t = (t + 1) / 2) d++;   // Karatsuba depth
 uintptr_t njob = (uintptr_t) 8 * d + 32;                 // job-stack capacity (~3d live, generous)
 uintptr_t scrn = (uintptr_t) 6 * n + 16 * (uintptr_t) d + 256;      // O(n) scratch, with margin
 uintptr_t jobs_off = KMUL_HDR, a_off = jobs_off + njob * KMUL_JW,
           b_off = a_off + n, r_off = b_off + n, scr_off = r_off + 2 * (uintptr_t) n;
 uintptr_t wslimbs = scr_off + scrn;
 uintptr_t sreq = str_type_width + b2w(wslimbs * sizeof(ai_limb));
 uintptr_t breq = Width(struct ai_buf) + Width(struct ai_tag);
 if (!ai_ok(g = ai_have(g, sreq + breq + 3))) return g;
 a = g->sp[0], b = g->sp[1];                              // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 (void) load_int_mag(a, sa, &la, &nega); (void) load_int_mag(b, sb, &lb, &negb);
 struct ai_str *ws_s = ini_str((struct ai_str*) g->hp, wslimbs * sizeof(ai_limb));
 g->hp += sreq;
 ai_limb *ws = (ai_limb*) txt(ws_s);
 for (uintptr_t i = 0; i < wslimbs; i++) ws[i] = 0;       // zero everything: clean result + scratch slots
 for (int i = 0; i < n; i++) ws[a_off + i] = la[i];
 for (int i = 0; i < n; i++) ws[b_off + i] = lb[i];
 ws[0] = (ai_limb) n, ws[1] = 1, ws[2] = (ai_limb) (nega != negb), ws[3] = r_off;   // n, top=1, sign, r_off
 ai_limb *j0 = ws + jobs_off;                             // the root job: multiply A x B -> R
 j0[0] = a_off, j0[1] = b_off, j0[2] = (ai_limb) n, j0[3] = r_off, j0[4] = scr_off, j0[5] = 0;
 union u *k = (union u*) g->hp; g->hp += breq;
 ((struct ai_buf*) k)->ap = lvm_buf, ((struct ai_buf*) k)->str = ws_s, tagthread(k, Width(struct ai_buf));
 union u *ret = g->ip + 1;
 g->sp[0] = word(k), g->sp[1] = word(ret);                // frame [ws_buf, ret_ip]  (was [a, b])
 g->ip = (union u*) kmul_loop;
 return g; }

lvm(lvm_kmul) {
 ai_limb *ws = (ai_limb*) txt(buf_str(Sp[0]));
 int n = (int) ws[0], top = (int) ws[1];
 ai_limb *jobs = ws + KMUL_HDR;
 long budget = kmul_chunk;
 while (top > 0 && budget > 0) {
  ws[1] = (ai_limb) top; YieldCheck();             // persist top, then a PER-JOB yield check: a
  // subquadratic multiply runs far fewer/cheaper dispatches than the O(n^2) schoolbook, so a
  // once-per-dispatch check (as in lvm_bmul) would yield too rarely to stay preemptible.
  ai_limb *J = jobs + (uintptr_t) (top - 1) * KMUL_JW;
  uintptr_t ar = J[0], br = J[1]; int jn = (int) J[2]; uintptr_t rr = J[3], sr = J[4]; int st = (int) J[5];
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
   ai_limb *z1J = jobs + (uintptr_t) top       * KMUL_JW;            // push z1 = sa*sb (pops first)
   z1J[0] = saO, z1J[1] = sbO, z1J[2] = (ai_limb) (h + 1), z1J[3] = z1O, z1J[4] = csr, z1J[5] = 0;
   ai_limb *z2J = jobs + (uintptr_t) (top + 1) * KMUL_JW;            // push z2 = a_hi*b_hi -> r[2m..]
   z2J[0] = ar + m, z2J[1] = br + m, z2J[2] = (ai_limb) h, z2J[3] = rr + 2 * (uintptr_t) m, z2J[4] = csr, z2J[5] = 0;
   ai_limb *z0J = jobs + (uintptr_t) (top + 2) * KMUL_JW;            // push z0 = a_lo*b_lo -> r[0..] (pops last)
   z0J[0] = ar, z0J[1] = br, z0J[2] = (ai_limb) m, z0J[3] = rr, z0J[4] = csr, z0J[5] = 0;
   top += 3; budget -= jn;                                            // pop order z0,z2,z1 then this (combine)
  } else {                                                // COMBINE (st == 1)
   uintptr_t z1O = sr + 2 * (uintptr_t) (h + 1);
   int z0n = 2 * m;      while (z0n > 0 && ws[rr + (uintptr_t) z0n - 1] == 0) z0n--;
   int z2n = 2 * h;      while (z2n > 0 && ws[rr + 2 * (uintptr_t) m + (uintptr_t) z2n - 1] == 0) z2n--;
   int nz1 = 2 * (h + 1); while (nz1 > 0 && ws[z1O + (uintptr_t) nz1 - 1] == 0) nz1--;
   nz1 = mag_sub(ws + z1O, ws + z1O, nz1, ws + rr, z0n);                       // z1 -= z0
   while (nz1 > 0 && ws[z1O + (uintptr_t) nz1 - 1] == 0) nz1--;
   nz1 = mag_sub(ws + z1O, ws + z1O, nz1, ws + rr + 2 * (uintptr_t) m, z2n);   // z1 -= z2
   while (nz1 > 0 && ws[z1O + (uintptr_t) nz1 - 1] == 0) nz1--;
   mag_add_off(ws + rr, 2 * jn, ws + z1O, nz1, m);                             // r += z1 * B^m
   budget -= jn; top--; }
 }
 ws[1] = (ai_limb) top;                                   // persist the stack pointer before any yield
 if (top > 0) { YieldCheck(); return Continue(); }
 bool neg = ws[2]; uintptr_t r_off = ws[3];               // done: ws[r_off..r_off+2n) is the product
 Have(Width(struct ai_big) + b2w(((size_t) 2 * (size_t) n + 1) * sizeof(ai_limb)));
 ws = (ai_limb*) txt(buf_str(Sp[0]));                     // re-fetch (Have may have GC'd)
 n = (int) ws[0], r_off = ws[3];
 word ret = Sp[1], res;
 Pack(g);
 res = ai_big_canon(&g->hp, ws + r_off, 2 * n, neg);
 Unpack(g);
 return Sp += 1, Sp[0] = res, Ip = cell(ret), Continue(); }

lvm(lvm_bmul_start) {
 // SMALL-PRODUCT FAST PATH. The resumable apparatus (ai_bmul_setup: promote BOTH operands to
 // heap bignums, allocate the result buffer + tag, lay out a 5-word work frame) exists so a
 // product big enough to span many chunks can YIELD between them -- a peer task (the repl's
 // Ctrl-C poller, the scheduler) must be able to interrupt a huge multiply. But when the whole
 // product fits in ONE chunk (na*nb <= bmul_chunk) the resumable loop runs exactly once and
 // never yields, so the setup is pure overhead -- and that is the common case (every fixnum
 // overflow, every big*small step of a factorial/Bell tower). Run those one-shot through
 // ai_big_binop, the same schoolbook the array lane uses. (na,nb >= 1 here: at least one operand
 // is a bignum or two fixnums overflowed, so the division can't divide by zero, and writing the
 // bound as na <= chunk/nb keeps na*nb from overflowing int on a 32-bit port.)
 word a = Sp[0], b = Sp[1];
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2;
 if (na <= bmul_chunk / nb) {
  Pack(g); g = ai_big_binop(g, vop_mul);
  if (!ai_ok(g)) return ghelp(g);
  return Unpack(g), Continue(); }
 if (bigp(a) && bigp(b) && na == nb) {           // equal-length large: subquadratic Karatsuba
  Pack(g); g = ai_kmul_setup(g);
  if (!ai_ok(g)) return ghelp(g);
  return Unpack(g), Continue(); }
 Pack(g); g = ai_bmul_setup(g);                  // unequal-length large: chunked schoolbook
 if (!ai_ok(g)) return ghelp(g);
 return Unpack(g), Continue(); }

lvm(lvm_bmul) {
 int i = (int) getcharm(Sp[0]);
 struct ai_big *A = (struct ai_big*) Sp[3], *B = (struct ai_big*) Sp[4];
 intptr_t sla = A->slen, slb = B->slen;
 int na = sla < 0 ? -sla : sla, nb = slb < 0 ? -slb : slb;
 if (!na || !nb) {                                // a zero operand: product is 0
  word ret = Sp[2]; return Sp += 4, Sp[0] = nil, Ip = cell(ret), Continue(); }
 ai_limb *la = A->limb, *lb = B->limb, *rl = (ai_limb*) txt(buf_str(Sp[1]));
 int end = min(i + max(1, bmul_chunk / nb), na);
 for (; i < end; i++) {                           // schoolbook outer loop, one chunk of rows
  ai_dlimb carry = 0; ai_limb love = la[i];         // limb-typed so love*lb[j] is the hardware 64x64->128, not a 128x128 multiply
  for (int j = 0; j < nb; j++) {
   ai_dlimb t = (ai_dlimb) love * lb[j] + rl[i+j] + carry;
   rl[i+j] = (ai_limb) t, carry = t >> limb_bits; }
  rl[i+nb] = (ai_limb) carry; }
 Sp[0] = putcharm(i);                               // persist progress before any yield/GC
 if (i < na) { YieldCheck(); return Continue(); }
 bool neg = (sla < 0) != (slb < 0); word ret;     // done: canonicalize the product
 Have(Width(struct ai_big) + b2w((size_t) (na + nb) * sizeof(ai_limb)));
 ret = Sp[2]; ai_limb *rmag = (ai_limb*) txt(buf_str(Sp[1]));   // re-fetch (Have may have GC'd)
 Pack(g);                                          // canon needs the synced g->hp (not &Hp: stack-local escapes block the sibcall)
 word res = ai_big_canon(&g->hp, rmag, na + nb, neg);
 Unpack(g);
 return Sp += 4, Sp[0] = res, Ip = cell(ret), Continue(); }

// --- resumable long division (the divmod twin of lvm_bmul) ------------------
// `//` and `%` on big operands run yieldably: normalize once into a pinned
// workspace [hdr | vn | un | q], then grind the Knuth-D quotient-limb loop in
// chunks, persisting the loop index j so a peer task can interrupt a huge divide.
// A GC during a chunk moves the workspace buffer; every entry re-reads its base
// from the stack (Sp[1]) and recomputes the region pointers, so only offsets are
// held across a yield (as in lvm_bmul). Cheap divides (short quotient, single-limb
// or larger divisor, |a|<|b|) run one-shot through ai_big_binop -- the resumable
// frame is pure overhead there. which: 0 = quotient (//), 1 = remainder (%).
#define bdiv_chunk (1 << 14)
#define BDIV_HDR 6           // ws header limbs: m, n, s(shift), which, nega, negb
static union u const bdiv_loop[1] = { { .ap = lvm_bdiv } };

static struct ai *ai_bdiv_setup(struct ai *g, int which) {
 word a = g->sp[0], b = g->sp[1];
 int mub = bigp(a) ? big_nlimbs(a) : wlimbs, nub = bigp(b) ? big_nlimbs(b) : wlimbs;
 uintptr_t wslimbs = BDIV_HDR + (uintptr_t) nub + (mub + 1) + (mub - nub + 1);
 uintptr_t sreq = str_type_width + b2w(wslimbs * sizeof(ai_limb));
 uintptr_t breq = Width(struct ai_buf) + Width(struct ai_tag);
 if (!ai_ok(g = ai_have(g, sreq + breq + 3))) return g;
 a = g->sp[0], b = g->sp[1];                          // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 int m = load_int_mag(a, sa, &la, &nega), n = load_int_mag(b, sb, &lb, &negb);
 struct ai_str *ws_s = ini_str((struct ai_str*) g->hp, wslimbs * sizeof(ai_limb));
 g->hp += sreq;
 ai_limb *ws = (ai_limb*) txt(ws_s);
 ai_limb *vn = ws + BDIV_HDR, *un = vn + n, *q = un + (m + 1);
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
 ((struct ai_buf*) k)->ap = lvm_buf, ((struct ai_buf*) k)->str = ws_s, tagthread(k, Width(struct ai_buf));
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
  if (!ai_ok(g)) return ghelp(g);
  return Unpack(g), Continue(); }
 Pack(g); g = ai_bdiv_setup(g, vop == vop_rem);
 if (!ai_ok(g)) return ghelp(g);
 return Unpack(g), Continue(); }

lvm(lvm_bdiv) {
 ai_limb *ws = (ai_limb*) txt(buf_str(Sp[1]));
 int m = (int) ws[0], n = (int) ws[1], s = (int) ws[2], which = (int) ws[3];
 bool nega = ws[4], negb = ws[5];
 ai_limb *vn = ws + BDIV_HDR, *un = vn + n, *q = un + (m + 1);
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
 if (j >= 0) { Sp[0] = putcharm(j); YieldCheck(); return Continue(); }
 // done: canonicalize the requested output. denormalize the remainder into vn (now
 // dead), NOT in place, so a GC-retry of this tail stays idempotent. persist j=-1
 // first so a retry skips the loop.
 Sp[0] = putcharm(-1);
 int outn = which ? n : (m - n + 1);
 Have(Width(struct ai_big) + b2w((size_t) (outn + 1) * sizeof(ai_limb)));
 ws = (ai_limb*) txt(buf_str(Sp[1]));                  // re-fetch (Have may have GC'd)
 vn = ws + BDIV_HDR, un = vn + n, q = un + (m + 1);
 bool rneg = which ? nega : (nega != negb);
 word ret = Sp[2], res;
 Pack(g);
 if (which) {
  for (int i = 0; i < n; i++) vn[i] = s ? (un[i] >> s) | ((ai_dlimb) un[i+1] << (limb_bits - s)) : un[i];
  res = ai_big_canon(&g->hp, vn, n, rneg); }
 else res = ai_big_canon(&g->hp, q, m - n + 1, rneg);
 Unpack(g);
 return Sp += 2, Sp[0] = res, Ip = cell(ret), Continue(); }

// --- reader / printer -------------------------------------------------------

// One digit, either radix -- decimal digits sort below 'a', so the same fold
// reads both and hex takes either case.
static ai_inline ai_limb rdigit(char c) {
 return (ai_limb) (c <= '9' ? c - '0' : (c | 32) - 'a' + 10); }

// g->sp[0] is a [+-]?<pfx><digits> token string in `radix`, `pfx` bytes of
// prefix after the sign (2 for "0x", 0 for decimal); replace it with the
// canonical value (fixnum / box / bignum). Accumulates `chunk` digits per
// mul-add pass, chunk chosen so radix**chunk fits one limb.
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

// g->sp[0] is a bignum; replace it with its base-10 string (with sign). Builds
// the digits into a fresh ai_str by repeated divide-by-10 of a heap-local copy
// of the magnitude; no allocation (hence no GC) once the single Have lands, so
// the work buffer and the string stay put through the loop.
struct ai *ai_big_dec(struct ai *g) {
 struct ai_big *a = (struct ai_big*) g->sp[0];
 intptr_t sl = a->slen;
 bool neg = sl < 0;
 int n = (int) (neg ? -sl : sl),
     cap = n * limb_dec_digits + 2 + (neg ? 1 : 0);   // upper-bound bytes (a limb prints in < limb_dec_digits digits)
 uintptr_t str_words = str_type_width + b2w((size_t) cap),
           scratch_words = b2w((size_t) n * sizeof(ai_limb));
 if (!ai_ok(g = ai_have(g, str_words + scratch_words))) return g;
 a = (struct ai_big*) g->sp[0];                   // re-fetch post-GC
 struct ai_str *st = (struct ai_str*) g->hp;
 ai_limb *work = (ai_limb*) (g->hp + str_words);
 for (int i = 0; i < n; i++) work[i] = a->limb[i];
 char *out = txt(st);                            // bytes area (offset only; st not yet inited)
 int m = n, pos = cap;
 while (m > 0) {
  ai_limb r = mag_divmod_small(work, m, 10);
  while (m > 0 && work[m-1] == 0) m--;
  out[--pos] = (char) ('0' + r); }
 if (pos == cap) out[--pos] = '0';               // (a bignum is never zero; defensive)
 if (neg) out[--pos] = '-';
 int dl = cap - pos;
 for (int i = 0; i < dl; i++) out[i] = out[pos + i];   // shift digits to the front
 ini_str(st, dl);
 g->hp += str_type_width + b2w((size_t) dl);
 g->sp[0] = word(st);
 return g; }

// --- (tray witness shape-list vals): THE typed array constructor ------------
// the generic ctor (off the surface -- mopped; the prel's star-/gem-/twin-/top-tray
// wrap it). `witness` is a GEM that names its tier by example (0/0.0/~(0 0)/() ->
// z/r/c/o); `shape` is a list of non-negative fixnum dimensions (empty -> a rank-0
// scalar box); `vals` fills row-major from a list (a non-numeric or missing entry
// stays 0; extras are ignored), and 0 (or any non-list) means zero-filled. A `c`
// array packs two floats (re,im) per element; zero-fill is 0+0i. Bad witness /
// negative dim / over-rank -> nil.
lvm(lvm_tray) {
 word t = Sp[0], shp = Sp[1];                  // t = a WITNESS GEM (names its tier), vals = Sp[2]
 // the type is read off the witness's KIND -- a value inhabiting the tier: 0 -> Z,
 // 0.0 -> R, ~(0 0) -> C, and anything else (canonically (), the O floor) -> O.
 intptr_t ty = Cp(t) ? ai_C : flop(t) ? ai_R
             : (charmp(t) || widep(t) || bigp(t)) ? ai_Z : ai_O;
 uintptr_t rank = 0, nelem = 1;
 for (word l = shp; chainp(l); l = B(l)) {
  word d = A(l);
  if (!charmp(d) || getcharm(d) < 0) return Sp[2] = nil, Sp += 2, Ip++, Continue();
  rank++, nelem *= (uintptr_t) getcharm(d); }
 if (rank > maxrank) return Sp[2] = nil, Sp += 2, Ip++, Continue();
 uintptr_t bytes = sizeof(struct ai_vec) + rank * sizeof(word) + nelem * ai_T[ty];
 Have(b2w(bytes));
 struct ai_vec *v = (struct ai_vec*) Hp;
 Hp += b2w(bytes);
 ini_vec(v, ty, rank);
 uintptr_t i = 0;                              // re-walk the (possibly moved) lists
 for (word l = Sp[1]; chainp(l); l = B(l)) v->shape[i++] = (uintptr_t) getcharm(A(l));
 if (ty == ai_O) for (i = 0; i < nelem; i++) vec_put_obj(v, i, ZeroPoint);  // O floor is () not 0
 else memset(vec_data(v), 0, nelem * ai_T[ty]);
 i = 0;                                        // no alloc below, so v/Sp[2] stay put
 for (word l = Sp[2]; chainp(l) && i < nelem; l = B(l), i++) {
  word e = A(l);
  if (ty == ai_O) { vec_put_obj(v, i, e); continue; }   // store any value verbatim
  if (ty == ai_C) {                                        // pack (re,im): a real -> (r,0)
   ai_flo_t *fp = vec_data(v);
   if (Cp(e)) fp[2*i] = cplx_re(e), fp[2*i+1] = cplx_im(e);
   else if (isnum(e)) fp[2*i] = toflo(e), fp[2*i+1] = 0;
   continue; }
  if (!isnum(e)) continue;
  if (ty >= ai_R) vec_put_flo(v, i, toflo(e));
  else vec_put_int(v, i, charmp(e) ? (intptr_t) getcharm(e)
                       : flop(e) ? (intptr_t) flo_get(e) : box_get(e)); }
 // The array invariant is RANK, not count: a vec exists at rank>=1, at any tally.
 // Only a RANK-0 point (an empty shape) demotes to its lone scalar gem -- read elem 0
 // and box it canonically (the same extraction peep does), which is what retires every
 // rank-0 vec and keeps the float/wide/complex boxes lean. A rank-1-len-1 STAYS an
 // array: it used to collapse too, which left the surface discontinuous -- tally 0 was
 // an array (the APL empties carry the reduction monoid units and the 0-axis
 // broadcast), tally 1 was a scalar, tally 2+ an array again. @(5) is now a one-cell
 // array, not the number 5. Root the built vec first, since the box alloc can GC.
 Sp[2] = word(v);
 if (rank == 0) {
  if (ty == ai_O) return Sp[2] = vec_get_obj(v, 0), Sp += 2, Ip++, Continue();
  if (ty == ai_C) { Have(cplx_req); v = vec(Sp[2]); ai_flo_t *fp = vec_data(v);
   return Sp[2] = mk_cplx(&Hp, fp[0], fp[1]), Sp += 2, Ip++, Continue(); }
  word _res; Have(box_req); v = vec(Sp[2]);
  if (ty >= ai_R) emit_flo(vec_get_flo(v, 0)); else emit_int(vec_get_int(v, 0));
  return Sp[2] = _res, Sp += 2, Ip++, Continue(); }
 return Sp[2] = word(v), Sp += 2, Ip++, Continue(); }

// (iota n) -- a z-array of the first n charms, 0..n-1, filled in C (no link
// spine): the array twin of `jot`. n<0 or non-fixnum -> nil. This is the baked
// range constructor, so (asum (iota n)) reduces a range end to end in C.
lvm(lvm_iota) {
 word nx = Sp[0];
 if (!charmp(nx) || getcharm(nx) < 0) return Sp[0] = ZeroPoint, Ip++, Continue();
 uintptr_t n = (uintptr_t) getcharm(nx);
 uintptr_t bytes = sizeof(struct ai_vec) + 1 * sizeof(word) + n * ai_T[ai_Z];
 Have(b2w(bytes));
 struct ai_vec *v = (struct ai_vec*) Hp;
 Hp += b2w(bytes);
 ini_vec(v, ai_Z, 1);
 v->shape[0] = n;
 for (uintptr_t i = 0; i < n; i++) vec_put_int(v, i, (intptr_t) i);
 return Sp[0] = word(v), Ip++, Continue(); }

// --- accessors -------------------------------------------------------------
// rank / element-type code as fixnums; nil for a non-vec. Both 0 for a scalar box.
op11(lvm_rank, packp(Sp[0]) ? putcharm(vec(Sp[0])->rank) : ZeroPoint)
op11(lvm_atype, packp(Sp[0]) ? putcharm(vec(Sp[0])->type) : ZeroPoint)

// total element count (1 for a scalar box), nil for a non-vec.
lvm(lvm_alen) {
 word x = Sp[0];
 if (!packp(x)) return Sp[0] = ZeroPoint, Ip++, Continue();
 return Sp[0] = putcharm(vec_nelem(vec(x))), Ip++, Continue(); }

// dimensions as a list (allocates rank link cells), nil for a non-vec.
lvm(lvm_shape) {
 word x = Sp[0];
 if (!packp(x)) return Sp[0] = ZeroPoint, Ip++, Continue();
 uintptr_t r = vec(x)->rank;
 Have(r * Width(struct ai_chain));
 struct ai_vec *v = vec(Sp[0]);                 // re-read post-Have
 struct ai_chain *p = (struct ai_chain*) Hp;
 Hp += r * Width(struct ai_chain);
 word list = ZeroPoint;                             // () terminator (nil-ontology)
 for (uintptr_t i = r; i--; )
  ini_chain(p, putcharm(v->shape[i]), list), list = word(p), p++;
 return Sp[0] = list, Ip++, Continue(); }


// ai_O reductions (sum/prod/max/min) fold through the promoting scalar op, so an
// object array reduces *exactly*. Defined after the object lane (below); the
// numeric reductions divert here when their operand is a ai_O array.
static struct ai *ored(struct ai *g, int kind);   // kind: 0 sum, 1 prod, 2 max, 3 min

// --- reductions: rank>=1 array -> rank-0 scalar; identity on a scalar -------
// The identity-on-scalar property makes `(aall (< a b))` rank-agnostic: the
// same expression works whether a/b are scalars or arrays.
lvm(lvm_asum) {
 word x = Sp[0];
 if (!packp(x)) return Ip++, Continue();        // scalar: (asum 5) = 5
 if (vec(x)->type == ai_O) {
  Pack(g); g = ored(g, 0);
  if (!ai_ok(g)) return ghelp(g);
  return Unpack(g), Continue(); }
 if (vec(x)->type == ai_C) {                   // complex sum -> a complex box
  struct ai_vec *v = vec(x); uintptr_t n = vec_nelem(v);  // K=4 accumulators (see aprod)
  ai_flo_t *fp = vec_data(v);                   // read all parts before Have (no alloc here)
  ai_flo_t a0=0,b0=0, a1=0,b1=0, a2=0,b2=0, a3=0,b3=0; uintptr_t j = 0;
  for (; j + 4 <= n; j += 4) {
   a0 += fp[2*j];   b0 += fp[2*j+1]; a1 += fp[2*j+2]; b1 += fp[2*j+3];
   a2 += fp[2*j+4]; b2 += fp[2*j+5]; a3 += fp[2*j+6]; b3 += fp[2*j+7]; }
  for (; j < n; j++) a0 += fp[2*j], b0 += fp[2*j+1];
  ai_flo_t sr = (a0+a1)+(a2+a3), si = (b0+b1)+(b2+b3);
  Have(cplx_req);
  return Sp[0] = mk_cplx(&Hp, sr, si), Ip++, Continue(); }
 struct ai_vec *v = vec(x);
 uintptr_t n = vec_nelem(v);
 bool fdom = v->type >= ai_R; word _res;
 Have(box_req);
 v = vec(Sp[0]);
 if (fdom) {                                    // K=4 accumulators (see aprod complex)
  ai_flo_t a0=0,a1=0,a2=0,a3=0; uintptr_t i = 0;
  for (; i + 4 <= n; i += 4) a0+=vec_get_flo(v,i), a1+=vec_get_flo(v,i+1), a2+=vec_get_flo(v,i+2), a3+=vec_get_flo(v,i+3);
  for (; i < n; i++) a0 += vec_get_flo(v, i);
  emit_flo((a0+a1)+(a2+a3)); }
 else {                                         // K=4 (modular, Z/2^64 is a commutative ring -> assoc+exact)
  uintptr_t a0=0,a1=0,a2=0,a3=0, i = 0;
  for (; i + 4 <= n; i += 4) a0+=(uintptr_t)vec_get_int(v,i), a1+=(uintptr_t)vec_get_int(v,i+1), a2+=(uintptr_t)vec_get_int(v,i+2), a3+=(uintptr_t)vec_get_int(v,i+3);
  for (; i < n; i++) a0 += (uintptr_t) vec_get_int(v, i);
  emit_int((intptr_t) ((a0+a1)+(a2+a3))); }
 return Sp[0] = _res, Ip++, Continue(); }

lvm(lvm_aprod) {
 word x = Sp[0];
 if (!packp(x)) return Ip++, Continue();
 if (vec(x)->type == ai_O) {
  Pack(g); g = ored(g, 1);
  if (!ai_ok(g)) return ghelp(g);
  return Unpack(g), Continue(); }
 if (vec(x)->type == ai_C) {                   // complex product -> a complex box
  // K=4 INDEPENDENT accumulators break the multiply latency chain (acc_n depends
  // on acc_n-1); reassociation is sound -- * is a commutative monoid, so the
  // product is grouping-invariant by definition (fp * differs only in last-bit
  // rounding per grouping). ~3x faster than the sequential chain; the compiler
  // schedules the four chains. Tail folds the <4 remainder into chain 0.
  struct ai_vec *v = vec(x); uintptr_t n = vec_nelem(v);
  ai_flo_t *fp = vec_data(v);
  ai_flo_t r0=1,i0=0, r1=1,i1=0, r2=1,i2=0, r3=1,i3=0, t; uintptr_t j = 0;
  for (; j + 4 <= n; j += 4) {
   t = r0*fp[2*j]  -i0*fp[2*j+1]; i0 = r0*fp[2*j+1]+i0*fp[2*j];   r0 = t;
   t = r1*fp[2*j+2]-i1*fp[2*j+3]; i1 = r1*fp[2*j+3]+i1*fp[2*j+2]; r1 = t;
   t = r2*fp[2*j+4]-i2*fp[2*j+5]; i2 = r2*fp[2*j+5]+i2*fp[2*j+4]; r2 = t;
   t = r3*fp[2*j+6]-i3*fp[2*j+7]; i3 = r3*fp[2*j+7]+i3*fp[2*j+6]; r3 = t; }
  for (; j < n; j++) { t = r0*fp[2*j]-i0*fp[2*j+1]; i0 = r0*fp[2*j+1]+i0*fp[2*j]; r0 = t; }
  ai_flo_t ra = r0*r1-i0*i1, ia = r0*i1+i0*r1, rb = r2*r3-i2*i3, ib = r2*i3+i2*r3;
  ai_flo_t pr = ra*rb-ia*ib, pi = ra*ib+ia*rb;
  Have(cplx_req);
  return Sp[0] = mk_cplx(&Hp, pr, pi), Ip++, Continue(); }
 struct ai_vec *v = vec(x);
 uintptr_t n = vec_nelem(v);
 bool fdom = v->type >= ai_R; word _res;
 Have(box_req); v = vec(Sp[0]);
 if (fdom) {                                    // K=4 accumulators (see complex above)
  ai_flo_t a0=1,a1=1,a2=1,a3=1; uintptr_t i = 0;
  for (; i + 4 <= n; i += 4) a0*=vec_get_flo(v,i), a1*=vec_get_flo(v,i+1), a2*=vec_get_flo(v,i+2), a3*=vec_get_flo(v,i+3);
  for (; i < n; i++) a0 *= vec_get_flo(v, i);
  emit_flo((a0*a1)*(a2*a3)); }
 else {                                         // K=4 (modular product; imul is latency-bound, ~3x)
  uintptr_t a0=1,a1=1,a2=1,a3=1, i = 0;
  for (; i + 4 <= n; i += 4) a0*=(uintptr_t)vec_get_int(v,i), a1*=(uintptr_t)vec_get_int(v,i+1), a2*=(uintptr_t)vec_get_int(v,i+2), a3*=(uintptr_t)vec_get_int(v,i+3);
  for (; i < n; i++) a0 *= (uintptr_t) vec_get_int(v, i);
  emit_int((intptr_t) ((a0*a1)*(a2*a3))); }
 return Sp[0] = _res, Ip++, Continue(); }

// max / min over a non-empty array (kind 2 = max, 3 = min, matching ored);
// empty -> nil; scalar -> identity. The kind selects the comparison sense.
static lvm(lvm_aextreme, int kind) {
 word x = Sp[0];
 if (!packp(x)) return Ip++, Continue();
 if (vec(x)->type == ai_O) {
  Pack(g); g = ored(g, kind);
  if (!ai_ok(g)) return ghelp(g);
  return Unpack(g), Continue(); }
 if (vec(x)->type == ai_C) return Sp[0] = ZeroPoint, Ip++, Continue();   // complex: unordered
 struct ai_vec *v = vec(x);
 uintptr_t n = vec_nelem(v);
 if (!n) return Sp[0] = ZeroPoint, Ip++, Continue();
 bool fdom = v->type >= ai_R, ismax = kind == 2; word _res;
 Have(box_req); v = vec(Sp[0]);
 // K=4 INDEPENDENT running extremes break the m_n<-m_n-1 latency chain. max/min
 // are commutative+associative+idempotent, so this is EXACT (it just selects an
 // existing element -- no rounding, unlike sum/prod). Each chain seeds from v[0].
 if (fdom) { ai_flo_t m0 = vec_get_flo(v, 0), m1=m0, m2=m0, m3=m0, e; uintptr_t i = 1;
  for (; i + 4 <= n; i += 4) {
   e = vec_get_flo(v,i);   if (ismax?e>m0:e<m0) m0=e;
   e = vec_get_flo(v,i+1); if (ismax?e>m1:e<m1) m1=e;
   e = vec_get_flo(v,i+2); if (ismax?e>m2:e<m2) m2=e;
   e = vec_get_flo(v,i+3); if (ismax?e>m3:e<m3) m3=e; }
  for (; i < n; i++) { e = vec_get_flo(v,i); if (ismax?e>m0:e<m0) m0=e; }
  if (ismax?m1>m0:m1<m0) m0=m1;
  if (ismax?m2>m0:m2<m0) m0=m2;
  if (ismax?m3>m0:m3<m0) m0=m3;
  emit_flo(m0); }
 else { intptr_t m0 = vec_get_int(v, 0), m1=m0, m2=m0, m3=m0, e; uintptr_t i = 1;
  for (; i + 4 <= n; i += 4) {
   e = vec_get_int(v,i);   if (ismax?e>m0:e<m0) m0=e;
   e = vec_get_int(v,i+1); if (ismax?e>m1:e<m1) m1=e;
   e = vec_get_int(v,i+2); if (ismax?e>m2:e<m2) m2=e;
   e = vec_get_int(v,i+3); if (ismax?e>m3:e<m3) m3=e; }
  for (; i < n; i++) { e = vec_get_int(v,i); if (ismax?e>m0:e<m0) m0=e; }
  if (ismax?m1>m0:m1<m0) m0=m1;
  if (ismax?m2>m0:m2<m0) m0=m2;
  if (ismax?m3>m0:m3<m0) m0=m3;
  emit_int(m0); }
 return Sp[0] = _res, Ip++, Continue(); }
lvm(lvm_max) { return Ap(lvm_aextreme, g, 2); }
lvm(lvm_min) { return Ap(lvm_aextreme, g, 3); }

// aall: the bool conjunction reduction. Scalar -> identity (so (aall 1) = 1, the
// linchpin of the rank-agnostic compare idiom). Over an array: "no zero element"
// (the falsy rule lifted to a conjunction); empty array -> true (vacuous). The
// DISJUNCTION (was `aany`) is just `len`: an array is truthy iff some element is
// nonzero, i.e. (nilp x) == (= 0 (len x)) -- so `(len x)` replaces `(aany x)`.
lvm(lvm_aall) {
 word x = Sp[0];
 if (!packp(x)) return Ip++, Continue();
 struct ai_vec *v = vec(x);
 uintptr_t n = vec_nelem(v);
 if (v->type == ai_O) {                         // object: a falsy element fails the conjunction
  for (uintptr_t i = 0; i < n; i++)
   if (ai_nilp(g, vec_get_obj(v, i))) return Sp[0] = nil, Ip++, Continue();
  return Sp[0] = putcharm(1), Ip++, Continue(); }
 if (v->type == ai_C) {                         // complex: a 0+0i element fails the conjunction
  ai_flo_t *fp = vec_data(v);
  for (uintptr_t i = 0; i < n; i++)
   if (fp[2*i] == 0 && fp[2*i+1] == 0) return Sp[0] = nil, Ip++, Continue();
  return Sp[0] = putcharm(1), Ip++, Continue(); }
 // a short-circuit sound, NOT an accumulator chain -- already load-bound (the
 // compiler vectorizes it), so multi-accumulating buys nothing; left as is.
 bool fdom = v->type >= ai_R;
 for (uintptr_t i = 0; i < n; i++)
  if (fdom ? vec_get_flo(v, i) == 0 : vec_get_int(v, i) == 0)
   return Sp[0] = nil, Ip++, Continue();
 return Sp[0] = putcharm(1), Ip++, Continue(); }

// (outer a b) — OUTER PRODUCT with *: result[I,J] = a[I] * b[J], rank ra+rb,
// shape a.shape ++ b.shape. z⊗z -> z (wrapping), any r -> r. complex/object or
// rank > maxrank -> nil. a's cell is hoisted out of the inner (b) loop.
lvm(lvm_outer) {
 word a = Sp[0], b = Sp[1];
 if (!(arrp(a) && arrp(b))) return *++Sp = ZeroPoint, Ip++, Continue();
 struct ai_vec *va = vec(a), *vb = vec(b);
 if (va->type > ai_R || vb->type > ai_R) return *++Sp = ZeroPoint, Ip++, Continue();
 uintptr_t M = vec_nelem(va), N = vec_nelem(vb), n = M * N, rank = va->rank + vb->rank;
 if (rank > maxrank) return *++Sp = ZeroPoint, Ip++, Continue();
 bool fdom = va->type == ai_R || vb->type == ai_R;
 enum ai_vec_type rt = fdom ? ai_R : ai_Z;
 uintptr_t bytes = sizeof(struct ai_vec) + rank * sizeof(word) + ai_T[rt] * n;
 Have(b2w(bytes));
 va = vec(Sp[0]), vb = vec(Sp[1]);          // re-read post-Have
 struct ai_vec *r = (struct ai_vec*) Hp; Hp += b2w(bytes);
 ini_vec(r, rt, rank);
 for (uintptr_t i = 0; i < va->rank; i++) r->shape[i] = va->shape[i];
 for (uintptr_t i = 0; i < vb->rank; i++) r->shape[va->rank + i] = vb->shape[i];
 if (fdom) { ai_flo_t *rp = vec_data(r);
  for (uintptr_t i = 0; i < M; i++) { ai_flo_t av = vec_get_flo(va, i);
   for (uintptr_t j = 0; j < N; j++) rp[i*N+j] = av * vec_get_flo(vb, j); } }
 else { intptr_t *rp = vec_data(r);
  for (uintptr_t i = 0; i < M; i++) { intptr_t av = vec_get_int(va, i);
   for (uintptr_t j = 0; j < N; j++) rp[i*N+j] = (intptr_t)((uintptr_t)av * (uintptr_t)vec_get_int(vb, j)); } }
 return *++Sp = word(r), Ip++, Continue(); }   // arity 2

// (inner a b) — INNER PRODUCT (+.×): contract a's LAST axis with b's FIRST axis.
// a is [..M.., K], b is [K, ..N..]; result [..M.., ..N..] with
// C[I,J] = sum_l a[I,l]*b[l,J]. 1D·1D = dot product -> a scalar number; 2D·2D =
// matrix multiply. z -> z (modular sum-of-products), any r -> r. axis mismatch /
// complex / object / rank > maxrank -> nil. ikj order streams b and c rows so the
// inner j loop vectorizes (the l contraction is the dependency, hoisted one out).
lvm(lvm_inner) {
 word a = Sp[0], b = Sp[1];
 if (!(arrp(a) && arrp(b))) return *++Sp = ZeroPoint, Ip++, Continue();
 struct ai_vec *va = vec(a), *vb = vec(b);
 if (va->type > ai_R || vb->type > ai_R || va->rank < 1 || vb->rank < 1)
  return *++Sp = ZeroPoint, Ip++, Continue();
 uintptr_t K = va->shape[va->rank - 1];
 if (K != vb->shape[0]) return *++Sp = ZeroPoint, Ip++, Continue();   // contracted axes must agree
 uintptr_t M = 1, N = 1;
 for (uintptr_t i = 0; i + 1 < va->rank; i++) M *= va->shape[i];
 for (uintptr_t i = 1; i < vb->rank; i++) N *= vb->shape[i];
 uintptr_t rank = (va->rank - 1) + (vb->rank - 1), n = M * N;
 if (rank > maxrank) return *++Sp = ZeroPoint, Ip++, Continue();
 bool fdom = va->type == ai_R || vb->type == ai_R, ar = va->type == ai_R, br = vb->type == ai_R;
 if (n == 1) {                                  // 1D·1D dot, or any 1-cell contraction -> scalar (invariant)
  word _res;
  if (fdom) { ai_flo_t *Ad = vec_data(va), *Bd = vec_data(vb);
   intptr_t *Ai = vec_data(va), *Bi = vec_data(vb); ai_flo_t acc = 0;
   for (uintptr_t l = 0; l < K; l++) acc += (ar ? Ad[l] : (ai_flo_t) Ai[l]) * (br ? Bd[l] : (ai_flo_t) Bi[l]);
   Have(box_req); emit_flo(acc); }
  else { intptr_t *A = vec_data(va), *B = vec_data(vb); uintptr_t acc = 0;
   for (uintptr_t l = 0; l < K; l++) acc += (uintptr_t) A[l] * (uintptr_t) B[l];
   Have(box_req); emit_int((intptr_t) acc); }
  return *++Sp = _res, Ip++, Continue(); }
 enum ai_vec_type rt = fdom ? ai_R : ai_Z;
 uintptr_t bytes = sizeof(struct ai_vec) + rank * sizeof(word) + ai_T[rt] * n;
 Have(b2w(bytes));
 va = vec(Sp[0]), vb = vec(Sp[1]);
 struct ai_vec *r = (struct ai_vec*) Hp; Hp += b2w(bytes);
 ini_vec(r, rt, rank);
 { uintptr_t s = 0;
   for (uintptr_t i = 0; i + 1 < va->rank; i++) r->shape[s++] = va->shape[i];
   for (uintptr_t i = 1; i < vb->rank; i++) r->shape[s++] = vb->shape[i]; }
 if (fdom) { ai_flo_t *C = vec_data(r);
  ai_flo_t *Ad = vec_data(va), *Bd = vec_data(vb);
  intptr_t *Ai = vec_data(va), *Bi = vec_data(vb);
  for (uintptr_t p = 0; p < n; p++) C[p] = 0;
  for (uintptr_t i = 0; i < M; i++)
   for (uintptr_t l = 0; l < K; l++) { ai_flo_t av = ar ? Ad[i*K+l] : (ai_flo_t) Ai[i*K+l];
    for (uintptr_t j = 0; j < N; j++) C[i*N+j] += av * (br ? Bd[l*N+j] : (ai_flo_t) Bi[l*N+j]); } }
 else { intptr_t *C = vec_data(r), *A = vec_data(va), *B = vec_data(vb);
  for (uintptr_t p = 0; p < n; p++) C[p] = 0;
  for (uintptr_t i = 0; i < M; i++)
   for (uintptr_t l = 0; l < K; l++) { intptr_t av = A[i*K+l];
    for (uintptr_t j = 0; j < N; j++) C[i*N+j] = (intptr_t)((uintptr_t)C[i*N+j] + (uintptr_t)av * (uintptr_t)B[l*N+j]); } }
 return *++Sp = word(r), Ip++, Continue(); }   // arity 2

// --- elementwise monadic math over an array (sin/cos/sqrt/... ) --------------
// Reached from lvm_math1 when its operand arrp. Result is a float array
// (ai_R) with the operand's shape. The fill loop takes no &local, so the
// lvm wrapper keeps its trailing tail call.
static ai_noinline void vmap1_fill(struct ai_vec *r, struct ai_vec *a, ai_flo_t (*fn)(ai_flo_t)) {
 uintptr_t n = vec_nelem(r);
 for (uintptr_t i = 0; i < n; i++) vec_put_flo(r, i, fn(vec_get_flo(a, i))); }

lvm(lvm_vmap1, ai_flo_t (*fn)(ai_flo_t)) {
 struct ai_vec *a = vec(Sp[0]);
 uintptr_t rank = a->rank, n = vec_nelem(a);
 uintptr_t bytes = sizeof(struct ai_vec) + rank * sizeof(word) + n * ai_T[ai_R];
 Have(b2w(bytes));
 a = vec(Sp[0]);                               // re-read post-Have
 struct ai_vec *r = (struct ai_vec*) Hp;
 Hp += b2w(bytes);
 ini_vec(r, ai_R, rank);
 for (uintptr_t i = 0; i < rank; i++) r->shape[i] = a->shape[i];
 vmap1_fill(r, a, fn);
 return Sp[0] = word(r), Ip++, Continue(); }

// --- elementwise dyadic engine (arith / compare / =) with broadcasting ------
// Per-element ops. Integer division guards /0 and INT_MIN/-1 -> 0 (the array
// convention; a scalar `/` promotes such cases to an IEEE inf/NaN instead, but
// one element can't change the whole result's domain).
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

// === ordered comparison: a total order over lisp values ======================
// `< <= > >=` extend across EVERY kind, not just numbers. The CROSS-kind order is
// the enum q type lattice (love.h) -- fixnum/number LOW, lambda HIGH, the very
// order the generic-op matrix diagonals encode: number < string < symbol < chain <
// map < lambda. (Arrays are the exception: an array operand compares ELEMENTWISE -> a
// 0/1 mask via lvm_vbin, never the scalar order.) WITHIN a kind:
//   numbers  by value across the tower; a real r is the complex (r, 0), so complex
//            sorts lexicographically by (re, im). IEEE-faithful: NaN is unordered,
//            so every ordering of it is false.
//   strings  lexicographic over bytes (a prefix sorts first)
//   symbols  the chain order: name lex (anonymous == the empty name), then
//            interned-first, then the mint serial -- TOTAL (see sym_cmp)
//   chains    lexicographic over (car, then cdr), recursively
//   maps     by representation hash, in their OWN band (chain < map < lambda)
//   lambdas  by representation hash (ports/bufs too) -- a GC-stable order
// ANTISYMMETRY: only `<` and `<=` are implemented; `>` and `>=` REVERSE the
// operands and reuse them (a > b == b < a), which is also the right NaN behaviour
// (swap, never negate). A total PREORDER: agrees with `=` (eqv) except where eqv
// is finer -- hash-colliding lambdas compare EQUAL in the order but are not `=`
// (symbols no longer: the mint serial made their order total).
// cross-kind rank = the enum q kind, every numeric kind folded to the arith lane
// (KCharm) so fix/box/big/float/complex order by VALUE, not representation. Arrays
// divert to lvm_vbin before this, so KArr* never appear. One source of truth:
// the enum q order itself.
// the true-blue total order, low -> high: () < mint < string < number < set < chain < map < hot.
// a symbol/mint is the blue floor; STRING sits above mint and just below the number band (the
// charm -- a byte-valued fixnum -- bridges string-bytes up to the number tower, the byte law);
// CHAIN is the GRAMMAR SUBSTRATE -- the language's most capable structure -- so it seats HIGH, just
// under book (a map only outranks it by being mutable); the general n-D set (a tray) sits just BELOW
// it. numbers fold to one by-value band; map/hot stay on top. The compare order is decoupled from the enum
// (dispatch) order: we remap kinds to lattice ranks here, the enum/matrices are untouched.
static ai_inline int cmp_rank(struct ai *g, word x) {
 if (nomp(x)) return 0;                            // mint/symbol -- the floor (a named sym is a (name . mint) chain)
 enum q k = ai_kind(x);
 if (k == KString) return 1;                       // string: above mint, below number
 if (isnum(x) || Cp(x)) return 2;                  // the number band, by value (charm bridges up from string)
 if (k == KArrZ || k == KArrR || k == KArrC) return 2;  // a GALAXY folds into the number band, ordered by its net
 if (k == KArrO) return 3;                         // object tray: above the numbers, BELOW chain
 if (k == KChain) return 4;                        // chain: the grammar substrate -- HIGH, just under book (only book's mutability seats it above)
 if (k == KMap) return 5;                          // map: above chain
 if (coinp(x) && die_get(g, coin_die(x), DIE_NET) == putcharm(2))
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
static ai_inline struct ai_zn vec_cell_zn(struct ai_vec *v, uintptr_t i) {
 if (v->type == ai_C) { ai_flo_t *d = vec_data(v); return zn(d[2*i], d[2*i+1]); }
 return zn(vec_get_flo(v, i), 0); }
static intptr_t galaxy_tie(struct ai_vec *va, struct ai_vec *vb) {
 if (va->rank != vb->rank) return va->rank < vb->rank ? -1 : 1;
 for (uintptr_t i = 0; i < va->rank; i++)
  if (va->shape[i] != vb->shape[i]) return va->shape[i] < vb->shape[i] ? -1 : 1;
 uintptr_t n = vec_nelem(va);                             // same shape -> same nelem
 for (uintptr_t i = 0; i < n; i++) {
  struct ai_zn ea = vec_cell_zn(va, i), eb = vec_cell_zn(vb, i);
  if (ea.re != eb.re) return ea.re < eb.re ? -1 : 1;
  if (ea.im != eb.im) return ea.im < eb.im ? -1 : 1; }
 return 0; }
// a RATIO coin's order is its VALUE's. the exact tier: components that fit a
// SIGNED 64-BIT integer (fixnum, wide box, or -- on a 32-bit word -- a 1-2 limb
// bignum; on a 64-bit word every bignum exceeds int64 by canonical demotion)
// cross-multiply exactly, so near-equal rationals order right where the float
// quotient would tie. wider components, float components, or a scalar/complex/
// galaxy other side fall to the sign-exact net quotients.
static ai_inline bool ratio_ifit(word x, int64_t *v) {
 if (charmp(x) || widep(x)) return *v = toint(x), true;
 if (!bigp(x)) return false;
 struct ai_big *b = (struct ai_big*) x;
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
   return galaxy_tie(vec(a), vec(b)); }                    // both galaxies, equal net: shape then content
  if (Cp(a) || Cp(b)) {                                    // both scalars -- complex: (re, im) lexicographic
   ai_flo_t ar = Cp(a) ? cplx_re(a) : toflo(a), br = Cp(b) ? cplx_re(b) : toflo(b);
   if (ar != br) return ar < br ? -1 : 1;
   ai_flo_t love = Cp(a) ? cplx_im(a) : 0, bi = Cp(b) ? cplx_im(b) : 0;
   return love < bi ? -1 : love > bi ? 1 : 0; }
  if (flop(a) || flop(b)) { ai_flo_t av = toflo(a), bv = toflo(b); return av < bv ? -1 : av > bv ? 1 : 0; }
  return ai_big_cmp(a, b); }                                // exact fix/box/big tower
 if (strp(a)) return bytes_cmp(txt(a), len(a), txt(b), len(b));
 if (formp(a)) { intptr_t c = cmp3(g, A(a), A(b)); return c ? c : cmp3(g, B(a), B(b)); }  // chain: car, then cdr
 if (coinp(a) && coinp(b) && coin_die(a) == coin_die(b))  // same die: order by payload
  return cmp3(g, coin_load(a), coin_load(b));
 uintptr_t ha = hash(g, a), hb = hash(g, b);               // lambda/map/port/buf: by repr hash
 return ha < hb ? -1 : ha > hb ? 1 : 0; }

// (sort l): sort a list ascending by the total order (cmp3), STABLE. One
// reservation up front -- n result chains (committed) plus 2n scratch words
// (left in the uncommitted gap, GC-invisible) -- then a bottom-up merge over
// the scratch lanes and a single spine fill. cmp3 is alloc-free and
// GC-stable, so nothing moves between the reservation and the fill. The
// prel's `sort` dispatches (<)/(>) here (descending = rev) and keeps the
// lisp merge sort for arbitrary predicates. A non-chain passes through; a
// 1-element list returns itself (identity preserved, like the lisp sort).
// (tally l): the spine length of a list -- the number of chains, blind to the
// elements (unlike $, which counts only the sat ones: a chain of nothings
// is nothing, but it still has a length). the compiler's frame arithmetic
// runs on tally; the egg pulls it at birth with the other internals.
// (tally x): THE COUNT -- the second canonical foldMap (every generator weighs
// 1) beside $'s net (every generator weighs itself). a string or buf counts
// its charms, a list its spine, an array its cells, a map its keys, a symbol
// its spelling; a scalar counts nothing. this is what "length" always was.
// the COUNT -- how many, never how much. One C body shared by lvm_tally and
// ai_net's DIE_NET mode-1 lane.
static intptr_t ai_count(struct ai *g, word l) {
 while (coinp(l)) l = coin_load(l);                  // a coin tallies its payload
 if (strp(l)) return (intptr_t) len(l);
 if (bufp(l)) return (intptr_t) len(buf_str(l));
 if (tabp(l)) return (intptr_t) map_len(l);
 if (arrp(l)) return (intptr_t) vec_nelem(vec(l));
 if (nomp(l)) { struct ai_str *nm = add_name(g, l); return nm ? (intptr_t) len(nm) : 0; }  // a sym counts its spelling; a bare mint / the core: 0
 intptr_t n = 0;
 while (chainp(l)) n++, l = B(l);
 return n; }
lvm(lvm_tally) {
 Sp[0] = putcharm(ai_count(g, Sp[0]));
 return Ip++, Continue(); }

lvm(lvm_sort) {
 word l = Sp[0];
 if (!chainp(l) || !chainp(B(l))) return Ip++, Continue();
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
 spine[n - 1].b = ZeroPoint;                        // () terminator (nil-ontology)
 return Sp[0] = word(spine), Ip++, Continue(); }

// the `<` / `<=` lane (op is vop_lt or vop_le). An array operand -> elementwise
// mask (lvm_vbin); a top-level float/complex chain is IEEE-faithful (NaN ->
// unordered -> false), so e.g. (<= nan nan) is nil.
static lvm(lvm_cmp_ord, int op) {
 word a = Sp[0], b = Sp[1]; intptr_t r;
 if (arrp(a) || arrp(b)) return Ap(lvm_vbin, g, op);      // array -> elementwise
 int ra = cmp_rank(g, a), rb = cmp_rank(g, b);
 if (ra != rb) r = vcmp_int(op, ra, rb);                   // cross-kind: the true-blue lattice (cmp_rank)
 else if (!(isnum(a) || Cp(a)) || coinp(b)) r = vcmp_int(op, cmp3(g, a, b), 0);  // same non-number band, or a ratio coin either side (a coin as `a` fails isnum; as `b` this catches it): via cmp3
 else if (Cp(a) || Cp(b)) {                                // complex: lexicographic, per op
  ai_flo_t ar = Cp(a) ? cplx_re(a) : toflo(a), br = Cp(b) ? cplx_re(b) : toflo(b);
  r = ar != br ? vcmp_flo(op, ar, br)
              : vcmp_flo(op, Cp(a) ? cplx_im(a) : 0, Cp(b) ? cplx_im(b) : 0); }
 else if (flop(a) || flop(b)) r = vcmp_flo(op, toflo(a), toflo(b));
 else if (bigp(a) || bigp(b)) r = vcmp_int(op, ai_big_cmp(a, b), 0);
 else r = vcmp_int(op, toint(a), toint(b));
 return *++Sp = r ? putcharm(1) : nil, Ip++, Continue(); }
// `<` `<=` -- the implemented side: both-fixnum fast path (tagged order is
// monotonic), else the lane. `>` `>=` are the other side: reverse the operands and
// reuse `<` `<=` (a > b == b < a; a >= b == b <= a).
//   COND FUSION: the overwhelmingly common consumer of a comparison is the very next
// op, a `?` (lvm_cond) testing its result. When the fixnum fast path sees lvm_cond
// next, it BRANCHES DIRECTLY -- pops both operands and jumps to the then/else arm --
// instead of materializing a 0/1 boolean, paying a second indirect dispatch, and
// re-reading it through ai_nilp. Layout: [Ip]=cmp [Ip+1]=cond [Ip+2]=else-addr
// [Ip+3]=then, so true -> Ip+3, false -> Ip[2].m (exactly lvm_cond's own targets:
// its Ip is our Ip+1, so its Ip+2 is our Ip+3 and its Ip[1].m is our Ip[2].m). The
// slow path is untouched: it falls through to the retained lvm_cond, which handles
// arrays (mask -> net), complex, strings, NaN unordered, everything. The gt/ge
// reversers reach this through Ap(lvm_lt/le) with Ip still on their own 1-cell op,
// so the same [cmp][cond] adjacency holds and they fuse for free.
#define cmp_lt(nom, vop) lvm(nom) { \
 word a = Sp[0], b = Sp[1]; \
 if (__builtin_expect(charmp(a) && charmp(b), 1)) { \
  intptr_t r = vcmp_int(vop, a, b); \
  if (Ip[1].ap == lvm_cond) return Sp += 2, Ip = r ? Ip + 3 : Ip[2].m, Continue(); \
  return *++Sp = r ? putcharm(1) : nil, Ip++, Continue(); } \
 return Ap(lvm_cmp_ord, g, vop); }
cmp_lt(lvm_lt, vop_lt) cmp_lt(lvm_le, vop_le)
#undef cmp_lt
lvm(lvm_gt) { word t = Sp[0]; Sp[0] = Sp[1], Sp[1] = t; return Ap(lvm_lt, g); }  // a > b == b < a
lvm(lvm_ge) { word t = Sp[0]; Sp[0] = Sp[1], Sp[1] = t; return Ap(lvm_le, g); }  // a >= b == b <= a

// Comparison from a 3-way sign of (lhs - rhs). Used when one operand is a bignum
// scalar: a bignum is always out of machine-int range (|bignum| > INTPTR_MAX, by
// canonical demotion), so it orders against any int element by its sign alone --
// exactly, where the low-bits truncation used for arithmetic would not.
static intptr_t vcmp_sign(int op, int s) {
 switch (op) {
  case vop_lt: return s < 0; case vop_le: return s <= 0;
  case vop_gt: return s > 0; case vop_ge: return s >= 0;
  default: return s == 0; } }                   // vop_eq

// The broadcast dim of two conformant axis sizes: a size-1 axis takes the
// OTHER size -- INCLUDING 0, so an empty axis stays empty (a max here turns
// (0,1) into 1 and fills one element out of an empty operand: garbage).
static ai_inline uintptr_t bdim(uintptr_t da, uintptr_t db) {
 return da == 1 ? db : db == 1 ? da : da; }

// The broadcast plumbing shared by the elementwise lanes (vbin / vmap2 / obin /
// cbin / cplx-build). The lvm wrappers keep scalar locals only (TCO), so the
// shape walk runs twice -- once to gate size before Have, once to fill the
// fresh result -- re-deriving from the operand words each time (they move).
// The broadcast element count of a and b, right-aligned; (uintptr_t) -1 when
// some axis pair doesn't conform.
static uintptr_t bshape_n(word a, word b) {
 bool aarr = arrp(a), barr = arrp(b);
 uintptr_t ra = aarr ? vec(a)->rank : 0, rb = barr ? vec(b)->rank : 0;
 uintptr_t R = ra > rb ? ra : rb, n = 1;
 for (uintptr_t k = 0; k < R; k++) {
  uintptr_t da = (aarr && k < ra) ? vec(a)->shape[ra - 1 - k] : 1;
  uintptr_t db = (barr && k < rb) ? vec(b)->shape[rb - 1 - k] : 1;
  if (da != db && da != 1 && db != 1) return (uintptr_t) -1;
  n *= bdim(da, db); }
 return n; }

// Fill shape[0..R) with the broadcast shape of a and b (conformance already
// gated by bshape_n).
static void bshape_put(uintptr_t *shape, uintptr_t R, word a, word b) {
 bool aarr = arrp(a), barr = arrp(b);
 uintptr_t ra = aarr ? vec(a)->rank : 0, rb = barr ? vec(b)->rank : 0;
 for (uintptr_t k = 0; k < R; k++) {
  uintptr_t da = (aarr && k < ra) ? vec(a)->shape[ra - 1 - k] : 1;
  uintptr_t db = (barr && k < rb) ? vec(b)->shape[rb - 1 - k] : 1;
  shape[R - 1 - k] = bdim(da, db); } }

// c[j]: the operand's flat-offset contribution of result axis j (0 when that
// axis is absent in the operand or is a size-1 broadcast axis); v == 0 reads
// a scalar operand (all zeros).
static void bstride(struct ai_vec *v, uintptr_t R, intptr_t *c) {
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
static ai_noinline void vbin_fill(struct ai_vec *r, word a, word b, int op, bool fdom) {
 uintptr_t R = r->rank, n = vec_nelem(r);
 bool aarr = arrp(a), barr = arrp(b);
 struct ai_vec *va = aarr ? vec(a) : 0, *vb = barr ? vec(b) : 0;
 // CONTIGUOUS MONOTYPE FAST PATH: no size-1 broadcasting (each operand is either a
 // scalar or a full-shape array of the compute domain's storage type), so oa==ob==p
 // -- the per-cell odometer, offset recompute, and accessor/op dispatch all vanish.
 // Raw pointers, the op hoisted out of the loop ONCE, a tight body the compiler
 // vectorizes. Mixed types, bignum scalars, or genuine broadcasting fall through to
 // the general odometer loop below (correct, just not accelerated). Results are
 // bit-identical: same reads (raw f64/i64), same op (matches vop_flo/int/cmp).
 { bool cmpf = op >= vop_lt;
   bool aok = !aarr || vec_nelem(va) == n, bok = !barr || vec_nelem(vb) == n;
   bool nobig = !((!aarr && bigp(a)) || (!barr && bigp(b)));
   if (aok && bok && nobig) {
    if (fdom && (!aarr || va->type == ai_R) && (!barr || vb->type == ai_R)) {
     ai_flo_t sa = aarr ? 0 : toflo(a), sb = barr ? 0 : toflo(b);
     ai_flo_t *ap = aarr ? (ai_flo_t*) vec_data(va) : 0, *bp = barr ? (ai_flo_t*) vec_data(vb) : 0;
     if (cmpf) { intptr_t *rp = (intptr_t*) vec_data(r);
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { ai_flo_t av = aarr?ap[p]:sa, bv = barr?bp[p]:sb; rp[p] = (E)?1:0; } } while (0)
      switch (op) { case vop_lt: VBF(av<bv); return; case vop_le: VBF(av<=bv); return;
        case vop_gt: VBF(av>bv); return; case vop_ge: VBF(av>=bv); return; case vop_eq: VBF(av==bv); return; }
      #undef VBF
     } else { ai_flo_t *rp = (ai_flo_t*) vec_data(r);
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { ai_flo_t av = aarr?ap[p]:sa, bv = barr?bp[p]:sb; rp[p] = (E); } } while (0)
      switch (op) { case vop_add: VBF(av+bv); return; case vop_sub: VBF(av-bv); return;
        case vop_mul: VBF(av*bv); return; case vop_quot: VBF(av/bv); return;
        case vop_fquot: VBF(ai_trunc(av/bv)); return; case vop_rem: VBF(ai_fmod(av,bv)); return; }
      #undef VBF
     }
    } else if (!fdom && (!aarr || va->type == ai_Z) && (!barr || vb->type == ai_Z)) {
     intptr_t sia = aarr ? 0 : (charmp(a) ? (intptr_t) getcharm(a) : box_get(a));
     intptr_t sib = barr ? 0 : (charmp(b) ? (intptr_t) getcharm(b) : box_get(b));
     intptr_t *ap = aarr ? (intptr_t*) vec_data(va) : 0, *bp = barr ? (intptr_t*) vec_data(vb) : 0;
     intptr_t *rp = (intptr_t*) vec_data(r);   // r is ai_Z for both int-arith and the mask
     if (cmpf) {
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { intptr_t av = aarr?ap[p]:sia, bv = barr?bp[p]:sib; rp[p] = (E)?1:0; } } while (0)
      switch (op) { case vop_lt: VBF(av<bv); return; case vop_le: VBF(av<=bv); return;
        case vop_gt: VBF(av>bv); return; case vop_ge: VBF(av>=bv); return; case vop_eq: VBF(av==bv); return; }
      #undef VBF
     } else {
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { intptr_t av = aarr?ap[p]:sia, bv = barr?bp[p]:sib; rp[p] = (E); } } while (0)
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
 // scalar values: the float domain widens a bignum full-magnitude (ai_big_to_flo
 // via toflo); the int domain has no room for a bignum, so arithmetic demotes it
 // by low bits (modular). A *comparison* against a bignum, though, is decided
 // exactly by the bignum's sign below -- never by these low bits.
 ai_flo_t sa = aarr ? 0 : toflo(a), sb = barr ? 0 : toflo(b);
 intptr_t ia = aarr ? 0 : charmp(a) ? getcharm(a) : bigp(a) ? ai_big_low(a) : box_get(a),
          ib = barr ? 0 : charmp(b) ? getcharm(b) : bigp(b) ? ai_big_low(b) : box_get(b);
 bool abig = !aarr && bigp(a), bbig = !barr && bigp(b);   // at most one (the other is an array)
 int asign = abig ? (((struct ai_big*) a)->slen < 0 ? -1 : 1) : 0;
 int bsign = bbig ? (((struct ai_big*) b)->slen < 0 ? -1 : 1) : 0;
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  if (fdom) {
   ai_flo_t av = aarr ? vec_get_flo(va, oa) : sa, bv = barr ? vec_get_flo(vb, ob) : sb;
   if (cmp) vec_put_int(r, p, vcmp_flo(op, av, bv) ? 1 : 0);
   else vec_put_flo(r, p, vop_flo(op, av, bv)); }
  else {
   intptr_t av = aarr ? vec_get_int(va, oa) : ia, bv = barr ? vec_get_int(vb, ob) : ib;
   if (cmp) {                                    // bignum side (if any) sorts by sign: a-b ~ asign, or -bsign
    intptr_t t = (abig || bbig) ? vcmp_sign(op, abig ? asign : -bsign) : vcmp_int(op, av, bv);
    vec_put_int(r, p, t ? 1 : 0); }
   else vec_put_int(r, p, vop_int(op, av, bv)); }
  odo_step(idx, R, r->shape); } }

// For `/` (vop_quot) over the integer domain: true if some broadcast element chain
// (av, bv) divides inexactly (bv == 0 or av % bv != 0), so the whole result must
// promote to f64. A bignum scalar forces the float lane (its low word can't decide
// divisibility). ai_noinline: its &-taken stride/odometer arrays stay off lvm_vbin's
// tail call. Called only after conformance is checked, so every offset is in range.
static ai_noinline bool vquot_needs_float(word a, word b) {
 bool aarr = arrp(a), barr = arrp(b);
 if ((!aarr && bigp(a)) || (!barr && bigp(b))) return true;
 struct ai_vec *va = aarr ? vec(a) : 0, *vb = barr ? vec(b) : 0;
 uintptr_t ra = aarr ? va->rank : 0, rb = barr ? vb->rank : 0, R = ra > rb ? ra : rb;
 uintptr_t n = bshape_n(a, b), shp[maxrank];
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 bshape_put(shp, R, a, b);
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 intptr_t ia = aarr ? 0 : toint(a), ib = barr ? 0 : toint(b);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  intptr_t av = aarr ? vec_get_int(va, oa) : ia, bv = barr ? vec_get_int(vb, ob) : ib;
  if (bv == 0 || av % bv != 0) return true;
  odo_step(idx, R, shp); }
 return false; }

lvm(lvm_vbin, int op) {
 word a = Sp[0], b = Sp[1];
 bool aarr = arrp(a), barr = arrp(b);
 // complex lane first: a packed ai_C array, or a complex scalar paired with an
 // array (a complex scalar isn't isnum, so it must divert before the gate below).
 // Mixing ai_C with a ai_O object array is unsupported (neither reads the other's
 // element encoding) -- the ai_O lane wins there.
 if (((aarr && vec(a)->type == ai_C) || (barr && vec(b)->type == ai_C) || Cp(a) || Cp(b))
     && !(aarr && vec(a)->type == ai_O) && !(barr && vec(b)->type == ai_O)) {
  if (vop_bitp(op)) return *++Sp = ZeroPoint, Ip++, Continue();   // no bits on a complex
  return Ap(lvm_cbin, g, op); }
 if (!(aarr || isnum(a)) || !(barr || isnum(b)))   // each operand: array or scalar
  return *++Sp = op == vop_eq ? nil : ZeroPoint, Ip++, Continue();   // `=` is boolean: undefined face -> 0, not ()
 if ((aarr && vec(a)->type == ai_O) || (barr && vec(b)->type == ai_O)) {
  // boxed cells are NOT the word lane: a big refuses the bits on a star, so the
  // object tray refuses them whole rather than answering per-element nil.
  if (vop_bitp(op)) return *++Sp = ZeroPoint, Ip++, Continue();
  return Ap(lvm_obin, g, op); }                   // object array -> promoting lane
 uintptr_t ra = aarr ? vec(a)->rank : 0, rb = barr ? vec(b)->rank : 0;
 uintptr_t R = ra > rb ? ra : rb;
 // compute-type = max element type; a scalar int contributes the lowest type
 // (i8) so it never widens an int array, a scalar float forces the float lane.
 int ta = aarr ? (int) vec(a)->type : flop(a) ? (int) ai_R : (int) ai_Z;
 int tb = barr ? (int) vec(b)->type : flop(b) ? (int) ai_R : (int) ai_Z;
 int ct = ta > tb ? ta : tb;
 bool fdom = ct >= ai_R, cmp = op >= vop_lt;
 if (vop_bitp(op) && fdom) return *++Sp = ZeroPoint, Ip++, Continue();   // no bits on a gem
 uintptr_t n = bshape_n(a, b);                     // conformance + result size
 if (n == (uintptr_t) -1) return *++Sp = op == vop_eq ? nil : ZeroPoint, Ip++, Continue();   // non-conformant `=` -> 0
 // `/` over an all-integer broadcast promotes the whole result to f64 the moment
 // any element divides inexactly (matching the scalar `/`); `//` (vop_fquot) stays
 // integer. Sound only after conformance is known good (offsets are then in range).
 if (op == vop_quot && !fdom && !cmp && vquot_needs_float(a, b)) fdom = true, ct = ai_R;
 enum ai_vec_type rt = cmp ? ai_Z : (enum ai_vec_type) ct;   // compare -> 0/1 Z mask
 uintptr_t bytes = sizeof(struct ai_vec) + R * sizeof(word) + n * ai_T[rt];
 Have(b2w(bytes));
 a = Sp[0], b = Sp[1];                                       // re-read post-Have
 struct ai_vec *r = (struct ai_vec*) Hp; Hp += b2w(bytes);
 ini_vec(r, rt, R);
 bshape_put(r->shape, R, a, b);
 vbin_fill(r, a, b, op, fdom);
 return *++Sp = word(r), Ip++, Continue(); }

// --- dyadic libm map with broadcasting (pow / atan2 over arrays) -------------
// The float-domain twin of lvm_vbin: same numpy broadcast, but the result is
// always a float array and each element is fn(av, bv) for an arbitrary libm
// dyadic fn. A scalar operand broadcasts, widening through toflo -- so a bignum
// scalar feeds in at full magnitude (ai_big_to_flo), same as the scalar `pow`.
// All the &-taking stack arrays live in this ai_noinline fill so the wrapper's
// trailing tail call survives.
static ai_noinline void vmap2_fill(struct ai_vec *r, word a, word b, ai_flo_t (*fn)(ai_flo_t, ai_flo_t)) {
 uintptr_t R = r->rank, n = vec_nelem(r);
 bool aarr = arrp(a), barr = arrp(b);
 struct ai_vec *va = aarr ? vec(a) : 0, *vb = barr ? vec(b) : 0;
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 ai_flo_t sa = aarr ? 0 : toflo(a), sb = barr ? 0 : toflo(b);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  ai_flo_t av = aarr ? vec_get_flo(va, oa) : sa, bv = barr ? vec_get_flo(vb, ob) : sb;
  vec_put_flo(r, p, fn(av, bv));
  odo_step(idx, R, r->shape); } }

lvm(lvm_vmap2, ai_flo_t (*fn)(ai_flo_t, ai_flo_t)) {
 word a = Sp[0], b = Sp[1];
 bool aarr = arrp(a), barr = arrp(b);
 if (!(aarr || isnum(a)) || !(barr || isnum(b)))   // each operand: array or scalar
  return *++Sp = ZeroPoint, Ip++, Continue();
 uintptr_t ra = aarr ? vec(a)->rank : 0, rb = barr ? vec(b)->rank : 0;
 uintptr_t R = ra > rb ? ra : rb, n = bshape_n(a, b);
 if (n == (uintptr_t) -1) return *++Sp = ZeroPoint, Ip++, Continue();
 uintptr_t bytes = sizeof(struct ai_vec) + R * sizeof(word) + n * ai_T[ai_R];
 Have(b2w(bytes));
 a = Sp[0], b = Sp[1];                                       // re-read post-Have
 struct ai_vec *r = (struct ai_vec*) Hp; Hp += b2w(bytes);
 ini_vec(r, ai_R, R);
 bshape_put(r->shape, R, a, b);
 vmap2_fill(r, a, b, fn);
 return *++Sp = word(r), Ip++, Continue(); }

// ============================================================================
// obin -- object-array elementwise lane (ai_O)
// ============================================================================
// The typed lanes (vbin/vmap) read raw C ints/floats and never allocate, so a
// fixed-width int array *wraps* on overflow. The object lane instead routes every
// element through the scalar dispatch (obin_elem), which promotes fixnum->box->
// bignum and boxes floats -- so a ai_O array adds/multiplies *exactly*. Cost: the
// inner loop allocates, so it runs Pack'd (lvm_obin -> obin_run) and re-fetches
// every live pointer (result, operands) after each element, exactly like the
// other allocate-in-a-loop paths (cf. host_run, ai_big_binop).

// One element op: a (op) b for two scalar values, allocating via *fp (may GC --
// a/b are passed by value and rooted before the first allocation here). Returns
// the result value, or nil for a non-numeric / complex operand (deferred).
static word obin_elem(struct ai **fp, int op, word a, word b) {
 if (op >= vop_lt) {                            // comparison -> 1 / nil, no allocation
  if (!isnum(a) || !isnum(b)) return nil;       // Cp not in isnum -> unordered -> nil
  intptr_t t = (flop(a) || flop(b)) ? vcmp_flo(op, toflo(a), toflo(b))
             : (bigp(a) || bigp(b)) ? vcmp_int(op, ai_big_cmp(a, b), 0)
                                    : vcmp_int(op, toint(a), toint(b));
  return t ? putcharm(1) : nil; }
 if (!isnum(a) || !isnum(b)) return nil;
 struct ai *g = *fp;
 if (flop(a) || flop(b)) {                      // float domain -> float box
  if (!ai_ok(g = ai_have(g, flo_req))) return *fp = g, nil;
  *fp = g;
  return mk_flo(&g->hp, vop_flo(op, toflo(a), toflo(b))); }
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
   if (t >= fix_min && t <= fix_max) return putcharm(t);
   if (!ai_ok(g = ai_have(g, wide_req))) return *fp = g, nil;
   *fp = g;
   return mk_wide(&g->hp, t); } }
 // bignum lane: ai_big_binop computes sp[0] (op) sp[1], leaves it at sp[1],
 // pops one, and advances ip -- so save/restore ip and pop the net result.
 if (!ai_ok(g = ai_push(g, 2, a, b))) return *fp = g, nil;
 union u *ip0 = g->ip;
 avec(g, ip0, g = ai_big_binop(g, op));
 if (!ai_ok(g)) return *fp = g, nil;
 g->ip = ip0;
 word r = g->sp[0]; g->sp++;
 return *fp = g, r; }

// Widen the numeric array at g->sp[slot] to a ai_O copy in place (box each
// element), so the obin loop reads values uniformly. Allocates per element; the
// source (rooted at its slot) and the partially-built copy (parked on the stack)
// are re-fetched after every box.
static struct ai *arr_to_obj(struct ai *g, int slot) {
 struct ai_vec *src = vec(g->sp[slot]);
 uintptr_t R = src->rank, n = 1;
 for (uintptr_t i = 0; i < R; i++) n *= src->shape[i];
 uintptr_t bytes = sizeof(struct ai_vec) + R * sizeof(word) + n * ai_T[ai_O];
 if (!ai_ok(g = ai_have(g, b2w(bytes)))) return g;
 src = vec(g->sp[slot]);
 struct ai_vec *dst = (struct ai_vec*) g->hp; g->hp += b2w(bytes);
 ini_vec(dst, ai_O, R);
 for (uintptr_t i = 0; i < R; i++) dst->shape[i] = src->shape[i];
 for (uintptr_t i = 0; i < n; i++) vec_put_obj(dst, i, nil);   // safe pre-fill (GC may see it)
 if (!ai_ok(g = ai_push(g, 1, word(dst)))) return g;             // sp[0]=dst, src now at slot+1
 for (uintptr_t i = 0; i < n; i++) {
  struct ai_vec *s = vec(g->sp[slot + 1]);
  word v;
  if (s->type >= ai_R) {                                        // float -> float box
   ai_flo_t e = vec_get_flo(s, i);
   if (!ai_ok(g = ai_have(g, flo_req))) return g;
   v = mk_flo(&g->hp, e); }
  else {                                                       // int -> fixnum or wide box
   intptr_t e = vec_get_int(s, i);
   if (e >= fix_min && e <= fix_max) v = putcharm(e);
   else { if (!ai_ok(g = ai_have(g, wide_req))) return g;
    v = mk_wide(&g->hp, e); } }
  vec_put_obj(vec(g->sp[0]), i, v); }                          // re-fetch dst post-box
 word d = g->sp[0]; g->sp++; g->sp[slot] = d;                  // install copy, drop the parked root
 return g; }

// Pack'd body of lvm_obin (operands at g->sp[0..1], >=1 is a ai_O array).
static struct ai *obin_run(struct ai *g, int op) {
 word a = g->sp[0], b = g->sp[1];
 bool aarr = arrp(a), barr = arrp(b);
 if (aarr && vec(a)->type != ai_O) { if (!ai_ok(g = arr_to_obj(g, 0))) return g; }
 if (barr && vec(b)->type != ai_O) { if (!ai_ok(g = arr_to_obj(g, 1))) return g; }
 a = g->sp[0], b = g->sp[1], aarr = arrp(a), barr = arrp(b);
 uintptr_t ra = aarr ? vec(a)->rank : 0, rb = barr ? vec(b)->rank : 0;
 uintptr_t R = ra > rb ? ra : rb, n = bshape_n(a, b), shp[maxrank];
 if (n == (uintptr_t) -1) {                                    // non-conforming -> nil
  g->sp[1] = nil, g->sp++, g->ip = (union u*) g->ip + 1; return g; }
 bshape_put(shp, R, a, b);
 uintptr_t bytes = sizeof(struct ai_vec) + R * sizeof(word) + n * ai_T[ai_O];
 if (!ai_ok(g = ai_have(g, b2w(bytes)))) return g;
 struct ai_vec *r = (struct ai_vec*) g->hp; g->hp += b2w(bytes);
 ini_vec(r, ai_O, R);
 for (uintptr_t k = 0; k < R; k++) r->shape[k] = shp[k];
 for (uintptr_t p = 0; p < n; p++) vec_put_obj(r, p, nil);     // nil-fill before any GC
 if (!ai_ok(g = ai_push(g, 1, word(r)))) return g;               // sp: [0]=r [1]=a [2]=b
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(aarr ? vec(g->sp[1]) : 0, R, ca), bstride(barr ? vec(g->sp[2]) : 0, R, cb);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  word ae = aarr ? vec_get_obj(vec(g->sp[1]), oa) : g->sp[1];  // scalar operand re-read each step
  word be = barr ? vec_get_obj(vec(g->sp[2]), ob) : g->sp[2];
  word res = obin_elem(&g, op, ae, be);
  if (!ai_ok(g)) return g;
  vec_put_obj(vec(g->sp[0]), p, res);                          // re-fetch result post-alloc
  odo_step(idx, R, shp); }
 word result = g->sp[0];                                       // collapse [r,a,b] -> r, advance ip
 g->sp += 2, g->sp[0] = result, g->ip = (union u*) g->ip + 1;
 return g; }

lvm(lvm_obin, int op) {
 Pack(g);
 g = obin_run(g, op);
 if (!ai_ok(g)) return ghelp(g);
 return Unpack(g), Continue(); }

// ai_O reduction body (kind: 0 sum, 1 prod, 2 max, 3 min). g->sp[0] is the array.
static struct ai *ored(struct ai *g, int kind) {
 struct ai_vec *v = vec(g->sp[0]);
 uintptr_t n = 1; for (uintptr_t i = 0; i < v->rank; i++) n *= v->shape[i];
 if (kind >= 2) {                                              // max/min: pick an element, no alloc
  if (!n) { g->sp[0] = nil, g->ip = (union u*) g->ip + 1; return g; }
  word acc = vec_get_obj(vec(g->sp[0]), 0);
  int cop = kind == 2 ? vop_gt : vop_lt;
  for (uintptr_t i = 1; i < n; i++) {
   word e = vec_get_obj(vec(g->sp[0]), i);
   if (obin_elem(&g, cop, e, acc) == putcharm(1)) acc = e; }
  g->sp[0] = acc, g->ip = (union u*) g->ip + 1; return g; }
 word init = kind == 0 ? putcharm(0) : putcharm(1);               // sum/prod: fold with allocation
 int aop = kind == 0 ? vop_add : vop_mul;
 if (!ai_ok(g = ai_push(g, 1, init))) return g;                 // sp[0]=acc, sp[1]=array
 for (uintptr_t i = 0; i < n; i++) {
  word e = vec_get_obj(vec(g->sp[1]), i);
  word acc = obin_elem(&g, aop, g->sp[0], e);
  if (!ai_ok(g)) return g;
  g->sp[0] = acc; }
 word result = g->sp[0]; g->sp++, g->sp[0] = result;          // collapse acc into the array slot
 g->ip = (union u*) g->ip + 1;
 return g; }

// (re, im) of an operand for the complex lane / equality: a complex contributes
// its two parts; a real number contributes (value, 0). toflo widens a fixnum /
// float box / wide-int box / bignum -- a bignum narrows to double here, since
// complex is a floating domain (decision 5). Caller guarantees x is Cp or
// isnum. The &out params stay inside ai_noinline callers, off the VM tail call.
static ai_inline void cplx_parts(word x, ai_flo_t *re, ai_flo_t *im) {
 if (Cp(x)) *re = cplx_re(x), *im = cplx_im(x);
 else *re = toflo(x), *im = 0; }

// (ar,love) `vop` (br,bi) in components: the one set of complex formulas, shared
// by the scalar lane (cplx_fill) and the packed array lane (cbin_fill).
static ai_inline void cplx_op(int vop, ai_flo_t ar, ai_flo_t love, ai_flo_t br, ai_flo_t bi,
                             ai_flo_t *re, ai_flo_t *im) {
 switch (vop) {
  case vop_sub: *re = ar - br; *im = love - bi; break;
  case vop_mul: *re = ar * br - love * bi; *im = ar * bi + love * br; break;
  case vop_quot: { ai_flo_t d = br * br + bi * bi;   // (ac+bd)/(c^2+d^2) + ...
   *re = (ar * br + love * bi) / d; *im = (love * br - ar * bi) / d; break; }
  default: *re = ar + br; *im = love + bi; } }        // vop_add

// Fill the rank-0 complex box v with a `vop` b. All the &-taking lives in this
// ai_noinline helper so the lvm wrapper keeps its trailing tail call; no
// allocation inside, so the operand pointers can't move under us.
static ai_noinline void cplx_fill(struct ai_cplx *v, word a, word b, int vop) {
 ai_flo_t ar, love, br, bi, re, im;
 cplx_parts(a, &ar, &love); cplx_parts(b, &br, &bi);
 cplx_op(vop, ar, love, br, bi, &re, &im);
 cplx_set(v, re, im); }

// The complex arithmetic lane. Reached from the arith slow paths when either
// operand is complex. A real operand promotes to (r, 0); a non-numeric operand,
// or vop_rem (% is undefined on complex), yields nil. TCO-clean: the validation
// and box are in the body (no &local), the math is in cplx_fill.
lvm(lvm_cplx_bin, int vop) {
 word a = Sp[0], b = Sp[1];
 if (!(Cp(a) || isnum(a)) || !(Cp(b) || isnum(b)) || vop > vop_quot)
  return *++Sp = ZeroPoint, Ip++, Continue();
 Have(cplx_req);
 a = Sp[0], b = Sp[1];                              // re-read post-Have
 struct ai_cplx *v = (struct ai_cplx*) Hp; v->ap = lvm_cbox; Hp += cplx_req;
 cplx_fill(v, a, b, vop);
 return *++Sp = word(v), Ip++, Continue(); }

// --- complex-array elementwise lane (ai_C) ----------------------------------
// The complex twin of lvm_vbin: packed (re,im) numpy broadcast. An array operand
// is a ai_C (packed) array or a real ai_Z/ai_R array (each element promotes to (v,0));
// a scalar operand is a complex box or any real number. + - * / use cplx_fill's
// formulas; `=` writes a ai_Z 0/1 mask (componentwise equal); ordering and % are
// undefined on complex (handled in the wrapper -> nil). The &-taking odometer/
// stride arrays live in this ai_noinline fill so the wrapper keeps its tail call.
static ai_inline void cbin_part(bool isarr, struct ai_vec *v, ai_flo_t sre, ai_flo_t sim,
                               uintptr_t o, ai_flo_t *re, ai_flo_t *im) {
 if (!isarr) { *re = sre; *im = sim; return; }
 if (v->type == ai_C) { ai_flo_t *fp = vec_data(v); *re = fp[2*o]; *im = fp[2*o+1]; }
 else { *re = vec_get_flo(v, o); *im = 0; } }

static ai_noinline void cbin_fill(struct ai_vec *r, word a, word b, int op, bool cmp) {
 uintptr_t R = r->rank, n = vec_nelem(r);
 bool aarr = arrp(a), barr = arrp(b);
 struct ai_vec *va = aarr ? vec(a) : 0, *vb = barr ? vec(b) : 0;
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 ai_flo_t sar = 0, sai = 0, sbr = 0, sbi = 0;
 if (!aarr) { if (Cp(a)) sar = cplx_re(a), sai = cplx_im(a); else sar = toflo(a); }
 if (!barr) { if (Cp(b)) sbr = cplx_re(b), sbi = cplx_im(b); else sbr = toflo(b); }
 ai_flo_t *rf = cmp ? 0 : vec_data(r);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  ai_flo_t ar, love, br, bi, re, im;
  cbin_part(aarr, va, sar, sai, oa, &ar, &love);
  cbin_part(barr, vb, sbr, sbi, ob, &br, &bi);
  if (cmp) {                                   // (re,im) LEXICOGRAPHIC -- the same order
   int t;                                      // cmp3's complex arm gives a scalar pair
   if (op == vop_eq) t = ar == br && love == bi;   // kept exact (a NaN is equal to nothing)
   else {
    int c = ar < br ? -1 : ar > br ? 1 : love < bi ? -1 : love > bi ? 1 : 0;
    t = op == vop_lt ? c < 0 : op == vop_le ? c <= 0
      : op == vop_gt ? c > 0 : c >= 0; }        // vop_ge
   vec_put_int(r, p, t ? 1 : 0); }
  else {
   cplx_op(op, ar, love, br, bi, &re, &im);
   rf[2*p] = re; rf[2*p+1] = im; }
  odo_step(idx, R, r->shape); } }

lvm(lvm_cbin, int op) {
 word a = Sp[0], b = Sp[1];
 bool aarr = arrp(a), barr = arrp(b);
 // operand: array / complex scalar / real number. % and // stay undefined on complex,
 // but the ORDERINGS hold: a complex scalar already sorts (cmp3's (re,im) lexicographic
 // arm -- sorting needs totality), so a complex tray answers the same mask a gem tray
 // does. A TRAY FOLLOWS ITS SCALAR; the lane refusing an order the scalar lane provides
 // was the anomaly. (vop_eq no longer arrives -- `=` is whole-array, see arr_eq.)
 if (!(aarr || Cp(a) || isnum(a)) || !(barr || Cp(b) || isnum(b))
     || op == vop_rem || op == vop_fquot)
  return *++Sp = op == vop_eq ? nil : ZeroPoint, Ip++, Continue();   // `=` is boolean: undefined face -> 0, not ()
 bool cmp = op >= vop_lt;
 uintptr_t ra = aarr ? vec(a)->rank : 0, rb = barr ? vec(b)->rank : 0;
 uintptr_t R = ra > rb ? ra : rb, n = bshape_n(a, b);
 if (n == (uintptr_t) -1) return *++Sp = op == vop_eq ? nil : ZeroPoint, Ip++, Continue();   // non-conformant `=` -> 0
 enum ai_vec_type rt = cmp ? ai_Z : ai_C;              // compare -> i64 mask, else packed complex
 uintptr_t bytes = sizeof(struct ai_vec) + R * sizeof(word) + n * ai_T[rt];
 Have(b2w(bytes));
 a = Sp[0], b = Sp[1];                                 // re-read post-Have
 struct ai_vec *r = (struct ai_vec*) Hp; Hp += b2w(bytes);
 ini_vec(r, rt, R);
 bshape_put(r->shape, R, a, b);
 cbin_fill(r, a, b, op, cmp);
 return *++Sp = word(r), Ip++, Continue(); }

// Fill complex box v with w ** z via the principal branch: w^z = exp(z * Log w),
// Log w = ln|w| + i*arg w. A real operand promotes to (r, 0) (cplx_parts). w == 0
// falls out as the IEEE limit (exp(-inf) -> 0 for Re z > 0), same domain stance as
// real pow. &-locals stay in this ai_noinline helper, off lvm_pow's tail call.
static ai_noinline void cplx_pow_fill(struct ai_cplx *v, word wbase, word zexp) {
 ai_flo_t wr, wi, zr, zi;
 cplx_parts(wbase, &wr, &wi); cplx_parts(zexp, &zr, &zi);
 ai_flo_t lr = (ai_flo_t) 0.5 * ai_log(wr * wr + wi * wi),    // ln|w|
         li = ai_atan2(wi, wr);                             // arg w
 ai_flo_t pr = zr * lr - zi * li, pi = zr * li + zi * lr,   // z * Log w
         e = ai_exp(pr);
 cplx_set(v, e * ai_cos(pi), e * ai_sin(pi)); }

// sin/cos of pi*x for finite non-integer x (the caller's flo_fracp guarantee,
// so the int cast is safe). The angle is reduced BEFORE multiplying by pi, so
// it never rounds through the float pi and a half-integer x lands exactly on
// the axis: sinpi(1/2) = 1, cospi(1/2) = 0 -- what makes ((/ 1 2) -1) = i
// bit-exact (the inputs are exact; pi*x in floats is where the error lives).
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

// (pow b e) = b ** e. Complex base or exponent -> the complex lane above; a finite
// negative real base to a non-integer power widens to its principal root
// |b|^e * (cospi(e), sinpi(e)) instead of nan -- pow climbs tiers like log, and
// ((/ 1 2) -1) = i exactly. An infinite/integer-power/array case keeps the IEEE
// real lanes (lvm_math2 -> real pow, or vmap2 elementwise over arrays).
lvm(lvm_pow) {
 word a = Sp[0], b = Sp[1];
 if (Cp(a) || Cp(b)) {
  if (!(Cp(a) || isnum(a)) || !(Cp(b) || isnum(b)))
   return *++Sp = ZeroPoint, Ip++, Continue();
  Have(cplx_req);
  a = Sp[0], b = Sp[1];                              // re-read post-Have
  struct ai_cplx *v = (struct ai_cplx*) Hp; v->ap = lvm_cbox; Hp += cplx_req;
  cplx_pow_fill(v, a, b);
  return *++Sp = word(v), Ip++, Continue(); }
 if (isnum(a) && isnum(b)) {
  ai_flo_t ad = toflo(a), bd = toflo(b);
  if (ad < 0 && !__builtin_isinf(ad) && flo_fracp(bd)) {
   ai_flo_t m = ai_pow(-ad, bd), re = m * ai_cospi(bd), im = m * ai_sinpi(bd);
   Have(cplx_req);
   return *++Sp = mk_cplx(&Hp, re, im), Ip++, Continue(); } }
 return Ap(lvm_math2, g, ai_pow); }

// (C re im): build a complex from two real numbers. Non-numeric arg -> nil.
// Fill packed ai_C array r with (re = a-element, im = b-element) under numpy
// broadcast; a, b are real (ai_Z/ai_R) arrays or real scalars. &-taking stride/
// odometer arrays live in this ai_noinline fill so lvm_cplx keeps its tail call.
static ai_noinline void cplx_build_fill(struct ai_vec *r, word a, word b) {
 uintptr_t R = r->rank, n = vec_nelem(r);
 bool aarr = arrp(a), barr = arrp(b);
 struct ai_vec *va = aarr ? vec(a) : 0, *vb = barr ? vec(b) : 0;
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 ai_flo_t sa = aarr ? 0 : toflo(a), sb = barr ? 0 : toflo(b);
 ai_flo_t *rf = vec_data(r);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  rf[2*p]   = aarr ? vec_get_flo(va, oa) : sa;
  rf[2*p+1] = barr ? vec_get_flo(vb, ob) : sb;
  odo_step(idx, R, r->shape); } }

// (C re im): build a complex from two reals. Scalars -> a rank-0 complex box;
// a real array operand (with the other broadcasting) -> a packed ai_C array, so
// (arg (C 1 x)) = atan and (arg (C x y)) = atan2 stay elementwise. A complex or
// object array operand, or a non-numeric scalar, -> nil.
lvm(lvm_cplx) {
 word a = Sp[0], b = Sp[1];
 bool aarr = arrp(a), barr = arrp(b);
 if (aarr || barr) {
  if ((aarr && vec(a)->type >= ai_C) || (barr && vec(b)->type >= ai_C)
      || (!aarr && !isnum(a)) || (!barr && !isnum(b)))
   return *++Sp = ZeroPoint, Ip++, Continue();
  uintptr_t ra = aarr ? vec(a)->rank : 0, rb = barr ? vec(b)->rank : 0;
  uintptr_t R = ra > rb ? ra : rb, n = bshape_n(a, b);
  if (n == (uintptr_t) -1) return *++Sp = ZeroPoint, Ip++, Continue();
  uintptr_t bytes = sizeof(struct ai_vec) + R * sizeof(word) + n * ai_T[ai_C];
  Have(b2w(bytes));
  a = Sp[0], b = Sp[1];                                     // re-read post-Have
  struct ai_vec *r = (struct ai_vec*) Hp; Hp += b2w(bytes);
  ini_vec(r, ai_C, R);
  bshape_put(r->shape, R, a, b);
  cplx_build_fill(r, a, b);
  return *++Sp = word(r), Ip++, Continue(); }
 if (!isnum(a) || !isnum(b)) return *++Sp = ZeroPoint, Ip++, Continue();
 ai_flo_t re = toflo(a), im = toflo(b);             // values extracted before alloc
 Have(cplx_req);
 return *++Sp = mk_cplx(&Hp, re, im), Ip++, Continue(); }

// (Cp x): is x a complex scalar?
op11(lvm_Cp, Cp(Sp[0]) ? putcharm(1) : nil)

// Fill r with component `off` (0 = re, 1 = im) of each element of the packed
// ai_C array v, as f64. off < 0 is the (im realarr) lane: r is a ai_Z array and
// every cell is 0, the array lift of (im real) = 0 (v goes unread). &-free but
// ai_noinline, so lvm_re/lvm_im keep their tail call.
static ai_noinline void cpart_fill(struct ai_vec *r, struct ai_vec *v, int off) {
 uintptr_t n = vec_nelem(r);
 if (off < 0) { intptr_t *zp = vec_data(r);
  for (uintptr_t p = 0; p < n; p++) zp[p] = 0;
  return; }
 ai_flo_t *rf = vec_data(r), *fp = vec_data(v);
 for (uintptr_t p = 0; p < n; p++) rf[p] = fp[2*p + off]; }

// The array lane of re/im, reached by tail call once the wrapper has ruled out
// the scalars and the ai_O tray. Result carries the operand's shape: f64 for a
// component of a ai_C array, i64 zeros for off < 0.
static lvm(lvm_cpart, int off) {
 struct ai_vec *v = vec(Sp[0]);
 enum ai_vec_type rt = off < 0 ? ai_Z : ai_R;
 uintptr_t R = v->rank, n = vec_nelem(v);
 uintptr_t bytes = sizeof(struct ai_vec) + R * sizeof(word) + n * ai_T[rt];
 Have(b2w(bytes));
 v = vec(Sp[0]);                                           // re-read post-Have
 struct ai_vec *r = (struct ai_vec*) Hp; Hp += b2w(bytes);
 ini_vec(r, rt, R);
 for (uintptr_t i = 0; i < R; i++) r->shape[i] = v->shape[i];
 cpart_fill(r, v, off);
 return Sp[0] = word(r), Ip++, Continue(); }

// (re z) / (im z): real / imaginary part as a rank-0 float box. On a real
// number, re is the number itself and im is 0. Over an ARRAY they go
// elementwise like `arg`: a packed ai_C array answers an f64 array of that
// component, and a real ai_Z/ai_R array lifts the scalar law -- re hands the
// array straight back (a real array IS its own real part, tier and all), im
// answers a fresh zero array of the same shape. An object array, and anything
// that is not a number at all, -> nil. The nil is the point of the third
// branch: without it a ai_C array would fall through to identity and re would
// answer the COMPLEX array as its own real part -- a wrong answer inside the
// numeric band, not an unhandled one.
lvm(lvm_re) {
 word a = Sp[0], _res;
 if (Cp(a)) { ai_flo_t re = cplx_re(a); Have(box_req); emit_flo(re);
  return Sp[0] = _res, Ip++, Continue(); }
 if (arrp(a)) { enum ai_vec_type t = vec(a)->type;
  if (t == ai_O) return Sp[0] = ZeroPoint, Ip++, Continue();   // a tray is not a number
  if (t != ai_C) return Ip++, Continue();          // a real array is its own real part
  return Ap(lvm_cpart, g, 0); }
 if (isnum(a)) return Ip++, Continue();            // re of a real is itself
 return Sp[0] = ZeroPoint, Ip++, Continue(); }

lvm(lvm_im) {
 word a = Sp[0], _res;
 if (Cp(a)) { ai_flo_t im = cplx_im(a); Have(box_req); emit_flo(im);
  return Sp[0] = _res, Ip++, Continue(); }
 if (arrp(a)) { enum ai_vec_type t = vec(a)->type;
  if (t == ai_O) return Sp[0] = ZeroPoint, Ip++, Continue();
  return Ap(lvm_cpart, g, t == ai_C ? 1 : -1); }   // real array -> zeros of its shape
 if (isnum(a)) return Sp[0] = putcharm(0), Ip++, Continue();   // im of a real is 0
 return Sp[0] = ZeroPoint, Ip++, Continue(); }

// (conj z): complex conjugate (re, -im). conj LIFTS: a real r becomes ~(r 0)
// (= r by value, the tower bridges), so conj is the monadic `~` -- it always
// lands in C. (It used to pass a real through; lifting makes conj == the old
// `twin`, so `~x` reads as (conj x) and the constructor takes the name `twin`.)
lvm(lvm_conj) {
 word a = Sp[0];
 if (Cp(a)) { ai_flo_t re = cplx_re(a), im = cplx_im(a);
  Have(cplx_req);
  return Sp[0] = mk_cplx(&Hp, re, -im), Ip++, Continue(); }
 if (isnum(a)) { ai_flo_t re = toflo(a);            // lift a real to ~(r 0)
  Have(cplx_req);
  return Sp[0] = mk_cplx(&Hp, re, 0), Ip++, Continue(); }
 return Sp[0] = ZeroPoint, Ip++, Continue(); }

// (abs z): type-aware magnitude. Complex -> sqrt(re^2+im^2) (a float). Real ->
// |z| in its own tier: fixnum stays fixnum (or boxes if |fix_min| overflows the
// tag), float stays float, bignum stays bignum (just flips its sign). A wide-int
// box holding INTPTR_MIN promotes to a bignum (the one magnitude the box can't
// hold), matching the arith lanes' INT_MIN/-1 edge -- the big oracle caught the
// old wrap here.
// |INTPTR_MIN| needs the bignum lane, and its limb[] scratch must live OUT OF
// LINE -- rng_canon's precedent, and for the same reason: a stack buffer and the
// ai_big_canon call force a FRAME in the lvm_, which turns the tail Continue()
// into a `ret`. The VM is tail-threaded, so an op that returns instead of
// jumping grows the stack EVERY STEP.
//
// ⚠ `make vmret` does NOT catch this: gcc sibcalls the inline form anyway, so
// the gate stays green while MOONCC emits the frame -- the fault is a raw-build
// STACK OVERFLOW (a segfault deep in an unrelated test), never a wrong answer.
// Caller has already Have'd the room; ai_big_canon only bumps g->hp (no GC).
static ai_noinline word abs_wmin(struct ai *g) {
 uintptr_t u = (uintptr_t) 1 << (Bits - 1);
 ai_limb lb[wlimbs];
 for (int i = 0; i < wlimbs; i++) lb[i] = (ai_limb) (u >> (limb_bits * i));
 return ai_big_canon(&g->hp, lb, wlimbs, false); }
lvm(lvm_abs) {
 word a = Sp[0], _res;
 if (Cp(a)) { ai_flo_t m = cplx_mod(a);
  Have(box_req); emit_flo(m); return Sp[0] = _res, Ip++, Continue(); }
 if (charmp(a)) { intptr_t n = getcharm(a);
  Have(box_req); emit_int(n < 0 ? (intptr_t) (0 - (uintptr_t) n) : n);
  return Sp[0] = _res, Ip++, Continue(); }
 if (flop(a)) { ai_flo_t v = flo_get(a); if (v < 0) v = -v;
  Have(box_req); emit_flo(v); return Sp[0] = _res, Ip++, Continue(); }
 if (widep(a)) { intptr_t n = box_get(a);
  if (n == INTPTR_MIN) {                              // |INTPTR_MIN| = 2^(W-1): the bignum lane
   Have(b2w(sizeof(struct ai_big) + wlimbs * sizeof(ai_limb)));
   return Sp[0] = abs_wmin(g), Ip++, Continue(); }
  Have(box_req); emit_int(n < 0 ? (intptr_t) (0 - (uintptr_t) n) : n);
  return Sp[0] = _res, Ip++, Continue(); }
 if (bigp(a)) {
  struct ai_big *x = (struct ai_big*) a;
  if (x->slen > 0) return Ip++, Continue();         // already non-negative
  uintptr_t bytes = ai_big_bytes(x); Have(b2w(bytes));
  x = (struct ai_big*) Sp[0];                         // re-read post-Have
  struct ai_big *y = (struct ai_big*) Hp; Hp += b2w(bytes);
  memcpy(y, x, bytes); y->slen = -x->slen;           // flip the sign
  return Sp[0] = word(y), Ip++, Continue(); }
 if (arrp(a)) {                                       // vector -> scalar: the Euclidean (L2) norm
  struct ai_vec *v = vec(a); uintptr_t i, n = vec_nelem(v);   // sqrt(sum of squares); abs of a
  ai_flo_t s = 0;                                      // complex elem is its 2-vector modulus; ai_C sums 2n floats
  if (v->type == ai_C) { ai_flo_t *fp = vec_data(v); for (i = 0; i < 2*n; i++) s += fp[i] * fp[i]; }
  else for (i = 0; i < n; i++) { ai_flo_t e = vec_get_flo(v, i); s += e * e; }
  Have(box_req); emit_flo(ai_sqrt(s)); return Sp[0] = _res, Ip++, Continue(); }
 if (tabp(a)) {                                       // table: its key count (so (int (abs t)) == (len t))
  Have(box_req); emit_int((intptr_t) map_len(a)); return Sp[0] = _res, Ip++, Continue(); }
 return Sp[0] = ZeroPoint, Ip++, Continue(); }

// fill f64 array r with arg of each element of v (a ai_C packed or ai_Z/ai_R real
// array, same shape). &-free, but ai_noinline to keep lvm_carg's tail call clean.
static ai_noinline void carg_fill(struct ai_vec *r, struct ai_vec *v) {
 uintptr_t n = vec_nelem(v);
 ai_flo_t *rf = vec_data(r);
 if (v->type == ai_C) { ai_flo_t *fp = vec_data(v);
  for (uintptr_t p = 0; p < n; p++) rf[p] = ai_atan2(fp[2*p+1], fp[2*p]); }
 else for (uintptr_t p = 0; p < n; p++) rf[p] = ai_atan2(0, vec_get_flo(v, p)); }

// (arg z): phase angle atan2(im, re) as a float. On a real number this is 0 for
// non-negative and pi for negative; on a complex/real array, an f64 array of the
// per-element phase (so (arg (C 1 x)) = atan elementwise); on a non-number, nil.
lvm(lvm_carg) {
 word a = Sp[0], _res;
 if (Cp(a)) { ai_flo_t r = ai_atan2(cplx_im(a), cplx_re(a));
  Have(box_req); emit_flo(r); return Sp[0] = _res, Ip++, Continue(); }
 if (arrp(a)) {
  struct ai_vec *v = vec(a);
  if (v->type == ai_O) return Sp[0] = ZeroPoint, Ip++, Continue();   // object array -> nil
  uintptr_t R = v->rank, n = 1; for (uintptr_t i = 0; i < R; i++) n *= v->shape[i];
  uintptr_t bytes = sizeof(struct ai_vec) + R * sizeof(word) + n * ai_T[ai_R];
  Have(b2w(bytes));
  v = vec(Sp[0]);                                           // re-read post-Have
  struct ai_vec *r = (struct ai_vec*) Hp; Hp += b2w(bytes);
  ini_vec(r, ai_R, R);
  for (uintptr_t i = 0; i < R; i++) r->shape[i] = v->shape[i];
  carg_fill(r, v);
  return Sp[0] = word(r), Ip++, Continue(); }
 if (isnum(a)) { ai_flo_t r = ai_atan2(0, toflo(a));
  Have(box_req); emit_flo(r); return Sp[0] = _res, Ip++, Continue(); }
 return Sp[0] = ZeroPoint, Ip++, Continue(); }
