#ifndef _love_h
#define _love_h
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>

#define Width(_) b2w(sizeof(_))
#define ai_core_of(ai_) ((struct ai*)((intptr_t)(ai_)&~(sizeof(intptr_t)-1)))
#define ai_code_of(g) ((enum ai_status)((intptr_t)(g)&(sizeof(intptr_t)-1)))
#define ai_ok(g) (ai_code_of(g) == ai_status_ok)

#define putcharm(_) ((ai_word)(((uintptr_t)(ai_word)(_)<<1)|1))
#define getcharm(_) ((ai_word)(_)>>1)

#ifndef EOF
#define EOF (-1)
#endif

#ifndef NAN
#define NAN (__builtin_nanf(""))
#endif

#define ai_zero putcharm(0)
#define ai_inline inline __attribute__((always_inline))
#define ai_noinline __attribute__((noinline))
// no identical-code-folding: the data sentinels have byte-identical bodies but
// their address IS their type tag (gcc -Os runs -fipa-icf; clang only under --icf=all)
#if defined(__GNUC__) && !defined(__clang__)
#define ai_noicf __attribute__((noipa))
#else
#define ai_noicf
#endif
// THE DATA SLOT LAYOUT: the sentinels tile one section at a fixed stride in enum d
// order, so a value's rep is arithmetic on its ap (love.c's DSENT, love_data.ld,
// ai_typ below). the seats that go without ask the sentinels BY NAME instead, and
// answer the same enum d: wasm has no sections to lay, mach-o spells them
// `segment,section`, and love0 opts out (host/build.mk) so the bootstrap owes no
// linker script. safe in every direction -- the heap image carries an ap as its
// INDEX in image_extra_aps, so a layout never crosses between two binaries.
#ifndef ai_data_section
#if defined(__wasm__) || defined(__APPLE__)
#define ai_data_section 0
#else
#define ai_data_section 1
#endif
#endif
// bytes per slot -- the linker pins it, love.h reads it. sized off the FATTEST
// body any backend emits, and the sentinels already SHARE their handlers (each is
// one tail jump to a data_*_apply), so this measures CALL LOWERING, not code:
// arm64 4, x64 5-8, thumb2 60, riscv64 88. the two fat ones spill their params to
// the frame and read them straight back; the IR sweeps that fold exactly that
// (stld, dehusk) run on x64 alone -- gen.l's build says so and says why. they do
// port, and pay (measured: riscv64 76, thumb2 48), but they land inside the
// regen's own ranking, where they cost arm64 its param homing. a rung, not a patch.
// ⚠ the LINK is the check: a body past the stride pushes ld's location counter
// backwards and it refuses out loud, and holo -- ours -- lays the grain the object
// declares, so neither can hand back a tiling ai_typ would misread.
#define ai_data_stride 128
#define ai_data_n 9         // slots; _Static_assert'd against enum d below
#define ai_digits "0123456789abcdefghijklmnopqrstuvwxyz"
#define countof(_) (sizeof(_)/sizeof(*_))

#ifndef ai_tco
#define ai_tco 1
#endif

// port read-buffer size in bytes (the one buffered-io knob)
#ifndef ai_iobuf
#define ai_iobuf 4096
#endif

#if ai_tco
#define _lvm(n, ...) struct ai *n(struct ai *restrict g, union u *Ip, ai_word *Hp, ai_word *restrict Sp, ##__VA_ARGS__)
#define Ap(fn, g, ...) fn(g, Ip, Hp, Sp, ##__VA_ARGS__)
#define Continue() Ap(Ip->ap, g)
// the stepped/answering tails as BARE CALLS (ai_musttail's operand may not be a comma):
// Next steps n cells, Nextp also pops k, Answer stores v at the top, Answerp under a pop
// of k, Push opens a fresh slot, Resume re-reads the packed g. the store rides INSIDE the
// Sp argument, sequenced by its own comma. ⚠ v must not touch Hp or Ip -- they are
// SIBLING arguments, the mutation would be unsequenced: such a site stores first, then
// `ai_musttail return Next(1);`. n and k evaluate twice: literals only.
#define Next(n) Ip[n].ap(g, Ip + (n), Hp, Sp)
#define Nextp(n, k) Ip[n].ap(g, Ip + (n), Hp, Sp + (k))
#define Answer(v) Ip[1].ap(g, Ip + 1, Hp, (Sp[0] = (v), Sp))
#define Answerp(k, v) Ip[1].ap(g, Ip + 1, Hp, (Sp[k] = (v), Sp + (k)))
#define Push(v) Ip[1].ap(g, Ip + 1, Hp, (*++Sp = (v), Sp))
#define Resume() g->ip->ap(g, g->ip, g->hp, g->sp)
#define Pack(g) (g->ip = Ip, g->hp = Hp, g->sp = Sp)
#define Unpack(g) (Ip = g->ip, Hp = g->hp, Sp = g->sp)
// every VM tail spells `ai_musttail return ..` and every compiler is HELD to the jump:
// clang/gcc 15+ take the attribute natively; mooncc marks the annotated call and its
// sibcall pass spells the tail jump or REFUSES the compile (musttail-not-a-tail /
// musttail-escape) -- an opportunistic miss is one frame per dispatch and a stack
// overflow down some long read. `make vmret` stays as the cross-check on the shipped
// binary. ⚠ the extra-arg lvms (vbin, gc, vmap*..) keep PLAIN returns: musttail wants
// matching prototypes.
#if defined(__mooncc__) || defined(__clang__) || (defined(__GNUC__) && __GNUC__ >= 15)
#define ai_musttail __attribute__((musttail))
#if defined(__GNUC__) && !defined(__clang__)
// gcc's "maybe" escape lint: an address-taken local handed to an EARLIER helper trips it,
// but the ⚠ no-scratch-in-lvm_ discipline already forbids a frame address outliving its call
#pragma GCC diagnostic ignored "-Wmaybe-musttail-local-addr"
#endif
#else
#define ai_musttail
#endif
#else
#define _lvm(n, ...) struct ai *n(struct ai *restrict g, ##__VA_ARGS__)
#define Ap(fn, g, ...) fn(g, ##__VA_ARGS__)
#define Continue() g
#define Next(n) (Ip += (n), g)
#define Nextp(n, k) (Sp += (k), Ip += (n), g)
#define Answer(v) (Sp[0] = (v), Ip += 1, g)
#define Answerp(k, v) (Sp[k] = (v), Sp += (k), Ip += 1, g)
#define Push(v) (*++Sp = (v), Ip += 1, g)
#define Resume() g
#define ai_musttail
#define Hp g->hp
#define Sp g->sp
#define Ip g->ip
#define Pack(g) ((void)0)
#define Unpack(g) ((void)0)
#endif
#define lvm(...) ai_noinline ai_noicf _lvm(__VA_ARGS__)

// ok thanks
typedef intptr_t ai_word;

union u;
typedef _lvm(lvm_t);

// typed n-dim array; rank 0 = scalar (no shape words); payload at shape+rank; immutable
struct ai_tray {
 lvm_t *ap;
 uintptr_t type, rank, shape[]; };

// status rides the 2 pointer tag bits, two flags: bit 0 SCARE (something is
// wrong), bit 1 MORE (more input wanted); eof = more|scare
enum ai_status { ai_status_ok = 0, ai_status_scare = 1, ai_status_more = 2, ai_status_eof = 3 };

struct ai_str {
 lvm_t *ap;
 uintptr_t len;        // byte count
 char bytes[]; };
// a cask: mutable bytes behind a 2-word wrapper, recognized by
// ap like ports. public so a host nif can wrap a C struct's bytes (host/cb.c).
struct ai_cask { lvm_t *ap; struct ai_str *str; };
// a mint: a bare nameless point -- just the hot and its serial
struct ai_mint {
 lvm_t *ap;
 uintptr_t code; };
// a nom: a named point, a flat 4-word leaf. code = serial (order key on a name
// tie); dig caches the SPELLING hash -- content, not serial, so bucket order
// never depends on intern history (the reproducible-build law).
struct ai_nom {
 lvm_t *ap;
 uintptr_t name;
 uintptr_t code;
 uintptr_t dig; };

struct ai_port_vt;   // the port's kind, in its head; spelled out below

struct ai {
 union u {
  lvm_t *ap;
  ai_word x;
  union u *m; } *ip;
 ai_word *hp, *sp;
 union u *tasks;       // RUN ring head; non-NULL after ai_ini. ⚠ no node here is
                       // fd-parked -- those move to `parked`, so a switch costs no syscall
 union u *parked;      // the PARKED ring (fd waiters) or NULL; its own ring so the
                       // fairness yield never walks it. never image-serialized.
 uintptr_t yield_ctr,  // ap-cycles since last cooperative yield (level-triggered at yield_interval)
           sweep_ctr,  // fairness yields since the last PARKED sweep; a slower counter on
                       // purpose -- a ring walk and a syscall cannot share one knob
           next_serial, // THE MINT STREAM: one monotonic counter every fresh identity draws
                        // from (task pids and nom serials alike; pre-incremented)
           next_wake_at; // raw deadline for next yield_sw snapshot's wake_at slot; 0 = always runnable
 intptr_t next_wait_fd; // fd the task suspended on, -1 = not waiting on I/O. Installed into next yield_sw snapshot's wait_fd slot.
 int next_wait_events;  // ai_wait_in (the default) or ai_wait_out (connect's handshake).
                        // ⚠ a field, not read off the parked op: the op that wants OUT
                        // may live in a frontend love.c cannot name.
 ai_word symbols;       // the WEAK intern map (string -> canonical atom), swept after the
                        // cheney fixpoint so dead spellings vanish. 0 only during early init.
 uintptr_t len;
 struct ai *pool;
 struct ai_r { ai_word *x; struct ai_r *n; } *root; // gc roots list
 struct ai_fz { // finalizers
  union u *p;
  void (*fn)(void *);
  struct ai_fz *next; } *fz;
 union { uintptr_t t0; ai_word *cp; };
 void *(*alloc)(struct ai*, void*, size_t);  // alloc(g,p,n): n>0 reserve n bytes (p ignored), n==0 free p; -> block or NULL
 uintptr_t b;
 ai_word *minor;        // minor watermark: [minor,hp) young, [end,minor) old. a raw pool
                        // pointer like hp -- recomputed every collection, never forwarded.
 uintptr_t n_gc, max_len, max_heap, // gc instrumentation (cycles, peak pool len, peak live heap; words)
           n_seen, n_evac;          // Σ per collection: occupancy entering / survivors copied.
                                    // mortality = (n_seen-n_evac)/n_seen; copy-amp = n_evac/max_heap
 // the REMEMBERED SET (the whole write barrier): old cells that took a young pointer,
 // rescanned by the next minor. rem_miss counts drops on overflow -- any miss forces
 // the next collection MAJOR, so a minor only runs under a complete set.
 ai_word *rem; uintptr_t rem_cap, rem_n, rem_hi, rem_miss;
 // the two pools: the main pool is pure MINOR (`minor` stays == end); OLD lives in
 // major_pool, its own two-space. a MINOR evacuates minor -> major active half; a
 // MAJOR drains both, compacts into the spare half, flips, rebuilds symbols, runs
 // finalizers. gc_fwd is the forwarding floor: word0 in [gc_fwd, gc_to_hi) = a copy
 // made THIS collection. gc_gen redirects bump() to major_hp during a collection.
 ai_word *major_pool, *major_base, *major_hp;   // major: malloc base (2*major_len words), active-half base, active bump
 uintptr_t major_len;                       // major half size (words)
 ai_word *gc_to_lo, *gc_to_hi, *gc_fwd;   // to-space tagp range [to_lo,to_hi) + forwarding floor (set per collection)
 ai_word *gc_f2lo, *gc_f2hi;              // a SECOND from-space range (0 = unused); a major traces {major ∪ minor} in one pass
 uintptr_t gc_gen;                        // !=0 during a generational collection: bump() targets major_hp, not hp
 uintptr_t n_minor;                       // MINOR collections so far (majors = n_gc - n_minor)
 uintptr_t minor_hi, major_hi;            // the PAUSE gauge: peak words one minor / one major copied
                                          // (gauge[14]/[15]; test/host/gcpause.l puts wall ns against them)
 uintptr_t since_major, major_live0;      // young words scanned since the last major; major live right after it.
                                          // a major fires once since_major > major_live0 + 4*minor-pool --
                                          // amortized against allocation, so dead tenured objects sweep
                                          // periodically and the pool can shrink (gen_please)
 uintptr_t win_alloc, win_copied;         // sliding window (words) for the deterministic minor-resize ratio:
                                          // overhead = copied/alloc; reset on a resize (gen_please)
 intptr_t lean;                           // resize-stickiness streak (+grow/-shrink); a resize needs |lean| >= 2
                                          // (a resize is a full copy + a total refault)
 uintptr_t n_resize;                      // pool reallocations so far -- gauge[13]; catches pool-cliff contamination
 uintptr_t budget;                        // total memory CAP in words (2*minor + 2*major); 0 = unbounded.
                                          // appel's rule: the nursery gets the free budget after the major pool.
 union {
  intptr_t v0;
  struct {
   ai_word book;   // global env map; the macro table is book[zero]. GC-forwarded in v0..end.
   ai_word scare_a, scare_b; // the last scare's condition data, stashed at the raise for
                  // the exit face (ai_scare_face_); zero zero = the bare oom
   // THE HOOKS: lisp the C lanes must reach, handed over by (seal-hook n f) and read
   // by SLOT -- no name lookup, no rebind can reach them. numbered in seal = boot
   // order; GC-traced (v0..end) + image-serialized. unsealed = zero -> hot_hook
   // traps, except slot 5, whose zero is a steady state the raise lanes nil-test.
   ai_word hot_read;  // 0: the corpus reader (p1's whole-text door, sealed by p1's own
                  // last act); zero = p1 not up yet, readtext falls back to p0
   ai_word hot_numap; // 1: the church C->lisp num-ap hook (lvm_numap/numtap, data_num_apply)
   ai_word hot_stack, hot_compose; // 2, 3: `+` and `*` OF TWO FUNCTIONS -- church add and
                  // compose, two lines of prel (lvm_addh/lvm_mulh build the partial)
   ai_word hot_opfix; // 4: the operator factor pass, sealed last; pre-seal the pass
                  // simply skips (everything up to the seal is written prefix)
   ai_word hot_help;  // 5: the INSTALLED HELP, the one DYNAMIC slot: (hear f) installs,
                  // (hear ()) uninstalls, (heard ()) answers; zero = helpless, raises
                  // take the default escape. read by ai_raise/lvm_index, never the book.
   ai_word mods;  // the MODULE REGISTRY book: name -> module-book, filled by `leave`,
                  // read by use/from. a lazy singleton, so both bootstrap prel runs
                  // capture the SAME tablet. in v0..end: traced + serialized.
   union {
    ai_word x;
    struct ai_io {
     lvm_t *ap;
     struct ai_port_vt const *vt;   // WHAT KIND OF PORT THIS IS -- the only answer there is
     ai_word ungetc_buf;            // pushed-back byte; putcharm(EOF) = empty
     // ⚠ three words: prel's tap/jug poke this layout by index (love/prel.l) --
     // a word added here is a renumbering there
    } *io; }; }; };
 intptr_t end[]; };

struct ai_def { char const *n; intptr_t x; };

// THE SOURCE LIBRARY: the .l texts lcat'd into this binary, name -> source, the rung
// `use` tries before the filesystem walk. A frontend defines ai_libs over its own
// table; the weak default answers none, so one that bakes nothing links unchanged.
// ⚠ IMMORTAL C STRINGS, and the whole point: `use` copies ONE of these for the length
// of ONE load. Nothing here is on the heap, traced by a collection, or in an image.
// ⚠ TERMINATED BY A {NULL, NULL} ROW -- there is no count to pass and none to keep.
struct ai_lib { char const *nom, *src; };
struct ai_lib const *ai_libs(void);

// host nif auto-registration: AI_NIF("name", fn) lands the entry in the ai_nifs
// section; boot drains [__start_ai_nifs, __stop_ai_nifs) via ai_defn, so an app
// adds nifs in its own host/<app>.c without touching the core. no linker script:
// the toolchain defines the bracket symbols.
#if defined(__APPLE__)
extern struct ai_def const __start_ai_nifs[] __asm("section$start$__DATA$ai_nifs");
extern struct ai_def const __stop_ai_nifs[]  __asm("section$end$__DATA$ai_nifs");
#define AI_NIF(nm, fn) \
  static struct ai_def const __attribute__((section("__DATA,ai_nifs"), used)) \
    _ainif_##fn = { (nm), (intptr_t) (fn) }
#else
extern struct ai_def const __start_ai_nifs[], __stop_ai_nifs[];
#define AI_NIF(nm, fn) \
  static struct ai_def const __attribute__((section("ai_nifs"), used)) \
    _ainif_##fn = { (nm), (intptr_t) (fn) }
#endif

// port vtable -- what a device owes, and nothing else. a NULL slot means no
// method (no readn reads END, no writen discards). neither blocks the scheduler;
// the generic layer above owns ungetc_buf.
//   writen: land up to n bytes in one motion: >0 landed, 0 no room now (caller
//     keeps the residue), -1 the device is GONE (io_wdrain drops the run).
//     ⚠ it may ALLOCATE, hence the frame BY ADDRESS: land nothing after an
//     allocating step -- grow, answer 0, let the caller re-derive src.
//     ⚠ only a door whose port keeps a write run may refuse; the static ports
//     cannot park, so their door must land what it takes.
//   readn: drink up to n waiting bytes: >0 bytes, 0 nothing yet (the scheduler
//     owns the wait), -1 end of stream. never allocates, hence frame by value.
//     ⚠ THE END IS STABLE: a spent device owes -1 to every ask, not just the
//     first (test/front/io.l law 3).
//   athand: of the next n bytes, how many are here already -- a source whose text
//     is in memory (a C string, a charlist) counts them without a device. NULL is
//     "ask the device", so a run must come out of a buffer instead. `chug` is the
//     one caller, and this is the whole of what it means to be readable now.
struct ai_port_vt {
 struct ai*(*flush)(struct ai*);
 intptr_t (*writen)(struct ai**, unsigned char const*, uintptr_t),
          (*readn)(struct ai*, unsigned char*, uintptr_t);
 uintptr_t (*athand)(struct ai*, uintptr_t); };

// only 2 tag bits on 32 bit so we can only have four of these
enum ai_status ai_fin(struct ai*);

static ai_inline size_t b2w(size_t b) {
 size_t q = b / sizeof(ai_word), r = b % sizeof(ai_word);
 return q + (r ? 1 : 0); }

lvm_t lvm_ret0, lvm_cur, lvm_port_io, lvm_help, lvm_cask,
// how a frontend nif PARKS: set g->next_wake_at (or next_wait_fd), leave Ip
// unadvanced, `return Ap(lvm_yield_sw, g)` -- the op re-runs on reschedule.
      lvm_yield_sw;

// the vtable every port backed by a real OS fd wears; the frontend defines it,
// and its ADDRESS is what says "there is an fd behind this one" (ai_io_fd).
extern struct ai_port_vt const ai_fd_port_vt;
// what a closed port wears: every door a no-op, and no fd behind it. a frontend
// owning `close` swaps this in -- that swap IS the close, there is no other mark.
extern struct ai_port_vt const ai_closed_vt;

// close an OS fd backing a heap port; weak no-op default, the host overrides
// with close(2). called by ai_io_alloc's finalizer.
void ai_fd_close(int fd);

// THE FD PORT: the head plus the descriptor. ⚠ the vt IS THE LICENSE to read it --
// nothing casts here without ai_io_fd, which answers -1 for every port whose door
// is not a device (a tap, a jug, a closed port: all real ports, none with an fd).
struct ai_fio { struct ai_io io; ai_word fd; };
intptr_t ai_io_fd(struct ai_io const*);
// THE BUFFERED PORT: the fd port plus both buffer lanes, PRIVATE to the generic
// dispatch (prel's tap/jug poke the bare shape; static ports stay bare -- nothing
// traces a static). rbuf/wbuf hold an ai_str backing or 0; [rpos,rlen) bounds the
// pending read run, wlen the filled write prefix. GC walks the extension words as
// ordinary thread words.
struct ai_bio { struct ai_fio f; ai_word rbuf, rpos, rlen, wbuf, wlen; };
// the two faces host nifs need (guards inside; both 0/no-op on a bare port):
// pending = bytes waiting in the read buffer; drain pops up to n of them into dst
uintptr_t ai_io_pending(struct ai*, struct ai_io*);
uintptr_t ai_io_read_drain(struct ai*, struct ai_io*, unsigned char*, uintptr_t);
struct ai *ai_io_wflush(struct ai*, struct ai_io*);   // TRY to push the write run out
uintptr_t ai_io_wpending(struct ai*, struct ai_io*);  // ... and what the device would not take.
// close and seal call the pair: wflush, then park on a nonzero wpending (see
// lvm_yield_sw). ⚠ neither may shut the fd on a residue -- a truncated stream.
struct ai *ai_io_alloc(struct ai *g, int fd);
// raw bytes at an fd with NO g machinery -- the GC-context finalizer drains a
// dying port through it. weak no-op default; the host overrides with write(2).
void ai_fd_drain(int fd, void const*, uintptr_t);

uintptr_t ai_clock(void); // used by garbage collector
intptr_t ai_nclock(void); // the fine interval clock (ns); weak ms-degraded default in love.c, hosts override with a real ns source
void ai_sleep(uintptr_t ticks); // per-frontend deep wait for at most `ticks` ai_clock()
// units (0 = infinite); no input wakeup (parked streams go via ai_wait_fds). default no-op.

struct ai
 *ai_ini(void),
 *ai_ini_m(void*(*)(struct ai*, void*, size_t)),
 *ai_evals_(struct ai*, const char*),
 *ai_egg_(struct ai*, char const*, char const*, char const*),  // (egg, p1, corpus)
 *ai_defn(struct ai*, struct ai_def const*, uintptr_t),   // ⚠ IMMORTAL values only
 *ai_defv(struct ai*, char const*),                // its twin for a LIVE heap value (rides sp[0], stays there)
 *ai_layer_(struct ai*),      // push a fresh writable layer (the runtime's enter); every frontend opens its session with it
 *ai_unsplice_(struct ai*);   // drop the link below the head (the runtime's bare leave)

// the heap-image codec (stdio-free): save compacts g and serializes into a fresh
// g->alloc'd buffer; load reconstructs a fresh g, or NULL on any mismatch (the
// caller boots normally). buffer-based so a freestanding frontend needs no filesystem.
void ai_image_note(uintptr_t stage);   // wake-progress hook, weak no-op; a port bringing the wake up on new metal overrides it
// a kept ABSOLUTE only survives a wake if it aims inside the binary's own load segments
// (one ASLR delta shifts them all); anything else -- a JIT W^X page, an mmap, a shared
// library -- dies with the bake process, so the dump refuses it. only the host can answer
// that (love.c is freestanding), and it answers by PARAMETER, like ai_image_load_m's
// allocator -- the audit owns no state here. a NULL guard is audit off.
// asked of every candidate absolute, and told which object carries it (heap word offset
// + that object's hot) -- enough for a seat to name an offender in its OWN frame, which
// is where a report belongs. answer 0 and the dump refuses.
struct ai_image_guard { uintptr_t (*ok)(void *ctx, uintptr_t v, uintptr_t off, uintptr_t ap); void *ctx; };
void *ai_image_save(struct ai*, uintptr_t *outlen, struct ai_image_guard const*);
void *ai_image_save_(struct ai*, uintptr_t *outlen, struct ai_image_guard const*);   // the unguarded worker: a MID-EVAL dump (the bake nif)
struct ai *ai_image_load(void const *buf, uintptr_t len);
struct ai *ai_image_load_m(void const *buf, uintptr_t len, void *(*)(struct ai*, void*, size_t));   // allocator-parameterized (a device heap has no malloc)

// the terminal scare face: prints ";; a b\n" (show forms) to the err port from
// the stashed condition data; the bare oom prints ";; oom@len=N\n".
void ai_scare_face_(struct ai*);

extern struct ai_fio ai_stdin, ai_stdout, ai_stderr;

// the boot driver: ai_egg_(g, egg, p1, corpus) applies love/egg.l to the quoted
// corpus -- compile the compiler with c0, recompile the corpus through itself,
// install as `ev`. the list is STITCHED (p0 reads egg + p1; p1, evaluated a step
// earlier, reads the corpus), so p1.l is the ONLY .l held to the pure lisp subset.
// ⚠ `corpus` is prel ++ ev juxtaposed at the call site -- one p1 read, because
// p1text mints a fresh list where p0onto extends the one on the stack.

// === internal API shared with host / free ===
#define A(o) two(o)->a
#define B(o) two(o)->b
#define len(_) (((struct ai_str*)(_))->len)
#define txt(_) (((struct ai_str*)(_))->bytes)
#define avail(g) ((uintptr_t)(g->sp-g->hp))
#define num(_) ((word)(_))
#define word(_) num(_)
#define oddp(_) ((uintptr_t)(_)&1)
#define evenp(_) !oddp(_)
#define cell(_) ((union u*)(_))
// the BLUE FLOOR: extra stack slack on every avail check, a buffer against
// off-by-one overshoots. 0 under GL_BOOTSTRAP so love0 keeps strict discipline;
// override with -Dai_avail_floor=N.
#ifndef ai_avail_floor
# ifdef GL_BOOTSTRAP
#  define ai_avail_floor 0
# else
#  define ai_avail_floor 8
# endif
#endif
#define Have(n) if (Sp < Hp + (n) + ai_avail_floor) return Ap(lvm_gc, g, (n) + ai_avail_floor)
#define Have1() Have(1)
#define ai_pop1(g) (*(g)->sp++)
#define op(nom, n, x) lvm(nom) { intptr_t _ = (x); *(Sp += n-1) = _; Ip++; ai_musttail return Continue(); }
#define zero ai_zero
struct ai_chain { lvm_t *ap; intptr_t a, b; };
// enum q, the value-kind lattice for generic dispatch: KMint the blue floor, then
// KNom, the arithmetic lane [KCharm..KTrayO] (scalars then their tray mirrors),
// the sequence lane [KString..KChain], tablet, thread last -- each dyadic lane one
// contiguous range, `max` the within-lane promotion join. ⚠ DISPATCH order only:
// the total compare order is cmp_rank's separate remap (love.c). KN is the matrix
// dimension.
// enum d is the other question: what a heap object's hot SAYS it is. ai_typ can
// answer these nine and nothing else, so a switch over them is exhaustive and
// carries no default -- add a data sentinel and -Wswitch names every site. the two
// do NOT share values; mx.h's ai_kind_of_d is the one crossing, and a tray is the
// one rep that dispatches four ways.
// both rosters are mx.l's -- edit THAT, not kinds.h; `make test_clay`
// regenerates and fails on drift.
#include "kinds.h"
typedef ai_word num, word;
// the unique empty string: data-segment, never moved (gcp's out-of-pool
// short-circuit); strings are immutable, so one suffices.
extern const struct ai_str ai_str_empty;
#define EmptyString ((word) &ai_str_empty)
// (): the one serial-0 mint, a data-segment const shared by EVERY core --
// immortal, never copied, so () is bakeable. serial 0 is never drawn, so it is
// unique and least; .ap = lvm_sym gives mintp/const-1-apply/()-print for free.
extern const struct ai_mint ai_mint_zero;
#define ZeroPoint ((word) &ai_mint_zero)
// one parked fd. ⚠ the layout IS poll(2)'s struct pollfd, on purpose: the host
// polls the block DIRECTLY (host/main.c static-asserts the match). the scheduler
// fills .fd/.events and zeroes .revents. ⚠ .revents IS READ BACK: nonzero = ready,
// and the scheduler takes that answer instead of re-asking the kernel per fd.
// filling it is optional (all-zero = "nothing to say") but saves a syscall per
// parked task per scheduling decision.
struct ai_wait_fd { int fd; short events, revents; };

// the two park directions, in poll(2)'s own bit values (host/main.c
// static-asserts them). ⚠ never OR them and ask as one: a socket is almost
// always writable, so a reader polled for both would spin.
#define ai_wait_in  1
#define ai_wait_out 4

// wait until one of n parked fds is ready for what its `events` asks, or ticks
// elapse (0 = no deadline). ⚠ n has no ceiling: the block rides the runtime's own
// uncommitted heap gap. ⚠ the SCHEDULER fills `events` -- a blanket mask would
// wake readers on writable.
void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ticks);
// the same question WITHOUT the wait, for the fairness path (must not block, one
// ask covers the ring). ⚠ AUTHORITATIVE, unlike ai_wait_fds's block: the weak
// default asks ai_ready per fd, so all-zero means "none ready". the host replaces
// the loop with a single poll(2).
void ai_ready_fds(struct ai_wait_fd *fds, int n);
bool ai_ready(int fd, int events), ai_strp(ai_word);
struct ai
 *ai_please(struct ai*, uintptr_t),
 *ai_push(struct ai*, uintptr_t, ...),
 *ai_strof(struct ai*, const char*),
 *gxl(struct ai*),
 *gxr(struct ai*),
 *intern(struct ai*),
 *str0(struct ai*, uintptr_t);
lvm(lvm_gc, uintptr_t);
// any value -> its enum q: KCharm for a fixnum, KHot for a non-data heap pointer,
// else ai_typ's rep, a tray refined by element tier (KTrayZ..KTrayO).
// both the +/* matrices and the apply sentinels dispatch on this.
enum q ai_kind(word);
extern union u const numap_drive[];          // [ap; swap; ret0] driver that runs (num-ap n x); shared by fixnum + data num apply
lvm_t lvm_ap, lvm_chain, lvm_tray, lvm_sym, lvm_nom, lvm_str, lvm_big, lvm_gembox, lvm_sunbox, lvm_twinbox; // the data-kind sentinels (+ ap); defined in love.c, read by inline predicates and ai_typ
// recover a data value's rep from its ap. the sentinels tile one section at
// ai_data_stride in enum d order, so the SLOT IS THE KIND: one subtract answers
// both questions, and the compiler shares it between a datp and the typ after it.
// the base is lvm_sym -- slot 0 IS the start, so no linker-synthesized bracket is
// owed anywhere. DChain is enum d's last member, so DChain+1 is its count.
_Static_assert(DChain + 1 == ai_data_n, "enum d and the love_data slots disagree");
#if ai_data_section
static ai_inline bool in_data(void *a) {
 return (uintptr_t) ((char*) a - (char*) lvm_sym) < (uintptr_t) (ai_data_n * ai_data_stride); }
static ai_inline enum d ai_typ(union u *o) {
 return (enum d) ((uintptr_t) ((char*) o->ap - (char*) lvm_sym) / ai_data_stride); }
#else
// the seats with no section to lay ask by name instead. ⚠ the ORDER is the
// measured frequency, not enum d's -- over a corpus run: chain 62%, nom 19%,
// string 13%, mint 5%, big 1.6%, the other three under a tenth of a percent each.
static ai_inline bool in_data(void *a) {
 lvm_t *p = (lvm_t*) a;
 return p == lvm_chain || p == lvm_nom || p == lvm_str || p == lvm_sym || p == lvm_big
     || p == lvm_tray || p == lvm_sunbox || p == lvm_gembox || p == lvm_twinbox; }
static ai_inline enum d ai_typ(union u *o) {
 lvm_t *p = o->ap;
 return p == lvm_chain  ? DChain
      : p == lvm_nom    ? DNom
      : p == lvm_str    ? DString
      : p == lvm_sym    ? DMint
      : p == lvm_big    ? DBig
      : p == lvm_tray   ? DTray
      : p == lvm_sunbox ? DSun
      : p == lvm_gembox ? DGem
      :                   DTwin; }   // the 9th and last: lvm_twinbox
#endif
uintptr_t hash(struct ai*, word), ai_tray_bytes(struct ai_tray*);
#define str(_) ((struct ai_str*)(_))
#define lamp evenp
#define two(_) ((struct ai_chain*)(_))
static ai_inline bool chainp(word _) { return lamp(_) && cell(_)->ap == lvm_chain; }
static ai_inline void *bump(struct ai *g, uintptr_t n) {
 if (g->gc_gen) { void *x = g->major_hp; g->major_hp += n; return x; }   // a generational collection promotes into the major pool
 if (avail(g) < n) __builtin_trap();
 void *x = g->hp; g->hp += n; return x; }
static ai_inline struct ai_chain *ini_chain(struct ai_chain *w, intptr_t a, intptr_t b) {
 return w->ap = lvm_chain, w->a = a, w->b = b, w; }
static ai_inline struct ai *encode(struct ai *g, enum ai_status s) { return
  (struct ai*) ((uintptr_t) g | s); }
// re-raise a failed op's scare at the installed help, else the status-encoded
// core straight back to C (love.c)
struct ai *ghelp(struct ai*),
          *grbufg(struct ai *g, uintptr_t len);
// ⚠ ai_have IS the phrase "this call may collect"; under AI_GC_STRESS every one
// DOES, so a raw local held across it goes stale on the first run, not years
// later. AI_GC_CHECK is the other half: it checks the collector where this
// checks the mutator (doc/verify.md).
static ai_inline struct ai *ai_have(struct ai *g, uintptr_t n) {
#ifdef AI_GC_STRESS
 return !ai_ok(g) ? g : ai_please(g, n);
#else
 return !ai_ok(g) || avail(g) >= n ? g : ai_please(g, n);
#endif
}

#endif
