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
// no identical-code-folding: the data sentinels' bodies are byte-identical but
// their address is the type tag (gcc -Os runs -fipa-icf)
#if defined(__GNUC__) && !defined(__clang__)
#define ai_noicf __attribute__((noipa))
#else
#define ai_noicf
#endif
// ai_data_section / ai_data_stride / ai_data_n ride kinds.h below, laid by mx.l
// beside the enum d roster they are a layout of
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
// the stepped/answering tails as bare calls (ai_musttail's operand may not be a comma):
// Next steps n cells, Nextp also pops k, Answer stores v at the top, Answerp under a pop
// of k, Push opens a fresh slot, Resume re-reads the packed g. the store rides inside the
// Sp argument, sequenced by its own comma, so v must not touch Hp or Ip -- sibling
// arguments, unsequenced: such a site stores first, then `ai_musttail return Next(1);`.
// n and k evaluate twice: literals only.
#define Next(n) Ip[n].ap(g, Ip + (n), Hp, Sp)
#define Nextp(n, k) Ip[n].ap(g, Ip + (n), Hp, Sp + (k))
#define Answer(v) Ip[1].ap(g, Ip + 1, Hp, (Sp[0] = (v), Sp))
#define Answerp(k, v) Ip[1].ap(g, Ip + 1, Hp, (Sp[k] = (v), Sp + (k)))
#define Push(v) Ip[1].ap(g, Ip + 1, Hp, (*++Sp = (v), Sp))
#define Resume() g->ip->ap(g, g->ip, g->hp, g->sp)
#define Pack(g) (g->ip = Ip, g->hp = Hp, g->sp = Sp)
#define Unpack(g) (Ip = g->ip, Hp = g->hp, Sp = g->sp)
// every VM tail spells `ai_musttail return ..` and every compiler is held to the jump:
// clang/gcc 15+ take the attribute, mooncc's sibcall pass spells it or refuses the
// compile -- an opportunistic miss is one frame per dispatch and a stack overflow down
// some long read. `make vmret` cross-checks the shipped binary. the extra-arg lvms
// (vbin, gc, vmap*..) keep plain returns: musttail wants matching prototypes.
#if defined(__mooncc__) || defined(__clang__) || (defined(__GNUC__) && __GNUC__ >= 15)
#define ai_musttail __attribute__((musttail))
#if defined(__GNUC__) && !defined(__clang__)
// gcc's "maybe" escape lint: an address-taken local handed to an earlier helper trips
// it, and the no-scratch-in-lvm_ discipline already forbids a frame address outliving its call
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

typedef intptr_t ai_word;

union u;
typedef _lvm(lvm_t);

// typed n-dim array; rank 0 = scalar (no shape words); payload at shape+rank; immutable
struct ai_tray {
 lvm_t *ap;
 uintptr_t type, rank, shape[]; };

// status rides the 2 pointer tag bits: bit 0 scare, bit 1 more input wanted; eof = both
enum ai_status { ai_status_ok = 0, ai_status_scare = 1, ai_status_more = 2, ai_status_eof = 3 };

struct ai_str {
 lvm_t *ap;
 uintptr_t len;        // byte count
 char bytes[]; };
// a cask: mutable bytes behind a 2-word wrapper, recognized by ap like ports.
// public so a host nif can wrap a C struct's bytes (host/cb.c).
struct ai_cask { lvm_t *ap; struct ai_str *str; };
// a mint: a bare nameless point -- just the hot and its serial
struct ai_mint {
 lvm_t *ap;
 uintptr_t code; };
// a nom: a named point, a flat 4-word leaf. code = serial (the order key on a name
// tie); dig caches the spelling hash -- content, so bucket order never depends on
// intern history, which is the reproducible-build law.
struct ai_nom {
 lvm_t *ap;
 uintptr_t name, code, dig; };

struct ai_port_vt;   // the port's kind, in its head; spelled out below

struct ai {
 union u {
  lvm_t *ap;
  ai_word x;
  union u *m; } *ip;
 ai_word *hp, *sp;
 union u *tasks;       // run ring head; non-NULL after ai_ini. no node here is
                       // fd-parked -- those move to `parked`, so a switch costs no syscall
 union u *parked;      // the parked ring (fd waiters) or NULL; its own ring so the
                       // fairness yield never walks it. never image-serialized.
 // the sub-word task scalars, adjacent so the five ride two words
 uint16_t yield_ctr,   // ap-cycles since last cooperative yield (level-triggered at yield_interval)
          sweep_ctr;   // fairness yields since the last parked sweep; slower on purpose --
                       // a ring walk and a syscall cannot share one knob
 int next_wait_fd,     // fd the task suspended on, -1 = not waiting on I/O; installed into
                       // the next yield_sw snapshot's wait_fd slot
     next_wait_events, // ai_wait_in (the default) or ai_wait_out (connect's handshake). a
                       // field, not read off the parked op: the op that wants out may live
                       // in a frontend love.c cannot name.
     b;                // what the last port refill left: a byte, EOF, or IoWouldBlock
 uintptr_t next_serial, // the mint stream: one monotonic counter every fresh identity draws
                        // from (task pids and nom serials alike; pre-incremented)
           next_wake_at; // raw deadline for next yield_sw snapshot's wake_at slot; 0 = always runnable
 ai_word symbols;       // the weak intern map (string -> canonical atom), swept after the
                        // cheney fixpoint so dead spellings vanish. 0 only during early init.
 uintptr_t len;         // main-pool size in words: the core sits at its base, [end,hp) is the young heap
 struct ai_r { ai_word *x; struct ai_r *n; } *root; // gc roots list
 struct ai_fz { // finalizers
  union u *p;
  void (*fn)(void *);
  struct ai_fz *next; } *fz;
 void *(*alloc)(struct ai*, void*, size_t); // alloc(g,p,n): n>0 reserve n bytes (p ignored), n==0 free p; -> block or NULL
 uintptr_t n_gc, max_len, max_heap, // gc instrumentation (cycles, peak pool len, peak live heap; words)
           n_seen, n_evac;          // Σ per collection: occupancy entering / survivors copied.
                                    // mortality = (n_seen-n_evac)/n_seen; copy-amp = n_evac/max_heap
 // the remembered set, which is the whole write barrier: old cells that took a young
 // pointer, rescanned by the next minor. rem_miss counts drops on overflow -- any miss
 // forces the next collection major, so a minor only runs under a complete set.
 ai_word *rem; uint32_t rem_n, rem_hi, rem_miss;   // all three bounded by AiRemCap, the fixed capacity
 // the sub-word collector/codec scalars, adjacent so the four ride the rem set's tail
 bool gc_gen,                             // set during a generational collection: bump() targets major_hp, not hp
      sym_raw;                            // the intern map is still the image's own: a minor scans it in place
                                          // and a woken one does not survive that, so the first collection after
                                          // a wake must be a major (major_symbols_rebuild)
 int8_t lean;                             // resize-stickiness streak (+grow/-shrink); a resize needs |lean| >= 2
                                          // (a resize is a full copy + a total refault)
 // the two pools: the main pool is pure minor, the young heap being [end, hp); old lives
 // in major_pool, its own two-space. a minor evacuates young -> the major active half; a
 // major drains both, compacts into the spare half, flips, rebuilds symbols, runs
 // finalizers. the ranges the pass itself walks are `struct ai_gcx`, on the collector's
 // own C stack (core/love.c).
 ai_word *major_pool, *major_base, *major_hp;   // major: malloc base (2*major_len words), active-half base, active bump
 uintptr_t
   major_len,                       // major half size (words)
   n_minor,                    // minor collections so far (majors = n_gc - n_minor)
   minor_hi, major_hi,            // the pause gauge: peak words one minor / one major copied
                                  // (gauge[14]/[15]; test/host/gcpause.l puts wall ns against them)
   since_major, major_live0,      // young words scanned since the last major; major live right
                                  // after it. a major fires once since_major > major_live0 +
                                  // 4*minor-pool, so tenured garbage sweeps and the pool can shrink
   win_alloc, win_copied,         // sliding window (words) for the deterministic minor-resize ratio:
                                          // overhead = copied/alloc; reset on a resize (gen_please)
   n_resize,                      // pool reallocations so far -- gauge[13]; catches pool-cliff contamination
   // the pinned prefix: the first `froze` words of major_base ride a major verbatim, so
   // frozen objects keep their heap offsets and a later image's blob begins with an earlier
   // one's (doc/misc/plan/image-chain.md). immortal, so only ai_image_freeze sets it.
   froze,
   budget,                     // total memory cap in words (2*minor + 2*major); 0 = unbounded.
                                            // appel's rule: the nursery gets the free budget after the major pool.
   minor0, major0, ratio;         // the other three live knobs: nursery floor, the major pool's
                                          // grow/shrink step, the copy-overhead setpoint. seeded at
                                          // ai_ini from ai_minor0/ai_major0/ai_gc_ratio; `tune` moves
                                          // all four. untraced: a bake does not carry them.
 union {
  intptr_t v0;
  struct {
   ai_word
     book,   // global env map; the macro table is book[zero]. GC-forwarded in v0..end.
     scare_a, scare_b, // the last scare's condition data, stashed at the raise for
                  // the exit face (ai_scare_face_); zero zero = the bare oom
   // the hooks: lisp the C lanes must reach, handed over by (seal-hook n f) and read by
   // slot -- no name lookup, so no rebind can reach them. numbered in seal = boot order;
   // GC-traced (v0..end) and image-serialized. unsealed = zero traps, except 5 and 7.
     hot_read,  // 0: the corpus reader (p1's whole-text door, sealed by p1's own
                  // last act); zero = p1 not up yet, readtext falls back to p0
     hot_numap, // 1: the church C->lisp num-ap hook (lvm_numap/numtap, data_num_apply)
     hot_stack, hot_compose, // 2, 3: `+` and `*` of two functions -- church add and
                  // compose, two lines of prel (lvm_addh/lvm_mulh build the partial)
     hot_opfix, // 4: the operator factor pass, sealed last; pre-seal the pass
                  // simply skips (everything up to the seal is written prefix)
     hot_help,  // 5: the installed help, the one dynamic slot: (hear f) installs, (hear ())
                  // uninstalls, (heard ()) answers; zero = nothing heard and raises take the
                  // default escape. read by ai_raise/lvm_index, never the book. per task --
                  // saved into the node and restored on the switch, inherited at spawn: one
                  // shared help would land an escaping handler in the wrong stack.
     hot_io,    // 6: the task's stdio, the second dynamic slot: (wear (i o e)) re-seats
                  // in/out/err for the running task, (wear ()) hands them back. zero = the
                  // console and the steady state, so every op tests one word first. per task
                  // like the help above -- dup2 is the process's. op-level only: id?, peek
                  // and the image still see the static, because prel's tap/jug poke the port
                  // head by index and must keep seeing it.
     hot_show,  // 7: `show` -- what `string` coerces the kinds it cannot spell through.
                  // unsealed = identity: show is post.l's, and prel runs before it.
     mods,  // the module registry book: name -> module-book, filled by `leave`, read by
                  // use/from. a lazy singleton, so both bootstrap prel runs capture the same
                  // tablet. in v0..end: traced and serialized.
     inport, // the buffered stdin port, or 0. a seat that can put fd 0 back where its
                  // reader stopped mints one at boot and parks it here -- `in` stays the
                  // static and reads through it (love.c's rbio_of). traced here so a
                  // collection forwards it: a static port could not hold a heap run.
     inflag; // fd 0's flags as we found them (a charm), or 0 for "we left them alone".
                  // set when a seat takes the O_NONBLOCK bit for the whole run instead of
                  // toggling it per read; the same handoff sites put it back. raw 0 is no
                  // charm, so the boot value cannot be read as a saved O_RDONLY.
   union {
    ai_word x;
    struct ai_io {
     lvm_t *ap;
     struct ai_port_vt const *vt;   // what kind of port this is -- the only answer there is
     ai_word ungetc_buf;            // pushed-back byte; putcharm(EOF) = empty
     // three words: prel's tap/jug poke this layout by index (love/prel.l), so a word
     // added here is a renumbering there
    } *io; }; }; };
 intptr_t end[]; };

struct ai_def { char const *n; intptr_t x; };

// the source library: the .l texts lcat'd into this binary, name -> source, the rung
// `use` tries before the filesystem walk. a frontend defines ai_libs over its own table;
// the weak default answers none, so one that bakes nothing links unchanged. immortal C
// strings -- nothing here is on the heap, traced by a collection, or in an image.
// terminated by a {NULL, NULL} row; there is no count to pass and none to keep.
struct ai_lib { char const *nom, *src; };
struct ai_lib const *ai_libs(void);

// host nif auto-registration: AiNif("name", fn) lands the entry in the ai_nifs section
// and boot drains [__start_ai_nifs, __stop_ai_nifs) through ai_defn, so an app adds nifs
// in its own host/<app>.c. no linker script -- the toolchain defines the bracket symbols.
// AiModNifs("mod", table) is the module twin: one row per (module, def table), so an
// app's nifs register under its module and (module 'mod ..) text reopens the same one.
struct ai_mod { char const *mod; struct ai_def const *defs; uintptr_t n; };
#if defined(__APPLE__)
extern struct ai_def const __start_ai_nifs[] __asm("section$start$__DATA$ai_nifs");
extern struct ai_def const __stop_ai_nifs[]  __asm("section$end$__DATA$ai_nifs");
extern struct ai_mod const __start_ai_mods[] __asm("section$start$__DATA$ai_mods");
extern struct ai_mod const __stop_ai_mods[]  __asm("section$end$__DATA$ai_mods");
#define AiNif(nm, fn) \
  static struct ai_def const __attribute__((section("__DATA,ai_nifs"), used)) \
    _ainif_##fn = { (nm), (intptr_t) (fn) }
#define AiModNifs(m, tab) \
  static struct ai_mod const __attribute__((section("__DATA,ai_mods"), used)) \
    _aimod_##tab = { (m), (tab), sizeof(tab)/sizeof*(tab) }
#else
extern struct ai_def const __start_ai_nifs[], __stop_ai_nifs[];
extern struct ai_mod const __start_ai_mods[], __stop_ai_mods[];
#define AiNif(nm, fn) \
  static struct ai_def const __attribute__((section("ai_nifs"), used)) \
    _ainif_##fn = { (nm), (intptr_t) (fn) }
#define AiModNifs(m, tab) \
  static struct ai_mod const __attribute__((section("ai_mods"), used)) \
    _aimod_##tab = { (m), (tab), sizeof(tab)/sizeof*(tab) }
#endif

// port vtable -- what a device owes, and nothing else. a NULL slot means no method
// (no readn reads END, no writen discards). neither blocks the scheduler; the generic
// layer above owns ungetc_buf.
//   writen: land up to n bytes in one motion: >0 landed, 0 no room now (caller keeps
//     the residue), -1 the device is gone (io_wdrain drops the run). it may allocate,
//     hence the frame by address: land nothing after an allocating step -- grow, answer
//     0, let the caller re-derive src. only a door whose port keeps a write run may
//     refuse; the static ports cannot park, so their door must land what it takes.
//   readn: drink up to n waiting bytes: >0 bytes, 0 nothing yet (the scheduler owns the
//     wait), -1 end of stream. never allocates, hence frame by value. the end is stable:
//     a spent device owes -1 to every ask, not just the first (test/front/io.l law 3).
//   athand: of the next n bytes, how many are here already -- a source whose text is in
//     memory (a C string, a charlist) counts them without a device. NULL is "ask the
//     device", so a run must come out of a buffer instead. `chug` is the one caller.
struct ai_port_vt {
 struct ai*(*flush)(struct ai*);
 intptr_t (*writen)(struct ai**, unsigned char const*, uintptr_t),
          (*readn)(struct ai*, unsigned char*, uintptr_t);
 uintptr_t (*athand)(struct ai*, uintptr_t); };

enum ai_status ai_fin(struct ai*);

static ai_inline size_t b2w(size_t b) {
 size_t q = b / sizeof(ai_word), r = b % sizeof(ai_word);
 return q + (r ? 1 : 0); }

lvm_t lvm_ret0, lvm_cur, lvm_port_io, lvm_help, lvm_cask,
// how a frontend nif parks: set g->next_wake_at (or next_wait_fd), leave Ip
// unadvanced, `return Ap(lvm_yield_sw, g)` -- the op re-runs on reschedule.
      lvm_yield_sw,
// how a frontend nif ends the running task (inle's seated quit, the love-machine
// _exit): stack down to one word (the retval), Ip at a {lvm_task_exit} cell,
// `return Ap(lvm_task_exit, g)` -- catch then reads the retval off the node.
      lvm_task_exit;

// the vtable every port backed by a real OS fd wears; the frontend defines it,
// and its address is what says "there is an fd behind this one" (ai_io_fd).
extern struct ai_port_vt const ai_fd_port_vt;
// what a closed port wears: every door a no-op, and no fd behind it. a frontend
// owning `close` swaps this in -- that swap IS the close, there is no other mark.
extern struct ai_port_vt const ai_closed_vt;

// close an OS fd backing a heap port; weak no-op default, the host overrides
// with close(2). called by ai_io_alloc's finalizer.
void ai_fd_close(int fd);

// the fd port: the head plus the descriptor. the vt is the license to read it --
// nothing casts here without ai_io_fd, which answers -1 for every port whose door
// is not a device (a tap, a jug, a closed port: all real ports, none with an fd).
struct ai_fio { struct ai_io io; ai_word fd; };
intptr_t ai_io_fd(struct ai_io const*);
// the buffered port: the fd port plus both buffer lanes, private to the generic
// dispatch (prel's tap/jug poke the bare shape; static ports stay bare -- nothing
// traces a static). rbuf/wbuf hold an ai_str backing or 0; [rpos,rlen) bounds the
// pending read run, wlen the filled write prefix. GC walks the extension words as
// ordinary thread words.
struct ai_bio { struct ai_fio f; ai_word rbuf, rpos, rlen, wbuf, wlen; };
// the two faces host nifs need (guards inside; both 0/no-op on a bare port):
// pending = bytes waiting in the read buffer; drain pops up to n of them into dst
uintptr_t
 ai_io_pending(struct ai*, struct ai_io*),
 ai_io_read_drain(struct ai*, struct ai_io*, unsigned char*, uintptr_t),
// unread moves the position inside the run and answers how many bytes moved: n > 0 gives
// back, n < 0 takes; the borrowed run counts, so stdin's seek-back sees it. signed because
// relative does not compose -- a caller that gave back must step forward again.
 ai_io_unread(struct ai*, struct ai_io*, intptr_t),
 ai_io_wpending(struct ai*, struct ai_io*);  // ... and what the device would not take.
struct ai
 *ai_io_wflush(struct ai*, struct ai_io*),   // try to push the write run out
// close and seal call the pair: wflush, then park on a nonzero wpending (see
// lvm_yield_sw). neither may shut the fd on a residue -- a truncated stream.
 *ai_io_alloc(struct ai *g, int fd);
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
 *ai_egg_(struct ai*, char const*, char const*, char const*, char const*),  // (egg, p1, corpus, post)
 *ai_defn(struct ai*, struct ai_def const*, uintptr_t, char const*),   // immortal values only; mod (or NULL = the book)
 *ai_defv(struct ai*, char const*),                // its twin for a LIVE heap value (rides sp[0], stays there)
 *ai_layer_(struct ai*),      // push a fresh writable layer (the runtime's enter); every frontend opens its session with it
 *ai_unsplice_(struct ai*);   // drop the link below the head (the runtime's bare leave)

// the heap-image codec (stdio-free): save compacts g and serializes into a fresh
// g->alloc'd buffer; load reconstructs a fresh g, or NULL on any mismatch (the
// caller boots normally). buffer-based so a freestanding frontend needs no filesystem.
// a kept absolute only survives a wake if it aims inside the binary's own load segments
// (one ASLR delta shifts them all); a JIT W^X page, an mmap or a shared library dies with
// the bake process, so the dump refuses it. only the host can answer that, so it answers
// by parameter and the audit owns no state here; a NULL guard is audit off. the guard is
// asked of every candidate absolute and told which object carries it (heap word offset
// plus that object's hot). answer 0 and the dump refuses.
struct ai_image_guard { uintptr_t (*ok)(void *ctx, uintptr_t v, uintptr_t off, uintptr_t ap); void *ctx; };
void *ai_image_save(struct ai*, uintptr_t *outlen, struct ai_image_guard const*),
     *ai_image_save_(struct ai*, uintptr_t *outlen, struct ai_image_guard const*);   // the unguarded worker: a MID-EVAL dump (the bake nif)
struct ai *ai_image_load(void const *buf, uintptr_t len);
// the layered bake (doc/misc/plan/image-chain.md). freeze dumps this layer and pins it, and
// answers an opaque {header, blob} record the caller hands back. save_over answers the
// full image plus each baseline's derived record -- its header and the prefix words that
// changed -- which load_over wakes against the parent's stream. all g->alloc'd; NULL is
// no image, never half of one.
void
 *ai_image_freeze(struct ai**, uintptr_t *outlen, struct ai_image_guard const*, uint8_t *why),
 *ai_image_save_over(struct ai*, uintptr_t *outlen, struct ai_image_guard const*, uint8_t *why,
                     void *const *bases, uintptr_t const *blens, uintptr_t nbase,
                     void **subout, uintptr_t *sublens);
struct ai
 *ai_image_load_over(void const *parent, uintptr_t plen, void const *sub, uintptr_t slen),
 *ai_image_load_m(void const *buf, uintptr_t len, void *(*)(struct ai*, void*, size_t));   // allocator-parameterized (a device heap has no malloc)

// the terminal scare face: prints ";; a b\n" (show forms) to the err port from
// the stashed condition data; the bare oom prints ";; oom@len=N\n".
void ai_scare_face_(struct ai*);

extern struct ai_fio ai_stdin, ai_stdout, ai_stderr;

// the boot driver: ai_egg_(g, egg, p1, corpus) applies love/egg.l to the quoted corpus
// -- compile the compiler with c0, recompile the corpus through itself, install as `ev`.
// the list is stitched (p0 reads egg + p1; p1, evaluated a step earlier, reads the
// corpus), so p1.l is the only .l held to the pure lisp subset.

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
#define charmp oddp
// the blue floor: extra stack slack on every avail check, a buffer against off-by-one
// overshoots. 0 under LoveBoot so love0 keeps strict discipline; -Dai_avail_floor=N overrides.
#ifndef ai_avail_floor
# ifdef LoveBoot
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
// enum q, the value-kind lattice for generic dispatch: KMint the blue floor, then KNom,
// the arithmetic lane [KCharm..KTrayO] (scalars then their tray mirrors), the sequence
// lane [KString..KChain], tablet, thread last -- each dyadic lane one contiguous range,
// `max` the within-lane promotion join. dispatch order only: the total compare order is
// cmp_rank's separate remap (love.c). KN is the matrix dimension.
// enum d is the other question: what a heap object's hot says it is. ai_typ can answer
// these nine and nothing else, so a switch over them is exhaustive and carries no
// default -- add a data sentinel and -Wswitch names every site. the two share no values;
// mx.h's ai_kind_of_d is the one crossing, and a tray is the one rep that dispatches
// four ways. both rosters are mx.l's -- edit that, not kinds.h.
#include "kinds.h"
typedef ai_word num, word;
// the unique empty string: data-segment, never moved (gcp's out-of-pool
// short-circuit); strings are immutable, so one suffices.
extern const struct ai_str ai_str_empty;
#define EmptyString ((word) &ai_str_empty)
// (): the one serial-0 mint, a data-segment const shared by every core -- immortal,
// never copied, so () is bakeable. serial 0 is never drawn, so it is unique and least;
// .ap = lvm_sym gives mintp/const-1-apply/()-print for free.
extern const struct ai_mint ai_mint_zero;
#define ZeroPoint ((word) &ai_mint_zero)
// one parked fd. the layout is poll(2)'s struct pollfd, so the host polls the block
// directly (host/main.c static-asserts the match). the scheduler fills .fd/.events and
// zeroes .revents, then reads .revents back: nonzero = ready, taken instead of re-asking
// the kernel per fd. filling it is optional -- all-zero is "nothing to say".
struct ai_wait_fd { int fd; short events, revents; };

// the two park directions, in poll(2)'s own bit values (host/main.c static-asserts
// them). never OR them and ask as one: a socket is almost always writable, so a reader
// polled for both would spin.
#define ai_wait_in  1
#define ai_wait_out 4

void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ticks), // wait for a fd to be ready
    ai_ready_fds(struct ai_wait_fd *fds, int n);                  // non-blocking variant
bool ai_ready(int fd, int events), ai_strp(ai_word);
struct ai
 *ai_please(struct ai*, uintptr_t),
 *ai_push(struct ai*, uintptr_t, ...),
 *ai_strof(struct ai*, const char*),
 *gxl(struct ai*),
 *gxr(struct ai*),
 *intern(struct ai*),
 *str0(struct ai*, uintptr_t),
 *grbufg(struct ai *g, uintptr_t len);
lvm(lvm_gc, uintptr_t);
uintptr_t hash(struct ai*, word), ai_tray_bytes(struct ai_tray*);
// any value -> its enum q: KCharm for a fixnum, KHot for a non-data heap pointer,
// else ai_typ's rep, a tray refined by element tier (KTrayZ..KTrayO).
// both the +/* matrices and the apply sentinels dispatch on this.
enum q ai_kind(word);
extern union u const numap_drive[];          // [ap; swap; ret0] driver that runs (num-ap n x); shared by fixnum + data num apply
lvm_t lvm_ap, lvm_chain, lvm_tray, lvm_sym, lvm_nom, lvm_str, lvm_big, lvm_gembox, lvm_sunbox, lvm_twinbox; // the data-kind sentinels (+ ap); defined in love.c, read by inline predicates and ai_typ
// recover a data value's rep from its ap. the sentinels tile one section at
// ai_data_stride in enum d order, so the slot is the kind: one subtract answers both
// questions, and the compiler shares it between a datp and the typ after it. the base
// is lvm_sym -- slot 0 is the start, so no linker-synthesized bracket is owed anywhere.
#if ai_data_section
static ai_inline bool in_data(void *a) {
 return (uintptr_t) ((char*) a - (char*) lvm_sym) < (uintptr_t) (ai_data_n * ai_data_stride); }
static ai_inline enum d ai_typ(union u *o) {
 return (enum d) ((uintptr_t) ((char*) o->ap - (char*) lvm_sym) / ai_data_stride); }
#else
// the seats with no section to lay ask by name instead. the order is measured frequency,
// not enum d's -- over a corpus run: chain 62%, nom 19%, string 13%, mint 5%, big 1.6%,
// the other three under a tenth of a percent each.
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

// call installed help with an error. _lvm prefix makes vmret skip it, since it returns to C if no help is installed
lvm_t _lvm_ghelp;
// ai_have is the phrase "this call may collect"; under AiGcStress every one does, so a
// raw local held across it goes stale on the first run rather than years later.
// AiGcCheck is the other half, checking the collector where this checks the mutator.
static ai_inline struct ai *ai_have(struct ai *g, uintptr_t n) {
#ifdef AiGcStress
 return !ai_ok(g) ? g : ai_please(g, n);
#else
 return !ai_ok(g) || avail(g) >= n ? g : ai_please(g, n);
#endif
}

#endif
