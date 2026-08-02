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
// Keep a function from being identical-code-folded with another. The data
// self-quote sentinels (flow.c) have byte-identical bodies but must keep
// distinct addresses, since their address *is* their type tag. GCC -Os runs
// -fipa-icf, which noipa disables; clang/lld only fold under an explicit
// --icf=all, which no build passes.
#if defined(__GNUC__) && !defined(__clang__)
#define ai_noicf __attribute__((noipa))
#else
#define ai_noicf
#endif
#define ai_digits "0123456789abcdefghijklmnopqrstuvwxyz"
#define countof(_) (sizeof(_)/sizeof(*_))

#ifndef ai_tco
#define ai_tco 1
#endif

// The generational collector is the ONLY collector: every frontend's g->alloc must supply the major
// pool (host + the inle kernel both malloc), or ai_ini_0 fails. Its instrumentation (the counters
// `gauge` reports) is always present and always cheap -- a handful of adds per (rare) collection.

// port read-buffer size in bytes (the one buffered-io knob; a small device
// shrinks it like ai_major0)
#ifndef ai_iobuf
#define ai_iobuf 4096
#endif

#if ai_tco
#define _lvm(n, ...) struct ai *n(struct ai *restrict g, union u *Ip, ai_word *Hp, ai_word *restrict Sp, ##__VA_ARGS__)
#define Ap(fn, g, ...) fn(g, Ip, Hp, Sp, ##__VA_ARGS__)
#define Continue() Ap(Ip->ap, g)
#define Pack(g) (g->ip = Ip, g->hp = Hp, g->sp = Sp)
#define Unpack(g) (Ip = g->ip, Hp = g->hp, Sp = g->sp)
#else
#define _lvm(n, ...) struct ai *n(struct ai *restrict g, ##__VA_ARGS__)
#define Ap(fn, g, ...) fn(g, ##__VA_ARGS__)
#define Continue() g
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

// Typed N-dim array. Rank 0 = scalar (no shape words); payload at
// (void*)(shape + rank). Immutable.
struct ai_vec {
 lvm_t *ap;
 uintptr_t type, rank, shape[]; };

// Status rides the 2 pointer tag bits, read as two flags: bit 0 is the SCARE
// bit (something is wrong -- the raise site puts the relevant data on struct ai for
// the help function to read; the bare scare is oom, which has no room to say
// more), bit 1 is the MORE bit (read control flow: more input is wanted).
// more alone = incomplete; eof = more|scare -- the end is the scary case of
// wanting more.
enum ai_status { ai_status_ok = 0, ai_status_scare = 1, ai_status_more = 2, ai_status_eof = 3 };

// the atom: ap, the code (an interned symbol's name hash / a mint's serial),
// the nom (a string = interned; 0 = a mint). UNIFORM 3 words -- the l/r
// intern-tree slots died with the tree: interning lives in struct ai's
// `symbols`, a weak map from name string to the canonical mint chain.
struct ai_str {
 lvm_t *ap;
 uintptr_t len;        // byte count
 char bytes[]; };
// a buf (the surface's cask): mutable bytes behind a 2-word wrapper whose ap is
// lvm_buf (recognized by ap, like ports). The PUBLIC FACE, here so a host nif
// file can wrap a C struct's bytes in a cask (host/cb.c does); the buf machinery
// itself stays in love.c.
struct ai_buf { lvm_t *ap; struct ai_str *str; };
// a mint: a bare nameless point -- just the hot and its serial. Named syms are
// the (name . mint) chains now, so a mint carries NO name (the old `nom` field
// is gone; nothing ever set it post-collapse).
struct ai_mint {
 lvm_t *ap;
 uintptr_t code; };
// a NOM: a NAMED point -- its OWN kind (KNom), not the (name . mint) chain the
// KSym->KMint collapse made it. `name` is the spelling string (GC-forwarded);
// `code` is the serial (the order key on a name tie); `dig` caches the SPELLING
// hash, computed once at creation -- content, not the serial, so a tablet's
// bucket order never depends on intern history (the reproducible-build law).
// No inner mint object: a nom is a flat 4-word leaf, distinct from the bare
// KMint above. `nomp` = the union (a bare mint OR a named nom); a chain is now
// ALWAYS a real compound.
struct ai_nom {
 lvm_t *ap;
 uintptr_t name;
 uintptr_t code;
 uintptr_t dig; };

struct ai {
 // (the core no longer masquerades as (): () is the const ZeroPoint (ai_mint_zero),
 // never (word)g, so the core needs no leading ap word -- word0 is just `ip`.)
 union u {
  lvm_t *ap;
  ai_word x;
  union u *m; } *ip;
 ai_word *hp, *sp;
 union u *tasks;       // RUN ring head (running task's node); always non-NULL after ai_ini.
                       // ⚠ NO NODE HERE IS FD-PARKED -- a task that parks on an fd leaves for
                       // `parked` below, which is what makes a switch cost no syscall at all.
 union u *parked;      // the PARKED ring: tasks waiting on an fd, or NULL when none are. Its own
                       // ring, not a segment of the run ring, so the fairness yield never walks
                       // it -- only a task that BLOCKS pays for the tasks that are blocked, and
                       // the readiness of the whole ring comes back from one ai_wait_fds/
                       // ai_ready_fds. Never image-serialized: an fd means nothing across a
                       // bake, and a baker is single-tasked, so a woken runtime starts empty.
 uintptr_t yield_ctr,  // ap-cycles since last cooperative yield; counts up to yield_interval (level-triggered)
           sweep_ctr,  // fairness yields since the last PARKED sweep; counts to sweep_interval.
                       // a second, slower counter on purpose -- handing the cpu to a runnable
                       // peer is a ring walk, asking the kernel about a parked one is a syscall,
                       // and one knob cannot price both (love.c, over sweep_interval).
           next_serial, // THE MINT STREAM: one monotonic counter every fresh identity draws
                        // from -- task pids and nom serials alike (pre-incremented).
                        // a nom's serial lands in its `code` slot: its
                        // hash AND its order tiebreak -- same-name noms order by creation,
                        // GC-stable (code rides the copy), closing trichotomy.
           next_wake_at; // raw deadline for next yield_sw snapshot's wake_at slot; 0 = always runnable
 intptr_t next_wait_fd; // fd the task suspended on, -1 = not waiting on I/O. Installed into next yield_sw snapshot's wait_fd slot.
 int next_wait_events;  // WHAT it suspended for: ai_wait_in (the default, restored on
                        // every consumption) or ai_wait_out, which only connect's
                        // handshake park sets. Rides beside the fd into the same
                        // snapshot slot. ⚠ it is a FIELD and not read off the parked
                        // op the way the port is, because the op that wants OUT lives
                        // in a frontend and love.c may not name it.
 ai_word symbols;        // the WEAK intern map (string -> the canonical atom; see struct ai_mint
                        // above): value-keyed by string content; a collection clones it untraced and
                        // sweeps it after the cheney fixpoint, so a dead atom's entry drops --
                        // dead spellings vanish. 0 only during early init.
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
 ai_word *minor;      // the MINOR-POOL watermark: hp right after the last collection, so [minor, hp) is
                        // the YOUNG set (allocated since) and [end, minor) the OLD/tenured set. A raw pool
                        // pointer (like hp/sp/cp), NOT a traced value -- recomputed every collection, never
                        // forwarded. Stage 1: maintained + observed (gauge `old`); the minor that reaps the
                        // young set lands later. See doc/gengc.md.
 // GENERATIONAL collector state -- ALWAYS present (it is the only collector; ai_ini_0 requires the
 // major pool). The counters are cheap (a handful of adds per collection, which is rare) and `gauge`
 // reports them.
 uintptr_t n_gc, max_len, max_heap, // gc instrumentation (cycles, peak pool len, peak live heap; words)
           n_seen, n_evac;          // Σ over collections: heap occupancy entering each (scanned = live+dead) and survivors copied out (live). mortality = (n_seen-n_evac)/n_seen; copy-amp = n_evac/max_heap -- the generational-GC justifier (does the same live set get recopied every cycle?)
 // the REMEMBERED SET -- the whole write barrier: old objects/cells that took a young pointer
 // (gen_wb at maps/rings; gen_wb_cell/gen_wb_two at compiler stores + poke), each rescanned by
 // the next minor. A malloc'd buffer (via g->alloc, like the major pool). rem_miss counts
 // entries DROPPED on overflow -- any miss forces the next collection to a MAJOR (roots-only
 // trace, needs no rem set), so a minor only ever runs under a complete set. Cleared (with
 // rem_n) on every collection.
 ai_word *rem; uintptr_t rem_cap, rem_n, rem_hi, rem_miss;
 // stage 3: the MINOR + the MAJOR pool. The main pool is now a pure MINOR pool
 // (the whole heap [end,hp) is young -- the `minor` watermark stays == end, never advanced); OLD lives
 // in the `major_pool`, a separate two-space (di) region. A MINOR evacuates the minor pool -> major-pool
 // active half (append) and resets hp=end; a MAJOR drains {minor pool + a full major-pool scan} -> the
 // major-pool active half, then COMPACTS it -> the spare half (or a fresh bigger pair), flips, rebuilds symbols, runs
 // finalizers. gc_to_{lo,hi} bound the to-space (a thread's home, for the terminator scan in
 // evac_thread); gc_fwd is the forwarding floor (word0 in [gc_fwd, gc_to_hi) <=> a copy made THIS
 // collection -- distinguishing it from a pointer to a PRE-EXISTING major object). gc_gen redirects
 // bump() to major_hp during a generational collection.
 ai_word *major_pool, *major_base, *major_hp;   // major: malloc base (2*major_len words), active-half base, active bump
 uintptr_t major_len;                       // major half size (words)
 ai_word *gc_to_lo, *gc_to_hi, *gc_fwd;   // to-space tagp range [to_lo,to_hi) + forwarding floor (set per collection)
 ai_word *gc_f2lo, *gc_f2hi;              // a SECOND from-space range (0 = unused); a major traces {major ∪ minor} in one pass
 uintptr_t gc_gen;                        // !=0 during a generational collection: bump() targets major_hp, not hp
 uintptr_t n_minor;                       // MINOR collections so far (majors = n_gc - n_minor)
 uintptr_t minor_hi, major_hi;            // the PAUSE gauge: peak words ONE minor / ONE major copied
                                          // (a copying collection's pause is its copy volume, so these
                                          // bound the worst pause in words -- deterministic on every
                                          // frontend; gauge[14]/[15]. test/host/gcpause.l puts wall ns
                                          // against them and proves the minor peak flat in the live set).
 uintptr_t since_major, major_live0;      // young words scanned since the last major; major-pool live right after it.
                                          // A major fires once since_major > major_live0 + 4*minor-pool -- majors amortized
                                          // against allocation, so floating dead tenured objects are swept periodically
                                          // (occupancy alone never triggers it: dead old garbage dies in place) and the
                                          // pool can SHRINK. See gen_please.
 uintptr_t win_alloc, win_copied;         // a sliding window (words) for the DETERMINISTIC minor-resize ratio: young
                                          // allocated vs survivors copied since the last resize. overhead = copied/alloc
                                          // (the GC's share of the work, in words not clock ticks); grow the nursery while
                                          // it exceeds 1/ratio, shrink when far below. Accumulating smooths the spikes a
                                          // per-collection ratio would chase. Reset on a resize. See gen_please.
 intptr_t lean;                           // the resize-stickiness streak: consecutive out-of-band window verdicts,
                                          // +grow/-shrink; a resize needs |lean| >= 2 (one window is a hint -- a resize
                                          // is a full copy + a total refault, the runtime's costliest single act).
 uintptr_t n_resize;                      // pool reallocations so far (gen_grow calls: band resizes + forced grows) --
                                          // gauge[13]; a bench that watches it catches pool-cliff contamination cold.
 uintptr_t budget;                        // total memory CAP in words (2*minor + 2*major, both two-space); 0 = unbounded.
                                          // Appel's rule: the nursery gets the free budget after the major pool. Inited from
                                          // the ai_budget tunable (the Teensy knob) but a field, so it can be set at runtime
                                          // like the g->alloc hook. See gen_please.
 union {
  intptr_t v0;
  struct {
   ai_word book;   // global env map (lookup-lambda); GC-forwarded in v0..end. The
                  // macro table is book[zero] -- no separate field.
   ai_word scare_a, scare_b; // the last bare scare's condition data, stashed at
                  // the raise so a terminal exit can speak (ai_scare_face_);
                  // zero zero = the bare oom, which has no data. GC-traced here.
   // THE FIVE HOOKS: lisp the C lanes must reach, handed over by (seal-hook n f) and
   // read by SLOT thereafter -- no per-call sym_probe + book lookup, and no later rebind
   // of a nom can reach them. They are declared and numbered in SEAL ORDER, which is also
   // boot order. All GC-traced (v0..end) and image-serialized, so a woken runtime comes up
   // sealed; unsealed = zero -> hot_hook traps, never a wild read.
   ai_word hot_read;  // 0: the CORPUS READER -- p1.l's whole-text door, sealed by p1's own
                  // last act, which is why the reader needs no name in the book at all.
                  // zero = p1 is not up yet, and readtext falls back to p0's lisp subset.
   ai_word hot_numap; // 1: the church C->lisp num-ap hook, read on the hot apply paths
                  // (lvm_numap/numtap, data_num_apply).
   ai_word hot_stack, hot_compose; // 2, 3: `+` and `*` OF TWO FUNCTIONS -- church add
                  // ((stack f g a x) = (f a (g a x))) and composition ((compose f g x) =
                  // (f (g x))), two lines of prel. lvm_addh/lvm_mulh build the partial from
                  // the slot.
   ai_word hot_opfix; // 4: the operator factor pass, sealed last (it does not exist until
                  // late in the prel) -- ai_eval reads the field, so a book rebind cannot
                  // reach the C compile lane; pre-seal it falls back to the book probe.
   ai_word mods;  // the MODULE REGISTRY book: name -> module-book, filled by `leave`
                  // on a named scope, read by `use`/`from` (love/prel.l). A lazy
                  // singleton (the `mods` nif creates it on first read, so both
                  // prel runs of a bootstrap capture the SAME tablet and pre-egg
                  // registrations survive the egg warm). In v0..end, so GC-traced
                  // and image-serialized with no further wiring.
   ai_word lib;   // the SOURCE LIBRARY book: name -> source text, read by `use`'s
                  // miss lane (the loader, love/prel.l) so a frontend can bake
                  // loadable module sources with no filesystem. Filled from C by
                  // ai_lib_ (each frontend registers what it wants loadable);
                  // a lazy singleton like mods, and traced/serialized the same way.
   union {
    ai_word x;
    struct ai_io {
     lvm_t *ap;
     ai_word fd;
     ai_word ungetc_buf;            // pushed-back byte; putcharm(EOF) = empty
     // ⚠ three words, and prel's tap/jug poke this layout word by word while
     // slurp peeks a jug's backing by index (love/prel.l) -- a word added here is
     // a renumbering there.
    } *io; }; }; };
 intptr_t end[]; };

struct ai_def { char const *n; intptr_t x; };

// Host nif auto-registration. A host/*.c file (or main.c) registers a nif by
// name with AI_NIF("name", nif_body) -- the entry lands in the `ai_nifs` section
// and main()'s boot drains [__start_ai_nifs, __stop_ai_nifs) via ai_defn. So an
// app thread adds nifs in its OWN host/<app>.c (auto-globbed + auto-registered)
// without editing love.c, love.h, or main.c's table. No linker script: the toolchain
// auto-defines the bracket symbols (Linux __start_/__stop_; mach-o section$).
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

// Port vtable -- what a DEVICE owes, and nothing else. One shape covers both
// directions. A NULL slot means NO METHOD, and the dispatcher answers for it:
// no readn reads END, no writen discards. Neither blocks the scheduler, and
// neither touches ungetc_buf (the generic layer above owns it).
//   writen: land up to n bytes from src in one motion WITHOUT waiting for the
//     device. >0 = bytes landed, 0 = no room right now (the caller KEEPS THE
//     RESIDUE and comes back), -1 = the device is GONE. the three answers are
//     readn's three, and the last one earns its place: a run held for a stream
//     that will never move again is a task parked forever, since close and seal
//     wait for an empty run. io_wdrain drops it. ⚠ A DOOR MAY ONLY REFUSE A PORT THAT KEEPS A
//     WRITE RUN. A heap port does (io_wdrain re-offers what was refused); the
//     static ports do NOT -- nothing traces a static, and their per-byte lane
//     prints from inside a structural printer with nowhere to park mid-shape --
//     so a refusal there is a byte on the floor and their door must land what it
//     takes. That is the only wait a frontend is still allowed, and it is
//     bounded by a console that drains.
//     ⚠ IT MAY ALLOCATE, which is why it takes the frame BY ADDRESS: a string
//     sink grows its backing, so a scare has to ride out and src may MOVE. Land
//     nothing after an allocating step -- grow, answer 0, and let the caller
//     re-derive its source; the one-byte lane's src is a C local, so it lands.
//   readn: drink up to n waiting bytes into dst WITHOUT blocking; it fills a
//     caller's buffer and so can never allocate, which is why it takes the frame
//     by value.
//     >0 = bytes, 0 = nothing waiting right now, -1 = end of stream. ⚠ THE END IS
//     STABLE: nothing above the device remembers it, so a spent device owes -1 to
//     every ask, not just the first (test/front/io.l's law 3 is the witness).
// ⚠ READN IS THE WHOLE READ DOOR. There was a per-byte `getc` beside it and its
// nine implementations were nine spellings of one fact -- five of them SPINNING
// on a probe they already had, which is how a quiet fd stopped the entire vm
// instead of the one task reading it. A device now ANSWERS "nothing yet" (0) and
// the scheduler owns the wait; io_refill turns that into IO_WOULDBLOCK and
// lvm_fgetc parks. There is no fourth answer to widen, and no second place to
// state it.
// ⚠ THE PUSHBACK IS NOT A DEVICE QUESTION and never was. `ungetc` used to be a
// slot, and all nine implementations were the same three lines against the port's
// own head words -- while the synthetic rows answered a no-op, so a pushback onto
// a jug was DISCARDED although zgetc reads ungetc_buf before it dispatches. One
// generic zungetc (love.c) replaced the lot and closed that asymmetry.
// ⚠ WRITEN IS THE WHOLE WRITE DOOR, for the reason readn is the whole read one.
// A per-byte `putc` sat beside it and every implementation was its writen at
// n = 1 -- while the slot's real job was elsewhere: it was the ONLY write path
// allowed to allocate, so the bulk lane answered "no room without an alloc" and
// bounced back through it. That cost a slot, a retry protocol, and a discarded
// return value in io_wdrain that dropped the tail of a buffer on a mid-stroke
// write error. writen allocates now, so there is nothing left to bounce to.
struct ai_port_vt {
 struct ai*(*flush)(struct ai*);
 intptr_t (*writen)(struct ai**, unsigned char const*, uintptr_t),
          (*readn)(struct ai*, unsigned char*, uintptr_t); };

// only 2 tag bits on 32 bit so we can only have four of these
enum ai_status ai_fin(struct ai*);

static ai_inline size_t b2w(size_t b) {
 size_t q = b / sizeof(ai_word), r = b % sizeof(ai_word);
 return q + (r ? 1 : 0); }

lvm_t lvm_ret0, lvm_cur, lvm_port_io, lvm_help, lvm_buf,
// how a frontend nif PARKS: set g->next_wake_at (or g->next_wait_fd), leave Ip
// unadvanced, and `return Ap(lvm_yield_sw, g)` -- the task gives up its turn and
// the op re-runs on reschedule, so the op must be re-runnable at that point.
// close/seal use it to finish a write run without stopping the vm.
      lvm_yield_sw;

// Frontend-provided vtable for ports backed by real OS file descriptors.
// Used whenever fd >= 0. Synthetic ports (fd <= -1) route through the
// shared synth table inside g.c instead.
extern struct ai_port_vt const ai_fd_port_vt;

// Frontend hook to close an OS file descriptor backing a heap port. Weak
// default in g.c is a no-op so kernel/pd/lcat link without having to care;
// the host overrides with close(2). Called by the finalizer that ai_io_alloc
// installs, so it runs when a heap port becomes unreachable.
void ai_fd_close(int fd);

// THE BUFFERED PORT: a heap fd port is really an ai_bio -- the plain ai_io
// head plus both buffer lanes, PRIVATE to the generic dispatch (prel's
// tap/jug poke the bare ai_io shape and never learn these words exist; the
// static ports stay bare too -- nothing traces a static, so a heap backing
// would dangle). rbuf/wbuf hold an ai_str backing or 0 (never dressed);
// rpos/rlen charms bound the pending read run [rpos, rlen); wlen charms the
// filled write prefix. Only bio_of-guarded code (heap + fd >= 0) reads past
// the head. GC walks the extension words as ordinary thread words.
struct ai_bio { struct ai_io io; ai_word rbuf, rpos, rlen, wbuf, wlen; };
// the two faces host nifs need (guards inside; both 0/no-op on a bare port):
// pending = bytes waiting in the read buffer; drain pops up to n of them into
// dst (swig's first course -- a buffered see may have gulped ahead of the fd).
uintptr_t ai_io_pending(struct ai*, struct ai_io*);
uintptr_t ai_io_read_drain(struct ai*, struct ai_io*, unsigned char*, uintptr_t);
struct ai *ai_io_wflush(struct ai*, struct ai_io*);   // TRY to push the write run out
uintptr_t ai_io_wpending(struct ai*, struct ai_io*);  // ... and what the device would not take.
// close and seal call the pair: wflush, then park on a nonzero wpending (see
// lvm_yield_sw) and come back. ⚠ NEITHER MAY SHUT THE FD ON A RESIDUE -- that is
// a truncated stream, and it is what a blocking write used to hide.
// frontend hook: raw bytes at an fd with NO g machinery -- the GC-context
// finalizer drains a dying port's write buffer through it. Weak no-op default;
// the host overrides with write(2).
void ai_fd_drain(int fd, void const*, uintptr_t);

// Heap-allocate a port for the given OS fd. Bumps Width(struct ai_bio) +
// ttag, fills the head + zeroed buffer lanes, pushes the port pointer on
// Sp[0], and registers a finalizer that drains + closes the fd when the
// port is collected. fd is stored as a plain integer at the C layer and
// tagged on the way in.
struct ai *ai_io_alloc(struct ai *g, int fd);

uintptr_t ai_clock(void); // used by garbage collector
intptr_t ai_nclock(void); // the fine interval clock (ns); weak ms-degraded default in love.c, hosts override with a real ns source
void ai_sleep(uintptr_t ticks); // per-frontend deep wait for at most `ticks`
// ai_clock() units (ticks=0 means infinite). No input wakeup; the scheduler
// dispatches to ai_wait_fds when tasks are parked on streams. Default = no-op.

struct ai
 *ai_ini(void),
 *ai_ini_m(void*(*)(struct ai*, void*, size_t)),
 *ai_evals_(struct ai*, const char*),
 *ai_egg_(struct ai*, char const*, char const*, char const*, char const*),  // (egg, p1, prel, ev)
 *ai_defn(struct ai*, struct ai_def const*, uintptr_t),   // ⚠ IMMORTAL values only
 *ai_defv(struct ai*, char const*),                // its twin for a LIVE heap value:
                                                   // the value rides sp[0] and stays
                                                   // there, so two names bind one thing
 *ai_lib_(struct ai*, char const*, char const*),   // register name -> source text in the
                                                   // source library (g->lib); `use` loads it
                                                   // on a miss. A frontend's boot calls this
                                                   // for each module it bakes.
 *ai_layer_(struct ai*),      // push a fresh writable layer (the runtime's enter):
                              // a frontend opens its session with this post-boot
 *ai_unsplice_(struct ai*);   // drop the link below the head (the runtime's bare
                              // leave): brackets a boot-time (use 'x) that should
                              // register without staying ambient

// the heap-image snapshot CODEC (love.c, stdio-free): save compacts g and serializes
// {header, blob} into a fresh g->alloc'd buffer (free it with g->alloc(g, buf, 0)); load
// reconstructs a fresh g from such a buffer, or NULL on any mismatch (the caller boots
// normally). The host wraps these with file I/O (host/image.c). Buffer-based so a
// freestanding frontend can load a baked image with no filesystem.
void ai_image_note(uintptr_t stage);   // wake-progress hook, weak no-op; a port bringing the wake up on new metal overrides it
void *ai_image_save(struct ai*, uintptr_t *outlen);
void *ai_image_save_(struct ai*, uintptr_t *outlen);   // the unguarded worker: a MID-EVAL dump (the bake nif)
struct ai *ai_image_load(void const *buf, uintptr_t len);
struct ai *ai_image_load_m(void const *buf, uintptr_t len, void *(*)(struct ai*, void*, size_t));   // allocator-parameterized (a device heap has no malloc)

// the terminal scare face: print ";; a b\n" (show forms) to the err port from
// the stashed condition data and answer 1; the bare scare (zero zero -- oom,
// which has no data) answers 0 so the frontend can report it raw.
int ai_scare_face_(struct ai*);

extern struct ai_io ai_stdin, ai_stdout, ai_stderr;

// Bootstrap driver (was tools/mkboot.l, now a compile-time C constructor). The
// lib/ headers are bare C string literals (lcat output), so a frontend hands the
// four texts straight over -- one per argument, in the order the boot needs them:
//
//   ai_egg_(g,
//   #include "egg.h"           // (\ egg (: ...)) -- the boot driver, one form
//     ,
//   #include "p1.h"            // the reader in love (doc/io.md rung 6)
//     ,
//   #include "prel.h"
//     ,
//   #include "ev.h"
//   );
//
// This applies the egg driver (love/egg.l) to the quoted corpus, i.e.
// ((\ egg (: ...)) '(<p1 forms> <prel forms> <ev forms>)): it compiles the love
// compiler with c0, recompiles the whole corpus through itself (exercising
// feel), and installs that as `ev`.
//
// The corpus list is STITCHED rather than read whole (love.c's ai_egg_): p0
// reads p1.l / prel.l / egg.l, which the pure lisp subset covers, and p1 -- the
// reader in love, evaluated a step earlier off p0 -- reads ev.l. So the C
// reader's sigil half is no longer on the boot path.

// === internal API shared with data.c / host / free (merged from former i.h) ===
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
// The BLUE FLOOR -- extra stack slack reserved on every avail check, a buffer
// against off-by-one stack overshoots (blue is green's floor in the color law; the
// floor is a green charm, a nonnegative count). It is 0 under GL_BOOTSTRAP so love0
// keeps STRICT discipline -- no buffer, a real overshoot surfaces -- and a small
// default elsewhere. Override at compile time with -Dai_avail_floor=N. Folded into
// both checks so they reserve floor extra words (Have1 is just Have(1)); at floor 0
// the checks reduce to the historical tight reservation.
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
#define op(nom, n, x) lvm(nom) { intptr_t _ = (x); *(Sp += n-1) = _; Ip++; return Continue(); }
#define zero ai_zero
struct ai_chain { lvm_t *ap; intptr_t a, b; };
// The fundamental value kind for generic-op dispatch (enum q). KMint is the bare point
// (the blue floor: () and the nameless mints; named syms are (name . mint) CHAINS now);
// KCharm is the odd fixnum tag; KHot is any non-data heap pointer (thread/function/map). The
// six DATA kinds (KVec, KBig, KString, KMint, KNom, KChain) are the ones ai_typ recovers from an
// ap's sentinel address (a plain compare); every vec reads as KVec there
// (coarse -- one sentinel for scalar boxes and arrays alike). The SEVEN non-sentinel
// numeric kinds are minted by ai_kind alone, refining a KVec by rank: a rank-0 box to
// a scalar GEM (KWide + ai_vec_type -> KWide/KFlo/KCplx) and a rank>=1 vec to an array
// (KArrZ + ai_vec_type). So the scalar GEM tower (charm/wide/float/complex/big -- the
// self-netting fixed-width numbers) and the array tower it mirrors both dispatch inline
// (KArrZ~int, KArrR~float, KArrC~complex, KArrO~object). The diagonal is the DISPATCH
// lattice by semantics then representation: KMint the blue floor (() and the nameless points,
// least of all) then KNom (the named points), then arithmetic lane [KCharm..KArrO] (scalars then their
// array counterparts), sequence/concat lane [KString..KChain], then map, thread last --
// so each dyadic lane is one contiguous range, `max` is the within-lane promotion join,
// and the lone undefined seam (arith <-> seq) is the KArrO|KString boundary. two (chain)
// caps the sequence lane; KMap is the map's own rung just under KHot (a map is still a
// lookup lambda for +/*/apply -- the rung exists for the dispatch matrix's honest cells).
// This enum is the DISPATCH order ONLY. The total COMPARE order is a SEPARATE remap --
// cmp_rank in love.c -- which reseats string just below the number band and the KArrO tray
// just BELOW chain (point < string < number < tray < chain < map < hot). The two coincide
// only on the chain < map < hot tail; do NOT read this enum as the compare order.
// KN is the matrix dimension -- the roster's own length, not a kind.
// The ROSTER itself is tools/mx.l's, the same list the +/* dispatch matrices are
// indexed by, so this enum and mx.h's [KN][KN] grid are one datum and cannot drift.
// EDIT tools/mx.l, NOT kinds.h; `make test_clay` regenerates and fails on drift.
#include "kinds.h"
typedef ai_word num, word;
// The unique empty string -- a data-segment global the GC never moves (gcp's
// out-of-pool short-circuit). Strings are immutable, so one empty string
// suffices and zero-length ones are never heap-allocated. (the empty SYMBOL
// died in the one-nothing round: () reads as 0.)
extern const struct ai_str ai_str_empty;
#define EmptyString ((word) &ai_str_empty)
// () -- the one serial-0 mint, a data-segment const shared by EVERY core (like
// ai_str_empty): immortal, never copied (gcp's out-of-pool short-circuit), the
// SAME value across cores and GCs (so () is bakeable, unlike the old per-core
// core-as-(): it relocated every collection). serial 0 is the one never drawn, so
// it is unique and seated least in the order; .ap = lvm_sym gives mintp/KMint/
// const-1-apply/()-print for free.
extern const struct ai_mint ai_mint_zero;
#define ZeroPoint ((word) &ai_mint_zero)
// One parked fd, as the scheduler hands it over. ⚠ THE LAYOUT IS POSIX poll(2)'s
// struct pollfd, on purpose: the host polls the block DIRECTLY, so it needs no
// vector of its own -- host/main.c static-asserts the match rather than trusting
// it. The scheduler fills .fd and .events and zeroes .revents.
// ⚠ .revents IS READ BACK, and is the whole reason a wait is cheap: for an fd the
// wait reported on, the scheduler takes that answer instead of asking the kernel
// again one fd at a time. Filling it is OPTIONAL -- a frontend that answers off a
// device flag can leave the block alone, and every entry then reads as "nothing to
// say" and is asked the old way -- but a frontend that CAN fill it should, because
// the alternative costs one syscall per parked task per scheduling decision.
// Nonzero means ready: only one direction is ever asked for, so nothing but poll's
// own error/hangup bits can appear on top of it, and those want waking too.
struct ai_wait_fd { int fd; short events, revents; };

// THE TWO DIRECTIONS A PARK CAN WANT, in poll(2)'s own bit values -- host/main.c
// static-asserts them against POLLIN/POLLOUT the way it does the struct above. A
// read park is every one there was until `connect` learned to park on its
// handshake; a write park is that one. ⚠ THEY CANNOT BE OR'd TOGETHER AND ASKED
// AS ONE: a socket is almost always writable, so a reader polled for both would
// wake on every pass and spin.
#define ai_wait_in  1
#define ai_wait_out 4

// Wait until one of `n` parked fds is ready for what its `events` asks, or
// `ticks` elapse (0 = no deadline). ⚠ n IS THE NUMBER OF PARKED TASKS AND HAS NO
// CEILING. The block rides the runtime's own uncommitted heap gap, sized to the
// count -- the door hark marshals argv through, and the reason neither the
// scheduler nor a frontend needs a fixed array, an allocator or a global.
// ⚠ THE SCHEDULER FILLS `events`, not the frontend: each parked task asks for its
// own direction, and a blanket mask over the block would wake readers on writable.
void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ticks);
// The same question WITHOUT THE WAIT: fill `revents` for all `n` fds as they stand
// right now. The scheduler asks it on the fairness path, where the running task still
// has work and only wants to know whether a parked peer has become runnable -- so it
// must not block, and one ask must cover the whole ring.
// ⚠ UNLIKE ai_wait_fds's block, THIS ONE IS AUTHORITATIVE: the weak default fills every
// slot by asking ai_ready one at a time, so an all-zero answer means "none ready", never
// "nobody told me". The host replaces the whole loop with a single poll(2), which is the
// only reason the fairness path can afford to ask at all.
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
// ai_kind maps any value to its enum q: KCharm for a fixnum, KHot for a non-data heap
// pointer (thread/function/map), else ai_typ's data kind -- refined for a rank>=1 vec,
// which expands by element tier to KArrZ..KArrO (a rank-0 box stays KVec). Lives in
// love.c (it needs ai_typ from the generated data.h) and is shared by data.c's apply
// sentinels. Both the `+`/`*` matrices and the apply matrix dispatch on this.
enum q ai_kind(word);
// (Apply is no longer a table: each data sentinel tail-jumps straight to its apply
// handler in love.c -- the sentinel already encodes the kind, and the apply was uniform
// in the argument kind, so the old ai_apply_mx[ai_typ][ai_kind] was pure indirection.)
extern union u const numap_drive[];          // [ap; swap; ret0] driver that runs (num-ap n x); shared by fixnum + data num apply
lvm_t lvm_ap, lvm_chain, lvm_vec, lvm_sym, lvm_nom, lvm_str, lvm_big, lvm_flo, lvm_wide, lvm_cbox; // the data-kind sentinels (+ ap); defined in love.c, read by inline predicates and ai_typ
// ai_typ recovers a data value's kind by comparing its first word (ap) against
// the sentinel addresses. A tiny compare on the COLD apply path (only reached
// when a data value is applied). This replaces the old ai_data ELF-section
// slot-index trick (data.c + tools/gen_data + data.ld + a reflected, generated
// data.h + data_boot.o), which existed solely to turn this compare into one
// shift -- all that build machinery for a couple of cold divisions. The
// sentinels are now ordinary functions in love.c; there is no section, no stride,
// no reflection, no platform split (this is what __APPLE__/wasm already did).
static ai_inline bool in_data(void *a) {
 lvm_t *p = (lvm_t*) a;
 return p == lvm_vec || p == lvm_big || p == lvm_str || p == lvm_sym || p == lvm_nom
     || p == lvm_chain || p == lvm_flo || p == lvm_wide || p == lvm_cbox; }
static ai_inline enum q ai_typ(union u *o) {
 lvm_t *p = o->ap;
 return p == lvm_vec   ? KVec
      : p == lvm_big    ? KBig
      : p == lvm_str    ? KString
      : p == lvm_sym    ? KMint
      : p == lvm_nom    ? KNom
      : p == lvm_chain  ? KChain
      : p == lvm_flo    ? KFlo
      : p == lvm_wide   ? KWide
      :                   KCplx; }   // the 8th and last: lvm_cbox
uintptr_t hash(struct ai*, word), ai_vec_bytes(struct ai_vec*);
#define str(_) ((struct ai_str*)(_))
#define lamp evenp
#define two(_) ((struct ai_chain*)(_))
static ai_inline bool chainp(word _) { return lamp(_) && cell(_)->ap == lvm_chain; }
static ai_inline void *bump(struct ai *g, uintptr_t n) {
 if (g->gc_gen) { void *x = g->major_hp; g->major_hp += n; return x; }   // a generational collection is promoting into the major pool (gc_gen==0 during normal mutation: one predictable branch)
 if (avail(g) < n) __builtin_trap();
 void *x = g->hp; g->hp += n; return x; }
static ai_inline struct ai_chain *ini_chain(struct ai_chain *w, intptr_t a, intptr_t b) {
 return w->ap = lvm_chain, w->a = a, w->b = b, w; }
static ai_inline struct ai *encode(struct ai *g, enum ai_status s) { return
  (struct ai*) ((uintptr_t) g | s); }
// Raise: to the global `help` function when installed, else the status-encoded
// core straight back to C (love.c).
// ghelp re-raises an already-tagged g's own status.
struct ai *ghelp2(struct ai*, enum ai_status), *ghelp(struct ai*);
// ⚠ ai_have IS THE PHRASE "this call may collect", and under AI_GC_STRESS it stops
// being a maybe: every one of them collects, so a raw local held across an
// allocating call is stale on the FIRST run instead of on the day an unrelated
// struct changes size. That is the whole bug class -- love.c's own obin_elem held
// g->ip across ai_big_binop for years and surfaced only when a 24-byte shrink moved
// allocation. Debug builds only, off the GCDBG knob -- doc/verify.md carries what
// it has caught and what it cannot run yet. AI_GC_CHECK is the other half: it checks
// the COLLECTOR where this checks the MUTATOR.
static ai_inline struct ai *ai_have(struct ai *g, uintptr_t n) {
#ifdef AI_GC_STRESS
 return !ai_ok(g) ? g : ai_please(g, n);
#else
 return !ai_ok(g) || avail(g) >= n ? g : ai_please(g, n);
#endif
}

#endif
