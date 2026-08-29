// io.c -- io. one translation unit of the runtime;
// the shared layouts and the cross-TU seam are src/love_int.h.
#include "love_int.h"
// this file's own, forward-declared so order within it does not matter.
static ai_noinline double strtod_wrap(struct ai*g, word x);
static ai_noinline struct ai *chug_str(struct ai *g, struct ai_io *i);
static ai_noinline struct ai *p0text(struct ai *g);
static bool bio_wpending(struct ai_bio *b);
static bool is_dec_int(char const *s, uintptr_t n);
static bool is_hex_int(char const *s, uintptr_t n);
static bool is_oct_int(char const *s, uintptr_t n);
static bool lam_head(struct ai *g, word a);
static int p0getc(struct ai *g, uintptr_t d);
static int p0peek(struct ai *g, uintptr_t d);
static int p0peek2(struct ai *g, uintptr_t d);
static int p0skip(struct ai *g, uintptr_t d);
static intptr_t ci_readn(struct ai *g, unsigned char *dst, uintptr_t n);
static intptr_t to_writen(struct ai **fp, unsigned char const *src, uintptr_t n);
static struct ai *applyq(struct ai *g, char const *driver);
static struct ai *bio_wgrow(struct ai *g);
static struct ai *facex(struct ai *g, word x, int d);
static struct ai *io_refill(struct ai *g);
static struct ai *io_wdrain(struct ai *g, struct ai_io *i);
static struct ai *ioread1str(struct ai*g, uintptr_t d);
static struct ai *ioread1sym(struct ai*g, uintptr_t d, int c);
static struct ai *noop_flush(struct ai *g);
static struct ai *p0chars(struct ai *g, char const *s);
static struct ai *p0onto(struct ai *g, char const *s);
static struct ai *p0read1(struct ai *g, uintptr_t d);
static struct ai *p0reads(struct ai *g, uintptr_t d);
static struct ai *p1text(struct ai *g, char const *s);
static struct ai *qtop(struct ai *g);
static struct ai *readtext(struct ai *g, char const *s);
static struct ai *zgetc(struct ai*g);
static struct ai *zungetc(struct ai*g, int c);
static struct ai*gfputbn(struct ai *g, intptr_t n, uint8_t b, struct ai_io *o);
static struct ai*ioputn(struct ai *g, intptr_t n, uint8_t b);
static struct ai_bio *rbio_of(struct ai *g, struct ai_io *i);
static uintptr_t ci_athand(struct ai *g, uintptr_t n);
static union u *fn_unc0(union u *k);
static void io_close(struct ai *g, void *p);
static void p0pop(struct ai *g, uintptr_t d);
static word *p0cur(struct ai *g, uintptr_t d);
// ============================================================================
// io
// ============================================================================
// the atomic-edge contract: every write can grow a backing (a GC), so an io op
// spanning more than one write must park its heap operand (ai_push -> g->sp) and
// re-read it across each one -- never hold a raw pointer over an edge. the whole
// io_* family holds it without exception; the lam_* helpers are pure (no io, no
// alloc) and cannot open an edge.
bool iop(word x) { return lamp(x) && cell(x)->ap == lvm_port_io; }
// the port an op acts on. in/out/err are three names, not three devices: a task wearing
// its own stdio (hook 6, the chain (i o e); a non-port element keeps the console) reaches
// them through here, and every op routes its operand in place so the re-read across a GC
// edge finds the same port it started on.
// op-level only -- id?, peek, hot? and the image still answer the static, since prel's
// tap/jug read the port head by index and a routed peek would lie to them.
word io_route(struct ai *g, word x) {
 word l = *task_io(g), s;
 if (l == zero) return x;
 if (x == (word) &ai_stdin)       s = A(l);
 else if (x == (word) &ai_stdout) s = chainp(B(l)) ? A(B(l)) : zero;
 else if (x == (word) &ai_stderr) s = chainp(B(l)) && chainp(BB(l)) ? A(BB(l)) : zero;
 else return x;
 return iop(s) ? s : x; }
// the descriptor, and the only way to it: the vt says whether there is one, so a
// port whose door is not a device answers -1 and no cast is ever taken on faith.
intptr_t ai_io_fd(struct ai_io const *i) {
 return i->vt == &ai_fd_port_vt ? getcharm(((struct ai_fio const*) i)->fd) : -1; }

// --- the buffered lanes (generic, above the vt) ---
// a heap fd port is an ai_bio (love.h), dressed lazily; bio_of is the one guard
// (heap and fd-backed), and nothing reads past the head without it. zgetc serves
// ungetc -> the pending run -> one readn gulp, and that order is the park law: a
// port holding bytes is readable however quiet its fd is; a dry gulp answers
// IoWouldBlock and the caller parks. a read drains pending writes first (the
// request/response crossover).
struct ai_bio *bio_of(struct ai *g, struct ai_io *i) {
 return i->vt == &ai_fd_port_vt && in_live_pool(ai_core_of(g), (word const*) i)
      ? (struct ai_bio*) i : NULL; }

bool bio_rpending(struct ai_bio *b) {
 return b && b->rbuf && !charmp(b->rbuf) && getcharm(b->rpos) < getcharm(b->rlen); }

static ai_inline bool bio_wpending(struct ai_bio *b) {
 return b && b->wbuf && !charmp(b->wbuf) && getcharm(b->wlen) > 0; }

// the scheduler's half of the park law above, declared up by find_runnable.
bool wait_buffered(struct ai *g, lvm_t *ap, word x, int fd) {
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

// what did not land stays pending: the run slides down to the front and the next
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
  // the device is gone: drop the run -- keeping it parks a task forever
  // (close and seal wait for an empty run)
  if (k < 0) return b->wlen = putcharm(0), g;
  if (!k) return g;
  char *w = txt(str(b->wbuf));
  if ((uintptr_t) k < n) memmove(w, w + k, n - (uintptr_t) k);
  b->wlen = putcharm(n - (uintptr_t) k); } }
// io_refill's third answer, beside a byte and EOF: the device has nothing right
// now. distinct on purpose; never escapes lvm_fgetc.
#define IoWouldBlock (-2)
// the three answers for every port. no read method = end; no buffer = ask for one byte.
// which bio owns this port's read run: its own, or -- for the static input port on a seat
// that lent it one -- the borrowed one in `inport`. a static cannot own a heap buffer, so
// a frontend that can put fd 0 back where its reader stopped parks a heap bio there; same
// fd and same vt, so every lane below reads it verbatim, and `in` keeps its identity.
// rebinding `in` to the bio would break (id? p in), which bao's `reads` folded at
// egg-compile time. only the device reads in gulps, so the fd offset it runs ahead of is
// the frontend's to rewind before anyone inherits it.
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
 if (!b->rbuf || charmp(b->rbuf)) {              // first buffered read: dress the backing
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
 // k == 0 is "would block", the ordinary answer. never wait here: this runs
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
// the pushback is the port's, not the device's: one head word for every kind of port
static ai_inline struct ai *zungetc(struct ai*g, int c) {
 if (!ai_ok(g)) return g;
 struct ai *fc = ai_core_of(g);
 struct ai_io *i = fc->io;
 i->ungetc_buf = putcharm(c);
 return fc->b = c, g; }
struct ai *ioputc(struct ai*g, int c) {
 if (!ai_ok(g)) return g;
 struct ai *fc = ai_core_of(g);
 struct ai_bio *b = bio_of(g, fc->io);
 struct ai_port_vt const *vt = fc->io->vt;
 if (!vt->writen) return g;                      // no write door: the byte goes nowhere
 if (!b) {                                       // no buffer: the same lane at n = 1.
  unsigned char x = (unsigned char) c;           // src is a C local, so a sink that
  if (!vt->writen(&g, &x, 1) && ai_ok(g))        // grows on the first ask lands it on
   vt->writen(&g, &x, 1);                        // the second -- the growth made room.
  return g; }
 if (!b->wbuf || charmp(b->wbuf)) {              // dress the write backing
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
// flush means try, never wait: what the device would not take stays in the write
// run and lands at the next write, at close, or through the finalizer's drain
struct ai *zflush(struct ai*g) {
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
// `unsee` over a count: move this port's position inside the run it holds and answer how
// many bytes moved -- a short answer is the refusal and the caller's only check. n > 0
// un-reads (gives back), n < 0 re-reads (takes); signed because relative does not compose,
// so a caller that gave back and walked on is behind its own position and must step
// forward again. rbio_of, not bio_of: the run borrowed under a static counts, which is
// what puts bytes back inside stdin's seek-back. it reaches only the current run -- a
// refill replaces rbuf and resets rpos, so the clamp to [0, rlen] answers what is really
// there rather than trusting n. the pushback byte chug lays in front is `unsee`'s to restore.
uintptr_t ai_io_unread(struct ai *g, struct ai_io *i, intptr_t n) {
 struct ai_bio *b = rbio_of(g, i);
 if (!b || !b->rbuf || charmp(b->rbuf)) return 0;
 uintptr_t p = getcharm(b->rpos), l = getcharm(b->rlen);
 if (n >= 0) { uintptr_t k = p < (uintptr_t) n ? p : (uintptr_t) n;
               b->rpos = putcharm(p - k); return k; }
 uintptr_t want = (uintptr_t) -n, room = l > p ? l - p : 0, k = room < want ? room : want;
 b->rpos = putcharm(p + k);
 return k; }
// (chug port): everything already readable, as one exact-length text -- the pushback
// byte if there is one, then the run. it never touches the device and never parks, so
// the gulp is: draw the first byte with `see` (which refills, and parks if it must),
// unsee it, then chug the run whole. "" is the ordinary answer, not a failure -- a port
// with no run answers it, so a caller must draw with `see` rather than spin here. the
// length is known before the string is minted: no over-allocate, no trim.
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
 if (*task_io(g) != zero) Sp[0] = io_route(g, Sp[0]);
 if (!iop(Sp[0])) { Sp[0] = EmptyString; ai_musttail return Next(1); }
 Pack(g); g = chug_str(g, (struct ai_io*) Sp[0]);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

// (inhand port): how many bytes this port holds ready -- the count `chug` would hand over.
// the borrowed run counts, so a reader can ask whether anyone else has drawn on the port
// since it last looked, which is the only way to know its own charlist is still the port's.
lvm(lvm_inhand) {
 if (*task_io(g) != zero) Sp[0] = io_route(g, Sp[0]);
 Sp[0] = putcharm(iop(Sp[0]) ? (ai_word) ai_io_pending(g, (struct ai_io*) Sp[0]) : 0);
 ai_musttail return Next(1); }

// (unchug port n): hand back up to n bytes of the run this port already gave out, so a
// caller that chugged more than it used leaves the rest where the port's position sees it.
// answers how many went back -- a short answer is the refusal (ai_io_unread's notes).
lvm(lvm_unchug) {
 if (*task_io(g) != zero) Sp[0] = io_route(g, Sp[0]);
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
struct to { struct ai_io io; struct ai_str *buf; ai_word i; }; // lisp string output
static struct ai *noop_flush(struct ai *g) { return g; }

// the charlist source's read door: walks the spine, never blocks, so a spent list
// is the end. a buffer is not the answer here -- no syscall on this row to
// amortize, and the spine is the run, which is what athand counts.
// a charm outside 0..255 lands as its low byte: the raw charm once forged the
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

// the string sink's write door: land what fits, else double and answer 0 having
// landed nothing. the grow and the copy cannot share a call: str0 collects, and
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
 ai_to_vt     = { noop_flush, to_writen, NULL,     NULL },       // a string sink: prel's `jug`
 ai_closed_vt = { noop_flush, NULL,      NULL,     NULL },       // what `close` leaves behind
 ai_ci_vt     = { noop_flush, NULL,      ci_readn, ci_athand };  // a charlist: prel's `tap`

// (fputc port byte) — write byte to port; return byte.
lvm(lvm_fputc) {
 if (*task_io(g) != zero) Sp[0] = io_route(g, Sp[0]);
 if (iop(Sp[0])) {
  g->io = (struct ai_io*) Sp[0];
  Pack(g);
  // backpressure, as in lvm_fputs -- but the drain is behind the test: draining
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

// (fflush port): flush means deliver -- a short-answering device parks the task
// and the op re-runs (safe: a flush consumes nothing)
lvm(lvm_fflush) {
 if (*task_io(g) != zero) Sp[0] = io_route(g, Sp[0]);
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
 if (*task_io(g) != zero) Sp[0] = io_route(g, Sp[0]);
 if (iop(Sp[0]) && (strp(Sp[1]) || caskp(Sp[1]))) {
  g->io = (struct ai_io*) Sp[0];
  uintptr_t i = 0, l = len(bytes_of(Sp[1]));
  // the bulk lane when the port has one; a 0 makes one byte of progress through
  // ioputc (its C-local src is the one shape that can grow and land in one breath).
  // the direct stroke is only for an empty buffer: going direct past a pending
  // run would overtake it and the stream comes out shuffled.
  intptr_t (*wn)(struct ai**, unsigned char const*, uintptr_t) = g->io->vt->writen;
  Pack(g);
  g = io_wdrain(g, (struct ai_io*) g->sp[0]);   // buffered puts land before the bulk stroke
  // backpressure: the write run is a buffer, not a queue -- an op that would push
  // it past its own size waits for the device (nothing consumed, the re-run free).
  // the bound is one buffer plus one say, never an accumulation across ops.
  if (ai_ok(g) && ai_io_wpending(g, (struct ai_io*) g->sp[0]) >= ai_iobuf) {
   Unpack(g);
   g->next_wake_at = ai_clock() + 1;            // the write residue's poll -- see io_wdrain
   ai_musttail return Ap(lvm_yield_sw, g); }
  while (ai_ok(g) && i < l) {
   struct ai *w = g;       // the frame by address, off the restrict-qualified param
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
 if (*task_io(g) != zero) Sp[0] = io_route(g, Sp[0]);
 if (iop(Sp[0])) {
   Pack(g);
   g = gfputbn(g, getcharm(Sp[1]), getcharm(Sp[2]), (struct ai_io*) Sp[0]);
   if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
   Unpack(g);
   Sp[2] = Sp[1]; }
 ai_musttail return Nextp(1, 2); }

struct ai*ioputs(struct ai*g, char const *s) {
 while (*s) g = ioputc(g, *s++);
 return g; }

static struct ai*ioputn(struct ai *g, intptr_t n, uint8_t b) {
 uintptr_t
  m = n >= 0 || b != 10 ? (uintptr_t) n : (g = ioputc(g, '-'), -(uintptr_t) n),
  q = m / b,
  r = m % b;
 if (q) g = ioputn(g, q, b);
 return ioputc(g, ai_digits[r]); }

// the terminal scare face's floor: the printer proper lives in love (post.l), and
// by here the VM has stopped, so there is nobody left to run it. this spells the
// shapes a condition actually wears -- name, text, number, list -- and hands every
// other kind its address. the full face is bao's shell-help, which is love and runs
// while the machine still does. no allocation: safe on an exhausted heap.
struct ai *facex(struct ai *g, word x, int d) {
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
bool fn_partialp(union u *k) {
 return k[0].ap == lvm_unc || (k[0].ap == lvm_cur && k[2].ap == lvm_unc); }
static ai_inline union u *fn_unc0(union u *k) {
 return k[0].ap == lvm_cur ? k + 2 : k; }       // first unc cell
union u *fn_base(union u *k, int *nargs) { // base value + captured-arg count
 union u *u = fn_unc0(k), *link;
 int n = 0;
 for (;;) { link = u[2].m; n++; if (link[0].ap != lvm_unc) break; u = link; }
 return *nargs = n, link - 2; }
word fn_arg(union u *k, int i, int nargs) { // i-th arg in application order
 union u *u = fn_unc0(k);
 for (int w = nargs - 1 - i; w > 0; w--) u = u[2].m;
 return u[1].x; }

// the source \-expr stashed at value[-1] by a compiled lambda, or 0. only an
// ala/k0s lambda reserves that leading cell, so probe the tag (which records the
// true start) instead of reading value[-1] -- a wrap/partial/continuation puts its
// value at the start, and value[-1] there reads the neighbouring object.
// in_heap: the main pool or the major pool (tenured objects live there).
bool in_heap(struct ai *c, word x) {
 return (ptr(x) >= ptr(c) && ptr(x) < ptr(c) + c->len) || (ptr(x) >= c->major_base && ptr(x) < c->major_hp); }
word fn_src(struct ai *c, union u *k, word x) {
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
// them is the name. the printer's other C-only question.
lvm(lvm_nifnom) {
 char const *nm = ai_nif_name(Sp[0]);
 if (!nm) ai_musttail return Answer(ZeroPoint);
 uintptr_t n = strlen(nm);
 Have(str_width(n));
 struct ai_str *s = ini_str(str(Hp), n); Hp += str_width(n);
 memcpy(txt(s), nm, n);
 ai_musttail return Answer(word(s)); }

static ai_inline bool lam_head(struct ai *g, word a) {        // is a the symbol \ ?
 struct ai_str *nm;                                          // a named sym (name . mint); nom_str is 0 for a bare mint / the core
 return (nm = nom_str(g, a)) && len(nm) == 1 && txt(nm)[0] == '\\'; }
bool lam_isp(struct ai *g, word x) {         // (\ b.. body): >=2 operands
 return chainp(x) && lam_head(g, A(x)) && chainp(B(x)) && chainp(BB(x)); }
// (fgetc port): a non-port reads as an already-empty stream (EOF), so a
// read-until-(-1) loop over a misused port is bounded
lvm(lvm_fgetc) {
 if (*task_io(g) != zero) Sp[0] = io_route(g, Sp[0]);
 if (iop(Sp[0])) {
  struct ai_io *i = (struct ai_io*) Sp[0];
  struct ai_bio *bb = bio_of(g, i);
  if (bio_wpending(bb)) {                 // our unsent ask goes out before we wait for the answer
   g->io = i;
   Pack(g);
   if (!ai_ok(g = io_wdrain(g, i))) ai_musttail return Ap(_lvm_ghelp, g);
   Unpack(g); }
  // no readiness pre-guard: zgetc already makes that test, and asking first
  // lied on the kernel (reading an output fd parked forever where it now reads
  // the end). cue?/await still ask -- they have no read to answer them.
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

// (await port): cooperatively park until the port's fd is readable, then return
// the port (so it chains into a read) -- for fds you can't drain a byte at a time
// (signalfd, timerfd). Ip is unadvanced, so the task re-checks on reschedule.
lvm(lvm_await) {
 if (*task_io(g) != zero) Sp[0] = io_route(g, Sp[0]);   // and the routed port is what it answers -- the read that chains off it lands there too
 if (iop(Sp[0])) {
  intptr_t fd = ai_io_fd((struct ai_io*) Sp[0]);
  // the buffer counts: a port holding bytes is readable however quiet its fd is
  if (fd >= 0 && !bio_rpending(bio_of(g, (struct ai_io*) Sp[0])) && !ai_ready(fd, ai_wait_in)) {
   g->next_wait_fd = fd;
   ai_musttail return Ap(lvm_yield_sw, g); } }
 ai_musttail return Next(1); }

// (fungetc port byte) — push back one byte, return the byte.
lvm(lvm_fungetc) {
 if (*task_io(g) != zero) Sp[0] = io_route(g, Sp[0]);
 if (iop(Sp[0])) {
  struct ai_io *i = (struct ai_io*) Sp[0];
  Pack(g);
  g->io = i;
  if (!ai_ok(g = zungetc(g, getcharm(g->sp[1])))) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g); }
 ai_musttail return Nextp(1, 1); }

// heap-port finalizer: runs inside GC (from-space readable); fd < 0 means
// already closed or a non-OS fd
void io_close(struct ai *g, void *p) {
 (void) g;
 struct ai_bio *b = p;                         // every finalized port is a bio (ai_io_alloc made it)
 intptr_t fd = ai_io_fd(&b->f.io);
 if (fd < 0) return;
 if (b->wbuf && !charmp(b->wbuf) && getcharm(b->wlen) > 0) // unflushed bytes ride out raw --
  ai_fd_drain((int) fd, txt(str(b->wbuf)), (uintptr_t) getcharm(b->wlen));   // from-space is readable here
 ai_fd_close(fd); }

// heap-allocate a stream port for an OS fd: push it on Sp[0], register io_close
ai_noinline struct ai *ai_io_alloc(struct ai *g, int fd) {
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

// a token is a plain decimal integer iff it is [+-]?[0-9]+ with no leading-zero
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

// ..and octal iff [+-]?0[0-7]+ ("08" keeps the strtod -> intern path). all three
// read at full precision through ai_big_read_*: an integer literal is fixnum /
// box / bignum by its value (strtol overflowed differently per libc, so the same
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

ai_noinline double strtod_wrap(struct ai*g, word x) {
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
// name string; a fixnum -> the one-byte string of its low byte. identity on any
// other type (strings, anonymous syms, zero, ...).
lvm(lvm_string) {
 word x = Sp[0];
 if (charmp(x)) {                                     // fixnum -> one-byte string
  uintptr_t req = str_width(1);
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
  uintptr_t n = llen(x), req = str_width(n);
  Have(req);
  struct ai_str *s = (void*) Hp;
  Hp += req;
  ini_str(s, n);
  for (uintptr_t i = 0; n--; x = B(x)) txt(s)[i++] = (char) getcharm(A(x));
  ai_musttail return Answer(word(s)); }
 if (caskp(x)) {                                      // a cask -> a fresh string copy of its bytes
  uintptr_t n = len(cask(x)->str), req = str_width(n);
  Have(req);
  struct ai_str *src = cask(Sp[0])->str;               // re-read post-Have (a GC may have moved the cask)
  struct ai_str *s = (void*) Hp;
  Hp += req;
  ini_str(s, n);
  memcpy(txt(s), txt(src), n);
  ai_musttail return Answer(word(s)); }
 // `string` answers a string: a string is the only identity, every other kind coerces
 // through hook 7. px reaches `string` on chains and noms only, so show cannot recur.
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
// p0's input is a charlist and its position is the list: the cursor is one love
// value on the l stack, named by its depth (a collection moves the stack, never
// a depth); p0reads piles datums above it. a lookahead needs no pushback --
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

// the next significant char, the cursor left at it: whitespace stepped over,
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
     // note: '\'' is not here -- a name keeps a trailing/internal prime (x', n'',
     // the prover idiom). a leading ' is still quote: p0read1 dispatches it as a
     // wrap before this sounder ever runs, so only a continuation ' reaches here.
     case '"': case ',': case 0 : case EOF: {   // the cursor stays on the terminator
      struct ai_str *s = str(g->sp[0]);
      txt(s)[len(s) = n] = 0; // zero terminate for am_strtod ; n < lim so this is safe
      // the three predicates are exhaustive over what a base-0 strtol accepts whole,
      // which is why the reader does not call it
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
      // no ieee-nan twin: mk_gem collapses NaN to 0, so there is no NaN value to
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
/// " p0 -- the bootstrap reader "
//
// the pure lisp subset and nothing else: delimiters, comments, strings, atoms, ' quote
// -- the sigil surface is p1's, and p1.l + egg.l are held to this subset so p0 can read
// them. control flow on the C stack, values on g->sp: datums pile on the l stack and
// fold at the close, so no love value sits in a C local across an allocation. a reader
// of a subset, not a validator -- enforcement is the differential (test/host/rdiff.l),
// and it must read what lcat prints. nesting rides the C stack, so p0 is depth-bounded
// (~100k on the host; the tree's deepest form is 38, rdiff.l pins 20000).
static struct ai *p0read1(struct ai *g, uintptr_t d);

// a list: read datums until `)`, then fold n of them off the stack. the tail is
// ZeroPoint, not zero -- reader lists are ()-terminated (the zero-ontology), and
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
  case '\\': return intern(ai_strof(g, "\\"));          // lambda/quote: never fuses (form space)
  default: return ioread1sym(g, d, c); } }              // name / number

// (sound0 text): sound's bootstrap twin over p0's grammar, for the differential.
// the text slot is the cursor: sp[0] comes in as the charlist and goes out as the
// answer; what is left in between is the residue. the body stays in an
// ai_noinline helper: a frame in the lvm_ would force the tail Continue() into a
// ret (make vmret).
ai_noinline static struct ai *p0text(struct ai *g) {
 uintptr_t const d = topof(g) - g->sp;                // the cursor's depth, and the rollback point
 g = p0read1(g, d);
 if (ai_ok(g)) return gxl(g);                         // (datum . residue), over the text slot
 enum ai_status const st = ai_code_of(g);             // no datum: which nothing?
 if (st != ai_status_eof && st != ai_status_more) return g;   // a real failure (oom) propagates
 // the rollback is not optional: a torn parse leaves p0reads's pile behind and
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
/// " the boot stitch "
//
// the egg's corpus is stitched: p0 reads the halves it owns (p1.l, prel.l,
// egg.l) and p1, the reader in love, reads ev.l -- the egg expression never
// learns. the circularity resolves by reading p1.l twice: once evaluated on the
// spot so p1 is callable, once into the corpus so it recompiles like everything
// else. at the head, never the tail: sit answers the last form's value, which
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

// read every top-level datum of a C string with p0 and cons them, in source
// order, onto the list already on top of the stack. reading the corpus right to
// left then stitches its halves with no append and no copy.
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
 if (!ai_ok(g = ai_push(g, 1, zero))) return g;       // reserve first, then copy the
 g->sp[0] = g->sp[n + 2];                            // tail up: a push can gc and move it
 for (; ai_ok(g) && n--; g = gxr(g));                //   (+2: the datums sit over the cursor)
 return ai_ok(g) ? (g->sp[2] = g->sp[0], g->sp += 2, g) : g; }

// the corpus, read by the reader in love: an ordinary call of hook 0 on the whole text
static struct ai *p1text(struct ai *g, char const *s) {
 g = ai_strof(g, s);
 g = gxr(push0(g));                                  // ("<text>")
 if (!ai_ok(g = ai_push(g, 1, zero))) return g;       // reserve first, then read the slot:
 g->sp[0] = ai_core_of(g)->hot_read;                 //   a push can gc, and the gc is what
 if (!ai_ok(g = ai_eval_(gxl(g)))) return g;          //   moves hot_read. (<reader> "<text>")
 // p1 answers `torn` for an unfinished shape; the egg would fold over it as an
 // empty corpus and silently pin ev to 0, so refuse it here (chainp and not nomp)
 word r = g->sp[0];
 return (chainp(r) && !nomp(r)) || r == ZeroPoint ? g
      : encode(ai_core_of(g), ai_status_more); }

// a text -> the list of its forms, pushed: p1 reads it once sealed, p0 until then
// (the sealed slot is the test)
static struct ai *readtext(struct ai *g, char const *s) {
 if (lamp(ai_core_of(g)->hot_read)) return p1text(g, s);
 return p0onto(push0(g), s); }

static struct ai *qtop(struct ai *g) {                // x on top -> 'x
 return gxl(pushq(gxr(push0(g)))); }                 // (x), then (\ x)

// apply a one-form driver text (pure lisp, p0-read) to the quoted list on top of
// the stack: (<driver> '(list))
static struct ai *applyq(struct ai *g, char const *driver) {
 if (!ai_ok(g)) return g;                            // ai_pop bumps sp unguarded
 g = p0onto(gxr(push0(qtop(g))), driver);            // ('(list)), then (driver '(list))
 return ai_pop(ai_eval_(g), 1); }

// the plain eval fold: run a list of forms in order, answer the last one's
// value. `ev` is read late so one text drives both of love0's passes.
static char const evfold[] = "((:(e a b)(? b(e(ev 'ev(cap b))(cup b))a)e)0)";

// every top-level form of a text, evaluated in order -- the frontends' door for
// a boot tail, a CLI driver, a corpus runner.
ai_noinline struct ai *ai_evals_(struct ai *g, char const *s) {
 g = readtext(g, s);
 return applyq(g, evfold); }

// the egg takes two corpora: `corpus` is sat twice (ev compiles itself), `post`
// once, after the hatch and before the mop -- the seat for love that needs the
// runtime-internal noms (peek/seek) the mop is about to take off the book.
ai_noinline struct ai *ai_egg_(struct ai *g, char const *egg, char const *p1,
                               char const *corpus, char const *post) {
 g = p0onto(ai_push(g, 1, ZeroPoint), p1);           // p1's forms, by p0 ..
 g = applyq(g, evfold);                              // .. and c0 evals them: p1 is live
 g = gxr(push0(qtop(p1text(g, post))));              // ('post), parked under the corpus
 g = p1text(g, corpus);                              // prel + ev, through the reader in love
 g = p0onto(g, p1);                                  // and p1 at the head of the corpus
 g = p0onto(gxl(qtop(g)), egg);                      // (egg 'corpus 'post)
 return ai_pop(ai_eval_(g), 1); }

