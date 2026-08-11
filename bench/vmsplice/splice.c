// the composition probe -- does splicing lvm_ bodies into one function pay, and does
// mooncc keep the VM state in registers across the splice?
//
// ⚠ v1 of this probe used cap;charmp over a charm and reported 60x. That was FOLDING:
// both ops are constant on a charm, so the whole composed body collapsed to one store
// (0.023 ns/op, under a cycle). Every operand here is reached through a volatile seed
// and a real heap chain, so nothing folds into the answer it is meant to compute.
//
// the sequence is 64 x lvm_cup -- walk a chain. Both lanes do the IDENTICAL walk, so
// the pointer-chase latency is common-mode and the difference is the dispatch.
//   thread   -- a real thread, dispatched the normal tail-threaded way (64 indirect jumps)
//   composed -- the same 64 op bodies spliced, one dispatch at the end
//   cmp+Have -- the same, with quon's Have1 safepoint in every body
// compile the same file with mooncc and with cc: if composition pays under cc and not
// under mooncc, the JIT's blocker is the regalloc arc, not the method.
#include "love.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

extern struct ai *ai_ini(void);
extern lvm_t lvm_cap, lvm_cup, lvm_charmp, lvm_quo1;

// the frontend seam love.c expects: this probe never collects, sleeps or does i/o,
// so a counter and zeroed immortals are the whole host it needs.
static uintptr_t tick_ = 0;
uintptr_t ai_clock(void) { return ++tick_; }
void ai_sleep(uintptr_t t) { (void) t; }
struct ai_port_vt const ai_fd_port_vt;
struct ai_fio ai_stdin, ai_stdout, ai_stderr;

#define NOPS 64
#define NLINK 96
#define ITERS 1000000

// the terminator: pack the state and hand g back, ending the chain
static _lvm(vm_stop) { Pack(g); return g; }

// --- the op bodies, verbatim from love.c with the dispatch deleted ---------
// op11(lvm_cup, chainp(Sp[0]) ? B(Sp[0]) : ZeroPoint)
#define B_CUP()  Sp[0] = chainp(Sp[0]) ? B(Sp[0]) : ZeroPoint;
// quon(lvm_quo1, 1) -- carries Have1(), the safepoint; paired with a pop to stay neutral
// the same cup body with a safepoint in front -- isolates what Have1 costs per op
#define B_CUP_HV() { Have1(); Sp[0] = chainp(Sp[0]) ? B(Sp[0]) : ZeroPoint; }

#define P2(a, b) a b
#define X4(m)  m m m m
#define X16(m) X4(m) X4(m) X4(m) X4(m)
#define X64(m) X16(m) X16(m) X16(m) X16(m)

// the COMPOSED lane: 64 op bodies, one dispatch at the end
static _lvm(composed) {
  X64(B_CUP())
  Ip += NOPS;
  ai_musttail return Continue();
}

// the SAFEPOINT lane: 64 bodies, every one carrying a Have1
static _lvm(composed_hv) {
  X64(B_CUP_HV())
  Ip += NOPS;
  ai_musttail return Continue();
}

static double now_s(void) { return (double) clock() / (double) CLOCKS_PER_SEC; }

int main(void) {
  struct ai *g = ai_ini();
  if (!g) { fprintf(stderr, "ai_ini failed\n"); return 1; }
  struct ai *c = ai_core_of(g);

  // a real chain, NLINK links, laid by love.h's own ctor. static storage: we never collect.
  static struct ai_chain link[NLINK];
  for (int i = NLINK - 1; i >= 0; i--)
    ini_chain(&link[i], putcharm(i), i + 1 < NLINK ? (intptr_t) &link[i + 1] : ZeroPoint);

  static union u th[NOPS + 1], thq[NOPS + 1];
  for (int i = 0; i < NOPS; i++) th[i].ap = lvm_cup;
  th[NOPS].ap = vm_stop;
  for (int i = 0; i < NOPS; i++) thq[i].ap = lvm_quo1;
  thq[NOPS].ap = vm_stop;

  ai_word *Hp = c->hp, *Sp0 = c->sp;
  volatile intptr_t seed = (intptr_t) &link[0];   // opaque: nothing below folds

  double t0, t1;
  struct ai *r = g;
  volatile intptr_t sink = 0;

  // --- lane 1: the thread, dispatched ---
  t0 = now_s();
  for (long k = 0; k < ITERS; k++) {
    ai_word *Sp = Sp0; Sp[0] = seed;
    r = th[0].ap(g, th, Hp, Sp);
    sink += ai_core_of(r)->sp[0]; }
  t1 = now_s();
  double d_thread = t1 - t0;
  intptr_t v_thread = ai_core_of(r)->sp[0];

  // --- lane 2: the composed body ---
  t0 = now_s();
  for (long k = 0; k < ITERS; k++) {
    ai_word *Sp = Sp0; Sp[0] = seed;
    r = composed(g, th, Hp, Sp);
    sink += ai_core_of(r)->sp[0]; }
  t1 = now_s();
  double d_comp = t1 - t0;
  intptr_t v_comp = ai_core_of(r)->sp[0];

  // --- lane 3: composed with a Have1 safepoint in every body (same walk) ---
  t0 = now_s();
  for (long k = 0; k < ITERS; k++) {
    ai_word *Sp = Sp0; Sp[0] = seed;
    r = composed_hv(g, th, Hp, Sp);
    sink += ai_core_of(r)->sp[0]; }
  t1 = now_s();
  double d_hv = t1 - t0;
  intptr_t v_hv = ai_core_of(r)->sp[0];

  double ns = 1e9 / (double) ITERS / (double) NOPS;
  printf("  ops/seq %d   iters %d   chain %d links\n", NOPS, ITERS, NLINK);
  printf("  cup  thread (dispatched) %7.4f s  %6.3f ns/op   walked to %ld\n",
         d_thread, d_thread * ns, (long) getcharm(A(v_thread)));
  printf("  cup  composed (spliced)  %7.4f s  %6.3f ns/op   walked to %ld\n",
         d_comp, d_comp * ns, (long) getcharm(A(v_comp)));
  printf("  cup  composed +Have1     %7.4f s  %6.3f ns/op   walked to %ld\n",
         d_hv, d_hv * ns, (long) getcharm(A(v_hv)));
  printf("  AGREE: %s\n", (v_thread == v_comp && v_thread == v_hv) ? "yes" : "NO -- composition is WRONG");
  printf("  speedup vs dispatch:  composed %.2fx   composed+Have1 %.2fx\n",
         d_thread / d_comp, d_thread / d_hv);
  if (sink == 0x7fffffffL) printf(" ");
  return v_thread == v_comp ? 0 : 2; }
