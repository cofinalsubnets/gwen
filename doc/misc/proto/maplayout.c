// maplayout.c -- isolate the cache effect of ai's map layout on the `hash` bench
// access pattern: interleaved (key,value) slots (TODAY) vs split keys[]/vals[]
// arrays (the proposed change). Same multiplicative hash + linear probe + sparse
// keys (1+97*i) + presized cap as ai's native glaze probe. Pure C -O2 so the
// numbers are the PROBE/CACHE cost (what the native lane pays), not ai eval.
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>

#define N   10000
#define CAP 16384                 // presized: next pow2 holding N below 0.75 (ai's (tablet n))
#define MASK (CAP - 1)
#define MIX  0x9e3779b97f4a7c15ULL // ai's mapmix
#define GAP  ((int64_t) 0)        // even = empty (ai's map_gap is word-aligned/even); keys stored tagged (2k+1, odd)

static inline uint64_t idx0(int64_t tagk) {
  uint64_t h = MIX * (uint64_t) tagk;
  h = (h << 32) | (h >> 32);                 // rol 32 (ai's `rol r0 32`)
  return h & MASK;
}

// ---- interleaved: slots[2i]=key, slots[2i+1]=value (ai TODAY) ----
static int64_t *il;
static void il_init(void){ for (uint64_t i=0;i<CAP;i++){ il[2*i]=GAP; il[2*i+1]=0; } }
static inline void il_put(int64_t tagk, int64_t v){
  uint64_t i=idx0(tagk);
  for(;; i=(i+1)&MASK){ int64_t k=il[2*i]; if(k==tagk){ il[2*i+1]=v; return; } if(k==GAP){ il[2*i]=tagk; il[2*i+1]=v; return; } } }
static inline int64_t il_get(int64_t tagk){
  uint64_t i=idx0(tagk);
  for(;; i=(i+1)&MASK){ int64_t k=il[2*i]; if(k==tagk) return il[2*i+1]; if(k==GAP) return 0; } }

// ---- split: keys[i], vals[i] in separate contiguous arrays (PROPOSED) ----
static int64_t *ks, *vs;
static void sp_init(void){ for (uint64_t i=0;i<CAP;i++){ ks[i]=GAP; vs[i]=0; } }
static inline void sp_put(int64_t tagk, int64_t v){
  uint64_t i=idx0(tagk);
  for(;; i=(i+1)&MASK){ int64_t k=ks[i]; if(k==tagk){ vs[i]=v; return; } if(k==GAP){ ks[i]=tagk; vs[i]=v; return; } } }
static inline int64_t sp_get(int64_t tagk){
  uint64_t i=idx0(tagk);
  for(;; i=(i+1)&MASK){ int64_t k=ks[i]; if(k==tagk) return vs[i]; if(k==GAP) return 0; } }

static inline int64_t tagk(int64_t i){ int64_t key=1+97*i; return 2*key+1; } // ai stores tagged keys

static double now(void){ struct timespec t; clock_gettime(CLOCK_MONOTONIC,&t); return t.tv_sec*1e3 + t.tv_nsec/1e6; }

int main(void){
  il = malloc(sizeof(int64_t)*2*CAP);
  ks = malloc(sizeof(int64_t)*CAP); vs = malloc(sizeof(int64_t)*CAP);
  int reps = 2000; volatile int64_t sink=0;

  // ---- interleaved: ins + scan + bump + scan, like the bench ----
  double t0=now();
  for(int r=0;r<reps;r++){
    il_init();
    for(int64_t i=0;i<N;i++) il_put(tagk(i), 2*i+1);
    int64_t a=0; for(int64_t i=0;i<N;i++) a += il_get(tagk(i))>>1;
    for(int64_t i=0;i<N;i++){ int64_t t=tagk(i); il_put(t, 2*((il_get(t)>>1)+1)+1); }
    for(int64_t i=0;i<N;i++) a += il_get(tagk(i))>>1;
    sink+=a;
  }
  double il_ms=(now()-t0)/reps;

  // ---- split ----
  t0=now();
  for(int r=0;r<reps;r++){
    sp_init();
    for(int64_t i=0;i<N;i++) sp_put(tagk(i), 2*i+1);
    int64_t a=0; for(int64_t i=0;i<N;i++) a += sp_get(tagk(i))>>1;
    for(int64_t i=0;i<N;i++){ int64_t t=tagk(i); sp_put(t, 2*((sp_get(t)>>1)+1)+1); }
    for(int64_t i=0;i<N;i++) a += sp_get(tagk(i))>>1;
    sink+=a;
  }
  double sp_ms=(now()-t0)/reps;

  printf("interleaved (k,v) slots : %.4f ms/it\n", il_ms);
  printf("split keys[]/vals[]     : %.4f ms/it\n", sp_ms);
  printf("speedup                 : %.2fx  (%.1f%% faster)\n", il_ms/sp_ms, 100*(1-sp_ms/il_ms));
  printf("(sink %lld)\n", (long long)sink);
  return 0;
}
