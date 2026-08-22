// maplayout2.c -- open addressing vs CDR-coded CHAINING for the `hash` bench
// access pattern (ins 10000 sparse keys, then scan/bump/scan). Chaining is shown
// in BOTH regimes: SCATTERED (insertion-order entries, chains pointer-chase -- what
// the bench actually hits, since it inserts then reads with no GC between) and
// COMPACTED (each bucket's chain a contiguous run, no next-pointers -- the post-GC
// ideal the copying collector would produce). Same multiplicative hash throughout.
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

#define N    10000
#define CAP  16384
#define MASK (CAP - 1)
#define MIX  0x9e3779b97f4a7c15ULL

static inline uint64_t h0(int64_t tagk){ uint64_t h=MIX*(uint64_t)tagk; h=(h<<32)|(h>>32); return h&MASK; }
static inline int64_t tagk(int64_t i){ return 2*(1+97*i)+1; }
static double now(void){ struct timespec t; clock_gettime(CLOCK_MONOTONIC,&t); return t.tv_sec*1e3+t.tv_nsec/1e6; }

// ---- A: open addressing, interleaved (key,value) slots (ai TODAY) ----
static int64_t *il;
static inline void il_put(int64_t k,int64_t v){ uint64_t i=h0(k); for(;;i=(i+1)&MASK){ int64_t s=il[2*i]; if(s==k){il[2*i+1]=v;return;} if(s==0){il[2*i]=k;il[2*i+1]=v;return;} } }
static inline int64_t il_get(int64_t k){ uint64_t i=h0(k); for(;;i=(i+1)&MASK){ int64_t s=il[2*i]; if(s==k)return il[2*i+1]; if(s==0)return 0; } }

// ---- B: chaining, SCATTERED (insertion order + next-index links) ----
static int64_t *bk;          // key[], val[] per entry (insertion order); nx[] next-index; hd[] bucket heads
static int64_t *bv, *bnx, *bhd; static int bne;
static inline void b_put(int64_t k,int64_t v){ uint64_t h=h0(k); for(int j=bhd[h];j>=0;j=bnx[j]) if(bk[j]==k){bv[j]=v;return;} bk[bne]=k;bv[bne]=v;bnx[bne]=bhd[h];bhd[h]=bne;bne++; }
static inline int64_t b_get(int64_t k){ uint64_t h=h0(k); for(int j=bhd[h];j>=0;j=bnx[j]) if(bk[j]==k)return bv[j]; return 0; }
static inline void b_bump(int64_t k){ uint64_t h=h0(k); for(int j=bhd[h];j>=0;j=bnx[j]) if(bk[j]==k){bv[j]=2*((bv[j]>>1)+1)+1;return;} }

// ---- C: chaining, COMPACTED (bucket-contiguous runs, cdr-coded: no next ptr) ----
static int64_t *ck, *cv; static int *coff, *ccnt;   // ck/cv sorted by bucket; coff[h]/ccnt[h]
static inline int64_t c_get(int64_t k){ uint64_t h=h0(k); int e=coff[h]+ccnt[h]; for(int j=coff[h];j<e;j++) if(ck[j]==k)return cv[j]; return 0; }
static inline void c_bump(int64_t k){ uint64_t h=h0(k); int e=coff[h]+ccnt[h]; for(int j=coff[h];j<e;j++) if(ck[j]==k){cv[j]=2*((cv[j]>>1)+1)+1;return;} }

int main(void){
  il=malloc(16*CAP);
  bk=malloc(8*N); bv=malloc(8*N); bnx=malloc(8*N); bhd=malloc(8*CAP);
  ck=malloc(8*N); cv=malloc(8*N); coff=malloc(4*CAP); ccnt=malloc(4*CAP);
  int reps=2000; volatile int64_t sink=0;

  // A: open addressing
  double t0=now();
  for(int r=0;r<reps;r++){ memset(il,0,16*CAP);
    for(int64_t i=0;i<N;i++) il_put(tagk(i),2*i+1);
    int64_t a=0; for(int64_t i=0;i<N;i++) a+=il_get(tagk(i))>>1;
    for(int64_t i=0;i<N;i++){ int64_t t=tagk(i); il_put(t,2*((il_get(t)>>1)+1)+1); }
    for(int64_t i=0;i<N;i++) a+=il_get(tagk(i))>>1; sink+=a; }
  double a_ms=(now()-t0)/reps;

  // B: chaining scattered (the bench's actual write-burst reality)
  t0=now();
  for(int r=0;r<reps;r++){ for(uint64_t i=0;i<CAP;i++) bhd[i]=-1; bne=0;
    for(int64_t i=0;i<N;i++) b_put(tagk(i),2*i+1);
    int64_t a=0; for(int64_t i=0;i<N;i++) a+=b_get(tagk(i))>>1;
    for(int64_t i=0;i<N;i++) b_bump(tagk(i));
    for(int64_t i=0;i<N;i++) a+=b_get(tagk(i))>>1; sink+=a; }
  double b_ms=(now()-t0)/reps;

  // C: chaining compacted (post-GC ideal). Build scattered, then compact ONCE
  // (as a collection would), then run the read/bump/read passes on the compact form.
  t0=now();
  for(int r=0;r<reps;r++){
    for(uint64_t i=0;i<CAP;i++) bhd[i]=-1; bne=0;
    for(int64_t i=0;i<N;i++) b_put(tagk(i),2*i+1);          // build (the insert pass; same cost as B's ins)
    // compact: bucket-sorted contiguous runs
    for(uint64_t i=0;i<CAP;i++) ccnt[i]=0;
    for(int j=0;j<bne;j++) ccnt[h0(bk[j])]++;
    int off=0; for(uint64_t i=0;i<CAP;i++){ coff[i]=off; off+=ccnt[i]; ccnt[i]=0; }
    for(int j=0;j<bne;j++){ uint64_t h=h0(bk[j]); int p=coff[h]+ccnt[h]++; ck[p]=bk[j]; cv[p]=bv[j]; }
    int64_t a=0; for(int64_t i=0;i<N;i++) a+=c_get(tagk(i))>>1;
    for(int64_t i=0;i<N;i++) c_bump(tagk(i));
    for(int64_t i=0;i<N;i++) a+=c_get(tagk(i))>>1; sink+=a; }
  double c_ms=(now()-t0)/reps;

  printf("A open-addressing interleaved (ai today) : %.4f ms/it   (baseline)\n", a_ms);
  printf("B cdr-chaining SCATTERED (bench reality)  : %.4f ms/it   %.2fx\n", b_ms, a_ms/b_ms);
  printf("C cdr-chaining COMPACTED (post-GC ideal)  : %.4f ms/it   %.2fx\n", c_ms, a_ms/c_ms);
  printf("(sink %lld)\n",(long long)sink);
  return 0;
}
