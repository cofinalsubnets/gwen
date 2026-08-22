// maplayout3.c -- isolate READ/UPDATE locality (the GC compaction is charged
// elsewhere, since the copying collector walks the live set anyway). Build each
// map ONCE, then time many (scan + bump + scan) passes. Answers: does a CDR-coded
// COMPACTED chain read faster than open addressing? And how bad is the SCATTERED
// (between-GC) chain, which is what the write-burst hash bench actually hits?
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#define N 10000
#define CAP 16384
#define MASK (CAP-1)
#define MIX 0x9e3779b97f4a7c15ULL
static inline uint64_t h0(int64_t k){ uint64_t h=MIX*(uint64_t)k; h=(h<<32)|(h>>32); return h&MASK; }
static inline int64_t tagk(int64_t i){ return 2*(1+97*i)+1; }
static double now(void){ struct timespec t; clock_gettime(CLOCK_MONOTONIC,&t); return t.tv_sec*1e3+t.tv_nsec/1e6; }

static int64_t *il, *bk,*bv,*bnx,*bhd, *ck,*cv; static int *coff,*ccnt; static int bne;
static inline int64_t il_get(int64_t k){ uint64_t i=h0(k); for(;;i=(i+1)&MASK){ int64_t s=il[2*i]; if(s==k)return il[2*i+1]; if(!s)return 0; } }
static inline void il_bump(int64_t k){ uint64_t i=h0(k); for(;;i=(i+1)&MASK){ int64_t s=il[2*i]; if(s==k){il[2*i+1]=2*((il[2*i+1]>>1)+1)+1;return;} if(!s)return; } }
static inline int64_t b_get(int64_t k){ uint64_t h=h0(k); for(int j=bhd[h];j>=0;j=bnx[j]) if(bk[j]==k)return bv[j]; return 0; }
static inline void b_bump(int64_t k){ uint64_t h=h0(k); for(int j=bhd[h];j>=0;j=bnx[j]) if(bk[j]==k){bv[j]=2*((bv[j]>>1)+1)+1;return;} }
static inline int64_t c_get(int64_t k){ uint64_t h=h0(k); int e=coff[h]+ccnt[h]; for(int j=coff[h];j<e;j++) if(ck[j]==k)return cv[j]; return 0; }
static inline void c_bump(int64_t k){ uint64_t h=h0(k); int e=coff[h]+ccnt[h]; for(int j=coff[h];j<e;j++) if(ck[j]==k){cv[j]=2*((cv[j]>>1)+1)+1;return;} }

int main(void){
  il=calloc(2*CAP,8); bk=malloc(8*N);bv=malloc(8*N);bnx=malloc(8*N);bhd=malloc(8*CAP);
  ck=malloc(8*N);cv=malloc(8*N);coff=malloc(4*CAP);ccnt=malloc(4*CAP);
  // build all three ONCE
  for(int64_t i=0;i<N;i++){ int64_t k=tagk(i); uint64_t p=h0(k); while(il[2*p]){ if(il[2*p]==k)break; p=(p+1)&MASK;} il[2*p]=k; il[2*p+1]=2*i+1; }
  for(uint64_t i=0;i<CAP;i++)bhd[i]=-1; bne=0;
  for(int64_t i=0;i<N;i++){ int64_t k=tagk(i); uint64_t h=h0(k); bk[bne]=k;bv[bne]=2*i+1;bnx[bne]=bhd[h];bhd[h]=bne;bne++; }
  for(uint64_t i=0;i<CAP;i++)ccnt[i]=0; for(int j=0;j<bne;j++)ccnt[h0(bk[j])]++;
  int off=0; for(uint64_t i=0;i<CAP;i++){coff[i]=off;off+=ccnt[i];ccnt[i]=0;}
  for(int j=0;j<bne;j++){uint64_t h=h0(bk[j]);int p=coff[h]+ccnt[h]++;ck[p]=bk[j];cv[p]=bv[j];}

  int reps=20000; volatile int64_t sink=0; double t0;
  t0=now(); for(int r=0;r<reps;r++){ int64_t a=0; for(int64_t i=0;i<N;i++)a+=il_get(tagk(i))>>1; for(int64_t i=0;i<N;i++)il_bump(tagk(i)); for(int64_t i=0;i<N;i++)a+=il_get(tagk(i))>>1; sink+=a; } double am=(now()-t0)/reps;
  t0=now(); for(int r=0;r<reps;r++){ int64_t a=0; for(int64_t i=0;i<N;i++)a+=b_get(tagk(i))>>1;  for(int64_t i=0;i<N;i++)b_bump(tagk(i));  for(int64_t i=0;i<N;i++)a+=b_get(tagk(i))>>1;  sink+=a; } double bm=(now()-t0)/reps;
  t0=now(); for(int r=0;r<reps;r++){ int64_t a=0; for(int64_t i=0;i<N;i++)a+=c_get(tagk(i))>>1;  for(int64_t i=0;i<N;i++)c_bump(tagk(i));  for(int64_t i=0;i<N;i++)a+=c_get(tagk(i))>>1;  sink+=a; } double cm=(now()-t0)/reps;

  printf("read/update passes only (scan+bump+scan), structure pre-built:\n");
  printf("A open-addressing interleaved : %.4f ms/it   (baseline)\n", am);
  printf("B chaining SCATTERED          : %.4f ms/it   %.2fx\n", bm, am/bm);
  printf("C chaining COMPACTED (cdr)    : %.4f ms/it   %.2fx\n", cm, am/cm);
  printf("(sink %lld)\n",(long long)sink);
  return 0;
}
