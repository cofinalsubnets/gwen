// host/cuda.c -- the GPU backend's C boundary for tele's device seam. A device
// PROBE (cuda-avail) plus the four kernel entry points the seam names: gemm, ew
// (elementwise), reduce, transp. Auto-globbed + AI_NIF-registered, same discipline
// as net.c/pty.c/init.c -- no love.c/love.h/main.c edit.
//
// TWO builds, one file:
//  * DEFAULT (no -DAI_CUDA): the probe reports 0 and the kernels are inert stubs.
//    crew/tele/cuda.l reads the probe and makes `cuda-dev` fall back to the NATIVE
//    backend, so (tl-use cuda-dev) is end-to-end runnable TODAY (it just runs on
//    the CPU, exactly like tl-cpu). Nothing here needs a GPU to compile or link.
//  * -DAI_CUDA against a real device: the kernels dispatch to cuBLAS / hand
//    kernels. Those bodies are sketched below and gated on ONE core change that is
//    a core-thread task, NOT smuggled in here: the vec cell accessors
//    (vec_data / ini_vec / vec_nelem) are static in love.c today, so a nif cannot yet
//    read/write a galaxy's float buffer. Until that's exported, the AI_CUDA bodies
//    stay as the marshaling sketch -- the interface is frozen, only the kernel
//    innards wait on the GPU (and that one export).
#include "love.h"

#ifdef AI_CUDA
#include <cuda_runtime.h>
#include <cublas_v2.h>
static int cuda_present(void) { int n = 0; return cudaGetDeviceCount(&n) == cudaSuccess && n > 0; }
#else
static int cuda_present(void) { return 0; }
#endif

// (cuda-avail _) -> 1 if a device is usable, else 0. Arity 1: it ignores its arg,
// so call (cuda-avail ()) -- a nullary (cuda-avail) hands back the closure unrun.
static lvm(lvm_cuda_avail) { Sp[0] = putcharm(cuda_present()); return Ip++, Continue(); }

#ifndef AI_CUDA
// Inert stubs. Never reached in the default build -- cuda-dev routes to native when
// the probe is 0 -- but present so crew/tele/cuda.l's references resolve with no warning.
// Stack effect mirrors the real ops: gemm/ew fold their operands to one result slot.
static lvm(lvm_cuda_gemm)   { *(Sp += 1) = ai_zero; return Ip++, Continue(); }   // (a b)    -> 1
static lvm(lvm_cuda_ew)     { *(Sp += 2) = ai_zero; return Ip++, Continue(); }   // (op a b) -> 1
static lvm(lvm_cuda_reduce) { Sp[0] = ai_zero;      return Ip++, Continue(); }   // (a)      -> 1
static lvm(lvm_cuda_transp) { Sp[0] = ai_zero;      return Ip++, Continue(); }   // (m)      -> 1
#else
// The real kernels. Each reads its galaxy operands' f64 buffers, runs on the
// device, and builds the result galaxy. The cuBLAS structure, ready for the vec
// accessor export (see the header note); until then these keep the stub return so
// an AI_CUDA build still links while the bodies are filled in incrementally.
static lvm(lvm_cuda_gemm) {                                  // C(m,n) = A(m,k) . B(k,n)
  // struct ai_vec *A = vec(Sp[0]), *B = vec(Sp[1]);
  // uintptr_t m = A->shape[0], k = A->shape[1], n = B->shape[1];
  // double *dA,*dB,*dC; cudaMalloc &c; cudaMemcpy H2D vec_data(A),vec_data(B);
  // double one=1, zero=0;                                   // column-major: compute B^T A^T = (A B)^T
  // cublasDgemm(h, CUBLAS_OP_N,CUBLAS_OP_N, n,m,k, &one, dB,n, dA,k, &zero, dC,n);
  // struct ai_vec *C = new gem-tray (m,n); cudaMemcpy D2H vec_data(C),dC; *(Sp += 1) = word(C);
 *(Sp += 1) = ai_zero; return Ip++, Continue();
}
static lvm(lvm_cuda_ew) {                                    // elementwise op over a,b (op = a tag, marshaled love-side)
  // dispatch a fused +/-/* kernel by the op tag in Sp[0]; broadcast a(Sp[1]), b(Sp[2]).
 *(Sp += 2) = ai_zero; return Ip++, Continue();
}
static lvm(lvm_cuda_reduce) {                                // sum-all -> a scalar
  // cublasDasum / a segmented reduce over vec_data(Sp[0]); emit a boxed f64.
 Sp[0] = ai_zero; return Ip++, Continue();
}
static lvm(lvm_cuda_transp) {                               // 2D transpose
  // cublasDgeam(h, CUBLAS_OP_T,CUBLAS_OP_N, ...) into a fresh (c,r) gem-tray.
 Sp[0] = ai_zero; return Ip++, Continue();
}
#endif

static union u const
  nif_cuda_avail[]  = {{lvm_cuda_avail}, {lvm_ret0}},
  nif_cuda_gemm[]   = {{lvm_cur}, {.x = putcharm(2)}, {lvm_cuda_gemm},   {lvm_ret0}},
  nif_cuda_ew[]     = {{lvm_cur}, {.x = putcharm(3)}, {lvm_cuda_ew},     {lvm_ret0}},
  nif_cuda_reduce[] = {{lvm_cuda_reduce}, {lvm_ret0}},
  nif_cuda_transp[] = {{lvm_cuda_transp}, {lvm_ret0}};
AI_NIF("cuda-avail",  nif_cuda_avail);
AI_NIF("cuda-gemm",   nif_cuda_gemm);
AI_NIF("cuda-ew",     nif_cuda_ew);
AI_NIF("cuda-reduce", nif_cuda_reduce);
AI_NIF("cuda-transp", nif_cuda_transp);
