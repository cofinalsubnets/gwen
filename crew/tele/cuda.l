; crew/tele/cuda.l -- the GPU backend for tele's device seam (host/cuda.c is its C half).
;
;   Run:  cat crew/tele/tele.l crew/tele/cuda.l | out/host/love     # then (tl-use cuda-dev)
;
; if host/cuda.c reports a device (cuda-avail -- 1 only in an -DAI_CUDA build on a
; box with a GPU), cuda-dev's kernels dispatch to the nifs. otherwise cuda-dev IS
; the native backend verbatim, so (tl-use cuda-dev) runs TODAY on the CPU and lights
; up on the GPU with zero change to tele's source. the four keys match the seam
; (gemm / ew / reduce / transp); `ew` carries the elementwise op -- on the GPU path
; that op is marshaled to a small tag the fused kernel selects (see host/cuda.c).
(: have (cuda-avail ())                                  ; probe once at load
   cuda-dev (? !have
               tl-cpu                                    ; no device -> native, unchanged
               (hash 'name "cuda"
                     'gemm   (\ a b    (cuda-gemm a b))
                     'ew     (\ op a b (cuda-ew op a b))
                     'reduce (\ a      (cuda-reduce a))
                     'transp (\ m      (cuda-transp m))))
   _ (puts (? !have ";; crew/tele/cuda: no device -- cuda-dev falls back to native (runs on CPU)\n"
                    ";; crew/tele/cuda: device present -- cuda-dev dispatches to the GPU nifs\n")))
