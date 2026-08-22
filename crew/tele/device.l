; crew/tele/device.l -- a SIM backend for tele's device seam, plus a roofline meter.
;
; tele.l owns the seam (the { gemm ew reduce transp } record + tl-use/tl-gemm/...);
; the DEFAULT backend `tl-cpu` is love's native array ops. this file adds `sim-dev`:
; the exact same math, but every kernel METERS itself -- FLOPs done, bytes touched,
; launches -- into `dv-stats`. `dv-roofline` then turns that tally into a time
; estimate for a named device (peak TFLOP/s + memory GB/s), so you can size the
; kernel decomposition (is this layer compute- or memory-bound?) before any silicon.
;
;   Run:  cat crew/tele/tele.l crew/tele/device.l | out/host/love      # then (tl-use sim-dev)
;
; a "kernel" here is coarse (one gemm / one elementwise / one reduce) -- roofline
; granularity, the right altitude to answer "how big a GPU do I need" without a
; warp simulator.

(: ;; -- shape helpers (shape is a LIST -- walk it with cap/cup, like tele does) --
   (dv-prod l)   (? (two? l) (<l * dv-prod >l) 1)
   (dv-rows v)   <(shape v)                            ; axis 0
   (dv-cols v)   <>(shape v)                           ; axis 1
   (dv-cells v)  (? (tray? v) (dv-prod (shape v)) 1)   ; product of the shape
   dv-elsz 8                                           ; a gem is 8 bytes (f64)

   ;; -- the meter: a mutable tally the sim kernels feed --
   dv-stats {}
   (dv-reset _) (pins dv-stats 'flop 0 'byte 0 'launch 0)
   (dv-bump f b) (pins dv-stats
    'flop   (f + peep dv-stats 'flop 0)
    'byte   (b + peep dv-stats 'byte 0)
    'launch (1 + peep dv-stats 'launch 0))

   ;; per-kernel FLOP / byte cost -- the roofline inputs.
   ;;   gemm (m,k)x(k,n): 2mkn flops; reads A+B, writes C
   ;;   ewise n cells    : n flops;    reads 2, writes 1
   ;;   reduce n cells   : n flops;    reads 1, writes ~0
   ;;   transp n cells   : 0 flops;    reads 1, writes 1 (pure data movement)
   (dv-gemm-cost a b) (: m (dv-rows a) k (dv-cols a) n (dv-cols b)
                       (2 * m * k * n >< dv-elsz * (m * k + k * n + m * n)))
   (dv-ew-cost a)     (: n (dv-cells a) (n >< dv-elsz * 3 * n))
   (dv-red-cost a)    (: n (dv-cells a) (n >< dv-elsz * n))
   (dv-tr-cost a)     (: n (dv-cells a) (0 >< dv-elsz * 2 * n))

   ;; -- the SIM backend: tele's four kernels, native math WRAPPED in the meter.
   ;; keys match tele's seam exactly (gemm/ew/reduce/transp); `ew` takes the op. --
   sim-dev {'name "sim"
            'gemm   (\ a b   (: c (dv-gemm-cost a b) _ (dv-bump <c >c) (inner a b)))
            'ew     (\ op a b (: c (dv-ew-cost a)    _ (dv-bump <c >c) (op a b)))
            'reduce (\ a     (: c (dv-red-cost a)    _ (dv-bump <c >c) (net a)))
            'transp (\ m     (: c (dv-tr-cost m)     _ (dv-bump <c >c) (transp2 m)))}

   ;; -- the roofline: turn the meter into a time estimate for a named device.
   ;; spec = { name tflops gbps }: peak compute (TFLOP/s) and memory BW (GB/s).
   ;; time = max(flops/peak, bytes/bw); intensity = flops/byte (the roofline knee).
   (device name tflops gbps) (hash 'name name 'tflops tflops 'gbps gbps)
   (dv-roofline spec)
      (: f  (peep dv-stats 'flop 0)   b (peep dv-stats 'byte 0)
         tf (peep spec 'tflops 0 * 1e12) bw (peep spec 'gbps 0 * 1e9)
         tc (f / tf)  tm (b / bw)                       ; compute-time, memory-time (seconds)
         t  (? (tc < tm) tm tc)  bound (? (tc < tm) "memory" "compute")
         love (? !b 0 (f / b))                            ; arithmetic intensity (flop/byte)
         _ (puts "-- roofline on ") _ (puts (peep spec 'name "?")) _ (putc 10)
         _ (puts "   flops     = ") _ (puts (show f)) _ (putc 10)
         _ (puts "   bytes     = ") _ (puts (show b)) _ (putc 10)
         _ (puts "   launches  = ") _ (puts (show (peep dv-stats 'launch 0))) _ (putc 10)
         _ (puts "   intensity = ") _ (puts (show love)) _ (puts " flop/byte") _ (putc 10)
         _ (puts "   est time  = ") _ (puts (show (* t 1e3))) _ (puts " ms (") _ (puts bound) _ (puts "-bound)") _ (putc 10)
         t))
