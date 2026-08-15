; mk/tools/gcstat.l -- weigh a GENERATIONAL collector empirically: measure infant
; mortality (how much of each collection's scan was already dead) and copy
; amplification (how many times the live set gets recopied) on real workloads.
; the gauge counters are always on:
;   cat mk/tools/gcstat.l | out/host/love                                     ; interpreted
;   cat love/glaze/emit.l love/glaze/auto.l mk/tools/gcstat.l | out/host/love ; glazed (x86-64)
;
; gauge 0 -> @(len heap stack n_gc max_len max_heap n_seen n_evac):
;   mortality = (n_seen - n_evac)/n_seen ; copy-amp = n_evac/max_heap.
(:
  G_GC 3  G_MAXH 5  G_SEEN 6  G_EVAC 7                                  ; gauge indices
  (gget i) (peep (gauge 0) i 0)
  (pct a b) (? (= b 0) 0 (// (* a 100) b))                             ; a/b as an integer percent
  (per a b) (? (= b 0) 0 (// a b))                                     ; integer a/b
  (runk work k) ((: (go i a) (? (< i k) (go (+ i 1) (work 0)) a)) 0 0)  ; run work k times -> last chk

  ; run `work` k times between two gauge reads; print the window's churn.
  (gcwin name k work)
   (: s0 (gget G_SEEN) e0 (gget G_EVAC) g0 (gget G_GC)
      chk (runk work k)
      ds (- (gget G_SEEN) s0) de (- (gget G_EVAC) e0) dg (- (gget G_GC) g0)
    (: _ (puts name) _ (puts "  colls=") _ (putx dg)
       _ (puts "  mortality=") _ (putx (pct (- ds de) ds)) _ (puts "%")
       _ (puts "  copied/coll=") _ (putx (per de dg))
       _ (puts "  scanned/coll=") _ (putx (per ds dg))
       _ (puts "  copied=") _ (putx de)
       _ (putc 10) chk))

  ; --- workloads --------------------------------------------------------------
  (sq x) (* x x)   (odd x) (= 1 (% x 2))
  ; deforest: the map/filter/fold LIST pipeline -- 3 intermediate lists/interp.
  (deforest n) (foldl (+) 0 (map sq (filter odd (jot n))))
  ; primes: trial division -- integer/branch bound, near-zero allocation.
  (prime? n) ((: (go d) (? (< n (* d d)) 1 (? (= 0 (% n d)) 0 (go (+ d 1))))) 2)
  (primes hi) ((: (go n c) (? (< n hi) (go (+ n 1) (? (prime? n) (+ c 1) c)) c)) 2 0)
  ; fib: naive recursion -- integer, allocation-free call churn.
  (fib n) (? (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))
  ; mapchurn: map a fresh n-list M times -- pure infant mortality (M-1 lists die).
  (mapchurn n m) ((: (go i l) (? (< i m) (go (+ i 1) (map (+ 1) l)) (tally l))) 0 (jot n))
  ; revbig: fold a jot into a fresh reversed list -- n cons cells, then dropped.
  (revbig n) (tally (foldl (\ a x (link x a)) () (jot n)))

  ; --- run --------------------------------------------------------------------
  _ (puts "workload              churn (a -DAI_STAT host; interpreted unless glazed)") _ (putc 10)
  _ (gcwin "deforest-20k x40 " 40 (\ _ (deforest 20000)))
  _ (gcwin "mapchurn-50k x40 " 40 (\ _ (mapchurn 50000 20)))
  _ (gcwin "revbig-100k  x40 " 40 (\ _ (revbig 100000)))
  _ (gcwin "primes-30k   x4  " 4  (\ _ (primes 30000)))
  _ (gcwin "fib-30       x3  " 3  (\ _ (fib 30)))
  ; --- whole-run cumulative ---------------------------------------------------
  _ (putc 10)
  _ (puts "cumulative (incl boot)  colls=") _ (putx (gget G_GC))
  _ (puts "  mortality=") _ (putx (pct (- (gget G_SEEN) (gget G_EVAC)) (gget G_SEEN))) _ (puts "%")
  _ (puts "  copy-amp(x100)=") _ (putx (per (* (gget G_EVAC) 100) (gget G_MAXH)))
  _ (puts "  peak-live=") _ (putx (gget G_MAXH))
  (putc 10))
