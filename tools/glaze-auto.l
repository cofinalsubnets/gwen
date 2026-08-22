; glaze-auto -- the AUTO-HOOK end to end: take the float benchmark's UNCHANGED source (the
; mand/col/grid mandelbrot nest from bench/benches/float.l), recognize it with `grok`, glaze it
; to native, and time it the way the bench does. demonstrates that ordinary interpreted source
; gets LuaJIT-territory native code with NO hand-annotation -- the payoff the benches would read
; once the recognizer is wired into ev's compile path.
;
; run:  out/host/love -l love/glaze/emit.l tools/glaze-auto.l   (emit.l leaks grok/jitfgrid as globals)

(: ; --- the EXACT bodies from bench/benches/float.l (verbatim) ---
   mps   '(cx cy zx zy it)
   mbody '(? (&& (< it 100) (<= (+ (* zx zx) (* zy zy)) 4.0))
             (mand cx cy (+ (- (* zx zx) (* zy zy)) cx) (+ (* (* 2.0 zx) zy) cy) (+ it 1)) it)
   cbody '(? (< py 64) (col px (+ py 1) (+ acc (mand (+ -2.0 (* px 0.046875)) (+ -1.5 (* py 0.046875)) 0.0 0.0 0))) acc)
   gbody '(? (< px 64) (grid (+ px 1) (+ acc (col px 0 0))) acc)
   ; the interpreted reference (what the bench runs today): rebuild the nest and call grid(0,0)
   interp ((ev `('\ '_ `(': `('mand 'cx 'cy 'zx 'zy 'it) mbody `('col 'px 'py 'acc) cbody `('grid 'px 'acc) gbody `('grid 0 0)))) 0)

   ; --- the AUTO-HOOK: recognize the nest from source and glaze it. NO change to the source. ---
   ng (grok '(\ _ 0) mps mbody cbody gbody 'x64)
   _ (? (id? ng ()) (: _ (. "grok did NOT recognize the nest\n") (exit 1)) 0)
   native (ng 0)

   _ (. (+ "checksum: interp=" (+ (show interp) (+ " native=" (+ (show native)
            (? (&& (= 87130 interp) (= interp native)) "  OK (auto-glazed, bit-exact)\n" "  MISMATCH\n"))))))

   ; --- time both, bench style: us/grid + the reps/time throughput the bench reports ---
   (rep f n a) (? (< n 0) a (rep f (- n 1) (+ a (f 0))))
   ti (: a (clock 0) _ ((\ _ ((: (gi px acc) (? (< px 64) (gi (+ px 1) (+ acc ((: (ci py ac) (? (< py 64) (ci (+ py 1) (+ ac ((: (mi cx cy zx zy it) (? (&& (< it 100) (<= (+ (* zx zx) (* zy zy)) 4.0)) (mi cx cy (+ (- (* zx zx) (* zy zy)) cx) (+ (* (* 2.0 zx) zy) cy) (+ it 1)) it)) (+ -2.0 (* px 0.046875)) (+ -1.5 (* py 0.046875)) 0.0 0.0 0))) ac)) 0 0))) acc)) 0 0)) 0) (- (clock 0) a))
   tn (: a (clock 0) _ (rep ng 1999 0) (// (* (- (clock 0) a) 1000) 2000))
   _ (. (+ "interp 1 grid:  " (+ (show ti) " ms\n")))
   _ (. (+ "native /grid:   " (+ (show tn) " us  (was interp ~14700 us/grid; node 196; luajit 220)\n")))
   (. ";; glaze-auto: float.l source auto-glazed to native, LuaJIT-territory, no source change\n"))
