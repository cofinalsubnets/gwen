; the collector's SHAPE: both degenerate while staying CORRECT, so no other assert reads
; them, and both are vacuous in a lane that barely collects.
; LATCH -- gen_please majors while major_free < g->len and a major re-sizes to need +25%,
; so a nursery above that quarter forces one at every collection. minor share measured
; 86 baked, 84 egg, 97 stress, 99 budgeted; a latch reads 0.
; REM SET -- an overflow is sound (a miss forces a major) but gen_remembered is a linear
; scan, so a grown set costs quadratically. peak 19 baked, 154 egg, 230 stress, cap 4096.
(: g (gauge 0) n (peep g 3 0) m (peep g 11 0)
 (assert (n < 64 || 2 * m > n) (0 = peep g 9 0) (peep g 10 0 < 1024)))
