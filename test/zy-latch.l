; THE LATCH, and why it wants an assertion of its own. gen_please forces a MAJOR while
; major_free < g->len, and every major re-sizes the pool to need + 25% -- so a nursery
; above that quarter forces one at EVERY collection, and the major it forces puts the
; quarter back. the collector stops being generational and stays CORRECT: every assert
; in this corpus still passes and the lane only takes longer, which is a thing no gate
; reads. so assert the COVERAGE. past enough collections for the share to mean anything,
; most of them must be minors; vacuous in a lane that barely collects.
; measured shares: 86 baked, 84 egg, 97 stress, 99 under a budget -- a latch reads 0.
(: g (gauge 0) n (peep g 3 0) m (peep g 11 0)
 (assert (n < 64 || 2 * m > n)))
