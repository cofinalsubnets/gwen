; sum 0..N over a contiguous ARRAY -- the honest O(n) twin of sum.rs, structurally
; matched: sum.rs collects a Vec<i64> and folds it, so love builds the galaxy (iota,
; the z-array 0..N-1 filled in one C loop) and reduces it with net (a C loop over
; the cells) -- contiguous memory both sides, not a cons-spine with per-cell
; interpreted dispatch. NEITHER side gets to close the sum: sum.rs funnels each
; element through black_box, and here the array is MATERIALIZED into a value before
; net, so love's loop-closer (which only matches foldl/foldr over jot/map/filter
; ranges) has nothing to fold -- the honest O(n) reduction runs. both sides build
; AND reduce each rep. checksum = sum 0..100000 = 5000050000.
(: (work _) (: d (iota 100001) (net d))
 (bench "sum" work))
