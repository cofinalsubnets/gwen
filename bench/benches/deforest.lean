import Bench

-- a map/filter/fold LIST pipeline: sum the squares-mod-p of the odd numbers in
-- [0, N) (see bench/benches/deforest.l). the `% p` keeps the body non-polynomial
-- (an honest O(n) fused loop, the FUSION counterpart to polysum's CLOSING).
-- checksum = 4891344686 (< 2^53, exact in every language).
def P : Int := 1000003

-- `z` is the harness's opaque 0, threaded into the range length so the whole
-- pipeline is not folded to a compile-time constant (see lib/Bench.lean).
def main : IO Unit := bench "deforest" (fun z =>
  (((List.range (20000 + z)).filter (fun x => x % 2 == 1)).map
    (fun x => let xi := Int.ofNat x; (xi * xi) % P)).foldl (· + ·) 0)
