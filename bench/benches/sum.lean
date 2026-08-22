import Bench

-- build the list 1..100000 then sum it. `z` is the harness's opaque 0, threaded
-- into the length so the list+fold is not folded to a constant (lib/Bench.lean).
def main : IO Unit := bench "sum" (fun z =>
  Int.ofNat ((List.range' 1 (100000 + z)).foldl (· + ·) 0))
