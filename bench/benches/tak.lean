import Bench

partial def tak (x y z : Int) : Int :=
  if y < x then tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y) else z

-- `s` is the harness's opaque 0, threaded into the input (see lib/Bench.lean).
def main : IO Unit := bench "tak" (fun s => tak (22 + Int.ofNat s) 12 6)
