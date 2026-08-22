import Bench

-- sort N pseudo-random ints (a MINSTD LCG, double-safe so every language produces
-- the identical sequence) with the built-in sort, then an order-dependent rolling
-- hash of the sorted data as the checksum (see bench/benches/sort.l). = 899994547.
def M : Int := 2147483647
def HMOD : Int := 1000000007
def N : Nat := 5000

-- `z` is the harness's opaque 0, threaded into the count so the result is not
-- folded to a compile-time constant (see lib/Bench.lean).
def main : IO Unit := bench "sort" (fun z => Id.run do
  let mut data : Array Int := Array.mkEmpty (N + z)
  let mut x : Int := 1
  for _ in [0 : N + z] do
    x := (16807 * x) % M
    data := data.push x
  let sorted := data.qsort (fun a b => decide (a < b))
  let mut h : Int := 0
  for v in sorted do
    h := (h * 31 + v) % HMOD
  return h)
