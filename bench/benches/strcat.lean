import Bench

-- build an N-char string by repeated single-char concatenation, then fold a
-- polynomial rolling hash over it (see bench/benches/strcat.l). the hash is taken
-- mod a prime so it stays < 2^31 and every language agrees. checksum = 222329890.
def HMOD : Int := 1000000007
def N : Nat := 4000

-- `z` is the harness's opaque 0, threaded into the build length so the result is
-- not folded to a compile-time constant (see lib/Bench.lean).
def main : IO Unit := bench "strcat" (fun z => Id.run do
  let mut s : String := ""
  for i in [0 : N + z] do
    s := s ++ String.singleton (Char.ofNat (48 + i % 10))
  let mut h : Int := 0
  for c in s.toList do
    h := (h * 31 + Int.ofNat c.toNat) % HMOD
  return h)
