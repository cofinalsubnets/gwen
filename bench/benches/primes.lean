import Bench

-- count primes below 30000 by trial division (see bench/benches/primes.l) --
-- integer arithmetic + branching. checksum = pi(30000) = 3245.
def isPrime (n : Nat) : Bool := Id.run do
  for d in [2:n] do
    if d * d > n then break
    if n % d == 0 then return false
  return true

-- `z` is the harness's opaque 0, threaded into the upper bound so the count is
-- not folded to a compile-time constant (see lib/Bench.lean).
def main : IO Unit := bench "primes" (fun z => Id.run do
  let mut c : Int := 0
  for n in [2 : 30000 + z] do
    if isPrime n then c := c + 1
  return c)
