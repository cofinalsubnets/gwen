import Std.Data.HashMap
import Bench
open Std

-- mutable hash-table throughput (see bench/benches/hash.l). checksum = N*N.
def N : Nat := 10000

-- `z` is the harness's opaque 0, threaded into every key so the table build is
-- not folded to a compile-time constant (see lib/Bench.lean).
def main : IO Unit := bench "hash" (fun z =>
  let zk := Int.ofNat z
  Id.run do
    let mut h : HashMap Int Int := {}
    for i in [0:N] do
      h := h.insert (97 * Int.ofNat i + 1 + zk) (Int.ofNat i)
    let mut a : Int := 0
    for i in [0:N] do
      a := a + h.get! (97 * Int.ofNat i + 1 + zk)
    for i in [0:N] do
      let k := 97 * Int.ofNat i + 1 + zk
      h := h.insert k (h.get! k + 1)
    for i in [0:N] do
      a := a + h.get! (97 * Int.ofNat i + 1 + zk)
    return a)
