import Bench

-- bintrees -- the benchmark-game binary-trees (see bench/benches/bintrees.l): a
-- GC-throughput / long-lived-survival workload. build a stretch tree of depth
-- max+1, hold a long-lived tree of depth max alive across the run, then for each
-- depth d in min..max step 2 build 2^(max-d+min) short-lived trees and sum their
-- node counts. checksum = stretch + long-count + the depth sums = 1600174.
inductive Tree where
  | leaf
  | node (l r : Tree)

def mk : Nat → Tree
  | 0 => .leaf
  | d + 1 => .node (mk d) (mk d)

def ck : Tree → Nat
  | .leaf => 0
  | .node l r => 1 + ck l + ck r

def btRun (mn mx : Nat) : Int := Id.run do
  let stretch := ck (mk (mx + 1))
  let long := mk mx                    -- LONG-LIVED -- survives the loop below
  let mut total : Nat := 0
  let steps := (mx - mn) / 2 + 1
  for j in [0:steps] do
    let d := mn + 2 * j
    let n := 1 <<< (mx - d + mn)        -- 2^(max-d+min) trees at this depth
    let mut s : Nat := 0
    for _ in [0:n] do
      s := s + ck (mk d)
    total := total + s
  return Int.ofNat (stretch + ck long + total)

-- `z` is the harness's opaque 0, threaded into max so the result is not folded to
-- a compile-time constant (see lib/Bench.lean).
def main : IO Unit := bench "bintrees" (fun z => btRun 4 (14 + z))
