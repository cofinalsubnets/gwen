import Bench

-- binary-trees allocation/GC stress (see bench/benches/tree.l): build a perfect
-- binary tree of depth D (2^D-1 internal nodes, leaves empty) then traverse
-- counting nodes. checksum = node count = 2^D-1 = 65535 for D = 16.
inductive Tree where
  | leaf
  | node (l r : Tree)

def mk : Nat → Tree
  | 0 => .leaf
  | d + 1 => .node (mk d) (mk d)

def ck : Tree → Nat
  | .leaf => 0
  | .node l r => 1 + ck l + ck r

-- `z` is the harness's opaque 0, threaded into the depth so the build is not
-- folded to a compile-time constant (see lib/Bench.lean).
def main : IO Unit := bench "tree" (fun z => Int.ofNat (ck (mk (16 + z))))
