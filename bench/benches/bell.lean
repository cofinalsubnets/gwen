import Bench

-- Bell numbers rendered in base 36, one per line while the rendering's length
-- stays <= limit (see bench/benches/bell.l) -- a bignum-tower stress:
--     B(n) = sum_{k<n} C(n-1,k) B(k),  C(n,k) = n!/(k!(n-k)!)
-- the values blow past 64 bits almost immediately; Lean's Nat is GMP-backed.
-- factorials and Bell numbers are memoized bottom-up in arrays (rebuilt fresh
-- per rep, like the reference's per-rep memo dicts). checksum = total characters
-- across all rendered lines = 36479.
def BASE : Nat := 36

-- number of base-36 digits of n (n >= 1).
def showLen (n : Nat) : Nat := Id.run do
  let mut m := n
  let mut c := 0
  while m > 0 do
    c := c + 1
    m := m / BASE
  return c

def bellRun (limit : Nat) : Int := Id.run do
  let mut facts : Array Nat := #[1]   -- facts[k] = k!
  let mut bells : Array Nat := #[]    -- bells[k] = B(k)
  let mut total : Int := 0
  let mut i : Nat := 0
  let mut stop := false
  while !stop do
    while facts.size ≤ i do            -- grow factorials so facts[i-1] is ready
      let s := facts.size
      facts := facts.push (facts[s-1]! * s)
    let bn : Nat :=
      if i < 2 then 1
      else Id.run do
        let mut r : Nat := 0
        for k in [0:i] do
          let c := facts[i-1]! / (facts[k]! * facts[i-1-k]!)   -- C(i-1, k)
          r := r + c * bells[k]!
        return r
    bells := bells.push bn
    let len := showLen bn
    if len > limit then
      stop := true
    else
      total := total + Int.ofNat len
      i := i + 1
  return total

-- `z` is the harness's opaque 0, threaded into the limit so the result is not
-- folded to a compile-time constant (see lib/Bench.lean).
def main : IO Unit := bench "bell" (fun z => bellRun (280 + z))
