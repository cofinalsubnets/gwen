import Bench
def twice (f : Int → Int) : Int → Int := fun x => f (f x)
def adder (i : Int) : Int → Int := fun x => x + i
def N : Nat := 100000
def M : Int := 1000000007
def main : IO Unit := bench "closure" (fun z => Id.run do
  let mut acc : Int := 0
  for i in [0 : N + z] do
    let ii := Int.ofNat i
    acc := (acc * 31 + twice (adder ii) ii) % M
  return acc)
