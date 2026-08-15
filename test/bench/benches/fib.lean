import Bench

partial def fib (n : Int) : Int := if n < 2 then n else fib (n - 1) + fib (n - 2)

-- `z` is the harness's opaque 0; it threads into the input so fib(30) is not
-- folded to a compile-time constant (see lib/Bench.lean).
def main : IO Unit := bench "fib" (fun z => fib (30 + Int.ofNat z))
