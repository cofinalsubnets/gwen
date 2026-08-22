/- Lean 4 benchmark harness -- mirrors bench/bench.l and lib/bench.{py,go,rs,...}.
   `bench name work` auto-scales the repetition count (doubling until one timed
   batch clears MIN_MS), then prints one line matching the other harnesses:
       <name> <lang> <reps> <ms> <checksum>
   `work` is `Nat -> Int`: it returns a deterministic checksum and takes an
   opaque runtime zero `z` that it threads into its hot computation. That zero
   matters: Lean's compiler lifts a CLOSED pure subterm (e.g. `fib 30`) to a
   top-level CAF computed ONCE, which would make the rep loop measure nothing
   (the same hazard rustc solves with `std::hint::black_box`). Reading `z` from
   the environment (always 0) makes the work depend on a value the compiler
   can't fold, so the real O(n) work runs every iteration. BENCH_LANG sets the
   column label, default "lean". Timed with the monotonic clock; the compile is
   done ahead of time (see run.sh), so it is outside the timed region. -/

def minMs : Float := 200.0

-- render a millisecond Float to 3 decimals, e.g. 240.5 -> "240.500".
@[inline] def fmt3 (x : Float) : String :=
  let n := (x * 1000.0).round.toUInt64.toNat
  let whole := n / 1000
  let frac := n % 1000
  let pad := if frac < 10 then "00" else if frac < 100 then "0" else ""
  s!"{whole}.{pad}{frac}"

-- an opaque 0 the compiler cannot constant-fold (so each work() recomputes).
@[noinline] def opaqueZero : IO Nat := do
  return ((← IO.getEnv "BENCH_Z").bind (·.toNat?)).getD 0

def bench (name : String) (work : Nat → Int) : IO Unit := do
  let lang := (← IO.getEnv "BENCH_LANG").getD "lean"
  let z ← opaqueZero
  let mut reps : Nat := 1
  while true do
    let t0 ← IO.monoNanosNow
    let mut chk : Int := 0
    for _ in [0:reps] do
      chk := work z
    let ms := (Float.ofNat ((← IO.monoNanosNow) - t0)) / 1.0e6
    if ms ≥ minMs then
      IO.println s!"{name} {lang} {reps} {fmt3 ms} {chk}"
      break
    reps := reps * 2
