Code.require_file("../lib/bench.exs", __DIR__)

# bintrees -- the benchmark-game binary-trees (see bench/benches/bintrees.l): a
# GC-throughput / long-lived-survival workload. build a stretch tree of depth
# max+1, hold a long-lived tree of depth max alive across the run, then for each
# depth d in min..max step 2 build 2^(max-d+min) short-lived trees and sum their
# node counts. a leaf is nil and counts 0.
defmodule Bintrees do
  def mk(d) when d < 1, do: nil
  def mk(d), do: {mk(d - 1), mk(d - 1)}

  def ck(nil), do: 0
  def ck({l, r}), do: 1 + ck(l) + ck(r)

  def bt_run(mn, mx) do
    stretch = ck(mk(mx + 1))
    long = mk(mx)                       # LONG-LIVED -- survives the loop below
    total =
      mn..mx//2
      |> Enum.reduce(0, fn d, total ->
        n = Bitwise.bsl(1, mx - d + mn)
        s = Enum.reduce(1..n, 0, fn _, s -> s + ck(mk(d)) end)
        total + s
      end)

    stretch + ck(long) + total
  end
end

Bench.run("bintrees", fn -> Bintrees.bt_run(4, 14) end)
