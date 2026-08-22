Code.require_file("../lib/bench.exs", __DIR__)

# binary-trees allocation/GC stress (see bench/benches/tree.l). checksum = 2^D-1.
defmodule Tree do
  def mk(0), do: nil
  def mk(d), do: {mk(d - 1), mk(d - 1)}
  def ck(nil), do: 0
  def ck({l, r}), do: 1 + ck(l) + ck(r)
end

Bench.run("tree", fn -> Tree.ck(Tree.mk(16)) end)
