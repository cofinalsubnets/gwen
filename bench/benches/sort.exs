Code.require_file("../lib/bench.exs", __DIR__)

# sort N pseudo-random ints (MINSTD LCG), order-dependent rolling-hash checksum.
defmodule Sort do
  def gen(0, _x, acc), do: acc
  def gen(n, x, acc) do
    nx = rem(16807 * x, 2147483647)
    gen(n - 1, nx, [nx | acc])
  end
  def hsh(list), do: Enum.reduce(list, 0, fn v, h -> rem(h * 31 + v, 1000000007) end)
end

Bench.run("sort", fn -> Sort.hsh(Enum.sort(Sort.gen(5000, 1, []))) end)
