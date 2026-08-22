Code.require_file("../lib/bench.exs", __DIR__)

# fixed string built once; the timed work is a linear rolling-hash scan.
defmodule Strscan do
  @hmod 1000000007

  def data, do: (for i <- 0..19999, into: <<>>, do: <<32 + rem(7 * i, 95)>>)

  def hash(<<>>, h), do: h
  def hash(<<c, rest::binary>>, h), do: hash(rest, rem(h * 31 + c, @hmod))
end

data = Strscan.data()
Bench.run("strscan", fn -> Strscan.hash(data, 0) end)
