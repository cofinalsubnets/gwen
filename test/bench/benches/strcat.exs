Code.require_file("../lib/bench.exs", __DIR__)

# build an N-char string by repeated single-char concatenation, then hash it.
defmodule Strcat do
  @hmod 1000000007
  @n 4000

  def work do
    s = build(0, "")
    hash(s, 0)
  end

  defp build(i, s) when i >= @n, do: s
  defp build(i, s), do: build(i + 1, s <> <<48 + rem(i, 10)>>)

  defp hash(<<>>, h), do: h
  defp hash(<<c, rest::binary>>, h), do: hash(rest, rem(h * 31 + c, @hmod))
end

Bench.run("strcat", fn -> Strcat.work() end)
