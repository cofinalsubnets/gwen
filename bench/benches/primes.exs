Code.require_file("../lib/bench.exs", __DIR__)

defmodule Primes do
  def prime?(n), do: prime?(n, 2)
  defp prime?(n, d) when d * d > n, do: true
  defp prime?(n, d) do
    if rem(n, d) == 0, do: false, else: prime?(n, d + 1)
  end

  def count(lo, hi) do
    Enum.reduce(lo..(hi - 1), 0, fn n, c -> if prime?(n), do: c + 1, else: c end)
  end
end

Bench.run("primes", fn -> Primes.count(2, 30000) end)
