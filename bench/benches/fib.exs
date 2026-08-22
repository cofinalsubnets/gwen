Code.require_file("../lib/bench.exs", __DIR__)

defmodule Fib do
  def fib(n) when n < 2, do: n
  def fib(n), do: fib(n - 1) + fib(n - 2)
end

Bench.run("fib", fn -> Fib.fib(30) end)
