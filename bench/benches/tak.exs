Code.require_file("../lib/bench.exs", __DIR__)

defmodule Tak do
  def tak(x, y, z) do
    if y < x,
      do: tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y)),
      else: z
  end
end

Bench.run("tak", fn -> Tak.tak(22, 12, 6) end)
