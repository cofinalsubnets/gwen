Code.require_file("../lib/bench.exs", __DIR__)
twice = fn f -> fn x -> f.(f.(x)) end end
adder = fn i -> fn x -> x + i end end
Bench.run("closure", fn ->
  Enum.reduce(0..99999, 0, fn i, acc -> rem(acc * 31 + twice.(adder.(i)).(i), 1000000007) end)
end)
