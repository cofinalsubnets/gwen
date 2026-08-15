Code.require_file("../lib/bench.exs", __DIR__)

# build the list 1..100000 then sum it.
Bench.run("sum", fn -> Enum.to_list(1..100000) |> Enum.sum() end)
