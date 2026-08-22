Code.require_file("../lib/bench.exs", __DIR__)

# map/filter/fold list pipeline: sum the squares of the odd numbers in [0, N).
# the range + intermediate lists are built INSIDE the timed work. checksum = 4891344686.
Bench.run("deforest", fn ->
  0..19999
  |> Enum.filter(&(rem(&1, 2) == 1))
  |> Enum.map(&(rem(&1 * &1, 1000003)))
  |> Enum.sum()
end)
