# elixir benchmark harness -- mirrors bench/bench.l.
# Bench.run(name, work) auto-scales the repetition count (doubling until the run
# clears @min_ms), then prints one line matching the other harnesses:
#     <name> <lang> <reps> <ms> <checksum>
# work is a 0-arity fun returning a deterministic checksum. the column label
# comes from BENCH_LANG (default "elixir").
defmodule Bench do
  @min_ms 200.0
  @lang System.get_env("BENCH_LANG") || "elixir"

  def run(name, work), do: loop(name, work, 1)

  defp loop(name, work, reps) do
    t0 = System.monotonic_time(:nanosecond)
    chk = rep(work, reps, nil)
    ms = (System.monotonic_time(:nanosecond) - t0) / 1.0e6
    if ms >= @min_ms do
      IO.puts("#{name} #{@lang} #{reps} #{:erlang.float_to_binary(ms, decimals: 3)} #{chk}")
    else
      loop(name, work, reps * 2)
    end
  end

  defp rep(_work, 0, chk), do: chk
  defp rep(work, n, _), do: rep(work, n - 1, work.())
end
