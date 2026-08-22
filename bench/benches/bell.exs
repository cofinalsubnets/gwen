Code.require_file("../lib/bench.exs", __DIR__)

# Bell numbers in base 36 (see bench/benches/bell.l). Fresh memo maps per rep,
# threaded functionally; checksum = total characters across all rendered lines.
defmodule Bell do
  @digits "0123456789abcdefghijklmnopqrstuvwxyz"
  @base 36

  defp fact(n, f) do
    case Map.fetch(f, n) do
      {:ok, v} -> {v, f}
      :error ->
        v = fact_calc(n, 1)
        {v, Map.put(f, n, v)}
    end
  end
  defp fact_calc(m, x) when m <= 1, do: x
  defp fact_calc(m, x), do: fact_calc(m - 1, x * m)

  defp choose(n, k, f) do
    {a, f} = fact(n, f)
    {b, f} = fact(k, f)
    {c, f} = fact(n - k, f)
    {div(a, b * c), f}
  end

  defp bell(n, b, f) do
    case Map.fetch(b, n) do
      {:ok, v} -> {v, b, f}
      :error ->
        {r, b, f} =
          if n < 2 do
            {1, b, f}
          else
            Enum.reduce(0..(n - 1), {0, b, f}, fn k, {acc, b, f} ->
              {ck, f} = choose(n - 1, k, f)
              {bk, b, f} = bell(k, b, f)
              {acc + ck * bk, b, f}
            end)
          end
        {r, Map.put(b, n, r), f}
    end
  end

  defp show(n), do: show(n, "")
  defp show(0, s), do: s
  defp show(n, s), do: show(div(n, @base), String.at(@digits, rem(n, @base)) <> s)

  def run(limit), do: loop(0, 0, %{}, %{}, limit)
  defp loop(i, total, b, f, limit) do
    {v, b, f} = bell(i, b, f)
    len = String.length(show(v))
    if len > limit, do: total, else: loop(i + 1, total + len, b, f, limit)
  end
end

Bench.run("bell", fn -> Bell.run(280) end)
