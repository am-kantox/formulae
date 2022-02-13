defmodule Formulae.Bench do
  use Benchfella

  @short Enum.to_list(1..50)
  @long Enum.to_list(1..1_000)

  bench "generate modules (function)" do
    for i <- @short, do: Formulae.compile("a > #{i}")
  end

  bench "generate modules (guard)" do
    for i <- @short, do: Formulae.compile("b > #{i}", eval: :guard)
  end

  bench "check (function)", [f: Formulae.compile("ff > 500")] do
    for i <- @long, do: f.eval.(ff: i)
  end

  bench "check (guard)", [f: Formulae.compile("fg > 500")] do
    for i <- @long, do: f.eval.(fg: i)
  end
end
