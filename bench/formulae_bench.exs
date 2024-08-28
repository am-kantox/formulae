defmodule Formulae.Bench do
  use Benchfella

  @short Enum.to_list(1..50)
  @long Enum.to_list(1..1_000)

  bench "generate modules (function)" do
    for i <- @short, do: Formulae.compile("a > #{i}", imports: :none)
  end

  bench "generate modules (guard)" do
    for i <- @short, do: Formulae.compile("b > #{i}", imports: :none, evaluator: :guard)
  end

  bench "check (function)", [f: Formulae.compile("ff > 500", imports: :none)] do
    for i <- @long, do: f.eval.(ff: i)
  end

  bench "check (anonymous function)", [f_eval: fn ff -> ff > 500 end] do
    for i <- @long, do: f_eval.(i)
  end

  bench "check (guard)", [f: Formulae.compile("fg > 500", imports: :none)] do
    for i <- @long, do: f.eval.(fg: i)
  end

  bench "check (guard in anonymous function)", [f_eval: fn fg when fg > 500 -> true; _ -> false end] do
    for i <- @long, do: f_eval.(i)
  end
end
