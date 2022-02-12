defmodule Formulae.Test do
  use ExUnit.Case
  require Formulae.Combinators

  doctest Formulae
  doctest Formulae.Combinators
  doctest Formulae.Combinators.Stream

  test "formulae with slashes (division)" do
    f = Formulae.compile("a / 2")
    assert 1.0 == f.eval.(a: 2)
  end

  test "dynamic number of combinations" do
    number = 2
    assert [[:a, :b], [:a, :c], [:b, :c]] == Formulae.combinations(~w|a b c|a, number)
  end

  test "guard" do
    f = Formulae.compile("rem(a, 2) == 0")

    assert {:defguard, _,
            [
              {:when, _,
               [{:guard, _, [{:a, _, nil}]}, {:==, _, [{:rem, _, [{:a, _, nil}, 2]}, 0]}]}
            ]} = f.guard

    alias :"Elixir.Formulae.rem(a, 2) == 0", as: AG
    require AG

    1..10
    |> Enum.chunk_every(2)
    |> Enum.each(fn [odd, even] ->
      assert match?(a when AG.guard(a), even)
      refute match?(a when AG.guard(a), odd)
    end)
  end
end
