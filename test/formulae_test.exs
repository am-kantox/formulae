defmodule Formulae.Test do
  use ExUnit.Case
  require Formulae.Combinators

  doctest Formulae
  doctest Formulae.Combinators
  doctest Formulae.Combinators.Stream

  test "dynamic number of combinations" do
    number = 2
    assert [[:a, :b], [:a, :c], [:b, :c]] == Formulae.combinations(~w|a b c|a, number)
  end
end
