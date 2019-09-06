defmodule Formulae.Combinators do
  @moduledoc """
  Functions to calculate all the combinations, permutations and
  repeated permutations of the list given.

  _Examples_

      iex> Formulae.Combinators.combinations(~w|a b c d|a, 2)
      [[:a, :b], [:a, :c], [:a, :d], [:b, :c], [:b, :d], [:c, :d]]
      iex> Formulae.Combinators.permutations(~w|a b c d|a, 2)
      [[:a, :b], [:a, :c], [:a, :d], [:b, :a], [:b, :c], [:b, :d],
       [:c, :a], [:c, :b], [:c, :d], [:d, :a], [:d, :b], [:d, :c]]
      iex> Formulae.Combinators.repeated_permutations(~w|a b c d|a, 2)
      [[:a, :a], [:a, :b], [:a, :c], [:a, :d], [:b, :a], [:b, :b],
       [:b, :c], [:b, :d], [:c, :a], [:c, :b], [:c, :c], [:c, :d],
       [:d, :a], [:d, :b], [:d, :c], [:d, :d]]

  **NB** this functions should not be used for relatively big `n` because
  they perform greedy evaluation. See to `Formulae.Combinators.Stream`
  for lazy analogues returning streams.

  """

  import Formulae.Combinators.H

  @doc "Calculates all combinations of the list, given as the first parameter"
  @spec combinations(list :: list(), count :: non_neg_integer()) :: [list()]

  defmacro combinations(_, 0), do: []
  defmacro combinations(l, 1), do: quote(do: Enum.map(unquote(l), &[&1]))

  defmacro combinations(l, n) do
    guards = mapper(n, 2, &{:>, [context: Elixir, import: Kernel], [idx(&1), idx(&1 - 1)]})

    {:for, [],
     Enum.reverse([
       [do: mapper(1, n, &var/1)]
       | guards ++ mapper(n, 1, &for_clause(&1, l))
     ])}
  end

  @doc "Calculates all permutations of the list, given as the first parameter"
  @spec permutations(list :: list(), count :: non_neg_integer()) :: [list()]

  defmacro permutations(_, 0), do: []
  defmacro permutations(l, 1), do: quote(do: Enum.map(unquote(l), &[&1]))

  defmacro permutations(l, n) do
    guards =
      n
      |> mapper(1, &idx/1)
      |> combinations(2)
      |> Enum.map(&{:!=, [context: Elixir, import: Kernel], &1})

    {:for, [],
     Enum.reverse([
       [do: mapper(1, n, &var/1)]
       | guards ++ mapper(n, 1, &for_clause(&1, l))
     ])}
  end

  @doc "Calculates all repeated permutations of the list, given as the first parameter"
  @spec repeated_permutations(list :: list(), count :: non_neg_integer()) :: [list()]

  defmacro repeated_permutations(_, 0), do: []
  defmacro repeated_permutations(l, 1), do: quote(do: Enum.map(unquote(l), &[&1]))

  defmacro repeated_permutations(l, n) do
    clause = fn i -> {:<-, [], [var(i), l]} end
    {:for, [], Enum.reverse([[do: mapper(1, n, &var/1)] | mapper(n, 1, clause)])}
  end

  # def combinations([], _), do: []
  # def combinations(list, n) when length(list) < n, do: []
  # def combinations(list, n) when length(list) == n, do: [list]
  # def combinations(list, 1), do: Enum.map(list, &[&1])
  # def combinations(list, n), do: m_combinations(list, n)

  # def permutations([], _), do: []
  # def permutations(list, n) when length(list) < n, do: []
  # def permutations(list, 1), do: Enum.map(list, &[&1])
  # def permutations(list, n), do: m_permutations(list, n)

  # def repeated_permutations([], _), do: []
  # def repeated_permutations(list, 1), do: Enum.map(list, &[&1])
  # def repeated_permutations(list, n), do: m_repeated_permutations(list, n)
end
