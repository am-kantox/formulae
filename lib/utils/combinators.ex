defmodule Formulae.Combinators do
  @moduledoc """
  Helper to calculate all the combinations of the list given.

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

  """
  @max_permutations Application.get_env(:formulae, :max_permutations, 20)

  Enum.each(2..@max_permutations, fn n ->
    @n n
    # combination
    # for {i1, i1_idx} <- Enum.with_index(l),
    #     {i2, i2_idx} <- Enum.with_index(l),
    #     {i3, i3_idx} <- Enum.with_index(l),
    #     i2_idx > i1_idx,
    #     i3_idx > i2_idx,
    #     do: [i1, i2, i3]
    defmacrop unquote(:"combinations_#{@n}")(l) do
      var = fn i -> {:"i#{i}", [], Elixir} end
      idx = fn i -> {:"i#{i}_idx", [], Elixir} end

      clause = fn i ->
        {:<-, [],
         [
           {var.(i), idx.(i)},
           {{:., [], [{:__aliases__, [alias: false], [:Enum]}, :with_index]}, [], [l]}
         ]}
      end

      guard = fn i -> {:>, [context: Elixir, import: Kernel], [idx.(i), idx.(i - 1)]} end

      {:for, [],
       Enum.reverse([
         [do: Enum.map(1..@n, var)] | Enum.map(@n..2, guard) ++ Enum.map(@n..1, clause)
       ])}
    end

    # # permutation
    # for {i1, i1_idx} <- Enum.with_index(l),
    #     {i2, i2_idx} <- Enum.with_index(l),
    #     {i3, i3_idx} <- Enum.with_index(l),
    #     i1_idx != i2_idx,
    #     i1_idx != i3_idx,
    #     i2_idx != i3_idx,
    #     do: [i1, i2, i3]

    defmacrop unquote(:"permutations_#{@n}")(l) do
      var = fn i -> {:"i#{i}", [], nil} end
      idx = fn i -> {:"i#{i}_idx", [], Elixir} end

      clause = fn i ->
        {:<-, [],
         [
           {var.(i), idx.(i)},
           {{:., [], [{:__aliases__, [alias: false], [:Enum]}, :with_index]}, [], [l]}
         ]}
      end

      guards =
        @n..1
        |> Enum.map(idx)
        |> combinations_2()
        |> Enum.map(&{:!=, [context: Elixir, import: Kernel], &1})

      {:for, [],
       Enum.reverse([
         [do: Enum.map(1..@n, var)] | guards ++ Enum.map(@n..1, clause)
       ])}
    end

    # # repeated permutation
    # for i1 <- l,
    #     i2 <- l,
    #     i3 <- l,
    #     do: [i1, i2, i3]
    defmacrop unquote(:"repeated_permutations_#{@n}")(l) do
      var = fn i -> {:"i#{i}", [], nil} end
      clause = fn i -> {:<-, [], [var.(i), l]} end
      {:for, [], Enum.reverse([[do: Enum.map(1..@n, var)] | Enum.map(@n..1, clause)])}
    end
  end)

  @doc "Calculates all combinations of the list, given as the first parameter"
  @spec combinations(list :: list(), count :: non_neg_integer()) :: [list()]
  def combinations([], _), do: []
  def combinations(list, n) when length(list) < n, do: []
  def combinations(list, n) when length(list) == n, do: [list]
  def combinations(list, 1), do: Enum.map(list, &[&1])

  Enum.each(2..@max_permutations, fn n ->
    @n n
    def combinations(list, unquote(@n)),
      do: Enum.map(unquote(:"combinations_#{@n}")(list), & &1)
  end)

  @doc "Calculates all permutations of the list, given as the first parameter"
  @spec permutations(list :: list(), count :: non_neg_integer()) :: [list()]
  def permutations([], _), do: []
  def permutations(list, n) when length(list) < n, do: []
  def permutations(list, 1), do: Enum.map(list, &[&1])

  Enum.each(2..@max_permutations, fn n ->
    @n n
    def permutations(list, unquote(@n)),
      do: Enum.map(unquote(:"permutations_#{@n}")(list), & &1)
  end)

  @doc "Calculates all repeated permutations of the list, given as the first parameter"
  @spec repeated_permutations(list :: list(), count :: non_neg_integer()) :: [list()]
  def repeated_permutations([], _), do: []
  def repeated_permutations(list, 1), do: Enum.map(list, &[&1])

  Enum.each(2..@max_permutations, fn n ->
    @n n
    def repeated_permutations(list, unquote(@n)),
      do: Enum.map(unquote(:"repeated_permutations_#{@n}")(list), & &1)
  end)
end
