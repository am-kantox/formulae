defmodule Formulae.Combinators.Stream do
  @moduledoc """
  Helper to calculate all the combinations / permutations of the enumerable given.

  Similar to `Formulae.Combinators` but returns a stream.

  _Examples_

      iex> ~w|a b c d|a
      ...> |> Formulae.Combinators.Stream.combinations(2)
      ...> |> elem(0)
      ...> |> Enum.to_list()
      [[:a, :b], [:a, :c], [:a, :d], [:b, :c], [:b, :d], [:c, :d]]
      iex> ~w|a b c d|a
      ...> |> Formulae.Combinators.Stream.permutations(2)
      ...> |> elem(0)
      ...> |> Enum.to_list()
      [[:a, :b], [:a, :c], [:a, :d], [:b, :a], [:b, :c], [:b, :d],
       [:c, :a], [:c, :b], [:c, :d], [:d, :a], [:d, :b], [:d, :c]]
      iex> ~w|a b c d|a
      ...> |> Formulae.Combinators.Stream.repeated_permutations(2)
      ...> |> elem(0)
      ...> |> Enum.to_list()
      [[:a, :a], [:a, :b], [:a, :c], [:a, :d], [:b, :a], [:b, :b],
       [:b, :c], [:b, :d], [:c, :a], [:c, :b], [:c, :c], [:c, :d],
       [:d, :a], [:d, :b], [:d, :c], [:d, :d]]

      iex> (for c <- ?a..?z, do: <<c>>)
      ...> |> Formulae.Combinators.Stream.combinations(12)
      ...> |> elem(0)
      ...> |> Stream.take_every(26)
      ...> |> Enum.take(2)
      [["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"],
       ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "l", "x"]]
      iex> l = for c <- ?a..?z, do: <<c>>
      iex> with {stream, :ok} <- Formulae.Combinators.Stream.permutations(l, 12),
      ...>   do: stream |> Stream.take_every(26) |> Enum.take(2)
      [["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"],
       ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "l", "w"]]
      iex> (for c <- ?a..?z, do: <<c>>)
      ...> |> Formulae.Combinators.Stream.repeated_permutations(12)
      ...> |> elem(0)
      ...> |> Enum.take(5)
      [["a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a"],
       ["a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "b"],
       ["a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "c"],
       ["a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "d"],
       ["a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "e"]]
  """
  import Formulae.Combinators.H

  @doc """
  Lazily calculates combinations of the list, given as the first parameter,
  returns `{Stream.t(), :ok}` tuple.

  See `Formulae.Combinators.combinations/2` for greedy version.
  """
  @spec combinations(list :: list(), count :: non_neg_integer()) :: {Stream.t(), :ok}

  defmacro combinations(l, n) do
    Enum.reduce(n..1, {[mapper(1, n, &var/1)], :ok}, fn i, body ->
      stream_combination_transform_clause(i, l, body)
    end)
  end

  @doc """
  Lazily calculates permutations of the list, given as the first parameter,
  returns `{Stream.t(), :ok}` tuple.

  See `Formulae.Combinators.permutations/2` for greedy version.
  """
  @spec permutations(list :: list(), count :: non_neg_integer()) :: {Stream.t(), :ok}

  defmacro permutations(l, n) do
    Enum.reduce(n..1, {[mapper(1, n, &var/1)], :ok}, fn i, body ->
      stream_permutation_transform_clause(i, l, body)
    end)
  end

  @doc """
  Lazily calculates repeated permutations of the list, given as the first parameter,
  returns `{Stream.t(), :ok}` tuple.

  See `Formulae.Combinators.repeated_permutations/2` for greedy version.
  """
  @spec repeated_permutations(list :: list(), count :: non_neg_integer()) :: {Stream.t(), :ok}

  defmacro repeated_permutations(l, n) do
    Enum.reduce(n..1, {[mapper(1, n, &var/1)], :ok}, fn i, body ->
      stream_transform_clause(i, l, body)
    end)
  end
end
