defmodule Permutations do
  @moduledoc """
  Helper to calculate all the permutations of the list given.

  The maximal allowed number of permutations is limited to 12.
  """
  Enum.each(2..12, fn n ->
    @n n
    defmacrop unquote(:"do_#{@n}!")(l) do
      clause = fn i -> {:<-, [], [{:"i#{i}", [], Elixir}, l]} end
      return = Enum.map(1..@n, fn i -> {:"i#{i}", [], Elixir} end)

      Enum.reduce(1..@n, return, fn i, acc ->
        {:for, [], [clause.(i), [do: acc]]}
      end)
    end
  end)

  @doc "Calculates all permutations of an empty list, returning an empty list"
  @spec do_0 :: []
  def do_0(), do: []

  @doc "Calculates all permutations of an empty list, returning an empty list"
  @spec do_0([]) :: []
  def do_0([]), do: []

  @doc """
  Calculates all permutations of a list having a single keyword element,
  returning the keyword wrapped into a list.
  """
  @spec do_1(kw :: keyword()) :: [keyword()]
  def do_1(kw), do: [kw]

  Enum.each(2..12, fn n ->
    @n n
    @doc "Calculates the permutations of `#{n}` for the list given"
    def unquote(:"do_#{@n}")(list),
      do: Enum.flat_map(unquote(:"do_#{@n}!")(list), & &1)
  end)
end
