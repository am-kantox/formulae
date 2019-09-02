defmodule NonRepeatedPermutations do
  @moduledoc """
  Helper to calculate all the non repeated permutations of the list given.

  The maximal allowed number of non repeated permutations is limited to 12.
  """
  @doc "Calculates all non repeated permutations of an empty list, returning an empty list"
  @spec do_0 :: []
  def do_0(), do: []

  @doc "Calculates all non repeated permutations of an empty list, returning an empty list"
  @spec do_0([]) :: []
  def do_0([]), do: []

  @doc """
  Calculates all non repeated permutations of a list having a single keyword element,
  returning the keyword wrapped into a list.
  """
  @spec do_1(kw :: keyword()) :: [keyword()]
  def do_1(kw), do: [kw]

  Enum.each(2..12, fn n ->
    @n n
    @doc """
    Calculates all non repeated permutations of a list of length `#{n}`.
    """
    @spec unquote(:"do_#{@n}")(list()) :: [list()]
    def unquote(:"do_#{@n}")(list) do
      len = length(list)

      Permutations
      |> apply(:"do_#{len}", [list])
      |> Enum.filter(&filter(&1, len))
    end

    defp filter(permutation, len) when is_list(permutation) do
      permutation
      |> Keyword.keys()
      |> Enum.uniq()
      |> length()
      |> Kernel.==(len)
    end
  end)
end
