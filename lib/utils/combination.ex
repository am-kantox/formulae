defmodule Combinations do
  @moduledoc """
  Helper to calculate all the combinations of the list given.

  The maximal allowed number of combinations is limited to 12.
  """
  Enum.each(1..12, fn n ->
    @n n
    @doc "Calculates the combinations of `#{n}` for the list given"
    def unquote(:"do_#{@n}")(list) do
      len = length(list)

      Permutations
      |> apply(:"do_#{len}", [list])
      |> Enum.filter(&filter(&1, len))
    end

    defp filter({var, {var, _, _}} = p, len), do: filter([p], len)

    defp filter(permutation, len) when is_list(permutation) do
      permutation
      |> Keyword.keys()
      |> Enum.uniq()
      |> length()
      |> Kernel.==(len)
    end
  end)
end
