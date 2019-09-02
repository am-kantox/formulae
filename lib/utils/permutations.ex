defmodule Permutations do
  @moduledoc """
  Helper to calculate all the permutations of the list given.

  The maximal allowed number of permutations is limited to 12.
  """
  Enum.each(1..12, fn n ->
    @n n
    defmacrop unquote(:"do_#{@n}!")(l) do
      clause = fn i -> {:<-, [], [{:"i#{i}", [], Elixir}, l]} end
      return = Enum.map(1..@n, fn i -> {:"i#{i}", [], Elixir} end)

      Enum.reduce(1..@n, return, fn i, acc ->
        {:for, [], [clause.(i), [do: acc]]}
      end)
    end
  end)

  Enum.each(1..12, fn n ->
    @n n
    @doc "Calculates the permutations of `#{n}` for the list given"
    def unquote(:"do_#{@n}")(list),
      do: Enum.flat_map(unquote(:"do_#{@n}!")(list), & &1)
  end)
end
