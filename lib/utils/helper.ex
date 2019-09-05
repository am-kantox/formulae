defmodule Combinators.H do
  @moduledoc false

  def var(i), do: {:"i_#{i}", [], Elixir}
  def idx(i), do: {:"idx_#{i}", [], Elixir}

  def for_clause(i, l) do
    {:<-, [],
     [
       {var(i), idx(i)},
       {{:., [], [{:__aliases__, [alias: false], [:Enum]}, :with_index]}, [], [l]}
     ]}
  end

  def stream_transform_clause(i, l, body) do
    {{{:., [], [{:__aliases__, [alias: false], [:Stream]}, :transform]}, [],
      [
        {{:., [], [{:__aliases__, [alias: false], [:Stream]}, :with_index]}, [], [l]},
        :ok,
        {:fn, [], [{:->, [], [[{var(i), idx(i)}, :ok], body]}]}
      ]}, :ok}
  end

  def and_many([]), do: [true]
  def and_many([clause]), do: clause

  def and_many([c1, c2 | clauses]) do
    Enum.reduce(clauses, {:&&, [context: Elixir, import: Kernel], [c1, c2]}, fn c, acc ->
      {:&&, [context: Elixir, import: Kernel], [acc, c]}
    end)
  end

  defmacro mapper(from, to, fun),
    do: quote(do: Enum.map(Range.new(unquote(from), unquote(to)), unquote(fun)))
end
