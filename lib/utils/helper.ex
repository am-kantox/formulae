defmodule Formulae.Combinators.H do
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

  def sink_permutation_clause(i),
    do: {:->, [], [[{{:_, [], Elixir}, {:^, [], [idx(i)]}}, :ok], {[], :ok}]}

  def sink_permutation_clauses(i, body) when i > 1 do
    Enum.reverse([
      {:->, [], [[{var(i), idx(i)}, :ok], body]}
      | Enum.map((i - 1)..1, &sink_permutation_clause/1)
    ])
  end

  def sink_permutation_clauses(1, body),
    do: [{:->, [], [[{var(1), idx(1)}, :ok], body]}]

  def stream_permutation_transform_clause(i, l, body) do
    clauses = sink_permutation_clauses(i, body)

    {{{:., [], [{:__aliases__, [alias: false], [:Stream]}, :transform]}, [],
      [
        {{:., [], [{:__aliases__, [alias: false], [:Stream]}, :with_index]}, [], [l]},
        :ok,
        {:fn, [], clauses}
      ]}, :ok}
  end

  def sink_combination_clause(i) when i > 1 do
    {:->, [],
     [
       [
         {:when, [],
          [
            {{:_, [], Elixir}, idx(i)},
            :ok,
            {:<=, [context: Elixir, import: Kernel], [idx(i), idx(i - 1)]}
          ]}
       ],
       {[], :ok}
     ]}
  end

  def sink_combination_clauses(i, body) when i > 1 do
    Enum.reverse([
      {:->, [], [[{var(i), idx(i)}, :ok], body]}
      | Enum.map(2..i, &sink_combination_clause/1)
    ])
  end

  def sink_combination_clauses(1, body) do
    [{:->, [], [[{var(1), idx(1)}, :ok], body]}]
  end

  def stream_combination_transform_clause(i, l, body) do
    clauses = sink_combination_clauses(i, body)

    {{{:., [], [{:__aliases__, [alias: false], [:Stream]}, :transform]}, [],
      [
        {{:., [], [{:__aliases__, [alias: false], [:Stream]}, :with_index]}, [], [l]},
        :ok,
        {:fn, [], clauses}
      ]}, :ok}
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
