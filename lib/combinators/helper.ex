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

  ### combinations

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

  ### permutations

  def sink_permutation_clause(i),
    do: {:->, [], [[{{:_, [], Elixir}, {:^, [], [idx(i)]}}, :ok], {[], :ok}]}

  def sink_permutation_clauses(i, body) when i > 1 do
    Enum.reverse([
      {:->, [], [[{var(i), idx(i)}, :ok], body]}
      | Enum.map((i - 1)..1//-1, &sink_permutation_clause/1)
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

  ### repeated permutations

  def stream_transform_clause(i, l, body) do
    {{{:., [], [{:__aliases__, [alias: false], [:Stream]}, :transform]}, [],
      [
        {{:., [], [{:__aliases__, [alias: false], [:Stream]}, :with_index]}, [], [l]},
        :ok,
        {:fn, [], [{:->, [], [[{var(i), idx(i)}, :ok], body]}]}
      ]}, :ok}
  end

  defmacro mapper(from, to, fun) when from <= to,
    do: quote(do: Enum.map(unquote(from)..unquote(to), unquote(fun)))

  defmacro mapper(from, to, fun),
    do: quote(do: Enum.map(unquote(from)..unquote(to)//-1, unquote(fun)))
end
