defmodule Formulae.Test do
  use ExUnit.Case
  doctest Formulae
  doctest Formulae.Combinators
  doctest Formulae.Combinators.Stream

  test "flow" do
    list = ~w[a b c d e]a
    count = 5
    sources = List.duplicate(list, count)

    stream =
      Stream.transform(Stream.with_index(list), :ok, fn {i1, idx1}, :ok ->
        {Stream.transform(Stream.with_index(list), :ok, fn
           {_, idx2}, :ok when idx2 <= idx1 ->
             {[], :ok}

           {i2, idx2}, :ok ->
             {Stream.transform(Stream.with_index(list), :ok, fn
                {_, idx3}, :ok when idx3 <= idx2 ->
                  {[], :ok}

                {i3, idx3}, :ok ->
                  {Stream.transform(Stream.with_index(list), :ok, fn
                     {_, idx4}, :ok when idx4 <= idx3 ->
                       {[], :ok}

                     {i4, _idx4}, :ok ->
                       {[[i1, i2, i3, i4]], :ok}
                   end), :ok}
              end), :ok}
         end), :ok}
      end)

    #     Flow.from_enumerable()
    # |> Flow.flat_map(&String.split(&1, " "))
    # |> Flow.partition()
    # |> Flow.reduce(fn -> %{} end, fn word, acc ->
    #   Map.update(acc, word, 1, & &1 + 1)
    # end)
    # |> Enum.to_list()
  end
end
