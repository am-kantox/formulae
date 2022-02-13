defmodule Formulae.Sigils do
  @moduledoc ~S"""
  Handles the sigil `~F` for `Formulae`.

  It returns a compiled `Formulae` instance.

  ## Examples

      iex> import Formulae.Sigils
      iex> ~F[x / y > 42]
      %Formulae{
        ast: Code.string_to_quoted!("x / y > 42"),
        eval: &:"Elixir.Formulae.x ÷ y > 42".eval/1,
        formula: "x / y > 42",
        guard: nil,
        module: :"Elixir.Formulae.x ÷ y > 42",
        variables: [:x, :y]
      }
      iex> ~F[x == y]g
      %Formulae{
        ast: Code.string_to_quoted!("x == y"),
        eval: &:"Elixir.Formulae.x == y".eval/1,
        formula: "x == y",
        guard: {
          :defguard,
          [{:generated, true}, {:context, Formulae}, {:import, Kernel}],
          [{:when, [generated: true], [{:guard, [generated: true], [{:x, [], nil}, {:y, [], nil}]}, {:==, [line: 1], [{:x, [line: 1], nil}, {:y, [line: 1], nil}]}]}]
        },
        module: :"Elixir.Formulae.x == y",
        variables: [:x, :y]
      }
  """

  @doc false
  defmacro sigil_F({:<<>>, _, [binary]}, modifiers) when is_binary(binary) do
    eval =
      case modifiers do
        [?g] -> :guard
        [] -> :function
      end

    quote(
      generated: true,
      location: :keep,
      do: Formulae.compile(unquote(binary), eval: unquote(eval))
    )
  end
end
