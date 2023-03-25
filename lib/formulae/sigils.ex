defmodule Formulae.Sigils do
  @moduledoc ~S"""
  Handles the sigil `~F` for `Formulae`.

  It returns a compiled `Formulae` instance.

  ## Examples

      iex> import Formulae.Sigils
      iex> ~F[x / y > 100]
      %Formulae{
        ast: Code.string_to_quoted!("x / y > 100"),
        eval: &:"Elixir.Formulae.x ÷ y > 100".eval/1,
        formula: "x / y > 100",
        guard: nil,
        module: :"Elixir.Formulae.x ÷ y > 100",
        remote_calls: [],
        variables: [:x, :y]
      }
      iex> ~F[xz == yz]ga
      %Formulae{
        ast: Code.string_to_quoted!("xz == yz"),
        eval: &:"Elixir.Formulae.xz == yz".eval/1,
        formula: "xz == yz",
        guard: {
          :defguard,
          Keyword.update!(elem(quote(generated: true, do: defguard(bar)), 1), :context, fn _ -> Formulae end),
          [{:when, [generated: true], [{:guard, [generated: true], [{:xz, [], nil}, {:yz, [], nil}]}, {:==, [line: 1], [{:xz, [line: 1], nil}, {:yz, [line: 1], nil}]}]}]
        },
        module: :"Elixir.Formulae.xz == yz",
        remote_calls: :all,
        variables: [:xz, :yz]
      }
  """

  @doc false
  defmacro sigil_F({:<<>>, _, [binary]}, modifiers) when is_binary(binary) do
    eval = if ?g in modifiers, do: :guard, else: :function
    remote_calls = if ?a in modifiers, do: :all, else: :none

    quote generated: true, location: :keep do
      Formulae.compile(unquote(binary), eval: unquote(eval), remote_calls: unquote(remote_calls))
    end
  end
end
