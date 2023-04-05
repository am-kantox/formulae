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
        variables: [:x, :y],
        options: [defaults: [], alias: nil, evaluator: :function, imports: []]
      }
      iex> ~F[x == y]ga
      %Formulae{
        ast: Code.string_to_quoted!("x == y"),
        eval: &:"Elixir.Formulae.x == y".eval/1,
        formula: "x == y",
        guard: {
          :defguard,
          Keyword.update!(elem(quote(generated: true, do: defguard(bar)), 1), :context, fn _ -> Formulae end),
          [{:when, [generated: true], [{:guard, [generated: true], [{:x, [], nil}, {:y, [], nil}]}, {:==, [line: 1], [{:x, [line: 1], nil}, {:y, [line: 1], nil}]}]}]
        },
        module: :"Elixir.Formulae.x == y",
        variables: [:x, :y],
        options: [defaults: [], alias: nil, evaluator: :guard, imports: [:...]]
      }
  """

  @doc """
  Convenience sigil to declare `Formulae`. Maps to `Formulae.compile/2` with `imports: :none`
    (or with `imports: :all` if the `a` modifier is given.)
  """
  defmacro sigil_F({:<<>>, _, [binary]}, modifiers) when is_binary(binary) do
    eval = if ?g in modifiers, do: :guard, else: :function
    imports = if ?a in modifiers, do: :all, else: :none

    quote generated: true, location: :keep do
      Formulae.compile(unquote(binary), evaluator: unquote(eval), imports: unquote(imports))
    end
  end
end
