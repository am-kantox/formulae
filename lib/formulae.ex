defmodule Formulae do
  @comparison [:<, :>, :=] # Damn it, José! :≠
  # @booleans   [:&, :|]

  @doc ~S"""
  Produces the normalized representation of formula. If the _rho_ is
  an instance of [`Integer`](http://elixir-lang.org/docs/stable/elixir/Integer.html#content)
  or [`Float`](http://elixir-lang.org/docs/stable/elixir/Float.html#content),
  it’s left intact, otherwise it’s moved to the left side with negation.

  @todo Try to `eval` _rho_ before guards; premature optimization

  ## Examples

      iex> Formulae.unit("3 > 2")
      {:>, [], [3, 2]}

      iex> Formulae.unit("3 - a > 2")
      {:>, [], [{:-, [line: 1], [3, {:a, [line: 1], nil}]}, 2]}

      iex> Formulae.unit("3 > a + 2")
      {:>, [],
       [{:-, [context: Formulae, import: Kernel],
         [3, {:+, [line: 1], [{:a, [line: 1], nil}, 2]}]}, 0]}

      iex> Formulae.unit("3 >= a + 2")
      ** (Formulae.Syntax) Formula [3 >= a + 2] syntax is incorrect (operand): “>=”.

      iex> Formulae.unit("3 a > a + 2")
      ** (Formulae.Syntax) Formula [3 a > a + 2] syntax is incorrect (parsing): syntax error before: “a”.
  """
  def unit(input, env \\ []) when is_binary(input) do
    case Code.string_to_quoted(input) do
      {:ok, {:>, _, [lh, rh]}} when is_integer(rh) or is_float(rh) -> {:>, env, [lh, rh]}
      {:ok, {:>, _, [lh, rh]}} -> {:>, env, [(quote do: unquote(lh) - unquote(rh)), 0]}

      {:ok, {:<, _, [lh, rh]}} when is_integer(rh) or is_float(rh) -> {:<, env, [lh, rh]}
      {:ok, {:<, _, [lh, rh]}} -> {:<, env, [(quote do: unquote(lh) - unquote(rh)), 0]}

      {:ok, {:=, _, [lh, rh]}} -> {:==, lh - rh}

      {:ok, {op, _, _}} -> raise(Formulae.Syntax, formula: input, error: {:operand, double_quote(op)})
      {:error, {1, message, op}} -> raise(Formulae.Syntax, formula: input, error: {:parsing,  message <> double_quote(op)})
      other -> raise(Formulae.Syntax, formula: input, error: {:unknown, inspect(other)})
    end
  end

  @doc ~S"""
  Evaluates normalized representation of formula.

  ## Examples

      iex> Formulae.evaluate(Formulae.unit("3 > 2"))
      true
      iex> Formulae.evaluate(Formulae.unit("3 < 2"))
      false
      iex> Formulae.evaluate(Formulae.unit("a < 2"), [a: 1])
      true
      iex> Formulae.evaluate(Formulae.unit("a > 2"), [a: 1])
      false
      iex> Formulae.evaluate(Formulae.unit("a < 2"), [])
      ** (Formulae.Runner) Formula failed to run (compile): undefined function a/0.
  """
  def evaluate(quoted, binding \\ [], opts \\ []) do
    try do
      case Code.eval_quoted(quoted, binding, opts) do
        {false, ^binding} -> false
        {true,  ^binding} -> true
        other             -> raise(Formulae.Runner, formula: quoted, error: {:weird, inspect(other)})
      end
    rescue
      e in CompileError -> raise(Formulae.Runner, formula: quoted, error: {:compile, e.description})
    end
  end

  defp double_quote(string) when is_binary(string), do: "“" <> string <> "”"
  defp double_quote(string), do: double_quote(to_string(string))
end
