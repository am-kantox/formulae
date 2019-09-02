defmodule Formulae do
  @moduledoc ~S"""
  A set of functions to deal with analytical formulae.
  """

  @type t :: %{__struct__: atom(), itself: binary(), ast: nil | tuple(), module: atom()}
  defstruct itself: nil, ast: nil, module: nil, variables: []

  @doc "Returns whether the formula was already compiled into module"
  @spec compiled?(Formulae.t()) :: boolean()
  def compiled?(%Formulae{module: nil}), do: false
  def compiled?(%Formulae{module: _}), do: true

  @doc "Compiles the formula into module"
  @spec compile(Formulae.t() | binary()) :: boolean()
  def compile(input) when is_binary(input) do
    Formulae
    |> Module.concat(input)
    |> Code.ensure_loaded()
    |> maybe_create_module(input)
  end

  def compile(%Formulae{itself: input} = f), do: compile(input)

  @spec maybe_create_module({:module, atom()} | {:error, any()}, input :: binary()) ::
          Formulae.t()
  defp maybe_create_module({:module, module}, input),
    do: %Formulae{itself: input, module: module, ast: module.ast(), variables: module.variables()}

  defp maybe_create_module({:error, _}, input) do
    with {:ok, macro} <- Code.string_to_quoted(input),
         {^macro, variables} =
           Macro.prewalk(macro, [], fn
             {var, _, nil} = v, acc -> {v, [var | acc]}
             v, acc -> {v, acc}
           end),
         len = length(variables),
         macro = Macro.escape(macro),
         #! FIXME add all permutations
         vars = Enum.map(variables, &{&1, Macro.var(&1, nil)}),
         IO.inspect(vars, label: "VARS"),
         # vars = apply(Combinations, :"do_#{len}", [vars]),
         ast = [
           quote generated: true do
             defmacrop do_eval(unquote(vars)), do: unquote(macro)
             # defmacrop do_eval(other), do: IO.inspect({unquote(vars), other}, label: "EVAL")

             def ast, do: unquote(macro)
             def variables, do: unquote(variables)
             def eval(unquote(vars)), do: do_eval(unquote(vars))
           end
           #  | Enum.map(vars, fn var ->
           #      IO.inspect(var)
           #      quote generated: true do
           #        def eval(unquote(var)), do: do_eval(unquote(var))
           #      end
           #    end)
         ],
         {:module, module, _, _} <- Module.create(Module.concat(Formulae, input), ast, __ENV__),
         do: %Formulae{itself: input, ast: macro, module: module, variables: variables}
  end

  ##############################################################################

  @doc ~S"""
  Revalidates the formula with bindings given.

  ## Examples

      iex> "a > 5" |> Formulae.check([a: 6])
      true

      iex> "a > 5" |> Formulae.check([a: 5])
      false

      iex> "a > 5" |> Formulae.check([a: 3])
      false

      iex> "a < 5" |> Formulae.check([b: 42])
      false

      iex> "a > 5" |> Formulae.check([a: nil])
      false

      iex> "a < 5" |> Formulae.check([{:a, nil}])
      false

      iex> "a > 5" |> Formulae.check([{:a, 6}])
      true

      iex> "a > 5" |> Formulae.check([{:a, 5}])
      false

      iex> "a > 5" |> Formulae.check
      false
  """
  def check(string, bindings \\ []) do
    try do
      Formulae.evaluate(string, bindings)
    rescue
      Formulae.RunnerError -> false
    end
  end

  @doc ~S"""
  Returns a normalized representation for the formula given.

  ## Example

      iex> {_, {:>, _, 0}, bindings} = "(temp - time * 4) > speed / 3.14" |> Formulae.normalize
      ...> bindings
      [:temp, :time, :speed]

      iex> {_, {:<, _, 3.14}, bindings} = "hello < 3.14" |> Formulae.normalize
      ...> bindings
      [:hello]

      iex> {_, {:<, _, 3.14}, bindings} = "HELLO < 3.14" |> Formulae.normalize
      ...> bindings
      [:hello]
  """
  def normalize(input) when is_binary(input) do
    with {normalized, {operation, _env, [formula, value]}} <- unit(input),
         bindings <- bindings?(formula) do
      {normalized, {operation, formula, value}, bindings}
    else
      _ -> raise(Formulae.SyntaxError, formula: input, error: {:unknown, inspect(input)})
    end
  end

  @doc ~S"""
  Curries the formula by substituting the known bindings into it.

  ## Example

      iex> "(temp - foo * 4) > speed / 3.14"
      ...> |> Formulae.curry(temp: 7, speed: 3.14)
      ...> |> Macro.to_string()
      "7 - foo * 4 > 3.14 / 3.14"
  """
  def curry(input, binding \\ [], opts \\ [])
      when is_tuple(input) or is_binary(input) do
    fun = fn
      {var, meta, val} when is_atom(val) ->
        if binding[var], do: binding[var], else: {var, meta, val}

      any ->
        any
    end

    Iteraptor.AST.map(input, fun, opts)
  end

  @doc ~S"""
  Guesses the binding this formula requires.
  FIXME: probably, more sophisticated way would be to analyze Macro.traverse

  ## Examples

      iex> "a > 5" |> Formulae.bindings?
      ~w|a|a

      iex> ":math.sin(a / (3.14 * b)) > c" |> Formulae.bindings?
      ~w|a b c|a

      iex> "a + b * 4 - :math.pow(c, 2) / d > 1.0 * e" |> Formulae.bindings?
      ~w|a b c d e|a
  """
  def bindings?(formula, bindings \\ [])

  def bindings?(formula, bindings) when is_binary(formula) do
    with {:ok, ast} = Code.string_to_quoted(formula),
      do: bindings?(ast, bindings)
  end

  def bindings?(formula, bindings) do
    formula
    |> Iteraptor.AST.reduce([], fn {var, _, _}, acc ->
      if is_nil(bindings[var]), do: [var | acc], else: acc
    end)
    |> Enum.reverse()
  end

  ##############################################################################

  # @comparison [:<, :>, :=] # Damn it, José! :≠
  # @booleans   [:&, :|]

  ##############################################################################

  @doc ~S"""
  Produces the normalized representation of formula. If the _rho_ is
  an instance of [`Integer`](http://elixir-lang.org/docs/stable/elixir/Integer.html#content)
  or [`Float`](http://elixir-lang.org/docs/stable/elixir/Float.html#content),
  it’s left intact, otherwise it’s moved to the left side with negation.

  @todo Try to `eval` _rho_ before guards; premature optimization

  ## Examples

      iex> Formulae.unit("3 > 2")
      {"3 > 2", {:>, [], [3, 2]}}

      iex> Formulae.unit("3 - a > 2")
      {"3 - a > 2", {:>, [], [{:-, [line: 1], [3, {:a, [line: 1], nil}]}, 2]}}

      iex> Formulae.unit("3 > A + 2")
      {"3 > a + 2",
        {:>, [],
          [{:-, [context: Formulae, import: Kernel],
            [3, {:+, [line: 1], [{:a, [line: 1], nil}, 2]}]}, 0]}}

      iex> Formulae.unit("3 >= a + 2")
      {"3 >= a + 2",
        {:>=, [],
          [{:-, [context: Formulae, import: Kernel],
            [3, {:+, [line: 1], [{:a, [line: 1], nil}, 2]}]}, 0]}}

      iex> Formulae.unit("3 a > A + 2")
      ** (Formulae.SyntaxError) Formula [3 a > A + 2] syntax is incorrect (parsing): syntax error before: “a”.

      iex> Formulae.unit("a + 2 = 3")
      {"a + 2 = 3", {:==, [], [{:+, [line: 1], [{:a, [line: 1], nil}, 2]}, 3]}}

      iex> Formulae.unit(~S|A = "3"|)
      {"a = \"3\"", {:==, [], [{:a, [line: 1], nil}, "3"]}}
  """
  def unit(input, env \\ []) when is_binary(input) do
    normalized = String.downcase(input)

    {
      normalized,
      case Code.string_to_quoted(normalized) do
        {:ok, {:>, _, [lh, rh]}} when is_integer(rh) or is_float(rh) ->
          {:>, env, [lh, rh]}

        {:ok, {:>, _, [lh, rh]}} ->
          {:>, env, [quote(do: unquote(lh) - unquote(rh)), 0]}

        {:ok, {:>=, _, [lh, rh]}} when is_integer(rh) or is_float(rh) ->
          {:>=, env, [lh, rh]}

        {:ok, {:>=, _, [lh, rh]}} ->
          {:>=, env, [quote(do: unquote(lh) - unquote(rh)), 0]}

        {:ok, {:<, _, [lh, rh]}} when is_integer(rh) or is_float(rh) ->
          {:<, env, [lh, rh]}

        {:ok, {:<, _, [lh, rh]}} ->
          {:<, env, [quote(do: unquote(lh) - unquote(rh)), 0]}

        {:ok, {:<=, _, [lh, rh]}} when is_integer(rh) or is_float(rh) ->
          {:<=, env, [lh, rh]}

        {:ok, {:<=, _, [lh, rh]}} ->
          {:<=, env, [quote(do: unquote(lh) - unquote(rh)), 0]}

        {:ok, {:=, _, [lh, rh]}} ->
          {:==, env, [lh, rh]}

        {:ok, {:==, _, [lh, rh]}} ->
          {:==, env, [lh, rh]}

        {:ok, {op, _, _}} ->
          raise(Formulae.SyntaxError, formula: input, error: {:operation, double_quote(op)})

        {:error, {1, message, op}} ->
          raise(
            Formulae.SyntaxError,
            formula: input,
            error: {:parsing, message <> double_quote(op)}
          )

        other ->
          raise(Formulae.SyntaxError, formula: input, error: {:unknown, inspect(other)})
      end
    }
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
      ** (Formulae.RunnerError) Formula failed to run (compile): incomplete binding to evaluate a formula, lacking: [:a].

      iex> Formulae.evaluate(Formulae.unit("a + 2 = 3"), [a: 1])
      true

      iex> Formulae.evaluate(Formulae.unit("a + 2 = 3"), [a: 2])
      false

      iex> Formulae.evaluate(Formulae.unit(~S|a = "3"|), [a: "3"])
      true

      iex> Formulae.evaluate(Formulae.unit(~S|a = "3"|), [a: 3])
      false

      iex> Formulae.evaluate(Formulae.unit(~S|a = "3"|), [a: "hello"])
      false

      iex> Formulae.evaluate("a + 2 = 3", [a: 2])
      false

      iex> Formulae.evaluate(~S|a = "3"|, [a: "3"])
      true

      iex> Formulae.evaluate(Formulae.unit("a_b_c_490000 > 2"), [a_b_c_490000: 3])
      true
  """
  def evaluate(input, binding \\ [], opts \\ [])

  def evaluate({_original, ast}, binding, opts),
    do: evaluate(ast, binding, opts)

  def evaluate(input, binding, opts) when is_binary(input),
    do: evaluate(unit(input), binding, opts)

  def evaluate(input, binding, opts) when is_tuple(input) do
    unresolved = bindings?(input, binding)

    if Enum.empty?(unresolved) do
      do_evaluate(input, binding, opts)
    else
      raise(
        Formulae.RunnerError,
        formula: input,
        error:
          {:compile, "incomplete binding to evaluate a formula, lacking: #{inspect(unresolved)}"}
      )
    end
  end

  defp do_evaluate(input, binding, opts) when is_tuple(input) do
    # FIXME Make `nil` acceptable through SQL-like `IS NULL` syntax!
    binding = Enum.reject(binding, fn {_, v} -> is_nil(v) end)

    try do
      case Code.eval_quoted(input, binding, opts) do
        {false, ^binding} -> false
        {true, ^binding} -> true
        other -> raise(Formulae.RunnerError, formula: input, error: {:weird, inspect(other)})
      end
    rescue
      e in CompileError ->
        raise(Formulae.RunnerError, formula: input, error: {:compile, e.description})
    end
  end

  ##############################################################################

  defp double_quote(string) when is_binary(string), do: "“" <> string <> "”"
  defp double_quote(string), do: double_quote(to_string(string))
end
