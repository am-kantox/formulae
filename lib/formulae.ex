defmodule Formulae do
  @moduledoc ~S"""
  A set of functions to deal with analytical formulae.

  The typical way of using this module would be to call `Formulae.compile/1`
  on the binary representing the string.

  ```elixir
  iex|1 ▶ f = Formulae.compile "a + :math.sin(3.14 * div(b, 2)) - c"

  %Formulae{
    ast: {:-, [line: 1],
    [
      {:+, [line: 1],
        [
          {:a, [line: 1], nil},
          {{:., [line: 1], [:math, :sin]}, [line: 1],
          [{:*, [line: 1], [3.14, {:div, [line: 1], [{:b, [line: 1], nil}, 2]}]}]}
        ]},
      {:c, [line: 1], nil}
    ]},
    eval: &:"Elixir.Formulae.a + :math.sin(3.14 * div(b, 2)) - c".eval/1,
    formula: "a + :math.sin(3.14 * div(b, 2)) - c",
    module: :"Elixir.Formulae.a + :math.sin(3.14 * div(b, 2)) - c",
    variables: [:a, :b, :c]
  }
  ```

  Now the formula is compiled and might be invoked by calling `Formulae.eval/2`
  passing a formula _and_ bindings. First call to `eval/2` would lazily compile
  the module if needed.

  ```elixir
  iex|2 ▶ f.eval.(a: 3, b: 4, c: 2)
  0.9968146982068622
  ```

  The formulae might be curried.

  ```elixir
  iex|3 ▶ Formulae.curry(f, a: 3, b: 4)
  %Formulae{
    ast: ...,
    eval: &:"Elixir.Formulae.3 + :math.sin(3.14 * div(4, 2)) - c".eval/1,
    formula: "3 + :math.sin(3.14 * div(4, 2)) - c",
    module: :"Elixir.Formulae.3 + :math.sin(3.14 * div(4, 2)) - c",
    variables: [:c]
  }
  ```
  """

  @typedoc """
  The formulae is internally represented as struct, exposing the original
  binary representing the formula, AST, the module this formula was compiled
  into, variables (bindings) this formula has _and_ the evaluator, which is
  the function of arity one, accepting the bindings as a keyword list and
  returning the result of this formula application.
  """
  @type t :: %{
          __struct__: atom(),
          formula: binary(),
          ast: nil | tuple(),
          module: nil | atom(),
          variables: nil | [atom()],
          eval: nil | (keyword() -> any())
        }
  defstruct formula: nil, ast: nil, module: nil, eval: nil, variables: nil

  @doc """
  Evaluates the formula returning the result back.

  _Examples:_

      iex> Formulae.eval("rem(a, 5) + rem(b, 4) == 0", a: 20, b: 20)
      true
      iex> Formulae.eval("rem(a, 5) == 0", a: 21)
      false
      iex> Formulae.eval("rem(a, 5) + rem(b, 4)", a: 21, b: 22)
      3
  """
  @spec eval(string :: binary(), bindings :: keyword()) :: term()
  def eval(string, bindings \\ []),
    do: with(f <- Formulae.compile(string), do: f.eval.(bindings))

  @doc """
  Checks whether the formula was already compiled into module.

  Typically one does not need to call this function, since this check would be
  nevertheless transparently performed before the evaluation.

  _Examples:_

      iex> Formulae.compiled?("foo > 42")
      false
      iex> Formulae.compile("foo > 42")
      iex> Formulae.compiled?("foo > 42")
      true
  """
  @spec compiled?(binary() | Formulae.t()) :: boolean()
  def compiled?(input) when is_binary(input),
    do: Formulae |> Module.concat(input) |> Code.ensure_loaded?()

  def compiled?(%Formulae{module: nil}), do: false
  def compiled?(%Formulae{module: _}), do: true

  @doc """
  Compiles the formula into module.

  _Examples:_

      iex> f = Formulae.compile("rem(a, 5) - b == 0")
      iex> f.formula
      "rem(a, 5) - b == 0"
      iex> f.variables
      [:a, :b]
      iex> f.module
      :"Elixir.Formulae.rem(a, 5) - b == 0"
      iex> f.module.eval(a: 12, b: 2)
      true

      iex> f = Formulae.compile("rem(a, 5) + b == a")
      iex> f.variables
      [:a, :b]
      iex> f.eval.(a: 7, b: 5)
      true
      iex> f.eval.(a: 7, b: 0)
      false
  """
  @spec compile(Formulae.t() | binary()) :: Formulae.t()
  def compile(input) when is_binary(input) do
    Formulae
    |> Module.concat(input)
    |> Code.ensure_loaded()
    |> maybe_create_module(input)
  end

  def compile(%Formulae{formula: input}), do: compile(input)

  @doc """
  Purges and discards the module for the formula given (if exists.)
  """
  @spec purge(Formulae.t() | binary()) :: :ok | {:error, :not_compiled} | {:error, :code_delete}
  def purge(input) when is_binary(input),
    do: Formulae |> Module.concat(input) |> do_purge()

  def purge(%Formulae{module: nil}), do: {:error, :not_compiled}

  def purge(%Formulae{module: mod}), do: do_purge(mod)

  @spec do_purge(atom()) :: :ok | {:error, :not_compiled} | {:error, :code_delete}
  defp do_purge(mod) do
    :code.purge(mod)
    if :code.delete(mod), do: :ok, else: {:error, :code_delete}
  end

  @doc ~S"""
  Curries the formula by substituting the known bindings into it.

  ## Example

      iex> Formulae.curry("(temp - foo * 4) > speed / 3.14", temp: 7, speed: 3.14).formula
      "7 - foo * 4 > 3.14 / 3.14"
  """
  @spec curry(input :: Formulae.t() | binary(), binding :: keyword(), opts :: keyword()) ::
          Formulae.t()
  def curry(input, binding \\ [], opts \\ [])

  def curry(input, binding, _opts) when is_binary(input) do
    {ast, vars} = ast_and_variables(input, binding)
    %Formulae{variables: ^vars} = Formulae.compile(Macro.to_string(ast))
  end

  def curry(%Formulae{formula: formula}, binding, opts) when is_binary(formula),
    do: curry(formula, binding, opts)

  @spec maybe_create_module({:module, atom()} | {:error, any()}, input :: binary()) ::
          Formulae.t()
  defp maybe_create_module({:module, module}, input),
    do: %Formulae{
      formula: input,
      module: module,
      ast: module.ast(),
      variables: module.variables(),
      eval: &module.eval/1
    }

  defp maybe_create_module({:error, _}, input) do
    with {:ok, macro} <- Code.string_to_quoted(input),
         {^macro, variables} =
           Macro.prewalk(macro, [], fn
             {var, _, nil} = v, acc -> {v, [var | acc]}
             v, acc -> {v, acc}
           end),
         escaped = Macro.escape(macro),
         variables = variables |> Enum.reverse() |> Enum.uniq(),
         vars = Enum.map(variables, &{&1, Macro.var(&1, nil)}),
         ast =
           (quote generated: true do
              @variables unquote(variables)

              def ast, do: unquote(escaped)
              def variables, do: @variables

              defmacrop do_eval(unquote(vars)), do: {unquote(escaped), unquote(vars)}
              def eval(unquote(vars)), do: unquote(vars) |> do_eval() |> elem(0)

              def eval(%{} = binding),
                do: eval(for k <- @variables, do: {k, Map.get(binding, k)})

              def eval(args),
                do:
                  {:error, {:invalid_arguments, [given: args, expected_keys: unquote(variables)]}}
            end),
         {:module, module, _, _} <-
           Module.create(Module.concat(Formulae, input), ast, __ENV__),
         do: %Formulae{
           formula: input,
           ast: macro,
           module: module,
           variables: variables,
           eval: &module.eval/1
         }
  end

  ##############################################################################

  @doc deprecated: "Use `Formulae.eval/2` instead"

  @doc ~S"""
  Revalidates the formula with bindings given. Returns true if the formula
  strictly evaluates to `true`, `false` otherwise. Compiles the formula
  before evaluation if needed.
  """
  @spec check(string :: binary(), bindings :: keyword()) :: boolean()
  def check(string, bindings \\ []) do
    Formulae.evaluate(string, bindings)
  rescue
    Formulae.RunnerError -> false
  end

  @doc deprecated: "Use `Formulae.compile/1` and `%Formulae{}.variables` instead"

  @doc ~S"""
  Returns a normalized representation for the formula given.
  """
  def normalize(input) when is_binary(input) do
    with {normalized, {operation, _env, [formula, value]}} <- unit(input),
         bindings <- bindings?(formula) do
      {normalized, {operation, formula, value}, bindings}
    else
      _ -> raise(Formulae.SyntaxError, formula: input, error: {:unknown, inspect(input)})
    end
  end

  @spec ast_and_variables(input :: binary() | Formulae.t(), binding :: keyword()) ::
          {tuple(), keyword()}
  defp ast_and_variables(input, binding) when is_binary(input) do
    input
    |> Formulae.compile()
    |> ast_and_variables(binding)
  end

  defp ast_and_variables(%Formulae{ast: nil, formula: input}, binding),
    do: ast_and_variables(input, binding)

  defp ast_and_variables(%Formulae{ast: ast}, binding) do
    {ast, vars} =
      Macro.prewalk(ast, [], fn
        {var, _, nil} = v, acc when is_atom(var) ->
          if binding[var], do: {binding[var], acc}, else: {v, [var | acc]}

        v, acc ->
          {v, acc}
      end)

    {ast, Enum.reverse(vars)}
  end

  @doc deprecated:
         "Use `Formulae.compile/1` and `%Formulae{}.variables` or `Formula.curry/2` instead"

  @doc ~S"""
  Returns the binding this formula requires.

  ## Examples

      iex> "a > 5" |> Formulae.bindings?
      ~w|a|a

      iex> ":math.sin(a / (3.14 * b)) > c" |> Formulae.bindings?
      ~w|a b c|a

      iex> "a + b * 4 - :math.pow(c, 2) / d > 1.0 * e" |> Formulae.bindings?
      ~w|a b c d e|a
  """
  @spec bindings?(formula :: Formulae.t() | binary(), binding :: keyword()) :: keyword()
  def bindings?(formula, bindings \\ [])

  def bindings?(formula, bindings) when is_binary(formula),
    do: with(f <- Formulae.curry(formula, bindings), do: f.variables)

  def bindings?(formula, bindings) when is_tuple(formula),
    do: bindings?(Macro.to_string(formula), bindings)

  def bindings?(%Formulae{formula: formula}, bindings),
    do: bindings?(formula, bindings)

  ##############################################################################

  # @comparison [:<, :>, :=] # Damn it, José! :≠
  # @booleans   [:&, :|]

  ##############################################################################

  @deprecated "Use `Formulae.eval/2` instead"

  @doc ~S"""
  Produces the normalized representation of formula. If the _rho_ is
  an instance of [`Integer`](http://elixir-lang.org/docs/stable/elixir/Integer.html#content)
  or [`Float`](http://elixir-lang.org/docs/stable/elixir/Float.html#content),
  it’s left intact, otherwise it’s moved to the left side with negation.

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
  # credo:disable-for-lines:50
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

  @deprecated "Use `Formulae.eval/2` instead"

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
    binding = Enum.reject(binding, fn {_, v} -> is_nil(v) end)

    try do
      case Code.eval_quoted(input, binding, opts) do
        {false, ^binding} -> false
        {true, ^binding} -> true
        other -> raise(Formulae.RunnerError, formula: input, error: {:weird, inspect(other)})
      end
    rescue
      e in CompileError ->
        reraise(Formulae.RunnerError, formula: input, error: {:compile, e.description})
    end
  end

  ##############################################################################

  defp double_quote(string) when is_binary(string), do: "“" <> string <> "”"
  defp double_quote(string), do: double_quote(to_string(string))
end
