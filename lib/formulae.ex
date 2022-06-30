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
          ast: nil | Macro.t(),
          guard: nil | Macro.t(),
          module: nil | atom(),
          variables: nil | [atom()],
          eval: nil | (keyword() -> any())
        }
  defstruct formula: nil, ast: nil, guard: nil, module: nil, eval: nil, variables: nil

  @typedoc false
  @type option :: {:eval, :function | :guard} | {:alias, module()}
  @typedoc false
  @type options :: [option()]

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
  @spec eval(input :: binary() | Formulae.t(), bindings :: keyword()) :: term() | {:error, any()}
  def eval(input, bindings \\ [])
  def eval(%Formulae{eval: eval}, bindings), do: eval.(bindings)

  def eval(input, bindings) when is_binary(input),
    do: input |> Formulae.compile() |> eval(bindings)

  @doc """
  Evaluates the formula returning the result back; throws in a case of unseccessful processing.

  _Examples:_

      iex> Formulae.eval!("rem(a, 5) == 0", a: 20)
      true
      iex> Formulae.eval!("rem(a, 5) == 0")
      ** (Formulae.RunnerError) Formula failed to run (compile): [:missing_arguments] wrong or incomplete eval call: [given_keys: [], expected_keys: [:a]].
  """
  @spec eval!(input :: binary() | Formulae.t(), bindings :: keyword()) :: term() | no_return()
  def eval!(input, bindings \\ []) do
    with {:error, {error, data}} <- Formulae.eval(input, bindings) do
      raise(
        Formulae.RunnerError,
        formula: input,
        error: {:compile, "[#{inspect(error)}] wrong or incomplete eval call: #{inspect(data)}"}
      )
    end
  end

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
  @spec compiled?(binary() | Formulae.t(), options :: options()) :: boolean()
  def compiled?(input, options \\ [])

  def compiled?(input, options) when is_binary(input) and is_list(options),
    do: input |> module_name(options) |> Code.ensure_loaded?()

  def compiled?(%Formulae{module: nil}, _), do: false
  def compiled?(%Formulae{module: _}, _), do: true

  @doc """
  Checks whether the formula was already compiled into module.
  Similar to `compiled?/1`, but returns what `Code.ensure_compiled/1` returns.

  Typically one does not need to call this function, since this check would be
  nevertheless transparently performed before the evaluation.

  _Examples:_

      iex> Formulae.ensure_compiled("bar > 42")
      {:error, :nofile}
      iex> Formulae.compile("bar > 42")
      iex> Formulae.ensure_compiled("bar > 42")
      {:module, :"Elixir.Formulae.bar > 42"}
  """
  @spec ensure_compiled(binary() | Formulae.t(), options :: options()) ::
          {:module, module()}
          | {:error, :embedded | :badfile | :nofile | :on_load_failure | :unavailable}
  def ensure_compiled(input, options \\ [])

  def ensure_compiled(input, options) when is_binary(input) and is_list(options),
    do: input |> module_name(options) |> Code.ensure_compiled()

  def ensure_compiled(%Formulae{module: nil}, _), do: {:error, :unavailable}
  def ensure_compiled(%Formulae{module: module}, _), do: {:module, module}

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
  @spec compile(Formulae.t() | binary(), options :: options()) :: Formulae.t()
  def compile(input, options \\ [])

  def compile(input, options) when is_binary(input) and is_list(options) do
    input
    |> ensure_compiled(options)
    |> maybe_create_module(input, options)
  end

  def compile(%Formulae{formula: input}, options), do: compile(input, options)

  @doc """
  Purges and discards the module for the formula given (if exists.)
  """
  @spec purge(Formulae.t() | binary(), options()) ::
          :ok | {:error, :not_compiled} | {:error, :code_delete}
  def purge(input, options \\ [])

  def purge(input, options) when is_binary(input) and is_list(options),
    do: input |> module_name(options) |> do_purge()

  def purge(%Formulae{module: nil}, _), do: {:error, :not_compiled}
  def purge(%Formulae{module: mod}, _), do: do_purge(mod)

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

  def curry(input, binding, opts) when is_binary(input) do
    {ast, vars} = ast_and_variables(input, binding)
    %Formulae{variables: ^vars} = Formulae.compile(Macro.to_string(ast), opts)
  end

  def curry(%Formulae{formula: formula}, binding, opts) when is_binary(formula),
    do: curry(formula, binding, opts)

  @spec maybe_create_module(
          {:module, atom()} | {:error, any()},
          input :: binary(),
          options :: options()
        ) ::
          Formulae.t()
  defp maybe_create_module({:module, module}, input, options) do
    eval = Keyword.get(options, :eval, :function)

    compatible =
      module == Keyword.get(options, :alias, module) and
        ((eval == :function and is_nil(module.guard_ast())) or
           (eval == :guard and not is_nil(module.guard_ast())))

    if not compatible,
      do:
        raise(Formulae.RunnerError,
          formula: input,
          error: {:incompatible_options, inspect(options)}
        )

    %Formulae{
      formula: input,
      module: module,
      ast: module.ast(),
      guard: module.guard_ast(),
      variables: module.variables(),
      eval: &module.eval/1
    }
  end

  defp maybe_create_module({:error, _}, input, options) do
    {:ok, macro} = Code.string_to_quoted(input)
    eval_kind = Keyword.get(options, :eval, :function)

    {^macro, variables} =
      Macro.prewalk(macro, [], fn
        {var, _, nil} = v, acc -> {v, [var | acc]}
        v, acc -> {v, acc}
      end)

    escaped = Macro.escape(macro)
    variables = variables |> Enum.reverse() |> Enum.uniq()

    guard = do_guard(eval_kind, variables, macro, input)
    guard_ast = Macro.escape(guard)

    eval = do_eval(eval_kind, variables, macro)

    ast = [
      guard,
      quote generated: true do
        @variables unquote(variables)

        def ast, do: unquote(escaped)
        def guard_ast, do: unquote(guard_ast)
        def variables, do: @variables
      end,
      eval
    ]

    {:module, module, _, _} = Module.create(module_name(input, options), ast, __ENV__)

    %Formulae{
      formula: input,
      ast: macro,
      module: module,
      guard: guard,
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
    Formulae.eval(string, bindings)
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
          if Keyword.has_key?(binding, var),
            do: {Keyword.fetch!(binding, var), acc},
            else: {v, [var | acc]}

        v, acc ->
          {v, acc}
      end)

    {ast, vars |> Enum.reverse() |> Enum.uniq()}
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
  @spec bindings?(formula :: Formulae.t() | binary() | tuple(), binding :: keyword()) :: keyword()
  def bindings?(formula, bindings \\ [])

  def bindings?(%Formulae{variables: variables}, []),
    do: variables

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

      iex > Formulae.unit("3 > 2")
      {"3 > 2", {:>, [], [3, 2]}}

      iex > Formulae.unit("3 - a > 2")
      {"3 - a > 2", {:>, [], [{:-, [line: 1], [3, {:a, [line: 1], nil}]}, 2]}}

      iex > Formulae.unit("3 > A + 2")
      {"3 > a + 2",
        {:>, [],
          [{:-, [context: Formulae, import: Kernel],
            [3, {:+, [line: 1], [{:a, [line: 1], nil}, 2]}]}, 0]}}

      iex > Formulae.unit("3 >= a + 2")
      {"3 >= a + 2",
        {:>=, [],
          [{:-, [context: Formulae, import: Kernel],
            [3, {:+, [line: 1], [{:a, [line: 1], nil}, 2]}]}, 0]}}

      iex > Formulae.unit("3 a > A + 2")
      ** (Formulae.SyntaxError) Formula [3 a > A + 2] syntax is incorrect (parsing): syntax error before: “a”.

      iex > Formulae.unit("a + 2 = 3")
      {"a + 2 = 3", {:==, [], [{:+, [line: 1], [{:a, [line: 1], nil}, 2]}, 3]}}

      iex > Formulae.unit(~S|A = "3"|)
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

        {:error, {_, message, op}} ->
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

      iex> Formulae.eval("3 > 2")
      true

      iex> Formulae.eval("3 < 2")
      false

      iex> Formulae.eval("a < 2", a: 1)
      true

      iex> Formulae.eval("a > 2", a: 1)
      false

      iex> Formulae.eval("a < 2", [])
      {:error, {:missing_arguments, [given_keys: [], expected_keys: [:a]]}}

      iex> Formulae.eval!("a < 2", [])
      ** (Formulae.RunnerError) Formula failed to run (compile): [:missing_arguments] wrong or incomplete eval call: [given_keys: [], expected_keys: [:a]].

      iex> Formulae.eval("a + 2 == 3", a: 1)
      true

      iex> Formulae.eval("a + 2 == 3", a: 2)
      false

      iex> Formulae.eval(~S|a == "3"|, a: "3")
      true

      iex> Formulae.eval(~S|a == "3"|, a: 3)
      false

      iex> Formulae.eval(~S|a == "3"|, a: "hello")
      false

      iex> Formulae.eval("a + 2 == 3", a: 2)
      false

      iex> Formulae.eval(~S|a == "3"|, a: "3")
      true

      iex> Formulae.eval("a_b_c_490000 > 2", a_b_c_490000: 3)
      true
  """
  @spec evaluate(input :: binary() | tuple(), binding :: keyword(), opts :: keyword()) ::
          boolean() | no_return()
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
        reraise(
          Formulae.RunnerError,
          [formula: input, error: {:compile, e.description}],
          __STACKTRACE__
        )
    end
  end

  ##############################################################################

  :formulae
  |> Application.compile_env(:generate_combinators, true)
  |> if do
    @max_combinations Application.compile_env(:formulae, :max_combinations, 42)
    @max_permutations Application.compile_env(:formulae, :max_permutations, 12)

    require Formulae.Combinators

    @spec combinations(list :: list(), count :: non_neg_integer()) :: [list()]
    @doc "Generated clauses for `n ∈ [1..#{@max_combinations}]` to be used with dynamic number"
    Enum.each(1..@max_combinations, fn n ->
      def combinations(l, unquote(n)), do: Formulae.Combinators.combinations(l, unquote(n))
    end)

    def combinations(_l, n),
      do: raise(Formulae.RunnerError, formula: :combinations, error: {:too_high, inspect(n)})

    @spec permutations(list :: list(), count :: non_neg_integer()) :: [list()]
    @doc "Generated clauses for `n ∈ [1..#{@max_permutations}]` to be used with dynamic number"
    Enum.each(1..@max_permutations, fn n ->
      def permutations(l, unquote(n)), do: Formulae.Combinators.permutations(l, unquote(n))
    end)

    def permutations(_l, n),
      do: raise(Formulae.RunnerError, formula: :permutations, error: {:too_high, inspect(n)})
  end

  ##############################################################################

  defp double_quote(string) when is_binary(string), do: "“" <> string <> "”"
  defp double_quote(string), do: double_quote(to_string(string))

  defp module_name(input, options) when is_binary(input) and is_list(options),
    do:
      Keyword.get_lazy(options, :alias, fn ->
        Module.concat(Formulae, String.replace(input, <<?/>>, "÷"))
      end)

  defp do_guard(:guard, variables, macro, _input) do
    vars = Enum.map(variables, &Macro.var(&1, nil))

    quote generated: true do
      defguard guard(unquote_splicing(vars)) when unquote(macro)
    end
  end

  defp do_guard(:function, _variables, _macro, _input), do: nil

  Enum.each(1..5, fn len ->
    defp do_eval(:guard, variables, _macro) when length(variables) == unquote(len) do
      vars = Enum.map(variables, &Macro.var(&1, nil))
      varsnames = variables |> Enum.zip(vars)

      require Formulae.Combinators

      varsnames
      |> Formulae.Combinators.permutations(unquote(len))
      |> Enum.map(fn varsnames ->
        quote generated: true do
          def eval(unquote(varsnames)) when guard(unquote_splicing(vars)), do: true
        end
      end)
      |> Kernel.++([
        quote generated: true do
          def eval(_), do: false
        end
      ])
    end
  end)

  defp do_eval(:guard, variables, macro), do: do_eval(:function, variables, macro)

  defp do_eval(:function, variables, macro) do
    vars = Enum.map(variables, &Macro.var(&1, nil))
    varsnames = Enum.zip(variables, vars)

    quote generated: true do
      def eval(unquote(varsnames)), do: unquote(macro)

      def eval(%{} = args) do
        bindings = for k <- @variables, v = Map.get(args, k), not is_nil(v), do: {k, v}

        if length(bindings) == length(@variables),
          do: eval(bindings),
          else:
            {:error,
             {:missing_arguments, [given_keys: Keyword.keys(bindings), expected_keys: @variables]}}
      end

      def eval(args) do
        if Keyword.keyword?(args),
          do: args |> Map.new() |> eval(),
          else: {:error, {:invalid_argument, [given: args, expected_keys: @variables]}}
      end
    end
  end

  defimpl String.Chars do
    @moduledoc false
    def to_string(%Formulae{formula: formula}) do
      "~F[" <> formula <> "]"
    end
  end

  defimpl Inspect do
    @moduledoc false
    import Inspect.Algebra

    def inspect(%Formulae{} = f, opts) do
      if Keyword.get(opts.custom_options, :sigil, false) do
        "~F[" <> f.formula <> "]"
      else
        inner = [
          ast: Macro.to_string(f.ast),
          eval: f.eval,
          formula: f.formula,
          guard: if(f.guard, do: Macro.to_string(f.guard)),
          module: f.module,
          variables: f.variables
        ]

        concat(["#ℱ<", to_doc(inner, opts), ">"])
      end
    end
  end
end
