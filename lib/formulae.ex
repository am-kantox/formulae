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

  Since `v0.10.0` there is an ability to pass `defaults` via `options`.

  _Examples:_

      iex> "z + t" |> Formulae.compile(defaults: [t: 5]) |> Formulae.eval(t: 10, z: 3)
      13
      iex> "z + t" |> Formulae.compile(defaults: [t: 5]) |> Formulae.eval(z: 3)
      8
  """

  @typedoc false
  @type option ::
          {:eval, :function | :guard}
          | {:alias, module()}
          | {:imports, :none | :all | [module()]}
          | {:defaults, keyword()}
  @typedoc false
  @type options :: [option()]

  @options_schema NimbleOptions.new!(
                    alias: [
                      required: false,
                      default: nil,
                      doc: "The alias to be used as a generated module name",
                      type: :atom
                    ],
                    evaluator: [
                      required: false,
                      default: :function,
                      doc: "The type of the evaluation to generate",
                      type: {:in, [:function, :guard]}
                    ],
                    imports: [
                      required: false,
                      default: nil,
                      doc: "The list of modules to allow remote calls from, or `:all | :none`",
                      type: {:or, [{:in, [nil, :none, :all]}, {:list, :atom}, :atom]}
                    ],
                    defaults: [
                      required: false,
                      default: [],
                      doc:
                        "The default values to be used with formula when not suppled in binding"
                    ]
                  )

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
          eval: nil | (keyword() -> any()),
          guard: nil | Macro.t(),
          module: nil | atom(),
          variables: nil | [atom()],
          options: options()
        }
  defstruct formula: nil,
            ast: nil,
            eval: nil,
            guard: nil,
            module: nil,
            variables: nil,
            options: NimbleOptions.validate!([], @options_schema)

  @doc """
  Lists all the compiled formulas.
  """
  @spec formulas(include_internals? :: boolean()) :: %{optional(binary) => module()}
  def formulas(include_internals? \\ false) do
    host_modules = [
      Formulae,
      Formulae.Combinators,
      Formulae.Combinators.H,
      Formulae.Combinators.Stream,
      Formulae.MixProject,
      Formulae.RunnerError,
      Formulae.Sigils,
      Formulae.SyntaxError
    ]

    :code.all_loaded()
    |> Enum.map(&elem(&1, 0))
    |> Enum.filter(
      &(&1 |> to_string() |> String.split(".") |> Enum.take(2) == ~w|Elixir Formulae|)
    )
    |> Kernel.--(host_modules)
    |> Map.new(&{&1, Macro.to_string(&1.ast)})
    |> then(fn result ->
      if include_internals?, do: Map.merge(result, Map.new(host_modules, &{&1, &1})), else: result
    end)
  end

  @doc """
  Evaluates the formula returning the result back.

  _Examples:_

      iex> Formulae.eval("rem(a, 5) + rem(b, 4) == 0", a: 20, b: 20)
      true
      iex> Formulae.eval("rem(a, 5) == 0", a: 21)
      false
      iex> Formulae.eval("rem(a, 5) + rem(b, 4)", a: 21, b: 22)
      3
      iex> Formulae.eval("rem(a, 5) == b", [a: 8], defaults: [b: 3])
      true
      iex> Formulae.eval("rem(a, 5) == c", [a: 8, c: 3], defaults: [b: 3])
      true
  """
  @spec eval(input :: binary() | Formulae.t(), bindings :: keyword(), options :: options()) ::
          term() | {:error, any()}
  def eval(input, bindings \\ [], options \\ [imports: :none])

  def eval(%Formulae{eval: eval, options: options}, bindings, _options),
    do: options[:defaults] |> Keyword.merge(bindings) |> eval.()

  @doc deprecated: """
       Create a formulae explicitly with `Formulae.compile/2`
         and then pass it as the first argument to `eval/2`".

       This call would compile the formulae with default options.
       """
  def eval(input, bindings, options) when is_binary(input),
    do: input |> Formulae.compile(options) |> eval(bindings, options)

  @doc """
  Evaluates the formula returning the result back; throws in a case of unseccessful processing.

  _Examples:_

      iex> Formulae.eval!("rem(a, 5) == 0", a: 20)
      true
      iex> Formulae.eval!("rem(a, 5) == 0")
      ** (Formulae.RunnerError) Formula ~F[rem(a, 5) == 0] failed to run (compile): [:missing_arguments], wrong or incomplete evaluator call: [given_keys: [], expected_keys: [:a]].
  """
  @spec eval!(input :: binary() | Formulae.t(), bindings :: keyword()) :: term() | no_return()
  def eval!(input, bindings \\ [], options \\ []) do
    with {:error, {error, data}} <- Formulae.eval(input, bindings, options) do
      raise(
        Formulae.RunnerError,
        formula: input,
        error:
          {:compile, "[#{inspect(error)}], wrong or incomplete evaluator call: #{inspect(data)}"}
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

  def compiled?(input, options) when is_binary(input) and is_list(options) do
    module = module_name(input, options)
    Map.get(formulas(true), module) == input and Code.ensure_loaded?(module)
  end

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
          | {:error,
             :embedded
             | :badfile
             | :nofile
             | :on_load_failure
             | :unavailable
             | {:already_taken, module()}
             | {:external_module, module()}}
  def ensure_compiled(input, options \\ [])

  def ensure_compiled(input, options) when is_binary(input) and is_list(options) do
    module = module_name(input, options)

    cond do
      function_exported?(module, :ast, 0) ->
        if Macro.to_string(module.ast()) == input,
          do: {:module, module},
          else: {:error, {:already_taken, module}}

      Code.ensure_loaded?(module) ->
        {:error, {:external_module, module}}

      true ->
        {:error, :nofile}
    end
  end

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
  @spec curry(input :: Formulae.t() | binary(), binding :: keyword(), options :: options()) ::
          Formulae.t()
  def curry(input, binding \\ [], options \\ [])

  def curry(input, binding, options) when is_binary(input) do
    curry(Formulae.compile(input, options), binding, options)
  end

  def curry(%Formulae{} = input, binding, options) do
    {ast, vars} = ast_and_variables(input, binding, options)
    %Formulae{variables: ^vars} = Formulae.compile(Macro.to_string(ast), options)
  end

  @spec maybe_create_module(
          {:module, atom()} | {:error, any()},
          input :: binary(),
          options :: options()
        ) ::
          Formulae.t()
  defp maybe_create_module({:module, module}, input, options) do
    with {:error, ex} <- validate_compatibility(module, options), do: raise(ex)

    legacy_ast = Macro.to_string(module.ast())

    unless input == legacy_ast do
      raise Formulae.RunnerError,
        formula: input,
        error: {:incompatible, "Existing: " <> inspect(Macro.to_string(module.ast()))}
    end

    %Formulae{
      formula: input,
      module: module,
      ast: module.ast(),
      guard: module.guard_ast(),
      variables: module.variables(),
      options: module.options(),
      eval: &module.eval/1
    }
  end

  defp maybe_create_module({:error, {:already_taken, module}}, input, options) do
    IO.warn(
      "Redefining Formulae for alias " <>
        inspect(options[:alias]) <>
        ", formula: ~F[#{Macro.to_string(module.ast())}] → ~F[#{input}] (module: " <>
        inspect(module) <> ")",
      []
    )

    maybe_create_module({:error, :redefining}, input, options)
  end

  defp maybe_create_module({:error, {:external_module, module}}, input, options) do
    IO.warn(
      "Alias given for Formulae (" <>
        inspect(options[:alias]) <>
        ") will overwrite existing module " <>
        inspect(module),
      []
    )

    maybe_create_module({:error, :redefining}, input, options)
  end

  defp maybe_create_module({:error, _}, input, options) do
    options =
      options
      |> NimbleOptions.validate!(@options_schema)
      |> Keyword.update(:imports, [], &fix_imports/1)

    imports =
      case Keyword.fetch!(options, :imports) do
        [] -> nil
        [:...] -> nil
        imports -> quote do: import(unquote_splicing(imports))
      end

    {macro, variables} = reduce_ast!(input, options)

    escaped = Macro.escape(macro)
    variables = variables |> Enum.reverse() |> Enum.uniq()

    guard = do_guard(options[:evaluator], variables, macro, input)
    guard_ast = Macro.escape(guard)

    eval = do_eval(options[:evaluator], variables, macro)

    ast = [
      guard,
      quote generated: true do
        @variables unquote(variables)

        import Kernel,
          except: [
            apply: 3,
            exit: 1,
            raise: 1,
            raise: 2,
            reraise: 2,
            reraise: 3,
            spawn: 3,
            spawn_link: 3,
            spawn_monitor: 3,
            throw: 1
          ]

        unquote(imports)

        def ast, do: unquote(escaped)
        def guard_ast, do: unquote(guard_ast)
        def options, do: unquote(options)
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
      options: options,
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
  @spec check(string :: binary(), bindings :: keyword(), options :: options()) :: boolean()
  def check(string, bindings \\ [], options \\ []) do
    Formulae.eval(string, bindings, options)
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

  @spec ast_and_variables(
          input :: binary() | Formulae.t(),
          binding :: keyword(),
          options :: options()
        ) ::
          {tuple(), keyword()}
  defp ast_and_variables(input, binding, options) when is_binary(input) do
    input
    |> Formulae.compile(Keyword.delete(options, :alias))
    |> ast_and_variables(binding, options)
  end

  defp ast_and_variables(
         %Formulae{ast: nil, formula: input, options: options},
         binding,
         new_options
       ) do
    options = Keyword.merge(new_options, options)
    # [AM] FIX
    ast_and_variables(input, binding, options)
  end

  defp ast_and_variables(%Formulae{ast: ast}, binding, _options) do
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

      iex> "a > 5" |> Formulae.bindings?()
      ~w|a|a

      iex> ":math.sin(a / (3.14 * b)) > c" |> Formulae.bindings?([], imports: [:math])
      ~w|a b c|a

      iex> "a + b * 4 - :math.pow(c, 2) / d > 1.0 * e" |> Formulae.bindings?([], imports: :math)
      ~w|a b c d e|a
  """
  @spec bindings?(formula :: Formulae.t() | binary() | tuple(), binding :: keyword()) :: keyword()
  def bindings?(formula, bindings \\ [], options \\ [imports: :none])

  def bindings?(%Formulae{variables: variables}, [], _options),
    do: variables

  def bindings?(formula, bindings, options) when is_binary(formula),
    do: with(f <- Formulae.curry(formula, bindings, options), do: f.variables)

  def bindings?(formula, bindings, options) when is_tuple(formula),
    do: bindings?(Macro.to_string(formula), bindings, options)

  def bindings?(%Formulae{formula: formula}, bindings, options),
    do: bindings?(formula, bindings, options)

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
      ** (Formulae.RunnerError) Formula ~F[a < 2] failed to run (compile): [:missing_arguments], wrong or incomplete evaluator call: [given_keys: [], expected_keys: [:a]].

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
  @spec evaluate(input :: binary() | tuple(), binding :: keyword(), options :: options()) ::
          boolean() | no_return()
  def evaluate(input, binding \\ [], options \\ [])

  def evaluate({_original, ast}, binding, options),
    do: evaluate(ast, binding, options)

  def evaluate(input, binding, options) when is_binary(input),
    do: evaluate(unit(input), binding, options)

  def evaluate(input, binding, options) when is_tuple(input) do
    unresolved = bindings?(input, binding)

    if Enum.empty?(unresolved) do
      do_evaluate(input, binding, options)
    else
      raise(
        Formulae.RunnerError,
        formula: input,
        error:
          {:compile, "incomplete binding to evaluate a formula, lacking: #{inspect(unresolved)}"}
      )
    end
  end

  defp do_evaluate(input, binding, options) when is_tuple(input) do
    binding = Enum.reject(binding, fn {_, v} -> is_nil(v) end)

    try do
      case Code.eval_quoted(input, binding, options) do
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

  defp module_name(input, options) when is_binary(input) and is_list(options) do
    Keyword.get(options, :alias) || Module.concat(Formulae, String.replace(input, <<?/>>, "÷"))
  end

  defp do_guard(:guard, variables, macro, _input) do
    vars = Enum.map(variables, &Macro.var(&1, nil))

    quote generated: true do
      # @doc guard: true
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

  defp fix_imports(imports) do
    case imports do
      nil -> fix_imports_warn()
      :none -> []
      :all -> [:...]
      alias when is_atom(alias) -> [alias]
      aliases when is_list(aliases) -> aliases
    end
  end

  # [AM] Remove this when we disallow default value for it
  defp fix_imports_warn do
    IO.warn(
      "Default value for `imports: :all` argument in a call to `Formulae.compile/2` is deprecated, " <>
        "and will be changed to `:none` in v1.0.0",
      []
    )

    [:...]
  end

  defp validate_compatibility(module, options) do
    with {:ok, validated} <- NimbleOptions.validate(options, @options_schema),
         {:imports, true} <-
           {:imports, validate_imports(module.options[:imports], validated[:imports])},
         {:alias, true} <- {:alias, module == Keyword.get(validated, :alias, module)},
         {:defaults, true} <-
           {:defaults, Keyword.equal?(module.options[:defaults], validated[:defaults])},
         {:evaluator, true} <- {:evaluator, validate_evaluator(validated[:evaluator], module)} do
      {:ok, validated}
    end
  end

  defp validate_imports(:..., _), do: true

  defp validate_imports(imports, :...),
    do:
      {:error,
       %Formulae.RunnerError{
         error: :imports,
         message: "Existing module has imports restricted to " <> inspect(imports)
       }}

  defp validate_imports(from_module, from_options) do
    [from_module, from_options]
    |> Enum.map(&fix_imports/1)
    |> Enum.reduce(&Kernel.--/2)
    |> Enum.empty?()
    |> Kernel.||(
      {:error,
       %Formulae.RunnerError{
         error: :imports,
         message:
           "Incompatible imports: " <> inspect(existing: from_module, supplied: from_options)
       }}
    )
  end

  defp validate_evaluator(:guard, module), do: is_nil(module.guard_ast())
  defp validate_evaluator(:function, module), do: not is_nil(module.guard_ast())
  # [AM] maybe warn
  defp validate_evaluator(_, module), do: not is_nil(module.guard_ast())

  defp reduce_ast!(input, options) do
    macro = Code.string_to_quoted!(input)
    imports = Keyword.fetch!(options, :imports)

    {^macro, {^imports, issues, variables}} =
      Macro.postwalk(macro, {imports, [], []}, fn
        {var, _, nil} = v, {imports, issues, acc} ->
          {v, {imports, issues, [var | acc]}}

        v, {[:...], _, _} = acc ->
          {v, acc}

        {:__aliases__, _, [_ | _]} = alias, {imports, issues, acc} ->
          wanna_import = :elixir_aliases.expand_or_concat(alias, __ENV__)
          do_wanna_import(:alias, wanna_import, alias, {imports, issues, acc})

        {:., _, [wanna_import, _fun]} = alias, {imports, issues, acc} ->
          do_wanna_import(:erlang, wanna_import, alias, {imports, issues, acc})

        {:import, _, wanna_import} = alias, {imports, issues, acc} ->
          do_wanna_import(:import, wanna_import, alias, {imports, issues, acc})

        v, acc ->
          {v, acc}
      end)

    unless Enum.empty?(issues) do
      raise %Formulae.SyntaxError{
        formula: input,
        error: "Restricted: " <> inspect(Enum.uniq(issues))
      }
    end

    {macro, variables}
  end

  defp do_wanna_import(kind, wanna_import, alias, {imports, issues, acc}) do
    if wanna_import in imports do
      {alias, {imports, issues, acc}}
    else
      {alias, {imports, [{kind, Macro.expand_literals(wanna_import, __ENV__)} | issues], acc}}
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
          variables: f.variables,
          options: f.options
        ]

        concat(["#ℱ<", to_doc(inner, opts), ">"])
      end
    end
  end
end
