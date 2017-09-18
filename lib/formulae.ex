defmodule Formulae do

  @moduledoc ~S"""
  A set of functions to deal with analytical formulae.
  """

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
  Returns a normalized (`{:<, "sumOaSTS3E14CSMSb", 0}`) representation
  for the formula given.

  ## Example

      iex> "(temp - time * 4) > speed / 3.14" |> Formulae.normalize
      {:>, {"tempSMStimeSTS4SMSspeedSDS3E14", "temp - time * 4 - speed / 3.14"}, 0}

      iex> "hello < 3.14" |> Formulae.normalize
      {:<, {"hello", "hello"}, 3.14}

      iex> "HELLO < 3.14" |> Formulae.normalize
      {:<, {"hello", "HELLO"}, 3.14}
  """
  def normalize(string) when is_binary(string) do
    {operation, _, [formula, value]} = unit(string)
    {operation, rigid(formula), value}
  end

  @doc ~S"""
  Curries the formula by substituting the known bindings into it.

  ## Example

      iex> "(temp - foo * 4) > speed / 3.14" |> Formulae.curry(temp: 7, speed: 3.14) |> Macro.to_string
      "{7 - foo * 4 - 1.0 > 0, {}}"
  """
  def curry(input, bindings \\ [], opts \\ __ENV__)
  def curry(input, binding, opts) when is_tuple(input) do
    pre = fn t, any -> {t, any} end
    post = fn t, any ->
      t = try do
            {var, _, _values} = t
            case binding[var] do
              nil ->
                {result, _ } = Code.eval_quoted(t, binding, opts)
                result
              other -> other
            end
          rescue
            _e in [CompileError, MatchError, UndefinedFunctionError] -> t
          end
      {t, any}
    end
    Macro.traverse(input, {}, pre, post)
  end
  def curry(input, binding, opts) when is_binary(input) do
    curry(unit(input), binding, opts)
  end

  @binding_finder ~r|undefined function (\w+)/0|

  @doc ~S"""
  Guesses the binding this formula requires.
  FIXME: probably, more sophisticated way would be to analyze Macro.traverse

  ## Examples

      iex> "a > 5" |> Formulae.bindings?
      ~w|a|a

      iex> ":math.sin(a / (3.14 * b)) > c" |> Formulae.bindings?
      ~w|a b c|a
  """
  def bindings?(string, bindings \\ []) do
    try do
      Formulae.evaluate(string, bindings)
      bindings |> Keyword.keys
    rescue
      e in Formulae.RunnerError ->
        case e.error do
          {:compile, message} ->
            neu = @binding_finder
                    |> Regex.run(message, capture: :all_but_first)
                    |> Enum.map(fn e -> {String.to_atom(e), 1} end)
            bindings?(string, bindings ++ neu)
          _ ->
            raise(e)
        end
    end
  end

  ##############################################################################

  # @comparison [:<, :>, :=] # Damn it, José! :≠
  # @booleans   [:&, :|]

  @surrogates [" ()+-*/.", "SOCPMTDE"]
  @rigid ~r|\A[a-z][a-z_\d]*\z|
  @varname ~r|\A[a-zA-Z]\w*\z|

  @regexps @surrogates |> Enum.map(fn e ->
    e |> String.split(~r|(?!\z)|) |> Enum.map(&Regex.escape/1) |> Enum.join("|") |> Regex.compile!
  end)
  # %{"C" => ")", "D" => "/", "M" => "-", "O" => "(", "P" => "+", "S" => " ", "T" => "*", "E" => "."}
  @var_to_fun @surrogates |> Enum.map(&String.split(&1, ~r|(?!\z)|)) |> Enum.reduce(&Enum.zip/2) |> Enum.into(%{})
  # %{" " => "S", "(" => "O", ")" => "C", "*" => "T", "+" => "P", "-" => "M", "/" => "D", "." => "E"}
  @fun_to_var ~w|keys values|a |> Enum.map(&apply(Map, &1, [@var_to_fun])) |> Enum.reduce(&Enum.zip/2) |> Enum.into(%{})

  ##############################################################################

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

      iex> Formulae.unit("3 > A + 2")
      {:>, [],
        [{:-, [context: Formulae, import: Kernel],
          [3, {:+, [line: 1], [{:__aliases__, [counter: 0, line: 1], [:A]}, 2]}]}, 0]}

      iex> Formulae.unit("3 >= a + 2")
      {:>=, [],
        [{:-, [context: Formulae, import: Kernel],
          [3, {:+, [line: 1], [{:a, [line: 1], nil}, 2]}]}, 0]}

      iex> Formulae.unit("3 a > A + 2")
      ** (Formulae.SyntaxError) Formula [3 a > A + 2] syntax is incorrect (parsing): syntax error before: “a”.

      iex> Formulae.unit("a + 2 = 3")
      {:==, [], [{:+, [line: 1], [{:a, [line: 1], nil}, 2]}, 3]}

      iex> Formulae.unit(~S|A = "3"|)
      {:==, [], [{:__aliases__, [counter: 0, line: 1], [:A]}, "3"]}
  """
  def unit(input, env \\ []) when is_binary(input) do
    case Code.string_to_quoted(input) do #  |> String.downcase
      {:ok, {:>, _, [lh, rh]}} when is_integer(rh) or is_float(rh) -> {:>, env, [lh, rh]}
      {:ok, {:>, _, [lh, rh]}} -> {:>, env, [(quote do: unquote(lh) - unquote(rh)), 0]}

      {:ok, {:>=, _, [lh, rh]}} when is_integer(rh) or is_float(rh) -> {:>=, env, [lh, rh]}
      {:ok, {:>=, _, [lh, rh]}} -> {:>=, env, [(quote do: unquote(lh) - unquote(rh)), 0]}

      {:ok, {:"=>", _, [lh, rh]}} when is_integer(rh) or is_float(rh) -> {:>=, env, [lh, rh]}
      {:ok, {:"=>", _, [lh, rh]}} -> {:>=, env, [(quote do: unquote(lh) - unquote(rh)), 0]}

      {:ok, {:<, _, [lh, rh]}} when is_integer(rh) or is_float(rh) -> {:<, env, [lh, rh]}
      {:ok, {:<, _, [lh, rh]}} -> {:<, env, [(quote do: unquote(lh) - unquote(rh)), 0]}

      {:ok, {:<=, _, [lh, rh]}} when is_integer(rh) or is_float(rh) -> {:<=, env, [lh, rh]}
      {:ok, {:<=, _, [lh, rh]}} -> {:<=, env, [(quote do: unquote(lh) - unquote(rh)), 0]}

      {:ok, {:"=<", _, [lh, rh]}} when is_integer(rh) or is_float(rh) -> {:<=, env, [lh, rh]}
      {:ok, {:"=<", _, [lh, rh]}} -> {:<=, env, [(quote do: unquote(lh) - unquote(rh)), 0]}

      {:ok, {:=, _, [lh, rh]}} when is_integer(rh) or is_float(rh) or is_binary(rh) or is_boolean(rh) or is_nil(rh) -> {:==, env, [lh, rh]}
      {:ok, {:=, _, [lh, rh]}} -> {:==, lh - rh}

      {:ok, {:==, _, [lh, rh]}} when is_integer(rh) or is_float(rh) or is_binary(rh) or is_boolean(rh) or is_nil(rh) -> {:==, env, [lh, rh]}
      {:ok, {:==, _, [lh, rh]}} -> {:==, lh - rh}

      {:ok, {op, _, _}} -> raise(Formulae.SyntaxError, formula: input, error: {:operation, double_quote(op)})
      {:error, {1, message, op}} -> raise(Formulae.SyntaxError, formula: input, error: {:parsing,  message <> double_quote(op)})
      other -> raise(Formulae.SyntaxError, formula: input, error: {:unknown, inspect(other)})
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

      # This test issues a warning about “variable "a" does not exist and is being expanded to "a()"”
      iex> Formulae.evaluate(Formulae.unit("a < 2"), [])
      ** (Formulae.RunnerError) Formula failed to run (compile): undefined function a/0.

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
  def evaluate(input, binding, opts) when is_tuple(input) do
    # FIXME Make `nil` acceptable through SQL-like `IS NULL` syntax!
    binding = Enum.filter(binding, fn {_, v} -> !is_nil(v) end)
    try do
      case Code.eval_quoted(input, binding, opts) do
        {false, ^binding} -> false
        {true,  ^binding} -> true
        other             -> raise(Formulae.RunnerError, formula: input, error: {:weird, inspect(other)})
      end
    rescue
      e in CompileError -> raise(Formulae.RunnerError, formula: input, error: {:compile, e.description})
    end
  end
  def evaluate(input, binding, opts) when is_binary(input) do
    evaluate(input |> unit, binding, opts)
  end

  @doc ~S"""
  Checks if the given string represents the rigid identifier.

  ## Examples

      iex> Formulae.rigid?("hello")
      true

      iex> Formulae.rigid?("HELLO")
      false

      iex> Formulae.rigid?("hello world!")
      false

      iex> Formulae.rigid?("sum0")
      true

      iex> Formulae.rigid?("sum(3.14 * 42)")
      false
  """
  def rigid?(string, strict \\ true) when is_binary(string) do
    (if strict, do: @rigid, else: @varname) |> Regex.match?(string)
  end

  def rigid(ast) when is_tuple(ast) do
    string = Macro.to_string(ast)
    {rigid!(string), string}
  end

  @doc ~S"""
  Makes a valid (rigid) identifier out of AST.

  ## Examples

    iex> "sum(a * 3.14) - b" |> Formulae.rigid!
    "sumOaSTS3E14CSMSb"

    iex> "sum(a * 3.14) - b ± 3" |> Formulae.rigid!
    ** (Formulae.SyntaxError) Formula [sum(a * 3.14) - b ± 3] syntax is incorrect (symbols): NYI.

  """
  def rigid!(ast) when is_binary(ast) do
    with [regexp_first | _] <- @regexps,
         ast <- String.downcase(ast),
         result <- Regex.replace(regexp_first, ast, fn key, _ -> @fun_to_var[key] end),
         true <- rigid?(result, false) do
      result
    else
      false -> raise(Formulae.SyntaxError, formula: ast, error: {:symbols, "NYI"})
      other -> raise(Formulae.SyntaxError, formula: ast, error: {:symbols, inspect(other)})
    end
  end

  def rigid!(ast) when is_tuple(ast) do
    case rigid(ast) do
      {result, _} -> result
      other -> raise(Formulae.SyntaxError, formula: inspect(other), error: {:ast, "NYI"})
    end
  end

  @doc ~S"""
  Makes a string representation of a formula out of it’s rigid implementation.

  ## Examples

    iex> "sumOaSTS3E14CSMSb" |> Formulae.unrigid!
    "sum(a * 3.14) - b"
    iex> "sum(a * 3.14) - b ± 3" |> Formulae.unrigid!
    ** (Formulae.SyntaxError) Formula [sum(a * 3.14) - b ± 3] syntax is incorrect (not_rigid): NYI.

  """
  def unrigid!(var) when is_binary(var) do
    unless rigid?(var, false), do: raise(Formulae.SyntaxError, formula: var, error: {:not_rigid, "NYI"})
    if rigid?(var, true), do: var, else: Regex.replace(@regexps |> List.last, var, fn key, _ -> @var_to_fun[key] end)
  end

  ##############################################################################

  defp double_quote(string) when is_binary(string), do: "“" <> string <> "”"
  defp double_quote(string), do: double_quote(to_string(string))
end
