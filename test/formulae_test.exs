defmodule Test.Formulae do
  use ExUnit.Case
  require Formulae.Combinators

  doctest Formulae
  doctest Formulae.Sigils
  doctest Formulae.Combinators
  doctest Formulae.Combinators.Stream

  test "formulae with slashes (division)" do
    f = Formulae.compile("a / 2", imports: :none)
    assert 1.0 == f.eval.(a: 2)
  end

  test "dynamic number of combinations" do
    number = 2
    assert [[:a, :b], [:a, :c], [:b, :c]] == Formulae.combinations(~w|a b c|a, number)
  end

  test "guard" do
    f = Formulae.compile("rem(a, 2) == 0", alias: AIsEven, evaluator: :guard, imports: :none)

    assert {:defguard, _,
            [
              {:when, _,
               [{:guard, _, [{:a, _, nil}]}, {:==, _, [{:rem, _, [{:a, _, nil}, 2]}, 0]}]}
            ]} = f.guard

    require AIsEven

    1..10
    |> Enum.chunk_every(2)
    |> Enum.each(fn [odd, even] ->
      assert AIsEven.eval(a: even)
      refute AIsEven.eval(a: odd)
      assert match?(a when AIsEven.guard(a), even)
      refute match?(a when AIsEven.guard(a), odd)
    end)
  end

  test "multi arguments in clause" do
    f =
      Formulae.compile("rem(a, 2) == 0 and rem(b, 2) != 0",
        evaluator: :guard,
        alias: AIsEvenBIsOdd,
        imports: :none
      )

    assert {:defguard, _,
            [
              {:when, _,
               [
                 {:guard, _, [{:a, _, nil}, {:b, _, nil}]},
                 {:and, _,
                  [
                    {:==, _, [{:rem, _, [{:a, _, nil}, 2]}, 0]},
                    {:!=, _, [{:rem, _, [{:b, _, nil}, 2]}, 0]}
                  ]}
               ]}
            ]} = f.guard

    require AIsEvenBIsOdd

    1..10
    |> Enum.chunk_every(2)
    |> Enum.each(fn [odd, even] ->
      assert AIsEvenBIsOdd.eval(a: even, b: odd)
      assert AIsEvenBIsOdd.eval(b: odd, a: even)
      refute AIsEvenBIsOdd.eval(a: odd, b: even)
      refute AIsEvenBIsOdd.eval(b: even, a: odd)
      assert match?({a, b} when AIsEvenBIsOdd.guard(a, b), {even, odd})
      refute match?({a, b} when AIsEvenBIsOdd.guard(a, b), {odd, even})
      refute match?({a, b} when AIsEvenBIsOdd.guard(a, b), {even, even})
      refute match?({a, b} when AIsEvenBIsOdd.guard(a, b), {odd, odd})
    end)
  end

  test "6+ arguments in guard" do
    f =
      Formulae.compile("a1 == 0 and a2 == 0 and a3 == 0 and a4 == 0 and a5 == 0 and a6 == 0",
        evaluator: :guard,
        alias: A6,
        imports: :none
      )

    assert {:defguard, _, _} = f.guard

    require A6

    assert A6.eval(a1: 0, a2: 0, a3: 0, a4: 0, a5: 0, a6: 0)
    assert A6.eval(a3: 0, a2: 0, a6: 0, a4: 0, a5: 0, a1: 0)
    refute A6.eval(a6: 1, a1: 0, a2: 0, a3: 0, a4: 0, a5: 0)

    Enum.each([{true, {0, 0, 0, 0, 0, 0}}, {false, {0, 0, 0, 0, 0, 1}}], fn {assert?, input} ->
      result = match?({a1, a2, a3, a4, a5, a6} when A6.guard(a1, a2, a3, a4, a5, a6), input)

      if assert?, do: assert(result), else: refute(result)
    end)
  end

  test "sigils" do
    f = Formulae.compile("rem(a, 2) == 0", alias: AIsEven, evaluator: :guard, imports: :none)
    assert "~F[rem(a, 2) == 0]" == "#{f}"

    assert ~s|#ℱ<[sigil: "~F[rem(a, 2) == 0]", eval: &AIsEven.eval/1, formula: "rem(a, 2) == 0", guard: "defguard guard(a) when rem(a, 2) == 0", module: AIsEven, variables: [:a], options: [| <>
             _ = inspect(f)
  end

  describe "curry" do
    test "different variables" do
      f =
        "a + b * 10"
        |> Formulae.compile(imports: :none)
        |> Formulae.curry([b: 3], imports: :none)

      assert 31 == f.eval.(a: 1)
    end

    test "repeated variables" do
      f =
        "a + a + b * 10"
        |> Formulae.compile(imports: :none)
        |> Formulae.curry([b: 3], imports: :none)

      assert "a + a + 3 * 10" == f.formula
      assert [:a] == f.variables
    end

    test "curry with boolean values" do
      f =
        "a || b"
        |> Formulae.compile(imports: :none)
        |> Formulae.curry([a: true, b: false], imports: :none)

      assert "true || false" == f.formula
    end

    test "use alias when curry" do
      f =
        "a + 1"
        |> Formulae.compile(imports: :none)
        |> Formulae.curry([a: 10], alias: SumA, imports: :none)

      assert SumA == f.module
    end
  end

  describe "imports" do
    test "works without imports" do
      f = Formulae.compile("2 + 1", imports: :none)
      assert 3 == Formulae.eval(f, [])
    end

    test "raises without imports" do
      assert_raise Formulae.SyntaxError,
                   ~S|Formula ~F[:math.pi() + 1] syntax is incorrect ("Restricted: [erlang: :math]")|,
                   fn -> Formulae.compile(":math.pi() + 1", imports: :none) end
    end

    test "works with erlang imports" do
      f = Formulae.compile(":math.pi() + 2", imports: [:math])
      assert Formulae.eval(f, []) > 5
      f = Formulae.compile("pi() + 2", imports: [:math])
      assert Formulae.eval(f, []) > 5
      f = Formulae.compile("pi() + 2", imports: [{:math, pi: 0}])
      assert Formulae.eval(f, []) > 5
    end

    test "works with elixir imports (fqn)" do
      f = Formulae.compile("A.B.C.D.foo() + 2", imports: [A.B.C.D])
      assert Formulae.eval(f, []) == 44
    end

    test "works with elixir imports (non-fqn)" do
      f = Formulae.compile("foo() + a", imports: [A.B.C.D])
      assert Formulae.eval(f, a: -41) == 1
    end

    test "works with mixed imports" do
      f = Formulae.compile("pi() + A.B.C.D.foo() + a", imports: [:math, A.B.C.D])
      assert_in_delta Formulae.eval(f, a: -42), :math.pi(), 0.01
    end

    test "works with all imports" do
      f = Formulae.compile(":math.pi() + A.B.C.D.foo() + b", imports: :all)
      assert_in_delta Formulae.eval(f, b: -42), :math.pi(), 0.01
    end

    test "works with selective imports" do
      f =
        Formulae.compile("pi() + A.B.C.D.foo() + c",
          imports: [[:math, [[only: [pi: 0]]]], A.B.C.D]
        )

      assert_in_delta Formulae.eval(f, c: -42), :math.pi(), 0.01
    end

    test "works with selective imports (friendly syntax)" do
      f =
        Formulae.compile("pi() + A.B.C.D.foo() + d",
          imports: [[:math, only: [pi: 0]], A.B.C.D]
        )

      assert_in_delta Formulae.eval(f, d: -42), :math.pi(), 0.01
    end

    test "works with selective imports (friendly syntax, tuple)" do
      f =
        Formulae.compile("pi() + A.B.C.D.foo() + e",
          imports: [{:math, only: [pi: 0]}, A.B.C.D]
        )

      assert_in_delta Formulae.eval(f, e: -42), :math.pi(), 0.01
    end

    test "works with unimports" do
      f = Formulae.compile("div(100, d)", imports: [Clashing], unimports: [div: 2])

      assert_in_delta Formulae.eval(f, d: 10), 10, 0.01
    end
  end

  describe "unexpected input" do
    test "long input https://www.erlang.org/doc/system/memory.html#system-limits" do
      defmodule Helper do
        def timestamp(datetime) do
          DateTime.to_unix(datetime)
        end
      end

      input = """
      # Some comment to indicate that if this string is too long it will error out
      # Some comment to indicate that if this string is too long it will error out
      # Some comment to indicate that if this string is too long it will error out
      # Some comment to indicate that if this string is too long it will error out

      timestamp(start) && timestamp(start)
      """

      formula = Formulae.compile(input, imports: [Helper])
      now = DateTime.utc_now()
      assert DateTime.to_unix(now) == Formulae.eval!(formula, start: now)
    end
  end
end
