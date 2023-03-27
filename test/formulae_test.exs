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

    case Version.compare(System.version(), "1.13.0") do
      :lt ->
        assert ~s|#ℱ<[ast: "rem(a, 2) == 0", eval: &AIsEven.eval/1, formula: "rem(a, 2) == 0", guard: "defguard(guard(a) when rem(a, 2) == 0)", module: AIsEven, variables: [:a], options: [| <>
                 _ = inspect(f)

      _ ->
        assert ~s|#ℱ<[ast: "rem(a, 2) == 0", eval: &AIsEven.eval/1, formula: "rem(a, 2) == 0", guard: "defguard guard(a) when rem(a, 2) == 0", module: AIsEven, variables: [:a], options: [| <>
                 _ = inspect(f)
    end
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
end
