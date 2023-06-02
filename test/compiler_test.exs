defmodule Test.Formulae.Compiler do
  use ExUnit.Case, async: true

  doctest Formulae.Compiler

  alias Formulae.Compiler

  describe "with finitomata" do
    @tag :finitomata
    test "works as expected" do
      start_link_supervised!({Finitomata.Supervisor, id: Sup1})
      Finitomata.start_fsm(Sup1, Compiler, Compiler, nil)

      assert :ok == Compiler.compile(Sup1, Compiler, "a / 2", imports: :none)
      assert 1.0 == Compiler.eval(Sup1, Compiler, "a / 2", a: 2)
      assert :ok == Compiler.compile(Sup1, Compiler, "a / 2", imports: :none)
      assert 1 == map_size(Compiler.formulas(Sup1, Compiler))

      assert 2 == :"Elixir.Formulae.a ÷ 2".eval(a: 4)
    end

    @tag :finitomata
    test "works under stress" do
      start_link_supervised!({Finitomata.Supervisor, id: Sup2})
      Finitomata.start_fsm(Sup2, Compiler, Compiler, nil)

      1..1_000
      |> Task.async_stream(
        fn _ ->
          Compiler.compile(Sup2, Compiler, "a * 2", imports: :none)
        end,
        max_concurrency: 100
      )
      |> Enum.to_list()

      assert 1 == map_size(Compiler.formulas(Sup2, Compiler))
      assert 4 == Compiler.eval(Sup2, Compiler, "a * 2", a: 2)

      assert 8 == :"Elixir.Formulae.a * 2".eval(a: 4)
    end
  end

  describe "with plain gen_server" do
    test "works as expected" do
      start_link_supervised!(Formulae.Compiler)

      assert :ok == Compiler.compile(nil, Compiler, "a / 2", imports: :none)
      assert 1.0 == Compiler.eval(nil, Compiler, "a / 2", a: 2)
      assert :ok == Compiler.compile(nil, Compiler, "a / 2", imports: :none)
      refute is_nil(Compiler.formulas(nil, Compiler)["a / 2"])

      assert 2 == :"Elixir.Formulae.a ÷ 2".eval(a: 4)
    end

    test "works under stress" do
      start_link_supervised!(Formulae.Compiler)

      1..1_000
      |> Task.async_stream(
        fn _ ->
          Compiler.compile(nil, Compiler, "a * 2", imports: :none)
        end,
        max_concurrency: 100
      )
      |> Enum.to_list()

      refute is_nil(Compiler.formulas(nil, Compiler)["a * 2"])
      assert 4 == Compiler.eval(nil, Compiler, "a * 2", a: 2)

      assert 8 == :"Elixir.Formulae.a * 2".eval(a: 4)
    end
  end
end
