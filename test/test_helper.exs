Formulae.compile("x / y > 42", evaluator: :function, imports: :none)
Formulae.compile("x == y", evaluator: :guard, imports: :all)

Formulae.compile("rem(a, 2) == 0", evaluator: :guard, alias: AIsEven, imports: :none)

Formulae.compile("rem(a, 2) == 0 and rem(b, 2) != 0",
  evaluator: :guard,
  alias: AIsEvenBIsOdd,
  imports: :none
)

Formulae.compile("a1 == 0 and a2 == 0 and a3 == 0 and a4 == 0 and a5 == 0 and a6 == 0",
  evaluator: :guard,
  alias: A6,
  imports: :none
)

defmodule A.B.C.D do
  @moduledoc false
  def foo, do: 42
end

defmodule Clashing do
  def div(a, b), do: a / b
end

ExUnit.start(exclude: :finitomata)
