Formulae.compile("x / y > 42", eval: :function, remote_calls: :none)
Formulae.compile("x == y", eval: :guard, remote_calls: :none)

Formulae.compile("rem(a, 2) == 0", eval: :guard, alias: AIsEven, remote_calls: :none)

Formulae.compile("rem(a, 2) == 0 and rem(b, 2) != 0",
  eval: :guard,
  alias: AIsEvenBIsOdd,
  remote_calls: :none
)

Formulae.compile("a1 == 0 and a2 == 0 and a3 == 0 and a4 == 0 and a5 == 0 and a6 == 0",
  eval: :guard,
  alias: A6,
  remote_calls: :none
)

ExUnit.start()
