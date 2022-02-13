Formulae.compile("rem(a, 2) == 0", eval: :guard, alias: AIsEven)
Formulae.compile("rem(a, 2) == 0 and rem(b, 2) != 0", eval: :guard, alias: AIsEvenBIsOdd)

Formulae.compile("a1 == 0 and a2 == 0 and a3 == 0 and a4 == 0 and a5 == 0 and a6 == 0",
  eval: :guard,
  alias: A6
)

ExUnit.start()
