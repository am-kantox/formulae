# Formulae    [![Kantox ❤ OSS](https://img.shields.io/badge/❤-kantox_oss-informational.svg)](https://kantox.com/)  ![Test](https://github.com/am-kantox/formulae/workflows/Test/badge.svg)  ![Dialyzer](https://github.com/am-kantox/formulae/workflows/Dialyzer/badge.svg)

**Nifty precompiled formulae handling**

## Installation

```elixir
def deps do
  [{:formulae, "~> 0.11"}]
end
```

## Using

```elixir
iex|1 ▸ f = Formulae.compile("rem(a, 5) - b == 0")
#ℱ<[
  sigil: "~F[rem(a, 5) - b == 0]",
  eval: &:"Elixir.Formulae.rem(a, 5) - b == 0".eval/1,
  formula: "rem(a, 5) - b == 0",
  guard: nil,
  module: :"Elixir.Formulae.rem(a, 5) - b == 0",
  variables: [:a, :b]
]>
iex|2 ▸ f.eval.(a: 11, b: 1)
true
iex|3 ▸ f.variables
[:a, :b]
iex|4 ▸ f.module
:"Elixir.Formulae.rem(a, 5) - b == 0"
```

## Changelog

- **`0.11.4`** [FIX] reattempt to create module if not existing on `eval/3` call
- **`0.11.2`** [FIX] restrict `apply/3`, `spawn/3`, and `import/2`
- **`0.11.1`** [FIX] `Formulae.formulas/1` + aliases are handled more naturally
- **`0.11.0`** `v1.0.0` pre-release
  - `options` in a call to `Formulae.compile/2`
  - accurate validation of module existence
  - default arguments
  - aliases
  - imports
- **`0.10.0`**
  - `Inspect` and `String.Chars` protocols implementation
  - `Formulae.Sigils.sigil_F/2` aka `~F[x > y]`
- **`0.9.0`**
  - `options: [eval: :guard, alias: SomeModName]`
  - fixes for modern _Elixir_
  - optimizations in module generation
  - benchmarks
- **`0.8.0`** — generate combinators functions in `Formulae` module

## [Documentation](https://hexdocs.pm/formulae)
