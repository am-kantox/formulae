defmodule Formulae.Compiler do
  @moduledoc """
  The process to be spawned to compile `Formulae` instances without clashes.

  _Elixir_ is great in parallel compilation, and in highly concurrent environment
  in-place compilation of the same formulas might result in clashes.

  If your application already uses `Finitomata`, compiler would be created as
  an instance of it, simply launch it as

  ```elixir
  Finitomata.start_fsm YOUR_FINITOMATA_SUP, Formulae.Compiler, "Compiler", nil
  ```

  otherwise it’ll be compiled into a regular `GenServer` module exporting
  `start_link/0`.

  Once started, one might simply use `Compiler.compile(formula, options)`
  and `Compiler.eval(formula, options)`.
  """

  @type t :: %{
          __struct__: __MODULE__,
          payload: term,
          formulas: %{optional(binary()) => Formulae.t()}
        }

  defstruct payload: nil, formulas: %{}

  require Logger

  import Formulae.Compiler.AST
  ast()

  defp collect_existing do
    for {mod, formula} <- Formulae.formulas(), into: %{} do
      {formula, Formulae.compile(formula, mod.options)}
    end
  end
end
