defmodule Formulae.SyntaxError do
  defexception [:formula, :error, :message]

  @doc false
  def exception(value) do
    {what, how} = value[:error]
    message = "Formula [#{value[:formula]}] syntax is incorrect (#{what}): #{how}"
    %Formulae.SyntaxError{error: value, formula: value[:formula], message: message}
  end

  @doc false
  def message(%__MODULE__{} = m) do
    m.message || "Formula [#{m.formula}] syntax is incorrect (#{inspect(m.error)})"
  end
end
