defmodule Formulae.RunnerError do
  defexception [:formula, :error, :message]

  def exception(value) do
    {what, how} = value[:error]
    message = "Formula ~F[#{value[:formula]}] failed to run (#{what}): #{how}."
    %Formulae.RunnerError{formula: value[:formula], message: message, error: value[:error]}
  end
end
