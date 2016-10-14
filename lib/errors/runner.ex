defmodule Formulae.Runner do
  defexception [:formula, :error, :message]

  def exception(value) do
    {what, how} = value[:error]
    message = "Formula failed to run (#{what}): #{how}."
    %Formulae.Runner{formula: value[:formula], message: message, error: value[:error]}
  end

end
