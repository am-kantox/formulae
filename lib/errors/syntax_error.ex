defmodule Formulae.SyntaxError do
  defexception [:formula, :error, :message]

  def exception(value) do
    {what, how} = value[:error]
    message = "Formula [#{value[:formula]}] syntax is incorrect (#{what}): #{how}."
    %Formulae.SyntaxError{formula: value[:formula], message: message}
  end

end
