defmodule Formulae.Mixfile do
  use Mix.Project

  @app :formulae
  @ver "0.4.1"

  def project do
    [
      app: @app,
      version: @ver,
      elixir: "~> 1.4",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      deps: deps()
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
      {:iteraptor, "~> 1.2"},
      {:credo, "~> 0.9", only: [:dev]},
      {:ex_doc, "~> 0.11", only: :dev}
    ]
  end

  defp description do
    """
    A set of functions to deal with analytical formulae.
    """
  end

  defp package do
    # These are the default files included in the package
    [
      name: @app,
      files: ["lib", "config", "mix.exs", "README*"],
      maintainers: ["Aleksei Matiushkin"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/am-kantox/formulae",
        "Docs" => "https://hexdocs.pm/formulae"
      }
    ]
  end
end
