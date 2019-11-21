defmodule Formulae.Mixfile do
  use Mix.Project

  @app :formulae
  @ver "0.7.2"

  def project do
    [
      app: @app,
      version: @ver,
      elixir: "~> 1.4",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      dialyzer: [
        plt_file: {:no_warn, ".dialyzer/plts/dialyzer.plt"},
        ignore_warnings: ".dialyzer/ignore.exs"
      ]
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
      {:dialyxir, "~> 1.0.0-rc.6", only: [:dev, :test, :ci], runtime: false},
      {:credo, "~> 1.0", only: [:dev, :test, :ci]},
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
      source_ref: "v#{@ver}",
      source_url: "https://github.com/am-kantox/#{@app}",
      canonical: "http://hexdocs.pm/#{@app}",
      maintainers: ["Aleksei Matiushkin"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/am-kantox/formulae",
        "Docs" => "https://hexdocs.pm/formulae"
      }
    ]
  end
end
