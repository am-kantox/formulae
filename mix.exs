defmodule Formulae.MixProject do
  use Mix.Project

  @app :formulae
  @version "0.18.0"

  def project do
    [
      app: @app,
      version: @version,
      elixir: "~> 1.11",
      compilers: Mix.compilers() ++ [:finitomata],
      prune_code_paths: Mix.env() == :prod,
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      aliases: aliases(),
      deps: deps(),
      docs: docs(),
      xref: [exclude: []],
      dialyzer: [
        plt_file: {:no_warn, ".dialyzer/plts/dialyzer.plt"},
        plt_add_apps: [:nimble_options, :finitomata],
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

  def cli do
    [preferred_envs: ["quality.ci": :ci]]
  end

  defp deps do
    [
      {:nimble_options, "~> 0.3 or ~> 1.0"},
      {:finitomata, "~> 0.14"},
      {:benchfella, "~> 0.3", only: [:dev]},
      {:dialyxir, "~> 1.0", only: [:dev, :ci], runtime: false},
      {:credo, "~> 1.0", only: [:dev, :ci]},
      {:ex_doc, "~> 0.11", only: [:dev]}
    ]
  end

  defp aliases do
    [
      quality: ["format", "credo --strict", "dialyzer"],
      "quality.ci": [
        "format --check-formatted",
        "credo --strict",
        "dialyzer --halt-exit-status"
      ]
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
      source_ref: "v#{@version}",
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

  defp docs do
    [
      main: "readme",
      source_ref: "v#{@version}",
      canonical: "http://hexdocs.pm/#{@app}",
      # logo: "stuff/images/logo.png",
      source_url: "https://github.com/am-kantox/#{@app}",
      # assets: "stuff/images",
      extras: [
        "README.md"
      ],
      groups_for_modules: [
        Combinators: [
          Formulae.Combinators,
          Formulae.Combinators.Stream
        ]
      ]
    ]
  end
end
