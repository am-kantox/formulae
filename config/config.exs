import Config

if Mix.env() == :test do
  config :logger, level: :warning
end

# config :formulae, :compiler, :finitomata
