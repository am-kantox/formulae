import Config

if Mix.env() == :test do
  config :logger,
    level: :warning,
    backends: [:console]
end
