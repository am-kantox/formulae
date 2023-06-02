import Config

if Mix.env() == :test do
  config :logger,
    level: :warn,
    backends: [:console]
end
