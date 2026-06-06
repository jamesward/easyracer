import Config

config :logger, :console,
  format: {EasyRacer.LogFormat, :format},
  metadata: [:pid, :mfa]
