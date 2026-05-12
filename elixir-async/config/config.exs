import Config

# Similar to java-cf `logback-test.xml` console pattern (time, thread, level, logger, message).
config :logger, :console,
  format: {EasyRacer.LogFormat, :format},
  metadata: [:pid, :mfa]
