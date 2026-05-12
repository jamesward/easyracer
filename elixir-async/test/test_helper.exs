# When Mix runs inside Docker with a mounted socket (devcontainers, `docker run` dev
# workflows), Testcontainers defaults to the bridge gateway IP to reach published ports.
# On Docker Desktop that address does not reach mapped ports; use the host shim hostname
# unless the user already set an override (Linux Engine may set `TESTCONTAINERS_HOST_OVERRIDE`
# or add `host.docker.internal` via `--add-host`).
if File.exists?("/.dockerenv") and System.get_env("TESTCONTAINERS_HOST_OVERRIDE") in [nil, ""] do
  System.put_env("TESTCONTAINERS_HOST_OVERRIDE", "host.docker.internal")
end

{:ok, _} = Testcontainers.start_link()
# `:seed` 0 disables per-module test shuffling (ExUnit.Runner); tests run in definition order.
# `mix test --seed N` still overrides when you want a different order or reproduction.
# Custom formatter (fork of ExUnit.CLIFormatter) omits the "." pass marker so Logger lines
# are not prefixed when not using --trace.
ExUnit.start(seed: 0, formatters: [Support.ExUnitCLIFormatterNoDot])
