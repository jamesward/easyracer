# When Mix runs inside Docker with a mounted socket (devcontainers, `docker run` dev
# workflows), Testcontainers defaults to the bridge gateway IP to reach published ports.
# On Docker Desktop that address does not reach mapped ports; use the host shim hostname
# unless the user already set an override (Linux Engine may set `TESTCONTAINERS_HOST_OVERRIDE`
# or add `host.docker.internal` via `--add-host`).
if File.exists?("/.dockerenv") and System.get_env("TESTCONTAINERS_HOST_OVERRIDE") in [nil, ""] do
  System.put_env("TESTCONTAINERS_HOST_OVERRIDE", "host.docker.internal")
end

# Pre-pull the scenario-server image so a slow `ghcr.io` pull doesn't consume the
# hardcoded 300s `Testcontainers.start_container/1` GenServer.call timeout
# (see `deps/testcontainers/lib/testcontainers.ex` `@timeout 300_000`). Native
# `docker pull` surfaces registry/auth/network errors directly; locally this is
# a no-op since the image is already cached.
require Logger

scenario_server_image = "ghcr.io/jamesward/easyracer:latest"
pull_started_ms = System.monotonic_time(:millisecond)

Logger.info("Pre-pulling scenario-server image: #{scenario_server_image}")

{pull_output, pull_exit_code} =
  System.cmd("docker", ["pull", scenario_server_image], stderr_to_stdout: true)

pull_elapsed_ms = System.monotonic_time(:millisecond) - pull_started_ms

if pull_exit_code != 0 do
  IO.puts(
    :stderr,
    "docker pull #{scenario_server_image} failed (exit #{pull_exit_code}):\n#{pull_output}"
  )

  System.halt(1)
end

Logger.info("Docker pull finished in #{pull_elapsed_ms}ms (exit #{pull_exit_code})")

{:ok, _} = Testcontainers.start_link()
# `:seed` 0 disables per-module test shuffling (ExUnit.Runner); tests run in definition order.
# `mix test --seed N` still overrides when you want a different order or reproduction.
# Custom formatter (fork of ExUnit.CLIFormatter) omits the "." pass marker so Logger lines
# are not prefixed when not using --trace.
ExUnit.start(seed: 0, formatters: [EasyRacer.ExUnitCLIFormatterNoDot])
