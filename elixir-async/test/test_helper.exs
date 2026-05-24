if File.exists?("/.dockerenv") and System.get_env("TESTCONTAINERS_HOST_OVERRIDE") in [nil, ""] do
  System.put_env("TESTCONTAINERS_HOST_OVERRIDE", "host.docker.internal")
end

require Logger

scenario_server_image = "ghcr.io/jamesward/easyracer:latest"

Logger.info("Pre-pulling scenario-server image: #{scenario_server_image}")

pull_started_ms = System.monotonic_time(:millisecond)

{pull_output, pull_exit_code} =
  System.cmd("docker", ["pull", scenario_server_image], stderr_to_stdout: true)

pull_elapsed_ms = System.monotonic_time(:millisecond) - pull_started_ms

case pull_exit_code do
  0 ->
    Logger.info("Docker pull finished in #{pull_elapsed_ms}ms (exit #{pull_exit_code})")

  _ ->
    IO.puts(
      :stderr,
      "docker pull #{scenario_server_image} failed (exit #{pull_exit_code}):\n#{pull_output}"
    )

    System.halt(1)
end

{:ok, _} = Testcontainers.start_link()

ExUnit.start(seed: 0, formatters: [EasyRacer.ExUnitCLIFormatterNoDot])
